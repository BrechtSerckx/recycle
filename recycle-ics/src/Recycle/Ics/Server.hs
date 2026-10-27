{-# LANGUAGE DataKinds #-}

module Recycle.Ics.Server
  ( recycleIcsServer,
    recycleIcsApp,
    recycleToHandler,
    calculateDateRange,
    runCollectionQuery,
    accessLogger,
  )
where

import qualified Colog
import Colog.Monad (usingLoggerT)
import Control.Exception.Safe (try)
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time hiding (getZonedTime)
import GHC.Generics (Generic)
import Network.HTTP.Types (HeaderName, statusCode)
import qualified Network.Wai as Wai
import Network.Wai.Application.Static
  ( defaultWebAppSettings,
  )
import Network.Wai.Middleware.RequestLogger
  ( Destination (Callback),
    OutputFormat (CustomOutputFormatWithDetailsAndHeaders),
    defaultRequestLoggerSettings,
    destination,
    mkRequestLogger,
    outputFormat,
  )
import Numeric.Natural (Natural)
import Recycle.AppM
import Recycle.Class (HasRecycleClient, HasTime (..))
import qualified Recycle.Class as Recycle
import Recycle.Ics.API (RecycleIcsAPI, pRecycleIcsAPI)
import Recycle.Ics.ICalendar
import Recycle.Ics.Types
import Recycle.Types
import Servant.API
import Servant.Server
  ( Context (EmptyContext, (:.)),
    ErrorFormatter,
    ErrorFormatters,
    Handler,
    ServerError (..),
    ServerT,
    bodyParserErrorFormatter,
    defaultErrorFormatters,
    err400,
    err500,
    err503,
    headerParseErrorFormatter,
    hoistServerWithContext,
    serveWithContext,
    urlParseErrorFormatter,
  )
import Servant.Server.StaticFiles (serveDirectoryWith)
import System.Log.FastLogger (fromLogStr, toLogStr)
import WaiAppStatic.Types

recycleIcsServer ::
  forall m.
  (HasRecycleClient m, HasTime m) =>
  FilePath ->
  ServerT RecycleIcsAPI m
recycleIcsServer dataDir =
  (searchZipcode :<|> searchStreet :<|> getFractions :<|> generateCollection) :<|> serveWww
  where
    searchZipcode ::
      SearchQuery Natural -> m (Union '[WithStatus 200 [FullZipcode]])
    searchZipcode query = do
      zipcodes <- Recycle.searchZipcodes (Just query)
      pure . Z . I $ WithStatus @200 zipcodes
    searchStreet ::
      ZipcodeId -> SearchQuery Text -> m (Union '[WithStatus 200 [Street]])
    searchStreet zipcodeId query = do
      streets <- Recycle.searchStreets (Just zipcodeId) (Just query)
      pure . Z . I $ WithStatus @200 streets
    getFractions ::
      ZipcodeId ->
      StreetId ->
      HouseNumber ->
      m (Union '[WithStatus 200 [Fraction]])
    getFractions zipcode street houseNumber = do
      fractions <- Recycle.getFractions zipcode street houseNumber
      pure . Z . I $ WithStatus @200 fractions
    generateCollection ::
      DateRange ->
      LangCode ->
      EncodingSettings ->
      ZipcodeId ->
      StreetId ->
      HouseNumber ->
      Filter ->
      m (Union '[WithStatus 200 BSL8.ByteString])
    generateCollection dateRange langCode encodingSettings zipcode street houseNumber filter' =
      do
        let collectionQuery = CollectionQuery {filter = filter', ..}
        collections <- runCollectionQuery collectionQuery
        pure . Z . I . WithStatus @200 $ printVCalendar collections
    serveWww =
      serveDirectoryWith
        (defaultWebAppSettings dataDir)
          { ssRedirectToIndex = True,
            ssIndices = [unsafeToPiece "index.html"]
          }

recycleToHandler :: Env -> RecycleM a -> Handler a
recycleToHandler env act = do
  result <- liftIO $ try (act `runRecycle` env)
  case result of
    Left (re :: RecycleError) -> throwError (toServerError re)
    Right a -> pure a

-- | Tags responses built from a classified 'RecycleError', so 'accessLogger'
-- can tell them apart from a response Servant rejected before our handler
-- (and thus our logging) ever ran.
headerErrorSource :: HeaderName
headerErrorSource = "X-Recycle-Error-Source"

-- | The JSON shape of every error response this API returns, whether raised
-- by our own handlers ('toServerError') or by Servant itself rejecting a
-- request before a handler ever ran ('recycleErrorFormatters'). 'accessLogger'
-- decodes it back to log just the human-readable 'message'.
data ErrorResponse = ErrorResponse
  { cause :: Text,
    message :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

toServerError :: RecycleError -> ServerError
toServerError re =
  base
    { errBody =
        Aeson.encode
          ErrorResponse {cause = causeName re.cause, message = T.pack (show re)},
      errHeaders = [("Content-Type", "application/json"), (headerErrorSource, "app")]
    }
  where
    base = case re.cause of
      ServiceUnavailable -> err503
      InvalidRequest _ -> err400
      DecodeError _ -> err500
      OtherError _ -> err500
    causeName :: RecycleErrorCause -> Text
    causeName = \case
      ServiceUnavailable -> "service_unavailable"
      InvalidRequest _ -> "invalid_request"
      DecodeError _ -> "decode_error"
      OtherError _ -> "other_error"

recycleIcsApp :: FilePath -> Env -> Wai.Application
recycleIcsApp wwwDir env =
  serveWithContext pRecycleIcsAPI (recycleErrorFormatters :. EmptyContext)
    . hoistServerWithContext
      pRecycleIcsAPI
      (Proxy @'[ErrorFormatters])
      (recycleToHandler env)
    $ recycleIcsServer wwwDir

-- | Servant rejects a malformed query param, request body, or header before
-- our handler code -- and so 'toServerError' -- ever runs. Match its own
-- error responses to the same @{"cause": ..., "message": ...}@ shape ours
-- use, tagged the same as any other request-level rejection
-- ('InvalidRequest'\'s "invalid_request"), so 'accessLogger' logs something
-- consistent regardless of which layer rejected the request.
recycleErrorFormatters :: ErrorFormatters
recycleErrorFormatters =
  defaultErrorFormatters
    { bodyParserErrorFormatter = jsonErrorFormatter,
      urlParseErrorFormatter = jsonErrorFormatter,
      headerParseErrorFormatter = jsonErrorFormatter
    }
  where
    jsonErrorFormatter :: ErrorFormatter
    jsonErrorFormatter _typeRep _req msg =
      err400
        { errBody =
            Aeson.encode
              ErrorResponse {cause = "invalid_request", message = T.pack msg},
          errHeaders = [("Content-Type", "application/json")]
        }

-- | The application's single WAI-level access logger: every request is
-- logged through the same Colog action the rest of the app uses, replacing
-- wai-extra's own @logStdoutDev@ (which wrote straight to stdout,
-- unconditionally, ignoring 'RECYCLE_ICS_VERBOSITY').
--
-- A response status >= 400 carrying no 'headerErrorSource' tag means
-- Servant rejected the request before our handler (and 'RecycleError'
-- classification) ever ran -- logged at Error, with as much of the
-- rejection as we can recover. Everything else logs as a plain access-log
-- line at Info.
accessLogger :: Colog.LogAction IO Colog.Message -> IO Wai.Middleware
accessLogger logAction =
  mkRequestLogger
    defaultRequestLoggerSettings
      { outputFormat = CustomOutputFormatWithDetailsAndHeaders formatLine,
        destination = Callback logLine
      }
  where
    requestLine req =
      decodeUtf8With lenientDecode (Wai.requestMethod req)
        <> " "
        <> decodeUtf8With lenientDecode (Wai.rawPathInfo req)
        <> decodeUtf8With lenientDecode (Wai.rawQueryString req)

    -- The formatter is a pure function and can't call Colog itself, so each
    -- line is tagged with which severity 'logLine' below should use; the
    -- tag never reaches the actual log output.
    --
    -- "I\t": any response below 400, e.g. @GET /api/fractions -> 200 (12ms)@.
    -- "E\t": a status >= 400 Servant produced on its own, before our handler
    -- ran, e.g. @GET /api/fractions -> 400: Required@. A classified
    -- 'RecycleError' (tagged via 'headerErrorSource') gets neither -- it's
    -- already been logged once, by 'Recycle.AppM.runRecycle''s catch-all.
    formatLine _zonedDate req status _size duration _reqBodyChunks rawResp headers
      | statusCode status < 400 =
          toLogStr $
            "I\t"
              <> requestLine req
              <> " -> "
              <> T.pack (show (statusCode status))
              <> " ("
              <> T.pack (show duration)
              <> ")"
      | any ((== headerErrorSource) . fst) headers = mempty -- already logged, classified
      | otherwise =
          let body = toLazyByteString rawResp
           in toLogStr $
                "E\t"
                  <> maybe
                    (TL.toStrict (TLE.decodeUtf8With lenientDecode body))
                    (\(er :: ErrorResponse) -> er.message)
                    (Aeson.decode body)

    logLine logStr = do
      let bs = fromLogStr logStr
      unless (BS.null bs) $
        let (tag, rest) = BS.splitAt 2 bs
            message = decodeUtf8With lenientDecode rest
         in usingLoggerT logAction $ case tag of
              "E\t" -> Colog.logError message
              _ -> Colog.logError message

runCollectionQuery ::
  (HasRecycleClient m, HasTime m) => CollectionQuery -> m VCalendar
runCollectionQuery CollectionQuery {filter = filter', ..} = do
  range <- calculateDateRange dateRange
  collections <- Recycle.getCollections zipcode street houseNumber range
  pure $ mkVCalendar langCode encodingSettings filter' collections

calculateDateRange :: (HasTime m) => DateRange -> m (Range Day)
calculateDateRange = \case
  AbsoluteDateRange r -> pure r
  RelativeDateRange relRange -> do
    today <- localDay . zonedTimeToLocalTime <$> getZonedTime
    pure
      Range
        { from = addDays relRange.from today,
          to = addDays relRange.to today
        }
