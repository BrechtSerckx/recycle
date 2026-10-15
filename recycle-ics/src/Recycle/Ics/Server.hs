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
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time hiding (getZonedTime)
import Network.HTTP.Types (statusCode)
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

toServerError :: RecycleError -> ServerError
toServerError re =
  base
    { errBody =
        Aeson.encode $
          Aeson.object
            [ "cause" Aeson..= causeName re.cause,
              "message" Aeson..= show re
            ],
      errHeaders = [("Content-Type", "application/json")]
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
            Aeson.encode $
              Aeson.object
                [ "cause" Aeson..= ("invalid_request" :: Text),
                  "message" Aeson..= msg
                ],
          errHeaders = [("Content-Type", "application/json")]
        }

-- | The application's single WAI-level access logger: every request is
-- logged through the same Colog action the rest of the app uses, replacing
-- wai-extra's own @logStdoutDev@ (which wrote straight to stdout,
-- unconditionally, ignoring 'RECYCLE_ICS_VERBOSITY').
accessLogger :: Colog.LogAction IO Colog.Message -> IO Wai.Middleware
accessLogger logAction =
  mkRequestLogger
    defaultRequestLoggerSettings
      { outputFormat = CustomOutputFormatWithDetailsAndHeaders formatLine,
        destination = Callback logLine
      }
  where
    formatLine _zonedDate req status _size duration _reqBodyChunks _rawResp _headers =
      toLogStr $
        decodeUtf8With lenientDecode (Wai.requestMethod req)
          <> " "
          <> decodeUtf8With lenientDecode (Wai.rawPathInfo req)
          <> decodeUtf8With lenientDecode (Wai.rawQueryString req)
          <> " -> "
          <> T.pack (show (statusCode status))
          <> " ("
          <> T.pack (show duration)
          <> ")"

    logLine logStr = do
      let bs = fromLogStr logStr
      unless (BS.null bs) . usingLoggerT logAction $
        Colog.logInfo (decodeUtf8With lenientDecode bs)

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
