{-# LANGUAGE DataKinds #-}

module Recycle.Ics.Server
  ( recycleIcsServer,
    recycleIcsApp,
    recycleToHandler,
    calculateDateRange,
    runCollectionQuery,
    logUnclassifiedErrors,
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
  ( Handler,
    ServerError (..),
    ServerT,
    err400,
    err500,
    err503,
    hoistServer,
    serve,
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

-- | Tags responses built from a classified 'RecycleError', so
-- 'logUnclassifiedErrors' can tell them apart from a response Servant
-- rejected before our handler (and thus our logging) ever ran.
headerErrorSource :: HeaderName
headerErrorSource = "X-Recycle-Error-Source"

toServerError :: RecycleError -> ServerError
toServerError re =
  base
    { errBody =
        Aeson.encode $
          Aeson.object
            [ "cause" Aeson..= causeName re.cause,
              "message" Aeson..= show re
            ],
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
  serve pRecycleIcsAPI
    . hoistServer pRecycleIcsAPI (recycleToHandler env)
    $ recycleIcsServer wwwDir

-- | WAI middleware that logs any response with status >= 400 which wasn't
-- already classified and logged via 'toServerError' -- most notably a
-- Servant-level request-decode failure (a malformed query param, a
-- 'Web.FormUrlEncoded.FromForm' parse failure), which is rejected by
-- Servant's own routing\/parsing layer before our handler code -- and so our
-- 'RecycleError' classification and Colog logging -- ever runs.
logUnclassifiedErrors :: Colog.LogAction IO Colog.Message -> IO Wai.Middleware
logUnclassifiedErrors logAction =
  mkRequestLogger
    defaultRequestLoggerSettings
      { outputFormat = CustomOutputFormatWithDetailsAndHeaders formatLine,
        destination = Callback logLine
      }
  where
    formatLine _zonedDate req status _size _duration _reqBodyChunks rawResp headers
      | statusCode status < 400 = mempty
      | any ((== headerErrorSource) . fst) headers = mempty
      | otherwise =
          toLogStr $
            "Servant rejected request before our handler ever ran: "
              <> decodeUtf8With lenientDecode (Wai.requestMethod req)
              <> " "
              <> decodeUtf8With lenientDecode (Wai.rawPathInfo req)
              <> decodeUtf8With lenientDecode (Wai.rawQueryString req)
              <> " -> "
              <> T.pack (show (statusCode status))
              <> ": "
              <> TL.toStrict (TLE.decodeUtf8With lenientDecode (toLazyByteString rawResp))
    logLine logStr =
      let bs = fromLogStr logStr
       in unless (BS.null bs) . usingLoggerT logAction $
            Colog.logError (decodeUtf8With lenientDecode bs)

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
