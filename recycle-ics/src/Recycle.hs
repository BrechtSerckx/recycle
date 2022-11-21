{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Recycle
  ( main,
  )
where

import Colog.Core (LogAction (..))
import Colog.Message
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Foldable (for_)
import Data.IORef (newIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time hiding (getZonedTime)
import Network.HTTP.Client.TLS
  ( newTlsManagerWith,
    tlsManagerSettings,
  )
import Network.HTTP.Media.MediaType
import Network.Wai.Application.Static
  ( defaultWebAppSettings,
  )
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Numeric.Natural (Natural)
import Paths_recycle_ics
import Recycle.AppM
import Recycle.Class
import Recycle.ICalendar
import Recycle.Opts
import Recycle.Types
import Servant.API
import Servant.API.QueryParamForm
import Servant.Client
  ( BaseUrl (..),
    Scheme (..),
    mkClientEnv,
  )
import Servant.Server
  ( Handler,
    ServerT,
    hoistServer,
    serve,
  )
import Servant.Server.StaticFiles (serveDirectoryWith)
import System.FilePath
import WaiAppStatic.Types

main :: IO ()
main = do
  Opts {apiClientOpts = ApiClientOpts {..}, ..} <- parseOpts
  httpManager <- newTlsManagerWith tlsManagerSettings
  let clientEnv =
        mkClientEnv httpManager $ BaseUrl Https "api.fostplus.be" 443 ""
      logAction = LogAction $ liftIO . putStrLn . T.unpack . fmtMessage

  authResult <- newIORef Nothing
  let env = Env {..}
  case cmd of
    ApiClient apiClientCmd -> flip runRecycle env $ case apiClientCmd of
      GetAccessToken -> liftIO . BSL8.putStrLn . Aeson.encode =<< getAuthResult
      SearchZipcodes mAccessToken mQuery -> do
        for_ mAccessToken setAccessToken
        zipcodes <- searchZipcodes mQuery
        liftIO . BSL8.putStrLn $ Aeson.encode zipcodes
      SearchStreets mAccessToken mZipcode mQuery -> do
        for_ mAccessToken setAccessToken
        streets <- searchStreets mZipcode mQuery
        liftIO . BSL8.putStrLn $ Aeson.encode streets
      GetCollections mAccessToken zipcode street houseNumber dateRange -> do
        for_ mAccessToken setAccessToken
        range <- liftIO $ calculateDateRange dateRange
        collections <- getCollections zipcode street houseNumber range
        liftIO . BSL8.putStrLn $ Aeson.encode collections
      GetFractions mAccessToken zipcode street houseNumber -> do
        for_ mAccessToken setAccessToken
        fractions <- getFractions zipcode street houseNumber
        liftIO . BSL8.putStrLn $ Aeson.encode fractions
    GenerateIcs GenerateIcsOpts {..} -> do
      vCalendar <- flip runRecycle env $ runCollectionQuery collectionQuery
      let bs = printVCalendar vCalendar
      case outputFile of
        Just f -> BSL.writeFile f bs
        Nothing -> BSL.putStr bs
    ServeIcs ServeIcsOpts {..} -> do
      dataDir <- getDataDir
      let wwwDir = dataDir </> "www"
      putStrLn "Starting server"
      run port
        . logStdoutDev
        . serve pRecycleIcsAPI
        . hoistServer pRecycleIcsAPI (recycleToHandler env)
        $ recycleIcsServer wwwDir

recycleToHandler :: Env -> RecycleM a -> Handler a
recycleToHandler env act = liftIO $ act `runRecycle` env

calculateDateRange :: HasTime m => DateRange -> m (Range Day)
calculateDateRange = \case
  AbsoluteDateRange r -> pure r
  RelativeDateRange relRange -> do
    today <- localDay . zonedTimeToLocalTime <$> getZonedTime
    pure
      Range
        { rangeFrom = addDays (rangeFrom relRange) today,
          rangeTo = addDays (rangeTo relRange) today
        }

-- brittany-disable-next-binding
type RecycleIcsAPI =
  ( "api"
      :> ( "search-zipcode"
             :> QueryParam' '[Required] "q" (SearchQuery Natural)
             :> UVerb 'GET '[JSON] '[WithStatus 200 [FullZipcode]]
             :<|> "search-street"
               :> QueryParam' '[Required] "zipcode" ZipcodeId
               :> QueryParam' '[Required] "q" (SearchQuery Text)
               :> UVerb 'GET '[JSON] '[WithStatus 200 [Street]]
             :<|> "generate"
               :> QueryParamForm DateRange
               :> QueryParam' '[Required] "lc" LangCode
               :> QueryParamForm FractionEncoding
               :> QueryParam' '[Required] "z" ZipcodeId
               :> QueryParam' '[Required] "s" StreetId
               :> QueryParam' '[Required] "hn" HouseNumber
               :> UVerb 'GET '[ICalendar] '[WithStatus 200 BSL.ByteString]
         )
      :<|> Raw
  )

data ICalendar

instance Accept ICalendar where
  contentType Proxy = "text" // "calendar"

instance MimeRender ICalendar BSL.ByteString where
  mimeRender Proxy = id

pRecycleIcsAPI :: Proxy RecycleIcsAPI
pRecycleIcsAPI = Proxy

recycleIcsServer ::
  forall m.
  (HasRecycleClient m, HasTime m) =>
  FilePath ->
  ServerT RecycleIcsAPI m
recycleIcsServer dataDir =
  (searchZipcode :<|> searchStreet :<|> generateCollection) :<|> serveWww
  where
    searchZipcode ::
      SearchQuery Natural -> m (Union '[WithStatus 200 [FullZipcode]])
    searchZipcode query = do
      zipcodes <- searchZipcodes (Just query)
      pure . Z . I $ WithStatus @200 zipcodes
    searchStreet ::
      ZipcodeId -> SearchQuery Text -> m (Union '[WithStatus 200 [Street]])
    searchStreet zipcodeId query = do
      streets <- searchStreets (Just zipcodeId) (Just query)
      pure . Z . I $ WithStatus @200 streets
    generateCollection ::
      DateRange ->
      LangCode ->
      FractionEncoding ->
      ZipcodeId ->
      StreetId ->
      HouseNumber ->
      m (Union '[WithStatus 200 BSL8.ByteString])
    generateCollection collectionQueryDateRange collectionQueryLangCode collectionQueryFractionEncoding collectionQueryZipcode collectionQueryStreet collectionQueryHouseNumber =
      do
        let collectionQuery = CollectionQuery {..}
        collections <- runCollectionQuery collectionQuery
        pure . Z . I . WithStatus @200 $ printVCalendar collections
    serveWww =
      serveDirectoryWith
        (defaultWebAppSettings dataDir)
          { ssRedirectToIndex = True,
            ssIndices = [unsafeToPiece "index.html"]
          }

runCollectionQuery ::
  (HasRecycleClient m, HasTime m) => CollectionQuery -> m VCalendar
runCollectionQuery CollectionQuery {..} = do
  range <- calculateDateRange collectionQueryDateRange
  collections <-
    getCollections
      collectionQueryZipcode
      collectionQueryStreet
      collectionQueryHouseNumber
      range
  -- for_ collections $ liftIO . print

  pure $
    mkVCalendar
      collectionQueryLangCode
      collectionQueryFractionEncoding
      collections
