{-# LANGUAGE TypeOperators #-}
module Recycle
  ( main
  )
where

import           Control.Monad.IO.Class         ( liftIO )
import           Data.IORef                     ( newIORef )
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.Foldable                  ( for_ )
import           Data.Time               hiding ( getZonedTime )
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Lazy.Char8    as BSL8
import           Network.HTTP.Client.TLS        ( newTlsManagerWith
                                                , tlsManagerSettings
                                                )
import           Data.Proxy                     ( Proxy(..) )
import           Network.HTTP.Media.MediaType
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Servant.API
import           Servant.Client                 ( BaseUrl(..)
                                                , Scheme(..)
                                                , mkClientEnv
                                                )
import           Colog.Core                     ( LogAction(..) )
import           Colog.Message
import qualified Data.Text                     as T
import           Servant.Server                 ( Handler
                                                , ServerT
                                                , serve
                                                , hoistServer
                                                , emptyServer
                                                )

import           Recycle.Class
import           Recycle.AppM
import           Recycle.ICalendar
import           Recycle.Opts
import           Recycle.Types

main :: IO ()
main = do
  Opts { apiClientOpts = ApiClientOpts {..}, ..} <- parseOpts
  httpManager <- newTlsManagerWith tlsManagerSettings
  let clientEnv =
        mkClientEnv httpManager $ BaseUrl Https "recycleapp.be" 443 ""
      logAction = LogAction $ liftIO . putStrLn . T.unpack . fmtMessage

  authResult <- newIORef Nothing
  let env = Env { .. }
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
        range       <- liftIO $ calculateDateRange dateRange
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
        Just f  -> BSL.writeFile f bs
        Nothing -> BSL.putStr bs

    ServeIcs ServeIcsOpts {..} -> do
      putStrLn "Starting server"
      run port
        . logStdoutDev
        . serve pRecycleIcsAPI
        . hoistServer pRecycleIcsAPI (recycleToHandler env)
        $ recycleIcsServer

recycleToHandler :: Env -> RecycleM a -> Handler a
recycleToHandler env act = liftIO $ act `runRecycle` env

calculateDateRange :: HasTime m => DateRange -> m (Range Day)
calculateDateRange = \case
  AbsoluteDateRange r        -> pure r
  RelativeDateRange relRange -> do
    today <- localDay . zonedTimeToLocalTime <$> getZonedTime
    pure Range { rangeFrom = addDays (rangeFrom relRange) today
               , rangeTo   = addDays (rangeTo relRange) today
               }

-- brittany-disable-next-binding
type RecycleIcsAPI
  =   "api"
  :> EmptyAPI

pRecycleIcsAPI :: Proxy RecycleIcsAPI
pRecycleIcsAPI = Proxy

recycleIcsServer
  :: forall m . (HasRecycleClient m, HasTime m) => ServerT RecycleIcsAPI m
recycleIcsServer = emptyServer
runCollectionQuery
  :: (HasRecycleClient m, HasTime m) => CollectionQuery -> m VCalendar
runCollectionQuery CollectionQuery {..} = do
  range       <- calculateDateRange collectionQueryDateRange
  collections <- getCollections collectionQueryZipcode
                                collectionQueryStreet
                                collectionQueryHouseNumber
                                range
  -- for_ collections $ liftIO . print

  pure $ mkVCalendar collectionQueryLangCode
                     collectionQueryFractionEncoding
                     collections
