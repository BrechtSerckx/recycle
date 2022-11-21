{-# LANGUAGE DataKinds #-}

module Main
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
import qualified Data.Text as T
import Network.HTTP.Client.TLS
  ( newTlsManagerWith,
    tlsManagerSettings,
  )
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Opts
import Paths_recycle_ics
import Recycle.AppM
import Recycle.Class
import Recycle.Ics.ICalendar
import Recycle.Ics.Server
import Servant.Client
  ( BaseUrl (..),
    Scheme (..),
    mkClientEnv,
  )
import System.FilePath

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
        $ recycleIcsApp wwwDir env
