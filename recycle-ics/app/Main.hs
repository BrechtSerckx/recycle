{-# LANGUAGE DataKinds #-}

module Main
  ( main,
  )
where

import Colog.Core (LogAction (..))
import Colog.Message
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
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