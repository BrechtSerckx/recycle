module Main
  ( main,
  )
where

import Colog.Core (LogAction (..))
import Colog.Message
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.IORef (newIORef)
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Haskell.TH.Env as TH.Env
import Network.HTTP.Client.TLS
  ( newTlsManagerWith,
    tlsManagerSettings,
  )
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger
import Opts
import Recycle.AppM
import Recycle.Ics.ICalendar
import Recycle.Ics.Server
import Servant.Client
  ( BaseUrl (..),
    Scheme (..),
    mkClientEnv,
  )
import qualified System.Environment as Env
import Text.Read (readMaybe)

main :: IO ()
main = do
  Opts {..} <- parseOpts
  httpManager <- newTlsManagerWith tlsManagerSettings
  let clientEnv =
        mkClientEnv httpManager $ BaseUrl Https "api.fostplus.be" 443 ""
      logAction = LogAction $ liftIO . putStrLn . T.unpack . fmtMessage

  authResult <- newIORef Nothing
  case cmd of
    GenerateIcs GenerateIcsOpts {apiClientOpts = ApiClientOpts {..}, ..} -> do
      let env = Env {..}
      vCalendar <- flip runRecycle env $ runCollectionQuery collectionQuery
      let bs = printVCalendar vCalendar
      case outputFile of
        Just f -> BSL.writeFile f bs
        Nothing -> BSL.putStr bs
    ServeIcs ServeIcsOpts -> do
      port <-
        lookupEnvRead
          "RECYCLE_ICS_PORT"
          "Port where recycle-ics runs on."
      consumer <-
        lookupEnvString
          "RECYCLE_ICS_CONSUMER"
          "X-Consumer header, get it by inspecting requests to recycleapp.be"
      authSecret <-
        lookupEnvString
          "RECYCLE_ICS_SECRET"
          "X-Authorization header, get it by inspecting requests to recycleapp.be"
      let env = Env {..}
      putStrLn "Starting server"
      run port
        . logStdoutDev
        . simpleCors
        $ recycleIcsApp
          $$(TH.Env.envQ' "RECYCLE_ICS_WWW_DIR")
          env

lookupEnvRead :: (Read a) => Text -> Text -> IO a
lookupEnvRead var descr = do
  mStr <- Env.lookupEnv (T.unpack var)
  case mStr of
    Nothing -> error . T.unpack $ "Environment variable " <> var <> " expected: " <> descr
    Just str -> case readMaybe str of
      Nothing -> error . T.unpack $ "Could not read environment variable " <> var <> ": " <> descr
      Just a -> pure a

lookupEnvString :: (IsString a) => Text -> Text -> IO a
lookupEnvString var descr = do
  mStr <- Env.lookupEnv (T.unpack var)
  case mStr of
    Nothing -> error . T.unpack $ "Environment variable " <> var <> " expected: " <> descr
    Just str -> pure $ fromString str
