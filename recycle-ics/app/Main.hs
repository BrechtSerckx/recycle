module Main
  ( main,
  )
where

import qualified Colog
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Functor.Compose (Compose (..))
import Data.IORef (newIORef)
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)
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

data LogHook = LogHook
  { hook :: FilePath,
    verbosity :: Colog.Severity
  }

mkLogHookAction :: LogHook -> Colog.LogAction
mkLogHookAction LogHook {..} = undefined

main :: IO ()
main = do
  Opts {..} <- parseOpts
  httpManager <- newTlsManagerWith tlsManagerSettings
  let clientEnv =
        mkClientEnv httpManager $ BaseUrl Https "api.fostplus.be" 443 ""

  authResult <- newIORef Nothing
  case cmd of
    GenerateIcs GenerateIcsOpts {apiClientOpts = ApiClientOpts {..}, ..} -> do
      let logAction =
            Colog.cfilter
              ((>= verbosity) . Colog.msgSeverity)
              Colog.simpleMessageAction
          env = Env {..}
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
      verbosity <-
        fromMaybe Colog.Warning <$> lookupEnvMRead "RECYCLE_ICS_VERBOSITY"
      mLogHook <- getCompose $ do
        hook <- Compose $ lookupEnvMString "RECYCLE_ICS_LOG_HOOK"
        verbosity' <- Compose $ lookupEnvMRead "RECYCLE_ICS_LOG_HOOK_VERBOSITY"
        pure LogHook {verbosity = verbosity', ..}
      let logAction =
            Colog.cfilter
              ((>= verbosity) . Colog.msgSeverity)
              Colog.simpleMessageAction
              <> maybe mempty mkLogHookAction mLogHook
          env = Env {..}
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
      Nothing -> error . T.unpack $ "Could not read environment variable " <> var
      Just a -> pure a

lookupEnvMRead :: (Read a) => Text -> IO (Maybe a)
lookupEnvMRead var = do
  mStr <- Env.lookupEnv (T.unpack var)
  for mStr $ \str -> case readMaybe str of
    Nothing -> error . T.unpack $ "Could not read environment variable " <> var
    Just a -> pure a

lookupEnvString :: (IsString a) => Text -> Text -> IO a
lookupEnvString var descr = do
  mStr <- Env.lookupEnv (T.unpack var)
  case mStr of
    Nothing -> error . T.unpack $ "Environment variable " <> var <> " expected: " <> descr
    Just str -> pure $ fromString str

lookupEnvMString :: (IsString a) => Text -> IO (Maybe a)
lookupEnvMString var = do
  fmap fromString <$> Env.lookupEnv (T.unpack var)
