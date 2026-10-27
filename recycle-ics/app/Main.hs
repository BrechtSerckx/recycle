module Main
  ( main,
  )
where

import qualified Colog
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Functor.Compose (Compose (..))
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
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import qualified System.Process.Typed as Proc
import Text.Read (readMaybe)

data LogHook = LogHook
  { hook :: FilePath,
    verbosity :: Colog.Severity
  }

mkLogHookAction :: (MonadIO m) => LogHook -> Colog.LogAction m Colog.Message
mkLogHookAction LogHook {..} =
  Colog.cfilter
    ((>= verbosity) . Colog.msgSeverity)
    . Colog.LogAction
    $ \Colog.Msg {..} ->
      Proc.runProcess_ $
        Proc.proc
          hook
          [show msgSeverity, show msgStack, T.unpack msgText]

main :: IO ()
main = do
  -- GHC block-buffers stdout by default once it's not a TTY (i.e. whenever
  -- this runs under podman/systemd/anything piping to a file or collector,
  -- the normal case in production). Colog's own writers don't force a
  -- flush, so without this a SIGTERM can silently drop an entire buffer's
  -- worth of unflushed log lines -- the previous logStdoutDev happened to
  -- paper over this via its own auto-flushing Handle destination.
  hSetBuffering stdout LineBuffering
  Opts {..} <- parseOpts
  httpManager <- newTlsManagerWith tlsManagerSettings
  let clientEnv =
        mkClientEnv httpManager $ BaseUrl Https "api.fostplus.be" 443 ""

  case cmd of
    GenerateIcs GenerateIcsOpts {..} -> do
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
          -- Same as 'logAction' above, just fixed to 'IO' for use outside
          -- 'RecycleM' (ApplicativeDo packs this do-block's let-bindings into
          -- a tuple, which can't hold 'logAction' at two different types).
          ioLogAction :: Colog.LogAction IO Colog.Message
          ioLogAction =
            Colog.cfilter
              ((>= verbosity) . Colog.msgSeverity)
              Colog.simpleMessageAction
              <> maybe mempty mkLogHookAction mLogHook
      accessLoggerMw <- accessLogger ioLogAction
      putStrLn "Starting server"
      run port
        . accessLoggerMw
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
