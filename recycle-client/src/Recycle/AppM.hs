{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Recycle.AppM
  ( RecycleM (..),
    Env (..),
    runRecycle,
  )
where

import Capability.Accessors (Field (..))
import Capability.Error
  ( HasThrow,
    MonadUnliftIO (..),
  )
import Capability.Reader
  ( HasReader,
    MonadReader (..),
  )
import Capability.Sink (HasSink)
import Capability.Source (HasSource)
import Capability.State
  ( HasState,
    ReaderIORef (..),
  )
import Colog
import Control.Exception.Safe
  ( MonadCatch,
    MonadThrow,
    SomeException,
    catch,
    displayException,
    throw,
  )
import Control.Monad.IO.Class (MonadIO (..))
import qualified Control.Monad.Reader as Mtl
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Generics.Labels (fieldLens)
import Data.IORef
import qualified Data.Text as T
import GHC.Generics
import Recycle.API
import Recycle.Class
import Recycle.Types
import Servant.Client
  ( ClientEnv,
    ClientError,
  )

data Env = Env
  { clientEnv :: ClientEnv,
    logAction :: LogAction RecycleM Message,
    consumer :: Consumer,
    authSecret :: AuthSecret,
    authResult :: IORef (Maybe AuthResult)
  }
  deriving (Generic)

type InnerM = ReaderT Env IO

newtype RecycleM a = RecycleM (InnerM a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving
    (HasReader "clientEnv" ClientEnv, HasSource "clientEnv" ClientEnv)
    via Field "clientEnv" () (MonadReader InnerM)
  deriving
    (HasReader "authSecret" AuthSecret, HasSource "authSecret" AuthSecret)
    via Field "authSecret" () (MonadReader InnerM)
  deriving
    (HasThrow "ClientError" ClientError)
    via MonadUnliftIO ClientError InnerM
  deriving
    (HasServantClient)
    via ServantClientT RecycleM
  deriving newtype (Mtl.MonadReader Env)
  deriving
    (HasReader "consumer" Consumer, HasSource "consumer" Consumer)
    via Field "consumer" () (MonadReader InnerM)
  deriving
    ( HasState "authResult" (Maybe AuthResult),
      HasSource "authResult" (Maybe AuthResult),
      HasSink "authResult" (Maybe AuthResult)
    )
    via ReaderIORef (Field "authResult" () (MonadReader InnerM))
  deriving
    (HasThrow "ApiError" ApiError)
    via MonadUnliftIO ApiError InnerM
  deriving
    (HasRecycleAuth)
    via RecycleAuthT RecycleM
  deriving
    (HasRecycleClient)
    via RecycleClientT RecycleM
  deriving (MonadThrow, MonadCatch) via InnerM

instance HasLog Env Message RecycleM where
  logActionL = fieldLens @"logAction" @Env

runRecycle :: RecycleM a -> Env -> IO a
runRecycle act env =
  let RecycleM act' =
        act
          `catch` ( \(e :: SomeException) -> do
                      logError . T.pack $ displayException e
                      liftIO $ throw e
                  )
   in act' `runReaderT` env
