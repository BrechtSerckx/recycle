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
import Capability.Source (HasSource)
import Colog
import Control.Exception.Safe
  ( MonadCatch,
    MonadThrow,
    SomeException,
    catch,
    displayException,
    throw,
  )
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Control.Monad.Reader as Mtl
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Generics.Labels (fieldLens)
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
    consumer :: Consumer
  }
  deriving (Generic)

type InnerM = ReaderT Env IO

newtype RecycleM a = RecycleM (InnerM a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving
    (HasReader "clientEnv" ClientEnv, HasSource "clientEnv" ClientEnv)
    via Field "clientEnv" () (MonadReader InnerM)
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
    (HasThrow "ApiError" ApiError)
    via MonadUnliftIO ApiError InnerM
  deriving
    (HasRecycleClient)
    via RecycleClientT RecycleM
  deriving (MonadThrow, MonadCatch, MonadMask) via InnerM

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
