{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Recycle.AppM
  ( RecycleM(..)
  , Env(..)
  , runRecycle
  )
where

import           Colog
import           Data.IORef
import           GHC.Generics
import           Capability.Reader
import           Capability.Error
import           Capability.State
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Trans.Reader     ( ReaderT(..) )
import qualified Control.Monad.Reader          as Mtl
import           Servant.Client                 ( ClientEnv
                                                , ClientError
                                                )
import           Data.Generics.Labels           ( fieldLens )

import           Recycle.API
import           Recycle.Class
import           Recycle.Types


data Env = Env
  { clientEnv :: ClientEnv
  , logAction :: LogAction RecycleM Message
  , consumer :: Consumer
  , authSecret :: AuthSecret
  , authResult :: IORef (Maybe AuthResult)
  } deriving Generic

type InnerM = ReaderT Env IO

newtype RecycleM a = RecycleM
  { runRecycleM :: InnerM a
  } deriving newtype (Functor, Applicative, Monad, MonadIO)
    deriving (HasReader "clientEnv" ClientEnv)
      via Field "clientEnv" () (MonadReader InnerM)
    deriving (HasReader "authSecret" AuthSecret)
      via Field "authSecret" () (MonadReader InnerM)
    deriving (HasThrow "ClientError" ClientError)
      via MonadUnliftIO ClientError InnerM
    deriving HasServantClient
      via ServantClientT RecycleM
    deriving newtype (Mtl.MonadReader Env)
    deriving (HasReader "consumer" Consumer)
      via Field "consumer" () (MonadReader InnerM)
    deriving (HasState "authResult" (Maybe AuthResult))
      via ReaderIORef (Field "authResult" () (MonadReader InnerM))
    deriving (HasThrow "ApiError" ApiError)
      via MonadUnliftIO ApiError InnerM
    deriving HasRecycleAuth
      via RecycleAuthT RecycleM
    deriving HasRecycleClient
      via RecycleClientT RecycleM

instance HasLog Env Message RecycleM where
  logActionL = fieldLens @"logAction" @Env

runRecycle :: RecycleM a -> Env -> IO a
runRecycle act env = runRecycleM act `runReaderT` env
