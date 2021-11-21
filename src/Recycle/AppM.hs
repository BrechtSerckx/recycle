{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Recycle.AppM
  ( RecycleM(..)
  , Env(..)
  , runRecycle
  )
where

import           GHC.Generics
import           Capability.Reader
import           Capability.Error
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Trans.Reader     ( ReaderT(..) )
import           Servant.Client                 ( ClientEnv
                                                , ClientError
                                                )

import           Recycle.API


data Env = Env
  { clientEnv :: ClientEnv
  } deriving Generic

type InnerM = ReaderT Env IO

newtype RecycleM a = RecycleM
  { runRecycleM :: InnerM a
  } deriving newtype (Functor, Applicative, Monad, MonadIO)
    deriving (HasReader "clientEnv" ClientEnv)
      via Field "clientEnv" () (MonadReader InnerM)
    deriving (HasThrow "ClientError" ClientError)
      via MonadUnliftIO ClientError InnerM
    deriving HasServantClient
      via ServantClientT RecycleM

runRecycle :: RecycleM a -> Env -> IO a
runRecycle act env = runRecycleM act `runReaderT` env
