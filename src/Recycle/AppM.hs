{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Recycle.AppM
  ( RecycleM(..)
  , Env(..)
  , runRecycle
  )
where

import           Colog
import           GHC.Generics
import           Capability.Reader
import           Capability.Error
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Trans.Reader     ( ReaderT(..) )
import qualified Control.Monad.Reader          as Mtl
import           Servant.Client                 ( ClientEnv
                                                , ClientError
                                                )
import           Data.Generics.Labels           ( fieldLens )

import           Recycle.API


data Env = Env
  { clientEnv :: ClientEnv
  , logAction :: LogAction RecycleM Message
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
    deriving newtype (Mtl.MonadReader Env)

instance HasLog Env Message RecycleM where
  logActionL = fieldLens @"logAction" @Env

runRecycle :: RecycleM a -> Env -> IO a
runRecycle act env = runRecycleM act `runReaderT` env
