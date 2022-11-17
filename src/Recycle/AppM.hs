{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Recycle.AppM
  ( RecycleM(..)
  , Env(..)
  , runRecycle
  ) where

import           Capability.Accessors           ( Field(..) )
import           Capability.Error               ( HasThrow
                                                , MonadUnliftIO(..)
                                                )
import           Capability.Reader              ( HasReader
                                                , MonadReader(..)
                                                )
import           Capability.Sink                ( HasSink )
import           Capability.Source              ( HasSource )
import           Capability.State               ( HasState
                                                , ReaderIORef(..)
                                                )
import           Colog
import           Control.Monad.IO.Class         ( MonadIO(..) )
import qualified Control.Monad.Reader          as Mtl
import           Control.Monad.Trans.Reader     ( ReaderT(..) )
import           Data.Generics.Labels           ( fieldLens )
import           Data.IORef
import           GHC.Generics
import           Servant.Client                 ( ClientEnv
                                                , ClientError
                                                )

import           Recycle.API
import           Recycle.Class
import           Recycle.Types


data Env = Env
  { clientEnv  :: ClientEnv
  , logAction  :: LogAction RecycleM Message
  , consumer   :: Consumer
  , authSecret :: AuthSecret
  , authResult :: IORef (Maybe AuthResult)
  }
  deriving Generic

type InnerM = ReaderT Env IO

newtype RecycleM a = RecycleM
  { runRecycleM :: InnerM a
  } deriving newtype (Functor, Applicative, Monad, MonadIO)
    deriving (HasReader "clientEnv" ClientEnv, HasSource "clientEnv" ClientEnv)
      via Field "clientEnv" () (MonadReader InnerM)
    deriving (HasReader "authSecret" AuthSecret, HasSource "authSecret" AuthSecret)
      via Field "authSecret" () (MonadReader InnerM)
    deriving (HasThrow "ClientError" ClientError)
      via MonadUnliftIO ClientError InnerM
    deriving HasServantClient
      via ServantClientT RecycleM
    deriving newtype (Mtl.MonadReader Env)
    deriving (HasReader "consumer" Consumer, HasSource "consumer" Consumer)
      via Field "consumer" () (MonadReader InnerM)
    deriving ( HasState "authResult" (Maybe AuthResult)
             , HasSource "authResult" (Maybe AuthResult)
             , HasSink "authResult" (Maybe AuthResult)
             )
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
