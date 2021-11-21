{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Recycle.API
  ( HasServantClient(..)
  , ServantClientT(..)
  )
where

import           Control.Monad.Trans
import           Capability.Reader
import           Capability.Error
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Servant.Client                 ( ClientM
                                                , ClientEnv
                                                , ClientError
                                                , runClientM
                                                )

class Monad m => HasServantClient m where
  runClient :: ClientM a -> m a

newtype ServantClientT m a = ServantClientT { runServantClientT :: m a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans ServantClientT where
  lift = ServantClientT

instance
  ( Monad m
  , HasReader "clientEnv" ClientEnv m
  , MonadIO m
  , HasThrow "ClientError" ClientError m
  ) => HasServantClient (ServantClientT m) where
  runClient act = do
    clientEnv <- lift $ ask @"clientEnv"
    eRes      <- lift . liftIO $ runClientM act clientEnv
    case eRes of
      Left  err -> lift $ throw @"ClientError" err
      Right a   -> pure a
