{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Recycle.API
  ( RecycleAPI
  , getAccessToken
  , liftApiError
  , HasServantClient(..)
  , ServantClientT(..)
  )
where

import           Control.Monad.Trans
import           Capability.Reader
import           Capability.Error
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Proxy                     ( Proxy(..) )
import           Data.SOP                       ( I(..)
                                                , NS(..)
                                                )
import           Servant.API
import           Servant.Client                 ( ClientM
                                                , client
                                                , hoistClient
                                                , ClientEnv
                                                , ClientError
                                                , runClientM
                                                )

import           Recycle.Types

-- brittany-disable-next-binding
type RecycleAPI
  =  "api"
  :> "app"
  :> "v1"
  :>  (  "access-token"
      :> Header' '[Required] "X-Consumer" Consumer
      :> Header' '[Required] "X-Secret" AuthSecret
      :> UVerb 'GET '[JSON] '[WithStatus 200 AuthResult, WithStatus 401 ApiError]
     )

getAccessToken
  :: HasServantClient m
  => Consumer
  -> AuthSecret
  -> m (NS I '[WithStatus 200 AuthResult, WithStatus 401 ApiError])
getAccessToken =
  hoistClient (Proxy @RecycleAPI) runClient (client $ Proxy @RecycleAPI)

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


liftApiError
  :: HasThrow "ApiError" ApiError m
  => NS I '[WithStatus 200 a, WithStatus err ApiError]
  -> m a
liftApiError = \case
  Z (I (WithStatus a)) -> pure a
  S x                  -> case x of
    Z (I (WithStatus err@ApiError{})) -> throw @"ApiError" err
    S x'                              -> case x' of {}
