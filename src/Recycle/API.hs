{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Recycle.API
  ( RecycleAPI
  , getAccessToken
  , searchZipcodes
  , searchStreets
  , getCollections
  , getFractions
  , liftApiError
  , HasServantClient(..)
  , ServantClientT(..)
  )
where

import           Control.Monad.Trans
import           Capability.Reader
import           Data.Text                      ( Text )
import           Capability.Error
import           Numeric.Natural                ( Natural )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Aeson.Extra.SingObject    ( SingObject )
import           Data.Proxy                     ( Proxy(..) )
import           Data.SOP                       ( I(..)
                                                , NS(..)
                                                )
import           Data.Time                      ( Day )
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
  =  "recycle-public"
  :> "app"
  :> "v1"
  :>  (  "access-token"
      :> Header' '[Required] "X-Consumer" Consumer
      :> Header' '[Required] "X-Secret" AuthSecret
      :> UVerb 'GET '[JSON] '[WithStatus 200 AuthResult, WithStatus 401 ApiError]
    :<|> "zipcodes"
      :> Header' '[Required] "X-Consumer" Consumer
      :> Header' '[Required] "Authorization" AccessToken
      :> QueryParam' '[Optional] "q" (SearchQuery Natural)
      :> UVerb 'GET '[JSON] '[WithStatus 200 (SingObject "items" [FullZipcode]), WithStatus 401 ApiError]
    :<|> "streets"
      :> Header' '[Required] "X-Consumer" Consumer
      :> Header' '[Required] "Authorization" AccessToken
      :> QueryParam' '[Optional] "zipcodes" ZipcodeId
      :> QueryParam' '[Optional] "q" (SearchQuery Text)
      :> UVerb 'POST '[JSON] '[WithStatus 200 (SingObject "items" [Street]), WithStatus 401 ApiError]
    :<|> "collections"
      :> Header' '[Required] "X-Consumer" Consumer
      :> Header' '[Required] "Authorization" AccessToken
      :> QueryParam' '[Required] "zipcodeId" ZipcodeId
      :> QueryParam' '[Required] "streetId" StreetId
      :> QueryParam' '[Required] "houseNumber" HouseNumber
      :> QueryParam' '[Required] "fromDate" Day
      :> QueryParam' '[Required] "untilDate" Day
      :> UVerb 'GET '[JSON] '[WithStatus 200 (SingObject "items" [CollectionEvent (Union '[FullFraction, Event])]), WithStatus 401 ApiError]
    :<|> "fractions"
      :> Header' '[Required] "X-Consumer" Consumer
      :> Header' '[Required] "Authorization" AccessToken
      :> QueryParam' '[Required] "zipcodeId" ZipcodeId
      :> QueryParam' '[Required] "streetId" StreetId
      :> QueryParam' '[Required] "houseNumber" HouseNumber
      :> UVerb 'GET '[JSON] '[WithStatus 200 (SingObject "items" [Fraction]), WithStatus 401 ApiError]
     )

getAccessToken
  :: HasServantClient m
  => Consumer
  -> AuthSecret
  -> m (NS I '[WithStatus 200 AuthResult, WithStatus 401 ApiError])
searchZipcodes
  :: HasServantClient m
  => Consumer
  -> AccessToken
  -> Maybe (SearchQuery Natural)
  -> m
       ( NS
           I
           '[WithStatus 200 (SingObject "items" [FullZipcode]), WithStatus
             401
             ApiError]
       )
searchStreets
  :: HasServantClient m
  => Consumer
  -> AccessToken
  -> Maybe ZipcodeId
  -> Maybe (SearchQuery Text)
  -> m
       ( NS
           I
           '[WithStatus 200 (SingObject "items" [Street]), WithStatus
             401
             ApiError]
       )
getCollections
  :: HasServantClient m
  => Consumer
  -> AccessToken
  -> ZipcodeId
  -> StreetId
  -> HouseNumber
  -> Day
  -> Day
  -> m
       ( NS
           I
           '[WithStatus
             200
             ( SingObject
                 "items"
                 [ CollectionEvent
                     (Union '[FullFraction, Event])
                 ]
             ), WithStatus 401 ApiError]
       )
getFractions
  :: HasServantClient m
  => Consumer
  -> AccessToken
  -> ZipcodeId
  -> StreetId
  -> HouseNumber
  -> m
       ( Union
           '[WithStatus 200 (SingObject "items" [Fraction]), WithStatus
             401
             ApiError]
       )
getAccessToken :<|> searchZipcodes :<|> searchStreets :<|> getCollections :<|> getFractions
  = hoistClient (Proxy @RecycleAPI) runClient (client $ Proxy @RecycleAPI)

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
