{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Recycle.Class
  ( HasTime (..),
    HasRecycleClient (..),
    RecycleClientT (..),
  )
where

import Capability.Error
import Capability.Reader
import Colog hiding (I)
import qualified Control.Monad.Reader as Mtl
import Control.Monad.Trans
import Data.Aeson.Extra.SingObject (SingObject (..))
import qualified Data.Text as T
import Data.Time hiding
  ( getCurrentTime,
    getZonedTime,
  )
import qualified Data.Time as Time
import Numeric.Natural (Natural)
import qualified Recycle.API as API
import Recycle.Types
import Servant.API

class (Monad m) => HasRecycleClient m where
  searchZipcodes :: Maybe (SearchQuery Natural) -> m [FullZipcode]
  searchStreets :: Maybe ZipcodeId -> Maybe (SearchQuery T.Text) -> m [Street]
  getCollections ::
    ZipcodeId ->
    StreetId ->
    HouseNumber ->
    Range Day ->
    m [CollectionEvent]
  getFractions ::
    ZipcodeId ->
    StreetId ->
    HouseNumber ->
    m [Fraction]

newtype RecycleClientT m a = RecycleClientT {runRecycleClientT :: m a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans RecycleClientT where
  lift = RecycleClientT

instance
  ( Monad m,
    HasReader "consumer" Consumer m,
    HasThrow "ApiError" ApiError m,
    API.HasServantClient m,
    Mtl.MonadReader env m,
    HasLog env Message m
  ) =>
  HasRecycleClient (RecycleClientT m)
  where
  searchZipcodes mQ = do
    lift
      . logInfo
      $ "Searching zipcodes: "
        <> maybe "<all>" (T.pack . show . (.unSearchQuery)) mQ
    SingObject zipcodes <- runRecycleOp $
      \consumer -> API.searchZipcodes consumer mQ
    pure zipcodes
  searchStreets mZipcode mQ = do
    lift
      . logInfo
      $ "Searching streets in zipcode "
        <> maybe "<all>" (.unZipcodeId) mZipcode
        <> ": "
        <> maybe "<all>" (.unSearchQuery) mQ
    SingObject streets <- runRecycleOp $ \consumer ->
      API.searchStreets consumer mZipcode mQ
    pure streets
  getCollections zipcode street houseNumber Range {..} = do
    lift
      . logInfo
      $ "Fetching collections for "
        <> zipcode.unZipcodeId
        <> ", "
        <> street.unStreetId
        <> ", "
        <> T.pack (show houseNumber.unHouseNumber)
        <> " from "
        <> T.pack (show from)
        <> " to "
        <> T.pack (show to)
    SingObject collections <- runRecycleOp $ \consumer ->
      API.getCollections
        consumer
        zipcode
        street
        houseNumber
        from
        to
    pure collections
  getFractions zipcode street houseNumber = do
    lift
      . logInfo
      $ "Fetching fractions for "
        <> zipcode.unZipcodeId
        <> ", "
        <> street.unStreetId
        <> ", "
        <> T.pack (show houseNumber.unHouseNumber)
    SingObject fractions <- runRecycleOp $ \consumer ->
      API.getFractions consumer zipcode street houseNumber
    pure fractions

runRecycleOp ::
  ( Monad m,
    HasReader "consumer" Consumer m,
    HasThrow "ApiError" ApiError m,
    API.HasServantClient m
  ) =>
  ( Consumer ->
    m (Union '[WithStatus 200 a, WithStatus err ApiError])
  ) ->
  RecycleClientT m a
runRecycleOp op = do
  consumer <- lift $ ask @"consumer"
  lift $ API.liftApiError =<< op consumer

class (Monad m) => HasTime m where
  getCurrentTime :: m UTCTime
  getZonedTime :: m ZonedTime

instance (Monad io, MonadIO io) => HasTime io where
  getZonedTime = liftIO Time.getZonedTime
  getCurrentTime = liftIO Time.getCurrentTime
