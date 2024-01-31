{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Recycle.Class
  ( HasTime (..),
    HasRecycleAuth (..),
    RecycleAuthT (..),
    getAccessToken,
    setAccessToken,
    HasRecycleClient (..),
    RecycleClientT (..),
  )
where

import Capability.Error
import Capability.Reader
import Capability.State
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

class (Monad m) => HasRecycleAuth m where
  getAuthResult :: m AuthResult
  setAuthResult :: AuthResult -> m ()

newtype RecycleAuthT m a = RecycleAuthT (m a)
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans RecycleAuthT where
  lift = RecycleAuthT

instance
  ( Monad m,
    HasState "authResult" (Maybe AuthResult) m,
    HasReader "authSecret" AuthSecret m,
    HasReader "consumer" Consumer m,
    HasThrow "ApiError" ApiError m,
    API.HasServantClient m,
    HasTime m,
    Mtl.MonadReader env m,
    HasLog env Message m
  ) =>
  HasRecycleAuth (RecycleAuthT m)
  where
  getAuthResult = do
    lift $ logDebug "Checking access token ..."
    mAuthResult <- lift $ get @"authResult"
    now <- lift getCurrentTime
    case mAuthResult of
      Just authResult@AuthResult {..} | now <= expiresAt -> do
        lift
          . logDebug @env
          $ "Existing access token is still valid, expires at: "
            <> T.pack (show expiresAt)
        pure authResult
      Just AuthResult {..} | otherwise -> do
        lift . logDebug @env $
          "Existing access token expired at: "
            <> T.pack
              (show expiresAt)
        refreshAccessToken
      Nothing -> do
        lift . logDebug @env $ "No access token present"
        refreshAccessToken
  setAuthResult = lift . put @"authResult" . Just

refreshAccessToken ::
  forall m env.
  ( Monad m,
    HasState "authResult" (Maybe AuthResult) m,
    HasReader "authSecret" AuthSecret m,
    HasReader "consumer" Consumer m,
    HasThrow "ApiError" ApiError m,
    API.HasServantClient m,
    HasTime m,
    HasLog env Message m,
    Mtl.MonadReader env m
  ) =>
  RecycleAuthT m AuthResult
refreshAccessToken = do
  consumer <- lift $ ask @"consumer"
  authSecret <- lift $ ask @"authSecret"
  lift . logDebug @env $ "Trying to get access token ..."
  authResult@AuthResult {..} <-
    lift $ API.liftApiError =<< API.getAccessToken consumer authSecret
  lift . logDebug @env $
    "Got new access token, valid till: "
      <> T.pack
        (show expiresAt)
  setAuthResult authResult
  pure authResult

getAccessToken :: (HasRecycleAuth m) => m AccessToken
getAccessToken = (.accessToken) <$> getAuthResult

setAccessToken :: (HasRecycleAuth m, HasTime m) => AccessToken -> m ()
setAccessToken accessToken = do
  currentTime <- getCurrentTime
  let expiresAt = nominalDay `addUTCTime` currentTime
  setAuthResult AuthResult {..}

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
    HasRecycleAuth m,
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
      \consumer accessToken -> API.searchZipcodes consumer accessToken mQ
    pure zipcodes
  searchStreets mZipcode mQ = do
    lift
      . logInfo
      $ "Searching streets in zipcode "
        <> maybe "<all>" (.unZipcodeId) mZipcode
        <> ": "
        <> maybe "<all>" (.unSearchQuery) mQ
    SingObject streets <- runRecycleOp $ \consumer accessToken ->
      API.searchStreets consumer accessToken mZipcode mQ
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
    SingObject collections <- runRecycleOp $ \consumer accessToken ->
      API.getCollections
        consumer
        accessToken
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
    SingObject fractions <- runRecycleOp $ \consumer accessToken ->
      API.getFractions consumer accessToken zipcode street houseNumber
    pure fractions

runRecycleOp ::
  ( Monad m,
    HasReader "consumer" Consumer m,
    HasRecycleAuth m,
    HasThrow "ApiError" ApiError m,
    API.HasServantClient m
  ) =>
  ( Consumer ->
    AccessToken ->
    m (Union '[WithStatus 200 a, WithStatus err ApiError])
  ) ->
  RecycleClientT m a
runRecycleOp op = do
  consumer <- lift $ ask @"consumer"
  accessToken <- lift getAccessToken
  lift $ API.liftApiError =<< op consumer accessToken

class (Monad m) => HasTime m where
  getCurrentTime :: m UTCTime
  getZonedTime :: m ZonedTime

instance (Monad io, MonadIO io) => HasTime io where
  getZonedTime = liftIO Time.getZonedTime
  getCurrentTime = liftIO Time.getCurrentTime
