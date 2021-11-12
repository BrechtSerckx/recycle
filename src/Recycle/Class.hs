{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Recycle.Class
  ( HasTime(..)
  , HasRecycleAuth(..)
  , RecycleAuthT(..)
  , getAccessToken
  , setAccessToken
  )
where
import           Capability.Error
import           Capability.Reader
import           Capability.State
import           Colog                   hiding ( I )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import qualified Control.Monad.Reader          as Mtl
import           Control.Monad.Trans
import           Data.Time               hiding ( getZonedTime
                                                , getCurrentTime
                                                )
import qualified Data.Text                     as T
import qualified Data.Time                     as Time

import qualified Recycle.API                   as API
import           Recycle.Types

class Monad m => HasRecycleAuth m where
  getAuthResult :: m AuthResult
  setAuthResult :: AuthResult -> m ()
newtype RecycleAuthT m a = RecycleAuthT (m a)
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans RecycleAuthT where
  lift = RecycleAuthT

instance
  ( Monad m
  , HasState "authResult" (Maybe AuthResult) m
  , HasReader "authSecret" AuthSecret m
  , HasReader "consumer" Consumer m
  , HasThrow "ApiError" ApiError m
  , API.HasServantClient m
  , HasTime m
  , Mtl.MonadReader env m
  , HasLog env Message m
  ) => HasRecycleAuth (RecycleAuthT m) where
  getAuthResult = do
    lift $ logDebug "Checking access token ..."
    mAuthResult <- lift $ get @"authResult"
    now         <- lift $ getCurrentTime
    case mAuthResult of
      Just authResult@AuthResult {..} | now <= authResultExpiresAt -> do
        lift
          .  logDebug @env
          $  "Existing access token is still valid, expires at: "
          <> T.pack (show authResultExpiresAt)
        pure authResult
      Just AuthResult {..} | otherwise -> do
        lift . logDebug @env $ "Existing access token expired at: " <> T.pack
          (show authResultExpiresAt)
        refreshAccessToken
      Nothing -> do
        lift . logDebug @env $ "No access token present"
        refreshAccessToken
  setAuthResult = lift . put @"authResult" . Just

refreshAccessToken
  :: forall m env
   . ( Monad m
     , HasState "authResult" (Maybe AuthResult) m
     , HasReader "authSecret" AuthSecret m
     , HasReader "consumer" Consumer m
     , HasThrow "ApiError" ApiError m
     , API.HasServantClient m
     , HasTime m
     , HasLog env Message m
     , Mtl.MonadReader env m
     )
  => RecycleAuthT m AuthResult
refreshAccessToken = do
  consumer   <- lift $ ask @"consumer"
  authSecret <- lift $ ask @"authSecret"
  lift . logDebug @env $ "Trying to get access token ..."
  authResult@AuthResult {..} <-
    lift $ API.liftApiError =<< API.getAccessToken consumer authSecret
  lift . logDebug @env $ "Got new access token, valid till: " <> T.pack
    (show authResultExpiresAt)
  setAuthResult authResult
  pure authResult

getAccessToken :: HasRecycleAuth m => m AccessToken
getAccessToken = authResultAccessToken <$> getAuthResult

setAccessToken :: (HasRecycleAuth m, HasTime m) => AccessToken -> m ()
setAccessToken authResultAccessToken = do
  currentTime <- getCurrentTime
  let authResultExpiresAt = nominalDay `addUTCTime` currentTime
  setAuthResult AuthResult { .. }

class Monad m => HasTime m where
  getCurrentTime :: m UTCTime
  getZonedTime :: m ZonedTime

instance (Monad io, MonadIO io) => HasTime io where
  getZonedTime   = liftIO Time.getZonedTime
  getCurrentTime = liftIO Time.getCurrentTime
