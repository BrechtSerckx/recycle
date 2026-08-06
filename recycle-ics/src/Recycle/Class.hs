{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Recycle.Class
  ( HasTime (..),
    HasRecycleClient (..),
    RecycleClientT (..),
  )
where

import Capability.Error (HasThrow)
import Capability.Reader
import Colog hiding (I)
import Control.Exception.Safe
  ( MonadCatch,
    MonadThrow,
    SomeException,
    catch,
    fromException,
    throwM,
    try,
  )
import qualified Control.Monad.Reader as Mtl
import Control.Monad.Trans
import Data.Aeson (eitherDecode)
import Data.Aeson.Extra.SingObject (SingObject (..))
import qualified Data.ByteString.Lazy as BSL
import Data.List (find)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Time hiding
  ( getCurrentTime,
    getZonedTime,
  )
import qualified Data.Time as Time
import Network.HTTP.Types (statusCode)
import Numeric.Natural (Natural)
import qualified Recycle.API as API
import Recycle.Types
import Servant.API
import Servant.Client (ClientError (..), responseBody, responseStatusCode)
import Text.Read (readMaybe)

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
  deriving newtype (Functor, Applicative, Monad, MonadCatch, MonadThrow)

instance MonadTrans RecycleClientT where
  lift = RecycleClientT

instance
  ( Monad m,
    MonadCatch m,
    MonadThrow m,
    HasReader "consumer" Consumer m,
    HasThrow "ApiError" ApiError m,
    API.HasServantClient m,
    Mtl.MonadReader env m,
    HasLog env Message m
  ) =>
  HasRecycleClient (RecycleClientT m)
  where
  searchZipcodes mQ = do
    lift . logInfo $
      "Searching zipcodes: "
        <> maybe "<all>" (T.pack . show . (.unSearchQuery)) mQ
    withRecycleError
      "search zip codes"
      (pure [("query", maybe "<all>" (T.pack . show . (.unSearchQuery)) mQ)])
      ( do
          SingObject zipcodes <- runRecycleOp $ \consumer -> API.searchZipcodes consumer mQ
          pure zipcodes
      )
  searchStreets mZipcode mQ = do
    lift . logInfo $
      "Searching streets in zipcode "
        <> maybe "<all>" (.unZipcodeId) mZipcode
        <> ": "
        <> maybe "<all>" (.unSearchQuery) mQ
    withRecycleError
      "search streets"
      ( do
          mDisplay <- traverse lookupZipcodeDisplay mZipcode
          pure $
            catMaybes
              [ ("zipcode",) <$> mDisplay,
                Just ("query", maybe "<all>" (.unSearchQuery) mQ)
              ]
      )
      ( do
          SingObject streets <- runRecycleOp $ \consumer ->
            API.searchStreets consumer mZipcode mQ
          pure streets
      )
  getCollections zipcode street houseNumber Range {..} = do
    lift . logInfo $
      "Fetching collections for "
        <> zipcode.unZipcodeId
        <> ", "
        <> street.unStreetId
        <> ", "
        <> T.pack (show houseNumber.unHouseNumber)
        <> " from "
        <> T.pack (show from)
        <> " to "
        <> T.pack (show to)
    withRecycleError
      "get collections"
      ( do
          zipcodeDisplay <- lookupZipcodeDisplay zipcode
          pure
            [ ("zipcode", zipcodeDisplay),
              ("street", street.unStreetId),
              ("house_number", T.pack (show houseNumber.unHouseNumber)),
              ("from", T.pack (show from)),
              ("to", T.pack (show to))
            ]
      )
      ( do
          SingObject collections <- runRecycleOp $ \consumer ->
            API.getCollections consumer zipcode street houseNumber from to
          pure collections
      )
  getFractions zipcode street houseNumber = do
    lift . logInfo $
      "Fetching fractions for "
        <> zipcode.unZipcodeId
        <> ", "
        <> street.unStreetId
        <> ", "
        <> T.pack (show houseNumber.unHouseNumber)
    withRecycleError
      "get fractions"
      ( do
          zipcodeDisplay <- lookupZipcodeDisplay zipcode
          pure
            [ ("zipcode", zipcodeDisplay),
              ("house_number", T.pack (show houseNumber.unHouseNumber))
            ]
      )
      ( do
          SingObject fractions <- runRecycleOp $ \consumer ->
            API.getFractions consumer zipcode street houseNumber
          pure fractions
      )

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

-- | Runs @action@; on failure enriches the exception with @operation@ name
-- and context computed by @mkCtx@ (which may make additional API calls).
-- Already-enriched 'RecycleError's pass through unchanged.
withRecycleError ::
  (MonadCatch m, MonadThrow m) =>
  T.Text ->
  m [(T.Text, T.Text)] ->
  m a ->
  m a
withRecycleError op mkCtx action =
  action `catch` \(e :: SomeException) ->
    case fromException @RecycleError e of
      Just _ -> throwM e
      Nothing -> do
        ctx <- mkCtx `catch` \(_ :: SomeException) -> pure []
        throwM RecycleError {operation = op, context = ctx, cause = classifyException e}

-- | Classifies a raw exception into a 'RecycleErrorCause'.
classifyException :: SomeException -> RecycleErrorCause
classifyException e
  | Just ce <- fromException @ClientError e = case ce of
      FailureResponse _ resp ->
        let sc = statusCode (responseStatusCode resp)
         in if sc `elem` [502, 503 :: Int]
              then ServiceUnavailable
              else
                if sc == 400
                  then InvalidRequest (parseBody (responseBody resp))
                  else OtherError ("HTTP " <> T.pack (show sc))
      DecodeFailure msg _ -> DecodeError msg
      ConnectionError _ -> ServiceUnavailable
      _ -> OtherError (T.pack (show ce))
  | Just ae <- fromException @ApiError e =
      if ae.status `elem` [502, 503]
        then ServiceUnavailable
        else
          if ae.status == 400
            then InvalidRequest ae.message
            else OtherError ae.message
  | otherwise = OtherError (T.pack (show e))
  where
    parseBody body =
      either
        (const $ T.pack (show (BSL.take 200 body)))
        (.message)
        (eitherDecode @ApiError body)

-- | Looks up the human-readable display name for a 'ZipcodeId'.
-- Falls back to the raw ID on any failure.
lookupZipcodeDisplay ::
  ( MonadCatch m,
    HasReader "consumer" Consumer m,
    API.HasServantClient m
  ) =>
  ZipcodeId ->
  RecycleClientT m T.Text
lookupZipcodeDisplay zipcodeId@(ZipcodeId t) = do
  consumer <- lift $ ask @"consumer"
  let mCode = readMaybe @Natural . T.unpack $ T.takeWhile (/= '-') t
  case mCode of
    Nothing -> pure t
    Just code -> do
      eResult <-
        lift $
          try $
            API.searchZipcodes consumer (Just (SearchQuery code))
      pure $ case eResult of
        Left (_ :: SomeException) -> t
        Right (Z (I (WithStatus (SingObject zips)))) ->
          case find ((== zipcodeId) . (.id)) zips of
            Nothing -> t
            Just z -> z.code <> " " <> z.city.name
        Right _ -> t

class (Monad m) => HasTime m where
  getCurrentTime :: m UTCTime
  getZonedTime :: m ZonedTime

instance (Monad io, MonadIO io) => HasTime io where
  getZonedTime = liftIO Time.getZonedTime
  getCurrentTime = liftIO Time.getCurrentTime
