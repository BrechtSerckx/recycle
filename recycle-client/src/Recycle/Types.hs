{-# LANGUAGE DataKinds #-}

module Recycle.Types
  ( module Recycle.Types.Geo,
    module Recycle.Types.Error,
    module Recycle.Types.LangCode,
    module Recycle.Utils,
    AccessToken (..),
    AuthResult (..),
    Consumer (..),
    AuthSecret (..),
    SearchQuery (..),
    Range (..),
    Logo (..),
    FullLogo (..),
    RGB (..),
    CollectionEventId (..),
    CollectionEvent (..),
    FractionId (..),
    Fraction (..),
    FullFraction (..),
    Event (..),
    partitionCollectionEvents,
    DateRange (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    (.:),
    (.=),
  )
import qualified Data.Aeson.Types as Aeson
import Data.Foldable
import Data.Map.Strict (Map)
import Data.SOP
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Deriving.Aeson
  ( CustomJSON (CustomJSON),
    FieldLabelModifier,
    StripPrefix,
  )
import GHC.Generics (Generic)
import Recycle.Types.Error
import Recycle.Types.Geo
import Recycle.Types.LangCode
import Recycle.Utils
import Web.HttpApiData
  ( FromHttpApiData,
    ToHttpApiData,
  )

-- | `X-Consumer` header value
newtype Consumer = Consumer Text
  deriving newtype (Eq, Show, IsString, ToHttpApiData)

-- | `X-Secret` header value
newtype AuthSecret = AuthSecret Text
  deriving newtype (Eq, Show, IsString, ToHttpApiData)

-- | Access token
--
-- Used in `Authorization` header value
newtype AccessToken = AccessToken Text
  deriving newtype (Eq, Show, IsString, FromJSON, ToJSON, ToHttpApiData)

-- | Result of authorization
data AuthResult = AuthResult
  { -- | an access token
    authResultAccessToken :: AccessToken,
    -- | an expiry date
    authResultExpiresAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON
          '[FieldLabelModifier (StripPrefix "authResult", PascalToCamel)]
          AuthResult

newtype SearchQuery a = SearchQuery {unSearchQuery :: a}
  deriving newtype (FromHttpApiData, ToHttpApiData)

deriving newtype instance IsString (SearchQuery Text)

data Range a = Range
  { rangeFrom :: a,
    rangeTo :: a
  }
  deriving (Show)

data Logo = Logo
  { logoRegular :: Map Text Text,
    logoReversed :: Map Text Text,
    logoName :: Map Text Text,
    logoId :: Text
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON
          '[FieldLabelModifier (StripPrefix "logo", PascalToCamel)]
          Logo

data FullLogo = FullLogo
  { fullLogoRegular :: Map Text Text,
    fullLogoReversed :: Map Text Text,
    fullLogoName :: Map Text Text,
    fullLogoId :: Text,
    fullLogoCreatedAt :: UTCTime,
    fullLogoUpdatedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON
          '[FieldLabelModifier (StripPrefix "fullLogo", PascalToCamel)]
          FullLogo

newtype RGB = RGB Text deriving newtype (Show, FromJSON, ToJSON)

-- * Collection

newtype CollectionEventId = CollectionEventId {unCollectionEventId :: Text}
  deriving newtype (Show, FromJSON, ToJSON)

data CollectionEvent a = CollectionEvent
  { collectionEventId :: CollectionEventId,
    collectionEventTimestamp :: UTCTime,
    collectionEventContent :: a
  }
  deriving stock (Generic, Show)

instance FromJSON (CollectionEvent (Union '[FullFraction, Event])) where
  parseJSON = Aeson.withObject "CollectionEvent" $ \o -> do
    collectionEventId <- o .: "id"
    collectionEventTimestamp <- o .: "timestamp"
    collectionEventContent <-
      (o .: "type" :: Aeson.Parser Text) >>= \case
        "collection" -> Z . I <$> o .: "fraction"
        "event" -> S . Z . I <$> o .: "event"
        _ -> fail "unknown type"
    pure CollectionEvent {..}

instance ToJSON (CollectionEvent (Union '[FullFraction, Event])) where
  toJSON CollectionEvent {..} =
    Aeson.object $
      ["id" .= collectionEventId, "timestamp" .= collectionEventTimestamp]
        <> case collectionEventContent of
          Z (I f) -> [("type", "collection"), "fraction" .= f]
          S (Z (I e)) -> [("type", "collection"), "event" .= e]
          S (S x) -> case x of {}

newtype FractionId = FractionId Text
  deriving newtype (Show, FromJSON, ToJSON, FromHttpApiData)

partitionCollectionEvents ::
  [CollectionEvent (Union '[FullFraction, Event])] ->
  ([CollectionEvent FullFraction], [CollectionEvent Event])
partitionCollectionEvents ces =
  let go ::
        ([CollectionEvent FullFraction], [CollectionEvent Event]) ->
        CollectionEvent (Union '[FullFraction, Event]) ->
        ([CollectionEvent FullFraction], [CollectionEvent Event])
      go (fs, es) ce = case collectionEventContent ce of
        Z (I f) -> (ce {collectionEventContent = f} : fs, es)
        S (Z (I e)) -> (fs, ce {collectionEventContent = e} : es)
        S (S x) -> case x of {}
   in foldl' go ([], []) ces

data Fraction = Fraction
  { fractionId :: FractionId,
    fractionName :: Map LangCode Text,
    fractionLogo :: Logo,
    fractionColor :: RGB,
    fractionVariations :: [Aeson.Value]
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON
          '[FieldLabelModifier (StripPrefix "fraction", PascalToCamel)]
          Fraction

data FullFraction = FullFraction
  { fullFractionId :: FractionId,
    fullFractionNational :: Bool,
    fullFractionNationalRef :: Maybe Text,
    fullFractionDatatankRef :: Maybe Text,
    fullFractionName :: Map LangCode Text,
    fullFractionLogo :: FullLogo,
    fullFractionColor :: RGB,
    fullFractionVariations :: (),
    fullFractionOrganisation :: Text,
    fullFractionCreatedAt :: UTCTime,
    fullFractionUpdatedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON
          '[FieldLabelModifier (StripPrefix "fullFraction", PascalToCamel)]
          FullFraction

-- * Event

data Event = Event
  { eventTitle :: Map LangCode Text,
    eventIntroduction :: Map LangCode Text,
    eventDescription :: Map LangCode Text,
    eventExternalLink :: Map LangCode Text
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON
          '[FieldLabelModifier (StripPrefix "event", PascalToCamel)]
          Event

-- * DateRange

data DateRange = AbsoluteDateRange (Range Day) | RelativeDateRange (Range Integer)
