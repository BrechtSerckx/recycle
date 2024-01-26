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
import GHC.Generics (Generic)
import Recycle.Types.Error
import Recycle.Types.Geo
import Recycle.Types.LangCode
import Recycle.Utils
import Web.HttpApiData
  ( FromHttpApiData,
    ToHttpApiData,
  )
import Prelude hiding (id)

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
    accessToken :: AccessToken,
    -- | an expiry date
    expiresAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype SearchQuery a = SearchQuery {unSearchQuery :: a}
  deriving newtype (FromHttpApiData, ToHttpApiData)

deriving newtype instance IsString (SearchQuery Text)

data Range a = Range
  { from :: a,
    to :: a
  }
  deriving (Show)

data Logo = Logo
  { regular :: Map Text Text,
    reversed :: Map Text Text,
    name :: Map Text Text,
    id :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data FullLogo = FullLogo
  { regular :: Map Text Text,
    reversed :: Map Text Text,
    name :: Map Text Text,
    id :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype RGB = RGB Text deriving newtype (Show, FromJSON, ToJSON)

-- * Collection

newtype CollectionEventId = CollectionEventId {unCollectionEventId :: Text}
  deriving newtype (Show, FromJSON, ToJSON)

data CollectionEvent a = CollectionEvent
  { id :: CollectionEventId,
    timestamp :: UTCTime,
    content :: a
  }
  deriving stock (Generic, Show)

instance FromJSON (CollectionEvent (Union '[FullFraction, Event])) where
  parseJSON = Aeson.withObject "CollectionEvent" $ \o -> do
    id <- o .: "id"
    timestamp <- o .: "timestamp"
    content <-
      (o .: "type" :: Aeson.Parser Text) >>= \case
        "collection" -> Z . I <$> o .: "fraction"
        "event" -> S . Z . I <$> o .: "event"
        _ -> fail "unknown type"
    pure CollectionEvent {..}

instance ToJSON (CollectionEvent (Union '[FullFraction, Event])) where
  toJSON CollectionEvent {..} =
    Aeson.object $
      ["id" .= id, "timestamp" .= timestamp]
        <> case content of
          Z (I f) -> [("type", "collection"), "fraction" .= f]
          S (Z (I e)) -> [("type", "collection"), "event" .= e]
          S (S x) -> case x of {}

newtype FractionId = FractionId Text
  deriving newtype (Show, Eq, IsString, FromJSON, ToJSON, FromHttpApiData)

partitionCollectionEvents ::
  [CollectionEvent (Union '[FullFraction, Event])] ->
  ([CollectionEvent FullFraction], [CollectionEvent Event])
partitionCollectionEvents ces =
  let go ::
        ([CollectionEvent FullFraction], [CollectionEvent Event]) ->
        CollectionEvent (Union '[FullFraction, Event]) ->
        ([CollectionEvent FullFraction], [CollectionEvent Event])
      go (fs, es) ce = case ce.content of
        Z (I f) -> (ce {content = f} : fs, es)
        S (Z (I e)) -> (fs, ce {content = e} : es)
        S (S x) -> case x of {}
   in foldl' go ([], []) ces

data Fraction = Fraction
  { id :: FractionId,
    name :: Map LangCode Text,
    logo :: Logo,
    color :: RGB,
    variations :: [Aeson.Value]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data FullFraction = FullFraction
  { id :: FractionId,
    national :: Bool,
    nationalRef :: Maybe Text,
    datatankRef :: Maybe Text,
    name :: Map LangCode Text,
    logo :: FullLogo,
    color :: RGB,
    variations :: (),
    organisation :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass
    (FromJSON, ToJSON)

-- * Event

data Event = Event
  { title :: Map LangCode Text,
    introduction :: Map LangCode Text,
    description :: Map LangCode Text,
    externalLink :: Map LangCode Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- * DateRange

data DateRange = AbsoluteDateRange (Range Day) | RelativeDateRange (Range Integer)
