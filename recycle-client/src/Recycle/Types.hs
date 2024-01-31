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
    FractionCollection (..),
    Event (..),
    InnerEvent (..),
    partitionCollectionEvents,
    DateRange (..),
  )
where

import Control.Applicative ((<|>))
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
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
  deriving stock (Generic, Show, Eq)
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
    name :: Translated Text,
    id :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data FullLogo = FullLogo
  { regular :: Map Text Text,
    reversed :: Map Text Text,
    name :: Translated Text,
    id :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

newtype RGB = RGB Text deriving newtype (Show, FromJSON, ToJSON, Eq)

-- * Collection

newtype CollectionEventId = CollectionEventId {unCollectionEventId :: Text}
  deriving newtype (Show, FromJSON, ToJSON, Eq, IsString)

newtype CollectionEvent = CollectionEvent
  { unCollectionEvent :: Union '[FractionCollection, Event]
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON CollectionEvent where
  parseJSON v =
    fmap CollectionEvent $
      (Z . I <$> parseJSON v)
        <|> (S . Z . I <$> parseJSON v)

instance ToJSON CollectionEvent where
  toJSON (CollectionEvent ce) = case ce of
    Z (I f) -> Aeson.toJSON f
    S (Z (I e)) -> Aeson.toJSON e
    S (S x) -> case x of {}

newtype FractionId = FractionId Text
  deriving newtype (Show, Eq, IsString, FromJSON, ToJSON, FromHttpApiData)

partitionCollectionEvents ::
  [CollectionEvent] ->
  ([FractionCollection], [Event])
partitionCollectionEvents ces =
  let go ::
        ([FractionCollection], [Event]) ->
        CollectionEvent ->
        ([FractionCollection], [Event])
      go (fs, es) (CollectionEvent ce) = case ce of
        Z (I f) -> (f : fs, es)
        S (Z (I e)) -> (fs, e : es)
        S (S x) -> case x of {}
   in foldl' go ([], []) ces

data Fraction = Fraction
  { id :: FractionId,
    name :: Translated Text,
    logo :: Logo,
    color :: RGB,
    variations :: [Aeson.Value]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data FullFraction = FullFraction
  { id :: FractionId,
    national :: Bool,
    nationalRef :: Maybe Text,
    datatankRef :: Maybe Text,
    name :: Translated Text,
    logo :: FullLogo,
    color :: RGB,
    variations :: (),
    organisation :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data FractionCollection = FractionCollection
  { id :: CollectionEventId,
    timestamp :: UTCTime,
    fraction :: FullFraction
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- * Event

data Event = Event
  { id :: CollectionEventId,
    timestamp :: UTCTime,
    event :: InnerEvent
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data InnerEvent = InnerEvent
  { title :: Translated Text,
    introduction :: Translated Text,
    description :: Translated Text,
    externalLink :: Translated Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- * DateRange

data DateRange = AbsoluteDateRange (Range Day) | RelativeDateRange (Range Integer)
