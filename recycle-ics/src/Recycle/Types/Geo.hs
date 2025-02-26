{-# LANGUAGE DataKinds #-}

module Recycle.Types.Geo
  ( CityId (..),
    City (..),
    ZipcodeId (..),
    Zipcode (..),
    FullZipcode (..),
    StreetId (..),
    Street (..),
    HouseNumber (..),
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Recycle.Utils (Translated)
import Web.HttpApiData
  ( FromHttpApiData,
    ToHttpApiData,
  )

-- * City

newtype CityId = CityId Text
  deriving newtype (Show, IsString, ToHttpApiData, FromJSON, ToJSON, Eq)

data City = City
  { id :: CityId,
    zipcodes :: [ZipcodeId],
    name :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    names :: Translated Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- * Zip code

newtype ZipcodeId = ZipcodeId {unZipcodeId :: Text}
  deriving newtype (Show, IsString, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON, Eq)

data Zipcode = Zipcode
  { city :: CityId,
    code :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    id :: ZipcodeId,
    names :: [Translated Text]
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON, ToJSON)

data FullZipcode = FullZipcode
  { city :: City,
    code :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    id :: ZipcodeId,
    names :: [Translated Text],
    available :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- * Street

newtype StreetId = StreetId {unStreetId :: Text}
  deriving newtype (Show, IsString, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON, Eq)

data Street = Street
  { id :: StreetId,
    city :: [City],
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    names :: Translated Text,
    name :: Text,
    deleted :: Bool,
    zipcode :: [Zipcode]
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- * House number

newtype HouseNumber = HouseNumber {unHouseNumber :: Natural}
  deriving newtype (Show, Read, FromHttpApiData, ToHttpApiData)
