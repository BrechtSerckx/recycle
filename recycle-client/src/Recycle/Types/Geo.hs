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
import Data.Map.Strict (Map)
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Web.HttpApiData
  ( FromHttpApiData,
    ToHttpApiData,
  )

-- * City

newtype CityId = CityId Text
  deriving newtype (Show, IsString, ToHttpApiData, FromJSON, ToJSON)

data City = City
  { id :: CityId,
    zipcodes :: [ZipcodeId],
    name :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    names :: Map Text Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- * Zip code

newtype ZipcodeId = ZipcodeId {unZipcodeId :: Text}
  deriving newtype (Show, IsString, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

data Zipcode = Zipcode
  { city :: CityId,
    code :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    id :: ZipcodeId,
    names :: [Map Text Text]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data FullZipcode = FullZipcode
  { city :: City,
    code :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    id :: ZipcodeId,
    names :: [Map Text Text],
    available :: Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- * Street

newtype StreetId = StreetId {unStreetId :: Text}
  deriving newtype (Show, IsString, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

data Street = Street
  { id :: StreetId,
    city :: [City],
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    names :: Map Text Text,
    name :: Text,
    deleted :: Bool,
    zipcode :: [Zipcode]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- * House number

newtype HouseNumber = HouseNumber {unHouseNumber :: Natural}
  deriving newtype (Show, Read, FromHttpApiData, ToHttpApiData)
