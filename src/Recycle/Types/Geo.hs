{-# LANGUAGE DataKinds #-}
module Recycle.Types.Geo
  ( CityId(..)
  , City(..)
  , ZipcodeId(..)
  , FullZipcode(..)
  ) where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Map.Strict                ( Map )
import           Data.String                    ( IsString )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           Deriving.Aeson                 ( CustomJSON(CustomJSON)
                                                , FieldLabelModifier
                                                , StripPrefix
                                                )
import           GHC.Generics                   ( Generic )
import           Web.HttpApiData                ( FromHttpApiData
                                                , ToHttpApiData
                                                )

import           Recycle.Utils                  ( PascalToCamel )

-- * City

newtype CityId = CityId Text
  deriving newtype (Show, IsString, ToHttpApiData, FromJSON, ToJSON)

data City = City
  { cityId        :: CityId
  , cityZipcodes  :: [ZipcodeId]
  , cityName      :: Text
  , cityCreatedAt :: UTCTime
  , cityUpdatedAt :: UTCTime
  , cityNames     :: Map Text Text
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via CustomJSON
    '[FieldLabelModifier (StripPrefix "city", PascalToCamel)]
    City

-- * Zip code

newtype ZipcodeId = ZipcodeId { unZipcodeId :: Text}
  deriving newtype (Show, IsString, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

data FullZipcode = FullZipcode
  { fullZipcodeCity      :: City
  , fullZipcodeCode      :: Text
  , fullZipcodeCreatedAt :: UTCTime
  , fullZipcodeUpdatedAt :: UTCTime
  , fullZipcodeId        :: ZipcodeId
  , fullZipcodeNames     :: [Map Text Text]
  , fullZipcodeAvailable :: Bool
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via CustomJSON
    '[FieldLabelModifier (StripPrefix "fullZipcode", PascalToCamel)]
    FullZipcode
