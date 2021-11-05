{-# LANGUAGE DataKinds #-}
module Recycle.Types
  ( module Recycle.Types.Geo
  , module Recycle.Types.Error
  , module Recycle.Utils
  , AccessToken(..)
  , AuthResult(..)
  , Consumer(..)
  , AuthSecret(..)
  , SearchQuery(..)
  )
where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.String                    ( IsString )
import           Data.Text                      ( Text )
import           Data.Time
import           Deriving.Aeson                 ( CustomJSON(..)
                                                , FieldLabelModifier
                                                , StripPrefix
                                                )
import           GHC.Generics                   ( Generic )
import           Web.HttpApiData                ( FromHttpApiData
                                                , ToHttpApiData
                                                )

import           Recycle.Types.Error
import           Recycle.Types.Geo
import           Recycle.Utils

newtype SearchQuery = SearchQuery Text
  deriving newtype (FromHttpApiData, ToHttpApiData, IsString)

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
  { authResultAccessToken :: AccessToken -- ^ an access token
  , authResultExpiresAt   :: UTCTime -- ^ an expiry date
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via CustomJSON
    '[FieldLabelModifier (StripPrefix "authResult", PascalToCamel)]
    AuthResult
