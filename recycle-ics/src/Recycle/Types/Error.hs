{-# LANGUAGE DataKinds #-}

module Recycle.Types.Error
  ( ApiError (..),
  )
where

import Control.Exception (Exception)
import qualified Data.Aeson.Extra.SingObject as Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Deriving.Aeson as Aeson
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Recycle.Utils (PascalToCamel)

data ApiError = ApiError
  { apiErrorHost :: Text,
    apiErrorIdentifier :: Text,
    apiErrorTimestamp :: UTCTime,
    apiErrorStatus :: Natural,
    apiErrorName :: Text,
    apiErrorMessage :: Text,
    apiErrorDetails :: Maybe [Aeson.SingObject "err" Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (Exception)
  deriving
    (Aeson.FromJSON)
    via Aeson.CustomJSON
          '[Aeson.FieldLabelModifier (Aeson.StripPrefix "apiError", PascalToCamel)]
          ApiError
