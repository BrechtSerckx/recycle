{-# LANGUAGE DataKinds #-}

module Recycle.Types.Error
  ( ApiError (..),
  )
where

import Control.Exception (Exception)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Extra.SingObject as Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import GHC.Natural (Natural)

data ApiError = ApiError
  { host :: Text,
    identifier :: Text,
    timestamp :: UTCTime,
    status :: Natural,
    name :: Text,
    message :: Text,
    details :: Maybe [Aeson.SingObject "err" Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (Exception, Aeson.FromJSON)
