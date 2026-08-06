{-# LANGUAGE DataKinds #-}

module Recycle.Types.Error
  ( ApiError (..),
    RecycleError (..),
    RecycleErrorCause (..),
  )
where

import Control.Exception (Exception)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Extra.SingObject as Aeson
import Data.Text (Text)
import qualified Data.Text as T
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

-- | Categorises why a Recycle API operation failed.
data RecycleErrorCause
  = -- | HTTP 502\/503 or connection failure — fostplus API is down.
    ServiceUnavailable
  | -- | HTTP 400 — the query (zipcode, street, house number) is not supported.
    InvalidRequest Text
  | -- | JSON decode failure — indicates a bug in this implementation.
    DecodeError Text
  | -- | Any other unexpected error.
    OtherError Text
  deriving stock (Show, Eq)

data RecycleError = RecycleError
  { operation :: Text,
    context :: [(Text, Text)],
    cause :: RecycleErrorCause
  }

instance Show RecycleError where
  show RecycleError {operation, context, cause} =
    T.unpack $
      "Failed to " <> operation
        <> if null context
          then ""
          else " [" <> T.intercalate ", " [k <> ": " <> v | (k, v) <- context] <> "]"
        <> ": "
        <> case cause of
          ServiceUnavailable -> "service temporarily unavailable"
          InvalidRequest msg -> "invalid request — " <> msg
          DecodeError msg -> "unexpected response format, please report this bug — " <> msg
          OtherError msg -> msg

instance Exception RecycleError
