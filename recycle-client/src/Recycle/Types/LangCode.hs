{-# LANGUAGE DataKinds #-}

module Recycle.Types.LangCode (LangCode (..)) where

import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Deriving.Aeson as Aeson
import GHC.Generics (Generic)
import Recycle.Utils (LowerCase)
import Web.HttpApiData
  ( FromHttpApiData (..),
    parseBoundedTextData,
  )

data LangCode = EN | NL | FR | DE
  deriving stock (Generic, Show, Read, Eq, Ord, Bounded, Enum)
  deriving
    (Aeson.FromJSON, Aeson.ToJSON)
    via Aeson.CustomJSON '[Aeson.ConstructorTagModifier LowerCase] LangCode

instance Aeson.FromJSONKey LangCode where
  fromJSONKey = Aeson.genericFromJSONKey langCodeJSONKeyOptions

instance Aeson.ToJSONKey LangCode where
  toJSONKey = Aeson.genericToJSONKey langCodeJSONKeyOptions

langCodeJSONKeyOptions :: Aeson.JSONKeyOptions
langCodeJSONKeyOptions =
  Aeson.defaultJSONKeyOptions {Aeson.keyModifier = map Char.toLower}

instance FromHttpApiData LangCode where
  parseUrlPiece = parseBoundedTextData . T.toUpper
