{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Recycle.Utils
  ( PascalToCamel,
    LangCode,
    Union,
    headMay,
    module Data.SOP,
    module Export,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Data.LanguageCodes as Export
  ( ISO639_1 (..),
  )
import qualified Data.LanguageCodes as LangCode
import Data.SOP
import qualified Data.Text as T
import qualified Deriving.Aeson as Aeson
import GHC.Generics (Generic)
import Servant.API (Union)
import Web.HttpApiData
  ( FromHttpApiData (..),
    parseBoundedTextData,
  )

data PascalToCamel

instance Aeson.StringModifier PascalToCamel where
  getStringModifier = \case
    [] -> []
    (x : xs) -> Char.toLower x : xs

type LangCode = LangCode.ISO639_1

data LowerCase

instance Aeson.StringModifier LowerCase where
  getStringModifier = map Char.toLower

deriving stock instance Generic LangCode

deriving via Aeson.CustomJSON '[Aeson.ConstructorTagModifier LowerCase] LangCode instance Aeson.FromJSON LangCode

deriving via Aeson.CustomJSON '[Aeson.ConstructorTagModifier LowerCase] LangCode instance Aeson.ToJSON LangCode

deriving stock instance Bounded LangCode

instance Aeson.FromJSONKey LangCode where
  fromJSONKey = Aeson.genericFromJSONKey langCodeJSONKeyOptions

instance Aeson.ToJSONKey LangCode where
  toJSONKey = Aeson.genericToJSONKey langCodeJSONKeyOptions

langCodeJSONKeyOptions :: Aeson.JSONKeyOptions
langCodeJSONKeyOptions =
  Aeson.defaultJSONKeyOptions {Aeson.keyModifier = map Char.toLower}

instance FromHttpApiData LangCode where
  parseUrlPiece = parseBoundedTextData . T.toUpper

headMay :: [a] -> Maybe a
headMay = \case
  [] -> Nothing
  x : _ -> Just x
