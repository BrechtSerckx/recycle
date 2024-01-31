{-# LANGUAGE DataKinds #-}

module Recycle.Utils
  ( LowerCase,
    Union,
    headMay,
    Translated (..),
    module Data.SOP,
  )
where

import qualified Data.Char as Char
import Data.SOP
import qualified Deriving.Aeson as Aeson
import GHC.Generics (Generic)
import Servant.API (Union)

data LowerCase

instance Aeson.StringModifier LowerCase where
  getStringModifier = map Char.toLower

headMay :: [a] -> Maybe a
headMay = \case
  [] -> Nothing
  x : _ -> Just x

data Translated a = Translated
  { nl :: a,
    fr :: a,
    en :: a,
    de :: a
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)
