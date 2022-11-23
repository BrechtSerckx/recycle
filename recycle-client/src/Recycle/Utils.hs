{-# LANGUAGE DataKinds #-}

module Recycle.Utils
  ( PascalToCamel,
    LowerCase,
    Union,
    headMay,
    module Data.SOP,
  )
where

import qualified Data.Char as Char
import Data.SOP
import qualified Deriving.Aeson as Aeson
import Servant.API (Union)

data PascalToCamel

instance Aeson.StringModifier PascalToCamel where
  getStringModifier = \case
    [] -> []
    (x : xs) -> Char.toLower x : xs

data LowerCase

instance Aeson.StringModifier LowerCase where
  getStringModifier = map Char.toLower

headMay :: [a] -> Maybe a
headMay = \case
  [] -> Nothing
  x : _ -> Just x
