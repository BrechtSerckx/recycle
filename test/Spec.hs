{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module Main where

import           Data.Aeson
import           Data.Aeson.Extra.SingObject
import qualified Data.ByteString.Lazy          as BSL
import           Data.Either
import           Data.FileEmbed
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )

import           Test.Hspec
import           Test.Hspec.QuickCheck
import qualified Test.QuickCheck               as QC

import           Recycle.API
import           Recycle.Types
import           Recycle.Utils

main :: IO ()
main = hspec spec

spec = describe "API responses" $ do
  it "parses a normal `AuthResult` response"
    $               eitherDecode @AuthResult
                      (BSL.fromStrict $(embedFile "test/responses/authResult.json"))
    `shouldSatisfy` isRight

  it "parses translations"
    $          eitherDecode @(Map.Map LangCode Text)
                 "{ \"en\": \"english\", \"nl\": \"nederlands\"}"
    `shouldBe` Right (Map.fromList [(EN, "english"), (NL, "nederlands")])

  it "parses a normal `Zipcodes` response"
    $               eitherDecode @(SingObject "items" [FullZipcode])
                      (BSL.fromStrict $(embedFile "test/responses/zipcodes.json"))
    `shouldSatisfy` isRight

  it "parses a normal `Streets` response"
    $               eitherDecode @(SingObject "items" [Street])
                      (BSL.fromStrict $(embedFile "test/responses/streets.json"))
    `shouldSatisfy` isRight
