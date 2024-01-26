{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Aeson
import Data.Aeson.Extra.SingObject
import qualified Data.ByteString.Lazy as BSL
import Data.Either
import Data.FileEmbed
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time (UTCTime (..))
import qualified Data.Time as Time
import Recycle.API
import Recycle.Types
import Recycle.Utils
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck as QC

main :: IO ()
main = hspec spec

spec = describe "API responses" $ do
  it "parses a normal `AuthResult` response" $
    eitherDecode @AuthResult
      (BSL.fromStrict $(embedFile "test/responses/authResult.json"))
      `shouldBe` Right
        ( AuthResult
            { expiresAt = read "2021-10-15 09:38:17.553 UTC",
              accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpYXQiOjE2MzQyODcxNTcsImV4cCI6MTYzNDI5MDc1NywiYXVkIjoicmVjeWNsZWFwcC5iZSJ9.M71tok8T0dOms_pISu_pLzGMpH84iNtOraJ5-PI1Ktk"
            }
        )

  it "parses translations" $
    eitherDecode @(Map.Map LangCode Text)
      "{ \"en\": \"english\", \"nl\": \"nederlands\"}"
      `shouldBe` Right (Map.fromList [(EN, "english"), (NL, "nederlands")])

  it "parses a normal `Zipcodes` response" $
    eitherDecode @(SingObject "items" [FullZipcode])
      (BSL.fromStrict $(embedFile "test/responses/zipcodes.json"))
      `shouldBe` Right
        ( SingObject
            [ FullZipcode
                { city =
                    City
                      { id = "24062",
                        zipcodes =
                          [ "3000-24062",
                            "3001-24062",
                            "3010-24062",
                            "3012-24062",
                            "3018-24062"
                          ],
                        name = "Leuven",
                        createdAt = read "2020-07-13 22:00:01.034 UTC",
                        updatedAt = read "2020-09-21 22:15:05.605 UTC",
                        names =
                          Map.fromList
                            [ ("de", "L\246wen"),
                              ("en", "Leuven"),
                              ("fr", "Louvain"),
                              ("nl", "Leuven")
                            ]
                      },
                  code = "3000",
                  createdAt = read "2020-07-13 22:00:01.034 UTC",
                  updatedAt = read "2020-09-21 22:15:05.604 UTC",
                  id = "3000-24062",
                  names =
                    [ Map.fromList
                        [ ("de", "L\246wen"),
                          ("en", "Leuven"),
                          ("fr", "Louvain"),
                          ("nl", "Leuven")
                        ]
                    ],
                  available = True
                }
            ]
        )

  it "parses a normal `Streets` response" $
    eitherDecode @(SingObject "items" [Street])
      (BSL.fromStrict $(embedFile "test/responses/streets.json"))
      `shouldSatisfy` isRight

  it "parses a normal `Collections` response" $
    eitherDecode
      @(SingObject "items" [CollectionEvent (Union '[FullFraction, Event])])
      (BSL.fromStrict $(embedFile "test/responses/collections.json"))
      `shouldSatisfy` isRight

  it "parses a normal `Fractions` response" $
    eitherDecode @(SingObject "items" [Fraction])
      (BSL.fromStrict $(embedFile "test/responses/fractions.json"))
      `shouldSatisfy` isRight
