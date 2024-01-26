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
      `shouldBe` Right
        ( SingObject
            [ Street
                { id = "https://data.vlaanderen.be/id/straatnaam-34637",
                  city =
                    [ City
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
                        }
                    ],
                  createdAt = read "2020-09-22 08:19:32.787 UTC",
                  updatedAt = read "2021-10-11 07:06:32.64 UTC",
                  names =
                    Map.fromList
                      [ ("de", "Andreas Vesaliusstraat"),
                        ("en", "Andreas Vesaliusstraat"),
                        ("fr", "Andreas Vesaliusstraat"),
                        ("nl", "Andreas Vesaliusstraat")
                      ],
                  name = "Andreas Vesaliusstraat",
                  deleted = False,
                  zipcode =
                    [ Zipcode
                        { city = "24062",
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
                            ]
                        }
                    ]
                }
            ]
        )

  it "parses a normal `Collections` response" $
    eitherDecode
      @(SingObject "items" [CollectionEvent (Union '[FullFraction, Event])])
      (BSL.fromStrict $(embedFile "test/responses/collections.json"))
      `shouldSatisfy` isRight

  it "parses a normal `Collections` fraction response" $
    eitherDecode
      @(SingObject "items" [CollectionEvent (Union '[FullFraction, Event])])
      (BSL.fromStrict $(embedFile "test/responses/collection-fraction.json"))
      `shouldBe` ( Right . SingObject . List.singleton $
                     CollectionEvent
                       { id = "5fed8c315214976fcf25764d",
                         timestamp = read "2021-09-15 00:00:00 UTC",
                         content =
                           Z $
                             I
                               FullFraction
                                 { id = "5e4e84d1bab65e9819d714d2",
                                   national = True,
                                   nationalRef = Just "5d610b87173c063cc0400103",
                                   datatankRef = Nothing,
                                   name =
                                     Map.fromList
                                       [ (EN, "PMD"),
                                         (NL, "PMD"),
                                         (FR, "PMC"),
                                         (DE, "PMD")
                                       ],
                                   logo =
                                     FullLogo
                                       { regular =
                                           Map.fromList
                                             [ ("1x", "5ef36735da06b266d294b3b9"),
                                               ("2x", "5ef36735da06b298a894b3ba"),
                                               ("3x", "5ef36735da06b23db894b3bb")
                                             ],
                                         reversed =
                                           Map.fromList
                                             [ ("1x", "5ef36735da06b25d5e94b3bc"),
                                               ("2x", "5ef36735da06b2a1a594b3bd"),
                                               ("3x", "5ef36735da06b2093194b3be")
                                             ],
                                         name =
                                           Map.fromList
                                             [ ("de", "PMD"),
                                               ("en", "PMD"),
                                               ("fr", "PMC"),
                                               ("nl", "PMD")
                                             ],
                                         id = "5d610b86162c063cc0400125",
                                         createdAt = read "2020-02-20 13:08:25.556 UTC",
                                         updatedAt = read "2020-06-24 14:46:14.194 UTC"
                                       },
                                   color = RGB "#60b1df",
                                   variations = (),
                                   organisation = "5e27908010cfeaa15c9cee93",
                                   createdAt = read "2020-02-20 13:08:32.883 UTC",
                                   updatedAt = read "2021-09-30 15:16:36.595 UTC"
                                 }
                       }
                 )

  it "parses a normal `Fractions` response" $
    eitherDecode @(SingObject "items" [Fraction])
      (BSL.fromStrict $(embedFile "test/responses/fractions.json"))
      `shouldSatisfy` isRight
