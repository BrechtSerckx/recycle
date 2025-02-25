{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Aeson
import Data.Aeson.Extra.SingObject
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Either
import Data.FileEmbed
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time (UTCTime (..))
import qualified Data.Time as Time
import qualified Data.Vector as V
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
            { expiresAt = read @UTCTime "2021-10-15 09:38:17.553 UTC",
              accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpYXQiOjE2MzQyODcxNTcsImV4cCI6MTYzNDI5MDc1NywiYXVkIjoicmVjeWNsZWFwcC5iZSJ9.M71tok8T0dOms_pISu_pLzGMpH84iNtOraJ5-PI1Ktk"
            }
        )

  it "parses translations" $
    eitherDecode @(Translated Text)
      "{ \"en\": \"english\", \"nl\": \"nederlands\", \"fr\": \"francais\", \"de\": \"deutsch\"}"
      `shouldBe` Right
        ( Translated
            { en = "english",
              nl = "nederlands",
              fr = "francais",
              de = "deutsch"
            }
        )

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
                        createdAt = read @UTCTime "2020-07-13 22:00:01.034 UTC",
                        updatedAt = read @UTCTime "2020-09-21 22:15:05.605 UTC",
                        names =
                          Translated
                            { de = "L\246wen",
                              en = "Leuven",
                              fr = "Louvain",
                              nl = "Leuven"
                            }
                      },
                  code = "3000",
                  createdAt = read @UTCTime "2020-07-13 22:00:01.034 UTC",
                  updatedAt = read @UTCTime "2020-09-21 22:15:05.604 UTC",
                  id = "3000-24062",
                  names =
                    [ Translated
                        { de = "L\246wen",
                          en = "Leuven",
                          fr = "Louvain",
                          nl = "Leuven"
                        }
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
                          createdAt = read @UTCTime "2020-07-13 22:00:01.034 UTC",
                          updatedAt = read @UTCTime "2020-09-21 22:15:05.605 UTC",
                          names =
                            Translated
                              { de = "L\246wen",
                                en = "Leuven",
                                fr = "Louvain",
                                nl = "Leuven"
                              }
                        }
                    ],
                  createdAt = read @UTCTime "2020-09-22 08:19:32.787 UTC",
                  updatedAt = read @UTCTime "2021-10-11 07:06:32.64 UTC",
                  names =
                    Translated
                      { de = "Andreas Vesaliusstraat",
                        en = "Andreas Vesaliusstraat",
                        fr = "Andreas Vesaliusstraat",
                        nl = "Andreas Vesaliusstraat"
                      },
                  name = "Andreas Vesaliusstraat",
                  deleted = False,
                  zipcode =
                    [ Zipcode
                        { city = "24062",
                          code = "3000",
                          createdAt = read @UTCTime "2020-07-13 22:00:01.034 UTC",
                          updatedAt = read @UTCTime "2020-09-21 22:15:05.604 UTC",
                          id = "3000-24062",
                          names =
                            [ Translated
                                { de = "L\246wen",
                                  en = "Leuven",
                                  fr = "Louvain",
                                  nl = "Leuven"
                                }
                            ]
                        }
                    ]
                }
            ]
        )

  it "parses a normal `Collections` response" $
    eitherDecode
      @(SingObject "items" [CollectionEvent])
      (BSL.fromStrict $(embedFile "test/responses/collections.json"))
      `shouldSatisfy` isRight

  it "parses a normal `Collections` fraction response" $
    eitherDecode
      @(SingObject "items" [FractionCollection])
      (BSL.fromStrict $(embedFile "test/responses/collection-fraction.json"))
      `shouldBe` Right
        ( SingObject
            [ FractionCollection
                { id = "5fed8c315214976fcf25764d",
                  timestamp = read @UTCTime "2021-09-15 00:00:00 UTC",
                  fraction =
                    FullFraction
                      { id = "5e4e84d1bab65e9819d714d2",
                        national = Just True,
                        nationalRef = Just "5d610b87173c063cc0400103",
                        datatankRef = Nothing,
                        name = Translated {en = "PMD", nl = "PMD", fr = "PMC", de = "PMD"},
                        organisation = "5e27908010cfeaa15c9cee93",
                        createdAt = Just $ read @UTCTime "2020-02-20 13:08:32.883 UTC",
                        updatedAt = Just $ read @UTCTime "2021-09-30 15:16:36.595 UTC"
                      },
                  exception = Nothing
                }
            ]
        )

  it "parses a normal `Collections` event response" $
    eitherDecode
      @(SingObject "items" [Event])
      (BSL.fromStrict $(embedFile "test/responses/collection-event.json"))
      `shouldBe` Right
        ( SingObject
            [ Event
                { id = "5fed94cf5214974871257660",
                  timestamp = read @UTCTime "2021-09-18 00:00:00 UTC",
                  event =
                    InnerEvent
                      { title = Translated {en = "Repair Caf\233", nl = "Repair Caf\233", fr = "Repair Caf\233", de = "Repair Caf\233"},
                        introduction = Translated {en = "Repair Caf\233 in MAAKbar", nl = "Repair Caf\233 in MAAKbar", fr = "Repair Caf\233 in MAAKbar", de = "Repair Caf\233 in MAAKbar"},
                        description = Translated {en = "Elke derde zaterdag van de maand tussen 14.00 en 17.00 uur in de Diestsestraat 142 in Leuven.", nl = "Elke derde zaterdag van de maand tussen 14.00 en 17.00 uur in de Diestsestraat 142 in Leuven.", fr = "Elke derde zaterdag van de maand tussen 14.00 en 17.00 uur in de Diestsestraat 142 in Leuven.", de = "Elke derde zaterdag van de maand tussen 14.00 en 17.00 uur in de Diestsestraat 142 in Leuven."},
                        externalLink = Translated {en = "https://www.maakbaarleuven.be/", nl = "https://www.maakbaarleuven.be/", fr = "https://www.maakbaarleuven.be/", de = "https://www.maakbaarleuven.be/"}
                      }
                }
            ]
        )

  it "parses a normal `Collections` fraction response with exceptions" $
    eitherDecode
      @(SingObject "items" [FractionCollection])
      (BSL.fromStrict $(embedFile "test/responses/collection-fraction-exception.json"))
      `shouldBe` Right
        ( SingObject
            [ FractionCollection
                { id = "658eb5838f94c6007edfa96e",
                  timestamp = read @UTCTime "2024-01-01 00:00:00 UTC",
                  fraction =
                    FullFraction
                      { id = "5e4e84d1bab65e9819d714d2",
                        national = Just True,
                        nationalRef = Just "5d610b87173c063cc0400103",
                        datatankRef = Nothing,
                        name = Translated {en = "PMD", nl = "PMD", fr = "PMC", de = "PMK"},
                        organisation = "5e27908010cfeaa15c9cee93",
                        createdAt = Just $ read @UTCTime "2020-02-20 13:08:32.883 UTC",
                        updatedAt = Just $ read @UTCTime "2024-01-24 13:24:35.213 UTC"
                      },
                  exception =
                    Just
                      ( ReplacedBy
                          ( ExceptionalFractionCollection
                              { isDeleted = Just False,
                                group = "5ed75ad48e14e76fe3c5febc",
                                organisation = "5e27908010cfeaa15c9cee93",
                                createdAt = read @UTCTime "2023-12-29 12:03:32.838 UTC",
                                updatedAt = read @UTCTime "2023-12-29 12:03:32.838 UTC",
                                fraction = "5e4e84d1bab65e9819d714d2",
                                timestamp = read @UTCTime "2024-01-04 00:00:00 UTC",
                                id = "658eb594262b65007e5accec",
                                exception =
                                  InnerExceptionReplaces
                                    { replaces = "658eb5838f94c6007edfa96e",
                                      reason =
                                        CollectionReplacementReason
                                          { id = "5d610b87162c063cc0400102",
                                            createdAt = read @UTCTime "2020-02-03 11:06:44.291 UTC",
                                            updatedAt = read @UTCTime "2020-03-03 16:17:29.678 UTC",
                                            name = Translated {en = "Public holiday", nl = "Feestdag", fr = "Jour f\233ri\233", de = "Feiertag"}
                                          }
                                    }
                              }
                          )
                      )
                },
              FractionCollection
                { id = "658eb594262b65007e5accec",
                  timestamp = read @UTCTime "2024-01-04 00:00:00 UTC",
                  fraction =
                    FullFraction
                      { id = "5e4e84d1bab65e9819d714d2",
                        national = Just True,
                        nationalRef = Just "5d610b87173c063cc0400103",
                        datatankRef = Nothing,
                        name = Translated {en = "PMD", nl = "PMD", fr = "PMC", de = "PMK"},
                        organisation = "5e27908010cfeaa15c9cee93",
                        createdAt = Just $ read @UTCTime "2020-02-20 13:08:32.883 UTC",
                        updatedAt = Just $ read @UTCTime "2024-01-24 13:24:35.213 UTC"
                      },
                  exception =
                    Just
                      ( Replaces
                          ( ExceptionalFractionCollection
                              { isDeleted = Just False,
                                group = "5ed75ad48e14e76fe3c5febc",
                                organisation = "5e27908010cfeaa15c9cee93",
                                createdAt = read @UTCTime "2023-12-29 12:03:15.126 UTC",
                                updatedAt = read @UTCTime "2023-12-29 12:03:32.849 UTC",
                                timestamp = read @UTCTime "2024-01-01 00:00:00 UTC",
                                id = "658eb5838f94c6007edfa96e",
                                fraction = "5e4e84d1bab65e9819d714d2",
                                exception =
                                  InnerExceptionReplacedBy
                                    { replacedBy = "658eb594262b65007e5accec"
                                    }
                              }
                          )
                          ( CollectionReplacementReason
                              { id = "5d610b87162c063cc0400102",
                                createdAt = read @UTCTime "2020-02-03 11:06:44.291 UTC",
                                updatedAt = read @UTCTime "2020-03-03 16:17:29.678 UTC",
                                name = Translated {en = "Public holiday", nl = "Feestdag", fr = "Jour f\233ri\233", de = "Feiertag"}
                              }
                          )
                      )
                }
            ]
        )

  it "parses a normal `Fractions` response" $
    eitherDecode @(SingObject "items" [Fraction])
      (BSL.fromStrict $(embedFile "test/responses/fractions.json"))
      `shouldBe` Right
        ( SingObject
            [ Fraction
                { id = "5ed7a542a2124463cc8814e6",
                  name = Translated {en = "Gft", nl = "Gft", fr = "Gft", de = "Gft"}
                }
            ]
        )
