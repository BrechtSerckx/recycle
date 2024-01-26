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

  it "parses a normal `Collections` event response" $
    eitherDecode
      @(SingObject "items" [CollectionEvent (Union '[FullFraction, Event])])
      (BSL.fromStrict $(embedFile "test/responses/collection-event.json"))
      `shouldBe` Right
        ( SingObject
            [ CollectionEvent
                { id = "5fed94cf5214974871257660",
                  timestamp = read "2021-09-18 00:00:00 UTC",
                  content =
                    S
                      ( Z
                          ( I
                              ( Event
                                  { title =
                                      Map.fromList
                                        [ (EN, "Repair Caf\233"),
                                          (NL, "Repair Caf\233"),
                                          (FR, "Repair Caf\233"),
                                          (DE, "Repair Caf\233")
                                        ],
                                    introduction =
                                      Map.fromList
                                        [ (EN, "Repair Caf\233 in MAAKbar"),
                                          (NL, "Repair Caf\233 in MAAKbar"),
                                          (FR, "Repair Caf\233 in MAAKbar"),
                                          (DE, "Repair Caf\233 in MAAKbar")
                                        ],
                                    description =
                                      Map.fromList
                                        [ (EN, "Elke derde zaterdag van de maand tussen 14.00 en 17.00 uur in de Diestsestraat 142 in Leuven."),
                                          (NL, "Elke derde zaterdag van de maand tussen 14.00 en 17.00 uur in de Diestsestraat 142 in Leuven."),
                                          (FR, "Elke derde zaterdag van de maand tussen 14.00 en 17.00 uur in de Diestsestraat 142 in Leuven."),
                                          (DE, "Elke derde zaterdag van de maand tussen 14.00 en 17.00 uur in de Diestsestraat 142 in Leuven.")
                                        ],
                                    externalLink =
                                      Map.fromList
                                        [ (EN, "https://www.maakbaarleuven.be/"),
                                          (NL, "https://www.maakbaarleuven.be/"),
                                          (FR, "https://www.maakbaarleuven.be/"),
                                          (DE, "https://www.maakbaarleuven.be/")
                                        ]
                                  }
                              )
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
                  name = Map.fromList [(EN, "Gft"), (NL, "Gft"), (FR, "Gft"), (DE, "Gft")],
                  logo =
                    Logo
                      { regular =
                          Map.fromList
                            [ ("1x", "5ef36735da06b2747894b353"),
                              ("2x", "5ef36735da06b2a9b994b354"),
                              ("3x", "5ef36735da06b273b894b355")
                            ],
                        reversed =
                          Map.fromList
                            [ ("1x", "5ef36735da06b276b594b356"),
                              ("2x", "5ef36735da06b2002c94b357"),
                              ("3x", "5ef36735da06b22deb94b358")
                            ],
                        name =
                          Map.fromList
                            [ ("de", "Bioabfall"),
                              ("en", "Biodegradable waste"),
                              ("fr", "D\233chets biod\233gradables"),
                              ("nl", "Groente-, fruit-, tuinafval")
                            ],
                        id = "5d610b86162c063cc0400108"
                      },
                  color = RGB "#C7D33B",
                  variations =
                    [ Object
                        ( Aeson.fromList
                            [ ( "link",
                                Object
                                  ( Aeson.fromList
                                      [ ( "external",
                                          Object
                                            ( Aeson.fromList
                                                [ ("de", String "https://www.ecowerf.be/ophaling-aan-huis"),
                                                  ("en", String "https://www.ecowerf.be/ophaling-aan-huis"),
                                                  ("fr", String "https://www.ecowerf.be/ophaling-aan-huis"),
                                                  ("nl", String "https://www.ecowerf.be/ophaling-aan-huis")
                                                ]
                                            )
                                        ),
                                        ("internal", Bool False)
                                      ]
                                  )
                              ),
                              ( "rules",
                                Object
                                  ( Aeson.fromList
                                      [ ( "excludes",
                                          Object
                                            ( Aeson.fromList
                                                [ ("de", String "\8226 Vloeistoffen (bv. soep, melk, koffie \8230)\n\8226 Sauzen, vet en oli\235n (uitgezonderd gerechten\nwaar een klein aandeel saus in zit)\n\8226 Theezakjes en koffiepads\n\8226 Behandeld hout (met verf, vernis of\nimpregneermiddel)\n\8226 Beenderen en dierlijk (slacht)afval\n\8226 Dierenkrengen\n\8226 Schelpen van mosselen, oesters \8230\n\8226 Kattenbakvulling en vogelkooizand\n\8226 Mest van grote huisdieren (katten, honden)\nof in grote hoeveelheden\n\8226 Stof uit de stofzuiger\n\8226 Wegwerpluier en ander hygi\235neafval\n\8226 Aarde en zand\n\8226 Kurk\n\8226 Asresten en houtskool\n\8226 Plastiek, glas, metalen\n\8226 Grof ongesnipperd snoeihout, dikke takken"),
                                                  ("en", String "\8226 Vloeistoffen (bv. soep, melk, koffie \8230)\n\8226 Sauzen, vet en oli\235n (uitgezonderd gerechten\nwaar een klein aandeel saus in zit)\n\8226 Theezakjes en koffiepads\n\8226 Behandeld hout (met verf, vernis of\nimpregneermiddel)\n\8226 Beenderen en dierlijk (slacht)afval\n\8226 Dierenkrengen\n\8226 Schelpen van mosselen, oesters \8230\n\8226 Kattenbakvulling en vogelkooizand\n\8226 Mest van grote huisdieren (katten, honden)\nof in grote hoeveelheden\n\8226 Stof uit de stofzuiger\n\8226 Wegwerpluier en ander hygi\235neafval\n\8226 Aarde en zand\n\8226 Kurk\n\8226 Asresten en houtskool\n\8226 Plastiek, glas, metalen\n\8226 Grof ongesnipperd snoeihout, dikke takken"),
                                                  ("fr", String "\8226 Vloeistoffen (bv. soep, melk, koffie \8230)\n\8226 Sauzen, vet en oli\235n (uitgezonderd gerechten\nwaar een klein aandeel saus in zit)\n\8226 Theezakjes en koffiepads\n\8226 Behandeld hout (met verf, vernis of\nimpregneermiddel)\n\8226 Beenderen en dierlijk (slacht)afval\n\8226 Dierenkrengen\n\8226 Schelpen van mosselen, oesters \8230\n\8226 Kattenbakvulling en vogelkooizand\n\8226 Mest van grote huisdieren (katten, honden)\nof in grote hoeveelheden\n\8226 Stof uit de stofzuiger\n\8226 Wegwerpluier en ander hygi\235neafval\n\8226 Aarde en zand\n\8226 Kurk\n\8226 Asresten en houtskool\n\8226 Plastiek, glas, metalen\n\8226 Grof ongesnipperd snoeihout, dikke takken"),
                                                  ("nl", String "\8226 Vloeistoffen (bv. soep, melk, koffie \8230)\n\8226 Sauzen, vet en oli\235n (uitgezonderd gerechten\nwaar een klein aandeel saus in zit)\n\8226 Theezakjes en koffiepads\n\8226 Behandeld hout (met verf, vernis of\nimpregneermiddel)\n\8226 Beenderen en dierlijk (slacht)afval\n\8226 Dierenkrengen\n\8226 Schelpen van mosselen, oesters \8230\n\8226 Kattenbakvulling en vogelkooizand\n\8226 Mest van grote huisdieren (katten, honden)\nof in grote hoeveelheden\n\8226 Stof uit de stofzuiger\n\8226 Wegwerpluier en ander hygi\235neafval\n\8226 Aarde en zand\n\8226 Kurk\n\8226 Asresten en houtskool\n\8226 Plastiek, glas, metalen\n\8226 Grof ongesnipperd snoeihout, dikke takken")
                                                ]
                                            )
                                        ),
                                        ( "includes",
                                          Object
                                            ( Aeson.fromList
                                                [ ("de", String "Organisch afval uit keuken en tuin:\n\8226 Schillen en resten van fruit, groenten en\naardappelen\n\8226 Dierlijk en plantaardig keukenafval en\netensresten\n\8226 Broodresten\n\8226 Koffiedik, papieren koffiefilter\n\8226 Papier van keukenrol\n\8226 Noten, pitten\n\8226 Vlees- en visresten, schaaldierresten\n(uitgezonderd mosselschelpen,\noesterschelpen \8230)\n\8226 Vaste zuivelproducten (kaasresten)\n\8226 Eieren, eierschalen\n\8226 Fijn tuin- en snoeiafval (bladeren, gras, onkruid,\nhaagscheersel, versnipperd snoeihout \8230)\n\8226 Kamer- en tuinplanten\n\8226 Schaafkrullen en zaagmeel van\nonbehandeld hout\n\8226 Mest van kleine huisdieren (cavia, konijn)"),
                                                  ("en", String "Organisch afval uit keuken en tuin:\n\8226 Schillen en resten van fruit, groenten en\naardappelen\n\8226 Dierlijk en plantaardig keukenafval en\netensresten\n\8226 Broodresten\n\8226 Koffiedik, papieren koffiefilter\n\8226 Papier van keukenrol\n\8226 Noten, pitten\n\8226 Vlees- en visresten, schaaldierresten\n(uitgezonderd mosselschelpen,\noesterschelpen \8230)\n\8226 Vaste zuivelproducten (kaasresten)\n\8226 Eieren, eierschalen\n\8226 Fijn tuin- en snoeiafval (bladeren, gras, onkruid,\nhaagscheersel, versnipperd snoeihout \8230)\n\8226 Kamer- en tuinplanten\n\8226 Schaafkrullen en zaagmeel van\nonbehandeld hout\n\8226 Mest van kleine huisdieren (cavia, konijn)"),
                                                  ("fr", String "Organisch afval uit keuken en tuin:\n\8226 Schillen en resten van fruit, groenten en\naardappelen\n\8226 Dierlijk en plantaardig keukenafval en\netensresten\n\8226 Broodresten\n\8226 Koffiedik, papieren koffiefilter\n\8226 Papier van keukenrol\n\8226 Noten, pitten\n\8226 Vlees- en visresten, schaaldierresten\n(uitgezonderd mosselschelpen,\noesterschelpen \8230)\n\8226 Vaste zuivelproducten (kaasresten)\n\8226 Eieren, eierschalen\n\8226 Fijn tuin- en snoeiafval (bladeren, gras, onkruid,\nhaagscheersel, versnipperd snoeihout \8230)\n\8226 Kamer- en tuinplanten\n\8226 Schaafkrullen en zaagmeel van\nonbehandeld hout\n\8226 Mest van kleine huisdieren (cavia, konijn)"),
                                                  ("nl", String "Organisch afval uit keuken en tuin:\n\8226 Schillen en resten van fruit, groenten en\naardappelen\n\8226 Dierlijk en plantaardig keukenafval en\netensresten\n\8226 Broodresten\n\8226 Koffiedik, papieren koffiefilter\n\8226 Papier van keukenrol\n\8226 Noten, pitten\n\8226 Vlees- en visresten, schaaldierresten\n(uitgezonderd mosselschelpen,\noesterschelpen \8230)\n\8226 Vaste zuivelproducten (kaasresten)\n\8226 Eieren, eierschalen\n\8226 Fijn tuin- en snoeiafval (bladeren, gras, onkruid,\nhaagscheersel, versnipperd snoeihout \8230)\n\8226 Kamer- en tuinplanten\n\8226 Schaafkrullen en zaagmeel van\nonbehandeld hout\n\8226 Mest van kleine huisdieren (cavia, konijn)")
                                                ]
                                            )
                                        ),
                                        ( "information",
                                          Object
                                            ( Aeson.fromList
                                                [ ("de", String "Plaats de groene container met een geldige sticker en met de handgreep naar de straatkant.\nContainers waarvan het deksel niet volledig gesloten is, worden niet geledigd."),
                                                  ("en", String "Plaats de groene container met een geldige sticker en met de handgreep naar de straatkant.\nContainers waarvan het deksel niet volledig gesloten is, worden niet geledigd."),
                                                  ("fr", String "Plaats de groene container met een geldige sticker en met de handgreep naar de straatkant.\nContainers waarvan het deksel niet volledig gesloten is, worden niet geledigd."),
                                                  ("nl", String "Plaats de groene container met een geldige sticker en met de handgreep naar de straatkant.\nContainers waarvan het deksel niet volledig gesloten is, worden niet geledigd.")
                                                ]
                                            )
                                        )
                                      ]
                                  )
                              ),
                              ( "subtitle",
                                Object
                                  ( Aeson.fromList
                                      [ ("de", String "Gft"),
                                        ("en", String "Gft"),
                                        ("fr", String "Gft"),
                                        ("nl", String "Gft")
                                      ]
                                  )
                              ),
                              ( "targets",
                                Array $
                                  V.fromList
                                    [ String "5ed62945a212442a6588124c",
                                      String "5eaac04536fe0b3bec4bf9be",
                                      String "5ed6291aa212448465881249",
                                      String "5ed753cb8e14e7a62cc5fe39",
                                      String "5ed75ad48e14e76fe3c5febc",
                                      String "5ed76d43a21244aa188814d7",
                                      String "5ed76a518e14e7a3b7c5ff17",
                                      String "5ed743e08e14e78c5fc5fdd9",
                                      String "5ed771f38e14e78ab3c5ff20",
                                      String "5ed7431ca2124470208813af",
                                      String "5ed661ad8e14e78068c5fdcb",
                                      String "5ed662e28e14e7143bc5fdcf",
                                      String "5ed66976a2124426cc8813aa"
                                    ]
                              ),
                              ( "title",
                                Object
                                  ( Aeson.fromList
                                      [ ("de", String "Variante 1"),
                                        ("en", String "Variant 1"),
                                        ("fr", String "Variante 1"),
                                        ("nl", String "Variant 1")
                                      ]
                                  )
                              ),
                              ("variations", Array V.empty)
                            ]
                        )
                    ]
                }
            ]
        )
