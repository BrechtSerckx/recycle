module Recycle.Opts
  ( Cmd(..)
  , Opts(..)
  , parseOpts
  , GenerateIcsOpts(..)
  , CollectionQuery(..)
  , ApiClientOpts(..)
  )
where

import qualified Data.Char                     as Char
import           Text.Read                      ( readMaybe )

import           Options.Applicative

import           Recycle.ICalendar
import           Recycle.Types
import           Recycle.Utils                  ( ISO639_1(EN)
                                                , LangCode
                                                )

data Opts = Opts
  { cmd           :: Cmd
  , apiClientOpts :: ApiClientOpts
  }

parseOpts :: IO Opts
parseOpts =
  let parserInfo = fullDesc <> progDesc
        "Generate iCalendar files for RecycleApp.be collections and events."
  in  execParser $ (pOpts <**> helper) `info` parserInfo

pOpts :: Parser Opts
pOpts = do
  cmd           <- pCmd
  apiClientOpts <- pApiClientOpts
  pure Opts { .. }

data Cmd = GenerateIcs GenerateIcsOpts

pCmd :: Parser Cmd
pCmd = hsubparser $ mconcat
  [ command "generate-ics"
      $ let generateInfo =
              mconcat [fullDesc, progDesc "Generate an iCalendar file"]
        in  (GenerateIcs <$> pGenerateIcsOpts) `info` generateInfo
  ]

data GenerateIcsOpts = GenerateIcsOpts
  { outputFile      :: Maybe FilePath
  , collectionQuery :: CollectionQuery
  }

data CollectionQuery = CollectionQuery
  { collectionQueryLangCode         :: LangCode
  , collectionQueryFractionEncoding :: FractionEncoding
  , collectionQueryZipcode          :: ZipcodeId
  , collectionQueryStreet           :: StreetId
  , collectionQueryHouseNumber      :: HouseNumber
  }

pGenerateIcsOpts :: Parser GenerateIcsOpts
pGenerateIcsOpts = do
  outputFile <- optional . strArgument $ metavar "OUTPUT_FILE" <> help
    "output file"
  collectionQuery <- pCollectionQuery
  pure GenerateIcsOpts { .. }

pCollectionQuery :: Parser CollectionQuery
pCollectionQuery = do
  collectionQueryLangCode <-
    option (maybeReader $ readMaybe . map Char.toUpper)
    $  long "language"
    <> short 'L'
    <> metavar "OUTPUT_FILE"
    <> value EN
    <> showDefault
    <> help "Preferred language for titles and descriptions"
  collectionQueryFractionEncoding <-
    let readFraction = \case
          "todo"  -> Right EncodeFractionAsVTodo
          "event" -> Right EncodeFractionAsVEvent
          _       -> Left "One of `todo` or `event`"
        showFraction = \case
          EncodeFractionAsVTodo  -> "todo"
          EncodeFractionAsVEvent -> "event"
    in  option (eitherReader readFraction)
        $  long "fraction-encoding"
        <> metavar "ENCODING"
        <> value EncodeFractionAsVTodo
        <> showDefaultWith showFraction
        <> help "Encode fraction collections as event or todo"
  collectionQueryZipcode     <- pZipcodeId
  collectionQueryStreet      <- pStreetId
  collectionQueryHouseNumber <- pHouseNumber
  pure CollectionQuery { .. }

data ApiClientOpts = ApiClientOpts
  { consumer   :: Consumer
  , authSecret :: AuthSecret
  }

pApiClientOpts :: Parser ApiClientOpts
pApiClientOpts = do
  consumer <- strOption
    (  long "consumer"
    <> help "One of [mobile-app, recycleapp.be]"
    <> value defaultConsumer
    <> showDefault
    )
  authSecret <-
    strOption
    $  long "secret"
    <> help "Authentication secret"
    <> value defaultAuthSecret
  pure ApiClientOpts { .. }

defaultConsumer :: Consumer
defaultConsumer = Consumer "recycleapp.be"

defaultAuthSecret :: AuthSecret
defaultAuthSecret =
  AuthSecret
    "Crgja3EGWe8jdapyr4EEoMBgZACYYjRRcRpaMQrLDW9HJBvmgkfGQyYqLgeXPavAGvnJqkV87PBB2b8zx43q46sUgzqio4yRZbABhtKeagkVKypTEDjKfPgGycjLyJTtLHYpzwJgp4YmmCuJZN9ZmJY8CGEoFs8MKfdJpU9RjkEVfngmmk2LYD4QzFegLNKUbcCeAdEW"

pZipcodeId :: Parser ZipcodeId
pZipcodeId = strOption $ long "zipcode" <> help "ZipcodeId"

pHouseNumber :: Parser HouseNumber
pHouseNumber =
  option auto
    $  long "house-number"
    <> short 'n'
    <> metavar "HOUSE_NUMBER"
    <> help "House number"

pStreetId :: Parser StreetId
pStreetId = strOption $ long "street" <> help "StreetId"
