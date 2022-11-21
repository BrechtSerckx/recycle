module Opts
  ( Opts (..),
    parseOpts,
    ApiClientOpts (..),
    ApiClientCmd (..),
  )
where

import Data.Text (Text)
import Numeric.Natural (Natural)
import Options.Applicative
import Recycle.Types

data Opts = Opts
  { cmd :: ApiClientCmd,
    apiClientOpts :: ApiClientOpts
  }

parseOpts :: IO Opts
parseOpts =
  let parserInfo = mconcat [fullDesc, progDesc "Client for the RecycleApp.be api"]
   in execParser $ (pOpts <**> helper) `info` parserInfo

pOpts :: Parser Opts
pOpts = do
  cmd <- pApiClientCmd
  apiClientOpts <- pApiClientOpts
  pure Opts {..}

pDateRange :: Parser DateRange
pDateRange =
  AbsoluteDateRange
    <$> do
      rangeFrom <-
        option auto $
          mconcat
            [long "absolute-from", help "Start date to fetch collections"]
      rangeTo <-
        option auto $
          mconcat [long "absolute-to", help "End date to fetch collections"]
      pure Range {..}
    <|> RelativeDateRange
      <$> do
        rangeFrom <-
          option auto $
            mconcat
              [ long "relative-from",
                help "Days before today to fetch collections"
              ]
        rangeTo <-
          option auto $
            mconcat
              [long "relative-to", help "Days before today to fetch collections"]
        pure Range {..}

data ApiClientOpts = ApiClientOpts
  { consumer :: Consumer,
    authSecret :: AuthSecret
  }

pApiClientOpts :: Parser ApiClientOpts
pApiClientOpts = do
  consumer <-
    strOption
      ( long "consumer"
          <> help "One of [mobile-app, recycleapp.be]"
          <> value defaultConsumer
          <> showDefault
      )
  authSecret <-
    strOption $
      long "secret"
        <> help
          "Authentication secret. Get from inspecting the requests on browsing `recycleapp.be`."
  pure ApiClientOpts {..}

defaultConsumer :: Consumer
defaultConsumer = Consumer "recycleapp.be"

data ApiClientCmd
  = GetAccessToken
  | SearchZipcodes (Maybe AccessToken) (Maybe (SearchQuery Natural))
  | SearchStreets (Maybe AccessToken) (Maybe ZipcodeId) (Maybe (SearchQuery Text))
  | GetCollections (Maybe AccessToken) ZipcodeId StreetId HouseNumber DateRange
  | GetFractions (Maybe AccessToken) ZipcodeId StreetId HouseNumber

pApiClientCmd :: Parser ApiClientCmd
pApiClientCmd =
  hsubparser $
    mconcat
      [ command "get-access-token" $
          pure GetAccessToken
            `info` (fullDesc <> progDesc "Get an access token"),
        command "search-zipcodes" $
          (SearchZipcodes <$> optional pAccessToken <*> optional pSearchQueryNat)
            `info` (fullDesc <> progDesc "Search for zipcodes"),
        command "search-streets" $
          ( SearchStreets
              <$> optional pAccessToken
              <*> optional pZipcodeId
              <*> optional pSearchQueryText
          )
            `info` (fullDesc <> progDesc "Search for streets"),
        command "get-collections" $
          ( GetCollections
              <$> optional pAccessToken
              <*> pZipcodeId
              <*> pStreetId
              <*> pHouseNumber
              <*> pDateRange
          )
            `info` (fullDesc <> progDesc "Get collections"),
        command "get-fractions" $
          ( GetFractions
              <$> optional pAccessToken
              <*> pZipcodeId
              <*> pStreetId
              <*> pHouseNumber
          )
            `info` (fullDesc <> progDesc "Get fractions")
      ]

pAccessToken :: Parser AccessToken
pAccessToken = strOption $ long "access-token" <> help "AccessToken"

pZipcodeId :: Parser ZipcodeId
pZipcodeId = strOption $ long "zipcode" <> help "ZipcodeId"

pHouseNumber :: Parser HouseNumber
pHouseNumber =
  option auto $
    long "house-number"
      <> short 'n'
      <> metavar "HOUSE_NUMBER"
      <> help "House number"

pStreetId :: Parser StreetId
pStreetId = strOption $ long "street" <> help "StreetId"

pSearchQueryText :: Parser (SearchQuery Text)
pSearchQueryText = strArgument $ metavar "QUERY" <> help "SearchQuery"

pSearchQueryNat :: Parser (SearchQuery Natural)
pSearchQueryNat =
  argument (SearchQuery <$> auto) $ metavar "QUERY" <> help "SearchQuery"
