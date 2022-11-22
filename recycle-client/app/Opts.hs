module Opts
  ( Opts (..),
    parseOpts,
    ApiClientOpts (..),
    ApiClientCmd (..),
    module Recycle.Types.Optparse
  )
where

import Data.Text (Text)
import Numeric.Natural (Natural)
import Options.Applicative
import Recycle.Types
import Recycle.Types.Optparse

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

pSearchQueryText :: Parser (SearchQuery Text)
pSearchQueryText = strArgument $ metavar "QUERY" <> help "SearchQuery"

pSearchQueryNat :: Parser (SearchQuery Natural)
pSearchQueryNat =
  argument (SearchQuery <$> auto) $ metavar "QUERY" <> help "SearchQuery"
