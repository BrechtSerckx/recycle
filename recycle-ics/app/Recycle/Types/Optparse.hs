module Recycle.Types.Optparse
  ( ApiClientOpts (..),
    pApiClientOpts,
    pZipcodeId,
    pHouseNumber,
    pStreetId,
    pDateRange,
    defaultConsumer,
  )
where

import Options.Applicative
import Recycle.Types

pDateRange :: Parser DateRange
pDateRange =
  AbsoluteDateRange
    <$> do
      from <-
        option auto $
          mconcat
            [long "absolute-from", help "Start date to fetch collections"]
      to <-
        option auto $
          mconcat [long "absolute-to", help "End date to fetch collections"]
      pure Range {..}
      <|> RelativeDateRange
    <$> do
      from <-
        option auto $
          mconcat
            [ long "relative-from",
              help "Days before today to fetch collections"
            ]
      to <-
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
