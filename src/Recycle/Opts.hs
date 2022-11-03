module Recycle.Opts
  ( Cmd(..)
  , Opts(..)
  , parseOpts
  , GenerateIcsOpts(..)
  , ServeIcsOpts(..)
  , CollectionQuery(..)
  , ApiClientOpts(..)
  , ApiClientCmd(..)
  )
where

import qualified Data.Char                     as Char
import qualified Network.Wai.Handler.Warp      as Warp
import           Options.Applicative
import           Text.Read                      ( readMaybe )

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

data Cmd
  = GenerateIcs GenerateIcsOpts
  | ApiClient ApiClientCmd
  | ServeIcs ServeIcsOpts

pCmd :: Parser Cmd
pCmd = hsubparser $ mconcat
  [ command "generate-ics"
    $ let generateInfo =
            mconcat [fullDesc, progDesc "Generate an iCalendar file"]
      in  (GenerateIcs <$> pGenerateIcsOpts) `info` generateInfo
  , command "client"
    $ let generateInfo =
            mconcat [fullDesc, progDesc "Client for the RecycleApp.be api"]
      in  (ApiClient <$> pApiClientCmd) `info` generateInfo
  , command "serve-ics"
    $ let serveInfo = mconcat [fullDesc, progDesc "Serve iCalendar files"]
      in  (ServeIcs <$> pServeIcsOpts) `info` serveInfo
  ]

data GenerateIcsOpts = GenerateIcsOpts
  { outputFile      :: Maybe FilePath
  , collectionQuery :: CollectionQuery
  }

data CollectionQuery = CollectionQuery
  { collectionQueryDateRange        :: DateRange
  , collectionQueryLangCode         :: LangCode
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
  collectionQueryDateRange <- pDateRange
  collectionQueryLangCode  <-
    option (maybeReader $ readMaybe . map Char.toUpper)
    $  long "language"
    <> short 'L'
    <> metavar "OUTPUT_FILE"
    <> value EN
    <> showDefault
    <> help "Preferred language for titles and descriptions"
  collectionQueryFractionEncoding <- pFractionEncoding
    -- let readFraction = \case
    --       "todo"  -> Right EncodeFractionAsVTodo
    --       "event" -> Right EncodeFractionAsVEvent
    --       _       -> Left "One of `todo` or `event`"
    --     showFraction = \case
    --       EncodeFractionAsVTodo  -> "todo"
    --       EncodeFractionAsVEvent -> "event"
    -- in  option (eitherReader readFraction)
    --     $  long "fraction-encoding"
    --     <> metavar "ENCODING"
    --     <> value EncodeFractionAsVTodo
    --     <> showDefaultWith showFraction
    --     <> help "Encode fraction collections as event or todo"
  collectionQueryZipcode          <- pZipcodeId
  collectionQueryStreet           <- pStreetId
  collectionQueryHouseNumber      <- pHouseNumber
  pure CollectionQuery { .. }

pFractionEncoding :: Parser FractionEncoding
pFractionEncoding =
  let pAsEvent = do
        eventRange <- do
          rangeFrom <- option auto $ mconcat
            [long "event-start", help "Start time of collection event"]
          rangeTo <- option auto
            $ mconcat [long "event-end", help "End time of collection event"]
          pure Range { .. }
        reminders <- many $ do
          flag' () $ long "reminder" <> help "Add a reminder"
          daysBefore <- option auto $ long "days-before" <> help
            "Remind x days before the collection"
          hoursBefore <- option auto $ long "hours-before" <> help
            "Remind x hours before the collection"
          minutesBefore <- option auto $ long "minutes-before" <> help
            "Remind x days before the collection"
          pure Reminder { .. }
        pure $ EncodeFractionAsVEvent eventRange reminders
      pAsTodo = do
        due <-
          let pDaysBefore = option auto $ long "todo-days-before" <> help
                "Set task x days before the collection"
              pTimeOfDay =
                option auto $ long "todo-time" <> help "Set task at this time"
          in  (TodoDueDateTime <$> pDaysBefore <*> pTimeOfDay)
                <|> (TodoDueDate <$> pDaysBefore)
        pure $ EncodeFractionAsVTodo due
  in  pAsEvent <|> pAsTodo

pDateRange :: Parser DateRange
pDateRange =
  AbsoluteDateRange
    <$> do
          rangeFrom <- option auto $ mconcat
            [long "absolute-from", help "Start date to fetch collections"]
          rangeTo <- option auto
            $ mconcat [long "absolute-to", help "End date to fetch collections"]
          pure Range { .. }
    <|> RelativeDateRange
    <$> do
          rangeFrom <-
            option auto
              $ mconcat
                  [ long "relative-from"
                  , help "Days before today to fetch collections"
                  ]
          rangeTo <- option auto $ mconcat
            [long "relative-to", help "Days before today to fetch collections"]
          pure Range { .. }

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
    <> help "Authentication secret. Get from inspecting the requests on browsing `recycleapp.be`."
  pure ApiClientOpts { .. }

defaultConsumer :: Consumer
defaultConsumer = Consumer "recycleapp.be"

data ApiClientCmd
  = GetAccessToken
  | SearchZipcodes (Maybe AccessToken) (Maybe SearchQuery)
  | SearchStreets (Maybe AccessToken) (Maybe ZipcodeId) (Maybe SearchQuery)
  | GetCollections (Maybe AccessToken) ZipcodeId StreetId HouseNumber DateRange
  | GetFractions (Maybe AccessToken) ZipcodeId StreetId HouseNumber

pApiClientCmd :: Parser ApiClientCmd
pApiClientCmd = hsubparser $ mconcat
  [ command "get-access-token"
  $      pure GetAccessToken
  `info` (fullDesc <> progDesc "Get an access token")
  , command "search-zipcodes"
  $      (SearchZipcodes <$> optional pAccessToken <*> optional pSearchQuery)
  `info` (fullDesc <> progDesc "Search for zipcodes")
  , command "search-streets"
  $      (   SearchStreets
         <$> optional pAccessToken
         <*> optional pZipcodeId
         <*> optional pSearchQuery
         )
  `info` (fullDesc <> progDesc "Search for streets")
  , command "get-collections"
  $      (   GetCollections
         <$> optional pAccessToken
         <*> pZipcodeId
         <*> pStreetId
         <*> pHouseNumber
         <*> pDateRange
         )
  `info` (fullDesc <> progDesc "Get collections")
  , command "get-fractions"
  $      (   GetFractions
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
  option auto
    $  long "house-number"
    <> short 'n'
    <> metavar "HOUSE_NUMBER"
    <> help "House number"

pStreetId :: Parser StreetId
pStreetId = strOption $ long "street" <> help "StreetId"

pSearchQuery :: Parser SearchQuery
pSearchQuery = strArgument $ metavar "QUERY" <> help "SearchQuery"

newtype ServeIcsOpts = ServeIcsOpts
  { port      :: Warp.Port
  }


pServeIcsOpts :: Parser ServeIcsOpts
pServeIcsOpts = do
  port <- option auto $ long "port" <> short 'p' <> metavar "PORT" <> help
    "port"
  pure ServeIcsOpts { .. }
