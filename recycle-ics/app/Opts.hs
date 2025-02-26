module Opts
  ( Cmd (..),
    Opts (..),
    parseOpts,
    GenerateIcsOpts (..),
    ServeIcsOpts (..),
    module Parsers,
  )
where

import Colog (Severity (..))
import qualified Data.Char as Char
import Options.Applicative
import Parsers
import Recycle.Ics.ICalendar
import Recycle.Ics.Types
import Recycle.Types
import Text.Read (readMaybe)

data Opts = Opts
  { cmd :: Cmd
  }

parseOpts :: IO Opts
parseOpts =
  let parserInfo =
        fullDesc
          <> progDesc
            "Generate iCalendar files for RecycleApp.be collections and events."
   in execParser $ (pOpts <**> helper) `info` parserInfo

pOpts :: Parser Opts
pOpts = do
  cmd <- pCmd
  pure Opts {..}

data Cmd
  = GenerateIcs GenerateIcsOpts
  | ServeIcs ServeIcsOpts

pCmd :: Parser Cmd
pCmd =
  hsubparser $
    mconcat
      [ command "generate-ics" $
          let generateInfo =
                mconcat [fullDesc, progDesc "Generate an iCalendar file"]
           in (GenerateIcs <$> pGenerateIcsOpts) `info` generateInfo,
        command "serve-ics" $
          let serveInfo = mconcat [fullDesc, progDesc "Serve iCalendar files"]
           in (ServeIcs <$> pServeIcsOpts) `info` serveInfo
      ]

data GenerateIcsOpts = GenerateIcsOpts
  { outputFile :: Maybe FilePath,
    collectionQuery :: CollectionQuery,
    apiClientOpts :: ApiClientOpts,
    verbosity :: Severity
  }

pGenerateIcsOpts :: Parser GenerateIcsOpts
pGenerateIcsOpts = do
  outputFile <-
    optional . strArgument $
      metavar "OUTPUT_FILE"
        <> help
          "output file"
  collectionQuery <- pCollectionQuery
  apiClientOpts <- pApiClientOpts
  verbosity <-
    option
      auto
      ( short 'v'
          <> long "verbosity"
          <> help "Log error messages of this severity and higher."
          <> metavar "SEVERITY"
          <> value Warning
      )
  pure GenerateIcsOpts {..}

pCollectionQuery :: Parser CollectionQuery
pCollectionQuery = do
  dateRange <- pDateRange
  langCode <-
    option (maybeReader $ readMaybe . map Char.toUpper) $
      long "language"
        <> short 'L'
        <> metavar "OUTPUT_FILE"
        <> value EN
        <> showDefault
        <> help "Preferred language for titles and descriptions"
  fractionEncoding <- pFractionEncoding
  zipcode <- pZipcodeId
  street <- pStreetId
  houseNumber <- pHouseNumber
  filter' <- do
    events <-
      not
        <$> switch (long "filter-events" <> help "Don't include events.")
    fractions <-
      optional $
        switch (long "filter-fractions" <> help "Only include specified fractions.")
          *> many (strOption $ long "include-fraction" <> help "Include this fraction.")
    pure Filter {..}
  pure CollectionQuery {filter = filter', ..}

pFractionEncoding :: Parser FractionEncoding
pFractionEncoding =
  let pAsEvent = do
        eventRange <- do
          from <-
            option auto $
              mconcat
                [long "event-start", help "Start time of collection event"]
          to <-
            option auto $
              mconcat [long "event-end", help "End time of collection event"]
          pure Range {..}
        reminders <- many $ do
          flag' () $ long "reminder" <> help "Add a reminder"
          daysBefore <-
            option auto $
              long "days-before"
                <> help
                  "Remind x days before the collection"
          hoursBefore <-
            option auto $
              long "hours-before"
                <> help
                  "Remind x hours before the collection"
          minutesBefore <-
            option auto $
              long "minutes-before"
                <> help
                  "Remind x days before the collection"
          pure Reminder {..}
        pure $ EncodeFractionAsVEvent eventRange reminders
      pAsTodo = do
        due <-
          let pDaysBefore =
                option auto $
                  long "todo-days-before"
                    <> help
                      "Set task x days before the collection"
              pTimeOfDay =
                option auto $ long "todo-time" <> help "Set task at this time"
           in (TodoDueDateTime <$> pDaysBefore <*> pTimeOfDay)
                <|> (TodoDueDate <$> pDaysBefore)
        pure $ EncodeFractionAsVTodo due
   in pAsEvent <|> pAsTodo

data ServeIcsOpts = ServeIcsOpts

pServeIcsOpts :: Parser ServeIcsOpts
pServeIcsOpts = pure ServeIcsOpts
