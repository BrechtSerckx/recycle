module Recycle.Ics.Types
  ( EncodingSettings (..),
    FractionEncoding (..),
    Reminder (..),
    TodoDue (..),
    CollectionQuery (..),
    Filter (..),
  )
where

import Data.List (find)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Time.Zones.All (TZLabel, toTZName)
import qualified Data.Time.Zones.All as TZ
import Recycle.Types
import Recycle.Types.Orphans ()
import Web.FormUrlEncoded
  ( Form,
    FromForm (..),
    lookupUnique,
    parseAll,
    parseUnique,
  )

data EncodingSettings = EncodingSettings
  { timezone :: TZLabel,
    fractionEncoding :: FractionEncoding
  }
  deriving (Show)

instance FromForm EncodingSettings where
  fromForm f = do
    timezone <- parseTZLabel f
    fractionEncoding <- fromForm f
    pure EncodingSettings {..}

parseTZLabel :: Form -> Either Text TZLabel
parseTZLabel f = case lookupUnique "tz" f of
  Right tzText ->
    maybe (Left $ "Unknown timezone: " <> tzText) Right $
      find (\l -> TE.decodeUtf8 (toTZName l) == tzText) [minBound .. maxBound]
  Left _ -> Right TZ.Europe__Brussels

data FractionEncoding
  = EncodeFractionAsVEvent (Range TimeOfDay) [Reminder]
  | EncodeFractionAsVTodo TodoDue
  deriving (Show)

instance FromForm FractionEncoding where
  fromForm f = do
    lookupUnique "fe" f >>= \case
      "event" -> do
        range <- do
          from <- parseUnique "es" f
          to <- parseUnique "ee" f
          pure Range {..}
        reminders <- do
          daysBefore <- parseAll "rdb" f
          hoursBefore <- parseAll "rhb" f
          minutesBefore <- parseAll "rmb" f
          pure $
            zipWith3
              Reminder
              daysBefore
              hoursBefore
              minutesBefore
        pure $ EncodeFractionAsVEvent range reminders
      "todo" -> EncodeFractionAsVTodo <$> fromForm f
      t -> Left $ "Must be one of [event,todo]: " <> t

data Reminder = Reminder
  { daysBefore :: Int,
    hoursBefore :: Int,
    minutesBefore :: Int
  }
  deriving (Eq, Show)

data TodoDue
  = TodoDueDateTime Integer TimeOfDay
  | TodoDueDate Integer
  deriving (Eq, Show)

instance FromForm TodoDue where
  fromForm f =
    lookupUnique "tdt" f >>= \case
      "date" -> do
        dateReminder <- parseUnique "tdb" f
        pure $ TodoDueDate dateReminder
      "datetime" -> do
        daysBefore <- parseUnique "tdb" f
        timeOfDay <- parseUnique "tt" f
        pure $ TodoDueDateTime daysBefore timeOfDay
      t -> Left $ "Must be one of [date,datetime]: " <> t

data CollectionQuery = CollectionQuery
  { dateRange :: DateRange,
    langCode :: LangCode,
    encodingSettings :: EncodingSettings,
    zipcode :: ZipcodeId,
    street :: StreetId,
    houseNumber :: HouseNumber,
    filter :: Filter
  }

data Filter = Filter
  { -- | Include events
    events :: Bool,
    -- | Include these fractions (or all of them)
    fractions :: Maybe [FractionId]
  }
  deriving stock (Show)

instance FromForm Filter where
  fromForm f = do
    fi <- parseAll @Text "fi" f
    fractions <- parseAll "fif" f
    pure
      Filter
        { events = "e" `elem` fi,
          fractions =
            if "f" `elem` fi then Nothing else Just fractions
        }
