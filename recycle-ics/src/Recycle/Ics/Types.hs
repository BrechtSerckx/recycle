module Recycle.Ics.Types
  ( FractionEncoding (..),
    Reminder (..),
    TodoDue (..),
    CollectionQuery (..),
    Filter (..),
  )
where

import Data.Text (Text)
import Data.Time
import Recycle.Types
import Recycle.Types.Orphans ()
import Web.FormUrlEncoded
  ( FromForm (..),
    lookupUnique,
    parseAll,
    parseUnique,
  )

data FractionEncoding
  = EncodeFractionAsVEvent (Range TimeOfDay) [Reminder]
  | EncodeFractionAsVTodo TodoDue
  deriving (Show)

instance FromForm FractionEncoding where
  fromForm f =
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
    fractionEncoding :: FractionEncoding,
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
