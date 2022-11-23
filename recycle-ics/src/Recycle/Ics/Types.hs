module Recycle.Ics.Types
  ( FractionEncoding (..),
    Reminder (..),
    TodoDue (..),
    CollectionQuery (..),
    Filter (..),
  )
where

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
        eventRange <- do
          rangeFrom <- parseUnique "es" f
          rangeTo <- parseUnique "ee" f
          pure Range {..}
        reminders <- do
          remindersDaysBefore <- parseAll "rdb" f
          remindersHoursBefore <- parseAll "rhb" f
          remindersMinutesBefore <- parseAll "rmb" f
          pure $
            zipWith3
              Reminder
              remindersDaysBefore
              remindersHoursBefore
              remindersMinutesBefore
        pure $ EncodeFractionAsVEvent eventRange reminders
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
  { collectionQueryDateRange :: DateRange,
    collectionQueryLangCode :: LangCode,
    collectionQueryFractionEncoding :: FractionEncoding,
    collectionQueryZipcode :: ZipcodeId,
    collectionQueryStreet :: StreetId,
    collectionQueryHouseNumber :: HouseNumber
  }

data Filter = Filter
  { -- | Include events
    filterEvents :: Bool,
    -- | Include these fractions
    filterFractions :: [FractionId]
  }

instance FromForm Filter where
  fromForm f = do
    fractions <- parseAll "fif" f
    pure
      Filter
        { filterEvents = "e" `elem` parseAll "fi" f,
          filterFractions =
            if "f" `elem` parseAll "fi" f
              then fractions
              else mempty
        }
