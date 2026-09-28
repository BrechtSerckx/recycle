module Recycle.Ics.Types
  ( FractionEncoding (..),
    Reminder (..),
    TodoDue (..),
    CollectionQuery (..),
    Filter (..),
  )
where

import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Time
import Recycle.Types
import Recycle.Types.Orphans ()
import Web.FormUrlEncoded
  ( Form (..),
    FromForm (..),
    ToForm (..),
    lookupUnique,
    parseAll,
    parseUnique,
  )
import Web.HttpApiData (ToHttpApiData (..))

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

instance ToForm FractionEncoding where
  toForm (EncodeFractionAsVEvent Range {from, to} reminders) =
    pairsToForm $
      [("fe", "event"), ("es", toQueryParam from), ("ee", toQueryParam to)]
        ++ map (\r -> ("rdb", toQueryParam r.daysBefore)) reminders
        ++ map (\r -> ("rhb", toQueryParam r.hoursBefore)) reminders
        ++ map (\r -> ("rmb", toQueryParam r.minutesBefore)) reminders
  toForm (EncodeFractionAsVTodo (TodoDueDate d)) =
    pairsToForm [("fe", "todo"), ("tdt", "date"), ("tdb", toQueryParam d)]
  toForm (EncodeFractionAsVTodo (TodoDueDateTime d t)) =
    pairsToForm [("fe", "todo"), ("tdt", "datetime"), ("tdb", toQueryParam d), ("tt", toQueryParam t)]

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

instance ToForm Filter where
  toForm Filter {events, fractions} =
    pairsToForm $
      (if events then [("fi", "e")] else [])
        ++ case fractions of
          Nothing -> [("fi", "f")]
          Just fs -> map (\(FractionId fid) -> ("fif", fid)) fs

pairsToForm :: [(Text, Text)] -> Form
pairsToForm = Form . HM.fromListWith (++) . map (\(k, v) -> (k, [v]))
