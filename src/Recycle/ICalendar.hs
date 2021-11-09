{-# LANGUAGE DataKinds #-}
module Recycle.ICalendar
  ( mkVCalendar
  , printVCalendar
  , VCalendar
  , DateRange(..)
  , FractionEncoding(..)
  , DateTimeReminder(..)
  , DateReminder(..)
  , TodoDue(..)
  )
where

import           Control.Applicative
import           Data.Bifunctor
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.CaseInsensitive          as CI
import           Data.Default.Class             ( def )
import           Data.Functor
import           Data.LanguageCodes
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text.Lazy                as TL
import           Data.Time
import           Data.Version                   ( Version(..) )
import           Web.FormUrlEncoded             ( FromForm(..)
                                                , lookupUnique
                                                , parseAll
                                                , parseUnique
                                                )
import           Web.HttpApiData                ( FromHttpApiData(..) )

import           Text.ICalendar.Printer
import           Text.ICalendar.Types    hiding ( Range )

import           Recycle.Types

getByLangCode :: LangCode -> Map.Map LangCode a -> Maybe (LangCode, a)
getByLangCode lc m =
  ((lc, ) <$> Map.lookup lc m) <|> ((EN, ) <$> Map.lookup EN m) <|> headMay
    (Map.toList m)

data DateRange = AbsoluteDateRange (Range Day) | RelativeDateRange (Range Integer)

instance FromForm DateRange where
  fromForm f =
    let lookupRange :: FromHttpApiData a => Either Text (Range a)
        lookupRange = do
          rangeFrom <- parseUnique "from" f
          rangeTo   <- parseUnique "to" f
          pure Range { .. }
    in  lookupUnique "date_range_type" f >>= \case
          "absolute" -> AbsoluteDateRange <$> lookupRange
          "relative" -> RelativeDateRange <$> lookupRange
          t          -> Left $ "Must be one of [absolute,relative]: " <> t

data FractionEncoding
  = EncodeFractionAsVEvent (Range TimeOfDay) [DateTimeReminder]
  | EncodeFractionAsVTodo TodoDue
  deriving Show

instance FromForm FractionEncoding where
  fromForm f = lookupUnique "fraction_encoding" f >>= \case
    "event" -> do
      eventRange <- do
        rangeFrom <- parseUnique "event_start" f
        rangeTo   <- parseUnique "event_end" f
        pure Range { .. }
      reminders <- do
        remindersDaysBefore <- parseAll "reminder_days_before" f
        remindersTime       <- parseAll "reminder_time" f
        pure $ zipWith DateTimeReminder remindersDaysBefore remindersTime
      pure $ EncodeFractionAsVEvent eventRange reminders
    "todo" -> EncodeFractionAsVTodo <$> fromForm f
    t      -> Left $ "Must be one of [event,todo]: " <> t

data DateTimeReminder = DateTimeReminder
  { dateTimeReminderDaysBefore :: Integer
  , dateTimeReminderTimeOfDay  :: TimeOfDay
  }
  deriving (Eq, Show)
newtype DateReminder = DateReminder
  { dateReminderDaysBefore :: Integer
  }
  deriving (Eq, Show)

data TodoDue
  = TodoDueDateTime DateTimeReminder
  | TodoDueDate DateReminder
  deriving (Eq, Show)

instance FromForm TodoDue where
  fromForm f = lookupUnique "todo_due_type" f >>= \case
    "date" -> do
      dateReminder <- do
        dateReminderDaysBefore <- parseUnique "todo_days_before" f
        pure DateReminder { .. }
      pure $ TodoDueDate dateReminder
    "datetime" -> do
      dateTimeReminder <- do
        dateTimeReminderDaysBefore <- parseUnique "todo_days_before" f
        dateTimeReminderTimeOfDay  <- parseUnique "todo_time" f
        pure DateTimeReminder { .. }
      pure $ TodoDueDateTime dateTimeReminder
    t -> Left $ "Must be one of [date,datetime]: " <> t

mkVCalendar
  :: LangCode
  -> FractionEncoding
  -> [CollectionEvent (Union '[FullFraction, Event])]
  -> VCalendar
mkVCalendar langCode fractionEncoding ces =
  let (collections, events) = partitionCollectionEvents ces
  in  (emptyVCalendar "recycle")
        { vcEvents = case fractionEncoding of
                       EncodeFractionAsVEvent _eventRange _reminders ->
                         mkMapWith (collectionToVEvent langCode) collections
                           <> mkMapWith (eventToVEvent langCode) events
                       EncodeFractionAsVTodo _due ->
                         mkMapWith (eventToVEvent langCode) events
        , vcTodos  = case fractionEncoding of
                       EncodeFractionAsVEvent _eventRange _reminders ->
                         Map.empty
                       EncodeFractionAsVTodo _due ->
                         mkMapWith (collectionToVTodo langCode) collections
        }

 where
  mkAssoc
    :: CollectionEvent a
    -> ((TL.Text, Maybe (Either Date DateTime)), CollectionEvent a)
  mkAssoc ce =
    ((TL.fromStrict . unCollectionEventId $ collectionEventId ce, Nothing), ce)
  mkMapWith f = Map.fromList . map (second f . mkAssoc)

printVCalendar :: VCalendar -> BSL.ByteString
printVCalendar = printICalendar def

collectionToVEvent :: LangCode -> CollectionEvent FullFraction -> VEvent
collectionToVEvent langCode CollectionEvent { collectionEventContent = FullFraction {..}, ..}
  = (emptyVEvent (unCollectionEventId collectionEventId)
                 collectionEventTimestamp
    )
    { veDescription = getByLangCode langCode fullFractionName <&> \(lc, d) ->
      Description
        { descriptionValue    = TL.fromStrict d
        , descriptionAltRep   = Nothing
        , descriptionLanguage = Just . Language . CI.mk . TL.pack $ show lc
        , descriptionOther    = def
        }
    , veSummary     = getByLangCode langCode fullFractionName <&> \(lc, d) ->
      Summary { summaryValue    = TL.fromStrict d
              , summaryAltRep   = Nothing
              , summaryLanguage = Just . Language . CI.mk . TL.pack $ show lc
              , summaryOther    = def
              }
    }

collectionToVTodo :: LangCode -> CollectionEvent FullFraction -> VTodo
collectionToVTodo langCode CollectionEvent { collectionEventContent = FullFraction {..}, ..}
  = (emptyVTodo (unCollectionEventId collectionEventId) collectionEventTimestamp
    )
    { vtDescription = getByLangCode langCode fullFractionName <&> \(lc, d) ->
      Description
        { descriptionValue    = TL.fromStrict d
        , descriptionAltRep   = Nothing
        , descriptionLanguage = Just . Language . CI.mk . TL.pack $ show lc
        , descriptionOther    = def
        }
    , vtSummary     = getByLangCode langCode fullFractionName <&> \(lc, d) ->
      Summary { summaryValue    = TL.fromStrict d
              , summaryAltRep   = Nothing
              , summaryLanguage = Just . Language . CI.mk . TL.pack $ show lc
              , summaryOther    = def
              }
    }

eventToVEvent :: LangCode -> CollectionEvent Event -> VEvent
eventToVEvent langCode CollectionEvent { collectionEventContent = Event {..}, ..}
  = (emptyVEvent (unCollectionEventId collectionEventId)
                 collectionEventTimestamp
    )
    { veDescription = getByLangCode langCode eventDescription <&> \(lc, d) ->
      Description
        { descriptionValue    = TL.fromStrict d
        , descriptionAltRep   = Nothing
        , descriptionLanguage = Just . Language . CI.mk . TL.pack $ show lc
        , descriptionOther    = def
        }
    , veSummary     = getByLangCode langCode eventTitle <&> \(lc, d) -> Summary
      { summaryValue    = TL.fromStrict d
      , summaryAltRep   = Nothing
      , summaryLanguage = Just . Language . CI.mk . TL.pack $ show lc
      , summaryOther    = def
      }
    , veDTStart = Just DTStartDateTime
                    { dtStartDateTimeValue = UTCDateTime
                                               collectionEventTimestamp
                    , dtStartOther         = def
                    }
    }

emptyVEvent :: Text -> UTCTime -> VEvent
emptyVEvent uid ts = VEvent
  { veUID           = UID { uidValue = TL.fromStrict uid, uidOther = def }
  , veDTStamp       = DTStamp { dtStampValue = ts, dtStampOther = def }
  , veClass         = def
  , veDTStart       = Nothing
  , veCreated       = Nothing
  , veDescription   = Nothing
  , veGeo           = Nothing
  , veLastMod       = Nothing
  , veLocation      = Nothing
  , veOrganizer     = Nothing
  , vePriority      = def
  , veSeq           = def
  , veStatus        = Nothing
  , veSummary       = Nothing
  , veTransp        = def
  , veUrl           = Nothing
  , veRecurId       = Nothing
  , veRRule         = Set.empty
  , veDTEndDuration = Nothing
  , veAttach        = Set.empty
  , veAttendee      = Set.empty
  , veCategories    = Set.empty
  , veComment       = Set.empty
  , veContact       = Set.empty
  , veExDate        = Set.empty
  , veRStatus       = Set.empty
  , veRelated       = Set.empty
  , veResources     = Set.empty
  , veRDate         = Set.empty
  , veAlarms        = Set.empty
  , veOther         = Set.empty
  }

emptyVCalendar :: Text -> VCalendar
emptyVCalendar prodId = VCalendar
  { vcProdId = ProdId { prodIdValue = TL.fromStrict prodId, prodIdOther = def }
  , vcVersion    = MaxICalVersion (Version [2, 0] []) def
  , vcScale      = def
  , vcMethod     = Nothing
  , vcOther      = Set.empty
  , vcTimeZones  = def
  , vcEvents     = def
  , vcTodos      = def
  , vcJournals   = def
  , vcFreeBusys  = def
  , vcOtherComps = def
  }

emptyVTodo :: Text -> UTCTime -> VTodo
emptyVTodo uid ts = VTodo
  { vtUID         = UID { uidValue = TL.fromStrict uid, uidOther = def }
  , vtDTStamp     = DTStamp { dtStampValue = ts, dtStampOther = def }
  , vtClass       = def
  , vtCompleted   = Nothing
  , vtCreated     = Nothing
  , vtDescription = Nothing
  , vtDTStart     = Nothing
  , vtGeo         = Nothing
  , vtLastMod     = Nothing
  , vtLocation    = Nothing
  , vtOrganizer   = Nothing
  , vtPercent     = Nothing
  , vtPriority    = def
  , vtRecurId     = Nothing
  , vtSeq         = def
  , vtStatus      = Nothing
  , vtSummary     = Nothing
  , vtUrl         = Nothing
  , vtRRule       = Set.empty
  , vtDueDuration = Nothing
  , vtAttach      = Set.empty
  , vtAttendee    = Set.empty
  , vtCategories  = Set.empty
  , vtComment     = Set.empty
  , vtContact     = Set.empty
  , vtExDate      = Set.empty
  , vtRStatus     = Set.empty
  , vtRelated     = Set.empty
  , vtResources   = Set.empty
  , vtRDate       = Set.empty
  , vtAlarms      = Set.empty
  , vtOther       = Set.empty
  }
