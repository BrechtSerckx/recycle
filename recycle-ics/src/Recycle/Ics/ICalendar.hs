{-# LANGUAGE DataKinds #-}

module Recycle.Ics.ICalendar
  ( mkVCalendar,
    printVCalendar,
    VCalendar,
    DateRange (..),
    FractionEncoding (..),
    Reminder (..),
    TodoDue (..),
  )
where

import Control.Applicative
import Data.Bifunctor
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import Data.Default.Class (def)
import Data.Function
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Time
import Data.Version (Version (..))
import Recycle.Ics.Types
import Recycle.Types
import Recycle.Types.Orphans ()
import Text.ICalendar.Printer
import Text.ICalendar.Types hiding (Range)

getByLangCode :: LangCode -> Map.Map LangCode a -> (LangCode, a)
getByLangCode lc m =
  fromMaybe (error "no translations") $
    ((lc,) <$> Map.lookup lc m)
      <|> ((EN,) <$> Map.lookup EN m)
      <|> headMay (Map.toList m)

mkVCalendar ::
  LangCode ->
  FractionEncoding ->
  Filter ->
  [CollectionEvent (Union '[FullFraction, Event])] ->
  VCalendar
mkVCalendar langCode fractionEncoding filter' ces =
  let (collections, events) = partitionCollectionEvents ces
   in (emptyVCalendar "recycle")
        { vcEvents = case fractionEncoding of
            EncodeFractionAsVEvent timeslot reminders ->
              mkMapWith
                (collectionToVEvent langCode timeslot reminders)
                ( maybe
                    collections
                    ( \fractions ->
                        filter
                          ((`elem` fractions) . (.content.id))
                          collections
                    )
                    filter'.fractions
                )
                <> if filter'.events then mkMapWith (eventToVEvent langCode) events else mempty
            EncodeFractionAsVTodo {} ->
              mkMapWith (eventToVEvent langCode) events,
          vcTodos = case fractionEncoding of
            EncodeFractionAsVEvent {} -> Map.empty
            EncodeFractionAsVTodo reminder ->
              mkMapWith
                (collectionToVTodo langCode reminder)
                collections
        }
  where
    mkAssoc ::
      CollectionEvent a ->
      ((TL.Text, Maybe (Either Date DateTime)), CollectionEvent a)
    mkAssoc ce =
      ((TL.fromStrict ce.id.unCollectionEventId, Nothing), ce)
    mkMapWith f = Map.fromList . map (second f . mkAssoc)

printVCalendar :: VCalendar -> BSL.ByteString
printVCalendar = printICalendar def

collectionToVEvent ::
  LangCode ->
  Range TimeOfDay ->
  [Reminder] ->
  CollectionEvent FullFraction ->
  VEvent
collectionToVEvent langCode range reminders collectionEvent =
  let description =
        getByLangCode langCode collectionEvent.content.name & \(lc, d) ->
          Description
            { descriptionValue = TL.fromStrict d,
              descriptionAltRep = Nothing,
              descriptionLanguage = Just . Language . CI.mk . TL.pack $ show lc,
              descriptionOther = def
            }
   in (emptyVEvent collectionEvent.id.unCollectionEventId collectionEvent.timestamp)
        { veDescription = Just description,
          veSummary =
            Just $
              getByLangCode langCode collectionEvent.content.name & \(lc, d) ->
                Summary
                  { summaryValue = TL.fromStrict d,
                    summaryAltRep = Nothing,
                    summaryLanguage = Just . Language . CI.mk . TL.pack $ show lc,
                    summaryOther = def
                  },
          veDTStart =
            Just
              DTStartDateTime
                { dtStartDateTimeValue =
                    UTCDateTime
                      collectionEvent.timestamp
                        { utctDayTime = timeOfDayToTime range.from
                        },
                  dtStartOther = def
                },
          veDTEndDuration =
            Just $
              Left
                DTEndDateTime
                  { dtEndDateTimeValue =
                      UTCDateTime
                        collectionEvent.timestamp
                          { utctDayTime = timeOfDayToTime range.to
                          },
                    dtEndOther = def
                  },
          veAlarms =
            Set.fromList $
              reminderToVAlarm description collectionEvent.timestamp
                <$> reminders
        }

collectionToVTodo ::
  LangCode -> TodoDue -> CollectionEvent FullFraction -> VTodo
collectionToVTodo langCode due collectionEvent =
  ( emptyVTodo collectionEvent.id.unCollectionEventId collectionEvent.timestamp
  )
    { vtDescription =
        Just $
          getByLangCode langCode collectionEvent.content.name & \(lc, d) ->
            Description
              { descriptionValue = TL.fromStrict d,
                descriptionAltRep = Nothing,
                descriptionLanguage = Just . Language . CI.mk . TL.pack $ show lc,
                descriptionOther = def
              },
      vtSummary =
        Just $
          getByLangCode langCode collectionEvent.content.name & \(lc, d) ->
            Summary
              { summaryValue = TL.fromStrict d,
                summaryAltRep = Nothing,
                summaryLanguage = Just . Language . CI.mk . TL.pack $ show lc,
                summaryOther = def
              },
      vtDueDuration = Just . Left $ makeDue collectionEvent.timestamp due
    }

makeDue :: UTCTime -> TodoDue -> Due
makeDue ts = \case
  TodoDueDateTime daysBefore timeOfDay ->
    DueDateTime
      { dueDateTimeValue =
          UTCDateTime
            UTCTime
              { utctDay = addDays (negate daysBefore) (utctDay ts),
                utctDayTime = timeOfDayToTime timeOfDay
              },
        dueOther = def
      }
  TodoDueDate daysBefore ->
    DueDate
      { dueDateValue =
          Date
            { dateValue = addDays (negate daysBefore) (utctDay ts)
            },
        dueOther = def
      }

reminderToVAlarm :: Description -> UTCTime -> Reminder -> VAlarm
reminderToVAlarm vaDescription _ts Reminder {..} =
  VAlarmDisplay
    { vaDescription,
      vaTrigger =
        TriggerDuration
          { triggerDuration =
              DurationDate
                { durSign = Negative,
                  durDay = daysBefore,
                  durHour = hoursBefore,
                  durMinute = minutesBefore,
                  durSecond = 0
                },
            -- { utctDay = addDays (negate dateTimeReminderDaysBefore)
            --                     (utctDay ts)
            -- , utctDayTime = timeOfDayToTime dateTimeReminderTimeOfDay
            -- }
            triggerRelated = Start,
            triggerOther = def
          },
      vaRepeat = def,
      vaDuration = Nothing,
      vaOther = Set.empty,
      vaActionOther = def
    }

eventToVEvent :: LangCode -> CollectionEvent Event -> VEvent
eventToVEvent langCode collectionEvent =
  ( emptyVEvent collectionEvent.id.unCollectionEventId collectionEvent.timestamp
  )
    { veDescription =
        Just $
          getByLangCode langCode collectionEvent.content.description & \(lc, d) ->
            Description
              { descriptionValue = TL.fromStrict d,
                descriptionAltRep = Nothing,
                descriptionLanguage = Just . Language . CI.mk . TL.pack $ show lc,
                descriptionOther = def
              },
      veSummary =
        Just $
          getByLangCode langCode collectionEvent.content.title & \(lc, d) ->
            Summary
              { summaryValue = TL.fromStrict d,
                summaryAltRep = Nothing,
                summaryLanguage = Just . Language . CI.mk . TL.pack $ show lc,
                summaryOther = def
              },
      veDTStart =
        Just
          DTStartDateTime
            { dtStartDateTimeValue =
                UTCDateTime
                  collectionEvent.timestamp,
              dtStartOther = def
            }
    }

emptyVEvent :: Text -> UTCTime -> VEvent
emptyVEvent uid ts =
  VEvent
    { veUID = UID {uidValue = TL.fromStrict uid, uidOther = def},
      veDTStamp = DTStamp {dtStampValue = ts, dtStampOther = def},
      veClass = def,
      veDTStart = Nothing,
      veCreated = Nothing,
      veDescription = Nothing,
      veGeo = Nothing,
      veLastMod = Nothing,
      veLocation = Nothing,
      veOrganizer = Nothing,
      vePriority = def,
      veSeq = def,
      veStatus = Nothing,
      veSummary = Nothing,
      veTransp = def,
      veUrl = Nothing,
      veRecurId = Nothing,
      veRRule = Set.empty,
      veDTEndDuration = Nothing,
      veAttach = Set.empty,
      veAttendee = Set.empty,
      veCategories = Set.empty,
      veComment = Set.empty,
      veContact = Set.empty,
      veExDate = Set.empty,
      veRStatus = Set.empty,
      veRelated = Set.empty,
      veResources = Set.empty,
      veRDate = Set.empty,
      veAlarms = Set.empty,
      veOther = Set.empty
    }

emptyVCalendar :: Text -> VCalendar
emptyVCalendar prodId =
  VCalendar
    { vcProdId = ProdId {prodIdValue = TL.fromStrict prodId, prodIdOther = def},
      vcVersion = MaxICalVersion (Version [2, 0] []) def,
      vcScale = def,
      vcMethod = Nothing,
      vcOther = Set.empty,
      vcTimeZones = def,
      vcEvents = def,
      vcTodos = def,
      vcJournals = def,
      vcFreeBusys = def,
      vcOtherComps = def
    }

emptyVTodo :: Text -> UTCTime -> VTodo
emptyVTodo uid ts =
  VTodo
    { vtUID = UID {uidValue = TL.fromStrict uid, uidOther = def},
      vtDTStamp = DTStamp {dtStampValue = ts, dtStampOther = def},
      vtClass = def,
      vtCompleted = Nothing,
      vtCreated = Nothing,
      vtDescription = Nothing,
      vtDTStart = Nothing,
      vtGeo = Nothing,
      vtLastMod = Nothing,
      vtLocation = Nothing,
      vtOrganizer = Nothing,
      vtPercent = Nothing,
      vtPriority = def,
      vtRecurId = Nothing,
      vtSeq = def,
      vtStatus = Nothing,
      vtSummary = Nothing,
      vtUrl = Nothing,
      vtRRule = Set.empty,
      vtDueDuration = Nothing,
      vtAttach = Set.empty,
      vtAttendee = Set.empty,
      vtCategories = Set.empty,
      vtComment = Set.empty,
      vtContact = Set.empty,
      vtExDate = Set.empty,
      vtRStatus = Set.empty,
      vtRelated = Set.empty,
      vtResources = Set.empty,
      vtRDate = Set.empty,
      vtAlarms = Set.empty,
      vtOther = Set.empty
    }
