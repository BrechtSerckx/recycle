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

import Data.Bifunctor
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import Data.Default.Class (def)
import Data.Function
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time
import Data.Version (Version (..))
import Recycle.Ics.Types
import Recycle.Types
import Recycle.Types.Orphans ()
import Text.ICalendar.Printer
import Text.ICalendar.Types hiding (Range)

getByLangCode :: LangCode -> Translated a -> a
getByLangCode lc m = case lc of
  NL -> m.nl
  FR -> m.fr
  DE -> m.de
  EN -> m.en

mkVCalendar ::
  LangCode ->
  FractionEncoding ->
  Filter ->
  [CollectionEvent] ->
  VCalendar
mkVCalendar langCode fractionEncoding filter' ces =
  let (collections, events) =
        bimap filterFractions filterEvents $
          partitionCollectionEvents ces
   in case fractionEncoding of
        EncodeFractionAsVEvent timeslot reminders ->
          let encodeCollection = collectionToVEvent langCode timeslot reminders
              encodeEvent = eventToVEvent langCode
           in (emptyVCalendar "recycle")
                { vcEvents =
                    mkMapWith encodeCollection collections
                      <> mkMapWith encodeEvent events,
                  vcTodos = Map.empty
                }
        EncodeFractionAsVTodo reminder ->
          let encodeCollection = collectionToVTodo langCode reminder
              encodeEvent = eventToVEvent langCode
           in (emptyVCalendar "recycle")
                { vcEvents = mkMapWith encodeEvent events,
                  vcTodos = mkMapWith encodeCollection collections
                }
  where
    mkAssoc a =
      ((TL.fromStrict a.id.unCollectionEventId, Nothing), a)
    mkMapWith f = Map.fromList . map (second f . mkAssoc)
    filterFractions =
      maybe
        id
        (\fractions -> filter ((`elem` fractions) . (.fraction.id)))
        filter'.fractions
    filterEvents = if filter'.events then id else const []

printVCalendar :: VCalendar -> BSL.ByteString
printVCalendar = printICalendar def

mkFractionCollectionSummary :: LangCode -> FractionCollection -> Summary
mkFractionCollectionSummary langCode fractionCollection =
  getByLangCode langCode fractionCollection.fraction.name & \d ->
    Summary
      { summaryValue = TL.fromStrict $ case fractionCollection.exception of
          Nothing -> d
          Just (ReplacedBy _) -> noCollectionText <> ": " <> d
          Just (Replaces _ _) -> extraCollectionText <> ": " <> d,
        summaryAltRep = Nothing,
        summaryLanguage = Just . Language . CI.mk . TL.pack $ show langCode,
        summaryOther = def
      }
  where
    noCollectionText =
      getByLangCode langCode $
        Translated
          { en = "No collection",
            nl = "Geen collectie",
            fr = "Pas de collecte",
            de = "Keine Sammlung"
          }
    extraCollectionText =
      getByLangCode langCode $
        Translated
          { en = "Extra collection",
            nl = "Extra collectie",
            fr = "Collecte supplémentaire",
            de = "Extra-Kollektion"
          }

mkFractionCollectionDescription :: LangCode -> FractionCollection -> Description
mkFractionCollectionDescription langCode fractionCollection =
  getByLangCode langCode fractionCollection.fraction.name & \d ->
    Description
      { descriptionValue = TL.fromStrict $ case fractionCollection.exception of
          Nothing -> d
          Just (ReplacedBy replacer) ->
            T.unlines
              [ replacedText <> ": " <> T.pack (show replacer.timestamp.utctDay),
                reasonText <> ": " <> getByLangCode langCode replacer.exception.reason.name,
                d
              ]
          Just (Replaces replaced reason) ->
            T.unlines
              [ replacesText <> ": " <> T.pack (show replaced.timestamp.utctDay),
                reasonText <> ": " <> getByLangCode langCode reason.name,
                d
              ],
        descriptionAltRep = Nothing,
        descriptionLanguage = Just . Language . CI.mk . TL.pack $ show langCode,
        descriptionOther = def
      }
  where
    replacedText =
      getByLangCode langCode $
        Translated
          { en = "Replaced by collection on",
            nl = "Vervangen door inzameling op",
            fr = "Remplacé par la collecte le",
            de = "Ersetzt durch Sammlung am"
          }
    replacesText =
      getByLangCode langCode $
        Translated
          { en = "Replaces collection on",
            nl = "Vervangt collectie op",
            fr = "Remplace la collecte sur",
            de = "Ersetzt die Sammlung auf"
          }
    reasonText =
      getByLangCode langCode $
        Translated
          { en = "Reason",
            nl = "Reden",
            fr = "Raison",
            de = "Grund"
          }

collectionToVEvent ::
  LangCode ->
  Range TimeOfDay ->
  [Reminder] ->
  FractionCollection ->
  VEvent
collectionToVEvent langCode range reminders fractionCollection =
  let description = mkFractionCollectionDescription langCode fractionCollection
   in (emptyVEvent fractionCollection.id.unCollectionEventId fractionCollection.timestamp)
        { veDescription = Just description,
          veSummary =
            Just $
              mkFractionCollectionSummary langCode fractionCollection,
          veDTStart =
            Just
              DTStartDateTime
                { dtStartDateTimeValue =
                    UTCDateTime
                      fractionCollection.timestamp
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
                        fractionCollection.timestamp
                          { utctDayTime = timeOfDayToTime range.to
                          },
                    dtEndOther = def
                  },
          veAlarms =
            Set.fromList $
              reminderToVAlarm description fractionCollection.timestamp
                <$> reminders,
          veStatus = case fractionCollection.exception of
            Just (ReplacedBy _) -> Just (CancelledEvent def)
            _ -> Nothing
        }

collectionToVTodo ::
  LangCode -> TodoDue -> FractionCollection -> VTodo
collectionToVTodo langCode due fractionCollection =
  ( emptyVTodo fractionCollection.id.unCollectionEventId fractionCollection.timestamp
  )
    { vtDescription =
        Just $ mkFractionCollectionDescription langCode fractionCollection,
      vtSummary =
        Just $ mkFractionCollectionSummary langCode fractionCollection,
      vtDueDuration = Just . Left $ makeDue fractionCollection.timestamp due,
      vtStatus = case fractionCollection.exception of
        Just (ReplacedBy _) -> Just (CancelledTodo def)
        _ -> Nothing
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

eventToVEvent :: LangCode -> Event -> VEvent
eventToVEvent langCode event =
  ( emptyVEvent event.id.unCollectionEventId event.timestamp
  )
    { veDescription =
        Just $
          getByLangCode langCode event.event.description & \d ->
            Description
              { descriptionValue = TL.fromStrict d,
                descriptionAltRep = Nothing,
                descriptionLanguage = Just . Language . CI.mk . TL.pack $ show langCode,
                descriptionOther = def
              },
      veSummary =
        Just $
          getByLangCode langCode event.event.title & \d ->
            Summary
              { summaryValue = TL.fromStrict d,
                summaryAltRep = Nothing,
                summaryLanguage = Just . Language . CI.mk . TL.pack $ show langCode,
                summaryOther = def
              },
      veDTStart =
        Just
          DTStartDateTime
            { dtStartDateTimeValue =
                UTCDateTime
                  event.timestamp,
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
