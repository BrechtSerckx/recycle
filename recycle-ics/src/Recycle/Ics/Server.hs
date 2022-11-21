{-# LANGUAGE DataKinds #-}

module Recycle.Ics.Server
  ( recycleIcsServer,
    recycleIcsApp,
    recycleToHandler,
    calculateDateRange,
    runCollectionQuery,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Text (Text)
import Data.Time hiding (getZonedTime)
import qualified Network.Wai as Wai
import Network.Wai.Application.Static
  ( defaultWebAppSettings,
  )
import Numeric.Natural (Natural)
import Recycle.AppM
import Recycle.Class
import Recycle.Ics.API (RecycleIcsAPI, pRecycleIcsAPI)
import Recycle.Ics.ICalendar
import Recycle.Ics.Opts
import Recycle.Types
import Servant.API
import Servant.Server
  ( Handler,
    ServerT,
    hoistServer,
    serve,
  )
import Servant.Server.StaticFiles (serveDirectoryWith)
import WaiAppStatic.Types

recycleIcsServer ::
  forall m.
  (HasRecycleClient m, HasTime m) =>
  FilePath ->
  ServerT RecycleIcsAPI m
recycleIcsServer dataDir =
  (searchZipcode :<|> searchStreet :<|> generateCollection) :<|> serveWww
  where
    searchZipcode ::
      SearchQuery Natural -> m (Union '[WithStatus 200 [FullZipcode]])
    searchZipcode query = do
      zipcodes <- searchZipcodes (Just query)
      pure . Z . I $ WithStatus @200 zipcodes
    searchStreet ::
      ZipcodeId -> SearchQuery Text -> m (Union '[WithStatus 200 [Street]])
    searchStreet zipcodeId query = do
      streets <- searchStreets (Just zipcodeId) (Just query)
      pure . Z . I $ WithStatus @200 streets
    generateCollection ::
      DateRange ->
      LangCode ->
      FractionEncoding ->
      ZipcodeId ->
      StreetId ->
      HouseNumber ->
      m (Union '[WithStatus 200 BSL8.ByteString])
    generateCollection collectionQueryDateRange collectionQueryLangCode collectionQueryFractionEncoding collectionQueryZipcode collectionQueryStreet collectionQueryHouseNumber =
      do
        let collectionQuery = CollectionQuery {..}
        collections <- runCollectionQuery collectionQuery
        pure . Z . I . WithStatus @200 $ printVCalendar collections
    serveWww =
      serveDirectoryWith
        (defaultWebAppSettings dataDir)
          { ssRedirectToIndex = True,
            ssIndices = [unsafeToPiece "index.html"]
          }

recycleToHandler :: Env -> RecycleM a -> Handler a
recycleToHandler env act = liftIO $ act `runRecycle` env

recycleIcsApp :: FilePath -> Env -> Wai.Application
recycleIcsApp wwwDir env =
  serve pRecycleIcsAPI
    . hoistServer pRecycleIcsAPI (recycleToHandler env)
    $ recycleIcsServer wwwDir

runCollectionQuery ::
  (HasRecycleClient m, HasTime m) => CollectionQuery -> m VCalendar
runCollectionQuery CollectionQuery {..} = do
  range <- calculateDateRange collectionQueryDateRange
  collections <-
    getCollections
      collectionQueryZipcode
      collectionQueryStreet
      collectionQueryHouseNumber
      range
  -- for_ collections $ liftIO . print

  pure $
    mkVCalendar
      collectionQueryLangCode
      collectionQueryFractionEncoding
      collections

calculateDateRange :: HasTime m => DateRange -> m (Range Day)
calculateDateRange = \case
  AbsoluteDateRange r -> pure r
  RelativeDateRange relRange -> do
    today <- localDay . zonedTimeToLocalTime <$> getZonedTime
    pure
      Range
        { rangeFrom = addDays (rangeFrom relRange) today,
          rangeTo = addDays (rangeTo relRange) today
        }
