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
import Recycle.Class (HasRecycleClient, HasTime (..))
import qualified Recycle.Class as Recycle
import Recycle.Ics.API (RecycleIcsAPI, pRecycleIcsAPI)
import Recycle.Ics.ICalendar
import Recycle.Ics.Types
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
  (searchZipcode :<|> searchStreet :<|> getFractions :<|> generateCollection) :<|> serveWww
  where
    searchZipcode ::
      SearchQuery Natural -> m (Union '[WithStatus 200 [FullZipcode]])
    searchZipcode query = do
      zipcodes <- Recycle.searchZipcodes (Just query)
      pure . Z . I $ WithStatus @200 zipcodes
    searchStreet ::
      ZipcodeId -> SearchQuery Text -> m (Union '[WithStatus 200 [Street]])
    searchStreet zipcodeId query = do
      streets <- Recycle.searchStreets (Just zipcodeId) (Just query)
      pure . Z . I $ WithStatus @200 streets
    getFractions ::
      ZipcodeId ->
      StreetId ->
      HouseNumber ->
      m (Union '[WithStatus 200 [Fraction]])
    getFractions zipcode street houseNumber = do
      fractions <- Recycle.getFractions zipcode street houseNumber
      pure . Z . I $ WithStatus @200 fractions
    generateCollection ::
      DateRange ->
      LangCode ->
      FractionEncoding ->
      ZipcodeId ->
      StreetId ->
      HouseNumber ->
      Filter ->
      m (Union '[WithStatus 200 BSL8.ByteString])
    generateCollection dateRange langCode fractionEncoding zipcode street houseNumber filter' =
      do
        let collectionQuery = CollectionQuery {filter = filter', ..}
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
runCollectionQuery CollectionQuery {filter = filter', ..} = do
  range <- calculateDateRange dateRange
  collections <- Recycle.getCollections zipcode street houseNumber range
  pure $ mkVCalendar langCode fractionEncoding filter' collections

calculateDateRange :: (HasTime m) => DateRange -> m (Range Day)
calculateDateRange = \case
  AbsoluteDateRange r -> pure r
  RelativeDateRange relRange -> do
    today <- localDay . zonedTimeToLocalTime <$> getZonedTime
    pure
      Range
        { from = addDays relRange.from today,
          to = addDays relRange.to today
        }
