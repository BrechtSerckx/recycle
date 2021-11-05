module Recycle
  ( main
  )
where

import           Control.Monad.IO.Class         ( liftIO )
import           Data.IORef                     ( newIORef )
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.Time               hiding ( getZonedTime )
import           Network.HTTP.Client.TLS        ( newTlsManagerWith
                                                , tlsManagerSettings
                                                )
import           Servant.Client                 ( BaseUrl(..)
                                                , Scheme(..)
                                                , mkClientEnv
                                                )
import           Colog.Core                     ( LogAction(..) )
import           Colog.Message
import qualified Data.Text                     as T

import           Recycle.Class
import           Recycle.AppM
import           Recycle.ICalendar
import           Recycle.Opts
import           Recycle.Types

main :: IO ()
main = do
  Opts { apiClientOpts = ApiClientOpts {..}, ..} <- parseOpts
  httpManager <- newTlsManagerWith tlsManagerSettings
  let clientEnv =
        mkClientEnv httpManager $ BaseUrl Https "recycleapp.be" 443 ""
      logAction = LogAction $ liftIO . putStrLn . T.unpack . fmtMessage

  authResult <- newIORef Nothing
  let env = Env { .. }
  case cmd of
    GenerateIcs GenerateIcsOpts {..} -> do
      vCalendar <- flip runRecycle env $ runCollectionQuery collectionQuery
      let bs = printVCalendar vCalendar
      case outputFile of
        Just f  -> BSL.writeFile f bs
        Nothing -> BSL.putStr bs

runCollectionQuery
  :: (HasRecycleClient m, HasTime m) => CollectionQuery -> m VCalendar
runCollectionQuery CollectionQuery {..} = do
  today <- localDay . zonedTimeToLocalTime <$> getZonedTime
  let fromDay  = addDays (negate 0) today
      untilDay = addDays 14 today
  collections <- getCollections collectionQueryZipcode
                                collectionQueryStreet
                                collectionQueryHouseNumber
                                (Range fromDay untilDay)
  -- for_ collections $ liftIO . print

  pure $ mkVCalendar collectionQueryLangCode
                     collectionQueryFractionEncoding
                     collections
