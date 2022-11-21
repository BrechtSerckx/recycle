{-# LANGUAGE DataKinds #-}

module Recycle
  ( main,
  )
where

import Colog.Core (LogAction (..))
import Colog.Message
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Foldable (for_)
import Data.IORef (newIORef)
import qualified Data.Text as T
import Data.Time hiding (getZonedTime)
import Network.HTTP.Client.TLS
  ( newTlsManagerWith,
    tlsManagerSettings,
  )
import Recycle.AppM
import Recycle.Class
import Recycle.Opts
import Recycle.Types
import Servant.Client
  ( BaseUrl (..),
    Scheme (..),
    mkClientEnv,
  )

main :: IO ()
main = do
  Opts {apiClientOpts = ApiClientOpts {..}, ..} <- parseOpts
  httpManager <- newTlsManagerWith tlsManagerSettings
  let clientEnv =
        mkClientEnv httpManager $ BaseUrl Https "api.fostplus.be" 443 ""
      logAction = LogAction $ liftIO . putStrLn . T.unpack . fmtMessage

  authResult <- newIORef Nothing
  let env = Env {..}
  flip runRecycle env $ case cmd of
    GetAccessToken -> liftIO . BSL8.putStrLn . Aeson.encode =<< getAuthResult
    SearchZipcodes mAccessToken mQuery -> do
      for_ mAccessToken setAccessToken
      zipcodes <- searchZipcodes mQuery
      liftIO . BSL8.putStrLn $ Aeson.encode zipcodes
    SearchStreets mAccessToken mZipcode mQuery -> do
      for_ mAccessToken setAccessToken
      streets <- searchStreets mZipcode mQuery
      liftIO . BSL8.putStrLn $ Aeson.encode streets
    GetCollections mAccessToken zipcode street houseNumber dateRange -> do
      for_ mAccessToken setAccessToken
      range <- liftIO $ calculateDateRange dateRange
      collections <- getCollections zipcode street houseNumber range
      liftIO . BSL8.putStrLn $ Aeson.encode collections
    GetFractions mAccessToken zipcode street houseNumber -> do
      for_ mAccessToken setAccessToken
      fractions <- getFractions zipcode street houseNumber
      liftIO . BSL8.putStrLn $ Aeson.encode fractions

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
