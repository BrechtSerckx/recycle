module Main
  ( main,
  )
where

import qualified Colog
import Control.Exception (SomeException, displayException, throwIO, try)
import Data.IORef (newIORef)
import Data.String (fromString)
import Data.Time (addDays, getCurrentTime, utctDay)
import Network.HTTP.Client.TLS
  ( newTlsManagerWith,
    tlsManagerSettings,
  )
import Recycle.AppM (Env (..), RecycleM, runRecycle)
import qualified Recycle.Class as Recycle
import Recycle.Types
import Servant.Client
  ( BaseUrl (..),
    Scheme (..),
    mkClientEnv,
  )
import qualified System.Environment as Env
import Test.Hspec

-- Known test address: Andreas Vesaliusstraat 1, 3000 Leuven
-- (matches fixture data in test/responses/)
testZipcode :: ZipcodeId
testZipcode = "3000-24062"

testStreet :: StreetId
testStreet = "https://data.vlaanderen.be/id/straatnaam-34637"

testHouseNumber :: HouseNumber
testHouseNumber = HouseNumber 1

main :: IO ()
main = do
  mConsumer <- Env.lookupEnv "RECYCLE_ICS_CONSUMER"
  mSecret <- Env.lookupEnv "RECYCLE_ICS_SECRET"
  case (fromString <$> mConsumer, fromString <$> mSecret) of
    (Just consumer, Just secret) -> do
      env <- mkTestEnv consumer secret
      hspec $ apiSpec env
    _ ->
      hspec $
        it "API tests" $
          pendingWith "set RECYCLE_ICS_CONSUMER and RECYCLE_ICS_SECRET to run API tests"

mkTestEnv :: Consumer -> AuthSecret -> IO Env
mkTestEnv consumer authSecret = do
  httpManager <- newTlsManagerWith tlsManagerSettings
  let clientEnv =
        mkClientEnv httpManager $
          BaseUrl Https "api.fostplus.be" 443 ""
      logAction =
        Colog.cfilter
          ((>= Colog.Warning) . Colog.msgSeverity)
          Colog.simpleMessageAction
  authResult <- newIORef Nothing
  pure Env {..}

run :: Env -> RecycleM a -> IO a
run env act = runRecycle act env

apiSpec :: Env -> Spec
apiSpec env = describe "Recycle API" $ do
  eAuth <- runIO $ try @SomeException (run env Recycle.getAuthResult)

  let requireAuth :: IO () -> IO ()
      requireAuth action = case eAuth of
        Left err -> pendingWith $ displayException err
        Right _ -> action

  describe "Authentication" $ do
    it "fetches a valid access token" $
      case eAuth of
        Left err -> throwIO err
        Right AuthResult {accessToken = AccessToken tok} -> tok `shouldNotBe` ""

  describe "Zip codes" $ do
    it "returns results for a numeric zip code search" $ requireAuth $ do
      zipcodes <- run env $ Recycle.searchZipcodes (Just $ SearchQuery 3000)
      zipcodes `shouldNotBe` []

    it "finds Leuven (3000-24062) by zip code" $ requireAuth $ do
      zipcodes <- run env $ Recycle.searchZipcodes (Just $ SearchQuery 3000)
      map (.id) zipcodes `shouldContain` [testZipcode]

  describe "Streets" $ do
    it "returns streets matching a query within a zip code" $ requireAuth $ do
      streets <-
        run env $
          Recycle.searchStreets
            (Just testZipcode)
            (Just $ SearchQuery "Grote Markt")
      streets `shouldNotBe` []

  describe "Fractions" $ do
    it "returns fractions for a known address" $ requireAuth $ do
      fractions <-
        run env $
          Recycle.getFractions testZipcode testStreet testHouseNumber
      fractions `shouldNotBe` []

  describe "Collections" $ do
    it "returns collections for a known address over the next 90 days" $ requireAuth $ do
      today <- utctDay <$> getCurrentTime
      let range = Range {from = today, to = addDays 90 today}
      collections <-
        run env $
          Recycle.getCollections testZipcode testStreet testHouseNumber range
      collections `shouldNotBe` []
