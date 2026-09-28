{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Data.Aeson as Aeson
import Data.Aeson.Extra.SingObject (SingObject (..))
import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed (embedFile)
import Data.Either (isRight)
import Data.Proxy (Proxy (..))
import Data.Time
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Numeric.Natural (Natural)
import Recycle.Class (HasRecycleClient (..), HasTime (..))
import Recycle.Ics.API (ICalendar, RecycleIcsAPI, pRecycleIcsAPI)
import Recycle.Ics.Server (recycleIcsServer)
import Recycle.Ics.Types (FractionEncoding (..), Filter (..), Reminder (..), TodoDue (..))
import Recycle.Types
import Servant.API
import Servant.Client
import Network.Wai (Application)
import Servant.Server (Handler, hoistServer, serve)
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Time as Time
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck

-- ---------------------------------------------------------------------------
-- Mock monad
-- ---------------------------------------------------------------------------

newtype MockM a = MockM (IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runMockM :: MockM a -> IO a
runMockM (MockM io) = io

instance HasRecycleClient MockM where
  searchZipcodes _ = pure mockZipcodes
  searchStreets _ _ = pure mockStreets
  getFractions _ _ _ = pure mockFractions
  getCollections _ _ _ _ = pure mockCollections

mockToHandler :: MockM a -> Handler a
mockToHandler = liftIO . runMockM

-- ---------------------------------------------------------------------------
-- Mock data (decoded from fixtures at compile time)
-- ---------------------------------------------------------------------------

decodeFixture :: forall a. Aeson.FromJSON a => BSL.ByteString -> [a]
decodeFixture bs = case Aeson.eitherDecode @(SingObject "items" [a]) bs of
  Right (SingObject xs) -> xs
  Left err -> error $ "fixture decode failed: " ++ err

mockZipcodes :: [FullZipcode]
mockZipcodes = decodeFixture $ BSL.fromStrict $(embedFile "test/responses/zipcodes.json")

mockStreets :: [Street]
mockStreets = decodeFixture $ BSL.fromStrict $(embedFile "test/responses/streets.json")

mockFractions :: [Fraction]
mockFractions = decodeFixture $ BSL.fromStrict $(embedFile "test/responses/fractions.json")

mockCollections :: [CollectionEvent]
mockCollections = decodeFixture $ BSL.fromStrict $(embedFile "test/responses/collections.json")

-- ---------------------------------------------------------------------------
-- Test application
-- ---------------------------------------------------------------------------

testApp :: IO Application
testApp =
  pure $
    serve pRecycleIcsAPI
      . hoistServer pRecycleIcsAPI mockToHandler
      $ recycleIcsServer "/tmp"

-- ---------------------------------------------------------------------------
-- servant-client functions
-- ---------------------------------------------------------------------------

( (searchZipcodeC :<|> searchStreetC :<|> fractionsC :<|> generateC) :<|> _rawC
  ) = client pRecycleIcsAPI

searchZipcodeC ::
  SearchQuery Natural ->
  ClientM (Union '[WithStatus 200 [FullZipcode]])

searchStreetC ::
  ZipcodeId ->
  SearchQuery T.Text ->
  ClientM (Union '[WithStatus 200 [Street]])

fractionsC ::
  ZipcodeId ->
  StreetId ->
  HouseNumber ->
  ClientM (Union '[WithStatus 200 [Fraction]])

generateC ::
  DateRange ->
  LangCode ->
  FractionEncoding ->
  ZipcodeId ->
  StreetId ->
  HouseNumber ->
  Filter ->
  ClientM (Union '[WithStatus 200 BSL.ByteString])

-- ---------------------------------------------------------------------------
-- Client runner
-- ---------------------------------------------------------------------------

runC :: Int -> ClientM a -> IO (Either ClientError a)
runC port action = do
  manager <- newManager defaultManagerSettings
  runClientM action (mkClientEnv manager (BaseUrl Http "localhost" port ""))

-- ---------------------------------------------------------------------------
-- Arbitrary instances for query parameters
-- ---------------------------------------------------------------------------

instance Arbitrary (SearchQuery Natural) where
  arbitrary = SearchQuery . fromIntegral <$> chooseInt (0, 9999)

instance Arbitrary ZipcodeId where
  arbitrary = elements ["3000-24062", "1000-21004", "9000-44021"]

instance Arbitrary (SearchQuery T.Text) where
  arbitrary = SearchQuery <$> elements ["Grote Markt", "Kerkstraat", "Laan"]

instance Arbitrary StreetId where
  arbitrary =
    elements
      [ "https://data.vlaanderen.be/id/straatnaam-34637",
        "https://data.vlaanderen.be/id/straatnaam-12345"
      ]

instance Arbitrary HouseNumber where
  arbitrary = HouseNumber . fromIntegral <$> chooseInt (1, 200)

instance Arbitrary LangCode where
  arbitrary = elements [EN, NL, FR, DE]

instance Arbitrary Day where
  arbitrary =
    fromGregorian
      <$> fmap fromIntegral (chooseInt (2020, 2030))
      <*> chooseInt (1, 12)
      <*> chooseInt (1, 28)

instance Arbitrary TimeOfDay where
  arbitrary = TimeOfDay <$> chooseInt (0, 23) <*> chooseInt (0, 59) <*> pure 0

instance Arbitrary Reminder where
  arbitrary = Reminder <$> chooseInt (0, 7) <*> chooseInt (0, 23) <*> chooseInt (0, 59)

instance Arbitrary TodoDue where
  arbitrary =
    oneof
      [ TodoDueDate . fromIntegral <$> chooseInt (-7, 7),
        TodoDueDateTime . fromIntegral <$> chooseInt (-7, 7) <*> arbitrary
      ]

instance Arbitrary FractionEncoding where
  arbitrary =
    oneof
      [ EncodeFractionAsVEvent
          <$> (Range <$> arbitrary <*> arbitrary)
          <*> listOf arbitrary,
        EncodeFractionAsVTodo <$> arbitrary
      ]

instance Arbitrary DateRange where
  arbitrary =
    oneof
      [ AbsoluteDateRange <$> (Range <$> arbitrary <*> arbitrary),
        RelativeDateRange <$> (Range <$> fmap fromIntegral (chooseInt (-30, 0)) <*> fmap fromIntegral (chooseInt (0, 365)))
      ]

instance Arbitrary FractionId where
  arbitrary =
    elements
      [ "5e4e84d1bab65e9819d714d2",
        "5ed7a542a2124463cc8814e6"
      ]

instance Arbitrary Filter where
  arbitrary = Filter <$> arbitrary <*> arbitrary

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

fromUnion200 :: Union '[WithStatus 200 a] -> a
fromUnion200 (Z (I (WithStatus a))) = a
fromUnion200 _ = error "impossible: empty union"

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

main :: IO ()
main = testWithApplication testApp $ \port ->
  hspec $
    modifyMaxSuccess (const 20) $
      describe "v1 API backwards compatibility" $ do
        describe "GET /api/search-zipcode" $ do
          prop "returns 200 for any numeric query" $
            \q -> ioProperty $ do
              result <- runC port (searchZipcodeC q)
              pure $ isRight result

          it "response body is a list of FullZipcode" $ do
            result <- runC port (searchZipcodeC (SearchQuery 3000))
            case result of
              Left err -> expectationFailure (show err)
              Right u -> fromUnion200 u `shouldBe` mockZipcodes

        describe "GET /api/search-street" $ do
          prop "returns 200 for any zipcode and query" $
            \z q -> ioProperty $ do
              result <- runC port (searchStreetC z q)
              pure $ isRight result

          it "response body is a list of Street" $ do
            result <- runC port (searchStreetC "3000-24062" "Andreas")
            case result of
              Left err -> expectationFailure (show err)
              Right u -> fromUnion200 u `shouldBe` mockStreets

        describe "GET /api/fractions" $ do
          prop "returns 200 for any address" $
            \z s hn -> ioProperty $ do
              result <- runC port (fractionsC z s hn)
              pure $ isRight result

          it "response body is a list of Fraction" $ do
            result <- runC port (fractionsC "3000-24062" "https://data.vlaanderen.be/id/straatnaam-34637" (HouseNumber 1))
            case result of
              Left err -> expectationFailure (show err)
              Right u -> fromUnion200 u `shouldBe` mockFractions

        describe "GET /api/generate" $ do
          prop "returns 200 for any valid parameter combination" $
            \dr lc fe z s hn f -> ioProperty $ do
              result <- runC port (generateC dr lc fe z s hn f)
              pure $ isRight result

          prop "ICS output always begins with BEGIN:VCALENDAR" $
            \dr lc fe z s hn f -> ioProperty $ do
              result <- runC port (generateC dr lc fe z s hn f)
              pure $ case result of
                Left _ -> False
                Right u -> BSL.isPrefixOf "BEGIN:VCALENDAR" (fromUnion200 u)

          prop "ICS output always ends with END:VCALENDAR" $
            \dr lc fe z s hn f -> ioProperty $ do
              result <- runC port (generateC dr lc fe z s hn f)
              pure $ case result of
                Left _ -> False
                Right u -> BSL.isSuffixOf "END:VCALENDAR\r\n" (fromUnion200 u)
