module Recycle
  ( main
  )
where

import           Control.Monad.IO.Class         ( liftIO )
import           Data.IORef                     ( newIORef )
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
import           Recycle.Types
import           Recycle.AppM

main :: IO ()
main = do
  httpManager <- newTlsManagerWith tlsManagerSettings
  let
    clientEnv = mkClientEnv httpManager $ BaseUrl Https "recycleapp.be" 443 ""
    logAction = LogAction $ liftIO . putStrLn . T.unpack . fmtMessage
    consumer  = Consumer "recycleapp.be"
    authSecret =
      AuthSecret
        "Crgja3EGWe8jdapyr4EEoMBgZACYYjRRcRpaMQrLDW9HJBvmgkfGQyYqLgeXPavAGvnJqkV87PBB2b8zx43q46sUgzqio4yRZbABhtKeagkVKypTEDjKfPgGycjLyJTtLHYpzwJgp4YmmCuJZN9ZmJY8CGEoFs8MKfdJpU9RjkEVfngmmk2LYD4QzFegLNKUbcCeAdEW"

  authResult <- newIORef Nothing
  let env = Env { .. }
  flip runRecycle env $ do

    AuthResult accessToken _ <- getAuthResult
    liftIO . putStrLn $ "Access Token: " <> show accessToken

    zipcodes <- searchZipcodes (Just "3000")
    zipcode  <- case zipcodes of
      []    -> error "No zip codes found"
      x : _ -> pure x

    liftIO . putStrLn $ "Zip Code: " <> show zipcode
