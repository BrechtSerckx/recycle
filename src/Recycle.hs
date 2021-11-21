module Recycle
  ( main
  )
where

import           Network.HTTP.Client.TLS        ( newTlsManagerWith
                                                , tlsManagerSettings
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Servant.Client                 ( BaseUrl(..)
                                                , Scheme(..)
                                                , mkClientEnv
                                                )
import           Colog.Core                     ( LogAction(..) )
import           Colog.Message
import qualified Data.Text                     as T

import           Recycle.AppM

main :: IO ()
main = do
  httpManager <- newTlsManagerWith tlsManagerSettings
  let clientEnv =
        mkClientEnv httpManager $ BaseUrl Https "recycleapp.be" 443 ""
      logAction = LogAction $ liftIO . putStrLn . T.unpack . fmtMessage
  let env = Env { .. }
  flip runRecycle env $ pure ()
