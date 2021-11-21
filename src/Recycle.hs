module Recycle
  ( main
  )
where

import           Network.HTTP.Client.TLS        ( newTlsManagerWith
                                                , tlsManagerSettings
                                                )
import           Servant.Client                 ( BaseUrl(..)
                                                , Scheme(..)
                                                , mkClientEnv
                                                )
import           Recycle.AppM

main :: IO ()
main = do
  httpManager <- newTlsManagerWith tlsManagerSettings
  let clientEnv =
        mkClientEnv httpManager $ BaseUrl Https "recycleapp.be" 443 ""
  let env = Env { .. }
  flip runRecycle env $ pure ()
