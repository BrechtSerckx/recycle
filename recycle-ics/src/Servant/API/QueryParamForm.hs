{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.API.QueryParamForm
  ( QueryParamForm,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.Kind (Type)
import Data.Proxy
import qualified Data.Text.Encoding as TE
import Network.Wai
import Servant.API
import Servant.Client.Core (HasClient (..))
import Servant.Client.Core.Request (appendToQueryString)
import Servant.Server
import Servant.Server.Internal
import Web.FormUrlEncoded (Form (..), ToForm (..))
import qualified Web.FormUrlEncoded as Form

data QueryParamForm (a :: Type)

instance
  ( Form.FromForm a,
    HasServer api context
  ) =>
  HasServer (QueryParamForm a :> api) context
  where
  type ServerT (QueryParamForm a :> api) m = a -> ServerT api m
  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s
  route Proxy context subserver =
    let parseParamForm :: Request -> DelayedIO a
        parseParamForm req =
          let rawQS = rawQueryString req
              queryString = case BS.length rawQS of
                0 -> ""
                _ -> BSL.drop 1 $ BSL.fromStrict rawQS
           in case Form.urlDecodeAsForm queryString of
                Left err ->
                  delayedFailFatal
                    err400
                      { errBody =
                          BSL.fromStrict
                            . TE.encodeUtf8
                            $ "Error: parsing query parameter form failed. "
                              <> err
                      }
                Right a -> pure a

        delayed =
          addParameterCheck subserver . withRequest $ \req -> parseParamForm req
     in route (Proxy :: Proxy api) context delayed

instance
  (ToForm a, HasClient m api) =>
  HasClient m (QueryParamForm a :> api)
  where
  type Client m (QueryParamForm a :> api) = a -> Client m api
  clientWithRoute pm _ req a =
    clientWithRoute pm (Proxy :: Proxy api) $
      foldr
        (\(k, v) r -> appendToQueryString k (Just (TE.encodeUtf8 v)) r)
        req
        (HM.toList (unForm (toForm a)) >>= \(k, vs) -> map (k,) vs)
  hoistClientMonad pm _ nt s = hoistClientMonad pm (Proxy :: Proxy api) nt . s
