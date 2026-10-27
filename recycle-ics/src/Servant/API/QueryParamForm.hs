{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.API.QueryParamForm
  ( QueryParamForm,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Kind (Type)
import Data.Proxy
import qualified Data.Text as Text
import Data.Typeable (Typeable, typeRep)
import Network.Wai
import Servant.API
import Servant.Server
import Servant.Server.Internal
import qualified Web.FormUrlEncoded as Form

data QueryParamForm (a :: Type)

instance
  ( Form.FromForm a,
    Typeable a,
    HasServer api context,
    HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
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
                Left err -> delayedFailFatal (formatError req err)
                Right a -> pure a

        -- Same mechanism Servant's own QueryParam'/Header/Capture decoding
        -- uses: pull the (possibly user-supplied, else default) formatter
        -- for this kind of failure out of the request context, rather than
        -- hardcoding a plain-text ServerError here.
        formatError :: Request -> Text.Text -> ServerError
        formatError req err =
          urlParseErrorFormatter
            (getContextEntry (mkContextWithErrorFormatter context))
            (typeRep (Proxy :: Proxy a))
            req
            ("Error parsing query parameter form: " <> Text.unpack err)

        delayed =
          addParameterCheck subserver . withRequest $ \req -> parseParamForm req
     in route (Proxy :: Proxy api) context delayed
