{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Recycle.Ics.API
  ( RecycleIcsAPI,
    pRecycleIcsAPI,
    ICalendar,
  )
where

import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text (Text)
import Network.HTTP.Media.MediaType ((//))
import Numeric.Natural (Natural)
import Recycle.Ics.Types
import Recycle.Types
import Servant.API
import Servant.API.QueryParamForm

type RecycleIcsAPI =
  ( "api"
      :> ( "search-zipcode"
             :> QueryParam' '[Required] "q" (SearchQuery Natural)
             :> UVerb 'GET '[JSON] '[WithStatus 200 [FullZipcode]]
             :<|> "search-street"
               :> QueryParam' '[Required] "zipcode" ZipcodeId
               :> QueryParam' '[Required] "q" (SearchQuery Text)
               :> UVerb 'GET '[JSON] '[WithStatus 200 [Street]]
             :<|> "generate"
               :> QueryParamForm DateRange
               :> QueryParam' '[Required] "lc" LangCode
               :> QueryParamForm FractionEncoding
               :> QueryParam' '[Required] "z" ZipcodeId
               :> QueryParam' '[Required] "s" StreetId
               :> QueryParam' '[Required] "hn" HouseNumber
               :> UVerb 'GET '[ICalendar] '[WithStatus 200 BSL.ByteString]
         )
      :<|> Raw
  )

data ICalendar

instance Accept ICalendar where
  contentType Proxy = "text" // "calendar"

instance MimeRender ICalendar BSL.ByteString where
  mimeRender Proxy = id

pRecycleIcsAPI :: Proxy RecycleIcsAPI
pRecycleIcsAPI = Proxy
