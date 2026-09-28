{-# OPTIONS_GHC -Wno-orphans #-}

module Recycle.Types.Orphans () where

import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Recycle.Types (DateRange (..), Range (..))
import Web.FormUrlEncoded
  ( Form (..),
    FromForm (..),
    ToForm (..),
    lookupUnique,
    parseUnique,
  )
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

instance FromForm DateRange where
  fromForm f =
    let lookupRange :: (FromHttpApiData a) => Either Text (Range a)
        lookupRange = do
          from <- parseUnique "f" f
          to <- parseUnique "t" f
          pure Range {..}
     in lookupUnique "drt" f >>= \case
          "absolute" -> AbsoluteDateRange <$> lookupRange
          "relative" -> RelativeDateRange <$> lookupRange
          t -> Left $ "Must be one of [absolute,relative]: " <> t

instance ToForm DateRange where
  toForm (AbsoluteDateRange Range {from, to}) =
    pairsToForm [("drt", "absolute"), ("f", toQueryParam from), ("t", toQueryParam to)]
  toForm (RelativeDateRange Range {from, to}) =
    pairsToForm [("drt", "relative"), ("f", toQueryParam from), ("t", toQueryParam to)]

pairsToForm :: [(Text, Text)] -> Form
pairsToForm = Form . HM.fromListWith (++) . map (\(k, v) -> (k, [v]))
