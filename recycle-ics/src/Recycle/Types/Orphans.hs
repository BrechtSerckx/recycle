{-# OPTIONS_GHC -Wno-orphans #-}

module Recycle.Types.Orphans () where

import Data.Text (Text)
import Recycle.Types (DateRange (..), Range (..))
import Web.FormUrlEncoded
  ( FromForm (..),
    lookupUnique,
    parseUnique,
  )
import Web.HttpApiData (FromHttpApiData (..))

instance FromForm DateRange where
  fromForm f =
    let lookupRange :: FromHttpApiData a => Either Text (Range a)
        lookupRange = do
          rangeFrom <- parseUnique "f" f
          rangeTo <- parseUnique "t" f
          pure Range {..}
     in lookupUnique "drt" f >>= \case
          "absolute" -> AbsoluteDateRange <$> lookupRange
          "relative" -> RelativeDateRange <$> lookupRange
          t -> Left $ "Must be one of [absolute,relative]: " <> t
