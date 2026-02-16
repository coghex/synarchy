module World.Base
  ( GeoCoord(..)
  , GeoFeatureId(..)
  ) where

import UPrelude


-- | Global tile coordinate for event placement.
data GeoCoord = GeoCoord !Int !Int
    deriving (Show, Eq)

-- | Unique identifier for a geological feature that persists
--   across geological periods.
newtype GeoFeatureId = GeoFeatureId Int
    deriving (Show, Eq, Ord)

