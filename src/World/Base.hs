module World.Base
  ( GeoCoord(..)
  , GeoFeatureId(..)
  ) where

import UPrelude
import Control.DeepSeq (NFData(..))


-- | Global tile coordinate for event placement.
data GeoCoord = GeoCoord !Int !Int
    deriving (Show, Eq)

instance NFData GeoCoord where
    rnf (GeoCoord x y) = rnf x `seq` rnf y `seq` ()

-- | Unique identifier for a geological feature that persists
--   across geological periods.
newtype GeoFeatureId = GeoFeatureId Int
    deriving (Show, Eq, Ord)

