{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module World.Base
  ( GeoCoord(..)
  , GeoFeatureId(..)
  ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Data.Hashable (Hashable)
import Control.DeepSeq (NFData(..))


-- | Global tile coordinate for event placement.
data GeoCoord = GeoCoord !Int !Int
    deriving (Show, Eq, Ord, Generic, Serialize, Hashable)

instance NFData GeoCoord where
    rnf (GeoCoord x y) = rnf x `seq` rnf y `seq` ()

-- | Unique identifier for a geological feature that persists
--   across geological periods.
newtype GeoFeatureId = GeoFeatureId Int
    deriving (Show, Eq, Ord, Generic, Serialize, Hashable, NFData)

