{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
module World.Region.Types
    ( RegionCoord(..)
    , regionSize
    , RegionalData(..)
    , emptyRegionalData
    , Region(..)
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM

-- | Coarse spatial grid coordinate.
--   Each region covers regionSize x regionSize chunks.
data RegionCoord = RegionCoord !Int !Int
    deriving (Show, Eq, Ord, Generic, Serialize, NFData)

instance Hashable RegionCoord where
    hashWithSalt s (RegionCoord x y) = s `hashWithSalt` x `hashWithSalt` y

-- | How many chunks per region side.
regionSize ∷ Int
regionSize = 8

-- | Regional climate and geological data.
data RegionalData = RegionalData
    { rdTemperature ∷ !(HM.HashMap RegionCoord Float)
    } deriving (Show, Eq)

emptyRegionalData ∷ RegionalData
emptyRegionalData = RegionalData
    { rdTemperature = HM.empty
    }

data Region = Region
    { regCoord    ∷ !RegionCoord
    , regOcean    ∷ !Bool         -- ^ Does this region contain any ocean chunks?
    , regSeaLevel ∷ !Int          -- ^ Base sea level (for future tidal variation)
    , regAvgElev  ∷ !Int          -- ^ Average elevation across region
    } deriving (Show, Eq)
