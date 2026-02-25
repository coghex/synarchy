{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module World.Region.Types
    ( RegionCoord(..)
    , regionSize
    , RegionalData(..)
    , emptyRegionalData
    , Region(..)
    , chunkToRegion
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import World.Chunk.Types (ChunkCoord(..))

-- | Coarse spatial grid coordinate.
--   Each region covers regionSize x regionSize chunks.
data RegionCoord = RegionCoord !Int !Int
    deriving (Show, Eq, Ord, Generic, Serialize)

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

chunkToRegion ∷ ChunkCoord → RegionCoord
chunkToRegion (ChunkCoord cx cy) =
    RegionCoord (floorDiv' cx regionSize) (floorDiv' cy regionSize)
  where
    floorDiv' a b = floor (fromIntegral a / fromIntegral b ∷ Double)
