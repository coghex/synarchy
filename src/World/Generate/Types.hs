{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate.Types
    ( WorldGenParams(..)
    , defaultWorldGenParams
    ) where

import UPrelude
import qualified Data.HashSet as HS
import World.Plate.Types (TectonicPlate(..))
import World.Time.Types
    ( CalendarConfig(..)
    , defaultCalendarConfig
    , SunConfig(..)
    , defaultSunConfig
    , MoonConfig(..)
    , defaultMoonConfig
    )
import World.Geology.Timeline.Types (GeoTimeline(..), emptyTimeline)
import World.Ocean.Types (OceanMap)

-- | Pure, serializable world generation parameters.
--   Same params + same ChunkCoord = same Chunk, always.
data WorldGenParams = WorldGenParams
    { wgpSeed       ∷ !Word64
    , wgpWorldSize  ∷ !Int     -- ^ World size in chunks (e.g. 64 → 64×64 chunks)
    , wgpPlateCount ∷ !Int     -- ^ Number of tectonic plates (for worldgen)
    , wgpPlates     ∷ ![TectonicPlate] -- ^ Pre-generated plate data for deterministic worldgen
    , wgpCalender   ∷ !CalendarConfig  -- ^ Calendar configuration for time/date calculations
    , wgpSunConfig   ∷ !SunConfig       -- ^ Sun configuration for time-of-day lighting
    , wgpMoonConfig  ∷ !MoonConfig      -- ^ Moon configuration for lunar phases
    , wgpGeoTimeline ∷ !GeoTimeline      -- ^ Geological timeline for terrain evolution
    , wgpOceanMap   ∷ !OceanMap         -- ^ Pre-generated ocean map for worldgen
    } deriving (Show, Eq)

defaultWorldGenParams ∷ WorldGenParams
defaultWorldGenParams = WorldGenParams
    { wgpSeed      = 42
    , wgpWorldSize = 128
    , wgpPlateCount = 10
    , wgpPlates = []
    , wgpCalender = defaultCalendarConfig
    , wgpSunConfig = defaultSunConfig
    , wgpMoonConfig = defaultMoonConfig
    , wgpGeoTimeline = emptyTimeline
    , wgpOceanMap = HS.empty
    }
