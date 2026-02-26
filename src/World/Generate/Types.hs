{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module World.Generate.Types
    ( WorldGenParams(..)
    , defaultWorldGenParams
    ) where

import UPrelude hiding (get)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize(..))
import Data.Hashable (Hashable)
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
import World.Weather.Types (ClimateParams, ClimateState
                           , defaultClimateParams, initClimateState)

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
    , wgpClimateParams ∷ !ClimateParams   -- ^ Climate parameters
    , wgpClimateState ∷ !ClimateState     -- ^ Initial climate state
    } deriving (Show, Eq, Generic, Serialize, NFData)
instance (Serialize a, Eq a, Hashable a)
    ⇒ Serialize (HS.HashSet a) where
    put = put . HS.toList
    get = HS.fromList <$> get

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
    , wgpClimateParams = defaultClimateParams
    , wgpClimateState = initClimateState 128
    }
