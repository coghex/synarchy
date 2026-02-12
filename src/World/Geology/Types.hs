{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Types
    ( GeoModification(..)
    , VolcanoEra(..)
    , noModification
    , CraterEra(..)
    , TimelineBuildState(..)
    , allocFeatureId
    , addPeriod
    , registerFeature
    , updateFeature
    -- * GeoState
    , GeoState(..)
    , RegionCoord(..)
    , RegionalData(..)
    , emptyRegionalData
    , initGeoState
    , lookupRegionTemp
    , modifyRegionTemp
    , modifyAllRegionTemp
    , globalToRegion
    -- * Date tracking
    , GeoDate(..)
    , advanceGeoDate
    , geoDateYears
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable(..))
import World.Types
import World.Plate (TectonicPlate(..), twoNearestPlates, isBeyondGlacier)

-----------------------------------------------------------
-- GeoModification
-----------------------------------------------------------

data GeoModification = GeoModification
    { gmElevDelta   ∷ !Int
    , gmMaterialOverride ∷ !(Maybe Word8)
    } deriving (Show)

noModification ∷ GeoModification
noModification = GeoModification 0 Nothing

-----------------------------------------------------------
-- Volcano Era
-----------------------------------------------------------

data VolcanoEra
    = VolcanoEra_Boundary
    | VolcanoEra_Hotspot
    deriving (Show, Eq)

-----------------------------------------------------------
-- Crater Era
-----------------------------------------------------------

data CraterEra
    = CraterEra_Primordial
    | CraterEra_Late
    deriving (Show, Eq)

-----------------------------------------------------------
-- Date Tracking
-----------------------------------------------------------

-- | Geological date in millions of years from world origin.
--   Accumulates forward through the timeline build.
--   At the end of the build, this represents the total
--   geological age of the world.
data GeoDate = GeoDate
    { gdMillionYears ∷ !Float
    } deriving (Show, Eq)

-- | Advance the date by a number of million years.
advanceGeoDate ∷ Float → GeoDate → GeoDate
advanceGeoDate my (GeoDate current) = GeoDate (current + my)

-- | Get total years (for display or frequency calculations).
geoDateYears ∷ GeoDate → Float
geoDateYears (GeoDate my) = my

-----------------------------------------------------------
-- Regional Data
-----------------------------------------------------------

-- | Coarse spatial grid coordinate.
--   Each region covers regionSize x regionSize chunks.
data RegionCoord = RegionCoord !Int !Int
    deriving (Show, Eq, Ord)

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

-- | Convert global tile coords to region coords.
globalToRegion ∷ Int → Int → Int → RegionCoord
globalToRegion worldSize gx gy =
    let chunkX = gx `div` 16
        chunkY = gy `div` 16
        -- Offset so that negative coords map correctly
        halfChunks = worldSize `div` 2
        rx = (chunkX + halfChunks) `div` regionSize
        ry = (chunkY + halfChunks) `div` regionSize
    in RegionCoord rx ry

lookupRegionTemp ∷ RegionCoord → RegionalData → Float
lookupRegionTemp rc rd = HM.lookupDefault 15.0 rc (rdTemperature rd)

modifyRegionTemp ∷ RegionCoord → (Float → Float) → RegionalData → RegionalData
modifyRegionTemp rc f rd = rd
    { rdTemperature = HM.adjust f rc (rdTemperature rd) }

modifyAllRegionTemp ∷ (Float → Float) → RegionalData → RegionalData
modifyAllRegionTemp f rd = rd
    { rdTemperature = HM.map f (rdTemperature rd) }

-----------------------------------------------------------
-- GeoState
-----------------------------------------------------------

-- | Global and regional geological state.
--   Threaded through the entire timeline build.
--   Modified by events, read by subsequent builders
--   to determine what happens next.
data GeoState = GeoState
    { -- Global values
      gsCO2       ∷ !Float          -- ^ Atmospheric CO2 (relative, 1.0 = baseline)
    , gsDate      ∷ !GeoDate        -- ^ Current geological date
      -- Regional values
    , gsRegional  ∷ !RegionalData
    } deriving (Show, Eq)

-- | Initialize GeoState from plate data.
--   Temperature varies by latitude: hot at equator, cold at poles.
initGeoState ∷ Word64 → Int → [TectonicPlate] → GeoState
initGeoState seed worldSize plates =
    let halfChunks = worldSize `div` 2
        regionsPerSide = worldSize `div` regionSize
        halfRegions = regionsPerSide `div` 2

        -- Build temperature map from latitude
        tempEntries =
            [ (RegionCoord rx ry, baseTemp ry)
            | rx ← [0 .. regionsPerSide - 1]
            , ry ← [0 .. regionsPerSide - 1]
            ]

        -- Temperature from latitude
        -- ry=0 is one pole, ry=regionsPerSide-1 is the other
        -- Middle is equator
        baseTemp ry =
            let latRatio = abs (fromIntegral (ry - halfRegions))
                         / fromIntegral halfRegions ∷ Float
            in 30.0 - 60.0 * latRatio  -- +30 at equator, -30 at poles

    in GeoState
        { gsCO2      = 1.0
        , gsDate     = GeoDate 0.0
        , gsRegional = RegionalData
            { rdTemperature = HM.fromList tempEntries
            }
        }

-----------------------------------------------------------
-- Timeline Build State & Helpers
-----------------------------------------------------------

data TimelineBuildState = TimelineBuildState
    { tbsFeatures   ∷ ![PersistentFeature]
    , tbsNextId     ∷ !Int
    , tbsPeriods    ∷ ![GeoPeriod]
    , tbsPeriodIdx  ∷ !Int
    , tbsGeoState   ∷ !GeoState
    }

allocFeatureId ∷ TimelineBuildState → (GeoFeatureId, TimelineBuildState)
allocFeatureId tbs =
    let fid = GeoFeatureId (tbsNextId tbs)
    in (fid, tbs { tbsNextId = tbsNextId tbs + 1 })

addPeriod ∷ GeoPeriod → TimelineBuildState → TimelineBuildState
addPeriod period tbs = tbs
    { tbsPeriods   = period : tbsPeriods tbs
    , tbsPeriodIdx = tbsPeriodIdx tbs + 1
    }

registerFeature ∷ PersistentFeature → TimelineBuildState → TimelineBuildState
registerFeature pf tbs = tbs
    { tbsFeatures = pf : tbsFeatures tbs
    }

updateFeature ∷ GeoFeatureId → (PersistentFeature → PersistentFeature)
              → TimelineBuildState → TimelineBuildState
updateFeature fid f tbs = tbs
    { tbsFeatures = map (\pf → if pfId pf ≡ fid then f pf else pf)
                        (tbsFeatures tbs)
    }
