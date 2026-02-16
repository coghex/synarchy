{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.Types
    ( -- * Persistent hydrological features
      HydroFeature(..)
    , RiverParams(..)
    , RiverSegment(..)
    , GlacierParams(..)
    , LakeParams(..)
    , LakeSource(..)
      -- * Activity states
    , RiverActivity(..)
    , GlacierActivity(..)
      -- * Evolution events
    , HydroEvolution(..)
      -- * Regional climate
    , RegionalClimate(..)
    , WindBand(..)
    ) where

import UPrelude
import World.Base (GeoCoord(..), GeoFeatureId(..))

-----------------------------------------------------------
-- Regional Climate (computed per-region during timeline)
-----------------------------------------------------------

-- | Climate data for a single region (8×8 chunks = 128×128 tiles).
--   Computed from latitude, elevation, ocean proximity, and
--   continental position. Drives precipitation → rivers/glaciers.
data RegionalClimate = RegionalClimate
    { rcTemperature   ∷ !Float    -- ^ Mean annual temp (°C), from latitude + elevation
    , rcHumidity      ∷ !Float    -- ^ 0.0-1.0, moisture availability
    , rcPrecipitation ∷ !Float    -- ^ Annual precipitation (arbitrary units, 0.0-1.0)
    , rcWindDir       ∷ !Float    -- ^ Prevailing wind direction (radians)
    , rcWindSpeed     ∷ !Float    -- ^ Wind strength (0.0-1.0)
    , rcElevAvg       ∷ !Int      -- ^ Average elevation across region
    , rcOceanDist     ∷ !Int      -- ^ Distance to nearest ocean region (in regions)
    } deriving (Show, Eq)

-- | Latitude-based wind bands (Hadley cells).
--   Determines prevailing wind direction for moisture transport.
data WindBand
    = TradeWinds       -- ^ 0°-30° latitude: blow E→W
    | Westerlies       -- ^ 30°-60° latitude: blow W→E
    | PolarEasterlies  -- ^ 60°-90° latitude: blow E→W
    deriving (Show, Eq)

-----------------------------------------------------------
-- River System
-----------------------------------------------------------

-- | A river is a chain of segments from source to mouth.
--   Each segment is a straight line between two GeoCoords.
--   The river carves a valley along its path, with width
--   and depth proportional to flow accumulation.
--
--   Like volcanoes, rivers have IDs and evolve over ages:
--   they can branch, meander, dry up, dam into lakes, or
--   be captured by another river system.
data RiverParams = RiverParams
    { rpSourceRegion  ∷ !GeoCoord       -- ^ Headwaters (high elevation)
    , rpMouthRegion   ∷ !GeoCoord       -- ^ Where it reaches ocean/lake/basin
    , rpSegments      ∷ ![RiverSegment] -- ^ Ordered list of path segments
    , rpFlowRate      ∷ !Float          -- ^ Accumulated precipitation along path
    , rpMeanderSeed   ∷ !Word64         -- ^ Sub-seed for meander noise
    } deriving (Show, Eq)

-- | A single segment of a river between two waypoints.
--   At chunk generation time, each segment applies a
--   V-shaped valley carve as a pure elevation transform.
data RiverSegment = RiverSegment
    { rsStart      ∷ !GeoCoord   -- ^ Segment start (global tile coords)
    , rsEnd        ∷ !GeoCoord   -- ^ Segment end
    , rsWidth      ∷ !Int        -- ^ River channel width in tiles
    , rsValleyWidth ∷ !Int       -- ^ Total valley width (channel + slopes)
    , rsDepth      ∷ !Int        -- ^ Channel depth below surrounding terrain
    , rsFlowRate   ∷ !Float      -- ^ Flow at this segment (increases downstream)
    } deriving (Show, Eq)

data RiverActivity
    = Flowing          -- ^ Active river
    | Seasonal         -- ^ Flows only part of the year (dry climate)
    | DriedUp          -- ^ Former river, now a dry valley
    | Dammed           -- ^ Blocked, feeding a lake
    deriving (Show, Eq)

-----------------------------------------------------------
-- Glacier System
-----------------------------------------------------------

-- | Alpine glaciers flow downhill from cold high elevations.
--   They carve U-shaped valleys and push moraines.
--   The glacier border (polar ice sheet edge) is separate
--   from alpine glaciers — it advances/retreats based on
--   global temperature, pushing sediment and carving
--   wide flat valleys.
data GlacierParams = GlacierParams
    { glCenter      ∷ !GeoCoord   -- ^ Glacier origin (mountain peak or ice sheet edge)
    , glFlowDir     ∷ !Float      -- ^ Direction of flow (radians, downhill)
    , glLength      ∷ !Int        -- ^ How far the glacier extends (tiles)
    , glWidth       ∷ !Int        -- ^ Glacier width at widest point
    , glThickness   ∷ !Int        -- ^ Ice thickness (drives carving depth)
    , glCarveDepth  ∷ !Int        -- ^ U-valley depth
    , glMoraineSize ∷ !Int        -- ^ Sediment pile at terminus
    , glIsIceSheet  ∷ !Bool       -- ^ True = polar ice sheet edge, False = alpine
    } deriving (Show, Eq)

data GlacierActivity
    = Advancing        -- ^ Growing, carving deeper
    | Stable           -- ^ Holding position
    | Retreating       -- ^ Melting back, leaving moraine
    | Melted           -- ^ Gone, only the carved valley remains
    deriving (Show, Eq)

-----------------------------------------------------------
-- Lake System
-----------------------------------------------------------

-- | Lakes form when rivers are dammed, or when glaciers
--   carve basins, or in volcanic calderas.
--   A lake is defined by its center, the elevation of its
--   spillway (lowest rim point), and its source.
data LakeParams = LakeParams
    { lkCenter      ∷ !GeoCoord   -- ^ Lake center
    , lkRadius      ∷ !Int        -- ^ Approximate radius
    , lkSurface     ∷ !Int        -- ^ Water surface elevation (= spillway height)
    , lkDepth       ∷ !Int        -- ^ Max depth below surface
    , lkSource      ∷ !LakeSource -- ^ What created this lake
    } deriving (Show, Eq)

data LakeSource
    = DammedRiver !GeoFeatureId    -- ^ River was blocked
    | GlacialBasin !GeoFeatureId   -- ^ Glacier carved a basin
    | TectonicBasin                -- ^ Low point between plates
    | CalderaLake !GeoFeatureId    -- ^ Volcanic caldera filled
    deriving (Show, Eq)

-----------------------------------------------------------
-- Hydrological Feature (unified, like VolcanicFeature)
-----------------------------------------------------------

data HydroFeature
    = RiverFeature    !RiverParams
    | GlacierFeature  !GlacierParams
    | LakeFeature     !LakeParams
    deriving (Show, Eq)

-----------------------------------------------------------
-- Hydrological Evolution (like FeatureEvolution)
-----------------------------------------------------------

-- | How a hydrological feature changes between ages.
--   Each is a pure event that gets stored in GeoPeriod.gpEvents.
data HydroEvolution
    = RiverBranch           -- ^ River splits, new tributary
        { heBranchPoint  ∷ !GeoCoord
        , heBranchAngle  ∷ !Float
        , heBranchLength ∷ !Int
        , heChildId      ∷ !GeoFeatureId
        }
    | RiverMeander          -- ^ River shifts its path laterally
        { heMeanderSeed   ∷ !Word64
        , heMeanderAmount ∷ !Float   -- ^ How far the path shifts (0.0-1.0)
        }
    | RiverCapture          -- ^ One river captures another's headwaters
        { heCapturedId    ∷ !GeoFeatureId
        , heCapturePoint  ∷ !GeoCoord
        }
    | RiverDam              -- ^ River gets blocked, lake forms
        { heDamPoint      ∷ !GeoCoord
        , heLakeId        ∷ !GeoFeatureId
        , heDamHeight     ∷ !Int
        }
    | RiverDryUp            -- ^ Climate shift, river goes seasonal or dies
    | GlacierAdvance        -- ^ Glacier grows longer/wider
        { heAdvanceLength ∷ !Int
        , heAdvanceWidth  ∷ !Int
        }
    | GlacierRetreat        -- ^ Glacier melts back
        { heRetreatLength ∷ !Int
        , heMoraineDeposit ∷ !Int  -- ^ Terminal moraine left behind
        }
    | GlacierBranch         -- ^ Glacier tongue splits
        { heBranchPoint  ∷ !GeoCoord
        , heBranchAngle  ∷ !Float
        , heBranchLength ∷ !Int
        , heChildId      ∷ !GeoFeatureId
        }
    | GlacierMelt           -- ^ Glacier dies, leaves valley + moraine
        { heMoraineDep   ∷ !Int
        }
    | LakeDrain             -- ^ Lake drains (dam breaks or outlet erodes down)
        { heDrainRate     ∷ !Float
        }
    | LakeExpand            -- ^ Lake grows from more precipitation
        { heNewRadius     ∷ !Int
        , heNewSurface    ∷ !Int
        }
    deriving (Show, Eq)
