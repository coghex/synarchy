{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | Compatibility entry point for the geology timeline's types. The
--   actual declarations live in the focused submodules below; this
--   module re-exports their full surface so existing call sites keep
--   working unqualified, alongside the timeline records (GeoPeriod /
--   GeoTimeline and friends) that live here directly.
module World.Geology.Timeline.Types
    ( GeoScale(..)
    , TimelineParams(..)
    , defaultTimelineParams
    , GeoPeriod(..)
    , GeoTimeline(..)
    , emptyTimeline
    , EventBBox(..)
    , noBBox
    , eventBBox
    , featureShapeBBox
    , volcanicFeatureBBox
    , glacierBBox
    , hydroFeatureBBox
    , hydroEvolutionBBox
    , tileInBBoxWrapped
    , bboxOverlapsChunk
    , tagEventsWithBBox
    , GeoEvent(..)
    , isRiverCarveEvent
    , OreSheetParams(..)
    , RiverSegmentCarve(..)
    , RiverDeltaParams(..)
    , FeatureShape(..)
    , featureShapeTag
    , FeatureActivity(..)
    , FeatureEvolution(..)
    , CraterParams(..)
    , VolcanicFeature(..)
    , ShieldParams(..)
    , CinderConeParams(..)
    , LavaDomeParams(..)
    , CalderaParams(..)
    , FissureParams(..)
    , LavaTubeParams(..)
    , SuperVolcanoParams(..)
    , HydrothermalParams(..)
    , LandslideParams(..)
    , FloodParams(..)
    , ErosionParams(..)
    , defaultErosionParams
    , VolcanicActivity(..)
    , PersistentFeature(..)
    , explodeRiverEvent
    ) where

import UPrelude hiding (get)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize(..))
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Fluid.Types (IceLevelGrid(..))
import World.Fluid.Lake.Types (WorldLakes, emptyWorldLakes)
import World.Fluid.Seabed.Types (SeabedTable, emptySeabedTable)
import World.Fluid.OceanMask (WorldOceanMask, emptyWorldOceanMask)
import World.Geology.Coastal.Types (CoastalTable, emptyCoastalTable)
import World.Fluid.River.Types (WorldRivers, emptyWorldRivers)
import World.Geology.Ore.Types (WorldOreDeposits, emptyWorldOreDeposits)
import World.Weather.Types (ClimateCoord(..))
import World.Geology.Timeline.Event
    ( GeoEvent(..)
    , isRiverCarveEvent
    , OreSheetParams(..)
    , RiverSegmentCarve(..)
    , RiverDeltaParams(..)
    , explodeRiverEvent
    )
import World.Geology.Timeline.Feature
    ( FeatureShape(..)
    , featureShapeTag
    , FeatureActivity(..)
    , FeatureEvolution(..)
    , CraterParams(..)
    , VolcanicFeature(..)
    , ShieldParams(..)
    , CinderConeParams(..)
    , LavaDomeParams(..)
    , CalderaParams(..)
    , FissureParams(..)
    , LavaTubeParams(..)
    , SuperVolcanoParams(..)
    , HydrothermalParams(..)
    , LandslideParams(..)
    , FloodParams(..)
    , ErosionParams(..)
    , defaultErosionParams
    , VolcanicActivity(..)
    , PersistentFeature(..)
    )
import World.Geology.Timeline.BBox
    ( EventBBox(..)
    , noBBox
    , eventBBox
    , featureShapeBBox
    , volcanicFeatureBBox
    , glacierBBox
    , hydroFeatureBBox
    , hydroEvolutionBBox
    , tileInBBoxWrapped
    , bboxOverlapsChunk
    , tagEventsWithBBox
    )

data GeoScale
    = Eon       -- ^ Billions of years — major crustal formation
    | Era       -- ^ Hundreds of millions — large-scale events
    | Period    -- ^ Tens of millions — mountain building, rifting
    | Epoch     -- ^ Millions — climate shifts, glaciation
    | Age       -- ^ Hundreds of thousands — local events, erosion detail
    deriving (Show, Eq, Ord, Generic, Serialize, NFData, Hashable)

-- | Player-configurable timeline depth (count of each geological
--   segment). Eon + Era are fixed counts; Period/Epoch/Age roll a
--   uniform count in [min,max] per parent. Lower counts ⇒ fewer Ages ⇒
--   faster worldgen (less per-tile erosion replay). See project_timeline_depth.
data TimelineParams = TimelineParams
    { tlpEonCount  ∷ !Int
    , tlpEraCount  ∷ !Int
    , tlpPeriodMin ∷ !Int
    , tlpPeriodMax ∷ !Int
    , tlpEpochMin  ∷ !Int
    , tlpEpochMax  ∷ !Int
    , tlpAgeMin    ∷ !Int
    , tlpAgeMax    ∷ !Int
    } deriving (Show, Eq, Generic, Serialize, NFData)

defaultTimelineParams ∷ TimelineParams
defaultTimelineParams = TimelineParams
    { tlpEonCount  = 1
    , tlpEraCount  = 2
    , tlpPeriodMin = 1
    , tlpPeriodMax = 3
    , tlpEpochMin  = 1
    , tlpEpochMax  = 3
    , tlpAgeMin    = 1
    , tlpAgeMax    = 3
    }

data GeoPeriod = GeoPeriod
    { gpName       ∷ !Text
    , gpScale      ∷ !GeoScale
    , gpDuration   ∷ !Int          -- ^ Relative duration (arbitrary units)
    , gpDate       ∷ !Float        -- ^ Date of period start
    , gpEvents     ∷ ![GeoEvent]
    , gpErosion    ∷ !ErosionParams
    , gpRegionalErosion ∷ !(HM.HashMap ClimateCoord ErosionParams)
    , gpTaggedEvents ∷ ![(GeoEvent, EventBBox)]
    , gpExplodedEvents ∷ !(V.Vector (GeoEvent, EventBBox))
    , gpPeriodBBox    ∷ !EventBBox      -- ^ Bounding box of all events in this period
    } deriving (Show, Eq, Generic, Serialize, NFData)

data GeoTimeline = GeoTimeline
    { gtSeed       ∷ !Word64
    , gtWorldSize  ∷ !Int
    , gtPeriods    ∷ ![GeoPeriod]
    , gtFeatures   ∷ ![PersistentFeature]
    , gtRiverExplodedEvents ∷ !(V.Vector (GeoEvent, EventBBox))
      -- ^ All river carve events (segments, deltas, hydro rivers) across
      --   all periods, pre-filtered and exploded with bounding boxes.
      --   Cached here to avoid recomputing per-tile in applyTimeline/Fast.
    , gtIceLevel ∷ !IceLevelGrid
      -- ^ Coarse-resolution ice fill levels, computed by running
      --   fillDepressions on the final elevation grid restricted to
      --   frozen cells. Used at chunk gen for basin/drape ice decision.
    , gtWorldLakes ∷ !WorldLakes
      -- ^ Global lake table — every basin identified by world-init
      --   tile-resolution priority flood, with per-chunk bitmasks of
      --   the in-lake tiles. Chunk gen reads this for surface fluid
      --   placement so every chunk agrees on each lake's @surface_z@.
    , gtWorldRivers ∷ !WorldRivers
      -- ^ Global river table — flow-accumulation rivers fed by
      --   precipitation and lake spillways, with per-chunk bitmasks
      --   plus per-tile quantised water surface z. Cross-chunk
      --   consistency is automatic because every chunk reads the
      --   same global table.
    , gtWorldLavaPools ∷ !WorldLakes
      -- ^ Global lava-pool table — surface lava pooled in
      --   depressions ('World.Magma.Pool.identifyLavaPools'),
      --   reusing the lake table shape (bitmask + uniform surface).
      --   Chunk gen places Lava where a pool bitmask is set and the
      --   pool surface covers the tile's terrain, with priority over
      --   water. 'Generic Serialize' is positional; keep field
      --   order stable (save schema).
    , gtCoastal ∷ !CoastalTable
      -- ^ Global coastal-erosion table — per-chunk elevation deltas
      --   + material overrides computed once at init on the stitched
      --   pre-coastal terrain ('identifyCoastalErosion'). Chunk gen
      --   applies this instead of running the windowed coastal pass,
      --   so adjacent chunks always agree on the coastline (save v25).
    , gtSeabed ∷ !SeabedTable
      -- ^ Global seabed table — ocean-floor relief (depth-from-shore
      --   ramp + gentle noise replacing the flat seaLevel−1 basin
      --   carve), seabed materials (sand→silt→muck by depth), and
      --   bedrock outcrops ('World.Fluid.Seabed.identifySeabed').
      --   Applied at chunk gen like 'gtCoastal' (save v26).
    , gtWorldOcean ∷ !WorldOceanMask
      -- ^ Tile-resolution edge-connected ocean, per-chunk bitmask.
      --   'composeFluidMap' ORs this into its chunk-level ocean test
      --   so sub-sea tiles the coarse chunk-flood missed still render
      --   ocean (fixes whole chunks rendering dry inside a sea — the
      --   sea-stops-at-a-chunk-boundary bug). Appended (positional
      --   schema; save v27).
    , gtOreDeposits ∷ !WorldOreDeposits
      -- ^ Per-chunk ore-volume summary aggregated from the timeline's
      --   'OreSheetEvent's at assembly. Read by the zoom-map info
      --   panel and the Lua query surface. Appended last (positional
      --   schema; save v30).
    } deriving (Show, Eq, Generic, Serialize, NFData)

emptyTimeline ∷ GeoTimeline
emptyTimeline = GeoTimeline
    { gtSeed = 0
    , gtWorldSize = 128
    , gtPeriods = []
    , gtFeatures = []
    , gtRiverExplodedEvents = V.empty
    , gtIceLevel = IceLevelGrid 0 1 VU.empty
    , gtWorldLakes = emptyWorldLakes
    , gtWorldRivers = emptyWorldRivers
    , gtWorldLavaPools = emptyWorldLakes
    , gtCoastal = emptyCoastalTable
    , gtSeabed = emptySeabedTable
    , gtWorldOcean = emptyWorldOceanMask
    , gtOreDeposits = emptyWorldOreDeposits
    }
