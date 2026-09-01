{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
-- Deliberate orphan: Serialize for HashSet, used by the save path
-- (wgpLocationStamped rides into world.synworld).
{-# OPTIONS_GHC -Wno-orphans #-}
module World.Generate.Types
    ( WorldGenParams(..)
    , defaultWorldGenParams
    , withVolcanoCtx
    , isArenaParams
    ) where

import UPrelude hiding (get)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize(..))
import Data.Hashable (Hashable)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import World.Plate.Types (TectonicPlate(..))
import World.Time.Types
    ( CalendarConfig(..)
    , defaultCalendarConfig
    , SunConfig(..)
    , defaultSunConfig
    , MoonConfig(..)
    , defaultMoonConfig
    )
import World.Geology.Ore.Types (OreLevers, defaultOreLevers)
import World.Geology.Timeline.Types (GeoTimeline(..), emptyTimeline
                                    , TimelineParams(..), defaultTimelineParams)
import World.Ocean.Types (OceanMap, OceanDistMap)
import World.Weather.Types (ClimateParams, ClimateState
                           , defaultClimateParams, initClimateState)
import World.Magma.Types (VolcanoCtx, emptyVolcanoCtx)
import World.Magma.Init (buildVolcanoCtx)
import Location.Overlay.Types (LocationOverlay, emptyLocationOverlay)
import Location.Instance (LocationInstances(..), emptyLocationInstances)
import World.River.Naming (RiverNames, emptyRiverNames)
import World.Chunk.Types (ChunkCoord)

-- | Pure, serializable world generation parameters.
--   Same params + same ChunkCoord = same Chunk, always.
--
--   @wgpVolcanoCtx@ is the only transient field — it is derived from
--   @wgpSeed + wgpWorldSize + gtFeatures wgpGeoTimeline@ and so is
--   skipped by the manual 'Serialize' instance below to keep the save
--   schema stable (see the lava v1 plan: VolcanoCtx is rebuilt at
--   load via 'buildVolcanoCtx' from already-persisted fields).
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
    , wgpOceanDist  ∷ !OceanDistMap    -- ^ Distance from ocean per chunk (BFS)
    , wgpClimateParams ∷ !ClimateParams   -- ^ Climate parameters
    , wgpClimateState ∷ !ClimateState     -- ^ Initial climate state
    , wgpErosionIntensity ∷ !Float        -- ^ Global erosion intensity multiplier
    , wgpVolcanicActivity ∷ !Float        -- ^ Volcanic activity multiplier (scales counts + eruption chance)
    , wgpLavaPoolDepth ∷ !Int             -- ^ Max lava head above a pool's landing floor (tiles)
    , wgpLavaPoolRadius ∷ !Int            -- ^ Max pool footprint radius (tiles)
    , wgpWaterfallQuantum ∷ !Int          -- ^ Max water-surface drop between adjacent river tiles before a stepped gorge is carved
    , wgpOreLevers ∷ !OreLevers           -- ^ Resource-abundance levers for the ore deposition pass
    , wgpTimelineParams ∷ !TimelineParams -- ^ Player-configured timeline depth (eon/era/period/epoch/age counts)
    , wgpLocationOverlay ∷ !LocationOverlay
      -- ^ Sparse chunk→location-id map placed at world init by the
      --   deterministic overlay pass (#89). Serialized (appended to the
      --   manual instance below) so a loaded world keeps its layout
      --   without recomputation.
    , wgpLocationInstances ∷ !LocationInstances
      -- ^ Per-page placed-location instance table (#911): one record
      --   per overlay entry, keyed by a stable
      --   'Location.Instance.LocationInstanceId' allocated at placement
      --   time from the deterministic overlay above. Carries each
      --   location's definition id, anchor, absolute bounds (#777 —
      --   the ONLY footprint since #1230 removed the discovery margin),
      --   display name, gameplay LIFECYCLE, and its
      --   one-time content-spawn flag (#90).
      --
      --   This replaced two former chunk-keyed sets — a chunk is not a
      --   location, so @wgpLocationDiscovered@ (#780) and
      --   @wgpLocationContentsSpawned@ (#90) became
      --   'Location.Instance.liLifecycle' and
      --   'Location.Instance.liContentsSpawned'. Their INDEPENDENCE
      --   from each other and from 'wgpLocationStamped' below is
      --   preserved exactly: discovery never spawns contents, spawning
      --   contents never discovers, and neither is implied by a stamp.
      --
      --   The lifecycle is checked + promoted every world tick by
      --   'World.Thread.Discovery.tickLocationDiscovery' against every
      --   page in 'wmWorlds' (not just the visible one — discovery must
      --   fire on a hidden page a player unit is simulated on, which is
      --   why the tick runs 'Unit.LineOfSight.visibleTilesOnPage'
      --   against that page's own state rather than the wmVisible-gated
      --   public query), independent of the pause flag so a freshly
      --   loaded, auto-paused save with a unit that can already SEE a
      --   location discovers it immediately.
      --
      --   Since #1230 the trigger is SIGHT, not proximity: a
      --   player-owned unit's night-aware visible-tile set intersecting
      --   the instance's own stored 'Location.Instance.liBounds',
      --   seam-aware, one tile being enough. The @discovery_margin@
      --   halo that used to expand those bounds is gone.
      --
      --   Serialized. 'Location.Instance.lisPendingLegacy' is the one
      --   transient part (skipped by the manual instance below and by
      --   the save DTO, like 'wgpVolcanoCtx'), holding a pre-#911
      --   save's per-chunk flags only until the load path resolves them.
    , wgpLocationStamped ∷ !(HS.HashSet ChunkCoord)
      -- ^ One-time geometry-stamp flag (#424): chunks whose placed
      --   location has already been stamped COMPLETELY — its builder
      --   ran and every placement it attempted succeeded (#1719), so a
      --   partial stamp is deliberately absent here and is retried on
      --   the chunk's next load. Was formerly
      --   inferred from @structure.hasAt gx gy "floor"@, but that
      --   check is fooled by a player who later clears the anchor
      --   floor tile — the location has still been materialized, but
      --   the guard would see "no floor" and re-run the builder,
      --   clobbering the player's edits. This flag is a dedicated
      --   marker, set once on first stamp and never revisited by
      --   player structure edits, so it stays true even after the
      --   anchor tile is cleared.
    , wgpRiverNames ∷ !RiverNames
      -- ^ Per-page river-name table (#1102): each river's name in this
      --   page's own generated language, plus its English gloss, keyed
      --   by the 'World.Base.GeoFeatureId' the timeline already
      --   allocated. Sits BESIDE 'wgpLocationInstances' for the same
      --   reason it exists at all — a name is not terrain, and
      --   'wgpGeoTimeline' is a positionally serialized worldgen-output
      --   schema (see "World.River.Naming").
      --
      --   Empty for a page with no #1092 language provenance and for
      --   every save written before #1102: a river then keeps its id
      --   and has no name, and nothing infers one. Written ONCE at
      --   world init and read thereafter (#708 principle 5).
      --
      --   Serialized.
    , wgpVolcanoCtx ∷ !VolcanoCtx
      -- ^ Pure-function lava system context. Transient: NOT serialized;
      --   rebuilt from gtFeatures + wgpSeed + wgpWorldSize on load.
    } deriving (Show, Eq, Generic, NFData)

-- | Manual Serialize: every field except @wgpVolcanoCtx@, plus the
--   equally transient 'Location.Instance.lisPendingLegacy' inside
--   @wgpLocationInstances@ (the instance table is written as its
--   allocator + map, so a decoded table always comes back with nothing
--   pending). Field order matches the data declaration.
--
--   NB this instance is NOT the save path — @world.synworld@ goes
--   through the frozen 'World.Save.Component.WorldGenCurrent.WorldGenParamsDTO'
--   and its own per-component versioning. This is the plain structural
--   encoding the params carry as an ordinary 'Serialize' value.
instance Serialize WorldGenParams where
    put p = do
        put (wgpSeed p)
        put (wgpWorldSize p)
        put (wgpPlateCount p)
        put (wgpPlates p)
        put (wgpCalender p)
        put (wgpSunConfig p)
        put (wgpMoonConfig p)
        put (wgpGeoTimeline p)
        put (wgpOceanMap p)
        put (wgpOceanDist p)
        put (wgpClimateParams p)
        put (wgpClimateState p)
        put (wgpErosionIntensity p)
        put (wgpVolcanicActivity p)
        put (wgpLavaPoolDepth p)
        put (wgpLavaPoolRadius p)
        put (wgpWaterfallQuantum p)
        put (wgpOreLevers p)
        put (wgpTimelineParams p)
        put (wgpLocationOverlay p)
        put (lisNextId (wgpLocationInstances p))
        put (lisById (wgpLocationInstances p))
        put (wgpLocationStamped p)
        put (wgpRiverNames p)
    get = do
        seed       ← get
        ws         ← get
        plateCount ← get
        plates     ← get
        cal        ← get
        sun        ← get
        moon       ← get
        timeline   ← get
        oceanMap   ← get
        oceanDist  ← get
        climateP   ← get
        climateS   ← get
        erosion    ← get
        volcanic   ← get
        poolDepth  ← get
        poolRadius ← get
        waterfallQ ← get
        oreLevers  ← get
        timelineP  ← get
        locOverlay ← get
        locNextId  ← get
        locById    ← get
        locStamped ← get
        riverNames ← get
        let vc = buildVolcanoCtx seed ws plates (gtFeatures timeline)
        pure WorldGenParams
            { wgpSeed             = seed
            , wgpWorldSize        = ws
            , wgpPlateCount       = plateCount
            , wgpPlates           = plates
            , wgpCalender         = cal
            , wgpSunConfig        = sun
            , wgpMoonConfig       = moon
            , wgpGeoTimeline      = timeline
            , wgpOceanMap         = oceanMap
            , wgpOceanDist        = oceanDist
            , wgpClimateParams    = climateP
            , wgpClimateState     = climateS
            , wgpErosionIntensity = erosion
            , wgpVolcanicActivity = volcanic
            , wgpLavaPoolDepth    = poolDepth
            , wgpLavaPoolRadius   = poolRadius
            , wgpWaterfallQuantum = waterfallQ
            , wgpOreLevers        = oreLevers
            , wgpTimelineParams   = timelineP
            , wgpLocationOverlay  = locOverlay
            , wgpLocationInstances = LocationInstances
                { lisNextId        = locNextId
                , lisById          = locById
                , lisPendingLegacy = Nothing
                }
            , wgpLocationStamped  = locStamped
            , wgpRiverNames       = riverNames
            , wgpVolcanoCtx       = vc
            }

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
    , wgpOceanDist = HM.empty
    , wgpClimateParams = defaultClimateParams
    , wgpClimateState = initClimateState 128
    , wgpErosionIntensity = 0.7
    , wgpVolcanicActivity = 1.25
    , wgpLavaPoolDepth = 6
    , wgpLavaPoolRadius = 22
    , wgpWaterfallQuantum = 12
    , wgpOreLevers = defaultOreLevers
    , wgpTimelineParams = defaultTimelineParams
    , wgpLocationOverlay = emptyLocationOverlay
    , wgpLocationInstances = emptyLocationInstances
    , wgpLocationStamped = HS.empty
    , wgpRiverNames = emptyRiverNames
    , wgpVolcanoCtx = emptyVolcanoCtx
    }

-- | Arena (test) worlds: flat chunks, no geological timeline. The
--   single authority for this predicate — chunk loading picks the
--   flat generator from it and the ore survey skips transient
--   generation on it.
isArenaParams ∷ WorldGenParams → Bool
isArenaParams p = wgpGeoTimeline p ≡ emptyTimeline ∧ wgpSeed p ≡ 0

-- | Refresh @wgpVolcanoCtx@ from the params' seed / worldSize /
--   plates / timeline. Called after the geological timeline is
--   finalised at world init so chunk-gen sees a populated context.
withVolcanoCtx ∷ WorldGenParams → WorldGenParams
withVolcanoCtx p = p
    { wgpVolcanoCtx = buildVolcanoCtx (wgpSeed p)
                                       (wgpWorldSize p)
                                       (wgpPlates p)
                                       (gtFeatures (wgpGeoTimeline p))
    }
