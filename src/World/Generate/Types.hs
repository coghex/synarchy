{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module World.Generate.Types
    ( WorldGenParams(..)
    , defaultWorldGenParams
    , withVolcanoCtx
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
import World.Geology.Timeline.Types (GeoTimeline(..), emptyTimeline)
import World.Ocean.Types (OceanMap, OceanDistMap)
import World.Flora.Types (FloraCatalog, emptyFloraCatalog)
import World.Weather.Types (ClimateParams, ClimateState
                           , defaultClimateParams, initClimateState)
import World.Magma.Types (VolcanoCtx, emptyVolcanoCtx)
import World.Magma.Init (buildVolcanoCtx)

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
    , wgpVolcanoCtx ∷ !VolcanoCtx
      -- ^ Pure-function lava system context. Transient: NOT serialized;
      --   rebuilt from gtFeatures + wgpSeed + wgpWorldSize on load.
    } deriving (Show, Eq, Generic, NFData)

-- | Manual Serialize: every field except @wgpVolcanoCtx@. Field order
--   matches the data declaration so the byte layout is identical to
--   the pre-Magma Generic-derived encoding — existing saves load
--   unchanged.
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
    , wgpVolcanoCtx = emptyVolcanoCtx
    }

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
