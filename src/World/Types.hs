{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Types where

import UPrelude
import Data.List (find, partition, sortOn)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable(..))
import Data.IORef (IORef, newIORef)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scene.Types.Batch (SortableQuad(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..))
import Engine.Graphics.Camera (CameraFacing(..))
import qualified Engine.Core.Queue as Q

-----------------------------------------------------------
-- World Page ID
-----------------------------------------------------------

newtype WorldPageId = WorldPageId Text
    deriving (Show, Eq, Ord)

-----------------------------------------------------------
-- Chunk Types
-----------------------------------------------------------

data ChunkCoord = ChunkCoord !Int !Int
    deriving (Show, Eq, Ord)

instance Hashable ChunkCoord where
    hashWithSalt s (ChunkCoord x y) = s `hashWithSalt` x `hashWithSalt` y

type Chunk = HM.HashMap (Int, Int, Int) Tile

data LoadedChunk = LoadedChunk
    { lcCoord      ∷ !ChunkCoord
    , lcTiles      ∷ !Chunk
    , lcSurfaceMap ∷ !(HM.HashMap (Int, Int) Int)
    , lcModified   ∷ !Bool
    } deriving (Show, Eq)

chunkSize ∷ Int
chunkSize = 16

-----------------------------------------------------------
-- World Generation Parameters
-----------------------------------------------------------

-- | Pure, serializable world generation parameters.
--   Same params + same ChunkCoord = same Chunk, always.
data WorldGenParams = WorldGenParams
    { wgpSeed       ∷ !Word64
    , wgpWorldSize  ∷ !Int     -- ^ World size in chunks (e.g. 64 → 64×64 chunks)
    , wgpPlateCount ∷ !Int     -- ^ Number of tectonic plates (for worldgen)
    , wgpCalender   ∷ !CalendarConfig  -- ^ Calendar configuration for time/date calculations
    , wgpSunConfig   ∷ !SunConfig       -- ^ Sun configuration for time-of-day lighting
    , wgpMoonConfig  ∷ !MoonConfig      -- ^ Moon configuration for lunar phases
    , wgpGeoTimeline ∷ !GeoTimeline      -- ^ Geological timeline for terrain evolution
    } deriving (Show, Eq)

defaultWorldGenParams ∷ WorldGenParams
defaultWorldGenParams = WorldGenParams
    { wgpSeed      = 42
    , wgpWorldSize = 128
    , wgpPlateCount = 10
    , wgpCalender = defaultCalendarConfig
    , wgpSunConfig = defaultSunConfig
    , wgpMoonConfig = defaultMoonConfig
    , wgpGeoTimeline = emptyTimeline
    }

-----------------------------------------------------------
-- World Tile Data
-----------------------------------------------------------

data Tile = Tile
    { tileType ∷ Word8
    , tileSlopeId ∷ Word8
    } deriving (Show, Eq)

data WorldTileData = WorldTileData
    { wtdChunks    ∷ !(HM.HashMap ChunkCoord LoadedChunk)
    , wtdMaxChunks ∷ !Int
    } deriving (Show, Eq)

emptyWorldTileData ∷ WorldTileData
emptyWorldTileData = WorldTileData
    { wtdChunks = HM.empty
    , wtdMaxChunks = 200
    }

lookupChunk ∷ ChunkCoord → WorldTileData → Maybe LoadedChunk
lookupChunk coord wtd = HM.lookup coord (wtdChunks wtd)

insertChunk ∷ LoadedChunk → WorldTileData → WorldTileData
insertChunk lc wtd =
    wtd { wtdChunks = HM.insert (lcCoord lc) lc (wtdChunks wtd) }

chunkCount ∷ WorldTileData → Int
chunkCount = HM.size . wtdChunks

-- | Evict chunks that are far from the camera, keeping at most wtdMaxChunks.
--   Keeps all chunks within the keep radius, evicts furthest-first beyond that.
--   Never evicts modified chunks (future-proofing for when chunks can be edited).
evictDistantChunks ∷ ChunkCoord → Int → WorldTileData → WorldTileData
evictDistantChunks (ChunkCoord camCX camCY) keepRadius wtd =
    let chunks = wtdChunks wtd
        maxC   = wtdMaxChunks wtd
    in if HM.size chunks ≤ maxC
       then wtd
       else
         let -- Must-keep: modified or within keep radius
             keep = HM.filterWithKey (\coord lc →
                 let ChunkCoord cx cy = coord
                     dx = abs (cx - camCX)
                     dy = abs (cy - camCY)
                 in lcModified lc ∨ (dx ≤ keepRadius ∧ dy ≤ keepRadius)
                 ) chunks
             -- Everything else is a candidate for eviction
             candidates = HM.filterWithKey (\coord _ → not (HM.member coord keep)) chunks
             -- Sort candidates by distance (furthest first), keep only what fits
             candidateList = sortOn (\lc →
                 let ChunkCoord cx cy = lcCoord lc
                 in negate (abs (cx - camCX) + abs (cy - camCY))
                 ) (HM.elems candidates)
             roomLeft = max 0 (maxC - HM.size keep)
             kept = take roomLeft candidateList
             keptMap = HM.fromList [(lcCoord lc, lc) | lc ← kept]
         in wtd { wtdChunks = HM.union keep keptMap }

-----------------------------------------------------------
-- World Tile Quad Cache
-----------------------------------------------------------

-- | Snapshot of camera state used to generate cached quads.
--   If the current camera matches this, we can reuse the quads.
data WorldCameraSnapshot = WorldCameraSnapshot
    { wcsPosition ∷ !(Float, Float)
    , wcsZoom     ∷ !Float
    , wcsZSlice   ∷ !Int
    , wcsFbSize   ∷ !(Int, Int)
    , wcsFacing   ∷ !CameraFacing
    } deriving (Show, Eq)

data WorldQuadCache = WorldQuadCache
    { wqcCamera ∷ !WorldCameraSnapshot
    , wqcQuads  ∷ !(V.Vector SortableQuad)
    } deriving (Show)

-----------------------------------------------------------
-- World Camera
-----------------------------------------------------------

data WorldCamera = WorldCamera
    { wcX ∷ Float
    , wcY ∷ Float
    } deriving (Show, Eq)

-----------------------------------------------------------
-- World Textures
-----------------------------------------------------------

data WorldTextures = WorldTextures
    { wtGraniteTexture   ∷ TextureHandle
    , wtGabbroTexture    ∷ TextureHandle
    , wtDioriteTexture   ∷ TextureHandle
    , wtIsoFaceMap       ∷ TextureHandle
    , wtNoTexture        ∷ TextureHandle
    , wtNoFaceMap        ∷ TextureHandle
    , wtZoomGranite      ∷ TextureHandle
    , wtZoomGabbro       ∷ TextureHandle
    , wtZoomDiorite      ∷ TextureHandle
    , wtZoomOcean        ∷ TextureHandle
    , wtZoomLava         ∷ TextureHandle
    , wtGlacierTexture   ∷ TextureHandle
    , wtLavaTexture      ∷ TextureHandle
    , wtZoomGlacier      ∷ TextureHandle
    , wtBlankTexture     ∷ TextureHandle
    , wtBgGranite        ∷ TextureHandle
    , wtBgGabbro         ∷ TextureHandle
    , wtBgDiorite        ∷ TextureHandle
    , wtBgOcean          ∷ TextureHandle
    , wtBgGlacier        ∷ TextureHandle
    , wtBgLava           ∷ TextureHandle
    , wtBasaltTexture    ∷ TextureHandle
    , wtObsidianTexture  ∷ TextureHandle
    , wtSandstoneTexture ∷ TextureHandle
    , wtLimestoneTexture ∷ TextureHandle
    , wtShaleTexture     ∷ TextureHandle
    , wtImpactiteTexture ∷ TextureHandle
    , wtIronTexture      ∷ TextureHandle
    , wtOlivineTexture   ∷ TextureHandle
    , wtPyroxeneTexture  ∷ TextureHandle
    , wtFeldsparTexture  ∷ TextureHandle
    , wtZoomBasalt       ∷ TextureHandle
    , wtZoomObsidian     ∷ TextureHandle
    , wtZoomImpactite    ∷ TextureHandle
    , wtBgBasalt         ∷ TextureHandle
    , wtBgImpactite      ∷ TextureHandle
    , wtBgObsidian       ∷ TextureHandle
    } deriving (Show, Eq)

defaultWorldTextures ∷ WorldTextures
defaultWorldTextures = WorldTextures
    { wtGraniteTexture  = TextureHandle 0
    , wtGabbroTexture   = TextureHandle 0
    , wtDioriteTexture  = TextureHandle 0
    , wtNoTexture       = TextureHandle 0
    , wtIsoFaceMap      = TextureHandle 0
    , wtNoFaceMap       = TextureHandle 0
    , wtZoomGranite     = TextureHandle 0
    , wtZoomGabbro      = TextureHandle 0
    , wtZoomDiorite     = TextureHandle 0
    , wtZoomOcean       = TextureHandle 0
    , wtGlacierTexture  = TextureHandle 0
    , wtLavaTexture     = TextureHandle 0
    , wtZoomGlacier     = TextureHandle 0
    , wtZoomLava         = TextureHandle 0
    , wtBlankTexture    = TextureHandle 0
    , wtBgGranite       = TextureHandle 0
    , wtBgGabbro        = TextureHandle 0
    , wtBgDiorite       = TextureHandle 0
    , wtBgOcean         = TextureHandle 0
    , wtBgGlacier       = TextureHandle 0
    , wtBgLava         = TextureHandle 0
    , wtBasaltTexture    = TextureHandle 0
    , wtObsidianTexture  = TextureHandle 0
    , wtSandstoneTexture = TextureHandle 0
    , wtLimestoneTexture = TextureHandle 0
    , wtShaleTexture     = TextureHandle 0
    , wtImpactiteTexture = TextureHandle 0
    , wtIronTexture      = TextureHandle 0
    , wtOlivineTexture   = TextureHandle 0
    , wtPyroxeneTexture  = TextureHandle 0
    , wtFeldsparTexture  = TextureHandle 0
    , wtZoomBasalt       = TextureHandle 0
    , wtZoomObsidian     = TextureHandle 0
    , wtZoomImpactite    = TextureHandle 0
    , wtBgBasalt         = TextureHandle 0
    , wtBgImpactite      = TextureHandle 0
    , wtBgObsidian       = TextureHandle 0
    }

-----------------------------------------------------------
-- World Time
-----------------------------------------------------------

-- | Time of day in the world.
--   hour: 0-23, minute: 0-59
--   sunAngle is derived: 0.0 = midnight, 0.25 = 6am (dawn),
--                         0.5 = noon, 0.75 = 6pm (dusk)
data WorldTime = WorldTime
    { wtHour   ∷ !Int   -- ^ 0-23
    , wtMinute ∷ !Int   -- ^ 0-59
    } deriving (Show, Eq)

defaultWorldTime ∷ WorldTime
defaultWorldTime = WorldTime
    { wtHour   = 10     -- start at 10:00am (pleasant morning light)
    , wtMinute = 0
    }

-- | Convert world time to sun angle (0.0 .. 1.0)
--   Mapping: midnight (0:00) = 0.0, 6am = 0.25, noon = 0.5, 6pm = 0.75
worldTimeToSunAngle ∷ WorldTime → Float
worldTimeToSunAngle (WorldTime h m) =
    let totalMinutes = fromIntegral h * 60.0 + fromIntegral m ∷ Float
    in totalMinutes / 1440.0   -- 1440 = 24 * 60

-- | Advance world time by a number of real seconds, scaled by a speed factor.
--   Returns the new time (wraps at 24:00).
--   timeScale: how many game-minutes pass per real-second.
advanceWorldTime ∷ Float → Float → WorldTime → WorldTime
advanceWorldTime timeScale dtSeconds (WorldTime h m) =
    let totalMinutes = fromIntegral h * 60 + fromIntegral m ∷ Float
        newTotal = totalMinutes + timeScale * dtSeconds
        -- Wrap around 1440 minutes (24 hours)
        wrapped = newTotal - 1440.0 * fromIntegral (floor (newTotal / 1440.0) ∷ Int)
        newH = floor wrapped `div` 60
        newM = floor wrapped `mod` 60
    in WorldTime (newH `mod` 24) (newM `mod` 60)

-- | World date (placeholder for seasons).
--   Currently unused for sun angle calculation.
data WorldDate = WorldDate
    { wdYear  ∷ !Int
    , wdMonth ∷ !Int   -- ^ 1-12
    , wdDay   ∷ !Int   -- ^ 1-31
    } deriving (Show, Eq)

defaultWorldDate ∷ WorldDate
defaultWorldDate = WorldDate
    { wdYear  = 1
    , wdMonth = 1
    , wdDay   = 1
    }

-----------------------------------------------------------
-- Calendar
-----------------------------------------------------------

data CalendarConfig = CalendarConfig
    { ccDaysPerMonth  ∷ !Int      -- ^ e.g. 30
    , ccMonthsPerYear ∷ !Int      -- ^ e.g. 12
    , ccHoursPerDay   ∷ !Int      -- ^ e.g. 24 (controls sun cycle)
    , ccMinutesPerHour ∷ !Int     -- ^ e.g. 60
    } deriving (Show, Eq)

defaultCalendarConfig ∷ CalendarConfig
defaultCalendarConfig = CalendarConfig
    { ccDaysPerMonth   = 30
    , ccMonthsPerYear  = 12
    , ccHoursPerDay    = 24
    , ccMinutesPerHour = 60
    }

-----------------------------------------------------------
-- Celestial Bodies
-----------------------------------------------------------

data SunConfig = SunConfig
    { scTiltAngle    ∷ !Float   -- ^ Axial tilt in radians, controls season intensity
    , scDayLength    ∷ !Float   -- ^ Base day/night ratio at equinox (0.5 = equal)
    } deriving (Show, Eq)

defaultSunConfig ∷ SunConfig
defaultSunConfig = SunConfig
    { scTiltAngle  = 0.4      -- ~23 degrees like Earth
    , scDayLength  = 0.5
    }

data MoonConfig = MoonConfig
    { mcCycleDays    ∷ !Int     -- ^ Days per lunar cycle
    , mcPhaseOffset  ∷ !Float   -- ^ Starting phase offset (0.0-1.0)
    } deriving (Show, Eq)

defaultMoonConfig ∷ MoonConfig
defaultMoonConfig = MoonConfig
    { mcCycleDays   = 28
    , mcPhaseOffset = 0.0
    }

-----------------------------------------------------------
-- Geologic Timeline
-----------------------------------------------------------

-- | Scale of geological time periods.
--   Controls how many erosion passes and how dramatic events are.
data GeoScale
    = Eon       -- ^ Billions of years — major crustal formation
    | Era       -- ^ Hundreds of millions — large-scale events
    | Period    -- ^ Tens of millions — mountain building, rifting
    | Epoch     -- ^ Millions — climate shifts, glaciation
    | Age       -- ^ Hundreds of thousands — local events, erosion detail
    deriving (Show, Eq, Ord)

-- | A single geological time period containing events and
--   erosion parameters.
data GeoPeriod = GeoPeriod
    { gpName       ∷ !Text
    , gpScale      ∷ !GeoScale
    , gpDuration   ∷ !Int          -- ^ Relative duration (arbitrary units)
    , gpEvents     ∷ ![GeoEvent]
    , gpErosion    ∷ !ErosionParams
    } deriving (Show, Eq)

-- | The full geological history, computed once at world init.
data GeoTimeline = GeoTimeline
    { gtSeed       ∷ !Word64
    , gtWorldSize  ∷ !Int
    , gtPeriods    ∷ ![GeoPeriod]
    , gtFeatures   ∷ ![PersistentFeature]
    } deriving (Show, Eq)

emptyTimeline ∷ GeoTimeline
emptyTimeline = GeoTimeline
    { gtSeed = 0
    , gtWorldSize = 128
    , gtPeriods = []
    , gtFeatures = []
    }

-----------------------------------------------------------
-- Geologic Events
-----------------------------------------------------------

-- | An event that modifies terrain. Each event is a pure
--   function of position — no simulation state needed.
data GeoEvent
    = CraterEvent !CraterParams
    | VolcanicEvent !VolcanicFeature
    | VolcanicModify !GeoFeatureId !FeatureEvolution
    | LandslideEvent !LandslideParams
    | GlaciationEvent !GlaciationParams
    | FloodEvent !FloodParams
    deriving (Show, Eq)

-- | How an existing feature evolves in a new period.
data FeatureEvolution
    = Reactivate          -- ^ Dormant → Active, grows taller, new material
        { feHeightGain    ∷ !Int       -- ^ Additional height from new eruption
        , feLavaExtension ∷ !Int       -- ^ Additional lava flow radius
        }
    | GoDormant           -- ^ Active → Dormant, no shape change
    | GoExtinct           -- ^ Active/Dormant → Extinct
    | CollapseToCaldera   -- ^ Structure collapses
        { feCollapseDepth ∷ !Int       -- ^ How deep the collapse goes
        , feCollapseRatio ∷ !Float     -- ^ What fraction of the cone collapses (0.3-0.8)
        }
    | ParasiticEruption   -- ^ New feature spawns on the flank
        { feChildFeature  ∷ !VolcanicFeature  -- ^ The new cinder cone / dome
        , feChildId       ∷ !GeoFeatureId     -- ^ ID for the child
        }
    | FlankCollapse       -- ^ One side of the volcano collapses (Mt St Helens)
        { feCollapseAngle ∷ !Float     -- ^ Direction of collapse (radians)
        , feCollapseWidth ∷ !Float     -- ^ Angular width of the collapse sector
        , feDebrisRadius  ∷ !Int       -- ^ How far the debris field extends
        }
    deriving (Show, Eq)

-- | Global tile coordinate for event placement.
data GeoCoord = GeoCoord !Int !Int
    deriving (Show, Eq)

data CraterParams = CraterParams
    { cpCenter     ∷ !GeoCoord   -- ^ Impact center (global tile coords)
    , cpRadius     ∷ !Int        -- ^ Outer rim radius in tiles
    , cpDepth      ∷ !Int        -- ^ Depth at center relative to rim
    , cpRimHeight  ∷ !Int        -- ^ Rim elevation above surroundings
    , cpEjectaRadius ∷ !Int      -- ^ How far ejecta spreads beyond rim
    , cpMeteorite  ∷ !(Maybe Word8) -- ^ Optional meteorite material ID at crater center
    } deriving (Show, Eq)

-- | Specific feature types for different volcanic structures
data VolcanicFeature
    = ShieldVolcano    !ShieldParams
    | CinderCone       !CinderConeParams
    | LavaDome         !LavaDomeParams
    | Caldera          !CalderaParams
    | FissureVolcano   !FissureParams
    | LavaTube         !LavaTubeParams
    | SuperVolcano     !SuperVolcanoParams
    | HydrothermalVent !HydrothermalParams
    deriving (Show, Eq)

data ShieldParams = ShieldParams
    { shCenter     ∷ !GeoCoord
    , shBaseRadius ∷ !Int       -- ^ Very wide (60-120)
    , shPeakHeight ∷ !Int       -- ^ Low relative to width (100-400)
    , shSummitPit  ∷ !Bool      -- ^ Small summit crater?
    , shPitRadius  ∷ !Int
    , shPitDepth   ∷ !Int
    } deriving (Show, Eq)

data CinderConeParams = CinderConeParams
    { ccCenter     ∷ !GeoCoord
    , ccBaseRadius ∷ !Int       -- ^ Small (5-15)
    , ccPeakHeight ∷ !Int       -- ^ Moderate (50-200)
    , ccCraterRadius ∷ !Int     -- ^ Always has a crater
    , ccCraterDepth  ∷ !Int
    } deriving (Show, Eq)

data LavaDomeParams = LavaDomeParams
    { ldCenter     ∷ !GeoCoord
    , ldBaseRadius ∷ !Int       -- ^ Small-medium (10-25)
    , ldHeight     ∷ !Int       -- ^ Squat (50-150)
    } deriving (Show, Eq)

data CalderaParams = CalderaParams
    { caCenter     ∷ !GeoCoord
    , caOuterRadius ∷ !Int      -- ^ Rim outer edge (30-80)
    , caInnerRadius ∷ !Int      -- ^ Rim inner edge (floor)
    , caRimHeight   ∷ !Int      -- ^ Rim above surroundings
    , caFloorDepth  ∷ !Int      -- ^ Floor below surroundings
    , caHasLake     ∷ !Bool     -- ^ Future: water fill
    } deriving (Show, Eq)

data FissureParams = FissureParams
    { fpStart      ∷ !GeoCoord  -- ^ One end of the fissure
    , fpEnd        ∷ !GeoCoord  -- ^ Other end
    , fpWidth      ∷ !Int       -- ^ Half-width perpendicular to line (5-10)
    , fpRidgeHeight ∷ !Int      -- ^ Height of the ridge (20-80)
    , fpHasMagma   ∷ !Bool      -- ^ Active fissure with magma at center
    } deriving (Show, Eq)

data LavaTubeParams = LavaTubeParams
    { ltStart      ∷ !GeoCoord
    , ltEnd        ∷ !GeoCoord
    , ltWidth      ∷ !Int       -- ^ Tube width (3-6)
    , ltRidgeHeight ∷ !Int      -- ^ Subtle surface bulge (5-15)
    , ltCollapses  ∷ !Int       -- ^ Number of ceiling collapse pits
    , ltCollapseSeed ∷ !Word64  -- ^ Seed for collapse placement
    } deriving (Show, Eq)

data SuperVolcanoParams = SuperVolcanoParams
    { svCenter      ∷ !GeoCoord
    , svCalderaRadius ∷ !Int    -- ^ Enormous (100-200)
    , svRimHeight    ∷ !Int     -- ^ Low rim relative to size
    , svFloorDepth   ∷ !Int     -- ^ Deep caldera
    , svEjectaRadius ∷ !Int     -- ^ Ash/debris field (300+)
    , svEjectaDepth  ∷ !Int     -- ^ Ash deposit thickness
    } deriving (Show, Eq)

data HydrothermalParams = HydrothermalParams
    { htCenter     ∷ !GeoCoord
    , htRadius     ∷ !Int       -- ^ Tiny (3-8)
    , htChimneyHeight ∷ !Int    -- ^ Small mound (10-30)
    } deriving (Show, Eq)

data LandslideParams = LandslideParams
    { lsCenter     ∷ !GeoCoord
    , lsRadius     ∷ !Int
    , lsDirection  ∷ !Float      -- ^ Angle of slide (radians)
    , lsVolume     ∷ !Int        -- ^ Amount of material displaced
    } deriving (Show, Eq)

data GlaciationParams = GlaciationParams
    { glLatitudeStart ∷ !Int     -- ^ How far from poles glaciers extend
    , glThickness     ∷ !Int     -- ^ Ice sheet thickness
    , glCarveDepth    ∷ !Int     -- ^ How deep glacial valleys are carved
    , glSeed          ∷ !Word64  -- ^ Sub-seed for glacier flow noise
    } deriving (Show, Eq)

data FloodParams = FloodParams
    { fpCenter     ∷ !GeoCoord
    , fpRadius     ∷ !Int
    , fpDepositDepth ∷ !Int      -- ^ Sediment deposited
    , fpMaterial   ∷ !Word8      -- ^ Sediment material ID
    } deriving (Show, Eq)

-----------------------------------------------------------
-- Erosion
-----------------------------------------------------------

-- | Erosion configuration for a geological period.
--   Different eras have different erosion characteristics.
data ErosionParams = ErosionParams
    { epIntensity    ∷ !Float    -- ^ Overall erosion strength (0.0-1.0)
    , epHydraulic    ∷ !Float    -- ^ Water erosion strength
    , epThermal      ∷ !Float    -- ^ Freeze-thaw weathering strength
    , epWind         ∷ !Float    -- ^ Aeolian erosion (deserts, coastlines)
    , epChemical     ∷ !Float    -- ^ Chemical weathering (limestone dissolution)
    , epSeed         ∷ !Word64   -- ^ Sub-seed for erosion noise
    } deriving (Show, Eq)

defaultErosionParams ∷ ErosionParams
defaultErosionParams = ErosionParams
    { epIntensity  = 0.5
    , epHydraulic  = 0.7
    , epThermal    = 0.3
    , epWind       = 0.1
    , epChemical   = 0.2
    , epSeed       = 0
    }

-----------------------------------------------------------
-- Persistent Geological Features
-----------------------------------------------------------

-- | Unique identifier for a geological feature that persists
--   across geological periods.
newtype GeoFeatureId = GeoFeatureId Int
    deriving (Show, Eq, Ord)

-- | Activity state of a volcanic feature.
data VolcanicActivity
    = Active           -- ^ Currently erupting / building
    | Dormant          -- ^ Quiet but structurally intact, could reactivate
    | Extinct          -- ^ Dead, will only erode from here
    | Collapsed        -- ^ Has collapsed into a caldera
    deriving (Show, Eq)

-- | A persistent feature that evolves across geological time.
--   Tracks the feature's identity, current state, and history
--   of modifications applied to it.
data PersistentFeature = PersistentFeature
    { pfId            ∷ !GeoFeatureId
    , pfFeature       ∷ !VolcanicFeature   -- ^ Current shape
    , pfActivity      ∷ !VolcanicActivity
    , pfFormationPeriod ∷ !Int              -- ^ Index of period when created
    , pfLastActivePeriod ∷ !Int             -- ^ Index of period last active
    , pfEruptionCount ∷ !Int               -- ^ How many times it has erupted
    , pfParentId      ∷ !(Maybe GeoFeatureId) -- ^ If parasitic (cinder cone on shield flank)
    } deriving (Show, Eq)

-----------------------------------------------------------
-- World State
-----------------------------------------------------------

data WorldState = WorldState
    { wsTilesRef     ∷ IORef WorldTileData
    , wsCameraRef    ∷ IORef WorldCamera
    , wsTexturesRef  ∷ IORef WorldTextures
    , wsGenParamsRef ∷ IORef (Maybe WorldGenParams)
    , wsTimeRef      ∷ IORef WorldTime
    , wsDateRef      ∷ IORef WorldDate
    , wsTimeScaleRef ∷ IORef Float    -- ^ Game-minutes per real-second
    , wsZoomCacheRef ∷ IORef (V.Vector ZoomChunkEntry)  -- ^ Pre-computed zoom map cache for current world state
    , wsQuadCacheRef  ∷ IORef (Maybe WorldQuadCache)  -- ^ Cached quads for current camera state
    , wsZoomQuadCacheRef ∷ IORef (Maybe ZoomQuadCache)  -- ^ Cached quads for zoomed-out view
    , wsBgQuadCacheRef ∷ IORef (Maybe ZoomQuadCache)    -- ^ Cached quads for background layer
    , wsBakedZoomRef ∷ IORef (V.Vector BakedZoomEntry)  -- ^ Pre-baked zoom entries with resolved textures and vertices
    , wsBakedBgRef ∷ IORef (V.Vector BakedZoomEntry)    -- ^ Pre-baked background entries with resolved textures and vertices
    }

emptyWorldState ∷ IO WorldState
emptyWorldState = do
    tilesRef     ← newIORef emptyWorldTileData
    cameraRef    ← newIORef (WorldCamera 0 0)
    texturesRef  ← newIORef defaultWorldTextures
    genParamsRef ← newIORef Nothing
    timeRef      ← newIORef defaultWorldTime
    dateRef      ← newIORef defaultWorldDate
    timeScaleRef ← newIORef 1.0   -- 1 game-minute per real-second
    zoomCacheRef ← newIORef V.empty
    quadCacheRef  ← newIORef Nothing
    zoomQCRef   ← newIORef Nothing
    bgQCRef     ← newIORef Nothing
    bakedZoomRef ← newIORef V.empty
    bakedBgRef   ← newIORef V.empty
    return $ WorldState tilesRef cameraRef texturesRef genParamsRef
                        timeRef dateRef timeScaleRef zoomCacheRef
                        quadCacheRef zoomQCRef bgQCRef
                        bakedZoomRef bakedBgRef

-----------------------------------------------------------
-- World Manager
-----------------------------------------------------------

data WorldManager = WorldManager
    { wmWorlds  ∷ [(WorldPageId, WorldState)]
    , wmVisible ∷ [WorldPageId]
    }

emptyWorldManager ∷ WorldManager
emptyWorldManager = WorldManager
    { wmWorlds  = []
    , wmVisible = []
    }

-----------------------------------------------------------
-- Zoom Map Cache
-----------------------------------------------------------

-- | Pre-computed zoom map entry for one chunk.
--   Stores everything needed to render the zoom quad.
data ZoomChunkEntry = ZoomChunkEntry
    { zceChunkX   ∷ !Int       -- ^ Canonical chunk X
    , zceChunkY   ∷ !Int       -- ^ Canonical chunk Y
    , zceDrawX    ∷ !Float     -- ^ Screen-space draw X
    , zceDrawY    ∷ !Float     -- ^ Screen-space draw Y
    , zceTexIndex ∷ !Word8     -- ^ Material ID (used to pick texture at render time)
    , zceElev     ∷ !Int       -- ^ Elevation (used to pick texture at render time)
    } deriving (Show, Eq)

-----------------------------------------------------------
-- Zoom/Background Quad Cache
-----------------------------------------------------------

-- | Camera snapshot for zoom/bg caching.
--   Simpler than WorldCameraSnapshot — no zSlice needed.
data ZoomCameraSnapshot = ZoomCameraSnapshot
    { zcsPosition ∷ !(Float, Float)
    , zcsZoom     ∷ !Float
    , zcsFbSize   ∷ !(Int, Int)
    } deriving (Show, Eq)

data ZoomQuadCache = ZoomQuadCache
    { zqcCamera ∷ !ZoomCameraSnapshot
    , zqcAlpha  ∷ !Float               -- ^ Alpha at time of caching
    , zqcQuads  ∷ !(V.Vector SortableQuad)
    } deriving (Show)

-----------------------------------------------------------
-- Baked Zoom Entry (pre-resolved vertices)
-----------------------------------------------------------

-- | A zoom cache entry with vertices pre-baked.
--   Texture slots are already resolved, vertices are ready to use.
--   The hot render loop only needs to: test visibility, shift X, patch alpha.
data BakedZoomEntry = BakedZoomEntry
    { bzeChunkX  ∷ !Int
    , bzeChunkY  ∷ !Int
    , bzeDrawX   ∷ !Float       -- ^ Canonical draw X (before wrap offset)
    , bzeDrawY   ∷ !Float
    , bzeSortKey ∷ !Float
    , bzeV0      ∷ !Vertex      -- ^ Top-left
    , bzeV1      ∷ !Vertex      -- ^ Top-right
    , bzeV2      ∷ !Vertex      -- ^ Bottom-right
    , bzeV3      ∷ !Vertex      -- ^ Bottom-left
    , bzeTexture ∷ !TextureHandle
    } deriving (Show)

-----------------------------------------------------------
-- World Commands
-----------------------------------------------------------

data WorldTextureType
    = GraniteTexture
    | DioriteTexture
    | GabbroTexture
    | NoTexture
    | IsoFaceMap
    | NoFaceMap
    | ZoomGraniteTexture
    | ZoomDioriteTexture
    | ZoomGabbroTexture
    | ZoomOceanTexture
    | GlacierTexture
    | LavaTexture
    | ZoomGlacierTexture
    | ZoomLavaTexture
    | BlankTexture
    | BgGraniteTexture
    | BgGabbroTexture
    | BgDioriteTexture
    | BgOceanTexture
    | BgGlacierTexture
    | BgLavaTexture
    | BasaltTexture
    | ObsidianTexture
    | SandstoneTexture
    | LimestoneTexture
    | ShaleTexture
    | ImpactiteTexture
    | IronTexture
    | OlivineTexture
    | PyroxeneTexture
    | FeldsparTexture
    | ZoomBasaltTexture
    | ZoomObsidianTexture
    | ZoomImpactiteTexture
    | BgBasaltTexture
    | BgImpactiteTexture
    | BgObsidianTexture
    deriving (Show, Eq)

data WorldCommand
    = WorldInit WorldPageId Word64 Int
    | WorldShow WorldPageId
    | WorldHide WorldPageId
    | WorldTick Double
    | WorldSetTexture WorldPageId WorldTextureType TextureHandle
    | WorldSetCamera WorldPageId Float Float
    | WorldSetTime WorldPageId Int Int         -- ^ hour minute
    | WorldSetDate WorldPageId Int Int Int     -- ^ year month day
    | WorldSetTimeScale WorldPageId Float      -- ^ game-minutes per real-second
    deriving (Show, Eq)
