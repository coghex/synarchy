{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.State.Types
    ( WorldState(..)
    , emptyWorldState
    , bumpQuadCacheGen
    , WorldManager(..)
    , emptyWorldManager
    , CursorSnapshot(..)
    , LoadPhase(..)
    ) where

import UPrelude
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Engine.Graphics.Camera (CameraFacing(..))
import World.Cursor.Types (CursorState(..), emptyCursorState)
import World.Page.Types (WorldPageId(..))
import World.Chunk.Types (ChunkCoord(..))
import World.Tile.Types (WorldTileData(..), emptyWorldTileData)
import World.Render.Camera.Types (WorldCamera(..), WorldQuadCache(..))
import World.Render.Textures.Types (WorldTextures(..), defaultWorldTextures)
import World.Render.Zoom.Types (ZoomChunkEntry(..), ZoomQuadCache(..), BakedZoomEntry(..), ZoomMapMode(..), ZoomAtlasInfo(..))
import World.Tool.Types (ToolMode(..))
import World.Generate.Types (WorldGenParams(..))
import World.Time.Types (WorldTime(..), WorldDate(..), defaultWorldTime, defaultWorldDate)
import World.Edit.Types (WorldEdit, WorldEdits, emptyWorldEdits)
import Structure.Types (ChunkStructures, emptyChunkStructures)
import World.Mine.Types (MineDesignations)
import World.Construct.Types (ConstructDesignations)
import World.Chop.Types (ChopDesignations)
import World.Spoil.Types (SpoilPiles, emptySpoilPiles)
import World.Flora.Harvest (FloraHarvests, emptyFloraHarvests)
import Item.Ground (GroundItems, emptyGroundItems)

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
    , wsQuadCacheGenRef ∷ IORef Int
      -- ^ Invalidation generation for the quad cache. Bumped atomically
      --   (from any thread) instead of nulling 'wsQuadCacheRef', so a
      --   cross-thread invalidation can't be clobbered by the render
      --   thread's read-rebuild-write of the cache. A cache is only valid
      --   when its 'wqcGen' matches this counter; the render thread is the
      --   sole writer of 'wsQuadCacheRef'. See 'bumpQuadCacheGen'.
    , wsZoomQuadCacheRef ∷ IORef (Maybe ZoomQuadCache)  -- ^ Cached quads for zoomed-out view
    , wsBgQuadCacheRef ∷ IORef (Maybe ZoomQuadCache)    -- ^ Cached quads for background layer
    , wsBakedZoomRef ∷ IORef (V.Vector BakedZoomEntry, WorldTextures, CameraFacing)  -- ^ Pre-baked
    , wsBakedBgRef ∷ IORef (V.Vector BakedZoomEntry, WorldTextures, CameraFacing)    -- ^ Pre-baked background entries with resolved textures and vertices
    , wsInitQueueRef ∷ IORef [ChunkCoord]  -- ^ Queue of chunks to generate at world init (for progress tracking)
    , wsMapModeRef ∷ IORef ZoomMapMode
    , wsCursorRef ∷ IORef CursorState
    , wsToolModeRef ∷ IORef ToolMode
    , wsCursorSnapshotRef ∷ IORef CursorSnapshot
    , wsLoadPhaseRef ∷ IORef LoadPhase
    , wsZoomAtlasRef ∷ IORef (Maybe ZoomAtlasInfo)  -- ^ Atlas info once uploaded to GPU
    , wsEditsRef    ∷ IORef WorldEdits
      -- ^ Player edits accumulated this session. Per-chunk so eviction
      --   doesn't lose them — chunks regenerate, edits replay onto the
      --   fresh chunk. Saved verbatim; restored before any chunk
      --   regeneration on load.
    , wsOreSurveyRef ∷ IORef (HM.HashMap ChunkCoord ([WorldEdit], Text))
      -- ^ Memo for the zoom-map Resources survey of UNLOADED chunks
      --   (transient generation is ~10–300 ms; reselecting shouldn't
      --   repeat it). Each entry stores the chunk's edit list at
      --   compute time — a lookup only hits when the current edit
      --   list is identical, so edits self-invalidate. Wholesale
      --   flush at 256 entries. Loaded chunks never consult this.
    , wsMineDesignationsRef ∷ IORef MineDesignations
      -- ^ Mine-designation set: tile (gx, gy) → designation (surface
      --   z + corner dig progress; see World.Mine.Types). Written by
      --   the world thread (WorldDesignateMine / dig commands), read
      --   by the render pass and the dig AI. Persisted in saves
      --   (sdMineDesignations, v31) including mid-dig corners.
    , wsGroundItemsRef ∷ IORef GroundItems
      -- ^ Items lying in the world (see Item.Ground — float x/y,
      --   height derived from current terrain at render). Persisted
      --   in saves (sdGroundItems, v32).
    , wsSpoilRef ∷ IORef SpoilPiles
      -- ^ Spoil mounds from digging, keyed by tile vertex (see
      --   World.Spoil.Types). Written by the world thread's dig
      --   handler; read by the spoil render pass. Persisted in
      --   saves (sdSpoilPiles, v34).
    , wsStructureStageRef ∷ IORef ChunkStructures
      -- ^ Lua-thread write-ahead staging for THIS world's structure
      --   placements. The authoritative structure state is the per-chunk
      --   'lcStructures' overlay (rendered + persisted), but those writes
      --   apply asynchronously on the world thread via WeSetStructure. The
      --   builder (scripts/structures.lua, scripts/locations.lua) places a
      --   piece then queries it within the SAME Lua call (floors→posts→
      --   walls), so structure.place records it here and structure.floorZAt/
      --   hasAt consult it before falling back to lcStructures —
      --   read-your-writes without a second authority. Per-world so it can't
      --   leak across worlds (it dies with the WorldState, and a reloaded
      --   world gets a fresh empty one); never saved.
    , wsConstructDesignationsRef ∷ IORef ConstructDesignations
      -- ^ Construction-designation set: tile (gx, gy) → designation
      --   (surface z, build target, status, progress; see
      --   World.Construct.Types). Written by the world thread
      --   (WorldDesignateConstruct / cancel / set-status commands), read
      --   by the render pass (blueprint ghost) and the build AI (#96).
      --   Persisted in saves (wpsConstructDesignations).
    , wsFloraHarvestsRef ∷ IORef FloraHarvests
      -- ^ Harvested flora tiles (#94): tile (gx, gy) → regrowth
      --   game-seconds remaining. World-level (NOT in lcFlora) so chunk
      --   eviction can't wipe it; written by world.harvestFlora + the
      --   regrowth tick, read by the flora render pass and the foraging
      --   AI's queries. Persisted in saves (wpsFloraHarvests, v66).
    , wsChopDesignationsRef ∷ IORef ChopDesignations
      -- ^ Chop-designation set (#97): tile (gx, gy) → designation
      --   (surface z; see World.Chop.Types). Written by the world
      --   thread (WorldDesignateChop / cancel commands), read by the
      --   render pass (marker) and the chop AI. Persisted in saves
      --   (wpsChopDesignations, v67).
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
    quadCacheGenRef ← newIORef 0
    zoomQCRef   ← newIORef Nothing
    bgQCRef     ← newIORef Nothing
    bakedZoomRef ← newIORef (V.empty, defaultWorldTextures, FaceSouth)
    bakedBgRef   ← newIORef (V.empty, defaultWorldTextures, FaceSouth)
    wsInitQueueRef ← newIORef []
    wsMapModeRef ← newIORef ZMDefault
    wsCursorRef ← newIORef emptyCursorState
    wsToolModeRef ← newIORef DefaultTool
    wsCursorSnapshotRef ← newIORef emptyCursorSnapshot
    wsLoadPhaseRef ← newIORef LoadIdle
    wsZoomAtlasRef ← newIORef Nothing
    wsEditsRef     ← newIORef emptyWorldEdits
    wsOreSurveyRef ← newIORef HM.empty
    wsMineDesignationsRef ← newIORef HM.empty
    wsGroundItemsRef ← newIORef emptyGroundItems
    wsSpoilRef ← newIORef emptySpoilPiles
    wsStructureStageRef ← newIORef emptyChunkStructures
    wsConstructDesignationsRef ← newIORef HM.empty
    wsFloraHarvestsRef ← newIORef emptyFloraHarvests
    wsChopDesignationsRef ← newIORef HM.empty
    return $ WorldState tilesRef cameraRef texturesRef genParamsRef
                        timeRef dateRef timeScaleRef zoomCacheRef
                        quadCacheRef quadCacheGenRef zoomQCRef bgQCRef
                        bakedZoomRef bakedBgRef wsInitQueueRef
                        wsMapModeRef
                        wsCursorRef wsToolModeRef wsCursorSnapshotRef
                        wsLoadPhaseRef wsZoomAtlasRef wsEditsRef
                        wsOreSurveyRef wsMineDesignationsRef
                        wsGroundItemsRef wsSpoilRef wsStructureStageRef
                        wsConstructDesignationsRef wsFloraHarvestsRef
                        wsChopDesignationsRef

-- | Invalidate a world's cached render quads in a thread-safe way.
--   Bumps the generation counter atomically rather than nulling
--   'wsQuadCacheRef', so an invalidation from the world/Lua thread can
--   never be lost to the render thread's read-rebuild-write of the cache:
--   the render thread stamps each rebuilt cache with the generation it
--   observed, and a cache whose 'wqcGen' no longer matches is treated as
--   stale and rebuilt next frame.
bumpQuadCacheGen ∷ WorldState → IO ()
bumpQuadCacheGen ws =
    atomicModifyIORef' (wsQuadCacheGenRef ws) (\g → (g + 1, ()))

data WorldManager = WorldManager
    { wmWorlds  ∷ [(WorldPageId, WorldState)]
    , wmVisible ∷ [WorldPageId]
    }

emptyWorldManager ∷ WorldManager
emptyWorldManager = WorldManager
    { wmWorlds  = []
    , wmVisible = []
    }

-- | Snapshot of the cursor selection state, used to detect changes
--   and avoid re-sending HUD info every frame.
data CursorSnapshot = CursorSnapshot
    { csZoomSel  ∷ !(Maybe (Int, Int))      -- ^ zoomSelectedPos last sent
    , csWorldSel ∷ !(Maybe (Int, Int, Int))  -- ^ worldSelectedTile last sent
    } deriving (Eq, Show)

emptyCursorSnapshot ∷ CursorSnapshot
emptyCursorSnapshot = CursorSnapshot Nothing Nothing

-- | Tracks overall world loading progress across both phases.
-- Phase 1: synchronous setup (timeline, ocean, climate, zoom cache, preview)
-- Phase 2: chunk generation (init queue draining)
data LoadPhase
    = LoadIdle
    | LoadPhase1 !Int !Int    -- ^ (currentStep, totalSteps) for setup work
    | LoadPhase2 !Int !Int    -- ^ (remaining, total) for chunk generation
    | LoadDone
    deriving (Eq, Show)
