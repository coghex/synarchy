{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.State.Types
    ( WorldState(..)
    , emptyWorldState
    , WorldManager(..)
    , emptyWorldManager
    , CursorSnapshot(..)
    , LoadPhase(..)
    ) where

import UPrelude
import Data.IORef (IORef, newIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Engine.Graphics.Camera (CameraFacing(..))
import World.Base (GeoFeatureId(..))
import World.Cursor.Types (CursorState(..), emptyCursorState)
import World.Page.Types (WorldPageId(..))
import World.Chunk.Types (ChunkCoord(..))
import World.Tile.Types (WorldTileData(..), emptyWorldTileData)
import World.Render.Camera.Types (WorldCamera(..), WorldQuadCache(..))
import World.Render.Textures.Types (WorldTextures(..), defaultWorldTextures)
import World.Render.Zoom.Types (ZoomChunkEntry(..), ZoomQuadCache(..), BakedZoomEntry(..), ZoomMapMode(..))
import World.Tool.Types (ToolMode(..))
import World.Generate.Types (WorldGenParams(..))
import World.Time.Types (WorldTime(..), WorldDate(..), defaultWorldTime, defaultWorldDate)
import World.Region.Types (RegionCoord(..))
import World.Weather.Types (RegionClimate(..))

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
    , wsBakedZoomRef ∷ IORef (V.Vector BakedZoomEntry, WorldTextures, CameraFacing)  -- ^ Pre-baked
    , wsBakedBgRef ∷ IORef (V.Vector BakedZoomEntry, WorldTextures, CameraFacing)    -- ^ Pre-baked background entries with resolved textures and vertices
    , wsInitQueueRef ∷ IORef [ChunkCoord]  -- ^ Queue of chunks to generate at world init (for progress tracking)
    , wsClimateRef ∷ IORef (HM.HashMap RegionCoord RegionClimate)  -- ^ Regional climate data (temperature, humidity)
    , wsRiverFlowRef ∷ IORef (HM.HashMap GeoFeatureId Float)
    , wsMapModeRef ∷ IORef ZoomMapMode
    , wsCursorRef ∷ IORef CursorState
    , wsToolModeRef ∷ IORef ToolMode
    , wsCursorSnapshotRef ∷ IORef CursorSnapshot
    , wsLoadPhaseRef ∷ IORef LoadPhase
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
    bakedZoomRef ← newIORef (V.empty, defaultWorldTextures, FaceSouth)
    bakedBgRef   ← newIORef (V.empty, defaultWorldTextures, FaceSouth)
    wsInitQueueRef ← newIORef []
    wsClimateRef ← newIORef HM.empty
    wsRiverFlowRef ← newIORef HM.empty
    wsMapModeRef ← newIORef ZMDefault
    wsCursorRef ← newIORef emptyCursorState
    wsToolModeRef ← newIORef DefaultTool
    wsCursorSnapshotRef ← newIORef emptyCursorSnapshot
    wsLoadPhaseRef ← newIORef LoadIdle
    return $ WorldState tilesRef cameraRef texturesRef genParamsRef
                        timeRef dateRef timeScaleRef zoomCacheRef
                        quadCacheRef zoomQCRef bgQCRef
                        bakedZoomRef bakedBgRef wsInitQueueRef
                        wsClimateRef wsRiverFlowRef wsMapModeRef
                        wsCursorRef wsToolModeRef wsCursorSnapshotRef
                        wsLoadPhaseRef

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
