{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread.Command
    ( handleWorldCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Control.Concurrent.MVar (putMVar)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logDebug, logError, logWarn
                       , LogCategory(..), LoggerState)
import Engine.Graphics.Camera (Camera2D(..))
import World.Types
import World.Constants (seaLevel)
import World.Generate (generateChunk)
import World.Generate.Constants (chunkLoadRadius)
import World.Generate.Timeline (applyTimelineFast)
import World.Geology.Log (formatTimeline, formatPlatesSummary)
import World.Fluids (computeOceanMap, isOceanChunk)
import World.Plate (generatePlates, elevationAtGlobal)
import World.Preview (buildPreviewImage, PreviewImage(..))
import World.Render (surfaceHeadroom)
import World.ZoomMap (buildZoomCache)
import World.Weather (initEarlyClimate, formatWeather, defaultClimateParams)
import World.Thread.Helpers (sendGenLog, unWorldPageId)
import World.Thread.ChunkLoading (maxChunksPerTick)
import World.Thread.Command.Basic (handleWorldTickCommand
                                  , handleWorldSetCameraCommand
                                  , handleWorldDestroyCommand)
import World.Thread.Command.Init (handleWorldInitCommand
                                 , handleWorldInitArenaCommand
                                 , handleWorldInitArenaDoneCommand)
import World.Thread.Command.Cursor (handleWorldSetZoomCursorHoverCommand
                                   , handleWorldSetZoomCursorSelectCommand
                                   , handleWorldSetZoomCursorDeselectCommand
                                   , handleWorldSetZoomCursorSelectTextureCommand
                                   , handleWorldSetZoomCursorHoverTextureCommand
                                   , handleWorldSetWorldCursorHoverCommand
                                   , handleWorldSetWorldCursorSelectCommand
                                   , handleWorldSetWorldCursorDeselectCommand
                                   , handleWorldSelectTileByCoordCommand
                                   , handleWorldSetWorldCursorSelectTextureCommand
                                   , handleWorldSetWorldCursorHoverTextureCommand
                                   , handleWorldSetWorldCursorSelectBgTextureCommand
                                   , handleWorldSetWorldCursorHoverBgTextureCommand)
import World.Thread.Command.Texture (handleWorldSetTextureCommand)
import World.Thread.Command.Time (handleWorldSetTimeCommand
                                 , handleWorldSetDateCommand
                                 , handleWorldSetTimeScaleCommand)
import World.Thread.Command.Save (handleWorldSaveCommand
                                 , handleWorldLoadSaveCommand)
import World.Thread.Command.UI (handleWorldShowCommand, handleWorldHideCommand
                               , handleWorldSetMapModeCommand
                               , handleWorldSetToolModeCommand)
import World.Thread.Command.Edit (handleWorldDeleteTileCommand
                                 , handleWorldSetFluidTileCommand)

-- * Command Handler

handleWorldCommand ∷ EngineEnv → LoggerState → WorldCommand → IO ()
handleWorldCommand env logger (WorldInit pageId seed worldSize placeCount)
  = handleWorldInitCommand env logger pageId seed worldSize placeCount
handleWorldCommand env logger (WorldInitArena pageId)
  = handleWorldInitArenaCommand env logger pageId
handleWorldCommand env logger (WorldInitArenaDone pageId)
  = handleWorldInitArenaDoneCommand env logger pageId
handleWorldCommand env logger (WorldSetTexture pageId texType texHandle)
  = handleWorldSetTextureCommand env logger pageId texType texHandle
handleWorldCommand env logger (WorldShow pageId)
  = handleWorldShowCommand env logger pageId
handleWorldCommand env logger (WorldHide pageId)
  = handleWorldHideCommand env logger pageId
handleWorldCommand env logger (WorldSetMapMode pageId mapMode)
  = handleWorldSetMapModeCommand env logger pageId mapMode
handleWorldCommand env logger (WorldSetToolMode pageId toolMode)
  = handleWorldSetToolModeCommand env logger pageId toolMode
handleWorldCommand env logger (WorldTick dt)
  = handleWorldTickCommand env logger dt
handleWorldCommand env logger (WorldSetCamera pageId x y)
  = handleWorldSetCameraCommand env logger pageId x y
handleWorldCommand env logger (WorldSetTime pageId hour minute)
  = handleWorldSetTimeCommand env logger pageId hour minute
handleWorldCommand env logger (WorldSetDate pageId year month day)
  = handleWorldSetDateCommand env logger pageId year month day
handleWorldCommand env logger (WorldSetTimeScale pageId scale)
  = handleWorldSetTimeScaleCommand env logger pageId scale
handleWorldCommand env logger (WorldSetZoomCursorHover pageId x y)
  = handleWorldSetZoomCursorHoverCommand env logger pageId x y
handleWorldCommand env logger (WorldSetZoomCursorSelect pageId)
  = handleWorldSetZoomCursorSelectCommand env logger pageId
handleWorldCommand env logger (WorldSetZoomCursorDeselect pageId)
  = handleWorldSetZoomCursorDeselectCommand env logger pageId
handleWorldCommand env logger (WorldSetZoomCursorSelectTexture pageId texHandle)
  = handleWorldSetZoomCursorSelectTextureCommand env logger pageId texHandle
handleWorldCommand env logger (WorldSetZoomCursorHoverTexture pageId texHandle)
  = handleWorldSetZoomCursorHoverTextureCommand env logger pageId texHandle
handleWorldCommand env logger (WorldSetWorldCursorHover pageId x y)
  = handleWorldSetWorldCursorHoverCommand env logger pageId x y
handleWorldCommand env logger (WorldSetWorldCursorSelect pageId)
  = handleWorldSetWorldCursorSelectCommand env logger pageId
handleWorldCommand env logger (WorldSetWorldCursorDeselect pageId)
  = handleWorldSetWorldCursorDeselectCommand env logger pageId
handleWorldCommand env logger (WorldSelectTileByCoord pageId gx gy)
  = handleWorldSelectTileByCoordCommand env logger pageId gx gy
handleWorldCommand env logger (WorldSetWorldCursorSelectTexture pageId texHandle)
  = handleWorldSetWorldCursorSelectTextureCommand env logger pageId texHandle
handleWorldCommand env logger (WorldSetWorldCursorHoverTexture pageId texHandle)
  = handleWorldSetWorldCursorHoverTextureCommand env logger pageId texHandle
handleWorldCommand env logger (WorldSetWorldCursorSelectBgTexture pageId texHandle)
  = handleWorldSetWorldCursorSelectBgTextureCommand env logger pageId texHandle
handleWorldCommand env logger (WorldSetWorldCursorHoverBgTexture pageId texHandle)
  = handleWorldSetWorldCursorHoverBgTextureCommand env logger pageId texHandle
handleWorldCommand env logger (WorldSave pageId saveName ts luaBlobs)
  = handleWorldSaveCommand env logger pageId saveName ts luaBlobs
handleWorldCommand env logger (WorldLoadSave pageId saveName)
  = handleWorldLoadSaveCommand env logger pageId saveName
handleWorldCommand env logger (WorldDeleteTile pageId gx gy)
  = handleWorldDeleteTileCommand env logger pageId gx gy
handleWorldCommand env logger (WorldSetFluidTile pageId gx gy fluidType)
  = handleWorldSetFluidTileCommand env logger pageId gx gy fluidType
handleWorldCommand env logger (WorldDestroy pageId)
  = handleWorldDestroyCommand env logger pageId
handleWorldCommand env _ (WorldApplyFluids batch)
  = handleApplyFluidsCommand env batch

-- | Sim → World: apply the sim's fluid writebacks to the visible
--   world's tile data. The world thread is the SOLE writer of
--   'wsTilesRef'; the sim only produces these batches. Acks the batch's
--   MVar (if any) after applying — the dump's fast-settle waits on it.
handleApplyFluidsCommand ∷ EngineEnv → FluidWritebackBatch → IO ()
handleApplyFluidsCommand env (FluidWritebackBatch writebacks mAck) = do
    when (not (null writebacks)) $ do
        mgr ← readIORef (worldManagerRef env)
        forM_ (wmVisible mgr) $ \pageId →
            case lookup pageId (wmWorlds mgr) of
                Nothing → pure ()
                Just ws → do
                    atomicModifyIORef' (wsTilesRef ws) $ \wtd →
                        (foldl' applyOneWriteback wtd writebacks, ())
                    writeIORef (wsQuadCacheRef ws)     Nothing
                    writeIORef (wsZoomQuadCacheRef ws) Nothing
                    writeIORef (wsBgQuadCacheRef ws)   Nothing
    forM_ mAck (`putMVar` ())

-- | Overwrite one chunk's sim-owned fields (fluid + terrain surface +
--   render surface + side decos), preserving everything else.
applyOneWriteback ∷ WorldTileData → FluidWriteback → WorldTileData
applyOneWriteback wtd fw =
    case lookupChunk (fwCoord fw) wtd of
        Nothing → wtd
        Just lc →
            let lc' = lc { lcFluidMap          = fwFluid fw
                         , lcTerrainSurfaceMap = fwTerrain fw
                         , lcSurfaceMap        = fwSurf fw
                         , lcSideDeco          = fwSideDeco fw
                         }
            in wtd { wtdChunks = HM.insert (fwCoord fw) lc' (wtdChunks wtd) }

