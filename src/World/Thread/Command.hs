{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread.Command
    ( handleWorldCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Control.Concurrent.MVar (putMVar)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LoggerState)
import World.Types
import World.Thread.Command.Basic (handleWorldTickCommand
                                  , handleWorldSetCameraCommand
                                  , handleWorldDestroyCommand
                                  , handleWorldDestroyAllCommand)
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
                                   , handleWorldSetWorldCursorHoverBgTextureCommand
                                   , handleWorldSetMineAnchorCommand
                                   , handleWorldClearMineAnchorCommand
                                   , handleWorldDesignateMineCommand
                                   , handleWorldSetMineDesignateTextureCommand
                                   , handleWorldSetConstructAnchorCommand
                                   , handleWorldClearConstructAnchorCommand
                                   , handleWorldDesignateConstructCommand
                                   , handleWorldCancelConstructCommand
                                   , handleWorldSetConstructStatusCommand
                                   , handleWorldAddConstructProgressCommand
                                   , handleWorldSetConstructDesignateTextureCommand
                                   , handleWorldSetChopAnchorCommand
                                   , handleWorldClearChopAnchorCommand
                                   , handleWorldDesignateChopCommand
                                   , handleWorldCancelChopCommand
                                   , handleWorldSetChopDesignateTextureCommand)
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
                                 , handleWorldSetFluidTileCommand
                                 , handleWorldSetSlopeCommand
                                 , handleWorldSetCellCommand
                                 , handleWorldSetStructureCommand
                                 , handleWorldClearStructureCommand
                                 , handleWorldClearAllStructuresCommand
                                 , handleWorldDigTileCommand
                                 , handleWorldAddTileCommand)
import World.Thread.Command.Location
    (handleWorldMarkLocationContentsSpawnedCommand)

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
handleWorldCommand env logger (WorldSelectTileByCoord pageId gx gy mz)
  = handleWorldSelectTileByCoordCommand env logger pageId gx gy mz
handleWorldCommand env logger (WorldSetMineAnchor pageId gx gy)
  = handleWorldSetMineAnchorCommand env logger pageId gx gy
handleWorldCommand env logger (WorldClearMineAnchor pageId)
  = handleWorldClearMineAnchorCommand env logger pageId
handleWorldCommand env logger (WorldDesignateMine pageId gx1 gy1 gx2 gy2)
  = handleWorldDesignateMineCommand env logger pageId gx1 gy1 gx2 gy2
handleWorldCommand env logger (WorldSetMineDesignateTexture pageId texHandle)
  = handleWorldSetMineDesignateTextureCommand env logger pageId texHandle
handleWorldCommand env logger (WorldSetConstructAnchor pageId gx gy)
  = handleWorldSetConstructAnchorCommand env logger pageId gx gy
handleWorldCommand env logger (WorldClearConstructAnchor pageId)
  = handleWorldClearConstructAnchorCommand env logger pageId
handleWorldCommand env logger (WorldDesignateConstruct pageId gx1 gy1 gx2 gy2 tgt)
  = handleWorldDesignateConstructCommand env logger pageId gx1 gy1 gx2 gy2 tgt
handleWorldCommand env logger (WorldCancelConstruct pageId gx gy)
  = handleWorldCancelConstructCommand env logger pageId gx gy
handleWorldCommand env logger (WorldSetConstructStatus pageId gx gy st)
  = handleWorldSetConstructStatusCommand env logger pageId gx gy st
handleWorldCommand env logger (WorldAddConstructProgress pageId gx gy delta)
  = handleWorldAddConstructProgressCommand env logger pageId gx gy delta
handleWorldCommand env logger (WorldSetConstructDesignateTexture pageId cat texHandle)
  = handleWorldSetConstructDesignateTextureCommand env logger pageId cat texHandle
handleWorldCommand env logger (WorldSetChopAnchor pageId gx gy)
  = handleWorldSetChopAnchorCommand env logger pageId gx gy
handleWorldCommand env logger (WorldClearChopAnchor pageId)
  = handleWorldClearChopAnchorCommand env logger pageId
handleWorldCommand env logger (WorldDesignateChop pageId gx1 gy1 gx2 gy2 tag)
  = handleWorldDesignateChopCommand env logger pageId gx1 gy1 gx2 gy2 tag
handleWorldCommand env logger (WorldCancelChop pageId gx gy)
  = handleWorldCancelChopCommand env logger pageId gx gy
handleWorldCommand env logger (WorldSetChopDesignateTexture pageId texHandle)
  = handleWorldSetChopDesignateTextureCommand env logger pageId texHandle
handleWorldCommand env logger (WorldDigTile pageId gx gy ux uy amount skill percep)
  = handleWorldDigTileCommand env logger pageId gx gy ux uy amount skill percep
handleWorldCommand env logger (WorldAddTile pageId gx gy mat)
  = handleWorldAddTileCommand env logger pageId gx gy mat
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
handleWorldCommand env logger (WorldSetSlope pageId gx gy z bits)
  = handleWorldSetSlopeCommand env logger pageId gx gy z bits
handleWorldCommand env logger (WorldSetCell pageId gx gy z mat)
  = handleWorldSetCellCommand env logger pageId gx gy z mat
handleWorldCommand env logger (WorldSetStructure pageId gx gy slotTag texId faceId z)
  = handleWorldSetStructureCommand env logger pageId gx gy slotTag texId faceId z
handleWorldCommand env logger (WorldClearStructure pageId gx gy slotTag)
  = handleWorldClearStructureCommand env logger pageId gx gy slotTag
handleWorldCommand env logger (WorldClearAllStructures pageId)
  = handleWorldClearAllStructuresCommand env logger pageId
handleWorldCommand env logger (WorldDestroy pageId)
  = handleWorldDestroyCommand env logger pageId
handleWorldCommand env logger WorldDestroyAll
  = handleWorldDestroyAllCommand env logger
handleWorldCommand env _ (WorldApplyFluids batch)
  = handleApplyFluidsCommand env batch
handleWorldCommand env _ (WorldMarkLocationContentsSpawned pageId gx gy)
  = handleWorldMarkLocationContentsSpawnedCommand env pageId gx gy

-- | Sim → World: apply the sim's fluid writebacks to the ORIGINATING
--   world's tile data, resolved by the batch's page id — not every
--   visible world (that leaked one world's fluid sim into another that
--   shared chunk coords, #59). The world thread is the SOLE writer of
--   'wsTilesRef'; the sim only produces these batches. Acks the batch's
--   MVar (if any) after applying — the dump's fast-settle waits on it.
handleApplyFluidsCommand ∷ EngineEnv → FluidWritebackBatch → IO ()
handleApplyFluidsCommand env (FluidWritebackBatch pageId writebacks mAck) = do
    when (not (null writebacks)) $ do
        mgr ← readIORef (worldManagerRef env)
        case lookup pageId (wmWorlds mgr) of
            Nothing → pure ()  -- world gone (destroyed/unloaded) — drop the batch
            Just ws → do
                atomicModifyIORef' (wsTilesRef ws) $ \wtd →
                    (foldl' applyOneWriteback wtd writebacks, ())
                bumpQuadCacheGen ws
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

