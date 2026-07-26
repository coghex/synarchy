{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread.Command
    ( handleWorldCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Control.Concurrent.MVar (putMVar)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv, statRNGRef, unitQueue)
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
                                   , handleWorldSelectChunkByCoordCommand
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
                                   , handleWorldSetConstructLineModeCommand
                                   , handleWorldSetChopAnchorCommand
                                   , handleWorldClearChopAnchorCommand
                                   , handleWorldDesignateChopCommand
                                   , handleWorldCancelChopCommand
                                   , handleWorldSetChopDesignateTextureCommand
                                   , handleWorldSetTillAnchorCommand
                                   , handleWorldClearTillAnchorCommand
                                   , handleWorldDesignateTillCommand
                                   , handleWorldCancelTillCommand
                                   , handleWorldSetTillDesignateTextureCommand
                                   , handleWorldDesignatePlantCommand
                                   , handleWorldCancelPlantCommand
                                   , handleWorldSetPlantDesignateTextureCommand)
import World.Thread.Command.Texture (handleWorldSetTextureCommand)
import World.Thread.Command.Time (handleWorldSetTimeCommand
                                 , handleWorldSetDateCommand
                                 , handleWorldSetTimeScaleCommand)
import World.Thread.Command.Save (handleWorldSaveCommand
                                 , handleWorldLoadTransactionCommand
                                 , handleWorldLoadPublishCommand)
import World.Thread.Command.UI (handleWorldShowCommand, handleWorldHideCommand
                               , handleWorldSetMapModeCommand
                               , handleWorldSetToolModeCommand)
import World.Thread.Command.Edit (handleWorldDeleteTileCommand
                                 , handleWorldSetFluidTileCommand
                                 , handleWorldSetSlopeCommand
                                 , handleWorldSetVegCommand
                                 , handleWorldSetCellCommand
                                 , handleWorldSetStructureCommand
                                 , handleWorldClearStructureCommand
                                 , handleWorldClearAllStructuresCommand
                                 , handleWorldDigTileCommand
                                 , handleWorldAddTileCommand
                                 , handleWorldPlantRowCropAtCommand)
import World.Thread.Command.Location
    (handleWorldMarkLocationContentsSpawnedCommand
    ,handleWorldSetLocationLifecycleCommand
    ,handleWorldMarkLocationStampedCommand)

-- * Command Handler

handleWorldCommand ∷ EngineEnv → LoggerState → WorldCommand → IO ()
handleWorldCommand env logger (WorldInit pageId seed worldSize placeCount identity)
  = handleWorldInitCommand env logger pageId seed worldSize placeCount identity
handleWorldCommand env logger (WorldInitArena pageId)
  = handleWorldInitArenaCommand env logger pageId
handleWorldCommand env logger (WorldInitArenaDone pageId)
  = handleWorldInitArenaDoneCommand env logger pageId
handleWorldCommand env logger (WorldSetTexture pageId texType texHandle)
  = handleWorldSetTextureCommand (toWorldSimCapability env) logger pageId texType texHandle
handleWorldCommand env logger (WorldShow pageId)
  = handleWorldShowCommand (toWorldSimCapability env) logger pageId
handleWorldCommand env logger (WorldHide pageId)
  = handleWorldHideCommand (toWorldSimCapability env) logger pageId
handleWorldCommand env logger (WorldSetMapMode pageId mapMode)
  = handleWorldSetMapModeCommand (toWorldSimCapability env) logger pageId mapMode
handleWorldCommand env logger (WorldSetToolMode pageId toolMode)
  = handleWorldSetToolModeCommand (toWorldSimCapability env) logger pageId toolMode
handleWorldCommand env logger (WorldTick dt)
  = handleWorldTickCommand env logger dt
handleWorldCommand env logger (WorldSetCamera pageId x y)
  = handleWorldSetCameraCommand env logger pageId x y
handleWorldCommand env logger (WorldSetTime pageId hour minute)
  = handleWorldSetTimeCommand (toWorldSimCapability env) logger pageId hour minute
handleWorldCommand env logger (WorldSetDate pageId year month day)
  = handleWorldSetDateCommand (toWorldSimCapability env) logger pageId year month day
handleWorldCommand env logger (WorldSetTimeScale pageId scale)
  = handleWorldSetTimeScaleCommand (toWorldSimCapability env) logger pageId scale
handleWorldCommand env logger (WorldSetZoomCursorHover pageId x y)
  = handleWorldSetZoomCursorHoverCommand (toWorldSimCapability env) logger pageId x y
handleWorldCommand env logger (WorldSetZoomCursorSelect pageId)
  = handleWorldSetZoomCursorSelectCommand (toWorldSimCapability env) logger pageId
handleWorldCommand env logger (WorldSetZoomCursorDeselect pageId)
  = handleWorldSetZoomCursorDeselectCommand (toWorldSimCapability env) logger pageId
handleWorldCommand env logger (WorldSetZoomCursorSelectTexture pageId texHandle)
  = handleWorldSetZoomCursorSelectTextureCommand (toWorldSimCapability env) logger pageId texHandle
handleWorldCommand env logger (WorldSetZoomCursorHoverTexture pageId texHandle)
  = handleWorldSetZoomCursorHoverTextureCommand (toWorldSimCapability env) logger pageId texHandle
handleWorldCommand env logger (WorldSetWorldCursorHover pageId x y)
  = handleWorldSetWorldCursorHoverCommand (toWorldSimCapability env) logger pageId x y
handleWorldCommand env logger (WorldSetWorldCursorSelect pageId)
  = handleWorldSetWorldCursorSelectCommand (toWorldSimCapability env) logger pageId
handleWorldCommand env logger (WorldSetWorldCursorDeselect pageId)
  = handleWorldSetWorldCursorDeselectCommand (toWorldSimCapability env) logger pageId
handleWorldCommand env logger (WorldSelectTileByCoord pageId gx gy mz)
  = handleWorldSelectTileByCoordCommand (toWorldSimCapability env) logger pageId gx gy mz
handleWorldCommand env logger (WorldSelectChunkByCoord pageId gx gy)
  = handleWorldSelectChunkByCoordCommand (toWorldSimCapability env) logger pageId gx gy
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
handleWorldCommand env logger (WorldSetConstructLineMode pageId enabled)
  = handleWorldSetConstructLineModeCommand env logger pageId enabled
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
handleWorldCommand env logger (WorldSetTillAnchor pageId gx gy)
  = handleWorldSetTillAnchorCommand env logger pageId gx gy
handleWorldCommand env logger (WorldClearTillAnchor pageId)
  = handleWorldClearTillAnchorCommand env logger pageId
handleWorldCommand env logger (WorldDesignateTill pageId gx1 gy1 gx2 gy2)
  = handleWorldDesignateTillCommand env logger pageId gx1 gy1 gx2 gy2
handleWorldCommand env logger (WorldCancelTill pageId gx gy)
  = handleWorldCancelTillCommand env logger pageId gx gy
handleWorldCommand env logger (WorldSetTillDesignateTexture pageId texHandle)
  = handleWorldSetTillDesignateTextureCommand env logger pageId texHandle
handleWorldCommand env logger (WorldDesignatePlant pageId gx gy cropName)
  = handleWorldDesignatePlantCommand env logger pageId gx gy cropName
handleWorldCommand env logger (WorldCancelPlant pageId gx gy)
  = handleWorldCancelPlantCommand env logger pageId gx gy
handleWorldCommand env logger (WorldSetPlantDesignateTexture pageId texHandle)
  = handleWorldSetPlantDesignateTextureCommand env logger pageId texHandle
handleWorldCommand env logger (WorldSetVeg pageId gx gy z vegId)
  = handleWorldSetVegCommand (toWorldSimCapability env) logger pageId gx gy z vegId
handleWorldCommand env logger (WorldPlantRowCropAt pageId gx gy cropName)
  = handleWorldPlantRowCropAtCommand (toWorldSimCapability env) logger pageId gx gy cropName
handleWorldCommand env logger (WorldDigTile pageId gx gy ux uy amount skill percep)
  = handleWorldDigTileCommand env (statRNGRef env) (unitQueue env) logger
                              pageId gx gy ux uy amount skill percep
handleWorldCommand env logger (WorldAddTile pageId gx gy mat)
  = handleWorldAddTileCommand env logger pageId gx gy mat
handleWorldCommand env logger (WorldSetWorldCursorSelectTexture pageId texHandle)
  = handleWorldSetWorldCursorSelectTextureCommand (toWorldSimCapability env) logger pageId texHandle
handleWorldCommand env logger (WorldSetWorldCursorHoverTexture pageId texHandle)
  = handleWorldSetWorldCursorHoverTextureCommand (toWorldSimCapability env) logger pageId texHandle
handleWorldCommand env logger (WorldSetWorldCursorSelectBgTexture pageId texHandle)
  = handleWorldSetWorldCursorSelectBgTextureCommand (toWorldSimCapability env) logger pageId texHandle
handleWorldCommand env logger (WorldSetWorldCursorHoverBgTexture pageId texHandle)
  = handleWorldSetWorldCursorHoverBgTextureCommand (toWorldSimCapability env) logger pageId texHandle
handleWorldCommand env logger (WorldSave pageId saveName ts luaComponents luaRefs)
  = handleWorldSaveCommand env logger pageId saveName ts luaComponents luaRefs
handleWorldCommand env logger (WorldLoadTransaction requestId saveData matReg)
  = handleWorldLoadTransactionCommand env logger requestId saveData matReg
handleWorldCommand env logger (WorldLoadPublish requestId)
  = handleWorldLoadPublishCommand env logger requestId
handleWorldCommand env logger (WorldDeleteTile pageId gx gy)
  = handleWorldDeleteTileCommand env logger pageId gx gy
handleWorldCommand env logger (WorldSetFluidTile pageId gx gy fluidType)
  = handleWorldSetFluidTileCommand (toWorldSimCapability env) logger pageId gx gy fluidType
handleWorldCommand env logger (WorldSetSlope pageId gx gy z bits)
  = handleWorldSetSlopeCommand env logger pageId gx gy z bits
handleWorldCommand env logger (WorldSetCell pageId gx gy z mat)
  = handleWorldSetCellCommand env logger pageId gx gy z mat
handleWorldCommand env logger (WorldSetStructure pageId gx gy slotTag texId faceId z)
  = handleWorldSetStructureCommand (toWorldSimCapability env) logger pageId gx gy slotTag texId faceId z
handleWorldCommand env logger (WorldClearStructure pageId gx gy slotTag)
  = handleWorldClearStructureCommand (toWorldSimCapability env) logger pageId gx gy slotTag
handleWorldCommand env logger (WorldClearAllStructures pageId)
  = handleWorldClearAllStructuresCommand (toWorldSimCapability env) logger pageId
handleWorldCommand env logger (WorldDestroy pageId)
  = handleWorldDestroyCommand env logger pageId
handleWorldCommand env logger WorldDestroyAll
  = handleWorldDestroyAllCommand env logger
handleWorldCommand env _ (WorldApplyFluids batch)
  = handleApplyFluidsCommand env batch
handleWorldCommand env _ (WorldMarkLocationContentsSpawned pageId iid)
  = handleWorldMarkLocationContentsSpawnedCommand (toWorldSimCapability env) pageId iid
handleWorldCommand env _ (WorldSetLocationLifecycle pageId iid lifecycle)
  = handleWorldSetLocationLifecycleCommand (toWorldSimCapability env) pageId iid lifecycle
handleWorldCommand env _ (WorldMarkLocationStamped pageId gx gy)
  = handleWorldMarkLocationStampedCommand (toWorldSimCapability env) pageId gx gy

-- | Sim → World: apply the sim's fluid writebacks to the ORIGINATING
--   world's tile data, resolved by the batch's page id — not every
--   visible world (that leaked one world's fluid sim into another that
--   shared chunk coords, #59). The world thread is the SOLE writer of
--   'wsTilesRef'; the sim only produces these batches. Acks the batch's
--   MVar (if any) after applying — the dump's fast-settle waits on it.
handleApplyFluidsCommand ∷ EngineEnv → FluidWritebackBatch → IO ()
handleApplyFluidsCommand env (FluidWritebackBatch pageId writebacks mAck) = do
    when (not (null writebacks)) $ do
        mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
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

