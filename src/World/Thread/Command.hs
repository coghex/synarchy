{-# LANGUAGE Strict #-}
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
import World.Thread.Command.BoundSpawn
    (handleWorldSpawnBoundBuildingCommand)
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
import World.Plant.Validate (revalidatePlantDesignations)
import World.Construct.Revalidate
    (ConstructScope(..), revalidateConstructDesignations)
import World.Thread.Command.Location
    (handleWorldMarkLocationContentsSpawnedCommand
    ,handleWorldRegisterLocationEncounterOccupantsCommand
    ,handleWorldRegisterLocationSignificantSpawnCommand
    ,handleWorldSetLocationEncounterOccupantStateCommand
    ,handleWorldSetLocationEncounterEpisodeStateCommand
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
handleWorldCommand env logger
    (WorldSpawnBoundBuilding bid defName gx gy gz pageId bindGen)
  = handleWorldSpawnBoundBuildingCommand env logger bid defName gx gy gz
                                         pageId bindGen
handleWorldCommand env logger
    (WorldDesignateConstruct pageId gx1 gy1 gx2 gy2 tgt mBindGen)
  = handleWorldDesignateConstructCommand env logger pageId gx1 gy1 gx2 gy2 tgt
                                         mBindGen
handleWorldCommand env logger WorldRevalidateConstructAll = do
  mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
  forM_ (wmWorlds mgr) $ \(_, ws) →
      void $ revalidateConstructDesignations env logger ws ConstructWholePage
handleWorldCommand env logger (WorldCancelConstruct pageId gx gy att)
  = handleWorldCancelConstructCommand env logger pageId gx gy att
handleWorldCommand env logger (WorldSetConstructStatus pageId gx gy st att win)
  = handleWorldSetConstructStatusCommand env logger pageId gx gy st att win
handleWorldCommand env logger (WorldAddConstructProgress pageId gx gy delta att)
  = handleWorldAddConstructProgressCommand env logger pageId gx gy delta att
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
handleWorldCommand env logger (WorldCancelChop pageId gx gy mIid)
  = handleWorldCancelChopCommand env logger pageId gx gy mIid
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
handleWorldCommand env logger (WorldSave pageId saveName ts luaComponents luaRefs mAuto)
  = handleWorldSaveCommand env logger pageId saveName ts luaComponents luaRefs mAuto
handleWorldCommand env logger (WorldLoadTransaction requestId saveData matReg)
  = handleWorldLoadTransactionCommand env logger requestId saveData matReg
handleWorldCommand env logger (WorldLoadPublish requestId)
  = handleWorldLoadPublishCommand env logger requestId
handleWorldCommand env logger (WorldDeleteTile pageId gx gy)
  = handleWorldDeleteTileCommand env logger pageId gx gy
handleWorldCommand env logger (WorldSetFluidTile pageId gx gy fluidType)
  = handleWorldSetFluidTileCommand env logger pageId gx gy fluidType
handleWorldCommand env logger (WorldSetSlope pageId gx gy z bits)
  = handleWorldSetSlopeCommand env logger pageId gx gy z bits
handleWorldCommand env logger (WorldSetCell pageId gx gy z mat)
  = handleWorldSetCellCommand env logger pageId gx gy z mat
handleWorldCommand env logger (WorldSetStructure pageId gx gy slotTag texId faceId z tok)
  = handleWorldSetStructureCommand env logger pageId gx gy slotTag texId faceId z tok
handleWorldCommand env logger (WorldClearStructure pageId gx gy slotTag)
  = handleWorldClearStructureCommand env logger pageId gx gy slotTag
handleWorldCommand env logger (WorldClearAllStructures pageId)
  = handleWorldClearAllStructuresCommand env logger pageId
handleWorldCommand env logger (WorldDestroy pageId)
  = handleWorldDestroyCommand env logger pageId
handleWorldCommand env logger WorldDestroyAll
  = handleWorldDestroyAllCommand env logger
handleWorldCommand env logger (WorldApplyFluids batch)
  = handleApplyFluidsCommand env logger batch
handleWorldCommand env _ (WorldMarkLocationContentsSpawned pageId iid)
  = handleWorldMarkLocationContentsSpawnedCommand (toWorldSimCapability env) pageId iid
handleWorldCommand env _ (WorldRegisterLocationEncounterOccupants pageId iid occupants)
  = handleWorldRegisterLocationEncounterOccupantsCommand
      (toWorldSimCapability env) pageId iid occupants
handleWorldCommand env _ (WorldRegisterLocationSignificantSpawn pageId iid slot itemId)
  = handleWorldRegisterLocationSignificantSpawnCommand
      (toWorldSimCapability env) pageId iid slot itemId
handleWorldCommand env _ (WorldSetLocationEncounterOccupantState pageId iid uid
        engaged returning)
  = handleWorldSetLocationEncounterOccupantStateCommand
      (toWorldSimCapability env) pageId iid uid engaged returning
handleWorldCommand env _ (WorldSetLocationEncounterEpisodeState pageId iid
        active aggressionAnnounced disengageAnnounced)
  = handleWorldSetLocationEncounterEpisodeStateCommand
      (toWorldSimCapability env) pageId iid active aggressionAnnounced
      disengageAnnounced
handleWorldCommand env _ (WorldSetLocationLifecycle pageId iid lifecycle)
  = handleWorldSetLocationLifecycleCommand (toWorldSimCapability env) pageId iid lifecycle
handleWorldCommand env logger (WorldMarkLocationStamped pageId gx gy mWindow)
  = handleWorldMarkLocationStampedCommand (toWorldSimCapability env) logger
      pageId gx gy mWindow

-- | Sim → World: apply the sim's fluid writebacks to the ORIGINATING
--   world's tile data, resolved by the batch's page id — not every
--   visible world (that leaked one world's fluid sim into another that
--   shared chunk coords, #59). The world thread is the SOLE writer of
--   'wsTilesRef'; the sim only produces these batches. Acks the batch's
--   MVar (if any) after applying — the dump's fast-settle waits on it.
--
--   Each writeback is applied only if it is FRESH: its 'fwEditGen' must
--   equal the page's own current live-edit generation for that chunk
--   ('wsChunkEditGenRef'). A batch the sim computed before a live edit
--   carries the pre-edit generation and is dropped, so it cannot
--   overwrite the edit the player just made (#1596). The decision is
--   per chunk, so an edit to one chunk never drops another chunk's
--   writeback from the same batch, and it is taken here rather than in
--   'applyOneWriteback' so the tiles are read and written exactly once.
--
--   The ack fires whatever the outcome — batch empty, page gone, or
--   every writeback dropped — or 'SimFastSettleAll' and the @--dump@
--   fast-settle path would block forever waiting on it.
handleApplyFluidsCommand ∷ EngineEnv → LoggerState → FluidWritebackBatch
                         → IO ()
handleApplyFluidsCommand env logger (FluidWritebackBatch pageId writebacks mAck) = do
    when (not (null writebacks)) $ do
        mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
        case lookup pageId (wmWorlds mgr) of
            Nothing → pure ()  -- world gone (destroyed/unloaded) — drop the batch
            Just ws → do
                gens ← readIORef (wsChunkEditGenRef ws)
                let fresh = filter (writebackIsFresh gens) writebacks
                when (not (null fresh)) $ do
                    atomicModifyIORef' (wsTilesRef ws) $ \wtd →
                        (foldl' applyOneWriteback wtd fresh, ())
                    bumpQuadCacheGen ws
                    writeIORef (wsZoomQuadCacheRef ws) Nothing
                    writeIORef (wsBgQuadCacheRef ws)   Nothing
                    -- #1858: an accepted writeback replaces lcSurfaceMap
                    -- without touching ctVeg, so it can move a designated
                    -- tile's resolved surface off its tilled cell with no
                    -- vegetation edit anywhere. Omitting this path would
                    -- let admission and continuous validation disagree.
                    _ ← revalidatePlantDesignations logger ws
                    -- #1844: for the same reason, and scoped to the
                    -- chunk the writeback replaced rather than the page.
                    _ ← revalidateConstructDesignations env logger ws
                            (ConstructChunks (map fwCoord fresh))
                    pure ()
    forM_ mAck (`putMVar` ())

-- | Is this writeback derived from the chunk state the page currently
--   holds? True exactly when the sim stamped it with the live-edit
--   generation the world has issued for that chunk — an absent entry
--   meaning generation 0, the baseline a never-edited (or evicted and
--   reloaded) chunk sits at on BOTH sides.
--
--   Equality, not @>=@: a writeback claiming a generation this page never
--   issued is no more derived from the current chunk than one claiming an
--   older, and that is exactly what a batch in flight across a chunk
--   eviction looks like (the eviction retires the entry, the reload
--   re-seeds the sim at 0).
writebackIsFresh ∷ HM.HashMap ChunkCoord Word64 → FluidWriteback → Bool
writebackIsFresh gens fw =
    fwEditGen fw ≡ HM.lookupDefault 0 (fwCoord fw) gens

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
