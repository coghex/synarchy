{-# LANGUAGE Strict #-}
module World.Thread.Command
    ( handleWorldCommand
    , handleApplyFluidsCommandWith
    , applyFluidWritebacks
    , batchIsCurrentIncarnation
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (partition)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Control.Concurrent.MVar (putMVar)
import Control.Exception (SomeException, throwIO, try)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv, statRNGRef, unitQueue)
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import World.Types
import World.Chunk.Admit (pageIncarnation)
import World.Chunk.Residency (ChunkGeneration)
import Sim.Fluid.Reaction (ReactionResult(..))
import World.Thread.Command.Reaction
    (commitReactions, convergeRejectedReactions, reactionChunks
    , reactionIsFresh)
import World.Thread.Command.Basic (handleWorldTickCommand
                                  , handleWorldSetCameraCommand
                                  , handleWorldDestroyCommand
                                  , handleWorldDestroyAllCommand
                                  , handleWorldRecordPortableKnowledgeCommand)
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
                                   , handleWorldSetConstructLineModeCommand
                                   , handleWorldSetConstructStructureTargetCommand
                                   , handleWorldDesignateChopInstancesCommand
                                   , handleWorldEraseChopInstancesCommand
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
    (WorldSpawnBoundBuilding bid defName gx gy gz pageId bindGen epoch)
  = handleWorldSpawnBoundBuildingCommand env logger bid defName gx gy gz
                                         pageId bindGen epoch
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
handleWorldCommand env logger (WorldSetConstructLineMode pageId enabled)
  = handleWorldSetConstructLineModeCommand env logger pageId enabled
handleWorldCommand env logger (WorldSetConstructStructureTarget pageId mPiece)
  = handleWorldSetConstructStructureTargetCommand env logger pageId mPiece
handleWorldCommand env logger (WorldDesignateChopInstances pageId iids tag)
  = handleWorldDesignateChopInstancesCommand env logger pageId iids tag
handleWorldCommand env logger (WorldEraseChopInstances pageId iids)
  = handleWorldEraseChopInstancesCommand env logger pageId iids
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
handleWorldCommand env _ (WorldRecordPortableKnowledge epoch iid mObs)
  = handleWorldRecordPortableKnowledgeCommand env epoch iid mObs
handleWorldCommand env logger (WorldApplyFluids batch)
  = handleApplyFluidsCommand env logger batch
handleWorldCommand env _ (WorldMarkLocationContentsSpawned pageId iid)
  = handleWorldMarkLocationContentsSpawnedCommand (toWorldSimCapability env) pageId iid
handleWorldCommand env _ (WorldRegisterLocationEncounterOccupants pageId iid occupants)
  = handleWorldRegisterLocationEncounterOccupantsCommand
      (toWorldSimCapability env) pageId iid occupants
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
--   A batch is refused outright unless it was computed against the
--   incarnation of that page id the manager currently holds (#2477).
--   That decision comes FIRST, because the per-chunk fence below cannot
--   make it: a replacement page has issued no live-edit generations at
--   all, so every chunk of it reads as generation zero — exactly where a
--   batch computed against the previous incarnation was stamped.
--
--   Each surviving writeback is applied only if it is FRESH: its 'fwEditGen' must
--   equal the page's own current live-edit generation for that chunk
--   ('wsChunkEditGenRef'). A batch the sim computed before a live edit
--   carries the pre-edit generation and is dropped, so it cannot
--   overwrite the edit the player just made (#1596). The decision is
--   per chunk, so an edit to one chunk never drops another chunk's
--   writeback from the same batch, and it is taken here rather than in
--   'applyOneWriteback' so the tiles are read and written exactly once.
--
--   The ack fires whatever the outcome — batch empty, page gone, the
--   whole batch refused as a previous incarnation's, every
--   writeback dropped, or the application itself raising — or
--   'SimFastSettleAll' and the @--dump@ fast-settle path would block
--   forever waiting on it. Only the first three are
--   'World.Command.Types.FluidAckApplied'; a raise acks
--   'World.Command.Types.FluidAckFailed' and is then RETHROWN, so the
--   world worker keeps the fail-stop it has always had (#2334).
handleApplyFluidsCommand ∷ EngineEnv → LoggerState → FluidWritebackBatch
                         → IO ()
handleApplyFluidsCommand = handleApplyFluidsCommandWith applyFluidWritebacks

-- | 'handleApplyFluidsCommand' with its application step supplied by the
--   caller (#2334). Production passes 'applyFluidWritebacks'; the
--   headless gate injects a throwing step to prove the ack is published
--   as a failure and the exception still leaves the handler, without
--   depending on a real writeback application ever failing. Same seam
--   idiom as 'World.Save.Autosave.prepareAutosaveCycleWithSync'.
--
--   The ack is delivered BEFORE the rethrow, so the waiter is released
--   even though this call does not return normally.
handleApplyFluidsCommandWith
    ∷ (EngineEnv → LoggerState → WorldPageId → Maybe ChunkGeneration
       → [FluidWriteback] → [ReactionResult] → IO ())
    → EngineEnv → LoggerState → FluidWritebackBatch → IO ()
handleApplyFluidsCommandWith apply env logger
        (FluidWritebackBatch pageId mEpoch writebacks reactions mAck) = do
    applied ← try (apply env logger pageId mEpoch writebacks reactions)
    case applied of
        Right () → forM_ mAck (`putMVar` FluidAckApplied)
        Left (e ∷ SomeException) → do
            forM_ mAck (`putMVar` FluidAckFailed (tshow e))
            throwIO e

-- | The production application step behind 'handleApplyFluidsCommand':
--   everything between dequeuing the batch and acknowledging it.
applyFluidWritebacks ∷ EngineEnv → LoggerState → WorldPageId
                     → Maybe ChunkGeneration → [FluidWriteback]
                     → [ReactionResult] → IO ()
applyFluidWritebacks env logger pageId mEpoch writebacks reactions = do
    when (not (null writebacks) ∨ not (null reactions)) $ do
        mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
        case lookup pageId (wmWorlds mgr) of
            Nothing → pure ()  -- world gone (destroyed/unloaded) — drop the batch
            Just ws → do
                -- The incarnation fence (#2477), ahead of the per-chunk
                -- one: a batch computed against a page this id no longer
                -- names never reaches 'writebackIsFresh', which would
                -- read every one of its writebacks as fresh. It covers
                -- the reaction results too (#2485) — they were computed
                -- from the same chunks, so a delivery this refuses has
                -- no admissible half at all.
                live ← pageIncarnation ws
                if not (batchIsCurrentIncarnation live mEpoch)
                  then logDebug logger CatWorld $
                    "Refusing a fluid writeback batch for "
                    <> unWorldPageId pageId
                    <> ": computed against incarnation " <> tshow mEpoch
                    <> ", the live page is " <> tshow live
                  else do
                    gens ← readIORef (wsChunkEditGenRef ws)
                    let (admitted, rejected) =
                            partition (reactionIsFresh gens) reactions
                        -- A rejected result's fluid outcome goes with
                        -- it (#2485 requirement 6). Its writeback is
                        -- the OTHER half of the same reaction — the
                        -- annihilated lava and the debited water — and
                        -- landing that without the stone would destroy
                        -- volume with no product to account for it, and
                        -- then be the state the convergence re-seed
                        -- reads back as authoritative.
                        quarantined = HS.fromList
                            (concatMap reactionChunks rejected)
                        fresh = [ w | w ← writebacks
                                    , writebackIsFresh gens w
                                    , not (HS.member (fwCoord w) quarantined) ]
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
                    -- The stone goes on TOP of this delivery's own fluid
                    -- result, in the same handler and before any
                    -- generation moves, so the reaction's surviving water
                    -- is not dropped by the generation its own stone
                    -- mints.
                    commitReactions env logger pageId ws admitted
                    convergeRejectedReactions env logger pageId ws rejected

-- | Is this batch's stamped incarnation the one the live page IS?
--
--   Equality against the page's own epoch, and 'Nothing' — the sim held
--   no epoch for this page, because no topology-bearing message had
--   reached it — is refused rather than waved through: an unstamped
--   batch makes no claim about which incarnation it was computed
--   against, and that claim is the whole content of the fence.
--
--   Kept apart from the IO around it so the decision can be exercised
--   directly, exactly as 'writebackIsFresh' is.
batchIsCurrentIncarnation ∷ ChunkGeneration → Maybe ChunkGeneration → Bool
batchIsCurrentIncarnation live = maybe False (≡ live)

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
