{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread.ChunkLoading
    ( updateChunkLoading
    , drainInitQueues
    , dispatchLocationStamps
    , maxChunksPerTick
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.List (partition, sortOn)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Camera (Camera2D(..))
import qualified Data.HashSet as HS
import World.Types
import World.Generate (generateLoadedChunk, cameraChunkCoord)
import World.Generate.Arena (generateFlatChunk)
import World.Generate.Constants (chunkLoadRadius)
import World.Grid (zoomFadeEnd)
import World.Slope (recomputeNeighborSlopes
                    , slopeRecomputeAffected
                    , patchEdgeStrata
                    , chunkNeighbors)
import World.SideFace.Compute (computeChunkSideDecos)
import World.Thread.Helpers (unWorldPageId)
import Engine.Scripting.Lua.Types (LuaMsg(..))
import World.Edit.Apply (replayEdits)
import World.Mine.Apply (applyDigSlopesTd)
import World.Construct.Apply (applyConstructSlopesTd)
import Sim.Command.Types (SimCommand(..))

-- | Maximum chunks to generate per world loop iteration.
--   parMap uses all available cores, so larger batches
--   utilize parallelism better during initial world generation.
maxChunksPerTick ∷ Int
maxChunksPerTick = 8

updateChunkLoading ∷ EngineEnv → LoggerState → IO ()
updateChunkLoading env _logger = do
    camera ← readIORef (cameraRef env)
    catalog ← readIORef (floraCatalogRef env)
    registry ← readIORef (materialRegistryRef env)
    let zoom = camZoom camera
    when (zoom < (zoomFadeEnd + 0.5)) $ do
        manager ← readIORef (worldManagerRef env)
        let (camX, camY) = camPosition camera
            facing = camFacing camera
            camChunk = cameraChunkCoord facing camX camY
            ChunkCoord ccx ccy = camChunk
            neededCoords = [ ChunkCoord (ccx + dx) (ccy + dy)
                           | dx ← [-chunkLoadRadius .. chunkLoadRadius]
                           , dy ← [-chunkLoadRadius .. chunkLoadRadius]
                           ]
        forM_ (wmVisible manager) $ \pageId →
            case lookup pageId (wmWorlds manager) of
                Nothing → return ()
                Just worldState → do
                    mParams ← readIORef (wsGenParamsRef worldState)
                    case mParams of
                        Nothing → return ()
                        Just params → do
                            tileData ← readIORef (wsTilesRef worldState)
                            let halfSize = wgpWorldSize params `div` 2
                                -- Shared with the slope recompute's seam
                                -- handling (World.Slope re-exports the
                                -- canonical World.Chunk.Types.wrapChunkCoordU,
                                -- also re-exported via World.Fluid.Internal for
                                -- the fluid/magma/zoom paths) so insert-time
                                -- and lookup-time wrapping can't diverge.
                                wrapChunkU = wrapChunkCoordU (wgpWorldSize params)
                                inBoundsV (ChunkCoord cx cy) =
                                    let v = cx + cy
                                        halfTiles = halfSize * chunkSize
                                    in abs (v * chunkSize) ≤ halfTiles
                                validCoords = map wrapChunkU $ filter inBoundsV neededCoords
                            let (_toPromote, toGenerate) = partitionChunks validCoords tileData
                            let toGenerateSorted = sortOn (\(ChunkCoord cx cy) →
                                    abs (cx - ccx) + abs (cy - ccy)) toGenerate
                                batch = take maxChunksPerTick toGenerateSorted
                            let isArena = isArenaParams params
                            when (not $ null batch) $ do
                                let seed = wgpSeed params
                                let !newChunks = if isArena
                                        then map generateFlatChunk batch
                                        else parMap rdeepseq
                                            (generateLoadedChunk registry catalog params)
                                            batch
                                -- Replay player edits onto the fresh chunks
                                -- before inserting. Chunks evicted earlier
                                -- in this session and now coming back will
                                -- carry their saved edits this way.
                                edits ← readIORef (wsEditsRef worldState)
                                desigs ← readIORef (wsMineDesignationsRef worldState)
                                cdesigs ← readIORef (wsConstructDesignationsRef worldState)
                                let newChunks' = map (replayEdits edits) newChunks
                                evicted ← atomicModifyIORef' (wsTilesRef worldState) $ \td →
                                    let td' = foldl' (\acc lc → insertChunk lc acc) td newChunks'
                                        (td'', evictedCoords) = evictDistantChunksWithReport
                                                                  camChunk chunkLoadRadius td'
                                        coords = map lcCoord newChunks'
                                        -- Recompute slopes for the loaded
                                        -- chunks AND the neighbours of any
                                        -- just-evicted chunk, so a slope that
                                        -- pointed across a now-unloaded border
                                        -- (e.g. a waterfall lip) is dropped —
                                        -- the surface reflects the currently
                                        -- loaded set, not the load order.
                                        changed = coords ⧺ evictedCoords
                                        td''' = recomputeNeighborSlopes seed
                                                  (wgpWorldSize params) registry
                                                  changed td''
                                        td3b   = patchEdgeStrata coords td'''
                                        -- sealCrossChunkRivers removed: mask-based
                                        -- river seeding produces consistent edges
                                        -- (both chunks use the same segments).
                                        -- The old seal's orphan removal was stripping
                                        -- ~50% of mask-seeded river tiles.
                                        td''''' = computeSideDecos seed coords td3b
                                        -- Mid-dig slope overrides (must follow the
                                        -- slope recompute, which would erase them).
                                        -- Restore over EXACTLY the set the
                                        -- recompute touched (incl. evicted-
                                        -- neighbour and wrapped-seam-neighbour
                                        -- chunks), or border dig masks there are
                                        -- silently lost.
                                        digCoords = slopeRecomputeAffected
                                            (wgpWorldSize params) changed td''
                                        td6 = applyDigSlopesTd desigs digCoords td'''''
                                        -- Construction corner-progress
                                        -- overrides (#96): same derived-
                                        -- state contract as dig slopes.
                                        td7 = applyConstructSlopesTd cdesigs
                                                digCoords td6
                                    in (td7, evictedCoords)
                                -- Notify sim thread of loaded chunks. Use
                                -- newChunks' so the sim sees post-replay
                                -- fluid + terrain (player edits matter).
                                forM_ newChunks' $ \lc →
                                    Q.writeQueue (simQueue env) $
                                        SimChunkLoaded pageId (lcCoord lc)
                                            (lcFluidMap lc)
                                            (lcTerrainSurfaceMap lc)
                                -- Stamp any placed locations on the loaded
                                -- chunks (#89).
                                dispatchLocationStamps env params pageId newChunks'
                                -- Notify sim thread of evicted chunks
                                forM_ evicted $ \cc →
                                    Q.writeQueue (simQueue env)
                                        (SimChunkUnloaded pageId cc)
                                bumpQuadCacheGen worldState
                                writeIORef (wsZoomQuadCacheRef worldState) Nothing
                                writeIORef (wsBgQuadCacheRef worldState) Nothing

-- | Dispatch a location-stamp request to the Lua thread for any
--   just-loaded chunk the overlay (#89) places a location on. Issued on
--   EVERY load of the chunk (fresh gen, eviction reload, or after a
--   save/load) — the Lua stamper skips it once already stamped (the
--   persisted 'World.Generate.Types.wgpLocationStamped' flag, #424), so
--   repeats are cheap no-ops. Consulting the persisted overlay on every
--   chunk load is what makes a location materialize even if the world was
--   saved before it was first stamped: there is no async queue to drain,
--   only the overlay (which always rides the save) and the chunk-load
--   trigger.
dispatchLocationStamps ∷ EngineEnv → WorldGenParams → WorldPageId
                       → [LoadedChunk] → IO ()
dispatchLocationStamps env params pageId chunks =
    forM_ chunks $ \lc →
        case HM.lookup (lcCoord lc) (wgpLocationOverlay params) of
            Nothing  → return ()
            Just lid →
                let ChunkCoord cx cy = lcCoord lc
                    gx = cx * chunkSize + chunkSize `div` 2
                    gy = cy * chunkSize + chunkSize `div` 2
                in Q.writeQueue (luaQueue env)
                       (LuaStampLocation (unWorldPageId pageId) lid gx gy)

partitionChunks ∷ [ChunkCoord] → WorldTileData → ([ChunkCoord], [ChunkCoord])
partitionChunks coords tileData =
    partition (\coord → HM.member coord (wtdChunks tileData)) coords

-- | Generate a limited batch of chunks from each world's init queue.
--   Runs every world tick until all initial chunks are loaded.
drainInitQueues ∷ EngineEnv → LoggerState → IO ()
drainInitQueues env logger = do
    manager ← readIORef (worldManagerRef env)
    catalog ← readIORef (floraCatalogRef env)
    registry ← readIORef (materialRegistryRef env)
    forM_ (wmWorlds manager) $ \(pageId, worldState) → do
        remaining ← readIORef (wsInitQueueRef worldState)
        case remaining of
            [] → return ()
            _  → do
                mParams ← readIORef (wsGenParamsRef worldState)
                case mParams of
                    Nothing → return ()
                    Just params → do
                        -- Claim this tick's batch by PEEKING the front of
                        -- the queue. The chunks stay enqueued through
                        -- generation and are removed only once they land in
                        -- wsTilesRef (below). Keeping them queued means an
                        -- in-flight chunk is still visible to a concurrent
                        -- world.loadChunksInRegion (which dedups against the
                        -- queue), so a repeat call for a still-generating
                        -- region no longer re-enqueues it (#43). The Lua
                        -- thread only ever appends and this thread is the
                        -- sole consumer, so the front batch is stable and the
                        -- by-coord removal below can't clobber an append that
                        -- landed during generation (no lost update).
                        let batch = take maxChunksPerTick remaining
                        -- Skip coords already in wsTilesRef. The camera-visible
                        -- loader (updateChunkLoading) loads chunks straight into
                        -- wsTilesRef without going through this queue, so a coord
                        -- queued here by loadChunksInRegion may already be loaded
                        -- by the time we reach it. Regenerating it would overwrite
                        -- the chunk and emit a duplicate SimChunkLoaded, resetting
                        -- its sim state. wsTilesRef is the shared "already loaded"
                        -- source of truth both loaders write and dedup against;
                        -- both run on this (world) thread, so the snapshot is
                        -- stable for the rest of the tick. The whole batch
                        -- (already-loaded + freshly generated) is dropped from the
                        -- queue below.
                        td0 ← readIORef (wsTilesRef worldState)
                        let (_alreadyLoaded, toGen) = partitionChunks batch td0
                        let seed = wgpSeed params

                        let newChunks = parMap rdeepseq
                                (generateLoadedChunk registry catalog params)
                                toGen

                        -- Replay player edits onto the fresh chunks
                        -- before inserting. On load, edits restored from
                        -- the save apply here; on first-time world init
                        -- the edits map is empty and this is a no-op.
                        edits ← readIORef (wsEditsRef worldState)
                        desigs ← readIORef (wsMineDesignationsRef worldState)
                        cdesigs ← readIORef (wsConstructDesignationsRef worldState)
                        let newChunks' = map (replayEdits edits) newChunks

                        -- Insert new chunks, then recompute slopes
                        -- for the new chunks + their existing neighbors,
                        -- then seal cross-chunk river boundaries.
                        atomicModifyIORef' (wsTilesRef worldState) $ \td →
                            let td' = foldl' (\acc lc → insertChunk lc acc) td newChunks'
                                coords = map lcCoord newChunks'
                                td'' = recomputeNeighborSlopes seed
                                         (wgpWorldSize params) registry
                                         coords td'
                                td2b  = patchEdgeStrata coords td''
                                td'''' = computeSideDecos seed coords td2b
                                -- Mid-dig slope overrides (after the slope
                                -- recompute, which would erase them). Restore
                                -- over EXACTLY the recomputed set — including
                                -- wrapped-seam neighbours — not just raw
                                -- neighbours.
                                digCoords = slopeRecomputeAffected
                                    (wgpWorldSize params) coords td'
                                td5 = applyDigSlopesTd desigs digCoords td''''
                                -- Construction corner-progress overrides
                                -- (#96), same contract as dig slopes.
                                td6 = applyConstructSlopesTd cdesigs
                                        digCoords td5
                            in (td6, ())

                        -- Notify the sim thread of the loaded chunks BEFORE
                        -- dropping the batch from the init queue. The dump
                        -- path treats an empty init queue as "safe to
                        -- fast-settle" and enqueues SimFastSettleAll at that
                        -- point; simQueue is FIFO and SimFastSettleAll only
                        -- settles chunks already present in sim state, so
                        -- these SimChunkLoaded messages must be enqueued
                        -- first — otherwise the final batch can race the
                        -- settle and never be simulated. (post-replay)
                        forM_ newChunks' $ \lc →
                            Q.writeQueue (simQueue env) $
                                SimChunkLoaded pageId (lcCoord lc)
                                    (lcFluidMap lc)
                                    (lcTerrainSurfaceMap lc)
                        -- Stamp any placed locations on the loaded chunks (#89).
                        dispatchLocationStamps env params pageId newChunks'

                        -- The batch is now in wsTilesRef AND the sim has been
                        -- notified, so drop it from the init queue — by coord,
                        -- which preserves any appends that arrived during
                        -- generation. Done here, after the insert and before
                        -- the progress read below, so a chunk is always in the
                        -- queue OR loaded (never in neither) and
                        -- LoadDone/LoadPhase2 still see the right remaining
                        -- count.
                        let batchSet = HS.fromList batch
                        atomicModifyIORef' (wsInitQueueRef worldState) $ \q →
                            (filter (\c → not (c `HS.member` batchSet)) q, ())

                        -- Force this batch's chunks (plus the neighbours the
                        -- slope / edge-strata / side-deco passes rebuilt) to
                        -- NF on the world thread, so LoadDone means "fully
                        -- evaluated" rather than "queued as thunks". Without
                        -- this the first reader (render or query thread)
                        -- collapses the lazy tower in one latency spike, and
                        -- the post-process passes run serially off the render
                        -- thread. The 'parMap rdeepseq' above already sparked
                        -- the raw generation in parallel, so this mostly
                        -- collects those results; cost is bounded to the
                        -- affected chunks (batch + 4-neighbours), not the
                        -- whole map.
                        forcedTd ← readIORef (wsTilesRef worldState)
                        let batchCoords = map lcCoord newChunks'
                            affected = HS.toList $ HS.fromList $
                                batchCoords ⧺ concatMap chunkNeighbors batchCoords
                            toForce = [ lc | c ← affected
                                           , Just lc ← [lookupChunk c forcedTd] ]
                        _ ← evaluate (rnf toForce)

                        -- Invalidate all render caches so new chunks appear immediately
                        bumpQuadCacheGen worldState
                        writeIORef (wsZoomQuadCacheRef worldState) Nothing
                        writeIORef (wsBgQuadCacheRef worldState) Nothing

                        -- Progress reads the queue AFTER the batch was
                        -- claimed, so appends that landed while we were
                        -- generating are counted (and keep the phase in
                        -- LoadPhase2) instead of being overwritten.
                        rest ← readIORef (wsInitQueueRef worldState)
                        when (null rest) $
                            logDebug logger CatWorld $
                                "Initial chunk loading complete for: "
                                <> unWorldPageId pageId

                        -- Update phase 2 progress
                        let totalChunks = (2 * chunkLoadRadius + 1) * (2 * chunkLoadRadius + 1)
                        writeIORef (wsLoadPhaseRef worldState)
                            (if null rest
                             then LoadDone
                             else LoadPhase2 (length rest) totalChunks)
computeSideDecos ∷ Word64 → [ChunkCoord] → WorldTileData → WorldTileData
computeSideDecos seed newCoords wtd =
    let chunks = wtdChunks wtd
        neighborLookup coord = case HM.lookup coord chunks of
            Just lc → Just (lcTerrainSurfaceMap lc)
            Nothing → Nothing
        updatedChunks = foldl' (\acc coord →
            case HM.lookup coord acc of
                Just lc →
                    let decos = computeChunkSideDecos seed coord
                            (lcTerrainSurfaceMap lc) (lcFluidMap lc) neighborLookup
                    in HM.insert coord (lc { lcSideDeco = decos }) acc
                Nothing → acc
            ) chunks newCoords
    in wtd { wtdChunks = updatedChunks }
