{-# LANGUAGE Strict #-}
module World.Thread.ChunkLoading
    ( updateChunkLoading
    , drainInitQueues
    , dispatchLocationStamps
    , locationStampsFor
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.List (nub, partition, sortOn)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.State (EngineEnv, luaQueue)
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Camera (Camera2D(..))
import qualified Data.HashSet as HS
import World.Types
import World.Generate (generateLoadedChunk, cameraChunkCoord)
import World.Generate.Arena (generateFlatChunk)
import World.Generate.Constants (chunkLoadRadius)
import World.Chunk.Admit
    ( admitResidentChunks, claimChunkGeneration, claimedChunkCoord
    , reconcileResidentChunks, releaseEvictedChunks )
import World.Chunk.Residency (canonicalChunkCoord)
import World.Grid (zoomFadeEnd)
import World.Slope (recomputeNeighborSlopes
                    , slopeRecomputeAffected
                    , patchEdgeStrata
                    , chunkNeighbors)
import World.SideFace.Compute (computeChunkSideDecos)
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
    camera ← readIORef (rvCameraRef (toRenderViewCapability env))
    catalog ← readIORef (wsFloraCatalogRef (toWorldSimCapability env))
    registry ← readIORef (wsMaterialRegistryRef (toWorldSimCapability env))
    let zoom = camZoom camera
    when (zoom < (zoomFadeEnd + 0.5)) $ do
        manager ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
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
                                -- The ONE canonical identity (#2001).
                                -- This used to be a bare
                                -- 'wrapChunkCoordU (wgpWorldSize params)',
                                -- which is the unguarded function applied
                                -- to a field that is a SENTINEL on arena
                                -- pages — so an arena coord past
                                -- u = ±50000 would have been silently
                                -- wrapped, and this loader disagreed with
                                -- the init queue's identity for exactly
                                -- the class of case #1723 was filed for.
                                -- 'canonicalChunkCoord' carries both the
                                -- arena-sentinel and the
                                -- non-positive-world-size guard, and every
                                -- other producer now measures through it
                                -- too, so insert-time and lookup-time
                                -- wrapping can't diverge.
                                canon = canonicalChunkCoord params
                                inBoundsV (ChunkCoord cx cy) =
                                    let v = cx + cy
                                        halfTiles = halfSize * chunkSize
                                    in abs (v * chunkSize) ≤ halfTiles
                                validCoords = map canon $ filter inBoundsV neededCoords
                            let (_toPromote, toGenerate) = partitionChunks validCoords tileData
                            let toGenerateSorted = sortOn (\(ChunkCoord cx cy) →
                                    abs (cx - ccx) + abs (cy - ccy)) toGenerate
                                wanted = take maxChunksPerTick toGenerateSorted
                            -- Claim the batch on the page's residency
                            -- owner before generating any of it (#2001).
                            -- A coord the init queue has already REQUESTED
                            -- is claimed here rather than skipped — the
                            -- camera and the queue name one physical
                            -- chunk, and refusing would move generation
                            -- out of the drain-then-camera order
                            -- 'World.Thread.worldTick' runs. A coord
                            -- something else already has in flight is
                            -- refused and drops out of the batch, so a
                            -- chunk is never generated twice.
                            claims ← claimChunkGeneration worldState pageId
                                                          params wanted
                            let batch = map claimedChunkCoord claims
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
                                -- Built against the snapshot read at the
                                -- top of this page's iteration, and
                                -- committed below. That is exact rather
                                -- than optimistic: the world thread is
                                -- the ONLY writer of wsTilesRef (this
                                -- loader, the queue drain, and the
                                -- command handlers all run on it), and
                                -- nothing between that read and the
                                -- write touches it — the reads in
                                -- between are reads, and the claim above
                                -- is on a different ref.
                                --
                                -- Splitting the transaction is what lets
                                -- the OWNER move first (#2001). The Lua
                                -- thread asks the owner whether a chunk
                                -- is already known, so an owner update
                                -- that LAGS the tile map opens a window
                                -- where a request is told a chunk is
                                -- resident whose payload has already
                                -- been evicted: that request appends
                                -- nothing, and the eviction then drops
                                -- the entry, losing the demand outright.
                                -- Leading instead is harmless in both
                                -- directions — a request that sees the
                                -- admission early is answered by a
                                -- payload that lands microseconds later,
                                -- and one that sees the eviction early
                                -- is queued and regenerates a chunk that
                                -- is about to be gone.
                                let td' = foldl' (\acc lc → insertChunk lc acc)
                                                 tileData newChunks'
                                    (td'', evicted) = evictDistantChunksWithReport
                                                        camChunk chunkLoadRadius td'
                                    coords = map lcCoord newChunks'
                                    -- Recompute slopes for the loaded
                                    -- chunks AND the neighbours of any
                                    -- just-evicted chunk, so a slope that
                                    -- pointed across a now-unloaded border
                                    -- (e.g. a waterfall lip) is dropped —
                                    -- the surface reflects the currently
                                    -- loaded set, not the load order.
                                    changed = coords ⧺ evicted
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
                                    finalTd = applyConstructSlopesTd cdesigs
                                                digCoords td6
                                -- THE admission boundary (#2001), then
                                -- the eviction, then the payloads. A
                                -- chunk admitted and immediately evicted
                                -- ends absent — matching the tile map
                                -- either way.
                                admitResidentChunks worldState claims
                                releaseEvictedChunks worldState pageId
                                                     params evicted
                                atomicModifyIORef' (wsTilesRef worldState) $ \_ →
                                    (finalTd, ())
                                -- Notify sim thread of loaded chunks. Use
                                -- newChunks' so the sim sees post-replay
                                -- fluid + terrain (player edits matter).
                                forM_ newChunks' $ \lc →
                                    Q.writeQueue (wsSimQueue (toWorldSimCapability env)) $
                                        SimChunkLoaded pageId (lcCoord lc)
                                            (lcFluidMap lc)
                                            (lcTerrainSurfaceMap lc)
                                -- Stamp any placed locations on the loaded
                                -- chunks (#89).
                                dispatchLocationStamps env params pageId newChunks'
                                -- Notify sim thread of evicted chunks
                                forM_ evicted $ \cc →
                                    Q.writeQueue (wsSimQueue (toWorldSimCapability env))
                                        (SimChunkUnloaded pageId cc)
                                -- Retire each evicted chunk's live-edit
                                -- generation (#1596). The sim drops the
                                -- chunk on the SimChunkUnloaded above and
                                -- re-seeds scsEditGen to 0 from the
                                -- SimChunkLoaded a reload sends, so the
                                -- two sides must return to the same
                                -- baseline together; leaving the entry
                                -- behind would make every writeback for
                                -- the reloaded chunk stale forever. Both
                                -- writes are the world thread's, so
                                -- nothing can interleave between them.
                                unless (null evicted) $
                                    atomicModifyIORef'
                                        (wsChunkEditGenRef worldState) $ \gens →
                                            ( foldl' (flip HM.delete) gens evicted
                                            , () )
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
    forM_ (locationStampsFor params chunks) $ \(lid, gx, gy) →
        Q.writeQueue (luaQueue env)
            (LuaStampLocation (unWorldPageId pageId) lid gx gy)

-- | The pure lookup 'dispatchLocationStamps' drives: every (location id,
--   global tile x, global tile y) triple among @chunks@ that carries a
--   placed location per the overlay. Split out (issue #763) so
--   "World.Load.Stage" can compute the SAME set during staging without
--   sending it through the live 'luaQueue' — staging must not touch any
--   live queue (requirement 6); "World.Load.Publish" dispatches the
--   deferred triples once the staged page is actually live.
locationStampsFor ∷ WorldGenParams → [LoadedChunk] → [(Text, Int, Int)]
locationStampsFor params chunks =
    [ (lid, gx, gy)
    | lc ← chunks
    , Just lid ← [HM.lookup (lcCoord lc) (wgpLocationOverlay params)]
    , let ChunkCoord cx cy = lcCoord lc
          gx = cx * chunkSize + chunkSize `div` 2
          gy = cy * chunkSize + chunkSize `div` 2
    ]

partitionChunks ∷ [ChunkCoord] → WorldTileData → ([ChunkCoord], [ChunkCoord])
partitionChunks coords tileData =
    partition (\coord → HM.member coord (wtdChunks tileData)) coords

-- | Generate a limited batch of chunks from each world's init queue.
--   Runs every world tick until all initial chunks are loaded.
drainInitQueues ∷ EngineEnv → LoggerState → IO ()
drainInitQueues env logger = do
    manager ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    catalog ← readIORef (wsFloraCatalogRef (toWorldSimCapability env))
    registry ← readIORef (wsMaterialRegistryRef (toWorldSimCapability env))
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
                            -- Canonicalise before generating or inserting.
                            -- Chunks are STORED u-wrapped — the whole render/
                            -- lookup stack assumes it (World.Render.ChunkLookup,
                            -- World.Generate.Coordinates.canonicalTileFrame) and
                            -- the camera-driven loader below already wraps every
                            -- coord it touches. This queue's three producers do
                            -- NOT: world.loadChunksInRegion takes an arbitrary
                            -- caller region, World.Load.Stage's fill is centred
                            -- on the SAVED camera chunk, and world init's is
                            -- centred on the origin. A seam-crossing region from
                            -- any of them used to be generated and inserted
                            -- under its RAW key, so the map ended up holding two
                            -- independently generated chunks for one physical
                            -- place — and every canonicalising lookup resolved
                            -- to whichever of them the camera loader had put
                            -- there, not the one this queue wrote. Wrapping HERE
                            -- fixes all three producers at once, at the single
                            -- point where the chunk is actually created.
                            --
                            -- nub because two raw coords in one batch can be
                            -- aliases of the same canonical chunk; generating it
                            -- twice would emit a duplicate SimChunkLoaded and
                            -- reset its sim state. Batches are maxChunksPerTick
                            -- long, so the quadratic scan is free. The producers
                            -- now dedup under the SAME identity before appending
                            -- (#1723), which is what keeps this queue's length an
                            -- honest count of remaining physical chunks; this
                            -- defence stays regardless, and has to canonicalise
                            -- exactly the way they do — 'canonicalChunkCoord', not a
                            -- bare 'wrapChunkCoordU', so an arena's sentinel
                            -- wgpWorldSize is identity on both sides.
                            batchCanon = nub (map (canonicalChunkCoord params) batch)
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
                        let (alreadyLoaded, notLoaded) =
                                partitionChunks batchCanon td0
                        -- Claim before generating (#2001), exactly as the
                        -- camera loader above does. These keys are
                        -- REQUESTED on the owner (every producer of this
                        -- queue registers demand), so the claim converts
                        -- that same demand to in-flight without adding an
                        -- entry; a key the camera already has in flight is
                        -- refused and left for the tile-map check on a
                        -- later tick.
                        claims ← claimChunkGeneration worldState pageId
                                                      params notLoaded
                        let toGen = map claimedChunkCoord claims
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
                        -- Owner first, tile map second, for the reason
                        -- spelled out in updateChunkLoading above: the
                        -- Lua thread reads the owner, so an owner update
                        -- that lags the tile map can lose a request
                        -- outright. Leading cannot — a request answered
                        -- "already resident" here is answered by a
                        -- payload committed on the very next line.
                        admitResidentChunks worldState claims
                        -- The half of the batch that was already loaded
                        -- (the camera loader put it there) is reconciled
                        -- to resident too, so the owner cannot keep a
                        -- stale request for a chunk the page holds after
                        -- its queue entry is dropped below.
                        reconcileResidentChunks worldState pageId params
                                                alreadyLoaded

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
                            Q.writeQueue (wsSimQueue (toWorldSimCapability env)) $
                                SimChunkLoaded pageId (lcCoord lc)
                                    (lcFluidMap lc)
                                    (lcTerrainSurfaceMap lc)
                        -- Stamp any placed locations on the loaded chunks (#89).
                        dispatchLocationStamps env params pageId newChunks'

                        -- The settled work is now in wsTilesRef AND the sim
                        -- has been notified, so drop it from the init queue —
                        -- by coord, which preserves any appends that arrived
                        -- during generation. Done here, after the insert and
                        -- before the progress read below, so a chunk is always
                        -- in the queue OR loaded (never in neither) and
                        -- LoadDone/LoadPhase2 still see the right remaining
                        -- count.
                        --
                        -- Only the SETTLED half is dropped (#2001): a coord
                        -- whose claim was refused above is still owed a
                        -- chunk, and dropping it would leave the owner
                        -- holding a request nothing is scheduled to meet.
                        -- The refusal is always transient — the only other
                        -- claimants are the camera loader and the cursor's
                        -- ore survey, both of which run to completion on this
                        -- same thread within their own tick — so keeping it
                        -- retries next tick rather than spinning.
                        let settled = HS.fromList
                                (alreadyLoaded ⧺ map lcCoord newChunks')
                            settledAlias c =
                                HS.member (canonicalChunkCoord params c) settled
                        atomicModifyIORef' (wsInitQueueRef worldState) $ \q →
                            (filter (not . settledAlias) q, ())

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

                        -- Update phase 2 progress. The total is the one
                        -- the page's producer recorded — the unique
                        -- PHYSICAL chunks of its initial box, counting
                        -- the synchronously loaded centre once (#1723).
                        -- Recomputing the raw (2r+1)^2 formula here
                        -- would put an alias-inflated total back on a
                        -- seam-crossing page and leave the bar unable to
                        -- reach it. Only a queue refilled AFTER the
                        -- initial load (a later loadChunksInRegion, so
                        -- the phase is no longer LoadPhase2) has no
                        -- recorded total to keep.
                        phaseBefore ← readIORef (wsLoadPhaseRef worldState)
                        let totalChunks = case phaseBefore of
                                LoadPhase2 _ recorded → recorded
                                _ → (2 * chunkLoadRadius + 1)
                                      * (2 * chunkLoadRadius + 1)
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
