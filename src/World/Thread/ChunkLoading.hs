{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread.ChunkLoading
    ( updateChunkLoading
    , drainInitQueues
    , maxChunksPerTick
    , fillOrphanedSubseaTiles
    , drainOceanLakes
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.List (partition, sortOn)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.DeepSeq (NFData)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Camera (Camera2D(..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.HashSet as HS
import World.Types
import World.Constants (seaLevel)
import World.Chunk.Types (chunkSize)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Generate (generateChunk, cameraChunkCoord)
import World.Generate.Arena (generateFlatChunk)
import World.Generate.Constants (chunkLoadRadius)
import World.Geology.Timeline.Types (GeoTimeline(..), emptyTimeline)
import World.Grid (zoomFadeEnd)
import World.Slope (recomputeNeighborSlopes, patchEdgeStrata)
import World.Fluids (sealCrossChunkRivers)
import World.SideFace.Compute (computeChunkSideDecos)
import World.Thread.Helpers (unWorldPageId)
import Sim.Command.Types (SimCommand(..))

-- | Maximum chunks to generate per world loop iteration.
--   parMap uses all available cores, so larger batches
--   utilize parallelism better during initial world generation.
maxChunksPerTick ∷ Int
maxChunksPerTick = 8

updateChunkLoading ∷ EngineEnv → LoggerState → IO ()
updateChunkLoading env logger = do
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
                                wrapChunkU (ChunkCoord cx cy) =
                                    let w = halfSize * 2
                                        u = cx - cy
                                        v = cx + cy
                                        halfW = w `div` 2
                                        wrappedU = ((u + halfW) `mod` w + w) `mod` w - halfW
                                        cx' = (wrappedU + v) `div` 2
                                        cy' = (v - wrappedU) `div` 2
                                    in ChunkCoord cx' cy'
                                inBoundsV (ChunkCoord cx cy) =
                                    let v = cx + cy
                                        halfTiles = halfSize * chunkSize
                                    in abs (v * chunkSize) ≤ halfTiles
                                validCoords = map wrapChunkU $ filter inBoundsV neededCoords
                            let (_toPromote, toGenerate) = partitionChunks validCoords tileData
                            let toGenerateSorted = sortOn (\(ChunkCoord cx cy) →
                                    abs (cx - ccx) + abs (cy - ccy)) toGenerate
                                batch = take maxChunksPerTick toGenerateSorted
                            let isArena = wgpGeoTimeline params
                                            == emptyTimeline
                                            && wgpSeed params == 0
                            when (not $ null batch) $ do
                                let seed = wgpSeed params
                                let !newChunks = if isArena
                                        then map generateFlatChunk batch
                                        else parMap rdeepseq (\coord →
                                            let (chunkTiles, surfMap, tMap, fluidMap, iceMap, flora) = generateChunk registry catalog params coord
                                            in LoadedChunk
                                                { lcCoord      = coord
                                                , lcTiles      = chunkTiles
                                                , lcSurfaceMap = surfMap
                                                , lcTerrainSurfaceMap = tMap
                                                , lcFluidMap   = fluidMap
                                                , lcIceMap     = iceMap
                                                , lcFlora      = flora
                                                , lcSideDeco   = VU.replicate (chunkSize * chunkSize) 0
                                                , lcModified   = False
                                                }) batch
                                evicted ← atomicModifyIORef' (wsTilesRef worldState) $ \td →
                                    let td' = foldl' (\acc lc → insertChunk lc acc) td newChunks
                                        (td'', evictedCoords) = evictDistantChunksWithReport
                                                                  camChunk chunkLoadRadius td'
                                        coords = map lcCoord newChunks
                                        td''' = recomputeNeighborSlopes seed
                                                  registry coords td''
                                        td3b   = patchEdgeStrata coords td'''
                                        td'''' = sealCrossChunkRivers coords td3b
                                        td''''' = stripCrossChunkLakeCliffs coords td''''
                                        td'''''' = computeSideDecos seed coords td'''''
                                    in (td'''''', evictedCoords)
                                -- Notify sim thread of loaded chunks
                                forM_ newChunks $ \lc →
                                    Q.writeQueue (simQueue env) $
                                        SimChunkLoaded (lcCoord lc)
                                            (lcFluidMap lc)
                                            (lcTerrainSurfaceMap lc)
                                -- Notify sim thread of evicted chunks
                                forM_ evicted $ \cc →
                                    Q.writeQueue (simQueue env) (SimChunkUnloaded cc)
                                writeIORef (wsQuadCacheRef worldState) Nothing
                                writeIORef (wsZoomQuadCacheRef worldState) Nothing
                                writeIORef (wsBgQuadCacheRef worldState) Nothing

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
                        let batch = take maxChunksPerTick remaining
                            rest  = drop maxChunksPerTick remaining
                            seed  = wgpSeed params

                        let newChunks = parMap rdeepseq (\coord →
                                let (chunkTiles, surfMap, tMap, fluidMap, iceMap, flora) = generateChunk registry catalog params coord
                                in LoadedChunk
                                    { lcCoord      = coord
                                    , lcTiles      = chunkTiles
                                    , lcSurfaceMap = surfMap
                                    , lcTerrainSurfaceMap = tMap
                                    , lcFluidMap   = fluidMap
                                    , lcIceMap     = iceMap
                                    , lcFlora      = flora
                                    , lcSideDeco   = VU.replicate (chunkSize * chunkSize) 0
                                    , lcModified   = False
                                    }) batch

                        -- Insert new chunks, then recompute slopes
                        -- for the new chunks + their existing neighbors,
                        -- then seal cross-chunk river boundaries.
                        atomicModifyIORef' (wsTilesRef worldState) $ \td →
                            let td' = foldl' (\acc lc → insertChunk lc acc) td newChunks
                                coords = map lcCoord newChunks
                                td'' = recomputeNeighborSlopes seed registry
                                         coords td'
                                td2b  = patchEdgeStrata coords td''
                                td''' = sealCrossChunkRivers coords td2b
                                td'''' = stripCrossChunkLakeCliffs coords td'''
                                td''''' = computeSideDecos seed coords td''''
                            in (td''''', ())

                        -- Notify sim thread of loaded chunks
                        forM_ newChunks $ \lc →
                            Q.writeQueue (simQueue env) $
                                SimChunkLoaded (lcCoord lc)
                                    (lcFluidMap lc)
                                    (lcTerrainSurfaceMap lc)

                        -- Invalidate all render caches so new chunks appear immediately
                        writeIORef (wsQuadCacheRef worldState) Nothing
                        writeIORef (wsZoomQuadCacheRef worldState) Nothing
                        writeIORef (wsBgQuadCacheRef worldState) Nothing

                        -- When all initial chunks are loaded, run a
                        -- final cross-chunk river seal across the
                        -- entire tile set. The per-batch seal may
                        -- miss boundaries between different batches.
                        -- Then defensively fill any tile with terrainZ
                        -- ≤ seaLevel that ended up dry — these would
                        -- otherwise show as 1-tile islands of dry land
                        -- below sea level.
                        --
                        -- IMPORTANT: this must run BEFORE writing to
                        -- wsInitQueueRef, because the dump path's
                        -- waitForChunks polls that ref to know when
                        -- loading is done. Updating the queue first
                        -- creates a race where the dump reads stale
                        -- tile data.
                        when (null rest) $ do
                            atomicModifyIORef' (wsTilesRef worldState) $ \td →
                                let allCoords = HM.keys (wtdChunks td)
                                    td'  = sealCrossChunkRivers allCoords td
                                    td'' = sealCrossChunkRivers allCoords td'
                                    td''' = drainOceanLakes
                                          $ fillOrphanedSubseaTiles td''
                                in (td''', ())
                            logDebug logger CatWorld $
                                "Initial chunk loading complete for: "
                                <> unWorldPageId pageId

                        writeIORef (wsInitQueueRef worldState) rest

                        -- Update phase 2 progress
                        let totalChunks = (2 * chunkLoadRadius + 1) * (2 * chunkLoadRadius + 1)
                        writeIORef (wsLoadPhaseRef worldState)
                            (if null rest
                             then LoadDone
                             else LoadPhase2 (length rest) totalChunks)

-- | Defensive cleanup: fill any tile with terrainZ ≤ seaLevel that
--   ended up without fluid. These tiles slip through the multi-pass
--   fluid pipeline (the exact mechanism isn't pinned down — the seal
--   and strip passes interact in ways that occasionally drop river
--   tiles below sea level). Without this, they appear as 1-tile dry
--   "islands" with surfaceZ=-1 inside the ocean.
--
--   Strictly additive: only fills tiles that were Nothing. Never
--   removes existing fluid.
fillOrphanedSubseaTiles ∷ WorldTileData → WorldTileData
fillOrphanedSubseaTiles wtd =
    wtd { wtdChunks = HM.map fillChunk (wtdChunks wtd) }
  where
    area = chunkSize * chunkSize
    fillChunk lc =
        let fluidMap = lcFluidMap lc
            terrMap  = lcTerrainSurfaceMap lc
            updates =
                [ (idx, Just (FluidCell Ocean seaLevel))
                | idx ← [0 .. area - 1]
                , isNothing (fluidMap V.! idx)
                , let terrZ = terrMap VU.! idx
                , terrZ ≤ seaLevel
                , terrZ > minBound
                ]
        in if null updates
           then lc
           else let newFluid = fluidMap V.// updates
                    newSurf  = VU.imap (\idx oldSurf →
                        case newFluid V.! idx of
                            Just fc → max (terrMap VU.! idx) (fcSurface fc)
                            Nothing → oldSurf
                      ) (lcSurfaceMap lc)
                in lc { lcFluidMap = newFluid, lcSurfaceMap = newSurf }

-- | Check if any cardinal neighbor (including cross-chunk) is ocean.
hasAdjacentOceanTile ∷ HM.HashMap ChunkCoord LoadedChunk → ChunkCoord
                     → Int → Int → Bool
hasAdjacentOceanTile chunks (ChunkCoord cx cy) lx ly =
    any checkDir [(lx-1,ly),(lx+1,ly),(lx,ly-1),(lx,ly+1)]
  where
    checkDir (nx, ny)
      | nx ≥ 0 ∧ nx < chunkSize ∧ ny ≥ 0 ∧ ny < chunkSize =
          case HM.lookup (ChunkCoord cx cy) chunks of
              Just lc → isOceanAt lc nx ny
              Nothing → False
      | otherwise =
          let cx' = cx + (if nx < 0 then -1 else if nx ≥ chunkSize then 1 else 0)
              cy' = cy + (if ny < 0 then -1 else if ny ≥ chunkSize then 1 else 0)
              nlx = ((nx `mod` chunkSize) + chunkSize) `mod` chunkSize
              nly = ((ny `mod` chunkSize) + chunkSize) `mod` chunkSize
          in case HM.lookup (ChunkCoord cx' cy') chunks of
              Just lc → isOceanAt lc nlx nly
              Nothing → False
    isOceanAt lc nx ny =
        case lcFluidMap lc V.! (ny * chunkSize + nx) of
            Just fc → fcType fc ≡ Ocean
            Nothing → False

-- | Convert lake tiles to ocean when they're below sea level and
--   adjacent to ocean (or connected via other such lake tiles).
--   The lake fill's 32-sample spillway check can miss narrow ocean
--   openings, leaving lake water sitting on below-sea-level terrain
--   directly next to the ocean. Iterates up to 8 passes so chains
--   of connected lake tiles all convert together.
drainOceanLakes ∷ WorldTileData → WorldTileData
drainOceanLakes = iterDrain (8 ∷ Int)
  where
    iterDrain 0 wtd = wtd
    iterDrain n wtd = iterDrain (n - 1) (drainPass wtd)

    drainPass wtd =
        let chunks = wtdChunks wtd
            updateChunk coord lc =
                let fm   = lcFluidMap lc
                    terr = lcTerrainSurfaceMap lc
                    area = chunkSize * chunkSize
                    updates =
                        [ (idx, Just (FluidCell Ocean seaLevel))
                        | idx ← [0 .. area - 1]
                        , Just fc ← [fm V.! idx]
                        , fcType fc ≡ Lake
                        , let terrZ = terr VU.! idx
                        , terrZ ≤ seaLevel
                        , let lx = idx `mod` chunkSize
                              ly = idx `div` chunkSize
                        , hasAdjacentOceanTile chunks coord lx ly
                        ]
                in if null updates
                   then lc
                   else let newFluid = fm V.// updates
                            newSurf  = VU.imap (\idx oldSurf →
                                case newFluid V.! idx of
                                    Just fc → max (terr VU.! idx) (fcSurface fc)
                                    Nothing → oldSurf
                              ) (lcSurfaceMap lc)
                        in lc { lcFluidMap = newFluid, lcSurfaceMap = newSurf }
        in wtd { wtdChunks = HM.mapWithKey updateChunk chunks }

-- | Compute side-face decorations for newly loaded chunks.
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

-- | Strip lake tiles that are adjacent to river tiles with a much lower
--   surface, across chunk boundaries. Iterates to peel back multiple layers.
stripCrossChunkLakeCliffs ∷ [ChunkCoord] → WorldTileData → WorldTileData
stripCrossChunkLakeCliffs newCoords wtd =
    let -- Affected chunks: new chunks + their neighbors
        affected = HS.toList $ HS.fromList $
            newCoords <>
            [ ChunkCoord (cx + dx) (cy + dy)
            | ChunkCoord cx cy ← newCoords
            , dx ← [-1, 0, 1]
            , dy ← [-1, 0, 1]
            , dx ≠ 0 ∨ dy ≠ 0
            , HM.member (ChunkCoord (cx+dx) (cy+dy)) (wtdChunks wtd)
            ]
    in go (10 ∷ Int) wtd affected
  where
    go 0 w _ = w
    go n w coords =
        let (w', anyChanged) = foldl' (\(acc, changed) cc →
                case HM.lookup cc (wtdChunks acc) of
                    Nothing → (acc, changed)
                    Just lc →
                        let (newFluid, didChange) = stripChunkLakeEdges acc cc lc
                        in if didChange
                           then let lc' = lc { lcFluidMap = newFluid
                                             , lcSurfaceMap = recomputeSurfMap lc newFluid }
                                in (acc { wtdChunks = HM.insert cc lc' (wtdChunks acc) }, True)
                           else (acc, changed)
                ) (w, False) coords
        in if anyChanged then go (n - 1) w' coords else w'

    stripChunkLakeEdges ∷ WorldTileData → ChunkCoord → LoadedChunk
                        → (V.Vector (Maybe FluidCell), Bool)
    stripChunkLakeEdges wtd' coord lc =
        let fluidMap = lcFluidMap lc
            ChunkCoord cx cy = coord
            changed = V.ifoldl' (\acc idx mfc →
                case mfc of
                    Just fc | fcType fc ≡ Lake →
                        let lx = idx `mod` chunkSize
                            ly = idx `div` chunkSize
                            lakeSurf = fcSurface fc
                            -- Check cardinal neighbors including cross-chunk
                            hasCliff = any (\(nx, ny) →
                                let (cc', nlx, nly) = resolveNeighbor cx cy lx ly nx ny
                                in case HM.lookup cc' (wtdChunks wtd') of
                                    Nothing → False
                                    Just nlc →
                                        let nIdx = nly * chunkSize + nlx
                                            nTerrZ = lcTerrainSurfaceMap nlc VU.! nIdx
                                        in case lcFluidMap nlc V.! nIdx of
                                            -- Adjacent to river with much lower surface
                                            Just nfc | fcType nfc ≡ River →
                                                lakeSurf - fcSurface nfc > 2
                                            -- Adjacent to dry tile with terrain well
                                            -- below lake surface (stripped or river valley)
                                            Nothing →
                                                lakeSurf - nTerrZ > 3
                                            _ → False
                                ) [(-1,0),(1,0),(0,-1),(0,1)]
                        in if hasCliff then idx : acc else acc
                    _ → acc
                ) [] fluidMap
        in if null changed
           then (fluidMap, False)
           else (V.imap (\idx v → if idx `elem` changed then Nothing else v) fluidMap, True)

    resolveNeighbor ∷ Int → Int → Int → Int → Int → Int → (ChunkCoord, Int, Int)
    resolveNeighbor cx cy lx ly dx dy =
        let nlx = lx + dx
            nly = ly + dy
            (cx', nlx') = if nlx < 0 then (cx - 1, nlx + chunkSize)
                          else if nlx ≥ chunkSize then (cx + 1, nlx - chunkSize)
                          else (cx, nlx)
            (cy', nly') = if nly < 0 then (cy - 1, nly + chunkSize)
                          else if nly ≥ chunkSize then (cy + 1, nly - chunkSize)
                          else (cy, nly)
        in (ChunkCoord cx' cy', nlx', nly')

    recomputeSurfMap ∷ LoadedChunk → V.Vector (Maybe FluidCell) → VU.Vector Int
    recomputeSurfMap lc newFluid =
        VU.imap (\idx terrZ →
            case newFluid V.! idx of
                Just fc → max terrZ (fcSurface fc)
                Nothing → terrZ
            ) (lcTerrainSurfaceMap lc)
