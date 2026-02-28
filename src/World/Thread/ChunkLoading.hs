{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread.ChunkLoading
    ( updateChunkLoading
    , drainInitQueues
    , maxChunksPerTick
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.List (partition, sortOn)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.DeepSeq (NFData)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import Engine.Graphics.Camera (Camera2D(..))
import World.Types
import World.Generate (generateChunk, cameraChunkCoord)
import World.Generate.Constants (chunkLoadRadius)
import World.Grid (zoomFadeEnd)
import World.Slope (recomputeNeighborSlopes)
import World.Thread.Helpers (unWorldPageId)

-- | Maximum chunks to generate per world loop iteration.
maxChunksPerTick ∷ Int
maxChunksPerTick = 4

updateChunkLoading ∷ EngineEnv → LoggerState → IO ()
updateChunkLoading env logger = do
    camera ← readIORef (cameraRef env)
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
                            when (not $ null batch) $ do
                                let seed = wgpSeed params
                                let !newChunks = parMap rdeepseq (\coord →
                                        let (chunkTiles, surfMap, tMap, fluidMap) = generateChunk params coord
                                        in LoadedChunk
                                            { lcCoord      = coord
                                            , lcTiles      = chunkTiles
                                            , lcSurfaceMap = surfMap
                                            , lcTerrainSurfaceMap = tMap
                                            , lcFluidMap   = fluidMap
                                            , lcFlora      = emptyFloraChunkData
                                            , lcModified   = False
                                            }) batch
                                atomicModifyIORef' (wsTilesRef worldState) $ \td →
                                    let td' = foldl' (\acc lc → insertChunk lc acc) td newChunks
                                        td'' = evictDistantChunks camChunk chunkLoadRadius td'
                                        td''' = recomputeNeighborSlopes seed
                                                  (map lcCoord newChunks) td''
                                    in (td''', ())
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
                                let (chunkTiles, surfMap, tMap, fluidMap) = generateChunk params coord
                                in LoadedChunk
                                    { lcCoord      = coord
                                    , lcTiles      = chunkTiles
                                    , lcSurfaceMap = surfMap
                                    , lcTerrainSurfaceMap = tMap
                                    , lcFluidMap   = fluidMap
                                    , lcFlora      = emptyFloraChunkData
                                    , lcModified   = False
                                    }) batch

                        -- Insert new chunks, then recompute slopes
                        -- for the new chunks + their existing neighbors
                        atomicModifyIORef' (wsTilesRef worldState) $ \td →
                            let td' = foldl' (\acc lc → insertChunk lc acc) td newChunks
                                td'' = recomputeNeighborSlopes seed
                                         (map lcCoord newChunks) td'
                            in (td'', ())

                        -- Invalidate all render caches so new chunks appear immediately
                        writeIORef (wsQuadCacheRef worldState) Nothing
                        writeIORef (wsZoomQuadCacheRef worldState) Nothing
                        writeIORef (wsBgQuadCacheRef worldState) Nothing

                        writeIORef (wsInitQueueRef worldState) rest

                        -- Update phase 2 progress
                        let totalChunks = (2 * chunkLoadRadius + 1) * (2 * chunkLoadRadius + 1)
                        writeIORef (wsLoadPhaseRef worldState)
                            (if null rest
                             then LoadDone
                             else LoadPhase2 (length rest) totalChunks)

                        when (null rest) $
                            logDebug logger CatWorld $
                                "Initial chunk loading complete for: "
                                <> unWorldPageId pageId
