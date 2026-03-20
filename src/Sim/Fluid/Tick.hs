{-# LANGUAGE Strict, UnicodeSyntax #-}
module Sim.Fluid.Tick
    ( simulateFluidTick
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import Control.Monad (forM_, when, unless)
import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef')
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Fluid.Internal (FluidMap)
import World.Constants (seaLevel)
import Sim.State.Types (SimState(..), SimChunkState(..))

simulateFluidTick ∷ SimState → SimState
simulateFluidTick ss
    | ssPaused ss = ss
    | HM.null (ssChunks ss) = ss
    | otherwise = simulateOnce ss

simulateOnce ∷ SimState → SimState
simulateOnce ss =
    let chunks = ssChunks ss
        results = HM.mapWithKey (simulateChunk chunks) chunks
        dirty = HM.foldlWithKey' (\acc cc (_, changed) →
            if changed then HS.insert cc acc else acc
            ) (ssDirtyChunks ss) results
        newChunks = HM.map fst results
    in ss { ssChunks = newChunks
          , ssDirtyChunks = dirty
          }

-- | Simulate fluid for a single chunk.
--
--   Simplified algorithm (generation now handles most placement):
--     1. Smooth (8 sub-iterations): Each water tile lowers toward
--        min(water neighbor surfaces) + 1. Raises when below all
--        neighbors. Tiles that drop to terrain get removed.
--     2. Drain: Isolated tiles (no water neighbors) lower by 1 per tick.
simulateChunk ∷ HM.HashMap ChunkCoord SimChunkState
              → ChunkCoord → SimChunkState → (SimChunkState, Bool)
simulateChunk allChunks coord scs
    | scsActive scs = (scs, False)  -- active chunks use volume sim
    | otherwise = simulatePassiveChunk allChunks coord scs

simulatePassiveChunk ∷ HM.HashMap ChunkCoord SimChunkState
                     → ChunkCoord → SimChunkState → (SimChunkState, Bool)
simulatePassiveChunk allChunks coord scs =
    let terrainV = scsTerrain scs
        fluidV   = scsFluid scs
        sz       = chunkSize * chunkSize
        (newFluid, changed) = runST $ do
            mv ← V.thaw fluidV
            changedRef ← newSTRef False

            -- Phase 1: Smooth — hybrid bidirectional propagation, 8 sub-iters.
            -- Lower: fast (jump) when diff > 3, slow (by 1) when diff ≤ 3.
            -- Raise: by 1 per sub-iter toward min(water neighbors) when below all.
            forM_ [(1∷Int)..8] $ \iter → do
                let indices = if even iter
                              then [0 .. sz - 1]
                              else [sz - 1, sz - 2 .. 0]
                forM_ indices $ \idx → do
                    cell ← MV.read mv idx
                    case cell of
                        Nothing → pure ()
                        Just fc → do
                            let terrZ  = terrainV VU.! idx
                                mySurf = fcSurface fc
                            when (fcType fc /= Ocean ∧ fcType fc /= Lake
                                 ∧ mySurf > terrZ) $ do
                                let lx = idx `mod` chunkSize
                                    ly = idx `div` chunkSize
                                    nbrs = [(lx-1,ly),(lx+1,ly)
                                           ,(lx,ly-1),(lx,ly+1)]
                                nbrInfo ← getNeighborInfo mv terrainV
                                              allChunks coord nbrs
                                let waterSurfs = [ s | (s, _, True) ← nbrInfo ]
                                unless (null waterSurfs) $ do
                                    let minW = minimum waterSurfs
                                        target = minW + 1
                                        diff   = mySurf - target
                                    if diff > 0
                                        then do
                                            let newSurf = if diff > 3
                                                          then target
                                                          else mySurf - 1
                                            if newSurf ≤ terrZ
                                                then MV.write mv idx Nothing
                                                else MV.write mv idx
                                                        (Just fc { fcSurface = newSurf })
                                            writeSTRef changedRef True
                                        else when (mySurf < minW) $ do
                                            let raised = mySurf + 1
                                            MV.write mv idx
                                                (Just fc { fcSurface = raised })
                                            writeSTRef changedRef True

            -- Phase 2: Drain — isolated tiles (no water neighbors) lower.
            forM_ [0 .. sz - 1] $ \idx → do
                cell ← MV.read mv idx
                case cell of
                    Nothing → pure ()
                    Just fc → do
                        let terrZ  = terrainV VU.! idx
                            mySurf = fcSurface fc
                        when (fcType fc /= Ocean ∧ mySurf > terrZ) $ do
                            let lx = idx `mod` chunkSize
                                ly = idx `div` chunkSize
                                nbrs = [(lx-1,ly),(lx+1,ly)
                                       ,(lx,ly-1),(lx,ly+1)]
                            nbrInfo ← getNeighborInfo mv terrainV
                                          allChunks coord nbrs
                            let hasWaterNbr = any (\(_, _, isW) → isW) nbrInfo
                            when (not hasWaterNbr) $ do
                                let newSurf = mySurf - 1
                                if newSurf ≤ terrZ
                                    then MV.write mv idx Nothing
                                    else MV.write mv idx
                                            (Just fc { fcSurface = newSurf })
                                writeSTRef changedRef True

            result ← V.freeze mv
            ch ← readSTRef changedRef
            pure (result, ch)
    in (scs { scsFluid = newFluid }, changed)

-- | Get info about cardinal neighbors.
--   Returns [(surface, terrainZ, isWater)]
getNeighborInfo ∷ MV.MVector s (Maybe FluidCell)
                → VU.Vector Int
                → HM.HashMap ChunkCoord SimChunkState
                → ChunkCoord
                → [(Int, Int)]
                → ST s [(Int, Int, Bool)]
getNeighborInfo mv terrainV allChunks myCoord nbrs = do
    results ← newSTRef []
    forM_ nbrs $ \(nlx, nly) → do
        let inBounds = nlx ≥ 0 ∧ nlx < chunkSize ∧ nly ≥ 0 ∧ nly < chunkSize
        if inBounds
            then do
                let nIdx   = nly * chunkSize + nlx
                    nTerrZ = terrainV VU.! nIdx
                nCell ← MV.read mv nIdx
                case nCell of
                    Just nfc | fcType nfc /= Ocean →
                        modifySTRef' results ((fcSurface nfc, nTerrZ, True) :)
                    Just nfc →
                        modifySTRef' results ((fcSurface nfc, nTerrZ, False) :)
                    Nothing →
                        modifySTRef' results ((nTerrZ, nTerrZ, False) :)
            else do
                let (nCoord, cnlx, cnly) = resolveNeighborCoord myCoord nlx nly
                case HM.lookup nCoord allChunks of
                    Nothing → pure ()
                    Just ncs → do
                        let nIdx   = cnly * chunkSize + cnlx
                            nTerrZ = scsTerrain ncs VU.! nIdx
                            nCell  = scsFluid ncs V.! nIdx
                        case nCell of
                            Just nfc | fcType nfc /= Ocean →
                                modifySTRef' results
                                    ((fcSurface nfc, nTerrZ, True) :)
                            Just nfc →
                                modifySTRef' results
                                    ((fcSurface nfc, nTerrZ, False) :)
                            Nothing →
                                modifySTRef' results
                                    ((nTerrZ, nTerrZ, False) :)
    readSTRef results

resolveNeighborCoord ∷ ChunkCoord → Int → Int → (ChunkCoord, Int, Int)
resolveNeighborCoord (ChunkCoord cx cy) lx ly =
    let (dcx, nlx) | lx < 0         = (-1, lx + chunkSize)
                   | lx ≥ chunkSize = (1,  lx - chunkSize)
                   | otherwise       = (0,  lx)
        (dcy, nly) | ly < 0         = (-1, ly + chunkSize)
                   | ly ≥ chunkSize = (1,  ly - chunkSize)
                   | otherwise       = (0,  ly)
    in (ChunkCoord (cx + dcx) (cy + dcy), nlx, nly)
