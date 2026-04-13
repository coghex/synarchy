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

            -- Phase 1: Water diffusion.
            --
            -- Each water tile flows toward its lowest neighbor.
            -- Dry neighbors count as having surface = terrain + 1.
            -- The target for each tile is:
            --   max(myTerrain + 1, min(all neighbor surfaces))
            --
            -- This produces:
            --   Pools:      uniform surface (all neighbors equal)
            --   Rivers:     surface = terrain + 1 (follows carved bed)
            --   Waterfalls: preserved (terrain floor prevents drainage)
            --   Water on land: drains to terrain + 1 via dry spillway
            --
            -- Multiple sub-iterations let the diffusion propagate
            -- across the chunk. Bidirectional scan ensures both
            -- upstream and downstream information flows.
            forM_ [(1∷Int)..8] $ \iter → do
                let indices = if even iter
                              then [0 .. sz - 1]
                              else [sz - 1, sz - 2 .. 0]
                forM_ indices $ \idx → do
                    cell ← MV.read mv idx
                    case cell of
                        Nothing → pure ()
                        Just fc' → do
                            let terrZ  = terrainV VU.! idx
                                mySurf = fcSurface fc'
                            when (fcType fc' /= Ocean ∧ fcType fc' /= Lake
                                 ∧ mySurf > terrZ) $ do
                                let lx = idx `mod` chunkSize
                                    ly = idx `div` chunkSize
                                -- Compute minimum neighbor surface.
                                -- Water neighbors: use their surface.
                                -- Dry neighbors: use terrain + 1
                                --   (the level water would sit at).
                                -- Out-of-chunk: use cross-chunk lookup.
                                nbrInfo ← getNeighborInfo mv terrainV
                                              allChunks coord
                                              [(lx-1,ly),(lx+1,ly)
                                              ,(lx,ly-1),(lx,ly+1)]
                                let nbrSurfs =
                                        [ s
                                        | (s, _, True) ← nbrInfo  -- water
                                        ] ⧺
                                        [ t + 1
                                        | (_, t, False) ← nbrInfo  -- dry/ocean
                                        , t > minBound
                                        ]
                                unless (null nbrSurfs) $ do
                                    let minNbr = foldl' min maxBound nbrSurfs
                                        target = max (terrZ + 1) minNbr
                                    if target < mySurf
                                        then do
                                            -- Gradual lowering: drop by 1
                                            -- per sub-iteration so water
                                            -- settles visually as the
                                            -- camera pans, not in a sudden
                                            -- snap.  With 8 sub-iterations
                                            -- per tick, water drops at most
                                            -- 8 z per tick (~100ms).
                                            let newSurf = mySurf - 1
                                            if newSurf ≤ terrZ
                                                then do
                                                    MV.write mv idx Nothing
                                                    writeSTRef changedRef True
                                                else do
                                                    MV.write mv idx
                                                        (Just (fc' { fcSurface = newSurf }))
                                                    writeSTRef changedRef True
                                        else when (mySurf < minNbr
                                                  ∧ minNbr < maxBound) $ do
                                            -- Raise: jump directly to the
                                            -- min neighbor level.  This fills
                                            -- basins in one step (water finds
                                            -- its level).  Rivers aren't
                                            -- affected because their surface
                                            -- already equals their min
                                            -- neighbor.
                                            when (minNbr > terrZ) $ do
                                                MV.write mv idx
                                                    (Just (fc' { fcSurface = minNbr }))
                                                writeSTRef changedRef True

            -- Phase 1b: Fill dry tiles below adjacent water.
            -- If a dry tile has ≥2 water neighbors and its terrain
            -- is below the minimum water neighbor surface, fill it
            -- with water at that surface. This is how basins fill:
            -- water extends into submerged dry tiles.
            forM_ [0 .. sz - 1] $ \idx → do
                cell ← MV.read mv idx
                case cell of
                    Nothing → do
                        let terrZ = terrainV VU.! idx
                        when (terrZ > minBound) $ do
                            let lx = idx `mod` chunkSize
                                ly = idx `div` chunkSize
                            -- Count water neighbors and find min surface
                            wCountRef ← newSTRef (0 ∷ Int)
                            wMinRef ← newSTRef maxBound
                            wTypeRef ← newSTRef River
                            forM_ [(lx-1,ly),(lx+1,ly),(lx,ly-1),(lx,ly+1)]
                                $ \(fnx, fny) →
                                    when (fnx ≥ 0 ∧ fnx < chunkSize
                                         ∧ fny ≥ 0 ∧ fny < chunkSize) $ do
                                        let fIdx = fny * chunkSize + fnx
                                        fc ← MV.read mv fIdx
                                        case fc of
                                            Just ffc | fcType ffc /= Ocean
                                                     ∧ fcType ffc /= Lake → do
                                                modifySTRef' wCountRef (+ 1)
                                                modifySTRef' wMinRef
                                                    (min (fcSurface ffc))
                                                writeSTRef wTypeRef (fcType ffc)
                                            _ → pure ()
                            wCount ← readSTRef wCountRef
                            wMin ← readSTRef wMinRef
                            wType ← readSTRef wTypeRef
                            when (wCount ≥ 2 ∧ terrZ < wMin) $ do
                                let fillSurf = wMin
                                when (fillSurf > terrZ) $ do
                                    MV.write mv idx
                                        (Just (FluidCell wType fillSurf))
                                    writeSTRef changedRef True
                    _ → pure ()

            -- Phase 2: Drain — isolated tiles (no water neighbors) lower.
            forM_ [0 .. sz - 1] $ \idx → do
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
