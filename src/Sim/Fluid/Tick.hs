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
                                            -- Raise toward neighbors if below
                                            -- all of them (fills basins).
                                            -- Pool tiles (≥3 water neighbors)
                                            -- jump directly to the min water
                                            -- neighbor level so basins fill
                                            -- quickly to a flat surface.
                                            -- River tiles (1-2 water neighbors)
                                            -- raise slowly (+1) to avoid
                                            -- flooding banks.
                                            let waterNbrSurfs =
                                                    [ s | (s, _, True) ← nbrInfo ]
                                                nWater = length waterNbrSurfs
                                                minWater = if null waterNbrSurfs
                                                           then maxBound
                                                           else minimum waterNbrSurfs
                                                raised = if nWater ≥ 3
                                                         then minWater  -- pool: fill
                                                         else min (mySurf + 1) minNbr
                                            when (raised > mySurf
                                                 ∧ raised > terrZ) $ do
                                                MV.write mv idx
                                                    (Just (fc' { fcSurface = raised }))
                                                writeSTRef changedRef True

            -- Phase 1b: Pool equalization.
            -- After diffusion, river tiles sit at terrain+1 (correct).
            -- But basin/pool tiles also sit at terrain+1, creating a
            -- chaotic stair-step where the water should be flat.
            -- BFS connected water tiles that have ≥3 water neighbors
            -- (pool interior). Set all tiles in the pool to the MAX
            -- surface in the pool (water fills to its highest level).
            -- This fills basins to a uniform level while leaving
            -- rivers (1-2 water neighbors) at their terrain-following
            -- surface.
            poolVisited ← MV.replicate sz (0 ∷ Int)
            forM_ [0 .. sz - 1] $ \pstart → do
                pv ← MV.read poolVisited pstart
                when (pv ≡ 0) $ do
                    pcell ← MV.read mv pstart
                    case pcell of
                        Just pfc | (fcType pfc ≡ River ∨ fcType pfc ≡ Lava) → do
                            -- Count water neighbors to see if this is a pool tile
                            let plx = pstart `mod` chunkSize
                                ply = pstart `div` chunkSize
                            pWaterCount ← do
                                cnt ← newSTRef (0 ∷ Int)
                                forM_ [(plx-1,ply),(plx+1,ply),(plx,ply-1),(plx,ply+1)]
                                    $ \(pnx, pny) →
                                        when (pnx ≥ 0 ∧ pnx < chunkSize
                                             ∧ pny ≥ 0 ∧ pny < chunkSize) $ do
                                            let pnIdx = pny * chunkSize + pnx
                                            pnCell ← MV.read mv pnIdx
                                            case pnCell of
                                                Just pnfc | fcType pnfc /= Ocean
                                                          ∧ fcType pnfc /= Lake →
                                                    modifySTRef' cnt (+ 1)
                                                _ → pure ()
                                readSTRef cnt
                            if pWaterCount < 3
                                then MV.write poolVisited pstart 1
                                else do
                                    -- BFS the pool: collect connected
                                    -- water tiles with ≥3 water neighbors.
                                    poolRef ← newSTRef ([] ∷ [Int])
                                    pqRef ← newSTRef [pstart]
                                    let poolBfs = do
                                            pq ← readSTRef pqRef
                                            case pq of
                                                [] → pure ()
                                                (pidx : prest) → do
                                                    writeSTRef pqRef prest
                                                    ppv ← MV.read poolVisited pidx
                                                    if ppv ≡ 1
                                                        then poolBfs
                                                        else do
                                                            MV.write poolVisited pidx 1
                                                            pc ← MV.read mv pidx
                                                            case pc of
                                                                Just pfc' | fcType pfc' ≡ River
                                                                            ∨ fcType pfc' ≡ Lava → do
                                                                    -- Count water neighbors
                                                                    let plx' = pidx `mod` chunkSize
                                                                        ply' = pidx `div` chunkSize
                                                                    wc ← do
                                                                        c ← newSTRef (0 ∷ Int)
                                                                        forM_ [(plx'-1,ply'),(plx'+1,ply')
                                                                              ,(plx',ply'-1),(plx',ply'+1)]
                                                                            $ \(pnx, pny) →
                                                                                when (pnx ≥ 0 ∧ pnx < chunkSize
                                                                                     ∧ pny ≥ 0 ∧ pny < chunkSize) $ do
                                                                                    let pni = pny * chunkSize + pnx
                                                                                    pnc ← MV.read mv pni
                                                                                    case pnc of
                                                                                        Just pnf | fcType pnf /= Ocean
                                                                                                 ∧ fcType pnf /= Lake →
                                                                                            modifySTRef' c (+ 1)
                                                                                        _ → pure ()
                                                                        readSTRef c
                                                                    if wc < 3
                                                                        then poolBfs  -- not a pool tile
                                                                        else do
                                                                            modifySTRef' poolRef (pidx :)
                                                                            forM_ [(plx'-1,ply'),(plx'+1,ply')
                                                                                  ,(plx',ply'-1),(plx',ply'+1)]
                                                                                $ \(pnx, pny) →
                                                                                    when (pnx ≥ 0 ∧ pnx < chunkSize
                                                                                         ∧ pny ≥ 0 ∧ pny < chunkSize) $
                                                                                        modifySTRef' pqRef
                                                                                            ((pny * chunkSize + pnx) :)
                                                                            poolBfs
                                                                _ → poolBfs
                                    poolBfs
                                    pool ← readSTRef poolRef
                                    case pool of
                                        [] → pure ()
                                        _  → do
                                            -- Find the spillway: the lowest
                                            -- "exit" level adjacent to the
                                            -- pool. This is the minimum of:
                                            --  - non-pool water neighbor surfs
                                            --  - dry neighbor (terrain + 1)
                                            -- The pool fills to this level
                                            -- (water can't be higher than its
                                            -- lowest outlet).
                                            let poolSet = foldl' (\s i → HS.insert i s)
                                                                 HS.empty pool
                                            spillRef ← newSTRef maxBound
                                            forM_ pool $ \pi' → do
                                                let plx' = pi' `mod` chunkSize
                                                    ply' = pi' `div` chunkSize
                                                forM_ [(plx'-1,ply'),(plx'+1,ply')
                                                      ,(plx',ply'-1),(plx',ply'+1)]
                                                    $ \(pnx, pny) →
                                                        when (pnx ≥ 0 ∧ pnx < chunkSize
                                                             ∧ pny ≥ 0 ∧ pny < chunkSize) $ do
                                                            let pni = pny * chunkSize + pnx
                                                            unless (HS.member pni poolSet) $ do
                                                                pnc ← MV.read mv pni
                                                                case pnc of
                                                                    Just pnf →
                                                                        modifySTRef' spillRef
                                                                            (min (fcSurface pnf))
                                                                    Nothing →
                                                                        modifySTRef' spillRef
                                                                            (min (terrainV VU.! pni + 1))
                                            spillway ← readSTRef spillRef
                                            -- Pool level = spillway (water
                                            -- fills to the outlet level).
                                            let pType = fcType pfc
                                            when (spillway < maxBound) $
                                                forM_ pool $ \pi' → do
                                                    pc ← MV.read mv pi'
                                                    case pc of
                                                        Just pfc' | fcSurface pfc' ≢ spillway → do
                                                            let ptZ = terrainV VU.! pi'
                                                            if spillway ≤ ptZ
                                                                then do
                                                                    MV.write mv pi' Nothing
                                                                    writeSTRef changedRef True
                                                                else do
                                                                    MV.write mv pi'
                                                                        (Just (FluidCell pType spillway))
                                                                    writeSTRef changedRef True
                                                        _ → pure ()
                        _ → MV.write poolVisited pstart 1

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
