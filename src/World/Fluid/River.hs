{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Fluid.River
    ( computeChunkRivers
    , fixupSegmentContinuity
    , hasAnyRiverQuick
    , sealCrossChunkRivers
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.STRef (newSTRef, readSTRef, modifySTRef')
import World.Base
import World.Types
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Fluid.Internal
import World.Constants (seaLevel)
import World.Hydrology.Types (RiverParams(..), RiverSegment(..))

-----------------------------------------------------------
-- Chunk-Level River Computation
-----------------------------------------------------------

computeChunkRivers ∷ [RiverParams] → Int → ChunkCoord
                   → VU.Vector Int
                   → FluidMap
computeChunkRivers rivers worldSize coord surfaceMap =
    let ChunkCoord cx cy = coord
        chunkMinGX = cx * chunkSize
        chunkMinGY = cy * chunkSize
        nearbyRivers = filter (riverNearChunk worldSize chunkMinGX chunkMinGY) rivers
    in withFluidMap $ \mv → do
        forM_ nearbyRivers $ \river →
            fillRiverDirect mv river worldSize chunkMinGX chunkMinGY surfaceMap
        -- Pass 2: extend river water into banks (2 iterations to cover
        -- gaps between the fill zone and the carved valley wall).
        extendRiverBanks mv surfaceMap
        extendRiverBanks mv surfaceMap
        -- Pass 3: fill single-tile holes in the river.
        fillRiverHoles mv surfaceMap
        -- Pass 4: clamp water surfaces to terrain + depth cap.
        clampRiverDepth mv surfaceMap
        -- Pass 5: smooth river surfaces to fix overlapping rivers.
        smoothRiverSurface mv surfaceMap
        -- Pass 6: remove tiles with unresolvable cliffs.
        removeUnresolvableCliffs mv surfaceMap
        -- Pass 6b: fill holes created by cliff removal.
        fillRiverHoles mv surfaceMap
        -- Pass 7: seal exposed river boundaries.
        sealRiverBoundaries mv surfaceMap
        -- Pass 8: re-smooth after sealing.
        smoothRiverSurface mv surfaceMap
        -- Pass 9: remove cliff artifacts created by sealing.
        removeUnresolvableCliffs mv surfaceMap
        -- Pass 10: extend banks and fill holes after seal/smooth/cliff
        -- passes may have created new gaps by removing tiles or adding
        -- new water tiles that weren't present earlier.
        extendRiverBanks mv surfaceMap
        fillRiverHoles mv surfaceMap
        -- Pass 11: final smooth and clamp to clean up any depth
        -- violations introduced by the bank/hole fill.
        clampRiverDepth mv surfaceMap
        smoothRiverSurface mv surfaceMap
        -- Pass 12: drain connectivity check.
        -- Note: only remove truly isolated interior tiles.
        -- Edge tiles and tiles connected to edges are preserved.
        removeDisconnectedWater mv surfaceMap
        -- Pass 13: fill any single-tile holes created by the
        -- disconnected-water removal. Iterate because each fill pass
        -- may expose new tiles that now have 3+ river neighbors.
        fillRiverHoles mv surfaceMap
        fillRiverHoles mv surfaceMap
        fillRiverHoles mv surfaceMap
        -- Pass 14: final smooth after hole fills.
        smoothRiverSurface mv surfaceMap
        -- Pass 15: raise low tiles to match terrain-limited neighbors.
        -- When a river tile is at terrain+1 and its neighbor is 2+
        -- levels lower, raise the neighbor toward this tile to close
        -- the gap. Iterates to propagate raises along the river.
        raiseToMatchTerrain mv surfaceMap
        pure ()

-- | Extend river water one tile into banks. For each empty tile
--   adjacent to a river tile, place a river fluid cell at the
--   neighbor's water surface — but only if the tile's terrain is
--   at or below the water surface. This prevents water from
--   appearing on hillsides above the river.
extendRiverBanks ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int → ST s ()
extendRiverBanks mv surfaceMap =
    forM_ [0 .. chunkSize * chunkSize - 1] $ \idx → do
        val ← MV.read mv idx
        when (isNothing val) $ do
            let lx = idx `mod` chunkSize
                ly = idx `div` chunkSize
                surfZ = surfaceMap VU.! idx
            adj ← adjacentRiverSurfaceWithTerrain mv surfaceMap lx ly
            case adj of
                Nothing → pure ()
                Just (surf, nTerrZ) →
                    -- Only extend to tiles at similar terrain elevation
                    -- as the river neighbor. Use the neighbor's water
                    -- surface directly — depth cap (pass 4) handles
                    -- any excessive depth. This ensures bank tiles
                    -- match neighbor surfaces for smooth transitions.
                    -- Only extend if the water surface is above sea level,
                    -- terrain is below the water, and terrain matches the
                    -- river neighbor (not a valley wall).
                    -- Only extend to tiles at or below the river
                    -- neighbor's terrain. Tiles above the neighbor
                    -- are valley walls, not river banks.
                    when (surfZ ≤ nTerrZ + 1
                         ∧ surfZ < surf ∧ surf - surfZ ≤ 5
                         ∧ surf > seaLevel) $
                        MV.write mv idx (Just (FluidCell River surf))

adjacentRiverSurface ∷ MV.MVector s (Maybe FluidCell) → Int → Int
                     → ST s (Maybe Int)
adjacentRiverSurface mv lx ly = do
    let check x y
          | x < 0 ∨ x ≥ chunkSize ∨ y < 0 ∨ y ≥ chunkSize = pure Nothing
          | otherwise = do
              v ← MV.read mv (y * chunkSize + x)
              pure $ case v of
                  Just fc | fcType fc ≡ River → Just (fcSurface fc)
                  _ → Nothing
    vN ← check lx (ly - 1)
    vS ← check lx (ly + 1)
    vE ← check (lx + 1) ly
    vW ← check (lx - 1) ly
    let surfaces = catMaybes [vN, vS, vE, vW]
    pure $ case surfaces of
        []     → Nothing
        (s:ss) → Just (foldl' min s ss)

-- | Get the maximum adjacent river surface along with the terrain
--   of that neighbor. Used by bank extension to fill dry gaps between
--   river segments at different levels. Picking the maximum ensures
--   tiles between a high-water segment and a low-water segment still
--   get water when the high segment's surface covers the tile.
--   The terrain of the neighbor with the highest water is returned
--   so the caller can verify the fill candidate isn't a valley wall.
adjacentRiverSurfaceWithTerrain ∷ MV.MVector s (Maybe FluidCell)
                                → VU.Vector Int → Int → Int
                                → ST s (Maybe (Int, Int))
adjacentRiverSurfaceWithTerrain mv surfaceMap lx ly = do
    let check x y
          | x < 0 ∨ x ≥ chunkSize ∨ y < 0 ∨ y ≥ chunkSize = pure Nothing
          | otherwise = do
              v ← MV.read mv (y * chunkSize + x)
              pure $ case v of
                  Just fc | fcType fc ≡ River →
                      let nIdx = y * chunkSize + x
                          terrZ = surfaceMap VU.! nIdx
                      in Just (fcSurface fc, terrZ)
                  _ → Nothing
    vN ← check lx (ly - 1)
    vS ← check lx (ly + 1)
    vE ← check (lx + 1) ly
    vW ← check (lx - 1) ly
    let results = catMaybes [vN, vS, vE, vW]
    pure $ case results of
        []    → Nothing
        (r:rs) → Just (foldl' (\(s1, t1) (s2, t2) →
            if s2 > s1 then (s2, t2) else (s1, t1)) r rs)

countRiverNeighbors ∷ MV.MVector s (Maybe FluidCell) → Int → Int → ST s Int
countRiverNeighbors mv lx ly = do
    let check nx ny
            | nx < 0 ∨ nx ≥ chunkSize ∨ ny < 0 ∨ ny ≥ chunkSize = pure 0
            | otherwise = do
                v ← MV.read mv (ny * chunkSize + nx)
                pure $ case v of
                    Just fc | fcType fc ≡ River → 1
                    _ → 0
    n ← check lx (ly - 1)
    e ← check (lx + 1) ly
    s ← check lx (ly + 1)
    w ← check (lx - 1) ly
    pure (n + e + s + w)

-- | Fill single-tile holes in the river. An empty tile surrounded
--   by river water on 3+ sides gets filled. Uses the maximum
--   neighbor water surface for the coverage check (to fill tiles
--   between rivers at different levels), and fills at the minimum
--   of the max surface and terrain+3 to prevent excessive depth.
--   Also removes 1-tile river spots (river tiles with 0 river
--   neighbors).
fillRiverHoles ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int → ST s ()
fillRiverHoles mv surfaceMap = do
    -- Pass A: fill holes (empty tiles with 3+ river neighbors)
    forM_ [0 .. chunkSize * chunkSize - 1] $ \idx → do
        val ← MV.read mv idx
        when (isNothing val) $ do
            let lx = idx `mod` chunkSize
                ly = idx `div` chunkSize
                surfZ = surfaceMap VU.! idx
            nCount ← countRiverNeighbors mv lx ly
            when (nCount ≥ 3) $ do
                maxSurf ← maxRiverNeighborSurface mv lx ly
                minSurf ← minRiverNeighborSurface mv lx ly
                case (maxSurf, minSurf) of
                    (Just nMax, Just nMin) → do
                        -- Standard fill: terrain at or below water surface,
                        -- cap fill depth at terrain+3.
                        let standard = surfZ ≤ nMax
                                     ∧ nMax - surfZ ≤ 5 ∧ nMax > seaLevel
                        -- Uniform-surface fill: all neighbors at the same
                        -- surface but terrain is above water. Place water at
                        -- the neighbor surface to maintain fluid continuity.
                        -- The water sits below terrain and is invisible, but
                        -- prevents gaps in the fluid map.
                            uniform = nMin ≡ nMax ∧ nCount ≥ 3
                                    ∧ surfZ ≤ nMax + 2 ∧ nMax > seaLevel
                        when (standard ∨ uniform) $ do
                            let fillZ | standard   = min nMax (surfZ + 3)
                                      | otherwise  = nMax
                            MV.write mv idx (Just (FluidCell River fillZ))
                    _ → pure ()
    -- Pass B: remove completely isolated river tiles (0 neighbors).
    -- Skip edge tiles — they may connect to the adjacent chunk's river.
    forM_ [0 .. chunkSize * chunkSize - 1] $ \idx → do
        val ← MV.read mv idx
        case val of
            Just fc | fcType fc ≡ River → do
                let lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    isEdge = lx ≡ 0 ∨ lx ≡ chunkSize - 1
                           ∨ ly ≡ 0 ∨ ly ≡ chunkSize - 1
                nCount ← countRiverNeighbors mv lx ly
                when (nCount ≡ 0 ∧ not isEdge) $
                    MV.write mv idx Nothing
            _ → pure ()

-- | Clamp all river water surfaces so they don't exceed
--   terrain + maxRiverDepth. Post-processing passes like
--   extendRiverBanks and fillRiverHoles can propagate high
--   water surfaces from headwater segments to lower terrain.
--
--   Edge tiles (on chunk boundaries) use the generous valley
--   depth cap to prevent asymmetric clamping. Two adjacent
--   chunks may classify the same terrain differently
--   (valley vs hill) because avgNeighborTerrain uses self-
--   terrain for out-of-bounds neighbors. Using the valley
--   cap at edges ensures both sides clamp identically.
clampRiverDepth ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int → ST s ()
clampRiverDepth mv surfaceMap = do
    -- Allow deeper water in valley tiles (terrain lower than neighbors).
    -- Use a generous depth cap: tiles in valleys can have depth up to 5,
    -- while tiles at or above neighbor terrain get capped at 3.
    -- This keeps water in carved channels while limiting hill artifacts.
    let maxDepthValley = 5 ∷ Int   -- carved channels can be deep
        maxDepthHill   = 3 ∷ Int   -- shallow cap for non-valley tiles
    forM_ [0 .. chunkSize * chunkSize - 1] $ \idx → do
        val ← MV.read mv idx
        case val of
            Just fc | fcType fc ≡ River → do
                let lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    surfZ = surfaceMap VU.! idx
                    -- Edge tiles get the valley cap to prevent asymmetric
                    -- clamping at chunk boundaries. The avgNeighborTerrain
                    -- check uses self-terrain for out-of-bounds directions,
                    -- which can differ from the actual cross-chunk terrain.
                    isEdge = lx ≡ 0 ∨ lx ≡ chunkSize - 1
                           ∨ ly ≡ 0 ∨ ly ≡ chunkSize - 1
                -- Check if this tile is in a valley (lower than average neighbors)
                avgNbr ← avgNeighborTerrain surfaceMap lx ly
                let inValley = surfZ < avgNbr - 1
                    maxD = if isEdge ∨ inValley then maxDepthValley else maxDepthHill
                    cap = surfZ + maxD
                -- Clamp: if water is deeper than allowed, reduce it.
                -- If the capped surface would be at or below terrain, remove.
                if fcSurface fc - surfZ > maxD
                    then if cap ≤ surfZ
                         then MV.write mv idx Nothing
                         else MV.write mv idx (Just (fc { fcSurface = cap }))
                    else pure ()
            _ → pure ()

-- | Surface smoothing. Lowers any river tile whose water is more than
--   2 above its lowest river neighbor. The threshold of 2 (rather than
--   1) prevents a neighboring lower river from dragging down a higher
--   river's channel through the smooth pass. Genuine cliffs of 3+ tiles
--   still get corrected. Floor at terrain + 1 so water covers terrain.
smoothRiverSurface ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int → ST s ()
smoothRiverSurface mv surfaceMap = go (5 ∷ Int)
  where
    go 0 = pure ()
    go n = do
        changed ← smoothPass mv surfaceMap
        when changed $ go (n - 1)

smoothPass ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int → ST s Bool
smoothPass mv surfaceMap = do
    changedRef ← MV.replicate 1 False
    let area = chunkSize * chunkSize
        smoothOne idx = do
            val ← MV.read mv idx
            case val of
                Just fc | fcType fc ≡ River → do
                    let lx = idx `mod` chunkSize
                        ly = idx `div` chunkSize
                        myWater = fcSurface fc
                        surfZ = surfaceMap VU.! idx
                    minNeighbor ← minRiverNeighborSurface mv lx ly
                    case minNeighbor of
                        Nothing → pure ()
                        Just nMin → do
                            let target = max (nMin + 2) (surfZ + 1)
                            when (myWater > target) $ do
                                MV.write mv idx (Just (fc { fcSurface = target }))
                                MV.write changedRef 0 True
                _ → pure ()
    -- Forward pass
    forM_ [0 .. area - 1] smoothOne
    -- Reverse pass — propagates changes in the other direction
    forM_ [area - 1, area - 2 .. 0] smoothOne
    MV.read changedRef 0

-- | Raise river tiles to reduce surface jumps with terrain-limited
--   neighbors. When a neighbor's water is at terrain+1 (can't be
--   lowered) and this tile's water is 2+ levels below, raise this
--   tile's water toward the neighbor. This closes diff=2 gaps caused
--   by terrain steps. Only raises; never lowers. Iterates to
--   propagate raises along the river. Cap at terrain + 5 depth.
raiseToMatchTerrain ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int → ST s ()
raiseToMatchTerrain mv surfaceMap = go (5 ∷ Int)
  where
    go 0 = pure ()
    go n = do
        changed ← raisePass mv surfaceMap
        when changed $ go (n - 1)

raisePass ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int → ST s Bool
raisePass mv surfaceMap = do
    changedRef ← MV.replicate 1 False
    let area = chunkSize * chunkSize
        raiseOne idx = do
            val ← MV.read mv idx
            case val of
                Just fc | fcType fc ≡ River → do
                    let lx = idx `mod` chunkSize
                        ly = idx `div` chunkSize
                        myWater = fcSurface fc
                        surfZ = surfaceMap VU.! idx
                    -- Find max river neighbor water surface
                    maxNeighbor ← maxRiverNeighborSurface mv lx ly
                    case maxNeighbor of
                        Nothing → pure ()
                        Just nMax → do
                            let diff = nMax - myWater
                                depthCap = surfZ + 5
                                -- Only raise if diff >= 2 (there's a visible jump)
                                -- and the raise won't exceed depth cap
                                target = min (nMax - 1) depthCap
                            when (diff ≥ 2 ∧ target > myWater ∧ target ≤ depthCap) $ do
                                MV.write mv idx (Just (fc { fcSurface = target }))
                                MV.write changedRef 0 True
                _ → pure ()
    -- Forward pass
    forM_ [0 .. area - 1] raiseOne
    -- Reverse pass
    forM_ [area - 1, area - 2 .. 0] raiseOne
    MV.read changedRef 0

-- | Average terrain elevation of cardinal neighbors.
--   Used for valley detection: a tile whose terrain is below
--   avgNeighborTerrain - 1 is considered "in a valley".
--   For out-of-bounds neighbors, uses the tile's own terrain.
avgNeighborTerrain ∷ VU.Vector Int → Int → Int → ST s Int
avgNeighborTerrain surfaceMap lx ly = do
    let myTerrZ = surfaceMap VU.! (ly * chunkSize + lx)
        terrAt x y
          | x < 0 ∨ x ≥ chunkSize ∨ y < 0 ∨ y ≥ chunkSize = myTerrZ
          | otherwise = surfaceMap VU.! (y * chunkSize + x)
        n = terrAt lx (ly - 1)
        s = terrAt lx (ly + 1)
        e = terrAt (lx + 1) ly
        w = terrAt (lx - 1) ly
    pure ((n + s + e + w) `div` 4)

-- | Remove river tiles where water surface is >3 above a river
--   neighbor's water, AND the tile can't be lowered because its
--   terrain is too high. These are valley wall tiles that create
--   visible cliffs. Tolerance of 3 allows natural gradients along
--   steep rivers without excessive tile deletion.
--   Iterates to peel away from the outside in.
removeUnresolvableCliffs ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int → ST s ()
removeUnresolvableCliffs mv surfaceMap = go (8 ∷ Int)
  where
    go 0 = pure ()
    go n = do
        changed ← unresolvableCliffPass mv surfaceMap
        when changed $ go (n - 1)

unresolvableCliffPass ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int → ST s Bool
unresolvableCliffPass mv surfaceMap = do
    changedRef ← MV.replicate 1 False
    forM_ [0 .. chunkSize * chunkSize - 1] $ \idx → do
        val ← MV.read mv idx
        case val of
            Just fc | fcType fc ≡ River → do
                let lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    myWater = fcSurface fc
                    surfZ = surfaceMap VU.! idx
                minNeighbor ← minRiverNeighborSurface mv lx ly
                case minNeighbor of
                    Nothing → pure ()
                    Just nMin →
                        -- Can't lower below terrain+1, and that's still >3 above neighbor
                        when (myWater > nMin + 3 ∧ surfZ + 1 > nMin + 3) $ do
                            MV.write mv idx Nothing
                            MV.write changedRef 0 True
            _ → pure ()
    MV.read changedRef 0

-- | Seal exposed river boundaries by flood-filling outward.
--   A river tile has an "exposed side" when a cardinal neighbor has
--   no water and terrain below the water surface — the carved valley
--   extends beyond the fill width. Fill those neighbors at the
--   adjacent river tile's water surface (capped at terrain + 5).
--   Iterate until all river edges are bounded by terrain or water.
sealRiverBoundaries ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int → ST s ()
sealRiverBoundaries mv surfaceMap = go (20 ∷ Int)
  where
    go 0 = pure ()
    go n = do
        changed ← sealPass mv surfaceMap
        when changed $ go (n - 1)

sealPass ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int → ST s Bool
sealPass mv surfaceMap = do
    -- Snapshot current state so we don't read our own writes this pass
    snapshot ← MV.clone mv
    changedRef ← MV.replicate 1 False
    forM_ [0 .. chunkSize * chunkSize - 1] $ \idx → do
        val ← MV.read snapshot idx
        case val of
            Just fc | fcType fc ≡ River → do
                let lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    waterZ = fcSurface fc
                forM_ (cardinalNeighbors lx ly) $ \(nx, ny) → do
                    let nIdx = ny * chunkSize + nx
                    nVal ← MV.read mv nIdx
                    case nVal of
                        Just _  → pure ()  -- already has fluid
                        Nothing → do
                            let nTerrZ = surfaceMap VU.! nIdx
                                myTerrZ = surfaceMap VU.! idx
                            -- Neighbor terrain is below our water: exposed side.
                            -- Only seal if neighbor terrain is at or below our
                            -- terrain — prevents spreading uphill onto valley
                            -- walls. Cap depth based on valley detection:
                            -- valley tiles get deeper seal, hill tiles get shallow.
                            nbrAvg ← avgNeighborTerrain surfaceMap nx ny
                            let nInValley = nTerrZ < nbrAvg - 1
                                maxSealDepth = if nInValley then 4 else 3
                                sealDepth = waterZ - nTerrZ
                            when (nTerrZ + 1 < waterZ ∧ sealDepth ≤ maxSealDepth
                                 ∧ nTerrZ ≤ myTerrZ
                                 ∧ waterZ > seaLevel) $ do
                                let fillZ = min waterZ (nTerrZ + maxSealDepth)
                                MV.write mv nIdx (Just (FluidCell River fillZ))
                                MV.write changedRef 0 True
            _ → pure ()
    MV.read changedRef 0

-- | Remove water tiles that can't reach a drainage point.
--   Flood-fills from "drain seeds" (water tiles on chunk edges, or
--   at/below seaLevel) through connected water, marking reachable
--   tiles. Any river water tile NOT reached is removed.
--
--   Connectivity: a water tile A can flow to neighbor B if B also
--   has water and B's water surface ≤ A's water surface. This
--   traces downhill flow paths.
--
--   Edge tiles are always drain seeds because they may connect to
--   water in adjacent chunks. Tiles at seaLevel are seeds because
--   they reach the ocean.
removeDisconnectedWater ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int → ST s ()
removeDisconnectedWater mv _surfaceMap = do
    let area = chunkSize * chunkSize
    -- Reachable mask: 0 = not reached, 1 = reached
    reached ← MV.replicate area (0 ∷ Int)

    -- Collect drain seeds: edge water tiles and tiles at/below seaLevel+1.
    -- Edge tiles are seeds because they may connect to water in adjacent
    -- chunks (which we can't see during per-chunk computation).
    seeds ← do
        acc ← newSTRef ([] ∷ [Int])
        forM_ [0 .. area - 1] $ \idx → do
            val ← MV.read mv idx
            case val of
                Just fc | fcType fc ≡ River → do
                    let lx = idx `mod` chunkSize
                        ly = idx `div` chunkSize
                        isEdge = lx ≡ 0 ∨ lx ≡ chunkSize - 1
                               ∨ ly ≡ 0 ∨ ly ≡ chunkSize - 1
                        atSea = fcSurface fc ≤ seaLevel + 1
                    when (isEdge ∨ atSea) $ do
                        MV.write reached idx 1
                        modifySTRef' acc (idx :)
                _ → pure ()
        readSTRef acc

    -- BFS from seeds: flow UPHILL (from lower to higher water).
    -- A tile is reachable if it can ultimately drain downhill to a seed.
    -- So we propagate reachability from seeds to their higher neighbors.
    let bfs [] = pure ()
        bfs (idx : queue) = do
            val ← MV.read mv idx
            case val of
                Just _fc → do
                    let lx = idx `mod` chunkSize
                        ly = idx `div` chunkSize
                    -- Check all 4 cardinal neighbors
                    newQueue ← do
                        acc ← newSTRef queue
                        forM_ [(lx,ly-1),(lx,ly+1),(lx-1,ly),(lx+1,ly)] $ \(nx, ny) →
                            when (nx ≥ 0 ∧ nx < chunkSize ∧ ny ≥ 0 ∧ ny < chunkSize) $ do
                                let nIdx = ny * chunkSize + nx
                                r ← MV.read reached nIdx
                                when (r ≡ 0) $ do
                                    nVal ← MV.read mv nIdx
                                    case nVal of
                                        Just nfc | fcType nfc ≡ River → do
                                            MV.write reached nIdx 1
                                            modifySTRef' acc (nIdx :)
                                        _ → pure ()
                        readSTRef acc
                    bfs newQueue
                _ → bfs queue

    bfs seeds

    -- Remove unreached non-edge river tiles. Edge tiles are preserved
    -- because they may connect to adjacent chunks' water that we
    -- can't see yet — the cross-chunk seal pass handles them.
    forM_ [0 .. area - 1] $ \idx → do
        r ← MV.read reached idx
        when (r ≡ 0) $ do
            val ← MV.read mv idx
            case val of
                Just fc | fcType fc ≡ River → do
                    let lx = idx `mod` chunkSize
                        ly = idx `div` chunkSize
                        isEdge = lx ≡ 0 ∨ lx ≡ chunkSize - 1
                               ∨ ly ≡ 0 ∨ ly ≡ chunkSize - 1
                    when (not isEdge) $
                        MV.write mv idx Nothing
                _ → pure ()

cardinalNeighbors ∷ Int → Int → [(Int, Int)]
cardinalNeighbors lx ly =
    [ (nx, ny)
    | (nx, ny) ← [(lx, ly-1), (lx, ly+1), (lx-1, ly), (lx+1, ly)]
    , nx ≥ 0, nx < chunkSize, ny ≥ 0, ny < chunkSize
    ]

minRiverNeighborSurface ∷ MV.MVector s (Maybe FluidCell) → Int → Int
                        → ST s (Maybe Int)
minRiverNeighborSurface mv lx ly = do
    let check x y
          | x < 0 ∨ x ≥ chunkSize ∨ y < 0 ∨ y ≥ chunkSize = pure Nothing
          | otherwise = do
              v ← MV.read mv (y * chunkSize + x)
              pure $ case v of
                  Just fc | fcType fc ≡ River → Just (fcSurface fc)
                  _ → Nothing
    vN ← check lx (ly - 1)
    vS ← check lx (ly + 1)
    vE ← check (lx + 1) ly
    vW ← check (lx - 1) ly
    let surfaces = catMaybes [vN, vS, vE, vW]
    pure $ case surfaces of
        []     → Nothing
        (s:ss) → Just (foldl' min s ss)

maxRiverNeighborSurface ∷ MV.MVector s (Maybe FluidCell) → Int → Int
                        → ST s (Maybe Int)
maxRiverNeighborSurface mv lx ly = do
    let check x y
          | x < 0 ∨ x ≥ chunkSize ∨ y < 0 ∨ y ≥ chunkSize = pure Nothing
          | otherwise = do
              v ← MV.read mv (y * chunkSize + x)
              pure $ case v of
                  Just fc | fcType fc ≡ River → Just (fcSurface fc)
                  _ → Nothing
    vN ← check lx (ly - 1)
    vS ← check lx (ly + 1)
    vE ← check (lx + 1) ly
    vW ← check (lx - 1) ly
    let surfaces = catMaybes [vN, vS, vE, vW]
    pure $ case surfaces of
        []     → Nothing
        (s:ss) → Just (foldl' max s ss)

-----------------------------------------------------------
-- River Proximity
-----------------------------------------------------------

-- | Check if any part of the river is near this chunk.
--   Uses proper point-to-segment distance for each segment,
--   plus waypoint proximity checks for bends.
--   Generous margin ensures we never miss a chunk.
riverNearChunk ∷ Int → Int → Int → RiverParams → Bool
riverNearChunk worldSize chunkGX chunkGY river =
    V.any (segOrWaypointNear worldSize chunkGX chunkGY) (rpSegments river)
  where
    segOrWaypointNear ws cgx cgy seg =
        let margin = rsValleyWidth seg + chunkSize + chunkSize
            -- chunk center
            cx = cgx + chunkSize `div` 2
            cy = cgy + chunkSize `div` 2

            -- Segment start proximity
            GeoCoord sx sy = rsStart seg
            (dxsi, dysi) = wrappedDeltaUVFluid ws sx sy cx cy
            dxs = abs dxsi
            dys = abs dysi
            startNear = dxs < margin ∧ dys < margin

            -- Segment end proximity
            GeoCoord ex ey = rsEnd seg
            (dxei, dyei) = wrappedDeltaUVFluid ws ex ey cx cy
            dxe = abs dxei
            dye = abs dyei
            endNear = dxe < margin ∧ dye < margin

            -- Closest point on segment to chunk center
            (fdxi, fdyi) = wrappedDeltaUVFluid ws sx sy ex ey
            fdx = fromIntegral fdxi ∷ Float
            fdy = fromIntegral fdyi ∷ Float
            segLen2 = fdx * fdx + fdy * fdy
            midNear = if segLen2 < 1.0
                then startNear
                else let (pxi, pyi) = wrappedDeltaUVFluid ws sx sy cx cy
                         px = fromIntegral pxi ∷ Float
                         py = fromIntegral pyi ∷ Float
                         t = max 0.0 (min 1.0 ((px * fdx + py * fdy) / segLen2))
                         closestX = t * fdx
                         closestY = t * fdy
                         distX = px - closestX
                         distY = py - closestY
                         dist = sqrt (distX * distX + distY * distY)
                     in dist < fromIntegral margin

        in startNear ∨ endNear ∨ midNear

-- | Quick boolean check: does this chunk have any river?
hasAnyRiverQuick ∷ [RiverParams] → Int → ChunkCoord → Bool
hasAnyRiverQuick rivers worldSize coord =
    let ChunkCoord cx cy = coord
        chunkMinGX = cx * chunkSize
        chunkMinGY = cy * chunkSize
    in any (riverNearChunk worldSize chunkMinGX chunkMinGY) rivers

-----------------------------------------------------------
-- River Fluid Fill (interpolated water surface)
-----------------------------------------------------------
--
-- Water surface is INTERPOLATED along each segment, smoothly
-- decreasing from start to end elevation. This gives a continuous
-- gradient instead of discrete per-segment steps.
--
-- With fixupSegmentContinuity ensuring adjacent segments share
-- endpoint elevations, the interpolated surface is continuous
-- at segment junctions — no visible water cliffs.
--
-- Uses floor() to avoid round's half-to-even creating alternating
-- elevation steps (checkerboard pattern) at transition tiles.

fillRiverDirect ∷ MV.MVector s (Maybe FluidCell)
                → RiverParams → Int → Int → Int
                → VU.Vector Int
                → ST s ()
fillRiverDirect mv river worldSize chunkGX chunkGY surfaceMap =
    let segments = rpSegments river
    in forEachSurface surfaceMap $ \idx lx ly surfZ →
        case bestRiverFill worldSize (chunkGX + lx) (chunkGY + ly)
                                     surfZ segments of
            Nothing → pure ()
            Just fc → do
                existing ← MV.read mv idx
                case existing of
                    -- When two rivers overlap, keep the LOWER water surface.
                    -- This prevents upstream high-water from flooding
                    -- downstream terrain where water should be lower.
                    Just old | fcType old ≡ River →
                        when (fcSurface fc < fcSurface old) $
                            MV.write mv idx (Just fc)
                    _ → MV.write mv idx (Just fc)

-- | At overlapping regions, pick the fill from the CLOSEST segment
--   (smallest perpendicular distance). This ensures each tile gets
--   water from the segment it's actually in, preventing upstream
--   segments from placing high water on tiles near downstream
--   segments.
bestRiverFill ∷ Int → Int → Int → Int → V.Vector RiverSegment
              → Maybe FluidCell
bestRiverFill worldSize gx gy surfZ segments =
    let segResults = V.toList $ V.map
            (riverFillFromSegmentWithDist worldSize gx gy surfZ) segments
    in pickClosest segResults

pickClosest ∷ [(Maybe FluidCell, Float)] → Maybe FluidCell
pickClosest = go Nothing
  where
    go acc [] = case acc of
        Nothing        → Nothing
        Just (mfc, _)  → mfc
    go Nothing ((mfc, d) : rest) = case mfc of
        Nothing → go Nothing rest
        Just _  → go (Just (mfc, d)) rest
    go (Just (bestFc, bestD)) ((mfc, d) : rest) = case mfc of
        Nothing → go (Just (bestFc, bestD)) rest
        Just _  → if d < bestD
                   then go (Just (mfc, d)) rest
                   else go (Just (bestFc, bestD)) rest

-- | Fill a tile from a segment, returning both the fluid cell
--   and the perpendicular distance for segment selection.
--   Water surface is interpolated from rsWaterStart to rsWaterEnd,
--   which are set during world generation (and corrected post-timeline
--   to match actual terrain). This decouples fill from carving references.
riverFillFromSegmentWithDist ∷ Int → Int → Int → Int → RiverSegment
                             → (Maybe FluidCell, Float)
riverFillFromSegmentWithDist worldSize gx gy surfZ seg =
    let GeoCoord sx sy = rsStart seg
        GeoCoord ex ey = rsEnd seg
        (dxi, dyi) = wrappedDeltaUVFluid worldSize sx sy ex ey
        dx' = fromIntegral dxi ∷ Float
        dy' = fromIntegral dyi ∷ Float
        segLen2 = dx' * dx' + dy' * dy'
    in if segLen2 < 1.0
       then (Nothing, 9999.0)
       else
       let segLen = sqrt segLen2
           (pxi, pyi) = wrappedDeltaUVFluid worldSize sx sy gx gy
           px = fromIntegral pxi ∷ Float
           py = fromIntegral pyi ∷ Float
           tRaw = (px * dx' + py * dy') / segLen2
       -- Small overlap at segment boundaries for gap prevention.
       -- Kept tight (0.05) to avoid bleeding into adjacent segments.
       in if tRaw < -0.05 ∨ tRaw > 1.05
          then (Nothing, 9999.0)
          else
          let signedPerp = (px * dy' - py * dx') / segLen
              effectivePerpDist = abs signedPerp

              -- Fill zone: extends into the valley beyond the channel.
              -- The carving creates a valley of rsValleyWidth, and the
              -- fill should cover enough of it to avoid dry gaps between
              -- the channel center and the valley walls. Use the midpoint
              -- between channel edge and valley edge to fill the carved
              -- floor while keeping water off the upper valley walls.
              channelEdge = fromIntegral (rsWidth seg) / 2.0 ∷ Float
              valleyEdge  = fromIntegral (rsValleyWidth seg) / 2.0 ∷ Float
              fillW = channelEdge + max 2.0 ((valleyEdge - channelEdge) * 0.5)
          in if effectivePerpDist > fillW
             then (Nothing, effectivePerpDist)
             else
             let -- Interpolate water surface along segment axis.
                 tClamped = max 0.0 (min 1.0 tRaw)
                 startW = fromIntegral (rsWaterStart seg) ∷ Float
                 endW   = fromIntegral (rsWaterEnd seg) ∷ Float
                 axialWaterSurface = floor (startW + tClamped * (endW - startW)) ∷ Int
                 -- Coastal flattening: when a segment ends at or below sea
                 -- level, the downstream portion should flatten to create a
                 -- smooth estuary. Blend toward seaLevel so river water
                 -- meets ocean at the same height.
                 coastalFlat = seaLevel
                 flattenedWater
                   | rsEndElev seg ≤ seaLevel ∧ axialWaterSurface > coastalFlat =
                       -- Only flatten the downstream portion of the
                       -- segment.  Keep the original interpolated surface
                       -- for the upstream part and smoothly blend toward
                       -- coastalFlat over the final 40%.
                       let blendStart = 0.6 ∷ Float
                           coastT = max 0.0 ((tClamped - blendStart)
                                            / (1.0 - blendStart))
                           smoothT = coastT * coastT * (3.0 - 2.0 * coastT)
                           blended = fromIntegral axialWaterSurface
                                   * (1.0 - smoothT)
                                   + fromIntegral coastalFlat * smoothT
                       in max coastalFlat (floor blended ∷ Int)
                   | otherwise = axialWaterSurface
                 -- Reference terrain at this axial position.
                 -- Tiles above this are on the valley wall, not in the channel.
                 startE = fromIntegral (rsStartElev seg) ∷ Float
                 endE   = fromIntegral (rsEndElev seg) ∷ Float
                 refElev = floor (startE + tClamped * (endE - startE)) ∷ Int
                 -- Perpendicular depth falloff: water depth decreases
                 -- from center to valley edge. In the channel proper,
                 -- full depth (rsDepth). On the valley floor beyond the
                 -- channel, depth tapers from rsDepth down to 1.
                 channelHalfW' = fromIntegral (rsWidth seg) / 2.0 ∷ Float
                 perpFraction = if effectivePerpDist ≤ channelHalfW'
                     then 0.0 ∷ Float
                     else min 1.0 ((effectivePerpDist - channelHalfW')
                                  / max 1.0 (fillW - channelHalfW'))
                 -- In channel center: max depth = rsDepth + 1
                 -- On valley floor: depth tapers linearly to 1.
                 -- Use smoothstep for gradual transition to avoid
                 -- sharp depth changes at the channel edge.
                 smoothPerp = let t' = max 0.0 (min 1.0 perpFraction)
                              in t' * t' * (3.0 - 2.0 * t')
                 rawDepth = round (fromIntegral (rsDepth seg + 1)
                                * (1.0 - smoothPerp) + 1.0 * smoothPerp ∷ Float)
                 -- Cap at 2 only for tiles well outside the channel
                 -- (beyond channelHalfW + 2). Tiles on the valley floor
                 -- near the channel can have deeper water.
                 maxFillDepth = if effectivePerpDist ≤ channelHalfW' + 2.0
                                then max 1 rawDepth
                                else max 1 (min 2 rawDepth)
                 -- Water surface is the MINIMUM of the axially-interpolated
                 -- surface and terrain + maxFillDepth. This ensures water
                 -- on valley slopes sits close to the terrain surface.
                 -- Use max (surfZ+1) to ensure at least 1 tile of water
                 -- when the tile is in the carved channel (below refElev).
                 waterSurface = min flattenedWater (surfZ + maxFillDepth)
                 -- Force water placement on tiles near the channel center
                 -- (perpDist within channelHalfW + 1) AND below reference.
                 -- The +1 margin accounts for coordinate rounding between
                 -- the fill and carving systems. Tiles further out on the
                 -- valley banks only get water if waterSurface naturally
                 -- exceeds terrain, preventing forced water on valley walls.
                 inChannelProper = effectivePerpDist ≤ channelHalfW' + 1.0
                                   ∧ surfZ < refElev
                 -- "In valley" = terrain at or below reference elevation.
                 -- Tiles above refElev are outside the valley entirely
                 -- and should never get water from this segment.
                 inValley = surfZ ≤ refElev
                 -- For deeply cut tiles: force at least 1 depth when
                 -- rounding causes waterSurface == surfZ. This ensures
                 -- carved tiles always show water.
                 -- For valley wall tiles: only place water if waterSurface
                 -- naturally exceeds terrain.
                 effectiveWater = if inChannelProper ∧ waterSurface ≤ surfZ
                                      ∧ flattenedWater ≥ surfZ
                                  then surfZ + 1
                                  else waterSurface
             in if effectiveWater ≤ surfZ
                then (Nothing, effectivePerpDist)
                else if not inValley
                     then (Nothing, effectivePerpDist)
                     else (Just (FluidCell River effectiveWater), effectivePerpDist)

-----------------------------------------------------------
-- Segment Continuity
-----------------------------------------------------------

-- | Ensure adjacent segments share consistent endpoint elevations.
--   After any modification (meander, deepen, etc.), the end of
--   segment N must equal the start of segment N+1.
--   Also enforces monotonic descent on water surface.
fixupSegmentContinuity ∷ V.Vector RiverSegment → V.Vector RiverSegment
fixupSegmentContinuity v
    | V.length v ≤ 1 = v
    | otherwise = V.fromList (V.head v : go (V.head v) (V.toList (V.tail v)))
  where
    go _ [] = []
    go prev (cur : xs) =
        let fixed = cur { rsStartElev  = rsEndElev prev
                        , rsStart      = rsEnd prev
                        , rsWaterStart = rsWaterEnd prev }
            fixed' = if rsEndElev fixed > rsStartElev fixed
                     then fixed { rsEndElev  = rsStartElev fixed
                                , rsWaterEnd = min (rsWaterEnd fixed)
                                                   (rsWaterStart fixed) }
                     else fixed
        in fixed' : go fixed' xs

-----------------------------------------------------------
-- Cross-Chunk River Boundary Sealing
-----------------------------------------------------------

-- | Post-insertion pass: seal river boundaries across chunk borders.
--   For each chunk in the list, examine edge river tiles and fill
--   exposed neighbors in adjacent chunks. Iterates to propagate
--   fills that create new exposed edges.
--   Also equalizes water surfaces across chunk boundaries so
--   adjacent river tiles don't have visible height jumps.
sealCrossChunkRivers ∷ [ChunkCoord] → WorldTileData → WorldTileData
sealCrossChunkRivers newCoords wtd =
    let -- Include new chunks AND their loaded neighbors — a river in
        -- an existing neighbor chunk may have exposed sides toward the
        -- newly-generated chunk.
        allCoords = let nbrSet = HM.fromList
                          [ (c, ())
                          | coord ← newCoords
                          , c ← coord : chunkNeighbors' coord
                          , HM.member c (wtdChunks wtd)
                          ]
                    in HM.keys nbrSet
        -- Phase 1: equalize water surfaces across chunk boundaries.
        -- This fixes visible height jumps where per-chunk post-processing
        -- (clamp, smooth, cap) independently adjusts water levels.
        equalized = equalizeCrossChunkRivers allCoords wtd
        -- Phase 2: seal exposed boundaries (fill empty tiles).
        sealed = sealLoop (10 ∷ Int) allCoords equalized
        -- Phase 3: re-equalize after sealing. The seal/flood-fill can
        -- create new water tiles that introduce height jumps at chunk
        -- boundaries that weren't present before.
        reEqualized = equalizeCrossChunkRivers allCoords sealed
        -- Phase 4: force-match boundary surfaces. The equalization above
        -- lowers/raises edge tiles and then runs smoothInwardFromEdge,
        -- which can re-raise lowered tiles when interior neighbors are
        -- high. This final pass forces exact boundary matching without
        -- inward propagation, accepting a small interior jump instead
        -- of a cross-chunk jump (interior jumps are handled by the
        -- renderer's smooth shading; cross-chunk jumps create visible
        -- seams).
        matched = forceMatchBoundaries allCoords reEqualized
        -- Phase 5: fill holes in affected chunks. The cross-chunk seal
        -- creates new water tiles that weren't present during per-chunk
        -- processing, leaving dry gaps between sealed tiles and the
        -- main river body.
        filled = fillCrossChunkHoles allCoords matched
        -- Phase 6: raise low tiles in each chunk to reduce jumps.
        raised = foldl' (\td coord →
            case HM.lookup coord (wtdChunks td) of
                Nothing → td
                Just lc →
                    let lc' = raiseInChunk lc
                    in if lcFluidMap lc' ≡ lcFluidMap lc
                       then td
                       else td { wtdChunks = HM.insert coord lc' (wtdChunks td) }
            ) filled allCoords
    in raised
  where
    sealLoop 0 _  td = td
    sealLoop n cs td =
        let (td', changed) = sealCrossChunkPass cs td
        in if changed then sealLoop (n - 1) cs td' else td'

-- | Equalize river water surfaces across chunk boundaries.
--   For each pair of adjacent edge river tiles in different chunks,
--   lower the higher water surface toward the lower one. This prevents
--   visible height jumps caused by per-chunk post-processing (depth
--   clamp, smooth, cap-to-rim) independently adjusting water levels.
--   Iterates to propagate changes inward.
equalizeCrossChunkRivers ∷ [ChunkCoord] → WorldTileData → WorldTileData
equalizeCrossChunkRivers coords wtd = eqLoop (10 ∷ Int) wtd
  where
    eqLoop 0 td = td
    eqLoop n td =
        let (td', changed) = eqPass td
        in if changed then eqLoop (n - 1) td' else td'

    eqPass td = foldl' eqChunk (td, False) coords

    eqChunk (td, changed) coord =
        case HM.lookup coord (wtdChunks td) of
            Nothing → (td, changed)
            Just lc →
                let edges = edgeRiverTiles lc
                in foldl' (eqEdgeTile coord) (td, changed) edges

    eqEdgeTile (ChunkCoord cx cy) (td, changed) (lx, ly, myWaterZ) =
        let dirs = [ (ChunkCoord cx (cy-1), lx, chunkSize-1) | ly ≡ 0 ]
                ⧺ [ (ChunkCoord cx (cy+1), lx, 0)           | ly ≡ chunkSize-1 ]
                ⧺ [ (ChunkCoord (cx-1) cy, chunkSize-1, ly) | lx ≡ 0 ]
                ⧺ [ (ChunkCoord (cx+1) cy, 0, ly)           | lx ≡ chunkSize-1 ]
        in foldl' (tryEqualize (ChunkCoord cx cy) lx ly myWaterZ) (td, changed) dirs

    -- | Maximum allowed water surface difference across a chunk boundary.
    --   Larger values permit natural rapids; smaller values force smoother
    --   transitions. A tolerance of 1 means adjacent river tiles may differ
    --   by at most 1 water level.
    maxBoundaryDiff = 1 ∷ Int

    -- | Maximum depth we allow when raising water to meet a neighbor.
    --   Prevents water from being raised unreasonably high above terrain.
    --   Generous (5) to handle cross-chunk carved valleys where the
    --   neighboring chunk's terrain is deeper than within-chunk capping
    --   expected.
    maxRaiseDepth = 5 ∷ Int

    tryEqualize myCoord myLX myLY myWaterZ (td, changed) (nbrCoord, nbrLX, nbrLY) =
        case HM.lookup nbrCoord (wtdChunks td) of
            Nothing → (td, changed)
            Just nbrLC →
                let nIdx = nbrLY * chunkSize + nbrLX
                    nbrFluid = lcFluidMap nbrLC V.! nIdx
                in case nbrFluid of
                    Just nbrFC | fcType nbrFC ≡ River →
                        let nbrWaterZ = fcSurface nbrFC
                            diff = abs (myWaterZ - nbrWaterZ)
                        in if diff ≤ maxBoundaryDiff
                           then (td, changed)  -- already within tolerance
                           else if myWaterZ > nbrWaterZ
                           then
                             -- Our side is higher. Two-pronged approach:
                             -- 1. Try to lower our water as much as terrain permits.
                             -- 2. Try to raise the neighbor's water to close the gap.
                             -- 3. If the gap is STILL large (>4), remove the high-side
                             --    water tile entirely to prevent a floating chunk.
                             let myIdx = myLY * chunkSize + myLX
                             in case HM.lookup myCoord (wtdChunks td) of
                                 Nothing → (td, changed)
                                 Just myLC →
                                     let myTerrZ = lcTerrainSurfaceMap myLC VU.! myIdx
                                         nbrTerrZ = lcTerrainSurfaceMap nbrLC VU.! nIdx
                                         -- Step 1: lower our side
                                         lowerTarget = max (nbrWaterZ + maxBoundaryDiff) (myTerrZ + 1)
                                         td1 = if lowerTarget < myWaterZ
                                               then let fm' = lcFluidMap myLC V.// [(myIdx, Just (FluidCell River lowerTarget))]
                                                        sm' = lcSurfaceMap myLC VU.// [(myIdx, max myTerrZ lowerTarget)]
                                                        myLC' = myLC { lcFluidMap = fm', lcSurfaceMap = sm' }
                                                        myLC'' = smoothInwardFromEdge myLC' myLX myLY
                                                        chunks' = HM.insert myCoord myLC'' (wtdChunks td)
                                                    in td { wtdChunks = chunks' }
                                               else td
                                         -- Get updated water level after lowering
                                         myWaterZ' = min myWaterZ lowerTarget
                                         -- Step 2: if still too high, raise the neighbor
                                         raiseTarget = myWaterZ' - maxBoundaryDiff
                                         nbrDepthCap = nbrTerrZ + maxRaiseDepth
                                         td2 = if raiseTarget > nbrWaterZ ∧ raiseTarget ≤ nbrDepthCap
                                               then case HM.lookup nbrCoord (wtdChunks td1) of
                                                   Nothing → td1
                                                   Just nbrLC' →
                                                       let fm' = lcFluidMap nbrLC' V.// [(nIdx, Just (FluidCell River raiseTarget))]
                                                           sm' = lcSurfaceMap nbrLC' VU.// [(nIdx, max nbrTerrZ raiseTarget)]
                                                           nbrLC'' = nbrLC' { lcFluidMap = fm', lcSurfaceMap = sm' }
                                                           nbrLC''' = raiseInwardFromEdge nbrLC'' nbrLX nbrLY raiseTarget
                                                           chunks' = HM.insert nbrCoord nbrLC''' (wtdChunks td1)
                                                       in td1 { wtdChunks = chunks' }
                                               else td1
                                         -- Step 3: Check if the gap is still too large.
                                         -- Use neighbor's actual water level (raised if step 2 fired).
                                         nbrWaterZ' = if raiseTarget > nbrWaterZ ∧ raiseTarget ≤ nbrDepthCap
                                                      then raiseTarget
                                                      else nbrWaterZ
                                         finalDiff = myWaterZ' - nbrWaterZ'
                                     in if finalDiff > 4
                                        then
                                          -- Gap too large even after lowering — remove
                                          -- our water tile to prevent a floating chunk.
                                          case HM.lookup myCoord (wtdChunks td2) of
                                            Nothing → (td2, True)
                                            Just myLC' →
                                              let myTerrZ' = lcTerrainSurfaceMap myLC' VU.! myIdx
                                                  fm' = lcFluidMap myLC' V.// [(myIdx, Nothing)]
                                                  sm' = lcSurfaceMap myLC' VU.// [(myIdx, myTerrZ')]
                                                  myLC'' = myLC' { lcFluidMap = fm', lcSurfaceMap = sm' }
                                                  chunks' = HM.insert myCoord myLC'' (wtdChunks td2)
                                              in (td2 { wtdChunks = chunks' }, True)
                                        else (td2, lowerTarget < myWaterZ ∨ raiseTarget > nbrWaterZ)
                           else
                             -- Neighbor is higher — handled when we process the neighbor chunk.
                             -- (Or we can handle symmetrically here.)
                             (td, changed)
                    _ → (td, changed)

-- | Final boundary matching pass: for each pair of adjacent edge
--   river tiles across a chunk boundary, force-set the higher side
--   to within 1 of the lower side. Unlike equalizeCrossChunkRivers,
--   this does NOT run smoothInwardFromEdge, so interior tiles won't
--   fight the correction. The resulting small interior jump (edge tile
--   vs its inward neighbor) is handled by the renderer's smooth shading
--   and is far less visible than a cross-chunk seam.
forceMatchBoundaries ∷ [ChunkCoord] → WorldTileData → WorldTileData
forceMatchBoundaries coords wtd = fmLoop (5 ∷ Int) wtd
  where
    fmLoop 0 td = td
    fmLoop n td =
        let (td', changed) = fmPass td
        in if changed then fmLoop (n - 1) td' else td'

    fmPass td = foldl' fmChunk (td, False) coords

    fmChunk (td, changed) coord =
        case HM.lookup coord (wtdChunks td) of
            Nothing → (td, changed)
            Just lc →
                let edges = edgeRiverTiles lc
                in foldl' (fmEdgeTile coord) (td, changed) edges

    fmEdgeTile (ChunkCoord cx cy) (td, changed) (lx, ly, myWaterZ) =
        let dirs = [ (ChunkCoord cx (cy-1), lx, chunkSize-1) | ly ≡ 0 ]
                ⧺ [ (ChunkCoord cx (cy+1), lx, 0)           | ly ≡ chunkSize-1 ]
                ⧺ [ (ChunkCoord (cx-1) cy, chunkSize-1, ly) | lx ≡ 0 ]
                ⧺ [ (ChunkCoord (cx+1) cy, 0, ly)           | lx ≡ chunkSize-1 ]
        in foldl' (fmTryMatch (ChunkCoord cx cy) lx ly myWaterZ) (td, changed) dirs

    fmTryMatch myCoord myLX myLY myWaterZ (td, changed) (nbrCoord, nbrLX, nbrLY) =
        case HM.lookup nbrCoord (wtdChunks td) of
            Nothing → (td, changed)
            Just nbrLC →
                let nIdx = nbrLY * chunkSize + nbrLX
                    nbrFluid = lcFluidMap nbrLC V.! nIdx
                in case nbrFluid of
                    Just nbrFC | fcType nbrFC ≡ River →
                        let nbrWaterZ = fcSurface nbrFC
                            diff = abs (myWaterZ - nbrWaterZ)
                        in if diff ≤ 1
                           then (td, changed)  -- already within tolerance
                           else if myWaterZ > nbrWaterZ
                           then
                             -- Lower our side to nbrWaterZ + 1.
                             let myIdx = myLY * chunkSize + myLX
                             in case HM.lookup myCoord (wtdChunks td) of
                                 Nothing → (td, changed)
                                 Just myLC →
                                     let myTerrZ = lcTerrainSurfaceMap myLC VU.! myIdx
                                         target = max (nbrWaterZ + 1) (myTerrZ + 1)
                                     in if target < myWaterZ
                                        then let fm' = lcFluidMap myLC V.// [(myIdx, Just (FluidCell River target))]
                                                 sm' = lcSurfaceMap myLC VU.// [(myIdx, max myTerrZ target)]
                                                 myLC' = myLC { lcFluidMap = fm', lcSurfaceMap = sm' }
                                                 chunks' = HM.insert myCoord myLC' (wtdChunks td)
                                             in (td { wtdChunks = chunks' }, True)
                                        else (td, changed)
                           else (td, changed)  -- neighbor is higher, handled when we process that chunk
                    _ → (td, changed)

-- | After lowering an edge tile's water surface, propagate the
--   change inward. Any interior river tile whose water is more
--   than 1 above its lowest river neighbor gets lowered.
--   This prevents sharp interior cliffs created by the edge fix.
-- | Fill single-tile holes in affected chunks after cross-chunk sealing.
--   The seal pass creates new water tiles that weren't present during
--   per-chunk processing. Empty tiles with 3+ river neighbors whose
--   water covers the tile should be filled. Also runs a smooth pass
--   to clean up any depth artifacts.
fillCrossChunkHoles ∷ [ChunkCoord] → WorldTileData → WorldTileData
fillCrossChunkHoles coords wtd = foldl' fillChunkHoles wtd coords
  where
    fillChunkHoles td coord =
        case HM.lookup coord (wtdChunks td) of
            Nothing → td
            Just lc →
                let lc' = holeFillLoop (3 ∷ Int) coord td lc
                in if lcFluidMap lc' ≡ lcFluidMap lc
                   then td  -- no changes
                   else let chunks' = HM.insert coord lc' (wtdChunks td)
                        in td { wtdChunks = chunks' }

    holeFillLoop 0 _     _  lc = lc
    holeFillLoop n coord td lc =
        let (lc', changed) = holeFillPass coord td lc
        in if changed then holeFillLoop (n - 1) coord td lc' else lc'

    holeFillPass coord td lc =
        foldl' (fillHoleTile coord td) (lc, False) [0 .. chunkSize * chunkSize - 1]

    fillHoleTile coord td (lc, changed) idx =
        case lcFluidMap lc V.! idx of
            Just _ → (lc, changed)  -- already has fluid
            Nothing →
                let lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    terrZ = lcTerrainSurfaceMap lc VU.! idx
                    isEdge = lx ≡ 0 ∨ lx ≡ chunkSize - 1
                           ∨ ly ≡ 0 ∨ ly ≡ chunkSize - 1
                    -- In-chunk river neighbors
                    inChunkNeighbors = [ (nx, ny)
                                       | (nx, ny) ← [(lx,ly-1),(lx,ly+1),(lx-1,ly),(lx+1,ly)]
                                       , nx ≥ 0, nx < chunkSize, ny ≥ 0, ny < chunkSize
                                       ]
                    riverNbrs = [ fcSurface nfc
                                | (nx, ny) ← inChunkNeighbors
                                , Just nfc ← [lcFluidMap lc V.! (ny * chunkSize + nx)]
                                , fcType nfc ≡ River
                                ]
                    -- Cross-chunk river neighbors for edge tiles.
                    -- Check the adjacent chunk's edge tile for water.
                    ChunkCoord cx cy = coord
                    crossNbrs = if not isEdge then []
                                else (if ly ≡ 0
                                      then maybeToList (crossChunkRiver td (ChunkCoord cx (cy-1)) lx (chunkSize-1))
                                      else [])
                                  ⧺ (if ly ≡ chunkSize-1
                                      then maybeToList (crossChunkRiver td (ChunkCoord cx (cy+1)) lx 0)
                                      else [])
                                  ⧺ (if lx ≡ 0
                                      then maybeToList (crossChunkRiver td (ChunkCoord (cx-1) cy) (chunkSize-1) ly)
                                      else [])
                                  ⧺ (if lx ≡ chunkSize-1
                                      then maybeToList (crossChunkRiver td (ChunkCoord (cx+1) cy) 0 ly)
                                      else [])
                    allNbrs = riverNbrs ⧺ crossNbrs
                in case allNbrs of
                    [] → (lc, changed)
                    (_:_) →
                        let nMax = foldl' max minBound allNbrs
                            nMin = foldl' min maxBound allNbrs
                            nCount = length allNbrs
                            -- Edge tiles need only 2 neighbors (the 3rd may be
                            -- across the boundary and visible only via crossNbrs).
                            -- Interior tiles still need 3.
                            threshold = if isEdge then 2 else 3
                            -- Standard: terrain below water.
                            standard = nCount ≥ threshold ∧ terrZ < nMax
                                     ∧ nMax - terrZ ≤ 5 ∧ nMax > seaLevel
                            -- Uniform: terrain slightly above uniform water.
                            -- Fill at nMax (below terrain) for continuity.
                            uniform = nCount ≥ threshold ∧ nMin ≡ nMax
                                    ∧ terrZ ≤ nMax + 2 ∧ nMax > seaLevel
                        in if standard ∨ uniform
                           then let fillZ | standard   = min nMax (terrZ + 3)
                                          | otherwise  = nMax
                                    fm' = lcFluidMap lc V.// [(idx, Just (FluidCell River fillZ))]
                                    sm' = lcSurfaceMap lc VU.// [(idx, max terrZ fillZ)]
                                    lc' = lc { lcFluidMap = fm', lcSurfaceMap = sm' }
                                in (lc', True)
                           else (lc, changed)

    -- | Look up a river fluid cell in a neighboring chunk at (lx, ly).
    crossChunkRiver td nbrCoord lx ly =
        case HM.lookup nbrCoord (wtdChunks td) of
            Nothing → Nothing
            Just nbrLC →
                let nIdx = ly * chunkSize + lx
                in case lcFluidMap nbrLC V.! nIdx of
                    Just nfc | fcType nfc ≡ River → Just (fcSurface nfc)
                    _ → Nothing

-- | Raise low river tiles to reduce surface jumps. For each river
--   tile where a neighbor's water is 2+ levels higher, raise this
--   tile's water to neighbor-1 (capped at terrain+5). Iterates to
--   propagate raises. Works on immutable LoadedChunk data.
raiseInChunk ∷ LoadedChunk → LoadedChunk
raiseInChunk lc0 = go (5 ∷ Int) lc0
  where
    go 0 lc = lc
    go n lc =
        let (lc', changed) = raiseChunkPass lc
        in if changed then go (n - 1) lc' else lc'

    raiseChunkPass lc =
        foldl' raiseTile (lc, False) [0 .. chunkSize * chunkSize - 1]

    raiseTile (lc, changed) idx =
        case lcFluidMap lc V.! idx of
            Just fc | fcType fc ≡ River →
                let lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    myWater = fcSurface fc
                    terrZ = lcTerrainSurfaceMap lc VU.! idx
                    neighbors = [ (nx, ny)
                                | (nx, ny) ← [(lx,ly-1),(lx,ly+1),(lx-1,ly),(lx+1,ly)]
                                , nx ≥ 0, nx < chunkSize, ny ≥ 0, ny < chunkSize
                                ]
                    nbrSurfaces = [ fcSurface nfc
                                 | (nx, ny) ← neighbors
                                 , Just nfc ← [lcFluidMap lc V.! (ny * chunkSize + nx)]
                                 , fcType nfc ≡ River
                                 ]
                in case nbrSurfaces of
                    [] → (lc, changed)
                    _  →
                        let nMax = foldl' max minBound nbrSurfaces
                            diff = nMax - myWater
                            depthCap = terrZ + 5
                            target = min (nMax - 1) depthCap
                        in if diff ≥ 2 ∧ target > myWater
                           then let fm' = lcFluidMap lc V.// [(idx, Just (fc { fcSurface = target }))]
                                    sm' = lcSurfaceMap lc VU.// [(idx, max terrZ target)]
                                    lc' = lc { lcFluidMap = fm', lcSurfaceMap = sm' }
                                in (lc', True)
                           else (lc, changed)
            _ → (lc, changed)

smoothInwardFromEdge ∷ LoadedChunk → Int → Int → LoadedChunk
smoothInwardFromEdge lc0 _startLX _startLY = go (8 ∷ Int) lc0
  where
    go 0 lc = lc
    go n lc =
        let (lc', changed) = smoothInwardPass lc
        in if changed then go (n - 1) lc' else lc'

    smoothInwardPass lc =
        foldl' smoothTile (lc, False) [0 .. chunkSize * chunkSize - 1]

    smoothTile (lc, changed) idx =
        case lcFluidMap lc V.! idx of
            Just fc | fcType fc ≡ River →
                let lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    myWater = fcSurface fc
                    terrZ = lcTerrainSurfaceMap lc VU.! idx
                    -- Find minimum river neighbor water surface
                    neighbors = [ (nx, ny)
                                | (nx, ny) ← [(lx,ly-1),(lx,ly+1),(lx-1,ly),(lx+1,ly)]
                                , nx ≥ 0, nx < chunkSize, ny ≥ 0, ny < chunkSize
                                ]
                    nbrSurfaces = [ fcSurface nfc
                                 | (nx, ny) ← neighbors
                                 , Just nfc ← [lcFluidMap lc V.! (ny * chunkSize + nx)]
                                 , fcType nfc ≡ River
                                 ]
                in case nbrSurfaces of
                    [] → (lc, changed)
                    (s:ss) →
                        let nMin = foldl' min s ss
                            target = max (nMin + 1) (terrZ + 1)
                        in if myWater > target
                           then let fm' = lcFluidMap lc V.// [(idx, Just (fc { fcSurface = target }))]
                                    sm' = lcSurfaceMap lc VU.// [(idx, max terrZ target)]
                                    lc' = lc { lcFluidMap = fm', lcSurfaceMap = sm' }
                                in (lc', True)
                           else (lc, changed)
            _ → (lc, changed)

-- | After raising an edge tile's water surface, propagate the raise
--   inward. Adjacent river tiles that are lower get raised to reduce
--   the step. Only raises tiles; won't lower anything.
--   Cap at terrain + maxRaiseDepth to prevent excessive flooding.
raiseInwardFromEdge ∷ LoadedChunk → Int → Int → Int → LoadedChunk
raiseInwardFromEdge lc0 _startLX _startLY _targetZ = go (4 ∷ Int) lc0
  where
    maxRaiseD = 5 ∷ Int
    go 0 lc = lc
    go n lc =
        let (lc', changed') = raisePass lc
        in if changed' then go (n - 1) lc' else lc'

    raisePass lc =
        foldl' raiseTile (lc, False) [0 .. chunkSize * chunkSize - 1]

    raiseTile (lc, changed) idx =
        case lcFluidMap lc V.! idx of
            Just fc | fcType fc ≡ River →
                let lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    myWater = fcSurface fc
                    terrZ = lcTerrainSurfaceMap lc VU.! idx
                    neighbors = [ (nx, ny)
                                | (nx, ny) ← [(lx,ly-1),(lx,ly+1),(lx-1,ly),(lx+1,ly)]
                                , nx ≥ 0, nx < chunkSize, ny ≥ 0, ny < chunkSize
                                ]
                    nbrSurfaces = [ fcSurface nfc
                                 | (nx, ny) ← neighbors
                                 , Just nfc ← [lcFluidMap lc V.! (ny * chunkSize + nx)]
                                 , fcType nfc ≡ River
                                 ]
                in case nbrSurfaces of
                    [] → (lc, changed)
                    (s:ss) →
                        let nMax = foldl' max s ss
                            -- Raise to 1 below neighbor max, capped by depth limit
                            target = min (nMax - 1) (terrZ + maxRaiseD)
                        in if target > myWater ∧ target > terrZ
                           then let fm' = lcFluidMap lc V.// [(idx, Just (fc { fcSurface = target }))]
                                    sm' = lcSurfaceMap lc VU.// [(idx, max terrZ target)]
                                    lc' = lc { lcFluidMap = fm', lcSurfaceMap = sm' }
                                in (lc', True)
                           else (lc, changed)
            _ → (lc, changed)

chunkNeighbors' ∷ ChunkCoord → [ChunkCoord]
chunkNeighbors' (ChunkCoord cx cy) =
    [ ChunkCoord (cx-1) cy, ChunkCoord (cx+1) cy
    , ChunkCoord cx (cy-1), ChunkCoord cx (cy+1) ]

sealCrossChunkPass ∷ [ChunkCoord] → WorldTileData → (WorldTileData, Bool)
sealCrossChunkPass coords wtd =
    foldl' processChunk (wtd, False) coords

processChunk ∷ (WorldTileData, Bool) → ChunkCoord → (WorldTileData, Bool)
processChunk (wtd, changed) coord =
    case HM.lookup coord (wtdChunks wtd) of
        Nothing → (wtd, changed)
        Just lc → foldl' (sealEdgeTile coord lc) (wtd, changed)
                         (edgeRiverTiles lc)

-- | Collect all river tiles on chunk edges with their cross-chunk
--   neighbor coordinates.
edgeRiverTiles ∷ LoadedChunk → [(Int, Int, Int)]
edgeRiverTiles lc =
    -- Each entry: (index in this chunk, waterZ, index to check in neighbor chunk)
    -- But we actually need: (myIdx, waterZ, nbrChunkOffset, nbrIdx)
    -- Let's just collect (lx, ly, waterZ) for edge tiles.
    [ (lx, ly, fcSurface fc)
    | ly ← [0..chunkSize-1], lx ← [0..chunkSize-1]
    , ly ≡ 0 ∨ ly ≡ chunkSize-1 ∨ lx ≡ 0 ∨ lx ≡ chunkSize-1
    , Just fc ← [lcFluidMap lc V.! (ly * chunkSize + lx)]
    , fcType fc ≡ River
    ]

sealEdgeTile ∷ ChunkCoord → LoadedChunk
             → (WorldTileData, Bool) → (Int, Int, Int)
             → (WorldTileData, Bool)
sealEdgeTile (ChunkCoord cx cy) _lc (wtd, changed) (lx, ly, waterZ) =
    -- For each outward-facing direction at this edge tile,
    -- check the cross-chunk neighbor
    let dirs = [ (ChunkCoord cx (cy-1), lx, chunkSize-1) | ly ≡ 0 ]
            ⧺ [ (ChunkCoord cx (cy+1), lx, 0)           | ly ≡ chunkSize-1 ]
            ⧺ [ (ChunkCoord (cx-1) cy, chunkSize-1, ly) | lx ≡ 0 ]
            ⧺ [ (ChunkCoord (cx+1) cy, 0, ly)           | lx ≡ chunkSize-1 ]
    in foldl' (trySealNeighbor waterZ) (wtd, changed) dirs

trySealNeighbor ∷ Int → (WorldTileData, Bool) → (ChunkCoord, Int, Int)
                → (WorldTileData, Bool)
trySealNeighbor waterZ (wtd, changed) (nbrCoord, nbrLX, nbrLY) =
    case HM.lookup nbrCoord (wtdChunks wtd) of
        Nothing → (wtd, changed)
        Just nbrLC →
            let nIdx = nbrLY * chunkSize + nbrLX
                nbrFluid = lcFluidMap nbrLC V.! nIdx
                nbrTerrZ = lcTerrainSurfaceMap nbrLC VU.! nIdx
            in case nbrFluid of
                Just _  → (wtd, changed)  -- neighbor already has fluid
                Nothing
                    | nbrTerrZ ≥ waterZ → (wtd, changed)  -- terrain bounds water
                    | nbrTerrZ + 1 ≥ waterZ → (wtd, changed) -- too shallow to seal
                    | waterZ - nbrTerrZ > 5 → (wtd, changed) -- too deep to seal
                    | otherwise →
                        let fillZ = min waterZ (nbrTerrZ + 3)
                        in if fillZ ≤ seaLevel
                           then (wtd, changed)
                           else let newFluid = lcFluidMap nbrLC V.// [(nIdx, Just (FluidCell River fillZ))]
                                    oldSurf = lcSurfaceMap nbrLC VU.! nIdx
                                    newSurf = lcSurfaceMap nbrLC VU.// [(nIdx, max oldSurf fillZ)]
                                    nbrLC' = nbrLC { lcFluidMap = newFluid
                                                   , lcSurfaceMap = newSurf }
                                    -- Propagate inward: the newly-filled tile may
                                    -- expose intra-chunk neighbors too.
                                    nbrLC'' = floodSealInChunk nbrLC' fillZ nbrLX nbrLY
                                    chunks' = HM.insert nbrCoord nbrLC'' (wtdChunks wtd)
                                in (wtd { wtdChunks = chunks' }, True)

-- | After inserting a river tile into a chunk, propagate to any
--   intra-chunk neighbors with terrain below the water surface.
--   Iterates to flood the full carved valley within the chunk.
--   Limits fill depth to terrain+3 to prevent deep water on
--   valley walls, and caps fill surface to neighbor water+1
--   to maintain smooth water gradients.
floodSealInChunk ∷ LoadedChunk → Int → Int → Int → LoadedChunk
floodSealInChunk lc0 startWaterZ startLX startLY =
    let lc1 = go [(startLX, startLY, startWaterZ)] lc0
    in smoothAfterFlood lc1
  where
    go [] lc = lc
    go ((lx, ly, waterZ):queue) lc =
        let newNeighbors =
              [ (nx, ny, fillZ)
              | (nx, ny) ← [ (lx-1,ly), (lx+1,ly), (lx,ly-1), (lx,ly+1) ]
              , nx ≥ 0, nx < chunkSize, ny ≥ 0, ny < chunkSize
              , let nIdx = ny * chunkSize + nx
              , isNothing (lcFluidMap lc V.! nIdx)
              , let nTerrZ = lcTerrainSurfaceMap lc VU.! nIdx
              , nTerrZ + 1 < waterZ
              , waterZ - nTerrZ ≤ 5
              -- Cap fill at terrain+3 to prevent deep water on valley walls
              , let fillZ = min waterZ (nTerrZ + 3)
              , fillZ > seaLevel
              ]
            lc' = foldl' (\acc (nx, ny, fz) →
                let nIdx = ny * chunkSize + nx
                    fm' = lcFluidMap acc V.// [(nIdx, Just (FluidCell River fz))]
                    oldS = lcSurfaceMap acc VU.! nIdx
                    sm' = lcSurfaceMap acc VU.// [(nIdx, max oldS fz)]
                in acc { lcFluidMap = fm', lcSurfaceMap = sm' }
                ) lc newNeighbors
        in go (queue ⧺ newNeighbors) lc'

    -- After flood-filling, smooth water surfaces to prevent jumps
    -- between the flood-filled tiles and existing river tiles.
    smoothAfterFlood lc = smoothLoop (8 ∷ Int) lc

    smoothLoop 0 lc = lc
    smoothLoop n lc =
        let (lc', changed) = smoothFloodPass lc
        in if changed then smoothLoop (n - 1) lc' else lc'

    smoothFloodPass lc =
        foldl' smoothFloodTile (lc, False) [0 .. chunkSize * chunkSize - 1]

    smoothFloodTile (lc, changed) idx =
        case lcFluidMap lc V.! idx of
            Just fc | fcType fc ≡ River →
                let lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    myWater = fcSurface fc
                    terrZ = lcTerrainSurfaceMap lc VU.! idx
                    neighbors = [ (nx, ny)
                                | (nx, ny) ← [(lx,ly-1),(lx,ly+1),(lx-1,ly),(lx+1,ly)]
                                , nx ≥ 0, nx < chunkSize, ny ≥ 0, ny < chunkSize
                                ]
                    nbrSurfaces = [ fcSurface nfc
                                 | (nx, ny) ← neighbors
                                 , Just nfc ← [lcFluidMap lc V.! (ny * chunkSize + nx)]
                                 , fcType nfc ≡ River
                                 ]
                in case nbrSurfaces of
                    [] → (lc, changed)
                    (s:ss) →
                        let nMin = foldl' min s ss
                            target = max (nMin + 1) (terrZ + 1)
                        in if myWater > target
                           then let fm' = lcFluidMap lc V.// [(idx, Just (fc { fcSurface = target }))]
                                    sm' = lcSurfaceMap lc VU.// [(idx, max terrZ target)]
                                    lc' = lc { lcFluidMap = fm', lcSurfaceMap = sm' }
                                in (lc', True)
                           -- Remove if unresolvable cliff
                           else if myWater > nMin + 2 ∧ terrZ + 1 > nMin + 2
                                then let fm' = lcFluidMap lc V.// [(idx, Nothing)]
                                         sm' = lcSurfaceMap lc VU.// [(idx, terrZ)]
                                         lc' = lc { lcFluidMap = fm', lcSurfaceMap = sm' }
                                     in (lc', True)
                                else (lc, changed)
            _ → (lc, changed)
