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
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef')
import World.Base
import World.Types
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Fluid.Internal
import World.Constants (seaLevel)
import World.Hydrology.Types (RiverParams(..), RiverSegment(..))

-- * Chunk-Level River Computation
--
-- The fill uses valley width (not a conservative sub-width) so water
-- reaches every carved tile. The carved terrain provides the depth
-- profile — no artificial depth taper. Water surface is interpolated
-- from segment endpoint elevations with no freeboard, so every tile
-- carved below refElev gets water.
--
-- Pipeline (5 passes, down from 15):
--   1. Segment fill (valley-width, simplified)
--   2. Hole fill (1-tile gaps at segment junctions)
--   3. Smooth (bidirectional, 3 iterations)
--   4. Clamp + disconnect check (safety net)
--   5. Final hole fill after disconnect removal

computeChunkRivers ∷ [RiverParams] → Int → ChunkCoord
                   → VU.Vector Int
                   → FluidMap
computeChunkRivers rivers worldSize coord surfaceMap =
    let ChunkCoord cx cy = coord
        chunkMinGX = cx * chunkSize
        chunkMinGY = cy * chunkSize
        nearbyRivers = filter (riverNearChunk worldSize chunkMinGX chunkMinGY) rivers
    in withFluidMap $ \mv → do
        -- Pass 1: Segment fill (widened to valley edge)
        forM_ nearbyRivers $ \river →
            fillRiverDirect mv river worldSize chunkMinGX chunkMinGY surfaceMap
        -- Pass 2: Fill single-tile holes at segment junctions.
        fillRiverHoles mv surfaceMap
        -- Pass 3: Containment — lower water at cliff edges where it would
        -- float over terrain. Propagates inward so connected tiles also lower.
        containWaterAtCliffs mv surfaceMap
        -- Pass 4: Smooth river surfaces (handles segment overlap artifacts).
        smoothRiverSurface mv surfaceMap
        -- Pass 5: Clamp depth (safety net) + remove disconnected water.
        clampRiverDepth mv surfaceMap
        removeDisconnectedWater mv surfaceMap
        -- Pass 6: Fill holes created by disconnect removal.
        fillRiverHoles mv surfaceMap
        pure ()

-- * Containment (cliff edge lowering)

-- | Lower water surfaces at cliff edges where water would float
--   over terrain. A water tile has a "cliff" when a dry cardinal
--   neighbor's terrain is far below the water surface. The water
--   should lower to the cliff edge, not float.
--
--   Algorithm: find the lowest dry neighbor terrain for each water-
--   edge tile. If water surface > dryTerrain + 3, lower water to
--   dryTerrain + 1 (thin layer at the edge). Then propagate inward:
--   tiles whose water is now much higher than their lowered neighbor
--   get lowered too. Iterate until stable.
containWaterAtCliffs ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int → ST s ()
containWaterAtCliffs mv surfaceMap = go (8 ∷ Int)
  where
    go 0 = pure ()
    go n = do
        changed ← containPass mv surfaceMap
        when changed $ go (n - 1)

containPass ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int → ST s Bool
containPass mv surfaceMap = do
    changedRef ← newSTRef False
    let area = chunkSize * chunkSize
    forM_ [0 .. area - 1] $ \idx → do
        val ← MV.read mv idx
        case val of
            Just fc | fcType fc ≡ River → do
                let lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    myWater = fcSurface fc
                    surfZ = surfaceMap VU.! idx
                -- Find lowest terrain among dry cardinal neighbors.
                -- Edge tiles (at chunk boundary) skip this — they may
                -- connect to water in adjacent chunks.
                let isEdge = lx ≡ 0 ∨ lx ≡ chunkSize - 1
                           ∨ ly ≡ 0 ∨ ly ≡ chunkSize - 1
                if isEdge then pure ()
                else do
                    lowestDry ← lowestDryNeighborTerrain mv surfaceMap lx ly
                    case lowestDry of
                        Nothing → pure ()  -- no dry neighbors (interior tile)
                        Just dryTerrZ → do
                            -- If water is floating >5 above dry terrain,
                            -- lower to dry terrain + 1.
                            let cliff = myWater - dryTerrZ
                            when (cliff > 5) $ do
                                let target = max (surfZ + 1) (dryTerrZ + 1)
                                when (target < myWater) $ do
                                    MV.write mv idx (Just (fc { fcSurface = target }))
                                    writeSTRef changedRef True
                -- Also check: if any WATER neighbor was lowered and is now
                -- much lower than us, we should lower too (inward propagation).
                minNbr ← minRiverNeighborSurface mv lx ly
                case minNbr of
                    Nothing → pure ()
                    Just nMin → do
                        -- If we're >5 above our lowest water neighbor,
                        -- lower toward them (but not below terrain+1).
                        when (myWater > nMin + 5) $ do
                            let target = max (surfZ + 1) (nMin + 3)
                            when (target < myWater) $ do
                                val' ← MV.read mv idx
                                case val' of
                                    Just fc' | fcType fc' ≡ River →
                                        when (target < fcSurface fc') $ do
                                            MV.write mv idx (Just (fc' { fcSurface = target }))
                                            writeSTRef changedRef True
                                    _ → pure ()
            _ → pure ()
    readSTRef changedRef

-- | Find the lowest terrain elevation among dry cardinal neighbors.
--   Returns Nothing if all neighbors have water.
lowestDryNeighborTerrain ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int
                         → Int → Int → ST s (Maybe Int)
lowestDryNeighborTerrain mv surfaceMap lx ly = do
    let check nx ny
          | nx < 0 ∨ nx ≥ chunkSize ∨ ny < 0 ∨ ny ≥ chunkSize = pure Nothing
          | otherwise = do
              v ← MV.read mv (ny * chunkSize + nx)
              pure $ case v of
                  Just _  → Nothing  -- has water, not dry
                  Nothing → Just (surfaceMap VU.! (ny * chunkSize + nx))
    vN ← check lx (ly - 1)
    vS ← check lx (ly + 1)
    vE ← check (lx + 1) ly
    vW ← check (lx - 1) ly
    let vals = catMaybes [vN, vS, vE, vW]
    pure $ case vals of
        []     → Nothing
        (v:vs) → Just (foldl' min v vs)

-- * Hole Filling

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
            when (nCount ≥ 2) $ do
                maxSurf ← maxRiverNeighborSurface mv lx ly
                minSurf ← minRiverNeighborSurface mv lx ly
                case (maxSurf, minSurf) of
                    (Just nMax, Just nMin) → do
                        let standard = nCount ≥ 3 ∧ surfZ ≤ nMax
                                     ∧ nMax - surfZ ≤ 5 ∧ nMax > seaLevel
                            uniform = nMin ≡ nMax ∧ nCount ≥ 3
                                    ∧ surfZ ≤ nMax + 2 ∧ nMax > seaLevel
                            bridge = nCount ≡ 2 ∧ nMax - nMin ≤ 2
                                   ∧ nMin > surfZ ∧ nMin > seaLevel
                        when (standard ∨ uniform ∨ bridge) $ do
                            let fillZ | standard ∨ bridge = min nMax (surfZ + 3)
                                      | otherwise         = nMax
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

-- * Depth Clamping

-- | Clamp all river water surfaces so they don't exceed
--   terrain + maxRiverDepth. Safety net for any edge cases
--   where the fill placed water too deep.
clampRiverDepth ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int → ST s ()
clampRiverDepth mv surfaceMap = do
    let maxDepthValley = 12 ∷ Int
        maxDepthHill   = 8 ∷ Int
    forM_ [0 .. chunkSize * chunkSize - 1] $ \idx → do
        val ← MV.read mv idx
        case val of
            Just fc | fcType fc ≡ River → do
                let lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    surfZ = surfaceMap VU.! idx
                    isEdge = lx ≡ 0 ∨ lx ≡ chunkSize - 1
                           ∨ ly ≡ 0 ∨ ly ≡ chunkSize - 1
                avgNbr ← avgNeighborTerrain surfaceMap lx ly
                let inValley = surfZ < avgNbr - 1
                    maxD = if isEdge ∨ inValley then maxDepthValley else maxDepthHill
                    cap = surfZ + maxD
                if fcSurface fc - surfZ > maxD
                    then if cap ≤ surfZ
                         then MV.write mv idx Nothing
                         else MV.write mv idx (Just (fc { fcSurface = cap }))
                    else pure ()
            _ → pure ()

-- * Surface Smoothing

-- | Surface smoothing. Lowers any river tile whose water is more than
--   2 above its lowest river neighbor. Floor at terrain + 1.
smoothRiverSurface ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int → ST s ()
smoothRiverSurface mv surfaceMap = go (3 ∷ Int)
  where
    go 0 = pure ()
    go n = do
        changed ← smoothPass mv surfaceMap
        when changed $ go (n - 1)

smoothPass ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int → ST s Bool
smoothPass mv surfaceMap = do
    changedRef ← newSTRef False
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
                            let target = max (nMin + 3) (surfZ + 1)
                            when (myWater > target) $ do
                                MV.write mv idx (Just (fc { fcSurface = target }))
                                writeSTRef changedRef True
                _ → pure ()
    -- Forward pass
    forM_ [0 .. area - 1] smoothOne
    -- Reverse pass — propagates changes in the other direction
    forM_ [area - 1, area - 2 .. 0] smoothOne
    readSTRef changedRef

-- * Disconnected Water Removal

-- | Remove water tiles that can't reach a drainage point.
--   Flood-fills from "drain seeds" (water tiles on chunk edges, or
--   at/below seaLevel) through connected water, marking reachable
--   tiles. Any river water tile NOT reached is removed.
removeDisconnectedWater ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int → ST s ()
removeDisconnectedWater mv _surfaceMap = do
    let area = chunkSize * chunkSize
    reached ← MV.replicate area (0 ∷ Int)
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
    let bfs [] = pure ()
        bfs (idx : queue) = do
            val ← MV.read mv idx
            case val of
                Just _fc → do
                    let lx = idx `mod` chunkSize
                        ly = idx `div` chunkSize
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

-- * Neighbor Helpers

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

-- * River Proximity

-- | Check if any part of the river is near this chunk.
riverNearChunk ∷ Int → Int → Int → RiverParams → Bool
riverNearChunk worldSize chunkGX chunkGY river =
    V.any (segOrWaypointNear worldSize chunkGX chunkGY) (rpSegments river)
  where
    segOrWaypointNear ws cgx cgy seg =
        let margin = rsValleyWidth seg + chunkSize + chunkSize
            cx = cgx + chunkSize `div` 2
            cy = cgy + chunkSize `div` 2
            GeoCoord sx sy = rsStart seg
            (dxsi, dysi) = wrappedDeltaUVFluid ws sx sy cx cy
            startNear = abs dxsi < margin ∧ abs dysi < margin
            GeoCoord ex ey = rsEnd seg
            (dxei, dyei) = wrappedDeltaUVFluid ws ex ey cx cy
            endNear = abs dxei < margin ∧ abs dyei < margin
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

-- * River Fluid Fill (interpolated water surface)
--
-- Water surface is INTERPOLATED along each segment, smoothly
-- decreasing from start to end elevation. With freeboard=0, the
-- water surface equals the reference terrain elevation. The carved
-- terrain provides the natural depth profile — deep in the channel
-- center, shallow on valley walls, zero at the valley rim.

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
                    Just old | fcType old ≡ River →
                        when (fcSurface fc < fcSurface old) $
                            MV.write mv idx (Just fc)
                    _ → MV.write mv idx (Just fc)

-- | At overlapping regions, pick the fill from the CLOSEST segment
--   (smallest perpendicular distance).
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

-- | Fill a tile from a segment. Water surface is interpolated from
--   rsWaterStart to rsWaterEnd (= reference terrain with no freeboard).
--   Fill zone extends to valley edge. The carved terrain provides the
--   depth profile — no artificial perpendicular taper.
--
--   A generous depth cap (rsDepth + 4) prevents excessive water in
--   deep natural depressions that happen to be near the river. This
--   cap never activates on properly carved terrain.
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
       in if tRaw < -0.05 ∨ tRaw > 1.05
          then (Nothing, 9999.0)
          else
          let signedPerp = (px * dy' - py * dx') / segLen
              effectivePerpDist = abs signedPerp

              -- Fill zone: extends to the full valley edge.
              -- The carved terrain provides the depth profile.
              valleyEdge = fromIntegral (rsValleyWidth seg) / 2.0 ∷ Float

          in if effectivePerpDist > valleyEdge
             then (Nothing, effectivePerpDist)
             else
             let -- Interpolate water surface along segment axis.
                 tClamped = max 0.0 (min 1.0 tRaw)
                 startW = fromIntegral (rsWaterStart seg) ∷ Float
                 endW   = fromIntegral (rsWaterEnd seg) ∷ Float
                 axialWaterSurface = floor (startW + tClamped * (endW - startW)) ∷ Int

                 -- Coastal flattening: when a segment ends at or below sea
                 -- level, blend toward seaLevel for smooth estuaries.
                 coastalFlat = seaLevel
                 flattenedWater
                   | rsEndElev seg ≤ seaLevel ∧ axialWaterSurface > coastalFlat =
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

                 -- Water surface is the axial interpolation directly.
                 -- No terrain-dependent cap — the valley/channel checks
                 -- below constrain placement, and clampRiverDepth provides
                 -- a backstop. This keeps the water surface uniform across
                 -- the river width (flat, like real water).
                 maxFillDepth = rsDepth seg + 4
                 waterSurface = flattenedWater

                 -- "In valley" = terrain at or below reference elevation
                 -- AND within the expected depth range. The upper bound
                 -- (≤ refElev) excludes valley walls. The lower bound
                 -- (≥ refElev - maxFillDepth) excludes terrain that was
                 -- naturally eroded far below the river — such terrain
                 -- isn't part of the carved valley and shouldn't get water.
                 inValley = surfZ ≤ refElev
                          ∧ surfZ ≥ refElev - maxFillDepth

                 -- Channel center tiles: within channel width + 1 margin.
                 channelHalfW = fromIntegral (rsWidth seg) / 2.0 ∷ Float
                 inChannel = effectivePerpDist ≤ channelHalfW + 1.0
                 inChannelRelaxed = inChannel ∧ surfZ ≤ refElev + 2
                                  ∧ surfZ ≥ refElev - maxFillDepth

                 shouldPlace = (inValley ∨ inChannelRelaxed)
                             ∧ waterSurface > surfZ

             in if not shouldPlace
                then (Nothing, effectivePerpDist)
                else (Just (FluidCell River waterSurface), effectivePerpDist)

-- * Segment Continuity

-- | Ensure adjacent segments share consistent endpoint elevations.
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

-- * Cross-Chunk River Boundary Handling
--
-- Simplified from 6 phases to 2:
--   1. Equalize water surfaces across chunk boundaries
--   2. Fill 1-tile gaps at chunk edges

sealCrossChunkRivers ∷ [ChunkCoord] → WorldTileData → WorldTileData
sealCrossChunkRivers newCoords wtd =
    let allCoords = let nbrSet = HM.fromList
                          [ (c, ())
                          | coord ← newCoords
                          , c ← coord : chunkNeighbors' coord
                          , HM.member c (wtdChunks wtd)
                          ]
                    in HM.keys nbrSet
        -- Phase 1: equalize water surfaces across chunk boundaries.
        equalized = equalizeCrossChunkRivers allCoords wtd
        -- Phase 2: fill 1-tile gaps at chunk edges.
        filled = fillCrossChunkHoles allCoords equalized
    in filled

chunkNeighbors' ∷ ChunkCoord → [ChunkCoord]
chunkNeighbors' (ChunkCoord cx cy) =
    [ ChunkCoord (cx-1) cy, ChunkCoord (cx+1) cy
    , ChunkCoord cx (cy-1), ChunkCoord cx (cy+1) ]

-- | Equalize river water surfaces across chunk boundaries.
--   For each pair of adjacent edge river tiles in different chunks,
--   lower the higher water surface toward the lower one.
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

    tryEqualize myCoord myLX myLY _myWaterZ (td, changed) (nbrCoord, nbrLX, nbrLY) =
        let myIdx = myLY * chunkSize + myLX
        in case HM.lookup myCoord (wtdChunks td) of
            Nothing → (td, changed)
            Just myLC →
                case lcFluidMap myLC V.! myIdx of
                    Just myFC | fcType myFC ≡ River →
                        let curWaterZ = fcSurface myFC
                        in case HM.lookup nbrCoord (wtdChunks td) of
                            Nothing → (td, changed)
                            Just nbrLC →
                                let nIdx = nbrLY * chunkSize + nbrLX
                                    nbrFluid = lcFluidMap nbrLC V.! nIdx
                                in case nbrFluid of
                                    Just nbrFC | fcType nbrFC ≡ River →
                                        let nbrWaterZ = fcSurface nbrFC
                                            diff = abs (curWaterZ - nbrWaterZ)
                                        in if diff ≤ 1
                                           then (td, changed)
                                           else if curWaterZ > nbrWaterZ
                                           then
                                             -- Lower our side to within 1 of neighbor.
                                             let myTerrZ = lcTerrainSurfaceMap myLC VU.! myIdx
                                                 target = max (nbrWaterZ + 1) (myTerrZ + 1)
                                             in if target < curWaterZ
                                                then let fm' = lcFluidMap myLC V.// [(myIdx, Just (FluidCell River target))]
                                                         sm' = lcSurfaceMap myLC VU.// [(myIdx, max myTerrZ target)]
                                                         myLC' = myLC { lcFluidMap = fm', lcSurfaceMap = sm' }
                                                         chunks' = HM.insert myCoord myLC' (wtdChunks td)
                                                     in (td { wtdChunks = chunks' }, True)
                                                else (td, changed)
                                           else (td, changed)
                                    _ → (td, changed)
                    _ → (td, changed)

-- | Collect all river tiles on chunk edges.
edgeRiverTiles ∷ LoadedChunk → [(Int, Int, Int)]
edgeRiverTiles lc =
    [ (lx, ly, fcSurface fc)
    | ly ← [0..chunkSize-1], lx ← [0..chunkSize-1]
    , ly ≡ 0 ∨ ly ≡ chunkSize-1 ∨ lx ≡ 0 ∨ lx ≡ chunkSize-1
    , Just fc ← [lcFluidMap lc V.! (ly * chunkSize + lx)]
    , fcType fc ≡ River
    ]

-- | Fill single-tile holes in affected chunks after cross-chunk equalization.
fillCrossChunkHoles ∷ [ChunkCoord] → WorldTileData → WorldTileData
fillCrossChunkHoles coords wtd = foldl' fillChunkHoles wtd coords
  where
    fillChunkHoles td coord =
        case HM.lookup coord (wtdChunks td) of
            Nothing → td
            Just lc →
                let lc' = holeFillLoop (2 ∷ Int) coord td lc
                in if lcFluidMap lc' ≡ lcFluidMap lc
                   then td
                   else let chunks' = HM.insert coord lc' (wtdChunks td)
                        in td { wtdChunks = chunks' }

    holeFillLoop 0 _     _  lc = lc
    holeFillLoop n coord td lc =
        let (lc', changed') = holeFillPass coord td lc
        in if changed' then holeFillLoop (n - 1) coord td lc' else lc'

    holeFillPass coord td lc =
        foldl' (fillHoleTile coord td) (lc, False) [0 .. chunkSize * chunkSize - 1]

    fillHoleTile coord td (lc, changed) idx =
        case lcFluidMap lc V.! idx of
            Just _ → (lc, changed)
            Nothing →
                let lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    terrZ = lcTerrainSurfaceMap lc VU.! idx
                    isEdge = lx ≡ 0 ∨ lx ≡ chunkSize - 1
                           ∨ ly ≡ 0 ∨ ly ≡ chunkSize - 1
                    inChunkNeighbors = [ (nx, ny)
                                       | (nx, ny) ← [(lx,ly-1),(lx,ly+1),(lx-1,ly),(lx+1,ly)]
                                       , nx ≥ 0, nx < chunkSize, ny ≥ 0, ny < chunkSize
                                       ]
                    riverNbrs = [ fcSurface nfc
                                | (nx, ny) ← inChunkNeighbors
                                , Just nfc ← [lcFluidMap lc V.! (ny * chunkSize + nx)]
                                , fcType nfc ≡ River
                                ]
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
                            threshold = if isEdge then 2 else 3
                            standard = nCount ≥ threshold ∧ terrZ ≤ nMax
                                     ∧ nMax - terrZ ≤ 5 ∧ nMax > seaLevel
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

    crossChunkRiver td nbrCoord lx ly =
        case HM.lookup nbrCoord (wtdChunks td) of
            Nothing → Nothing
            Just nbrLC →
                let nIdx = ly * chunkSize + lx
                in case lcFluidMap nbrLC V.! nIdx of
                    Just nfc | fcType nfc ≡ River → Just (fcSurface nfc)
                    _ → Nothing
