{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Fluid.River
    ( computeChunkRivers
    , fixupSegmentContinuity
    , hasAnyRiverQuick
    ) where

import UPrelude
import Data.Word (Word64)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import World.Base
import World.Types
import World.Plate (TectonicPlate(..))
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Fluid.Internal
import World.Constants (seaLevel)
import World.Hydrology.Types (HydroFeature(..), RiverParams(..)
                             , RiverSegment(..))

-----------------------------------------------------------
-- Chunk-Level River Computation
-----------------------------------------------------------

computeChunkRivers ∷ [PersistentFeature] → Word64 → [TectonicPlate]
                   → Int → ChunkCoord
                   → VU.Vector Int
                   → FluidMap
computeChunkRivers features _seed _plates worldSize coord surfaceMap =
    let ChunkCoord cx cy = coord
        chunkMinGX = cx * chunkSize
        chunkMinGY = cy * chunkSize
        nearbyRivers = filter (isNearbyRiver worldSize chunkMinGX chunkMinGY) features
    in withFluidMap $ \mv → do
        forM_ nearbyRivers $ \pf →
            fillRiverFromFeature mv pf worldSize chunkMinGX chunkMinGY surfaceMap
        -- Pass 2: extend river water one tile into banks so the water
        -- surface quad covers the terrain side-face at the waterline.
        -- Same approach as ocean's coastal extension in Ocean.hs.
        extendRiverBanks mv surfaceMap
        -- Pass 3: drain water hanging over cliffs. Only affects
        -- edge tiles (≤2 river neighbors) to avoid creating holes
        -- in the river interior.
        drainCliffWater mv surfaceMap
        -- Pass 4: drain isolated river tiles not connected to a
        -- chunk edge. River water must connect to the chunk boundary
        -- (where it continues in adjacent chunks). Interior-only
        -- pockets are artifacts and get removed.
        drainIsolatedPockets mv
        -- Pass 5: fill single-tile holes in the river. An empty
        -- tile surrounded by river on 3+ sides gets filled.
        fillRiverHoles mv surfaceMap
        -- Pass 6: clamp all river water surfaces to terrain + depth.
        -- Previous passes (extend banks, fill holes) can propagate
        -- inflated water surfaces from high-elevation source segments.
        -- This final pass ensures no river tile has water unreasonably
        -- far above its local terrain.
        clampRiverDepth mv surfaceMap

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
            adj ← adjacentRiverSurface mv lx ly
            case adj of
                Nothing   → pure ()
                Just surf → when (surfZ ≤ surf) $
                    -- Cap: water surface at most a few tiles above terrain
                    let cappedSurf = min surf (surfZ + 4)
                    in MV.write mv idx (Just (FluidCell River cappedSurf))

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

-- | Remove river tiles that hang over cliffs. Only affects edge
--   tiles (with ≤2 river neighbors) to avoid punching holes in
--   the river interior. Checks if adjacent empty terrain drops
--   below the tile's own terrain (cliff face).
drainCliffWater ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int → ST s ()
drainCliffWater mv surfaceMap = go (40 ∷ Int)
  where
    go 0 = pure ()
    go n = do
        changed ← drainCliffPass mv surfaceMap
        when changed $ go (n - 1)

drainCliffPass ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int → ST s Bool
drainCliffPass mv surfaceMap = do
    changedRef ← MV.replicate 1 False
    forM_ [0 .. chunkSize * chunkSize - 1] $ \idx → do
        val ← MV.read mv idx
        case val of
            Just fc | fcType fc ≡ River → do
                let lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    mySurfZ = surfaceMap VU.! idx
                -- Count river neighbors
                nCount ← countRiverNeighbors mv lx ly
                -- Only drain edge tiles (≤2 river neighbors)
                when (nCount ≤ 2) $ do
                    cliff ← hasCliffEdge mv surfaceMap lx ly mySurfZ
                    when cliff $ do
                        MV.write mv idx Nothing
                        MV.write changedRef 0 True
            _ → pure ()
    MV.read changedRef 0

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

-- | Check if a river tile has an adjacent empty tile where terrain
--   drops below the tile's own terrain. This indicates the water
--   is sitting on a cliff edge where it would spill off.
hasCliffEdge ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int
             → Int → Int → Int → ST s Bool
hasCliffEdge mv surfaceMap lx ly mySurfZ = do
    let check nx ny
            | nx < 0 ∨ nx ≥ chunkSize ∨ ny < 0 ∨ ny ≥ chunkSize =
                pure False  -- chunk boundary → assume contained
            | otherwise = do
                let nIdx = ny * chunkSize + nx
                nVal ← MV.read mv nIdx
                case nVal of
                    Just _  → pure False  -- has water → not a cliff edge
                    Nothing →
                        let nSurf = surfaceMap VU.! nIdx
                        in pure (nSurf < mySurfZ - 1)
    n ← check lx (ly - 1)
    e ← check (lx + 1) ly
    s ← check lx (ly + 1)
    w ← check (lx - 1) ly
    pure (n ∨ e ∨ s ∨ w)

-- | Remove river tiles not connected to a chunk edge.
--   River water must reach the chunk boundary (where it continues
--   in adjacent chunks). Interior-only pockets are artifacts.
drainIsolatedPockets ∷ MV.MVector s (Maybe FluidCell) → ST s ()
drainIsolatedPockets mv = do
    visited ← MV.replicate (chunkSize * chunkSize) False
    -- Seed flood fill from river tiles on any chunk edge
    forM_ [0 .. chunkSize - 1] $ \i → do
        -- Top edge (ly=0): idx = i + 0*chunkSize = i
        seedIfRiver mv visited i
        -- Bottom edge (ly=chunkSize-1): idx = i + (chunkSize-1)*chunkSize
        seedIfRiver mv visited (i + (chunkSize - 1) * chunkSize)
        -- Left edge (lx=0): idx = 0 + i*chunkSize
        seedIfRiver mv visited (i * chunkSize)
        -- Right edge (lx=chunkSize-1): idx = (chunkSize-1) + i*chunkSize
        seedIfRiver mv visited ((chunkSize - 1) + i * chunkSize)
    -- Remove unvisited river tiles
    forM_ [0 .. chunkSize * chunkSize - 1] $ \idx → do
        val ← MV.read mv idx
        case val of
            Just fc | fcType fc ≡ River → do
                vis ← MV.read visited idx
                when (not vis) $ MV.write mv idx Nothing
            _ → pure ()

seedIfRiver ∷ MV.MVector s (Maybe FluidCell)
            → MV.MVector s Bool → Int → ST s ()
seedIfRiver mv visited idx = do
    val ← MV.read mv idx
    case val of
        Just fc | fcType fc ≡ River →
            floodFillRiver mv visited idx
        _ → pure ()

-- | BFS flood fill from a seed tile to all connected river tiles.
floodFillRiver ∷ MV.MVector s (Maybe FluidCell)
               → MV.MVector s Bool → Int → ST s ()
floodFillRiver mv visited seedIdx = do
    vis ← MV.read visited seedIdx
    when (not vis) $ do
        MV.write visited seedIdx True
        val ← MV.read mv seedIdx
        case val of
            Just fc | fcType fc ≡ River → do
                let lx = seedIdx `mod` chunkSize
                    ly = seedIdx `div` chunkSize
                    neighbors = filter (\(nx, ny) →
                        nx ≥ 0 ∧ nx < chunkSize ∧ ny ≥ 0 ∧ ny < chunkSize)
                        [(lx, ly-1), (lx+1, ly), (lx, ly+1), (lx-1, ly)]
                forM_ neighbors $ \(nx, ny) → do
                    let nIdx = ny * chunkSize + nx
                    nVis ← MV.read visited nIdx
                    when (not nVis) $ do
                        nVal ← MV.read mv nIdx
                        case nVal of
                            Just nfc | fcType nfc ≡ River →
                                floodFillRiver mv visited nIdx
                            _ → pure ()
            _ → pure ()

-- | Fill single-tile holes in the river. An empty tile surrounded
--   by river water on 3+ sides gets filled at the minimum
--   neighbor water surface. Also removes 1-tile river spots
--   (river tiles with 0 river neighbors).
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
                adj ← adjacentRiverSurface mv lx ly
                case adj of
                    Nothing → pure ()
                    Just surf → when (surfZ ≤ surf) $
                        MV.write mv idx (Just (FluidCell River surf))
    -- Pass B: remove 1-tile spots (0 river neighbors)
    forM_ [0 .. chunkSize * chunkSize - 1] $ \idx → do
        val ← MV.read mv idx
        case val of
            Just fc | fcType fc ≡ River → do
                let lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                nCount ← countRiverNeighbors mv lx ly
                when (nCount ≡ 0) $ MV.write mv idx Nothing
            _ → pure ()

-- | Clamp all river water surfaces so they don't exceed
--   terrain + maxRiverDepth. Post-processing passes like
--   extendRiverBanks and fillRiverHoles can propagate high
--   water surfaces from headwater segments to lower terrain.
clampRiverDepth ∷ MV.MVector s (Maybe FluidCell) → VU.Vector Int → ST s ()
clampRiverDepth mv surfaceMap = do
    let maxWaterAboveTerrain = 6  -- reasonable max depth
    forM_ [0 .. chunkSize * chunkSize - 1] $ \idx → do
        val ← MV.read mv idx
        case val of
            Just fc | fcType fc ≡ River → do
                let surfZ = surfaceMap VU.! idx
                    cap = surfZ + maxWaterAboveTerrain
                when (fcSurface fc > cap) $
                    MV.write mv idx (Just (fc { fcSurface = cap }))
            _ → pure ()

-----------------------------------------------------------
-- River Proximity
-----------------------------------------------------------

isNearbyRiver ∷ Int → Int → Int → PersistentFeature → Bool
isNearbyRiver worldSize chunkGX chunkGY pf =
    case pfFeature pf of
        HydroShape (RiverFeature river) →
            case pfActivity pf of
                FActive  → riverNearChunk worldSize chunkGX chunkGY river
                FDormant → riverNearChunk worldSize chunkGX chunkGY river
                _        → False
        _ → False

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
hasAnyRiverQuick ∷ [PersistentFeature] → Int → ChunkCoord → Bool
hasAnyRiverQuick features worldSize coord =
    let ChunkCoord cx cy = coord
        chunkMinGX = cx * chunkSize
        chunkMinGY = cy * chunkSize
    in any (isNearbyRiver worldSize chunkMinGX chunkMinGY) features

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

fillRiverFromFeature ∷ MV.MVector s (Maybe FluidCell)
                     → PersistentFeature → Int → Int → Int
                     → VU.Vector Int
                     → ST s ()
fillRiverFromFeature mv pf worldSize chunkGX chunkGY surfaceMap =
    case pfFeature pf of
        HydroShape (RiverFeature river) →
            let segments = rpSegments river
            in forEachSurface surfaceMap $ \idx lx ly surfZ →
                case bestRiverFill worldSize (chunkGX + lx) (chunkGY + ly)
                                   surfZ segments of
                    Nothing → pure ()
                    Just fc → MV.write mv idx (Just fc)
        _ → pure ()

-- | At overlapping regions, pick the LOWER water surface.
--   With interpolated surfaces and segment continuity, overlapping
--   segments produce matching surfaces at junctions. Picking lower
--   is physically correct: water seeks its lowest level.
pickBestFill ∷ Maybe FluidCell → Maybe FluidCell → Maybe FluidCell
pickBestFill Nothing b = b
pickBestFill a Nothing = a
pickBestFill (Just a) (Just b) =
    if fcSurface b < fcSurface a then Just b else Just a

bestRiverFill ∷ Int → Int → Int → Int → V.Vector RiverSegment
              → Maybe FluidCell
bestRiverFill worldSize gx gy surfZ segments =
    let segResults = V.toList $ V.map
            (riverFillFromSegment worldSize gx gy surfZ) segments
    in foldl' pickBestFill Nothing segResults

-- | Fill a tile from a segment using the PRECOMPUTED water surface.
--   Water surface is interpolated from rsWaterStart to rsWaterEnd,
--   which are set during world generation (and corrected post-timeline
--   to match actual terrain). This decouples fill from carving references.
riverFillFromSegment ∷ Int → Int → Int → Int → RiverSegment
                     → Maybe FluidCell
riverFillFromSegment worldSize gx gy surfZ seg =
    let GeoCoord sx sy = rsStart seg
        GeoCoord ex ey = rsEnd seg
        (dxi, dyi) = wrappedDeltaUVFluid worldSize sx sy ex ey
        dx' = fromIntegral dxi ∷ Float
        dy' = fromIntegral dyi ∷ Float
        segLen2 = dx' * dx' + dy' * dy'
    in if segLen2 < 1.0
       then Nothing
       else
       let segLen = sqrt segLen2
           (pxi, pyi) = wrappedDeltaUVFluid worldSize sx sy gx gy
           px = fromIntegral pxi ∷ Float
           py = fromIntegral pyi ∷ Float
           tRaw = (px * dx' + py * dy') / segLen2
       -- Modest overlap at segment boundaries for continuity
       in if tRaw < -0.15 ∨ tRaw > 1.15
          then Nothing
          else
          let signedPerp = (px * dy' - py * dx') / segLen
              effectivePerpDist = abs signedPerp

              -- Fill zone: valley width (the carved area).
              -- No margin — surfZ check prevents fill on terrain
              -- above water level. The valley IS the river bed.
              fillW = fromIntegral (rsValleyWidth seg) / 2.0 ∷ Float
          in if effectivePerpDist > fillW
             then Nothing
             else
             let -- Interpolate precomputed water surface along segment.
                 tClamped = max 0.0 (min 1.0 tRaw)
                 startW = fromIntegral (rsWaterStart seg) ∷ Float
                 endW   = fromIntegral (rsWaterEnd seg) ∷ Float
                 waterSurface = floor (startW + tClamped * (endW - startW)) ∷ Int
                 -- Clamp to sea level at ocean interface
                 clampedSurface = if waterSurface ≤ seaLevel
                     then seaLevel
                     else waterSurface
                 -- Cap water depth above local terrain. The interpolated
                 -- surface can be much higher than the terrain on valley
                 -- slopes (headwater segments have high water at the
                 -- source that drops steeply). Capping to terrain + depth
                 -- keeps water in the carved channel.
                 depthCap = rsDepth seg + 2
                 finalSurface = min clampedSurface (surfZ + depthCap)
             in if surfZ > clampedSurface
                then Nothing
                else Just (FluidCell River finalSurface)

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
