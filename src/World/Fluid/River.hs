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
        -- Pass 1: fill from segments (deterministic).
        forM_ nearbyRivers $ \pf →
            fillRiverFromFeature mv pf worldSize chunkMinGX chunkMinGY surfaceMap
        -- Pass 2: extend river water one tile into banks.
        extendRiverBanks mv surfaceMap
        -- Pass 3: fill single-tile holes in the river.
        fillRiverHoles mv surfaceMap
        -- Pass 4: clamp water surfaces to terrain + depth cap.
        clampRiverDepth mv surfaceMap
        -- Pass 5: smooth river surfaces to fix overlapping rivers.
        smoothRiverSurface mv surfaceMap
        -- Pass 6: remove tiles with unresolvable cliffs.
        removeUnresolvableCliffs mv surfaceMap

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
                    when (surf > seaLevel ∧ surfZ < surf ∧ surfZ ≥ nTerrZ - 3) $
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

-- | Get minimum adjacent river surface along with the terrain
--   of the neighbor. Used by bank extension to prevent extending
--   water onto valley walls.
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
            if s2 < s1 then (s2, t2) else (s1, t1)) r rs)

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
                    Just surf → when (surf > seaLevel ∧ surfZ < surf) $
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
    let maxDepthInterior = 5   -- keeps water close to terrain surface
        maxDepthEdge = 3       -- tighter cap at chunk edges to limit
                               -- cross-chunk water cliff magnitude
    forM_ [0 .. chunkSize * chunkSize - 1] $ \idx → do
        val ← MV.read mv idx
        case val of
            Just fc | fcType fc ≡ River → do
                let lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    surfZ = surfaceMap VU.! idx
                    isEdge = lx ≡ 0 ∨ lx ≡ chunkSize - 1
                           ∨ ly ≡ 0 ∨ ly ≡ chunkSize - 1
                    maxD = if isEdge then maxDepthEdge else maxDepthInterior
                    -- Floor cap at seaLevel+1 so river water never goes
                    -- below sea level, even on below-sea-level terrain.
                    cap = max (seaLevel + 1) (surfZ + maxD)
                -- Remove river tiles whose water surface is at/below sea level.
                -- Tiles on below-sea-level terrain are fine if the water
                -- surface is above sea level (carved river channel).
                if fcSurface fc ≤ seaLevel
                    then MV.write mv idx Nothing
                    else when (fcSurface fc > cap) $
                        MV.write mv idx (Just (fc { fcSurface = cap }))
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
                            let target = max (nMin + 4) (surfZ + 1)
                            when (myWater > target) $ do
                                MV.write mv idx (Just (fc { fcSurface = target }))
                                MV.write changedRef 0 True
                _ → pure ()
    -- Forward pass
    forM_ [0 .. area - 1] smoothOne
    -- Reverse pass — propagates changes in the other direction
    forM_ [area - 1, area - 2 .. 0] smoothOne
    MV.read changedRef 0

-- | Remove river tiles where water surface is >4 above a river
--   neighbor's water, AND the tile can't be lowered because its
--   terrain is too high. These are valley wall tiles that create
--   visible cliffs. Tolerance of 4 allows natural gradients along
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
                        -- Can't lower below terrain+1, and that's still >4 above neighbor
                        when (myWater > nMin + 4 ∧ surfZ + 1 > nMin + 4) $ do
                            MV.write mv idx Nothing
                            MV.write changedRef 0 True
            _ → pure ()
    MV.read changedRef 0

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
                    Just fc → do
                        existing ← MV.read mv idx
                        case existing of
                            -- Only overwrite if new fill is higher (dominant river wins)
                            Just old | fcType old ≡ River ∧ fcSurface old ≥ fcSurface fc
                                → pure ()
                            _ → MV.write mv idx (Just fc)
        _ → pure ()

-- | At overlapping regions, pick the HIGHER water surface.
--   When multiple rivers overlap, the dominant (higher water level)
--   river takes precedence — it floods the junction area. This also
--   prevents downstream segments from flattening upstream gradients
--   at segment junctions via the overlap margin.
pickBestFill ∷ Maybe FluidCell → Maybe FluidCell → Maybe FluidCell
pickBestFill Nothing b = b
pickBestFill a Nothing = a
pickBestFill (Just a) (Just b) =
    if fcSurface b > fcSurface a then Just b else Just a

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
       -- Small overlap at segment boundaries for gap prevention.
       -- Kept tight (0.05) to avoid bleeding into adjacent segments.
       in if tRaw < -0.05 ∨ tRaw > 1.05
          then Nothing
          else
          let signedPerp = (px * dy' - py * dx') / segLen
              effectivePerpDist = abs signedPerp

              -- Fill zone: half channel width + small bank margin.
              -- rsWidth is total width, so half gives distance from
              -- center to channel edge. Add 2 tiles for banks.
              fillW = fromIntegral (rsWidth seg) / 2.0 + 2.0 ∷ Float
          in if effectivePerpDist > fillW
             then Nothing
             else
             let -- Interpolate water surface along segment axis.
                 tClamped = max 0.0 (min 1.0 tRaw)
                 startW = fromIntegral (rsWaterStart seg) ∷ Float
                 endW   = fromIntegral (rsWaterEnd seg) ∷ Float
                 waterSurface = floor (startW + tClamped * (endW - startW)) ∷ Int
                 -- Reference terrain at this axial position.
                 -- Tiles above this are on the valley wall, not in the channel.
                 startE = fromIntegral (rsStartElev seg) ∷ Float
                 endE   = fromIntegral (rsEndElev seg) ∷ Float
                 refElev = floor (startE + tClamped * (endE - startE)) ∷ Int
             in if waterSurface ≤ seaLevel
                then Nothing  -- water at/below sea level: ocean territory
                else if surfZ ≥ waterSurface
                then Nothing  -- terrain at/above water: no fill
                else if surfZ > refElev
                     then Nothing  -- valley wall: terrain above reference
                     else Just (FluidCell River waterSurface)

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
