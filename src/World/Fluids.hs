{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Fluids
    ( -- * Types
      FluidType(..)
    , FluidCell(..)
    , OceanMap
      -- * Constants
    , seaLevel
    , regionSize
      -- * Region
    , RegionCoord(..)
    , Region(..)
    , chunkToRegion
      -- * Ocean flood fill
    , computeOceanMap
      -- * Chunk-level fluid
    , computeChunkFluid
    , computeChunkLava
    , computeChunkLakes
    , computeChunkRivers
    , unionFluidMap
    , fixupSegmentContinuity
      -- * Query
    , isOceanChunk
    , hasAnyLavaQuick
    , hasAnyOceanFluid
    ) where

import UPrelude
import Data.Word (Word64)
import qualified Data.HashSet as HS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Hashable (Hashable(..))
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import World.Base
import World.Types
import World.Material (MaterialId(..), matGlacier)
import World.Plate (TectonicPlate(..), generatePlates, elevationAtGlobal
                   , isBeyondGlacier, wrapGlobalU)
import World.Geology.Evolution (getFeatureCenter, getFeatureRadius)
import World.Hydrology.Types (HydroFeature(..), LakeParams(..)
                             , RiverParams(..), RiverSegment(..))

type FluidMap = V.Vector (Maybe FluidCell)

-----------------------------------------------------------
-- Helpers (Vector builders)
-----------------------------------------------------------

emptyFluidMap ∷ FluidMap
emptyFluidMap = V.replicate (chunkSize * chunkSize) Nothing

withFluidMap ∷ (forall s. MV.MVector s (Maybe FluidCell) → ST s ()) → FluidMap
withFluidMap action = runST $ do
    mv ← MV.replicate (chunkSize * chunkSize) Nothing
    action mv
    V.freeze mv

forEachSurface ∷ VU.Vector Int → (Int → Int → Int → Int → ST s ()) → ST s ()
forEachSurface surfaceMap f =
    forM_ [0 .. chunkSize * chunkSize - 1] $ \idx → do
        let lx = idx `mod` chunkSize
            ly = idx `div` chunkSize
            surfZ = surfaceMap VU.! idx
        f idx lx ly surfZ

-----------------------------------------------------------
-- Ocean Flood Fill
-----------------------------------------------------------

computeOceanMap ∷ Word64 → Int → Int → [TectonicPlate]
               → (Int → Int → (Int, MaterialId) → (Int, MaterialId))
               → OceanMap
computeOceanMap seed worldSize plateCount plates applyTL =
    let halfSize = worldSize `div` 2

        chunkElev ∷ ChunkCoord → Int
        chunkElev (ChunkCoord cx cy) =
            let midGX = cx * chunkSize + chunkSize `div` 2
                midGY = cy * chunkSize + chunkSize `div` 2
                (gx', gy') = wrapGlobalU worldSize midGX midGY
            in if isBeyondGlacier worldSize gx' gy'
               then seaLevel + 100
               else let (baseElev, baseMat) = elevationAtGlobal seed plates worldSize gx' gy'
                    in if baseMat ≡ matGlacier
                       then seaLevel + 100
                       else fst (applyTL gx' gy' (baseElev, baseMat))

        oceanSeeds = concatMap (\plate →
            if plateIsLand plate
            then []
            else let cx = floorDiv' (plateCenterX plate) chunkSize
                     cy = floorDiv' (plateCenterY plate) chunkSize
                     coord = ChunkCoord cx cy
                 in if cx ≥ -halfSize ∧ cx < halfSize
                     ∧ cy ≥ -halfSize ∧ cy < halfSize
                     ∧ chunkElev coord < seaLevel
                    then [coord]
                    else []
            ) plates

        -- Wrap chunk coords in u-space (consistent with the isometric world)
        wrapChunkU (ccx, ccy) =
            let w = halfSize * 2
                u = ccx - ccy
                v = ccx + ccy
                halfW = w `div` 2
                wrappedU = ((u + halfW) `mod` w + w) `mod` w - halfW
                cx' = (wrappedU + v) `div` 2
                cy' = (v - wrappedU) `div` 2
            in (cx', cy')

        neighbors (ChunkCoord cx cy) =
            [ let (cx', cy') = wrapChunkU (cx + dx, cy + dy)
              in ChunkCoord cx' cy'
            | (dx, dy) ← [(-1,0), (1,0), (0,-1), (0,1)]
            , let (_, cy') = wrapChunkU (cx + dx, cy + dy)
            , cy' ≥ -halfSize ∧ cy' < halfSize
            ]

        bfs ∷ Seq ChunkCoord → HS.HashSet ChunkCoord → HS.HashSet ChunkCoord
        bfs Empty visited = visited
        bfs (current :<| queue) visited =
            let nextNeighbors = filter (\n →
                    not (HS.member n visited)
                    ∧ chunkElev n < seaLevel
                    ) (neighbors current)
                visited' = foldl' (flip HS.insert) visited nextNeighbors
                queue' = foldl' (:|>) queue nextNeighbors
            in bfs queue' visited'

        initialVisited = HS.fromList oceanSeeds
        initialQueue = Seq.fromList oceanSeeds

    in bfs initialQueue initialVisited

isOceanChunk ∷ OceanMap → ChunkCoord → Bool
isOceanChunk = flip HS.member

-----------------------------------------------------------
-- Chunk-Level Fluid Computation
-----------------------------------------------------------
-- helper
wrapChunkCoordU ∷ Int → ChunkCoord → ChunkCoord
wrapChunkCoordU worldSize (ChunkCoord cx cy) =
    let halfSize = worldSize `div` 2
        w = halfSize * 2
        u = cx - cy
        v = cx + cy
        halfW = w `div` 2
        wrappedU = ((u + halfW) `mod` w + w) `mod` w - halfW
    in ChunkCoord ((wrappedU + v) `div` 2) ((v - wrappedU) `div` 2)

computeChunkFluid ∷ Int → OceanMap → ChunkCoord
                  → VU.Vector Int
                  → FluidMap
computeChunkFluid worldSize oceanMap coord surfaceMap
    | isOceanChunk oceanMap coord =
        buildOceanSurface surfaceMap
    | hasOceanNeighbor =
        buildOceanSurface surfaceMap
    | otherwise = emptyFluidMap
  where
    ChunkCoord cx cy = coord
    wrap = wrapChunkCoordU worldSize
    hasOceanNeighbor =
        isOceanChunk oceanMap (wrap (ChunkCoord cx (cy - 1)))
      ∨ isOceanChunk oceanMap (wrap (ChunkCoord cx (cy + 1)))
      ∨ isOceanChunk oceanMap (wrap (ChunkCoord (cx + 1) cy))
      ∨ isOceanChunk oceanMap (wrap (ChunkCoord (cx - 1) cy))
      ∨ isOceanChunk oceanMap (wrap (ChunkCoord (cx + 1) (cy - 1)))
      ∨ isOceanChunk oceanMap (wrap (ChunkCoord (cx - 1) (cy - 1)))
      ∨ isOceanChunk oceanMap (wrap (ChunkCoord (cx + 1) (cy + 1)))
      ∨ isOceanChunk oceanMap (wrap (ChunkCoord (cx - 1) (cy + 1)))

    buildOceanSurface surf =
        withFluidMap $ \mv →
            forEachSurface surf $ \idx _lx _ly surfZ →
                when (surfZ < seaLevel) $
                    MV.write mv idx (Just (FluidCell Ocean seaLevel))

computeChunkLava ∷ [PersistentFeature] → Word64 → [TectonicPlate]
                 → Int → ChunkCoord
                 → VU.Vector Int
                 → FluidMap
computeChunkLava features seed plates worldSize coord surfaceMap =
    let ChunkCoord cx cy = coord
        chunkMinGX = cx * chunkSize
        chunkMinGY = cy * chunkSize
        nearbyActive = filter (isNearbyActive worldSize chunkMinGX chunkMinGY) features
    in withFluidMap $ \mv →
        forM_ nearbyActive $ \pf →
            fillLavaFromFeature mv pf seed plates worldSize chunkMinGX chunkMinGY surfaceMap

computeChunkLakes ∷ [PersistentFeature] → Word64 → [TectonicPlate]
                  → Int → ChunkCoord
                  → VU.Vector Int
                  → FluidMap
computeChunkLakes features seed plates worldSize coord surfaceMap =
    let ChunkCoord cx cy = coord
        chunkMinGX = cx * chunkSize
        chunkMinGY = cy * chunkSize
        nearbyLakes = filter (isNearbyLake worldSize chunkMinGX chunkMinGY) features
    in withFluidMap $ \mv →
        forM_ nearbyLakes $ \pf →
            fillLakeFromFeature mv pf seed plates worldSize chunkMinGX chunkMinGY surfaceMap

computeChunkRivers ∷ [PersistentFeature] → Word64 → [TectonicPlate]
                   → Int → ChunkCoord
                   → VU.Vector Int
                   → FluidMap
computeChunkRivers features _seed _plates worldSize coord surfaceMap =
    let ChunkCoord cx cy = coord
        chunkMinGX = cx * chunkSize
        chunkMinGY = cy * chunkSize
        nearbyRivers = filter (isNearbyRiver worldSize chunkMinGX chunkMinGY) features
    in withFluidMap $ \mv →
        forM_ nearbyRivers $ \pf →
            fillRiverFromFeature mv pf worldSize chunkMinGX chunkMinGY surfaceMap

-----------------------------------------------------------
-- Fluid Map Union
-----------------------------------------------------------

-- | joins all fluid maps, overwrite priority occurs here
unionFluidMap ∷ FluidMap → FluidMap → FluidMap
unionFluidMap = V.zipWith (\a b → case a of
    Just _  → a
    Nothing → b
  )


-----------------------------------------------------------
-- River Fluid Fill
-----------------------------------------------------------

fillRiverFromFeature ∷ MV.MVector s (Maybe FluidCell)
                     → PersistentFeature → Int → Int → Int
                     → VU.Vector Int
                     → ST s ()
fillRiverFromFeature mv pf worldSize chunkGX chunkGY surfaceMap =
    case pfFeature pf of
        HydroShape (RiverFeature river) →
            let segments = rpSegments river
                meanderSeed = rpMeanderSeed river
            in forEachSurface surfaceMap $ \idx lx ly surfZ →
                case bestRiverFill worldSize (chunkGX + lx) (chunkGY + ly)
                                   surfZ meanderSeed segments of
                    Nothing → pure ()
                    Just fc → MV.write mv idx (Just fc)
        _ → pure ()

-- | At overlapping regions, pick the higher water surface.
--   A tile might be claimed by two segments — we want the
--   one that actually covers this tile with more water.
pickBestFill ∷ Maybe FluidCell → Maybe FluidCell → Maybe FluidCell
pickBestFill Nothing b = b
pickBestFill a Nothing = a
pickBestFill (Just a) (Just b) =
    if fcSurface b > fcSurface a then Just b else Just a

bestRiverFill ∷ Int → Int → Int → Int → Word64 → V.Vector RiverSegment
              → Maybe FluidCell
bestRiverFill worldSize gx gy surfZ meanderSeed segments =
    let -- Check each segment
        segResults = V.toList $ V.map (riverFillFromSegment worldSize gx gy surfZ meanderSeed) segments
        -- Check each waypoint (joint between segments)
        waypointResults = V.toList (V.map (riverFillFromWaypoint worldSize gx gy surfZ) segments)
                       <> if V.null segments then []
                          else [riverFillFromEndpoint worldSize gx gy surfZ (V.last segments)]
    in foldl' pickBestFill Nothing (segResults <> waypointResults)

-- | Water depth in tiles above the channel floor, based on flow rate.
--   Small streams: 1-2 tiles of water
--   Large rivers: 3-5 tiles of water
riverWaterDepth ∷ Float → Int
riverWaterDepth flow = max 1 (round (1.0 + flow * 2.5))

riverFillFromSegment ∷ Int → Int → Int → Int → Word64 → RiverSegment
                     → Maybe FluidCell
riverFillFromSegment worldSize gx gy surfZ _meanderSeed seg =
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
       in if tRaw < 0.0 ∨ tRaw > 1.0
          then Nothing
          else
          let closestX = tRaw * dx'
              closestY = tRaw * dy'
              perpX = px - closestX
              perpY = py - closestY
              perpDist = sqrt (perpX * perpX + perpY * perpY)
              channelHalfW = fromIntegral (rsWidth seg) / 2.0 ∷ Float
          in if perpDist > channelHalfW
             then Nothing
             else
             -- Water surface = terrain surface + water depth
             -- The terrain has already been carved by applyRiverCarve,
             -- so surfZ IS the channel floor for tiles inside the channel.
             -- We just add water on top.
             let waterDepth = riverWaterDepth (rsFlowRate seg)
                 waterSurface = surfZ + waterDepth
             in Just (FluidCell River waterSurface)

riverFillFromWaypoint ∷ Int → Int → Int → Int → RiverSegment
                      → Maybe FluidCell
riverFillFromWaypoint worldSize gx gy surfZ seg =
    let GeoCoord wx wy = rsStart seg
        (dxi, dyi) = wrappedDeltaUVFluid worldSize wx wy gx gy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)
        channelHalfW = fromIntegral (rsWidth seg) / 2.0 ∷ Float
    in if dist > channelHalfW
       then Nothing
       else
       let waterDepth = riverWaterDepth (rsFlowRate seg)
           waterSurface = surfZ + waterDepth
       in Just (FluidCell River waterSurface)

riverFillFromEndpoint ∷ Int → Int → Int → Int → RiverSegment
                      → Maybe FluidCell
riverFillFromEndpoint worldSize gx gy surfZ seg =
    let GeoCoord wx wy = rsEnd seg
        (dxi, dyi) = wrappedDeltaUVFluid worldSize wx wy gx gy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)
        channelHalfW = fromIntegral (rsWidth seg) / 2.0 ∷ Float
    in if dist > channelHalfW
       then Nothing
       else
       let waterDepth = riverWaterDepth (rsFlowRate seg)
           waterSurface = surfZ + waterDepth
       in Just (FluidCell River waterSurface)

-- | Ensure adjacent segments share consistent endpoint elevations.
--   After any modification (meander, deepen, etc.), the end of
--   segment N must equal the start of segment N+1.
--   Also enforces monotonic descent on water surface.
fixupSegmentContinuity ∷ V.Vector RiverSegment → V.Vector RiverSegment
fixupSegmentContinuity v
    | V.length v ≤ 1 = v
    | otherwise = V.fromList (go (V.head v) (V.toList (V.tail v)))
  where
    go _ [] = []
    go prev (cur : xs) =
        let fixed = cur { rsStartElev = rsEndElev prev
                        , rsStart     = rsEnd prev }
            fixed' = if rsEndElev fixed > rsStartElev fixed
                     then fixed { rsEndElev = rsStartElev fixed }
                     else fixed
        in fixed' : go fixed' xs

-----------------------------------------------------------
-- Lakes
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

isNearbyLake ∷ Int → Int → Int → PersistentFeature → Bool
isNearbyLake worldSize chunkGX chunkGY pf =
    case pfFeature pf of
        HydroShape (LakeFeature lk) →
            case pfActivity pf of
                FActive  → checkLakeRange worldSize chunkGX chunkGY lk
                FDormant → checkLakeRange worldSize chunkGX chunkGY lk
                _        → False
        _ → False

checkLakeRange ∷ Int → Int → Int → LakeParams → Bool
checkLakeRange worldSize chunkGX chunkGY lk =
    let GeoCoord fx fy = lkCenter lk
        maxR = lkRadius lk
        (dxi, dyi) = wrappedDeltaUVFluid worldSize chunkGX chunkGY fx fy
        dx = abs dxi
        dy = abs dyi
    in dx < maxR + chunkSize ∧ dy < maxR + chunkSize

fillLakeFromFeature ∷ MV.MVector s (Maybe FluidCell)
                    → PersistentFeature → Word64 → [TectonicPlate]
                    → Int → Int → Int
                    → VU.Vector Int
                    → ST s ()
fillLakeFromFeature mv pf seed plates worldSize chunkGX chunkGY surfaceMap =
    case pfFeature pf of
        HydroShape (LakeFeature lk) →
            let GeoCoord fx fy = lkCenter lk
                poolRadius = lkRadius lk
                (baseElev, _) = elevationAtGlobal seed plates worldSize fx fy
                lakeSurface = baseElev + lkSurface lk
            in fillLakePool mv seed plates worldSize chunkGX chunkGY
                   fx fy poolRadius lakeSurface surfaceMap
        _ → pure ()

fillLakePool ∷ MV.MVector s (Maybe FluidCell)
             → Word64 → [TectonicPlate] → Int → Int → Int
             → Int → Int → Int → Int
             → VU.Vector Int
             → ST s ()
fillLakePool mv seed plates worldSize chunkGX chunkGY fx fy poolRadius lakeSurface surfaceMap =
    let pr = fromIntegral poolRadius ∷ Float
        rimSamples = 32 ∷ Int

        spillway = foldl' (\minElev i →
            let angle = fromIntegral i * 2.0 * π / fromIntegral rimSamples
                rimGX = fx + round (pr * cos angle)
                rimGY = fy + round (pr * sin angle)
                rimLX = rimGX - chunkGX
                rimLY = rimGY - chunkGY
                rimElev =
                    if rimLX ≥ 0 ∧ rimLX < chunkSize ∧ rimLY ≥ 0 ∧ rimLY < chunkSize
                    then surfaceMap VU.! columnIndex rimLX rimLY
                    else
                        let (e, _) = elevationAtGlobal seed plates worldSize rimGX rimGY
                        in e
            in min minElev rimElev
            ) lakeSurface [0 .. rimSamples - 1]

        clampedSurface = min lakeSurface spillway

    in if clampedSurface ≤ seaLevel
       then pure ()
       else forEachSurface surfaceMap $ \idx lx ly surfZ →
            let gx = chunkGX + lx
                gy = chunkGY + ly
                (dxi, dyi) = wrappedDeltaUVFluid worldSize gx gy fx fy
                dx = fromIntegral dxi ∷ Float
                dy = fromIntegral dyi ∷ Float
                dist = sqrt (dx * dx + dy * dy)
            in when (dist < pr ∧ surfZ < clampedSurface) $
                MV.write mv idx (Just (FluidCell Lake clampedSurface))

-----------------------------------------------------------
-- Lava
-----------------------------------------------------------

isNearbyActive ∷ Int → Int → Int → PersistentFeature → Bool
isNearbyActive worldSize chunkGX chunkGY pf =
    case pfFeature pf of
        HydroShape _ → False
        VolcanicShape _ → case pfActivity pf of
            FActive    → checkRange
            FCollapsed → checkRange
            _          → False
  where
    checkRange = let maxR = getFeatureRadius (pfFeature pf)
                     GeoCoord fx fy = getFeatureCenter (pfFeature pf)
                     (dxi, dyi) = wrappedDeltaUVFluid worldSize chunkGX chunkGY fx fy
                     dx = abs dxi
                     dy = abs dyi
                 in dx < maxR + chunkSize ∧ dy < maxR + chunkSize

-- | Wrapped distance in u-space for fluid computations.
--   Returns (dx, dy) accounting for cylindrical wrapping along u-axis.
{-# INLINE wrappedDeltaUVFluid #-}
wrappedDeltaUVFluid ∷ Int → Int → Int → Int → Int → (Int, Int)
wrappedDeltaUVFluid worldSize gx1 gy1 gx2 gy2 =
    let w = worldSize * chunkSize
        halfW = w `div` 2
        du = (gx1 - gy1) - (gx2 - gy2)
        dv = (gx1 + gy1) - (gx2 + gy2)
        wrappedDU = ((du + halfW) `mod` w + w) `mod` w - halfW
        dx = (wrappedDU + dv) `div` 2
        dy = (dv - wrappedDU) `div` 2
    in (dx, dy)

fillLavaFromFeature ∷ MV.MVector s (Maybe FluidCell)
                    → PersistentFeature → Word64 → [TectonicPlate]
                    → Int → Int → Int
                    → VU.Vector Int
                    → ST s ()
fillLavaFromFeature mv pf seed plates worldSize chunkGX chunkGY surfaceMap =
    case (pfFeature pf) of
        VolcanicShape (SuperVolcano p) →
            let (fx, fy) = let GeoCoord x y = svCenter p in (x, y)
                poolRadius = svCalderaRadius p
                (baseElev, _) = elevationAtGlobal seed plates worldSize fx fy
                lavaSurface = baseElev + svRimHeight p - 5
            in fillPool mv seed plates worldSize chunkGX chunkGY fx fy
                   poolRadius lavaSurface surfaceMap

        VolcanicShape (Caldera p) →
            let (fx, fy) = let GeoCoord x y = caCenter p in (x, y)
                poolRadius = caInnerRadius p
                (baseElev, _) = elevationAtGlobal seed plates worldSize fx fy
                lavaSurface = baseElev + caRimHeight p - 3
            in fillPool mv seed plates worldSize chunkGX chunkGY fx fy
                   poolRadius lavaSurface surfaceMap

        VolcanicShape (ShieldVolcano p) | shSummitPit p →
            let (fx, fy) = let GeoCoord x y = shCenter p in (x, y)
                poolRadius = shPitRadius p
                (baseElev, _) = elevationAtGlobal seed plates worldSize fx fy
                lavaSurface = baseElev + shPeakHeight p - shPitDepth p + 2
            in fillPool mv seed plates worldSize chunkGX chunkGY fx fy
                   poolRadius lavaSurface surfaceMap

        VolcanicShape (CinderCone p) →
            let (fx, fy) = let GeoCoord x y = ccCenter p in (x, y)
                poolRadius = ccCraterRadius p
                (baseElev, _) = elevationAtGlobal seed plates worldSize fx fy
                lavaSurface = baseElev + ccPeakHeight p - ccCraterDepth p + 2
            in fillPool mv seed plates worldSize chunkGX chunkGY fx fy
                   poolRadius lavaSurface surfaceMap

        VolcanicShape (FissureVolcano p) | fpHasMagma p →
            let GeoCoord sx sy = fpStart p
                GeoCoord ex ey = fpEnd p
                midX = (sx + ex) `div` 2
                midY = (sy + ey) `div` 2
                (baseElev, _) = elevationAtGlobal seed plates worldSize midX midY
                poolWidth = fpWidth p `div` 2
                lavaSurface = baseElev + fpRidgeHeight p - 3
            in fillFissurePool mv seed plates worldSize chunkGX chunkGY
                   sx sy ex ey poolWidth lavaSurface surfaceMap

        VolcanicShape (HydrothermalVent p) →
            let (fx, fy) = let GeoCoord x y = htCenter p in (x, y)
                poolRadius = max 2 (htRadius p `div` 3)
                (baseElev, _) = elevationAtGlobal seed plates worldSize fx fy
                lavaSurface = baseElev + htChimneyHeight p - 2
            in fillPool mv seed plates worldSize chunkGX chunkGY fx fy
                   poolRadius lavaSurface surfaceMap
        _ → pure ()

fillPool ∷ MV.MVector s (Maybe FluidCell)
         → Word64 → [TectonicPlate] → Int → Int → Int → Int → Int → Int → Int
         → VU.Vector Int
         → ST s ()
fillPool mv seed plates worldSize chunkGX chunkGY fx fy poolRadius lavaSurface surfaceMap =
    let pr = fromIntegral poolRadius ∷ Float
        rimSamples = 32 ∷ Int
        spillway = foldl' (\minElev i →
            let angle = fromIntegral i * 2.0 * π / fromIntegral rimSamples
                rimGX = fx + round (pr * cos angle)
                rimGY = fy + round (pr * sin angle)
                rimLX = rimGX - chunkGX
                rimLY = rimGY - chunkGY
                rimElev =
                    if rimLX ≥ 0 ∧ rimLX < chunkSize ∧ rimLY ≥ 0 ∧ rimLY < chunkSize
                    then surfaceMap VU.! columnIndex rimLX rimLY
                    else
                        let (e, _) = elevationAtGlobal seed plates worldSize rimGX rimGY
                        in e
            in min minElev rimElev
            ) lavaSurface [0 .. rimSamples - 1]

        clampedSurface = min lavaSurface spillway

    in if clampedSurface ≤ seaLevel
       then pure ()
       else forEachSurface surfaceMap $ \idx lx ly surfZ →
            let gx = chunkGX + lx
                gy = chunkGY + ly
                (dxi, dyi) = wrappedDeltaUVFluid worldSize gx gy fx fy
                dx = fromIntegral dxi ∷ Float
                dy = fromIntegral dyi ∷ Float
                dist = sqrt (dx * dx + dy * dy)
            in when (dist < pr ∧ surfZ < clampedSurface) $
                MV.write mv idx (Just (FluidCell Lava clampedSurface))

fillFissurePool ∷ MV.MVector s (Maybe FluidCell)
                → Word64 → [TectonicPlate] → Int → Int → Int
                → Int → Int → Int → Int
                → Int → Int
                → VU.Vector Int
                → ST s ()
fillFissurePool mv seed plates worldSize chunkGX chunkGY sx sy ex ey halfWidth lavaSurface surfaceMap =
    let lineLen = sqrt (fromIntegral ((ex-sx)*(ex-sx) + (ey-sy)*(ey-sy))) ∷ Float
    in if lineLen < 0.001 then pure ()
    else
    let edgeSamples = 16 ∷ Int
        hw = fromIntegral halfWidth ∷ Float
        perpX = negate (fromIntegral (ey - sy)) / lineLen ∷ Float
        perpY = fromIntegral (ex - sx) / lineLen ∷ Float

        spillway = foldl' (\minElev i →
            let t = fromIntegral i / fromIntegral (edgeSamples - 1) ∷ Float
                mx = fromIntegral sx + t * fromIntegral (ex - sx)
                my = fromIntegral sy + t * fromIntegral (ey - sy)
                e1gx = round (mx + perpX * hw) ∷ Int
                e1gy = round (my + perpY * hw) ∷ Int
                e2gx = round (mx - perpX * hw) ∷ Int
                e2gy = round (my - perpY * hw) ∷ Int

                e1lx = e1gx - chunkGX
                e1ly = e1gy - chunkGY
                elev1 =
                    if e1lx ≥ 0 ∧ e1lx < chunkSize ∧ e1ly ≥ 0 ∧ e1ly < chunkSize
                    then surfaceMap VU.! columnIndex e1lx e1ly
                    else fst (elevationAtGlobal seed plates worldSize e1gx e1gy)

                e2lx = e2gx - chunkGX
                e2ly = e2gy - chunkGY
                elev2 =
                    if e2lx ≥ 0 ∧ e2lx < chunkSize ∧ e2ly ≥ 0 ∧ e2ly < chunkSize
                    then surfaceMap VU.! columnIndex e2lx e2ly
                    else fst (elevationAtGlobal seed plates worldSize e2gx e2gy)
            in min minElev (min elev1 elev2)
            ) lavaSurface [0 .. edgeSamples - 1]

        clampedSurface = min lavaSurface spillway

    in if clampedSurface ≤ seaLevel
       then pure ()
       else forEachSurface surfaceMap $ \idx lx ly surfZ →
            let gx = chunkGX + lx
                gy = chunkGY + ly
                (dxi, dyi) = wrappedDeltaUVFluid worldSize gx gy sx sy
                dx = fromIntegral dxi ∷ Float
                dy = fromIntegral dyi ∷ Float
                lx' = fromIntegral (ex - sx) ∷ Float
                ly' = fromIntegral (ey - sy) ∷ Float
                t = max 0 (min 1 ((dx * lx' + dy * ly') / (lineLen * lineLen)))
                projX = t * lx'
                projY = t * ly'
                perpDist = sqrt ((dx - projX) * (dx - projX) + (dy - projY) * (dy - projY))
            in when (perpDist < hw ∧ surfZ < clampedSurface) $
                MV.write mv idx (Just (FluidCell Lava clampedSurface))

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

floorDiv' ∷ Int → Int → Int
floorDiv' a b = floor (fromIntegral a / fromIntegral b ∷ Double)

-- | Quick boolean check: does this chunk have any lava?
--   Avoids allocating a full FluidMap — just checks if any
--   active volcanic feature is near enough to produce lava.
hasAnyLavaQuick ∷ [PersistentFeature] → Word64 → [TectonicPlate]
                → Int → ChunkCoord → Int → Bool
hasAnyLavaQuick features seed plates worldSize coord _avgElev =
    let ChunkCoord cx cy = coord
        chunkMinGX = cx * chunkSize
        chunkMinGY = cy * chunkSize
    in any (isNearbyActive worldSize chunkMinGX chunkMinGY) features

-- | Quick boolean check: does this chunk have any ocean fluid?
--   Just checks the ocean map and neighbors — no vector allocation.
hasAnyOceanFluid ∷ OceanMap → ChunkCoord → Bool
hasAnyOceanFluid oceanMap coord =
    let ChunkCoord cx cy = coord
    in isOceanChunk oceanMap coord
     ∨ isOceanChunk oceanMap (ChunkCoord cx (cy - 1))
     ∨ isOceanChunk oceanMap (ChunkCoord cx (cy + 1))
     ∨ isOceanChunk oceanMap (ChunkCoord (cx + 1) cy)
     ∨ isOceanChunk oceanMap (ChunkCoord (cx - 1) cy)
     ∨ isOceanChunk oceanMap (ChunkCoord (cx + 1) (cy - 1))
     ∨ isOceanChunk oceanMap (ChunkCoord (cx - 1) (cy - 1))
     ∨ isOceanChunk oceanMap (ChunkCoord (cx + 1) (cy + 1))
     ∨ isOceanChunk oceanMap (ChunkCoord (cx - 1) (cy + 1))
