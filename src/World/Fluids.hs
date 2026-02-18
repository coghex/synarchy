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

        wrapChunkX cx =
            let wrapped = ((cx + halfSize) `mod` (halfSize * 2) + (halfSize * 2))
                          `mod` (halfSize * 2) - halfSize
            in wrapped

        neighbors (ChunkCoord cx cy) =
            [ ChunkCoord (wrapChunkX (cx + dx)) (cy + dy)
            | (dx, dy) ← [(-1,0), (1,0), (0,-1), (0,1)]
            , let ny = cy + dy
            , ny ≥ -halfSize ∧ ny < halfSize
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

computeChunkFluid ∷ OceanMap → ChunkCoord
                  → VU.Vector Int
                  → FluidMap
computeChunkFluid oceanMap coord surfaceMap
    | isOceanChunk oceanMap coord =
        buildOceanSurface surfaceMap
    | hasOceanNeighbor =
        buildOceanSurface surfaceMap
    | otherwise = emptyFluidMap
  where
    ChunkCoord cx cy = coord
    hasOceanNeighbor =
        isOceanChunk oceanMap (ChunkCoord cx (cy - 1))
      ∨ isOceanChunk oceanMap (ChunkCoord cx (cy + 1))
      ∨ isOceanChunk oceanMap (ChunkCoord (cx + 1) cy)
      ∨ isOceanChunk oceanMap (ChunkCoord (cx - 1) cy)
      ∨ isOceanChunk oceanMap (ChunkCoord (cx + 1) (cy - 1))
      ∨ isOceanChunk oceanMap (ChunkCoord (cx - 1) (cy - 1))
      ∨ isOceanChunk oceanMap (ChunkCoord (cx + 1) (cy + 1))
      ∨ isOceanChunk oceanMap (ChunkCoord (cx - 1) (cy + 1))

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

bestRiverFill ∷ Int → Int → Int → Int → Word64 → [RiverSegment]
              → Maybe FluidCell
bestRiverFill worldSize gx gy surfZ meanderSeed segments =
    let results = map (riverFillFromSegment worldSize gx gy surfZ meanderSeed) segments
        best = foldl' pickBestFill Nothing results
    in best

pickBestFill ∷ Maybe FluidCell → Maybe FluidCell → Maybe FluidCell
pickBestFill Nothing b = b
pickBestFill a Nothing = a
pickBestFill (Just a) (Just b) =
    if fcSurface b > fcSurface a then Just b else Just a

riverFillFromSegment ∷ Int → Int → Int → Int → Word64 → RiverSegment
                     → Maybe FluidCell
riverFillFromSegment worldSize gx gy surfZ _meanderSeed seg =
    let GeoCoord sx sy = rsStart seg
        GeoCoord ex ey = rsEnd seg
        dx' = fromIntegral (wrappedDeltaForFluid worldSize ex sx) ∷ Float
        dy' = fromIntegral (ey - sy) ∷ Float
        segLen2 = dx' * dx' + dy' * dy'
    in if segLen2 < 1.0
       then Nothing
       else
       let px = fromIntegral (wrappedDeltaForFluid worldSize gx sx) ∷ Float
           py = fromIntegral (gy - sy) ∷ Float
           t = max 0.0 (min 1.0 ((px * dx' + py * dy') / segLen2))
           closestX = t * dx'
           closestY = t * dy'
           perpX = px - closestX
           perpY = py - closestY
           perpDist = sqrt (perpX * perpX + perpY * perpY)
           channelHalfW = fromIntegral (rsWidth seg) / 2.0 ∷ Float
       in if perpDist > channelHalfW
          then Nothing
          else
          let flow = rsFlowRate seg
              waterDepth = 1 + floor (flow * 3.0) ∷ Int
              startElev = rsStartElev seg
              endElev   = rsEndElev seg
              interpElev = fromIntegral startElev
                         + t * (fromIntegral endElev - fromIntegral startElev)
              channelFloor = interpElev - fromIntegral (rsDepth seg)
              waterSurface = round channelFloor + waterDepth
          in if surfZ ≥ waterSurface
             then Nothing
             else Just (FluidCell River waterSurface)

-----------------------------------------------------------
-- Lakes
-----------------------------------------------------------

isNearbyRiver ∷ Int → Int → Int → PersistentFeature → Bool
isNearbyRiver worldSize chunkGX chunkGY pf =
    case pfFeature pf of
        HydroShape (RiverFeature river) →
            case pfActivity pf of
                FActive  → anySegmentNearby worldSize chunkGX chunkGY river
                FDormant → anySegmentNearby worldSize chunkGX chunkGY river
                _        → False
        _ → False

anySegmentNearby ∷ Int → Int → Int → RiverParams → Bool
anySegmentNearby worldSize chunkGX chunkGY river =
    any (segmentNearChunk worldSize chunkGX chunkGY) (rpSegments river)

segmentNearChunk ∷ Int → Int → Int → RiverSegment → Bool
segmentNearChunk worldSize chunkGX chunkGY seg =
    let GeoCoord sx sy = rsStart seg
        GeoCoord ex ey = rsEnd seg
        margin = rsValleyWidth seg + chunkSize
        segMinX = min sx ex - margin
        segMaxX = max sx ex + margin
        segMinY = min sy ey - margin
        segMaxY = max sy ey + margin
        chunkMaxGX = chunkGX + chunkSize - 1
        chunkMaxGY = chunkGY + chunkSize - 1
        dx = abs (wrappedDeltaForFluid worldSize
                  (chunkGX + chunkSize `div` 2)
                  ((sx + ex) `div` 2))
        maxDX = (segMaxX - segMinX) `div` 2 + chunkSize
    in dx < maxDX ∧ segMaxY ≥ chunkGY ∧ segMinY ≤ chunkMaxGY

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
        dx = abs (wrappedDeltaForFluid worldSize chunkGX fx)
        dy = abs (chunkGY - fy)
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
                dx = fromIntegral (wrappedDeltaForFluid worldSize gx fx) ∷ Float
                dy = fromIntegral (gy - fy) ∷ Float
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
                     dx = abs (wrappedDeltaForFluid worldSize chunkGX fx)
                     dy = abs (chunkGY - fy)
                 in dx < maxR + chunkSize ∧ dy < maxR + chunkSize

wrappedDeltaForFluid ∷ Int → Int → Int → Int
wrappedDeltaForFluid worldSize a b =
    let w = worldSize * chunkSize
        raw = b - a
        halfW = w `div` 2
    in ((raw + halfW) `mod` w + w) `mod` w - halfW

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

    in if clampedSurface ≤ 0
       then pure ()
       else forEachSurface surfaceMap $ \idx lx ly surfZ →
            let gx = chunkGX + lx
                gy = chunkGY + ly
                dx = fromIntegral (wrappedDeltaForFluid worldSize gx fx) ∷ Float
                dy = fromIntegral (gy - fy) ∷ Float
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

    in if clampedSurface ≤ 0
       then pure ()
       else forEachSurface surfaceMap $ \idx lx ly surfZ →
            let gx = chunkGX + lx
                gy = chunkGY + ly
                dx = fromIntegral (wrappedDeltaForFluid worldSize gx sx) ∷ Float
                dy = fromIntegral (gy - sy) ∷ Float
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
