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
      -- * Query
    , isOceanChunk
    ) where

import UPrelude
import Data.Word (Word64)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable(..))
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import World.Types
import World.Material (MaterialId(..), matGlacier)
import World.Plate (TectonicPlate(..), generatePlates, elevationAtGlobal
                   , isBeyondGlacier, wrapGlobalU)

-- | Compute which chunks are ocean by flood-filling from
--   ocean plate centers. A chunk is ocean if:
--   1. Its center elevation (from plates) is below sea level
--   2. It's reachable from an ocean plate center without
--      crossing chunks above sea level
--
--   This correctly leaves inland basins unflooded.
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

        -- Find seed chunks: for each ocean plate, find the chunk
        -- containing its center, verify it's below sea level
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

        -- BFS flood fill from seeds
        -- Wraps X, clamps Y to world bounds
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

-- | Check if a chunk is in the ocean map.
isOceanChunk ∷ OceanMap → ChunkCoord → Bool
isOceanChunk = flip HS.member

-----------------------------------------------------------
-- Chunk-Level Fluid Computation
-----------------------------------------------------------

computeChunkFluid ∷ OceanMap → ChunkCoord
                  → HM.HashMap (Int, Int) Int   -- ^ surfaceMap (terrain elevation)
                  → HM.HashMap (Int, Int) FluidCell
computeChunkFluid oceanMap coord surfaceMap
    -- Case 1: full ocean chunk — every below-sea-level column gets water
    | isOceanChunk oceanMap coord = HM.foldlWithKey' (\acc (lx, ly) surfZ →
        if surfZ < seaLevel
        then HM.insert (lx, ly) (FluidCell Ocean seaLevel) acc
        else acc
        ) HM.empty surfaceMap

    -- Case 2: land chunk bordering ocean — flood ALL below-sea-level columns.
    -- The chunk-level ocean map already prevents inland basins from being
    -- adjacent to ocean chunks, so any below-sea-level tile in a chunk
    -- bordering the ocean should have water.
    | hasOceanNeighbor = HM.foldlWithKey' (\acc (lx, ly) surfZ →
        if surfZ < seaLevel
        then HM.insert (lx, ly) (FluidCell Ocean seaLevel) acc
        else acc
        ) HM.empty surfaceMap

    -- Case 3: no ocean neighbors — no ocean water
    | otherwise = HM.empty
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

-- | Compute lava fluid cells for a chunk.
--   For each active volcanic feature near this chunk,
--   fills columns where the lava surface (from the feature's
--   caldera floor or crater) is above the terrain surface.
--
--   Lava fluid does NOT overwrite existing ocean fluid —
--   ocean takes priority (lava hitting ocean = obsidian crust,
--   handled later).
computeChunkLava ∷ [PersistentFeature] → Word64 → [TectonicPlate]
                 → Int → ChunkCoord
                 → HM.HashMap (Int, Int) Int
                 → HM.HashMap (Int, Int) FluidCell
computeChunkLava features seed plates worldSize coord surfaceMap =
    let ChunkCoord cx cy = coord
        chunkMinGX = cx * chunkSize
        chunkMinGY = cy * chunkSize
        nearbyActive = filter (isNearbyActive worldSize chunkMinGX chunkMinGY) features
    in foldl' (\acc pf →
        fillLavaFromFeature pf seed plates worldSize chunkMinGX chunkMinGY surfaceMap acc
        ) HM.empty nearbyActive

-- | Is this feature active and close enough to affect this chunk?
isNearbyActive ∷ Int → Int → Int → PersistentFeature → Bool
isNearbyActive worldSize chunkGX chunkGY pf =
    case pfActivity pf of
        Active    → checkRange
        Collapsed → checkRange  -- calderas still have lava
        _         → False
  where
    checkRange = let maxR = featureMaxRadius (pfFeature pf)
                     (fx, fy) = featureCenter' (pfFeature pf)
                     dx = abs (wrappedDeltaForFluid worldSize chunkGX fx)
                     dy = abs (chunkGY - fy)
                 in dx < maxR + chunkSize ∧ dy < maxR + chunkSize

-- | Get the maximum radius of influence for a feature's lava pool.
featureMaxRadius ∷ VolcanicFeature → Int
featureMaxRadius (ShieldVolcano p)    = shBaseRadius p
featureMaxRadius (CinderCone p)       = ccBaseRadius p
featureMaxRadius (LavaDome p)         = ldBaseRadius p
featureMaxRadius (Caldera p)          = caOuterRadius p
featureMaxRadius (SuperVolcano p)     = svCalderaRadius p
featureMaxRadius (FissureVolcano p)   = fpWidth p * 2
featureMaxRadius (HydrothermalVent _) = 0
featureMaxRadius (LavaTube _)         = 0

-- | Extract center from a feature (same as Timeline.featureCenter
--   but local to Fluids to avoid circular imports).
featureCenter' ∷ VolcanicFeature → (Int, Int)
featureCenter' (ShieldVolcano p)    = let GeoCoord x y = shCenter p in (x, y)
featureCenter' (CinderCone p)       = let GeoCoord x y = ccCenter p in (x, y)
featureCenter' (LavaDome p)         = let GeoCoord x y = ldCenter p in (x, y)
featureCenter' (Caldera p)          = let GeoCoord x y = caCenter p in (x, y)
featureCenter' (SuperVolcano p)     = let GeoCoord x y = svCenter p in (x, y)
featureCenter' (FissureVolcano p)   = let GeoCoord sx sy = fpStart p
                                          GeoCoord ex ey = fpEnd p
                                      in ((sx + ex) `div` 2, (sy + ey) `div` 2)
featureCenter' (HydrothermalVent p) = let GeoCoord x y = htCenter p in (x, y)
featureCenter' (LavaTube p)         = let GeoCoord sx sy = ltStart p
                                          GeoCoord ex ey = ltEnd p
                                      in ((sx + ex) `div` 2, (sy + ey) `div` 2)

-- | Wrapped delta for fluid proximity check.
wrappedDeltaForFluid ∷ Int → Int → Int → Int
wrappedDeltaForFluid worldSize a b =
    let w = worldSize * chunkSize
        raw = b - a
        halfW = w `div` 2
    in ((raw + halfW) `mod` w + w) `mod` w - halfW

-- | Fill lava cells from a single active feature into a chunk.
--   Each feature type defines a lava pool shape:
--     - Caldera/SuperVolcano: fills the bowl up to rim height
--     - ShieldVolcano with summit pit: fills the pit
--     - CinderCone: fills the crater
--     - FissureVolcano with magma: fills along the fissure
--     - LavaDome: no pool (too viscous)
fillLavaFromFeature ∷ PersistentFeature → Word64 → [TectonicPlate]
                    → Int → Int → Int
                    → HM.HashMap (Int, Int) Int
                    → HM.HashMap (Int, Int) FluidCell
                    → HM.HashMap (Int, Int) FluidCell
fillLavaFromFeature pf seed plates worldSize chunkGX chunkGY surfaceMap acc =
    case pfFeature pf of
        SuperVolcano p →
            let (fx, fy) = let GeoCoord x y = svCenter p in (x, y)
                poolRadius = svCalderaRadius p
                (baseElev, _) = elevationAtGlobal seed plates worldSize fx fy
                -- Lava fills the caldera bowl, up near the rim
                lavaSurface = baseElev + svRimHeight p - 5
            in fillPool seed plates worldSize chunkGX chunkGY fx fy
                   poolRadius lavaSurface surfaceMap acc

        Caldera p →
            let (fx, fy) = let GeoCoord x y = caCenter p in (x, y)
                poolRadius = caInnerRadius p
                (baseElev, _) = elevationAtGlobal seed plates worldSize fx fy
                lavaSurface = baseElev + caRimHeight p - 3
            in fillPool seed plates worldSize chunkGX chunkGY fx fy
                   poolRadius lavaSurface surfaceMap acc

        ShieldVolcano p | shSummitPit p →
            let (fx, fy) = let GeoCoord x y = shCenter p in (x, y)
                poolRadius = shPitRadius p
                (baseElev, _) = elevationAtGlobal seed plates worldSize fx fy
                lavaSurface = baseElev + shPeakHeight p - shPitDepth p + 2
            in fillPool seed plates worldSize chunkGX chunkGY fx fy
                   poolRadius lavaSurface surfaceMap acc

        CinderCone p →
            let (fx, fy) = let GeoCoord x y = ccCenter p in (x, y)
                poolRadius = ccCraterRadius p
                (baseElev, _) = elevationAtGlobal seed plates worldSize fx fy
                lavaSurface = baseElev + ccPeakHeight p - ccCraterDepth p + 2
            in fillPool seed plates worldSize chunkGX chunkGY fx fy
                   poolRadius lavaSurface surfaceMap acc

        FissureVolcano p | fpHasMagma p →
            let GeoCoord sx sy = fpStart p
                GeoCoord ex ey = fpEnd p
                midX = (sx + ex) `div` 2
                midY = (sy + ey) `div` 2
                (baseElev, _) = elevationAtGlobal seed plates worldSize midX midY
                poolWidth = fpWidth p `div` 2
                lavaSurface = baseElev + fpRidgeHeight p - 3
            in fillFissurePool seed plates worldSize chunkGX chunkGY
                   sx sy ex ey poolWidth lavaSurface surfaceMap acc

        -- Hydrothermal vents: small magma pool at the chimney
        HydrothermalVent p →
            let (fx, fy) = let GeoCoord x y = htCenter p in (x, y)
                poolRadius = max 2 (htRadius p `div` 3)
                (baseElev, _) = elevationAtGlobal seed plates worldSize fx fy
                lavaSurface = baseElev + htChimneyHeight p - 2
            in fillPool seed plates worldSize chunkGX chunkGY fx fy
                   poolRadius lavaSurface surfaceMap acc

        _ → acc

-- | Fill a circular lava pool into the fluid map.
--   First finds the spillway — the lowest point on the pool's
--   perimeter — and clamps the lava surface to that level.
--   Lava can't be higher than its lowest escape point.
fillPool ∷ Word64 → [TectonicPlate] → Int → Int → Int → Int → Int → Int → Int
         → HM.HashMap (Int, Int) Int
         → HM.HashMap (Int, Int) FluidCell
         → HM.HashMap (Int, Int) FluidCell
fillPool seed plates worldSize chunkGX chunkGY fx fy poolRadius lavaSurface surfaceMap acc =
    let pr = fromIntegral poolRadius ∷ Float
        rimSamples = 32 ∷ Int

        -- Sample the rim at poolRadius distance, using the surface map
        -- for in-chunk points and elevationAtGlobal for out-of-chunk points.
        spillway = foldl' (\minElev i →
            let angle = fromIntegral i * 2.0 * π / fromIntegral rimSamples
                rimGX = fx + round (pr * cos angle)
                rimGY = fy + round (pr * sin angle)
                rimLX = rimGX - chunkGX
                rimLY = rimGY - chunkGY
                rimElev = case HM.lookup (rimLX, rimLY) surfaceMap of
                    Just e  → e
                    Nothing →
                        -- Outside this chunk — use base plate elevation
                        let (e, _) = elevationAtGlobal seed plates worldSize rimGX rimGY
                        in e
            in min minElev rimElev
            ) lavaSurface [0 .. rimSamples - 1]

        clampedSurface = min lavaSurface spillway

    in if clampedSurface ≤ 0
       then acc
       else HM.foldlWithKey' (\acc' (lx, ly) surfZ →
            let gx = chunkGX + lx
                gy = chunkGY + ly
                dx = fromIntegral (wrappedDeltaForFluid worldSize gx fx) ∷ Float
                dy = fromIntegral (gy - fy) ∷ Float
                dist = sqrt (dx * dx + dy * dy)
            in if dist < pr ∧ surfZ < clampedSurface
               then HM.insert (lx, ly) (FluidCell Lava clampedSurface) acc'
               else acc'
            ) acc surfaceMap

-- | Fill lava along a fissure line.
fillFissurePool ∷ Word64 → [TectonicPlate] → Int → Int → Int
                → Int → Int → Int → Int
                → Int → Int
                → HM.HashMap (Int, Int) Int
                → HM.HashMap (Int, Int) FluidCell
                → HM.HashMap (Int, Int) FluidCell
fillFissurePool seed plates worldSize chunkGX chunkGY sx sy ex ey halfWidth lavaSurface surfaceMap acc =
    let lineLen = sqrt (fromIntegral ((ex-sx)*(ex-sx) + (ey-sy)*(ey-sy))) ∷ Float
    in if lineLen < 0.001 then acc
    else
    let -- Sample perpendicular to the fissure at intervals along it
        -- to find the spillway
        edgeSamples = 16 ∷ Int
        hw = fromIntegral halfWidth ∷ Float
        -- Perpendicular direction
        perpX = negate (fromIntegral (ey - sy)) / lineLen ∷ Float
        perpY = fromIntegral (ex - sx) / lineLen ∷ Float

        spillway = foldl' (\minElev i →
            let t = fromIntegral i / fromIntegral (edgeSamples - 1) ∷ Float
                -- Point along fissure
                mx = fromIntegral sx + t * fromIntegral (ex - sx)
                my = fromIntegral sy + t * fromIntegral (ey - sy)
                -- Sample both edges
                e1gx = round (mx + perpX * hw) ∷ Int
                e1gy = round (my + perpY * hw) ∷ Int
                e2gx = round (mx - perpX * hw) ∷ Int
                e2gy = round (my - perpY * hw) ∷ Int
                elev1 = case HM.lookup (e1gx - chunkGX, e1gy - chunkGY) surfaceMap of
                    Just e  → e
                    Nothing → fst (elevationAtGlobal seed plates worldSize e1gx e1gy)
                elev2 = case HM.lookup (e2gx - chunkGX, e2gy - chunkGY) surfaceMap of
                    Just e  → e
                    Nothing → fst (elevationAtGlobal seed plates worldSize e2gx e2gy)
            in min minElev (min elev1 elev2)
            ) lavaSurface [0 .. edgeSamples - 1]

        clampedSurface = min lavaSurface spillway

    in if clampedSurface ≤ 0
       then acc
       else HM.foldlWithKey' (\acc' (lx, ly) surfZ →
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
            in if perpDist < hw ∧ surfZ < clampedSurface
               then HM.insert (lx, ly) (FluidCell Lava clampedSurface) acc'
               else acc'
            ) acc surfaceMap

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

floorDiv' ∷ Int → Int → Int
floorDiv' a b = floor (fromIntegral a / fromIntegral b ∷ Double)
