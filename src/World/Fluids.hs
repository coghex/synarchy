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
      -- * Query
    , isOceanChunk
    ) where

import UPrelude
import Data.Word (Word64)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import Data.Hashable (Hashable(..))
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import World.Base
import World.Types
import World.Material (MaterialId(..), matGlacier)
import World.Plate (TectonicPlate(..), generatePlates, elevationAtGlobal
                   , isBeyondGlacier, wrapGlobalU)
import World.Geology.Evolution (getFeatureCenter, getFeatureRadius)
import World.Hydrology.Types (HydroFeature(..), LakeParams(..)
                             , RiverParams(..), RiverSegment(..))

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
                  → VU.Vector Int
                  → HM.HashMap (Int, Int) FluidCell
computeChunkFluid oceanMap coord surfaceMap
    -- Case 1: full ocean chunk — every below-sea-level column gets water
    | isOceanChunk oceanMap coord =
        VU.ifoldl' (\acc idx surfZ ->
            let lx = idx `mod` chunkSize
                ly = idx `div` chunkSize
            in if surfZ < seaLevel
               then HM.insert (lx, ly) (FluidCell Ocean seaLevel) acc
               else acc
        ) HM.empty surfaceMap

    -- Case 2: land chunk bordering ocean — flood ALL below-sea-level columns.
    -- The chunk-level ocean map already prevents inland basins from being
    -- adjacent to ocean chunks, so any below-sea-level tile in a chunk
    -- bordering the ocean should have water.
    | hasOceanNeighbor =
        VU.ifoldl' (\acc idx surfZ ->
            let lx = idx `mod` chunkSize
                ly = idx `div` chunkSize
            in if surfZ < seaLevel
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
                 → VU.Vector Int
                 → HM.HashMap (Int, Int) FluidCell
computeChunkLava features seed plates worldSize coord surfaceMap =
    let ChunkCoord cx cy = coord
        chunkMinGX = cx * chunkSize
        chunkMinGY = cy * chunkSize
        nearbyActive = filter (isNearbyActive worldSize chunkMinGX chunkMinGY) features
    in foldl' (\acc pf →
        fillLavaFromFeature pf seed plates worldSize chunkMinGX chunkMinGY surfaceMap acc
        ) HM.empty nearbyActive

-- | Compute lake fluid cells for a chunk.
--   For each active lake feature near this chunk,
--   fills columns where the lake surface is above the
--   terrain surface and the column is within the lake basin.
--
--   Lakes come from two sources during world gen:
--     1. Moraine-dammed glacial lakes (spawnMoraineLake)
--     2. River-dammed lakes (evolveRiver → RiverDam)
--
--   A lake's fluid surface = base terrain elevation at center + lkSurface.
--   A lake's basin extends lkRadius tiles from center.
--   Columns within the basin that are below the lake surface get fluid.
--
--   Lake fluid does NOT overwrite existing ocean fluid —
--   ocean takes priority (a lake at the coast would just
--   be part of the ocean). Lava also takes priority over
--   lakes (lava boils the lake = steam, handled later).
--
--   The spillway mechanic from fillPool is reused: the lake
--   surface is clamped to the lowest point on the basin rim
--   so water can't be higher than its escape point. This
--   naturally handles oddly-shaped terrain — the lake fills
--   to the rim and no higher.
computeChunkLakes ∷ [PersistentFeature] → Word64 → [TectonicPlate]
                  → Int → ChunkCoord
                  → VU.Vector Int
                  → HM.HashMap (Int, Int) FluidCell
computeChunkLakes features seed plates worldSize coord surfaceMap =
    let ChunkCoord cx cy = coord
        chunkMinGX = cx * chunkSize
        chunkMinGY = cy * chunkSize
        nearbyLakes = filter (isNearbyLake worldSize chunkMinGX chunkMinGY) features
    in foldl' (\acc pf →
        fillLakeFromFeature pf seed plates worldSize chunkMinGX chunkMinGY
                            surfaceMap acc
        ) HM.empty nearbyLakes

-----------------------------------------------------------
-- River Fluid Fill
-----------------------------------------------------------

-- | Compute river fluid cells for a chunk.
--   For each active river whose segments pass near this chunk,
--   fills columns that are within the river channel (not the
--   full valley — just the flat channel floor) with River fluid.
--
--   The water surface at each column is computed from the
--   segment's endpoint elevations (interpolated along the
--   segment) plus a shallow depth derived from flow rate.
--   This gives rivers that follow their carved channel with
--   a thin layer of water on top.
--
--   River fluid does NOT overwrite ocean or lake fluid —
--   if a river reaches the ocean, the ocean fluid takes over.
--   Lava also takes priority (river hitting lava = steam).
computeChunkRivers ∷ [PersistentFeature] → Word64 → [TectonicPlate]
                   → Int → ChunkCoord
                   → VU.Vector Int
                   → HM.HashMap (Int, Int) FluidCell
computeChunkRivers features _seed _plates worldSize coord surfaceMap =
    let ChunkCoord cx cy = coord
        chunkMinGX = cx * chunkSize
        chunkMinGY = cy * chunkSize
        nearbyRivers = filter (isNearbyRiver worldSize chunkMinGX chunkMinGY) features
    in foldl' (\acc pf →
        fillRiverFromFeature pf worldSize chunkMinGX chunkMinGY surfaceMap acc
        ) HM.empty nearbyRivers

-- | Is this feature a flowing river close enough to affect this chunk?
isNearbyRiver ∷ Int → Int → Int → PersistentFeature → Bool
isNearbyRiver worldSize chunkGX chunkGY pf =
    case pfFeature pf of
        HydroShape (RiverFeature river) →
            case pfActivity pf of
                FActive  → anySegmentNearby worldSize chunkGX chunkGY river
                FDormant → anySegmentNearby worldSize chunkGX chunkGY river
                _        → False
        _ → False

-- | Check if any segment of a river passes within range of this chunk.
--   We check each segment's bounding box (expanded by valley width)
--   against the chunk's tile range.
anySegmentNearby ∷ Int → Int → Int → RiverParams → Bool
anySegmentNearby worldSize chunkGX chunkGY river =
    any (segmentNearChunk worldSize chunkGX chunkGY) (rpSegments river)

segmentNearChunk ∷ Int → Int → Int → RiverSegment → Bool
segmentNearChunk worldSize chunkGX chunkGY seg =
    let GeoCoord sx sy = rsStart seg
        GeoCoord ex ey = rsEnd seg
        margin = rsValleyWidth seg + chunkSize
        -- Bounding box of this segment (with margin)
        segMinX = min sx ex - margin
        segMaxX = max sx ex + margin
        segMinY = min sy ey - margin
        segMaxY = max sy ey + margin
        chunkMaxGX = chunkGX + chunkSize - 1
        chunkMaxGY = chunkGY + chunkSize - 1
        -- Simple AABB overlap (ignoring wrapping for now —
        -- rivers don't usually span the world wrap boundary)
        dx = abs (wrappedDeltaForFluid worldSize
                  (chunkGX + chunkSize `div` 2)
                  ((sx + ex) `div` 2))
        maxDX = (segMaxX - segMinX) `div` 2 + chunkSize
    in dx < maxDX ∧ segMaxY ≥ chunkGY ∧ segMinY ≤ chunkMaxGY

-- | Fill river fluid from a single river feature into the fluid map.
fillRiverFromFeature ∷ PersistentFeature → Int → Int → Int
                     → VU.Vector Int
                     → HM.HashMap (Int, Int) FluidCell
                     → HM.HashMap (Int, Int) FluidCell
fillRiverFromFeature pf worldSize chunkGX chunkGY surfaceMap acc =
    case pfFeature pf of
        HydroShape (RiverFeature river) →
            let segments = rpSegments river
                meanderSeed = rpMeanderSeed river
            in VU.ifoldl' (\acc' idx surfZ →
                 let lx = idx `mod` chunkSize
                     ly = idx `div` chunkSize
                 in case bestRiverFill worldSize (chunkGX + lx) (chunkGY + ly)
                                    surfZ meanderSeed segments of
                                      Nothing → acc'
                                      Just fc → HM.insert (lx, ly) fc acc'
                 ) acc surfaceMap
        _ → acc

-- | For a single tile, find the closest river segment and compute
--   the fluid cell if this tile is within the channel.
--
--   Water surface = terrain surface + water depth.
--   Water depth is derived from flow rate:
--     depth = 1 + floor(flowRate * 3)
--   So a trickle (flow 0.1) gives 1 tile of water,
--   a major river (flow 1.0+) gives 4 tiles.
--
--   We only fill if the tile is within the channel width
--   (not the full valley width — the valley walls are dry).
bestRiverFill ∷ Int → Int → Int → Int → Word64 → [RiverSegment]
              → Maybe FluidCell
bestRiverFill worldSize gx gy surfZ meanderSeed segments =
    let results = map (riverFillFromSegment worldSize gx gy surfZ meanderSeed) segments
        -- Pick the segment that gives the deepest water (closest/widest)
        best = foldl' pickBestFill Nothing results
    in best

pickBestFill ∷ Maybe FluidCell → Maybe FluidCell → Maybe FluidCell
pickBestFill Nothing b = b
pickBestFill a Nothing = a
pickBestFill (Just a) (Just b) =
    if fcSurface b > fcSurface a then Just b else Just a

-- | Compute river fluid for a single tile from a single segment.
--
--   KEY FIX: The water surface is derived from the segment's
--   own endpoint elevations (which are monotonically decreasing
--   from source to mouth), NOT from the local terrain.
--
--   The river was traced downhill on the base plate elevation.
--   Then carveFromSegment applied a relative carve. But other
--   timeline events (volcanism, erosion, later rivers) can
--   alter the terrain after the carve, creating humps in the
--   channel. If we derive water level from the local terrain,
--   water follows those humps uphill.
--
--   Instead: interpolate the water surface between the segment
--   start and end elevations (the original walkDownhill values),
--   minus the channel depth (the carve). This gives a smooth,
--   monotonically decreasing water surface that follows the
--   river's intended path regardless of what happened to the
--   terrain afterward.
--
--   If the terrain at a tile is ABOVE the interpolated water
--   surface, that tile is dry (the river was blocked — a future
--   RiverDam event could create a lake there). If the terrain
--   is BELOW the water surface, we fill with water up to the
--   interpolated level.
riverFillFromSegment ∷ Int → Int → Int → Int → Word64 → RiverSegment
                     → Maybe FluidCell
riverFillFromSegment worldSize gx gy surfZ _meanderSeed seg =
    let GeoCoord sx sy = rsStart seg
        GeoCoord ex ey = rsEnd seg

        -- Vector from start to end
        dx' = fromIntegral (wrappedDeltaForFluid worldSize ex sx) ∷ Float
        dy' = fromIntegral (ey - sy) ∷ Float
        segLen2 = dx' * dx' + dy' * dy'

    in if segLen2 < 1.0
       then Nothing  -- degenerate segment
       else
       let -- Vector from start to tile
           px = fromIntegral (wrappedDeltaForFluid worldSize gx sx) ∷ Float
           py = fromIntegral (gy - sy) ∷ Float

           -- Project onto segment: t in [0,1]
           t = max 0.0 (min 1.0 ((px * dx' + py * dy') / segLen2))

           -- Closest point on segment
           closestX = t * dx'
           closestY = t * dy'

           -- Perpendicular distance
           perpX = px - closestX
           perpY = py - closestY
           perpDist = sqrt (perpX * perpX + perpY * perpY)

           -- Channel half-width
           channelHalfW = fromIntegral (rsWidth seg) / 2.0 ∷ Float

       in if perpDist > channelHalfW
          then Nothing  -- outside the channel
          else
          let -- Water depth from flow rate
              flow = rsFlowRate seg
              waterDepth = 1 + floor (flow * 3.0) ∷ Int

              -- INTERPOLATED water surface from segment geometry.
              -- The segment stores start/end elevations from the
              -- original walkDownhill path. These are guaranteed
              -- to be monotonically decreasing.
              --
              -- Water surface = interpolated base elevation
              --               - channel depth (the carve)
              --               + water depth (the fill)
              --
              -- This is the elevation the water SHOULD be at,
              -- independent of what the terrain actually looks
              -- like after all timeline events.
              startElev = rsStartElev seg
              endElev   = rsEndElev seg
              interpElev = fromIntegral startElev
                         + t * (fromIntegral endElev - fromIntegral startElev)
              channelFloor = interpElev - fromIntegral (rsDepth seg)
              waterSurface = round channelFloor + waterDepth

          in if surfZ ≥ waterSurface
             then Nothing  -- terrain is above water, tile is dry
                           -- (volcanic deposit blocked the river)
             else Just (FluidCell River waterSurface)

-- | Is this feature an active lake and close enough to affect this chunk?
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

-- | Fill a lake basin into the fluid map.
--   Uses the same spillway approach as fillPool (lava):
--   sample the rim at lkRadius distance, find the lowest
--   point, clamp the lake surface to that level.
--
--   Lake surface elevation = base terrain at center + lkSurface.
--   lkSurface is relative (set by moraine height or dam height
--   during world gen). We add it to the actual terrain elevation
--   at the lake center to get the absolute water level.
fillLakeFromFeature ∷ PersistentFeature → Word64 → [TectonicPlate]
                    → Int → Int → Int
                    → VU.Vector Int
                    → HM.HashMap (Int, Int) FluidCell
                    → HM.HashMap (Int, Int) FluidCell
fillLakeFromFeature pf seed plates worldSize chunkGX chunkGY surfaceMap acc =
    case pfFeature pf of
        HydroShape (LakeFeature lk) →
            let GeoCoord fx fy = lkCenter lk
                poolRadius = lkRadius lk
                -- Base terrain elevation at the lake center
                (baseElev, _) = elevationAtGlobal seed plates worldSize fx fy
                -- Lake surface is relative to the terrain at center
                -- (e.g., moraine height above the glacial valley floor)
                lakeSurface = baseElev + lkSurface lk
            in fillLakePool seed plates worldSize chunkGX chunkGY
                   fx fy poolRadius lakeSurface surfaceMap acc
        _ → acc

-- | Fill a circular lake basin into the fluid map.
--   Nearly identical to fillPool for lava, but produces
--   FluidCell Lake instead of FluidCell Lava.
--
--   The spillway clamp ensures physically correct behavior:
--   water finds its own level and can't be higher than the
--   lowest escape point on the basin rim.
fillLakePool ∷ Word64 → [TectonicPlate] → Int → Int → Int
             → Int → Int → Int → Int
             → VU.Vector Int
             → HM.HashMap (Int, Int) FluidCell
             → HM.HashMap (Int, Int) FluidCell
fillLakePool seed plates worldSize chunkGX chunkGY fx fy poolRadius lakeSurface surfaceMap acc =
    let pr = fromIntegral poolRadius ∷ Float
        rimSamples = 32 ∷ Int

        -- Sample the rim at poolRadius distance to find the spillway.
        -- Uses surfaceMap for in-chunk points, elevationAtGlobal for
        -- out-of-chunk points — same fallback pattern as fillPool.
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
       -- If the lake would be at or below sea level, it's just ocean.
       -- Don't create lake fluid here — let computeChunkFluid handle it.
       then acc
       else VU.ifoldl' (\acc' idx surfZ →
            let lx = idx `mod` chunkSize
                ly = idx `div` chunkSize
                gx = chunkGX + lx
                gy = chunkGY + ly
                dx = fromIntegral (wrappedDeltaForFluid worldSize gx fx) ∷ Float
                dy = fromIntegral (gy - fy) ∷ Float
                dist = sqrt (dx * dx + dy * dy)
            in if dist < pr ∧ surfZ < clampedSurface
               then HM.insert (lx, ly) (FluidCell Lake clampedSurface) acc'
               else acc'
            ) acc surfaceMap

-- | Is this feature active and close enough to affect this chunk?
isNearbyActive ∷ Int → Int → Int → PersistentFeature → Bool
isNearbyActive worldSize chunkGX chunkGY pf =
    case pfFeature pf of
        HydroShape _ → False
        VolcanicShape _ → case pfActivity pf of
            FActive    → checkRange
            FCollapsed → checkRange  -- calderas still have lava
            _          → False
  where
    checkRange = let maxR = getFeatureRadius (pfFeature pf)
                     GeoCoord fx fy = getFeatureCenter (pfFeature pf)
                     dx = abs (wrappedDeltaForFluid worldSize chunkGX fx)
                     dy = abs (chunkGY - fy)
                 in dx < maxR + chunkSize ∧ dy < maxR + chunkSize

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
                    → VU.Vector Int
                    → HM.HashMap (Int, Int) FluidCell
                    → HM.HashMap (Int, Int) FluidCell
fillLavaFromFeature pf seed plates worldSize chunkGX chunkGY surfaceMap acc =
    case (pfFeature pf) of
        VolcanicShape (SuperVolcano p) →
            let (fx, fy) = let GeoCoord x y = svCenter p in (x, y)
                poolRadius = svCalderaRadius p
                (baseElev, _) = elevationAtGlobal seed plates worldSize fx fy
                -- Lava fills the caldera bowl, up near the rim
                lavaSurface = baseElev + svRimHeight p - 5
            in fillPool seed plates worldSize chunkGX chunkGY fx fy
                   poolRadius lavaSurface surfaceMap acc

        VolcanicShape (Caldera p) →
            let (fx, fy) = let GeoCoord x y = caCenter p in (x, y)
                poolRadius = caInnerRadius p
                (baseElev, _) = elevationAtGlobal seed plates worldSize fx fy
                lavaSurface = baseElev + caRimHeight p - 3
            in fillPool seed plates worldSize chunkGX chunkGY fx fy
                   poolRadius lavaSurface surfaceMap acc

        VolcanicShape (ShieldVolcano p) | shSummitPit p →
            let (fx, fy) = let GeoCoord x y = shCenter p in (x, y)
                poolRadius = shPitRadius p
                (baseElev, _) = elevationAtGlobal seed plates worldSize fx fy
                lavaSurface = baseElev + shPeakHeight p - shPitDepth p + 2
            in fillPool seed plates worldSize chunkGX chunkGY fx fy
                   poolRadius lavaSurface surfaceMap acc

        VolcanicShape (CinderCone p) →
            let (fx, fy) = let GeoCoord x y = ccCenter p in (x, y)
                poolRadius = ccCraterRadius p
                (baseElev, _) = elevationAtGlobal seed plates worldSize fx fy
                lavaSurface = baseElev + ccPeakHeight p - ccCraterDepth p + 2
            in fillPool seed plates worldSize chunkGX chunkGY fx fy
                   poolRadius lavaSurface surfaceMap acc

        VolcanicShape (FissureVolcano p) | fpHasMagma p →
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
        VolcanicShape (HydrothermalVent p) →
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
         → VU.Vector Int
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
       then acc
       else VU.ifoldl' (\acc' idx surfZ →
            let lx = idx `mod` chunkSize
                ly = idx `div` chunkSize
                gx = chunkGX + lx
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
                → VU.Vector Int
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
       then acc
           else VU.ifoldl' (\acc' idx surfZ →
               let lx = idx `mod` chunkSize
                   ly = idx `div` chunkSize
                   gx = chunkGX + lx
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
