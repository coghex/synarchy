{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Ocean-floor (seabed) generation. Runs ONCE at world init on the
--   stitched terrain and produces a 'SeabedTable' of per-chunk
--   elevation deltas + material overrides; chunk gen applies it the
--   same way it applies the coastal table.
--
--   Why this exists: the tile-resolution priority flood identifies
--   semi-enclosed sub-sea areas as clamped lakes and carved their
--   whole floor to exactly @seaLevel − 1@ (see the old
--   'buildLakeCarveIndex'), producing vast dead-flat plains with a
--   hard edge against land and against the natural deep ocean. This
--   pass replaces that with a real continental margin:
--
--     * RELIEF — the seabed BLENDS a depth-from-shore PROFILE (gentle
--       continental shelf → shelf break → gradient-limited continental
--       slope, plus gentle value noise) with the natural floor. Near
--       shore the profile dominates, so shoals, ridges and clamped-
--       lake flats are smoothed into the shelf+slope and there are no
--       flank cliffs; in the deep abyss the natural floor dominates,
--       so deep features survive. The profile reaches abyssal depth by
--       the blend handoff, so it is seamless.
--     * MATERIALS — sand across the shelf + upper slope, muck once it
--       gets deep (no loam/silt bands).
--     * OUTCROPS — sparse bedrock; a stronger threshold also shoals
--       the floor up into a reef.
--
--   The profile covers clamped lakes too: a small land-rimmed pond has
--   tiny dist-from-shore → shallow profile (stays a shallow pond, ≤
--   @seaLevel − 1@ so it fills); a sea-surrounded clamped basin has
--   large dist → deep profile (merges into the sea, no shoal cliff).
--   This supersedes the old flat lake carve.
module World.Fluid.Seabed
    ( identifySeabed
    , applySeabedTable
    ) where

import UPrelude
import Control.Monad.ST (runST)
import Data.STRef
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Constants (seaLevel)
import World.Fluid.Lake.Types
    (WorldLakes(..), Lake(..), LakeChunkEntry(..))
import World.Fluid.Seabed.Types (SeabedTable(..))
import World.Fluid.Internal (wrapChunkCoordU)
import World.Generate.Constants (chunkBorder)
import World.Generate.Coordinates (chunkToGlobal)
import World.Material (MaterialId(..), matBasalt, matSand
                      , matMuck, matGlacier)
import World.Ocean.Types (OceanDistMap, oceanDistAt)
import World.Plate (wrappedValueNoise2D)

-- * Tunables — continental-margin profile
--
-- The seabed is a depth-from-shore PROFILE (gentle shelf → shelf break
-- → gradient-limited continental slope) BLENDED with the natural floor
-- by distance from shore: profile near shore, natural in the deep
-- abyss. The blend is what removes the straight shelf-break cliff —
-- the margin becomes one smooth surface instead of a flat shelf
-- meeting a natural plunge, and the blend reaches abyssal depth before
-- handing off, so deep features survive without a seam.

-- | Width of the gentle continental SHELF (tiles from shore) before
--   the shelf break.
shelfWidth ∷ Int
shelfWidth = 16

-- | Depth (tiles below sea) at the shelf break — the bottom of the
--   gentle shelf, top of the steeper continental slope.
shelfBreakDepth ∷ Int
shelfBreakDepth = 13

-- | Continental-slope gradient past the shelf break, as num/den tiles
--   of depth per tile of distance. 5/2 = 2.5 — steep enough to read as
--   a slope, gentle enough never to look like a cliff.
contSlopeNum, contSlopeDen ∷ Int
contSlopeNum = 5
contSlopeDen = 2

-- | Cap on profile depth. Set below 'minBound' concerns but DEEPER
--   than the deepest natural abyss (~−127) so the slope always reaches
--   past the natural floor and hands off to it (no fill in the true
--   abyss).
profileDepthCap ∷ Int
profileDepthCap = 160

-- | BFS cap on shore distance — large enough that the profile reaches
--   'profileDepthCap' (≈ shelfWidth + (cap−break)·den/num) so deep
--   open ocean far from shore resolves to the natural floor.
shoreDistCap ∷ Int
shoreDistCap = 80

-- | Profile→natural blend window (distance from shore, tiles). Within
--   'blendFullDist' the seabed IS the smooth profile (shoals erased
--   into the shelf/slope → no flank cliffs); past 'blendNatDist' it is
--   the natural floor (deep abyssal features survive); a linear blend
--   between. By @blendNatDist@ the profile has descended to roughly
--   abyssal depth, so the two agree and the blend is seamless.
blendFullDist, blendNatDist ∷ Int
blendFullDist = 44
blendNatDist  = 68

-- | Depth at/above which the seabed is SAND (the whole shelf and upper
--   slope). Below it, muck. (User spec: all sand until it gets deep,
--   then muck — no loam/silt bands.)
sandMaxDepth ∷ Int
sandMaxDepth = 14

-- | Gentle seabed undulation: value-noise feature size (tiles) and
--   peak extra depth it adds on top of the profile.
seabedNoiseScale ∷ Int
seabedNoiseScale = 8

seabedNoiseAmp ∷ Int
seabedNoiseAmp = 4

-- | Outcrops: small-feature noise, a high threshold for a bedrock
--   patch and a higher one for a patch that also shoals up. Bump
--   height is small so reefs read as outcrops, not pillars.
outcropNoiseScale ∷ Int
outcropNoiseScale = 3

outcropMatThreshold ∷ Float
outcropMatThreshold = 0.93

outcropBumpThreshold ∷ Float
outcropBumpThreshold = 0.965

outcropBumpHeight ∷ Int
outcropBumpHeight = 6

-- | Continental-margin depth (tiles below sea) at a given distance
--   from shore: gentle shelf, then a constant-gradient slope.
profileDepth ∷ Int → Int
profileDepth dist
    | dist ≤ shelfWidth = (dist * shelfBreakDepth) `div` shelfWidth
    | otherwise = min profileDepthCap
        (shelfBreakDepth
         + ((dist - shelfWidth) * contSlopeNum) `div` contSlopeDen)

-- * Identification

identifySeabed
    ∷ Word64
    → Int                  -- ^ worldSize (chunks per side)
    → OceanDistMap         -- ^ chunk-level ocean classifier
    → WorldLakes           -- ^ final lakes (for clamped sub-sea basins)
    → VU.Vector Int        -- ^ stitched world terrain (pre-lake-carve)
    → SeabedTable
identifySeabed seed worldSize oceanDist worldLakes terrain =
    let worldTiles = worldSize * chunkSize
        halfWorld  = worldTiles `div` 2
        halfChunks = worldSize `div` 2
        chunkArea  = chunkSize * chunkSize
        worldIdx gx gy = (gy + halfWorld) * worldTiles + (gx + halfWorld)

        -- A tile is sub-sea if it is below-sea ocean: the chunk-ocean
        -- render test (terrain ≤ seaLevel ∧ chunk oceanic) OR the
        -- tile-level edge-connected ocean BFS (worldOcean — catches
        -- below-sea tiles connected to the open sea whose chunk the
        -- coarse oceanic test misses, so the profile fills them
        -- consistently and they don't become dry pits beside filled
        -- seabed) OR a clamped (sub-sea-surface) lake basin.
        subSea = buildSubSeaMask worldSize oceanDist worldLakes terrain
        distField = shoreDistField worldTiles
                        (VU.map (≠ 0) subSea)

        chunkCoords = [ ChunkCoord cx cy
                      | cx ← [-halfChunks .. halfChunks - 1]
                      , cy ← [-halfChunks .. halfChunks - 1] ]

        -- Pre-shave seabed elevation per sub-sea tile: BLEND the
        -- smooth continental-margin profile (near shore) with the
        -- natural floor (deep abyss). In the margin the profile wins,
        -- so the shelf+slope is smooth; in the abyss the natural floor
        -- wins, so deep features survive; the handoff is seamless
        -- because the profile has reached abyssal depth by then.
        -- Same profile covers clamped lakes (small land-rimmed pond →
        -- tiny dist → shallow; sea-surrounded basin → large dist →
        -- deep, merges into the sea). Outcrop shoals up locally.
        baseElevAt gx gy =
            let idx  = worldIdx gx gy
                nat  = terrain VU.! idx
                dist = distField VU.! idx
                ndep = round (wrappedValueNoise2D (seed `xor` 0x5EAB)
                                 worldSize gx gy seabedNoiseScale
                              * fromIntegral seabedNoiseAmp)
                isBump = wrappedValueNoise2D (seed `xor` 0x0C20)
                             worldSize gx gy outcropNoiseScale
                         ≥ outcropBumpThreshold
                profElev = seaLevel - 1 - profileDepth dist - ndep
                w | dist ≤ blendFullDist = 256
                  | dist ≥ blendNatDist  = 0
                  | otherwise =
                      (256 * (blendNatDist - dist))
                      `div` (blendNatDist - blendFullDist)
                blended = (w * profElev + (256 - w) * nat) `div` 256
                sf = if isBump then blended + outcropBumpHeight else blended
            in min (seaLevel - 1) sf

        -- Materialised seabed elevation grid (minBound where not
        -- sub-sea), so the peak-shave below can read neighbours.
        baseGrid = VU.generate (worldTiles * worldTiles) $ \idx →
            if subSea VU.! idx ≡ 0 ∨ terrain VU.! idx ≡ minBound
            then minBound
            else let gy = idx `div` worldTiles - halfWorld
                     gx = idx `mod` worldTiles - halfWorld
                 in baseElevAt gx gy

        -- Shallow banks seed the distance field too (see
        -- 'shoreDistField'), so the profile skirts them — no separate
        -- shave pass needed.
        shavedGrid = baseGrid

        entryFor coord@(ChunkCoord cx cy) =
            let elevs = VU.generate chunkArea $ \i →
                    let lx = i `mod` chunkSize
                        ly = i `div` chunkSize
                        gx = cx * chunkSize + lx
                        gy = cy * chunkSize + ly
                        gi = worldIdx gx gy
                    in if shavedGrid VU.! gi ≡ minBound
                       then 0
                       else shavedGrid VU.! gi - terrain VU.! gi
                mats = VU.generate chunkArea $ \i →
                    let lx = i `mod` chunkSize
                        ly = i `div` chunkSize
                        gx = cx * chunkSize + lx
                        gy = cy * chunkSize + ly
                        gi = worldIdx gx gy
                        elev = shavedGrid VU.! gi
                    in if elev ≡ minBound
                       then 0   -- not sub-sea: no material override
                       else
                         let isMat = wrappedValueNoise2D (seed `xor` 0x0C20)
                                         worldSize gx gy outcropNoiseScale
                                     ≥ outcropMatThreshold
                         in if isMat then unMaterialId matBasalt
                            else seabedMatForDepth (seaLevel - elev)
            in (coord, elevs, mats)

        entries = map entryFor chunkCoords
    in SeabedTable
        { sbElevDelta   = HM.fromList
            [ (cc, dv) | (cc, dv, _) ← entries, VU.any (≠ 0) dv ]
        , sbMatOverride = HM.fromList
            [ (cc, mv) | (cc, _, mv) ← entries, VU.any (≠ 0) mv ]
        }

-- | Seabed surface material by depth below sea level: sand across the
--   whole shelf and upper slope, muck once it gets deep. (User spec —
--   no loam/silt transition bands.)
seabedMatForDepth ∷ Int → Word8
seabedMatForDepth depth
    | depth ≤ sandMaxDepth = unMaterialId matSand   -- shelf + upper slope
    | otherwise            = unMaterialId matMuck   -- deep

-- | A chunk renders ocean if it or a cardinal neighbour has
--   ocean-distance 0 — the same test 'composeFluidMap' uses
--   ('chunkOrNeighborOceanic').
chunkOceanic ∷ Int → OceanDistMap → ChunkCoord → Bool
chunkOceanic worldSize oceanDist (ChunkCoord cx cy) =
    let check cc = oceanDistAt oceanDist (wrapChunkCoordU worldSize cc) ≡ 0
    in check (ChunkCoord cx cy)
       ∨ check (ChunkCoord (cx + 1) cy) ∨ check (ChunkCoord (cx - 1) cy)
       ∨ check (ChunkCoord cx (cy + 1)) ∨ check (ChunkCoord cx (cy - 1))

-- | Sub-sea classification: 0 = not sub-sea, 1 = OPEN SEA (tiles
--   'composeFluidMap' renders as ocean: terrain ≤ seaLevel ∧ chunk
--   oceanic, plus the world-edge ocean BFS), 2 = small CLAMPED LAKE
--   (sub-sea-surface lake tiles not already open sea). The split lets
--   the open sea get the full depth ramp while small ponds get only a
--   flat shallow fill (the ramp would dig them into pits).
buildSubSeaMask
    ∷ Int → OceanDistMap → WorldLakes → VU.Vector Int
    → VU.Vector Word8
buildSubSeaMask worldSize oceanDist worldLakes terrain = runST $ do
    let worldTiles = worldSize * chunkSize
        halfWorld  = worldTiles `div` 2
        halfChunks = worldSize `div` 2
    m ← VUM.replicate (worldTiles * worldTiles) (0 ∷ Word8)
    -- Open-sea tiles (matches composeFluidMap's ocean branch),
    -- computed per chunk so the oceanic test runs once per chunk.
    forM_ [ ChunkCoord cx cy
          | cx ← [-halfChunks .. halfChunks - 1]
          , cy ← [-halfChunks .. halfChunks - 1] ] $ \coord@(ChunkCoord cx cy) →
        when (chunkOceanic worldSize oceanDist coord) $
            forM_ [0 .. chunkSize * chunkSize - 1] $ \li → do
                let lx    = li `mod` chunkSize
                    ly    = li `div` chunkSize
                    gxOff = cx * chunkSize + lx + halfWorld
                    gyOff = cy * chunkSize + ly + halfWorld
                when (gxOff ≥ 0 ∧ gxOff < worldTiles
                      ∧ gyOff ≥ 0 ∧ gyOff < worldTiles) $ do
                    let idx = gyOff * worldTiles + gxOff
                        t   = terrain VU.! idx
                    when (t ≠ minBound ∧ t ≤ seaLevel) $ VUM.write m idx 1
    -- Clamped sub-sea lakes (surface ≤ seaLevel). Marked 2 only where
    -- not already open sea, so chunk-oceanic basins keep full ramp.
    forM_ (HM.toList (wlByChunk worldLakes)) $ \(ChunkCoord cx cy, entries) →
        V.forM_ entries $ \entry → do
            let lake = wlLakes worldLakes V.! lceLakeId entry
            when (lkSurface lake ≤ seaLevel) $ do
                let bm = lceBitmask entry
                forM_ [0 .. chunkSize * chunkSize - 1] $ \li →
                    when (bm VU.! li) $ do
                        let lx    = li `mod` chunkSize
                            ly    = li `div` chunkSize
                            gxOff = cx * chunkSize + lx + halfWorld
                            gyOff = cy * chunkSize + ly + halfWorld
                        when (gxOff ≥ 0 ∧ gxOff < worldTiles
                              ∧ gyOff ≥ 0 ∧ gyOff < worldTiles) $ do
                            let idx = gyOff * worldTiles + gxOff
                            cur ← VUM.read m idx
                            when (cur ≡ 0) $ VUM.write m idx 2
    VU.unsafeFreeze m

-- | Distance (tiles) from the coastline, for every sub-sea tile. Seeds
--   (distance 1) are sub-sea tiles that touch land or the world edge;
--   multi-source BFS expands through sub-sea only. Non-sub-sea reads 0;
--   unreached interior (seas larger than the cap) clamps to
--   'shoreDistCap' so it resolves to the natural abyss.
shoreDistField ∷ Int → VU.Vector Bool → VU.Vector Int
shoreDistField worldTiles subSea = runST $ do
    let n = worldTiles * worldTiles
    dist ← VUM.replicate n 0
    frontierRef ← newSTRef []
    -- Seed: sub-sea tiles touching land/edge, OR shallow banks.
    forM_ [0 .. n - 1] $ \i → when (subSea VU.! i) $ do
        let bx = i `mod` worldTiles
            by = i `div` worldTiles
            landN =
                (bx ≡ 0)              ∨ (bx ≡ worldTiles - 1) ∨
                (by ≡ 0)              ∨ (by ≡ worldTiles - 1) ∨
                (bx > 0              ∧ not (subSea VU.! (i - 1))) ∨
                (bx < worldTiles - 1 ∧ not (subSea VU.! (i + 1))) ∨
                (by > 0              ∧ not (subSea VU.! (i - worldTiles))) ∨
                (by < worldTiles - 1 ∧ not (subSea VU.! (i + worldTiles)))
        when landN $ do
            VUM.write dist i 1
            modifySTRef' frontierRef (i :)
    let bfs level = when (level < shoreDistCap) $ do
            frontier ← readSTRef frontierRef
            if null frontier then pure () else do
                nextRef ← newSTRef []
                forM_ frontier $ \i → do
                    let bx = i `mod` worldTiles
                        by = i `div` worldTiles
                        tryN ok j = when ok $ do
                            when (subSea VU.! j) $ do
                                d ← VUM.read dist j
                                when (d ≡ 0) $ do
                                    VUM.write dist j (level + 1)
                                    modifySTRef' nextRef (j :)
                    tryN (bx > 0)              (i - 1)
                    tryN (bx < worldTiles - 1) (i + 1)
                    tryN (by > 0)              (i - worldTiles)
                    tryN (by < worldTiles - 1) (i + worldTiles)
                next ← readSTRef nextRef
                writeSTRef frontierRef next
                bfs (level + 1)
    bfs 1
    -- Sub-sea tiles never reached (interior of seas larger than the
    -- cap) clamp to the cap so their ramp is fully deep.
    forM_ [0 .. n - 1] $ \i → when (subSea VU.! i) $ do
        d ← VUM.read dist i
        when (d ≡ 0) (VUM.write dist i shoreDistCap)
    VU.unsafeFreeze dist

-- * Application (chunk gen)

-- | Apply the seabed table to one chunk's BORDERED post-coastal
--   vectors (after the first despike, before river carve). Border
--   tiles read their owning chunk's entry — the cross-chunk lookup
--   convention shared with the coastal / carve tables — so adjacent
--   chunks always agree.
applySeabedTable
    ∷ SeabedTable
    → ChunkCoord
    → (VU.Vector Int, VU.Vector MaterialId)
    → (VU.Vector Int, VU.Vector MaterialId)
applySeabedTable table coord (elevVec, matVec) =
    let borderSize = chunkSize + 2 * chunkBorder
        borderArea = borderSize * borderSize
        fromIndex idx =
            let (by, bx) = idx `divMod` borderSize
            in (bx - chunkBorder, by - chunkBorder)
        lookupTile gx gy =
            let cx = gx `div` chunkSize
                cy = gy `div` chunkSize
                ilx = ((gx `mod` chunkSize) + chunkSize) `mod` chunkSize
                ily = ((gy `mod` chunkSize) + chunkSize) `mod` chunkSize
                li  = ily * chunkSize + ilx
                d   = case HM.lookup (ChunkCoord cx cy) (sbElevDelta table) of
                        Just dv → dv VU.! li
                        Nothing → 0
                m   = case HM.lookup (ChunkCoord cx cy) (sbMatOverride table) of
                        Just mv → mv VU.! li
                        Nothing → 0
            in (d, m)
        elev' = VU.generate borderArea $ \idx →
            let z = elevVec VU.! idx
            in if z ≡ minBound
               then z
               else
                 let (lx, ly) = fromIndex idx
                     (gx, gy) = chunkToGlobal coord lx ly
                     (d, _)   = lookupTile gx gy
                 in z + d
        mat' = VU.generate borderArea $ \idx →
            let m0 = matVec VU.! idx
                (lx, ly) = fromIndex idx
                (gx, gy) = chunkToGlobal coord lx ly
                (_, mo)  = lookupTile gx gy
            in if mo ≡ 0 ∨ m0 ≡ matGlacier
               then m0
               else MaterialId mo
    in (elev', mat')
