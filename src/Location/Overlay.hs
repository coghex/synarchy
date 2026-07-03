{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | World-gen location overlay placement pass (#89).
--
--   Given the finalised generation params (seed, plates, ocean map)
--   and the loaded 'LocationDef's, deterministically chooses which
--   chunks host which locations. The result is a sparse
--   'LocationOverlay' (chunk → location id) stored in
--   'World.Generate.Types.WorldGenParams', so it rides the save and a
--   loaded world keeps its layout.
--
--   Design notes:
--
--     * /Coarse pass./ Suitability is judged from the plate-based
--       elevation function ('elevationAtGlobal') sampled at five points
--       per chunk — the same cheap, chunk-independent signal the ocean
--       flood fill uses ('World.Fluid.Ocean'). No chunk needs to exist
--       yet, so the pass fits naturally at the end of world init.
--
--     * /Adaptive thresholds./ "flat" / "mountain" / "highland" /
--       "lowland" are resolved against percentiles of the world's own
--       land elevations, so the same anchor tags behave sensibly at any
--       world size without hand-tuned absolute constants.
--
--     * /Deterministic./ Candidate ordering is a pure hash of
--       (seed, location id, chunk) — same seed always yields the same
--       overlay. Spacing is enforced greedily in that hashed order.
module Location.Overlay
    ( computeLocationOverlay
    , ChunkMetrics(..)
    , chunkMetricsAt
    , chunkSeamChebyshev
    ) where

import UPrelude
import Data.List (sort, sortOn)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import World.Chunk.Types (ChunkCoord(..), chunkSize, wrapChunkCoordU)
import World.Plate (TectonicPlate, elevationAtGlobal, isBeyondGlacier)
import World.Material (MaterialId, matGlacier)
import World.Ocean.Types (OceanMap, OceanDistMap, oceanDistAt)
import World.Fluid.Lake.Types (WorldLakes, lakesInChunk)
import World.Fluid.River.Types (WorldRivers, riversInChunk)
import World.Constants (seaLevel)
import Location.Types (LocationDef(..))
import Location.Overlay.Types (LocationOverlay, emptyLocationOverlay)

-- | Coarse per-chunk terrain summary used for anchor matching.
data ChunkMetrics = ChunkMetrics
    { cmMedianElev ∷ !Int  -- ^ median of the 5 sampled elevations
    , cmElevRange  ∷ !Int  -- ^ max − min sampled elevation (flatness)
    , cmOceanDist  ∷ !Int  -- ^ BFS distance from ocean (0 = ocean)
    } deriving (Show, Eq)

-- | Elevation at a chunk's centre + 4 corners (median-stable sampling,
--   mirroring 'World.Fluid.Ocean'\'s @chunkElev@).
sampleElevs ∷ Word64 → [TectonicPlate] → Int → ChunkCoord → [(Int, MaterialId)]
sampleElevs seed plates worldSize (ChunkCoord cx cy) =
    let bx = cx * chunkSize
        by = cy * chunkSize
        c  = chunkSize `div` 2
        pts = [ (bx + c, by + c)
              , (bx, by)
              , (bx + chunkSize - 1, by)
              , (bx, by + chunkSize - 1)
              , (bx + chunkSize - 1, by + chunkSize - 1) ]
    in map (\(gx, gy) → elevationAtGlobal seed plates worldSize gx gy) pts

-- | Coarse terrain metrics for one chunk — the plate-based elevation
--   summary the suitability scan reads. Pure and chunk-independent.
chunkMetricsAt ∷ Word64 → [TectonicPlate] → Int → OceanDistMap → ChunkCoord → ChunkMetrics
chunkMetricsAt seed plates worldSize oceanDist coord =
    let elevs  = map fst (sampleElevs seed plates worldSize coord)
        sorted = sort elevs
        elevRange = case sorted of
            lo : _ → last sorted - lo
            []     → 0  -- unreachable: sampleElevs always returns 5 points
    in ChunkMetrics
         { cmMedianElev = sorted !! 2
         , cmElevRange  = elevRange
         , cmOceanDist  = oceanDistAt oceanDist coord
         }

-- | Elevation percentile cut-offs derived from the world's land chunks.
data Cuts = Cuts
    { flatCut     ∷ !Int  -- ^ elev-range at/below this reads as "flat"
    , mountainCut ∷ !Int  -- ^ median elev at/above this reads as "mountain"
    , highlandCut ∷ !Int
    , lowlandCut  ∷ !Int
    }

-- | Place every location def into the world. Returns the sparse
--   chunk→id overlay. Empty when no defs are registered (the common
--   headless-dump path), which also short-circuits the per-chunk scan.
computeLocationOverlay
    ∷ Word64          -- ^ world seed
    → Int             -- ^ world size in chunks
    → [TectonicPlate] -- ^ pre-generated plates
    → OceanMap        -- ^ submerged-chunk set
    → OceanDistMap    -- ^ distance-from-ocean per chunk
    → WorldLakes      -- ^ per-chunk lakes
    → WorldRivers     -- ^ per-chunk rivers
    → [LocationDef]   -- ^ registered location defs
    → LocationOverlay
computeLocationOverlay seed worldSize plates oceanMap oceanDist lakes rivers defs
    | null defs = emptyLocationOverlay
    | otherwise = fst (foldl' placeDef (emptyLocationOverlay, HS.empty) defsSorted)
  where
    half = worldSize `div` 2
    -- One candidate per PHYSICAL chunk. The raw square grid double-covers
    -- the seam neighbourhood (a near-seam chunk appears at its canonical
    -- coord AND a u-alias), so an unfiltered scan could place one def
    -- twice on the same physical chunk — and an alias-keyed entry would
    -- never stamp, because chunk loading canonicalises through
    -- 'wrapChunkCoordU' before insert ('World.Thread.ChunkLoading'), so
    -- overlay lookups only ever see canonical coords.
    allCoords = [ coord
                | cx ← [-half .. half - 1], cy ← [-half .. half - 1]
                , let coord = ChunkCoord cx cy
                , wrapChunkCoordU worldSize coord ≡ coord ]

    -- Land chunks (not ocean, not glacier, above sea level) with metrics.
    landMetrics ∷ [(ChunkCoord, ChunkMetrics)]
    landMetrics = [ (coord, cm) | coord ← allCoords
                                , let (isLand, cm) = classify coord
                                , isLand ]

    classify ∷ ChunkCoord → (Bool, ChunkMetrics)
    classify coord@(ChunkCoord cx cy) =
        let cxg = cx * chunkSize + chunkSize `div` 2
            cyg = cy * chunkSize + chunkSize `div` 2
            (_, centerMat) = elevationAtGlobal seed plates worldSize cxg cyg
            cm = chunkMetricsAt seed plates worldSize oceanDist coord
            isLand = centerMat /= matGlacier
                   ∧ not (isBeyondGlacier worldSize cxg cyg)
                   ∧ not (HS.member coord oceanMap)
                   ∧ cmMedianElev cm > seaLevel
        in (isLand, cm)

    cuts ∷ Cuts
    cuts = Cuts
        { flatCut     = pctl rangeList 0.5
        , mountainCut = pctl elevList  0.75
        , highlandCut = pctl elevList  0.6
        , lowlandCut  = pctl elevList  0.4
        }
      where elevList  = sort (map (cmMedianElev . snd) landMetrics)
            rangeList = sort (map (cmElevRange  . snd) landMetrics)

    -- A chunk is too close to water if it (or any of its 8 neighbours)
    -- holds a lake or river, or it sits within one chunk of the ocean.
    -- Locations avoid those: flattening a footprint next to water leaves
    -- the water overhanging the carved rim (#414). A def opts back IN via
    -- a coast anchor.
    --
    -- Every coord is canonicalised through 'wrapChunkCoordU' first, because
    -- the ocean / lake / river tables are keyed by the wrapped coord (see
    -- 'World.Generate.Chunk') — a seam-crossing neighbour read raw would
    -- otherwise miss the water on the far side of the wrap.
    dryEnough ∷ ChunkCoord → Bool
    dryEnough coord@(ChunkCoord cx cy) =
        oceanDistAt oceanDist (wrap coord) ≥ 2
        ∧ all noStandingWater
            [ ChunkCoord (cx + dx) (cy + dy)
            | dx ← [-1, 0, 1], dy ← [-1, 0, 1] ]
      where
        wrap = wrapChunkCoordU worldSize
        noStandingWater c =
            let cc = wrap c
            in V.null (lakesInChunk lakes cc) ∧ V.null (riversInChunk rivers cc)

    defsSorted = sortOn ldId defs

    -- Place one def, threading the accumulating overlay + the set of
    -- chunks already taken by any def (so two locations never collide
    -- on one chunk).
    placeDef ∷ (LocationOverlay, HS.HashSet ChunkCoord)
             → LocationDef
             → (LocationOverlay, HS.HashSet ChunkCoord)
    placeDef (ov, occupied) def = greedy [] ov occupied scored
      where
        lid        = ldId def
        maxCount   = max 0 (ldMaxCount def)
        minSpacing = max 1 (ldMinSpacing def)
        -- Suitable land chunks, ordered by a deterministic per-chunk
        -- hash so the distribution is semi-random (not a grid) yet
        -- stable for a given seed.
        wantWater = wantsWater (ldAnchor def)
        scored = sortOn snd
            [ (coord, scoreFor lid coord)
            | (coord, cm) ← landMetrics
            , anchorOk cuts (ldAnchor def) cm
            , wantWater ∨ dryEnough coord ]

        greedy _      ov' occ [] = (ov', occ)
        greedy placed ov' occ ((coord, _) : rest)
            | length placed ≥ maxCount        = (ov', occ)
            | HS.member coord occ             = greedy placed ov' occ rest
            | any (tooClose coord) placed     = greedy placed ov' occ rest
            | otherwise = greedy (coord : placed)
                                 (HM.insert coord lid ov')
                                 (HS.insert coord occ)
                                 rest
        -- Seam-aware (#422): two chunks just across the u-wrap are
        -- physical neighbours even though their raw coords are half a
        -- world apart.
        tooClose a b = chunkSeamChebyshev worldSize a b < minSpacing

    scoreFor ∷ Text → ChunkCoord → Word64
    scoreFor lid (ChunkCoord cx cy) =
        let s0 = seed `xor` idSalt lid
            h1 = s0 `xor` (fromIntegral cx * 0x517cc1b727220a95)
            h2 = h1 `xor` (fromIntegral cy * 0x6c62272e07bb0142)
            h3 = (h2 `xor` (h2 `shiftR` 33)) * 0xff51afd7ed558ccd
            h4 = (h3 `xor` (h3 `shiftR` 33)) * 0xc4ceb9fe1a85ec53
        in  h4 `xor` (h4 `shiftR` 33)

-- | Chebyshev distance between two chunk coords under the cylindrical
--   u-wrap (the 'wrapChunkCoordU' topology): the minimum raw Chebyshev
--   distance over the three u-alias images of the first coord. Shifting
--   u by ±w moves a coord by (±w/2, ∓w/2) — v = cx + cy is
--   glacier-bounded and never wraps, so one alias step each way covers
--   every canonical pair. Equals the raw distance for interior pairs
--   and for non-wrapping (arena / zero-size) worlds.
chunkSeamChebyshev ∷ Int → ChunkCoord → ChunkCoord → Int
chunkSeamChebyshev worldSize (ChunkCoord ax ay) (ChunkCoord bx by)
    | halfW ≤ 0 = raw 0
    | otherwise = minimum [ raw k | k ← [-1, 0, 1] ]
  where
    halfW = worldSize `div` 2
    raw k = max (abs (ax + k * halfW - bx)) (abs (ay - k * halfW - by))

-- | FNV-1a hash of a location id — a fully deterministic salt (no
--   dependence on hashable's per-run seed) so the overlay is identical
--   across runs and machines.
idSalt ∷ Text → Word64
idSalt = T.foldl' (\acc c → (acc `xor` fromIntegral (fromEnum c)) * 0x100000001b3)
                  0xcbf29ce484222325

-- | True if a def's anchors opt it INTO water proximity (a coast / shore /
--   waterside location), so the dry-ground filter (#414) is skipped for it.
--   @coast@/@coastal@ also constrain to the ocean shore; @waterside@ just
--   tolerates nearby water without any other terrain requirement. Every
--   other location keeps clear of lakes / rivers / the ocean shore.
wantsWater ∷ [Text] → Bool
wantsWater = any (`elem` (["coast", "coastal", "waterside"] ∷ [Text]))

-- | Does a chunk satisfy ALL of a def's anchor tags? Unknown tags
--   impose no constraint (a soft no-op; #88 only ships the "flat" tag,
--   and removing this would let one typo'd tag suppress all placement).
anchorOk ∷ Cuts → [Text] → ChunkMetrics → Bool
anchorOk cuts tags cm = all ok tags
  where
    ok tag = case tag of
        "flat"     → cmElevRange  cm ≤ flatCut cuts
        "mountain" → cmMedianElev cm ≥ mountainCut cuts
        "highland" → cmMedianElev cm ≥ highlandCut cuts
        "lowland"  → cmMedianElev cm ≤ lowlandCut cuts
        "coast"    → cmOceanDist  cm ≡ 1
        "coastal"  → cmOceanDist  cm ≡ 1
        "inland"   → cmOceanDist  cm ≥ 4
        _          → True

-- | p-quantile of a pre-sorted list (0 for the empty list).
pctl ∷ [Int] → Double → Int
pctl [] _ = 0
pctl xs p =
    let n = length xs
        i = min (n - 1) (max 0 (floor (p * fromIntegral n)))
    in xs !! i
