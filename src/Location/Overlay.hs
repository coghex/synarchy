{-# LANGUAGE Strict #-}
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
--
--     * /Guaranteed placement (#997)./ The strict pass above can reject
--       every land chunk, leaving a world with no locations at all —
--       which makes the expedition arc unplayable on that save. When
--       that happens (and only then) a single fallback placement is
--       chosen deterministically; see 'guaranteedEntry'.
module Location.Overlay
    ( computeLocationOverlay
    , computeLocationPlacement
    , LocationPlacement(..)
    , PlacementOutcome(..)
    , ChunkMetrics(..)
    , chunkMetricsAt
    -- * Anchor semantics
    --
    -- | Exported for the always-blocking #1681 gate, which pins one
    --   expectation per 'Location.Anchor.LocationAnchor' constructor.
    --   Driving them through 'computeLocationOverlay' alone cannot do
    --   that: a generated world exercises whichever tags its own
    --   terrain happens to satisfy, which is how the pre-#1681 tree
    --   shipped six of the eight constructors with no direct coverage
    --   at all.
    , Cuts(..)
    , anchorOk
    , wantsWater
    ) where

import UPrelude
import Data.List (sort, sortOn)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import World.Chunk.Types (ChunkCoord(..), chunkSize, wrapChunkCoordU, chunkSeamChebyshev)
import World.Plate (TectonicPlate, elevationAtGlobal, isBeyondGlacier)
import World.Material (MaterialId, matGlacier)
import World.Ocean.Types (OceanMap, OceanDistMap, oceanDistAt)
import World.Fluid.Lake.Types (WorldLakes, lakesInChunk)
import World.Fluid.River.Types (WorldRivers, riversInChunk)
import World.Constants (seaLevel)
import Location.Anchor (LocationAnchor(..))
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

-- | Why the placement pass produced the overlay it did (#997). The
--   overlay alone cannot say: an empty one has four distinct causes and
--   only one of them ("this world has no land") is a legitimate
--   no-location world the player should be told about.
data PlacementOutcome
    = PlacedStrict
      -- ^ The strict pass placed at least one location. The overlay is
      --   exactly what it has always been; the guarantee never ran.
    | PlacedGuaranteed
      -- ^ The strict pass rejected every land chunk, so the guarantee
      --   placed exactly one location ('guaranteedEntry').
    | NoPlaceableDefinitions
      -- ^ Nothing to place: no definitions registered (the common
      --   headless-dump path) or every registered definition is
      --   authored @max_count: 0@. The guarantee deliberately does NOT
      --   fire here — an authored "do not place" is not a generation
      --   failure, and a def-less world must keep yielding an empty
      --   overlay or the tracked world-gen baselines shift. Reported
      --   only for a world that HOLDS land; see 'NoLand'.
    | NoLand
      -- ^ The world holds no land chunk at all, so no placement of any
      --   kind is possible. The explicit no-location result: callers
      --   surface this rather than reporting a successful generation.
      --
      --   Takes precedence over 'NoPlaceableDefinitions' (#1414): when
      --   BOTH hold, a landless world reports 'NoLand', because that is
      --   the fact the caller has to surface — nothing a later
      --   definition registration could change.
    deriving (Show, Eq)

-- | The placement pass's full result: the overlay plus why it looks the
--   way it does.
data LocationPlacement = LocationPlacement
    { lpOverlay ∷ !LocationOverlay
    , lpOutcome ∷ !PlacementOutcome
    } deriving (Show, Eq)

-- | Elevation percentile cut-offs derived from the world's land chunks.
data Cuts = Cuts
    { flatCut     ∷ !Int  -- ^ elev-range at/below this reads as "flat"
    , mountainCut ∷ !Int  -- ^ median elev at/above this reads as "mountain"
    , highlandCut ∷ !Int
    , lowlandCut  ∷ !Int
    }

-- | Place every location def into the world. Returns the sparse
--   chunk→id overlay. Empty when no defs are registered (the common
--   headless-dump path), which short-circuits the placement and
--   settlement work. Not the per-chunk land scan: this module is
--   {-# LANGUAGE Strict #-}, so that one is forced on every path (#1414).
--
--   The overlay-only view of 'computeLocationPlacement', kept because
--   most callers only ever want the map.
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
computeLocationOverlay seed worldSize plates oceanMap oceanDist lakes rivers =
    lpOverlay
    . computeLocationPlacement seed worldSize plates oceanMap oceanDist lakes rivers

-- | The placement pass proper: the overlay plus the outcome that
--   explains it (#997).
computeLocationPlacement
    ∷ Word64          -- ^ world seed
    → Int             -- ^ world size in chunks
    → [TectonicPlate] -- ^ pre-generated plates
    → OceanMap        -- ^ submerged-chunk set
    → OceanDistMap    -- ^ distance-from-ocean per chunk
    → WorldLakes      -- ^ per-chunk lakes
    → WorldRivers     -- ^ per-chunk rivers
    → [LocationDef]   -- ^ registered location defs
    → LocationPlacement
computeLocationPlacement seed worldSize plates oceanMap oceanDist lakes rivers defs
    -- Land first (#1414): 'NoLand' is the stronger physical fact, so it
    -- is what a caller is told when both conditions hold. Ordering it
    -- first costs nothing the other path was not already paying —
    -- 'landMetrics' is a `where` value binding and this module is
    -- {-# LANGUAGE Strict #-}, so it is forced to WHNF before any guard
    -- runs, and @null@ needs exactly WHNF.
    | null landMetrics = LocationPlacement emptyLocationOverlay NoLand
    | null placeable   = LocationPlacement emptyLocationOverlay NoPlaceableDefinitions
    | otherwise        = settle (fst (foldl' placeDef (emptyLocationOverlay, HS.empty)
                                             placeable))
  where
    -- Definitions that are actually allowed to place. A def authored
    -- @max_count: 0@ is an explicit "do not place" — it contributes
    -- nothing to the strict pass (which clamps the same way below) and
    -- must never be resurrected by the guarantee.
    placeable = filter ((> 0) . ldMaxCount) defsSorted

    -- A function, not a value binding: this module is {-# LANGUAGE
    -- Strict #-}, so a `where` value would be forced on every path,
    -- including the two short-circuits above. Unlike the already-forced
    -- 'landMetrics' detection, settling is real work the headless-dump
    -- path relies on skipping.
    settle ∷ LocationOverlay → LocationPlacement
    settle strict
        | not (HM.null strict) = LocationPlacement strict PlacedStrict
        | otherwise = case guaranteedEntry placeable of
            Just (coord, lid) →
                LocationPlacement (HM.singleton coord lid) PlacedGuaranteed
            -- Unreachable: `placeable` and `landMetrics` are both
            -- non-empty here, and the guarantee filters neither away.
            Nothing → LocationPlacement emptyLocationOverlay NoLand

    -- The #997 guarantee. Reached only when the strict pass placed
    -- NOTHING, so it can never perturb a world that already has
    -- locations. It must violate something — an empty strict result
    -- means no land chunk satisfied @anchorOk ∧ (wantsWater ∨
    -- dryEnough)@ — so the contract is stated as what still holds:
    --
    --   * the chunk comes from 'landMetrics' (never ocean, glacier,
    --     beyond-glacier, or below sea level) and is therefore already
    --     canonical under 'wrapChunkCoordU', so it stamps normally;
    --   * the id is a registered definition's, taken from the head of
    --     the canonical 'defsSorted' order;
    --   * the choice is a pure function of the generation tuple.
    --
    -- What it may violate is the definition's anchor tags and the #414
    -- 'dryEnough' proximity filter — but only as far as it has to: a
    -- dry chunk is preferred whenever one exists, and the tie-break is
    -- the SAME seeded 'scoreFor' hash the strict pass orders by, not a
    -- second ranking policy.
    guaranteedEntry ∷ [LocationDef] → Maybe (ChunkCoord, Text)
    guaranteedEntry []        = Nothing
    guaranteedEntry (def : _) =
        let lid    = ldId def
            coords = map fst landMetrics
            dry    = filter dryEnough coords
            pool   = if null dry then coords else dry
        in case sortOn (scoreFor lid) pool of
            coord : _ → Just (coord, lid)
            []        → Nothing

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
            isLand = centerMat ≢ matGlacier
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
    -- This is a CHUNK-level placement filter, deliberately coarser than —
    -- and independent of — a location's own tile-level footprint
    -- ('Location.Types.ldBounds', #777): it keeps a whole chunk clear of
    -- water candidates for placement, it does not describe the physical
    -- extent of whatever gets stamped there. The authoritative bounding
    -- box for that is 'Location.Bounds.translateBounds' applied to
    -- 'ldBounds', not this filter.
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

-- | FNV-1a hash of a location id — a fully deterministic salt (no
--   dependence on hashable's per-run seed) so the overlay is identical
--   across runs and machines.
idSalt ∷ Text → Word64
idSalt = T.foldl' (\acc c → (acc `xor` fromIntegral (fromEnum c)) * 0x100000001b3)
                  0xcbf29ce484222325

-- | True if a def's anchors opt it INTO water proximity (a coast / shore /
--   waterside location), so the dry-ground filter (#414) is skipped for it.
--   'AnchorCoast'\/'AnchorCoastal' also constrain to the ocean shore;
--   'AnchorWaterside' just tolerates nearby water without any other terrain
--   requirement. Every other location keeps clear of lakes / rivers / the
--   ocean shore.
--
--   TOTAL over the closed vocabulary (#1681), with no catch-all: a new
--   'LocationAnchor' constructor must state its water-proximity policy
--   here or the @-Werror@ build fails, so it cannot silently inherit the
--   dry-ground filter.
wantsWater ∷ [LocationAnchor] → Bool
wantsWater = any tolerates
  where
    tolerates a = case a of
        AnchorCoast     → True
        AnchorCoastal   → True
        AnchorWaterside → True
        AnchorFlat      → False
        AnchorMountain  → False
        AnchorHighland  → False
        AnchorLowland   → False
        AnchorInland    → False

-- | Does a chunk satisfy ALL of a def's anchor tags? TOTAL over the closed
--   'Location.Anchor' vocabulary (#801\/#1681) with no catch-all branch, so
--   adding a constructor without giving it terrain semantics fails the
--   @-Werror@ build rather than becoming the always-true match #801 existed
--   to remove.
--
--   'AnchorWaterside' is unconstrained ON PURPOSE and says so as its own
--   case: it is the #414 water-proximity opt-out modifier (tolerate nearby
--   water — see 'wantsWater'), carrying no terrain requirement of its own.
anchorOk ∷ Cuts → [LocationAnchor] → ChunkMetrics → Bool
anchorOk cuts tags cm = all ok tags
  where
    ok tag = case tag of
        AnchorFlat      → cmElevRange  cm ≤ flatCut cuts
        AnchorMountain  → cmMedianElev cm ≥ mountainCut cuts
        AnchorHighland  → cmMedianElev cm ≥ highlandCut cuts
        AnchorLowland   → cmMedianElev cm ≤ lowlandCut cuts
        AnchorCoast     → cmOceanDist  cm ≡ 1
        AnchorCoastal   → cmOceanDist  cm ≡ 1
        AnchorInland    → cmOceanDist  cm ≥ 4
        AnchorWaterside → True

-- | p-quantile of a pre-sorted list (0 for the empty list).
pctl ∷ [Int] → Double → Int
pctl [] _ = 0
pctl xs p =
    let n = length xs
        i = min (n - 1) (max 0 (floor (p * fromIntegral n)))
    in xs !! i
