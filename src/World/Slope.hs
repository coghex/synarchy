{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Slope
    ( -- * Slope Computation
      computeChunkSlopes
    , recomputeNeighborSlopes
    , slopeRecomputeAffected
    , wrapChunkCoordU
    , patchEdgeStrata
    , chunkNeighbors
      -- * Slope Face Map Generation
    , SlopeFaceMaps(..)
    , generateSlopeFaceMaps
    , generateSideFaceMapLeft
    , generateSideFaceMapRight
      -- * Slope → Face Map Mapping
    , slopeToFaceMapIndex
      -- * Constants
    , slopeHardnessThreshold
      -- * Internals (exposed for testing)
    , slopeBit
    ) where

import UPrelude
import Data.Bits ((.&.), (.|.), shiftL, testBit, shiftR, shiftL, xor)
import Data.Word (Word8, Word32)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import World.Types
import World.Material (getMaterialProps, MaterialProps(..), MaterialId(..)
                      , MaterialRegistry(..))

-- * Constants

slopeHardnessThreshold ∷ Float
slopeHardnessThreshold = 0.7

-- | Jaggedness knobs for EXPOSED HARD ROCK (issue #224). The chance a
--   bare-rock tile gets a semi-random jagged slope is
--   @clamp01 (base + (hardness − threshold)·hardK + reliefNorm·reliefK)@,
--   so the harder and steeper the rock, the more broken it reads. These
--   are the inverse of the soft-terrain roughness taper in
--   'applyRoughness' (which fades OUT as material softens). Tunable;
--   render-only, so changing them never touches saved terrain.
rockJaggedBase ∷ Float
rockJaggedBase = 0.45

rockJaggedHardK ∷ Float
rockJaggedHardK = 1.0

rockJaggedReliefK ∷ Float
rockJaggedReliefK = 0.4

-- | Local relief (in z-levels) at which rock jaggedness saturates.
rockJaggedReliefMax ∷ Float
rockJaggedReliefMax = 6.0

tilePixelWidth ∷ Int
tilePixelWidth = 96

tilePixelHeight ∷ Int
tilePixelHeight = 64

diamondRows ∷ Int
diamondRows = 48

-- * Slope Computation (Post-Processing Pass)

-- | How many tiles in from a chunk edge a column's slope can depend on
--   a NEIGHBOURING chunk. The slope rule reads the immediate 4-neighbour
--   (N/E/S/W) — see 'computeTileSlope' — so radius 1: only the 1-tile
--   border of a chunk ever reads across a chunk boundary; interior
--   columns read in-chunk data only. 'recomputeNeighborSlopes' uses this
--   to re-slide just the boundary strip when a neighbour loads. Bump it
--   if the slope rule ever grows a wider horizontal stencil (e.g. future
--   underground slopes); the perimeter strip widens to match and the
--   recompute stays correct.
slopeStencilRadius ∷ Int
slopeStencilRadius = 1

-- | Column indices within 'slopeStencilRadius' of any chunk edge — the
--   only columns whose slope can change when a neighbour loads. Computed
--   once (constant for a given radius / chunk size).
perimeterColumnSet ∷ HS.HashSet Int
perimeterColumnSet = HS.fromList
    [ ly * chunkSize + lx
    | ly ← [0 .. chunkSize - 1]
    , lx ← [0 .. chunkSize - 1]
    , let r = slopeStencilRadius
    , lx < r ∨ lx ≥ chunkSize - r ∨ ly < r ∨ ly ≥ chunkSize - r
    ]

-- | Recompute one column's surface slope; pass the column through
--   unchanged if its surface z is outside the strata range.
recomputeColumnSlope ∷ Word64 → ChunkCoord → VU.Vector Int → MaterialRegistry
                     → V.Vector (Maybe FluidCell) → Chunk
                     → (ChunkCoord → Maybe (VU.Vector Int))
                     → (ChunkCoord → Maybe (V.Vector (Maybe FluidCell)))
                     → Int → ColumnTiles → ColumnTiles
recomputeColumnSlope seed coord surfMap registry fluidMap chunk
                     neighborLookup fluidNeighborLookup idx col =
    let lx = idx `mod` chunkSize
        ly = idx `div` chunkSize
        surfZ = surfMap VU.! idx
        i = surfZ - ctStartZ col
    in if i ≥ 0 ∧ i < VU.length (ctSlopes col)
       then let newSlope = computeTileSlope seed coord lx ly
                                            surfZ registry
                                            surfMap fluidMap
                                            chunk neighborLookup
                                            fluidNeighborLookup
            in col { ctSlopes = ctSlopes col VU.// [(i, newSlope)] }
       else col

-- | Recompute every column's surface slope (initial chunk gen).
computeChunkSlopes ∷ Word64 → ChunkCoord → VU.Vector Int → MaterialRegistry
                   → V.Vector (Maybe FluidCell) → Chunk
                   → (ChunkCoord → Maybe (VU.Vector Int))
                   → (ChunkCoord → Maybe (V.Vector (Maybe FluidCell))) → Chunk
computeChunkSlopes seed coord surfMap registry fluidMap chunk
                   neighborLookup fluidNeighborLookup =
    V.imap (recomputeColumnSlope seed coord surfMap registry fluidMap
                                 chunk neighborLookup fluidNeighborLookup) chunk

-- | Recompute slopes for ONLY the given column indices; all other
--   columns pass through untouched. Used by 'recomputeNeighborSlopes' to
--   re-slide just the boundary strip when a neighbour loads. Interior
--   columns can't change (their 4-neighbours are all in-chunk and their
--   terrain is fixed at gen), so restricting to the perimeter is
--   output-identical to a full recompute but O(perimeter) not O(area).
computeChunkSlopesCols ∷ Word64 → ChunkCoord → VU.Vector Int → MaterialRegistry
                       → V.Vector (Maybe FluidCell) → Chunk
                       → (ChunkCoord → Maybe (VU.Vector Int))
                       → (ChunkCoord → Maybe (V.Vector (Maybe FluidCell)))
                       → HS.HashSet Int → Chunk
computeChunkSlopesCols seed coord surfMap registry fluidMap chunk
                       neighborLookup fluidNeighborLookup cols =
    V.imap (\idx col →
        if HS.member idx cols
        then recomputeColumnSlope seed coord surfMap registry fluidMap
                                  chunk neighborLookup fluidNeighborLookup idx col
        else col
    ) chunk

computeTileSlope ∷ Word64 → ChunkCoord
                → Int → Int → Int → MaterialRegistry
                → VU.Vector Int
                → V.Vector (Maybe FluidCell)
                → Chunk
                → (ChunkCoord → Maybe (VU.Vector Int))
                → (ChunkCoord → Maybe (V.Vector (Maybe FluidCell)))
                → Word8
computeTileSlope seed coord lx ly z registry surfMap fluidMap tiles
                 neighborLookup fluidNeighborLookup =
    let col = tiles V.! columnIndex lx ly
        i = z - ctStartZ col
        matId = if i ≥ 0 ∧ i < VU.length (ctMats col)
                then ctMats col VU.! i
                else 0
        props = getMaterialProps registry (MaterialId matId)
        hardness = mpHardness props

        passesHardness = hardness < slopeHardnessThreshold

        -- If THIS tile is a wet tile (river / lake / ocean / lava), it's
        -- a river bed or basin floor and should slope toward lower-by-1
        -- neighbors even if those neighbors are wet too. Without this,
        -- a river that descends a z-level renders as a sloped water
        -- surface over a stepped terrain top — the upstream block's
        -- corner pokes through the water. Sloping the bed matches the
        -- water surface and hides the corner.
        --
        -- Dry tiles still keep the bank rule: they don't slope into wet
        -- neighbors, which would otherwise look like land dipping into
        -- the water.
        myHasFluid = case fluidMap V.! columnIndex lx ly of
            Just _  → True
            Nothing → False

        neighN = neighborElev coord lx (ly - 1) surfMap neighborLookup
        neighE = neighborElev coord (lx + 1) ly surfMap neighborLookup
        neighS = neighborElev coord lx (ly + 1) surfMap neighborLookup
        neighW = neighborElev coord (lx - 1) ly surfMap neighborLookup

        bitN = slopeBit myHasFluid z neighN lx (ly - 1) coord fluidMap neighborLookup
        bitE = slopeBit myHasFluid z neighE (lx + 1) ly coord fluidMap neighborLookup
        bitS = slopeBit myHasFluid z neighS lx (ly + 1) coord fluidMap neighborLookup
        bitW = slopeBit myHasFluid z neighW (lx - 1) ly coord fluidMap neighborLookup

        -- Is each cardinal neighbour a WET tile? Resolves across chunk
        -- boundaries via 'fluidNeighborLookup' (the jagged path can fire on
        -- a multi-level drop, so it must check the bank rule at seams too,
        -- not just in-chunk). The dry-rock jagged path must never slope a
        -- dry tile into a water neighbour — it would read as rock dipping
        -- into the river/lake/sea.
        wetN = neighborHasFluidAt coord lx (ly - 1) fluidMap fluidNeighborLookup
        wetE = neighborHasFluidAt coord (lx + 1) ly fluidMap fluidNeighborLookup
        wetS = neighborHasFluidAt coord lx (ly + 1) fluidMap fluidNeighborLookup
        wetW = neighborHasFluidAt coord (lx - 1) ly fluidMap fluidNeighborLookup

        rawSlope = (if bitN then 1 else 0)
               .|. (if bitE then 2 else 0)
               .|. (if bitS then 4 else 0)
               .|. (if bitW then 8 else 0) ∷ Word8

        -- Local relief: the steepest single-neighbour DROP from this tile
        -- (absent neighbours read back as the minBound sentinel and never
        -- count). Drives both the bare-rock eligibility (a tile with no
        -- downhill neighbour is flat-topped and stays blocky) and the
        -- jaggedness intensity.
        drops = [ z - nz | nz ← [neighN, neighE, neighS, neighW]
                         , nz ≠ minBound, nz < z ]
        maxDrop = if null drops then 0 else maximum drops
    in if myHasFluid
       -- Wet tiles (river bed / basin floor) keep the existing rule
       -- unchanged: the bed slopes to match the water surface, gated the
       -- same way it always was. Jaggedness is a DRY-rock feature only.
       then if not passesHardness ∨ rawSlope ≡ 0 ∨ rawSlope ≡ 15
            then 0
            else applyRoughness seed coord lx ly hardness rawSlope
       else if passesHardness
            -- Soft dry terrain: unchanged terrace rule. Flat biomes have
            -- no qualifying step and stay flat-topped.
            then if rawSlope ≡ 0 ∨ rawSlope ≡ 15
                 then 0
                 else applyRoughness seed coord lx ly hardness rawSlope
            -- Exposed hard rock (the material the soft gate bars). #224:
            -- make mountain flanks slope and peaks read as jagged rock.
            else rockJaggedSlope seed coord lx ly hardness z maxDrop rawSlope
                                 neighN neighE neighS neighW wetN wetE wetS wetW

-- | Slope rule for EXPOSED HARD ROCK — material at/above
--   'slopeHardnessThreshold', which the soft-terrain gate in
--   'computeTileSlope' otherwise forces flat. Bare rock on mountain
--   flanks and peaks should read as jagged, broken rock rather than
--   blocky prisms (issue #224). It dovetails with #225: gentle ground
--   keeps its soil veneer (soft surface → soft path), so only steep,
--   soil-shed faces reach this rock path.
--
--   Two effects, both gated on the tile having a downhill neighbour
--   (@maxDrop ≥ 1@) so flat/low rocky ground — plateau tops, rocky flats
--   — stays blocky and genuinely flat biomes are untouched:
--
--     1. Clean single-step flanks slope toward their lower neighbours
--        ('rawSlope'), so rock mountainsides taper instead of stepping.
--     2. JAGGEDNESS: with a probability that RISES with hardness and local
--        relief (the inverse of 'applyRoughness'), override with a
--        semi-random slope toward ONE strictly-lower neighbour. This
--        breaks the regular terrace into irregular angular rock, and fires
--        even where no neighbour is exactly one lower (steep multi-level
--        faces) — the case the strict terrace rule leaves flat.
--
--   The ramp only ever points at a strictly-lower DRY neighbour, so it
--   stays a geometrically valid (and pathing-walkable) ramp — never a
--   notch into a higher wall, and never a dry tile dipping into water
--   (the dry-land bank rule, honoured here via the @wet*@ flags).
rockJaggedSlope ∷ Word64 → ChunkCoord → Int → Int → Float → Int → Int → Word8
                → Int → Int → Int → Int
                → Bool → Bool → Bool → Bool → Word8
rockJaggedSlope seed (ChunkCoord cx cy) lx ly hardness z maxDrop rawSlope
                neighN neighE neighS neighW wetN wetE wetS wetW
    | maxDrop < 1 = 0   -- no downhill neighbour: flat-topped rock, blocky
    | otherwise =
        let h    = tileHash seed cx cy lx ly
            roll = fromIntegral (h .&. 0xFF) / 255.0 ∷ Float
            reliefNorm = min 1.0 (fromIntegral maxDrop / rockJaggedReliefMax)
            jaggedChance = clamp01 $ rockJaggedBase
                + (hardness - slopeHardnessThreshold) * rockJaggedHardK
                + reliefNorm * rockJaggedReliefK
            -- Wet-direction bitmask (seam-aware via the wet* flags).
            wetMask = (if wetN then 1 else 0) .|. (if wetE then 2 else 0)
                  .|. (if wetS then 4 else 0) .|. (if wetW then 8 else 0) ∷ Word8
            -- Candidate ramp directions: strictly-lower cardinal
            -- neighbours that are NOT wet (bank rule). May be empty if the
            -- only downhill neighbour is water — then we fall through to
            -- the dry clean-flank below.
            cand = [ b | (b, nz, wet) ← [ (1 ∷ Word8, neighN, wetN)
                                        , (2, neighE, wetE)
                                        , (4, neighS, wetS)
                                        , (8, neighW, wetW) ]
                       , nz ≠ minBound, nz < z, not wet ]
            -- Clean-flank fallback: rawSlope with any wet directions
            -- cleared. rawSlope is built by 'slopeBit', whose bank check is
            -- IN-CHUNK only, so a 1-z lower wet neighbour across a chunk
            -- seam would otherwise survive in rawSlope and ramp into water.
            -- (15 `xor` wetMask) is the 4-bit complement of the wet mask.
            dryFlank = rawSlope .&. (15 `xor` wetMask)
        in if roll < jaggedChance ∧ not (null cand)
           then cand !! fromIntegral ((h `shiftR` 8) `mod`
                                      fromIntegral (length cand))
           else if dryFlank ≢ 0 ∧ dryFlank ≢ 15
                then dryFlank   -- clean terrace flank (wet dirs masked out)
                else 0

slopeBit ∷ Bool → Int → Int → Int → Int → ChunkCoord
         → V.Vector (Maybe FluidCell)
         → (ChunkCoord → Maybe (VU.Vector Int))
         → Bool
slopeBit myHasFluid myZ neighborZ nlx nly coord fluidMap neighborLookup =
    let diff = myZ - neighborZ

        -- An absent neighbour (not-yet-loaded chunk, or beyond the world
        -- edge) reads back as the 'minBound' sentinel from 'neighborElev'.
        -- It must never count as a drop: water would otherwise slope
        -- toward nothing. When the neighbour's chunk loads OR evicts, the
        -- cross-chunk recompute path ('recomputeNeighborSlopes') re-runs
        -- this border strip, so the slope always reflects the currently
        -- loaded set — not the load order. Cross-SEAM neighbours resolve
        -- correctly because that recompute wraps the lookup coord.
        neighborLoaded = neighborZ ≠ minBound

        -- Dry land keeps the strict single-step terrace rule (a neighbour
        -- exactly one lower). A WET tile additionally slopes toward any
        -- EXPOSED-AIR edge — a present neighbour whose surface drops by
        -- one OR MORE levels. That is the waterfall-lip / water-cliff
        -- case (issue #222): the source water tile at the top of a fall
        -- borders a multi-level drop, so 'diff > 1' there; the old
        -- 'diff ≡ 1' rule left it flat. Tipping the surface toward the
        -- drop makes the water visibly pour over the lip. Water enclosed
        -- by equal/higher surfaces (diff ≤ 0) stays flat.
        validDiff
            | myHasFluid = neighborLoaded ∧ diff ≥ 1
            | otherwise  = diff ≡ 1

        (normLx, normLy, neighborCoord) = normalizeCoord coord nlx nly
        hasFluid = case neighborCoord of
            c | c ≡ coord ->
                case fluidMap V.! columnIndex normLx normLy of
                    Just _  → True
                    Nothing → False
            _ → False
    in validDiff ∧ (myHasFluid ∨ not hasFluid)

neighborElev ∷ ChunkCoord → Int → Int
             → VU.Vector Int
             → (ChunkCoord → Maybe (VU.Vector Int))
             → Int
neighborElev coord lx ly surfMap neighborLookup
    | lx ≥ 0 ∧ lx < chunkSize ∧ ly ≥ 0 ∧ ly < chunkSize
                       = surfMap VU.! columnIndex lx ly
    | otherwise =
        let (neighborCoord, (nlx, nly)) = normalizeToChunk coord lx ly
        in case neighborLookup neighborCoord of
            Just neighSurf → neighSurf VU.! columnIndex nlx nly
            Nothing        → minBound

normalizeToChunk ∷ ChunkCoord → Int → Int → (ChunkCoord, (Int, Int))
normalizeToChunk (ChunkCoord cx cy) lx ly =
    let cx' = cx + floorDiv' lx chunkSize
        cy' = cy + floorDiv' ly chunkSize
        lx' = floorMod' lx chunkSize
        ly' = floorMod' ly chunkSize
    in (ChunkCoord cx' cy', (lx', ly'))

normalizeCoord ∷ ChunkCoord → Int → Int → (Int, Int, ChunkCoord)
normalizeCoord coord lx ly =
    let (c, (lx', ly')) = normalizeToChunk coord lx ly
    in (lx', ly', c)

-- | Does the cardinal neighbour at local (nlx, nly) hold a fluid cell?
--   In-chunk neighbours read this chunk's 'fluidMap'; out-of-chunk
--   neighbours resolve through 'fluidNeighborLookup' (the loaded
--   neighbour's fluid map). A not-yet-loaded neighbour reads back as dry
--   (Nothing) — exactly like the terrain 'neighborElev' minBound sentinel;
--   the cross-chunk recompute re-runs this border strip once the neighbour
--   loads, so the bank rule converges on the loaded set, not load order.
neighborHasFluidAt ∷ ChunkCoord → Int → Int → V.Vector (Maybe FluidCell)
                   → (ChunkCoord → Maybe (V.Vector (Maybe FluidCell))) → Bool
neighborHasFluidAt coord nlx nly fluidMap fluidNeighborLookup
    | nlx ≥ 0 ∧ nlx < chunkSize ∧ nly ≥ 0 ∧ nly < chunkSize
        = isJustCell (fluidMap V.! columnIndex nlx nly)
    | otherwise =
        let (neighborCoord, (nlx', nly')) = normalizeToChunk coord nlx nly
        in case fluidNeighborLookup neighborCoord of
            Just nf → isJustCell (nf V.! columnIndex nlx' nly')
            Nothing → False
  where isJustCell (Just _) = True
        isJustCell Nothing  = False

floorDiv' ∷ Int → Int → Int
floorDiv' a b = floor (fromIntegral a / fromIntegral b ∷ Double)

floorMod' ∷ Int → Int → Int
floorMod' a b = a - floorDiv' a b * b

-- * Probabilistic Roughness

applyRoughness ∷ Word64 → ChunkCoord → Int → Int → Float → Word8 → Word8
applyRoughness seed (ChunkCoord cx cy) lx ly hardness rawSlope
    | hardness < 0.3  = rawSlope
    | otherwise =
        let h = tileHash seed cx cy lx ly
            roll = fromIntegral (h .&. 0xFF) / 255.0 ∷ Float
            roughnessChance = (hardness - 0.3) * 0.75
        in if roll < roughnessChance
           then let dirBits = (h `shiftR` 8) .&. 0x3
                    randomSlope = case dirBits of
                        0 → 1
                        1 → 2
                        2 → 4
                        _ → 8
                in fromIntegral randomSlope
           else rawSlope

tileHash ∷ Word64 → Int → Int → Int → Int → Word64
tileHash seed cx cy lx ly =
    let a = seed `xor` (fromIntegral cx * 2654435761)
        b = a `xor` (fromIntegral cy * 2246822519)
        c = b `xor` (fromIntegral lx * 3266489917)
        d = c `xor` (fromIntegral ly * 668265263)
        e = d `xor` (d `shiftR` 16)
        f = e * 2246822519
        g = f `xor` (f `shiftR` 13)
    in g

-- * Slope → Face Map Index Mapping

slopeToFaceMapIndex ∷ Word8 → Int
slopeToFaceMapIndex  = fromIntegral

-- * Procedural Face Map Generation

data SlopeFaceMaps = SlopeFaceMaps
    { sfmFlat  ∷ !(VS.Vector Word8)
    , sfmNorth ∷ !(VS.Vector Word8)
    , sfmEast  ∷ !(VS.Vector Word8)
    , sfmSouth ∷ !(VS.Vector Word8)
    , sfmWest  ∷ !(VS.Vector Word8)
    } deriving (Show)

generateSlopeFaceMaps ∷ SlopeFaceMaps
generateSlopeFaceMaps = SlopeFaceMaps
    { sfmFlat  = generateFlatFaceMap
    , sfmNorth = generateRampFaceMap RampNorth
    , sfmEast  = generateRampFaceMap RampEast
    , sfmSouth = generateRampFaceMap RampSouth
    , sfmWest  = generateRampFaceMap RampWest
    }

data RampDirection = RampNorth | RampEast | RampSouth | RampWest

generateFlatFaceMap ∷ VS.Vector Word8
generateFlatFaceMap = VS.generate (tilePixelWidth * tilePixelHeight * 4) $ \i →
    let px  = i `div` 4
        col = px `mod` tilePixelWidth
        row = px `div` tilePixelWidth
        chan = i `mod` 4
    in if row < diamondRows
       then case chan of
                0 → 0
                1 → 255
                2 → 0
                _ → 255
       else let halfW = tilePixelWidth `div` 2
            in if col < halfW
               then case chan of
                   0 → 0
                   1 → 0
                   2 → 255
                   _ → 255
               else case chan of
                   0 → 255
                   1 → 0
                   2 → 0
                   _ → 255

generateRampFaceMap ∷ RampDirection → VS.Vector Word8
generateRampFaceMap dir = VS.generate (tilePixelWidth * tilePixelHeight * 4) $ \i →
    let px  = i `div` 4
        col = px `mod` tilePixelWidth
        row = px `div` tilePixelWidth
        chan = i `mod` 4
    in if row < diamondRows
       then let t = rampGradient dir col row
                green = round (255.0 * (1.0 - t * 0.7)) ∷ Int
                red   = round (255.0 * t * 0.5) ∷ Int
                blue  = round (255.0 * t * 0.5) ∷ Int
            in case chan of
                0 → clampByte red
                1 → clampByte green
                2 → clampByte blue
                _ → 255
       else let halfW = tilePixelWidth `div` 2
            in if col < halfW
               then case chan of
                   0 → 0
                   1 → 0
                   2 → 255
                   _ → 255
               else case chan of
                   0 → 255
                   1 → 0
                   2 → 0
                   _ → 255

rampGradient ∷ RampDirection → Int → Int → Float
rampGradient dir col row =
    let cx = fromIntegral tilePixelWidth / 2.0 ∷ Float
        cy = fromIntegral diamondRows / 2.0 ∷ Float
        nx = (fromIntegral col - cx) / cx
        ny = (fromIntegral row - cy) / cy
    in clamp01 $ case dir of
        RampNorth → (1.0 - ny) / 2.0
        RampSouth → (1.0 + ny) / 2.0
        RampEast  → (1.0 + nx) / 2.0
        RampWest  → (1.0 - nx) / 2.0

clampByte ∷ Int → Word8
clampByte x = fromIntegral (max 0 (min 255 x))

-- * Side Face Maps (left/right only, no top face)

-- | Left side face map: G=0 (no top), B=255 for left-side pixels only.
--   The left side face occupies the bottom-left portion of the tile sprite.
generateSideFaceMapLeft ∷ VS.Vector Word8
generateSideFaceMapLeft = VS.generate (tilePixelWidth * tilePixelHeight * 4) $ \i →
    let px  = i `div` 4
        col = px `mod` tilePixelWidth
        row = px `div` tilePixelWidth
        chan = i `mod` 4
        halfW = tilePixelWidth `div` 2
    in if row < diamondRows
       then 0  -- no top face at all
       else if col < halfW
            then case chan of  -- left side face
                0 → 0
                1 → 0
                2 → 255
                _ → 255
            else 0  -- no right side face

-- | Right side face map: G=0 (no top), R=255 for right-side pixels only.
generateSideFaceMapRight ∷ VS.Vector Word8
generateSideFaceMapRight = VS.generate (tilePixelWidth * tilePixelHeight * 4) $ \i →
    let px  = i `div` 4
        col = px `mod` tilePixelWidth
        row = px `div` tilePixelWidth
        chan = i `mod` 4
        halfW = tilePixelWidth `div` 2
    in if row < diamondRows
       then 0  -- no top face at all
       else if col ≥ halfW
            then case chan of  -- right side face
                0 → 255
                1 → 0
                2 → 0
                _ → 255
            else 0  -- no left side face

-- * Recompute Neighbor Slopes

-- | The loaded chunks whose border slopes 'recomputeNeighborSlopes' will
--   rewrite for the given changed coords — the changed chunks themselves
--   plus their loaded (seam-wrapped) neighbours. Exposed so a caller can
--   re-apply, over EXACTLY this set, any pass the recompute clobbers
--   (notably the mid-dig slope masks restored by
--   'World.Mine.Apply.applyDigSlopesTd'); keying that restore off a
--   narrower set would silently drop overrides on evicted-neighbour or
--   wrapped-seam-neighbour tiles.
slopeRecomputeAffected ∷ Int → [ChunkCoord] → WorldTileData → [ChunkCoord]
slopeRecomputeAffected worldSize changedCoords wtd =
    let chunks = wtdChunks wtd
        wrap = wrapChunkCoordU worldSize
    in HS.toList $ HS.fromList $
        [ c | c ← changedCoords, HM.member c chunks ] <>
        [ nb
        | chg ← changedCoords
        , raw ← chunkNeighbors chg
        , let nb = wrap raw
        , HM.member nb chunks
        ]

recomputeNeighborSlopes ∷ Word64 → Int → MaterialRegistry
                        → [ChunkCoord]
                        → WorldTileData
                        → WorldTileData
recomputeNeighborSlopes seed worldSize registry changedCoords wtd =
    let chunks = wtdChunks wtd
        wrap = wrapChunkCoordU worldSize
        -- 'changedCoords' are chunks whose presence just changed — loaded
        -- OR evicted. A loaded chunk's own border strip plus its loaded
        -- neighbours' strips may need re-sloping; an evicted chunk's
        -- former neighbours must re-slope to drop the slope that pointed
        -- at it (so the surface depends only on the currently loaded set,
        -- not load order). Cross-SEAM neighbours live under a wrapped
        -- coord, so wrap before testing membership and before lookup.
        affected = slopeRecomputeAffected worldSize changedCoords wtd
        neighborLookup coord = case HM.lookup (wrap coord) chunks of
            Just lc → Just (lcTerrainSurfaceMap lc)
            Nothing → Nothing
        -- Parallel fluid lookup so the dry-rock bank rule (jagged path)
        -- can see a wet neighbour ACROSS a chunk seam, not just in-chunk.
        fluidNeighborLookup coord = case HM.lookup (wrap coord) chunks of
            Just lc → Just (lcFluidMap lc)
            Nothing → Nothing
        -- Only the boundary strip can change when a neighbour loads —
        -- interior columns read in-chunk data only, so recomputing them
        -- would reproduce their stored value. Restricting to the
        -- perimeter is output-identical but skips ~the chunk interior.
        updatedChunks = foldl' (\acc coord →
            case HM.lookup coord acc of
                Just lc →
                    let newTiles = computeChunkSlopesCols seed coord
                            (lcTerrainSurfaceMap lc) registry
                            (lcFluidMap lc) (lcTiles lc) neighborLookup
                            fluidNeighborLookup perimeterColumnSet
                    in HM.insert coord (lc { lcTiles = newTiles }) acc
                Nothing → acc
            ) chunks affected
    in wtd { wtdChunks = updatedChunks }

-- | After neighbors load, extend edge column strata downward where
--   the actual cross-chunk neighbor terrain is lower than what the
--   bordered grid showed at generation time. This fixes cliff-face
--   voids at chunk boundaries caused by coastal erosion lowering
--   terrain in one chunk that the adjacent chunk couldn't see.
--
--   The extension repeats the bottom-most material in the existing
--   strata, so the cliff face looks like a natural continuation
--   rather than exposing underground geology.
patchEdgeStrata ∷ [ChunkCoord] → WorldTileData → WorldTileData
patchEdgeStrata newCoords wtd =
    let chunks = wtdChunks wtd
        affected = HS.toList $ HS.fromList $
            newCoords <>
            [ neighbor
            | new ← newCoords
            , neighbor ← chunkNeighbors new
            , HM.member neighbor chunks
            ]
        neighborLookup coord = case HM.lookup coord chunks of
            Just lc → Just (lcTerrainSurfaceMap lc)
            Nothing → Nothing
        updatedChunks = foldl' (\acc coord →
            case HM.lookup coord acc of
                Just lc →
                    let tiles = lcTiles lc
                        tSurf = lcTerrainSurfaceMap lc
                        newTiles = patchChunkEdges coord tSurf neighborLookup tiles
                    in HM.insert coord (lc { lcTiles = newTiles }) acc
                Nothing → acc
            ) chunks affected
    in wtd { wtdChunks = updatedChunks }

-- | Patch edge columns of a single chunk. For each column at the
--   chunk boundary, check if any cross-chunk neighbor is lower than
--   ctStartZ. If so, extend ctMats/ctSlopes/ctVeg downward by
--   repeating the bottom material.
patchChunkEdges ∷ ChunkCoord → VU.Vector Int
                → (ChunkCoord → Maybe (VU.Vector Int))
                → Chunk → Chunk
patchChunkEdges coord tSurf neighborLookup tiles =
    let ChunkCoord cx cy = coord
        -- Get neighbor terrain at a local position, crossing chunks if needed
        getNeighborZ lx ly
            | lx ≥ 0 ∧ lx < chunkSize ∧ ly ≥ 0 ∧ ly < chunkSize
                = tSurf VU.! (ly * chunkSize + lx)
            | otherwise =
                let cx' = cx + (if lx < 0 then -1 else if lx ≥ chunkSize then 1 else 0)
                    cy' = cy + (if ly < 0 then -1 else if ly ≥ chunkSize then 1 else 0)
                    nlx = ((lx `mod` chunkSize) + chunkSize) `mod` chunkSize
                    nly = ((ly `mod` chunkSize) + chunkSize) `mod` chunkSize
                in case neighborLookup (ChunkCoord cx' cy') of
                    Just nSurf → nSurf VU.! (nly * chunkSize + nlx)
                    -- Neighbor chunk not loaded: clamp to own edge tile,
                    -- which returns this tile's own elevation. This is
                    -- intentional — it means a missing neighbor never
                    -- drives a downward extension. patchEdgeStrata re-runs
                    -- on existing neighbors when new chunks load, so the
                    -- extension will be applied once the neighbor arrives.
                    Nothing → tSurf VU.! (max 0 (min (chunkSize-1) ly)
                                          * chunkSize
                                          + max 0 (min (chunkSize-1) lx))
    in V.imap (\idx col →
        let lx = idx `mod` chunkSize
            ly = idx `div` chunkSize
            isEdge = lx ≡ 0 ∨ lx ≡ chunkSize - 1
                   ∨ ly ≡ 0 ∨ ly ≡ chunkSize - 1
        in if not isEdge
           then col
           else let -- Find the actual minimum neighbor elevation.
                    -- Filter out minBound sentinels (beyond-glacier tiles)
                    -- to prevent integer overflow in the extension calc.
                    nN = getNeighborZ lx (ly - 1)
                    nS = getNeighborZ lx (ly + 1)
                    nE = getNeighborZ (lx + 1) ly
                    nW = getNeighborZ (lx - 1) ly
                    clampSentinel z = if z ≡ minBound then maxBound else z
                    actualMinZ = min (clampSentinel nN) (min (clampSentinel nS)
                                     (min (clampSentinel nE) (clampSentinel nW)))
                    currentStart = ctStartZ col
                in if actualMinZ ≥ currentStart ∨ VU.null (ctMats col)
                   then col  -- No extension needed
                   else let extension = currentStart - actualMinZ
                            bottomMat = ctMats col VU.! 0
                            -- Extend all vectors by repeating bottom values
                            extMats = VU.replicate extension bottomMat
                                      VU.++ ctMats col
                            extSlopes = VU.replicate extension 0
                                        VU.++ ctSlopes col
                            extVeg = VU.replicate extension 0
                                     VU.++ ctVeg col
                        in col { ctStartZ = actualMinZ
                               , ctMats   = extMats
                               , ctSlopes = extSlopes
                               , ctVeg    = extVeg
                               }
        ) tiles

chunkNeighbors ∷ ChunkCoord → [ChunkCoord]
chunkNeighbors (ChunkCoord cx cy) =
    [ ChunkCoord (cx - 1) cy
    , ChunkCoord (cx + 1) cy
    , ChunkCoord cx (cy - 1)
    , ChunkCoord cx (cy + 1)
    ]
