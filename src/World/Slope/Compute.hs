{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Slope.Compute
    ( -- * Slope Computation
      computeChunkSlopes
    , computeChunkSlopesCols
    , chunkNeighbors
      -- * Internals (exposed for testing)
    , slopeBit
    ) where

import UPrelude
import qualified Data.HashSet as HS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import World.Types
import World.Material (getMaterialProps, MaterialProps(..), MaterialId(..)
                      , MaterialRegistry)
import World.Slope.Roughness (applyRoughness, rockJaggedSlope, slopeHardnessThreshold)

-- * Slope Computation (Post-Processing Pass)

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
--   columns pass through untouched. Used by 'World.Slope.Recompute.recomputeNeighborSlopes'
--   to re-slide just the boundary strip when a neighbour loads. Interior
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

slopeBit ∷ Bool → Int → Int → Int → Int → ChunkCoord
         → V.Vector (Maybe FluidCell)
         → (ChunkCoord → Maybe (VU.Vector Int))
         → Bool
slopeBit myHasFluid myZ neighborZ nlx nly coord fluidMap _neighborLookup =
    let diff = myZ - neighborZ

        -- An absent neighbour (not-yet-loaded chunk, or beyond the world
        -- edge) reads back as the 'minBound' sentinel from 'neighborElev'.
        -- It must never count as a drop: water would otherwise slope
        -- toward nothing. When the neighbour's chunk loads OR evicts, the
        -- cross-chunk recompute path ('World.Slope.Recompute.recomputeNeighborSlopes')
        -- re-runs this border strip, so the slope always reflects the
        -- currently loaded set — not the load order. Cross-SEAM neighbours
        -- resolve correctly because that recompute wraps the lookup coord.
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

chunkNeighbors ∷ ChunkCoord → [ChunkCoord]
chunkNeighbors (ChunkCoord cx cy) =
    [ ChunkCoord (cx - 1) cy
    , ChunkCoord (cx + 1) cy
    , ChunkCoord cx (cy - 1)
    , ChunkCoord cx (cy + 1)
    ]
