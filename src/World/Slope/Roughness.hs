{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Slope.Roughness
    ( -- * Constants
      slopeHardnessThreshold
      -- * Probabilistic Roughness
    , applyRoughness
    , rockJaggedSlope
    , tileHash
    ) where

import UPrelude
import World.Types (ChunkCoord(..))

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

-- | Slope rule for EXPOSED HARD ROCK — material at/above
--   'slopeHardnessThreshold', which the soft-terrain gate in
--   'World.Slope.Compute.computeTileSlope' otherwise forces flat. Bare rock
--   on mountain flanks and peaks should read as jagged, broken rock rather
--   than blocky prisms (issue #224). It dovetails with #225: gentle ground
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
--        relief (the inverse of 'applyRoughness'), ADD a semi-random lean
--        toward one non-HIGHER dry neighbour (lower or equal-height) ON TOP
--        of the clean downhill flank. This breaks the regular terrace into
--        irregular angular rock, and fires even where no neighbour is
--        exactly one lower (steep multi-level faces) — the case the strict
--        terrace rule leaves flat.
--
--   Jaggedness is ADDITIVE, not a substitution: the lean is OR-ed onto the
--   genuine downhill flank rather than replacing it. Slope bits drive
--   pathing ('Unit.Pathing.Cost' reads a bit as a walkable 1-z ramp toward
--   that neighbour), so dropping the real downhill flank for a decorative
--   lean would turn a walkable rock ramp into a cliff (#337). The added
--   lean points only at a non-higher DRY neighbour — a strictly-lower one
--   (a valid descent, or a ≥2 drop that is a cliff regardless) or an
--   equal-height one (a 0-z step, always walkable regardless of the bit) —
--   so it is pathing-neutral while supplying the directional variety a
--   coherent face otherwise lacks. Never leans uphill (a notch into a
--   higher wall) or into water (the bank rule, via the @wet*@ flags).
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
            -- Candidate jagged-lean directions: cardinal neighbours that
            -- are present, NOT wet (bank rule), and NOT strictly HIGHER
            -- (@nz ≤ z@). Strictly-lower neighbours are downhill ramps;
            -- EQUAL-height neighbours are visual leans, not invalid notches
            -- into a higher wall. Including the equal-height directions
            -- (#337) is what gives a COHERENT rock face — a uniform
            -- mountainside or simple outcrop whose only strictly-lower
            -- neighbour is a single downhill direction — genuine directional
            -- variety, instead of a deterministic one-element pick that
            -- slopes every such tile the same downhill way. Never points
            -- uphill (a notch into a higher wall) or into water. May be
            -- empty only if every non-higher neighbour is wet — then we
            -- fall through to the dry clean-flank below.
            cand = [ b | (b, nz, wet) ← [ (1 ∷ Word8, neighN, wetN)
                                        , (2, neighE, wetE)
                                        , (4, neighS, wetS)
                                        , (8, neighW, wetW) ]
                       , nz ≠ minBound, nz ≤ z, not wet ]
            -- Clean-flank fallback: rawSlope with any wet directions
            -- cleared. rawSlope is built by 'World.Slope.Compute.slopeBit',
            -- whose bank check is IN-CHUNK only, so a 1-z lower wet neighbour
            -- across a chunk seam would otherwise survive in rawSlope and
            -- ramp into water. (15 `xor` wetMask) is the 4-bit complement of
            -- the wet mask.
            dryFlank = rawSlope .&. (15 `xor` wetMask)
            -- The non-jagged result: the clean terrace flank, or flat (0)
            -- when it is empty or the degenerate all-four (15 renders as a
            -- flat top, never a slope). This is also the floor the jagged
            -- branch falls back to, so the result is NEVER a subset of the
            -- walkable flank.
            cleanFlank = if dryFlank ≢ 0 ∧ dryFlank ≢ 15 then dryFlank else 0
        in if roll < jaggedChance ∧ not (null cand)
           -- Jaggedness is ADDED to the clean flank, never substituted for
           -- it. Slope bits are not purely visual: 'Unit.Pathing.Cost'
           -- reads a bit as "this tile has a walkable 1-z ramp down toward
           -- that neighbour", so replacing the genuine downhill flank
           -- ('dryFlank') with an equal-height lean would turn a walkable
           -- rock ramp into a cliff (#337 review). OR-ing instead keeps
           -- every real ramp: the extra lean is pathing-neutral — it points
           -- only at a lower neighbour (already a valid descent / ≥2-drop
           -- cliff regardless) or an equal-height one (a 0-z step, always
           -- walkable regardless of the bit). The lean is what supplies the
           -- directional variety on a coherent face whose dryFlank is a
           -- single downhill bit (or 0 on a steep multi-level face).
           --
           -- 'lean' MUST stay inside this @not (null cand)@ branch: this
           -- module is compiled @{-# LANGUAGE Strict #-}@, so a top-level
           -- let would be forced eagerly and @`mod` (length cand)@ would
           -- divide by zero on the empty-cand fall-through case.
           then let lean = cand !! fromIntegral ((h `shiftR` 8) `mod`
                                                  fromIntegral (length cand))
                    combined = dryFlank .|. lean
                -- If OR-ing the lean would complete the degenerate all-four
                -- (15 renders as a flat top, never a slope) we must NOT
                -- return 'lean' alone: it can be a subset of dryFlank and so
                -- strand genuine ramps as cliffs (e.g. dryFlank = E|S|W = 14,
                -- lean = N → combined 15, but N alone drops E/S/W). In that
                -- case dryFlank is either a real 3-sided flank — keep all
                -- three ramps — or already all-four itself, in which case we
                -- fall back to the single 'lean' bit (what the pre-#337 code
                -- rendered for such a tile; cand is the 4 downhill dirs).
                in if combined ≢ 15      then combined
                   else if dryFlank ≢ 15 then dryFlank
                   else                       lean
           else cleanFlank

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
