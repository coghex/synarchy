{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Deterministic, integer-only hashing for the generated-language
--   layer (#710). Mirrors the stateless fmix64-style avalanche mixer
--   already used across worldgen (e.g. 'World.Plate.Hash.hashCoord',
--   'World.Gem.mix64') and the FNV-1a text hash already used for a
--   stable id-derived seed ('Location.Overlay.idSalt') — no shared
--   module exists to import, so this is a small self-contained copy in
--   the same house style, scoped to this feature.
--
--   Every draw is (seed, step) → Word64: stateless and reproducible by
--   construction, never a threaded/mutable generator. No 'Float' or
--   'Double' anywhere (#710 requirement 12 — floating-point behavior
--   can vary across platforms; integer 'mod'/'xor'/'shiftR' cannot).
module Language.Generated.Hash
    ( fmix64
    , textSeed
    , conceptSeed
    , draw
    , wordInRange
    , pickIndex
    , shuffleBy
    ) where

import UPrelude
import qualified Data.Text as T
import Language.Semantic.Types (ConceptId(..))
import Language.Generated.Types (GeneratorVersion(..), LangSeed(..))

-- | 64-bit avalanche finalizer (the murmur3/splitmix64 finalizer
--   family already used throughout worldgen hashing).
fmix64 ∷ Word64 → Word64
fmix64 x0 =
    let x1 = x0 `xor` (x0 `shiftR` 33)
        x2 = x1 * 0xff51afd7ed558ccd
        x3 = x2 `xor` (x2 `shiftR` 33)
        x4 = x3 * 0xc4ceb9fe1a85ec53
        x5 = x4 `xor` (x4 `shiftR` 33)
    in x5

-- | FNV-1a 64-bit hash of a 'Text', identical to 'Location.Overlay.idSalt'
--   — a fully deterministic salt with no dependence on any per-run
--   hashing seed.
textSeed ∷ Text → Word64
textSeed = T.foldl' (\acc c → (acc `xor` fromIntegral (fromEnum c)) * 0x100000001b3)
                     0xcbf29ce484222325

-- | The deterministic per-concept seed a root is generated from:
--   a function of the generator version, the language seed, the
--   concept id's text, and a retry "attempt" counter (used by
--   collision resolution to deterministically reroll a colliding
--   root without depending on any external traversal order).
conceptSeed ∷ GeneratorVersion → LangSeed → ConceptId → Int → Word64
conceptSeed (GeneratorVersion v) (LangSeed s) cid attempt =
    fmix64 $ s
           `xor` fmix64 (fromIntegral v * 0x9E3779B97F4A7C15)
           `xor` textSeed (conceptIdText cid)
           `xor` fmix64 (fromIntegral attempt * 0xD6E8FEB86659FD93)

-- | One independent, stateless draw from a base seed at a given step.
--   Bumping the step index is how a pure fold makes successive
--   "random" choices without threading mutable generator state.
draw ∷ Word64 → Int → Word64
draw seed step = fmix64 (seed `xor` (fromIntegral step * 0x9E3779B97F4A7C15))

-- | Map a drawn word into an inclusive integer range @[lo, hi]@.
--   Integer 'mod' only — no float division/floor.
wordInRange ∷ Word64 → Int → Int → Int
wordInRange w lo hi = lo + fromIntegral (w `mod` fromIntegral (hi - lo + 1))

-- | Map a drawn word into an index @[0, n)@ for a nonempty list of
--   length @n@.
pickIndex ∷ Word64 → Int → Int
pickIndex w n = fromIntegral (w `mod` fromIntegral n)

-- | Deterministic Fisher-Yates-style shuffle: pure, seeded by (seed,
--   starting step). Used to pick a stable "random" subset of a pool
--   (e.g. @take k (shuffleBy seed 0 pool)@) without bias toward the
--   pool's original order.
shuffleBy ∷ Word64 → Int → [α] → [α]
shuffleBy _ _ [] = []
shuffleBy seed step xs =
    let n = length xs
        i = pickIndex (draw seed step) n
        x = xs !! i
        rest = take i xs <> drop (i + 1) xs
    in x : shuffleBy seed (step + 1) rest
