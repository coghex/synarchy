{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Deterministic concept-root derivation (#710 requirements 7, 8, 9,
--   16). Every concept gets a native root built from syllables of the
--   profile's own consonant/vowel inventory and shapes — a pure
--   function of (profile, concept id, retry attempt), never of
--   spelling, lookup order, or any other concept's assignment.
module Language.Generated.Root
    ( generateRoot
    , assignRoots
    , minNativeWordLength
    ) where

import UPrelude
import Data.List (sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Semantic.Types (ConceptId(..))
import Language.Generated.Types
import Language.Generated.Hash

-- | Assign every concept in @ids@ a stable, unique native root under
--   @prof@. Concepts are processed in ascending 'ConceptId' order — the
--   catalogue's own intrinsic order, not whatever order @ids@ happens
--   to arrive in — so two calls over the same concept set always agree
--   regardless of the caller's traversal order (#710 requirement 8).
--
--   A raw root that collides (case-insensitively) with an
--   earlier-placed root in that canonical order is deterministically
--   rerolled via an incrementing attempt counter until it is unique;
--   the reroll depends only on (profile, concept id, attempt), so which
--   concept "already had" the colliding root is fixed by concept id
--   order, not by request order (#710 requirement 16).
assignRoots ∷ Profile → [ConceptId] → M.Map ConceptId Text
assignRoots prof ids = foldl' place M.empty (sort ids)
  where
    place acc cid =
        let usedLower = S.fromList (map T.toLower (M.elems acc))
            root = resolve cid 0 usedLower
        in M.insert cid root acc

    resolve cid attempt usedLower =
        let candidate = generateRoot prof cid attempt
        in if S.member (T.toLower candidate) usedLower
           then resolve cid (attempt + 1) usedLower
           else candidate

-- | The native root generated for one concept at one retry attempt.
--   Attempt 0 is the first candidate every caller sees before any
--   collision resolution; 'assignRoots' is the only caller that ever
--   passes a nonzero attempt.
generateRoot ∷ Profile → ConceptId → Int → Text
generateRoot prof cid attempt =
    let baseSeed = conceptSeed (profVersion prof) (profSeed prof) cid attempt
        extra = wordInRange (draw baseSeed 0) 0 (profMaxSyllables prof - profMinSyllables prof)
        targetSyll = profMinSyllables prof + extra
        (raw, nextStep) = buildSyllables prof baseSeed targetSyll 1
    in ensureMinLength prof baseSeed raw nextStep

-- | A rendered root must stand alone as a Bare-form proper name, so it
--   alone must already clear the 3-character floor of #710 requirement
--   6 — a compound built from two such roots is always longer still.
minNativeWordLength ∷ Int
minNativeWordLength = 3

ensureMinLength ∷ Profile → Word64 → Text → Int → Text
ensureMinLength prof baseSeed raw step
    | T.length raw ≥ minNativeWordLength = raw
    | otherwise =
        let (extra, step') = buildSyllables prof baseSeed 1 step
        in ensureMinLength prof baseSeed (raw <> extra) step'

-- | Render @n@ syllables starting at Rng @step@, returning the text and
--   the next unused step (so callers can keep drawing from the same
--   deterministic sequence, e.g. 'ensureMinLength' topping up a root
--   that came out too short).
buildSyllables ∷ Profile → Word64 → Int → Int → (Text, Int)
buildSyllables _ _ 0 step = ("", step)
buildSyllables prof baseSeed n step =
    let shapes = profSyllableShapes prof
        shape = shapes !! pickIndex (draw baseSeed step) (length shapes)
        (syll, step1) = renderShape prof baseSeed shape (step + 1)
        (rest, step2) = buildSyllables prof baseSeed (n - 1) step1
    in (syll <> rest, step2)

renderShape ∷ Profile → Word64 → SyllableShape → Int → (Text, Int)
renderShape prof baseSeed shape step0 = go (shapeSegments shape) step0 ""
  where
    go [] step acc = (acc, step)
    go (ConsonantSlot : rest) step acc =
        let cs = profConsonants prof
            c = cs !! pickIndex (draw baseSeed step) (length cs)
        in go rest (step + 1) (acc <> T.singleton c)
    go (VowelSlot : rest) step acc =
        let vs = profVowels prof
            v = vs !! pickIndex (draw baseSeed step) (length vs)
        in go rest (step + 1) (acc <> T.singleton v)
