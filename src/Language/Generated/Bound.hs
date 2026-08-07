{-# LANGUAGE Strict #-}
-- | Bound morphemes (#1096): a small, deterministic set of concepts
--   whose root is SHORTENED when it stands in a dependent compound slot.
--
--   Versions 1-3 gave every concept exactly one string, reused verbatim
--   whether the root stood alone, modified another root, took
--   grammatical marking, or filled another compound's dependent slot.
--   A language whose @kara@ recurs as @kar-@ inside compounds makes the
--   shared morpheme visible; version 4 gives at most eight concepts per
--   language such an alternation.
--
--   Four properties bound the whole feature, and each is what keeps a
--   deliberately small piece of morphology from turning into
--   irregularity:
--
--   * __Terminal deletion only.__ An accepted bound form is a nonempty
--     STRICT PREFIX of its free form. Stem substitution, internal
--     deletion, vowel gradation, and suppletion are all excluded by
--     construction rather than by convention, because "shorter prefix"
--     is the only shape 'boundCandidate' can produce.
--   * __One notion of legality.__ A candidate's internal consonant
--     clusters are judged by #1094's own exported
--     'Language.Generated.Onset.admissibleOnset' relation — over every
--     adjacent pair of CONSONANT-CAPABLE characters, a dual-role @y@
--     included. No second legality predicate exists here.
--   * __Selection is ranked, not traversed.__ Concepts are visited in
--     ascending @('boundSeed', 'ConceptId')@ order. The seed is
--     domain-separated from root generation and the concept id breaks
--     ties, so the order is TOTAL and provably independent of the order
--     the caller's catalogue happens to enumerate in.
--   * __Rendering never derives anything.__ Every candidate is derived,
--     validated, and either accepted or skipped HERE, while roots are
--     assigned. 'Language.Generated.Render' does an ordinary map lookup
--     and nothing else — no catalogue scan, no search, no retry.
--
--   Bound forms exist only from 'boundFormVersion' onward. A language
--   whose #1092 provenance reconstructs an earlier version gets an empty
--   bound map and renders every dependent slot with the free form, which
--   is exactly what keeps versions 1-3's pinned goldens byte-identical.
--
--   Nothing here is persisted: a language's bound forms are recomputed
--   from (generator version, language seed, catalogue) on demand, so
--   this adds no save component, root state owner, or typed reference.
module Language.Generated.Bound
    ( LanguageRoots(..)
    , freeRootsOnly
    , maxBoundForms
    , boundFormVersion
    , formsBoundMorphemes
    , boundSelectionOrder
    , assignBoundForms
    , boundCandidates
    , boundFormLegal
    , boundFormAdmissible
    , countBoundCollisions
    ) where

import UPrelude
import Data.Char (isAsciiUpper, isAsciiLower)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Semantic.Types (ConceptId(..))
import Language.Generated.Types
import Language.Generated.Hash
import Language.Generated.Onset (admissibleOnset, consonantCapable)

-- | One language's complete concept→morpheme assignment: the free root
--   every concept has, plus the bound form the few selected concepts
--   additionally have.
--
--   The two maps travel together in one value rather than as two
--   positional arguments of the same type, so a caller cannot silently
--   swap them. @lrBound@ is always a sub-map of @lrFree@'s key set: a
--   bound form is derived FROM a free root and never exists without one.
data LanguageRoots = LanguageRoots
    { lrFree  ∷ !(M.Map ConceptId Text)
    , lrBound ∷ !(M.Map ConceptId Text)
    } deriving (Show, Eq)

-- | The assignment a language with no bound morphology has: free roots
--   only. What every version below 'boundFormVersion' produces, and the
--   reference point a test compares a bound rendering against.
freeRootsOnly ∷ M.Map ConceptId Text → LanguageRoots
freeRootsOnly free = LanguageRoots { lrFree = free, lrBound = M.empty }

-- | The per-language cap (#1096 requirement 2). Deliberately a small
--   fraction of the catalogue (~5% of the production 150): this is a
--   noticeable alternation on a handful of morphemes, not a second
--   lexicon.
maxBoundForms ∷ Int
maxBoundForms = 8

-- | The first generator version whose languages form bound morphemes.
--   A fixed literal, never a comparison against
--   'currentGeneratorVersion' (#1092 requirement 4): a world records the
--   version that named it, and an older world must keep rendering its
--   dependent slots with free forms after the current version advances.
boundFormVersion ∷ Int
boundFormVersion = 4

-- | Whether this profile's version forms bound morphemes at all.
formsBoundMorphemes ∷ Profile → Bool
formsBoundMorphemes prof =
    generatorVersionInt (profVersion prof) ≥ boundFormVersion

-- | The deterministic order concepts are considered in (#1096
--   requirement 2): ascending domain-separated rank, ties broken by
--   ascending 'ConceptId'.
--
--   The tie-break is load bearing, not decoration. 'boundSeed' is a
--   'Word64', so two concepts CAN in principle rank equal; without a
--   second key the survivor of that tie would be whichever the caller's
--   list happened to present first, and requirement 2's
--   "must not depend on catalogue traversal order" would hold only
--   probabilistically. With it the order is a total order on a set of
--   distinct ids, so it is a function of the SET alone.
boundSelectionOrder ∷ Profile → [ConceptId] → [ConceptId]
boundSelectionOrder prof ids = sortOn rankKey ids
  where
    rankKey cid = (boundSeed (profVersion prof) (profSeed prof) cid, cid)

-- | Assign bound forms over a language's complete free-root map.
--
--   Visits every concept in 'boundSelectionOrder' and accepts the first
--   legal, collision-free candidate each one offers, stopping once
--   'maxBoundForms' have been accepted or the catalogue is exhausted. A
--   concept whose candidates are all rejected is SKIPPED and the walk
--   continues, so a language ends up with at most — and often fewer
--   than — the cap.
--
--   Existing free roots are never rerolled or changed by any of this
--   (#1096 requirement 5): the free map is read-only input.
assignBoundForms ∷ Profile → M.Map ConceptId Text → M.Map ConceptId Text
assignBoundForms prof free
    | not (formsBoundMorphemes prof) = M.empty
    | otherwise = go (boundSelectionOrder prof (M.keys free)) M.empty
  where
    -- Every free form in the language, folded once. Collision is
    -- case-insensitive, and against the WHOLE free vocabulary rather
    -- than only the selected concepts' own roots.
    freeLower = S.fromList (map T.toLower (M.elems free))

    go [] acc = acc
    go (cid : rest) acc
        | M.size acc ≥ maxBoundForms = acc
        | otherwise = case M.lookup cid free of
            Nothing → go rest acc
            Just r  →
                let takenLower = S.fromList (map T.toLower (M.elems acc))
                    ok b = boundFormLegal prof r b
                           ∧ not (S.member (T.toLower b) freeLower)
                           ∧ not (S.member (T.toLower b) takenLower)
                in case filter ok (boundCandidates prof cid r) of
                    (b : _) → go rest (M.insert cid b acc)
                    []      → go rest acc

-- | The candidate bound forms one concept offers, best first.
--
--   Every candidate is a prefix of the free root retaining between
--   @⌈L/2⌉@ and @L-1@ characters: at least one visible character is a
--   floor the issue sets, but a form that kept only a single letter of a
--   nine-letter root would not be a recognizable alternation of it, so
--   the retained half is where the ladder actually starts. The first
--   candidate is drawn from this concept's own domain-separated value;
--   the rest descend from there.
--
--   Descending is the right direction because internal-cluster
--   legality is MONOTONE under truncation: a shorter prefix's adjacent
--   pairs are a subset of a longer one's, so if a candidate fails
--   'boundFormAdmissible' every longer one fails too. The ladder is
--   bounded by the root's own length — a fixed, tiny list built once at
--   assignment time, never a search at render time.
boundCandidates ∷ Profile → ConceptId → Text → [Text]
boundCandidates prof cid r
    | len < 2   = []
    | otherwise = [ T.take k r | k ← [k0, k0 - 1 .. kMin] ]
  where
    len  = T.length r
    kMin = max 1 ((len + 1) `div` 2)
    kMax = len - 1
    k0   = wordInRange (draw (boundSeed (profVersion prof) (profSeed prof) cid) 1)
                       kMin kMax

-- | Whether @bound@ is an acceptable bound form of the free root
--   @free@ (#1096 requirements 3 and 4).
--
--   "Differs only by deleting terminal characters" is stated exactly
--   once, as "nonempty strict prefix, strictly shorter" — the two
--   together admit no other edit, which is what excludes stem
--   substitution, internal deletion, and gradation structurally rather
--   than by a rule somebody has to remember.
boundFormLegal ∷ Profile → Text → Text → Bool
boundFormLegal prof free bound =
    not (T.null bound)
    ∧ T.length bound < T.length free
    ∧ bound `T.isPrefixOf` free
    ∧ T.any asciiLetter bound
    ∧ boundFormAdmissible prof bound
  where
    asciiLetter c = isAsciiUpper c ∨ isAsciiLower c

-- | Whether every adjacent pair of CONSONANT-CAPABLE visible characters
--   inside a bound form is accepted by this profile's own exported
--   relation (#1096 requirement 4).
--
--   "Consonant-capable" is the scoping the requirement names, and it is
--   deliberately WIDER than #1095's boundary repair, which asks about
--   consonant-ONLY pairs. A dual-role @y@ (#1094 requirement 6) is
--   consonant-capable, so a @by@ adjacency inside a candidate is
--   validated here even though the same adjacency at a morpheme
--   boundary would not be treated as a cluster. The two are not in
--   tension: this is a candidate FILTER, free to be conservative
--   because a rejected candidate only costs one concept its bound form,
--   whereas the boundary rule REWRITES text and must not disturb an
--   adjacency whose slot provenance it cannot know.
--
--   No second legality predicate is introduced: the verdict is always
--   'admissibleOnset''s. Identical adjacent consonants are rejected by
--   the relation itself (it is irreflexive), so a doubled consonant
--   makes a candidate inadmissible and the ladder simply moves on; that
--   costs a candidate, never a doubled letter in completed output,
--   which comes from the free head root and from morphemes this
--   predicate never sees.
boundFormAdmissible ∷ Profile → Text → Bool
boundFormAdmissible prof bound = all ok (T.zip bound (T.drop 1 bound))
  where
    ok (a, b)
        | consonantCapable prof a ∧ consonantCapable prof b
        = admissibleOnset prof a b
        | otherwise
        = True

-- | How many accepted bound forms collide, case-insensitively, with
--   another concept's free form or with another accepted bound form
--   (#1096 requirement 5's second total — reported SEPARATELY from
--   'Language.Generated.Report.countDuplicateRoots'' free/free count, so
--   the two kinds of collision can never be confused for one another).
--
--   Zero for a correctly-selecting generator: 'assignBoundForms' skips
--   any candidate that would collide. A nonzero count here is a
--   generator bug, not an expected occurrence.
countBoundCollisions ∷ M.Map ConceptId Text → M.Map ConceptId Text → Int
countBoundCollisions free bound = length
    [ ()
    | (cid, b) ← M.toList bound
    , let bl = T.toLower b
    , S.member bl (otherFreeLower cid) ∨ S.member bl (otherBoundLower cid) ]
  where
    otherFreeLower cid = S.fromList
        [ T.toLower r | (c, r) ← M.toList free, c ≢ cid ]
    otherBoundLower cid = S.fromList
        [ T.toLower r | (c, r) ← M.toList bound, c ≢ cid ]
