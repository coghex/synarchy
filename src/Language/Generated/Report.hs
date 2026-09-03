{-# LANGUAGE Strict #-}
-- | The generated-language report's pure data layer (#710 requirements
--   17-20): one canonical (label, 'NameExpr') set covering every #709
--   name form, shared verbatim by @tools/language_report.py@'s
--   underlying Haskell dispatch and by the focused hspec golden tests
--   (#710 requirement 19 amendment — "one deterministic canonical
--   expression set shared by report generation and report tests"), and
--   a pure function assembling one seed's full report record from it.
--   No IO here — 'buildSeedReport' only needs an already-loaded
--   'Catalogue'.
--
--   #1096 adds a SECOND, separate dataset beside that canonical one:
--   every selected concept rendered in 'Bare' and in each dependent
--   slot. It is kept separate deliberately. @tools/language_report.py@
--   thresholds its distinct-name ratio, profile-signature count, and
--   pinned length distribution against the canonical set's population,
--   so folding tens of thousands of extra renderings into that array
--   would move those denominators and let a real regression hide behind
--   added sample volume. The new names are still subject to every
--   zero-gated structural check (output contract, 3-32 length,
--   triple-letter runs).
module Language.Generated.Report
    ( canonicalExpressions
    , CanonicalRendering(..)
    , BoundFormRecord(..)
    , BoundSlotRendering(..)
    , SeedReport(..)
    , buildSeedReport
    , countDuplicateRoots
    , boundSlotExpressions
    ) where

import UPrelude
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Language.Semantic.Types
import Language.Semantic.English (renderGloss)
import Language.Generated.Types
import Language.Generated.Profile (generateProfile)
import Language.Generated.Root (assignLanguageRoots)
import Language.Generated.Bound
import Language.Generated.Render (renderNative, nativeRenderErrorText)
import Language.Generated.Signature (profileSignature)

-- | The canonical (label, expression) set: every #709 name form, using
--   the exact concept ids #709's own pinned English golden tests use
--   (@Semantic.hs@'s "pinned acceptance forms" block), so a #710 golden
--   native rendering sits directly alongside its already-pinned gloss.
canonicalExpressions ∷ [(Text, NameExpr)]
canonicalExpressions =
    [ ("bare",       Bare (ConceptId "SILENCE"))
    , ("modifier",    Modifier (ConceptId "ASH") (ConceptId "LAND"))
    , ("of_singular", Of (ConceptId "GATE") Singular (ConceptId "WINTER"))
    , ("of_plural",   Of (ConceptId "EYE") Plural (ConceptId "STORM"))
    , ("possessive",  Possessive (ConceptId "WOLF") (ConceptId "HEART"))
    ]

-- | #1096 requirement 6's complete slot matrix for ONE concept, as
--   expressions: the concept standing alone, then filling the dependent
--   slot of every compound form against a fixed head.
--
--   This is the single statement of which slot is dependent, shared by
--   the report and its tests exactly as 'canonicalExpressions' is —
--   a matrix asserted in one place and generated in another is a matrix
--   that can drift.
boundSlotExpressions ∷ ConceptId → ConceptId → [(Text, NameExpr)]
boundSlotExpressions dep hd =
    [ ("bare",        Bare dep)
    , ("modifier",    Modifier dep hd)
    , ("of_singular", Of hd Singular dep)
    , ("of_plural",   Of hd Plural dep)
    , ("possessive",  Possessive dep hd)
    ]

-- | One canonical expression's renderings for one seed. Failures are
--   kept as descriptive text (never silently dropped) so the report and
--   its tests can distinguish "rendered X" from "failed with reason Y".
data CanonicalRendering = CanonicalRendering
    { crForm   ∷ !Text
    , crNative ∷ !(Either Text Text)
    , crGloss  ∷ !(Either Text Text)
    } deriving (Show, Eq)

-- | One concept that received a bound form, with both of its morphemes
--   and the Haskell-computed verdict of #1094's exported admissibility
--   predicate.
--
--   The verdict is computed HERE rather than left for the Python
--   checker on purpose: @tools/language_report.py@'s documented
--   contract is that it reimplements no generation logic and only
--   inspects the generator's real output, and the admissibility
--   relation is generation logic. The prefix and collision rules ARE
--   directly checkable from the exposed strings, so only this one
--   signal has to cross the boundary as a verdict.
data BoundFormRecord = BoundFormRecord
    { bfConcept    ∷ !ConceptId
    , bfFree       ∷ !Text
    , bfBound      ∷ !Text
    , bfAdmissible ∷ !Bool
    } deriving (Show, Eq)

-- | One selected concept rendered in one slot of the matrix.
--
--   @bsrShortened@ is the honest "a shortening is VISIBLE in completed
--   output" signal: the same expression rendered with this language's
--   bound forms suppressed, compared against the real rendering. It is
--   not simply "the concept has a bound form" — a boundary repair can
--   insert a linking segment that gives back exactly what the
--   shortening removed, and that case must not be counted as a visible
--   alternation.
data BoundSlotRendering = BoundSlotRendering
    { bsrConcept   ∷ !ConceptId
    , bsrSlot      ∷ !Text
    , bsrNative    ∷ !(Either Text Text)
    , bsrShortened ∷ !Bool
    } deriving (Show, Eq)

data SeedReport = SeedReport
    { srSeed              ∷ !Word64
    , srProfile           ∷ !Profile
    , srProfileSignature  ∷ !Text
    , srRenderings        ∷ ![CanonicalRendering]
    , srRootCollisions    ∷ !Int
    , srBoundForms        ∷ ![BoundFormRecord]
    , srBoundCollisions   ∷ !Int
    , srBoundRenderings   ∷ ![BoundSlotRendering]
    } deriving (Show, Eq)

-- | Build one seed's full report at an EXPLICIT generator version: its
--   profile, profile signature, native + English renderings of the
--   canonical expression set, the count of concept-root collisions
--   remaining after resolution over @cat@'s complete concept catalogue,
--   and #1096's bound-form dataset.
--
--   Both collision counts should always be 0 — resolution is supposed to
--   eliminate free/free collisions and bound-form selection is supposed
--   to skip any candidate that would collide — so they are the
--   diagnostics @tools/language_report.py --check@ pins to zero, kept
--   as two SEPARATE totals (#1096 requirement 5) so a report can say
--   which kind occurred.
--
--   The version is a parameter and construction goes through the real
--   'generateProfile' dispatcher (#1094 requirement 9): hardcoding a
--   single version's builder here while the JSON header reported
--   'currentGeneratorVersion' would mislabel every profile the moment
--   the current version advanced. An unsupported version fails
--   descriptively rather than silently falling back, and so does a seed
--   whose profile has too small a root space to name the catalogue
--   (#2206).
buildSeedReport ∷ Catalogue → GeneratorVersion → Word64
                → Either GeneratorError SeedReport
buildSeedReport cat ver rawSeed = do
    prof ← generateProfile ver (LangSeed rawSeed)
    roots ← assignLanguageRoots prof (catOrdinals cat) (conceptIds cat)
    let free  = lrFree roots
        bound = lrBound roots
        -- The same assignment with bound morphology suppressed: the
        -- reference every "did this actually shorten the output?"
        -- comparison below is made against.
        freeOnly = freeRootsOnly free

        renderOne (label, expr) = CanonicalRendering
            { crForm = label
            , crNative = either (Left ∘ nativeRenderErrorText) Right
                                (renderNative prof roots expr)
            , crGloss = either (Left ∘ renderErrorText) Right
                                (renderGloss cat expr)
            }

        boundRecord (c, b) = BoundFormRecord
            { bfConcept    = c
            , bfFree       = M.findWithDefault "" c free
            , bfBound      = b
            , bfAdmissible = boundFormAdmissible prof b
            }

        slotRendering c (label, expr) = BoundSlotRendering
            { bsrConcept = c
            , bsrSlot    = label
            , bsrNative  = either (Left ∘ nativeRenderErrorText) Right
                                  (renderNative prof roots expr)
            , bsrShortened =
                renderNative prof roots expr ≢ renderNative prof freeOnly expr
            }

    pure SeedReport
        { srSeed = rawSeed
        , srProfile = prof
        , srProfileSignature = profileSignature prof
        , srRenderings = map renderOne canonicalExpressions
        , srRootCollisions = countDuplicateRoots free
        , srBoundForms = map boundRecord (M.toList bound)
        , srBoundCollisions = countBoundCollisions free bound
        , srBoundRenderings =
            [ slotRendering c row
            | c ← M.keys bound
            , row ← boundSlotExpressions c (headConceptFor cat c) ]
        }

-- | The fixed head concept a selected concept is rendered against.
--
--   The catalogue's own first id in ascending order, skipping the
--   selected concept itself so an expression never puts one concept in
--   both slots. Deterministic and independent of which concepts were
--   selected, so the dataset is comparable across seeds.
headConceptFor ∷ Catalogue → ConceptId → ConceptId
headConceptFor cat c = case filter (≢ c) (conceptIds cat) of
    (h : _) → h
    []      → c

-- | How many concept-root assignments remain case-insensitively
--   duplicated after collision resolution (#710 requirement 16). Zero
--   for a correctly-resolving generator; a nonzero count here is a
--   generator bug, not an expected occurrence.
--
--   FREE/FREE only, and deliberately so: #1096 reports collisions
--   involving a bound form as its own separate total
--   ('countBoundCollisions'), because the two have different causes and
--   different fixes.
countDuplicateRoots ∷ M.Map ConceptId Text → Int
countDuplicateRoots roots =
    let counts = M.fromListWith (+)
            [ (T.toLower r, 1 ∷ Int) | r ← M.elems roots ]
    in sum [ c - 1 | c ← M.elems counts, c > 1 ]
