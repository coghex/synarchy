{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | The generated-language report's pure data layer (#710 requirements
--   17-20): one canonical (label, 'NameExpr') set covering every #709
--   name form, shared verbatim by @tools/language_report.py@'s
--   underlying Haskell dispatch and by the focused hspec golden tests
--   (#710 requirement 19 amendment — "one deterministic canonical
--   expression set shared by report generation and report tests"), and
--   a pure function assembling one seed's full report record from it.
--   No IO here — 'buildSeedReport' only needs an already-loaded
--   'Catalogue'.
module Language.Generated.Report
    ( canonicalExpressions
    , CanonicalRendering(..)
    , SeedReport(..)
    , buildSeedReport
    , countDuplicateRoots
    ) where

import UPrelude
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Language.Semantic.Types
import Language.Semantic.English (renderGloss)
import Language.Generated.Types
import Language.Generated.Profile (buildProfileV1)
import Language.Generated.Root (assignRoots)
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

-- | One canonical expression's renderings for one seed. Failures are
--   kept as descriptive text (never silently dropped) so the report and
--   its tests can distinguish "rendered X" from "failed with reason Y".
data CanonicalRendering = CanonicalRendering
    { crForm   ∷ !Text
    , crNative ∷ !(Either Text Text)
    , crGloss  ∷ !(Either Text Text)
    } deriving (Show, Eq)

data SeedReport = SeedReport
    { srSeed             ∷ !Word64
    , srProfile          ∷ !Profile
    , srProfileSignature ∷ !Text
    , srRenderings       ∷ ![CanonicalRendering]
    , srRootCollisions   ∷ !Int
    } deriving (Show, Eq)

-- | Build one seed's full report: its profile, profile signature,
--   native + English renderings of the canonical expression set, and
--   the count of concept-root collisions remaining after resolution
--   over @cat@'s complete concept catalogue. That count should always
--   be 0 — collision resolution is supposed to eliminate every
--   collision — so it is the diagnostic 'countDuplicateRoots' pins to
--   zero for @tools/language_report.py --check@.
buildSeedReport ∷ Catalogue → Word64 → SeedReport
buildSeedReport cat rawSeed =
    let prof = buildProfileV1 (LangSeed rawSeed)
        roots = assignRoots prof (conceptIds cat)
        renderOne (label, expr) = CanonicalRendering
            { crForm = label
            , crNative = either (Left ∘ nativeRenderErrorText) Right
                                (renderNative prof roots expr)
            , crGloss = either (Left ∘ renderErrorText) Right
                                (renderGloss cat expr)
            }
    in SeedReport
        { srSeed = rawSeed
        , srProfile = prof
        , srProfileSignature = profileSignature prof
        , srRenderings = map renderOne canonicalExpressions
        , srRootCollisions = countDuplicateRoots roots
        }

-- | How many concept-root assignments remain case-insensitively
--   duplicated after collision resolution (#710 requirement 16). Zero
--   for a correctly-resolving generator; a nonzero count here is a
--   generator bug, not an expected occurrence.
countDuplicateRoots ∷ M.Map ConceptId Text → Int
countDuplicateRoots roots =
    let counts = M.fromListWith (+)
            [ (T.toLower r, 1 ∷ Int) | r ← M.elems roots ]
    in sum [ c - 1 | c ← M.elems counts, c > 1 ]
