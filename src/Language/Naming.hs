{-# LANGUAGE Strict #-}
-- | The naming machine shared by every generated proper name in a
--   world (epic #708): resolve ONE world's language once, then render
--   'NameExpr's through it natively and as an English gloss.
--
--   #1101 built this inside "Location.Naming" for placed locations.
--   #1102 named rivers the same way, so the language-facing half moved
--   here and both callers are now users of ONE implementation rather
--   than two parallel copies. What stays with each caller is only what
--   is genuinely its own: which concepts it may draw on, and what its
--   fallback is when the page has no language.
--
--   The three layers this sits on are unchanged: #709's
--   language-independent 'NameExpr' over stable 'ConceptId's, #710's
--   generated 'Profile' + concept roots, and #1092's persisted
--   'LanguageProvenance' that says WHICH language named a world.
--
--   Pure: no engine, world, Lua, IO, or wall-clock state. A caller
--   supplies a 'Catalogue' and the page's 'LanguageProvenance'.
module Language.Naming
    ( Namer(..)
    , mkNamer
    , namerProvenance
    , renderNamed
    , nameDrawSeed
    ) where

import UPrelude
import Language.Semantic.Types (Catalogue(..), NameExpr, conceptIds)
import Language.Semantic.English (renderGloss)
import Language.Generated.Types
    ( LanguageProvenance(..), Profile(..), GeneratorError, LangSeed(..)
    , GeneratorVersion(..) )
import Language.Generated.Profile (generateProfile)
import Language.Generated.Root (assignLanguageRoots)
import Language.Generated.Bound (LanguageRoots)
import Language.Generated.Render (renderNative)
import Language.Generated.Hash (fmix64, textSeed)

-- | Everything needed to name things in ONE world's language, resolved
--   once per world rather than once per named entity: the language's
--   'Profile', its concept→morpheme assignment, and the catalogue the
--   English gloss is rendered from.
--
--   The roots are assigned over the WHOLE catalogue
--   ('assignLanguageRoots' with every 'conceptIds' entry), which is
--   what makes a location's root for @KEEP@ — or a river's for @FORD@ —
--   the identical morpheme the world's own name would use for it: root
--   assignment resolves collisions across the full concept set, so a
--   partial set would silently produce a different language.
data Namer = Namer
    { nmrProfile   ∷ !Profile
    , nmrRoots     ∷ !LanguageRoots
    , nmrCatalogue ∷ !Catalogue
    }

-- | Build a namer for a world's recorded language. Two failures, both
--   reported rather than silently substituted with another language —
--   that would render names in a DIFFERENT language than the one that
--   named the world:
--
--   * the provenance names a generator version this build cannot
--     construct (#710 requirement 15);
--   * the profile builds, but its root space is too small to give every
--     catalogue concept a distinct root (#2206), so no assignment over
--     this catalogue exists.
mkNamer ∷ Catalogue → LanguageProvenance → Either GeneratorError Namer
mkNamer cat prov = do
    prof ← generateProfile (lpVersion prov) (lpSeed prov)
    roots ← assignLanguageRoots prof (catOrdinals cat) (conceptIds cat)
    pure Namer
        { nmrProfile   = prof
        , nmrRoots     = roots
        , nmrCatalogue = cat
        }

-- | One expression's @(native text, English gloss)@ pair, both rendered
--   from the SAME 'NameExpr' so the gloss always explains the name
--   beside it. 'Nothing' when either rendering fails, which every
--   caller turns into its own no-language fallback rather than into
--   half a name.
-- | The #1092 provenance a namer renders under, read back off the
--   profile it already resolved. This is what #1104's etymology source
--   records beside a name's expression, so a rendered name's language
--   is stated by the thing that rendered it rather than looked up again
--   somewhere that could answer differently.
namerProvenance ∷ Namer → LanguageProvenance
namerProvenance nmr = LanguageProvenance
    { lpSeed    = profSeed (nmrProfile nmr)
    , lpVersion = profVersion (nmrProfile nmr)
    }

renderNamed ∷ Namer → NameExpr → Maybe (Text, Text)
renderNamed nmr expr =
    case ( renderNative (nmrProfile nmr) (nmrRoots nmr) expr
         , renderGloss (nmrCatalogue nmr) expr ) of
        (Right native, Right gloss) → Just (native, gloss)
        _                           → Nothing

-- | The per-entity draw seed a caller mixes its concept choices from:
--   the language's own seed and generator version, a caller-supplied
--   kind tag, and the entity's stable id.
--
--   The language seed and version are in the mix because without them
--   every world's first ruin (or first river) would draw the SAME
--   concepts and only their spelling would differ, which reads as
--   mechanical; with them two worlds differ in meaning as well as in
--   phonology. The tag separates kinds sharing one id space — and, for
--   locations, one definition's instances from another's.
--
--   The mix is the same integer-only avalanche the rest of the
--   generated-language layer uses (#710 requirement 12 — no floating
--   point, identical on every platform), mirroring
--   'Language.Generated.Hash.conceptSeed''s shape.
nameDrawSeed ∷ Profile → Text → Int → Word64
nameDrawSeed prof tag rawId =
    fmix64 $ langSeedWord (profSeed prof)
       `xor` fmix64 (fromIntegral (generatorVersionInt (profVersion prof))
                        * 0x9E3779B97F4A7C15)
       `xor` textSeed tag
       `xor` fmix64 (fromIntegral rawId * 0xD6E8FEB86659FD93)
