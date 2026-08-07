{-# LANGUAGE Strict #-}
-- | Naming a placed location in its world's own language (#1101, epic
--   #708). This is the arc's first real surface area: a world name
--   alone is one string in isolation, but two ruins named from the SAME
--   language — and from the same language that named their world — is
--   what lets a player see a root recur and infer that it means
--   something.
--
--   The three layers this sits on are already built and unchanged here:
--   #709's language-independent 'NameExpr' over stable 'ConceptId's,
--   #710's generated 'Profile' + concept roots, and #1092's persisted
--   'LanguageProvenance' that says WHICH language named a world. This
--   module only decides which expression a given instance gets, and
--   renders it twice — natively and as an English gloss.
--
--   /Write-once./ #708 principle 5 applies to every generated name, not
--   just a world's: a name is rendered when the instance is CREATED and
--   stored ('Location.Instance.newLocationInstance'). Nothing here is
--   ever consulted again for an instance that already exists — not on
--   load, not on migration, not when a definition is edited.
--
--   /No language, no invention./ Provenance is optional by design
--   (#1092 requirement 2): a custom-named world has no language, and a
--   world saved before provenance was recorded has none recoverable.
--   Both fall back to the definition's 'ldLabel' with NO gloss —
--   absence is never papered over by inventing a language.
--
--   Pure: no engine, world, Lua, IO, or wall-clock state. The only
--   thing a caller must supply from outside is a 'Catalogue' and the
--   world's 'LanguageProvenance'.
module Location.Naming
    ( LocationNamer(..)
    , mkLocationNamer
    , locationNameExpr
    , nameLocationInstance
    , locationNamingErrors
    ) where

import UPrelude
import qualified Data.Text as T
import Language.Semantic.Types
import Language.Semantic.English (renderGloss)
import Language.Generated.Types
    ( LanguageProvenance(..), Profile(..), GeneratorError, LangSeed(..)
    , GeneratorVersion(..) )
import Language.Generated.Profile (generateProfile)
import Language.Generated.Root (assignLanguageRoots)
import Language.Generated.Bound (LanguageRoots)
import Language.Generated.Render (renderNative)
import Language.Generated.Hash (fmix64, textSeed, draw, pickIndex)
import Location.Types (LocationDef(..), LocationNaming(..))

-- | Everything needed to name locations in ONE world's language,
--   resolved once per world rather than once per instance: the
--   language's 'Profile', its concept→morpheme assignment, and the
--   catalogue the English gloss is rendered from.
--
--   The roots are assigned over the WHOLE catalogue
--   ('assignLanguageRoots' with every 'conceptIds' entry), which is
--   what makes a location's root for @KEEP@ the identical morpheme the
--   world's own name would use for @KEEP@ — root assignment resolves
--   collisions across the full concept set, so a partial set would
--   silently produce a different language.
data LocationNamer = LocationNamer
    { lnrProfile   ∷ !Profile
    , lnrRoots     ∷ !LanguageRoots
    , lnrCatalogue ∷ !Catalogue
    }

-- | Build a namer for a world's recorded language. Fails only when the
--   provenance names a generator version this build cannot construct —
--   which is #710 requirement 15's error, reported rather than silently
--   substituted with the current version (that would render names in a
--   DIFFERENT language than the one that named the world).
mkLocationNamer
    ∷ Catalogue → LanguageProvenance → Either GeneratorError LocationNamer
mkLocationNamer cat prov = do
    prof ← generateProfile (lpVersion prov) (lpSeed prov)
    pure LocationNamer
        { lnrProfile   = prof
        , lnrRoots     = assignLanguageRoots prof (conceptIds cat)
        , lnrCatalogue = cat
        }

-- | The name expression one instance gets: a 'Modifier' compound over
--   the definition's own authored pools.
--
--   Deterministic from the instance's identity (#1101 requirement 3).
--   'Location.Instance.LocationInstanceId' is stable, page-local,
--   allocated at placement time in 'Location.Overlay.Types.overlayToList'
--   order, and survives save/load and chunk eviction — so a choice
--   derived from it is reproducible, while one derived from hashmap
--   iteration order or placement wall-clock is not.
--
--   The language's own seed and version are mixed in beside the
--   definition id and the instance id. Without them every world's first
--   ruin would draw the SAME two concepts and only their spelling would
--   differ, which reads as mechanical; with them two worlds differ in
--   meaning as well as in phonology. The mix is the same integer-only
--   avalanche the rest of the generated-language layer uses (#710
--   requirement 12 — no floating point, identical on every platform).
--
--   Takes the instance id as a plain 'Int' rather than the newtype so
--   this module stays below 'Location.Instance' in the import graph.
locationNameExpr ∷ Profile → LocationDef → Int → NameExpr
locationNameExpr prof def rawId =
    Modifier (pick 0 (lnModifiers naming)) (pick 1 (lnHeads naming))
  where
    naming = ldNaming def
    base   = instanceNameSeed prof def rawId
    -- Total for the pools this can ever see: both are validated
    -- nonempty when the definition loads, and a 'Bare' fallback for an
    -- empty pool would silently name a location under a scheme its
    -- author did not write. An empty pool cannot reach here.
    pick _    []       = ConceptId ""
    pick step pool@(_:_) =
        pool !! pickIndex (draw base step) (length pool)

-- | The per-instance draw seed: language seed, generator version,
--   definition id, and instance id, mixed. Mirrors
--   'Language.Generated.Hash.conceptSeed''s shape — the same fixed
--   golden-ratio\/mixing constants the rest of the layer uses.
instanceNameSeed ∷ Profile → LocationDef → Int → Word64
instanceNameSeed prof def rawId =
    fmix64 $ langSeedWord (profSeed prof)
       `xor` fmix64 (fromIntegral (generatorVersionInt (profVersion prof))
                        * 0x9E3779B97F4A7C15)
       `xor` textSeed (ldId def)
       `xor` fmix64 (fromIntegral rawId * 0xD6E8FEB86659FD93)

-- | One instance's stored @(display name, English gloss)@ pair.
--
--   With no namer — a world with no language provenance (#1101
--   requirement 6) — this is the definition's 'ldLabel' and NO gloss,
--   exactly today's behavior.
--
--   With one, both renderings come from the SAME 'NameExpr', so the
--   gloss always explains the name beside it. A rendering failure also
--   falls back to label + no gloss: it is defensive only, since the
--   definition's pools are validated against this catalogue at load
--   time and the roots cover every catalogue concept, so neither
--   renderer has a reachable failure here.
nameLocationInstance
    ∷ Maybe LocationNamer → LocationDef → Int → (Text, Maybe Text)
nameLocationInstance Nothing    def _     = (ldLabel def, Nothing)
nameLocationInstance (Just nmr) def rawId =
    case (renderNative prof (lnrRoots nmr) expr
         , renderGloss (lnrCatalogue nmr) expr) of
        (Right native, Right gloss) → (native, Just gloss)
        _                           → (ldLabel def, Nothing)
  where
    prof = lnrProfile nmr
    expr = locationNameExpr prof def rawId

-- | Validate a definition's authored naming scheme against the concept
--   catalogue. Empty ⇒ the scheme is usable. Run when the definition
--   FILE loads, so a bad scheme is a loud, definition-and-field-naming
--   rejection rather than a location that silently falls back to
--   'ldLabel' in every world forever (the fallback means "this world
--   has no language", and authored data must never be able to fake it).
--
--   Each pool is checked for the lexical form its slot actually needs:
--   'Modifier' renders its modifier slot from 'FormModifier' and its
--   head slot from 'FormSingular', which is exactly what
--   'Language.Semantic.English.renderGloss' demands.
locationNamingErrors ∷ Catalogue → LocationDef → [Text]
locationNamingErrors cat def =
    poolErrors "heads" FormSingular (lnHeads naming)
    ⧺ poolErrors "modifiers" FormModifier (lnModifiers naming)
  where
    naming = ldNaming def
    prefix = "location '" <> ldId def <> "': naming."

    poolErrors field kind pool
        | null pool = [prefix <> field <> " must not be empty"]
        | otherwise = concatMap (conceptErrors field kind) pool

    conceptErrors field kind cid@(ConceptId raw) =
        case lookupConcept cid cat of
            Nothing → [prefix <> field <> " references unknown concept '"
                        <> raw <> "'"]
            Just ce → case formOf kind ce of
                Nothing → [prefix <> field <> " concept '" <> raw
                            <> "' has no " <> formKindText kind <> " form"]
                Just w | T.null w →
                    [prefix <> field <> " concept '" <> raw
                        <> "' has an empty " <> formKindText kind <> " form"]
                Just _ → []
