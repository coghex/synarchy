-- | Shared fixtures and canonical samples for the generated-language
--   suite (#2067). Owns what more than one of the versioned
--   contract owners needs; a fixture used by exactly one of them
--   lives in that owner's module instead.
--
--   Imports no versioned spec owner, so the dependency runs one way:
--   façade → owners → here → production @Language.*@ modules.
module Test.Headless.Language.Generated.Support
    ( Ctx(..)
    , loadProductionCatalogue
    , cid
    , wordInitialOnsets
    , boundaryFixture
    , contractOk
    ) where

import UPrelude
import Data.Char (toLower)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Semantic.Types
import Language.Semantic.Catalogue
import Language.Generated.Types
import Language.Generated.Orthography
import Language.Generated.Onset
import Language.Generated.Bound
import Language.Generated.Profile
import Language.Generated.Root
import Language.Generated.Render
import Language.Generated.Report (canonicalExpressions)

cid ∷ Text → ConceptId
cid = ConceptId

-- | The two-consonant onsets in a rendered NAME that a @CCV@ syllable
--   provably produced: the first two glyphs, and the first two after
--   each @-@ join. Roots are flat text (#1094 requirement 7), so
--   interior adjacencies come from syllable/compound concatenation and
--   belong to L1c. A position counts only when both glyphs are
--   consonant-capable and NEITHER is vowel-capable — otherwise a
--   dual-role @y@ filling a vowel slot would masquerade as an onset.
--   The capitalized initial is folded first.
wordInitialOnsets ∷ Profile → Text → [(Char, Char)]
wordInitialOnsets prof name =
    [ (a, b)
    | i ← 0 : [ j + 1 | (j, c) ← zip [0 ..] glyphs, c ≡ '-' ]
    , (a : b : _) ← [ drop i glyphs ]
    , consonantCapable prof a, consonantCapable prof b
    , not (vowelCapable prof a), not (vowelCapable prof b)
    ]
  where
    glyphs = case T.unpack name of
        (c : cs) → toLower c : cs
        []       → []
    -- Full 'toLower', not an ASCII shift: rendering capitalizes the
    -- initial and #1100's repertoire has uppercase forms of its own, so
    -- an ASCII-only fold would silently skip every marked-initial name
    -- instead of checking its onset.

-- | A tiny, fully explicit version-3 language for #1095's boundary
--   unit tests. Hand-built rather than seed-drawn so each rule's
--   behaviour is READ off the fixture instead of inferred from whatever
--   a seed happened to draw: consonants @bhkst@, vowels @aeo@, the only
--   admissible onsets @bh@ and @ks@, a one-letter @h@ possessive and a
--   one-letter @s@ plural, epenthetic vowel @a@, linking consonants
--   @k@/@t@.
boundaryFixture ∷ BoundaryRule → Profile
boundaryFixture rule = Profile
    { profVersion        = GeneratorVersion 3
    , profSeed           = LangSeed 0
    , profConsonants     = "bhkst"
    , profVowels         = "aeo"
    , profSyllableShapes = [SyllableShape [ConsonantSlot, VowelSlot]]
    , profMinSyllables   = 1
    , profMaxSyllables   = 2
    , profCompoundOrder  = ModifierFirst
    , profPossessive     = PossessiveMarking OwnerFirst "h"
    , profPlural         = PluralMarking "s"
    , profJoin           = JoinCompact
    , profOnset          = OnsetRelation (S.fromList [('b', 'h'), ('k', 's')])
    , profBoundary       = BoundaryMediated BoundaryRepair
        { brRule       = rule
        , brEpenthetic = 'a'
        , brLinker     = 'k'
        , brLinkerAlt  = 't'
        }
    }

-- | The 3-32/repertoire/capitalization/punctuation contract every
--   rendered native word must satisfy (#710 requirement 6, widened to
--   #1100's extended repertoire), pinned as a concrete predicate rather
--   than left implicit.
--
--   Structurally EQUIVALENT to @tools/language_report.py@'s
--   @CONTRACT_RE@ — @^[UPPER][lower]*(?:['-][lower]+)*\$@ — rather than
--   a looser set of separate conditions. It used to check the initial,
--   the final character, the admitted character set and repeated marks
--   independently, which let through three shapes the regex rejects: an
--   uppercase letter in the INTERIOR (@KAra@), and two DIFFERENT marks
--   side by side (@K-'ara@, @K'-ara@). One contract stated two ways is
--   one contract only if the two agree on every string.
--
--   Length is counted in CODE POINTS (#1100 requirement 6): 'T.length'
--   already does, and every repertoire member is one precomposed code
--   point, so a marked name and its unmarked equivalent measure the
--   same.
--
--   Deliberately stated over
--   'Language.Generated.Orthography.outputInventory' rather than over
--   whichever characters a profile happens to hold: this is the
--   contract, and the version-5 owner's separate per-language check is
--   what proves an emitted mark also belonged to the language that
--   emitted it.
contractOk ∷ Text → Bool
contractOk w =
    T.length w ≥ 3 ∧ T.length w ≤ 32 ∧ wellFormed (T.unpack w)
  where
    -- ^[UPPER] then the remainder.
    wellFormed (c : cs) = isNameInitial c ∧ leadingRun cs
    wellFormed []       = False

    -- [lower]* — the run after the initial may be empty.
    leadingRun cs = case break isMark cs of
        (letters, [])       → all isNameLower letters
        (letters, _ : rest) → all isNameLower letters ∧ markedRun rest

    -- (?:['-][lower]+)* — every later run follows exactly one mark and
    -- must be NONEMPTY, which is what rejects a trailing mark, two
    -- identical marks, and two different ones alike.
    markedRun cs = case break isMark cs of
        ([], _)             → False
        (letters, [])       → all isNameLower letters
        (letters, _ : rest) → all isNameLower letters ∧ markedRun rest

    isMark c = c `elem` nameMarks

-- | Every value the split's child spec owners share, built once and
--   threaded into each of them as an ordinary argument.
--
--   The façade performs the ONE catalogue load per suite run and hands
--   the result to every owner (#2067 requirements 5, 6): there is
--   deliberately no memoizing top-level CAF here, because a child owner
--   introducing mutable global state is exactly what this split forbids.
--
--   Every field is lazy — no @StrictData@, no bangs — so a narrower
--   Hspec @--match@ still forces only the samples the selected examples
--   actually consume, which is what the expensive version-4 and
--   version-5 assignment sweeps depend on.
data Ctx = Ctx
    { prodCat            ∷ Catalogue
    , prodOrds           ∷ ConceptOrdinals
    , rootsFor           ∷ Profile → LanguageRoots
    , renderingsFor      ∷ Profile → [Either Text Text]
    , nativeRenderings   ∷ Word64 → [Either Text Text]
    , nativeRenderingsV2 ∷ Word64 → [Either Text Text]
    , nativeRenderingsV3 ∷ Word64 → [Either Text Text]
    , nativeRenderingsV4 ∷ Word64 → [Either Text Text]
    , nativeRenderingsV5 ∷ Word64 → [Either Text Text]
    , v2Profiles         ∷ [Profile]
    , v2ProfilesWithRole ∷ YRole → [Profile]
    , v3Profiles         ∷ [Profile]
    , v4Profiles         ∷ [Profile]
    , v4Assignments      ∷ [(Profile, LanguageRoots)]
    , v4BoundForms       ∷ [(Profile, ConceptId, Text, Text)]
    , v5Profiles         ∷ [Profile]
    , v5Marked           ∷ [Profile]
    , v5Plain            ∷ [Profile]
    , v5Assignments      ∷ [(Profile, LanguageRoots)]
    , headAgainst        ∷ ConceptId → ConceptId
    }

-- | Read the production concept catalogue and its ordinal artifact, and
--   build the shared context over them. Called from exactly one
--   'Test.Hspec.runIO' in the façade, so the YAML and the JSON are each
--   parsed once per focused suite run.
loadProductionCatalogue ∷ IO Ctx
loadProductionCatalogue = do
    prodCatE ← loadCatalogue conceptCataloguePath conceptOrdinalPath
    pure $ mkCtx (either (error ∘ T.unpack ∘ catalogueErrorText) id prodCatE)

-- | The shared samples over one loaded catalogue.
mkCtx ∷ Catalogue → Ctx
mkCtx prodCat = Ctx {..}
  where
    prodOrds = catOrdinals prodCat

    -- Every canonical expression's native rendering for one seed,
    -- assigning roots over the COMPLETE production catalogue (#710
    -- requirement 8's "unique within one language across the
    -- complete production concept catalogue" is only meaningful
    -- when tested against the real thing, not a hand-picked
    -- fixture list — the catalogue is read at test time, so this
    -- keeps covering it as #713 grows it further).
    -- The language's COMPLETE morpheme assignment (#1096): free
    -- roots plus whatever bound forms this version's selection
    -- accepted. Empty bound map below generator version 4.
    rootsFor ∷ Profile → LanguageRoots
    rootsFor prof = assignLanguageRoots prof prodOrds (conceptIds prodCat)

    renderingsFor ∷ Profile → [Either Text Text]
    renderingsFor prof =
        let roots = rootsFor prof
        in [ either (Left ∘ nativeRenderErrorText) Right
                    (renderNative prof roots expr)
           | (_, expr) ← canonicalExpressions ]

    nativeRenderings ∷ Word64 → [Either Text Text]
    nativeRenderings = renderingsFor ∘ buildProfileV1 ∘ LangSeed

    nativeRenderingsV2 ∷ Word64 → [Either Text Text]
    nativeRenderingsV2 = renderingsFor ∘ buildProfileV2 ∘ LangSeed

    nativeRenderingsV3 ∷ Word64 → [Either Text Text]
    nativeRenderingsV3 = renderingsFor ∘ buildProfileV3 ∘ LangSeed

    nativeRenderingsV4 ∷ Word64 → [Either Text Text]
    nativeRenderingsV4 = renderingsFor ∘ buildProfileV4 ∘ LangSeed

    nativeRenderingsV5 ∷ Word64 → [Either Text Text]
    nativeRenderingsV5 = renderingsFor ∘ buildProfileV5 ∘ LangSeed

    -- #1100's canonical sample: the same 256 seeds
    -- tools/language_report.py gates, at the current version.
    v5Profiles ∷ [Profile]
    v5Profiles = [ buildProfileV5 (LangSeed s) | s ← [0 .. 255] ]

    v5Marked, v5Plain ∷ [Profile]
    v5Marked = [ p | p ← v5Profiles, not (null (profileExtendedChars p)) ]
    v5Plain  = [ p | p ← v5Profiles, null (profileExtendedChars p) ]

    -- Every version-5 language of that sample with its full
    -- assignment. Assigning roots over the complete 150-concept
    -- catalogue is the expensive part, so the #1100 sweeps share
    -- ONE list rather than rebuilding it per assertion — the same
    -- arrangement 'v4Assignments' uses.
    v5Assignments ∷ [(Profile, LanguageRoots)]
    v5Assignments = [ (p, rootsFor p) | p ← v5Profiles ]

    v3Profiles ∷ [Profile]
    v3Profiles = [ buildProfileV3 (LangSeed s) | s ← [0 .. 255] ]

    v4Profiles ∷ [Profile]
    v4Profiles = [ buildProfileV4 (LangSeed s) | s ← [0 .. 255] ]

    -- Every version-4 language of the canonical sample together with
    -- its full assignment. Assigning roots over the complete
    -- 150-concept catalogue is the expensive part, so version 4's
    -- sweep shares ONE list rather than rebuilding per assertion.
    v4Assignments ∷ [(Profile, LanguageRoots)]
    v4Assignments = [ (p, rootsFor p) | p ← v4Profiles ]

    -- Every (language, selected concept, its free root, its bound
    -- form) in that sample: the population #1096's acceptance calls
    -- "every stored bound candidate", as opposed to the handful the
    -- five canonical expressions happen to touch.
    v4BoundForms ∷ [(Profile, ConceptId, Text, Text)]
    v4BoundForms =
        [ (p, c, M.findWithDefault "" c (lrFree lr), b)
        | (p, lr) ← v4Assignments, (c, b) ← M.toList (lrBound lr) ]

    -- A fixed head to render each selected concept's dependent slots
    -- against: the catalogue's first id that is not the concept
    -- itself, the same rule Language.Generated.Report uses.
    headAgainst ∷ ConceptId → ConceptId
    headAgainst c = case filter (≢ c) (conceptIds prodCat) of
        (h : _) → h
        []      → c

    -- Every version-2 profile in the report tool's canonical
    -- sample. Profile construction assigns no roots, so building
    -- all 256 is cheap enough to assert relation properties over
    -- the same population tools/language_report.py gates.
    v2Profiles ∷ [Profile]
    v2Profiles = [ buildProfileV2 (LangSeed s) | s ← [0 .. 255] ]

    v2ProfilesWithRole ∷ YRole → [Profile]
    v2ProfilesWithRole r =
        [ p | p ← v2Profiles, profileYRole p ≡ Just r ]
