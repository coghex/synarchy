-- | Generated-language rendering (#710): deterministic profile
--   generation, concept-root assignment/collision resolution, and
--   native-name rendering over #709's semantic proper names. Mirrors
--   'Test.Headless.Language.Semantic''s shape — the production concept
--   catalogue read straight from @data/language/concepts.yaml@, pinned
--   golden outputs, and no engine/Lua/random state anywhere.
module Test.Headless.Language.Generated (spec) where

import UPrelude
import Test.Hspec
import Data.Char (toLower, toUpper)
import Data.List (nub, sort)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Engine.Asset.Types (GlyphInfo(..))
import Engine.Core.Log
    (LogConfig(..), LoggerState, defaultLogConfig, initLogger)
import Engine.Graphics.Font.Data (FontAtlas(..))
import Engine.Graphics.Font.Fallback (isMissingGlyph)
import Engine.Graphics.Font.Repertoire
    (generatedNameFonts, repertoireForFont)
import Engine.Graphics.Font.SDF (generateSDFFontAtlas, sdfAtlasErrorMessage)
import Engine.Graphics.Font.Util (calculateTextWidthScaled)
import Language.Semantic.Types
import Language.Semantic.Catalogue
import Language.Generated.Types
import Language.Generated.Orthography
import Language.Generated.Hash (boundSeed, conceptSeed)
import Language.Generated.Onset
import Language.Generated.Boundary
import Language.Generated.Bound
import Language.Generated.Profile
import Language.Generated.Root
import Language.Generated.Render
import Language.Generated.Signature
import Language.Generated.Report (canonicalExpressions, countDuplicateRoots,
                                   boundSlotExpressions)

cid ∷ Text → ConceptId
cid = ConceptId

-- | The one syllable shape with an in-syllable two-consonant onset —
--   the shape #1094 constrains.
ccvShape ∷ SyllableShape
ccvShape = SyllableShape [ConsonantSlot, ConsonantSlot, VowelSlot]

-- | Force every syllable of a profile through the @CCV@ path, so a
--   generated root exercises the REAL 'renderShape' onset selection on
--   every syllable rather than only where the profile's own shape draw
--   happened to land on @CCV@. Nothing else about the profile changes,
--   so the onsets produced must satisfy that same profile's exported
--   relation.
forceCCV ∷ Profile → Profile
forceCCV p = p { profSyllableShapes = [ccvShape] }

-- | The onset of each 3-character syllable in a root rendered by a
--   'forceCCV' profile.
syllableOnsets ∷ Text → [(Char, Char)]
syllableOnsets = go ∘ T.unpack
  where
    go (a : b : _ : rest) = (a, b) : go rest
    go _                  = []

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

-- | A fixture whose @y@ is BOTH a consonant and a vowel and whose first
--   linking consonant is that same @y@ — the one shape in which the
--   primary linker would itself repeat the segment it is separating, so
--   the distinct alternative has to take over.
dualRoleFixture ∷ Profile
dualRoleFixture = (boundaryFixture BoundaryEpenthetic)
    { profConsonants = "byk"
    , profVowels     = "ay"
    , profOnset      = emptyOnsetRelation
    , profBoundary   = BoundaryMediated BoundaryRepair
        { brRule       = BoundaryEpenthetic
        , brEpenthetic = 'a'
        , brLinker     = 'y'
        , brLinkerAlt  = 'k'
        }
    }

-- | A version-3 profile whose ONSET relation is empty, so @CCV@ falls
--   back to #1094's historical independent-draw path and a syllable can
--   really come out as @bba@. Beside @CVC@ — which ends in a consonant —
--   that puts a genuine @b|bb@ junction within reach, so ordinary
--   multi-syllable root construction really can produce a triple: the
--   case #1095's reviewed spec requires covering beyond the four
--   morpheme sites. Only two consonants, so identical draws are common.
tripleProneRoot ∷ BoundaryPolicy → Profile
tripleProneRoot policy = (boundaryFixture BoundaryEpenthetic)
    { profConsonants     = "bk"
    , profVowels         = "ao"
    , profSyllableShapes =
        [ SyllableShape [ConsonantSlot, VowelSlot, ConsonantSlot]
        , SyllableShape [ConsonantSlot, ConsonantSlot, VowelSlot] ]
    , profMinSyllables   = 3
    , profMaxSyllables   = 3
    , profOnset          = emptyOnsetRelation
    , profBoundary       = policy
    }

-- | A version-3 profile every one of whose raw roots is exactly two
--   characters, so EVERY root goes through 'ensureMinLength''s top-up —
--   the fourth of #1095's named boundaries. Both a vowel-final @VC@ and
--   a consonant-initial @CV@ syllable are reachable, so the top-up
--   really can present the existing text with an inadmissible consonant
--   cluster rather than only with a potential triple.
topUpFixture ∷ BoundaryPolicy → Profile
topUpFixture policy = (boundaryFixture BoundaryEpenthetic)
    { profSyllableShapes = [ SyllableShape [VowelSlot, ConsonantSlot]
                           , SyllableShape [ConsonantSlot, VowelSlot] ]
    , profMinSyllables   = 1
    , profMaxSyllables   = 1
    , profBoundary       = policy
    }

-- | Whether the two glyphs at a root's top-up junction (the raw root is
--   two characters, so index 1 meets index 2) form a two-consonant
--   cluster this profile's own relation rejects.
illegalTopUpCluster ∷ Profile → Text → Bool
illegalTopUpCluster prof r = case T.unpack r of
    (_ : a : b : _) →
        a ≢ b
        ∧ consonantCapable prof a ∧ consonantCapable prof b
        ∧ not (vowelCapable prof a) ∧ not (vowelCapable prof b)
        ∧ not (admissibleOnset prof a b)
    _ → False

-- | A hand-built version-4 language for #1096's slot-matrix unit tests,
--   in the same spirit as 'boundaryFixture': every rendered answer below
--   is READ off this fixture rather than inferred from whatever a seed
--   happened to draw.
--
--   The affixes are chosen so that neither mark triggers a boundary
--   repair against the bound form used here — the plural @h@ follows
--   @kab@'s @b@ across an ADMISSIBLE @bh@ onset, and the possessive
--   @'s@ carries its own separator — so each expected string shows the
--   bound form and its mark plainly instead of a repair segment.
--   'boundFixtureRepair' below covers the repaired case.
boundFixture ∷ CompoundOrder → GenitiveOrder → Profile
boundFixture compound genitive = (boundaryFixture BoundaryEpenthetic)
    { profVersion       = GeneratorVersion 4
    , profCompoundOrder = compound
    , profPossessive    = PossessiveMarking genitive "'s"
    , profPlural        = PluralMarking "h"
    }

-- | A version-4 fixture whose @y@ is BOTH a consonant and a vowel, with
--   a relation that admits @by@ and not @ky@ — so the two adjacencies
--   #1096 requirement 4's "consonant-capable" scoping brings into range
--   have different answers, and a test can tell the wider scoping from
--   #1095's consonant-only one rather than only observing that both
--   pass.
dualRoleBoundFixture ∷ Profile
dualRoleBoundFixture = (boundFixture ModifierFirst OwnerFirst)
    { profConsonants = "bhky"
    , profVowels     = "aey"
    , profOnset      = OnsetRelation (S.fromList [('b', 'h'), ('b', 'y')])
    }

-- | The fixture's assignment: one concept with a bound form, one
--   without. @HEAD@ has none, so it doubles as the "a concept with no
--   bound form uses its free form in every slot" case.
boundFixtureRoots ∷ LanguageRoots
boundFixtureRoots = LanguageRoots
    { lrFree  = M.fromList [(cid "DEP", "kaba"), (cid "HEAD", "ota")]
    , lrBound = M.fromList [(cid "DEP", "kab")]
    }

-- | The same assignment with bound morphology switched off — what a
--   pre-version-4 language's rendering of the identical expressions
--   looks like.
boundFixtureFree ∷ LanguageRoots
boundFixtureFree = freeRootsOnly (lrFree boundFixtureRoots)

-- | The one assignment whose bound form's own final segment meets its
--   grammatical mark across an INADMISSIBLE cluster, so #1095's repair
--   has to mediate the join the bound form created.
boundFixtureRepair ∷ LanguageRoots
boundFixtureRepair = LanguageRoots
    { lrFree  = M.fromList [(cid "DEP", "kasa"), (cid "HEAD", "ota")]
    , lrBound = M.fromList [(cid "DEP", "kas")]
    }

-- | The same assignment with one concept's bound form promoted to be
--   its FREE root and bound morphology switched off.
--
--   The reference every slot-matrix property below is stated against:
--   if a dependent slot really selected @c@'s bound form and a head slot
--   really did not, then rendering under this substitution — where that
--   same string is simply the concept's only form — must produce
--   BYTE-IDENTICAL output. It pins the selection without reimplementing
--   ordering, marking, or boundary repair, all of which stay in the
--   production path on both sides of the comparison.
asFreeRoot ∷ ConceptId → LanguageRoots → LanguageRoots
asFreeRoot c lr = freeRootsOnly $ case M.lookup c (lrBound lr) of
    Nothing → lrFree lr
    Just b  → M.insert c b (lrFree lr)

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
--   contract, and the separate per-language check below is what proves
--   an emitted mark also belonged to the language that emitted it.
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

-- | The title font. Named here rather than reached for through
--   'generatedNameFonts' because the point of the font-coverage group
--   below is that it is NOT in that list.
gothicFontPath ∷ FilePath
gothicFontPath = "assets/fonts/gothic.ttf"

-- | A real SDF atlas for one font at its shipped repertoire, generated
--   from the checked-in @.ttf@ through the production path.
--
--   Everything involved is CPU-side (stb rasterization plus a pure
--   packing planner), so no GPU is required; the device limit the
--   planner is bounded by is a parameter for exactly that reason, and
--   16384 is above anything the shipped fonts need.
nameFontAtlas ∷ FilePath → IO FontAtlas
nameFontAtlas path = do
    logger ← quietLogger
    result ← generateSDFFontAtlas logger path (repertoireForFont path) 16384
    case result of
        Left err → fail $ "atlas generation failed for " ⧺ path ⧺ ": "
                        ⧺ T.unpack (sdfAtlasErrorMessage err)
        Right a  → pure a
  where
    quietLogger ∷ IO LoggerState
    quietLogger = initLogger defaultLogConfig { lcEnableByDefault = False }

-- | Whether an atlas can actually DRAW a character.
--
--   Three conditions, and the third is the one that matters: #1098's
--   atlas publishes a mapped-but-outline-less character (U+00A0) with a
--   real advance and a zero-sized glyph, so glyph-map membership alone
--   would call an invisible cell "covered". A letter must have real
--   extent.
drawsGlyph ∷ FontAtlas → Char → Bool
drawsGlyph atlas c = case M.lookup c (faGlyphData atlas) of
    Nothing → False
    Just gi → not (isMissingGlyph atlas c)
              ∧ fst (giSize gi) > 0 ∧ snd (giSize gi) > 0

-- | Every extended character in @w@ that the profile which rendered it
--   does not hold — #1100 requirement 1's "an accent is part of the
--   inventory, not applied to output". Compared case-folded, because
--   rendering capitalizes the initial and inventories are lowercase.
foreignExtended ∷ Profile → Text → [Char]
foreignExtended prof w =
    [ c | c ← T.unpack w, isExtendedLetter c, lowerOf c `notElem` own ]
  where
    own = profVowels prof <> profConsonants prof
    lowerOf c = case [ lo | (_, _, _, lo, up) ← extendedLetterTable, up ≡ c ] of
        (lo : _) → lo
        []       → c

spec ∷ Spec
spec = describe "Generated language names" $ do
    prodCatE ← runIO $ loadCatalogue conceptCataloguePath conceptOrdinalPath
    let prodCat = either (error ∘ T.unpack ∘ catalogueErrorText) id prodCatE
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
        -- arrangement 'v4Assignments' uses above.
        v5Assignments ∷ [(Profile, LanguageRoots)]
        v5Assignments = [ (p, rootsFor p) | p ← v5Profiles ]

        v3Profiles ∷ [Profile]
        v3Profiles = [ buildProfileV3 (LangSeed s) | s ← [0 .. 255] ]

        v4Profiles ∷ [Profile]
        v4Profiles = [ buildProfileV4 (LangSeed s) | s ← [0 .. 255] ]

        -- Every version-4 language of the canonical sample together with
        -- its full assignment. Assigning roots over the complete
        -- 150-concept catalogue is the expensive part, so the sweep
        -- below shares ONE list rather than rebuilding per assertion.
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

    describe "profile generation (requirements 1, 2, 12)" $ do
        it "builds a PINNED version-1 profile for seed 7" $ do
            -- Determinism is only observable against a value fixed at
            -- authoring time. Calling the builder twice and comparing
            -- the results cannot fail in a pure language: a generator
            -- that consistently drew a different inventory would pass
            -- it unchanged, and fails these lines instead.
            let p = buildProfileV1 (LangSeed 7)
            (profVowels p, profConsonants p) `shouldBe` ("aeo", "thknbz")
            (profMinSyllables p, profMaxSyllables p) `shouldBe` (2, 3)
            profCompoundOrder p `shouldBe` HeadFirst
            profileSignature p `shouldBe` "4378792190029212613"

        it "accepts version 1" $
            case generateProfile (GeneratorVersion 1) (LangSeed 7) of
                Right _ → pure ()
                Left e  → expectationFailure (T.unpack (generatorErrorText e))

        it "accepts version 2" $
            case generateProfile (GeneratorVersion 2) (LangSeed 7) of
                Right _ → pure ()
                Left e  → expectationFailure (T.unpack (generatorErrorText e))

        it "accepts version 3" $
            case generateProfile (GeneratorVersion 3) (LangSeed 7) of
                Right _ → pure ()
                Left e  → expectationFailure (T.unpack (generatorErrorText e))

        it "accepts version 4" $
            case generateProfile (GeneratorVersion 4) (LangSeed 7) of
                Right _ → pure ()
                Left e  → expectationFailure (T.unpack (generatorErrorText e))

        it "accepts version 5" $
            case generateProfile (GeneratorVersion 5) (LangSeed 7) of
                Right _ → pure ()
                Left e  → expectationFailure (T.unpack (generatorErrorText e))

        -- Version 6 stands in for "never existed". This test used to
        -- name versions 2, 3, 4 and then 5, which #1094, #1095, #1096
        -- and #1100 made real — a rejection test must always point at a
        -- version outside the supported set, never at the next one
        -- about to be implemented.
        it "rejects an unsupported version descriptively rather than falling back to version 1" $ do
            let r = generateProfile (GeneratorVersion 6) (LangSeed 7)
            r `shouldBe` Left (UnsupportedGeneratorVersion 6)
            case r of
                Left e  → generatorErrorText e `shouldSatisfy` T.isInfixOf "5"
                Right _ → expectationFailure "expected UnsupportedGeneratorVersion"

        -- #1092 requirement 4: a save records the version that named its
        -- world, so every DEFINED version must stay constructible after
        -- the current one advances. These pin the dispatcher to explicit
        -- versions rather than to whatever 'currentGeneratorVersion'
        -- happens to be — the defect that made a version bump silently
        -- orphan every existing world.
        it "builds a profile for every supported version, independently \
           \of which one is current" $
            forM_ supportedGeneratorVersions $ \ver →
                case generateProfile ver (LangSeed 7) of
                    Left e  → expectationFailure
                        (show ver <> ": " <> T.unpack (generatorErrorText e))
                    Right p → profVersion p `shouldBe` ver

        it "stamps a v1 profile with the LITERAL version 1, not the \
           \current-version constant" $ do
            -- Language.Generated.Root feeds profVersion into its
            -- per-concept seed, so stamping the mutable constant would
            -- re-render every reconstructed v1 root the moment the
            -- current version advanced.
            profVersion (buildProfileV1 (LangSeed 7))
                `shouldBe` GeneratorVersion 1
            case generateProfile (GeneratorVersion 1) (LangSeed 7) of
                Left e  → expectationFailure (T.unpack (generatorErrorText e))
                Right p → profVersion p `shouldBe` GeneratorVersion 1

        it "stamps a v2 profile with the LITERAL version 2, for the same \
           \reason" $ do
            profVersion (buildProfileV2 (LangSeed 7))
                `shouldBe` GeneratorVersion 2
            case generateProfile (GeneratorVersion 2) (LangSeed 7) of
                Left e  → expectationFailure (T.unpack (generatorErrorText e))
                Right p → profVersion p `shouldBe` GeneratorVersion 2

        it "stamps a v3 profile with the LITERAL version 3, for the same \
           \reason" $ do
            profVersion (buildProfileV3 (LangSeed 7))
                `shouldBe` GeneratorVersion 3
            case generateProfile (GeneratorVersion 3) (LangSeed 7) of
                Left e  → expectationFailure (T.unpack (generatorErrorText e))
                Right p → profVersion p `shouldBe` GeneratorVersion 3

        it "stamps a v4 profile with the LITERAL version 4, for the same \
           \reason" $ do
            profVersion (buildProfileV4 (LangSeed 7))
                `shouldBe` GeneratorVersion 4
            case generateProfile (GeneratorVersion 4) (LangSeed 7) of
                Left e  → expectationFailure (T.unpack (generatorErrorText e))
                Right p → profVersion p `shouldBe` GeneratorVersion 4

        it "stamps a v5 profile with the LITERAL version 5, for the same \
           \reason" $ do
            profVersion (buildProfileV5 (LangSeed 7))
                `shouldBe` GeneratorVersion 5
            case generateProfile (GeneratorVersion 5) (LangSeed 7) of
                Left e  → expectationFailure (T.unpack (generatorErrorText e))
                Right p → profVersion p `shouldBe` GeneratorVersion 5

        it "version 5 is the current generator, with versions 1 to 4 \
           \still constructible beside it" $ do
            currentGeneratorVersion `shouldBe` GeneratorVersion 5
            sort (map generatorVersionInt supportedGeneratorVersions)
                `shouldBe` [1, 2, 3, 4, 5]

        it "pins what one seed builds and renders, for every supported \
           \version" $ do
            -- The version-general half of "byte-identical across
            -- constructions", stated so it can fail: one seed through
            -- the dispatcher, per version, against values fixed at
            -- authoring time. Signature and first canonical name are
            -- deliberately both — versions 3 and 4 hash to the same
            -- style signature (bound morphemes live in the root
            -- assignment, not in the style fields), and it is the
            -- rendered name that separates them.
            let expected =
                    [ (GeneratorVersion 1, ("9306776994284989454", "Tazo"))
                    , (GeneratorVersion 2, ("6603857679469430450", "Mudu"))
                    , (GeneratorVersion 3, ("2340532553696747361", "Ygte"))
                    , (GeneratorVersion 4, ("2340532553696747361", "Ygme"))
                    , (GeneratorVersion 5, ("17173116883382338059", "Pead")) ]
            -- Version-GENERAL by construction: a sixth supported
            -- version with no pinned row fails here rather than
            -- quietly going uncovered.
            sort (map (generatorVersionInt ∘ fst) expected)
                `shouldBe` sort (map generatorVersionInt supportedGeneratorVersions)
            forM_ expected $ \(ver, (sig, firstName)) →
                case generateProfile ver (LangSeed 4242) of
                    Left e  → expectationFailure (T.unpack (generatorErrorText e))
                    Right p → do
                        (ver, profileSignature p) `shouldBe` (ver, sig)
                        (ver, take 1 (renderingsFor p))
                            `shouldBe` (ver, [Right firstName])

        it "the supported-version list matches what the dispatcher \
           \actually builds, in both directions" $ do
            -- Neither list can drift silently: every declared version
            -- builds (above), and no version outside the declared set
            -- does. The scan covers the plausible neighbourhood,
            -- including 0 and a negative.
            currentGeneratorVersion `shouldSatisfy`
                (`elem` supportedGeneratorVersions)
            forM_ [(-1) .. 10] $ \v → do
                let ver = GeneratorVersion v
                    built = generateProfile ver (LangSeed 7)
                if ver `elem` supportedGeneratorVersions
                    then built `shouldSatisfy` isRight'
                    else built `shouldBe` Left (UnsupportedGeneratorVersion v)

        it "the unsupported-version message enumerates every supported \
           \version, not just the current one" $ do
            let msg = generatorErrorText (UnsupportedGeneratorVersion 99)
            forM_ supportedGeneratorVersions $ \ver →
                msg `shouldSatisfy`
                    T.isInfixOf (T.pack (show (generatorVersionInt ver)))
            msg `shouldNotSatisfy` T.isInfixOf "only version"

        it "different seeds usually produce different profile signatures (requirement 14)" $ do
            let sigs = map (profileSignature ∘ buildProfileV1 ∘ LangSeed) [0 .. 49]
            length (nub sigs) `shouldSatisfy` (≥ 48)

    describe "concept roots (requirements 7, 8, 16; #1868 placement order)" $ do
        it "assigns the same roots regardless of the input list's order" $ do
            let prof = buildProfileV1 (LangSeed 99)
                ids  = conceptIds prodCat
            assignRoots prof prodOrds ids
                `shouldBe` assignRoots prof prodOrds (reverse ids)

        it "has zero root collisions over the complete production catalogue, for several seeds" $ do
            let seeds = [0, 1, 7, 42, 99, 12345, 999999]
                collisionsFor sd =
                    countDuplicateRoots (assignRoots (buildProfileV1 (LangSeed sd))
                                                      prodOrds
                                                      (conceptIds prodCat))
            map collisionsFor seeds `shouldBe` map (const 0) seeds

        -- #1868. Placement moved from ascending id order to the
        -- catalogue's recorded append-only ordinal. The next three
        -- cases are what make that safe: the assignment is UNCHANGED
        -- for today's catalogue, an ADDITION no longer moves anything,
        -- and one seed's whole map is pinned so a future reordering
        -- fails loudly rather than quietly re-rooting every language.
        --
        -- Every case runs the whole PANEL — each supported generator
        -- version crossed with a seed spread — not just the current
        -- version, because Language.Etymology rebuilds the profile at
        -- the SOURCE's recorded version before assigning roots from the
        -- current catalogue, so a historical version's assignment is
        -- exactly as load-bearing as version 5's.
        let panelSeeds = [0, 1, 7, 42, 99, 1337, 2718, 12345, 999999, 31337]
            panel = [ (ver, sd, prof)
                    | ver ← supportedGeneratorVersions
                    , sd  ← panelSeeds
                    , Right prof ← [generateProfile ver (LangSeed sd)] ]
            prodIds = conceptIds prodCat
            -- The placement this issue REPLACED, written out rather
            -- than referenced: it is the reference the identity case
            -- below compares against, and the adversarial twin that
            -- proves the addition case is not vacuous.
            -- Takes (and ignores) the ordinals so it shares
            -- 'movedUnder' with the real assignment below: the two are
            -- driven through one comparison, not two similar ones.
            ascendingIdPlacement ∷ Profile → ConceptOrdinals → [ConceptId]
                                 → M.Map ConceptId Text
            ascendingIdPlacement prof _ords ids = foldl' step M.empty (sort ids)
              where
                step acc cid =
                    let used = S.fromList (map T.toLower (M.elems acc))
                        pick attempt =
                            let cand = generateRoot prof cid attempt
                            in if S.member (T.toLower cand) used
                               then pick (attempt + 1)
                               else cand
                    in M.insert cid (pick (0 ∷ Int)) acc
            -- One added id, ratcheted the way --update-baseline would:
            -- appended after every recorded ordinal.
            withProbe pid =
                either (error ∘ T.unpack ∘ catalogueErrorText) id $
                    mkConceptOrdinals (zip prodIds [0 ..]
                                       ⧺ [(pid, length prodIds)])
            probeIds = [ cid (T.pack ("APROBE" ⧺ show n))
                       | n ← [1 .. 30 ∷ Int] ]
            movedUnder place ords pid (_, _, prof) =
                let before = place prof ords prodIds
                    after  = place prof (withProbe pid) (pid : prodIds)
                in [ c | c ← prodIds, M.lookup c before ≢ M.lookup c after ]

        it "assigns exactly what ascending-id placement did, for every \
           \supported version and seed" $ do
            -- Requirement 2. The seeded ordinals ARE ascending-id rank,
            -- so introducing them changed no existing world's roots and
            -- needed no currentGeneratorVersion bump. That identity is
            -- also what makes blessing the golden below safe.
            panel `shouldSatisfy` ((≡ 50) ∘ length)
            [ (generatorVersionInt ver, langSeedWord (profSeed prof))
              | (ver, _, prof) ← panel
              , assignRoots prof prodOrds prodIds
                  ≢ ascendingIdPlacement prof prodOrds prodIds ] `shouldBe` []

        it "leaves every existing concept's root untouched when an id is \
           \added, for every supported version and seed" $ do
            -- Requirement 3, the defect itself. Scope: this is the FREE
            -- root. From version 4 on, bound-form selection ranks the
            -- complete current concept set, so an addition can still
            -- move a bound form (#1096) — deliberately out of scope.
            [ (generatorVersionInt ver, langSeedWord (profSeed prof), pid, moved)
              | entry@(ver, _, prof) ← panel
              , pid ← probeIds
              , let moved = movedUnder assignRoots prodOrds pid entry
              , not (null moved) ] `shouldBe` []

        it "would have caught the defect: ascending-id placement DOES \
           \move incumbents over the same panel" $ do
            -- Without this the case above could pass because the panel
            -- is too small to contain a collision, rather than because
            -- ordinal placement fixed anything.
            let movers = [ (generatorVersionInt ver, pid, moved)
                         | entry@(ver, _, _) ← panel
                         , pid ← probeIds
                         , let moved = movedUnder ascendingIdPlacement
                                                  prodOrds pid entry
                         , not (null moved) ]
            length movers `shouldSatisfy` (≥ 20)

        it "pins one seed's complete root map (generator version 5, \
           \seed 1337)" $ do
            -- Blessed from current output, which requirement 2's
            -- identity above makes safe: this map is byte-identical to
            -- what ascending-id placement produced before #1868. Any
            -- later change to placement order, the ordinal artifact, or
            -- collision resolution fails here naming the concepts that
            -- moved.
            let prof = case generateProfile (GeneratorVersion 5)
                                            (LangSeed 1337) of
                    Right p → p
                    Left e  → error ("test setup: " <> show e)
                golden ∷ [(Text, Text)]
                golden =
                  [ ("AMBER", "jikry")
                  , ("ANGEL", "lav")
                  , ("ASH", "ijhi")
                  , ("AUTUMN", "lhyry")
                  , ("BAY", "lip")
                  , ("BEAR", "vpy")
                  , ("BLADE", "vtivpi")
                  , ("BLESSING", "wakwi")
                  , ("BOAR", "yhiphy")
                  , ("BONE", "jyhra")
                  , ("BRIDGE", "vtyjpa")
                  , ("BRONZE", "jwyła")
                  , ("CAVERN", "łijti")
                  , ("CLAW", "walti")
                  , ("CLAY", "yhitap")
                  , ("CLIFF", "vpiir")
                  , ("CLOUD", "rilky")
                  , ("COAL", "jalti")
                  , ("COMET", "wiav")
                  , ("COPPER", "łwi")
                  , ("COURAGE", "łwijij")
                  , ("CROSSING", "vłi")
                  , ("CROWN", "yjril")
                  , ("CURSE", "hita")
                  , ("DAWN", "hiłwa")
                  , ("DEMON", "lpajta")
                  , ("DESPAIR", "phiła")
                  , ("DESTINY", "jyvwy")
                  , ("DOUBT", "vłita")
                  , ("DREAM", "ryti")
                  , ("DUSK", "atipał")
                  , ("EARTH", "ławkiw")
                  , ("ECLIPSE", "lihal")
                  , ("EMBER", "ywipy")
                  , ("ENVY", "vra")
                  , ("EXILE", "jahy")
                  , ("EYE", "awilty")
                  , ("FAITH", "wili")
                  , ("FAMINE", "ikłak")
                  , ("FANG", "lyh")
                  , ("FATE", "tiyt")
                  , ("FEAR", "wyljri")
                  , ("FEATHER", "hałi")
                  , ("FIRE", "vił")
                  , ("FORD", "waik")
                  , ("FROST", "pyvyw")
                  , ("GATE", "jyk")
                  , ("GHOST", "tyłyt")
                  , ("GLASS", "wiiw")
                  , ("GLORY", "łak")
                  , ("GOD", "łhiiv")
                  , ("GOLD", "kałar")
                  , ("GREED", "łarvhi")
                  , ("GRIEF", "vryji")
                  , ("HARBOR", "ilijpa")
                  , ("HAVEN", "kipyp")
                  , ("HAWK", "avivy")
                  , ("HEART", "jywit")
                  , ("HEARTH", "talav")
                  , ("HOLLOW", "lłi")
                  , ("HONOR", "yłijwa")
                  , ("HOPE", "lwi")
                  , ("HORIZON", "kayp")
                  , ("HORN", "pyha")
                  , ("HOUND", "łiri")
                  , ("ICE", "ivly")
                  , ("IRON", "lra")
                  , ("ISLE", "łyv")
                  , ("JOY", "łiap")
                  , ("KEEP", "ilwy")
                  , ("KING", "ritta")
                  , ("LAND", "tip")
                  , ("LEAD", "irivłi")
                  , ("LEGEND", "jpa")
                  , ("LIGHTNING", "riak")
                  , ("LION", "jhalpa")
                  , ("MANE", "piji")
                  , ("MARSH", "hijir")
                  , ("MEMORY", "kaji")
                  , ("MERCY", "kyviw")
                  , ("MIDNIGHT", "jil")
                  , ("MIST", "vhyij")
                  , ("MOON", "jwyky")
                  , ("MYTH", "phy")
                  , ("NIGHT", "tyvpi")
                  , ("NOON", "awihyl")
                  , ("OATH", "wapky")
                  , ("OBSIDIAN", "arjra")
                  , ("OMEN", "tyw")
                  , ("ORACLE", "hijta")
                  , ("OWL", "pyly")
                  , ("PEAK", "pyj")
                  , ("PLAGUE", "vikaj")
                  , ("PRIDE", "pył")
                  , ("PROPHET", "viyp")
                  , ("QUARTZ", "ałłi")
                  , ("QUEEN", "vtiwyj")
                  , ("RAGE", "jwakha")
                  , ("RAIN", "alkyp")
                  , ("RAINBOW", "yrhal")
                  , ("RAVEN", "wiwah")
                  , ("REALM", "wakyr")
                  , ("RELIC", "jawkry")
                  , ("RIDGE", "ikikar")
                  , ("RIVER", "łaj")
                  , ("RUIN", "yvaw")
                  , ("SALT", "ral")
                  , ("SANCTUARY", "illła")
                  , ("SAND", "kra")
                  , ("SERPENT", "hah")
                  , ("SHADOW", "yhita")
                  , ("SHAME", "kyaw")
                  , ("SILENCE", "katvy")
                  , ("SILVER", "payh")
                  , ("SKULL", "tiij")
                  , ("SKY", "aryj")
                  , ("SLOTH", "kylki")
                  , ("SMOKE", "aklyl")
                  , ("SORROW", "jwiyv")
                  , ("SOUL", "ralta")
                  , ("SPIDER", "hypkyl")
                  , ("SPIRE", "ltypi")
                  , ("SPIRIT", "alwal")
                  , ("SPRING", "lhi")
                  , ("STAG", "khiat")
                  , ("STAR", "kylvri")
                  , ("STEEL", "yjikyw")
                  , ("STONE", "yhihyl")
                  , ("STORM", "ripki")
                  , ("SUMMER", "jały")
                  , ("SUN", "łałi")
                  , ("TALON", "iłyv")
                  , ("THRESHOLD", "vwi")
                  , ("THRONE", "wyłrył")
                  , ("THUNDER", "ijilły")
                  , ("TIDE", "jiłyj")
                  , ("TIN", "jyh")
                  , ("TITAN", "jwy")
                  , ("TOWER", "hytyv")
                  , ("TRIUMPH", "wyvy")
                  , ("TUSK", "kytyt")
                  , ("VALE", "ikral")
                  , ("VALOR", "jhi")
                  , ("WALL", "tiwaw")
                  , ("WATER", "pav")
                  , ("WIND", "hawi")
                  , ("WINTER", "łakyt")
                  , ("WITCH", "vtykik")
                  , ("WOLF", "lpi")
                  , ("WRAITH", "apipi")
                  , ("WRATH", "iłjah")
                  ]
            M.toList (assignRoots prof prodOrds prodIds)
                `shouldBe` [ (cid c, r) | (c, r) ← golden ]

    describe "every #709 name form renders natively (requirement 10)" $ do
        let prof  = buildProfileV1 (LangSeed 7)
            roots = rootsFor prof
        it "Bare" $ renderNative prof roots (Bare (cid "SILENCE")) `shouldSatisfy` isRight'
        it "Modifier" $ renderNative prof roots (Modifier (cid "ASH") (cid "LAND"))
            `shouldSatisfy` isRight'
        it "Of (plural)" $ renderNative prof roots (Of (cid "EYE") Plural (cid "STORM"))
            `shouldSatisfy` isRight'
        it "Of (singular)" $ renderNative prof roots (Of (cid "GATE") Singular (cid "WINTER"))
            `shouldSatisfy` isRight'
        it "Possessive" $ renderNative prof roots (Possessive (cid "WOLF") (cid "HEART"))
            `shouldSatisfy` isRight'

    describe "rendering failures (no silent fallback)" $
        it "fails descriptively for a concept absent from the roots map" $ do
            let prof  = buildProfileV1 (LangSeed 7)
                roots = rootsFor prof
                r = renderNative prof roots (Bare (cid "NOT_A_REAL_CONCEPT"))
            r `shouldBe` Left (NativeUnknownConcept (cid "NOT_A_REAL_CONCEPT"))
            case r of
                Left err → nativeRenderErrorText err
                    `shouldSatisfy` T.isInfixOf "NOT_A_REAL_CONCEPT"
                Right w → expectationFailure $ "unexpected render: " ⧺ T.unpack w

    describe "grammatical marking retains the stem (requirement 9)" $ do
        let prof = buildProfileV1 (LangSeed 55)
            root = "karad"
        it "plural marking keeps the bare root as a prefix" $
            applyPluralMark prof root `shouldSatisfy` T.isPrefixOf root
        it "possessive marking keeps the bare root as a prefix" $
            applyPossessiveMark prof root `shouldSatisfy` T.isPrefixOf root

    describe "different seeds produce different compound orderings (requirement 11)" $ do
        it "seed 0's profile orders compounds head-first" $
            profCompoundOrder (buildProfileV1 (LangSeed 0)) `shouldBe` HeadFirst
        it "seed 42's profile orders compounds modifier-first" $
            profCompoundOrder (buildProfileV1 (LangSeed 42)) `shouldBe` ModifierFirst

    describe "output contract (requirement 6)" $
        it "every canonical native name across many seeds satisfies the ASCII/length/capitalization/punctuation contract" $ do
            let allRenderings = concatMap nativeRenderings [0 .. 40]
                texts = [ w | Right w ← allRenderings ]
            length texts `shouldBe` length allRenderings
            filter (not ∘ contractOk) texts `shouldBe` []

    describe "determinism (requirement 13)" $ do
        it "renders a PINNED string for one fixed profile and expression" $ do
            let prof  = buildProfileV1 (LangSeed 321)
                roots = rootsFor prof
                expr  = Modifier (cid "ASH") (cid "LAND")
            -- What requirement 13 promises is that THIS profile and
            -- THIS expression keep producing this text across builds,
            -- which only a fixed expected side can hold the generator
            -- to.
            renderNative prof roots expr `shouldBe` Right "Mpuimbzoppa"

        it "pins one profile's signature, and a neighbouring seed's \
           \differs from it" $ do
            profileSignature (buildProfileV1 (LangSeed 321))
                `shouldBe` "10826199575149732283"
            -- The other half of "stable": a signature that ignored the
            -- profile entirely would be perfectly stable too.
            profileSignature (buildProfileV1 (LangSeed 322))
                `shouldBe` "9332460123107599855"
            profileSignature (buildProfileV1 (LangSeed 321))
                `shouldNotBe` profileSignature (buildProfileV1 (LangSeed 322))

    -- #1094: version 2's admissible two-consonant onset relation.
    describe "admissible onsets (#1094 requirements 3, 4, 5, 7)" $ do
        it "is irreflexive, and every stored pair lies inside the \
           \profile's own consonant inventory" $ do
            let offenders =
                    [ (profSeed p, a, b)
                    | p ← v2Profiles
                    , (a, b) ← onsetPairs (profOnset p)
                    , a ≡ b
                      ∨ not (a `elem` profConsonants p)
                      ∨ not (b `elem` profConsonants p) ]
                reflexive =
                    [ (profSeed p, c)
                    | p ← v2Profiles, c ← profConsonants p
                    , admissibleOnset p c c ]
            offenders `shouldBe` []
            reflexive `shouldBe` []

        it "rejects any character outside the profile's consonant \
           \inventory, in either position" $ do
            let probes = ['a' .. 'z'] <> "AQ'-0 "
                offenders =
                    [ (profSeed p, x, c)
                    | p ← take 64 v2Profiles
                    , x ← probes
                    , not (consonantCapable p x)
                    , c ← profConsonants p
                    , admissibleOnset p x c ∨ admissibleOnset p c x ]
            offenders `shouldBe` []

        it "admits between 25% and 45% of each profile's n*(n-1) \
           \ordered pairs, and is never empty" $ do
            let offenders =
                    [ (profSeed p, onsetPairCount (profOnset p), onsetTotalPairs p)
                    | p ← v2Profiles
                    , not (onsetDensityOk p) ∨ onsetPairCount (profOnset p) ≡ 0 ]
            offenders `shouldBe` []
            -- A relation admitting every distinct pair would pass an
            -- irreflexivity test but defeat the whole issue, so pin the
            -- band's arithmetic itself rather than only its outcome.
            onsetDensityBounds 30 `shouldBe` (8, 13)
            onsetDensityBounds 132 `shouldBe` (33, 59)

        it "every profile offering CCV has a pair the renderer can \
           \select" $ do
            let offering = [ p | p ← v2Profiles
                           , ccvShape `elem` profSyllableShapes p ]
            offering `shouldSatisfy` not ∘ null
            filter (null ∘ onsetPairs ∘ profOnset) offering `shouldBe` []

        it "the same visible pair is admissible in some languages and \
           \inadmissible in others (cross-seed diversity)" $ do
            -- A relation keyed only on letters would give every
            -- language identical phonotactics and score zero here.
            let tally = M.fromListWith plus
                    [ ((a, b), (1 ∷ Int, if admissibleOnset p a b then 1 else 0 ∷ Int))
                    | p ← v2Profiles
                    , let inv = sort (nub (profConsonants p))
                    , a ← inv, b ← inv, a ≢ b ]
                plus (s1, k1) (s2, k2) = (s1 + s2, k1 + k2)
                qualifying = [ v | v@(shared, _) ← M.elems tally, shared ≥ 8 ]
                disagreeing = [ () | (shared, adm) ← qualifying
                              , adm > 0, adm < shared ]
            length qualifying `shouldSatisfy` (> 0)
            (2 * length disagreeing) `shouldSatisfy` (≥ length qualifying)

        it "participates in the profile signature" $ do
            let prof = buildProfileV2 (LangSeed 3)
            profileSignature (prof { profOnset = emptyOnsetRelation })
                `shouldNotBe` profileSignature prof

        it "version 1 constrains nothing, and the query is still total \
           \there" $ do
            -- #1092 keeps historical versions constructible, and L1c
            -- consumes this query without knowing which version built
            -- the profile — so a v1 profile must answer, not diverge.
            let offenders =
                    [ (s, a, b)
                    | s ← [0, 1, 42, 12345 ∷ Word64]
                    , let p = buildProfileV1 (LangSeed s)
                    , a ← profConsonants p, b ← profConsonants p
                    , admissibleOnset p a b ]
            offenders `shouldBe` []
            map (onsetPairs ∘ profOnset ∘ buildProfileV1 ∘ LangSeed)
                [0, 1, 42, 12345 ∷ Word64]
                `shouldBe` replicate 4 []

    describe "version-2 CCV rendering selects from the relation (#1094 requirement 5)" $ do
        it "every onset the real CCV rendering path produces is \
           \admissible under the exported relation" $ do
            -- Drives the production renderShape with every syllable
            -- forced through CCV, so this covers the actual selection
            -- code L1c's contract depends on, not a reimplementation.
            let ids = take 40 (conceptIds prodCat)
                rootsUnder s =
                    let forced = forceCCV (buildProfileV2 (LangSeed s))
                    in [ (forced, generateRoot forced c attempt)
                       | c ← ids, attempt ← [0 .. 2 ∷ Int] ]
                everyRoot = concatMap rootsUnder [0 .. 63 ∷ Word64]
                misChunked = [ r | (_, r) ← everyRoot
                             , T.length r `mod` 3 ≢ 0 ]
                offenders = [ (profSeed p, r, a, b)
                            | (p, r) ← everyRoot
                            , (a, b) ← syllableOnsets r
                            , not (admissibleOnset p a b) ]
            everyRoot `shouldSatisfy` not ∘ null
            -- Guards the 3-character chunking the assertion relies on.
            misChunked `shouldBe` []
            offenders `shouldBe` []

        it "makes identical-consonant onsets impossible" $ do
            let ids = take 40 (conceptIds prodCat)
                offenders =
                    [ (s, r, a)
                    | s ← [0 .. 63 ∷ Word64]
                    , let forced = forceCCV (buildProfileV2 (LangSeed s))
                    , c ← ids, attempt ← [0 .. 2 ∷ Int]
                    , let r = generateRoot forced c attempt
                    , (a, b) ← syllableOnsets r
                    , a ≡ b ]
            offenders `shouldBe` []

        it "no canonical version-2 name begins with an inadmissible or \
           \repeated two-consonant onset" $ do
            -- The same word-initial scoping tools/language_report.py
            -- gates, run against real (unforced) profiles.
            let checked =
                    [ (s, w, a, b)
                    | s ← [0 .. 63 ∷ Word64]
                    , let p = buildProfileV2 (LangSeed s)
                    , Right w ← nativeRenderingsV2 s
                    , (a, b) ← wordInitialOnsets p w ]
                offenders =
                    [ (s, w, a, b)
                    | (s, w, a, b) ← checked
                    , let p = buildProfileV2 (LangSeed s)
                    , a ≡ b ∨ not (admissibleOnset p a b) ]
            checked `shouldSatisfy` not ∘ null
            offenders `shouldBe` []

    describe "version-2 'y' roles (#1094 requirements 6, 7)" $ do
        it "assigns every profile exactly one of the three roles — \
           \never 'neither'" $ do
            filter (isNothing ∘ profileYRole) v2Profiles `shouldBe` []
            let mismatched =
                    [ profSeed p
                    | p ← v2Profiles
                    , let inCons = 'y' `elem` profConsonants p
                    , let inVow  = 'y' `elem` profVowels p
                    , case profileYRole p of
                        Just YConsonantOnly → not inCons ∨ inVow
                        Just YVowelOnly     → inCons ∨ not inVow
                        Just YBothRoles     → not (inCons ∧ inVow)
                        Nothing             → True ]
            mismatched `shouldBe` []

        it "seeds 0:255 include a profile in each of the three states" $
            forM_ [YConsonantOnly, YVowelOnly, YBothRoles] $ \r →
                v2ProfilesWithRole r `shouldSatisfy` not ∘ null

        it "surface-glyph capability follows the role" $ do
            forM_ (v2ProfilesWithRole YConsonantOnly) $ \p → do
                consonantCapable p 'y' `shouldBe` True
                vowelCapable p 'y' `shouldBe` False
            forM_ (v2ProfilesWithRole YVowelOnly) $ \p → do
                consonantCapable p 'y' `shouldBe` False
                vowelCapable p 'y' `shouldBe` True
            forM_ (v2ProfilesWithRole YBothRoles) $ \p → do
                consonantCapable p 'y' `shouldBe` True
                vowelCapable p 'y' `shouldBe` True

        it "a vowel-only 'y' is never consonant-capable, so no onset \
           \query involving it can succeed" $ do
            let offenders =
                    [ (profSeed p, c)
                    | p ← v2ProfilesWithRole YVowelOnly
                    , c ← profConsonants p
                    , admissibleOnset p 'y' c ∨ admissibleOnset p c 'y' ]
            offenders `shouldBe` []

        it "a dual-role 'y' still takes the consonant interpretation \
           \beside another consonant-capable glyph" $ do
            -- Requirement 7's surface-glyph semantics: L1c sees flat
            -- text, so a dual-role 'y' must be answerable as a cluster
            -- member. At least one such language really admits it.
            let admittingY =
                    [ profSeed p
                    | p ← v2ProfilesWithRole YBothRoles
                    , (a, b) ← onsetPairs (profOnset p)
                    , a ≡ 'y' ∨ b ≡ 'y' ]
            admittingY `shouldSatisfy` not ∘ null

    -- #1095: version 3's morpheme-boundary phonology.
    describe "boundary phonology (#1095 requirements 1, 2, 4, 5, 7)" $ do
        it "leaves a boundary its own onset relation admits untouched" $ do
            -- The reviewed spec: an admissible boundary MAY remain
            -- unchanged, and only the triple-run invariant can force a
            -- change. 'bh' is admissible in the fixture.
            forM_ [BoundaryEpenthetic, BoundaryHarmonic, BoundarySimplifying] $ \rule →
                joinMorphemes (boundaryFixture rule) "kab" "ha"
                    `shouldBe` "kabha"

        it "breaks an identical segment pair with the language's own \
           \epenthetic vowel" $
            -- The 'Eytoc-hohh' shape: a root ending in 'h' meeting the
            -- 'h' possessive affix. The root stays a prefix and the
            -- one-letter mark survives.
            joinMorphemes (boundaryFixture BoundaryEpenthetic) "hoh" "h"
                `shouldBe` "hohah"

        it "assimilates the inserted vowel to the left morpheme's own \
           \nucleus under the harmonic rule" $ do
            joinMorphemes (boundaryFixture BoundaryHarmonic) "hoh" "h"
                `shouldBe` "hohoh"
            -- Same left shape, different nucleus: the copied vowel
            -- really tracks the left morpheme rather than being fixed.
            joinMorphemes (boundaryFixture BoundaryHarmonic) "heh" "h"
                `shouldBe` "heheh"
            -- And it differs from the fixed-epenthesis language's answer
            -- for the identical input, so the rule is per-language.
            joinMorphemes (boundaryFixture BoundaryEpenthetic) "heh" "h"
                `shouldBe` "hehah"

        it "simplifies an inadmissible cluster by dropping the right \
           \morpheme's initial segment" $
            -- 'bs' is not admissible in the fixture; trimming leaves
            -- 'a', and 'b'/'a' needs no repair.
            joinMorphemes (boundaryFixture BoundarySimplifying) "keb" "sa"
                `shouldBe` "keba"

        it "falls back to epenthesis rather than erasing a one-letter \
           \grammatical mark" $
            -- The simplifying rule cannot delete the whole affix, so the
            -- mark survives and the boundary is broken instead.
            joinMorphemes (boundaryFixture BoundarySimplifying) "hoh" "h"
                `shouldBe` "hohah"

        it "breaks an identical VOWEL pair with a linking consonant, \
           \not another vowel" $
            joinMorphemes (boundaryFixture BoundaryEpenthetic) "ka" "ab"
                `shouldBe` "kakab"

        it "uses the alternative linker when the primary one would \
           \itself repeat the segment it separates" $
            -- A dual-role 'y' takes the vowel interpretation here, and
            -- the primary linker IS 'y' — inserting it would build the
            -- very triple the repair exists to prevent.
            joinMorphemes dualRoleFixture "sy" "yt" `shouldBe` "sykyt"

        it "repairs an inadmissible cluster in every language, by \
           \whichever rule that language chose" $ do
            -- One input, three languages, three distinct answers: the
            -- mediation is per-language rather than universal.
            let joins = [ joinMorphemes (boundaryFixture r) "keb" "sa"
                        | r ← [BoundaryEpenthetic, BoundaryHarmonic, BoundarySimplifying] ]
            joins `shouldBe` ["kebasa", "kebesa", "keba"]
            length (nub joins) `shouldBe` 3

        it "never modifies the left morpheme, so a bare stem stays a \
           \prefix of every repaired join" $ do
            let lefts  = ["hoh", "keb", "ka", "kabb", "kess", "bo"]
                rights = ["h", "s", "ab", "bo", "sa", "ha", "ok"]
                offenders =
                    [ (rule, l, r, joined)
                    | rule ← [BoundaryEpenthetic, BoundaryHarmonic, BoundarySimplifying]
                    , l ← lefts, r ← rights
                    , let joined = joinMorphemes (boundaryFixture rule) l r
                    , not (l `T.isPrefixOf` joined)
                      ∨ T.length joined ≤ T.length l ]
            offenders `shouldBe` []

        it "preserves a doubled letter that lies wholly inside either \
           \morpheme, repaired boundary or not" $ do
            -- An admissible boundary: nothing is touched at all.
            joinMorphemes (boundaryFixture BoundaryEpenthetic) "kobb" "ha"
                `shouldBe` "kobbha"
            -- A REPAIRED boundary: the repair breaks the run the join
            -- would have created without disturbing the 'bb' the left
            -- morpheme already carried.
            joinMorphemes (boundaryFixture BoundaryEpenthetic) "kabb" "bo"
                `shouldBe` "kabbabo"
            forM_ [BoundaryEpenthetic, BoundaryHarmonic, BoundarySimplifying] $ \rule →
                joinMorphemes (boundaryFixture rule) "kobb" "ha"
                    `shouldSatisfy` T.isInfixOf "bb"

        it "mediates a syllable join only where a triple would form" $ do
            let prof = boundaryFixture BoundaryEpenthetic
            -- A plain double across a syllable join is ordinary
            -- orthography and must survive untouched.
            joinSyllables prof "ab" "ba" `shouldBe` "abba"
            -- A THIRD identical segment is, from either side of the
            -- junction.
            joinSyllables prof "abb" "ba" `shouldBe` "abbaba"
            joinSyllables prof "ab" "bba" `shouldBe` "ababba"

        it "recognizes a triple-letter run case-insensitively, with \
           \punctuation interrupting it" $ do
            map hasTripleRun ["aaa", "Aaa", "aAa", "kaaan", "zoccce"]
                `shouldBe` replicate 5 True
            -- A hyphen join's a-a and an apostrophe affix's h'h are not
            -- contiguous letters, and a double is not a triple.
            map hasTripleRun ["a-aa", "aa-a", "h'hh", "abba", "kobbha", ""]
                `shouldBe` replicate 6 False

        it "reports a boundary as needing repair exactly where the \
           \shared admissibility relation says so" $ do
            let prof = boundaryFixture BoundaryEpenthetic
            -- Admissible cluster: no repair. Inadmissible: repair.
            boundaryNeedsRepair prof "kab" "ha" `shouldBe` False
            boundaryNeedsRepair prof "keb" "sa" `shouldBe` True
            boundaryNeedsRepair prof "hoh" "h"  `shouldBe` True
            -- An empty side is not a boundary, and a historical profile
            -- has no boundary phonology to apply.
            boundaryNeedsRepair prof "" "ha" `shouldBe` False
            boundaryNeedsRepair (buildProfileV2 (LangSeed 42)) "hoh" "h"
                `shouldBe` False

    describe "versions 1 and 2 keep joining morphemes raw (#1095)" $
        it "carries no boundary policy, so historical output is \
           \byte-identical" $ do
            let historical = [ buildProfileV1 (LangSeed s) | s ← [0 .. 63] ]
                          <> [ buildProfileV2 (LangSeed s) | s ← [0 .. 63] ]
            filter ((≢ BoundaryUnmediated) ∘ profBoundary) historical
                `shouldBe` []
            filter ((≡ BoundaryUnmediated) ∘ profBoundary) v3Profiles
                `shouldBe` []

    describe "no triple-letter run survives any join (#1095 requirement 3)" $ do
        it "holds for every canonical version-3 name across many seeds, \
           \covering all four join sites" $ do
            let named = [ (s, w) | s ← [0 .. 127 ∷ Word64]
                        , Right w ← nativeRenderingsV3 s ]
                offenders = [ (s, w) | (s, w) ← named, hasTripleRun w ]
            -- The sample is only meaningful if it actually reaches every
            -- site: both compound join styles, an apostrophe-bearing
            -- possessive, and a plain-letter affix all appear.
            named `shouldSatisfy` ((≥ 600) ∘ length)
            named `shouldSatisfy` any (T.isInfixOf "-" ∘ snd)
            named `shouldSatisfy` any (T.isInfixOf "'" ∘ snd)
            named `shouldSatisfy` any (not ∘ T.isInfixOf "-" ∘ snd)
            offenders `shouldBe` []

        it "holds for bare roots built by ordinary syllable \
           \concatenation and min-length top-up" $ do
            let ids = take 60 (conceptIds prodCat)
                rootsOf p = [ generateRoot p c attempt
                            | c ← ids, attempt ← [0 .. 2 ∷ Int] ]
                everyRoot = [ (s, r) | s ← [0 .. 63 ∷ Word64]
                            , r ← rootsOf (buildProfileV3 (LangSeed s)) ]
            everyRoot `shouldSatisfy` not ∘ null
            filter (hasTripleRun ∘ snd) everyRoot `shouldBe` []

        it "holds even for a profile whose syllables really can produce \
           \one — the mediation, not the shape vocabulary, is what \
           \prevents it" $ do
            -- An empty onset relation puts CCV back on #1094's
            -- independent-draw path, so a 'bba' syllable beside a 'b'
            -- coda is reachable. The UNMEDIATED twin proves the fixture
            -- is genuinely adversarial rather than vacuously clean.
            let ids = take 60 (conceptIds prodCat)
                rootsUnder policy =
                    [ generateRoot (tripleProneRoot policy) c attempt
                    | c ← ids, attempt ← [0 .. 4 ∷ Int] ]
                raw = rootsUnder BoundaryUnmediated
                mediated = rootsUnder (profBoundary
                                        (boundaryFixture BoundaryEpenthetic))
            filter hasTripleRun raw `shouldSatisfy` not ∘ null
            filter hasTripleRun mediated `shouldBe` []

    describe "min-length top-up is a full morpheme boundary (#1095)" $ do
        it "repairs an inadmissible cluster there, not merely a triple" $ do
            -- The top-up is one of the issue's four NAMED sites, so it
            -- consults the admissibility relation like the affix and
            -- compound joins do — the root's own interior syllable joins
            -- are the only place the weaker triple-only guard applies.
            -- The unmediated twin proves the fixture really presents
            -- illegal clusters rather than passing vacuously.
            let ids = take 60 (conceptIds prodCat)
                rootsOf p = [ generateRoot p c attempt
                            | c ← ids, attempt ← [0 .. 4 ∷ Int] ]
                rawProf = topUpFixture BoundaryUnmediated
            filter (illegalTopUpCluster rawProf) (rootsOf rawProf)
                `shouldSatisfy` not ∘ null
            forM_ [BoundaryEpenthetic, BoundaryHarmonic, BoundarySimplifying] $ \rule → do
                let prof = topUpFixture (profBoundary (boundaryFixture rule))
                    roots = rootsOf prof
                roots `shouldSatisfy` not ∘ null
                filter (illegalTopUpCluster prof) roots `shouldBe` []
                filter ((< minNativeWordLength) ∘ T.length) roots `shouldBe` []
                filter hasTripleRun roots `shouldBe` []

    describe "grammatical marking survives boundary repair (#1095)" $ do
        it "keeps the bare root a prefix and the mark nonempty, for \
           \every version-3 language" $ do
            let stems = ["hoh", "karad", "sess", "bo", "ky"]
                offenders =
                    [ (profSeed p, stem, marked)
                    | p ← take 128 v3Profiles, stem ← stems
                    , marked ← [applyPluralMark p stem, applyPossessiveMark p stem]
                    , not (stem `T.isPrefixOf` marked)
                      ∨ T.length marked ≤ T.length stem
                      ∨ hasTripleRun marked ]
            offenders `shouldBe` []

        it "never leaves a root-final segment touching an identical \
           \affix-initial one" $ do
            -- The 'Eytoc-hohh' acceptance shape, built deliberately for
            -- every language's OWN real affixes rather than a fixture's:
            -- a stem ending in exactly the letter that affix starts with.
            -- An apostrophe-leading possessive is excluded because its
            -- own separator already keeps the letters apart.
            let checked =
                    [ (profSeed p, a0, stem, mark p stem)
                    | p ← take 128 v3Profiles
                    , (affix, mark) ← [ (plmAffix (profPlural p), applyPluralMark)
                                      , (pmAffix (profPossessive p), applyPossessiveMark) ]
                    , Just (a0, _) ← [T.uncons affix]
                    , a0 ≢ '\''
                    -- A filler distinct from a0, so the stem carries no
                    -- double of its own.
                    , filler ← take 1 [ c | c ← profConsonants p, c ≢ a0 ]
                    , let stem = T.pack [filler, a0] ]
                -- The exact defect: whatever follows the intact stem must
                -- not repeat its final letter. Testing for the doubled
                -- pair ANYWHERE would instead flag an affix like "yy"
                -- that carries its own legal double (a dual-role 'y' is
                -- in both inventories, so genAffix can draw it twice).
                offenders = [ e | e@(_, a0, stem, marked) ← checked
                            , T.take 1 (T.drop (T.length stem) marked)
                                ≡ T.singleton a0 ]
            checked `shouldSatisfy` ((≥ 128) ∘ length)
            offenders `shouldBe` []

    describe "separators survive boundary phonology (#1095 requirement 5)" $ do
        it "a hyphen-joining language still emits exactly one hyphen per \
           \compound, and never a doubled one" $ do
            let hyphenated =
                    [ w | p ← take 128 v3Profiles, profJoin p ≡ JoinHyphen
                        , Right w ← [renderNative p (rootsFor p)
                                                 (Modifier (cid "ASH") (cid "LAND"))] ]
            hyphenated `shouldSatisfy` not ∘ null
            filter ((≢ 1) ∘ T.count "-") hyphenated `shouldBe` []

        it "an apostrophe-bearing possessive affix keeps its apostrophe, \
           \exactly once and never leading or trailing" $ do
            let apostrophe =
                    [ (profSeed p, applyPossessiveMark p "karad")
                    | p ← take 128 v3Profiles
                    , "'" `T.isPrefixOf` pmAffix (profPossessive p) ]
            apostrophe `shouldSatisfy` not ∘ null
            filter (\(_, w) → T.count "'" w ≢ 1) apostrophe `shouldBe` []
            filter (\(_, w) → "'" `T.isPrefixOf` w ∨ "'" `T.isSuffixOf` w)
                   apostrophe `shouldBe` []

    describe "boundary phonology is per-language style state (#1095)" $ do
        it "seeds 0:255 use all three repair rules" $ do
            let ruleOf p = case profBoundary p of
                    BoundaryUnmediated      → Nothing
                    BoundaryMediated rep    → Just (brRule rep)
            forM_ [BoundaryEpenthetic, BoundaryHarmonic, BoundarySimplifying] $ \r →
                filter ((≡ Just r) ∘ ruleOf) v3Profiles
                    `shouldSatisfy` not ∘ null

        it "draws its segments from the profile's own inventories, with \
           \two DISTINCT linking consonants" $ do
            let offenders =
                    [ profSeed p
                    | p ← v3Profiles
                    , BoundaryMediated rep ← [profBoundary p]
                    , not (brEpenthetic rep `elem` profVowels p)
                      ∨ not (brLinker rep `elem` profConsonants p)
                      ∨ not (brLinkerAlt rep `elem` profConsonants p)
                      ∨ brLinker rep ≡ brLinkerAlt rep ]
            offenders `shouldBe` []

        it "participates in the profile signature" $ do
            let prof = buildProfileV3 (LangSeed 3)
            profileSignature (prof { profBoundary = BoundaryUnmediated })
                `shouldNotBe` profileSignature prof
            case profBoundary prof of
                BoundaryUnmediated   → expectationFailure "expected a policy"
                BoundaryMediated rep → do
                    let other = rep { brRule = if brRule rep ≡ BoundaryEpenthetic
                                                then BoundaryHarmonic
                                                else BoundaryEpenthetic }
                    profileSignature (prof { profBoundary = BoundaryMediated other })
                        `shouldNotBe` profileSignature prof

        it "keeps version 3's non-boundary style identical to version \
           \2's for the same seed" $ do
            -- The boundary draw is APPENDED at a fresh step index, so
            -- version 3 differs from version 2 in exactly one field —
            -- which is what makes its goldens attributable to boundary
            -- phonology rather than to a reshuffled profile.
            let stripped p = p { profVersion = GeneratorVersion 0
                               , profBoundary = BoundaryUnmediated }
                offenders =
                    [ s | s ← [0 .. 127 ∷ Word64]
                        , stripped (buildProfileV3 (LangSeed s))
                            ≢ stripped (buildProfileV2 (LangSeed s)) ]
            offenders `shouldBe` []

    describe "boundary phonology is deterministic (#1095 requirement 7)" $ do
        it "draws a PINNED policy for each of four fixed seeds" $ do
            -- The draw itself, against values fixed at authoring time:
            -- a generator that consistently picked a different rule or
            -- different segments satisfies any same-input comparison
            -- and fails these. The NAMES these four languages render
            -- are pinned separately, in the version-3 golden block
            -- below.
            let policyOf s = profBoundary (buildProfileV3 (LangSeed s))
            policyOf 0 `shouldBe` BoundaryMediated BoundaryRepair
                { brRule = BoundaryEpenthetic, brEpenthetic = 'o'
                , brLinker = 'b', brLinkerAlt = 'c' }
            policyOf 1 `shouldBe` BoundaryMediated BoundaryRepair
                { brRule = BoundaryHarmonic, brEpenthetic = 'a'
                , brLinker = 'k', brLinkerAlt = 'g' }
            policyOf 42 `shouldBe` BoundaryMediated BoundaryRepair
                { brRule = BoundaryHarmonic, brEpenthetic = 'i'
                , brLinker = 'y', brLinkerAlt = 't' }
            policyOf (12345 ∷ Word64) `shouldBe` BoundaryMediated BoundaryRepair
                { brRule = BoundaryHarmonic, brEpenthetic = 'i'
                , brLinker = 'r', brLinkerAlt = 'y' }

        it "repairs a boundary as a pure function of (profile, the two \
           \pieces), pinned across all three rules" $ do
            -- The SAME two pieces through three languages that drew
            -- three different rules. The repair follows the profile, so
            -- the three results differ from one another, and each is a
            -- fixed value rather than a second call to itself.
            let epenthetic  = buildProfileV3 (LangSeed 0)
                harmonic    = buildProfileV3 (LangSeed 9)
                simplifying = buildProfileV3 (LangSeed 3)
                profiles    = [epenthetic, harmonic, simplifying]
            map (boundaryRuleText ∘ profBoundary) profiles
                `shouldBe` ["epenthetic", "harmonic", "simplifying"]
            map (\p → (joinMorphemes p "hoh" "h", joinSyllables p "abb" "ba"))
                profiles
                `shouldBe` [ ("hohoh", "abboba")
                           , ("hohzh", "abbzba")
                           , ("hohgh", "abbgba") ]

    -- #1096: version 4's bound morphemes.
    describe "bound-form selection (#1096 requirements 2, 3, 5)" $ do
        it "selects PINNED bound forms for fixed seeds" $ do
            -- What "deterministic for the same version, seed, and
            -- catalogue" actually claims: THESE concepts, with THESE
            -- forms, for this seed over the production catalogue. Seed
            -- 0's own map is the golden further below, so these are the
            -- three other languages of the same fixed set.
            let boundFor s =
                    M.toList (lrBound (rootsFor (buildProfileV4 (LangSeed s))))
            boundFor 1 `shouldBe`
                [ (cid "BLESSING", "hy"), (cid "HAWK", "sy")
                , (cid "HEARTH", "ysy"), (cid "HOUND", "su")
                , (cid "MOON", "uzy"), (cid "SAND", "ypy")
                , (cid "SHAME", "uhu"), (cid "SMOKE", "uf") ]
            boundFor 42 `shouldBe`
                [ (cid "ANGEL", "voko"), (cid "COAL", "vik")
                , (cid "FAMINE", "gokt"), (cid "FATE", "bkeb")
                , (cid "FROST", "ki"), (cid "GATE", "ter")
                , (cid "RAVEN", "bit"), (cid "WINTER", "kta") ]
            boundFor (12345 ∷ Word64) `shouldBe`
                [ (cid "BRIDGE", "piy"), (cid "CROWN", "gaph")
                , (cid "HARBOR", "hayhuha"), (cid "HOLLOW", "ruyk")
                , (cid "HORIZON", "hak"), (cid "SHADOW", "huk")
                , (cid "THUNDER", "yuh"), (cid "TITAN", "hu") ]

        it "does not depend on the order the catalogue is enumerated in" $ do
            -- Requirement 2's "must not depend on catalogue traversal
            -- order", pinned at BOTH levels: the ranking itself, and the
            -- assignment built on top of it.
            let ids = conceptIds prodCat
                shuffled = reverse (take 75 ids) <> drop 75 ids
            forM_ [0, 7, 99 ∷ Word64] $ \s → do
                let p = buildProfileV4 (LangSeed s)
                boundSelectionOrder p ids
                    `shouldBe` boundSelectionOrder p (reverse ids)
                boundSelectionOrder p ids
                    `shouldBe` boundSelectionOrder p shuffled
                lrBound (assignLanguageRoots p prodOrds ids)
                    `shouldBe` lrBound (assignLanguageRoots p prodOrds
                                                            (reverse ids))

        it "visits every concept exactly once, in a total order" $ do
            -- The (rank, ConceptId) tie-break is what makes the order
            -- total: without it two concepts that hashed equal would be
            -- ordered by whatever the caller presented first.
            let ids = conceptIds prodCat
                p   = buildProfileV4 (LangSeed 3)
                order = boundSelectionOrder p ids
            length order `shouldBe` length ids
            sort order `shouldBe` sort ids

        it "never accepts more than eight per language" $ do
            maxBoundForms `shouldBe` 8
            let over = [ (profSeed p, M.size (lrBound lr))
                       | (p, lr) ← v4Assignments
                       , M.size (lrBound lr) > maxBoundForms ]
            over `shouldBe` []

        it "accepts some in every language of the canonical sample" $ do
            -- Every rule above is "no bad form exists", which a
            -- generator that quietly stopped selecting anything would
            -- also satisfy.
            let empties = [ profSeed p | (p, lr) ← v4Assignments
                          , M.null (lrBound lr) ]
            empties `shouldBe` []

        it "stores only nonempty strictly-shorter prefixes that retain a \
           \visible letter (requirement 3)" $ do
            let visible = T.any isNameLetter
                offenders =
                    [ (profSeed p, c, free, b)
                    | (p, c, free, b) ← v4BoundForms
                    , T.null b
                      ∨ not (b `T.isPrefixOf` free)
                      ∨ T.length b ≥ T.length free
                      ∨ not (visible b) ]
            v4BoundForms `shouldSatisfy` not ∘ null
            offenders `shouldBe` []

        it "differs from the free form ONLY by deleted terminal \
           \characters" $ do
            -- Stated as its own assertion rather than folded into the
            -- prefix check above: this is the property that excludes
            -- stem substitution, internal deletion, and gradation, and
            -- it deserves to fail by name if it ever stops holding.
            let offenders =
                    [ (profSeed p, c, free, b)
                    | (p, c, free, b) ← v4BoundForms
                    , b <> T.drop (T.length b) free ≢ free ]
            offenders `shouldBe` []

        it "stores only forms its own profile's admissibility relation \
           \accepts (requirement 4)" $ do
            let offenders = [ (profSeed p, c, b)
                            | (p, c, _, b) ← v4BoundForms
                            , not (boundFormAdmissible p b) ]
            offenders `shouldBe` []

            -- The predicate really rejects: a bound form carrying an
            -- inadmissible cluster is refused by the same relation.
            let fx = boundFixture ModifierFirst OwnerFirst
            boundFormAdmissible fx "kab" `shouldBe` True    -- 'bh'-free
            boundFormAdmissible fx "kabh" `shouldBe` True   -- 'bh' admitted
            boundFormAdmissible fx "kabs" `shouldBe` False  -- 'bs' is not
            boundFormAdmissible fx "kabb" `shouldBe` False  -- irreflexive

        it "validates a pair involving a dual-role 'y', which a \
           \consonant-ONLY scoping would have skipped" $ do
            -- Requirement 4 says CONSONANT-CAPABLE, and a dual-role 'y'
            -- (#1094 requirement 6) is exactly that. This is where the
            -- filter is deliberately WIDER than #1095's boundary rule,
            -- which asks about consonant-only pairs because it rewrites
            -- text whose slot provenance it cannot know.
            boundFormAdmissible dualRoleBoundFixture "by" `shouldBe` True
            boundFormAdmissible dualRoleBoundFixture "ky" `shouldBe` False
            boundFormAdmissible dualRoleBoundFixture "yb" `shouldBe` False

            -- And it is load bearing on real languages, not only on a
            -- fixture: candidates exist that this scoping rejects and a
            -- consonant-only one would have let through.
            let narrowOnly p c = consonantCapable p c ∧ not (vowelCapable p c)
                narrowAdmissible p t = and
                    [ not (narrowOnly p a ∧ narrowOnly p b)
                      ∨ admissibleOnset p a b
                    | (a, b) ← T.zip t (T.drop 1 t) ]
                divergent =
                    [ (profSeed p, c, cand)
                    | (p, lr) ← take 64 v4Assignments
                    , (c, r) ← M.toList (lrFree lr)
                    , cand ← boundCandidates p c r
                    , narrowAdmissible p cand
                    , not (boundFormAdmissible p cand) ]
            divergent `shouldSatisfy` not ∘ null

        it "has zero free/free and zero bound-related collisions \
           \(requirement 5)" $ do
            let freeFree = [ (profSeed p, countDuplicateRoots (lrFree lr))
                           | (p, lr) ← v4Assignments
                           , countDuplicateRoots (lrFree lr) ≢ 0 ]
                boundAny = [ (profSeed p, n)
                           | (p, lr) ← v4Assignments
                           , let n = countBoundCollisions (lrFree lr) (lrBound lr)
                           , n ≢ 0 ]
            freeFree `shouldBe` []
            boundAny `shouldBe` []

            -- The bound-collision counter really counts: an assignment
            -- whose bound form equals another concept's free root is
            -- reported, and one that equals another bound form is too.
            countBoundCollisions
                (M.fromList [(cid "A", "kaba"), (cid "B", "kab")])
                (M.fromList [(cid "A", "kab")]) `shouldBe` 1
            countBoundCollisions
                (M.fromList [(cid "A", "kabo"), (cid "B", "kabe")])
                (M.fromList [(cid "A", "kab"), (cid "B", "kab")])
                `shouldBe` 2
            countBoundCollisions (lrFree boundFixtureRoots)
                                  (lrBound boundFixtureRoots) `shouldBe` 0

        it "never rerolls or changes a free root because of a bound form" $ do
            -- Requirement 5: existing free-root uniqueness is untouched,
            -- so the free half of a version-4 assignment is EXACTLY what
            -- assignRoots alone produces.
            let offenders =
                    [ profSeed p
                    | (p, lr) ← take 64 v4Assignments
                    , lrFree lr ≢ assignRoots p prodOrds (conceptIds prodCat) ]
            offenders `shouldBe` []

        it "ranks by a value domain-separated from root generation" $ do
            -- Requirement 2's "dedicated" value: if the ranking simply
            -- reused the per-concept root seed, which concepts got bound
            -- forms would be a function of the root draw rather than a
            -- selection of its own.
            let p = buildProfileV4 (LangSeed 11)
                ids = take 40 (conceptIds prodCat)
                same = [ c | c ← ids
                       , boundSeed (profVersion p) (profSeed p) c
                           ≡ conceptSeed (profVersion p) (profSeed p) c 0 ]
            same `shouldBe` []

    describe "bound forms exist only from generator version 4 (#1096 requirement 1)" $ do
        it "versions 1-3 assign none at all" $ do
            let historical = [ buildProfileV1 (LangSeed s) | s ← [0 .. 15] ]
                          <> [ buildProfileV2 (LangSeed s) | s ← [0 .. 15] ]
                          <> [ buildProfileV3 (LangSeed s) | s ← [0 .. 15] ]
                offenders = [ (profVersion p, profSeed p)
                            | p ← historical
                            , not (M.null (lrBound (rootsFor p)))
                              ∨ formsBoundMorphemes p ]
            offenders `shouldBe` []
            boundFormVersion `shouldBe` 4

        it "so a historical language renders every dependent slot with \
           \the free form — which is what keeps its goldens identical" $ do
            let exprs = map snd canonicalExpressions
                offenders =
                    [ (profVersion p, profSeed p, e)
                    | p ← [ buildProfileV1 (LangSeed s) | s ← [0 .. 15] ]
                       <> [ buildProfileV2 (LangSeed s) | s ← [0 .. 15] ]
                       <> [ buildProfileV3 (LangSeed s) | s ← [0 .. 15] ]
                    , let lr = rootsFor p
                    , e ← exprs
                    , renderNative p lr e
                        ≢ renderNative p (freeRootsOnly (lrFree lr)) e ]
            offenders `shouldBe` []

        it "version 4 assigns them" $
            filter (not ∘ formsBoundMorphemes) v4Profiles `shouldBe` []

    describe "the bound-form slot matrix (#1096 requirements 6, 7)" $ do
        let dep = cid "DEP"
            hd  = cid "HEAD"
            renderRow prof lr (_, e) = renderNative prof lr e

        it "renders every row of the matrix, both ordering directions" $ do
            -- Read straight off the fixture: DEP's free form is 'kaba'
            -- and its bound form 'kab', HEAD's only form is 'ota', the
            -- plural mark is 'h' and the possessive "'s".
            let modFirst = boundFixture ModifierFirst OwnerFirst
                headFirst = boundFixture HeadFirst HeadFirstGenitive
                rows p = map (renderRow p boundFixtureRoots)
                             (boundSlotExpressions dep hd)
            rows modFirst `shouldBe`
                [ Right "Kaba", Right "Kabota", Right "Kabota"
                , Right "Kabhota", Right "Kab'sota" ]
            rows headFirst `shouldBe`
                [ Right "Kaba", Right "Otakab", Right "Otakab"
                , Right "Otakabh", Right "Otakab's" ]

        it "Bare always uses the free form, in both directions" $ do
            forM_ [ boundFixture ModifierFirst OwnerFirst
                  , boundFixture HeadFirst HeadFirstGenitive ] $ \p → do
                renderNative p boundFixtureRoots (Bare dep)
                    `shouldBe` Right "Kaba"
                renderNative p boundFixtureRoots (Bare dep)
                    `shouldBe` renderNative p boundFixtureFree (Bare dep)

        it "a concept with no bound form uses its free form in every \
           \slot" $ do
            -- HEAD carries no bound form, so putting IT in the dependent
            -- slot must render exactly as a bound-form-free language
            -- would.
            forM_ [ boundFixture ModifierFirst OwnerFirst
                  , boundFixture HeadFirst HeadFirstGenitive ] $ \p →
                forM_ (boundSlotExpressions hd dep) $ \(_, e) →
                    renderNative p boundFixtureRoots e
                        `shouldBe` renderNative p boundFixtureFree e

        it "applies grammatical marking AFTER selecting the bound form, \
           \not before (requirement 7)" $ do
            let p = boundFixture ModifierFirst OwnerFirst
            -- Marking the bound form gives 'kabh'/"kab's"; marking the
            -- free form first and shortening afterwards could not, since
            -- truncation would take the mark off again.
            applyPluralMark p "kab" `shouldBe` "kabh"
            applyPossessiveMark p "kab" `shouldBe` "kab's"
            renderNative p boundFixtureRoots (Of hd Plural dep)
                `shouldBe` Right "Kabhota"
            renderNative p boundFixtureRoots (Possessive dep hd)
                `shouldBe` Right "Kab'sota"
            -- Singular Of applies no number marker at all.
            renderNative p boundFixtureRoots (Of hd Singular dep)
                `shouldBe` Right "Kabota"

        it "sends the join the bound form created through #1095's \
           \boundary repair" $ do
            -- 'kas' meets the 'h' plural across an inadmissible 'sh'
            -- cluster this language rejects, so the repair mediates a
            -- boundary that exists only BECAUSE of the shortening.
            let p = boundFixture ModifierFirst OwnerFirst
            admissibleOnset p 's' 'h' `shouldBe` False
            boundaryNeedsRepair p "kas" "h" `shouldBe` True
            applyPluralMark p "kas" `shouldBe` "kasah"
            renderNative p boundFixtureRepair (Of hd Plural dep)
                `shouldBe` Right "Kasahota"

        it "keeps hyphens and apostrophes exactly as #1095 left them" $ do
            let p = (boundFixture ModifierFirst OwnerFirst)
                        { profJoin = JoinHyphen }
            renderNative p boundFixtureRoots (Modifier dep hd)
                `shouldBe` Right "Kab-ota"
            renderNative p boundFixtureRoots (Possessive dep hd)
                `shouldBe` Right "Kab's-ota"

        it "renders every row from an assignment holding only the \
           \concepts the expression references (requirement 8)" $ do
            -- renderNative never sees a Catalogue, so "no catalogue
            -- scan" is testable directly: hand it a two-concept
            -- assignment carved out of a real 150-concept language and
            -- every row must render, byte-identically.
            let offenders =
                    [ (profSeed p, c, label)
                    | (p, lr) ← take 32 v4Assignments
                    , c ← M.keys (lrBound lr)
                    , let h = headAgainst c
                    , let keep k _ = k ≡ c ∨ k ≡ h
                    , let minimal = LanguageRoots
                              { lrFree  = M.filterWithKey keep (lrFree lr)
                              , lrBound = M.filterWithKey keep (lrBound lr) }
                    , (label, e) ← boundSlotExpressions c h
                    , renderNative p minimal e ≢ renderNative p lr e
                      ∨ isLeft' (renderNative p minimal e) ]
            offenders `shouldBe` []

    describe "the slot matrix holds for real languages (#1096 requirement 6)" $ do
        it "a dependent slot renders exactly as if the bound form WERE \
           \the concept's only form" $ do
            -- The strongest statement available without reimplementing
            -- ordering, marking, or boundary repair: substitute the
            -- bound form in as the free root, switch bound morphology
            -- off, and the production renderer must produce the same
            -- bytes. Covers every row, every seed, both directions.
            let offenders =
                    [ (profSeed p, c, label)
                    | (p, lr) ← v4Assignments
                    , c ← M.keys (lrBound lr)
                    , let h = headAgainst c
                    , (label, e) ← boundSlotExpressions c h
                    , label ≢ "bare"
                    , renderNative p lr e ≢ renderNative p (asFreeRoot c lr) e ]
            offenders `shouldBe` []

        it "a head slot never consults a bound form, whichever side the \
           \profile writes first" $ do
            -- Deleting the HEAD concept's own bound form must change
            -- nothing. If the head slot ever selected one, this would
            -- differ for every language whose head happened to have one.
            let checked =
                    [ (p, lr, c, h, row)
                    | (p, lr) ← v4Assignments
                    , c ← M.keys (lrBound lr)
                    , let h = headAgainst c
                    , M.member h (lrBound lr)
                    , row ← boundSlotExpressions c h ]
                offenders =
                    [ (profSeed p, c, label)
                    | (p, lr, c, h, (label, e)) ← checked
                    , let noHead = lr { lrBound = M.delete h (lrBound lr) }
                    , renderNative p lr e ≢ renderNative p noHead e ]
            -- Only meaningful if the head really carries a bound form
            -- somewhere in the sample.
            checked `shouldSatisfy` not ∘ null
            offenders `shouldBe` []

        it "exercises both compound and both genitive ordering \
           \directions" $ do
            forM_ [ModifierFirst, HeadFirst] $ \o →
                filter ((≡ o) ∘ profCompoundOrder) v4Profiles
                    `shouldSatisfy` not ∘ null
            forM_ [OwnerFirst, HeadFirstGenitive] $ \o →
                filter ((≡ o) ∘ pmOrder ∘ profPossessive) v4Profiles
                    `shouldSatisfy` not ∘ null

        it "produces a visible shortening in completed output" $ do
            -- Requirement 3 shortens a MORPHEME; this is the separate,
            -- weaker-looking but load-bearing claim that the shortening
            -- reaches the finished name. A boundary repair can insert
            -- exactly what the truncation removed, so this is not
            -- implied by "a bound form exists".
            let shortened =
                    [ (profSeed p, c, label)
                    | (p, lr) ← v4Assignments
                    , c ← M.keys (lrBound lr)
                    , (label, e) ← boundSlotExpressions c (headAgainst c)
                    , renderNative p lr e
                        ≢ renderNative p (freeRootsOnly (lrFree lr)) e ]
            shortened `shouldSatisfy` not ∘ null

        it "every bound-slot name satisfies the output contract and \
           \carries no triple-letter run" $ do
            let named =
                    [ (profSeed p, w)
                    | (p, lr) ← v4Assignments
                    , c ← M.keys (lrBound lr)
                    , (_, e) ← boundSlotExpressions c (headAgainst c)
                    , Right w ← [renderNative p lr e] ]
            named `shouldSatisfy` ((≥ 1000) ∘ length)
            filter (not ∘ contractOk ∘ snd) named `shouldBe` []
            filter (hasTripleRun ∘ snd) named `shouldBe` []

    describe "no triple-letter run survives version 4's joins (#1096 requirement 8)" $
        it "holds for every canonical version-4 name across many seeds" $ do
            let named = [ (s, w) | s ← [0 .. 127 ∷ Word64]
                        , Right w ← nativeRenderingsV4 s ]
                offenders = [ (s, w) | (s, w) ← named, hasTripleRun w ]
            named `shouldSatisfy` ((≥ 600) ∘ length)
            named `shouldSatisfy` any (T.isInfixOf "-" ∘ snd)
            named `shouldSatisfy` any (T.isInfixOf "'" ∘ snd)
            named `shouldSatisfy` any (not ∘ T.isInfixOf "-" ∘ snd)
            offenders `shouldBe` []

    -- Golden outputs (#710 requirement 15): a change to any of these
    -- pins requires incrementing the language-generator version rather
    -- than silently changing an existing version's output.
    describe "golden outputs (pinned, generator version 1)" $ do
        it "seed 0" $ nativeRenderings 0 `shouldBe`
            [ Right "Jowwem", Right "Sinmoyiawga", Right "Hahaslegaen"
            , Right "Binotlomehyoyimbo", Right "Selibohsaamj" ]

        it "seed 1" $ nativeRenderings 1 `shouldBe`
            [ Right "Uyjac", Right "Gut-yez", Right "Bub-ulay"
            , Right "Dez-ulurla", Right "Yudz-zag" ]

        it "seed 42" $ nativeRenderings 42 `shouldBe`
            [ Right "Rregeg", Right "Jigpa-gyigez", Right "Jiggji-rorjar"
            , Right "Gapzraz-recpog", Right "Zoccce-payi'g" ]

        it "seed 12345" $ nativeRenderings 12345 `shouldBe`
            [ Right "Vurkussuv", Right "Ravvusjirik", Right "Vuvrujakisaj"
            , Right "Wivjasijrivarwir", Right "Ravsikirjas" ]

    -- A SEPARATE block, not a replacement: version 1's pins above stay
    -- exactly as they were (including "Rregeg", the identical-consonant
    -- onset #1094 deliberately preserves in the historical version),
    -- and version 2 gets its own.
    describe "golden outputs (pinned, generator version 2)" $ do
        it "seed 0" $ nativeRenderingsV2 0 `shouldBe`
            [ Right "Lifor", Right "Ilbicbyfviv", Right "Ehsoveslev"
            , Right "Yrejohkifce", Right "Enisnyhcyihirv" ]

        it "seed 1" $ nativeRenderingsV2 1 `shouldBe`
            [ Right "Gun", Right "Asap-pyp", Right "Ynan-tub"
            , Right "Nup-hahny", Right "Bybb-fuf" ]

        it "seed 42" $ nativeRenderingsV2 42 `shouldBe`
            [ Right "Kovta", Right "Tikkyi-revvro", Right "Roybiy-bravri"
            , Right "Tevogr-yartey", Right "Vyirek-rebor'b" ]

        it "seed 12345" $ nativeRenderingsV2 12345 `shouldBe`
            [ Right "Kipahkug", Right "Payihgipayyig", Right "Guruhkuyagyih"
            , Right "Piyugyarpagyur", Right "Yiyagaykuyr" ]

        it "every canonical version-2 name satisfies the output \
           \contract (requirement 6)" $ do
            let allRenderings = concatMap nativeRenderingsV2 [0 .. 40]
                texts = [ w | Right w ← allRenderings ]
            length texts `shouldBe` length allRenderings
            filter (not ∘ contractOk) texts `shouldBe` []

    -- Version 3's own block, added ALONGSIDE the two above rather than
    -- replacing either (#1095 requirement 6): #1092 keeps every
    -- historical version constructible, so their pins — including
    -- version 1's triple-bearing "Zoccce-payi'g", the defect #1095 fixes
    -- going forward — must keep passing unchanged.
    describe "golden outputs (pinned, generator version 3)" $ do
        it "seed 0" $ nativeRenderingsV3 0 `shouldBe`
            [ Right "Ihec", Right "Vokreryjy", Right "Senycyrosbin"
            , Right "Hovlenefolentysoce", Right "Nytivyvcehybycov" ]

        it "seed 1" $ nativeRenderingsV3 1 `shouldBe`
            [ Right "Tyh", Right "Fyn-ytap", Right "Azapat-put"
            , Right "Byg-anagyzny", Right "Ubupugub-yftyk" ]

        it "seed 42" $ nativeRenderingsV3 42 `shouldBe`
            [ Right "Yokvya", Right "Tabvib-gigbi", Right "Vaktok-bkivra"
            , Right "Kgagatar-vigkor", Right "Vrevte-ragi'b" ]

        it "seed 12345" $ nativeRenderingsV3 12345 `shouldBe`
            [ Right "Ruyri", Right "Puypurugaripkap", Right "Rurkipiyuya"
            , Right "Pukapigipipuk", Right "Kurkaghikurkuyr" ]

        it "every canonical version-3 name satisfies the output \
           \contract (requirement 6)" $ do
            let allRenderings = concatMap nativeRenderingsV3 [0 .. 40]
                texts = [ w | Right w ← allRenderings ]
            length texts `shouldBe` length allRenderings
            filter (not ∘ contractOk) texts `shouldBe` []

    -- Version 4's own block, added ALONGSIDE the three above rather than
    -- replacing any of them (#1096 requirement 1): #1092 keeps every
    -- historical version constructible, so their pins — version 1's
    -- triple-bearing "Zoccce-payi'g" included — must keep passing
    -- unchanged while this version's dependent slots shorten.
    describe "golden outputs (pinned, generator version 4)" $ do
        it "seed 0" $ nativeRenderingsV4 0 `shouldBe`
            [ Right "Nelyhlon", Right "Kiivenocetethoj", Right "Ysseyhcovyh"
            , Right "Fifytojceiboce", Right "Likihisibtivbev" ]

        it "seed 1" $ nativeRenderingsV4 1 `shouldBe`
            [ Right "Ufupyn", Right "Ahaz-yfypug", Right "Agafak-upag"
            , Right "Zas-fynyny", Right "Ypabab-tah" ]

        it "seed 42" $ nativeRenderingsV4 42 `shouldBe`
            [ Right "Gatir", Right "Vbare-gokig", Right "Kta-terbka"
            , Right "Kevayr-gikre", Right "Yere-btokre'b" ]

        it "seed 12345" $ nativeRenderingsV4 12345 `shouldBe`
            [ Right "Yupar", Right "Ripgakruhupakrahgak", Right "Gagpagupupahirkih"
            , Right "Gahyuyigiyaga", Right "Yakyahapugahruyr" ]

        it "every canonical version-4 name satisfies the output \
           \contract (requirement 6)" $ do
            let allRenderings = concatMap nativeRenderingsV4 [0 .. 40]
                texts = [ w | Right w ← allRenderings ]
            length texts `shouldBe` length allRenderings
            filter (not ∘ contractOk) texts `shouldBe` []

        -- The one golden that shows the feature: a selected concept
        -- rendered bare, then in each dependent slot of the matrix, in
        -- a real generated language rather than a fixture.
        it "seed 0's bound forms, and one concept through every slot" $ do
            let p  = buildProfileV4 (LangSeed 0)
                lr = rootsFor p
                c  = cid "DAWN"
            M.toList (lrBound lr) `shouldBe`
                [ (cid "CURSE", "syr"), (cid "DAWN", "bois")
                , (cid "MIDNIGHT", "se"), (cid "PROPHET", "sic")
                , (cid "RELIC", "ihovy"), (cid "SPIDER", "hilili")
                , (cid "SPIRE", "yly"), (cid "STAG", "cenin") ]
            M.lookup c (lrFree lr) `shouldBe` Just "boisfen"
            map (\(_, e) → renderNative p lr e)
                (boundSlotExpressions c (headAgainst c)) `shouldBe`
                [ Right "Boisfen", Right "Ocyrobois", Right "Ocyrobois"
                , Right "Ocyroboisoce", Right "Ocyroboisov" ]
    -- #1100: per-language extended orthography. Everything below is
    -- stated over the SAME 256-seed sample tools/language_report.py
    -- gates, so a property that holds here holds for the population the
    -- report measures.
    describe "extended letters are inventory, not decoration (#1100 requirement 1)" $ do
        it "extends the same seed's version-4 inventory rather than \
           \replacing it" $ do
            -- Requirement 1's "inventory, not decoration", stated as a
            -- relation to the UNMARKED language of the same seed rather
            -- than as a comparison of one construction to another:
            -- version 5 keeps version 4's letters in order, and
            -- everything it adds is exactly what
            -- 'profileExtendedChars' reports. A decorating
            -- implementation would substitute rather than extend, and
            -- shows up here as a broken prefix.
            let offenders =
                    [ (s, profVowels four, profVowels five
                      , profConsonants four, profConsonants five
                      , profileExtendedChars five)
                    | s ← [0 .. 63 ∷ Word64]
                    , let four = buildProfileV4 (LangSeed s)
                          five = buildProfileV5 (LangSeed s)
                          keptV = take (length (profVowels four))
                                       (profVowels five)
                          keptC = take (length (profConsonants four))
                                       (profConsonants five)
                          added = drop (length (profVowels four))
                                       (profVowels five)
                                ⧺ drop (length (profConsonants four))
                                       (profConsonants five)
                    , keptV ≢ profVowels four
                      ∨ keptC ≢ profConsonants four
                      ∨ sort (profileExtendedChars five) ≢ sort added ]
            offenders `shouldBe` []
            -- Two fixed languages of that sample, pinned: seed 7 drew
            -- three marked consonants, seed 63 a single marked vowel.
            -- (Seeds 0, 1 and 42 are the golden at the end of this
            -- module.)
            (profileExtendedChars (buildProfileV5 (LangSeed 7))
             , profileDiacritic (buildProfileV5 (LangSeed 7)))
                `shouldBe` ("\x011D\x0125\x0135", Just DiaCircumflex)
            (profileExtendedChars (buildProfileV5 (LangSeed 63))
             , profileDiacritic (buildProfileV5 (LangSeed 63)))
                `shouldBe` ("\x016F", Just DiaRing)

        it "only marks a base sound the language already has" $ do
            -- The rule that ties an accent to the language rather than
            -- to the alphabet: 'á' means "this language distinguishes
            -- its own /a/", so a language without 'a' cannot have it.
            let orphans =
                    [ (profSeed p, marked, base)
                    | p ← v5Profiles
                    , (_, slot, base, marked, _) ← extendedLetterTable
                    , marked `elem` profileExtendedChars p
                    , let inventory = case slot of
                            VowelSlot     → profVowels p
                            ConsonantSlot → profConsonants p
                    , base `notElem` inventory ]
            v5Marked `shouldSatisfy` not ∘ null
            orphans `shouldBe` []

        it "puts a marked letter in the inventory its base belongs to" $ do
            let misplaced =
                    [ (profSeed p, marked)
                    | p ← v5Profiles
                    , (_, slot, _, marked, _) ← extendedLetterTable
                    , let wrong = case slot of
                            VowelSlot     → profConsonants p
                            ConsonantSlot → profVowels p
                    , marked `elem` wrong ]
            misplaced `shouldBe` []

        it "gives one language exactly one diacritic family" $ do
            let familyOf c = [ f | (f, _, _, lo, _) ← extendedLetterTable
                             , lo ≡ c ]
                mixed = [ (profSeed p, profileExtendedChars p)
                        | p ← v5Marked
                        , length (nub (concatMap familyOf
                                        (profileExtendedChars p))) ≢ 1 ]
            mixed `shouldBe` []
            -- And the derived accessor reports that single family
            -- rather than merely the first letter's.
            forM_ v5Marked $ \p →
                case nub (concatMap familyOf (profileExtendedChars p)) of
                    [f] → profileDiacritic p `shouldBe` Just f
                    fs  → expectationFailure $
                            "seed " ⧺ show (langSeedWord (profSeed p))
                            ⧺ " spans diacritic families " ⧺ show fs

        it "keeps the marked set small enough to read as a convention" $ do
            -- Consistency has an upper bound as well as a lower one: a
            -- language that marked everything would be noise, which is
            -- the failure mode the whole design principle rejects.
            let oversized = [ (profSeed p, profileExtendedChars p)
                            | p ← v5Marked
                            , length (profileExtendedChars p)
                                > 2 * maxMarksPerInventory ]
            oversized `shouldBe` []

        it "is drawn by some languages of the canonical sample and not \
           \others" $ do
            -- The acceptance criterion the whole design rests on: the
            -- choice VARIES by seed. One-sided in either direction and
            -- an accent identifies no language in particular.
            length v5Marked `shouldSatisfy` (≥ 100)
            length v5Plain `shouldSatisfy` (≥ 20)
            length v5Marked + length v5Plain `shouldBe` 256
            -- More than one family across the sample, for the same
            -- reason: difference ACROSS worlds, not just within one.
            nub [ profileDiacritic p | p ← v5Marked ]
                `shouldSatisfy` ((≥ 2) ∘ length)

        it "adds no extended letter below generator version 5" $ do
            -- Versions 1-4 are frozen output (#1092 requirement 4). An
            -- accented letter appearing in one would re-render an
            -- existing world's name.
            let historical = concat
                    [ [ buildProfileV1 (LangSeed s) | s ← [0 .. 63] ]
                    , [ buildProfileV2 (LangSeed s) | s ← [0 .. 63] ]
                    , [ buildProfileV3 (LangSeed s) | s ← [0 .. 63] ]
                    , [ buildProfileV4 (LangSeed s) | s ← [0 .. 63] ] ]
            filter (not ∘ null ∘ profileExtendedChars) historical
                `shouldBe` []
            map profileDiacritic historical
                `shouldSatisfy` all (≡ Nothing)
            extendedOrthographyVersion `shouldBe` 5

        it "never emits a mark the rendering language does not hold" $ do
            -- The negative form of the same property, measured on
            -- completed output rather than on the profile: a
            -- post-render substitution pass would show up here as a
            -- character the language never had.
            let offenders = [ (profSeed p, w, foreignExtended p w)
                            | (p, lr) ← v5Assignments
                            , (_, e) ← canonicalExpressions
                            , Right w ← [renderNative p lr e]
                            , not (null (foreignExtended p w)) ]
            offenders `shouldBe` []

        it "actually reaches completed names, in more than one of a \
           \language's own names" $ do
            -- Every rule above is "nothing wrong appears", which a
            -- generator that drew inventories and then never used them
            -- would also satisfy.
            let markedNames p lr =
                    [ w | (_, e) ← canonicalExpressions
                        , Right w ← [renderNative p lr e]
                        , T.any isExtendedLetter w ]
                perLanguage = [ length (markedNames p lr)
                              | (p, lr) ← v5Assignments
                              , not (null (profileExtendedChars p)) ]
            sum perLanguage `shouldSatisfy` (> 0)
            -- A marked language shows its marks across its names, not
            -- in a single one — that is what "a convention" means.
            length (filter (≥ 2) perLanguage)
                `shouldSatisfy` (≥ length perLanguage `div` 2)

    describe "extended letters obey every phonological rule (#1100 requirement 2)" $ do
        it "participates in #1094's admissible-onset relation" $ do
            let markedPairs =
                    [ (profSeed p, a, b)
                    | p ← v5Marked, (a, b) ← onsetPairs (profOnset p)
                    , isExtendedLetter a ∨ isExtendedLetter b ]
                -- Every admitted pair is still drawn from the profile's
                -- own inventory, marked letters included.
                outside =
                    [ (profSeed p, a, b)
                    | p ← v5Profiles, (a, b) ← onsetPairs (profOnset p)
                    , a `notElem` profConsonants p
                      ∨ b `notElem` profConsonants p ]
            markedPairs `shouldSatisfy` not ∘ null
            outside `shouldBe` []

        it "keeps every version-5 relation inside #1094's density band" $ do
            -- The widened inventories change the n(n-1) denominator, so
            -- the band is re-checked rather than assumed to survive.
            filter (not ∘ onsetDensityOk) v5Profiles `shouldBe` []

        it "can be a #1095 boundary-repair segment" $ do
            let marked = [ profSeed p | p ← v5Marked
                         , T.any isExtendedLetter
                                 (boundarySegmentText (profBoundary p)) ]
                outside =
                    [ (profSeed p, c)
                    | p ← v5Profiles
                    , c ← T.unpack (boundarySegmentText (profBoundary p))
                    , c `notElem` (profVowels p <> profConsonants p) ]
            marked `shouldSatisfy` not ∘ null
            outside `shouldBe` []

        it "can be an affix letter" $ do
            let marked = [ profSeed p | p ← v5Marked
                         , T.any isExtendedLetter (plmAffix (profPlural p))
                           ∨ T.any isExtendedLetter
                                   (pmAffix (profPossessive p)) ]
            marked `shouldSatisfy` not ∘ null

        it "can appear in a #1096 bound form, admissibly" $ do
            let boundForms = [ (p, b) | (p, lr) ← v5Assignments
                             , b ← M.elems (lrBound lr) ]
                marked = [ b | (_, b) ← boundForms, T.any isExtendedLetter b ]
                inadmissible = [ (profSeed p, b) | (p, b) ← boundForms
                               , not (boundFormAdmissible p b) ]
            marked `shouldSatisfy` not ∘ null
            inadmissible `shouldBe` []

        it "forms no triple-letter run in any canonical version-5 name" $ do
            -- #1095's guarantee over the widened inventory. The
            -- detector itself had to widen for this to mean anything —
            -- an ASCII-only letter predicate walks straight past 'ááá'.
            let named = [ (s, w) | s ← [0 .. 127 ∷ Word64]
                        , Right w ← nativeRenderingsV5 s ]
                offenders = [ (s, w) | (s, w) ← named, hasTripleRun w ]
            named `shouldSatisfy` ((≥ 600) ∘ length)
            named `shouldSatisfy` any (T.any isExtendedLetter ∘ snd)
            offenders `shouldBe` []

        it "begins no canonical version-5 name with an inadmissible or \
           \repeated two-consonant onset" $ do
            -- The same word-initial scoping the version-2 sweep above
            -- uses, re-run over the widened inventories: an extended
            -- consonant is subject to #1094's relation exactly as an
            -- ASCII one is, including at the capitalized initial.
            let checked =
                    [ (profSeed p, w, a, b)
                    | p ← v5Profiles
                    , Right w ← renderingsFor p
                    , (a, b) ← wordInitialOnsets p w ]
                offenders =
                    [ x
                    | x@(sd, _, a, b) ← checked
                    , p ← [buildProfileV5 sd]
                    , a ≡ b ∨ not (admissibleOnset p a b) ]
            checked `shouldSatisfy` not ∘ null
            checked `shouldSatisfy`
                any (\(_, _, a, b) → isExtendedLetter a ∨ isExtendedLetter b)
            offenders `shouldBe` []

        it "detects a triple of a marked letter the same as an ASCII one" $ do
            -- Pins the widened predicate directly, so the zero above is
            -- evidence the guarantee holds rather than evidence the
            -- detector cannot see a violation.
            hasTripleRun "\x00E1\x00E1\x00E1" `shouldBe` True
            hasTripleRun "\x00C1\x00E1\x00E1" `shouldBe` True
            hasTripleRun "a\x00E1\x00E1" `shouldBe` False
            hasTripleRun "\x00E1-\x00E1\x00E1" `shouldBe` False

    describe "capitalization covers extended initials (#1100 requirement 5)" $ do
        it "pairs every repertoire member with the uppercase rendering \
           \actually produces" $
            -- Language.Generated.Render.capitalizeWord uses toUpper, so
            -- the table is only the authority if the two agree. A
            -- member whose simple uppercase were itself, or a different
            -- character, would render an uncapitalized or unlisted
            -- initial.
            forM_ extendedLetterTable $ \(_, _, _, lo, up) → do
                toUpper lo `shouldBe` up
                extendedUppercaseOf lo `shouldBe` Just up
                up `shouldSatisfy` (≢ lo)
                lo `shouldSatisfy` (`elem` outputInventory)
                up `shouldSatisfy` (`elem` outputInventory)

        it "capitalizes a real name whose root starts with a marked \
           \letter" $ do
            let initials = [ (profSeed p, w)
                           | (p, lr) ← v5Assignments
                           , (_, e) ← canonicalExpressions
                           , Right w ← [renderNative p lr e]
                           , Just (c, _) ← [T.uncons w]
                           , isExtendedLetter c ]
                lowercased = [ x | x@(_, w) ← initials
                             , Just (c, _) ← [T.uncons w]
                             , c `elem` extendedLetters ]
            initials `shouldSatisfy` not ∘ null
            lowercased `shouldBe` []

    describe "the output contract over the widened repertoire (#1100 requirements 4, 6)" $ do
        it "holds for every canonical version-5 name" $ do
            let allRenderings = concatMap nativeRenderingsV5 [0 .. 40]
                texts = [ w | Right w ← allRenderings ]
            length texts `shouldBe` length allRenderings
            texts `shouldSatisfy` any (T.any isExtendedLetter)
            filter (not ∘ contractOk) texts `shouldBe` []

        it "admits nothing outside the canonical output inventory" $ do
            let stray = [ (profSeed p, w, c)
                        | (p, lr) ← v5Assignments
                        , (_, e) ← canonicalExpressions
                        , Right w ← [renderNative p lr e]
                        , c ← T.unpack w
                        , c `notElem` outputInventory ]
            stray `shouldBe` []

        it "counts length in code points, not bytes" $ do
            -- #1100 requirement 6. Text.length is already code points;
            -- this pins that the 3-32 contract is being read that way,
            -- by exhibiting a name whose UTF-8 encoding is longer than
            -- its length.
            let wide = [ w | (p, lr) ← v5Assignments
                       , (_, e) ← canonicalExpressions
                       , Right w ← [renderNative p lr e]
                       , T.any isExtendedLetter w ]
            wide `shouldSatisfy` not ∘ null
            forM_ wide $ \w → do
                T.length w `shouldSatisfy` (≤ 32)
                T.length w `shouldSatisfy` (≥ 3)
                BS.length (encodeUtf8 w) `shouldSatisfy` (> T.length w)

        it "accepts and rejects exactly what the report tool's regex \
           \does" $ do
            -- The predicate and @tools/language_report.py@'s
            -- CONTRACT_RE are two statements of ONE contract, so the
            -- cases are mirrored verbatim in that tool's --self-test.
            -- Without the negative half a weaker predicate reports
            -- "zero contract violations" for output the enforced regex
            -- would reject.
            let accepted =
                    [ "Kara", "Kara'b", "Kara-bo", "Kar"
                    , "K\x00E1r\x00F3", "\x00C1r\x00F3-b\x00E1"
                    , "\x00D8ka", "Ka\x00F8-r\x00E1'b" ]
                rejected =
                    [ ("lowercase initial",            "kara")
                    , ("lowercase extended initial",   "\x00E1ra")
                    , ("below the 3-character floor",  "Ka")
                    , ("uppercase in the interior",    "KAra")
                    , ("uppercase extended interior",  "K\x00C1ra")
                    , ("repeated hyphen",              "Kara--bo")
                    , ("repeated apostrophe",          "Kara''bo")
                    , ("hyphen then apostrophe",       "K-'ara")
                    , ("apostrophe then hyphen",       "K'-ara")
                    , ("leading mark",                 "-Kara")
                    , ("leading extended mark",        "-K\x00E1ra")
                    , ("trailing mark",                "Kara-")
                    , ("trailing extended mark",       "K\x00E1r\x00E1-")
                    , ("a digit",                      "Kar3")
                    , ("a letter outside the set",     "Kar\x00E6")
                    , ("a curly quote for the mark",   "Kara\x2019\&b")
                    -- A combining sequence renders identically to the
                    -- accepted precomposed letter and must still be
                    -- rejected: the repertoire is single code points.
                    , ("a combining mark",             "A\x0301ra\x0301")
                    -- Python's `$` matches before a trailing
                    -- newline, so the report tool's regex needs
                    -- `fullmatch` to agree with this predicate.
                    , ("a trailing newline",           "Kara\n")
                    , ("a trailing carriage return",   "Kara\r")
                    , ("an embedded newline",          "Ka\nra")
                    , ("empty",                        "") ]
            filter (not ∘ contractOk) accepted `shouldBe` []
            [ label | (label, w) ← rejected, contractOk w ] `shouldBe` []

        it "describes one canonical set, shared with the report tool" $ do
            -- The single explicit inventory the reviewed spec asks for:
            -- ASCII in both cases, the repertoire in both cases, and the
            -- two marks — sorted, deduplicated, nothing else.
            length outputInventory `shouldBe` 26 * 2 + 61 * 2 + 2
            outputInventory `shouldBe` sort (nub outputInventory)
            length extendedLetterTable `shouldBe` 61
            length extendedLetters `shouldBe` 61
            nameMarks `shouldBe` ['\'', '-']
            filter (not ∘ isNameLetter) outputInventory `shouldBe` nameMarks

    -- #1100 requirement 3: the font decision, proved rather than
    -- asserted. The atlases below are generated from the shipped .ttf
    -- files by the production path, so this is real rasterizable
    -- coverage — not membership in a repertoire list, which is only a
    -- REQUEST, and not membership in the glyph map alone, which #1098's
    -- own tests show can hold for a character that draws nothing.
    describe "every generated-name font supplies the whole repertoire (#1100 requirement 3)" $ do
        it "names the fonts the decision covers, and excludes the title \
           \font" $ do
            generatedNameFonts `shouldBe`
                ["assets/fonts/arcade.ttf", "assets/fonts/shell.ttf"]
            gothicFontPath `shouldSatisfy` (`notElem` generatedNameFonts)

        forM_ generatedNameFonts $ \path → do
            atlas ← runIO (nameFontAtlas path)
            it ("draws every output character in " ⧺ path) $ do
                let absent = [ c | c ← outputInventory, not (drawsGlyph atlas c) ]
                -- The sweep is over the COMPLETE possible output set —
                -- lowercase repertoire, uppercase initials, ASCII
                -- letters and both marks — not the characters some
                -- 1,280-name sample happened to produce.
                length outputInventory `shouldBe` 176
                absent `shouldBe` []

            it ("lays out real marked names in " ⧺ path) $ do
                -- The acceptance criteria's manual step, mechanised.
                -- Generated names reach no UI surface yet (#708's
                -- Phase 2), so there is nothing to photograph; what can
                -- be checked is that real names out of the real
                -- generator measure in a real atlas at the sizes the
                -- game loads these fonts at. The no-fallback sweep
                -- above is what makes a positive width mean something:
                -- the #1097 mark carries an advance of its own, so
                -- width alone would not distinguish a drawn glyph from
                -- a visible substitute.
                let marked = [ w | s ← [0 .. 63 ∷ Word64]
                             , Right w ← nativeRenderingsV5 s
                             , T.any isExtendedLetter w ]
                marked `shouldSatisfy` ((≥ 50) ∘ length)
                forM_ marked $ \w → forM_ [24, 48 ∷ Float] $ \sz →
                    calculateTextWidthScaled atlas sz (T.unpack w)
                        `shouldSatisfy` (> 0)

        gothic ← runIO (nameFontAtlas gothicFontPath)
        it "would fail for the title font, which is why it is excluded" $ do
            -- Without this the group could pass by accident on a
            -- repertoire every shipped font supplies, and the decision
            -- above would be recording a choice that never mattered.
            let absent = [ c | c ← outputInventory, not (drawsGlyph gothic c) ]
            absent `shouldSatisfy` not ∘ null
            gothicFontPath `shouldSatisfy` (`notElem` generatedNameFonts)

    -- Version 5's own golden block, added ALONGSIDE the four above
    -- rather than replacing any of them (#1092 requirement 4): every
    -- historical version stays constructible, so their pins must keep
    -- passing unchanged while this version's names gain their marks.
    describe "golden outputs (pinned, generator version 5)" $ do
        it "seed 0" $ nativeRenderingsV5 0 `shouldBe`
            [ Right "Ovnisij", Right "Soteskebobo", Right "Ocicycjev"
            , Right "Inokohysesoce", Right "Rerojonivfov" ]

        it "seed 1" $ nativeRenderingsV5 1 `shouldBe`
            [ Right "Fuh", Right "Gaf-f\x00E4k", Right "Uz\x00E4g-k\x00E4h"
            , Right "\x00C4\&ftap-puzn\x00E4", Right "Kanab-\x00E4kyg" ]

        it "seed 42" $ nativeRenderingsV5 42 `shouldBe`
            [ Right "Tobra", Right "Kivk\x0105-kogo", Right "Vyibey-kyogi"
            , Right "Gtibk\x0105r-tekva", Right "Bavvko-bgove'b" ]

        it "seed 12345" $ nativeRenderingsV5 12345 `shouldBe`
            [ Right "Pakur", Right "H\x00E0hg\x00F9ypuhuhugikkip"
            , Right "R\x00F9y\x00E0r\x00E0hargakgu"
            , Right "Yupupugug\x00E0gg\x00E0ry\x00E0k", Right "Pak\x00F9h\x00F9hkipr" ]

        -- The one golden that shows the feature: three languages of the
        -- same sample, one unmarked and two carrying different families,
        -- with the inventories their names are drawn from.
        it "seed 0 is unmarked while seeds 1 and 42 carry their own \
           \families" $ do
            let p0 = buildProfileV5 (LangSeed 0)
                p1 = buildProfileV5 (LangSeed 1)
                p42 = buildProfileV5 (LangSeed 42)
            (profileExtendedChars p0, profileDiacritic p0)
                `shouldBe` ("", Nothing)
            (profileExtendedChars p1, profileDiacritic p1)
                `shouldBe` ("\x00E4", Just DiaDiaeresis)
            (profVowels p1, profConsonants p1)
                `shouldBe` ("uya\x00E4", "pbnhgzfkst")
            (profileExtendedChars p42, profileDiacritic p42)
                `shouldBe` ("\x0105", Just DiaOgonek)
            (profVowels p42, profConsonants p42)
                `shouldBe` ("oaei\x0105", "gkyvbtr")

  where
    isRight' (Right _) = True
    isRight' (Left _)  = False

    isLeft' (Right _) = False
    isLeft' (Left _)  = True
