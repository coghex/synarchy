-- | Generated-language rendering (#710): deterministic profile
--   generation, concept-root assignment/collision resolution, and
--   native-name rendering over #709's semantic proper names. Mirrors
--   'Test.Headless.Language.Semantic''s shape — the production concept
--   catalogue read straight from @data/language/concepts.yaml@, pinned
--   golden outputs, and no engine/Lua/random state anywhere.
module Test.Headless.Language.Generated (spec) where

import UPrelude
import Test.Hspec
import Data.Char (isAsciiUpper, isAsciiLower)
import Data.List (nub, sort)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Semantic.Types
import Language.Semantic.Catalogue
import Language.Generated.Types
import Language.Generated.Onset
import Language.Generated.Boundary
import Language.Generated.Profile
import Language.Generated.Root
import Language.Generated.Render
import Language.Generated.Signature
import Language.Generated.Report (canonicalExpressions, countDuplicateRoots)

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
        (c : cs) → toLowerAscii c : cs
        []       → []
    toLowerAscii c
        | isAsciiUpper c = toEnum (fromEnum c + 32)
        | otherwise      = c

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

-- | The 3-32/ASCII/capitalization/punctuation contract every rendered
--   native word must satisfy (#710 requirement 6), pinned as a concrete
--   predicate rather than left implicit.
contractOk ∷ Text → Bool
contractOk w =
    T.length w ≥ 3 ∧ T.length w ≤ 32
    ∧ startsUpperLetter ∧ endsLetter
    ∧ T.all okChar w
    ∧ not ("--" `T.isInfixOf` w)
    ∧ not ("''" `T.isInfixOf` w)
  where
    startsUpperLetter = case T.uncons w of
        Just (c, _) → isAsciiUpper c
        Nothing     → False
    endsLetter = case T.unsnoc w of
        Just (_, c) → isAsciiUpper c ∨ isAsciiLower c
        Nothing     → False
    okChar c = isAsciiUpper c ∨ isAsciiLower c ∨ c ≡ '\'' ∨ c ≡ '-'

spec ∷ Spec
spec = describe "Generated language names" $ do
    prodBytes ← runIO $ BS.readFile conceptCataloguePath
    let prodCat = either (error ∘ T.unpack ∘ catalogueErrorText) id
                         (parseCatalogue prodBytes)

        -- Every canonical expression's native rendering for one seed,
        -- assigning roots over the COMPLETE production catalogue (#710
        -- requirement 8's "unique within one language across the
        -- complete production concept catalogue" is only meaningful
        -- when tested against the real thing, not a hand-picked
        -- fixture list — the catalogue is read at test time, so this
        -- keeps covering it as #713 grows it further).
        renderingsFor ∷ Profile → [Either Text Text]
        renderingsFor prof =
            let roots = assignRoots prof (conceptIds prodCat)
            in [ either (Left ∘ nativeRenderErrorText) Right
                        (renderNative prof roots expr)
               | (_, expr) ← canonicalExpressions ]

        nativeRenderings ∷ Word64 → [Either Text Text]
        nativeRenderings = renderingsFor ∘ buildProfileV1 ∘ LangSeed

        nativeRenderingsV2 ∷ Word64 → [Either Text Text]
        nativeRenderingsV2 = renderingsFor ∘ buildProfileV2 ∘ LangSeed

        nativeRenderingsV3 ∷ Word64 → [Either Text Text]
        nativeRenderingsV3 = renderingsFor ∘ buildProfileV3 ∘ LangSeed

        v3Profiles ∷ [Profile]
        v3Profiles = [ buildProfileV3 (LangSeed s) | s ← [0 .. 255] ]

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
        it "is deterministic for the same version and seed" $
            buildProfileV1 (LangSeed 7) `shouldBe` buildProfileV1 (LangSeed 7)

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

        -- Version 4 stands in for "never existed". This test used to
        -- name version 2 and then version 3, which #1094 and #1095 made
        -- real — a rejection test must always point at a version outside
        -- the supported set, never at the next one about to be
        -- implemented.
        it "rejects an unsupported version descriptively rather than falling back to version 1" $ do
            let r = generateProfile (GeneratorVersion 4) (LangSeed 7)
            r `shouldBe` Left (UnsupportedGeneratorVersion 4)
            case r of
                Left e  → generatorErrorText e `shouldSatisfy` T.isInfixOf "4"
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

        it "version 3 is the current generator, with versions 1 and 2 \
           \still constructible beside it" $ do
            currentGeneratorVersion `shouldBe` GeneratorVersion 3
            sort (map generatorVersionInt supportedGeneratorVersions)
                `shouldBe` [1, 2, 3]

        it "repeated construction of the same (version, seed) is \
           \byte-identical, for every supported version" $
            forM_ supportedGeneratorVersions $ \ver → do
                generateProfile ver (LangSeed 4242)
                    `shouldBe` generateProfile ver (LangSeed 4242)
                case generateProfile ver (LangSeed 4242) of
                    Left e  → expectationFailure (T.unpack (generatorErrorText e))
                    Right p → renderingsFor p `shouldBe` renderingsFor p

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

    describe "concept roots (requirements 7, 8, 16)" $ do
        it "assigns the same roots regardless of the input list's order" $ do
            let prof = buildProfileV1 (LangSeed 99)
                ids  = conceptIds prodCat
            assignRoots prof ids `shouldBe` assignRoots prof (reverse ids)

        it "has zero root collisions over the complete production catalogue, for several seeds" $ do
            let seeds = [0, 1, 7, 42, 99, 12345, 999999]
                collisionsFor sd =
                    countDuplicateRoots (assignRoots (buildProfileV1 (LangSeed sd))
                                                      (conceptIds prodCat))
            map collisionsFor seeds `shouldBe` map (const 0) seeds

    describe "every #709 name form renders natively (requirement 10)" $ do
        let prof  = buildProfileV1 (LangSeed 7)
            roots = assignRoots prof (conceptIds prodCat)
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
                roots = assignRoots prof (conceptIds prodCat)
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
        it "rendering the same profile and expression twice is byte-identical" $ do
            let prof  = buildProfileV1 (LangSeed 321)
                roots = assignRoots prof (conceptIds prodCat)
                expr  = Modifier (cid "ASH") (cid "LAND")
            renderNative prof roots expr `shouldBe` renderNative prof roots expr

        it "profile signatures are stable for the same profile" $
            profileSignature (buildProfileV1 (LangSeed 321))
                `shouldBe` profileSignature (buildProfileV1 (LangSeed 321))

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
                        , Right w ← [renderNative p (assignRoots p (conceptIds prodCat))
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
        it "the same seed and version render byte-identically" $ do
            forM_ [0, 1, 42, 12345 ∷ Word64] $ \s → do
                buildProfileV3 (LangSeed s) `shouldBe` buildProfileV3 (LangSeed s)
                nativeRenderingsV3 s `shouldBe` nativeRenderingsV3 s

        it "a repair is a pure function of (profile, the two pieces)" $ do
            let prof = buildProfileV3 (LangSeed 9)
            joinMorphemes prof "hoh" "h" `shouldBe` joinMorphemes prof "hoh" "h"
            joinSyllables prof "abb" "ba" `shouldBe` joinSyllables prof "abb" "ba"

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
            [ Right "Tyh", Right "Fyn-ytap", Right "Azpat-put"
            , Right "Byg-angyzny", Right "Ubpugub-yftyk" ]

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
  where
    isRight' (Right _) = True
    isRight' (Left _)  = False
