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
import Data.List (nub)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Language.Semantic.Types
import Language.Semantic.Catalogue
import Language.Generated.Types
import Language.Generated.Profile
import Language.Generated.Root
import Language.Generated.Render
import Language.Generated.Signature
import Language.Generated.Report (canonicalExpressions, countDuplicateRoots)

cid ∷ Text → ConceptId
cid = ConceptId

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
        nativeRenderings ∷ Word64 → [Either Text Text]
        nativeRenderings sd =
            let prof  = buildProfileV1 (LangSeed sd)
                roots = assignRoots prof (conceptIds prodCat)
            in [ either (Left ∘ nativeRenderErrorText) Right
                        (renderNative prof roots expr)
               | (_, expr) ← canonicalExpressions ]

    describe "profile generation (requirements 1, 2, 12)" $ do
        it "is deterministic for the same version and seed" $
            buildProfileV1 (LangSeed 7) `shouldBe` buildProfileV1 (LangSeed 7)

        it "accepts version 1" $
            case generateProfile (GeneratorVersion 1) (LangSeed 7) of
                Right _ → pure ()
                Left e  → expectationFailure (T.unpack (generatorErrorText e))

        it "rejects an unsupported version descriptively rather than falling back to version 1" $ do
            let r = generateProfile (GeneratorVersion 2) (LangSeed 7)
            r `shouldBe` Left (UnsupportedGeneratorVersion 2)
            case r of
                Left e  → generatorErrorText e `shouldSatisfy` T.isInfixOf "2"
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

    -- Golden outputs (#710 requirement 15): a change to any of these
    -- pins requires incrementing the language-generator version rather
    -- than silently changing version 1's output.
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
  where
    isRight' (Right _) = True
    isRight' (Left _)  = False
