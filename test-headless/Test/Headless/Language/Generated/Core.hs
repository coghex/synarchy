-- | The core generated-language contracts (#710): version-1 profile
--   generation, concept-root assignment and #1868 placement-order
--   stability, native rendering and its failure behaviour,
--   grammatical marking and compound order, the base output
--   contract, and the pinned deterministic observations.
module Test.Headless.Language.Generated.Core
    ( spec
    ) where

import UPrelude
import Test.Hspec
import Data.List (nub, sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Semantic.Types
import Language.Generated.Types
import Language.Generated.Profile
import Language.Generated.Root
import Language.Generated.Render
import Language.Generated.Signature
import Language.Generated.Report (countDuplicateRoots)
import Test.Headless.Language.Generated.Support

spec ∷ Ctx → Spec
spec Ctx{..} = do
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

-- | A rendering succeeded.
isRight' ∷ Either α β → Bool
isRight' (Right _) = True
isRight' (Left _)  = False
