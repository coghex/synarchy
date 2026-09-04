-- | The flora schema's CLOSED VOCABULARIES, gated at the authoring
--   boundary (#2315).
--
--   Three vocabularies — lifecycle, life phase and annual stage — are
--   authored at five distinct positions: @lifecycle@, @phases[].tag@,
--   @annualCycle[].tag@, and the @cycleOverrides[].phase@ /
--   @cycleOverrides[].cycle@ pair that reuse the phase and stage
--   vocabularies. Every one of those positions used to decode
--   as unrestricted 'Text' and be resolved at REGISTRATION, where an
--   unrecognized @lifecycle@ became 'Evergreen' and an unrecognized
--   phase, stage or override was dropped without a word.
--
--   A dropped token is not a cosmetic loss.
--   'World.Flora.Growth.harvestOpen' gates the seasonal harvest window
--   on the species declaring a @fruiting@ stage; misspell it and the
--   stage is gone, @hasFruiting@ is 'False', and the species falls into
--   the documented "no fruiting stage → open year-round" branch. The
--   seasonal gate is silently off. A misspelled @lifecycle@ is the same
--   defect pointed the other way: an annual crop becomes an evergreen.
--
--   The fix is at the AUTHORING boundary and nowhere else (requirement
--   6), so this spec gates the DECODER and there is deliberately no
--   consumer fallback left to test. It mirrors the three halves
--   "Test.Headless.Asset.FloraRegrowthSchema" established for the
--   sibling field one over (#1711):
--
--     * __rejection__ — every one of the five authored positions
--       separately, plus the two undeclared-override cases, each
--       asserted through the real 'loadFloraYaml' on a real file,
--       because whole-FILE rejection (the 'Engine.Asset.YamlList'
--       contract) is half of what is under test: the loader must hand
--       back @[]@ AND warn.
--     * __the diagnostic__ — the warning names the FILE (from
--       'Engine.Asset.YamlList.loadYamlList'), the SPECIES, the authored
--       PATH, the KEY and the offending token, and for a membership
--       failure the declared set it failed against. @tag@ alone would
--       not tell an author which of the two lists to open.
--     * __the shipped corpus__ — all eight @data\/flora\/*.yaml@ files
--       still load, with every lifecycle, phase, annual-cycle stage and
--       override pair unchanged.
--
--   One half is not reachable from the pure decoder: that a refused
--   file leaves the ENGINE untouched and answers the Lua binding's
--   opt-in outcome with @parsed == false@. That gets a real headless
--   engine, exactly as #2241's atomicity coverage does.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Asset.FloraVocabularySchema"'@.
module Test.Headless.Asset.FloraVocabularySchema (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import Data.List (sort)
import qualified Data.Text as T
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', writeIORef)
import System.FilePath ((</>))
import Engine.Asset.TextureNameRegistry (lookupTextureName)
import Engine.Asset.YamlFlora
    ( FloraLifecycle(..), FloraYamlDef(..), FloraYamlPhase(..)
    , FloraYamlCycleStage(..), FloraYamlCycleOverride(..)
    , annualStageVocabulary, lifePhaseVocabulary, lifecycleText
    , lifecycleVocabulary, loadFloraYaml, parseCycleTag, parseLifecycleTag
    , parsePhaseTag )
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.Queue (QueueStats(..), queueStats)
import Engine.Core.State
    ( EngineEnv, floraCatalogRef, loggerRef, luaToEngineQueue, luaQueue
    , assetPoolRef, nextObjectIdRef, inputStateRef )
import Engine.Core.Thread (ThreadControl(..))
import Engine.Core.Log
    ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
    , LogCategory(..), LogLevel(..), LogEntry(..), LoggerState )
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness.Isolation
    (withExclusiveTempDirectory, withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import World.Flora.Growth (annualStageText, lifePhaseText)
import World.Flora.Types
    ( AnnualStageTag(..), FloraCatalog(..), FloraSpecies(..)
    , LifePhaseTag(..), findSpeciesByName )

-- * Fixtures
--
--   One authored species with every vocabulary position exposed as a
--   field, so a rejection test perturbs exactly ONE token and nothing
--   else can be what failed. The declared sets are deliberately PARTIAL
--   — two phases of nine and two stages of five — so a recognized token
--   this species does not declare is available to test requirement 3
--   with.

data Fixture = Fixture
    { fxName      ∷ String
    , fxLifecycle ∷ Maybe String        -- ^ 'Nothing' omits the key
    , fxPhases    ∷ [(String, String)]  -- ^ (tag, texture)
    , fxCycle     ∷ [(String, Int)]     -- ^ (tag, startDay)
    , fxOverrides ∷ [(String, String)]  -- ^ (phase, cycle)
    }

-- | The species every rejection assertion names, valid as it stands.
probe ∷ Fixture
probe = Fixture
    { fxName      = "probe_vocab"
    , fxLifecycle = Just "perennial"
    , fxPhases    = [("sprout", "sprout.png"), ("matured", "matured.png")]
    , fxCycle     = [("dormant", 0), ("fruiting", 180)]
    , fxOverrides = [("sprout", "dormant")]
    }

-- | A second, entirely VALID species — the witness for whole-file
--   rejection. A per-entry skip would leave this one registered.
sibling ∷ Fixture
sibling = probe { fxName = "probe_sound" }

renderFixture ∷ Fixture → String
renderFixture fx = unlines $
    [ "  - name: \"" ⧺ fxName fx ⧺ "\""
    , "    type: \"shrub\""
    , "    texDir: \"assets/textures/flora/probe\""
    ]
    ⧺ [ "    lifecycle: " ⧺ lc | Just lc ← [fxLifecycle fx] ]
    ⧺ [ "    phases:" | not (null (fxPhases fx)) ]
    ⧺ [ "      - {tag: " ⧺ tag ⧺ ", texture: \"" ⧺ tex ⧺ "\", age: 0}"
      | (tag, tex) ← fxPhases fx ]
    ⧺ [ "    annualCycle:" | not (null (fxCycle fx)) ]
    ⧺ [ "      - {tag: " ⧺ tag ⧺ ", startDay: " ⧺ show day
        ⧺ ", texture: \"stage.png\"}"
      | (tag, day) ← fxCycle fx ]
    ⧺ [ "    cycleOverrides:" | not (null (fxOverrides fx)) ]
    ⧺ [ "      - {phase: " ⧺ p ⧺ ", cycle: " ⧺ c
        ⧺ ", texture: \"override.png\"}"
      | (p, c) ← fxOverrides fx ]
    ⧺ [ "    worldGen:"
      , "      category: shrub"
      , "      minTemp: -10"
      , "      maxTemp: 40"
      , "      idealTemp: 15"
      , "      minPrecip: 0.1"
      , "      maxPrecip: 3.0"
      , "      idealPrecip: 1.0"
      ]

floraFile ∷ [Fixture] → String
floraFile fxs = unlines ("flora:" : map renderFixture fxs)

-- * Assertions

-- | Load @src@ through the REAL loader and require the established
--   whole-file rejection: an empty list plus exactly one 'CatAsset'
--   'LevelWarn' naming the file, the species and every token in
--   @tokens@.
--
--   Tokens are matched as whole WORDS of a punctuation-scrubbed
--   message, never substrings, so @dormant@ cannot be satisfied by a
--   message that only ever says @dormancy@. The scrub leaves @.@, @-@
--   and @[]@ alone: they are inside the authored paths
--   (@annualCycle[].tag@) and values the tokens have to match.
rejectsNaming ∷ [String] → String → Expectation
rejectsNaming tokens src =
    withFloraFixture src $ \path → do
        (logger, entriesRef) ← callbackLogger
        defs ← loadFloraYaml logger path
        map fydName defs `shouldBe` []
        entries ← readIORef entriesRef
        case entries of
            [entry] → do
                leLevel entry `shouldBe` LevelWarn
                leCategory entry `shouldBe` CatAsset
                let msg     = T.unpack (leMessage entry)
                    ws      = words (map scrub msg)
                    wanted  = path : "probe_vocab" : tokens
                    missing = [t | t ← wanted, t `notElem` ws]
                if null missing
                  then pure ()
                  else expectationFailure $
                      "rejected, but the warning does not name "
                      ⧺ show missing ⧺ ": " ⧺ msg
            other → expectationFailure $
                "expected exactly one captured log entry, got "
                ⧺ show (length other)
  where
    scrub c = if c `elem` ("'\"(),:;=\8212" ∷ String) then ' ' else c

-- | Load @src@ and hand the decoded definitions to @check@.
accepts ∷ ([FloraYamlDef] → Expectation) → String → Expectation
accepts check src =
    withFloraFixture src $ \path → do
        (logger, _) ← callbackLogger
        defs ← loadFloraYaml logger path
        check defs

spec ∷ Spec
spec = do
    describe "unrecognized tokens (requirement 1)" $ do

        it "rejects an unrecognized lifecycle instead of quietly \
           \answering it with evergreen — the substitution that turns \
           \an annual crop into a perennial nothing ever kills" $
            rejectsNaming
                ["lifecycle", "purennial", "evergreen", "perennial"
                , "annual", "biennial"]
                (floraFile [probe { fxLifecycle = Just "purennial" }])

        it "rejects an unrecognized phases[].tag, naming the list \
           \rather than the ambiguous leaf key" $
            rejectsNaming ["phases[].tag", "tag", "maturd", "matured"]
                (floraFile [probe { fxPhases = [("maturd", "matured.png")] }])

        it "rejects an unrecognized annualCycle[].tag — the case that \
           \silently disables the seasonal harvest window, because a \
           \species with no fruiting stage is harvestable year-round" $
            rejectsNaming ["annualCycle[].tag", "tag", "fruting", "fruiting"]
                (floraFile [probe { fxCycle = [("dormant", 0)
                                              , ("fruting", 180)] }])

        it "rejects an unrecognized cycleOverrides[].phase" $
            rejectsNaming ["cycleOverrides[].phase", "phase", "sprouut"]
                (floraFile [probe { fxOverrides = [("sprouut", "dormant")] }])

        it "rejects an unrecognized cycleOverrides[].cycle" $
            rejectsNaming ["cycleOverrides[].cycle", "cycle", "dormnt"]
                (floraFile [probe { fxOverrides = [("sprout", "dormnt")] }])

        it "rejects a phases[].tag that is a number rather than a token" $
            rejectsNaming ["phases[].tag", "tag"]
                (floraFile [probe { fxPhases = [("7", "matured.png")] }])

    describe "present versus absent (requirement 2)" $ do

        it "keeps the documented default when lifecycle is OMITTED — \
           \only a present value is checked" $
            accepts (\defs → map fydLifecycle defs
                        `shouldBe` [LifecycleEvergreen])
                (floraFile [probe { fxLifecycle = Nothing }])

        it "rejects an authored lifecycle: null, which aeson's .:? \
           \would have read as absent and defaulted to evergreen" $
            rejectsNaming ["lifecycle", "null"]
                (floraFile [probe { fxLifecycle = Just "null" }])

        it "still accepts every authored lifecycle spelling" $
            forM_ [ ("evergreen", LifecycleEvergreen)
                  , ("perennial", LifecyclePerennial)
                  , ("annual",    LifecycleAnnual)
                  , ("biennial",  LifecycleBiennial) ] $ \(token, want) →
                accepts (\defs → map fydLifecycle defs `shouldBe` [want])
                    (floraFile [probe { fxLifecycle = Just token }])

        it "leaves an absent annualCycle and absent cycleOverrides as \
           \empty, exactly as the .:? .!= [] they replace did" $
            accepts (\defs → map (\d → ( length (fydAnnualCycle d)
                                       , length (fydCycleOverrides d) )) defs
                        `shouldBe` [(0, 0)])
                (floraFile [probe { fxCycle = [], fxOverrides = [] }])

    describe "undeclared override references (requirement 3)" $ do

        it "rejects an override naming a well-spelled phase this \
           \species never declares — it would register a texture no \
           \plant could ever select" $
            rejectsNaming
                [ "cycleOverrides[].phase", "phase", "dead", "phases[]"
                , "sprout", "matured" ]
                (floraFile [probe { fxOverrides = [("dead", "dormant")] }])

        it "rejects an override naming a well-spelled annual stage this \
           \species never declares" $
            rejectsNaming
                [ "cycleOverrides[].cycle", "cycle", "flowering"
                , "annualCycle[]", "dormant", "fruiting" ]
                (floraFile [probe { fxOverrides = [("sprout", "flowering")] }])

        it "rejects EVERY override on a species that declares no phases \
           \at all — the empty declared set can select nothing" $
            rejectsNaming ["cycleOverrides[].phase", "sprout", "phases[]"]
                (floraFile [probe { fxPhases = [] }])

        it "accepts an override whose phase and cycle are both declared" $
            accepts (\defs → map (\o → (fycoPhase o, fycoCycle o))
                                 (concatMap fydCycleOverrides defs)
                        `shouldBe` [(PhaseMatured, CycleFruiting)])
                (floraFile [probe { fxOverrides = [("matured", "fruiting")] }])

    describe "whole-file rejection (requirement 4)" $

        it "drops EVERY species in the file, not just the offending \
           \entry — the established Engine.Asset.YamlList contract" $
            -- The malformed species authors NO overrides, so the
            -- misspelled stage tag is the only thing in the file that
            -- can refuse it: with the overrides left in, an override
            -- naming the now-undeclared `dormant` would refuse the file
            -- too, and the example would pass without the tag check
            -- ever running.
            withFloraFixture
                (floraFile [ probe { fxCycle = [("fruting", 180)]
                                   , fxOverrides = [] }
                           , sibling ])
                $ \path → do
                    (logger, _) ← callbackLogger
                    defs ← loadFloraYaml logger path
                    map fydName defs `shouldBe` []

    describe "the advertised vocabularies (requirements 5 and 6)" $ do

        it "advertises exactly the phase tokens parsePhaseTag accepts, \
           \so a diagnostic can never name a spelling the parser would \
           \reject" $ do
            lifePhaseVocabulary
                `shouldBe` map lifePhaseText [minBound .. maxBound]
            map parsePhaseTag lifePhaseVocabulary
                `shouldBe` map Just [minBound .. maxBound ∷ LifePhaseTag]

        it "advertises exactly the annual-stage tokens parseCycleTag \
           \accepts" $ do
            annualStageVocabulary
                `shouldBe` map annualStageText [minBound .. maxBound]
            map parseCycleTag annualStageVocabulary
                `shouldBe` map Just [minBound .. maxBound ∷ AnnualStageTag]

        it "advertises exactly the lifecycle tokens parseLifecycleTag \
           \accepts" $ do
            lifecycleVocabulary
                `shouldBe` map lifecycleText [minBound .. maxBound]
            map parseLifecycleTag lifecycleVocabulary
                `shouldBe` map Just [minBound .. maxBound ∷ FloraLifecycle]

    describe "the refusal reaches the Lua binding (requirement 4)" $ do

        it "answers engine.loadFloraYaml with zero textures and \
           \parsed == false, leaving the catalog, the id allocator, the \
           \texture registry and the load queue untouched" $
            withFloraEngine $ \eng → do
            -- parsed == false is the half that distinguishes THIS
            -- refusal from #2241's duplicate-name one, which decodes
            -- fine and is refused afterwards. Both end at zero
            -- textures, so a count-only assertion would not tell them
            -- apart.
            before ← snapshotFlora eng
            withFloraFixture
                (floraFile [ probe { fxName = "probe_vocab_bad"
                                   , fxLifecycle = Just "purennial" }
                           , sibling { fxName = "probe_vocab_sound" } ])
                $ \path → do
                    (count, parsed, refusal) ←
                        loadFloraOutcome eng (T.pack path)
                    (count, parsed) `shouldBe` ("0", "false")
                    refusal `shouldBe` "nil"
                    snapshotFlora eng `shouldReturn` before
                    cat ← readIORef (floraCatalogRef (feEnv eng))
                    findSpeciesByName "probe_vocab_bad" cat
                        `shouldBe` Nothing
                    -- and specifically: the VALID species behind the
                    -- malformed one registered nothing either.
                    findSpeciesByName "probe_vocab_sound" cat
                        `shouldBe` Nothing
                    nameRegistered eng "flora_base_probe_vocab_sound"
                        `shouldReturn` False

        it "registers the same file once its token is spelled \
           \correctly, so the refusal is about the token and not the \
           \fixture" $ withFloraEngine $ \eng →
            withFloraFixture
                (floraFile [ probe { fxName = "probe_vocab_fixed" }
                           , sibling { fxName = "probe_vocab_fixed2" } ])
                $ \path → do
                    (count, parsed, refusal) ←
                        loadFloraOutcome eng (T.pack path)
                    parsed `shouldBe` "true"
                    refusal `shouldBe` "nil"
                    count `shouldNotBe` "0"
                    cat ← readIORef (floraCatalogRef (feEnv eng))
                    (fsName ∘ snd <$>
                        findSpeciesByName "probe_vocab_fixed" cat)
                        `shouldBe` Just "probe_vocab_fixed"
                    -- The registered texture NAMES are the other half:
                    -- registration no longer has the authored token to
                    -- build them from and renders the parsed tag back
                    -- through lifePhaseText / annualStageText instead.
                    -- Those have to be the parsers' exact inverses, or
                    -- every flora texture silently changes name.
                    forM_ [ "flora_base_probe_vocab_fixed"
                          , "flora_phase_probe_vocab_fixed_sprout"
                          , "flora_phase_probe_vocab_fixed_matured"
                          , "flora_cycle_probe_vocab_fixed_dormant"
                          , "flora_cycle_probe_vocab_fixed_fruiting"
                          , "flora_ov_probe_vocab_fixed_sprout_dormant"
                          ] $ \texName →
                        nameRegistered eng texName `shouldReturn` True

    describe "the shipped corpus (requirement 7)" $ do

        it "every data/flora/*.yaml still loads, with its lifecycles, \
           \phases, annual-cycle stages and overrides unchanged" $ do
            (logger, _) ← callbackLogger
            loaded ← forM shippedFlora $ \(file, want) → do
                defs ← loadFloraYaml logger ("data/flora" </> file)
                pure (file, map shippedOf defs, want)
            forM_ loaded $ \(file, got, want) →
                (file, got) `shouldBe` (file, want)

        it "the shipped species and override totals are unchanged" $ do
            (logger, _) ← callbackLogger
            defs ← concat <$> forM shippedFlora (\(file, _) →
                loadFloraYaml logger ("data/flora" </> file))
            length defs `shouldBe` 16
            sum (map (length ∘ fydCycleOverrides) defs) `shouldBe` 83

        it "every shipped override names a phase and an annual stage \
           \its own species declares — the property the decoder now \
           \enforces, checked against the shipped values themselves" $ do
            (logger, _) ← callbackLogger
            defs ← concat <$> forM shippedFlora (\(file, _) →
                loadFloraYaml logger ("data/flora" </> file))
            let stray =
                    [ (fydName d, fycoPhase o, fycoCycle o)
                    | d ← defs, o ← fydCycleOverrides d
                    , fycoPhase o `notElem` map fypTag (fydPhases d)
                      ∨ fycoCycle o `notElem` map fycsTag (fydAnnualCycle d) ]
            stray `shouldBe` []

-- * The shipped baseline

-- | One species as the corpus authors it, pinned by VALUE so a swapped
--   lifecycle or a dropped override is caught and not just a changed
--   count.
data ShippedSpecies = ShippedSpecies
    { ssName       ∷ Text
    , ssLifecycle  ∷ FloraLifecycle
    , ssPhases     ∷ [LifePhaseTag]
    , ssCycle      ∷ [AnnualStageTag]
    , ssOverrides  ∷ [(LifePhaseTag, AnnualStageTag)]
    } deriving (Show, Eq)

shippedOf ∷ FloraYamlDef → ShippedSpecies
shippedOf d = ShippedSpecies
    { ssName      = fydName d
    , ssLifecycle = fydLifecycle d
    , ssPhases    = map fypTag (fydPhases d)
    , ssCycle     = map fycsTag (fydAnnualCycle d)
    , ssOverrides = [ (fycoPhase o, fycoCycle o) | o ← fydCycleOverrides d ]
    }

-- | The shipped baseline this change must not move: all eight
--   @data\/flora\/*.yaml@ files, in file order, and every species'
--   lifecycle, phase tags, annual-cycle tags and override pairs.
shippedFlora ∷ [(FilePath, [ShippedSpecies])]
shippedFlora =
    [ ("boreal_evergreen.yaml",
        [ ShippedSpecies "scots_pine" LifecycleEvergreen
            [PhaseSprout, PhaseMatured]
            []
            []
        , ShippedSpecies "white_spruce" LifecycleEvergreen
            [PhaseSprout, PhaseMatured]
            []
            []
        ])
    , ("crops.yaml",
        [ ShippedSpecies "tomato_plant" LifecycleAnnual
            [PhaseSprout, PhaseMatured, PhaseDead]
            [ CycleDormant, CycleBudding, CycleFlowering, CycleFruiting
            , CycleSenescing ]
            [ (PhaseSprout, CycleDormant), (PhaseSprout, CycleBudding)
            , (PhaseSprout, CycleSenescing), (PhaseDead, CycleDormant)
            , (PhaseDead, CycleBudding), (PhaseDead, CycleFlowering)
            , (PhaseDead, CycleFruiting), (PhaseDead, CycleSenescing)
            ]
        , ShippedSpecies "wheat" LifecycleAnnual
            [PhaseSprout, PhaseVegetating, PhaseDead]
            [CycleDormant, CycleBudding, CycleFlowering, CycleSenescing]
            [ (PhaseDead, CycleDormant), (PhaseDead, CycleBudding)
            , (PhaseDead, CycleFlowering), (PhaseDead, CycleSenescing)
            ]
        ])
    , ("saguaro.yaml",
        [ ShippedSpecies "saguaro" LifecyclePerennial
            [PhaseSprout, PhaseMatured, PhaseDead]
            [ CycleDormant, CycleBudding, CycleFlowering, CycleFruiting
            , CycleSenescing ]
            [ (PhaseSprout, CycleDormant), (PhaseSprout, CycleBudding)
            , (PhaseSprout, CycleFlowering), (PhaseSprout, CycleFruiting)
            , (PhaseSprout, CycleSenescing), (PhaseDead, CycleDormant)
            , (PhaseDead, CycleBudding), (PhaseDead, CycleFlowering)
            , (PhaseDead, CycleFruiting), (PhaseDead, CycleSenescing)
            ]
        ])
    , ("temperate_deciduous.yaml",
        [ ShippedSpecies "white_oak" LifecyclePerennial
            [PhaseSprout, PhaseMatured, PhaseDead]
            [CycleDormant, CycleBudding, CycleFlowering, CycleSenescing]
            [ (PhaseSprout, CycleDormant), (PhaseSprout, CycleBudding)
            , (PhaseSprout, CycleSenescing), (PhaseDead, CycleDormant)
            , (PhaseDead, CycleBudding), (PhaseDead, CycleFlowering)
            , (PhaseDead, CycleSenescing)
            ]
        , ShippedSpecies "paper_birch" LifecyclePerennial
            [PhaseSprout, PhaseMatured, PhaseDead]
            [CycleDormant, CycleBudding, CycleFlowering, CycleSenescing]
            [ (PhaseSprout, CycleDormant), (PhaseSprout, CycleBudding)
            , (PhaseSprout, CycleSenescing), (PhaseDead, CycleDormant)
            , (PhaseDead, CycleBudding), (PhaseDead, CycleFlowering)
            , (PhaseDead, CycleSenescing)
            ]
        , ShippedSpecies "weeping_willow" LifecyclePerennial
            [PhaseSprout, PhaseMatured, PhaseDead]
            [CycleDormant, CycleBudding, CycleFlowering, CycleSenescing]
            [ (PhaseSprout, CycleDormant), (PhaseSprout, CycleBudding)
            , (PhaseSprout, CycleSenescing), (PhaseDead, CycleDormant)
            , (PhaseDead, CycleBudding), (PhaseDead, CycleFlowering)
            , (PhaseDead, CycleSenescing)
            ]
        , ShippedSpecies "sugar_maple" LifecyclePerennial
            [PhaseSprout, PhaseMatured, PhaseDead]
            [CycleDormant, CycleBudding, CycleFlowering, CycleSenescing]
            [ (PhaseSprout, CycleDormant), (PhaseSprout, CycleBudding)
            , (PhaseSprout, CycleSenescing), (PhaseDead, CycleDormant)
            , (PhaseDead, CycleBudding), (PhaseDead, CycleFlowering)
            , (PhaseDead, CycleSenescing)
            ]
        ])
    , ("temperate_shrubs.yaml",
        [ ShippedSpecies "bracken_fern" LifecyclePerennial
            [PhaseSprout, PhaseMatured, PhaseDead]
            [CycleDormant, CycleBudding, CycleFlowering, CycleSenescing]
            [ (PhaseSprout, CycleDormant), (PhaseSprout, CycleBudding)
            , (PhaseSprout, CycleSenescing), (PhaseDead, CycleDormant)
            , (PhaseDead, CycleBudding), (PhaseDead, CycleFlowering)
            , (PhaseDead, CycleSenescing)
            ]
        , ShippedSpecies "red_raspberry" LifecyclePerennial
            [PhaseSprout, PhaseMatured, PhaseDead]
            [ CycleDormant, CycleBudding, CycleFlowering, CycleFruiting
            , CycleSenescing ]
            [ (PhaseSprout, CycleDormant), (PhaseSprout, CycleBudding)
            , (PhaseSprout, CycleSenescing), (PhaseDead, CycleDormant)
            , (PhaseDead, CycleBudding), (PhaseDead, CycleFlowering)
            , (PhaseDead, CycleFruiting), (PhaseDead, CycleSenescing)
            ]
        ])
    , ("temperate_wildflowers.yaml",
        [ ShippedSpecies "common_dandelion" LifecyclePerennial
            [PhaseSprout, PhaseVegetating, PhaseDead]
            [CycleDormant, CycleBudding, CycleFlowering, CycleSenescing]
            [ (PhaseDead, CycleDormant), (PhaseDead, CycleBudding)
            , (PhaseDead, CycleFlowering), (PhaseDead, CycleSenescing)
            ]
        , ShippedSpecies "white_clover" LifecyclePerennial
            [PhaseSprout, PhaseVegetating, PhaseDead]
            [CycleDormant, CycleBudding, CycleFlowering, CycleSenescing]
            [ (PhaseDead, CycleDormant), (PhaseDead, CycleBudding)
            , (PhaseDead, CycleFlowering), (PhaseDead, CycleSenescing)
            ]
        ])
    , ("tropical.yaml",
        [ ShippedSpecies "coconut_palm" LifecycleEvergreen
            [PhaseSprout, PhaseMatured]
            []
            []
        , ShippedSpecies "red_mangrove" LifecycleEvergreen
            [PhaseSprout, PhaseMatured]
            []
            []
        ])
    , ("wetlands.yaml",
        [ ShippedSpecies "common_cattail" LifecyclePerennial
            [PhaseSprout, PhaseMatured, PhaseDead]
            [ CycleDormant, CycleBudding, CycleFlowering, CycleFruiting
            , CycleSenescing ]
            [ (PhaseSprout, CycleDormant), (PhaseSprout, CycleBudding)
            , (PhaseSprout, CycleFlowering), (PhaseSprout, CycleFruiting)
            , (PhaseSprout, CycleSenescing), (PhaseDead, CycleDormant)
            , (PhaseDead, CycleBudding), (PhaseDead, CycleFlowering)
            , (PhaseDead, CycleFruiting), (PhaseDead, CycleSenescing)
            ]
        ])
    ]

-- * Harness

-- | A logger whose backend appends every emitted 'LogEntry' to an
--   'IORef'. 'CatAsset' debug logging stays OFF (the default) so a
--   rejection's warning is the only entry captured, which is what lets
--   'rejectsNaming' require exactly one.
callbackLogger ∷ IO (LoggerState, IORef [LogEntry])
callbackLogger = do
    entriesRef ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' entriesRef (e :)) }
    pure (logger, entriesRef)

-- | Write one throwaway flora YAML into a directory this call created,
--   and hand its path to @action@.
--
--   'withExclusiveTempDirectory', never a predictable @\/tmp@ path
--   claimed with @createDirectoryIfMissing@: the suite's rule is that a
--   cleanup routine may only ever delete a directory the SAME call made
--   ('Test.Headless.Harness.Isolation'). A fixed name would adopt — and
--   then recursively remove — a stale root from an interrupted run.
withFloraFixture ∷ String → (FilePath → Expectation) → Expectation
withFloraFixture body action =
    withExclusiveTempDirectory "synarchy-2315-vocab" $ \dir → do
        let path = dir </> "probe.yaml"
        writeFile path body
        action path

-- | A throwaway headless engine with the real Lua API registered.
--
--   PRIVATE per example, and ISOLATED because the boot itself writes
--   @config\/@ (#1357) — both for the reasons
--   "Test.Headless.Asset.FloraContent" records at its own copy of this
--   harness. The scratch root symlinks @data\/@, so a shipped path
--   still resolves.
data FloraEngine = FloraEngine
    { feEnv ∷ EngineEnv
    , feLua ∷ LuaBackendState
    }

withFloraEngine ∷ (FloraEngine → Expectation) → Expectation
withFloraEngine action = withIsolatedResourceRoot $ do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\_ → pure ()) }
    writeIORef (loggerRef env) logger
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    action (FloraEngine env ls)

evalLua ∷ FloraEngine → Text → IO Text
evalLua eng src =
    T.strip ∘ T.filter (≢ '"') <$> executeDebugLua (lbsLuaState (feLua eng)) src

-- | The three values the binding answers with when the caller opts in:
--   count, parse outcome, and #2241's refusal detail (@nil@ when the
--   file was not refused by NAME).
loadFloraOutcome ∷ FloraEngine → Text → IO (Text, Text, Text)
loadFloraOutcome eng path = do
    out ← evalLua eng
        ("local n, parsed, refusal = engine.loadFloraYaml('" <> path
         <> "', true); return string.format('%d|%s|%s', n, \
            \tostring(parsed), tostring(refusal))")
    case T.splitOn "|" out of
        [n, parsed, refusal] → pure (n, parsed, refusal)
        _                    → pure (out, out, out)

-- | Everything a flora registration touches, captured together: a
--   refused file must move none of it.
data FloraSnapshot = FloraSnapshot
    { fsnNextId   ∷ Word16
    , fsnSpecies  ∷ [Text]
    , fsnWorldGen ∷ Int
    , fsnEnqueued ∷ Word64  -- ^ cumulative asset-queue writes
    } deriving (Show, Eq)

snapshotFlora ∷ FloraEngine → IO FloraSnapshot
snapshotFlora eng = do
    cat ← readIORef (floraCatalogRef (feEnv eng))
    stats ← queueStats (fst (lbsMsgQueues (feLua eng)))
    pure FloraSnapshot
        { fsnNextId   = fcNextId cat
        , fsnSpecies  = sort [ fsName sp | sp ← HM.elems (fcSpecies cat) ]
        , fsnWorldGen = HM.size (fcWorldGen cat)
        , fsnEnqueued = qsEnqueued stats
        }

nameRegistered ∷ FloraEngine → Text → IO Bool
nameRegistered eng name = do
    reg ← readIORef (rvTextureNameRegistryRef
                        (toRenderViewCapability (feEnv eng)))
    pure (isJust (lookupTextureName name reg))

