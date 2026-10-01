-- | The flora visual-state declarations, gated at the authoring
--   boundary (#2539, EFM-2 of epic #2526).
--
--   @textureVariants@ and @corpsePolicy@ follow
--   @docs/flora_visual_state_contract.md@ (#2530). Nothing renders or
--   retains differently yet, so this spec gates the LOADER: every
--   refusal through the real 'loadFloraYaml' on a real file (whole-file
--   rejection plus one warning naming the file, species, authored path,
--   key and token), the legacy default an omitted policy loads as, the
--   shipped corpus declaring its policies, and registration through
--   @engine.loadFloraYaml@ — variant textures counted, keyed by semantic
--   selector, and a missing one substituted with a warning — with a
--   refused file leaving the engine untouched.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Asset.FloraVisualSchema"'@.
module Test.Headless.Asset.FloraVisualSchema (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import Data.List (isInfixOf, sort, sortOn)
import qualified Data.Text as T
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', writeIORef)
import System.FilePath ((</>))
import Engine.Asset.TextureNameRegistry (lookupTextureName)
import Engine.Asset.YamlFlora
    ( FloraYamlDef(..), FloraYamlTextureVariant(..), conditionText
    , conditionVocabulary, contextText, contextVocabulary, deathCauseText
    , deathCauseVocabulary, loadFloraYaml, successorText
    , successorVocabulary, variantSelectorText )
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
import Engine.Asset.Handle (TextureHandle(..))
import World.Flora.Growth (deadWindowDays)
import World.Flora.Types
    ( AnnualStageTag(..), CorpseOutcome(..), CorpseOverrideSelector(..)
    , CorpsePolicyProvenance(..), CorpseSuccessor(..), FloraCatalog(..)
    , FloraCondition(..), FloraContext(..), FloraCorpsePolicy(..)
    , FloraDeathCause(..), FloraSpecies(..), FloraVariantSelector(..)
    , LifePhaseTag(..), findSpeciesByName, legacyCorpseDurationDays
    , legacyCorpsePolicy, newFloraSpecies, wildcardVariantSelector )

-- * Fixtures
--
--   One species whose declared sets are deliberately PARTIAL — two
--   phases of nine, two stages of five — so a recognized token this
--   species does not declare is available for the membership refusals.
--   'fxExtra' carries the block under test, already indented.

data Fixture = Fixture
    { fxName   ∷ String
    , fxTexDir ∷ String
    , fxPhases ∷ [(String, String)]  -- ^ (tag, texture)
    , fxCycle  ∷ [(String, Int)]     -- ^ (tag, startDay)
    , fxExtra  ∷ [String]
    }

probe ∷ Fixture
probe = Fixture
    { fxName   = "probe_visual"
    , fxTexDir = "assets/textures/flora/probe"
    , fxPhases = [("sprout", "sprout.png"), ("matured", "matured.png")]
    , fxCycle  = [("dormant", 0), ("fruiting", 180)]
    , fxExtra  = []
    }

renderFixture ∷ Fixture → String
renderFixture fx = unlines $
    [ "  - name: \"" ⧺ fxName fx ⧺ "\""
    , "    type: \"shrub\""
    , "    texDir: \"" ⧺ fxTexDir fx ⧺ "\""
    , "    lifecycle: perennial"
    ]
    ⧺ [ "    phases:" | not (null (fxPhases fx)) ]
    ⧺ [ "      - {tag: " ⧺ tag ⧺ ", texture: \"" ⧺ tex ⧺ "\", age: 0}"
      | (tag, tex) ← fxPhases fx ]
    ⧺ [ "    annualCycle:" | not (null (fxCycle fx)) ]
    ⧺ [ "      - {tag: " ⧺ tag ⧺ ", startDay: " ⧺ show day
        ⧺ ", texture: \"stage.png\"}"
      | (tag, day) ← fxCycle fx ]
    ⧺ fxExtra fx
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

-- | @probe@ declaring these @textureVariants@ entries, each a list of
--   (key, raw YAML value) pairs written as one flow mapping.
withVariants ∷ [[(String, String)]] → Fixture
withVariants entries = probe
    { fxExtra = "    textureVariants:" : map entry entries }
  where
    entry kvs = "      - {" ⧺ intercalateS ", " [k ⧺ ": " ⧺ v | (k, v) ← kvs]
                ⧺ "}"

-- | @probe@ declaring this raw @corpsePolicy@ body (lines indented
--   under the key).
withPolicy ∷ [String] → Fixture
withPolicy body = probe
    { fxExtra = "    corpsePolicy:" : map ("      " ⧺) body }

intercalateS ∷ String → [String] → String
intercalateS _ []       = ""
intercalateS _ [x]      = x
intercalateS s (x : xs) = x ⧺ s ⧺ intercalateS s xs

-- | A transient outcome's three valid lines, for override bodies.
transientLines ∷ [String]
transientLines =
    ["visibility: transient", "durationDays: 60", "successor: reseed"]

-- * Assertions

-- | Load @src@ through the REAL loader and require whole-file rejection:
--   an empty list plus exactly one 'CatAsset' 'LevelWarn' naming the
--   file, the species and every token in @tokens@, matched as whole
--   words of a punctuation-scrubbed message (the scrub keeps @.@, @-@
--   and @[]@, which are inside authored paths).
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
                    wanted  = path : "probe_visual" : tokens
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

accepts ∷ ([FloraYamlDef] → Expectation) → String → Expectation
accepts check src =
    withFloraFixture src $ \path → do
        (logger, entriesRef) ← callbackLogger
        defs ← loadFloraYaml logger path
        entries ← readIORef entriesRef
        map leMessage entries `shouldBe` []
        check defs

selectorsOf ∷ [FloraYamlDef] → [(FloraVariantSelector, Text)]
selectorsOf defs =
    [ (fytvSelector v, fytvTexture v) | d ← defs, v ← fydTextureVariants d ]

policiesOf ∷ [FloraYamlDef] → [FloraCorpsePolicy]
policiesOf = map fydCorpsePolicy

sel ∷ FloraVariantSelector
sel = wildcardVariantSelector

spec ∷ Spec
spec = do
    describe "textureVariants refusals (requirements 1 and 2)" $ do

        it "rejects an unknown context token, naming the vocabulary" $
            rejectsNaming
                ["textureVariants[0]", "context", "feral", "wild", "cultivated"]
                (floraFile [withVariants
                    [[("context", "feral"), ("texture", "\"a.png\"")]]])

        it "rejects an unknown phase token" $
            rejectsNaming ["textureVariants[0]", "phase", "seedlng"]
                (floraFile [withVariants
                    [[("phase", "seedlng"), ("texture", "\"a.png\"")]]])

        it "rejects an unknown stage token" $
            rejectsNaming ["textureVariants[0]", "stage", "fruting"]
                (floraFile [withVariants
                    [[("stage", "fruting"), ("texture", "\"a.png\"")]]])

        it "rejects an unknown condition token" $
            rejectsNaming
                ["textureVariants[0]", "condition", "dying", "alive", "dead"]
                (floraFile [withVariants
                    [[("condition", "dying"), ("texture", "\"a.png\"")]]])

        it "rejects an unknown cause token" $
            rejectsNaming ["textureVariants[0]", "cause", "lightning", "fire"]
                (floraFile [withVariants
                    [[ ("condition", "dead"), ("cause", "lightning")
                     , ("texture", "\"a.png\"") ]]])

        it "names the failing index, not just the list" $
            rejectsNaming ["textureVariants[1]", "condition", "dying"]
                (floraFile [withVariants
                    [ [("condition", "dead"), ("texture", "\"a.png\"")]
                    , [("condition", "dying"), ("texture", "\"b.png\"")] ]])

        it "rejects a phase this species never declares, naming the \
           \declared set" $
            rejectsNaming
                [ "textureVariants[0]", "phase", "ripening", "phases[]"
                , "sprout", "matured" ]
                (floraFile [withVariants
                    [[("phase", "ripening"), ("texture", "\"a.png\"")]]])

        it "rejects a stage this species never declares, naming the \
           \declared set" $
            rejectsNaming
                [ "textureVariants[0]", "stage", "flowering", "annualCycle[]"
                , "dormant", "fruiting" ]
                (floraFile [withVariants
                    [[("stage", "flowering"), ("texture", "\"a.png\"")]]])

        it "rejects phase: dead even on a species that authors a legacy \
           \dead phase — dead is never a selector phase (contract §1.1)" $
            rejectsNaming ["textureVariants[0]", "phase", "dead"]
                (floraFile [(withVariants
                    [[ ("phase", "dead"), ("condition", "dead")
                     , ("texture", "\"a.png\"") ]])
                    { fxPhases = [ ("sprout", "sprout.png")
                                 , ("dead", "dead.png") ] }])

        it "rejects two entries with the same selector, naming both" $
            rejectsNaming
                ["textureVariants[1]", "textureVariants[0]", "selector"]
                (floraFile [withVariants
                    [ [ ("phase", "sprout"), ("condition", "dead")
                      , ("texture", "\"a.png\"") ]
                    , [ ("condition", "dead"), ("phase", "sprout")
                      , ("texture", "\"b.png\"") ] ]])

        it "rejects a variant restating the selector a legacy dead phase \
           \already declares (contract §2.1)" $
            rejectsNaming ["textureVariants[0]", "selector", "phases[]", "dead"]
                (floraFile [(withVariants
                    [[("condition", "dead"), ("texture", "\"a.png\"")]])
                    { fxPhases = [ ("sprout", "sprout.png")
                                 , ("dead", "dead.png") ] }])

        it "rejects a variant restating a legacy phase-dead cycle \
           \override's stage-specific selector" $
            rejectsNaming
                ["textureVariants[0]", "selector", "cycleOverrides[]", "dormant"]
                (floraFile [probe
                    { fxPhases = [ ("sprout", "sprout.png")
                                 , ("dead", "dead.png") ]
                    , fxExtra =
                        [ "    cycleOverrides:"
                        , "      - {phase: dead, cycle: dormant, \
                          \texture: \"d.png\"}"
                        , "    textureVariants:"
                        , "      - {condition: dead, stage: dormant, \
                          \texture: \"a.png\"}" ] }])

        it "rejects an empty texture" $
            rejectsNaming ["textureVariants[0]", "texture"]
                (floraFile [withVariants
                    [[("condition", "dead"), ("texture", "\"\"")]]])

        it "rejects an absent texture" $
            rejectsNaming ["textureVariants[0]", "texture"]
                (floraFile [withVariants [[("condition", "dead")]]])

        it "rejects null at a present axis rather than reading it as a \
           \wildcard" $
            forM_ ["context", "phase", "stage", "condition", "cause"] $ \k →
                rejectsNaming ["textureVariants[0]", k, "null"]
                    (floraFile [withVariants
                        [[(k, "null"), ("texture", "\"a.png\"")]]])

        it "rejects a cause without condition: dead — nothing could \
           \request it (contract §2 rule 4)" $ do
            rejectsNaming ["textureVariants[0]", "cause", "fire"]
                (floraFile [withVariants
                    [[("cause", "fire"), ("texture", "\"a.png\"")]]])
            rejectsNaming ["textureVariants[0]", "cause", "fire"]
                (floraFile [withVariants
                    [[ ("condition", "alive"), ("cause", "fire")
                     , ("texture", "\"a.png\"") ]]])

        it "rejects a misspelled axis key, which would otherwise widen \
           \the selector to a wildcard" $
            rejectsNaming ["textureVariants[0]", "condtion"]
                (floraFile [withVariants
                    [[("condtion", "dead"), ("texture", "\"a.png\"")]]])

        it "refuses the WHOLE file, valid sibling included" $
            withFloraFixture
                (floraFile [ withVariants
                                [[("phase", "seedlng"), ("texture", "\"a.png\"")]]
                           , probe { fxName = "probe_sibling" } ])
                $ \path → do
                    (logger, _) ← callbackLogger
                    defs ← loadFloraYaml logger path
                    map fydName defs `shouldBe` []

    describe "textureVariants acceptance (requirement 1)" $ do

        it "leaves an absent textureVariants empty" $
            accepts (\defs → selectorsOf defs `shouldBe` []) (floraFile [probe])

        it "accepts sparse declarations: an all-wildcard selector, a \
           \nested relative path, distinct selectors sharing one path, \
           \and explicit-wild beside context-less" $
            accepts (\defs → selectorsOf defs `shouldBe`
                [ (sel, "base_variant.png")
                , ( sel { fvsContext = Just ContextCultivated
                        , fvsCondition = Just ConditionDead }
                  , "cultivated/dead.png" )
                , (sel { fvsCondition = Just ConditionDead }, "dead.png")
                , ( sel { fvsContext = Just ContextWild
                        , fvsCondition = Just ConditionDead }
                  , "dead.png" )
                , ( sel { fvsPhase = Just PhaseMatured
                        , fvsStage = Just CycleFruiting
                        , fvsCondition = Just ConditionDead
                        , fvsCause = Just CauseFire }
                  , "matured_fruiting_charred.png" )
                ])
                (floraFile [withVariants
                    [ [("texture", "\"base_variant.png\"")]
                    , [ ("context", "cultivated"), ("condition", "dead")
                      , ("texture", "\"cultivated/dead.png\"") ]
                    , [("condition", "dead"), ("texture", "\"dead.png\"")]
                    , [ ("context", "wild"), ("condition", "dead")
                      , ("texture", "\"dead.png\"") ]
                    , [ ("phase", "matured"), ("stage", "fruiting")
                      , ("condition", "dead"), ("cause", "fire")
                      , ("texture", "\"matured_fruiting_charred.png\"") ] ]])

        it "accepts every token of every selector vocabulary" $ do
            forM_ contextVocabulary $ \t →
                accepts (\defs → length (selectorsOf defs) `shouldBe` 1)
                    (floraFile [withVariants
                        [[("context", T.unpack t), ("texture", "\"a.png\"")]]])
            forM_ conditionVocabulary $ \t →
                accepts (\defs → length (selectorsOf defs) `shouldBe` 1)
                    (floraFile [withVariants
                        [[("condition", T.unpack t), ("texture", "\"a.png\"")]]])
            forM_ deathCauseVocabulary $ \t →
                accepts (\defs → length (selectorsOf defs) `shouldBe` 1)
                    (floraFile [withVariants
                        [[ ("condition", "dead"), ("cause", T.unpack t)
                         , ("texture", "\"a.png\"") ]]])

        it "advertises exactly the contract's vocabularies, each the \
           \inverse of its renderer" $ do
            contextVocabulary `shouldBe` ["wild", "cultivated"]
            conditionVocabulary `shouldBe` ["alive", "dead"]
            deathCauseVocabulary `shouldBe`
                [ "natural", "drought", "frost", "fire", "disease", "damage"
                , "unknown" ]
            successorVocabulary `shouldBe` ["reseed", "absent"]
            map contextText [minBound .. maxBound] `shouldBe` contextVocabulary
            map conditionText [minBound .. maxBound]
                `shouldBe` conditionVocabulary
            map deathCauseText [minBound .. maxBound]
                `shouldBe` deathCauseVocabulary
            map successorText [minBound .. maxBound]
                `shouldBe` successorVocabulary

    describe "corpsePolicy refusals (requirement 3)" $ do

        it "rejects a present corpsePolicy: null rather than reading it \
           \as the legacy default" $
            rejectsNaming ["corpsePolicy", "null"]
                (floraFile [probe { fxExtra = ["    corpsePolicy: null"] }])

        it "rejects a non-block corpsePolicy" $
            rejectsNaming ["corpsePolicy", "transient"]
                (floraFile [probe { fxExtra = ["    corpsePolicy: transient"] }])

        it "rejects an unknown visibility token" $
            rejectsNaming
                ["corpsePolicy", "visibility", "ephemeral", "transient"
                , "persistent"]
                (floraFile [withPolicy ["visibility: ephemeral"]])

        it "rejects a missing visibility" $
            rejectsNaming ["corpsePolicy", "visibility"]
                (floraFile [withPolicy
                    ["durationDays: 60", "successor: reseed"]])

        it "rejects a transient policy without durationDays" $
            rejectsNaming ["corpsePolicy", "durationDays"]
                (floraFile [withPolicy
                    ["visibility: transient", "successor: reseed"]])

        it "rejects a transient policy without successor" $
            rejectsNaming ["corpsePolicy", "successor"]
                (floraFile [withPolicy
                    ["visibility: transient", "durationDays: 60"]])

        it "rejects durationDays on a persistent policy" $
            rejectsNaming ["corpsePolicy", "durationDays", "persistent"]
                (floraFile [withPolicy
                    ["visibility: persistent", "durationDays: 60"]])

        it "rejects successor on a persistent policy" $
            rejectsNaming ["corpsePolicy", "successor", "persistent"]
                (floraFile [withPolicy
                    ["visibility: persistent", "successor: reseed"]])

        it "rejects await_replanting — the cultivated outcome is not a \
           \species token (contract §7.2)" $
            rejectsNaming
                ["corpsePolicy", "successor", "await_replanting", "reseed"
                , "absent"]
                (floraFile [withPolicy
                    [ "visibility: transient", "durationDays: 60"
                    , "successor: await_replanting" ]])

        it "rejects an unknown successor and a null one" $ do
            rejectsNaming ["corpsePolicy", "successor", "regrow"]
                (floraFile [withPolicy
                    [ "visibility: transient", "durationDays: 60"
                    , "successor: regrow" ]])
            rejectsNaming ["corpsePolicy", "successor", "null"]
                (floraFile [withPolicy
                    [ "visibility: transient", "durationDays: 60"
                    , "successor: null" ]])

        it "rejects a misspelled policy key" $
            rejectsNaming ["corpsePolicy", "duration"]
                (floraFile [withPolicy
                    [ "visibility: transient", "duration: 60"
                    , "successor: reseed" ]])

        describe "durationDays is a whole number of days of at least 1, \
                 \at the base and at an override" $ do
            let bad = [ "0", "-5", "1.5", "null", "sixty", "\".inf\""
                      , "1.0e+30", "9223372036854775808" ]
            forM_ bad $ \raw → do
                it ("refuses durationDays: " ⧺ raw ⧺ " at the base") $
                    rejectsNaming ["corpsePolicy", "durationDays"]
                        (floraFile [withPolicy
                            [ "visibility: transient"
                            , "durationDays: " ⧺ raw
                            , "successor: reseed" ]])
                it ("refuses durationDays: " ⧺ raw ⧺ " at an override") $
                    rejectsNaming
                        ["corpsePolicy.overrides[0]", "durationDays"]
                        (floraFile [withPolicy
                            [ "visibility: persistent"
                            , "overrides:"
                            , "  - phase: sprout"
                            , "    visibility: transient"
                            , "    durationDays: " ⧺ raw
                            , "    successor: reseed" ]])

        it "rejects an override selecting neither phase nor cause" $
            rejectsNaming ["corpsePolicy.overrides[0]", "phase"]
                (floraFile [withPolicy
                    ("visibility: persistent" : "overrides:"
                     : overrideBody [] transientLines)])

        it "rejects an override phase outside the vocabulary, undeclared, \
           \dead, or null" $ do
            rejectsNaming ["corpsePolicy.overrides[0]", "phase", "sprouut"]
                (floraFile [withPolicy (persistentWith
                    [("phase", "sprouut")])])
            rejectsNaming
                [ "corpsePolicy.overrides[0]", "phase", "ripening", "phases[]"
                , "sprout", "matured" ]
                (floraFile [withPolicy (persistentWith
                    [("phase", "ripening")])])
            rejectsNaming ["corpsePolicy.overrides[0]", "phase", "dead"]
                (floraFile [(withPolicy (persistentWith [("phase", "dead")]))
                    { fxPhases = [ ("sprout", "sprout.png")
                                 , ("dead", "dead.png") ] }])
            rejectsNaming ["corpsePolicy.overrides[0]", "phase", "null"]
                (floraFile [withPolicy (persistentWith [("phase", "null")])])

        it "rejects an override cause outside the vocabulary, or null" $ do
            rejectsNaming ["corpsePolicy.overrides[0]", "cause", "lightning"]
                (floraFile [withPolicy (persistentWith
                    [("cause", "lightning")])])
            rejectsNaming ["corpsePolicy.overrides[0]", "cause", "null"]
                (floraFile [withPolicy (persistentWith [("cause", "null")])])

        it "rejects an override's field coupling violations — it declares \
           \a complete outcome and inherits nothing" $ do
            rejectsNaming ["corpsePolicy.overrides[0]", "visibility"]
                (floraFile [withPolicy
                    ("visibility: persistent" : "overrides:"
                     : overrideBody [("phase", "sprout")]
                         ["durationDays: 60", "successor: reseed"])])
            rejectsNaming
                ["corpsePolicy.overrides[0]", "durationDays", "persistent"]
                (floraFile [withPolicy
                    ( [ "visibility: transient", "durationDays: 60"
                      , "successor: reseed", "overrides:" ]
                    ⧺ overrideBody [("phase", "sprout")]
                         ["visibility: persistent", "durationDays: 60"])])
            rejectsNaming ["corpsePolicy.overrides[0]", "successor"]
                (floraFile [withPolicy
                    ("visibility: persistent" : "overrides:"
                     : overrideBody [("phase", "sprout")]
                         ["visibility: transient", "durationDays: 60"])])

        it "rejects two overrides with the same selector, naming both" $
            rejectsNaming
                ["corpsePolicy.overrides[1]", "corpsePolicy.overrides[0]"
                , "selector"]
                (floraFile [withPolicy
                    ( "visibility: persistent" : "overrides:"
                    : overrideBody [("phase", "sprout"), ("cause", "fire")]
                        transientLines
                    ⧺ overrideBody [("cause", "fire"), ("phase", "sprout")]
                        transientLines )])

        it "rejects a non-list overrides" $
            rejectsNaming ["corpsePolicy.overrides", "overrides"]
                (floraFile [withPolicy
                    ["visibility: persistent", "overrides: sprout"]])

    describe "corpsePolicy acceptance (requirements 3 and 4)" $ do

        it "loads an ABSENT corpsePolicy as the legacy default — transient, \
           \60 days, reseed — marked as defaulted" $
            accepts (\defs → do
                policiesOf defs `shouldBe` [legacyCorpsePolicy]
                map fcpProvenance (policiesOf defs)
                    `shouldBe` [CorpsePolicyDefaulted]
                map fcpOutcome (policiesOf defs)
                    `shouldBe` [CorpseTransient 60 SuccessorReseed])
                (floraFile [probe])

        it "keeps the legacy window equal to World.Flora.Growth's dead \
           \window" $
            fromIntegral legacyCorpseDurationDays `shouldBe` deadWindowDays

        it "loads an authored transient policy as authored" $
            accepts (\defs → policiesOf defs `shouldBe`
                [FloraCorpsePolicy
                    { fcpOutcome    = CorpseTransient 7 SuccessorAbsent
                    , fcpOverrides  = HM.empty
                    , fcpProvenance = CorpsePolicyAuthored }])
                (floraFile [withPolicy
                    [ "visibility: transient", "durationDays: 7"
                    , "successor: absent" ]])

        it "accepts the minimum window of one day" $
            accepts (\defs → map fcpOutcome (policiesOf defs)
                        `shouldBe` [CorpseTransient 1 SuccessorReseed])
                (floraFile [withPolicy
                    [ "visibility: transient", "durationDays: 1"
                    , "successor: reseed" ]])

        it "loads a persistent policy with phase, cause, and phase+cause \
           \overrides, each keyed by its semantic selector" $
            accepts (\defs → policiesOf defs `shouldBe`
                [FloraCorpsePolicy
                    { fcpOutcome    = CorpsePersistent
                    , fcpOverrides  = HM.fromList
                        [ ( CorpseOverrideSelector (Just PhaseSprout) Nothing
                          , CorpseTransient 60 SuccessorReseed )
                        , ( CorpseOverrideSelector Nothing (Just CauseFire)
                          , CorpseTransient 10 SuccessorAbsent )
                        , ( CorpseOverrideSelector (Just PhaseSprout)
                                (Just CauseFire)
                          , CorpsePersistent ) ]
                    , fcpProvenance = CorpsePolicyAuthored }])
                (floraFile [withPolicy
                    ( "visibility: persistent" : "overrides:"
                    : overrideBody [("phase", "sprout")] transientLines
                    ⧺ overrideBody [("cause", "fire")]
                        [ "visibility: transient", "durationDays: 10"
                        , "successor: absent" ]
                    ⧺ overrideBody [("phase", "sprout"), ("cause", "fire")]
                        ["visibility: persistent"] )])

    describe "the shipped corpus (requirement 5)" $ do

        it "every one of the sixteen species declares its policy per \
           \D-12/D-18/D-20, none defaulted, none declaring variants" $ do
            (logger, _) ← callbackLogger
            defs ← concat <$> forM shippedFiles (\file →
                loadFloraYaml logger ("data/flora" </> file))
            length defs `shouldBe` 16
            let got = sortOn fst [ (fydName d, fydCorpsePolicy d) | d ← defs ]
            got `shouldBe` sortOn fst shippedPolicies
            [ fydName d | d ← defs, not (null (fydTextureVariants d)) ]
                `shouldBe` []

    describe "registration (requirement 6)" $ do

        it "newFloraSpecies starts with no variants and the defaulted \
           \legacy policy" $ do
            let sp = newFloraSpecies "x" (TextureHandle 0)
            fsTextureVariants sp `shouldBe` HM.empty
            fsCorpsePolicy sp `shouldBe` legacyCorpsePolicy

        it "registers every declared variant through engine.loadFloraYaml: \
           \counted, keyed by selector, named by selector, and a missing \
           \file substituted with a warning" $
            withFloraEngine $ \eng →
            withFloraFixture (floraFile [registrationFixture]) $ \path → do
                (count, parsed, refusal) ← loadFloraOutcome eng (T.pack path)
                -- base + one phase + four variants; no annualCycle.
                (count, parsed, refusal) `shouldBe` ("6", "true", "nil")
                cat ← readIORef (floraCatalogRef (feEnv eng))
                case findSpeciesByName "probe_visual" cat of
                    Nothing → expectationFailure "species not registered"
                    Just (_, sp) → do
                        sort (HM.keys (fsTextureVariants sp))
                            `shouldBe` sort registrationSelectors
                        fcpProvenance (fsCorpsePolicy sp)
                            `shouldBe` CorpsePolicyAuthored
                        fcpOutcome (fsCorpsePolicy sp)
                            `shouldBe` CorpsePersistent
                forM_ registrationSelectors $ \s →
                    nameRegistered eng
                        ("flora_variant_probe_visual_" <> variantSelectorText s)
                        `shouldReturn` True
                warnings ← map (T.unpack ∘ leMessage) ∘ filter
                    ((≡ LevelWarn) ∘ leLevel) <$> readIORef (feLog eng)
                let missingWarnings =
                        [ w | w ← warnings
                        , "missing_variant.png" `isInfixOf` w
                        , "unknown_flora.png" `isInfixOf` w ]
                length missingWarnings `shouldBe` 1
                -- The three present files are not substituted.
                [ w | w ← warnings, "cultivated/dead.png" `isInfixOf` w
                                  ∨ "wild/dead.png" `isInfixOf` w ]
                    `shouldBe` []

        it "an invalid species beside a valid sibling refuses the file: \
           \parsed == false and no catalog, allocator, texture-registry or \
           \queue mutation" $
            withFloraEngine $ \eng → do
                before ← snapshotFlora eng
                withFloraFixture
                    (floraFile
                        [ withVariants
                            [[("cause", "fire"), ("texture", "\"a.png\"")]]
                        , registrationFixture { fxName = "probe_visual_sound" } ])
                    $ \path → do
                        (count, parsed, refusal) ←
                            loadFloraOutcome eng (T.pack path)
                        (count, parsed, refusal)
                            `shouldBe` ("0", "false", "nil")
                        snapshotFlora eng `shouldReturn` before
                        nameRegistered eng
                            "flora_variant_probe_visual_sound_*"
                            `shouldReturn` False
                        nameRegistered eng "flora_base_probe_visual_sound"
                            `shouldReturn` False

-- | An override entry: its selector pairs then its outcome lines,
--   indented as list items under @overrides:@.
overrideBody ∷ [(String, String)] → [String] → [String]
overrideBody selector outcome = case lines' of
    []       → ["  - {}"]
    (l : ls) → ("  - " ⧺ l) : map ("    " ⧺) ls
  where
    lines' = [ k ⧺ ": " ⧺ v | (k, v) ← selector ] ⧺ outcome

-- | A persistent policy with one transient override selected by these
--   pairs.
persistentWith ∷ [(String, String)] → [String]
persistentWith selector =
    "visibility: persistent" : "overrides:" : overrideBody selector transientLines

-- | Real art under @assets/textures/flora/wheat/@ (PR #2136) exercises a
--   nested relative path and two selectors sharing one file; the fourth
--   variant names a file that does not exist.
registrationFixture ∷ Fixture
registrationFixture = probe
    { fxTexDir = "assets/textures/flora/wheat"
    , fxPhases = [("sprout", "wild/sprout.png")]
    , fxCycle  = []
    , fxExtra  =
        [ "    textureVariants:"
        , "      - {texture: \"wild/dead.png\"}"
        , "      - {condition: dead, texture: \"wild/dead.png\"}"
        , "      - {context: cultivated, condition: dead, \
          \texture: \"cultivated/dead.png\"}"
        , "      - {phase: sprout, condition: dead, \
          \texture: \"missing_variant.png\"}"
        , "    corpsePolicy:"
        , "      visibility: persistent"
        ]
    }

registrationSelectors ∷ [FloraVariantSelector]
registrationSelectors =
    [ sel
    , sel { fvsCondition = Just ConditionDead }
    , sel { fvsContext = Just ContextCultivated
          , fvsCondition = Just ConditionDead }
    , sel { fvsPhase = Just PhaseSprout, fvsCondition = Just ConditionDead }
    ]

-- * The shipped baseline

shippedFiles ∷ [FilePath]
shippedFiles =
    [ "boreal_evergreen.yaml", "crops.yaml", "saguaro.yaml"
    , "temperate_deciduous.yaml", "temperate_shrubs.yaml"
    , "temperate_wildflowers.yaml", "tropical.yaml", "wetlands.yaml" ]

-- | D-12/D-18/D-20: the eight trees and saguaro keep a persistent
--   corpse with a transient 60-day reseeding sprout; everything else is
--   transient for 60 days and reseeds.
shippedPolicies ∷ [(Text, FloraCorpsePolicy)]
shippedPolicies =
    [ (n, persistentSprout)
    | n ← [ "scots_pine", "white_spruce", "coconut_palm", "red_mangrove"
          , "white_oak", "paper_birch", "weeping_willow", "sugar_maple"
          , "saguaro" ] ]
    ⧺ [ (n, transient60)
      | n ← [ "bracken_fern", "red_raspberry", "common_dandelion"
            , "white_clover", "common_cattail", "tomato_plant", "wheat" ] ]
  where
    sixty = CorpseTransient 60 SuccessorReseed
    transient60 = FloraCorpsePolicy sixty HM.empty CorpsePolicyAuthored
    persistentSprout = FloraCorpsePolicy CorpsePersistent
        (HM.singleton (CorpseOverrideSelector (Just PhaseSprout) Nothing) sixty)
        CorpsePolicyAuthored

-- * Harness

callbackLogger ∷ IO (LoggerState, IORef [LogEntry])
callbackLogger = do
    entriesRef ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' entriesRef (e :)) }
    pure (logger, entriesRef)

withFloraFixture ∷ String → (FilePath → Expectation) → Expectation
withFloraFixture body action =
    withExclusiveTempDirectory "synarchy-2539-visual" $ \dir → do
        let path = dir </> "probe.yaml"
        writeFile path body
        action path

-- | A private, isolated headless engine with the real Lua API, whose
--   logger captures every entry so substitution warnings are visible.
data FloraEngine = FloraEngine
    { feEnv ∷ EngineEnv
    , feLua ∷ LuaBackendState
    , feLog ∷ IORef [LogEntry]
    }

withFloraEngine ∷ (FloraEngine → Expectation) → Expectation
withFloraEngine action = withIsolatedResourceRoot $ do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    (logger, entriesRef) ← callbackLogger
    writeIORef (loggerRef env) logger
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    action (FloraEngine env ls entriesRef)

evalLua ∷ FloraEngine → Text → IO Text
evalLua eng src =
    T.strip ∘ T.filter (≢ '"') <$> executeDebugLua (lbsLuaState (feLua eng)) src

loadFloraOutcome ∷ FloraEngine → Text → IO (Text, Text, Text)
loadFloraOutcome eng path = do
    out ← evalLua eng
        ("local n, parsed, refusal = engine.loadFloraYaml('" <> path
         <> "', true); return string.format('%d|%s|%s', n, \
            \tostring(parsed), tostring(refusal))")
    case T.splitOn "|" out of
        [n, parsed, refusal] → pure (n, parsed, refusal)
        _                    → pure (out, out, out)

data FloraSnapshot = FloraSnapshot
    { fsnNextId   ∷ Word16
    , fsnSpecies  ∷ [Text]
    , fsnWorldGen ∷ Int
    , fsnEnqueued ∷ Word64
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
