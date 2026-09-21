{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The @Faction tag catalogue@ gate (#2506, FTS-2 of #2496): the
--   validated @data\/factions\/@ authority, the @faction_tags:@ key on a
--   unit definition, and the D-26 legacy mapping those two feed.
--
--   __Everything here runs a REAL loader.__ The refusal rules are
--   asserted through 'Engine.Asset.YamlFactions.loadFactionYamlOutcome'
--   on a real file and through the real @engine.loadFactionYaml@ /
--   @engine.loadUnitYaml@ bindings on a live headless engine, because
--   whole-file rejection is a claim about what the ENGINE did — a pure
--   predicate over a decoded document could pass while the loader
--   registered half the file anyway.
--
--   That is also why the ordering example injects an atlas resolver and
--   registers through 'registerUnitDefs' directly: the definition ahead
--   of the offending one has to be one that WOULD have registered, or
--   "validation completes before anything is published" is a sentence
--   rather than a test.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Faction tag catalogue"'@.
module Test.Headless.Unit.FactionCatalogue (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sort)
import System.FilePath ((</>))
import Engine.Asset.Types (defaultAssetPool)
import Engine.Asset.YamlUnits (UnitYamlDef(..), loadUnitYaml)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Init (EngineInitResult(..))
import qualified Engine.Core.Queue as Q
import Engine.Core.Log
    ( LogBackend(..), LogConfig(..), LoggerState, defaultLogConfig
    , initLogger )
import Engine.Core.State
    ( EngineEnv, assetPoolRef, inputStateRef, loggerRef, luaQueue
    , luaToEngineQueue, nextObjectIdRef )
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.API.Units.Yaml
    (AtlasResolver, registerUnitDefs, resolveUnitFactionTags)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness.Isolation
    (withExclusiveTempDirectory, withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Unit.Faction (Faction(..), FactionRelation(..))
import Unit.Faction.Catalogue
import Unit.Faction.Profile
import Unit.Types (UnitDef(..), UnitManager(..))

-----------------------------------------------------------------------
-- Fixtures
-----------------------------------------------------------------------

-- | The shipped catalogue, and the shipped units checked against it.
shippedCatalogue ∷ FilePath
shippedCatalogue = "data" </> "factions" </> "base.yaml"

shippedUnitPath ∷ Text → FilePath
shippedUnitPath name = "data" </> "units" </> T.unpack name ⧺ ".yaml"

-- | Requirement 7's table, written out rather than read back off the
--   files, so a shipped YAML that quietly loses its @faction_tags:@ is a
--   failure rather than a self-fulfilling comparison.
shippedDefaults ∷ [(Text, [Text])]
shippedDefaults =
    [ ("acolyte",           ["acolyte"])
    , ("technomule",        ["acolyte"])
    , ("nomad_primitive",   ["nomad"])
    , ("bear_brown",        ["wildlife"])
    , ("red_squirrel",      ["wildlife"])
    , ("white_tailed_deer", ["wildlife"])
    , ("tiller",            [])
    , ("unknown_unit",      [])
    ]

-- | Requirement 6's table: the four declared tags, and the four
--   symmetric hostile pairs expanded into the eight ordered pairs they
--   mean. Everything else must be absent, which is what makes
--   @nomad@\/@wildlife@ neutral by silence (D-8).
shippedTags ∷ [Text]
shippedTags = ["acolyte", "legacy_hostile", "nomad", "wildlife"]

shippedHostilePairs ∷ [(Text, Text)]
shippedHostilePairs =
    [ (a, b)
    | (x, y) ← [ ("acolyte", "nomad"), ("acolyte", "wildlife")
               , ("legacy_hostile", "acolyte")
               , ("legacy_hostile", "wildlife") ]
    , (a, b) ← [(x, y), (y, x)] ]

-- | Fails loudly rather than degrading, so a fixture typo can never make
--   an example vacuously pass against a tag it does not actually name.
tag ∷ Text → FactionTag
tag t = fromMaybe (error ("invalid fixture tag: " ⧺ show t)) (mkFactionTag t)

localPlayer ∷ ControllerId
localPlayer = humanController "local"

quoted ∷ FilePath → Text
quoted path = "'" <> T.pack path <> "'"

-- | A catalogue file body, written to a temp path.
catalogueYaml ∷ [Text] → [Text] → String
catalogueYaml tags relations = T.unpack ∘ T.unlines $
    ("faction_tags:" : [ "  - id: " <> t | t ← tags ])
    ⧺ ("relations:" : [ "  - " <> r | r ← relations ])

-- | A unit file body declaring one definition per @(name, tags)@ pair.
--   @Nothing@ omits @faction_tags:@ entirely, which is a different thing
--   from an empty list only in how it is written.
unitYaml ∷ [(Text, Maybe [Text])] → String
unitYaml defs = T.unpack ∘ T.unlines $ "units:" : concatMap one defs
  where
    one (name, mTags) =
        [ "  - name: " <> name
        , "    sprite: assets/textures/utility/blanktexture.png" ]
        ⧺ case mTags of
            Nothing   → []
            Just tags → [ "    faction_tags: ["
                          <> T.intercalate ", " [ "\"" <> t <> "\"" | t ← tags ]
                          <> "]" ]

withFixture ∷ String → String → (FilePath → Expectation) → Expectation
withFixture label body action =
    withFixtureDir label [("probe.yaml", body)] $ \at →
        action (at "probe.yaml")

-- | Several files in ONE directory, which is what a catalogue family
--   actually is — and the only way to exercise the rules that span it.
--
--   The body is handed a lookup BY NAME rather than a list, because
--   every example here cares which file is which and none of them cares
--   what order they were written in; the loader's own enumeration order
--   is the thing under test.
withFixtureDir ∷ String → [(String, String)]
               → ((String → FilePath) → Expectation) → Expectation
withFixtureDir label files action =
    withExclusiveTempDirectory ("synarchy-2506-" ⧺ label) $ \dir → do
        forM_ files $ \(name, body) → writeFile (dir </> name) body
        action (dir </>)

-----------------------------------------------------------------------
-- The live headless engine
-----------------------------------------------------------------------

-- | A throwaway headless engine with the real Lua API registered.
--
--   PRIVATE per example and ISOLATED, because the boot itself writes
--   @config\/@ (#1357) — the same harness
--   "Test.Headless.Asset.FloraVocabularySchema" uses, and for the same
--   reason. The scratch root symlinks @data\/@, so a shipped path still
--   resolves.
data FactionEngine = FactionEngine
    { feEnv ∷ EngineEnv
    , feLua ∷ LuaBackendState
    }

withFactionEngine ∷ (FactionEngine → Expectation) → Expectation
withFactionEngine action = withIsolatedResourceRoot $ do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\_ → pure ()) }
    writeIORef (loggerRef env) logger
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    action (FactionEngine env ls)

evalLua ∷ FactionEngine → Text → IO Text
evalLua eng src =
    T.strip ∘ T.filter (≢ '"') <$> executeDebugLua (lbsLuaState (feLua eng)) src

-- | The four values an opted-in @engine.load*Yaml@ call can answer with:
--   count, decode outcome, refusal detail and refusal reason. The last
--   two are @nil@ for anything that was not refused.
data LoadOutcome = LoadOutcome
    { loCount  ∷ Text
    , loParsed ∷ Text
    , loDetail ∷ Text
    , loReason ∷ Text
    } deriving (Show, Eq)

callLoader ∷ FactionEngine → Text → FilePath → IO LoadOutcome
callLoader eng verb path = do
    out ← evalLua eng
        ("local n, parsed, detail, reason = engine." <> verb <> "('"
         <> T.pack path <> "', true); return string.format('%d|%s|%s|%s', \
            \n, tostring(parsed), tostring(detail), tostring(reason))")
    case T.splitOn "|" out of
        [n, p, d, r] → pure (LoadOutcome n p d r)
        _            → pure (LoadOutcome out out out out)

loadCatalogue ∷ FactionEngine → FilePath → IO LoadOutcome
loadCatalogue eng = callLoader eng "loadFactionYaml"

loadUnits ∷ FactionEngine → FilePath → IO LoadOutcome
loadUnits eng = callLoader eng "loadUnitYaml"

refused ∷ Text → Text → LoadOutcome → Expectation
refused reason detail outcome = outcome `shouldBe`
    LoadOutcome { loCount = "0", loParsed = "true"
                , loDetail = detail, loReason = reason }

-- | How many values the real binding actually pushed. @select('#',
--   ...)@ counts them, which is the only way to tell a pushed @nil@
--   from a value that was never pushed at all.
arityOf ∷ FactionEngine → Text → Text → IO Text
arityOf eng verb call =
    evalLua eng ("return tostring(select('#', engine." <> verb <> "("
                 <> call <> ")))")

catalogueOf ∷ FactionEngine → IO FactionCatalogue
catalogueOf eng =
    readIORef (crFactionCatalogueRef (toContentRegistriesCapability (feEnv eng)))

registeredUnits ∷ FactionEngine → IO [Text]
registeredUnits eng = do
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability (feEnv eng)))
    pure (sort (HM.keys (umDefs um)))

registeredDef ∷ FactionEngine → Text → IO (Maybe UnitDef)
registeredDef eng name = do
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability (feEnv eng)))
    pure (HM.lookup name (umDefs um))

quietLogger ∷ IO LoggerState
quietLogger = initLogger defaultLogConfig
    { lcBackend = LogToCallback (\_ → pure ()) }

-- | Load the SHIPPED catalogue through the real binding; every example
--   that goes on to load a unit file needs it, because a unit file
--   naming a tag nothing declared is refused (D-30).
withShippedCatalogue ∷ (FactionEngine → Expectation) → Expectation
withShippedCatalogue action = withFactionEngine $ \eng → do
    out ← loadCatalogue eng shippedCatalogue
    loParsed out `shouldBe` "true"
    action eng

-----------------------------------------------------------------------
-- An injected atlas resolver, for the ordering proof
-----------------------------------------------------------------------

-- | A compiled animation, as the resolver would report it. Copied in
--   shape from "Test.Headless.Unit.Atlas.Loader" — what matters here is
--   only that resolution SUCCEEDS, so a definition ahead of a refusal
--   would genuinely have been published.
alwaysResolves ∷ AtlasResolver
alwaysResolves _ _ = pure (Right HM.empty)

-- | Register a decoded unit file through the production registration
--   path with atlas resolution stubbed out.
runRegister ∷ FactionEngine → FilePath → [UnitYamlDef]
            → IO (Either Text Int)
runRegister eng path defs = do
    poolRef ← newIORef =≪ defaultAssetPool
    q ← Q.newQueue
    result ← registerUnitDefs (feEnv eng) poolRef q alwaysResolves path defs
    pure (either (const (Left "refused")) Right result)

-----------------------------------------------------------------------
-- The gate
-----------------------------------------------------------------------

spec ∷ Spec
spec = describe "Faction tag catalogue" $ do
    catalogueRefusalSpec
    catalogueShapeSpec
    unitTagRefusalSpec
    unitTagAdmissionSpec
    shippedCorpusSpec
    directoryScopeSpec
    openNamespaceSpec
    legacyMappingSpec

-----------------------------------------------------------------------
-- Requirement 2: the catalogue's own refusals
-----------------------------------------------------------------------

catalogueRefusalSpec ∷ Spec
catalogueRefusalSpec = describe "catalogue refusals (requirement 2)" $ do

    it "refuses a tag declared twice, naming the id" $
        withFactionEngine $ \eng →
            withFixture "dup-tag"
                (catalogueYaml ["acolyte", "nomad", "acolyte"] []) $ \path → do
                    out ← loadCatalogue eng path
                    refused "duplicate faction tag declaration" "acolyte" out
                    cat ← catalogueOf eng
                    catalogueDeclarations cat `shouldBe` []

    it "refuses a tag already declared by an EARLIER file, so the rule \
       \spans the directory rather than one document" $
        withFactionEngine $ \eng →
            withFixture "dup-first" (catalogueYaml ["acolyte"] []) $ \first →
            withFixture "dup-second" (catalogueYaml ["acolyte"] []) $ \second → do
                (loParsed <$> loadCatalogue eng first)
                    `shouldReturn` "true"
                out ← loadCatalogue eng second
                refused "duplicate faction tag declaration" "acolyte" out
                cat ← catalogueOf eng
                map (factionTagText ∘ ftdTag) (catalogueDeclarations cat)
                    `shouldBe` ["acolyte"]

    it "refuses a malformed tag id, naming it" $
        withFactionEngine $ \eng →
            withFixture "bad-tag" (catalogueYaml ["\"two words\""] []) $ \path → do
                out ← loadCatalogue eng path
                refused "malformed faction tag id" "two words" out

    it "refuses a relation naming an undeclared tag, naming that tag" $
        withFactionEngine $ \eng →
            withFixture "undeclared"
                (catalogueYaml ["acolyte"]
                    ["{ pair: [acolyte, nomad], relation: hostile }"]) $ \path → do
                        out ← loadCatalogue eng path
                        refused "undeclared faction tag" "nomad" out
                        cat ← catalogueOf eng
                        -- The TAGS the same file declared are not kept
                        -- either: the refusal is whole-file.
                        catalogueDeclarations cat `shouldBe` []

    it "refuses a relation value outside ally / neutral / hostile" $
        withFactionEngine $ \eng →
            withFixture "bad-relation"
                (catalogueYaml ["acolyte", "nomad"]
                    ["{ pair: [acolyte, nomad], relation: enemy }"]) $ \path → do
                        out ← loadCatalogue eng path
                        refused "invalid faction relation value" "enemy" out

    it "refuses the same ordered pair declared twice, by a directed \
       \entry colliding with an earlier symmetric one" $
        withFactionEngine $ \eng →
            withFixture "dup-pair"
                (catalogueYaml ["acolyte", "nomad"]
                    [ "{ pair: [acolyte, nomad], relation: hostile }"
                    , "{ from: nomad, to: acolyte, relation: hostile }" ]) $ \path → do
                        out ← loadCatalogue eng path
                        refused "duplicate faction relation"
                                "nomad → acolyte" out

    it "refuses a re-declared pair even when the two values AGREE, \
       \because agreement is not what the rule is about" $
        withFactionEngine $ \eng →
            withFixture "dup-pair-agree"
                (catalogueYaml ["acolyte", "nomad"]
                    [ "{ from: acolyte, to: nomad, relation: hostile }"
                    , "{ from: acolyte, to: nomad, relation: hostile }" ]) $ \path → do
                        out ← loadCatalogue eng path
                        refused "duplicate faction relation"
                                "acolyte → nomad" out

    it "refuses an authored SAME-TAG relation row: same-tag alliance is \
       \an engine rule and never data (requirement 3)" $
        withFactionEngine $ \eng →
            withFixture "self-relation"
                (catalogueYaml ["acolyte"]
                    ["{ pair: [acolyte, acolyte], relation: ally }"]) $ \path → do
                        out ← loadCatalogue eng path
                        refused "self-paired faction relation" "acolyte" out

    it "answers a refusal with exactly FOUR values and a healthy call \
       \with the one or two it always did — the fourth is this \
       \family's own reason, and only a refusal pushes it" $
        withFactionEngine $ \eng → do
            -- Arity is published contract: `executeDebugLua` tab-joins
            -- every value a chunk returns, so a value appended to a
            -- HEALTHY call would rewrite what a bare
            -- `return engine.loadFactionYaml(p)` reads back. Flora's
            -- own three-value refusal is pinned in
            -- "Test.Headless.Asset.FloraContent".
            withFixture "arity-refused"
                (catalogueYaml ["acolyte", "acolyte"] []) $ \path → do
                    arityOf eng "loadFactionYaml" (quoted path <> ", true")
                        `shouldReturn` "4"
                    arityOf eng "loadFactionYaml" (quoted path)
                        `shouldReturn` "1"
            withFixture "arity-healthy" (catalogueYaml ["acolyte"] []) $ \path → do
                arityOf eng "loadFactionYaml" (quoted path <> ", true")
                    `shouldReturn` "2"
                arityOf eng "loadFactionYaml" (quoted path)
                    `shouldReturn` "1"

    it "answers a DECODE failure as a parse failure, not a refusal, so \
       \the startup loader tells the two apart" $
        withFactionEngine $ \eng →
            withFixture "broken" "faction_tags: [\n" $ \path → do
                out ← loadCatalogue eng path
                loCount out `shouldBe` "0"
                loParsed out `shouldBe` "false"
                loDetail out `shouldBe` "nil"

-----------------------------------------------------------------------
-- Requirement 1/3: what a healthy catalogue means
-----------------------------------------------------------------------

catalogueShapeSpec ∷ Spec
catalogueShapeSpec = describe "the loaded authority" $ do

    it "expands a symmetric entry into BOTH directions" $
        withFactionEngine $ \eng →
            withFixture "symmetric"
                (catalogueYaml ["a", "b"]
                    ["{ pair: [a, b], relation: hostile }"]) $ \path → do
                        _ ← loadCatalogue eng path
                        pol ← cataloguePolicy <$> catalogueOf eng
                        baseRelationFor pol (tag "a") (tag "b")
                            `shouldBe` Just RelHostile
                        baseRelationFor pol (tag "b") (tag "a")
                            `shouldBe` Just RelHostile

    it "leaves a directed entry ONE-way" $
        withFactionEngine $ \eng →
            withFixture "directed"
                (catalogueYaml ["a", "b"]
                    ["{ from: a, to: b, relation: hostile }"]) $ \path → do
                        _ ← loadCatalogue eng path
                        pol ← cataloguePolicy <$> catalogueOf eng
                        baseRelationFor pol (tag "a") (tag "b")
                            `shouldBe` Just RelHostile
                        baseRelationFor pol (tag "b") (tag "a")
                            `shouldBe` Nothing

    it "counts tags AND relations, so a relations-only file does not \
       \read as empty" $
        withFactionEngine $ \eng →
            withFixture "counted"
                (catalogueYaml ["a", "b"]
                    ["{ pair: [a, b], relation: hostile }"]) $ \path → do
                        out ← loadCatalogue eng path
                        loCount out `shouldBe` "3"
                        loParsed out `shouldBe` "true"

    it "keeps a tag's authored description without letting it affect \
       \any rule" $
        withFactionEngine $ \eng →
            withFixture "described"
                (T.unpack (T.unlines
                    [ "faction_tags:"
                    , "  - id: a"
                    , "    description: \"what a means\""
                    , "relations: []" ])) $ \path → do
                        _ ← loadCatalogue eng path
                        cat ← catalogueOf eng
                        map ftdDescription (catalogueDeclarations cat)
                            `shouldBe` [Just "what a means"]

-----------------------------------------------------------------------
-- Requirement 4: the unit definition's own refusals
-----------------------------------------------------------------------

unitTagRefusalSpec ∷ Spec
unitTagRefusalSpec = describe "unit faction_tags refusals (requirement 4)" $ do

    it "refuses a unit file naming a tag the catalogue does not declare, \
       \through the Lua binding, exactly as flora's duplicate name does" $
        withShippedCatalogue $ \eng →
            withFixture "unit-undeclared"
                (unitYaml [("spec_2506_unit", Just ["red_tribe"])]) $ \path → do
                    out ← loadUnits eng path
                    refused "undeclared faction tag" "red_tribe" out
                    registeredDef eng "spec_2506_unit" `shouldReturn` Nothing

    it "refuses a duplicate tag in one unit's list" $
        withShippedCatalogue $ \eng →
            withFixture "unit-dup"
                (unitYaml [("spec_2506_unit", Just ["acolyte", "acolyte"])]) $ \path → do
                    out ← loadUnits eng path
                    refused "duplicate faction tag" "acolyte" out

    it "answers a unit refusal with exactly FOUR values too, and a \
       \healthy unit file with the one or two it always did" $
        withShippedCatalogue $ \eng → do
            withFixture "unit-arity-refused"
                (unitYaml [("spec_2506_arity", Just ["red_tribe"])]) $ \path → do
                    arityOf eng "loadUnitYaml" (quoted path <> ", true")
                        `shouldReturn` "4"
                    arityOf eng "loadUnitYaml" (quoted path)
                        `shouldReturn` "1"
            withFixture "unit-arity-healthy"
                (unitYaml [("spec_2506_arity_ok", Just ["acolyte"])]) $ \path → do
                    arityOf eng "loadUnitYaml" (quoted path <> ", true")
                        `shouldReturn` "2"
                    arityOf eng "loadUnitYaml" (quoted path)
                        `shouldReturn` "1"

    it "refuses a malformed tag id in a unit's list" $
        withShippedCatalogue $ \eng →
            withFixture "unit-bad"
                (unitYaml [("spec_2506_unit", Just ["two words"])]) $ \path → do
                    out ← loadUnits eng path
                    refused "malformed faction tag id" "two words" out

    it "completes validation BEFORE publishing anything: a valid \
       \definition ahead of an offending one is not registered, and \
       \the same valid definition alone IS" $
        withShippedCatalogue $ \eng → do
            logger ← quietLogger
            before ← registeredUnits eng
            withFixture "unit-order"
                (unitYaml [ ("spec_2506_good", Just ["acolyte"])
                          , ("spec_2506_bad",  Just ["red_tribe"]) ]) $ \path → do
                    defs ← loadUnitYaml logger path
                    map uydName defs `shouldBe`
                        ["spec_2506_good", "spec_2506_bad"]
                    runRegister eng path defs
                        `shouldReturn` Left "refused"
                    registeredUnits eng `shouldReturn` before
            -- The control: the SAME leading definition, with the
            -- offending one removed, really does register — so the
            -- assertion above is about the preflight and not about a
            -- definition that could never have been published.
            withFixture "unit-order-control"
                (unitYaml [("spec_2506_good", Just ["acolyte"])]) $ \path → do
                    defs ← loadUnitYaml logger path
                    runRegister eng path defs `shouldReturn` Right 1
                    registeredUnits eng
                        `shouldReturn` sort ("spec_2506_good" : before)

    it "names the offending unit definition as well as the tag" $ do
        logger ← quietLogger
        withFixture "unit-name"
            (unitYaml [("spec_2506_unit", Just ["acolyte"])]) $ \path → do
                defs ← loadUnitYaml logger path
                -- An EMPTY catalogue: `acolyte` is perfectly well
                -- formed and still undeclared, which is the case D-30
                -- exists for.
                case resolveUnitFactionTags emptyFactionCatalogue defs of
                    Right _ → expectationFailure
                        "an undeclared tag must be refused"
                    Left (unit, rejection) → do
                        unit `shouldBe` "spec_2506_unit"
                        rejectionReason rejection
                            `shouldBe` "undeclared faction tag"
                        rejectionDetail rejection `shouldBe` "acolyte"

-----------------------------------------------------------------------
-- Requirement 4: what a healthy unit file means
-----------------------------------------------------------------------

unitTagAdmissionSpec ∷ Spec
unitTagAdmissionSpec = describe "unit faction_tags admission" $ do

    it "loads an OMITTED faction_tags as empty defaults" $
        withShippedCatalogue $ \eng → do
            logger ← quietLogger
            withFixture "unit-omitted"
                (unitYaml [("spec_2506_plain", Nothing)]) $ \path → do
                    defs ← loadUnitYaml logger path
                    map uydFactionTags defs `shouldBe` [[]]
                    runRegister eng path defs `shouldReturn` Right 1
                    def ← registeredDef eng "spec_2506_plain"
                    fmap udFactionTags def `shouldBe` Just []

    it "carries a declared tag through to the UnitDef in authored order" $
        withShippedCatalogue $ \eng → do
            logger ← quietLogger
            withFixture "unit-two"
                (unitYaml [("spec_2506_two", Just ["nomad", "wildlife"])]) $ \path → do
                    defs ← loadUnitYaml logger path
                    _ ← runRegister eng path defs
                    def ← registeredDef eng "spec_2506_two"
                    fmap (map factionTagText ∘ udFactionTags) def
                        `shouldBe` Just ["nomad", "wildlife"]

-----------------------------------------------------------------------
-- Requirements 6 and 7: the shipped corpus
-----------------------------------------------------------------------

shippedCorpusSpec ∷ Spec
shippedCorpusSpec = describe "the shipped corpus" $ do

    it "loads data/factions/base.yaml through the real loader with \
       \exactly requirement 6's tags" $
        withShippedCatalogue $ \eng → do
            cat ← catalogueOf eng
            sort (map (factionTagText ∘ ftdTag) (catalogueDeclarations cat))
                `shouldBe` shippedTags

    it "declares exactly requirement 6's ordered hostile pairs and \
       \nothing else — so nomad/wildlife and nomad/legacy_hostile are \
       \neutral by silence" $
        withShippedCatalogue $ \eng → do
            cat ← catalogueOf eng
            let pairs = catalogueDeclaredPairs cat
                rendered = sort [ (factionTagText s, factionTagText t)
                                | ((s, t), _) ← Map.toList pairs ]
            rendered `shouldBe` sort shippedHostilePairs
            Set.fromList (Map.elems pairs) `shouldBe` Set.singleton RelHostile

    it "loads every shipped data/units/*.yaml through the real loaders \
       \with requirement 7's defaults" $
        withShippedCatalogue $ \eng → do
            logger ← quietLogger
            cat ← catalogueOf eng
            forM_ shippedDefaults $ \(name, expected) → do
                defs ← loadUnitYaml logger (shippedUnitPath name)
                map uydName defs `shouldBe` [name]
                case resolveUnitFactionTags cat defs of
                    Left (unit, rejection) →
                        expectationFailure (T.unpack
                            (name <> ": " <> unit <> " refused — "
                             <> rejectionReason rejection <> " '"
                             <> rejectionDetail rejection <> "'"))
                    Right resolved →
                        map (map factionTagText) resolved
                            `shouldBe` [expected]

    it "registers every shipped unit through engine.loadUnitYaml with \
       \the shipped catalogue loaded" $
        withShippedCatalogue $ \eng →
            forM_ shippedDefaults $ \(name, _) → do
                out ← loadUnits eng (shippedUnitPath name)
                loParsed out `shouldBe` "true"
                loDetail out `shouldBe` "nil"

-----------------------------------------------------------------------
-- Requirement 3: the namespace stays open
-----------------------------------------------------------------------

openNamespaceSpec ∷ Spec
openNamespaceSpec = describe "an undeclared runtime tag (requirement 3)" $ do

    -- The shipped table as FTS-1's policy sees it. Built from the real
    -- loaded catalogue rather than restated, because requirement 3 is
    -- about what the LOADED authority hands the policy.
    let withPolicy body = withShippedCatalogue $ \eng →
            body ∘ cataloguePolicy =≪ catalogueOf eng
        runtime = tag "fight_team_A"
        other   = tag "fight_team_B"

    it "is neutral toward an unrelated profile" $ withPolicy $ \pol → do
        let a = mkProfile Nothing [runtime] []
            b = mkProfile Nothing [other] []
        relationFromTo pol a b `shouldBe` RelNeutral

    it "is ALLIED with a profile that shares it, by the engine's \
       \same-tag rule rather than by any row" $ withPolicy $ \pol → do
        let a = mkProfile Nothing [runtime] []
            b = mkProfile Nothing [runtime, tag "acolyte"] []
        relationFromTo pol a b `shouldBe` RelAlly
        baseRelationFor pol runtime runtime `shouldBe` Nothing

    it "is still subject to a live directed cause" $ withPolicy $ \pol → do
        let a = mkProfile Nothing [runtime] []
            b = mkProfile Nothing [other] []
            live = addRelationCause (relationCauseId "staged_fight")
                       runtime other RelHostile pol
        relationFromTo live a b `shouldBe` RelHostile
        relationFromTo live b a `shouldBe` RelNeutral

    it "does not become declared merely by being evaluated" $
        withShippedCatalogue $ \eng → do
            cat ← catalogueOf eng
            declaresTag cat runtime `shouldBe` False

-----------------------------------------------------------------------
-- Requirement 5: the D-26 legacy mapping
-----------------------------------------------------------------------

legacyMappingSpec ∷ Spec
legacyMappingSpec = describe "the D-26 legacy mapping (requirement 5)" $ do

    let acolyteDefaults = [tag "acolyte"]
        profileFor defaults f = legacyProfile localPlayer defaults f

    it "maps all five values for a definition WITH authored defaults" $ do
        profileController (profileFor acolyteDefaults FactionPlayer)
            `shouldBe` Just localPlayer
        profileTags (profileFor acolyteDefaults FactionPlayer)
            `shouldBe` Set.fromList acolyteDefaults

        profileController (profileFor acolyteDefaults FactionWildlife)
            `shouldBe` Nothing
        profileTags (profileFor acolyteDefaults FactionWildlife)
            `shouldBe` Set.fromList acolyteDefaults

        profileFor acolyteDefaults FactionHostile
            `shouldBe` mkProfile Nothing [tagLegacyHostile] []
        profileFor acolyteDefaults FactionNeutral `shouldBe` emptyProfile
        profileFor acolyteDefaults FactionDebug
            `shouldBe` mkProfile Nothing [] allFactionCapabilities

    it "maps all five values for a definition WITHOUT defaults, where \
       \only the wildlife row falls back" $ do
        profileFor [] FactionPlayer
            `shouldBe` mkProfile (Just localPlayer) [] []
        profileFor [] FactionWildlife
            `shouldBe` mkProfile Nothing [tagWildlife] []
        profileFor [] FactionHostile
            `shouldBe` mkProfile Nothing [tagLegacyHostile] []
        profileFor [] FactionNeutral `shouldBe` emptyProfile
        profileFor [] FactionDebug
            `shouldBe` mkProfile Nothing [] allFactionCapabilities

    it "never reads a unit name: two definitions with the same defaults \
       \produce identical profiles" $ do
        let nomadish = [tag "nomad"]
        map (profileFor nomadish) [minBound .. maxBound]
            `shouldBe` map (profileFor nomadish) [minBound .. maxBound]

    it "is the mapping the SHIPPED acolyte definition feeds, end to end" $
        withShippedCatalogue $ \eng → do
            logger ← quietLogger
            cat ← catalogueOf eng
            defs ← loadUnitYaml logger (shippedUnitPath "acolyte")
            case resolveUnitFactionTags cat defs of
                Left _          → expectationFailure "acolyte was refused"
                Right [resolved] →
                    profileTags (profileFor resolved FactionPlayer)
                        `shouldBe` Set.fromList [tag "acolyte"]
                Right _         →
                    expectationFailure "acolyte.yaml declares one definition"

-----------------------------------------------------------------------
-- The family is a DIRECTORY, not a file
-----------------------------------------------------------------------

directoryScopeSpec ∷ Spec
directoryScopeSpec = describe "the family spans its whole directory" $ do

    -- One tag file and one relations-only file. `engine.listFiles`
    -- hands back RAW filesystem order, so the only thing that makes
    -- this tree mean the same on two machines is that admission does
    -- not depend on which of the two is enumerated first.
    let tagsFile = catalogueYaml ["acolyte", "nomad"] []
        relFile  = T.unpack (T.unlines
            [ "relations:"
            , "  - { pair: [acolyte, nomad], relation: hostile }" ])

        expectBothLoaded eng first second = do
            (loParsed <$> loadCatalogue eng first) `shouldReturn` "true"
            (loParsed <$> loadCatalogue eng second) `shouldReturn` "true"
            cat ← catalogueOf eng
            sort (map (factionTagText ∘ ftdTag) (catalogueDeclarations cat))
                `shouldBe` ["acolyte", "nomad"]
            let pol = cataloguePolicy cat
            baseRelationFor pol (tag "acolyte") (tag "nomad")
                `shouldBe` Just RelHostile
            baseRelationFor pol (tag "nomad") (tag "acolyte")
                `shouldBe` Just RelHostile

    it "admits a relations-only file loaded AFTER the file declaring \
       \its tags" $
        withFactionEngine $ \eng →
            withFixtureDir "order-forward"
                [("a_tags.yaml", tagsFile), ("b_relations.yaml", relFile)] $
                \at → expectBothLoaded eng (at "a_tags.yaml")
                                          (at "b_relations.yaml")

    it "admits the same two files in the OPPOSING enumeration order — \
       \a relations-only file loaded BEFORE the file declaring its \
       \tags" $
        withFactionEngine $ \eng →
            withFixtureDir "order-reverse"
                [("a_tags.yaml", tagsFile), ("b_relations.yaml", relFile)] $
                \at → expectBothLoaded eng (at "b_relations.yaml")
                                          (at "a_tags.yaml")

    it "still refuses an endpoint NO file in the directory declares, \
       \whichever order the two are loaded in" $
        withFactionEngine $ \eng →
            withFixtureDir "order-undeclared"
                [ ("a_tags.yaml", catalogueYaml ["acolyte"] [])
                , ("b_relations.yaml", relFile) ] $ \at → do
                    (loParsed <$> loadCatalogue eng (at "a_tags.yaml"))
                        `shouldReturn` "true"
                    out ← loadCatalogue eng (at "b_relations.yaml")
                    refused "undeclared faction tag" "nomad" out

    it "refuses a RELOAD that drops a tag another file's relation \
       \names, and leaves the registered catalogue exactly as it was" $
        withFactionEngine $ \eng →
            withFixtureDir "reload-integrity"
                [ ("a_tags.yaml", tagsFile), ("b_relations.yaml", relFile) ] $
                \at → do
                    let tags = at "a_tags.yaml"
                    expectBothLoaded eng tags (at "b_relations.yaml")
                    before ← catalogueOf eng
                    -- The replacement document is FAULTLESS in itself —
                    -- one well-formed tag, no relations — and the
                    -- catalogue it would produce is not, because
                    -- b_relations.yaml still names `nomad`.
                    writeFile tags (catalogueYaml ["acolyte"] [])
                    out ← loadCatalogue eng tags
                    refused "undeclared faction tag" "nomad" out
                    catalogueOf eng `shouldReturn` before

    it "accepts a reload that drops nothing anyone depends on, \
       \replacing that file's own contribution rather than colliding \
       \with it" $
        withFactionEngine $ \eng →
            withFixtureDir "reload-clean"
                [("a_tags.yaml", catalogueYaml ["acolyte", "nomad"] [])] $
                \at → do
                    let tags = at "a_tags.yaml"
                    (loParsed <$> loadCatalogue eng tags) `shouldReturn` "true"
                    writeFile tags (catalogueYaml ["acolyte", "wildlife"] [])
                    out ← loadCatalogue eng tags
                    loParsed out `shouldBe` "true"
                    loDetail out `shouldBe` "nil"
                    cat ← catalogueOf eng
                    sort (map (factionTagText ∘ ftdTag)
                              (catalogueDeclarations cat))
                        `shouldBe` ["acolyte", "wildlife"]
