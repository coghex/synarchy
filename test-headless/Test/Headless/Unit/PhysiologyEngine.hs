{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The engine-backed half of the @Unit.Physiology@ gate (#2828): the
--   real loader, the unit registration boundary and the registered Lua
--   bindings, all on a live headless engine with the REAL @unit.*@ and
--   @engine.*@ API registered.
--
--   Everything here asserts on what the ENGINE did. Whole-file refusal
--   is a claim about what 'registerUnitDefs' published, so the
--   refusal examples read the unit manager back; read purity is a claim
--   about state, so those examples snapshot stats, modifiers, the stat
--   generator and the definitions' physiology before and after.
--
--   The stat generator is always set explicitly (@mkStdGen 2828@), never
--   entropy-seeded: an example whose answer depends on a roll would flake
--   at the roll's rate.
module Test.Headless.Unit.PhysiologyEngine (engineSpec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS
import System.Directory (createDirectoryIfMissing)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, sort)
import System.FilePath ((</>))
import System.Random (StdGen, mkStdGen)
import Engine.Asset.Discovery (walkFilesWithExtension)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Asset.Types (defaultAssetPool)
import Engine.Asset.YamlUnits (loadUnitYaml)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Init (EngineInitResult(..))
import qualified Engine.Core.Queue as Q
import Engine.Core.Log
    ( LogBackend(..), LogConfig(..), LoggerState, defaultLogConfig
    , initLogger )
import Engine.Core.State
    ( EngineEnv, assetPoolRef, enginePausedRef, inputStateRef, loadStatusRef
    , loggerRef, luaQueue, luaToEngineQueue, nextObjectIdRef )
import Engine.Load.Status
    (LoadOutcome(..), LoadPhase(..), LoadStatus(..), readLoadStatus)
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.API.YamlResult (YamlRefusal(..))
import Engine.Scripting.Lua.API.Units.Physiology
    (PhysioReadResult(..), physioRead, physioReadBase)
import Engine.Scripting.Lua.API.Units.Yaml (AtlasResolver, registerUnitDefs)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine)
import Test.Headless.Harness.Isolation
    (withExclusiveTempDirectory, withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Test.Headless.Unit.PhysiologyFixture
    ( ensurePhysiologyRegistry, shippedPhysiologyPath )
import Test.Headless.Unit.TransferApi (minimalDef)
import Test.Headless.World.Save.Components.Fixture
    ( minimalGlobals, minimalPage, minimalSimState, minimalUnitInstance
    , page1 )
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Faction.Membership (resolveLegacyFaction)
import Unit.Physiology.Types
import Unit.Types
import World.Page.Types (WorldPageId(..))
import World.Save.Envelope (encodeSessionSnapshot)
import World.Save.Snapshot
    (PageSnapshot(..), captureSessionSnapshot)
import World.Save.Snapshot.Adapter
    (SaveRequestMeta(..), snapshotSaveMetadata)
import World.Save.Types (UnitSnapshot(..), UnitInstanceSnapshot(..))

-----------------------------------------------------------------------
-- The engine
-----------------------------------------------------------------------

data PhysioEngine = PhysioEngine
    { peEnv ∷ EngineEnv
    , peLua ∷ LuaBackendState
    }

-- | A throwaway headless engine with the real Lua API registered, in a
--   private scratch resource root (the boot writes @config\/@, #1357).
--   The registry is loaded only when asked, because "no registry
--   loaded" is itself a state under test.
withPhysioEngine ∷ Bool → (PhysioEngine → Expectation) → Expectation
withPhysioEngine loadRegistry action = withIsolatedResourceRoot $ do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    logger ← quietLogger
    writeIORef (loggerRef env) logger
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    when loadRegistry (ensurePhysiologyRegistry env)
    let eng = PhysioEngine env ls
    _ ← luaText eng
        "function _G.__p(v, m) return (v == nil and 'nil' or \
        \string.format('%.4f', v)) .. '|' .. tostring(m) end; return true"
    action eng

quietLogger ∷ IO LoggerState
quietLogger = initLogger defaultLogConfig
    { lcBackend = LogToCallback (\_ → pure ()) }

-- | Evaluate a Lua chunk that returns ONE string, without its JSON
--   quotes.
luaText ∷ PhysioEngine → Text → IO Text
luaText eng src =
    T.strip ∘ T.dropAround (≡ '"') <$> executeDebugLua (lbsLuaState (peLua eng)) src

-- | What a read verb answered, as @value|mode@ (value to four places).
readsAs ∷ PhysioEngine → Text → IO Text
readsAs eng call = luaText eng ("return __p(" <> call <> ")")

-----------------------------------------------------------------------
-- Scenes
-----------------------------------------------------------------------

-- | The reference 70 kg unit's inputs.
inputs ∷ [(Text, Float)]
inputs = [ ("body_mass", 70), ("max_hydration", 42)
         , ("max_hunger", 700), ("max_calories", 1400) ]

-- | Stored owned values, as a Simulated unit carries them.
ownedValues ∷ [(Text, Float)]
ownedValues = [ ("hydration", 21), ("salt", 35), ("salt_store", 100)
              , ("core_temp", 38), ("hunger", 300), ("calories", 900) ]

mkInstance ∷ Text → [(Text, Float)] → UnitInstance
mkInstance defName stats = UnitInstance
    { uiDefName = defName, uiName = "", uiPage = WorldPageId "physio_page"
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 10, uiGridY = 10
    , uiGridZ = 0, uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.fromList stats
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = []
    , uiFaction = resolveLegacyFaction [] FactionPlayer
    , uiWounds = [], uiScars = [], uiImmuneResponse = 0
    , uiImmunities = HM.empty, uiBlood = 70 * 0.075
    , uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiPhysioOverrides = emptyPhysioOverrides, uiTrailState = Nothing
    }

-- | A definition with the given physiology and lazy templates.
mkDef ∷ Text → UnitPhysiology → [(Text, (Float, Float))] → UnitDef
mkDef name up templates = (minimalDef name name)
    { udPhysiology = up, udStatTemplates = HM.fromList templates }

-- | Install defs and instances (unit ids 1, 2, ...), the game clock and
--   an explicit stat generator.
installScene ∷ PhysioEngine → [UnitDef] → [UnitInstance] → Double → StdGen
             → IO ()
installScene eng defs insts now gen = do
    let env = peEnv eng
    writeIORef (ucUnitManagerRef (toUnitCombatCapability env)) emptyUnitManager
        { umDefs = HM.fromList [ (udName d, d) | d ← defs ]
        , umInstances = HM.fromList (zip (map UnitId [1 ..]) insts) }
    writeIORef (wsGameTimeRef (toWorldSimCapability env)) now
    writeIORef (ucStatRNGRef (toUnitCombatCapability env)) gen

rngSeed ∷ StdGen
rngSeed = mkStdGen 2828

-- | Everything a read must leave alone, for unit 1.
data Snapshot = Snapshot
    { snStats ∷ Maybe (HM.HashMap Text Float)
    , snModifiers ∷ Maybe (HM.HashMap Text [StatModifier])
    , snOverrides ∷ Maybe PhysioOverrides
    , snDefs ∷ HM.HashMap Text UnitDef
    , snRng ∷ StdGen
    } deriving (Show, Eq)

snapshot ∷ PhysioEngine → IO Snapshot
snapshot eng = do
    let cap = toUnitCombatCapability (peEnv eng)
    um ← readIORef (ucUnitManagerRef cap)
    gen ← readIORef (ucStatRNGRef cap)
    let inst = HM.lookup (UnitId 1) (umInstances um)
    pure Snapshot { snStats = uiStats <$> inst, snModifiers = uiModifiers <$> inst
                  , snOverrides = uiPhysioOverrides <$> inst
                  , snDefs = umDefs um, snRng = gen }

-- | The standard fixture: unit 1 bodied with Simulated hydration and
--   salt and a lazy template, unit 2 bodyless, unit 3 bodied with no
--   authored defaults.
standardScene ∷ PhysioEngine → [(Text, Float)] → IO ()
standardScene eng stats1 = installScene eng
    [ mkDef "bodied" (plainUnitPhysiology True (Just "reduced")
          (HM.fromList [ ("hydration", PhysioSimulated)
                       , ("salt", PhysioSimulated)
                       , ("nutrition", PhysioNominal) ]))
          [("custom_lazy", (10, 4))]
    , mkDef "bare" noUnitPhysiology []
    , mkDef "plain" (plainUnitPhysiology True Nothing HM.empty) [] ]
    [ mkInstance "bodied" stats1
    , mkInstance "bare" (inputs ⧺ ownedValues)
    , mkInstance "plain" inputs ]
    0 rngSeed

standardStats ∷ [(Text, Float)]
standardStats = inputs ⧺ ownedValues ⧺ [("max_stamina", 80)]

-----------------------------------------------------------------------
-- Unit files
-----------------------------------------------------------------------

-- | A one-definition unit file; @extra@ lines are indented under it.
unitFile ∷ Text → [Text] → String
unitFile name extra = T.unpack $ T.unlines $
    [ "units:", "  - name: " <> name
    , "    sprite: assets/textures/utility/blanktexture.png" ]
    ⧺ map ("    " <>) extra

withUnitFile ∷ String → String → (FilePath → Expectation) → Expectation
withUnitFile label body action =
    withExclusiveTempDirectory ("synarchy-2828-" ⧺ label) $ \dir → do
        let path = dir </> "units.yaml"
        writeFile path body
        action path

alwaysResolves ∷ AtlasResolver
alwaysResolves _ _ = pure (Right HM.empty)

-- | Decode a unit file and register it through the production
--   registration path with atlas resolution stubbed out.
registerFile ∷ PhysioEngine → FilePath → IO (Either YamlRefusal Int)
registerFile eng path = do
    logger ← quietLogger
    defs ← loadUnitYaml logger path
    poolRef ← newIORef =≪ defaultAssetPool
    q ← Q.newQueue
    registerUnitDefs (peEnv eng) poolRef q alwaysResolves path defs

registeredDef ∷ PhysioEngine → Text → IO (Maybe UnitDef)
registeredDef eng name = do
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability (peEnv eng)))
    pure (HM.lookup name (umDefs um))

registeredNames ∷ PhysioEngine → IO [Text]
registeredNames eng = do
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability (peEnv eng)))
    pure (sort (HM.keys (umDefs um)))

refusal ∷ Text → Text → Either YamlRefusal Int
refusal reason detail = Left (YamlRefusal (Just reason) detail)

quoted ∷ FilePath → Text
quoted p = "'" <> T.pack p <> "'"

-----------------------------------------------------------------------
-- The gate
-----------------------------------------------------------------------

engineSpec ∷ Spec
engineSpec = do
    loaderSpec
    registrationSpec
    shippedRosterSpec
    surfaceSpec
    modeVerbSpec
    loadBoundarySpec
    readVerbSpec
    unownedSpec
    registryAccessorSpec

-----------------------------------------------------------------------
-- Requirement 1: the loader
-----------------------------------------------------------------------

loaderSpec ∷ Spec
loaderSpec = describe "engine.loadPhysiologyYaml (requirement 1)" $ do

    it "registers the shipped registry, answering the system count and, \
       \when asked, the parsed flag" $
        withPhysioEngine False $ \eng → do
            luaText eng ("return tostring(select('#', engine.loadPhysiologyYaml(\
                         \" <> quoted shippedPhysiologyPath <> ", true)))")
                `shouldReturn` "2"
            luaText eng ("local n, p = engine.loadPhysiologyYaml(\
                         \" <> quoted shippedPhysiologyPath <> ", true); \
                         \return string.format('%d|%s', n, tostring(p))")
                `shouldReturn` "7|true"
            luaText eng ("return tostring(select('#', engine.loadPhysiologyYaml(\
                         \" <> quoted shippedPhysiologyPath <> ")))")
                `shouldReturn` "1"
            reg ← readIORef (crPhysiologyRegistryRef
                      (toContentRegistriesCapability (peEnv eng)))
            map psId (prSystems reg)
                `shouldBe` ["thermo", "salt", "hydration", "nutrition", "stamina", "exhaustion", "sleep"]
            prSource reg `shouldBe` Just shippedPhysiologyPath

    it "reports a malformed file as NOT parsed, at zero, and registers \
       \nothing" $
        withPhysioEngine False $ \eng →
            withUnitFile "malformed" "systems: [ unterminated\n" $ \path → do
                luaText eng ("local n, p = engine.loadPhysiologyYaml(\
                             \" <> quoted path <> ", true); \
                             \return string.format('%d|%s', n, tostring(p))")
                    `shouldReturn` "0|false"
                reg ← readIORef (crPhysiologyRegistryRef
                          (toContentRegistriesCapability (peEnv eng)))
                physiologyRegistryLoaded reg `shouldBe` False

    it "refuses a decoded but unusable registry with FOUR values, naming \
       \the offender, and registers nothing" $
        withPhysioEngine False $ \eng →
            withUnitFile "refused"
                "systems:\n  - id: a\n    requires: [wings]\n    owns:\n      - \
                \{ key: k, healthy: { const: 1.0 } }\n" $ \path → do
                luaText eng ("return tostring(select('#', \
                             \engine.loadPhysiologyYaml(" <> quoted path
                             <> ", true)))") `shouldReturn` "4"
                luaText eng ("local n, p, d, r = engine.loadPhysiologyYaml(\
                             \" <> quoted path <> ", true); \
                             \return string.format('%d|%s|%s|%s', n, \
                             \tostring(p), d, r)")
                    `shouldReturn`
                    "0|true|a: wings|unknown physiology capability"
                reg ← readIORef (crPhysiologyRegistryRef
                          (toContentRegistriesCapability (peEnv eng)))
                physiologyRegistryLoaded reg `shouldBe` False

    it "replaces the registry on a reload of the SAME path, refuses a \
       \DIFFERENT path, and leaves the registered registry intact on a \
       \refusal" $
        withPhysioEngine False $ \eng →
            withUnitFile "reload"
                "systems:\n  - id: only\n    owns:\n      - \
                \{ key: k, healthy: { const: 1.0 } }\n" $ \path → do
                let load p = luaText eng
                        ("local n, _, d, r = engine.loadPhysiologyYaml(\
                         \" <> quoted p <> ", true); \
                         \return string.format('%d|%s|%s', n, tostring(d), \
                         \tostring(r))")
                    systemsNow = do
                        reg ← readIORef (crPhysiologyRegistryRef
                                  (toContentRegistriesCapability (peEnv eng)))
                        pure (map psId (prSystems reg))
                load shippedPhysiologyPath `shouldReturn` "7|nil|nil"
                -- the same path again is a reload, not a collision
                load shippedPhysiologyPath `shouldReturn` "7|nil|nil"
                -- a different file is never merged into the first
                load path `shouldReturn`
                    "0|" <> T.pack shippedPhysiologyPath
                        <> "|duplicate physiology registry"
                systemsNow `shouldReturn`
                    ["thermo", "salt", "hydration", "nutrition", "stamina", "exhaustion", "sleep"]

    it "refuses a registry carrying a non-finite literal, naming it, and \
       \leaves the previously registered registry untouched" $
        withPhysioEngine False $ \eng →
            withUnitFile "non-finite"
                "systems:\n  - id: only\n    owns:\n      - \
                \{ key: k, healthy: { const: 1.0e999 } }\n" $ \path → do
                let load p = luaText eng
                        ("local n, _, d, r = engine.loadPhysiologyYaml(\
                         \" <> quoted p <> ", true); \
                         \return string.format('%d|%s|%s', n, tostring(d), \
                         \tostring(r))")
                load shippedPhysiologyPath `shouldReturn` "7|nil|nil"
                -- the content decides: validation runs before the
                -- one-registry check
                load path `shouldReturn`
                    "0|only.k: Infinity|non-finite physiology value"
                reg ← readIORef (crPhysiologyRegistryRef
                          (toContentRegistriesCapability (peEnv eng)))
                map psId (prSystems reg)
                    `shouldBe` ["thermo", "salt", "hydration", "nutrition", "stamina", "exhaustion", "sleep"]
                prSource reg `shouldBe` Just shippedPhysiologyPath

-----------------------------------------------------------------------
-- Requirements 2, 3 and 5: the registration boundary
-----------------------------------------------------------------------

registrationSpec ∷ Spec
registrationSpec = describe "unit registration (requirements 2 and 5)" $ do

    it "REFUSES to register with no registry loaded, rather than skipping \
       \the template check, and publishes nothing" $
        withPhysioEngine False $ \eng →
            withUnitFile "no-registry" (unitFile "spec_2828_unit" []) $ \path → do
                registerFile eng path `shouldReturn`
                    refusal "physiology registry not loaded"
                            "data/physiology/systems.yaml"
                registeredNames eng `shouldReturn` []
                -- and through the real binding, with all four values
                luaText eng ("return tostring(select('#', \
                             \engine.loadUnitYaml(" <> quoted path
                             <> ", true)))") `shouldReturn` "4"

    forM_ [ ("stats", "hydration"), ("stats", "salt_store")
          , ("stats", "core_temp"), ("stats", "hunger")
          , ("stats", "salt_conc"), ("stats", "hypothermia")
          , ("skills", "salt"), ("skills", "calories")
          , ("skills", "salt_conc"), ("skills", "hyperthermia") ] $
        \(section, key) →
            it ("refuses a " <> T.unpack section <> " template naming the \
                \registry key " <> T.unpack key <> ", identifying the \
                \definition and the key, and publishes nothing") $
                withPhysioEngine True $ \eng →
                    withUnitFile "template"
                        (unitFile "spec_2828_unit"
                            [ section <> ":"
                            , "  " <> key <> ": { base: 1.0, range: 0.0 }" ]) $
                        \path → do
                            registerFile eng path `shouldReturn`
                                refusal "physiology key in unit template"
                                    ("spec_2828_unit: " <> section <> "." <> key)
                            registeredNames eng `shouldReturn` []

    it "accepts unowned templates in stats and skills, lazy ones included, \
       \and a shipped-style definition" $
        withPhysioEngine True $ \eng →
            withUnitFile "unowned"
                (unitFile "spec_2828_unit"
                    [ "body: {}", "stats:"
                    , "  custom_lazy: { base: 1.0, range: 0.5 }"
                    , "  heart_rate: { base: 100.0, range: 0.0 }"
                    , "skills:"
                    , "  custom_skill: { base: 1.0, range: 0.0 }" ]) $ \path → do
                registerFile eng path `shouldReturn` Right 1
                registeredNames eng `shouldReturn` ["spec_2828_unit"]

    it "completes validation BEFORE publishing: a valid definition ahead \
       \of an offending one is not registered, and alone it IS" $
        withPhysioEngine True $ \eng → do
            let two = T.unpack $ T.unlines
                    [ "units:"
                    , "  - name: spec_2828_good"
                    , "    sprite: assets/textures/utility/blanktexture.png"
                    , "  - name: spec_2828_bad"
                    , "    sprite: assets/textures/utility/blanktexture.png"
                    , "    stats:"
                    , "      hydration: { base: 1.0 }" ]
            withUnitFile "order" two $ \path → do
                registerFile eng path `shouldReturn`
                    refusal "physiology key in unit template"
                            "spec_2828_bad: stats.hydration"
                registeredNames eng `shouldReturn` []
            withUnitFile "order-control" (unitFile "spec_2828_good" []) $
                \path → do
                    registerFile eng path `shouldReturn` Right 1
                    registeredNames eng `shouldReturn` ["spec_2828_good"]

    it "keeps the faction-tag refusal ahead of the physiology one" $
        withPhysioEngine False $ \eng →
            withUnitFile "precedence"
                (unitFile "spec_2828_unit" ["faction_tags: [red_tribe]"]) $
                \path → do
                    registerFile eng path `shouldReturn`
                        refusal "undeclared faction tag" "red_tribe"

    describe "the physiology block (requirement 2)" $ do

        it "expands a profile and lets a per-system entry override it" $
            withPhysioEngine True $ \eng →
                withUnitFile "block"
                    (unitFile "spec_2828_unit"
                        [ "body: {}"
                        , "physiology: { profile: reduced, thermo: nominal, \
                          \hydration: simulated }" ]) $ \path → do
                    registerFile eng path `shouldReturn` Right 1
                    def ← registeredDef eng "spec_2828_unit"
                    fmap udPhysiology def `shouldBe` Just UnitPhysiology
                        { upBodyAuthored = True, upProfile = Just "reduced"
                        , upDefaults = HM.fromList
                            [ ("hydration", PhysioSimulated)   -- entry wins
                            , ("salt", PhysioSimulated)
                            , ("thermo", PhysioNominal)        -- entry wins
                            , ("nutrition", PhysioNominal)
                            , ("stamina", PhysioSimulated)
                            , ("exhaustion", PhysioSimulated)
                            , ("sleep", PhysioSimulated) ]
                        , upEngagement = HM.empty }

        it "accepts entries without a profile, and a profile without \
           \entries" $
            withPhysioEngine True $ \eng → do
                withUnitFile "entries-only"
                    (unitFile "spec_2828_a"
                        ["body: {}", "physiology: { salt: simulated }"]) $
                    \path → registerFile eng path `shouldReturn` Right 1
                withUnitFile "profile-only"
                    (unitFile "spec_2828_b"
                        ["body: {}", "physiology: { profile: exempt }"]) $
                    \path → registerFile eng path `shouldReturn` Right 1
                fmap (upDefaults ∘ udPhysiology) <$> registeredDef eng "spec_2828_a"
                    `shouldReturn` Just (HM.fromList [("salt", PhysioSimulated)])
                fmap (HM.size ∘ upDefaults ∘ udPhysiology)
                    <$> registeredDef eng "spec_2828_b"
                    `shouldReturn` Just 7

        it "leaves a definition with no block with NO authored defaults, \
           \and a bodyless one without the body capability" $
            withPhysioEngine True $ \eng → do
                withUnitFile "no-block"
                    (unitFile "spec_2828_a" ["body: {}"]) $ \path →
                    registerFile eng path `shouldReturn` Right 1
                withUnitFile "bodyless" (unitFile "spec_2828_b" []) $ \path →
                    registerFile eng path `shouldReturn` Right 1
                fmap udPhysiology <$> registeredDef eng "spec_2828_a"
                    `shouldReturn` Just (plainUnitPhysiology True Nothing HM.empty)
                fmap udPhysiology <$> registeredDef eng "spec_2828_b"
                    `shouldReturn` Just noUnitPhysiology
                -- `body:` with nothing under it declares no body
                withUnitFile "null-body"
                    (unitFile "spec_2828_c" ["body:"]) $ \path →
                    registerFile eng path `shouldReturn` Right 1
                fmap (upBodyAuthored ∘ udPhysiology) <$> registeredDef eng
                    "spec_2828_c" `shouldReturn` Just False

        forM_ [ ("an unknown profile", "physiology: { profile: royal }"
                , "unknown physiology profile", "spec_2828_unit: royal")
              , ("an unknown system id", "physiology: { ghost: nominal }"
                , "unknown physiology system", "spec_2828_unit: ghost")
              , ("an invalid mode", "physiology: { thermo: pinned }"
                , "invalid physiology mode", "spec_2828_unit: thermo: pinned")
              , ("a non-text mode", "physiology: { thermo: 5 }"
                , "invalid physiology mode", "spec_2828_unit: thermo: 5")
              ] $ \(what, block, reason, detail) →
            it ("refuses " <> what <> ", naming the definition, and \
                \publishes nothing") $
                withPhysioEngine True $ \eng →
                    withUnitFile "block-refused"
                        (unitFile "spec_2828_unit" ["body: {}", block]) $
                        \path → do
                            registerFile eng path `shouldReturn`
                                refusal reason detail
                            registeredNames eng `shouldReturn` []

-----------------------------------------------------------------------
-- The shipped roster
-----------------------------------------------------------------------

-- | The per-system defaults a shipped definition's profile expands to.
shippedDefaults ∷ Text → HM.HashMap Text PhysioMode
shippedDefaults name = HM.fromList $ case name of
    "acolyte"         → [ (s, PhysioSimulated) | s ← allSystems ]
    "nomad_primitive" → [ (s, PhysioNominal)   | s ← allSystems ]
    -- the `simple` profile (SD-10, #2833): the squirrel's salt is Nominal
    "red_squirrel"    → [ ("hydration", PhysioNominal), ("salt", PhysioNominal)
                        , ("thermo", PhysioSimulated), ("nutrition", PhysioNominal)
                        , ("stamina", PhysioSimulated)
                        -- S8b (#2835): no go_to_sleep behavior, no sleep need
                        , ("exhaustion", PhysioSimulated), ("sleep", PhysioNominal) ]
    "bear_brown" →
        [ ("hydration", PhysioNominal), ("salt", PhysioSimulated)
        , ("thermo", PhysioSimulated), ("nutrition", PhysioNominal)
        , ("stamina", PhysioSimulated), ("exhaustion", PhysioSimulated)
        , ("sleep", PhysioSimulated) ]
    -- the mule has no stamina pool configuration: Nominal (S8-F1, #2834)
    "technomule" →
        [ ("hydration", PhysioNominal), ("salt", PhysioSimulated)
        , ("thermo", PhysioSimulated), ("nutrition", PhysioNominal)
        , ("stamina", PhysioNominal)
        -- no fatigue or sleep-debt pool configuration either (#2835)
        , ("exhaustion", PhysioNominal), ("sleep", PhysioNominal) ]
    _                 → []
  where allSystems = [ "hydration", "salt", "thermo", "nutrition", "stamina"
                     , "exhaustion", "sleep" ]

bodiedRoster, bodylessRoster ∷ [Text]
bodiedRoster = [ "acolyte", "bear_brown", "nomad_primitive", "red_squirrel"
               , "technomule" ]
bodylessRoster = ["tiller", "unknown_unit", "white_tailed_deer"]

shippedRosterSpec ∷ Spec
shippedRosterSpec = describe "the shipped roster (requirements 2 and 6)" $ do

    it "registers every shipped definition through the real binding, \
       \resolves all four systems to Unsupported for the three bodyless \
       \ones, and to something else for the five bodied ones" $
        withPhysioEngine True $ \eng → do
            _ ← luaText eng "return engine.loadFactionYaml('data/factions/base.yaml')"
            forM_ (bodiedRoster ⧺ bodylessRoster) $ \name → do
                out ← luaText eng
                    ("local n, p, d = engine.loadUnitYaml('data/units/"
                     <> name <> ".yaml', true); \
                     \return string.format('%d|%s|%s', n, tostring(p), \
                     \tostring(d))")
                out `shouldBe` "1|true|nil"
            defs ← forM (bodiedRoster ⧺ bodylessRoster) $ \name → do
                d ← registeredDef eng name
                pure (name, d)
            forM_ defs $ \(name, mDef) → case mDef of
                Nothing → expectationFailure
                    (T.unpack name ⧺ " did not register")
                Just d → do
                    upBodyAuthored (udPhysiology d)
                        `shouldBe` (name `elem` bodiedRoster)
                    -- the profile each bodied definition authors (#2831,
                    -- design D-10); a bodyless one authors none
                    upDefaults (udPhysiology d) `shouldBe` shippedDefaults name
            -- and through the registered Lua read bindings
            let defList = [ d | (_, Just d) ← defs ]
            installScene eng defList
                [ mkInstance (udName d) inputs | d ← defList ] 0 rngSeed
            forM_ (zip [1 ∷ Int ..] defList) $ \(i, d) →
                forM_ ["hydration", "salt", "thermo", "nutrition", "stamina"] $ \sys → do
                    got ← luaText eng ("return tostring(unit.getPhysioMode("
                                       <> tshow i <> ", '" <> sys <> "'))")
                    got `shouldBe`
                        (if udName d `elem` bodylessRoster
                            then "nil"
                            else maybe "nominal" physioModeText
                                    (HM.lookup sys (shippedDefaults (udName d))))

-----------------------------------------------------------------------
-- Requirement 4: the installed surface
-----------------------------------------------------------------------

surfaceSpec ∷ Spec
surfaceSpec = describe "the installed read surface (requirements 4 and 6)" $ do

    it "installs the five read verbs and the two mutating verbs (#2831)" $
        withPhysioEngine True $ \eng →
            luaText eng
                "local names = {}; for k, v in pairs(unit) do \
                \if k:find('Physio') then names[#names + 1] = k end end; \
                \table.sort(names); return table.concat(names, ',')"
                `shouldReturn`
                "clearPhysioMode,getPhysioDefault,getPhysioMode,\
                \getPhysioOverride,readPhysio,readPhysioBase,setPhysioMode"

    it "reaches the verbs from scripts/ only through the S5 facade (#2830), \
       \and the wound tick reads through the pure consumer view" $ do
        luas ← walkFilesWithExtension "scripts" ".lua"
        -- relative to the directory walked
        users ← fmap catMaybes $ forM luas $ \p → do
            body ← TIO.readFile ("scripts" </> p)
            pure $ if any (`T.isInfixOf` body)
                         ["readPhysio", "getPhysioMode", "getPhysioDefault"
                         , "getPhysioOverride", "getPhysiologyRegistry"]
                       then Just p else Nothing
        users `shouldBe` ["physiology.lua"]
        healing ← readFile "src/Combat/Wounds/Healing.hs"
        ("physioRead" `isInfixOf` healing) `shouldBe` False
        tick ← readFile "src/Combat/Wounds/Tick.hs"
        ("physioConsumerView" `isInfixOf` tick) `shouldBe` True

-----------------------------------------------------------------------
-- Requirement 4: getPhysioMode / Override / Default
-----------------------------------------------------------------------

modeVerbSpec ∷ Spec
modeVerbSpec = describe "getPhysioMode / getPhysioOverride / getPhysioDefault" $ do

    it "resolves the mode from the capability and the authored defaults" $
        withPhysioEngine True $ \eng → do
            standardScene eng standardStats
            let mode uid sys = luaText eng
                    ("return tostring(unit.getPhysioMode(" <> uid <> ", '"
                     <> sys <> "'))")
            -- unit 1: bodied, defaults hydration/salt Simulated
            mode "1" "hydration" `shouldReturn` "simulated"
            mode "1" "salt" `shouldReturn` "simulated"
            mode "1" "nutrition" `shouldReturn` "nominal"
            mode "1" "thermo" `shouldReturn` "nominal"   -- profile gave none
            -- unit 2: bodyless — Unsupported reads as nil, every system
            forM_ ["hydration", "salt", "thermo", "nutrition"] $ \s →
                mode "2" s `shouldReturn` "nil"
            -- unit 3: bodied, no defaults — Nominal, never nil
            mode "3" "thermo" `shouldReturn` "nominal"
            -- unknown system id and nonexistent unit
            mode "1" "ghost" `shouldReturn` "nil"
            mode "99" "hydration" `shouldReturn` "nil"

    it "reports the authored default, or nil when there is none — the \
       \final Nominal fallback is not a default" $
        withPhysioEngine True $ \eng → do
            standardScene eng standardStats
            let dflt uid sys = luaText eng
                    ("return tostring(unit.getPhysioDefault(" <> uid <> ", '"
                     <> sys <> "'))")
            dflt "1" "hydration" `shouldReturn` "simulated"
            dflt "1" "nutrition" `shouldReturn` "nominal"
            dflt "1" "thermo" `shouldReturn` "nil"
            dflt "3" "hydration" `shouldReturn` "nil"
            dflt "2" "hydration" `shouldReturn` "nil"
            dflt "1" "ghost" `shouldReturn` "nil"
            dflt "99" "hydration" `shouldReturn` "nil"

    it "reports no override for a unit this build created: the map is \
       \empty and no setter exists" $
        withPhysioEngine True $ \eng → do
            standardScene eng standardStats
            forM_ ["1", "2", "3", "99"] $ \uid →
                forM_ ["hydration", "salt", "thermo", "nutrition", "ghost"] $
                    \sys → luaText eng
                        ("return tostring(unit.getPhysioOverride(" <> uid
                         <> ", '" <> sys <> "'))")
                        `shouldReturn` "nil"

    it "reflects a STORED override exactly (#2829): present means \
       \explicit intent, even when it equals the species default, and an \
       \absent entry still follows the default" $
        withPhysioEngine True $ \eng → do
            -- unit 1: default hydration/salt Simulated, nutrition Nominal.
            -- Overrides: hydration Simulated (EQUAL to the default),
            -- nutrition Simulated (against it), thermo Nominal (no
            -- default at all).
            standardScene eng standardStats
            let cap = toUnitCombatCapability (peEnv eng)
            atomicModifyIORef' (ucUnitManagerRef cap) $ \um →
                (um { umInstances = HM.adjust
                        (\i → i { uiPhysioOverrides = HM.fromList
                            [ ("hydration", PhysioSimulated)
                            , ("nutrition", PhysioSimulated)
                            , ("thermo", PhysioNominal) ] })
                        (UnitId 1) (umInstances um) }, ())
            let ovr sys = luaText eng
                    ("return tostring(unit.getPhysioOverride(1, '" <> sys
                     <> "'))")
                mode sys = luaText eng
                    ("return tostring(unit.getPhysioMode(1, '" <> sys <> "'))")
                dflt sys = luaText eng
                    ("return tostring(unit.getPhysioDefault(1, '" <> sys
                     <> "'))")
            ovr "hydration" `shouldReturn` "simulated"
            ovr "nutrition" `shouldReturn` "simulated"
            ovr "thermo" `shouldReturn` "nominal"
            -- absent: no override reported, and the default still rules
            ovr "salt" `shouldReturn` "nil"
            mode "salt" `shouldReturn` "simulated"
            -- an override wins over the default it disagrees with …
            mode "nutrition" `shouldReturn` "simulated"
            dflt "nutrition" `shouldReturn` "nominal"
            -- … and over no default at all
            mode "thermo" `shouldReturn` "nominal"
            -- an unknown system and a missing unit report nothing
            ovr "ghost" `shouldReturn` "nil"
            luaText eng "return tostring(unit.getPhysioOverride(99, 'thermo'))"
                `shouldReturn` "nil"
            -- the override picks the READ semantics too: nutrition is
            -- Simulated by intent, so a stored value reads as stored
            readsAs eng "unit.readPhysio(1, 'hunger')"
                `shouldReturn` "300.0000|simulated"
            -- reading it changed nothing: the stored intent is intact
            snap ← snapshot eng
            snOverrides snap `shouldBe` Just (HM.fromList
                [ ("hydration", PhysioSimulated)
                , ("nutrition", PhysioSimulated)
                , ("thermo", PhysioNominal) ])

-----------------------------------------------------------------------
-- Requirement 4: readPhysio / readPhysioBase
-----------------------------------------------------------------------

readVerbSpec ∷ Spec
readVerbSpec = describe "readPhysio / readPhysioBase" $ do

    it "reads a Simulated key as base plus active modifiers, with the \
       \mode, and the base verb as the stored base" $
        withPhysioEngine True $ \eng → do
            standardScene eng standardStats
            readsAs eng "unit.readPhysio(1, 'hydration')"
                `shouldReturn` "21.0000|simulated"
            _ ← luaText eng "return unit.addModifier(1, 'hydration', 5, \
                            \'spec', 100)"
            readsAs eng "unit.readPhysio(1, 'hydration')"
                `shouldReturn` "26.0000|simulated"
            readsAs eng "unit.readPhysioBase(1, 'hydration')"
                `shouldReturn` "21.0000|simulated"
            -- the modifier expires with the game clock, with no tick
            writeIORef (wsGameTimeRef (toWorldSimCapability (peEnv eng))) 150
            readsAs eng "unit.readPhysio(1, 'hydration')"
                `shouldReturn` "21.0000|simulated"

    it "reads a Nominal key through its healthy rule, ignoring modifiers \
       \on the key and honouring them on its inputs" $
        withPhysioEngine True $ \eng → do
            standardScene eng standardStats
            readsAs eng "unit.readPhysio(1, 'core_temp')"
                `shouldReturn` "37.0000|nominal"
            _ ← luaText eng "return unit.addModifier(1, 'core_temp', -5, 'spec')"
            readsAs eng "unit.readPhysio(1, 'core_temp')"
                `shouldReturn` "37.0000|nominal"
            readsAs eng "unit.readPhysioBase(1, 'core_temp')"
                `shouldReturn` "37.0000|nominal"
            -- hunger is Nominal: the healthy rule is max_hunger, EFFECTIVE
            _ ← luaText eng "return unit.addModifier(1, 'max_hunger', 100, 'spec')"
            readsAs eng "unit.readPhysio(1, 'hunger')"
                `shouldReturn` "800.0000|nominal"
            readsAs eng "unit.readPhysio(3, 'salt_store')"
                `shouldReturn` "140.0000|nominal"

    it "reads meters as 0 (Nominal), as stored (Simulated), and 0 when a \
       \Simulated meter is absent" $
        withPhysioEngine True $ \eng → do
            standardScene eng (("salt_imbalance", 0) : ("hypothermia", 0.4)
                              : standardStats)
            -- salt is Simulated, hydration too; thermo is Nominal
            readsAs eng "unit.readPhysio(1, 'salt_imbalance')"
                `shouldReturn` "0.0000|simulated"
            readsAs eng "unit.readPhysio(1, 'hypothermia')"
                `shouldReturn` "0.0000|nominal"
            readsAs eng "unit.readPhysio(1, 'hyperthermia')"
                `shouldReturn` "0.0000|nominal"
            standardScene eng standardStats
            readsAs eng "unit.readPhysio(1, 'salt_imbalance')"
                `shouldReturn` "0.0000|simulated"

    it "answers nil, nil for an Unsupported key and a nonexistent unit" $
        withPhysioEngine True $ \eng → do
            standardScene eng standardStats
            forM_ ["hydration", "salt", "salt_conc", "hypothermia"] $ \k → do
                readsAs eng ("unit.readPhysio(2, '" <> k <> "')")
                    `shouldReturn` "nil|nil"
                readsAs eng ("unit.readPhysioBase(2, '" <> k <> "')")
                    `shouldReturn` "nil|nil"
            readsAs eng "unit.readPhysio(99, 'hydration')"
                `shouldReturn` "nil|nil"
            readsAs eng "unit.readPhysioBase(99, 'hydration')"
                `shouldReturn` "nil|nil"

    it "raises, rather than returning a value, when a Simulated owned key \
       \has no stored value or a healthy-rule input is missing" $
        withPhysioEngine True $ \eng → do
            standardScene eng (filter ((≢ "hydration") ∘ fst) standardStats)
            luaText eng "local ok, err = pcall(unit.readPhysio, 1, 'hydration'); \
                        \return tostring(ok) .. '|' .. tostring(err)"
                >>= (`shouldSatisfy` T.isInfixOf
                        "false|physiology read of 'hydration' (system \
                        \'hydration'): Simulated owned key 'hydration' has \
                        \no stored value")
            -- salt_conc needs the pool too
            luaText eng "local ok = pcall(unit.readPhysio, 1, 'salt_conc'); \
                        \return tostring(ok)" `shouldReturn` "false"
            -- unit 3 is Nominal everywhere; a missing input still raises
            installScene eng
                [ mkDef "plain" (plainUnitPhysiology True Nothing HM.empty) [] ]
                [ mkInstance "plain" (filter ((≢ "max_hunger") ∘ fst) inputs) ]
                0 rngSeed
            luaText eng "local ok, err = pcall(unit.readPhysio, 1, 'hunger'); \
                        \return tostring(ok) .. '|' .. tostring(err)"
                >>= (`shouldSatisfy` T.isInfixOf "input 'max_hunger' is absent")
            -- and a key whose rule needs no input does not
            readsAs eng "unit.readPhysio(1, 'core_temp')"
                `shouldReturn` "37.0000|nominal"

    it "reads salt_conc on demand across the four combinations, never \
       \from a stored legacy value, following inputs with no tick" $
        withPhysioEngine True $ \eng → do
            -- unit 1: salt AND hydration Simulated
            standardScene eng (("salt_conc", 0.123) : ("hydration", 31.5)
                : filter ((∉ ["hydration"]) ∘ fst) standardStats)
            before ← snapshot eng
            readsAs eng "unit.readPhysio(1, 'salt_conc')"
                `shouldReturn` "0.6667|simulated"
            readsAs eng "unit.readPhysioBase(1, 'salt_conc')"
                `shouldReturn` "0.6667|simulated"
            snapshot eng `shouldReturn` before
            -- change the inputs; the derived answer follows
            _ ← luaText eng "return unit.setStat(1, 'salt', 56)"
            readsAs eng "unit.readPhysio(1, 'salt_conc')"
                `shouldReturn` "1.0667|simulated"
            _ ← luaText eng "return unit.setStat(1, 'hydration', 21)"
            readsAs eng "unit.readPhysio(1, 'salt_conc')"
                `shouldReturn` "1.6000|simulated"
            -- the stored legacy value was never touched or consulted
            luaText eng "return string.format('%.3f', unit.getStat(1, 'salt_conc'))"
                `shouldReturn` "0.123"
            -- unit 3 (Nominal salt) reads 1.0 whatever its pools hold
            readsAs eng "unit.readPhysio(3, 'salt_conc')"
                `shouldReturn` "1.0000|nominal"

    it "raises, through the registered verb, for a Simulated hydration pool \
       \that is negative, NaN or infinite, reads a present ZERO pool through \
       \the 0.05 clamp, and still raises when a modifier masks a negative \
       \stored pool" $
        withPhysioEngine True $ \eng → do
            let withPool v = standardScene eng
                    (("hydration", v) : filter ((≢ "hydration") ∘ fst)
                                              standardStats)
                raises = luaText eng
                    "local ok, err = pcall(unit.readPhysio, 1, 'salt_conc'); \
                    \return tostring(ok) .. '|' .. tostring(err)"
            forM_ [-3, 0 / 0, 1 / 0] $ \bad → do
                withPool bad
                out ← raises
                out `shouldSatisfy` T.isPrefixOf "false|physiology read of \
                    \'salt_conc' (system 'salt'): input 'hydration' is not "
            -- an EMPTY pool is a valid reading (#2831): the concentration
            -- is the salt fraction over the clamped 0.05 floor
            withPool 0
            raises >>= (`shouldSatisfy` T.isPrefixOf "true|")
            -- a positive modifier must not mask a NEGATIVE stored pool
            -- (the reviewer's masking case)
            withPool (-3)
            _ ← luaText eng "return unit.addModifier(1, 'hydration', 30, \
                            \'spec')"
            raises >>= (`shouldSatisfy` T.isPrefixOf "false|physiology \
                \read of 'salt_conc' (system 'salt'): input 'hydration' \
                \is not non-negative (stored")
            -- the owned key itself still reads, as stored: only the
            -- DEPENDENT value judges the pool
            withPool 0
            readsAs eng "unit.readPhysio(1, 'hydration')"
                `shouldReturn` "0.0000|simulated"
            -- and a valid pool reads normally again
            withPool 31.5
            readsAs eng "unit.readPhysio(1, 'salt_conc')"
                `shouldReturn` "0.6667|simulated"

    it "leaves stats, modifiers, definitions and the stat generator \
       \untouched across owned, derived and meter reads, including ones \
       \that raise" $
        withPhysioEngine True $ \eng → do
            standardScene eng (("salt_conc", 0.9) : standardStats)
            _ ← luaText eng "return unit.addModifier(1, 'salt', 3, 'spec', 500)"
            before ← snapshot eng
            forM_ [ "hydration", "salt", "salt_store", "core_temp", "hunger"
                  , "calories", "salt_conc", "salt_imbalance", "hypothermia"
                  , "hyperthermia" ] $ \k → do
                _ ← readsAs eng ("unit.readPhysio(1, '" <> k <> "')")
                _ ← readsAs eng ("unit.readPhysioBase(1, '" <> k <> "')")
                _ ← readsAs eng ("unit.readPhysio(2, '" <> k <> "')")
                pure ()
            _ ← luaText eng "return tostring(pcall(unit.readPhysio, 99, 'x'))"
            forM_ ["getPhysioMode", "getPhysioOverride", "getPhysioDefault"] $
                \v → luaText eng ("return tostring(unit." <> v
                                  <> "(1, 'salt'))")
            snapshot eng `shouldReturn` before
            -- an invariant error is also a no-op
            standardScene eng (filter ((≢ "salt") ∘ fst) standardStats)
            beforeErr ← snapshot eng
            _ ← luaText eng "return tostring(pcall(unit.readPhysio, 1, 'salt'))"
            snapshot eng `shouldReturn` beforeErr

    it "answers the Haskell physioRead exactly as the verbs do" $
        withPhysioEngine True $ \eng → do
            standardScene eng standardStats
            let env = peEnv eng
            physioRead env (UnitId 1) "hydration"
                `shouldReturn` ReadOwned ModeSimulated 21
            physioReadBase env (UnitId 1) "hydration"
                `shouldReturn` ReadOwned ModeSimulated 21
            physioRead env (UnitId 1) "core_temp"
                `shouldReturn` ReadOwned ModeNominal 37
            physioRead env (UnitId 2) "hydration" `shouldReturn` ReadUnsupported
            physioRead env (UnitId 99) "hydration" `shouldReturn` ReadNoUnit
            physioRead env (UnitId 1) "max_stamina"
                `shouldReturn` ReadUnowned (Just 80)
            physioRead env (UnitId 1) "nonesuch"
                `shouldReturn` ReadUnowned Nothing

-----------------------------------------------------------------------
-- SD-19: unowned keys keep today's reads
-----------------------------------------------------------------------

unownedSpec ∷ Spec
unownedSpec = describe "unowned keys keep today's getStat semantics (SD-19)" $ do

    it "reads an authored-but-unrolled template exactly as getStat does: \
       \same value, same cache, same generator advance, and no reroll" $
        withPhysioEngine True $ \eng → do
            let fresh = standardScene eng standardStats
                run call = do
                    fresh
                    out ← luaText eng ("return __p(" <> call <> ")")
                    s ← snapshot eng
                    pure (out, s)
            (viaGet, afterGet) ← run "unit.getStat(1, 'custom_lazy')"
            (viaPhysio, afterPhysio) ← run "unit.readPhysio(1, 'custom_lazy')"
            -- the same number, with a nil mode, and an identical world
            viaPhysio `shouldBe` viaGet
            snStats afterPhysio `shouldBe` snStats afterGet
            snRng afterPhysio `shouldBe` snRng afterGet
            -- and it really did roll and cache: the control is not vacuous
            snRng afterGet `shouldNotBe` rngSeed
            fmap (HM.member "custom_lazy") (snStats afterGet)
                `shouldBe` Just True
            -- repeated reads do not reroll
            _ ← luaText eng "return unit.readPhysio(1, 'custom_lazy')"
            afterTwice ← snapshot eng
            snRng afterTwice `shouldBe` snRng afterPhysio
            snStats afterTwice `shouldBe` snStats afterPhysio
            -- the base verb pairs with getStatBase the same way
            (baseGet, bGet) ← run "unit.getStatBase(1, 'custom_lazy')"
            (basePhysio, bPhysio) ←
                run "unit.readPhysioBase(1, 'custom_lazy')"
            basePhysio `shouldBe` baseGet
            snStats bPhysio `shouldBe` snStats bGet
            snRng bPhysio `shouldBe` snRng bGet

    it "passes the Lua-formula keys through exactly as getStat returns \
       \them: the stored value when present, nil when not" $
        withPhysioEngine True $ \eng → do
            standardScene eng (("metabolism_rate", 1.3) : standardStats)
            forM_ ["max_stamina", "metabolism_rate", "max_exhaustion"
                  , "max_sleep_pressure"] $ \k → do
                viaGet ← readsAs eng ("unit.getStat(1, '" <> k <> "')")
                viaPhysio ← readsAs eng ("unit.readPhysio(1, '" <> k <> "')")
                viaPhysio `shouldBe` viaGet
                viaBaseGet ← readsAs eng ("unit.getStatBase(1, '" <> k <> "')")
                viaBase ← readsAs eng ("unit.readPhysioBase(1, '" <> k <> "')")
                viaBase `shouldBe` viaBaseGet
            readsAs eng "unit.readPhysio(1, 'max_stamina')"
                `shouldReturn` "80.0000|nil"
            readsAs eng "unit.readPhysio(1, 'max_exhaustion')"
                `shouldReturn` "nil|nil"

    it "answers nil, nil for a genuinely undefined key and a missing unit" $
        withPhysioEngine True $ \eng → do
            standardScene eng standardStats
            readsAs eng "unit.readPhysio(1, 'nonesuch')" `shouldReturn` "nil|nil"
            readsAs eng "unit.readPhysio(99, 'max_stamina')"
                `shouldReturn` "nil|nil"
            readsAs eng "unit.readPhysioBase(1, 'nonesuch')"
                `shouldReturn` "nil|nil"

    it "does not call a key Unsupported merely because no system owns it, \
       \even on a bodyless unit" $
        withPhysioEngine True $ \eng → do
            standardScene eng (("heart_rate", 55) : standardStats)
            installScene eng [ mkDef "bare" noUnitPhysiology [] ]
                [ mkInstance "bare" [("heart_rate", 55)] ] 0 rngSeed
            readsAs eng "unit.readPhysio(1, 'heart_rate')"
                `shouldReturn` "55.0000|nil"

-----------------------------------------------------------------------
-- Requirement 1: the Lua accessor and the constants
-----------------------------------------------------------------------

registryAccessorSpec ∷ Spec
registryAccessorSpec = describe "engine.getPhysiologyRegistry (requirement 1)" $ do

    it "answers nil until a registry is loaded" $
        withPhysioEngine False $ \eng →
            luaText eng "return tostring(engine.getPhysiologyRegistry())"
                `shouldReturn` "nil"

    it "returns the loaded systems, keys, rules, requirements, \
       \substitutes, profiles and constants" $
        withPhysioEngine True $ \eng → do
            luaText eng
                "local r = engine.getPhysiologyRegistry(); \
                \return table.concat(r.systemOrder, ',')"
                `shouldReturn` "thermo,salt,hydration,nutrition,stamina,exhaustion,sleep"
            luaText eng
                "local s = engine.getPhysiologyRegistry().systems.salt; \
                \return s.owns[1].key .. ',' .. s.owns[2].key .. '|' .. \
                \s.derived[1].key .. '|' .. s.meters[1] .. '|' .. \
                \s.requires[1]"
                `shouldReturn` "salt,salt_store|salt_conc|salt_imbalance|body"
            luaText eng
                "local o = engine.getPhysiologyRegistry().systems.salt.owns[2]\
                \.healthy; return o.kind .. '|' .. o.from .. '|' .. \
                \string.format('%.1f', o.scale) .. '|' .. \
                \table.concat(o.scaleRefs, ',')"
                `shouldReturn`
                "from|body_mass|2.0|SALT_PER_KG,SALT_STORE_CAP_FRAC"
            luaText eng
                "local t = engine.getPhysiologyRegistry().systems.thermo; \
                \return string.format('%.1f', t.owns[1].healthy.value) .. '|' \
                \.. t.owns[1].healthy.ref .. '|' .. t.contributes[1].name \
                \.. '|' .. t.contributes[1].substitute.kind"
                `shouldReturn` "37.0|THERMO_BASELINE|sweat_debit|none"
            luaText eng
                "local n = engine.getPhysiologyRegistry().systems.nutrition\
                \.contributes[1]; return n.name .. '|' .. n.substitute.kind"
                `shouldReturn` "meal_salt_credit|virtual_meal"
            luaText eng
                "local p = engine.getPhysiologyRegistry().profiles.reduced; \
                \return p.hydration .. ',' .. p.salt .. ',' .. p.thermo .. \
                \',' .. p.nutrition .. ',' .. p.stamina"
                `shouldReturn` "nominal,simulated,simulated,nominal,simulated"
            -- stamina's healthy rule is the named one (#2834)
            luaText eng
                "local o = engine.getPhysiologyRegistry().systems.stamina.owns[1]\
                \.healthy; return o.kind"
                `shouldReturn` "max_stamina"

    it "exposes no mutating entry: no function anywhere in it, and \
       \changing the returned table changes nothing" $
        withPhysioEngine True $ \eng → do
            luaText eng
                "local function hasFn(t) for _, v in pairs(t) do \
                \if type(v) == 'function' then return true end; \
                \if type(v) == 'table' and hasFn(v) then return true end \
                \end return false end; \
                \return tostring(hasFn(engine.getPhysiologyRegistry()))"
                `shouldReturn` "false"
            luaText eng
                "local r = engine.getPhysiologyRegistry(); \
                \r.constants.SALT_PER_KG = 99; r.systemOrder[1] = 'x'; \
                \local again = engine.getPhysiologyRegistry(); \
                \return string.format('%.1f', again.constants.SALT_PER_KG) \
                \.. '|' .. again.systemOrder[1]"
                `shouldReturn` "1.0|thermo"

    it "keeps the registry constants equal to the live Lua copies they \
       \duplicate until S4 migrates the owners" $
        withPhysioEngine True $ \eng → do
            standardScene eng standardStats
            luaText eng
                "local thermo = require('scripts.thermo'); \
                \local salts = require('scripts.salts'); \
                \local c = engine.getPhysiologyRegistry().constants; \
                \local mass = unit.getStat(1, 'body_mass'); \
                \return tostring(thermo.BASELINE == c.THERMO_BASELINE) .. '|' \
                \.. tostring(math.abs(salts.maxSalt(1) / mass - \
                \c.SALT_PER_KG) < 1e-6) .. '|' .. \
                \tostring(math.abs(salts.storeCap(1) / salts.maxSalt(1) - \
                \c.SALT_STORE_CAP_FRAC) < 1e-6)"
                `shouldReturn` "true|true|true"

-----------------------------------------------------------------------
-- Requirement 3: the real load boundary
-----------------------------------------------------------------------

-- | A one-page save with one unit, written through the production
--   encoder as @saves/<slot>/world.synworld@: the bytes a real save of
--   that session would hold.
writeSave ∷ String → UnitInstanceSnapshot → IO ()
writeSave slot unit = do
    let page = (minimalPage page1)
            { pgsUnits = UnitSnapshot
                (HM.singleton (UnitId 7) unit) 10
            , pgsUnitSimStates = HM.singleton (UnitId 7) minimalSimState }
    case captureSessionSnapshot minimalGlobals [page] of
        Left errs → expectationFailure ("fixture session invalid: " ⧺ show errs)
        Right snap → do
            createDirectoryIfMissing True ("saves" </> slot)
            BS.writeFile ("saves" </> slot </> "world.synworld")
                (encodeSessionSnapshot
                    (snapshotSaveMetadata
                        (SaveRequestMeta (T.pack slot) "ts" False) snap)
                    snap [])

loadBoundarySpec ∷ Spec
loadBoundarySpec = describe "the load boundary (requirement 3)" $ do

    forM_ [ ( "an override naming an undeclared system"
            , (minimalUnitInstance [])
                { uisDefName = "acolyte"
                , uisPhysioOverrides = HM.fromList
                    [("ghost", PhysioSimulated)] }
            , [ "unit #7", "acolyte", "ghost", "does not declare" ] )
          , ( "a non-finite Simulated value"
            , (minimalUnitInstance [])
                { uisDefName = "acolyte"
                , uisPhysioOverrides = HM.fromList
                    [("hydration", PhysioSimulated)]
                , uisStats = HM.fromList [("hydration", 1 / 0)] }
            , [ "unit #7", "acolyte", "hydration", "malformed" ] ) ] $
        \(what, unit, words') →
            it ("rejects a save carrying " <> what <> " before anything is \
                \staged, naming the unit, and leaves the live session \
                \exactly as it was") $
                withIsolatedResourceRoot $ withHeadlessEngine $ \env → do
                    ensurePhysiologyRegistry env
                    -- the live session the rejected load must not touch:
                    -- one registered definition and one live unit
                    let cap = toUnitCombatCapability env
                        liveDef = mkDef "acolyte"
                            (plainUnitPhysiology True Nothing HM.empty) []
                    writeIORef (ucUnitManagerRef cap) emptyUnitManager
                        { umDefs = HM.singleton "acolyte" liveDef
                        , umInstances = HM.singleton (UnitId 50)
                            (mkInstance "acolyte" standardStats) }
                    before ← readIORef (ucUnitManagerRef cap)
                    writeSave "badphysiology" unit
                    ls ← newBareLuaBackend env
                    out ← executeDebugLua (lbsLuaState ls)
                        "return engine.loadSave('badphysiology')"
                    out `shouldSatisfy` T.isInfixOf "false"
                    Just st ← readLoadStatus (loadStatusRef env)
                    lsPhase st `shouldBe` LoadFailed
                    lsFailedAtPhase st `shouldBe` Just LoadContentValidated
                    case lsOutcome st of
                        Just (LoadAborted msg) → do
                            msg `shouldSatisfy` T.isInfixOf
                                "saved unit physiology state is invalid"
                            forM_ words' $ \w →
                                msg `shouldSatisfy` T.isInfixOf w
                        other → expectationFailure
                            ("expected a LoadAborted outcome, got "
                             ⧺ show other)
                    -- nothing was published: the old session is the same
                    readIORef (ucUnitManagerRef cap) `shouldReturn` before
                    -- and it is left paused, per the paused-failure contract
                    readIORef (enginePausedRef env) `shouldReturn` True

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls
