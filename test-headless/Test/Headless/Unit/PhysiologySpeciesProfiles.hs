{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The "physiology species profiles" gate (#2833, S7 of epic #2827; owner
--   decision SD-10, design D-10): the SHIPPED unit definitions carry the
--   settled species table, and what that table means holds through spawn
--   seeding, the shipped Lua and the real save codec.
--
--   __Nothing here is a second copy of the profile tables.__ The
--   definitions are loaded by the real @engine.loadUnitYaml@ over the real
--   registry, and a mode is whatever 'resolveMode' says of the loaded
--   'UnitPhysiology'; the table below is the OWNER'S expected result, the
--   thing the YAML is held to.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "physiology species profiles"'@.
module Test.Headless.Unit.PhysiologySpeciesProfiles (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Control.Exception (finally)
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.List (sort)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import Engine.Core.State (EngineEnv(..))
import Equipment.Types (emptyEquipmentClassManager)
import Infection.Types (emptyInfectionManager)
import Item.Types (emptyItemManager)
import Test.Headless.Unit.PhysiologyFixture
    (profilePhysiology, readShippedPhysiologyRegistry)
import Test.Headless.Unit.PhysiologyModes.Harness
import Test.Headless.Unit.TransferApi (evalDebug)
import Test.Headless.World.Save.Components.Fixture
    ( minimalGlobals, minimalPage, minimalSimState, page1 )
import Unit.Physiology (registrySystem, resolveMode)
import Unit.Physiology.Types
import Unit.Types
import World.Save.Envelope (decodeSessionEnvelope, encodeSessionSnapshot)
import World.Save.Physiology (PhysiologyNormalization(..), physiologyLoadGate)
import World.Save.Snapshot
    ( PageSnapshot(..), SessionSnapshot(..), captureSessionSnapshot )
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotSaveMetadata)
import World.Save.Types
    ( UnitInstanceSnapshot(..), UnitSnapshot(..), fromUnitSnapshot
    , toUnitSnapshot )

-----------------------------------------------------------------------
-- The owner's table (SD-10) -- the EXPECTED result the YAML is held to
-----------------------------------------------------------------------

-- | A species and the mode each system resolves to, in
--   (hydration, salt, thermo, nutrition) order, with the profile it names.
data Row = Row Text Text (ResolvedMode, ResolvedMode, ResolvedMode, ResolvedMode)

rowDef ∷ Row → Text
rowDef (Row n _ _) = n

sim, nom ∷ ResolvedMode
sim = ModeSimulated
nom = ModeNominal

table ∷ [Row]
table =
    [ Row "acolyte"         "rich"    (sim, sim, sim, sim)
    , Row "bear_brown"      "reduced" (nom, sim, sim, nom)
    , Row "technomule"      "reduced" (nom, sim, sim, nom)
    , Row "red_squirrel"    "simple"  (nom, nom, sim, nom)
    , Row "nomad_primitive" "exempt"  (nom, nom, nom, nom) ]

-- | The four systems in the table's column order.
systems ∷ [Text]
systems = ["hydration", "salt", "thermo", "nutrition"]

modesList ∷ (a, a, a, a) → [a]
modesList (a, b, c, d) = [a, b, c, d]

-----------------------------------------------------------------------
-- The shipped content, loaded the way the startup loader loads it
-----------------------------------------------------------------------

contentDirs ∷ [(FilePath, Text)]
contentDirs =
    [ ("data/substances",  "engine.loadSubstanceYaml")
    , ("data/items",       "engine.loadItemYaml")
    , ("data/equipment",   "engine.loadEquipmentYaml")
    , ("data/materials",   "engine.loadMaterialYaml")
    , ("data/factions",    "engine.loadFactionYaml")
    , ("data/units",       "engine.loadUnitYaml")
    , ("data/loot_tables", "engine.loadLootTableYaml") ]

-- | The registries up to and including every shipped unit definition. The
--   physiology registry is already in place ('withScene' installs it).
loadContent ∷ Scene → IO ()
loadContent (_, ls) = forM_ contentDirs $ \(dir, fn) → do
    files ← sort ∘ filter ((≡ ".yaml") ∘ takeExtension) <$> listDirectory dir
    forM_ files $ \f → void (evalDebug ls (fn <> "('" <> T.pack (dir </> f) <> "')"))

-- | One engine with every shipped definition registered.
withShipped ∷ (Scene → IO a) → IO a
withShipped act = withScene [] $ \sc → do
    loadContent sc
    act sc

shippedDefs ∷ EngineEnv → IO (HM.HashMap Text UnitDef)
shippedDefs env = umDefs <$> readIORef (unitManagerRef env)

defNamed ∷ HM.HashMap Text UnitDef → Text → IO UnitDef
defNamed defs n = case HM.lookup n defs of
    Just d  → pure d
    Nothing → do
        expectationFailure ("the shipped definition " <> show n
                            <> " did not register; registered: "
                            <> show (sort (HM.keys defs)))
        pure (error "unreachable")

-- | A system's resolved mode for a definition, by the real resolver.
modeOfDef ∷ PhysiologyRegistry → UnitDef → Text → IO ResolvedMode
modeOfDef reg def sid = case registrySystem reg sid of
    Just sys → pure (resolveMode sys (udPhysiology def) Nothing)
    Nothing → do
        expectationFailure ("registry has no system " <> show sid)
        pure ModeUnsupported

-- | The body a species is spawned with in the fixtures.
bodyOf ∷ Text → Body
bodyOf "red_squirrel" = squirrelBody
bodyOf _              = acolyteBody

-- | The stored keys a system owns, from the REGISTRY.
ownedKeysOf ∷ PhysiologyRegistry → Text → [Text]
ownedKeysOf reg sid = maybe [] (map okKey ∘ psOwns) (registrySystem reg sid)

-- | Every key a Nominal or Unsupported system must not store, owned,
--   derived or meter, across all four systems.
nonStoredKeys ∷ PhysiologyRegistry → Text → [Text]
nonStoredKeys reg sid = case registrySystem reg sid of
    Nothing → []
    Just sys → map okKey (psOwns sys) ⧺ map dkKey (psDerived sys) ⧺ psMeters sys

-- | Replace the session's units with exactly these, built from the shipped
--   definitions by the seeding rule spawn and load use.
placeUnits ∷ EngineEnv → [(Int, Text, Maybe UnitPhysiology, [(Text, PhysioMode)], [(Text, Float)])]
           → IO ()
placeUnits env specs = do
    defs ← shippedDefs env
    insts ← forM specs $ \(uid, name, mUp, overrides, stats) → do
        base ← defNamed defs name
        let def = maybe base (\up → base { udPhysiology = up }) mUp
            sp = (unitSpec uid name Nothing)
                    { usBody = bodyOf name, usOverrides = overrides, usStats = stats }
        inst ← buildUnit def sp
        pure (UnitId (fromIntegral uid), inst { uiPage = page1 })
    modifyIORef' (unitManagerRef env) $ \um →
        um { umInstances = HM.fromList insts }

-----------------------------------------------------------------------
-- The page snapshot and the REAL codec (as the persistence group does it)
-----------------------------------------------------------------------

pageSnapshotOf ∷ EngineEnv → IO PageSnapshot
pageSnapshotOf env = do
    um ← readIORef (unitManagerRef env)
    let snap = toUnitSnapshot page1 um
    pure (minimalPage page1)
        { pgsUnits = snap
        , pgsUnitSimStates = HM.fromList
            [ (uid, minimalSimState) | uid ← HM.keys (usnInstances snap) ] }

-- | Through the production session codec and back.
codecRoundTrip ∷ PageSnapshot → IO PageSnapshot
codecRoundTrip pg = case captureSessionSnapshot minimalGlobals [pg] of
    Left errs → failWith (show errs)
    Right snap →
        let meta = snapshotSaveMetadata
                (SaveRequestMeta { srmSlotName = "slot", srmTimestamp = "ts"
                                 , srmAutosave = False }) snap
        in case decodeSessionEnvelope HS.empty HS.empty
                    (encodeSessionSnapshot meta snap []) of
            Left err → failWith (show err)
            Right (_, back, _, _) → case HM.lookup page1 (snapPages back) of
                Just p  → pure p
                Nothing → failWith "the page did not survive the codec"
  where
    failWith msg = do
        expectationFailure msg
        pure pg

-- | The load gate over one page of units against the shipped definitions.
gate ∷ PhysiologyRegistry → HM.HashMap Text UnitDef → UnitSnapshot
     → IO (UnitSnapshot, [PhysiologyNormalization])
gate reg defs us = case physiologyLoadGate reg (HM.map udPhysiology defs) 0 [(page1, us)] of
    Right ([(_, us')], notes) → pure (us', notes)
    Right _ → do
        expectationFailure "unexpected page shape"
        pure (us, [])
    Left msgs → do
        expectationFailure ("the load gate rejected: " <> show msgs)
        pure (us, [])

-----------------------------------------------------------------------
-- The group
-----------------------------------------------------------------------

spec ∷ Spec
spec = describe "physiology species profiles" $
    aroundAll withShipped $ do
        tableSpec
        bodylessSpec
        spawnSpec
        squirrelSpec
        smallMassSpec
        loadSpec

tableSpec ∷ SpecWith Scene
tableSpec = describe "the shipped species table (SD-10)" $
    forM_ table $ \(Row name profile modes) →
        it (T.unpack name <> " is `" <> T.unpack profile <> "` and resolves "
            <> show (modesList modes)) $ \(env, _) → do
            reg ← readShippedPhysiologyRegistry
            def ← shippedDefs env ≫= \ds → defNamed ds name
            upProfile (udPhysiology def) `shouldBe` Just profile
            upBodyAuthored (udPhysiology def) `shouldBe` True
            got ← mapM (modeOfDef reg def) systems
            got `shouldBe` modesList modes

bodylessSpec ∷ SpecWith Scene
bodylessSpec = describe "bodyless definitions" $ do
    it "are exactly the loaded definitions that author no body, and every one \
       \resolves Unsupported in every system" $ \(env, _) → do
        reg ← readShippedPhysiologyRegistry
        defs ← shippedDefs env
        let bodyless = sort [ n | (n, d) ← HM.toList defs
                                , not (upBodyAuthored (udPhysiology d)) ]
            bodied   = sort [ n | (n, d) ← HM.toList defs
                                , upBodyAuthored (udPhysiology d) ]
        bodied `shouldBe` sort (map rowDef table)
        -- the three shipped bodyless definitions, named so a vanished one is noticed
        forM_ ["tiller", "unknown_unit", "white_tailed_deer"] $ \n →
            (n, n `elem` bodyless) `shouldBe` (n, True)
        forM_ bodyless $ \n → do
            def ← defNamed defs n
            upProfile (udPhysiology def) `shouldBe` Nothing
            got ← mapM (modeOfDef reg def) systems
            (n, got) `shouldBe` (n, replicate 4 ModeUnsupported)

spawnSpec ∷ SpecWith Scene
spawnSpec = describe "spawn resolves the table before simulation" $ do
    forM_ table $ \(Row name _ modes) →
        it (T.unpack name <> ": a spawned unit holds exactly its Simulated systems' keys") $
            \(env, _) → do
                reg ← readShippedPhysiologyRegistry
                placeUnits env [(1, name, Nothing, [], [])]
                inst ← readInstance env 1
                forM_ (zip systems (modesList modes)) $ \(sid, m) →
                    forM_ (ownedKeysOf reg sid) $ \k →
                        (sid, k, HM.member k (uiStats inst)) `shouldBe` (sid, k, m ≡ sim)
                -- derived values and meters are never seeded, in any mode
                forM_ systems $ \sid →
                    forM_ (nonStoredKeys reg sid) $ \k →
                        unless (k `elem` ownedKeysOf reg sid) $
                            (k, HM.member k (uiStats inst)) `shouldBe` (k, False)
    it "a bodyless definition spawns holding no physiology key at all" $ \(env, _) → do
        reg ← readShippedPhysiologyRegistry
        defs ← shippedDefs env
        forM_ [ n | (n, d) ← HM.toList defs, not (upBodyAuthored (udPhysiology d)) ] $ \n → do
            placeUnits env [(1, n, Nothing, [], [])]
            inst ← readInstance env 1
            forM_ systems $ \sid →
                forM_ (nonStoredKeys reg sid) $ \k →
                    (n, k, HM.member k (uiStats inst)) `shouldBe` (n, k, False)

-- | The squirrel's actual Nominal-salt semantics, over the shipped Lua.
squirrelSpec ∷ SpecWith Scene
squirrelSpec = describe "the shipped squirrel (simple profile)" $ do
    it "reads salt_conc 1.0 Nominal, stores no salt state, has no imbalance or cramps, \
       \and keeps thermo Simulated through two game minutes" $ \(env, ls) → do
        placeUnits env [(1, "red_squirrel", Nothing, [], squirrelStats)]
        climate ls 22 0.5
        ex ← track ls 1 120 0.1 ["core_temp", "salt_conc"]
        let core = ex HM.! "core_temp"
        exMin core `shouldSatisfy` (>= 33.5)
        exMax core `shouldSatisfy` (<= 39)
        exNaN core `shouldBe` 0
        readOwned ls 1 "salt_conc" `shouldReturn` "1|nominal"
        readOwned ls 1 "salt_imbalance" `shouldReturn` "0|nominal"
        luaText ls "return table.concat({ __physiology.mode(1, 'thermo') or '-', \
                   \__physiology.mode(1, 'salt') or '-', __physiology.mode(1, 'hydration') or '-', \
                   \__physiology.mode(1, 'nutrition') or '-' }, ',')"
            `shouldReturn` "simulated,nominal,nominal,nominal"
        forM_ ["salt", "salt_store", "salt_conc", "salt_imbalance", "hydration", "hunger", "calories"] $ \k →
            luaText ls ("return tostring(unit.getStatBase(1, '" <> k <> "'))")
                `shouldReturn` "nil"
        luaText ls "return tostring(unit.getStatBase(1, 'core_temp') ~= nil)" `shouldReturn` "true"
        luaNum ls "require('scripts.salts').speedMultiplier(1)" >>= near 1e-12 1
        luaText ls "return tostring(unit.getPhysioOverride(1, 'salt'))" `shouldReturn` "nil"
        killed env `shouldReturn` []
        scriptErrors ls `shouldReturn` ""
    it "differs from a squirrel left on `reduced`: salt Simulated, the pool and store stored" $ \(env, ls) → do
        reduced ← profilePhysiology "reduced"
        shipped ← shippedDefs env
        base ← defNamed shipped "red_squirrel"
        let setDef d = modifyIORef' (unitManagerRef env) $ \um →
                um { umDefs = HM.insert "red_squirrel" d (umDefs um) }
        -- the definition left on `reduced` (what an un-flipped YAML would load),
        -- restored afterwards: the engine is shared by the whole group
        (do setDef base { udPhysiology = reduced }
            placeUnits env [(1, "red_squirrel", Nothing, [], squirrelStats)]
            luaText ls "return tostring(__physiology.mode(1, 'salt'))" `shouldReturn` "simulated"
            luaText ls "return tostring(unit.getStatBase(1, 'salt') ~= nil and unit.getStatBase(1, 'salt_store') ~= nil)"
                `shouldReturn` "true")
            `finally` setDef base
  where
    squirrelStats = [ ("constitution", 0.5), ("metabolism", 1.2)
                    , ("fat_mass", 0.03), ("lean_mass", 0.24) ]

-- | The independent small-mass SIMULATED coverage: the production default's
--   Nominal result must not stand in for it, so the shipped squirrel pins salt
--   Simulated through the facade and is held to the probe's own bounds. (The
--   physiology stat modes group keeps T-1's four salt x hydration
--   combinations and T-12 on explicit fixtures; this one runs the shipped
--   definition.)
smallMassSpec ∷ SpecWith Scene
smallMassSpec = describe "independent small-mass Simulated coverage (0.3 kg)" $
    forM_ [ ("idle, metabolism 1.2", 1.2, "idle")
          , ("forced moving, metabolism 1.2", 1.2, "walking") ] $ \(label, metab, activity) →
        it (label <> ": salt pinned Simulated stays inside the small-creature bounds") $ \(env, ls) → do
            placeUnits env [ (1, "red_squirrel", Nothing, []
                             , [ ("constitution", 0.5), ("metabolism", metab)
                               , ("fat_mass", 0.03), ("lean_mass", 0.24) ]) ]
            modifyInstance env 1 (\u → u { uiActivity = activity })
            -- the pin: salt Simulated, reseeded healthy by the transition
            luaText ls "return tostring(__physiology.setMode(1, 'salt', 'simulated'))"
                `shouldReturn` "true"
            climate ls 22 0.5
            ex ← track ls 1 120 0.1 ["salt_conc", "core_temp"]
            let conc = ex HM.! "salt_conc"
                core = ex HM.! "core_temp"
            exMin conc `shouldSatisfy` (>= 0.7)
            exMax conc `shouldSatisfy` (<= 1.3)
            exMin core `shouldSatisfy` (>= 33.5)
            exMax core `shouldSatisfy` (<= 39)
            (exNaN conc, exNaN core) `shouldBe` (0, 0)
            killed env `shouldReturn` []
            -- a Simulated pool is stored again (it is the pin, not the default)
            luaText ls "return tostring(unit.getStatBase(1, 'salt') ~= nil)" `shouldReturn` "true"
            scriptErrors ls `shouldReturn` ""

-- | The unpinned and pinned load cases, through the real codec and gate.
loadSpec ∷ SpecWith Scene
loadSpec = describe "load honours the live default and pinned intent" $ do
    it "an unpinned squirrel saved under `reduced` adopts `simple` on load: salt state \
       \scrubbed, normalization logged once, unowned state kept, idempotent, re-save/reload stable" $
        \(env, ls) → do
            reg ← readShippedPhysiologyRegistry
            defs ← shippedDefs env
            reduced ← profilePhysiology "reduced"
            placeUnits env [(1, "red_squirrel", Just reduced, [], [])]
            before ← readInstance env 1
            HM.member "salt" (uiStats before) `shouldBe` True
            HM.member "salt_store" (uiStats before) `shouldBe` True
            -- saved, through the production codec
            pg ← pageSnapshotOf env ≫= codecRoundTrip
            (us1, notes1) ← gate reg defs (pgsUnits pg)
            length notes1 `shouldBe` 1
            let uOf us = HM.lookup (UnitId 1) (usnInstances us)
            case uOf us1 of
                Nothing → expectationFailure "the unit vanished"
                Just u → do
                    forM_ (nonStoredKeys reg "salt") $ \k →
                        HM.member k (uisStats u) `shouldBe` False
                    -- thermo stays Simulated and every unowned stat is untouched
                    HM.member "core_temp" (uisStats u) `shouldBe` True
                    forM_ ["body_mass", "frame_mass", "fat_mass", "lean_mass", "height"] $ \k →
                        HM.lookup k (uisStats u) `shouldBe` HM.lookup k (uiStats before)
                    uisPhysioOverrides u `shouldBe` HM.empty
            -- idempotent: a second pass changes nothing and logs nothing
            (us2, notes2) ← gate reg defs us1
            notes2 `shouldBe` []
            us2 `shouldBe` us1
            -- re-save and reload: the same canonical content, no new note
            let pg2 = pg { pgsUnits = us2 }
            back ← codecRoundTrip pg2
            (us3, notes3) ← gate reg defs (pgsUnits back)
            notes3 `shouldBe` []
            us3 `shouldBe` us2
            -- loaded the way load loads it, the squirrel reads healthy salt before a tick
            let (restored, orphans, _, _, _) = fromUnitSnapshot page1 defs
                    emptyInfectionManager emptyEquipmentClassManager emptyItemManager us3
            orphans `shouldBe` []
            writeIORef (unitManagerRef env) restored
            _ ← luaText ls "__resources.onSaveLoaded({ 1 }, {}); return true"
            readOwned ls 1 "salt_conc" `shouldReturn` "1|nominal"

    it "a squirrel pinned to salt Simulated keeps its pin and its durable salt/reserve state exactly, \
       \and so does a pin equal to the old default" $ \(env, _) → do
        reg ← readShippedPhysiologyRegistry
        defs ← shippedDefs env
        reduced ← profilePhysiology "reduced"
        -- salt pinned Simulated (the old default, and the intent) and hydration
        -- pinned Nominal (also equal to the old default): both are intent
        placeUnits env
            [ (1, "red_squirrel", Just reduced
              , [("salt", PhysioSimulated), ("hydration", PhysioNominal)]
              , [("salt", 0.21), ("salt_store", 0.17)]) ]
        before ← readInstance env 1
        pg ← pageSnapshotOf env ≫= codecRoundTrip
        (us, notes) ← gate reg defs (pgsUnits pg)
        notes `shouldBe` []
        case HM.lookup (UnitId 1) (usnInstances us) of
            Nothing → expectationFailure "the unit vanished"
            Just u → do
                HM.lookup "salt" (uisStats u) `shouldBe` Just 0.21
                HM.lookup "salt_store" (uisStats u) `shouldBe` Just 0.17
                uisPhysioOverrides u `shouldBe`
                    HM.fromList [("salt", PhysioSimulated), ("hydration", PhysioNominal)]
                uisStats u `shouldBe` uiStats before
        -- and a second pass is still a no-op
        (us', notes') ← gate reg defs us
        (us', notes') `shouldBe` (us, [])
