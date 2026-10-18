{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The "physiology mind species profiles" gate (#2840, S11b of epic #2827;
--   owner decision SD-23, design D-10, T-S11-3 and T-S11-6): the SHIPPED unit
--   definitions carry the settled mind column (acolyte Simulated; bear,
--   squirrel, technomule and nomad Nominal; consciousness Simulated for every
--   bodied species, both Unsupported for a bodyless one), and what a default
--   Nominal mind means holds through spawn seeding, the shipped brain,
--   thought, mental and consumable Lua, and the real save codec and load gate.
--
--   __Nothing here is an override standing in for a default.__ Every
--   default-Nominal case spawns the shipped definition with NO mind override
--   (@placeUnits@ over the definitions the real @engine.loadUnitYaml@ loaded),
--   and a mode is whatever 'resolveMode' says of the loaded 'UnitPhysiology'.
--   The S11a cases that force a Nominal mind by override
--   ("Test.Headless.Unit.PhysiologyBrainMental.Machine") prove the machinery;
--   they are not this group's evidence and are not relabeled as it.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "physiology mind species profiles"'@.
module Test.Headless.Unit.PhysiologyMindSpeciesProfiles (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (writeIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState)
import Equipment.Types (emptyEquipmentClassManager)
import Infection.Types (emptyInfectionManager)
import Item.Types (ItemManager(..), emptyItemManager)
import Test.Headless.Unit.PhysiologyBrainMental.Machine (coffeeDef, pot)
import Test.Headless.Unit.PhysiologyFixture (readShippedPhysiologyRegistry)
import Test.Headless.Unit.PhysiologyModes.Harness
import Test.Headless.Unit.PhysiologySpeciesProfiles
    ( codecRoundTrip, gate, onSavePage, pageSnapshotOf, placeUnits
    , shippedDefs, withShipped )
import Unit.Physiology (registrySystem, resolveMode)
import Unit.Physiology.Types
import Unit.Types
import World.Save.Snapshot (PageSnapshot(..))
import World.Save.Types (UnitInstanceSnapshot(..), UnitSnapshot(..), fromUnitSnapshot)
import Test.Headless.World.Save.Components.Fixture (page1)

spec ∷ Spec
spec = describe "physiology mind species profiles" $
    aroundAll withShipped $ do
        tableSpec
        spawnSpec
        suppressionSpec
        consumableSpec
        regenSpec
        liveConsciousnessSpec
        intentSpec
        loadSpec

-----------------------------------------------------------------------
-- The owner's table (SD-23) -- the EXPECTED result the YAML is held to
-----------------------------------------------------------------------

-- | A shipped species, its profile, and the (consciousness, mind) modes it
--   resolves to.
data Row = Row Text Text ResolvedMode ResolvedMode

table ∷ [Row]
table =
    [ Row "acolyte"         "rich"    ModeSimulated ModeSimulated
    , Row "bear_brown"      "reduced" ModeSimulated ModeNominal
    , Row "red_squirrel"    "simple"  ModeSimulated ModeNominal
    , Row "technomule"      "reduced" ModeSimulated ModeNominal
    , Row "nomad_primitive" "exempt"  ModeSimulated ModeNominal ]

-- | The eleven mind-owned keys and the three derived values, from the spec.
mindKeys, derivedKeys ∷ [Text]
mindKeys =
    [ "mood", "emotional_pain", "caffeine", "mental_state", "mental_since"
    , "mental_until", "mental_next_roll_at", "mental_cooldown_until"
    , "mental_high_since", "mental_break_behavior", "thought_next_at" ]
derivedKeys = ["consciousness", "concentration", "state_of_mind"]

-- | The species that are NOT acolytes: the default-Nominal ones.
nominalSpecies ∷ [(Int, Text)]
nominalSpecies =
    [(2, "bear_brown"), (3, "red_squirrel"), (4, "technomule"), (5, "nomad_primitive")]

modeOf' ∷ PhysiologyRegistry → UnitDef → Text → IO ResolvedMode
modeOf' reg def sid = case registrySystem reg sid of
    Just sys → pure (resolveMode sys (udPhysiology def) False Nothing)
    Nothing → do
        expectationFailure ("registry has no system " <> show sid)
        pure ModeUnsupported

defOf ∷ EngineEnv → Text → IO UnitDef
defOf env n = do
    defs ← shippedDefs env
    case HM.lookup n defs of
        Just d  → pure d
        Nothing → do
            expectationFailure ("the shipped definition " <> show n <> " did not register")
            pure (error "unreachable")

tableSpec ∷ SpecWith Scene
tableSpec = describe "the SD-23 table, through the real definitions" $ do
    forM_ table $ \(Row name profile cons mind) →
        it (T.unpack name <> " is `" <> T.unpack profile <> "`: consciousness "
            <> show cons <> ", mind " <> show mind) $ \(env, _) → do
            reg ← readShippedPhysiologyRegistry
            def ← defOf env name
            upProfile (udPhysiology def) `shouldBe` Just profile
            modeOf' reg def "consciousness" `shouldReturn` cons
            modeOf' reg def "mind" `shouldReturn` mind
    it "resolves a mind override Simulated for a default-Nominal species and Nominal \
       \for the acolyte, by the real resolver (intent beats the default)" $ \(env, _) → do
        reg ← readShippedPhysiologyRegistry
        forM_ table $ \(Row name _ _ mind) → do
            def ← defOf env name
            let flipped = if mind ≡ ModeSimulated then PhysioNominal else PhysioSimulated
                got = fmap (\s → resolveMode s (udPhysiology def) False (Just flipped))
                           (registrySystem reg "mind")
            (name, got) `shouldBe`
                (name, Just (if flipped ≡ PhysioNominal then ModeNominal else ModeSimulated))
    it "leaves every bodyless definition Unsupported for both, and no other system's \
       \default changes (the five other-system columns are the species table's)" $ \(env, _) → do
        reg ← readShippedPhysiologyRegistry
        defs ← shippedDefs env
        let bodyless = [ n | (n, d) ← HM.toList defs, not (upBodyAuthored (udPhysiology d)) ]
        forM_ ["tiller", "unknown_unit", "white_tailed_deer"] $ \n →
            (n, n `elem` bodyless) `shouldBe` (n, True)
        forM_ bodyless $ \n → do
            def ← defOf env n
            forM_ ["consciousness", "mind"] $ \sid →
                modeOf' reg def sid `shouldReturn` ModeUnsupported

-----------------------------------------------------------------------
-- Spawn, and repeated passes
-----------------------------------------------------------------------

allFive ∷ [(Int, Text, Maybe UnitPhysiology, [(Text, PhysioMode)], [(Text, Float)])]
allFive = (1, "acolyte", Nothing, [], []) : [ (i, n, Nothing, [], []) | (i, n) ← nominalSpecies ]

-- | One pass of the three psychological entry points over the given units.
passes ∷ LuaBackendState → [Int] → IO ()
passes ls uids = void $ luaText ls $
    "for _, uid in ipairs({" <> T.intercalate "," (map tshow uids) <> "}) do \
    \  __brain.tick(uid, 0.5); \
    \  __thoughtsMod.tick(uid, unit.getInfo(uid), 0.5); \
    \  __mental.tick(uid, 0.5) \
    \end return true"

counters ∷ Text
counters = T.unlines
    [ "_G.__mental = require('scripts.mental_state')"
    , "_G.__brain = require('scripts.brain')"
    , "_G.__thoughtsMod = require('scripts.thoughts')"
    , "__mental.TUNE.BREAK_CHANCE_MAX = 1.0"
    , "__thoughtsMod.loadCatalogue()"
    , "_G.__n = 0"
    -- the Lua state is shared by the whole group: wrap the ORIGINAL emitter
    -- once, never a previous example's wrapper (it would count twice)
    , "if not _G.__emitOrig then _G.__emitOrig = thought.emit end"
    , "thought.emit = function(...) __n = __n + 1; return __emitOrig(...) end"
    , "return true" ]

spawnSpec ∷ SpecWith Scene
spawnSpec = describe "spawn and repeated passes under the shipped defaults" $ do
    it "spawns the acolyte with all eleven mind keys and the other four species \
       \with none; no derived value is stored for any" $ \(env, _) → do
        placeUnits env allFive
        a ← readInstance env 1
        forM_ mindKeys $ \k → (k, HM.member k (uiStats a)) `shouldBe` (k, True)
        forM_ nominalSpecies $ \(i, n) → do
            u ← readInstance env i
            forM_ (mindKeys ⧺ derivedKeys) $ \k →
                (n, k, HM.member k (uiStats u)) `shouldBe` (n, k, False)
        forM_ derivedKeys $ \k → (k, HM.member k (uiStats a)) `shouldBe` (k, False)
    it "keeps the default-Nominal keys absent over repeated brain, thought and mental \
       \passes across ten game minutes, and reads the declared substitutes" $ \(env, ls) → do
        placeUnits env allFive
        _ ← luaText ls counters
        forM_ [1 .. 60 ∷ Int] $ \i → do
            writeGameTime env (fromIntegral i * 10)
            passes ls (map fst nominalSpecies)
        forM_ nominalSpecies $ \(i, n) → do
            u ← readInstance env i
            forM_ (mindKeys ⧺ derivedKeys) $ \k →
                (n, k, HM.member k (uiStats u)) `shouldBe` (n, k, False)
            forM_ [ ("mood", "1"), ("emotional_pain", "0"), ("caffeine", "0")
                  , ("mental_state", "0"), ("mental_break_behavior", "0") ] $ \(k, want) →
                readOwned ls i k `shouldReturn` (want <> "|nominal")
            -- state_of_mind is the live consciousness value
            c ← luaNum ls ("unit.readPhysio(" <> tshow i <> ", 'consciousness')")
            luaNum ls ("unit.readPhysio(" <> tshow i <> ", 'state_of_mind')") >>= near 1e-9 c
        luaText ls "return tostring(__n)" `shouldReturn` "0"
        scriptErrors ls `shouldReturn` ""

-----------------------------------------------------------------------
-- T-S11-3 with the real defaults
-----------------------------------------------------------------------

suppressionSpec ∷ SpecWith Scene
suppressionSpec = describe "T-S11-3: the machinery under the real defaults" $ do
    forM_ nominalSpecies $ \(_, name) →
        it (T.unpack name <> " (default Nominal) refuses the stress batch the acolyte \
           \(default Simulated) enters BREAK from, with no override anywhere") $ \(env, ls) → do
            placeUnits env [(1, "acolyte", Nothing, [], []), (2, name, Nothing, [], [])]
            climate ls 20 0.5
            _ ← luaText ls counters
            writeGameTime env 100
            -- both lucid, no override on either
            luaNum ls "unit.readPhysio(1, 'consciousness')" >>= near 1e-9 1
            luaNum ls "unit.readPhysio(2, 'consciousness')" >>= near 1e-9 1
            luaText ls "return tostring(unit.getPhysioOverride(1, 'mind'))" `shouldReturn` "nil"
            luaText ls "return tostring(unit.getPhysioOverride(2, 'mind'))" `shouldReturn` "nil"
            let stress uid = "local now = engine.gameTime(); \
                \local ok, why = unit.commitPhysio(" <> tshow (uid ∷ Int) <> ", { mood = 0, \
                \emotional_pain = 1, mental_state = 1, \
                \mental_since = now - __mental.TUNE.SUSTAIN, mental_next_roll_at = now }); \
                \return tostring(ok) .. '|' .. tostring(why)"
            luaText ls (stress 1) `shouldReturn` "true|nil"
            luaNum ls "unit.readPhysio(1, 'state_of_mind')" >>= near 1e-9 0
            _ ← luaText ls "__mental.tick(1, 0.1); return true"
            readOwned ls 1 "mental_state" `shouldReturn` "2|simulated"
            luaText ls "return tostring(__n)" `shouldReturn` "1"
            out ← luaText ls (stress 2)
            out `shouldSatisfy` T.isPrefixOf "nil|forbidden_key:"
            -- no owned random draw, no record, no event from any entry point
            _ ← luaText ls "math.randomseed(5); return true"
            r1 ← luaText ls "return tostring(math.random())"
            _ ← luaText ls
                "math.randomseed(5); \
                \__brain.tick(2, 0.1); \
                \__thoughtsMod.tick(2, unit.getInfo(2), 0.1); \
                \__mental.tick(2, 0.1); __mental.forceBreak(2, 'flee'); return true"
            r2 ← luaText ls "return tostring(math.random())"
            r1 `shouldBe` r2
            readOwned ls 2 "mental_state" `shouldReturn` "0|nominal"
            luaText ls "return tostring(__n)" `shouldReturn` "1"
            luaText ls "return tostring(__mental.isBreaking(2))" `shouldReturn` "false"
            -- the mode did it: the unit is lucid (not delirious) throughout
            luaNum ls "unit.readPhysio(2, 'consciousness')" >>= near 1e-9 1
            u2 ← readInstance env 2
            forM_ mindKeys $ \k → (k, HM.member k (uiStats u2)) `shouldBe` (k, False)
            scriptErrors ls `shouldReturn` ""
    it "still collapses a default-Nominal bear whose blood oxygen falls below the \
       \consciousness floor: physiological collapse is consciousness's, not mind's" $ \(env, ls) → do
        placeUnits env [(2, "bear_brown", Nothing, [], [])]
        luaText ls "return tostring(unit.getPhysioOverride(2, 'mind'))" `shouldReturn` "nil"
        modifyInstance env 2 $ \i →
            i { uiStats = HM.insert "blood_oxygen" 0.4 (uiStats i) }
        luaNum ls "unit.readPhysio(2, 'consciousness')" >>= \c → c `shouldSatisfy` (< 0.15)
        _ ← drainCommands env
        runSeconds ls 0.1 0.1
        collapses env >>= (`shouldContain` [UnitId 2])
        scriptErrors ls `shouldReturn` ""

-----------------------------------------------------------------------
-- T-S11-6 with the real defaults
-----------------------------------------------------------------------

consumableSpec ∷ SpecWith Scene
consumableSpec = describe "T-S11-6: the consumable under the real defaults" $
    it "credits hydration to the hydration-pinned bear and acolyte alike, but caffeine \
       \and mood only to the acolyte: the bear's mind stays unpinned Nominal and its keys absent" $
        \(env, ls) → do
            placeUnits env
                [ (1, "acolyte", Nothing, [("hydration", PhysioSimulated)], [("hydration", 21)])
                , (2, "bear_brown", Nothing, [("hydration", PhysioSimulated)], [("hydration", 21)]) ]
            luaText ls "return tostring(unit.getPhysioOverride(2, 'mind'))" `shouldReturn` "nil"
            writeIORef (itemManagerRef env)
                (ItemManager (HM.singleton "coffee_pot" coffeeDef))
            forM_ [1, 2] $ \uid → modifyInstance env uid $ \i →
                i { uiInventory = [pot (fromIntegral uid * 100)], uiActivity = "idle" }
            _ ← luaText ls
                "_G.__consumable = require('scripts.consumable'); return true"
            let drink uid = luaText ls $ "local r = __consumable.drink(" <> tshow (uid ∷ Int)
                    <> ", 'coffee_pot'); return string.format('%.4f|%.4f|%.4f|%.4f', \
                    \r.sip, r.hydration, r.caffeine, r.mood)"
                fields t = map (\x → read (T.unpack x) ∷ Double) (T.splitOn "|" t)
            [sipA, hydA, cafA, moodA] ← fields <$> drink 1
            [sipB, hydB, cafB, moodB] ← fields <$> drink 2
            sipA `shouldSatisfy` (> 0)
            near 1e-6 sipA sipB
            near 1e-6 hydA hydB
            hydB `shouldSatisfy` (> 0)
            cafA `shouldSatisfy` (> 0)
            moodA `shouldSatisfy` (/= 0)
            luaNum ls "unit.getStatBase(1, 'hydration')" >>= near 1e-4 (21 + hydA)
            luaNum ls "unit.getStatBase(1, 'caffeine')" >>= \c → c `shouldSatisfy` (> 0)
            luaNum ls "unit.getStatBase(2, 'hydration')" >>= near 1e-4 (21 + hydB)
            cafB `shouldBe` 0
            moodB `shouldBe` 0
            bear ← readInstance env 2
            forM_ mindKeys $ \k → (k, HM.member k (uiStats bear)) `shouldBe` (k, False)
            scriptErrors ls `shouldReturn` ""

-----------------------------------------------------------------------
-- The caffeine stamina-regen bonus (T-S11-6, amended by the issue review)
-----------------------------------------------------------------------

-- | The bear's own stamina parameters carry no @caffeine_regen_bonus@, so a
--   bear proves nothing here. The nomad's copy the acolyte's, bonus 0.3
--   included: its stamina is pinned Simulated and the resource tick runs for
--   real.
regenSpec ∷ SpecWith Scene
regenSpec = describe "caffeine's stamina-regen bonus under the real defaults" $
    it "adds nothing for a default-Nominal nomad even holding a stored caffeine of 1, \
       \and exactly caffeine x 0.3 for a Simulated mind (nomad pinned, acolyte default)" $
        \(env, ls) → do
            let pinStamina = ("stamina", PhysioSimulated)
            placeUnits env
                [ (1, "nomad_primitive", Nothing, [pinStamina], [("stamina", 5)])
                , (2, "nomad_primitive", Nothing, [pinStamina, ("mind", PhysioSimulated)]
                  , [("stamina", 5), ("caffeine", 1.0)])
                , (3, "nomad_primitive", Nothing, [pinStamina, ("mind", PhysioSimulated)]
                  , [("stamina", 5), ("caffeine", 0)])
                , (4, "acolyte", Nothing, [], [("stamina", 5), ("caffeine", 1.0)])
                , (5, "acolyte", Nothing, [], [("stamina", 5), ("caffeine", 0)]) ]
            -- the default-Nominal nomad's mind is unpinned and holds NO caffeine key;
            -- a raw stored one is placed to prove the READ ignores it by mode
            luaText ls "return tostring(unit.getPhysioOverride(1, 'mind'))" `shouldReturn` "nil"
            u1 ← readInstance env 1
            HM.member "caffeine" (uiStats u1) `shouldBe` False
            modifyInstance env 1 $ \i → i { uiStats = HM.insert "caffeine" 1.0 (uiStats i) }
            luaNum ls "unit.getStatBase(1, 'caffeine')" >>= near 1e-9 1.0
            luaNum ls "unit.readPhysio(1, 'caffeine')" >>= near 1e-9 0
            _ ← luaText ls
                "_G.__tick = require('scripts.unit_resource_tick'); \
                \_G.__cfg = require('scripts.unit_resource_config'); return true"
            let regen uid def = luaNum ls $
                    "(function() local before = unit.readPhysio(" <> tshow (uid ∷ Int)
                    <> ", 'stamina'); __tick.tickResource(" <> tshow uid <> ", '" <> def
                    <> "', 'stamina', __cfg." <> def <> ".stamina, 'idle', 'standing', 1.0); \
                    \return unit.readPhysio(" <> tshow uid <> ", 'stamina') - before end)()"
            nominal ← regen 1 "nomad_primitive"
            withCaffeine ← regen 2 "nomad_primitive"
            noCaffeine ← regen 3 "nomad_primitive"
            acolyteCaf ← regen 4 "acolyte"
            acolyteNone ← regen 5 "acolyte"
            -- the tick really regenerates, so the equalities below are not vacuous
            noCaffeine `shouldSatisfy` (> 0)
            acolyteNone `shouldSatisfy` (> 0)
            near 1e-6 noCaffeine nominal
            near 1e-6 (noCaffeine + 0.3) withCaffeine
            -- the acolyte's positive control: its default mind holds caffeine > 0
            near 1e-6 (acolyteNone + 0.3) acolyteCaf
            scriptErrors ls `shouldReturn` ""

-----------------------------------------------------------------------
-- Live consciousness under a default Nominal mind
-----------------------------------------------------------------------

liveConsciousnessSpec ∷ SpecWith Scene
liveConsciousnessSpec = describe "consciousness stays live under a default Nominal mind" $ do
    it "responds to injury pain, low stamina and a hypoxic blood oxygen: concentration and \
       \consciousness move, state_of_mind equals consciousness, no psychological episode fires" $
        \(env, ls) → do
            placeUnits env [(2, "bear_brown", Nothing, [], [])]
            _ ← luaText ls counters
            let conc = luaNum ls "unit.readPhysio(2, 'concentration')"
                cons = luaNum ls "unit.readPhysio(2, 'consciousness')"
                som  = luaNum ls "unit.readPhysio(2, 'state_of_mind')"
            c0 ← conc
            c0 `shouldSatisfy` (> 0.95)
            -- low stamina: concentration falls, consciousness does not
            modifyInstance env 2 $ \i → i { uiStats = HM.insert "stamina" 10 (uiStats i) }
            c1 ← conc
            c1 `shouldSatisfy` (< c0 - 0.2)
            cons >>= near 1e-9 1
            -- injury pain: a fracture erodes concentration further
            modifyInstance env 2 $ \i → i { uiWounds = [fracture] }
            c2 ← conc
            c2 `shouldSatisfy` (< c1 - 0.05)
            -- physiological impairment: hypoxia lowers consciousness itself
            modifyInstance env 2 $ \i →
                i { uiStats = HM.insert "blood_oxygen" 0.65 (uiStats i) }
            k ← cons
            k `shouldSatisfy` (\v → v > 0.3 ∧ v < 0.7)
            som >>= near 1e-9 k
            c3 ← conc
            c3 `shouldSatisfy` (< c2)
            -- passes of every psychological entry point change nothing mental
            forM_ [1 .. 30 ∷ Int] $ \i → do
                writeGameTime env (fromIntegral i * 10)
                passes ls [2]
            u ← readInstance env 2
            forM_ mindKeys $ \key → (key, HM.member key (uiStats u)) `shouldBe` (key, False)
            readOwned ls 2 "mental_state" `shouldReturn` "0|nominal"
            luaText ls "return tostring(__n)" `shouldReturn` "0"
            -- the shared finite effectiveness formula reads the live concentration
            c4 ← conc
            luaNum ls "unit.getMentalEffectiveness(2)" >>= near 1e-5 (0.75 + 0.25 * c4)
            scriptErrors ls `shouldReturn` ""
    it "keeps the acolyte's euphoria: one +0.10 concentration lift and the separate \
       \x1.10 effectiveness bonus" $ \(env, ls) → do
        placeUnits env [(1, "acolyte", Nothing, [], [("stamina", 20)])]
        writeGameTime env 500
        let conc = luaNum ls "unit.readPhysio(1, 'concentration')"
            eff  = luaNum ls "unit.getMentalEffectiveness(1)"
        c0 ← conc
        e0 ← eff
        -- the seeded mood at a stamina fraction of 0.2 (1 - 0.20 x 0.8)
        luaNum ls "unit.readPhysio(1, 'state_of_mind')" >>= near 1e-3 0.84
        c0 `shouldSatisfy` (\v → v > 0.4 ∧ v < 0.6)
        near 1e-5 (0.75 + 0.25 * c0) e0
        luaText ls "local now = engine.gameTime(); \
                   \local ok, why = unit.commitPhysio(1, { mental_state = 3, \
                   \mental_since = now, mental_until = now + 100, mental_high_since = now }); \
                   \return tostring(ok) .. '|' .. tostring(why)"
            `shouldReturn` "true|nil"
        c1 ← conc
        near 1e-5 (c0 + 0.10) c1
        eff >>= near 1e-4 ((0.75 + 0.25 * c1) * 1.10)
        scriptErrors ls `shouldReturn` ""

-- | An unhealed severity-1 fracture (the pain source the unified rule prices).
fracture ∷ Wound
fracture = Wound
    { woundPart = "l_thigh", woundKind = "fracture", woundSeverity = 1.0
    , woundAt = 0, woundBandage = 1.0, woundClot = 0.0, woundHeal = 0.0
    , woundDressing = "", woundInfection = 0.0, woundClean = True
    , woundInfectionType = "", woundNecrosis = 0.0 }

-----------------------------------------------------------------------
-- Intent: set, clear and the transition hooks
-----------------------------------------------------------------------

intentSpec ∷ SpecWith Scene
intentSpec = describe "set/clear intent and the transition hooks, on the shipped species" $ do
    it "seeds a pinned-Simulated bear's mind whole, re-rolls its thought clock, keeps \
       \its simulation, and scrubs it again when the pin is cleared" $ \(env, ls) → do
        placeUnits env [(2, "bear_brown", Nothing, [], [])]
        _ ← luaText ls counters
        writeGameTime env 123
        luaText ls "return tostring(__physiology.setMode(2, 'mind', 'simulated'))"
            `shouldReturn` "true"
        u ← readInstance env 2
        forM_ mindKeys $ \k → (k, HM.member k (uiStats u)) `shouldBe` (k, True)
        forM_ derivedKeys $ \k → (k, HM.member k (uiStats u)) `shouldBe` (k, False)
        luaNum ls "unit.getStatBase(2, 'mental_since')" >>= near 1e-6 123
        luaNum ls "unit.getStatBase(2, 'mental_next_roll_at')" >>= near 1e-6 131
        luaNum ls "unit.getStatBase(2, 'thought_next_at')" >>= \t →
            t `shouldSatisfy` (\v → v ≥ 123 ∧ v ≤ 153)
        -- the pinned mind simulates: a stressed batch is accepted and breaks
        let stress = "local now = engine.gameTime(); \
                \local ok, why = unit.commitPhysio(2, { mood = 0, \
                \emotional_pain = 1, mental_state = 1, \
                \mental_since = now - __mental.TUNE.SUSTAIN, mental_next_roll_at = now }); \
                \return tostring(ok) .. '|' .. tostring(why)"
        luaText ls stress `shouldReturn` "true|nil"
        _ ← luaText ls "__mental.tick(2, 0.1); return true"
        readOwned ls 2 "mental_state" `shouldReturn` "2|simulated"
        -- clearing follows the NEW default: Nominal, scrubbed, episode gone
        luaText ls "local ok, ch = __physiology.clearMode(2, 'mind'); \
                   \return tostring(ok) .. '|' .. tostring(ch)" `shouldReturn` "true|true"
        u' ← readInstance env 2
        forM_ mindKeys $ \k → (k, HM.member k (uiStats u')) `shouldBe` (k, False)
        luaText ls "return tostring(__mental.isBreaking(2))" `shouldReturn` "false"
        -- an unpinned clear changes nothing and re-seeds nothing
        luaText ls "local ok, ch = __physiology.clearMode(2, 'mind'); \
                   \return tostring(ok) .. '|' .. tostring(ch)" `shouldReturn` "true|false"
        scriptErrors ls `shouldReturn` ""
    it "keeps a Nominal-pinned acolyte Nominal, and returns it to its Simulated default \
       \(seeded again) when the pin is cleared" $ \(env, ls) → do
        placeUnits env [(1, "acolyte", Nothing, [("mind", PhysioNominal)], [])]
        _ ← luaText ls counters
        u ← readInstance env 1
        forM_ mindKeys $ \k → (k, HM.member k (uiStats u)) `shouldBe` (k, False)
        writeGameTime env 77
        luaText ls "local ok, ch = __physiology.clearMode(1, 'mind'); \
                   \return tostring(ok) .. '|' .. tostring(ch)" `shouldReturn` "true|true"
        u' ← readInstance env 1
        forM_ mindKeys $ \k → (k, HM.member k (uiStats u')) `shouldBe` (k, True)
        luaNum ls "unit.getStatBase(1, 'mental_since')" >>= near 1e-6 77

-----------------------------------------------------------------------
-- Changed-default load normalization, through the real codec and gate
-----------------------------------------------------------------------

-- | A shipped species as the S11a content had it: its definition's physiology
--   with the mind default put back to Simulated (the old interim default).
oldDefaults ∷ EngineEnv → Text → IO UnitPhysiology
oldDefaults env n = do
    def ← defOf env n
    let up = udPhysiology def
    pure up { upDefaults = HM.insert "mind" PhysioSimulated (upDefaults up) }

-- | A mid-episode durable mind: a BREAK with every deadline set.
breakEpisode ∷ [(Text, Float)]
breakEpisode =
    [ ("mood", 0.3), ("emotional_pain", 0.4), ("caffeine", 0.5)
    , ("mental_state", 2), ("mental_since", 100), ("mental_until", 200)
    , ("mental_next_roll_at", 120), ("mental_cooldown_until", 0)
    , ("mental_high_since", 0), ("mental_break_behavior", 1)
    , ("thought_next_at", 150) ]

setStats ∷ EngineEnv → Int → [(Text, Float)] → IO ()
setStats env i kvs = modifyInstance env i $ \u →
    u { uiStats = foldr (uncurry HM.insert) (uiStats u) kvs }

loadSpec ∷ SpecWith Scene
loadSpec = describe "load honours the live default and pinned intent" $ do
    it "an unpinned bear, squirrel, technomule and nomad saved under the old all-Simulated \
       \mind adopt the new default on load: mind keys and episode scrubbed, ONE note per \
       \unit, a second pass and a re-save/reload repair nothing, nothing resumes" $
        \(env, ls) → do
            reg ← readShippedPhysiologyRegistry
            defs ← shippedDefs env
            olds ← forM nominalSpecies $ \(i, n) → do
                up ← oldDefaults env n
                pure (i, n, Just up, [], [])
            placeUnits env olds
            forM_ nominalSpecies $ \(i, _) → do
                setStats env i breakEpisode
                before ← readInstance env i
                forM_ mindKeys $ \k → (k, HM.member k (uiStats before)) `shouldBe` (k, True)
            onSavePage env (map fst nominalSpecies)
            pg ← pageSnapshotOf env ≫= codecRoundTrip
            (us1, notes1) ← gate reg defs (pgsUnits pg)
            length notes1 `shouldBe` length nominalSpecies
            forM_ nominalSpecies $ \(i, n) →
                case HM.lookup (UnitId (fromIntegral i)) (usnInstances us1) of
                    Nothing → expectationFailure ("the unit vanished: " <> T.unpack n)
                    Just u → do
                        forM_ (mindKeys ⧺ derivedKeys) $ \k →
                            (n, k, HM.member k (uisStats u)) `shouldBe` (n, k, False)
                        -- the other systems' state is untouched, and no pin was invented
                        forM_ ["body_mass", "frame_mass", "fat_mass", "lean_mass", "height"] $ \k →
                            (n, k, HM.member k (uisStats u)) `shouldBe` (n, k, True)
                        uisPhysioOverrides u `shouldBe` HM.empty
            (us2, notes2) ← gate reg defs us1
            notes2 `shouldBe` []
            us2 `shouldBe` us1
            back ← codecRoundTrip (pg { pgsUnits = us2 })
            (us3, notes3) ← gate reg defs (pgsUnits back)
            notes3 `shouldBe` []
            us3 `shouldBe` us2
            -- loaded the way load loads it: no episode effect or thought resumes
            let (restored, orphans, _, _, _) = fromUnitSnapshot page1 defs
                    emptyInfectionManager emptyEquipmentClassManager emptyItemManager us3
            orphans `shouldBe` []
            writeIORef (unitManagerRef env) restored
            _ ← luaText ls counters
            _ ← luaText ls "__resources.onSaveLoaded({ 2, 3, 4, 5 }, {}); return true"
            writeGameTime env 150
            passes ls (map fst nominalSpecies)
            forM_ nominalSpecies $ \(i, _) → do
                readOwned ls i "mental_state" `shouldReturn` "0|nominal"
                luaText ls ("return tostring(__mental.isBreaking(" <> tshow i <> "))")
                    `shouldReturn` "false"
            luaText ls "return tostring(__n)" `shouldReturn` "0"
            scriptErrors ls `shouldReturn` ""

    it "preserves a pinned-Simulated bear's complete episode exactly, repairs a partial \
       \one only in its absent keys, and keeps a Nominal-pinned acolyte Nominal" $
        \(env, _) → do
            reg ← readShippedPhysiologyRegistry
            defs ← shippedDefs env
            placeUnits env
                [ (2, "bear_brown", Nothing, [("mind", PhysioSimulated)], [])
                , (3, "technomule", Nothing, [("mind", PhysioSimulated)], [])
                , (1, "acolyte", Nothing, [("mind", PhysioNominal)], []) ]
            setStats env 2 breakEpisode
            setStats env 3 breakEpisode
            -- the technomule's episode is partial: it lost its end and its behavior
            modifyInstance env 3 $ \u →
                u { uiStats = foldr HM.delete (uiStats u)
                        ["mental_until", "mental_break_behavior"] }
            onSavePage env [1, 2, 3]
            before2 ← readInstance env 2
            pg ← pageSnapshotOf env ≫= codecRoundTrip
            (us, notes) ← gate reg defs (pgsUnits pg)
            -- exactly one note: the partial episode's repair
            length notes `shouldBe` 1
            case ( HM.lookup (UnitId 2) (usnInstances us)
                 , HM.lookup (UnitId 3) (usnInstances us)
                 , HM.lookup (UnitId 1) (usnInstances us) ) of
                (Just bear, Just mule, Just acolyte) → do
                    uisStats bear `shouldBe` uiStats before2
                    uisPhysioOverrides bear `shouldBe` HM.fromList [("mind", PhysioSimulated)]
                    forM_ [ (k, v) | (k, v) ← breakEpisode
                          , k `notElem` ["mental_until", "mental_break_behavior"] ] $ \(k, v) →
                        HM.lookup k (uisStats mule) `shouldBe` Just v
                    forM_ ["mental_until", "mental_break_behavior"] $ \k →
                        (k, HM.member k (uisStats mule)) `shouldBe` (k, True)
                    forM_ mindKeys $ \k →
                        (k, HM.member k (uisStats acolyte)) `shouldBe` (k, False)
                    uisPhysioOverrides acolyte `shouldBe` HM.fromList [("mind", PhysioNominal)]
                _ → expectationFailure "a unit vanished"
            (us', notes') ← gate reg defs us
            (us', notes') `shouldBe` (us, [])
