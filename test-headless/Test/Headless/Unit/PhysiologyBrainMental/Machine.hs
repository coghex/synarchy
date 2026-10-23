{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The mental machine under each mode (#2838, S11a): T-S11-3 (machinery,
--   with explicit overrides, not S11b's defaults), T-S11-5 (initialization and
--   legacy episodes) and T-S11-6 (the Nominal-mind consumable). The shipped
--   Lua runs over the real registered @unit.*@ API; nothing is stubbed but the
--   thought-record counter the examples read.
module Test.Headless.Unit.PhysiologyBrainMental.Machine (spec, coffeeDef, pot) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef)
import Engine.Core.State (EngineEnv(..))
import Item.Types
    (ItemContainer(..), ItemDef(..), ItemInstance(..), ItemManager(..))
import Engine.Asset.Handle (TextureHandle(..))
import Test.Headless.Unit.PhysiologyFixture (readShippedPhysiologyRegistry)
import Test.Headless.Unit.PhysiologyModes.Harness
import Test.Headless.Unit.TransferApi (mkItem)
import Unit.Physiology.Types
import Unit.Types
import World.Save.Physiology
    ( PhysiologyLoadDefinition, PhysiologyNormalization(..)
    , physiologyLoadDefinition, physiologyLoadGate )
import World.Save.Types (UnitInstanceSnapshot(..), UnitSnapshot(..), toUnitSnapshot)
import World.Page.Types (WorldPageId(..))

spec ∷ Spec
spec = describe "the mental machine under each mode" $ do
    stressSpec
    seedSpec
    injuredSeedSpec
    legacySpec
    consumableSpec

-- | Count thought records; pin the lucid, temperate, well-supplied fixture.
counters ∷ Text
counters = T.unlines
    [ "_G.__mental = require('scripts.mental_state')"
    , "_G.__brain = require('scripts.brain')"
    , "_G.__thoughtsMod = require('scripts.thoughts')"
    , "__mental.TUNE.BREAK_CHANCE_MAX = 1.0"
    , "_G.__n = 0"
    , "local emit = thought.emit"
    , "thought.emit = function(...) __n = __n + 1; return emit(...) end"
    , "return true" ]

-----------------------------------------------------------------------
-- T-S11-3
-----------------------------------------------------------------------

stressSpec ∷ Spec
stressSpec = describe "T-S11-3: the machinery, with explicit overrides" $ do
    it "breaks an acolyte's stressed Simulated mind, refuses the same stress \
       \batch for a bear whose mind is overridden Nominal, and a Nominal mind \
       \has no episodes, thoughts or forced break — by mode, not by delirium" $
        withScene [ unitSpec 1 "acolyte" (Just "rich")
                  , (unitSpec 2 "bear_brown" (Just "reduced"))
                      { usOverrides = [("mind", PhysioNominal)] } ] $ \(env, ls) → do
            _ ← luaText ls counters
            writeGameTime env 100
            -- both lucid
            luaNum ls "unit.readPhysio(1, 'consciousness')" >>= near 1e-9 1
            luaNum ls "unit.readPhysio(2, 'consciousness')" >>= near 1e-9 1
            let stress uid = "local now = engine.gameTime(); \
                \local ok, why = unit.commitPhysio(" <> tshow (uid ∷ Int) <> ", { mood = 0, \
                \emotional_pain = 1, mental_state = 1, \
                \mental_since = now - __mental.TUNE.SUSTAIN, mental_next_roll_at = now }); \
                \return tostring(ok) .. '|' .. tostring(why)"
            -- the acolyte: state of mind 0 (severity exactly 1), STRESSED for SUSTAIN
            luaText ls (stress 1) `shouldReturn` "true|nil"
            luaNum ls "unit.readPhysio(1, 'state_of_mind')" >>= near 1e-9 0
            _ ← luaText ls "__mental.tick(1, 0.1); return true"
            readOwned ls 1 "mental_state" `shouldReturn` "2|simulated"
            luaText ls "return tostring(__n)" `shouldReturn` "1"
            -- the bear: the SAME batch is refused whole (no key written)
            out ← luaText ls (stress 2)
            out `shouldSatisfy` T.isPrefixOf "nil|forbidden_key:"
            -- every mind entry point is a no-op: no draw, no record, no event
            _ ← luaText ls "math.randomseed(5); return true"
            r1 ← luaText ls "return tostring(math.random())"
            _ ← luaText ls
                "math.randomseed(5); \
                \__brain.tick(2, 0.1); \
                \__thoughtsMod.loadCatalogue(); __thoughtsMod.tick(2, unit.getInfo(2), 0.1); \
                \__mental.tick(2, 0.1); __mental.forceBreak(2, 'flee'); return true"
            r2 ← luaText ls "return tostring(math.random())"
            r1 `shouldBe` r2
            readOwned ls 2 "mental_state" `shouldReturn` "0|nominal"
            luaText ls "return tostring(__n)" `shouldReturn` "1"        -- still only the acolyte's
            luaText ls "return tostring(__mental.isBreaking(2))" `shouldReturn` "false"
            -- suppression came from the mode: the bear is lucid
            luaNum ls "unit.readPhysio(2, 'consciousness')" >>= near 1e-9 1
            instance2 ← readInstance env 2
            HM.member "mental_state" (uiStats instance2) `shouldBe` False
            HM.member "mood" (uiStats instance2) `shouldBe` False

    it "still collapses a Nominal-mind bear whose blood oxygen falls below the \
       \consciousness floor: physiological collapse is consciousness's, not mind's" $
        withScene [ (unitSpec 2 "bear_brown" (Just "reduced"))
                      { usOverrides = [("mind", PhysioNominal)] } ] $ \(env, ls) → do
            modifyInstance env 2 $ \i →
                i { uiStats = HM.insert "blood_oxygen" 0.4 (uiStats i) }
            luaNum ls "unit.readPhysio(2, 'consciousness')" >>= \c → c `shouldSatisfy` (< 0.15)
            _ ← drainCommands env
            runSeconds ls 0.1 0.1
            collapses env >>= (`shouldContain` [UnitId 2])
            scriptErrors ls `shouldReturn` ""

-----------------------------------------------------------------------
-- T-S11-5: seeds and hooks
-----------------------------------------------------------------------

seedSpec ∷ Spec
seedSpec = describe "T-S11-5: initialization" $ do
    it "holds every seed on a fresh Simulated mind: mood is the current drift \
       \target, the clock and the cadence are the seeding sample" $
        withScene [ (unitSpec 1 "acolyte" (Just "rich")) { usStats = [("stamina", 20)] } ] $
            \(_, ls) → do
                _ ← luaText ls counters
                -- stamina fraction 0.2: 1 - 0.20 x (1 - 0.2) = 0.84, no pain, lucid, aware
                forM_ [ ("mood", 0.84), ("emotional_pain", 0), ("caffeine", 0)
                      , ("mental_state", 0), ("mental_since", 0), ("mental_until", 0)
                      , ("mental_next_roll_at", 8), ("mental_cooldown_until", 0)
                      , ("mental_high_since", 0), ("mental_break_behavior", 0)
                      , ("thought_next_at", 0) ] $ \(k, want) →
                    luaNum ls ("unit.getStatBase(1, '" <> k <> "')") >>= near 1e-5 want
                -- ... and the Haskell seed equals the Lua drift target for the same inputs
                luaNum ls "unit.getStatBase(1, 'mood') - __brain.moodTarget(1)" >>= near 1e-5 0

    it "re-rolls the thought clock to now + random x 30 on the gameplay stream \
       \only on an EFFECTIVE change to a Simulated mind" $
        withScene [ (unitSpec 1 "acolyte" (Just "rich"))
                      { usOverrides = [("mind", PhysioNominal)] } ] $ \(env, ls) → do
            _ ← luaText ls counters
            writeGameTime env 123
            _ ← luaText ls "math.randomseed(7); return true"
            expected ← luaNum ls "123 + math.random() * 30"
            _ ← luaText ls "math.randomseed(7); return true"
            luaText ls "return tostring(__physiology.setMode(1, 'mind', 'simulated'))"
                `shouldReturn` "true"
            luaNum ls "unit.getStatBase(1, 'thought_next_at')" >>= near 1e-3 expected
            luaNum ls "unit.getStatBase(1, 'mental_since')" >>= near 1e-6 123
            luaNum ls "unit.getStatBase(1, 'mental_next_roll_at')" >>= near 1e-6 131
            -- an unchanged set is not an event: nothing re-rolls
            before ← luaNum ls "unit.getStatBase(1, 'thought_next_at')"
            _ ← luaText ls "__physiology.setMode(1, 'mind', 'simulated'); return true"
            luaNum ls "unit.getStatBase(1, 'thought_next_at')" >>= near 1e-9 before

-- | An unhealed severity-1 fracture: @kindPainFactor@ prices it 1.6 (the Lua
--   getter used to price every kind but slash, stab and blunt at 1.0).
fracture ∷ Wound
fracture = Wound
    { woundPart = "l_thigh", woundKind = "fracture", woundSeverity = 1.0
    , woundAt = 0, woundBandage = 1.0, woundClot = 0.0, woundHeal = 0.0
    , woundDressing = "", woundInfection = 0.0, woundClean = True
    , woundInfectionType = "", woundNecrosis = 0.0 }

injuredSeedSpec ∷ Spec
injuredSeedSpec = describe "T-S11-5: the mood seed prices pain as the drift does" $ do
    it "seeds an injured unit's mood at its Nominal to Simulated transition to the \
       \Lua drift target (one pain, one kind factor)" $
        withScene [ (unitSpec 1 "acolyte" (Just "rich"))
                      { usOverrides = [("mind", PhysioNominal)], usWounds = [fracture] } ] $
            \(_, ls) → do
                _ ← luaText ls counters
                -- the getter and the Haskell pain agree: 1.0 x 1.6
                luaNum ls "unit.getPain(1)" >>= near 1e-5 1.6
                luaText ls "return tostring(__physiology.setMode(1, 'mind', 'simulated'))"
                    `shouldReturn` "true"
                -- 1 - 0.45 x (1.6 / 5): not the 0.91 a pain of 1.0 would give
                luaNum ls "unit.getStatBase(1, 'mood')" >>= near 1e-5 0.856
                luaNum ls "unit.getStatBase(1, 'mood') - __brain.moodTarget(1)" >>= near 1e-5 0

    it "repairs an absent mood at load with the same pain, priced from the saved wounds" $
        withScene [ (unitSpec 1 "acolyte" (Just "rich")) { usWounds = [fracture] } ] $
            \(env, ls) → do
                _ ← luaText ls counters
                (reg, defs, u0) ← gateInputs env
                let legacy = u0 { uisStats = HM.delete "mood" (uisStats u0) }
                case gate reg defs 123 legacy of
                    Left msgs → expectationFailure (show msgs)
                    Right (u1, notes) → do
                        length notes `shouldBe` 1
                        case HM.lookup "mood" (uisStats u1) of
                            Just m → realToFrac m `shouldSatisfy` (\v → abs (v - 0.856 ∷ Double) < 1e-5)
                            Nothing → expectationFailure "mood was not repaired"
                        -- the live Lua target for the same unit agrees
                        luaNum ls "__brain.moodTarget(1)" >>= near 1e-5 0.856

-----------------------------------------------------------------------
-- T-S11-5: legacy episodes through the load gate
-----------------------------------------------------------------------

-- | The canonical snapshot of unit 1 (seeded as a spawn would), the registry
--   and the definitions, for the gate.
gateInputs ∷ EngineEnv → IO (PhysiologyRegistry, HM.HashMap Text PhysiologyLoadDefinition, UnitInstanceSnapshot)
gateInputs env = do
    reg ← readShippedPhysiologyRegistry
    um ← readIORef (unitManagerRef env)
    inst ← readInstance env 1
    let snap = toUnitSnapshot (uiPage inst) um
        defs = HM.map physiologyLoadDefinition (umDefs um)
    case HM.lookup (UnitId 1) (usnInstances snap) of
        Just u → pure (reg, defs, u)
        Nothing → expectationFailure "no unit snapshot" >> error "unreachable"

gate ∷ PhysiologyRegistry → HM.HashMap Text PhysiologyLoadDefinition → Double → UnitInstanceSnapshot
     → Either [Text] (UnitInstanceSnapshot, [Text])
gate reg defs now u = case physiologyLoadGate reg defs now [(WorldPageId "p", UnitSnapshot (HM.singleton (UnitId 1) u) 10 mempty mempty)] of
    Left msgs → Left msgs
    Right ([(_, us)], notes) → case HM.lookup (UnitId 1) (usnInstances us) of
        Just u' → Right (u', concatMap pnNotes notes)
        Nothing → Left ["unit vanished"]
    Right _ → Left ["unexpected page shape"]

legacySpec ∷ Spec
legacySpec = describe "T-S11-5: legacy episodes (the load gate)" $ do
    it "gives a BREAK or EUPHORIC episode that lost its end the sample, so it expires \
       \on the next normal tick, and a BREAK without a behavior WANDER" $
        forM_ [2, 3] $ \st →
            withScene [unitSpec 1 "acolyte" (Just "rich")] $ \(env, ls) → do
                (reg, defs, u0) ← gateInputs env
                let legacy = u0 { uisStats = foldr HM.delete
                        (HM.insert "mental_state" st (uisStats u0))
                        ["mental_until", "mental_break_behavior"] }
                case gate reg defs 123 legacy of
                    Left msgs → expectationFailure (show msgs)
                    Right (u1, notes) → do
                        HM.lookup "mental_until" (uisStats u1) `shouldBe` Just 123
                        HM.lookup "mental_break_behavior" (uisStats u1) `shouldBe` Just 0
                        HM.lookup "mental_state" (uisStats u1) `shouldBe` Just st
                        length notes `shouldBe` 1
                        -- idempotent: a second pass repairs nothing
                        gate reg defs 123 u1 `shouldBe` Right (u1, [])
                        -- load it and run the NORMAL tick: the episode ends
                        _ ← luaText ls counters
                        writeGameTime env 123
                        modifyInstance env 1 $ \i → i { uiStats = uisStats u1 }
                        _ ← luaText ls "__mental.tick(1, 0.1); return true"
                        readOwned ls 1 "mental_state" `shouldReturn` "0|simulated"
                        luaNum ls "unit.getStatBase(1, 'mental_cooldown_until')"
                            >>= \c → c `shouldSatisfy` (> 300)

    it "fills the other absent keys with their declared seeds and preserves a \
       \complete episode, stale deadlines included, exactly" $
        withScene [unitSpec 1 "acolyte" (Just "rich")] $ \(env, _) → do
            (reg, defs, u0) ← gateInputs env
            let complete = u0 { uisStats = foldr (uncurry HM.insert) (uisStats u0)
                    [ ("mental_state", 2), ("mental_since", 1), ("mental_until", 5)
                    , ("mental_next_roll_at", 9), ("mental_cooldown_until", 7)
                    , ("mental_break_behavior", 1) ] }
            gate reg defs 123 complete `shouldBe` Right (complete, [])
            -- partially populated: the absent keys take their seeds, present ones stay
            let sparse = u0 { uisStats = foldr HM.delete (uisStats u0)
                    ["mental_since", "mental_next_roll_at", "thought_next_at", "caffeine"] }
            case gate reg defs 123 sparse of
                Left msgs → expectationFailure (show msgs)
                Right (u1, notes) → do
                    HM.lookup "mental_since" (uisStats u1) `shouldBe` Just 123
                    HM.lookup "mental_next_roll_at" (uisStats u1) `shouldBe` Just 131
                    HM.lookup "thought_next_at" (uisStats u1) `shouldBe` Just 123
                    HM.lookup "caffeine" (uisStats u1) `shouldBe` Just 0
                    HM.lookup "mood" (uisStats u1) `shouldBe` HM.lookup "mood" (uisStats u0)
                    length notes `shouldBe` 1

    it "scrubs legacy consciousness, concentration and state_of_mind in every \
       \mode, logs once, and a second load repairs nothing" $
        forM_ [Nothing, Just PhysioNominal] $ \pin →
            withScene [ (unitSpec 1 "acolyte" (Just "rich"))
                          { usOverrides = maybe [] (\m → [("mind", m)]) pin } ] $ \(env, _) → do
                (reg, defs, u0) ← gateInputs env
                let legacy = u0 { uisStats = foldr (uncurry HM.insert) (uisStats u0)
                        [("consciousness", 0.3), ("concentration", 0.4), ("state_of_mind", 0.2)] }
                case gate reg defs 123 legacy of
                    Left msgs → expectationFailure (show msgs)
                    Right (u1, notes) → do
                        forM_ ["consciousness", "concentration", "state_of_mind"] $ \k →
                            HM.member k (uisStats u1) `shouldBe` False
                        -- one note per system holding a stale derived key
                        length notes `shouldBe` 2
                        gate reg defs 123 u1 `shouldBe` Right (u1, [])

-----------------------------------------------------------------------
-- T-S11-6: the consumable
-----------------------------------------------------------------------

coffeeDef ∷ ItemDef
coffeeDef = ItemDef
    { idName = "coffee_pot", idDisplayName = "Coffee Pot"
    , idTexture = TextureHandle 0, idIconTexture = TextureHandle 0, idWeight = 0.4
    , idWeightSpec = Nothing, idBulk = 1.6, idStorage = Nothing, idKind = "misc"
    , idCategory = "Supplies", idMake = "", idMaterial = ""
    , idQualitySpec = Just (30, 90), idQualityTiers = []
    , idContainer = Just (ItemContainer { icCapacity = 1.0, icHolds = "coffee"
                                        , icFillWeight = 1.0, icDefaultFill = 1.0 })
    , idDefaultContents = [], idFood = Nothing, idWeapon = Nothing
    , idArmor = Nothing, idUnequippable = False, idBuffs = []
    , idInsulation = 0, idSourcePath = "test-fixture" }

pot ∷ Word64 → ItemInstance
pot iid = (mkItem "coffee_pot" iid 0.4)
    { iiCurrentFill = 1.0, iiQuality = 90, iiTemp = Just 80 }

consumableSpec ∷ Spec
consumableSpec = describe "T-S11-6: the Nominal-mind consumable" $ do
    it "credits hydration to both, but caffeine and mood only to a Simulated mind: \
       \the bear's keys stay absent, the fluid draws and the result still succeed" $
        withScene [ (unitSpec 1 "acolyte" (Just "rich"))
                      { usStats = [("hydration", 21)] }
                  , (unitSpec 2 "bear_brown" (Just "reduced"))
                      { usOverrides = [("mind", PhysioNominal), ("hydration", PhysioSimulated)]
                      , usStats = [("hydration", 21)] } ] $ \(env, ls) → do
            writeIORef (itemManagerRef env)
                (ItemManager (HM.singleton "coffee_pot" coffeeDef))
            forM_ [1, 2] $ \uid → modifyInstance env uid $ \i →
                i { uiInventory = [pot (fromIntegral uid * 100)], uiActivity = "idle" }
            _ ← luaText ls
                "_G.__consumable = require('scripts.consumable'); return true"
            let drink uid = luaText ls $ "local r = __consumable.drink(" <> tshow (uid ∷ Int)
                    <> ", 'coffee_pot'); return string.format('%.4f|%.4f|%.4f|%.4f', \
                    \r.sip, r.hydration, r.caffeine, r.mood)"
                num t = read (T.unpack t) ∷ Double
                fields t = map num (T.splitOn "|" t)
            acolyteBefore ← readInstance env 1
            bearBefore ← readInstance env 2
            [sipA, hydA, cafA, moodA] ← fields <$> drink 1
            [sipB, hydB, cafB, moodB] ← fields <$> drink 2
            -- the fluid drew from both pots, the same sip
            sipA `shouldSatisfy` (> 0)
            near 1e-6 sipA sipB
            near 1e-6 hydA hydB
            -- the acolyte gained all three
            cafA `shouldSatisfy` (> 0)
            moodA `shouldSatisfy` (/= 0)
            luaNum ls "unit.getStatBase(1, 'hydration')" >>= near 1e-4 (21 + hydA)
            luaNum ls "unit.getStatBase(1, 'caffeine')" >>= \c → c `shouldSatisfy` (> 0)
            -- the bear gained hydration only; the credits were discarded
            luaNum ls "unit.getStatBase(2, 'hydration')" >>= near 1e-4 (21 + hydB)
            cafB `shouldBe` 0
            moodB `shouldBe` 0
            bearAfter ← readInstance env 2
            forM_ ["caffeine", "mood"] $ \k → HM.member k (uiStats bearAfter) `shouldBe` False
            acolyteBefore `shouldSatisfy` (\_ → True)
            bearBefore `shouldSatisfy` (\_ → True)

    it "supplies a Nominal mind's caffeine as zero to the stamina regen: the actual \
       \resource tick adds the bonus for a Simulated mind and none for a Nominal one" $
        withScene [ (unitSpec 1 "acolyte" (Just "rich"))
                      { usStats = [("stamina", 20), ("caffeine", 1.0)] }
                  , (unitSpec 2 "acolyte" (Just "rich"))
                      { usOverrides = [("mind", PhysioNominal)], usStats = [("stamina", 20)] } ] $
            \(_, ls) → do
                _ ← luaText ls
                    "_G.__tick = require('scripts.unit_resource_tick'); \
                    \_G.__cfg = require('scripts.unit_resource_config'); return true"
                let regen uid = luaNum ls $
                        "(function() local before = unit.readPhysio(" <> tshow (uid ∷ Int)
                        <> ", 'stamina'); __tick.tickResource(" <> tshow uid
                        <> ", 'acolyte', 'stamina', __cfg.acolyte.stamina, 'idle', 'standing', 1.0); \
                        \return unit.readPhysio(" <> tshow uid <> ", 'stamina') - before end)()"
                withBonus ← regen 1
                withoutBonus ← regen 2
                -- the configured bonus is caffeine x caffeine_regen_bonus per second
                (withBonus - withoutBonus) `shouldSatisfy` (> 0.01)
