{-# LANGUAGE Strict, OverloadedStrings #-}
-- | Persistence of the stamina modes and the SD-20 engagement (#2834;
--   design S8.5, S8.6 T-S8-4 (h) and (o), T-S8-6): pending, applied,
--   explicit and pinned state through the REAL session codec and the real
--   load gate, legacy content normalization, and malformed markers.
--
--   Nothing here adds a wire type: @stamina@ rides @uiStats@, its override
--   rides the units-v4 override map, and @combat_engaged@ is an unowned
--   durable @uiStats@ key. What the specs establish is that those values
--   round-trip EXACTLY, that the gate normalizes only what the content
--   changed, and that a second load of normalized output changes nothing.
--   The units are the shipped nomad and acolyte, spawned through the real
--   handler from the shipped definitions ("Tuning"'s encounter scene).
module Test.Headless.Unit.PhysiologyStaminaModes.Persistence (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Combat.Resolution (resolveAttack)
import Combat.Types (AttackMode(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState)
import Equipment.Types (emptyEquipmentClassManager)
import Infection.Types (emptyInfectionManager)
import Item.Types (emptyItemManager)
import qualified System.Random as Random
import Test.Headless.Unit.PhysiologyModes.Harness
import Test.Headless.Unit.PhysiologySpeciesProfiles
    (codecRoundTrip, gate, pageSnapshotOf, shippedDefs)
import Test.Headless.Unit.PhysiologyFixture (readShippedPhysiologyRegistry)
import Test.Headless.Unit.PhysiologyStaminaModes.Tuning
    (encounterPage, withEncounter)
import Test.Headless.World.Save.Components.Fixture (page1)
import Unit.Physiology.Types
import Unit.Types
import World.Save.Physiology (PhysiologyNormalization(..), physiologyLoadGate)
import World.Save.Snapshot (PageSnapshot(..))
import World.Save.Types (fromUnitSnapshot)

spec ∷ Spec
spec = describe "persistence of the stamina modes and the engagement" $ do
    maximumSpec
    pendingAppliedSpec
    legacySpec
    malformedSpec

-----------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------

-- | One admitted attempt by @atk@ on @tgt@ through the REAL resolution (a
--   fixed stream), whatever it does to the target.
swing ∷ EngineEnv → Int → Int → AttackMode → IO ()
swing env atk tgt mode = do
    modifyInstance env atk $ \i → i { uiStats = HM.insert "stance" 1 (uiStats i) }
    _ ← resolveAttack env (Random.mkStdGen 7) (fromIntegral atk)
            (fromIntegral tgt) mode 0 0
    _ ← drainCommands env
    pure ()

-- | One physiology pass of the shipped Lua.
pass ∷ LuaBackendState → IO ()
pass ls = runSeconds ls 0.1 0.1

stat ∷ EngineEnv → Int → Text → IO (Maybe Float)
stat env uid k = HM.lookup k ∘ uiStats <$> readInstance env uid

-- | Save the whole scene through the production codec and the load gate,
--   restore it the way a load does, and put the units back on the live page
--   so the shipped ticks see them. Answers the gate's notes.
saveAndLoad ∷ EngineEnv → LuaBackendState → IO [PhysiologyNormalization]
saveAndLoad env ls = do
    reg ← readShippedPhysiologyRegistry
    defs ← shippedDefs env
    um0 ← readIORef (unitManagerRef env)
    -- the starting kits carry item instance ids the codec fixture's allocator
    -- does not account for, and items are not what these specs are about
    forM_ (HM.keys (umInstances um0)) $ \(UnitId u) →
        modifyInstance env (fromIntegral u) $ \i → i
            { uiPage = page1, uiInventory = [], uiEquipment = HM.empty
            , uiAccessories = [] }
    pg ← pageSnapshotOf env ≫= codecRoundTrip
    (us, notes) ← gate reg defs (pgsUnits pg)
    -- a second pass over its own output changes nothing and logs nothing
    (us2, notes2) ← gate reg defs us
    notes2 `shouldBe` []
    us2 `shouldBe` us
    let (restored, orphans, _, _, _) = fromUnitSnapshot page1 defs
            emptyInfectionManager emptyEquipmentClassManager emptyItemManager us2
    orphans `shouldBe` []
    writeIORef (unitManagerRef env) restored
    forM_ (HM.keys (umInstances restored)) $ \(UnitId u) →
        modifyInstance env (fromIntegral u) $ \i → i { uiPage = encounterPage }
    _ ← luaText ls "__resources.onSaveLoaded({ 1, 2 }, {}); return true"
    pure notes

-- | The marker, the pinned override and the pool, for assertions.
engagement ∷ EngineEnv → Int → IO (Maybe Float, Maybe PhysioMode, Maybe Float)
engagement env uid = do
    i ← readInstance env uid
    pure ( HM.lookup "combat_engaged" (uiStats i)
         , HM.lookup "stamina" (uiPhysioOverrides i)
         , HM.lookup "stamina" (uiStats i) )

-- | The stamina-related notes of a normalization.
staminaNotes ∷ [PhysiologyNormalization] → [Text]
staminaNotes ns = [ n | PhysiologyNormalization _ _ _ notes ← ns, n ← notes
                      , "stamina" `T.isInfixOf` n ]

-- | Install the engagement outcome recorder.
recordEngagement ∷ LuaBackendState → IO ()
recordEngagement ls = void $ luaText ls $ T.unlines
    [ "_G.__eng = {}"
    , "local real = unit.applyCombatEngagement"
    , "unit.applyCombatEngagement = function(uid)"
    , "  local o, c = real(uid)"
    , "  __eng[#__eng + 1] = tostring(uid) .. ':' .. tostring(o)"
    , "  return o, c"
    , "end"
    , "return true" ]

outcomes ∷ LuaBackendState → IO Text
outcomes ls = luaText ls "return table.concat(__eng, ',')"

-----------------------------------------------------------------------
-- T-S8-6: the maximum across a save and load
-----------------------------------------------------------------------

-- | The three reads of the maximum that must agree: @stats.get@, the verb
--   (which is @maxStaminaFor@'s rule) and the commit bound.
maxima ∷ LuaBackendState → Int → IO (Double, Double, Double)
maxima ls uid = do
    viaStats ← luaNum ls ("require('scripts.unit_stats').get(" <> tshow uid <> ", 'max_stamina')")
    viaVerb ← luaNum ls ("unit.getMaxStamina(" <> tshow uid <> ")")
    -- the commit bound: a Simulated commit reports it; a Nominal receiver
    -- is refused, so read it through the registry's healthy value instead
    viaHealthy ← luaNum ls ("unit.readPhysio(" <> tshow uid <> ", 'stamina')")
    pure (viaStats, viaVerb, viaHealthy)

maximumSpec ∷ Spec
maximumSpec = describe "T-S8-6: an endurance-1 nomad's maximum is 15 everywhere" $
    it "before engagement, after engagement and after a save and load" $
        withEncounter $ \(env, ls) → do
            -- the shipped definition's innate +50%, copied to the unit at spawn
            mods ← uiModifiers <$> readInstance env 1
            HM.member "max_stamina" mods `shouldBe` True
            stat env 1 "endurance" `shouldReturn` Just 1
            -- before: Nominal reads the effective maximum as its healthy value
            maxima ls 1 `shouldReturn` (15, 15, 15)
            -- after: engaged, applied, Simulated
            swing env 1 2 Quick
            pass ls
            engagement env 1 ≫= \(m, o, _) → (m, o) `shouldBe` (Just 2, Just PhysioSimulated)
            (a, b, _) ← maxima ls 1
            (a, b) `shouldBe` (15, 15)
            -- and the commit bound: a zero commit reports the same maximum
            luaNum ls "unit.commitStamina(1, 0).maximum" `shouldReturn` 15
            -- after a save and a load
            _ ← saveAndLoad env ls
            (a', b', _) ← maxima ls 1
            (a', b') `shouldBe` (15, 15)
            luaNum ls "unit.commitStamina(1, 0).maximum" `shouldReturn` 15

-----------------------------------------------------------------------
-- T-S8-4 (h), (o) and the applied/pinned state
-----------------------------------------------------------------------

pendingAppliedSpec ∷ Spec
pendingAppliedSpec = describe "pending, applied and pinned state" $ do
    it "(h) a pending save reloads pending and Nominal with no stamina, \
       \normalization changes no stamina, and its first eligible pass transitions it" $
        withEncounter $ \(env, ls) → do
            recordEngagement ls
            swing env 1 2 Quick
            engagement env 1 `shouldReturn` (Just 1, Nothing, Nothing)
            notes ← saveAndLoad env ls
            staminaNotes notes `shouldBe` []
            engagement env 1 `shouldReturn` (Just 1, Nothing, Nothing)
            luaText ls "return tostring(unit.getPhysioMode(1, 'stamina'))"
                `shouldReturn` "nominal"
            pass ls
            outcomes ls `shouldReturn` "1:transitioned"
            engagement env 1 `shouldReturn` (Just 2, Just PhysioSimulated, Just 15)

    it "(o) an explicit Simulated override with a partial pool, pending, is \
       \preserved exactly through the load, and its first pass latches the \
       \marker without reseeding" $
        withEncounter $ \(env, ls) → do
            recordEngagement ls
            -- an explicit Simulated pin at 4.75 of 15: the triggering Quick
            -- strike is CHARGED (5% of 15), leaving exactly 4.0
            luaText ls "return tostring(__physiology.setMode(1, 'stamina', 'simulated', { stamina = 4.75 }))"
                `shouldReturn` "true"
            swing env 1 2 Quick
            (m, o, p) ← engagement env 1
            (m, o) `shouldBe` (Just 1, Just PhysioSimulated)
            maybe 0 realToFrac p `shouldSatisfy` (\v → abs (v - 4.0) < (1e-4 ∷ Double))
            notes ← saveAndLoad env ls
            staminaNotes notes `shouldBe` []
            (m', o', p') ← engagement env 1
            (m', o') `shouldBe` (Just 1, Just PhysioSimulated)
            p' `shouldBe` p
            pass ls
            outcomes ls `shouldReturn` "1:latched"
            (m'', o'', p'') ← engagement env 1
            (m'', o'') `shouldBe` (Just 2, Just PhysioSimulated)
            -- a pass also ticks the pool (idle regen), but never reseeds it
            maybe 0 realToFrac p'' `shouldSatisfy` (\v → v > (3.9 ∷ Double) ∧ v < 5)

    it "an applied marker and its pinned override persist exactly, a canonical \
       \save normalizes zero times, and a re-save and reload is stable" $
        withEncounter $ \(env, ls) → do
            swing env 1 2 Quick
            pass ls
            before ← engagement env 1
            notes ← saveAndLoad env ls
            notes `shouldBe` []
            engagement env 1 `shouldReturn` before
            luaText ls "return tostring(unit.getPhysioMode(1, 'stamina'))"
                `shouldReturn` "simulated"
            -- re-save and reload: nothing moved
            notes2 ← saveAndLoad env ls
            notes2 `shouldBe` []
            engagement env 1 `shouldReturn` before

    it "a pending unit with an explicit Nominal override saves and loads its \
       \override and marker exactly, with no stamina" $
        withEncounter $ \(env, ls) → do
            luaText ls "return tostring(__physiology.setMode(1, 'stamina', 'nominal'))"
                `shouldReturn` "true"
            swing env 1 2 Quick
            notes ← saveAndLoad env ls
            staminaNotes notes `shouldBe` []
            engagement env 1 `shouldReturn` (Just 1, Just PhysioNominal, Nothing)

-----------------------------------------------------------------------
-- Legacy content
-----------------------------------------------------------------------

legacySpec ∷ Spec
legacySpec = describe "legacy content is normalized once, observably and idempotently" $ do
    it "a legacy nomad with a stray stamina = 0 and no marker resolves Nominal; \
       \the load scrubs it, and its first engagement afterwards seeds a full pool" $
        withEncounter $ \(env, ls) → do
            recordEngagement ls
            -- the S8-F1 leftover: a swing at an absent pool wrote an absolute 0
            modifyInstance env 1 $ \i →
                i { uiStats = HM.insert "stamina" 0 (uiStats i) }
            notes ← saveAndLoad env ls
            length (staminaNotes notes) `shouldBe` 1
            engagement env 1 `shouldReturn` (Nothing, Nothing, Nothing)
            -- the second load of the normalized output finds nothing (saveAndLoad
            -- asserts that), and the unit still behaves
            swing env 1 2 Quick
            pass ls
            outcomes ls `shouldReturn` "1:transitioned"
            engagement env 1 `shouldReturn` (Just 2, Just PhysioSimulated, Just 15)

    it "a nomad saved before this change loads WITHOUT the innate modifier \
       \(modifiers are not migrated): its maximum is 10, and the engagement seed is 10" $
        withEncounter $ \(env, ls) → do
            modifyInstance env 1 $ \i →
                i { uiModifiers = HM.delete "max_stamina" (uiModifiers i) }
            _ ← saveAndLoad env ls
            luaNum ls "unit.getMaxStamina(1)" `shouldReturn` 10
            swing env 1 2 Quick
            pass ls
            engagement env 1 `shouldReturn` (Just 2, Just PhysioSimulated, Just 10)

    it "scrubs a technomule's stray stamina too, and leaves a rich acolyte's pool alone" $
        withEncounter $ \(env, ls) → do
            -- the scene's acolyte (unit 2) keeps its Simulated pool; add a mule
            modifyInstance env 2 $ \i →
                i { uiStats = HM.insert "stamina" 6 (uiStats i) }
            modifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup (UnitId 1) (umInstances um) of
                    Nothing → um
                    Just nomadInst → um { umInstances =
                        HM.insert (UnitId 3)
                            nomadInst { uiDefName = "technomule"
                                      , uiPhysioOverrides = HM.empty
                                      , uiStats = HM.insert "stamina" 0 (uiStats nomadInst)
                                      , uiModifiers = HM.empty }
                            (umInstances um) }
            notes ← saveAndLoad env ls
            -- exactly the mule's stray key is scrubbed
            length (staminaNotes notes) `shouldBe` 1
            stat env 3 "stamina" `shouldReturn` Nothing
            stat env 2 "stamina" `shouldReturn` Just 6

-----------------------------------------------------------------------
-- Malformed markers
-----------------------------------------------------------------------

malformedSpec ∷ Spec
malformedSpec = describe "malformed engagement markers reject the load" $ do
    it "a non-finite or out-of-set combat_engaged is rejected before \
       \publication, naming the marker; 1 and 2 are accepted" $
        withEncounter $ \(env, _) → do
            reg ← readShippedPhysiologyRegistry
            defs ← shippedDefs env
            let gateWith v = do
                    modifyInstance env 1 $ \i →
                        i { uiStats = HM.insert "combat_engaged" v (uiStats i)
                          , uiPage = page1 }
                    pg ← pageSnapshotOf env
                    pure (physiologyLoadGate reg (HM.map udPhysiology defs) 0
                              [(page1, pgsUnits pg)])
            forM_ [0, 3, -1, 0.5, 2.5, 1 / 0, 0 / 0] $ \bad → do
                r ← gateWith bad
                case r of
                    Left msgs → (bad, any ("combat_engaged" `T.isInfixOf`) msgs)
                                    `shouldSatisfy` (\(_, named) → named)
                    Right _ → expectationFailure
                        ("accepted a malformed marker: " <> show bad)
            forM_ [1, 2] $ \ok' → do
                r ← gateWith ok'
                either (\msgs → expectationFailure (show msgs)) (const (pure ())) r

    it "a valid marker on a species that does not declare the trigger is \
       \retained and changes nothing" $
        withEncounter $ \(env, ls) → do
            modifyInstance env 2 $ \i →
                i { uiStats = HM.insert "combat_engaged" 2 (uiStats i) }
            let keep = HM.filterWithKey (\k _ → k `elem` ["combat_engaged", "stamina"])
            before ← keep ∘ uiStats <$> readInstance env 2
            notes ← saveAndLoad env ls
            staminaNotes notes `shouldBe` []
            -- (the load also rebuilds the circulation and brain caches, which
            -- are not what is asserted here)
            keep ∘ uiStats <$> readInstance env 2 `shouldReturn` before
            luaText ls "return tostring(unit.getPhysioMode(2, 'stamina'))"
                `shouldReturn` "simulated"
