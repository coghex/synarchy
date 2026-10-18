{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The brain's and the mental machine's writes are atomic (#2838, S11a):
--   T-S11-1 (no torn read is observable) and T-S11-4 (the batch verb refuses
--   whole). Everything runs through the SHIPPED Lua over the REAL registered
--   @unit.*@ API and a real unit-manager ref.
--
--   __T-S11-1__ is established three ways. (1) No writer touches a mind key
--   with a raw @unit.setStat@ (the verb that exposes intermediate states),
--   which the recorder counts. (2) Each event publishes its COMPLETE key set
--   in ONE @unit.commitPhysio@ update. (3) A sampler thread reads the unit
--   manager continuously while the Lua worker advances the whole machine, as
--   the combat thread does, and every state it observes is one the machine
--   was in after a complete batch.
module Test.Headless.Unit.PhysiologyBrainMental.Atomicity (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (forkIO, yield)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Text.Printf (printf)
import Combat.Resolution.Common (mentalEffectiveness)
import Combat.Resolution.Strike (defenderDodgeChance, hitChance)
import Craft.Execute (applyMentalQuality)
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState)
import Test.Headless.Unit.PhysiologyFixture (readShippedPhysiologyRegistry)
import Test.Headless.Unit.PhysiologyModes.Harness
import Unit.Physiology.Types
import Unit.Types

spec ∷ Spec
spec = describe "atomic writes (T-S11-1, T-S11-4)" $ do
    eventSpec
    tornReadSpec
    refusalSpec

-- | The keys of the mental machine that make up one observable state.
machineKeys ∷ [Text]
machineKeys =
    [ "mental_state", "mental_since", "mental_until", "mental_next_roll_at"
    , "mental_cooldown_until", "mental_high_since", "mental_break_behavior" ]

-- | A unit whose brain and mind are the system under test: every survival
--   system Nominal (the exempt tier), a stamina fraction well under the cap
--   so the euphoria lift is visible in the concentration.
brainUnit ∷ UnitSpec
brainUnit = (unitSpec 1 "acolyte" (Just "exempt"))
    { usOverrides = [("stamina", PhysioSimulated)], usStats = [("stamina", 20)] }

-- | Recorders over the engine verbs the writers use: every commit's key set
--   (sorted), every raw @setStat@ of a registry mind key, every thought
--   record, the boundary tuple after each commit, and the raw
--   effectiveness sample at that boundary.
recorders ∷ Text
recorders = T.unlines
    [ "_G.__commits, _G.__rawMind, _G.__thoughts, _G.__tuples, _G.__effs, _G.__stops = {}, {}, 0, {}, {}, 0"
    , "_G.__machine = { 'mental_state', 'mental_since', 'mental_until', 'mental_next_roll_at',"
    , "  'mental_cooldown_until', 'mental_high_since', 'mental_break_behavior' }"
    , "local function tuple()"
    , "  local t = {}"
    , "  for _, k in ipairs(__machine) do t[#t + 1] = string.format('%.3f', unit.readPhysio(1, k)) end"
    , "  return table.concat(t, ',')"
    , "end"
    , "_G.__tupleNow = tuple"
    , "local commit = unit.commitPhysio"
    , "unit.commitPhysio = function(uid, batch)"
    , "  local keys = {}"
    , "  for k in pairs(batch) do keys[#keys + 1] = k end"
    , "  table.sort(keys)"
    , "  local ok, why = commit(uid, batch)"
    , "  __commits[#__commits + 1] = table.concat(keys, ',') .. (ok and '' or ' REFUSED')"
    , "  if ok then"
    , "    __tuples[#__tuples + 1] = tuple()"
    , "    __effs[#__effs + 1] = string.format('%.4f', unit.getMentalEffectiveness(1))"
    , "  end"
    , "  return ok, why"
    , "end"
    , "local setStat = unit.setStat"
    , "local mind = { mood=1, emotional_pain=1, caffeine=1, mental_state=1, mental_since=1,"
    , "  mental_until=1, mental_next_roll_at=1, mental_cooldown_until=1,"
    , "  mental_high_since=1, mental_break_behavior=1, thought_next_at=1 }"
    , "unit.setStat = function(uid, k, v)"
    , "  if mind[k] then __rawMind[#__rawMind + 1] = k end"
    , "  return setStat(uid, k, v)"
    , "end"
    , "local emit = thought.emit"
    , "thought.emit = function(...) __thoughts = __thoughts + 1; return emit(...) end"
    , "local stop = unit.stop"
    , "unit.stop = function(...) __stops = __stops + 1; return stop(...) end"
    , "return true" ]

commitsOf ∷ LuaBackendState → IO [Text]
commitsOf ls = T.splitOn ";" <$> luaText ls "return table.concat(__commits, ';')"

-----------------------------------------------------------------------
-- Each event is one complete batch
-----------------------------------------------------------------------

eventSpec ∷ Spec
eventSpec = describe "every writer event commits its whole key set in one update" $ do
    it "brain, a break entry and end, euphoria, the stressed label and a thought \
       \each publish one complete batch, and no mind key is ever written raw" $
        withScene [brainUnit] $ \(env, ls) → do
            _ ← luaText ls recorders
            _ ← luaText ls "_G.__mental = require('scripts.mental_state'); \
                           \_G.__brain = require('scripts.brain'); \
                           \_G.__thoughts_mod = require('scripts.thoughts'); \
                           \return true"
            -- brain.tick: mood, emotional pain and caffeine together
            _ ← luaText ls "__brain.tick(1, 0.1); return true"
            commitsOf ls `shouldReturn` ["caffeine,emotional_pain,mood"]
            -- a break entry: behavior, deadline, state, since and next roll
            _ ← luaText ls "__commits = {}; __mental.forceBreak(1, 'wander'); return true"
            commitsOf ls `shouldReturn`
                ["mental_break_behavior,mental_next_roll_at,mental_since,mental_state,mental_until"]
            luaText ls "return __thoughts .. '|' .. __stops" `shouldReturn` "1|1"
            -- the episode ends: cooldown, state, since and next roll together
            writeGameTime env 500
            _ ← luaText ls "__commits = {}; __mental.tick(1, 0.1); return true"
            commitsOf ls `shouldReturn`
                ["mental_cooldown_until,mental_next_roll_at,mental_since,mental_state"]
            -- a thought: the deadline, and the mood nudge when it has one
            _ ← luaText ls "__commits = {}; __thoughts_mod.loadCatalogue(); return true"
            writeGameTime env 1000
            _ ← luaText ls "__thoughts_mod.tick(1, unit.getInfo(1), 0.1); return true"
            c ← commitsOf ls
            c `shouldSatisfy` (\xs → xs `elem` [["thought_next_at"], ["mood,thought_next_at"]])
            -- nothing above ever used the raw verb on a mind key
            luaText ls "return #__rawMind" `shouldReturn` "0"
            ignore env
  where ignore _ = pure ()

-----------------------------------------------------------------------
-- No torn read
-----------------------------------------------------------------------

tornReadSpec ∷ Spec
tornReadSpec = describe "no torn read (T-S11-1)" $
    it "every state a concurrent reader observes while the machine runs a break \
       \and an euphoric episode is one the machine was in after a complete batch, \
       \and every combat, getter and craft sample is an allowed serial state" $ do
        reg ← readShippedPhysiologyRegistry
        withScene [brainUnit] $ \(env, ls) → do
            _ ← luaText ls recorders
            _ ← luaText ls "_G.__mental = require('scripts.mental_state'); \
                           \_G.__brain = require('scripts.brain'); \
                           \_G.__mental.TUNE.BREAK_CHANCE_MAX = 1.0; \
                           \_G.__mental.TUNE.EUPHORIA_CHANCE = 1.0; \
                           \return true"
            start ← uiStats <$> readInstance env 1
            let tupleOf i = T.intercalate ","
                    [ T.pack (printf "%.3f" (realToFrac (HM.lookupDefault (-1) k (uiStats i)) ∷ Double))
                    | k ← machineKeys ]
                effOf i = do
                    um ← readIORef (unitManagerRef env)
                    now ← readIORef (gameTimeRef env)
                    pure $ case HM.lookup (uiDefName i) (umDefs um) of
                        Just def → Just (mentalEffectiveness reg def now i)
                        Nothing → Nothing
            initial ← tupleOf <$> readInstance env 1
            seen ← newIORef (Set.singleton initial)
            seenEffs ← newIORef (Set.empty ∷ Set.Set Text)
            stop ← newIORef False
            done ← newEmptyMVar
            _ ← forkIO $ do
                let loop = do
                        halt ← readIORef stop
                        unless halt $ do
                            i ← readInstance env 1
                            modIORef seen (Set.insert (tupleOf i))
                            effOf i >>= \case
                                Just e → modIORef seenEffs
                                    (Set.insert (T.pack (printf "%.4f" (realToFrac e ∷ Double))))
                                Nothing → pure ()
                            yield
                            loop
                loop
                putMVar done ()
            -- the scenario: an euphoric episode, its end, a stressed break, its end
            let step t src = writeGameTime env t ≫ void (luaText ls src)
            -- lift the machine into the euphoria band: som = 1, SUSTAIN passes
            step 10 "__mental.tick(1, 0.1); return true"
            step 40 "__mental.tick(1, 0.1); return true"
            step 41 "__mental.tick(1, 0.1); return true"          -- enters euphoria
            step 42 "__mental.tick(1, 0.1); return true"
            step 400 "__mental.tick(1, 0.1); return true"         -- the episode ends
            _ ← luaText ls "unit.commitPhysio(1, { mood = 0, emotional_pain = 1 }); return true"
            step 401 "__mental.tick(1, 0.1); return true"         -- stressed
            step 440 "__mental.tick(1, 0.1); return true"
            step 449 "__mental.tick(1, 0.1); return true"         -- the break
            step 900 "__mental.tick(1, 0.1); return true"         -- ends
            writeIORef stop True
            takeMVar done
            observed ← readIORef seen
            boundary ← T.splitOn ";" <$> luaText ls "return table.concat(__tuples, ';')"
            let allowed = Set.insert initial (Set.fromList boundary)
            -- (1) the machine key tuples the reader saw are all post-batch states
            Set.toList (observed `Set.difference` allowed) `shouldBe` []
            -- the scenario really did cross a break and an euphoric episode
            Set.size allowed `shouldSatisfy` (≥ 5)
            -- (2) the effectiveness samples: the sampler's raw Haskell values and
            --     the Lua getter's at every boundary are among the allowed serial
            --     values, so no intermediate bump exists
            effsLua ← T.splitOn ";" <$> luaText ls "return table.concat(__effs, ';')"
            seenE ← readIORef seenEffs
            let allowedE = Set.fromList effsLua
            Set.toList (seenE `Set.difference` allowedE `Set.difference` Set.singleton "")
                `shouldSatisfy` all (`elem` Set.toList allowedE)
            -- (3) the consumers of one allowed sample: hit chance, the active
            --     dodge and craft quality all stay inside their bands
            tgt ← readInstance env 1
            forM_ (Set.toList allowedE) $ \eT → do
                let e = read (T.unpack eT) ∷ Float
                e `shouldSatisfy` (\v → v ≥ 0.75 ∧ v ≤ 1.10)
                hitChance e 0.5 0.2 `shouldSatisfy` (\v → v ≥ 0.05 ∧ v ≤ 0.95)
                defenderDodgeChance e tgt False 0 `shouldSatisfy` (\v → v ≥ 0 ∧ v ≤ 0.95)
                applyMentalQuality e 50 `shouldSatisfy` (\v → v ≥ 40 ∧ v ≤ 60)
            luaText ls "return #__rawMind" `shouldReturn` "0"
            ignore start
  where
    ignore _ = pure ()
    modIORef ref f = atomicModifyIORef' ref (\x → (f x, ()))

-----------------------------------------------------------------------
-- T-S11-4: refusal
-----------------------------------------------------------------------

refusalSpec ∷ Spec
refusalSpec = describe "unit.commitPhysio refuses a batch whole (T-S11-4)" $ do
    it "stores a valid batch in one update and answers true" $
        withScene [brainUnit] $ \(env, ls) → do
            luaText ls "return tostring(unit.commitPhysio(1, { mood = 0.3, caffeine = 0.2 }))"
                `shouldReturn` "true"
            i ← readInstance env 1
            HM.lookup "mood" (uiStats i) `shouldBe` Just 0.3
            HM.lookup "caffeine" (uiStats i) `shouldBe` Just 0.2

    it "refuses an otherwise-valid mind value beside a derived key, a Nominal-owned key \
       \or a field-backed key, leaving every key and override unchanged" $
        withScene [ brainUnit
                  , (unitSpec 2 "bear_brown" (Just "reduced"))
                      { usOverrides = [("mind", PhysioNominal)] } ] $ \(env, ls) → do
            before1 ← readInstance env 1
            before2 ← readInstance env 2
            forM_ [ (1 ∷ Int, "{ mood = 0.3, concentration = 0.5 }", "forbidden_key:concentration")
                  , (1, "{ mood = 0.3, state_of_mind = 0.5 }", "forbidden_key:state_of_mind")
                  , (1, "{ mood = 0.3, core_temp = 40 }", "forbidden_key:core_temp")
                  , (1, "{ mood = 0.3, blood = 1.0 }", "forbidden_key:blood")
                  , (2, "{ caffeine = 0.5 }", "forbidden_key:caffeine") ] $ \(uid, batch, why) → do
                out ← luaText ls ("local ok, why = unit.commitPhysio(" <> tshow uid <> ", "
                                  <> batch <> "); return tostring(ok) .. '|' .. tostring(why)")
                out `shouldBe` "nil|" <> why
            after1 ← readInstance env 1
            after2 ← readInstance env 2
            uiStats after1 `shouldBe` uiStats before1
            uiPhysioOverrides after1 `shouldBe` uiPhysioOverrides before1
            uiStats after2 `shouldBe` uiStats before2
            uiPhysioOverrides after2 `shouldBe` uiPhysioOverrides before2

    it "refuses every malformed call with its own reason and mutates nothing" $
        withScene [brainUnit] $ \(env, ls) → do
            before ← readInstance env 1
            forM_ [ ("'1', { mood = 0.3 }", "invalid_unit_id")
                  , ("-1, { mood = 0.3 }", "invalid_unit_id")
                  , ("1e12, { mood = 0.3 }", "invalid_unit_id")
                  , ("1, nil", "invalid_batch")
                  , ("1, 'mood'", "invalid_batch")
                  , ("1, {}", "invalid_batch")
                  , ("1, { [1] = 0.3 }", "invalid_key")
                  , ("1, { mood = '0.3' }", "invalid_value")
                  , ("1, { mood = 0/0 }", "invalid_value")
                  , ("1, { mood = 1/0 }", "invalid_value")
                  , ("1, { mood = 1e40 }", "invalid_value")
                  , ("1, { mood = 0.3, caffeine = -1/0 }", "invalid_value")
                  , ("99, { mood = 0.3 }", "no_such_unit") ] $ \(args, why) → do
                out ← luaText ls ("local ok, why = unit.commitPhysio(" <> args
                                  <> "); return tostring(ok) .. '|' .. tostring(why)")
                (args, out) `shouldBe` (args, "nil|" <> why)
            after ← readInstance env 1
            uiStats after `shouldBe` uiStats before

    it "makes a writer event whose batch is refused publish nothing: no thought \
       \record, no stop, no event, and the state is left alone" $
        withScene [brainUnit] $ \(env, ls) → do
            _ ← luaText ls recorders
            _ ← luaText ls "_G.__mental = require('scripts.mental_state'); \
                           \local commit = unit.commitPhysio; \
                           \unit.commitPhysio = function() return nil, 'refused_for_test' end; \
                           \_G.__restore = function() unit.commitPhysio = commit end; \
                           \return true"
            before ← uiStats <$> readInstance env 1
            _ ← luaText ls "__mental.forceBreak(1, 'wander'); return true"
            luaText ls "return __thoughts .. '|' .. __stops" `shouldReturn` "0|0"
            survivalEvents ls `shouldReturn` []
            luaText ls "return table.concat(__events, '|')" `shouldReturn` ""
            after ← uiStats <$> readInstance env 1
            after `shouldBe` before
            scriptErrors ls `shouldReturn` ""
            ignore env
  where ignore _ = pure ()
