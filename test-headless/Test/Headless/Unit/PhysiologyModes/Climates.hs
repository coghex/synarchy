{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The climate-driven contract cases of the "physiology stat modes" group
--   (#2831; design T-1, T-2, T-4, T-5, T-12, T-13): the small creature's
--   salt in the four D-4 combinations, the acolyte's reference heat, the
--   salt↔thermo feedback, a per-unit override running on the registry's
--   default parameters, and the virtual meal (its trigger and its sustained
--   re-arming).
--
--   Everything is inputs pinned in the spec: no roll, no entropy, a fixed
--   climate, the shipped Lua over the real @unit.*@ API. Where the design
--   gives a derivation (T-13's drain budget) the assertions are the
--   design's, with the bands unchanged; a case that cannot meet them is
--   reported, never retuned.
module Test.Headless.Unit.PhysiologyModes.Climates (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Engine.Scripting.Lua.Types (LuaBackendState)
import Test.Headless.Unit.PhysiologyModes.Harness
import Unit.Types

-- | A 0.3 kg squirrel as T-1, T-12 and T-13 pin it: constitution 0.5, fat
--   fraction 0.10, the given metabolism, thermo and salt Simulated, hydration
--   and nutrition Nominal (the @reduced@ profile), in the given activity.
--   The profile is stated here, not inherited from @data\/units\/red_squirrel.yaml@
--   (which S7, #2833, flips to @simple@: salt Nominal), so these salt-Simulated
--   cases are the independent small-mass Simulated coverage and keep their
--   inputs, combinations and bounds whatever the shipped default is.
squirrelWith ∷ Float → Text → [(Text, PhysioMode)] → UnitSpec
squirrelWith metabolism activity modes = (unitSpec 1 "red_squirrel" (Just "reduced"))
    { usBody = squirrelBody, usActivity = activity, usModes = modes
    , usStats = [ ("constitution", 0.5), ("metabolism", metabolism)
                , ("fat_mass", 0.03), ("lean_mass", 0.24) ] }

sim, nom ∷ PhysioMode
sim = PhysioSimulated
nom = PhysioNominal

spec ∷ Spec
spec = describe "climate-driven cases" $ do
    t1
    t2
    t4
    t5
    t12
    t13

-----------------------------------------------------------------------
-- T-1: the small creature, four combinations
-----------------------------------------------------------------------

-- | The probe's own bounds, applied over the run.
inBounds ∷ Extremes → Extremes → Expectation
inBounds conc core = do
    exMin conc `shouldSatisfy` (>= 0.7)
    exMax conc `shouldSatisfy` (<= 1.3)
    exMin core `shouldSatisfy` (>= 33.5)
    exMax core `shouldSatisfy` (<= 39)
    (exNaN conc, exNaN core) `shouldBe` (0, 0)

t1 ∷ Spec
t1 = describe "T-1 small-creature salt (0.3 kg, constitution 0.5, fat 0.10, 22 C, 120 s)" $
    forM_ [ ("idle, metabolism 1.2", 1.2, "idle")
          , ("idle, metabolism 1.3", 1.3, "idle")
          , ("forced moving, metabolism 1.2", 1.2, "walking") ] $ \(label, metab, activity) →
      forM_ [ (saltMode, hydMode) | saltMode ← [sim, nom], hydMode ← [sim, nom] ] $
        \(saltMode, hydMode) →
          it (label <> " / salt " <> show saltMode <> " / hydration " <> show hydMode) $
            withScene [squirrelWith metab activity
                         [("salt", saltMode), ("hydration", hydMode)]] $ \(env, ls) → do
                climate ls 22 0.5
                ex ← track ls 1 120 0.1 ["salt_conc", "core_temp"]
                inBounds (ex HM.! "salt_conc") (ex HM.! "core_temp")
                killed env `shouldReturn` []
                scriptErrors ls `shouldReturn` ""
                -- a Nominal system reads its healthy values
                when (saltMode ≡ nom) $
                    readOwned ls 1 "salt_conc" `shouldReturn` "1|nominal"
                when (hydMode ≡ nom) $
                    readOwned ls 1 "hydration" `shouldReturn` "0.18|nominal"

-----------------------------------------------------------------------
-- T-2: the acolyte in #2761's reference heat
-----------------------------------------------------------------------

t2 ∷ Spec
t2 = describe "T-2 acolyte reference heat (39.6 C, humidity 0.26)" $
    it "keeps salt and thermo Simulated, supplied with water, inside the healthy band" $
        withScene [unitSpec 1 "acolyte" (Just "rich")] $ \(env, ls) → do
            climate ls 39.6 0.26
            -- water supplied: the pool is topped up every tick, as the
            -- salt reserve group does
            ex ← trackWith ls 1 1800 0.1 ["salt_conc", "core_temp", "cons"]
                "function() unit.setStat(1, 'hydration', unit.getStat(1, 'max_hydration')) end"
            exMin (ex HM.! "salt_conc") `shouldSatisfy` (>= 0.75)
            exMax (ex HM.! "salt_conc") `shouldSatisfy` (<= 1.35)
            exMax (ex HM.! "core_temp") `shouldSatisfy` (<= 39)
            exMin (ex HM.! "cons") `shouldSatisfy` (>= 0.999)
            killed env `shouldReturn` []

-----------------------------------------------------------------------
-- T-4: feedback
-----------------------------------------------------------------------

t4 ∷ Spec
t4 = describe "T-4 salt and thermo feedback (desert heat)" $
    it "lets a pre-depleted salt pool slow sweating and warm the core faster, within the clamps" $ do
        let run conc = withScene
                [ (unitSpec 1 "acolyte" (Just "rich"))
                    -- nutrition stays Simulated: a Nominal one would answer the
                    -- depleted pool with virtual meals (SD-4) and mask the loop
                    { usModes = [("hydration", nom)]
                    , usStats = [ ("salt", conc * 70), ("salt_store", 0) ] } ] $ \(_, ls) → do
                    climate ls 45 0.15
                    factor0 ← luaNum ls "require('scripts.salts').sweatFactor(1)"
                    _ ← luaText ls "_G.__prev, _G.__maxstep = nil, 0; return true"
                    ex ← trackWith ls 1 300 0.1 ["core_temp", "salt_conc"]
                        "function() local c = unit.readPhysio(1, 'core_temp'); \
                        \if __prev then __maxstep = math.max(__maxstep, math.abs(c - __prev)) end; \
                        \__prev = c end"
                    step ← luaNum ls "__maxstep"
                    pure (factor0, exMax (ex HM.! "core_temp"), exNaN (ex HM.! "core_temp"), step)
        (fHealthy, coreHealthy, nanH, stepH) ← run 1.0
        (fDepleted, coreDepleted, nanD, stepD) ← run 0.7
        fHealthy `shouldSatisfy` (> 0.999)
        fDepleted `shouldSatisfy` (< 0.95)
        (coreHealthy, coreDepleted) `shouldSatisfy` (\(h, d) → d > h)
        -- no NaN, inside the thermo clamps, and no per-tick jump near the
        -- integrator's own limit (no oscillation beyond the clamps)
        (nanH, nanD) `shouldBe` (0, 0)
        coreDepleted `shouldSatisfy` (\c → c >= 15 ∧ c <= 45)
        max stepH stepD `shouldSatisfy` (< 0.1)

-----------------------------------------------------------------------
-- T-5: a per-unit override on the default parameters
-----------------------------------------------------------------------

t5 ∷ Spec
t5 = describe "T-5 per-unit override on the registry's default parameters" $ do

    it "dehydrates over game-days, not seconds, and dies by death_threshold" $
        withScene [squirrelWith 1.0 "idle" []] $ \(env, ls) → do
            climate ls 22 0.5
            luaText ls "return tostring(__physiology.setMode(1, 'hydration', 'simulated'))"
                `shouldReturn` "true"
            -- 10 game-minutes: still nearly full (the squirrel has no
            -- hydration entry of its own, so this is the registry default)
            runSeconds ls 600 1.0
            frac ← luaNum ls "unit.readPhysio(1, 'hydration') / unit.readPhysio(1, 'max_hydration')"
            frac `shouldSatisfy` (> 0.8)
            killed env `shouldReturn` []
            -- three game days (1 day = 1440 s): it has run dry and died
            runSeconds ls 3500 1.0
            dead ← killed env
            dead `shouldBe` [UnitId 1]

    it "recovers from a scripted credit" $
        withScene [squirrelWith 1.0 "idle" []] $ \(env, ls) → do
            climate ls 22 0.5
            _ ← luaText ls "__physiology.setMode(1, 'hydration', 'simulated', \
                           \{ hydration = 0.05 }); return true"
            luaNum ls "__physiology.credit(1, 'hydration', 1)" >>= near 1e-4 0.13
            readOwned ls 1 "hydration" `shouldReturn` "0.18|simulated"
            runSeconds ls 120 1.0
            killed env `shouldReturn` []

-----------------------------------------------------------------------
-- T-12: the virtual meal trigger
-----------------------------------------------------------------------

-- | A salt-Simulated squirrel with thermo, hydration and nutrition Nominal
--   (no sweat), the pool and reserve set by value, run for @seconds@:
--   how many virtual meals fired and when.
mealRun ∷ [(Text, PhysioMode)] → Double → Double → Double → IO [Double]
mealRun modes poolFrac reserveFrac seconds =
    withScene [squirrelWith 1.0 "idle" (("thermo", nom) : modes)] $ \(_, ls) → do
        climate ls 22 0.5
        _ ← luaText ls $
            "__physiology.setMode(1, 'salt', 'simulated', { salt = "
            <> tshow (poolFrac * 0.3) <> ", salt_store = "
            <> tshow (reserveFrac * 0.3) <> " }); return true"
        meals ← countMeals ls seconds
        pure meals

-- | Run and return the times at which the VIRTUAL meal credited the
--   reserve (every call of @salts.mealSalt@: nothing else eats here).
countMeals ∷ LuaBackendState → Double → IO [Double]
countMeals ls seconds = do
    _ ← luaText ls
        "_G.__mealAt = {}; _G.__now = 0; \
        \local salts = require('scripts.salts'); local orig = salts.mealSalt; \
        \salts.mealSalt = function(uid) __mealAt[#__mealAt + 1] = __now; return orig(uid) end; \
        \return true"
    _ ← trackWith ls 1 seconds 0.1 ["salt"] "function(step, t) __now = t end"
    out ← luaText ls "return table.concat(__mealAt, ',')"
    pure [ v | w ← T.splitOn "," out, [(v, "")] ← [reads (T.unpack w)] ]

t12 ∷ Spec
t12 = describe "T-12 virtual meal trigger (salt Simulated, the rest Nominal, squirrel mass)" $ do

    it "(a) fires exactly one meal when the pool crosses 0.85 with an empty reserve, \
       \and the meal lands in the reserve" $ do
        meals ← mealRun [] 0.8505 0 30
        length meals `shouldBe` 1
        -- baseline excretion crosses 0.85 within about 9 s
        listToMaybe meals `shouldSatisfy` maybe False (\t → t > 7 ∧ t < 11)
        -- the meal is 0.30 x maxSalt: read it right after, before releases blur it
        withScene [squirrelWith 1.0 "idle" [("thermo", nom)]] $ \(_, ls) → do
            _ ← luaText ls "__physiology.setMode(1, 'salt', 'simulated', \
                           \{ salt = 0.25515, salt_store = 0 }); return true"
            runSeconds ls 10 0.1
            luaNum ls "unit.readPhysio(1, 'salt_store')" >>= \r →
                r `shouldSatisfy` (\x → x > 0.08 ∧ x <= 0.09 + 1e-6)

    it "(b) fires none while a reserve is already releasing (pool 0.80, reserve 0.5)" $
        mealRun [] 0.80 0.5 60 `shouldReturn` []

    it "(c) fires none for a pool above the threshold over 60 s (control)" $
        mealRun [] 0.90 0 60 `shouldReturn` []

    it "(d) fires none when nutrition is Simulated" $
        mealRun [("nutrition", sim)] 0.8505 0 30 `shouldReturn` []

-----------------------------------------------------------------------
-- T-13: sustained coupled exertion
-----------------------------------------------------------------------

t13 ∷ Spec
t13 = describe "T-13 sustained coupled exertion (one game day)" $
    it "re-arms the virtual meal after each replenishment and holds the band" $
        withScene [squirrelWith 1.2 "walking" [("thermo", sim), ("salt", sim)]] $ \(_, ls) → do
            climate ls 22 0.5
            _ ← luaText ls
                "__physiology.setMode(1, 'thermo', 'simulated', { core_temp = 37.0 }); \
                \__physiology.setMode(1, 'salt', 'simulated', \
                \{ salt = 0.3, salt_store = 0 }); return true"
            _ ← luaText ls
                "_G.__mealAt, _G.__after, _G.__pending, _G.__now = {}, {}, 0, 0; \
                \local salts = require('scripts.salts'); local orig = salts.mealSalt; \
                \salts.mealSalt = function(uid) \
                \  __mealAt[#__mealAt + 1] = __now; __pending = 2; return orig(uid) end; \
                \return true"
            -- the pool is judged on the tick AFTER the meal: the meal fires
            -- when the pool dips below 0.85 (by at most one tick's drain) and
            -- the next tick's release lifts it back above (design T-13)
            ex ← trackWith ls 1 1440 0.1 ["salt_conc", "salt", "core_temp"]
                "function(step, t) __now = t; \
                \if __pending > 0 then __pending = __pending - 1; \
                \if __pending == 0 then \
                \__after[#__after + 1] = unit.readPhysio(1, 'salt') / 0.3 end end end"
            mealTimes ← parse <$> luaText ls "return table.concat(__mealAt, ',')"
            after ← parse <$> luaText ls "return table.concat(__after, ',')"
            -- at least two meals: the predicate re-arms after a replenishment
            length mealTimes `shouldSatisfy` (>= 2)
            -- after each meal the pool is back above 0.85 of maxSalt
            forM_ after $ \frac → frac `shouldSatisfy` (> 0.85)
            -- the band, and no NaN
            exMin (ex HM.! "salt_conc") `shouldSatisfy` (>= 0.80)
            exMax (ex HM.! "salt_conc") `shouldSatisfy` (<= 1.35)
            forM_ (HM.elems ex) $ \e → exNaN e `shouldBe` 0
            -- the design's drain budget predicts meals near 195 s, 585 s,
            -- 970 s and 1360 s (period 0.30 / 0.000773 = 388 s)
            listToMaybe mealTimes `shouldSatisfy` maybe False (\t → t > 150 ∧ t < 250)
            forM_ (zip mealTimes (drop 1 mealTimes)) $ \(a, b) →
                (b - a) `shouldSatisfy` (\g → g > 300 ∧ g < 480)
  where
    parse out = [ v | w ← T.splitOn "," out, [(v, "")] ← [reads (T.unpack w) ∷ [(Double, String)]] ]
