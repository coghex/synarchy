{-# LANGUAGE Strict, OverloadedStrings #-}
-- | Nominal neutrality, interface equivalence and the healthy steady state
--   of the "physiology stat modes" group (#2831; design I1, I2, I3).
--
--   * __I1__ — each system Nominal in turn, the others Simulated, in an
--     arctic and a desert climate: reads equal the declared substitutes,
--     meters read 0, contributions into it are discarded, no effect of it
--     fires, its keys stay absent. Run through the real consumers (the AI
--     need utilities, @consumable.drink@, the brain, the strength refresh).
--   * __I2__ — the six dependency edges between systems: the system on one
--     side Simulated, the other Nominal, then the other replaced by a STUB
--     that supplies exactly its declared substitute interface; the
--     trajectories agree to 1e-6 over 600 ticks. This is what proves a
--     Nominal system leaks nothing beyond its interface. The limits are the
--     design's: sampled trajectories, one scenario per edge.
--   * __I3__ — each system alone Simulated (the others Nominal), temperate
--     climate, idle and sustained exertion, 600 tick-seconds: the healthy
--     band holds and nothing fires.
module Test.Headless.Unit.PhysiologyModes.Neutrality (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Engine.Scripting.Lua.Types (LuaBackendState)
import Unit.Command.Types (UnitCommand(..))
import Test.Headless.Unit.PhysiologyModes.Harness
import Unit.Types

-- | The reference acolyte with the named systems Nominal by default.
acolyteWith ∷ [Text] → UnitSpec
acolyteWith nominal = (unitSpec 1 "acolyte" (Just "rich"))
    { usModes = [ (s, PhysioNominal) | s ← nominal ] }

allSystems ∷ [Text]
allSystems = ["thermo", "salt", "hydration", "nutrition"]

-- | The keys each system owns, derives or meters.
keysOf ∷ Text → [Text]
keysOf "thermo"    = ["core_temp", "hypothermia", "hyperthermia"]
keysOf "salt"      = ["salt", "salt_store", "salt_conc", "salt_imbalance"]
keysOf "hydration" = ["hydration"]
keysOf _           = ["hunger", "calories"]

-- | What a Nominal system's keys read: the key, the Lua expression for the
--   healthy value (a Nominal pool follows the unit's own body, which a
--   Simulated nutrition elsewhere in the unit may regrow), and the mode.
substitutes ∷ Text → [(Text, Text)]
substitutes "thermo" =
    [("core_temp", "37"), ("hypothermia", "0"), ("hyperthermia", "0")]
substitutes "salt" =
    [ ("salt", "unit.getStat(1, 'body_mass')")
    , ("salt_store", "2 * unit.getStat(1, 'body_mass')")
    , ("salt_conc", "1"), ("salt_imbalance", "0") ]
substitutes "hydration" = [("hydration", "unit.getStat(1, 'max_hydration')")]
substitutes _ =
    [ ("hunger", "unit.getStat(1, 'max_hunger')")
    , ("calories", "unit.getStat(1, 'max_calories')") ]

-- | The two climates of I1. Cold and hot enough that a Simulated thermo
--   is under real stress, mild enough that nothing dies inside 300 s.
arctic, desert ∷ (Text, Double, Double)
arctic = ("arctic", -20, 0.3)
desert = ("desert", 40, 0.1)

spec ∷ Spec
spec = describe "Nominal neutrality, interface equivalence, healthy steady state" $ do
    describe "I1 Nominal neutrality" $
        forM_ allSystems $ \sys → forM_ [arctic, desert] $ \(label, temp, hum) →
            it (T.unpack sys <> " Nominal in the " <> T.unpack label) $
                withScene [acolyteWith [sys]] $ \(env, ls) → do
                    climate ls temp hum
                    runSeconds ls 300 0.1
                    -- reads equal the substitutes, meters read 0
                    forM_ (substitutes sys) $ \(k, want) → do
                        got ← luaNum ls ("(unit.readPhysio(1, '" <> k <> "'))")
                        wanted ← luaNum ls want
                        (k, got) `shouldSatisfy` (\(_, g) → abs (g - wanted) < 1e-4 * max 1 wanted)
                        luaText ls ("local _, m = unit.readPhysio(1, '" <> k <> "'); return m")
                            `shouldReturn` "nominal"
                    -- its keys stay absent after the ticks
                    inst ← readInstance env 1
                    forM_ (keysOf sys) $ \k →
                        HM.member k (uiStats inst) `shouldBe` False
                    -- no effect of it fires
                    killed env `shouldReturn` []
                    scriptErrors ls `shouldReturn` ""
                    survivalEvents ls `shouldReturn` []

    describe "I1 through the real consumers" $ do
        it "hydration Nominal: the AI never wants a drink, a credit is discarded, \
           \and the facade reports 0 applied" $
            withScene [acolyteWith ["hydration"]] $ \(env, ls) → do
                climate ls 50 0.1
                runSeconds ls 120 0.1
                luaText ls (needsLua "needs.drinkUtility(1, {}, PARAMS)")
                    `shouldReturn` "-inf"
                luaText ls "return tostring(__physiology.credit(1, 'hydration', 9))"
                    `shouldReturn` "0"
                luaText ls "return tostring(__physiology.debit(1, 'hydration', 9))"
                    `shouldReturn` "0"
                inst ← readInstance env 1
                HM.member "hydration" (uiStats inst) `shouldBe` False

        it "hydration Nominal: a coffee sip changes nothing it owns" $
            withScene [acolyteWith ["hydration"]] $ \(env, ls) → do
                _ ← luaText ls stockCoffee
                out ← luaText ls
                    "local r = require('scripts.consumable').drink(1, 'coffee_pot'); \
                    \return tostring(r ~= nil)"
                out `shouldBe` "true"
                inst ← readInstance env 1
                HM.member "hydration" (uiStats inst) `shouldBe` False
                readOwned ls 1 "hydration" `shouldReturn` "42|nominal"

        it "nutrition Nominal: the AI never wants to eat, nothing digests and \
           \strength carries no calorie discount" $
            withScene [acolyteWith ["nutrition"] ] $ \(env, ls) → do
                runSeconds ls 120 0.1
                luaText ls (needsLua "needs.eatUtility(1, {}, PARAMS)")
                    `shouldReturn` "-inf"
                luaNum ls "unit.getStat(1, 'strength') - unit.getStat(1, 'strength_body')"
                    >>= near 1e-6 0
                inst ← readInstance env 1
                HM.member "hunger" (uiStats inst) `shouldBe` False
                HM.member "calories" (uiStats inst) `shouldBe` False

        it "thermo Nominal: the brain stays lucid and the circulation healthy in the arctic, \
           \where a Simulated thermo's does not" $ do
            let consciousnessAfter modes = withScene [acolyteWith modes] $ \(_, ls) → do
                    climate ls (-40) 0.3
                    ex ← track ls 1 600 0.1 ["cons", "core_temp"]
                    pure (exMin (ex HM.! "cons"), exMin (ex HM.! "core_temp"))
            (nominalCons, nominalCore) ← consciousnessAfter ["thermo"]
            (simCons, simCore) ← consciousnessAfter []
            nominalCons `shouldSatisfy` (> 0.99)
            nominalCore `shouldBe` 37
            simCore `shouldSatisfy` (< 36)
            simCons `shouldSatisfy` (<= nominalCons)

        it "salt Nominal: sweating in the desert neither cramps nor impairs sweat" $
            withScene [acolyteWith ["salt"]] $ \(_, ls) → do
                climate ls 50 0.1
                runSeconds ls 300 0.1
                luaNum ls "require('scripts.salts').speedMultiplier(1)" >>= near 1e-9 1
                luaNum ls "require('scripts.salts').sweatFactor(1)" >>= near 1e-9 1
                luaNum ls "require('scripts.salts').imbalanceFailure(1)" >>= near 1e-9 0
                luaText ls "return require('scripts.salts').stateLabel(1)"
                    `shouldReturn` "Balanced"

    describe "I2 interface equivalence (six edges)" $ do
        edge "nutrition → thermo: cal_factor"
            (acolyteWith ["salt", "hydration", "nutrition"]) (-10, 0.3)
            ["core_temp", "circulation"]
            (acolyteWith ["salt", "hydration"])
            "unit.setStat(1, 'calories', 0.74 * unit.getStat(1, 'max_calories')); \
            \unit.setStat(1, 'hunger', 0)"
        edge "thermo → hydration: the sweat debit is discarded"
            (acolyteWith ["salt", "hydration", "nutrition"]) (45, 0.2)
            ["core_temp", "hydration"]
            (acolyteWith ["salt", "nutrition"])
            "unit.setStat(1, 'hydration', unit.getStat(1, 'max_hydration'))"
        edge "thermo → salt: the sweat debit is discarded"
            (acolyteWith ["salt", "hydration", "nutrition"]) (45, 0.2)
            ["core_temp", "salt", "salt_store"]
            (acolyteWith ["hydration", "nutrition"])
            "unit.setStat(1, 'salt', unit.getStat(1, 'body_mass')); \
            \unit.setStat(1, 'salt_store', 2 * unit.getStat(1, 'body_mass'))"
        edge "salt → thermo: sweat_factor"
            (acolyteWith ["salt", "hydration", "nutrition"]) (50, 0.1)
            ["core_temp", "salt_conc"]
            (acolyteWith ["hydration", "nutrition"])
            "unit.setStat(1, 'salt', unit.getStat(1, 'body_mass')); \
            \unit.setStat(1, 'salt_store', 2 * unit.getStat(1, 'body_mass'))"
        edge "hydration → salt: body_water_fraction"
            ((acolyteWith ["thermo", "hydration", "nutrition"])
                { usStats = [("salt", 50), ("salt_store", 100)] }) (20, 0.5)
            ["salt", "salt_store", "salt_conc"]
            ((acolyteWith ["thermo", "nutrition"])
                { usStats = [("salt", 50), ("salt_store", 100)] })
            "unit.setStat(1, 'hydration', unit.getStat(1, 'max_hydration'))"
        virtualMealEdge

    describe "I3 healthy steady state" $
        forM_ allSystems $ \sys → forM_ [("idle", "idle"), ("exertion", "walking")] $
          \(lbl, activity) →
            it (T.unpack sys <> " alone Simulated, " <> lbl <> ", 600 tick-seconds") $
                withScene [ (acolyteWith (filter (≢ sys) allSystems))
                                { usActivity = activity } ] $ \(env, ls) → do
                    climate ls 20 0.5
                    ex ← track ls 1 600 0.1 (healthyKeys sys)
                    -- no meter above 0, no NaN
                    forM_ (HM.toList ex) $ \(k, e) → do
                        (k, exNaN e) `shouldBe` (k, 0)
                        when (k `elem` ["hypothermia", "hyperthermia", "salt_imbalance"]) $
                            (k, exMax e) `shouldBe` (k, 0)
                    -- the system's healthy band
                    case sys of
                        "thermo" → do
                            exMin (ex HM.! "core_temp") `shouldSatisfy` (>= 35.0)
                            exMax (ex HM.! "core_temp") `shouldSatisfy` (<= 39.0)
                        "salt" → do
                            exMin (ex HM.! "salt_conc") `shouldSatisfy` (>= 0.80)
                            exMax (ex HM.! "salt_conc") `shouldSatisfy` (<= 1.35)
                        "hydration" →
                            exMin (ex HM.! "hydration") `shouldSatisfy` (> 0.25 * 42)
                        _ → exMin (ex HM.! "calories") `shouldSatisfy` (> 0)
                    -- nothing fires: no collapse, death_threshold, kill_on_zero,
                    -- survival alert or death
                    cmds ← drainCommands env
                    [ u | UnitKill u ← cmds ] `shouldBe` []
                    collapses' cmds `shouldBe` []
                    survivalEvents ls `shouldReturn` []
                    scriptErrors ls `shouldReturn` ""

-- | The reads tracked for one system's healthy band.
healthyKeys ∷ Text → [Text]
healthyKeys "thermo"    = ["core_temp", "hypothermia", "hyperthermia", "cons"]
healthyKeys "salt"      = ["salt_conc", "salt_imbalance", "salt", "cons"]
healthyKeys "hydration" = ["hydration"]
healthyKeys _           = ["calories", "hunger"]

collapses' ∷ [UnitCommand] → [UnitId]
collapses' cmds = [ u | UnitCollapse u ← cmds ]

-- | The Lua that stocks one quality coffee pot for @consumable.drink@, with
--   the three engine verbs the drink touches intercepted (the pattern the
--   consumable's own spec uses).
stockCoffee ∷ Text
stockCoffee = T.unlines
    [ "local inv = { { defName = 'coffee_pot', instanceId = 31,"
    , "  currentFill = 1.0, quality = 80 } }"
    , "unit.getInventory = function() return inv end"
    , "unit.getItemTemp = function() return 80 end"
    , "unit.drink = function() end"
    , "unit.modifyItemFillById = function(uid, iid, delta)"
    , "  local it = inv[1]"
    , "  local applied = math.max(delta, -it.currentFill)"
    , "  it.currentFill = it.currentFill + applied"
    , "  return applied"
    , "end"
    , "return true" ]

-- | A Lua expression over the shipped AI need utilities with the acolyte's
--   tunables, as text (@-inf@ for minus infinity).
needsLua ∷ Text → Text
needsLua expr = T.unlines
    [ "local needs = require('scripts.unit_ai_needs')"
    , "local PARAMS = require('scripts.unit_ai_config_defaults')"
    , "  .applyAll(require('scripts.unit_ai_tunables')).acolyte"
    , "local v = " <> expr
    , "if v == -math.huge then return '-inf' end"
    , "return tostring(v)" ]

-----------------------------------------------------------------------
-- I2
-----------------------------------------------------------------------

-- | One dependency edge. @nominalSide@ runs the system under test with the
--   neighbour Nominal; @stubSide@ runs the same scenario with the neighbour
--   Simulated and, after EVERY tick, the stub resetting it to the healthy
--   state its substitute interface declares. The recorded keys must match
--   at every tick.
edge ∷ String → UnitSpec → (Double, Double) → [Text] → UnitSpec → Text → Spec
edge label nominalSide (temp, hum) keys stubSide stubLua =
    it label $ do
        a ← trajectory nominalSide Nothing
        b ← trajectory stubSide (Just stubLua)
        length a `shouldBe` length b
        let worst = maximum (0 : zipWith (\x y → abs (x - y)) a b)
        worst `shouldSatisfy` (< 1e-6)
  where
    trajectory sp mStub = withScene [sp] $ \(_, ls) → do
        climate ls temp hum
        recordTrajectory ls keys 600 0.1 mStub

-- | The recorded values, key by key, tick by tick.
recordTrajectory ∷ LuaBackendState → [Text] → Int → Double → Maybe Text
                 → IO [Double]
recordTrajectory ls keys ticks dt mStub = do
    let keyList = "{" <> T.intercalate "," [ "'" <> k <> "'" | k ← keys ] <> "}"
        stub = fromMaybe "" mStub
    out ← luaText ls $ T.unlines
        [ "local keys = " <> keyList
        , "local o = {}"
        , "for _ = 1, " <> tshow ticks <> " do"
        , "  __resources.update(" <> tshow dt <> ")"
        , "  " <> stub
        , "  for _, k in ipairs(keys) do"
        , "    local v = unit.readPhysio(1, k)"
        , "    if v == nil then v = unit.getStat(1, k) end"
        , "    o[#o + 1] = string.format('%.9g', v or -1)"
        , "  end"
        , "end"
        , "return table.concat(o, ',')" ]
    pure [ v | t ← T.splitOn "," out, [(v, "")] ← [reads (T.unpack t)] ]

-- | Edge six: nutrition's meal contribution into salt. The Nominal side
--   fires the VIRTUAL meal inside the salt tick; the stub side has a
--   Simulated nutrition whose meal is supplied by the stub, under the same
--   predicate, right after the tick. Both start at the T-12 (a) values, so
--   the meal fires inside the window.
virtualMealEdge ∷ Spec
virtualMealEdge = it "nutrition → salt: the virtual meal" $ do
    let start = [("salt", 0.8505 * 70), ("salt_store", 0)]
        -- the stub's calories sit below the regrowth band and its stomach is
        -- empty, so the stand-in nutrition neither regrows body mass (which
        -- would move maxSalt) nor digests: only its declared meal contribution
        -- reaches salt
        pinned = [("calories", 0.74 * 1400), ("hunger", 0)]
        nominalSide = (acolyteWith ["thermo", "hydration", "nutrition"]) { usStats = start }
        stubSide    = (acolyteWith ["thermo", "hydration"]) { usStats = start ⧺ pinned }
        stub = "local mx = require('scripts.salts').maxSalt(1); \
               \if unit.readPhysio(1, 'salt') / mx < 0.85 \
               \and unit.readPhysio(1, 'salt_store') < 0.30 * mx then \
               \require('scripts.salts').mealSalt(1) end"
        run sp mStub = withScene [sp] $ \(_, ls) → do
            climate ls 20 0.5
            recordTrajectory ls ["salt", "salt_store", "salt_conc"] 600 0.1 mStub
    a ← run nominalSide Nothing
    b ← run stubSide (Just stub)
    length a `shouldBe` length b
    maximum (0 : zipWith (\x y → abs (x - y)) a b) `shouldSatisfy` (< 1e-6)
    -- the scenario really did exercise the meal: the reserve was credited
    maximum [ v | (i, v) ← zip [0 ∷ Int ..] a, i `mod` 3 ≡ 1 ] `shouldSatisfy` (> 20)
