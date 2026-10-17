{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The "physiology exhaustion and sleep modes" gate (#2835, S8b of the
--   physiology stat-modes epic #2827): exhaustion and sleep pressure as the
--   sixth and seventh registry systems, run through the SHIPPED registry,
--   the SHIPPED Lua (@scripts\/unit_resources.lua@ and everything it ticks,
--   @scripts\/exhaustion.lua@, @scripts\/movement_speed.lua@,
--   @scripts\/unit_ai_sleep.lua@) and the REAL registered @unit.*@ API over
--   a live headless engine, with the real save codec for persistence.
--
--   One module per contract area:
--
--     * this module: the declarations, the maxima, Nominal neutrality (I1),
--       the interface equivalence (I2), live behavior (I3) and the mixed
--       modes;
--     * "Test.Headless.Unit.PhysiologyExhaustionSleepModes.Sleep": the sleep
--       need (utility, wake predicate, cancellation of a started sequence);
--     * "Test.Headless.Unit.PhysiologyExhaustionSleepModes.Persistence":
--       transitions (I5) and persistence (I6), through the real codec and
--       the load gate.
--
--   Nothing is rolled: every input is a literal. Run just this gate:
--   @cabal test synarchy-test-headless --test-options='--match
--   "physiology exhaustion and sleep modes"'@.
module Test.Headless.Unit.PhysiologyExhaustionSleepModes (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Engine.Core.State (EngineEnv)
import Engine.Scripting.Lua.Types (LuaBackendState)
import qualified Test.Headless.Unit.PhysiologyExhaustionSleepModes.Persistence as Persistence
import qualified Test.Headless.Unit.PhysiologyExhaustionSleepModes.Sleep as Sleep
import Test.Headless.Unit.PhysiologyFixture (readShippedPhysiologyRegistry)
import Test.Headless.Unit.PhysiologyModes.Harness
import Unit.Command.Types (UnitCommand(..))
import Unit.Physiology
import Unit.Physiology.Types
import Unit.Types

spec ∷ Spec
spec = describe "physiology exhaustion and sleep modes" $ do
    declarationSpec
    maximumSpec
    neutralitySpec
    interfaceSpec
    liveSpec
    mixedSpec
    Sleep.spec
    Persistence.spec

-----------------------------------------------------------------------
-- Fixtures
-----------------------------------------------------------------------

-- | An acolyte-config unit whose ONLY simulated-or-not systems are the two
--   under test: every other system is Nominal (the @exempt@ profile), so
--   nothing else drains, shivers or kills. The endurance is a literal.
--   The two modes are the UNIT'S pins, not the definition's defaults:
--   units of one scene share the definition named @acolyte@ (the scene keys
--   definitions by name), so a per-unit default would make the last unit's
--   modes everyone's.
poolUnit ∷ Int → Float → PhysioMode → PhysioMode → UnitSpec
poolUnit uid e ex sl = (unitSpec uid "acolyte" (Just "exempt"))
    { usOverrides = [("exhaustion", ex), ("sleep", sl)]
    , usStats = [("endurance", e)] }

-- | The stored (base) value of a pool, or a failing expectation.
stored ∷ EngineEnv → Int → Text → IO (Maybe Float)
stored env uid key = HM.lookup key ∘ uiStats <$> readInstance env uid

-- | How many kills or collapses the units' queue holds (and empty it).
fatalCount ∷ EngineEnv → IO Int
fatalCount env = length ∘ filter fatal <$> drainCommands env
  where
    fatal (UnitKill _)     = True
    fatal (UnitCollapse _) = True
    fatal _                = False

-- | The sleep utility the shipped goal scores for a unit (a string, so
--   @-inf@ survives).
sleepUtilityOf ∷ LuaBackendState → Int → IO Text
sleepUtilityOf ls uid = luaText ls $ T.concat
    [ "require('scripts.unit_ai_core').ensureState(", tshow uid, "); "
    , "return tostring(require('scripts.unit_ai_sleep').sleepUtility(", tshow uid
    , ", require('scripts.unit_ai').getState(", tshow uid, "), "
    , "require('scripts.unit_ai_tunables').acolyte))" ]

-----------------------------------------------------------------------
-- Declarations
-----------------------------------------------------------------------

declarationSpec ∷ Spec
declarationSpec = describe "the two systems and their declarations" $ do
    it "registers exhaustion and sleep separately, each requiring a body and \
       \owning exactly its pool, with no meters and no derived keys" $ do
        reg ← readShippedPhysiologyRegistry
        forM_ [("exhaustion", "exhaustion"), ("sleep", "sleep_pressure")] $
            \(sid, key) → case registrySystem reg sid of
                Nothing → expectationFailure ("no system " <> show sid)
                Just sys → do
                    psRequires sys `shouldBe` ["body"]
                    map okKey (psOwns sys) `shouldBe` [key]
                    psMeters sys `shouldBe` []
                    psDerived sys `shouldBe` []
        classifyKey reg "max_exhaustion" `shouldBe` Nothing
        classifyKey reg "max_sleep_pressure" `shouldBe` Nothing

    it "declares the interfaces the design lists, as constants, and an effect \
       \that is absent while Nominal" $ do
        reg ← readShippedPhysiologyRegistry
        let section f sid = maybe [] (map (\e → (ieName e, ieSubstitute e)) ∘ f)
                                  (registrySystem reg sid)
        section psProvides "exhaustion" `shouldBe`
            [ ("restedness_fraction", SubValue (QLit 1.0))
            , ("movement_multiplier", SubValue (QLit 1.0)) ]
        section psContributes "exhaustion" `shouldBe`
            [("sleep_exhaustion_deficit", SubValue (QLit 0.0))]
        section psEffects "exhaustion" `shouldBe` [("speed_penalty", SubNone)]
        section psProvides "sleep" `shouldBe`
            [("sleep_deficit", SubValue (QLit 0.0))]
        section psEffects "sleep" `shouldBe`
            [("sleep_need", SubNone), ("pressure_wake", SubNone)]

    it "gives every profile the exhaustion and sleep modes of the species \
       \table: rich S/S, reduced S/S, simple S/N, exempt N/N" $ do
        reg ← readShippedPhysiologyRegistry
        forM_ [ ("rich", PhysioSimulated, PhysioSimulated)
              , ("reduced", PhysioSimulated, PhysioSimulated)
              , ("simple", PhysioSimulated, PhysioNominal)
              , ("exempt", PhysioNominal, PhysioNominal) ] $ \(p, ex, sl) →
            fmap (\pr → ( Map.lookup "exhaustion" (ppModes pr)
                        , Map.lookup "sleep" (ppModes pr)))
                 (registryProfile reg p)
                `shouldBe` Just (Just ex, Just sl)

-----------------------------------------------------------------------
-- The maxima
-----------------------------------------------------------------------

-- | Binary32 in Lua: the round trip the engine's @Float@ storage performs.
f32Lua ∷ Text
f32Lua = "local function f32(x) return (string.unpack('<f', string.pack('<f', x))) end"

maximumSpec ∷ Spec
maximumSpec = describe "the healthy value is the Lua maximum" $ do
    it "equals stats.get(max_*) to binary32 precision for every shipped \
       \species and a spread of rolled endurances, read Nominal or Simulated, \
       \and a Simulated unit's spawn seed is that value" $ do
        let species = [ ("acolyte", "rich", acolyteBody)
                      , ("bear_brown", "reduced", acolyteBody)
                      , ("red_squirrel", "simple", squirrelBody) ]
            endurances = [0.3, 0.55, 1.0, 1.37, 2.9] ∷ [Float]
            specs = [ (unitSpec uid name (Just prof))
                          { usBody = body, usStats = [("endurance", e)] }
                    | (si, (name, prof, body)) ← zip [0 ∷ Int ..] species
                    , (ei, e) ← zip [0 ∷ Int ..] endurances
                    , let uid = 10 * (si + 1) + ei + 1 ]
        withScene specs $ \(_, ls) → do
            out ← luaText ls $ T.unlines
                [ f32Lua
                , "local st = require('scripts.unit_stats')"
                , "local bad, checked = {}, 0"
                , "for _, uid in ipairs(unit.getAllIds()) do"
                , "  for _, c in ipairs({ { 'exhaustion', 'exhaustion', 'max_exhaustion' },"
                , "      { 'sleep_pressure', 'sleep', 'max_sleep_pressure' } }) do"
                , "    local key, system, maxKey = c[1], c[2], c[3]"
                , "    local want = f32(st.get(uid, maxKey))"
                , "    local v, mode = unit.readPhysio(uid, key)"
                , "    checked = checked + 1"
                , "    if v ~= want then bad[#bad + 1] = uid .. ':' .. key .. ':' .. tostring(v) .. '~' .. tostring(want) end"
                , "    if mode == 'simulated' and unit.getStatBase(uid, key) ~= want then"
                , "      bad[#bad + 1] = uid .. ':' .. key .. ':seed' end"
                , "  end"
                , "end"
                , "return checked .. '|' .. table.concat(bad, ',')" ]
            out `shouldBe` "30|"

    it "answers the substitutes as constants, never as get(owned) / get(max): \
       \at a rolled endurance of 0.3 the binary32 healthy value is a hair \
       \under the double maximum, and a ratio would read as a deficit" $
        withScene [poolUnit 1 0.3 PhysioNominal PhysioNominal] $ \(_, ls) → do
            -- the trap, stated: 0.3f * 10 rounds to 3.0 in binary32 while the
            -- double product is 3.0000001192...
            luaText ls "return __f(unit.readPhysio(1, 'exhaustion'))"
                `shouldReturn` "3"
            luaBool ls "unit.readPhysio(1, 'exhaustion') / \
                       \require('scripts.unit_stats').get(1, 'max_exhaustion') < 1.0"
                `shouldReturn` True
            -- the constants
            luaBool ls "require('scripts.exhaustion').fraction(1) == 1.0"
                `shouldReturn` True
            luaBool ls "require('scripts.exhaustion').speedMultiplier(1) == 1.0"
                `shouldReturn` True
            luaBool ls "require('scripts.movement_speed').bandMultiplier(1) == 1.0"
                `shouldReturn` True
            sleepUtilityOf ls 1 `shouldReturn` "-inf"

-----------------------------------------------------------------------
-- Nominal neutrality (I1)
-----------------------------------------------------------------------

neutralitySpec ∷ Spec
neutralitySpec = describe "Nominal neutrality (I1)" $ do
    forM_ [("arctic", -20, 0.4), ("desert", 45, 0.1)] $ \(label, temp, hum) →
        it ("keeps both owned keys absent, the healthy reads stable and the \
            \interfaces neutral over repeated passes in the " <> label) $
            withScene [poolUnit 1 0.3 PhysioNominal PhysioNominal] $ \(env, ls) → do
                climate ls temp hum
                _ ← luaText ls "_G.__rand = 0; local r = math.random; \
                               \math.random = function(...) \
                               \__rand = __rand + 1; return r(...) end; \
                               \return true"
                forM_ [1 ∷ Int .. 3] $ \_ → do
                    runSeconds ls 60 0.1
                    -- attempted contributions and raw writes create nothing
                    luaText ls "local p = __physiology; return table.concat({ \
                               \tostring(p.credit(1, 'exhaustion', 5)), \
                               \tostring(p.debit(1, 'exhaustion', 5)), \
                               \tostring(p.credit(1, 'sleep_pressure', 5)), \
                               \tostring(p.debit(1, 'sleep_pressure', 5)), \
                               \tostring(unit.setStat(1, 'exhaustion', 2)), \
                               \tostring(unit.setStat(1, 'sleep_pressure', 2)) }, ',')"
                        `shouldReturn` "0,0,0,0,false,false"
                    inst ← readInstance env 1
                    forM_ ["exhaustion", "sleep_pressure"] $ \k →
                        (k, HM.member k (uiStats inst)) `shouldBe` (k, False)
                    readOwned ls 1 "exhaustion" `shouldReturn` "3|nominal"
                    readOwned ls 1 "sleep_pressure" `shouldReturn` "3|nominal"
                    luaBool ls "require('scripts.exhaustion').fraction(1) == 1.0 \
                               \and require('scripts.exhaustion').speedMultiplier(1) == 1.0"
                        `shouldReturn` True
                    sleepUtilityOf ls 1 `shouldReturn` "-inf"
                fatalCount env `shouldReturn` 0
                scriptErrors ls `shouldReturn` ""
                -- repeated owned reads leave storage, overrides and RNG alone
                -- (the ticks above may roll; the reads themselves must not)
                _ ← luaText ls "__rand = 0; return true"
                before ← readInstance env 1
                _ ← luaText ls "for _ = 1, 200 do unit.readPhysio(1, 'exhaustion'); \
                               \unit.readPhysio(1, 'sleep_pressure'); \
                               \unit.readPhysioBase(1, 'exhaustion'); \
                               \__physiology.get(1, 'sleep_pressure') end; \
                               \return true"
                after ← readInstance env 1
                uiStats after `shouldBe` uiStats before
                uiPhysioOverrides after `shouldBe` uiPhysioOverrides before
                luaText ls "return __rand" `shouldReturn` "0"

    it "gives movement multiplier exactly 1.0 in every gait, identical to a \
       \rested Simulated twin, while a depleted Simulated control keeps the \
       \existing 0.35 / 0.55 band across the whole band" $
        withScene [ poolUnit 1 1.0 PhysioNominal PhysioNominal
                  , poolUnit 2 1.0 PhysioSimulated PhysioNominal
                  , (poolUnit 3 1.0 PhysioSimulated PhysioNominal)
                        { usStats = [("endurance", 1.0), ("exhaustion", 0.5)] } ] $
            \(_, ls) → do
                -- a definition-level top speed, which the minimal fixture lacks
                _ ← luaText ls "unit.getMaxSpeed = function() return 2.0 end; \
                               \_G.__mv = require('scripts.movement_speed'); \
                               \return true"
                forM_ ["sprint", "comfort", "ordered", "meander"] $ \gait → do
                    let at u = luaNum ls ("__mv." <> gait <> "(" <> tshow (u ∷ Int) <> ")")
                    nominal ← at 1
                    twin ← at 2
                    spent ← at 3
                    nominal `shouldSatisfy` (> 0)
                    twin `shouldBe` nominal
                    -- 5 % restedness: 1 - 0.45 * (0.35 - 0.05) / 0.35
                    near 1e-6 (1 - 0.45 * (0.35 - 0.05) / 0.35) (spent / nominal)
                luaBool ls "__mv.bandMultiplier(1) == 1.0" `shouldReturn` True

    it "reports an Unsupported (bodyless) unit as Unsupported, yet answers \
       \speed multiplier exactly 1.0, a full restedness and no sleep utility" $
        withScene [unitSpec 1 "white_tailed_deer" Nothing] $ \(_, ls) → do
            readOwned ls 1 "exhaustion" `shouldReturn` "nil|nil"
            readOwned ls 1 "sleep_pressure" `shouldReturn` "nil|nil"
            luaText ls "return tostring(unit.getPhysioMode(1, 'exhaustion')) \
                       \.. tostring(unit.getPhysioMode(1, 'sleep'))"
                `shouldReturn` "nilnil"
            luaBool ls "require('scripts.exhaustion').fraction(1) == 1.0 \
                       \and require('scripts.exhaustion').speedMultiplier(1) == 1.0"
                `shouldReturn` True
            sleepUtilityOf ls 1 `shouldReturn` "-inf"

-----------------------------------------------------------------------
-- Interface equivalence (I2)
-----------------------------------------------------------------------

interfaceSpec ∷ Spec
interfaceSpec = describe "interface equivalence (I2)" $ do
    forM_ [("standing", "standing"), ("asleep", "sleeping")] $ \(label, pose) →
        it ("a Simulated sleep with exhaustion Nominal tracks a full healthy \
            \exhaustion over 600 ticks, within 1e-6, " <> label) $
            withScene
                [ (poolUnit 1 1.0 PhysioNominal PhysioSimulated)
                    { usStats = [("endurance", 1.0), ("sleep_pressure", 5)]
                    , usPose = pose }
                , (poolUnit 2 1.0 PhysioSimulated PhysioSimulated)
                    { usStats = [("endurance", 1.0), ("sleep_pressure", 5)]
                    , usPose = pose } ] $ \(_, ls) → do
                out ← luaText ls $ T.unlines
                    [ "local ai = require('scripts.unit_ai')"
                    , "local core = require('scripts.unit_ai_core')"
                    , "local sleep = require('scripts.unit_ai_sleep')"
                    , "local params = require('scripts.unit_ai_tunables').acolyte"
                    , "core.ensureState(1); core.ensureState(2)"
                    , "world.getSunAngleAt = function() return 0.75 end"
                    , "local function util(u) return sleep.sleepUtility(u, ai.getState(u), params) end"
                    , "local worst, utilWorst, finite = 0, 0, true"
                    , "for _ = 1, 600 do"
                    , "  __resources.update(0.1)"
                    -- the stub: unit 2's exhaustion is held at the full healthy value
                    , "  unit.setStat(2, 'exhaustion', 10)"
                    , "  local a, b = unit.getStat(1, 'sleep_pressure'), unit.getStat(2, 'sleep_pressure')"
                    , "  local ua, ub = util(1), util(2)"
                    , "  if a ~= a or b ~= b or ua ~= ua then finite = false end"
                    , "  worst = math.max(worst, math.abs(a - b))"
                    , "  utilWorst = math.max(utilWorst, math.abs(ua - ub))"
                    , "end"
                    , "return tostring(finite) .. '|' .. string.format('%.3g|%.3g', worst, utilWorst)" ]
                let (finite, rest) = T.breakOn "|" out
                    nums = T.splitOn "|" (T.drop 1 rest)
                finite `shouldBe` "true"
                forM_ nums $ \n → case reads (T.unpack n) of
                    [(v, "")] → (v ∷ Double) `shouldSatisfy` (< 1e-6)
                    _ → expectationFailure ("not a number: " <> T.unpack n)

    it "adds the existing weighted deficit and speed penalty for a depleted \
       \Simulated exhaustion control" $
        withScene
            [ (poolUnit 1 1.0 PhysioNominal PhysioSimulated)
                { usStats = [("endurance", 1.0), ("sleep_pressure", 5)] }
            , (poolUnit 2 1.0 PhysioSimulated PhysioSimulated)
                { usStats = [("endurance", 1.0), ("sleep_pressure", 5)
                            , ("exhaustion", 1.0)] } ] $ \(_, ls) → do
            _ ← luaText ls "world.getSunAngleAt = function() return 0.75 end; \
                           \return true"
            let util u = sleepUtilityOf ls u >>= \t → case reads (T.unpack t) of
                    [(v, "")] → pure (v ∷ Double)
                    _ → expectationFailure ("not a number: " <> T.unpack t) >> pure 0
            healthy ← util 1
            depleted ← util 2
            -- sleep_exhaustion_weight 2.0 * (1 - 0.1)
            near 1e-6 1.8 (depleted - healthy)
            -- the speed penalty: 1 - 0.45 * (0.35 - 0.1) / 0.35
            luaNum ls "require('scripts.exhaustion').speedMultiplier(2)"
                >>= near 1e-6 (1 - 0.45 * (0.35 - 0.1) / 0.35)
            luaText ls "return tostring(require('scripts.exhaustion').speedMultiplier(1))"
                `shouldReturn` "1.0"

-----------------------------------------------------------------------
-- Live behavior (I3)
-----------------------------------------------------------------------

-- | Tick @seconds@ at 0.1 and report one key's extremes and last value.
extremesOf ∷ LuaBackendState → Int → Double → Text → IO (Extremes, Double)
extremesOf ls uid seconds key = do
    ex ← track ls uid seconds 0.1 [key]
    end ← luaNum ls ("unit.getStat(" <> tshow uid <> ", '" <> key <> "')")
    case HM.lookup key ex of
        Just e  → pure (e, end)
        Nothing → expectationFailure "no extremes" >> pure (Extremes 0 0 0, end)

firstOf ∷ [Double] → Double
firstOf (x : _) = x
firstOf []      = 0

liveSpec ∷ Spec
liveSpec = describe "each system alone Simulated (I3)" $ do
    it "exhaustion holds when idle, drains under exertion down to the 0.55 \
       \floor, recovers at rest, and stays finite and bounded throughout" $
        withScene [ (poolUnit 1 1.0 PhysioSimulated PhysioNominal)
                        { usActivity = "idle" } ] $ \(env, ls) → do
            (idle, idleEnd) ← extremesOf ls 1 600 "exhaustion"
            exNaN idle `shouldBe` 0
            exMax idle `shouldSatisfy` (<= 10.0001)
            idleEnd `shouldSatisfy` (> 9.99)               -- idle regen beats the drain
            -- exertion: 600 s of walking nets -0.02 / s from a full pool
            modifyInstance env 1 (\u → u { uiActivity = "walking" })
            (walk, walkEnd) ← extremesOf ls 1 600 "exhaustion"
            exNaN walk `shouldBe` 0
            exMin walk `shouldSatisfy` (>= 0)
            walkEnd `shouldSatisfy` (< 1.0)
            -- the band: restedness below 0.35 slows, at empty the floor binds
            luaNum ls "require('scripts.exhaustion').speedMultiplier(1)"
                >>= \m → m `shouldSatisfy` (\x → x >= 0.55 - 1e-9 && x < 0.62)
            -- rest: the pool climbs back to full
            modifyInstance env 1 (\u → u { uiActivity = "idle" })
            (rest, restEnd) ← extremesOf ls 1 600 "exhaustion"
            exMax rest `shouldSatisfy` (<= 10.0001)
            restEnd `shouldSatisfy` (> 9.99)
            luaNum ls "require('scripts.exhaustion').speedMultiplier(1)"
                >>= near 1e-12 1.0
            fatalCount env `shouldReturn` 0
            scriptErrors ls `shouldReturn` ""

    it "scales exhaustion's timing with size and endurance: the restedness \
       \fraction follows the same trajectory at endurance 0.5, 1 and 2" $
        withScene [ (poolUnit i e PhysioSimulated PhysioNominal)
                        { usActivity = "walking" }
                  | (i, e) ← [(1, 0.5), (2, 1.0), (3, 2.0)] ] $ \(_, ls) → do
            runSeconds ls 300 0.1
            fracs ← forM [1 ∷ Int, 2, 3] $ \u → luaNum ls $
                "unit.getStat(" <> tshow u <> ", 'exhaustion') / \
                \require('scripts.unit_stats').get(" <> tshow u <> ", 'max_exhaustion')"
            forM_ fracs $ \f → near 2e-3 (firstOf fracs) f
            firstOf fracs `shouldSatisfy` (< 1.0)

    it "sleep debt drains while awake whatever the activity, is not cleared \
       \by ordinary idle, and is recovered by real sleeping" $
        withScene [ (poolUnit 1 1.0 PhysioNominal PhysioSimulated)
                        { usActivity = "idle" } ] $ \(env, ls) → do
            (awake, awakeEnd) ← extremesOf ls 1 600 "sleep_pressure"
            exNaN awake `shouldBe` 0
            exMax awake `shouldSatisfy` (<= 10.0001)
            -- the shipped drain: max / 3600 per second = 10 * 600 / 3600 in 600 s
            near 2e-3 (10 - 10 * 600 / 3600) awakeEnd
            -- the same drain under exertion: sleep debt accrues from being
            -- awake, not from exertion
            modifyInstance env 1 (\u → u { uiActivity = "walking" })
            (_, walkEnd) ← extremesOf ls 1 600 "sleep_pressure"
            near 2e-3 (awakeEnd - 10 * 600 / 3600) walkEnd
            -- ordinary rest (a crouch) clears nothing
            modifyInstance env 1 (\u → u { uiActivity = "idle", uiPose = "crouching" })
            (_, restEnd) ← extremesOf ls 1 600 "sleep_pressure"
            restEnd `shouldSatisfy` (< walkEnd)
            -- real sleeping recovers it, to the maximum and no further
            modifyInstance env 1 (\u → u { uiPose = "sleeping" })
            (asleep, asleepEnd) ← extremesOf ls 1 600 "sleep_pressure"
            exNaN asleep `shouldBe` 0
            exMax asleep `shouldSatisfy` (<= 10.0001)
            asleepEnd `shouldSatisfy` (> restEnd)
            fatalCount env `shouldReturn` 0
            scriptErrors ls `shouldReturn` ""

-----------------------------------------------------------------------
-- Mixed modes, and the squirrel
-----------------------------------------------------------------------

mixedSpec ∷ Spec
mixedSpec = describe "mixed modes" $ do
    forM_ [ (PhysioNominal, PhysioSimulated), (PhysioSimulated, PhysioNominal)
          , (PhysioNominal, PhysioNominal), (PhysioSimulated, PhysioSimulated) ] $
        \(ex, sl) →
            it ("runs exhaustion " <> show ex <> " with sleep " <> show sl
                <> ": only a Simulated system stores or ticks its pool") $
                withScene [ (poolUnit 1 1.0 ex sl) { usActivity = "walking" } ] $
                    \(env, ls) → do
                        runSeconds ls 60 0.1
                        inst ← readInstance env 1
                        HM.member "exhaustion" (uiStats inst)
                            `shouldBe` (ex ≡ PhysioSimulated)
                        HM.member "sleep_pressure" (uiStats inst)
                            `shouldBe` (sl ≡ PhysioSimulated)
                        -- the Simulated ones moved (walking drains exhaustion,
                        -- being awake drains sleep); the Nominal ones read healthy
                        v1 ← luaNum ls "unit.readPhysio(1, 'exhaustion')"
                        v2 ← luaNum ls "unit.readPhysio(1, 'sleep_pressure')"
                        if ex ≡ PhysioSimulated
                            then v1 `shouldSatisfy` (< 9.5)
                            else near 1e-9 10 v1
                        if sl ≡ PhysioSimulated
                            then v2 `shouldSatisfy` (< 9.9)
                            else near 1e-9 10 v2
                        fatalCount env `shouldReturn` 0
                        scriptErrors ls `shouldReturn` ""

    it "keeps the shipped squirrel's sleep Nominal with no pool, and a \
       \squirrel pinned Simulated still drains the old way" $ do
        let squirrel = (unitSpec 1 "red_squirrel" (Just "simple"))
                { usBody = squirrelBody }
        withScene [squirrel] $ \(env, ls) → do
            runSeconds ls 360 0.1
            stored env 1 "sleep_pressure" `shouldReturn` Nothing
            readOwned ls 1 "sleep_pressure" `shouldReturn` "10|nominal"
            stored env 1 "exhaustion" >>= \v → v `shouldSatisfy` (/= Nothing)
        withScene [squirrel { usOverrides = [("sleep", PhysioSimulated)] }] $
            \(env, ls) → do
                -- drain-only: max / 3600 per second, no regen entry
                start ← stored env 1 "sleep_pressure"
                start `shouldBe` Just 10
                runSeconds ls 360 0.1
                luaNum ls "unit.getStat(1, 'sleep_pressure')" >>= near 2e-3 9.0
                scriptErrors ls `shouldReturn` ""
