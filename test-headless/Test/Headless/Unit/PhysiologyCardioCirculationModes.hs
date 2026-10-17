{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The "physiology cardio and circulation modes" gate (#2836, S9 of the
--   physiology stat-modes epic #2827): cardio as the eighth registry system
--   and circulation as its on-demand derived key, run through the SHIPPED
--   registry, the SHIPPED Lua (@scripts\/unit_resources.lua@ and everything
--   it ticks: @scripts\/cardio.lua@, @scripts\/thermo.lua@,
--   @scripts\/brain.lua@, the hypoxia meter) and the REAL registered
--   @unit.*@ API over a live headless engine, with the real save codec for
--   persistence.
--
--   One module per contract area:
--
--     * this module: the declarations, Nominal neutrality (I1), live
--       behavior (I3), the interface equivalences (I2), the lung path that
--       still suffocates (T-S9-1), the live circulation that still drives
--       frostbite (T-S9-3) and the bodyless Unsupported cardio;
--     * "Test.Headless.Unit.PhysiologyCardioCirculationModes.Rule": the
--       on-demand rule against an independent reference (T-S9-2), its
--       coherence with no tick, and the unmoded blood read;
--     * "Test.Headless.Unit.PhysiologyCardioCirculationModes.Persistence":
--       transitions (I5) and persistence (I6), through the real codec and
--       the load gate.
--
--   Nothing is rolled: every input is a literal. Run just this gate:
--   @cabal test synarchy-test-headless --test-options='--match
--   "physiology cardio and circulation modes"'@.
module Test.Headless.Unit.PhysiologyCardioCirculationModes (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Engine.Core.State (EngineEnv)
import Engine.Scripting.Lua.Types (LuaBackendState)
import qualified Test.Headless.Unit.PhysiologyCardioCirculationModes.Persistence as Persistence
import qualified Test.Headless.Unit.PhysiologyCardioCirculationModes.Rule as Rule
import Test.Headless.Unit.PhysiologyFixture (readShippedPhysiologyRegistry)
import Test.Headless.Unit.PhysiologyModes.Harness
import Unit.Command.Types (UnitCommand(..))
import Unit.Physiology
import Unit.Physiology.Types
import Unit.Types

spec ∷ Spec
spec = describe "physiology cardio and circulation modes" $ do
    declarationSpec
    neutralitySpec
    liveSpec
    orchestrationSpec
    interfaceSpec
    hypoxiaSpec
    frostbiteSpec
    unsupportedSpec
    Rule.spec
    Persistence.spec

-----------------------------------------------------------------------
-- Fixtures
-----------------------------------------------------------------------

-- | An acolyte-config unit whose ONLY simulated system is cardio (the
--   @exempt@ tier keeps cardio live and everything else Nominal), so
--   nothing else drains, shivers or kills. @mode@ is the UNIT'S cardio pin,
--   not the definition's default: units of one scene share the definition
--   named @acolyte@.
cardioUnit ∷ Int → PhysioMode → UnitSpec
cardioUnit uid mode = (unitSpec uid "acolyte" (Just "exempt"))
    { usOverrides = [("cardio", mode)] }

-- | How many kills or collapses the units' queue holds (and empty it).
fatalCount ∷ EngineEnv → IO Int
fatalCount env = length ∘ filter fatal <$> drainCommands env
  where
    fatal (UnitKill _)     = True
    fatal (UnitCollapse _) = True
    fatal _                = False

-- | A unit's value of a key through the read verb, as a number.
readNum ∷ LuaBackendState → Int → Text → IO Double
readNum ls uid key = luaNum ls ("unit.readPhysio(" <> tshow uid <> ", '" <> key <> "')")

clampD ∷ Double → Double → Double → Double
clampD lo hi = max lo ∘ min hi

-----------------------------------------------------------------------
-- Declarations
-----------------------------------------------------------------------

declarationSpec ∷ Spec
declarationSpec = describe "the cardio system and its declarations" $ do
    it "is one registry system requiring a body, owning heart rate and blood \
       \oxygen, deriving circulation live, with no meters and no outgoing \
       \contribution, and an oxygen-path effect that is absent while Nominal" $ do
        reg ← readShippedPhysiologyRegistry
        case registrySystem reg "cardio" of
            Nothing → expectationFailure "no cardio system"
            Just sys → do
                psRequires sys `shouldBe` ["body"]
                [ (okKey o, okHealthy o) | o ← psOwns sys ] `shouldBe`
                    [ ("heart_rate", HealthyConst (QConst "RESTING_HR"))
                    , ("blood_oxygen", HealthyConst (QLit 1.0)) ]
                [ (dkKey d, dkRule d, dkHealthy d) | d ← psDerived sys ] `shouldBe`
                    [("circulation", DerivedCirculation, DerivedLive)]
                psMeters sys `shouldBe` []
                psContributes sys `shouldBe` []
                [ (ieName e, ieSubstitute e) | e ← psProvides sys ] `shouldBe`
                    [ ("heart_rate", SubValue (QConst "RESTING_HR"))
                    , ("blood_oxygen", SubValue (QLit 1.0))
                    , ("circulation", SubLive) ]
                [ (ieName e, ieSubstitute e) | e ← psEffects sys ] `shouldBe`
                    [("oxygen_path_hypoxia", SubNone)]

    it "leaves the hypoxia meter and the lung path to injury physiology: no \
       \system owns the meter, and no key of the oxygen path is a meter" $ do
        reg ← readShippedPhysiologyRegistry
        classifyKey reg "hypoxia" `shouldBe` Nothing
        forM_ (prSystems reg) $ \s → psMeters s `shouldNotContain` ["hypoxia"]

    it "keeps cardio.RESTING_HR equal to the registry constant, and leaves no \
       \second circulation formula behind in the Lua" $ do
        reg ← readShippedPhysiologyRegistry
        withScene [] $ \(_, ls) → do
            let want = maybe 0 realToFrac (Map.lookup "RESTING_HR" (prConstants reg))
            want `shouldBe` (70 ∷ Double)
            luaNum ls "require('scripts.cardio').RESTING_HR" >>= near 1e-9 want
            luaNum ls "engine.getPhysiologyRegistry().constants.RESTING_HR"
                >>= near 1e-9 want
        src ← TIO.readFile "scripts/circulation.lua"
        -- the divisor and every other constant of the formula live in the
        -- Haskell rule only
        forM_ ["/ 70", "0.45", "FAT_PENALTY", "VASO_", "HR_FACTOR"] $ \needle →
            (needle, needle `T.isInfixOf` src) `shouldBe` (needle, False)

-----------------------------------------------------------------------
-- Nominal neutrality (I1)
-----------------------------------------------------------------------

neutralitySpec ∷ Spec
neutralitySpec = describe "Nominal neutrality (I1)" $ do
    forM_ [("arctic", -20, 0.4), ("desert", 45, 0.1)] $ \(label, temp, hum) →
        it ("keeps both owned keys absent, the healthy reads stable, the raw \
            \writes refused and cardio unticked over repeated passes in the "
            <> label) $
            withScene [cardioUnit 1 PhysioNominal] $ \(env, ls) → do
                climate ls temp hum
                _ ← luaText ls "_G.__rand = 0; local r = math.random; \
                               \math.random = function(...) \
                               \__rand = __rand + 1; return r(...) end; \
                               \_G.__ticks = 0; local cardio = require('scripts.cardio'); \
                               \local t = cardio.tick; \
                               \cardio.tick = function(...) __ticks = __ticks + 1; \
                               \return t(...) end; return true"
                forM_ [1 ∷ Int .. 3] $ \_ → do
                    runSeconds ls 60 0.1
                    luaText ls "return table.concat({ \
                               \tostring(unit.setStat(1, 'heart_rate', 2)), \
                               \tostring(unit.setStat(1, 'blood_oxygen', 0.2)), \
                               \tostring(unit.setStat(1, 'circulation', 0.1)) }, ',')"
                        `shouldReturn` "false,false,false"
                    inst ← readInstance env 1
                    forM_ ["heart_rate", "blood_oxygen", "circulation"] $ \k →
                        (k, HM.member k (uiStats inst)) `shouldBe` (k, False)
                    readOwned ls 1 "heart_rate" `shouldReturn` "70|nominal"
                    readOwned ls 1 "blood_oxygen" `shouldReturn` "1|nominal"
                    -- an uninjured unit's circulation is 1 -- but it is the
                    -- live rule, which the injury cases below move
                    readOwned ls 1 "circulation" `shouldReturn` "1|nominal"
                    -- no owned oxygen-path effect
                    luaNum ls "unit.getStat(1, 'hypoxia') or 0" >>= near 1e-9 0
                    luaNum ls "require('scripts.cardio').hypoxiaFailure(1)"
                        >>= near 1e-9 0
                luaText ls "return tostring(__ticks)" `shouldReturn` "0"
                fatalCount env `shouldReturn` 0
                scriptErrors ls `shouldReturn` ""
                _ ← luaText ls "__rand = 0; return true"
                before ← readInstance env 1
                _ ← luaText ls "for _ = 1, 200 do unit.readPhysio(1, 'heart_rate'); \
                               \unit.readPhysio(1, 'blood_oxygen'); \
                               \unit.readPhysioBase(1, 'circulation'); \
                               \__physiology.get(1, 'circulation') end; return true"
                after ← readInstance env 1
                uiStats after `shouldBe` uiStats before
                uiPhysioOverrides after `shouldBe` uiPhysioOverrides before
                luaText ls "return __rand" `shouldReturn` "0"

    it "still moves a Nominal cardio's circulation with blood loss, sepsis, \
       \fitness and cold, and only the heart-rate factor reads healthy (SD-21)" $
        withScene
            [ (cardioUnit 1 PhysioNominal) { usBlood = Just (0.5 * 5.25) }
            , (cardioUnit 2 PhysioNominal)
                { usStats = [("sepsis", 0.6)] }
            , (cardioUnit 3 PhysioNominal)
                { usStats = [("constitution", 0.2)] }
            , (cardioUnit 4 PhysioNominal) { usBlood = Nothing }
            , (cardioUnit 5 PhysioSimulated)
                { usBlood = Just (0.5 * 5.25), usStats = [("heart_rate", 35)] } ] $
            \(_, ls) → do
                readNum ls 4 "circulation" >>= near 1e-6 1.0
                readNum ls 1 "circulation" >>= near 1e-5 (1.1 * 0.5)
                readNum ls 2 "circulation" >>= near 1e-5 (1 - 0.5 * 0.6)
                readNum ls 3 "circulation" >>= near 1e-5 ((0.5 + 0.5 * 0.2))
                -- the same blood loss with a slow Simulated heart reads lower
                -- by exactly the heart-rate factor 35 / 70
                readNum ls 5 "circulation" >>= near 1e-5 (1.1 * 0.5 * 0.5)

-----------------------------------------------------------------------
-- Live behavior (I3)
-----------------------------------------------------------------------

-- | The old cardio integration, one tick, in Double with the lag removed:
--   the heart rate first, then blood oxygen from the circulation of THAT
--   heart rate (blood fraction 0.4 of a 70 kg body, idle, core 37, no
--   wounds).
cardioStep ∷ Double → (Double, Double) → (Double, Double)
cardioStep dt (hr, o2) = (hr', o2')
  where
    bloodFrac = 0.4
    base = 70 + (1 - bloodFrac) * 70
    target = clampD 30 200 (if o2 < 0.9 then base + (0.9 - o2) * 80 else base)
    hr' = hr + (target - hr) * clampD 0 1 (0.30 * dt)
    circ = clampD 0.05 1.0
        (clampD 0.1 1.0 (bloodFrac * 1.1) * 1.0 * 1.0 * 1.0
         * clampD 0.5 1.0 (hr' / 70))
    delivery = 0.6 + 0.4 * clampD 0 1 (circ * clampD 0.4 1.0 (hr' / 70))
    o2target = clampD 0 1 delivery
    o2' = clampD 0 1 (o2 + (o2target - o2) * clampD 0 1 (0.15 * dt))

liveSpec ∷ Spec
liveSpec = describe "live behavior (I3)" $ do
    forM_ [("idle", "idle", 70), ("walking", "walking", 95), ("running", "running", 125)] $
        \(label, activity, steady) →
            it ("integrates finite, clamped heart-rate and oxygen trajectories \
                \over 600 tick-seconds while " <> label <> ", settling at the \
                \unchanged target") $
                withScene [(cardioUnit 1 PhysioSimulated) { usActivity = activity }] $
                    \(_, ls) → do
                        out ← track ls 1 600 0.1
                                  ["heart_rate", "blood_oxygen", "circulation"]
                        forM_ (HM.toList out) $ \(k, e) → do
                            (k, exNaN e) `shouldBe` (k, 0)
                        let ex k = HM.lookupDefault (Extremes 0 0 0) k out
                        exMin (ex "heart_rate") `shouldSatisfy` (≥ 69.99)
                        exMax (ex "heart_rate") `shouldSatisfy` (≤ steady + 0.01)
                        exMax (ex "heart_rate") `shouldSatisfy` (> steady - 0.5)
                        exMin (ex "blood_oxygen") `shouldSatisfy` (> 0.9999)
                        exMax (ex "blood_oxygen") `shouldSatisfy` (≤ 1.0)
                        exMin (ex "circulation") `shouldSatisfy` (> 0.9999)
                        scriptErrors ls `shouldReturn` ""

    it "follows the unchanged integration equations under blood loss, with the \
       \one-tick oxygen-delivery lag removed (delivery reads this tick's \
       \heart rate)" $
        withScene [(cardioUnit 1 PhysioSimulated) { usBlood = Just (0.4 * 5.25) }] $
            \(_, ls) → do
                let steps n = iterate (cardioStep 0.1) (70, 1) !! n
                    check n = do
                        let (hr, o2) = steps n
                        readNum ls 1 "heart_rate" >>= near 2e-3 hr
                        readNum ls 1 "blood_oxygen" >>= near 2e-3 o2
                runSeconds ls 0.1 0.1
                check 1
                runSeconds ls 9.9 0.1
                check 100
                runSeconds ls 50 0.1
                check 600
                runSeconds ls 240 0.1
                check 3000
                -- it genuinely moved: this is not comparing constants
                readNum ls 1 "heart_rate" >>= \hr → hr `shouldSatisfy` (> 100)
                readNum ls 1 "blood_oxygen" >>= \o → o `shouldSatisfy` (< 0.85)
                scriptErrors ls `shouldReturn` ""

-----------------------------------------------------------------------
-- Orchestration: cardio ticks first, and only while Simulated
-----------------------------------------------------------------------

orchestrationSpec ∷ Spec
orchestrationSpec = describe "the tick order and its gate" $ do
    it "ticks cardio before thermo in a pass, for a Simulated cardio only; a \
       \Nominal cardio and a bodyless unit never tick it" $
        withScene [ (unitSpec 1 "acolyte" (Just "rich"))
                  , (unitSpec 2 "acolyte" (Just "rich"))
                        { usOverrides = [("cardio", PhysioNominal)] }
                  , unitSpec 3 "white_tailed_deer" Nothing ] $ \(_, ls) → do
            _ ← luaText ls $ T.unlines
                [ "_G.__order = {}"
                , "local cardio = require('scripts.cardio')"
                , "local thermo = require('scripts.thermo')"
                , "local ct, tt = cardio.tick, thermo.tick"
                , "cardio.tick = function(uid, ...)"
                , "  __order[#__order + 1] = 'c' .. uid; return ct(uid, ...) end"
                , "thermo.tick = function(uid, ...)"
                , "  __order[#__order + 1] = 't' .. uid; return tt(uid, ...) end"
                , "return true" ]
            runSeconds ls 0.1 0.1
            order ← luaText ls "return table.concat(__order, ',')"
            let calls = T.splitOn "," order
            -- unit 1: cardio, then thermo; unit 2: thermo only (its cardio is
            -- Nominal); unit 3: neither (every system Unsupported)
            [ c | c ← calls, c `elem` ["c1", "t1"] ] `shouldBe` ["c1", "t1"]
            calls `shouldNotContain` ["c2"]
            calls `shouldContain` ["t2"]
            calls `shouldNotContain` ["c3"]
            calls `shouldNotContain` ["t3"]

-----------------------------------------------------------------------
-- Interface equivalence (I2)
-----------------------------------------------------------------------

interfaceSpec ∷ Spec
interfaceSpec = describe "interface equivalence (I2)" $ do
    it "cardio -> brain: a Nominal cardio and a Simulated one held at the \
       \healthy interface (heart rate 70, oxygen 1.0) drive the brain \
       \identically over 600 ticks, within 1e-6" $
        withScene [cardioUnit 1 PhysioNominal, cardioUnit 2 PhysioSimulated] $
            \(_, ls) → do
                out ← luaText ls $ T.unlines
                    [ "local worst, finite = 0, true"
                    , "local keys = { 'consciousness', 'concentration', 'state_of_mind', 'mood' }"
                    , "for _ = 1, 600 do"
                    , "  __resources.update(0.1)"
                    -- the stub: unit 2's interface is held at the healthy values
                    , "  unit.setStat(2, 'heart_rate', 70); unit.setStat(2, 'blood_oxygen', 1)"
                    , "  for _, k in ipairs(keys) do"
                    -- the brain's values are derived (#2838): the mode-aware read
                    , "    local a, b = unit.readPhysio(1, k), unit.readPhysio(2, k)"
                    , "    if a ~= a or b ~= b then finite = false end"
                    , "    if a ~= nil and b ~= nil then"
                    , "      worst = math.max(worst, math.abs(a - b)) end"
                    , "  end"
                    , "end"
                    , "return tostring(finite) .. '|' .. string.format('%.3g', worst)" ]
                let (finite, rest) = T.breakOn "|" out
                finite `shouldBe` "true"
                case reads (T.unpack (T.drop 1 rest)) of
                    [(v, "")] → (v ∷ Double) `shouldSatisfy` (< 1e-6)
                    _ → expectationFailure ("not a number: " <> T.unpack rest)
                -- ... and the brain was genuinely driven by the interface: the
                -- same units with oxygen at 0.4 are unconscious
                -- (derived on demand: no refresh to run)
                _ ← luaText ls "unit.setStat(2, 'blood_oxygen', 0.4); return true"
                luaNum ls "unit.readPhysio(2, 'consciousness')"
                    >>= \c → c `shouldSatisfy` (< 0.1)
                luaNum ls "unit.readPhysio(1, 'consciousness')"
                    >>= \c → c `shouldSatisfy` (> 0.99)

    it "blood -> cardio: with blood at its maximum (the future Nominal blood \
       \fraction) a Simulated cardio tracks the healthy heart rate and oxygen \
       \over 600 ticks, within 1e-6, while a bled unit does not" $
        withScene
            [ cardioUnit 1 PhysioSimulated
            , (cardioUnit 2 PhysioSimulated) { usBlood = Just (0.7 * 5.25) } ] $
            \(_, ls) → do
                out ← track ls 1 60 0.1 ["heart_rate", "blood_oxygen"]
                forM_ [("heart_rate", 70), ("blood_oxygen", 1)] $ \(k, want) → do
                    let e = HM.lookupDefault (Extremes 0 0 0) k out
                    exMin e `shouldSatisfy` (\x → abs (x - want) < 1e-6)
                    exMax e `shouldSatisfy` (\x → abs (x - want) < 1e-6)
                readNum ls 2 "heart_rate" >>= \hr → hr `shouldSatisfy` (> 80)

    it "reads the blood fraction against the BASE body mass: a body-mass \
       \modifier does not move cardio's input" $
        withScene
            [ (cardioUnit 1 PhysioSimulated)
                { usModifiers = [("body_mass", [StatModifier 0 "spec" Nothing 1.0])]
                , usBlood = Just (0.5 * 5.25) } ] $ \(_, ls) → do
            runSeconds ls 0.1 0.1
            -- fraction 0.5: target 105, one 0.03 step from 70 is 71.05 (a
            -- doubled maximum would have read 0.25 and stepped to 71.575)
            readNum ls 1 "heart_rate" >>= near 1e-3 71.05

-----------------------------------------------------------------------
-- T-S9-1: a Nominal cardio does not stop the lung path killing
-----------------------------------------------------------------------

punctureLung ∷ Wound
punctureLung = Wound
    { woundPart = "lungs", woundKind = "internal", woundSeverity = 0.95
    , woundAt = 0, woundBandage = 1.0, woundClot = 0.0, woundHeal = 0.0
    , woundDressing = "", woundInfection = 0.0, woundClean = False
    , woundInfectionType = "", woundNecrosis = 0.0 }

hypoxiaSpec ∷ Spec
hypoxiaSpec = describe "the lung path still suffocates under a Nominal cardio (T-S9-1)" $ do
    it "fills the unowned hypoxia meter from the real lung driver alone and \
       \kills the punctured unit, while an uninjured Nominal control with \
       \the same blood does not die" $
        withScene
            [ (cardioUnit 1 PhysioNominal) { usWounds = [punctureLung] }
            , cardioUnit 2 PhysioNominal ] $ \(env, ls) → do
            -- the oxygen path is zero for both: blood oxygen is the healthy
            -- 1.0, so the whole driver is the lung path
            luaNum ls "require('scripts.cardio').hypoxiaFailure(1)" >>= near 1e-6 0.95
            luaNum ls "require('scripts.cardio').hypoxiaFailure(2)" >>= near 1e-9 0
            luaNum ls "require('scripts.injuries').pulmonaryFailure(1)" >>= near 1e-6 0.95
            -- blood is full and no other death source is in play
            luaNum ls "unit.readBloodView(1).fraction" >>= near 1e-9 1
            runSeconds ls 400 0.5
            readOwned ls 1 "blood_oxygen" `shouldReturn` "1|nominal"
            readOwned ls 2 "blood_oxygen" `shouldReturn` "1|nominal"
            luaNum ls "unit.getStat(1, 'hypoxia')" >>= near 1e-9 1
            luaNum ls "unit.getStat(2, 'hypoxia') or 0" >>= near 1e-9 0
            forM_ ["neuro", "shock", "organ", "sepsis", "hypothermia", "hyperthermia"] $ \m →
                luaNum ls ("unit.getStat(1, '" <> m <> "') or 0") >>= near 1e-9 0
            dead ← killed env
            dead `shouldBe` [UnitId 1]
            -- the death alert names the lung/suffocation cause
            evs ← luaText ls "local o = {}; for _, e in ipairs(__events) do \
                             \if e:find('^survival_critical') then \
                             \o[#o + 1] = e end end; return table.concat(o, '|')"
            T.isInfixOf "died of" evs `shouldBe` True
            cause ← luaText ls "return tostring(require('scripts.injuries').deathCause(1) or 'suffocation')"
            T.isInfixOf cause evs `shouldBe` True
            scriptErrors ls `shouldReturn` ""

    it "never scrubs or disables the whole hypoxia meter as a cardio effect: \
       \a stored meter survives the cardio going Nominal and back" $
        withScene [(cardioUnit 1 PhysioSimulated) { usStats = [("hypoxia", 0.3)] }] $
            \(_, ls) → do
                luaText ls "return tostring(select(2, __physiology.setMode(1, 'cardio', 'nominal')))"
                    `shouldReturn` "true"
                luaNum ls "unit.getStat(1, 'hypoxia')" >>= near 1e-6 0.3
                luaText ls "return tostring(select(2, __physiology.setMode(1, 'cardio', 'simulated')))"
                    `shouldReturn` "true"
                luaNum ls "unit.getStat(1, 'hypoxia')" >>= near 1e-6 0.3

-----------------------------------------------------------------------
-- T-S9-3: frostbite still follows blood loss under a Nominal cardio
-----------------------------------------------------------------------

frostbiteSpec ∷ Spec
frostbiteSpec = describe "circulation still drives frostbite under a Nominal cardio (T-S9-3)" $ do
    it "lowers the derived circulation and raises the real frostbite drive \
       \for heavy blood loss versus an identical full-blood control, with \
       \core, constitution, fat and sepsis pinned" $
        withScene
            [ frostUnit 1 Nothing
            , frostUnit 2 (Just (0.25 * 5.25)) ] $ \(_, ls) → do
            climate ls (-20) 0.4
            c1 ← readNum ls 1 "circulation"
            c2 ← readNum ls 2 "circulation"
            near 1e-5 1.0 c1
            near 1e-5 (1.1 * 0.25) c2
            readOwned ls 2 "heart_rate" `shouldReturn` "70|nominal"
            -- the real thermo tick, with the core re-pinned before each pass
            -- and the engine's frostbite verb recording what it was asked for
            out ← luaText ls $ T.unlines
                [ "_G.__frost = {}"
                , "unit.frostbite = function(u, part, nec)"
                , "  __frost[u] = (__frost[u] or 0) + nec end"
                , "for _ = 1, 50 do"
                , "  unit.setStat(1, 'core_temp', 37); unit.setStat(2, 'core_temp', 37)"
                , "  __resources.update(0.1)"
                , "end"
                , "return string.format('%.9g|%.9g', __frost[1] or 0, __frost[2] or 0)" ]
            let (a, b) = T.breakOn "|" out
                perTick circ = 4 * 0.015 * 0.1 * clampD 0 1 (20 / 25)
                                 * clampD 0 1 (1.05 - circ)
                parse t = case reads (T.unpack t) of
                    [(v, "")] → v ∷ Double
                    _ → -1
            parse a `shouldSatisfy` (\x → abs (x - 50 * perTick c1) < 1e-7)
            parse (T.drop 1 b) `shouldSatisfy` (\x → abs (x - 50 * perTick c2) < 1e-7)
            parse (T.drop 1 b) `shouldSatisfy` (> 10 * parse a)
            scriptErrors ls `shouldReturn` ""

    it "reads the thermo substitute 37 under a Nominal thermo, where a \
       \Simulated thermo's cold stored core lowers the same unit's \
       \circulation" $ do
        let hurt = Just (0.25 * 5.25)
        withScene
            [ (frostUnit 1 hurt) { usOverrides =
                  [("cardio", PhysioNominal), ("thermo", PhysioNominal)] }
            , (frostUnit 2 hurt) { usStats = [("core_temp", 31)] } ] $ \(_, ls) → do
            readOwned ls 1 "core_temp" `shouldReturn` "37|nominal"
            readNum ls 1 "circulation" >>= near 1e-5 (1.1 * 0.25)
            -- 31 degrees: five under the vasoconstriction onset, a factor of 0.7
            readNum ls 2 "circulation" >>= near 1e-5 (1.1 * 0.25 * 0.7)

-- | A unit with a Simulated thermo and a Nominal cardio, every other system
--   Nominal, at the given blood volume (litres; 'Nothing' = full).
frostUnit ∷ Int → Maybe Float → UnitSpec
frostUnit uid blood = (unitSpec uid "acolyte" (Just "exempt"))
    { usOverrides = [("thermo", PhysioSimulated), ("cardio", PhysioNominal)]
    , usBlood = blood }

-----------------------------------------------------------------------
-- A bodyless unit: cardio is Unsupported
-----------------------------------------------------------------------

unsupportedSpec ∷ Spec
unsupportedSpec = describe "a bodyless unit's cardio is Unsupported (an intended change)" $ do
    it "reads nil for heart rate, blood oxygen and circulation, stores and \
       \integrates nothing, and every consumer takes the substitute" $
        withScene [(unitSpec 1 "white_tailed_deer" Nothing) { usBlood = Just 0 }] $
            \(env, ls) → do
                forM_ ["heart_rate", "blood_oxygen", "circulation"] $ \k →
                    readOwned ls 1 k `shouldReturn` "nil|nil"
                luaText ls "return tostring(unit.getPhysioMode(1, 'cardio'))"
                    `shouldReturn` "nil"
                -- the bodyless 0 L seed that used to integrate compensatory
                -- tachycardia and depressed oxygen into a consumer-visible
                -- state now touches nothing
                runSeconds ls 60 0.1
                inst ← readInstance env 1
                forM_ ["heart_rate", "blood_oxygen", "circulation"] $ \k →
                    (k, HM.member k (uiStats inst)) `shouldBe` (k, False)
                luaNum ls "require('scripts.cardio').heartRate(1)" >>= near 1e-9 70
                luaNum ls "require('scripts.cardio').bloodOxygen(1)" >>= near 1e-9 1
                luaNum ls "require('scripts.cardio').hypoxiaFailure(1)" >>= near 1e-9 0
                luaText ls "return tostring(require('scripts.circulation').get(1))"
                    `shouldReturn` "nil"
                luaNum ls "unit.getStat(1, 'hypoxia') or 0" >>= near 1e-9 0
                fatalCount env `shouldReturn` 0
                scriptErrors ls `shouldReturn` ""
