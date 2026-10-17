{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The brain's pure rules and their interfaces (#2838, S11a): T-S11-2 and
--   I2. The rules ('Unit.Physiology.Mind') are compared against independent
--   pinned equation cases and against a verbatim copy of the legacy
--   @scripts/brain.lua@ equations; euphoria's lift appears exactly once; a
--   change to any input is read at once with no tick, and a read stores
--   nothing and draws nothing; a non-finite input is contained.
module Test.Headless.Unit.PhysiologyBrainMental.Rules (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Engine.Core.State (EngineEnv)
import Engine.Scripting.Lua.Types (LuaBackendState)
import Combat.Resolution.Common (mentalMultiplier)
import Test.Headless.Unit.PhysiologyModes.Harness
import Unit.Physiology.Mind
import Unit.Physiology.Types
import Unit.Types

spec ∷ Spec
spec = describe "the brain's pure rules (T-S11-2) and interfaces (I2)" $ do
    pinnedSpec
    legacySpec
    euphoriaSpec
    freshnessSpec
    containmentSpec
    interfaceSpec

near' ∷ Float → Float → Float → Expectation
near' tol want got = abs (got - want) `shouldSatisfy` (< tol)

-----------------------------------------------------------------------
-- Independent pinned equation cases
-----------------------------------------------------------------------

pinnedSpec ∷ Spec
pinnedSpec = describe "independent pinned equation cases" $ do
    it "consciousness is the worst of the temperature, oxygen and salt factors" $
        forM_ cases $ \((core, o2, salt), want) →
            near' 1e-5 want (consciousnessRule (ConsciousnessInputs core o2 salt))
  where
    cases =
        [ ((37, 1.0, 1.0), 1.0)
        , ((32, 1.0, 1.0), 0.5)            -- (32 - 30) / 4
        , ((30, 1.0, 1.0), 0.0)
        , ((25, 1.0, 1.0), 0.0)            -- clamped
        , ((41, 1.0, 1.0), 0.4)            -- (42 - 41) / 2.5
        , ((42, 1.0, 1.0), 0.0)
        , ((34, 1.0, 1.0), 1.0)            -- the band edges are healthy
        , ((39.5, 1.0, 1.0), 1.0)
        , ((37, 0.65, 1.0), 0.5)           -- (0.65 - 0.5) / 0.3
        , ((37, 0.5, 1.0), 0.0)
        , ((37, 0.8, 1.0), 1.0)
        , ((37, 1.0, 1.425), 0.5)          -- 1 - (0.425 - 0.25) / 0.35
        , ((37, 1.0, 0.575), 0.5)          -- the same deviation, below
        , ((37, 1.0, 1.25), 1.0)           -- inside the dead band
        , ((32, 0.65, 1.0), 0.5)
        , ((32, 0.6, 1.0), 1 / 3) ]        -- the minimum wins

-----------------------------------------------------------------------
-- The legacy Lua equations, verbatim
-----------------------------------------------------------------------

-- | The original @scripts/brain.lua@ equations, copied as they were before
--   #2838 removed them, so the Haskell rules are compared against the code
--   they replaced rather than against themselves.
legacyLua ∷ Text
legacyLua = T.unlines
    [ "local function clamp(x, a, b) return math.max(a, math.min(b, x)) end"
    , "function _G.__legacyCons(core, o2, conc)"
    , "  local tf"
    , "  if core < 34.0 then tf = clamp((core - 30.0) / 4.0, 0, 1)"
    , "  elseif core > 39.5 then tf = clamp((42.0 - core) / 2.5, 0, 1)"
    , "  else tf = 1.0 end"
    , "  local of = clamp((o2 - 0.5) / 0.3, 0, 1)"
    , "  local dev = math.abs(conc - 1.0)"
    , "  local sf = clamp(1.0 - (dev - 0.25) / 0.35, 0, 1)"
    , "  return math.min(tf, of, sf)"
    , "end"
    , "function _G.__legacyConc(c, pain, stam, caf)"
    , "  local staminaFactor = 0.4 + (1.0 - 0.4) * stam"
    , "  local base = c * (1.0 - 0.6 * pain) * staminaFactor"
    , "  return clamp(base + 0.15 * caf, 0, 1)"
    , "end"
    , "function _G.__legacySom(c, mood, ep)"
    , "  return math.min(c, clamp(mood - 0.5 * ep, 0, 1))"
    , "end"
    , "return true" ]

-- | A unit whose thermo, salt and cardio are Simulated (the three consciousness
--   inputs are stored), hydration Nominal so the salt concentration is
--   exactly salt over its maximum, and the brain's own systems Simulated.
inputsUnit ∷ UnitSpec
inputsUnit = (unitSpec 1 "acolyte" (Just "exempt"))
    { usOverrides = [("thermo", PhysioSimulated), ("salt", PhysioSimulated)] }

setStatOf ∷ EngineEnv → Int → Text → Float → IO ()
setStatOf env uid k v = modifyInstance env uid $ \i →
    i { uiStats = HM.insert k v (uiStats i) }

legacySpec ∷ Spec
legacySpec = describe "the legacy brain equations" $ do
    it "equal the derived consciousness over a grid of core temperature, oxygen \
       \and salt concentration" $
        withScene [inputsUnit] $ \(env, ls) → do
            _ ← luaText ls legacyLua
            forM_ [28, 30, 32, 33.9, 34, 36, 37, 39.5, 40, 41.5, 42, 44] $ \core →
              forM_ [0.3, 0.5, 0.6, 0.7, 0.8, 1.0] $ \o2 →
                forM_ [0.4, 0.6, 0.8, 1.0, 1.2, 1.3, 1.5] $ \conc → do
                    setStatOf env 1 "core_temp" core
                    setStatOf env 1 "blood_oxygen" o2
                    setStatOf env 1 "salt" (70 * conc)
                    want ← luaNum ls ("__legacyCons(" <> tshow core <> ", "
                                      <> tshow o2 <> ", " <> tshow conc <> ")")
                    got ← luaNum ls "unit.readPhysio(1, 'consciousness')"
                    abs (got - want) `shouldSatisfy` (< 1e-4)

    it "equal the derived concentration and state of mind over a grid of the \
       \remaining inputs" $
        withScene [inputsUnit] $ \(env, ls) → do
            _ ← luaText ls legacyLua
            forM_ [0, 0.25, 0.5, 1.0] $ \caf →
              forM_ [0.2, 0.5, 0.9, 1.0] $ \mood →
                forM_ [0, 0.4, 1.0] $ \ep → do
                    setStatOf env 1 "caffeine" caf
                    setStatOf env 1 "mood" mood
                    setStatOf env 1 "emotional_pain" ep
                    cons ← luaNum ls "unit.readPhysio(1, 'consciousness')"
                    -- healthy unit: pain 0, stamina Nominal (fraction 1.0)
                    wantC ← luaNum ls ("__legacyConc(" <> tshow cons <> ", 0, 1.0, "
                                       <> tshow caf <> ")")
                    gotC ← luaNum ls "unit.readPhysio(1, 'concentration')"
                    abs (gotC - wantC) `shouldSatisfy` (< 1e-5)
                    wantS ← luaNum ls ("__legacySom(" <> tshow cons <> ", "
                                       <> tshow mood <> ", " <> tshow ep <> ")")
                    gotS ← luaNum ls "unit.readPhysio(1, 'state_of_mind')"
                    abs (gotS - wantS) `shouldSatisfy` (< 1e-5)

    it "price the stamina fraction and the injury pain exactly as the Lua did" $
        withScene [ (unitSpec 1 "acolyte" (Just "rich")) { usStats = [("stamina", 25)] } ] $
            \(_, ls) → do
                _ ← luaText ls legacyLua
                -- max_stamina is the body's 100, so the stamina fraction is 0.25
                want ← luaNum ls "__legacyConc(1.0, 0, 0.25, 0)"
                got ← luaNum ls "unit.readPhysio(1, 'concentration')"
                abs (got - want) `shouldSatisfy` (< 1e-5)

-----------------------------------------------------------------------
-- Euphoria
-----------------------------------------------------------------------

euphoriaSpec ∷ Spec
euphoriaSpec = describe "euphoria" $ do
    it "lifts the derived concentration by +0.10 exactly once, and the lifted \
       \value is capped at 1" $
        withScene [ (unitSpec 1 "acolyte" (Just "rich")) { usStats = [("stamina", 20)] } ] $
            \(env, ls) → do
                -- stamina fraction 0.2 of the body's 100: 1 x 1 x (0.4 + 0.6 x 0.2)
                stable ← luaNum ls "unit.readPhysio(1, 'concentration')"
                near' 1e-5 0.52 (realToFrac stable)
                setStatOf env 1 "mental_state" 3
                euph ← luaNum ls "unit.readPhysio(1, 'concentration')"
                near' 1e-5 0.1 (realToFrac (euph - stable))
                -- exactly once: a second read in the same state is identical
                luaNum ls "unit.readPhysio(1, 'concentration')" >>= (`shouldBe` euph)
                -- the pure rule agrees, and caps
                near' 1e-6 0.1 (concentrationRule 0.5 0 1 0 True - concentrationRule 0.5 0 1 0 False)
                concentrationRule 0.95 0 1 0 True `shouldBe` 1.0

    it "keeps combat's separate x1.10 as a distinct effect inside the finite \
       \0.75..1.10 guard" $ do
        mentalMultiplier 0.0 True `shouldSatisfy` (\v → abs (v - 0.75 * 1.10) < 1e-6)
        mentalMultiplier 1.0 True `shouldBe` 1.10
        mentalMultiplier 1.0 False `shouldBe` 1.0

-----------------------------------------------------------------------
-- Freshness, purity
-----------------------------------------------------------------------

freshnessSpec ∷ Spec
freshnessSpec = describe "a read is on demand, pure and exact" $ do
    it "follows a dependency change and a modifier's expiry at once, with no tick" $
        withScene [ (unitSpec 1 "acolyte" (Just "exempt"))
                      { usStats = [("caffeine", 0.4)]
                      , usModifiers = [("caffeine", [StatModifier 0 "t" (Just 10) 1.0])] } ] $
            \(env, ls) → do
                -- an active +100 % on caffeine doubles it: 0.8 -> + 0.15 x 0.8
                modifyInstance env 1 $ \i →
                    i { uiStats = HM.insert "stamina" 0 (uiStats i) }
                a ← luaNum ls "unit.readPhysio(1, 'caffeine')"
                near' 1e-5 0.8 (realToFrac a)
                writeGameTime env 11
                b ← luaNum ls "unit.readPhysio(1, 'caffeine')"
                near' 1e-5 0.4 (realToFrac b)
                -- the oxygen input moves consciousness at once
                c0 ← luaNum ls "unit.readPhysio(1, 'consciousness')"
                setStatOf env 1 "blood_oxygen" 0.6
                c1 ← luaNum ls "unit.readPhysio(1, 'consciousness')"
                near' 1e-5 1.0 (realToFrac c0)
                near' 1e-5 (1 / 3) (realToFrac c1)

    it "stores nothing and draws nothing: the stat map and the gameplay random \
       \stream are untouched by any number of reads" $
        withScene [unitSpec 1 "acolyte" (Just "rich")] $ \(env, ls) → do
            before ← uiStats <$> readInstance env 1
            _ ← luaText ls "math.randomseed(42); return true"
            r1 ← luaText ls "return tostring(math.random())"
            _ ← luaText ls
                "math.randomseed(42); \
                \for _ = 1, 25 do \
                \  unit.readPhysio(1, 'consciousness'); \
                \  unit.readPhysio(1, 'concentration'); \
                \  unit.readPhysio(1, 'state_of_mind') end; \
                \return true"
            r2 ← luaText ls "return tostring(math.random())"
            r1 `shouldBe` r2
            after ← uiStats <$> readInstance env 1
            after `shouldBe` before

-----------------------------------------------------------------------
-- Finite guard
-----------------------------------------------------------------------

containmentSpec ∷ Spec
containmentSpec = describe "a non-finite input is a violated precondition, never a value" $
    it "refuses the derived read (a raise in Lua) and leaves the unit alone" $
        forM_ [("NaN", 0 / 0), ("+Infinity", 1 / 0), ("-Infinity", -1 / 0)] $ \(label, bad) →
            withScene [unitSpec 1 "acolyte" (Just "exempt")] $ \(env, ls) → do
                setStatOf env 1 "caffeine" bad
                out ← luaText ls
                    "local ok = pcall(unit.readPhysio, 1, 'concentration'); \
                    \return tostring(ok)"
                (label, out) `shouldBe` (label, "false")
                -- consciousness does not read caffeine: it still answers
                luaNum ls "unit.readPhysio(1, 'consciousness')" >>= near 1e-9 1

-----------------------------------------------------------------------
-- I2: the interfaces, against complete healthy stubs over 600 ticks
-----------------------------------------------------------------------

interfaceSpec ∷ Spec
interfaceSpec = describe "interface equivalence (I2)" $ do
    it "thermo, salt and cardio -> consciousness: Nominal systems and Simulated \
       \ones held at the healthy interface drive the brain identically over \
       \600 ticks, within 1e-6" $
        withScene [ (unitSpec 1 "acolyte" (Just "rich"))
                      { usOverrides = [ ("thermo", PhysioNominal), ("salt", PhysioNominal)
                                      , ("cardio", PhysioNominal) ] }
                  , unitSpec 2 "acolyte" (Just "rich") ] $ \(_, ls) → do
            worst ← heldRun ls
                "unit.setStat(2, 'core_temp', 37); unit.setStat(2, 'blood_oxygen', 1); \
                \unit.setStat(2, 'salt', 70); unit.setStat(2, 'heart_rate', 70)"
                ["consciousness", "concentration", "state_of_mind"]
            worst `shouldSatisfy` (< 1e-6)

    it "stamina -> concentration: a Nominal stamina and a Simulated one held full \
       \drive the derived concentration identically over 600 ticks, within 1e-6" $
        withScene [ (unitSpec 1 "acolyte" (Just "rich"))
                      { usOverrides = [("stamina", PhysioNominal)] }
                  , unitSpec 2 "acolyte" (Just "rich") ] $ \(_, ls) → do
            worst ← heldRun ls
                "unit.setStat(2, 'stamina', unit.getMaxStamina(2))"
                ["concentration", "state_of_mind"]
            worst `shouldSatisfy` (< 1e-6)

    it "caffeine -> stamina regen: a Nominal mind and a Simulated one held at zero \
       \caffeine regenerate the same stamina over 600 ticks, within 1e-6" $
        withScene [ (unitSpec 1 "acolyte" (Just "rich"))
                      { usOverrides = [("mind", PhysioNominal)], usStats = [("stamina", 1)] }
                  , (unitSpec 2 "acolyte" (Just "rich")) { usStats = [("stamina", 1)] } ] $
            \(_, ls) → do
                worst ← heldRun ls
                    "unit.setStat(2, 'caffeine', 0)"
                    ["stamina"]
                worst `shouldSatisfy` (< 1e-6)

-- | Tick 600 x 0.1 s, holding the stub unit's interface each tick, and answer
--   the worst difference between the two units' reads of @keys@.
heldRun ∷ LuaBackendState → Text → [Text] → IO Double
heldRun ls hold keys = luaNum ls $ T.concat
    [ "(function() local worst = 0; for _ = 1, 600 do __resources.update(0.1); "
    , hold, "; "
    , "for _, k in ipairs({", T.intercalate "," [ "'" <> k <> "'" | k ← keys ], "}) do "
    , "local a, b = unit.readPhysio(1, k), unit.readPhysio(2, k); "
    , "worst = math.max(worst, math.abs(a - b)) end end; return worst end)()" ]
