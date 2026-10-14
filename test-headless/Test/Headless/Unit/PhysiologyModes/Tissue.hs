{-# LANGUAGE Strict, OverloadedStrings #-}
-- | Nutrition floors and the Nominal tissue upkeep of the "physiology stat
--   modes" group (#2831; design T-9, SD-5, SD-18).
--
--   A Nominal nutrition does not starve a unit and does not rebuild it
--   either, except for one depletion-only job: a unit wasted below its
--   spawn margin recovers toward the species mean. These cases run the
--   SHIPPED tick (@unit_resources@, @unit_resource_upkeep@, the stamina
--   tick's organ-failure branch, the starvation strength discount) over the
--   real @unit.*@ API, on the reference 70 kg acolyte (frame 70 kg, so the
--   fat floor is 1.4 kg, the spawn margin 2.38 kg and the species target
--   14.0 kg; lean 14.0 / 14.98 / 28.0) and on a 0.3 kg squirrel where a
--   whole recovery fits inside a game day (fat 0.006 / 0.0102 / 0.03 kg).
module Test.Headless.Unit.PhysiologyModes.Tissue (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import Test.Headless.Unit.PhysiologyModes.Harness
import Unit.Types

-- | The reference acolyte, species mean body fat 0.2 (and the body's other
--   templates). Hydration is Nominal ("water supplied"): the long walking
--   runs below would otherwise end in the acolyte's death from thirst
--   (the walking drain is the shipped one), which is not what is under test.
acolyteWith ∷ [(Text, Float)] → UnitSpec
acolyteWith stats = (unitSpec 1 "acolyte" (Just "rich"))
    { usStats = stats, usModes = [("hydration", PhysioNominal)]
    , usBodyTemplates = [("bodyfat", (0.2, 0.36)), ("height", (1.8, 1.0)), ("bulk", (1.0, 1.0))] }

-- | The 0.3 kg squirrel (@reduced@: nutrition Nominal), species mean body fat 0.10.
squirrelWith ∷ [(Text, Float)] → UnitSpec
squirrelWith stats = (unitSpec 1 "red_squirrel" (Just "reduced"))
    { usBody = squirrelBody, usStats = stats
    , usBodyTemplates = [("bodyfat", (0.10, 0.06)), ("height", (0.22, 0.06)), ("bulk", (0.28, 0.06))] }

-- | The all-Nominal nomad (no resource config entry at all).
nomadWith ∷ [(Text, Float)] → UnitSpec
nomadWith stats = (unitSpec 1 "nomad_primitive" (Just "exempt"))
    { usStats = stats
    , usBodyTemplates = [("bodyfat", (0.2, 0.36)), ("height", (1.8, 1.0)), ("bulk", (1.0, 1.0))] }

fat, lean ∷ Text
fat = "fat_mass"
lean = "lean_mass"

-- | A stored stat read back from the engine.
statOf ∷ Scene → Text → IO Double
statOf (_, ls) k = luaNum ls ("unit.getStat(1, '" <> k <> "') or -1")

-- | A tissue sampled every @every@ seconds over @total@ seconds.
sample ∷ Scene → Text → Double → Double → Double → IO [Double]
sample sc@(_, ls) key total every dt = do
    let n = floor (total / every) ∷ Int
    forM [1 .. n] $ \_ → do
        runSeconds ls every dt
        statOf sc key

monotone ∷ [Double] → Bool
monotone xs = and (zipWith (<=) xs (drop 1 xs))

spec ∷ Spec
spec = describe "nutrition floors and Nominal tissue upkeep" $ do
    floorsSpec
    recoverySpec
    flagSpec
    deathsSpec

-----------------------------------------------------------------------
-- A Nominal nutrition has no starvation or organ-failure death
-----------------------------------------------------------------------

floorsSpec ∷ Spec
floorsSpec = describe "T-9 floors" $
    forM_ [ ("fat", [("fat_mass", 1.4)])
          , ("lean", [("lean_mass", 14.0)]) ] $ \(label, stats) →
        it ("a unit at its " <> T.unpack label <> " floor, switched to Nominal, neither \
            \drains nor dies over a game day") $
            withScene [acolyteWith (("calories", 0) : stats)] $ \sc@(env, ls) → do
                climate ls 22 0.5
                luaText ls "return tostring(__physiology.setMode(1, 'nutrition', 'nominal'))"
                    `shouldReturn` "true"
                -- the stamina tick's fat-floor organ failure and the lean-floor
                -- respiratory death are NUTRITION's effects: gone with it
                runSeconds ls 1440 0.5
                killed env `shouldReturn` []
                stamina ← statOf sc "stamina"
                stamina `shouldSatisfy` (> 99)
                scriptErrors ls `shouldReturn` ""

-----------------------------------------------------------------------
-- Depletion-only recovery
-----------------------------------------------------------------------

recoverySpec ∷ Spec
recoverySpec = describe "T-9 recovery toward the species mean" $ do

    forM_ [("idle", "idle", 1.17e-5 ∷ Double), ("walking", "walking", 3.9e-6)] $
      \(label, activity, fatRate) →
        it ("rises monotonically and stops EXACTLY at the fat target, " <> label) $
            withScene [(acolyteWith [ ("fat_mass", 13.99), ("tissue_recovering", 1) ])
                         { usActivity = activity }
                       ] $ \sc@(_, ls) → do
                luaText ls "return tostring(__physiology.setMode(1, 'nutrition', 'nominal'))"
                    `shouldReturn` "true"
                -- 0.01 kg short of the 14.0 kg target at fatRate kg/s
                let seconds = ceiling (0.01 / fatRate * 1.3 ∷ Double) ∷ Int
                trace ← sample sc fat (fromIntegral seconds) (fromIntegral seconds / 20) 1.0
                monotone trace `shouldBe` True
                maximum trace `shouldSatisfy` (<= 14.0 + 1e-4)
                last trace `shouldSatisfy` (\x → abs (x - 14.0) < 1e-4)
                -- recovered: the flag is cleared once both tissues are at target
                statOf sc "tissue_recovering" >>= (`shouldBe` 0)

    it "recovers lean mass too, walking faster than idle (70 % of the share)" $ do
        let run activity = withScene
                [(acolyteWith [("lean_mass", 27.99), ("tissue_recovering", 1)])
                    { usActivity = activity }] $ \sc@(_, ls) → do
                    _ ← luaText ls "__physiology.setMode(1, 'nutrition', 'nominal'); return true"
                    runSeconds ls 120 1.0
                    statOf sc lean
        idle ← run "idle"
        walking ← run "walking"
        idle `shouldSatisfy` (> 27.99)
        walking `shouldSatisfy` (> idle)

    it "retains a tissue ABOVE its target exactly, and does not hand its share to the other" $
        withScene [acolyteWith [ ("fat_mass", 20), ("lean_mass", 27.9), ("tissue_recovering", 1) ]] $
            \sc@(_, ls) → do
                _ ← luaText ls "__physiology.setMode(1, 'nutrition', 'nominal'); return true"
                fatBefore ← statOf sc fat
                runSeconds ls 300 1.0
                statOf sc fat >>= (`shouldBe` fatBefore)
                -- lean grows at ITS OWN share: 0.1 x 0.1 kcal/s / 1800 kcal/kg
                leanNow ← statOf sc lean
                (leanNow - 27.9) `shouldSatisfy` (\d → abs (d - 300 * 0.1 * 0.1 / 1800) < 5e-5)

    it "restores the undiscounted strength immediately on the transition, with no mass change and no tick" $
        withScene [acolyteWith [ ("calories", 100), ("strength", 0.6), ("strength_body", 1.0) ]] $
            \sc@(_, ls) → do
                fatBefore ← statOf sc fat
                _ ← luaText ls "__physiology.setMode(1, 'nutrition', 'nominal'); return true"
                statOf sc "strength" >>= near 1e-6 1.0
                statOf sc fat >>= (`shouldBe` fatBefore)

    it "gives a nomad_primitive, which has no resource config, the same upkeep" $
        withScene [nomadWith [("fat_mass", 1.0)]] $ \sc@(_, ls) → do
            runSeconds ls 600 1.0
            fatNow ← statOf sc fat
            fatNow `shouldSatisfy` (> 1.0)
            statOf sc "tissue_recovering" >>= (`shouldBe` 1)
            scriptErrors ls `shouldReturn` ""

-----------------------------------------------------------------------
-- The persisted flag (SD-18)
-----------------------------------------------------------------------

flagSpec ∷ Spec
flagSpec = describe "SD-18 tissue_recovering" $ do

    it "leaves a healthy individual above its margins but below the mean alone: unflagged, no growth" $
        withScene [acolyteWith [("fat_mass", 5), ("lean_mass", 20)]] $ \sc@(_, ls) → do
            _ ← luaText ls "__physiology.setMode(1, 'nutrition', 'nominal'); return true"
            runSeconds ls 600 1.0
            statOf sc fat >>= (`shouldBe` 5)
            statOf sc lean >>= (`shouldBe` 20)
            statOf sc "tissue_recovering" >>= (`shouldSatisfy` (<= 0))

    it "flags a unit pushed below a margin, recovers it to the target and then unflags it" $
        withScene [squirrelWith [("fat_mass", 0.009)]] $ \sc@(_, ls) → do
            runSeconds ls 1 0.5
            statOf sc "tissue_recovering" >>= (`shouldBe` 1)
            runSeconds ls 2400 1.0
            fatNow ← statOf sc fat
            abs (fatNow - 0.03) `shouldSatisfy` (< 1e-4)
            statOf sc "tissue_recovering" >>= (`shouldBe` 0)

    it "flags tissue lost to Simulated catabolism even above the margins, and recovers it once Nominal" $
        -- a species mean of 0.20 puts the target at the starting fat, so any
        -- catabolism leaves the unit below target while above its margins
        withScene [(squirrelWith []) { usBodyTemplates =
                      [("bodyfat", (0.20, 0.06)), ("height", (0.22, 0.06)), ("bulk", (0.28, 0.06))] }] $ \sc@(_, ls) → do
            climate ls 22 0.5
            _ ← luaText ls "__physiology.setMode(1, 'nutrition', 'simulated', \
                           \{ calories = 0, hunger = 0 }); return true"
            fatBefore ← statOf sc fat
            runSeconds ls 600 1.0
            fatLost ← statOf sc fat
            fatLost `shouldSatisfy` (< fatBefore)
            fatLost `shouldSatisfy` (> 0.0102)           -- still above the spawn margin
            statOf sc "tissue_recovering" >>= (`shouldBe` 1)
            _ ← luaText ls "__physiology.setMode(1, 'nutrition', 'nominal'); return true"
            runSeconds ls 600 1.0
            statOf sc fat >>= (`shouldSatisfy` (> fatLost))

    it "reads a legacy unit with no flag as 0, and flags a below-margin one on its first pass" $ do
        withScene [squirrelWith [("fat_mass", 0.2 * 0.3)]] $ \sc@(_, ls) → do
            statOf sc "tissue_recovering" >>= (`shouldBe` (-1))     -- absent
            runSeconds ls 1 0.5
            statOf sc "tissue_recovering" >>= (`shouldBe` (-1))     -- healthy: still absent
        withScene [squirrelWith [("fat_mass", 0.005)]] $ \sc@(_, ls) → do
            statOf sc "tissue_recovering" >>= (`shouldBe` (-1))
            runSeconds ls 1 0.5
            statOf sc "tissue_recovering" >>= (`shouldBe` 1)

-----------------------------------------------------------------------
-- What still kills
-----------------------------------------------------------------------

deathsSpec ∷ Spec
deathsSpec = describe "deaths a Nominal nutrition does not remove" $ do

    it "still lets a wound-driven organ meter kill" $
        withScene [(acolyteWith []) { usModes = [ ("nutrition", PhysioNominal)
                                                , ("hydration", PhysioNominal) ]
                                    , usWounds = [liverWound] }] $ \(env, ls) → do
            climate ls 22 0.5
            runSeconds ls 650 0.5
            killed env >>= (`shouldContain` [UnitId 1])
            luaNum ls "unit.getStat(1, 'organ') or 0" >>= (`shouldSatisfy` (>= 1))

    it "still lets an exhausted stamina pool kill" $
        withScene [(acolyteWith [("stamina", 0)]) { usModes = [("nutrition", PhysioNominal)] }] $
            \(env, ls) → do
                runSeconds ls 1 0.5
                killed env >>= (`shouldContain` [UnitId 1])
                scriptErrors ls `shouldReturn` ""

liverWound ∷ Wound
liverWound = Wound
    { woundPart = "liver", woundKind = "internal", woundSeverity = 1.0
    , woundAt = 0, woundBandage = 1.0, woundClot = 0.0, woundHeal = 0.0
    , woundDressing = "", woundInfection = 0.0, woundClean = False
    , woundInfectionType = "", woundNecrosis = 0.0 }
