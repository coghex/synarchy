{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | #353: the canonical mental-effectiveness contract — a single
--   derived 0.75..1.10 multiplier ('Combat.Resolution.Common.
--   mentalEffectiveness') shared by combat hit/active-dodge
--   ('Combat.Resolution.Strike') and craft quality
--   ('Craft.Execute.applyMentalQuality'). Craft-bill progress (Lua,
--   scripts/unit_ai_craft.lua) reads the SAME Haskell calculation
--   through the @unit.getMentalEffectiveness@ verb — that plumbing is
--   covered end-to-end by tools/mental_efficiency_probe.py, not here.
module Test.Headless.Combat.MentalEffectiveness (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Engine.Asset.Handle (TextureHandle(..))
import Unit.Types
import World.Page.Types (WorldPageId(..))
import Unit.Direction (Direction(..))
import Combat.Types (AttackMode(..))
import Combat.Resolution.Common (mentalEffectiveness, maxStaminaFor)
import Combat.Resolution.Strike
    ( computeAttackerSkill, computeDefenderEvasion, hitChance
    , defenderDodgeChance )
import Combat.Resolution.Constants (dodgeMaxChance)
import Combat.Resolution.Damage (computeSeverity)
import Substance.Types (emptySubstanceManager)
import Item.Types (emptyItemManager)
import Craft.Execute (craftQuality, applyMentalQuality)

-- A minimal unit carrying exactly the given stats/skills; every other
-- field is inert boilerplate, mirroring Test.Headless.Combat.Wounds's
-- fixture.
mkInst ∷ HM.HashMap Text Float → HM.HashMap Text Float → UnitInstance
mkInst stats skills = UnitInstance
    { uiDefName = "t", uiName = "", uiPage = WorldPageId "test"
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0, uiRealZ = 0
    , uiFacing = DirS, uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = stats
    , uiModifiers = HM.empty, uiSkills = skills, uiKnowledge = HM.empty
    , uiInventory = [], uiEquipment = HM.empty, uiAccessories = []
    , uiFactionId = "t", uiWounds = [], uiScars = []
    , uiImmuneResponse = 0, uiImmunities = HM.empty, uiBlood = 100
    , uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing }

-- A minimal single-part target body for computeSeverity — mirrors
-- Test.Headless.Combat.Wounds's `def` fixture. bpLayers = [] is
-- deliberate: Combat.Resolution.Damage.resolvePartLayersNamed falls
-- back to one synthetic 40mm flesh layer for an unlayered part, giving
-- a fully deterministic, non-crashing result with no substance catalog
-- needed (Combat.Resolution.Damage's own layer-absorb defaults handle
-- an unresolved "flesh" substance the same way, and this exact fallback
-- is already asserted by Test.Headless.Combat.Damage).
targetDef ∷ UnitDef
targetDef = UnitDef
    { udName = "t", udNamePool = Nothing, udDisplayName = Nothing
    , udTexture = TextureHandle 0, udPortrait = Nothing, udDirSprites = Map.empty
    , udBaseWidth = 0, udMaxSpeed = 1.0, udRunThreshold = 0.6
    , udAnimations = HM.empty, udStateAnims = HM.empty, udEagerStats = False
    , udStatTemplates = HM.empty, udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = []
    , udEquipmentClass = Nothing, udStartingEquipment = HM.empty
    , udStartingAccessories = []
    , udBodyParts =
        [ BodyPart
            { bpId = "torso", bpName = "torso", bpParent = Nothing
            , bpVital = False, bpAreaWeight = 0.3, bpTacticalValue = 0.5
            , bpBleedFactor = 1.0, bpHeightLow = 0, bpHeightHigh = 2
            , bpLayers = [], bpTargetable = True, bpDepth = 0.0
            , bpAffectsLocomotion = False, bpAffectsBalance = False } ]
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = [] }

-- A unit with only concentration (and, optionally, an EUPHORIC
-- mental_state = 3) set — everything mentalEffectiveness reads.
concEuph ∷ Float → Bool → UnitInstance
concEuph conc euphoric = mkInst
    (HM.fromList (("concentration", conc) : [("mental_state", 3.0) | euphoric]))
    HM.empty

near ∷ Float → Float → Bool
near expect x = abs (x - expect) < 0.0005

-- Every (hi, lo) pair from a list where hi > lo — used to sweep
-- "decreasing effectiveness" across a whole scalar range at once.
descendingPairs ∷ [Float] → [(Float, Float)]
descendingPairs xs = [ (hi, lo) | hi ← xs, lo ← xs, hi > lo ]

spec ∷ Spec
spec = describe "Mental effectiveness" $ do

    -- ---- 1/2: the mentalEffectiveness formula itself ----
    describe "mentalEffectiveness formula" $ do
        it "normal concentration without euphoria is neutral at 1.00" $
            mentalEffectiveness (concEuph 1.0 False) `shouldBe` 1.00

        it "concentration 0.50 without euphoria is 0.875" $
            mentalEffectiveness (concEuph 0.5 False) `shouldBe` 0.875

        it "missing concentration defaults to 1.0 (neutral)" $
            mentalEffectiveness (mkInst HM.empty HM.empty) `shouldBe` 1.00

        it "missing mental_state defaults to non-euphoric" $
            mentalEffectiveness
                (mkInst (HM.fromList [("concentration", 0.5)]) HM.empty)
                `shouldBe` 0.875

        it "zero concentration bottoms out at a 25% penalty (0.75)" $
            mentalEffectiveness (concEuph 0.0 False) `shouldBe` 0.75

        it "concentration is clamped to 0..1 before use (below 0)" $
            mentalEffectiveness (concEuph (-5.0) False) `shouldBe` 0.75

        it "concentration is clamped to 0..1 before use (above 1)" $
            mentalEffectiveness (concEuph 5.0 False) `shouldBe` 1.00

        it "euphoria at zero concentration multiplies the 0.75 floor by 1.10" $
            mentalEffectiveness (concEuph 0.0 True) `shouldSatisfy` near (0.75 * 1.10)

        it "euphoria at full concentration is capped at 1.10 (not 1.10 x 1.10)" $
            mentalEffectiveness (concEuph 1.0 True) `shouldSatisfy` near 1.10

        it "the final result never leaves the 0.75..1.10 band" $ do
            mentalEffectiveness (concEuph 1.0 True) `shouldSatisfy` (≤ 1.10)
            mentalEffectiveness (concEuph 0.0 False) `shouldSatisfy` (≥ 0.75)

    -- ---- 3: sign-safe hit/dodge monotonicity ----
    describe "attacker hit chance (Combat.Resolution.Strike.hitChance)" $ do
        let effs = [0.75, 0.80, 0.90, 1.00, 1.05, 1.10]
            -- (atkSkill, defEva) pairs: a saturating-positive gap, a
            -- neutral wash, and two ADVERSE combinations where the raw
            -- composite goes negative (a pained/heavy attacker) —
            -- exactly the case a bare multiply would get backwards.
            pairs = [ (0.9, -0.9), (0.5, 0.5), (-0.3, 0.9)
                    , (-1.0, -1.0), (0.2, -0.1) ]

        it "reproduces the pre-#353 clamp(rawHit, 0.05, 0.95) exactly at effectiveness 1.00" $ do
            hitChance 1.00 0.5 0.2
                `shouldSatisfy` near (clamp 0.05 0.95 (0.7 + (0.5 - 0.2) * 0.3))
            hitChance 1.00 (-5.0) 5.0 `shouldBe` 0.05
            hitChance 1.00 5.0 (-5.0) `shouldBe` 0.95

        it "pHit is non-increasing as attacker effectiveness decreases, for every pair" $
            forM_ (descendingPairs effs) $ \(eHi, eLo) →
                forM_ pairs $ \(atk, def) →
                    hitChance eLo atk def
                        `shouldSatisfy` (≤ hitChance eHi atk def)

        it "stays within the outer [0.05, 0.95] clamp, including at euphoria" $ do
            hitChance 1.10 10.0 (-10.0) `shouldBe` 0.95
            hitChance 1.10 (-10.0) 10.0 `shouldBe` 0.05
            hitChance 0.75 10.0 (-10.0) `shouldBe` 0.95
            hitChance 0.75 (-10.0) 10.0 `shouldBe` 0.05

    describe "active-dodge chance (Combat.Resolution.Strike.defenderDodgeChance)" $ do
        let effs = [0.75, 0.85, 0.95, 1.00, 1.10]
            -- (target, isLunge, pain) combos: a saturating-high dodge
            -- build, a plain low-stat defender, and a heavily pained one
            -- whose uncapped inner expression goes negative.
            hotTgt   = mkInst (HM.fromList [("agility", 3.0)])
                               (HM.fromList [("dodge", 100.0)])
            plainTgt = mkInst (HM.fromList [("agility", 1.0)])
                               (HM.fromList [("dodge", 0.0)])
            cases = [ (hotTgt, False, 0.0)
                    , (plainTgt, False, 0.0)
                    , (plainTgt, True, 0.3)
                    , (plainTgt, False, 0.95) ]

        it "reproduces the pre-#353 base*lunge - pain expression, clamped, at effectiveness 1.00" $
            defenderDodgeChance 1.00 hotTgt True 0.0 `shouldBe` dodgeMaxChance

        it "active-dodge chance is non-increasing as defender effectiveness decreases, for every case" $
            forM_ (descendingPairs effs) $ \(eHi, eLo) →
                forM_ cases $ \(tgt, isLunge, pain) →
                    defenderDodgeChance eLo tgt isLunge pain
                        `shouldSatisfy`
                            (≤ defenderDodgeChance eHi tgt isLunge pain)

        it "stays within the outer [0, dodgeMaxChance] clamp, including at euphoria" $ do
            defenderDodgeChance 1.10 hotTgt True 0.0 `shouldBe` dodgeMaxChance
            defenderDodgeChance 1.10 plainTgt False 100.0 `shouldBe` 0.0
            defenderDodgeChance 0.75 plainTgt False 100.0 `shouldBe` 0.0

    -- ---- 5: mental effectiveness never leaks into damage energy or
    --         recovery/cooldown. computeAttackerSkill / computeDefender-
    --         Evasion supply the pain/skill/evasion terms that feed the
    --         damage-energy and stamina/stance-drain pipeline
    --         (Combat.Resolution.Damage / Wear); hitChance and
    --         defenderDodgeChance take effectiveness as a SEPARATE
    --         scalar argument that neither function receives. Showing
    --         these two are bit-identical whether or not concentration/
    --         mental_state vary proves effectiveness can't reach damage,
    --         weapon wear, or stamina drain, all of which are computed
    --         purely from these same terms plus the untouched pain/mode
    --         inputs.
    describe "mental effectiveness does not leak into damage energy or recovery" $ do
        it "computeAttackerSkill is unaffected by concentration/mental_state" $ do
            let base = HM.fromList
                    [ ("dexterity", 1.2), ("perception", 1.1)
                    , ("strength", 1.0), ("body_mass", 75.0) ]
                calm       = mkInst base HM.empty
                distracted = mkInst
                    (HM.insert "mental_state" 3.0
                        (HM.insert "concentration" 0.1 base))
                    HM.empty
            computeAttackerSkill calm Nothing Nothing 0 0.2 Quick
                `shouldBe`
                computeAttackerSkill distracted Nothing Nothing 0 0.2 Quick
            computeAttackerSkill calm Nothing Nothing 0 0.2 Heavy
                `shouldBe`
                computeAttackerSkill distracted Nothing Nothing 0 0.2 Heavy

        it "computeDefenderEvasion is unaffected by concentration/mental_state" $ do
            let base = HM.fromList
                    [ ("agility", 1.2), ("reflexes", 1.1)
                    , ("perception", 1.0), ("dexterity", 1.0)
                    , ("body_mass", 70.0) ]
                calm     = mkInst base HM.empty
                euphoric = mkInst
                    (HM.insert "mental_state" 3.0
                        (HM.insert "concentration" 1.0 base))
                    HM.empty
            computeDefenderEvasion calm 0.1
                `shouldBe` computeDefenderEvasion euphoric 0.1

        -- Direct proof for damage energy itself (not just the terms
        -- that feed it): computeSeverity never reads "concentration" or
        -- "mental_state" from uiStats anywhere in its body, so its full
        -- result — severity, "raw" driver energy, "eff" post-resistance
        -- damage, weapon load, and weapon hardness alike — must be
        -- bit-identical for two attackers differing ONLY in those two
        -- stats. Unarmed (mEquipped = natW = Nothing) keeps the strike
        -- profile a fixed function of bodyMass alone (pinned via
        -- body_mass on both fixtures), matching this suite's combat
        -- damage-sampling probe technique.
        it "computeSeverity (damage energy itself) is unaffected by concentration/mental_state" $ do
            let base = HM.fromList
                    [ ("strength", 1.0), ("dexterity", 1.0)
                    , ("body_mass", 70.0), ("height", 1.8) ]
                calm       = mkInst base HM.empty
                distracted = mkInst
                    (HM.insert "mental_state" 3.0
                        (HM.insert "concentration" 0.0 base))
                    HM.empty
                tgt = mkInst (HM.fromList [("body_mass", 70.0)]) HM.empty
                severityOf atk = computeSeverity emptySubstanceManager emptyItemManager
                    atk targetDef Nothing Nothing tgt "torso" "blunt" Quick 0.5 1.0 0.0
            severityOf calm `shouldBe` severityOf distracted

        -- The Haskell-side resource-cost analogue of "recovery": the
        -- stamina pool applyStaminaDrain (Combat.Resolution.Wear) draws
        -- against is maxStaminaFor, and its cost fraction
        -- (staminaCostFraction) is a plain function of AttackMode
        -- alone — it doesn't even take a UnitInstance, so it structurally
        -- cannot vary with concentration/mental_state. Lua-side attack
        -- cooldown (scripts/unit_ai_combat.lua) is untouched by #353's
        -- diff and isn't Haskell-pure-testable; this is the Haskell
        -- combat-thread's own recovery-cost path.
        it "maxStaminaFor (the recovery/stamina-drain pool) is unaffected by concentration/mental_state" $ do
            let base = HM.fromList [("endurance", 1.4)]
                calm     = mkInst base HM.empty
                euphoric = mkInst
                    (HM.insert "mental_state" 3.0
                        (HM.insert "concentration" 1.0 base))
                    HM.empty
            maxStaminaFor calm `shouldBe` maxStaminaFor euphoric

    -- ---- 6: craft-quality delta ----
    describe "craft-quality mental-effectiveness delta (Craft.Execute.applyMentalQuality)" $ do
        it "effectiveness 1.00 leaves base quality unchanged" $
            applyMentalQuality 1.00 55.0 `shouldBe` 55.0

        it "the exact delta formula: 100 x (effectiveness - 1.00)" $ do
            applyMentalQuality 1.05 50.0 `shouldSatisfy` near 55.0
            applyMentalQuality 0.95 50.0 `shouldSatisfy` near 45.0
            applyMentalQuality 1.033 50.0 `shouldSatisfy` near 53.3  -- float-valued, unrounded

        it "delta clamps at -10 once effectiveness drops enough to exceed it" $
            -- unclamped delta at 0.75 would be -25; clamps to -10.
            applyMentalQuality 0.75 50.0 `shouldSatisfy` near 40.0

        it "delta clamps at +10 exactly at euphoria's 1.10 cap" $
            -- delta at 1.10 is exactly 10 (the clamp boundary, not past it).
            applyMentalQuality 1.10 50.0 `shouldSatisfy` near 60.0

        it "final quality clamps to [0, 100] at the low end" $
            applyMentalQuality 0.75 5.0 `shouldBe` 0.0

        it "final quality clamps to [0, 100] at the high end" $
            applyMentalQuality 1.10 95.0 `shouldBe` 100.0

        it "composes with a skill-tagged recipe's deterministic base (#343 craftQuality)" $ do
            -- #343's skill x knowledge base (deterministic, unlike an
            -- untagged recipe's rolled quality), then #353's delta on
            -- top — exactly the applyCraft pipeline for a tagged recipe.
            let base = craftQuality 90 (Just 80)
            base `shouldSatisfy` near 87.0
            applyMentalQuality 1.10 base `shouldSatisfy` near 97.0
            applyMentalQuality 0.75 base `shouldSatisfy` near 77.0
