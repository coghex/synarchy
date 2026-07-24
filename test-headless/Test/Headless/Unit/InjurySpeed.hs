{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Tests for Unit.Thread.Command.injurySpeedMult (#393): which parts
--   count as "leg" (bpAffectsLocomotion) or "torso" (bpAffectsBalance)
--   is DATA-DRIVEN off the unit's own body-part list, not a hardcoded
--   id match — so a body plan using non-standard ids (a robot's
--   "hindquarter", say) still slows down correctly as long as its YAML
--   sets the flags.
module Test.Headless.Unit.InjurySpeed (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Engine.Asset.Handle (TextureHandle(..))
import Unit.Types
import World.Page.Types (WorldPageId(..))
import Unit.Direction (Direction(..))
import Unit.Thread.Command (injurySpeedMult)

-- A body part builder: id plus the two locomotion-relevant flags.
part ∷ Text → Bool → Bool → BodyPart
part pid loco bal = BodyPart
    { bpId = pid, bpName = pid, bpParent = Nothing, bpVital = False
    , bpAreaWeight = 0.1, bpTacticalValue = 0.5, bpBleedFactor = 1.0
    , bpHeightLow = 0, bpHeightHigh = 1, bpLayers = []
    , bpTargetable = True, bpDepth = 0.0
    , bpAffectsLocomotion = loco, bpAffectsBalance = bal }

-- Non-standard ids on purpose: proves classification rides on the
-- flag, not on matching "l_leg"/"torso"-style names.
bodyParts ∷ [BodyPart]
bodyParts =
    [ part "chassis"     False True    -- torso-equivalent (balance)
    , part "hindquarter" True  False   -- leg-equivalent (locomotion)
    , part "tail"        False False   -- affects neither
    ]

-- A unit at full blood (bloodMul = 1.0) carrying the given wounds, so
-- the tests isolate the leg/torso terms.
inst ∷ [Wound] → UnitInstance
inst ws = UnitInstance
    { uiDefName = "t", uiName = "", uiPage = WorldPageId "test"
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0, uiRealZ = 0
    , uiFacing = DirS, uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.fromList [("body_mass", 70)]
    , uiModifiers = HM.empty, uiSkills = HM.empty, uiKnowledge = HM.empty
    , uiInventory = [], uiEquipment = HM.empty, uiAccessories = []
    , uiFactionId = "t", uiWounds = ws, uiScars = []
    , uiImmuneResponse = 0, uiImmunities = HM.empty, uiBlood = 70 * bloodMassRatio
    , uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing }

wound ∷ Text → Float → Wound
wound p sev = Wound
    { woundPart = p, woundKind = "blunt", woundSeverity = sev
    , woundAt = 0, woundBandage = 0.0, woundClot = 0.0, woundHeal = 0.0
    , woundDressing = "", woundInfection = 0.0, woundClean = False
    , woundInfectionType = "", woundNecrosis = 0.0 }

speedMult ∷ [Wound] → Float
speedMult ws = injurySpeedMult bodyParts (inst ws)

spec ∷ Spec
spec = describe "injurySpeedMult (data-driven leg/torso classification, #393)" $ do
    it "an uninjured unit at full blood moves at full speed" $
        speedMult [] `shouldBe` 1.0

    it "a wound on a bpAffectsLocomotion part slows movement, even with a non-standard id" $
        speedMult [wound "hindquarter" 0.5] `shouldSatisfy` (< 1.0)

    it "a bpAffectsBalance (torso-equivalent) wound slows movement less than an equal-severity leg wound" $
        let legMult   = speedMult [wound "hindquarter" 0.5]
            torsoMult = speedMult [wound "chassis" 0.5]
        in torsoMult `shouldSatisfy` (> legMult)

    it "a wound on a part with neither flag doesn't affect movement speed" $
        speedMult [wound "tail" 1.0] `shouldBe` 1.0

    it "a wound on an id absent from the body-part list is inert, not a crash" $
        speedMult [wound "nonexistent" 1.0] `shouldBe` 1.0

    it "matches the severity -> multiplier formula for a leg wound" $
        -- legCut = min 1 (0.5 * 1.2) = 0.6; raw = (1-0.6) * (1-0) * 1.0 = 0.4
        speedMult [wound "hindquarter" 0.5] `shouldSatisfy` (\v → abs (v - 0.4) < 1e-4)

    it "clamps the floor at 0.1 under a maximal combined leg+torso wound" $
        speedMult [wound "hindquarter" 1.0, wound "chassis" 1.0] `shouldBe` 0.1
