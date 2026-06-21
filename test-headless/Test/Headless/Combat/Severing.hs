{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Tests for the severing cascade (Combat.Wounds.propagateSevering):
--   a destroyed (pulverised / cut-through) part takes its attached
--   children with it — a shattered arm severs the hand.
module Test.Headless.Combat.Severing (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Engine.Asset.Handle (TextureHandle(..))
import Unit.Types
import Unit.Direction (Direction(..))
import Combat.Wounds (propagateSevering)

-- Body: arm → hand → finger chain, so we can watch the loss propagate.
def ∷ UnitDef
def = UnitDef
    { udName = "t", udTexture = TextureHandle 0, udDirSprites = Map.empty
    , udBaseWidth = 0, udMaxSpeed = 1.0, udRunThreshold = 0.6
    , udAnimations = HM.empty, udStateAnims = HM.empty, udEagerStats = False
    , udStatTemplates = HM.empty, udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udStartingInventory = []
    , udEquipmentClass = Nothing, udStartingEquipment = HM.empty
    , udStartingAccessories = []
    , udBodyParts =
        [ bp "torso"   Nothing
        , bp "l_arm"   (Just "torso")
        , bp "l_hand"  (Just "l_arm")
        , bp "l_finger"(Just "l_hand")
        ]
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = [] }
  where
    bp pid par = BodyPart
        { bpId = pid, bpName = pid, bpParent = par, bpVital = False
        , bpAreaWeight = 0.1, bpTacticalValue = 0.5
        , bpBleedFactor = 1.0, bpHeightLow = 0, bpHeightHigh = 1, bpLayers = []
        , bpTargetable = True, bpDepth = 0.0 }

inst ∷ [Wound] → UnitInstance
inst ws = UnitInstance
    { uiDefName = "t", uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0, uiRealZ = 0
    , uiFacing = DirS, uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty, uiModifiers = HM.empty, uiSkills = HM.empty
    , uiInventory = [], uiEquipment = HM.empty, uiAccessories = []
    , uiFactionId = "t", uiWounds = ws, uiBlood = 5
    , uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing }

wound ∷ Text → Text → Float → Wound
wound part kind sev = Wound { woundPart = part, woundKind = kind
                            , woundSeverity = sev, woundAt = 0 }

severedParts ∷ UnitInstance → [Text]
severedParts i = [ woundPart w | w ← uiWounds i, woundKind w ≡ "severed" ]

severedPartsOf ∷ UnitDef → UnitInstance → [Text]
severedPartsOf _ = severedParts

-- Neck → cervical_spine → head: destroying the spine severs the head.
decapDef ∷ UnitDef
decapDef = def
    { udBodyParts =
        [ bpD "torso"          Nothing
        , bpD "neck"           (Just "torso")
        , bpD "cervical_spine" (Just "neck")
        , bpD "head"           (Just "cervical_spine")
        ] }
  where
    bpD pid par = BodyPart
        { bpId = pid, bpName = pid, bpParent = par, bpVital = (pid ≡ "head")
        , bpAreaWeight = 0.1, bpTacticalValue = 0.5
        , bpBleedFactor = 1.0, bpHeightLow = 0, bpHeightHigh = 1, bpLayers = []
        , bpTargetable = (pid ≡ "head" ∨ pid ≡ "neck" ∨ pid ≡ "torso")
        , bpDepth = 0.0 }

spec ∷ Spec
spec = do
    describe "propagateSevering" $ do
        it "a pulverised arm (fracture ≥1) severs the attached hand" $ do
            let out = propagateSevering def (inst [wound "l_arm" "fracture" 1.0])
            severedParts out `shouldContain` ["l_hand"]

        it "leaves an intact body alone" $ do
            let out = propagateSevering def (inst [wound "l_arm" "fracture" 0.5])
            severedParts out `shouldBe` []

        it "is idempotent — doesn't re-sever an already-severed part" $ do
            let i1 = propagateSevering def (inst [wound "l_arm" "fracture" 1.0])
                i2 = propagateSevering def i1
            length (filter (≡ "l_hand") (severedParts i2)) `shouldBe` 1

        it "propagates down the chain over successive ticks (hand→finger)" $ do
            -- tick 1: arm destroyed → hand severed; tick 2: severed hand
            -- (severity 1) is itself destroyed → finger severed.
            let i1 = propagateSevering def (inst [wound "l_arm" "fracture" 1.0])
                i2 = propagateSevering def i1
            severedParts i2 `shouldContain` ["l_finger"]

        it "decapitates: a destroyed cervical spine severs the head" $ do
            -- head.parent = cervical_spine, so a broken-through neck takes
            -- the head with it (the acolyte's body wiring).
            let i1 = propagateSevering decapDef
                        (inst [wound "cervical_spine" "fracture" 1.0])
            severedPartsOf decapDef i1 `shouldContain` ["head"]
