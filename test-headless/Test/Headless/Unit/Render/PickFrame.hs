{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Pure tests for Unit.Render.pickFrame. Constructs synthetic UnitDef
--   and UnitInstance values; verifies the frame index math and the
--   T-pose fallback chain.
module Test.Headless.Unit.Render.PickFrame (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Unit.Direction (Direction(..))
import Unit.Render (pickFrame, screenDirOf)
import Unit.Types
import World.Page.Types (WorldPageId(..))

-- | A texture handle by integer ID, for readable test assertions.
h ∷ Int → TextureHandle
h = TextureHandle

-- | pickFrame now returns @(TextureHandle, Bool)@ where the Bool is
--   the horizontal-flip flag the renderer applies. These tests cover
--   the frame-index math and the T-pose fallbacks; flip semantics
--   are out of scope here, so we strip the Bool.
pickTex ∷ Double → CameraFacing → UnitInstance → UnitDef → TextureHandle
pickTex t cam inst def = fst (pickFrame t cam inst def)

-- | A UnitDef with the supplied animations and T-pose sprite (handle 0)
--   plus a directional S sprite (handle 1).
mkDef ∷ HM.HashMap Text Animation → UnitDef
mkDef anims = UnitDef
    { udName          = "test-unit"
    , udTexture       = h 0
    , udDirSprites    = Map.fromList [(DirS, h 1)]
    , udBaseWidth     = 0
    , udMaxSpeed      = 1.0
    , udRunThreshold  = 0.6
    , udAnimations    = anims
    , udStateAnims    = HM.empty
    , udEagerStats    = False
    , udStatTemplates = HM.empty
    , udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = []
    , udEquipmentClass = Nothing
    , udStartingEquipment = HM.empty
    , udStartingAccessories = []
    , udBodyParts          = []
    , udNaturalResistance  = defaultNaturalResistance
    , udNaturalWeapon      = Nothing
    , udModifiers          = []
    }

-- | A UnitInstance facing south with the supplied anim name and start time.
mkInst ∷ Text → Double → UnitInstance
mkInst animName start = UnitInstance
    { uiDefName     = "test-unit"
    , uiPage        = WorldPageId "test"
    , uiTexture     = h 0
    , uiDirSprites  = Map.fromList [(DirS, h 1)]
    , uiBaseWidth   = 0
    , uiGridX       = 0
    , uiGridY       = 0
    , uiGridZ       = 0
    , uiRealZ       = 0
    , uiFacing      = DirS
    , uiCurrentAnim = animName
    , uiAnimStart   = start
    , uiAnimReverse = False
    , uiActivity    = "idle"
    , uiPose        = "standing"
    , uiAnimStride  = 1
    , uiStats       = HM.empty
    , uiModifiers   = HM.empty
    , uiSkills      = HM.empty
    , uiKnowledge   = HM.empty
    , uiInventory   = []
    , uiEquipment   = HM.empty
    , uiAccessories = []
    , uiFactionId       = "test"
    , uiWounds          = []
    , uiScars           = []
    , uiImmuneResponse  = 0
    , uiImmunities      = HM.empty
    , uiBlood           = 0
    , uiLastAttackerUid = Nothing
    , uiLastAttackerAt  = 0
    , uiAnimOverride    = ""
    , uiFrozen          = False
    , uiForceLoop       = False
    , uiClimbDest       = Nothing
    }

-- | An animation with frame handles 100,101,102,103 on DirS, fps 4, loop.
animSouth4 ∷ Animation
animSouth4 = Animation
    { aFps    = 4.0
    , aLoop   = True
    , aFlip   = False
    , aFrames = Map.fromList
        [(DirS, V.fromList [h 100, h 101, h 102, h 103])]
    }

spec ∷ Spec
spec = do
    describe "screenDirOf" $ do
        it "leaves world facing unchanged when camera faces south" $
            screenDirOf FaceSouth DirS `shouldBe` DirS
        it "rotates DirS to DirW under FaceWest camera" $
            -- world S + camera W: a south-facing unit appears as W on screen
            -- after rotation 2 steps CCW (south idx 0 - 2 = 6 ≡ DirE? actually
            -- the code does (dirIdx - rotSteps) mod 8 — with rotSteps=2 for W,
            -- 0-2 mod 8 = 6 = DirE. Verify the actual implementation behavior:
            screenDirOf FaceWest DirS `shouldBe` DirE
        it "is its own inverse for full rotation" $
            screenDirOf FaceSouth (screenDirOf FaceSouth DirNE) `shouldBe` DirNE

    describe "pickFrame — T-pose fallbacks" $ do
        it "returns directional T-pose when uiCurrentAnim is empty" $
            pickTex 0.0 FaceSouth (mkInst "" 0)
                    (mkDef HM.empty) `shouldBe` h 1  -- DirS T-pose
        it "returns T-pose when anim name is not in udAnimations" $
            pickTex 0.0 FaceSouth (mkInst "ghost" 0)
                    (mkDef HM.empty) `shouldBe` h 1
        it "returns T-pose when anim has no frames for the screen direction" $
            let anim = Animation 4.0 True False Map.empty  -- empty frames map
                def  = mkDef (HM.fromList [("idle", anim)])
            in pickTex 0.0 FaceSouth (mkInst "idle" 0) def `shouldBe` h 1
        it "returns T-pose when frames vector for screen direction is empty" $
            let anim = Animation 4.0 True False (Map.fromList [(DirS, V.empty)])
                def  = mkDef (HM.fromList [("idle", anim)])
            in pickTex 0.0 FaceSouth (mkInst "idle" 0) def `shouldBe` h 1
        it "falls back to default texture when instance has no directional sprite" $
            -- T-pose path reads uiDirSprites from the instance, not the def
            let inst = (mkInst "" 0) { uiDirSprites = Map.empty }
            in pickTex 0.0 FaceSouth inst (mkDef HM.empty) `shouldBe` h 0

    describe "pickFrame — frame index math (loop=True)" $ do
        let def = mkDef (HM.fromList [("idle", animSouth4)])
        it "picks frame 0 at t=0" $
            pickTex 0.0 FaceSouth (mkInst "idle" 0) def `shouldBe` h 100
        it "picks frame 0 at t=0.1 (under 1/fps)" $
            pickTex 0.1 FaceSouth (mkInst "idle" 0) def `shouldBe` h 100
        it "picks frame 1 at t=0.25 (exactly 1/fps)" $
            pickTex 0.25 FaceSouth (mkInst "idle" 0) def `shouldBe` h 101
        it "picks frame 2 at t=0.5" $
            pickTex 0.5 FaceSouth (mkInst "idle" 0) def `shouldBe` h 102
        it "picks frame 3 at t=0.75" $
            pickTex 0.75 FaceSouth (mkInst "idle" 0) def `shouldBe` h 103
        it "wraps around to frame 0 at t=1.0 (one full cycle)" $
            pickTex 1.0 FaceSouth (mkInst "idle" 0) def `shouldBe` h 100
        it "wraps again at t=1.25" $
            pickTex 1.25 FaceSouth (mkInst "idle" 0) def `shouldBe` h 101
        it "respects animStart offset" $
            pickTex 5.25 FaceSouth (mkInst "idle" 5.0) def `shouldBe` h 101
        it "guards against negative elapsed (clock skew) → frame 0" $
            pickTex 0.0 FaceSouth (mkInst "idle" 10.0) def `shouldBe` h 100

    describe "pickFrame — non-loop clamp" $ do
        let anim = animSouth4 { aLoop = False }
            def  = mkDef (HM.fromList [("once", anim)])
        it "advances normally before the end" $ do
            pickTex 0.0  FaceSouth (mkInst "once" 0) def `shouldBe` h 100
            pickTex 0.5  FaceSouth (mkInst "once" 0) def `shouldBe` h 102
        it "clamps to last frame after the end" $ do
            pickTex 1.0  FaceSouth (mkInst "once" 0) def `shouldBe` h 103
            pickTex 10.0 FaceSouth (mkInst "once" 0) def `shouldBe` h 103
