{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Pure tests for Unit.Anim — the (pose, activity) → anim-name resolver.
module Test.Headless.Unit.Anim (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Engine.Asset.Handle (TextureHandle(..))
import Unit.Anim
import Unit.Sim.Types (Pose(..), UnitActivity(..))
import Unit.Types

mkDef ∷ HM.HashMap Text Text → UnitDef
mkDef stateAnims = UnitDef
    { udName          = "t"
    , udTexture       = TextureHandle 0
    , udDirSprites    = Map.empty
    , udBaseWidth     = 0
    , udMaxSpeed      = 1.0
    , udRunThreshold  = 0.6
    , udAnimations    = HM.empty
    , udStateAnims    = stateAnims
    , udEagerStats    = False
    , udStatTemplates = HM.empty
    , udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty
    , udStartingInventory = []
    , udEquipmentClass = Nothing
    , udStartingEquipment = HM.empty
    , udStartingAccessories = []
    , udBodyParts          = []
    , udNaturalResistance  = defaultNaturalResistance
    , udNaturalWeapon      = Nothing
    , udModifiers          = []
    }

spec ∷ Spec
spec = do
    describe "stateKey" $ do
        it "composes <pose>-idle for Idle activity" $ do
            stateKey Standing  Idle `shouldBe` "standing-idle"
            stateKey Crouching Idle `shouldBe` "crouching-idle"
            stateKey Crawling  Idle `shouldBe` "crawling-idle"
            stateKey Collapsed Idle `shouldBe` "collapsed-idle"
            stateKey Dead      Idle `shouldBe` "dead-idle"
        it "composes <pose>-walk for Walking" $
            stateKey Standing Walking `shouldBe` "standing-walk"
        it "composes <pose>-drink for Drinking" $ do
            stateKey Standing  Drinking `shouldBe` "standing-drink"
            stateKey Crouching Drinking `shouldBe` "crouching-drink"
        it "composes <pose>-pickup for Picking" $
            stateKey Standing Picking `shouldBe` "standing-pickup"
        it "composes <from>-to-<to> for TransitioningTo" $ do
            stateKey Standing  (TransitioningTo Crouching) `shouldBe` "standing-to-crouching"
            stateKey Crouching (TransitioningTo Standing)  `shouldBe` "crouching-to-standing"
            stateKey Crawling  (TransitioningTo Collapsed) `shouldBe` "crawling-to-collapsed"

    describe "resolveStateAnim" $ do
        it "uses udStateAnims mapping when present" $
            resolveStateAnim
                (mkDef (HM.fromList [("standing-idle", "idle-standing")]))
                "standing-idle" `shouldBe` "idle-standing"
        it "falls back to the state key itself when no mapping exists" $
            resolveStateAnim (mkDef HM.empty) "standing-idle" `shouldBe` "standing-idle"
        it "doesn't apply the mapping to unrelated keys" $
            resolveStateAnim
                (mkDef (HM.fromList [("standing-idle", "idle-standing")]))
                "standing-walk" `shouldBe` "standing-walk"

    describe "chooseAnim (override vs death precedence)" $ do
        it "a non-empty override wins over the state animation" $
            chooseAnim Standing "combat-idle" "standing-idle"
              `shouldBe` "combat-idle"
        it "an empty override falls through to the state animation" $
            chooseAnim Standing "" "standing-walk" `shouldBe` "standing-walk"
        it "a Dead unit shows its death animation even with an override set" $
            -- The regression: an acolyte killed mid-swing kept its combat
            -- override and never showed the death anim. Death wins.
            chooseAnim Dead "combat-idle" "dead-idle" `shouldBe` "dead-idle"
        it "a Dead unit with no override still shows the death animation" $
            chooseAnim Dead "" "dead-idle" `shouldBe` "dead-idle"
