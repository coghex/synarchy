{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Pure tests for Unit.Anim — the sim-state → anim-name resolver.
module Test.Headless.Unit.Anim (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Engine.Asset.Handle (TextureHandle(..))
import Unit.Anim
import Unit.Sim.Types (UnitActivity(..))
import Unit.Types

mkDef ∷ HM.HashMap Text Text → UnitDef
mkDef stateAnims = UnitDef
    { udName       = "t"
    , udTexture    = TextureHandle 0
    , udDirSprites = Map.empty
    , udBaseWidth  = 0
    , udAnimations = HM.empty
    , udStateAnims = stateAnims
    }

spec ∷ Spec
spec = do
    describe "activityToStateKey" $ do
        it "maps Idle to \"idle\"" $
            activityToStateKey Idle `shouldBe` "idle"
        it "maps Walking to \"walk\"" $
            activityToStateKey Walking `shouldBe` "walk"

    describe "resolveStateAnim" $ do
        it "uses udStateAnims mapping when present" $
            resolveStateAnim
                (mkDef (HM.fromList [("idle", "breathing-idle")]))
                "idle" `shouldBe` "breathing-idle"
        it "falls back to the state key itself when no mapping exists" $
            resolveStateAnim (mkDef HM.empty) "idle" `shouldBe` "idle"
        it "doesn't apply the mapping to unrelated keys" $
            resolveStateAnim
                (mkDef (HM.fromList [("idle", "breathing-idle")]))
                "walk" `shouldBe` "walk"
