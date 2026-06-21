{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Pure tests for Unit.Injury — the shared, fully tissue-driven
--   injury vocabulary that keeps combat and falls speaking the same
--   language (a broken bone is a "fracture" no matter how it happened;
--   neural tissue concusses; an opened artery bleeds).
module Test.Headless.Unit.Injury (spec) where

import UPrelude
import Test.Hspec
import Unit.Injury

spec ∷ Spec
spec = do
    describe "tissueInjuryKind (tissue-driven, body-plan agnostic)" $ do
        it "structural tissue fractures, regardless of mechanism" $ do
            tissueInjuryKind "bone"      "blunt" `shouldBe` Just "fracture"
            tissueInjuryKind "bone"      "slash" `shouldBe` Just "fracture"
            tissueInjuryKind "cartilage" "blunt" `shouldBe` Just "fracture"

        it "neural tissue (brain/cord) concusses — not tied to 'the head'" $
            tissueInjuryKind "nerve" "blunt" `shouldBe` Just "concussion"
        it "an organ takes internal trauma" $
            tissueInjuryKind "organ" "stab" `shouldBe` Just "internal"
        it "an artery bleeds (arterial)" $
            tissueInjuryKind "artery" "slash" `shouldBe` Just "arterial"

        it "soft tissue takes the mechanism's surface wound" $ do
            tissueInjuryKind "flesh" "blunt" `shouldBe` Just "blunt"
            tissueInjuryKind "flesh" "slash" `shouldBe` Just "slash"
            tissueInjuryKind "flesh" "stab"  `shouldBe` Just "stab"

        it "worn armour (non-tissue) is never wounded" $ do
            tissueInjuryKind "steel" "slash" `shouldBe` Nothing
            tissueInjuryKind "wool"  "blunt" `shouldBe` Nothing
            tissueInjuryKind ""      "blunt" `shouldBe` Nothing

        it "a combat-broken bone and a fall-broken bone are the SAME kind" $
            tissueInjuryKind "bone" "blunt" `shouldBe` tissueInjuryKind "bone" "blunt"

    describe "isBodyTissue" $
        it "separates living tissue from armour" $ do
            isBodyTissue "bone"   `shouldBe` True
            isBodyTissue "nerve"  `shouldBe` True
            isBodyTissue "artery" `shouldBe` True
            isBodyTissue "steel"  `shouldBe` False

    describe "capInjurySeverity" $ do
        it "caps a bruise shallow (never lethal)" $
            capInjurySeverity "blunt" 1.5 `shouldBe` bruiseCap
        it "lets structural injuries reach the lethal ceiling" $
            capInjurySeverity "fracture" 2.0 `shouldBe` maxInjurySeverity
