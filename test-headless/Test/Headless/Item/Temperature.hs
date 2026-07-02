{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Pure tests for Item.Temperature (#344): Newtonian relaxation of a
--   tracked item temperature toward ambient, mass-scaled time constant,
--   snap-to-untracked on arrival, and container-contents recursion.
module Test.Headless.Item.Temperature (spec) where

import UPrelude
import Test.Hspec
import Item.Temperature (coolItem, effectiveItemTemp, hasTrackedTemp
                        , itemTempSnapEpsilon, itemTempTauPerKg)
import Item.Types (ItemInstance(..), ItemManager, emptyItemManager)

-- | A minimal instance — no defs needed: itemTotalWeight with an empty
--   manager is iiWeight + fill (1 kg/L fallback) + contents.
mkItem ∷ Float → Maybe Float → ItemInstance
mkItem wght mTemp = ItemInstance
    { iiDefName     = "test_item"
    , iiCurrentFill = 0
    , iiQuality     = 100
    , iiCondition   = 100
    , iiWeight      = wght
    , iiSharpness   = 100
    , iiContents    = []
    , iiInstanceId  = 1
    , iiTemp        = mTemp
    }

im ∷ ItemManager
im = emptyItemManager

spec ∷ Spec
spec = do
    describe "effectiveItemTemp" $ do
        it "reads ambient when untracked" $
            effectiveItemTemp 18 (mkItem 1 Nothing) `shouldBe` 18
        it "reads the tracked value when present" $
            effectiveItemTemp 18 (mkItem 1 (Just 95)) `shouldBe` 95

    describe "hasTrackedTemp" $ do
        it "is False for a plain untracked item" $
            hasTrackedTemp (mkItem 1 Nothing) `shouldBe` False
        it "sees a tracked item nested in an untracked container" $ do
            let kit = (mkItem 1 Nothing)
                    { iiContents = [mkItem 0.3 (Just 90)] }
            hasTrackedTemp kit `shouldBe` True

    describe "coolItem" $ do
        it "leaves an untracked item untouched" $ do
            let it0 = mkItem 1 Nothing
            coolItem im 20 1000 it0 `shouldBe` it0

        it "moves a hot item toward ambient without overshooting" $ do
            let it0 = mkItem 1 (Just 100)
            case iiTemp (coolItem im 20 600 it0) of
                Just t  → t `shouldSatisfy` (\v → v > 20 ∧ v < 100)
                Nothing → expectationFailure "snapped too early"

        it "warms a cold item toward ambient" $ do
            let it0 = mkItem 1 (Just (-30))
            case iiTemp (coolItem im 20 600 it0) of
                Just t  → t `shouldSatisfy` (\v → v > (-30) ∧ v < 20)
                Nothing → expectationFailure "snapped too early"

        it "a longer step closes more of the gap" $ do
            let it0 = mkItem 1 (Just 100)
                after dt = fromMaybe 20 (iiTemp (coolItem im 20 dt it0))
            after 1200 `shouldSatisfy` (< after 300)

        it "a heavier item cools slower" $ do
            let after w = fromMaybe 20
                    (iiTemp (coolItem im 20 600 (mkItem w (Just 100))))
            after 0.5 `shouldSatisfy` (< after 5.0)

        it "snaps to untracked once within epsilon of ambient" $ do
            -- One tau closes ~63% of the gap; 20 taus close it to
            -- ~2e-9 of 80 °C — far inside the snap epsilon.
            let tau = itemTempTauPerKg * 1.0
            iiTemp (coolItem im 20 (20 * tau) (mkItem 1 (Just 100)))
                `shouldBe` Nothing

        it "an enormous step is stable (lands at ambient, no overshoot)" $
            iiTemp (coolItem im 20 1e9 (mkItem 1 (Just 100)))
                `shouldBe` Nothing

        it "cools tracked contents inside an untracked container" $ do
            let kit = (mkItem 1 Nothing)
                    { iiContents = [mkItem 0.3 (Just 90)] }
                kit' = coolItem im 20 300 kit
            iiTemp kit' `shouldBe` Nothing
            case iiContents kit' of
                [c] → case iiTemp c of
                    Just t  → t `shouldSatisfy` (\v → v > 20 ∧ v < 90)
                    Nothing → expectationFailure "content snapped too early"
                _ → expectationFailure "contents list changed shape"

        it "keeps a nearly-equilibrated item within snap epsilon of ambient" $ do
            let it0 = mkItem 1 (Just (20 + itemTempSnapEpsilon * 2))
            case iiTemp (coolItem im 20 60 it0) of
                Just t  → abs (t - 20) `shouldSatisfy`
                              (< itemTempSnapEpsilon * 2)
                Nothing → pure ()   -- snapping here is also correct
