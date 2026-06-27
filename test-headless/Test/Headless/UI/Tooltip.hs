{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Locked-tooltip click-swallow contract. The bug under test (#117):
--   a locked tooltip only swallowed clicks when it had a textured
--   background box. For a style with no box textures the panel's bounds
--   collapsed to nothing, so clicks on the visible locked tooltip fell
--   through and cleared the lock.
--
--   The fix makes the tooltip panel's geometry element ALWAYS present
--   while a tooltip is shown — an invisible 'RenderNone' element when the
--   style is boxless — so 'isPointInLockedTooltip' reports the real panel
--   rectangle either way. These specs exercise that predicate against a
--   manager assembled the way 'rebuildVisuals' now assembles it.
module Test.Headless.UI.Tooltip (spec) where

import UPrelude
import Test.Hspec
import UI.Types
import UI.Manager (createElement, setElementPosition)
import UI.Tooltip (isPointInLockedTooltip)

-- | A manager holding a single tooltip panel geometry element at the
--   given rectangle (the invisible 'RenderNone' element rebuildVisuals
--   creates for a boxless style), with the tooltip locked or not.
withPanel ∷ Bool → (Float, Float, Float, Float) → UIPageManager
withPanel locked (x, y, w, h) =
    let page          = PageHandle 1
        (handle, mgr) = createElement "__tooltip_bg" w h page emptyUIPageManager
        mgr'          = setElementPosition handle x y mgr
        tts           = (upmTooltip mgr')
                          { ttsBoxHandle    = Just handle
                          , ttsActiveContent = Just
                              (TooltipContent (Just "hi") Nothing [] Nothing)
                          , ttsLocked       = locked
                          }
    in mgr' { upmTooltip = tts }

spec ∷ Spec
spec = do
    let rect = (100, 100, 80, 30)  -- x y w h

    describe "isPointInLockedTooltip (boxless panel — #117)" $ do
        it "swallows a click inside the panel of a boxless locked tooltip" $
            isPointInLockedTooltip (120, 110) (withPanel True rect)
                `shouldBe` True

        it "swallows clicks at the panel edges (inclusive bounds)" $ do
            isPointInLockedTooltip (100, 100) (withPanel True rect) `shouldBe` True
            isPointInLockedTooltip (180, 130) (withPanel True rect) `shouldBe` True

        it "does not swallow a click outside the panel" $ do
            isPointInLockedTooltip (90, 110)  (withPanel True rect) `shouldBe` False
            isPointInLockedTooltip (120, 140) (withPanel True rect) `shouldBe` False

    describe "isPointInLockedTooltip (lock gating)" $ do
        it "never swallows when the tooltip is not locked" $
            isPointInLockedTooltip (120, 110) (withPanel False rect)
                `shouldBe` False

        it "never swallows when there is no tooltip panel element" $
            isPointInLockedTooltip (120, 110)
                (emptyUIPageManager
                  { upmTooltip = (upmTooltip emptyUIPageManager)
                      { ttsLocked = True } })
                `shouldBe` False
