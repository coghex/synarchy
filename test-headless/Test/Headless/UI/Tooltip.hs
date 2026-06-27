{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Locked-tooltip click-swallow contract. The bug under test (#117):
--   a locked tooltip only swallowed clicks when its style had a textured
--   background box. 'rebuildVisuals' created the panel geometry element
--   (the source of the swallow region) ONLY when a box-texture set was
--   configured, so for a boxless style 'ttsBoxHandle' stayed Nothing and
--   the panel bounds collapsed to nothing — clicks on the visible locked
--   tooltip fell through and cleared the lock.
--
--   These specs drive the real 'rebuildVisuals' path with a boxless
--   style (the default style has no box textures) so they exercise the
--   exact branch that was broken: pre-fix, 'ttsBoxHandle' would be
--   Nothing here and 'isPointInLockedTooltip' would be False even at the
--   panel centre.
module Test.Headless.UI.Tooltip (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Map as Map
import UI.Types
import UI.Tooltip (rebuildVisuals, isPointInLockedTooltip)
import Engine.Graphics.Font.Data (defaultFontCache)
import Engine.Asset.Handle (FontHandle(..))

-- | A boxless tooltip style: a font is set (so the panel sizes to wrap
--   real text) but no box-texture set is configured — the #117 case.
boxlessStyle ∷ TooltipStyle
boxlessStyle = defaultTooltipStyle { tsFont = FontHandle 1 }

-- | Build + lock a tooltip the way the engine does: seed the manager
--   with the boxless style, run 'rebuildVisuals' (which is what creates
--   the panel geometry element), then lock it.
lockedTooltip ∷ TooltipContent → UIPageManager
lockedTooltip content =
    let page = PageHandle 1
        mgr0 = emptyUIPageManager
                 { upmTooltip = (upmTooltip emptyUIPageManager)
                     { ttsStyle = boxlessStyle } }
        mgr1 = rebuildVisuals page content defaultFontCache mgr0
        tts  = (upmTooltip mgr1)
                 { ttsLocked        = True
                 , ttsActiveContent = Just content }
    in mgr1 { upmTooltip = tts }

-- | (x, y, w, h) of the panel geometry element 'rebuildVisuals' created.
panelRect ∷ UIPageManager → Maybe (Float, Float, Float, Float)
panelRect mgr = do
    h    ← ttsBoxHandle (upmTooltip mgr)
    elem ← Map.lookup h (upmElements mgr)
    let (x, y) = uePosition elem
        (w, s) = ueSize elem
    pure (x, y, w, s)

spec ∷ Spec
spec = do
    let content = TooltipContent (Just "hello") Nothing [] Nothing
        mgr     = lockedTooltip content

    describe "rebuildVisuals (boxless style — #117)" $ do
        it "creates a panel geometry element even with no box textures" $
            ttsBoxHandle (upmTooltip mgr) `shouldSatisfy` isJust

        it "gives the panel a non-degenerate (positive-area) rectangle" $
            case panelRect mgr of
                Just (_, _, w, h) → (w > 0 ∧ h > 0) `shouldBe` True
                Nothing           → expectationFailure "no panel element"

    describe "isPointInLockedTooltip (boxless locked tooltip — #117)" $ do
        it "swallows a click at the centre of the boxless panel" $
            case panelRect mgr of
                Just (x, y, w, h) →
                    isPointInLockedTooltip (x + w/2, y + h/2) mgr `shouldBe` True
                Nothing → expectationFailure "no panel element"

        it "does not swallow a click clearly outside the panel" $
            case panelRect mgr of
                Just (x, y, w, h) →
                    isPointInLockedTooltip (x + w + 50, y + h + 50) mgr
                        `shouldBe` False
                Nothing → expectationFailure "no panel element"

    describe "isPointInLockedTooltip (lock gating)" $ do
        it "never swallows when the tooltip is not locked" $
            let unlocked = mgr { upmTooltip = (upmTooltip mgr)
                                   { ttsLocked = False } }
            in case panelRect mgr of
                Just (x, y, w, h) →
                    isPointInLockedTooltip (x + w/2, y + h/2) unlocked
                        `shouldBe` False
                Nothing → expectationFailure "no panel element"
