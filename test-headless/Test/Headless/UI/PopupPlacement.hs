{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | #747 gate: the shared viewport-aware floating-placement contract
--   (Phase C child C1 of #741) — "UI.PopupPlacement" is pure (no
--   Vulkan/window/Lua engine), so placement is fully Hspec-testable in
--   isolation, mirroring "UI.InputOwnership"/"UI.Clipping". Tooltip
--   placement deliberately stays on its own separate implementation
--   ("UI.Tooltip.Layout"/"UI.Tooltip.Render", untouched by this
--   change) — its own @Test.Headless.UI.Tooltip@ suite passing
--   unmodified is the regression proof for "existing tooltip
--   placement may remain separate but cannot regress".
module Test.Headless.UI.PopupPlacement (spec) where

import UPrelude
import Test.Hspec
import UI.PopupPlacement

-- | A 1280x720 framebuffer — a typical desktop resolution, used as the
--   default budget for tests that aren't specifically exercising edge/
--   corner clamping.
fb ∷ (Float, Float)
fb = (1280, 720)

mkReq ∷ (Float, Float) → (Float, Float) → (Float, Float) → PopupDirection → (Float, Float) → PlacementRequest
mkReq (ax, ay) (aw, ah) (cw, ch) dir (fbW, fbH) = PlacementRequest
    { prAnchorX = ax, prAnchorY = ay, prAnchorW = aw, prAnchorH = ah
    , prContentW = cw, prContentH = ch, prPreferred = dir
    , prFramebufferW = fbW, prFramebufferH = fbH
    }

spec ∷ Spec
spec = do
    describe "placePopup — preferred direction / opposite-direction fallback" $ do
        it "PopupBelow: opens directly below the anchor when there's room" $
            let req = mkReq (100, 100) (50, 20) (80, 60) PopupBelow fb
                p = placePopup req
            in (plX p, plY p, plFlipped p) `shouldBe` (100, 120, False)

        it "PopupBelow: flips above when there's no room below" $
            let req = mkReq (100, 690) (50, 20) (80, 60) PopupBelow fb
                p = placePopup req
            in do
                plFlipped p `shouldBe` True
                plY p `shouldBe` 630  -- anchorY(690) - contentH(60)

        it "PopupAbove: opens directly above when there's room" $
            let req = mkReq (100, 200) (50, 20) (80, 60) PopupAbove fb
                p = placePopup req
            in (plY p, plFlipped p) `shouldBe` (140, False)

        it "PopupAbove: flips below when there's no room above" $
            let req = mkReq (100, 10) (50, 20) (80, 60) PopupAbove fb
                p = placePopup req
            in do
                plFlipped p `shouldBe` True
                plY p `shouldBe` 30  -- anchorY(10) + anchorH(20)

        it "PopupRight: opens directly right when there's room (submenu preference)" $
            let req = mkReq (100, 100) (50, 0) (80, 60) PopupRight fb
                p = placePopup req
            in (plX p, plY p, plFlipped p) `shouldBe` (150, 100, False)

        it "PopupRight: flips left when there's no room on the right (submenu flip)" $
            let req = mkReq (1200, 100) (50, 0) (80, 60) PopupRight fb
                p = placePopup req
            in do
                plFlipped p `shouldBe` True
                plX p `shouldBe` 1120  -- anchorX(1200) - contentW(80)

        it "PopupLeft: opens directly left when there's room" $
            let req = mkReq (500, 100) (50, 0) (80, 60) PopupLeft fb
                p = placePopup req
            in (plX p, plFlipped p) `shouldBe` (420, False)

        it "PopupLeft: flips right when there's no room on the left" $
            let req = mkReq (30, 100) (50, 0) (80, 60) PopupLeft fb
                p = placePopup req
            in do
                plFlipped p `shouldBe` True
                plX p `shouldBe` 80  -- anchorX(30) + anchorW(50)

        it "PopupAnchored: places exactly at the anchor point, never flips" $
            let req = mkReq (300, 300) (0, 0) (80, 60) PopupAnchored fb
                p = placePopup req
            in (plX p, plY p, plFlipped p) `shouldBe` (300, 300, False)

    describe "placePopup — submenu vertical clamp (never flipped on the non-preferred axis)" $
        it "a horizontal-direction popup's Y is clamped, not flipped, even when it would overflow" $
            let req = mkReq (100, 690) (50, 0) (80, 60) PopupRight fb
                p = placePopup req
            in do
                plY p `shouldBe` 660  -- clamped to fbH(720) - contentH(60), NOT flipped
                plFlipped p `shouldBe` False  -- flip tracks the RIGHT/LEFT decision only

    describe "placePopup — edges and corners" $ do
        it "clamps a below-preferred popup horizontally at the left edge" $
            let req = mkReq (-30, 100) (50, 20) (80, 60) PopupBelow fb
                p = placePopup req
            in plX p `shouldBe` 0

        it "clamps a below-preferred popup horizontally at the right edge" $
            let req = mkReq (1250, 100) (50, 20) (80, 60) PopupBelow fb
                p = placePopup req
            in plX p `shouldBe` 1200  -- fbW(1280) - contentW(80)

        it "clamps fully into the top-left corner when the anchor sits off both edges" $
            let req = mkReq (-40, -40) (50, 20) (80, 60) PopupAnchored fb
                p = placePopup req
            in (plX p, plY p) `shouldBe` (0, 0)

        it "clamps fully into the bottom-right corner when the anchor sits off both edges" $
            let req = mkReq (1300, 750) (50, 20) (80, 60) PopupAnchored fb
                p = placePopup req
            in (plX p, plY p) `shouldBe` (1280 - 80, 720 - 60)

        it "degenerate case: content larger than the framebuffer still clamps to the edge, not negative" $
            let req = mkReq (10, 10) (0, 0) (2000, 60) PopupAnchored fb
                p = placePopup req
            in plX p `shouldBe` 0

    describe "placePopup — full interactive size includes the scrollbar strip" $
        it "a wider contentW (display + scrollbar) changes the clamp outcome vs. the display alone" $
            let displayOnly = mkReq (1220, 100) (50, 20) (60, 200) PopupBelow fb
                withScrollbar = mkReq (1220, 100) (50, 20) (84, 200) PopupBelow fb  -- +24px scrollbar
                pDisplay = placePopup displayOnly
                pScroll  = placePopup withScrollbar
            in do
                plX pDisplay `shouldBe` 1220          -- fits fully on screen unclamped
                plX pScroll `shouldBe` (1280 - 84)     -- now clamped to keep the scrollbar reachable

    describe "fitVisibleRows — oversized dropdown row reduction" $ do
        it "keeps the preferred count when it fits" $
            fitVisibleRows 8 36 400 `shouldBe` 8

        it "reduces to however many rows fit when the preferred count doesn't" $
            fitVisibleRows 8 36 100 `shouldBe` 2  -- floor(100/36) = 2

        it "never returns fewer than 1 row even in a degenerate (near-zero) space" $
            fitVisibleRows 8 36 5 `shouldBe` 1

        it "returns 0 for a non-positive preferred count (nothing to show)" $
            fitVisibleRows 0 36 400 `shouldBe` 0

    describe "placePopup — resize recomputes cleanly (pure function, nothing cached)" $
        it "the same anchor/content placed against a smaller framebuffer reflows correctly" $
            let wide = mkReq (700, 100) (50, 20) (80, 60) PopupBelow (1280, 720)
                narrow = mkReq (700, 100) (50, 20) (80, 60) PopupBelow (720, 720)
                pWide = placePopup wide
                pNarrow = placePopup narrow
            in do
                plX pWide `shouldBe` 700       -- fits at 1280 wide
                plX pNarrow `shouldBe` (720 - 80) -- clamped once the framebuffer shrinks

    describe "placePopup — final placement is what both paint and hit-testing would use" $
        it "the resolved rect is always fully within the framebuffer when content fits at all" $
            let req = mkReq (50, 50) (20, 20) (100, 50) PopupBelow fb
                p = placePopup req
            in do
                plX p `shouldSatisfy` (≥ 0)
                plY p `shouldSatisfy` (≥ 0)
                (plX p + 100) `shouldSatisfy` (≤ 1280)
                (plY p + 50) `shouldSatisfy` (≤ 720)

    describe "placePopup — scale-invariant geometry (UI scale is applied by callers in pixels)" $
        it "uniformly scaling every input by the same factor scales the result identically" $
            let base = mkReq (100, 100) (50, 20) (80, 60) PopupBelow (1280, 720)
                scaled = mkReq (200, 200) (100, 40) (160, 120) PopupBelow (2560, 1440)
                pBase = placePopup base
                pScaled = placePopup scaled
            in do
                plX pScaled `shouldBe` plX pBase * 2
                plY pScaled `shouldBe` plY pBase * 2
                plFlipped pScaled `shouldBe` plFlipped pBase
