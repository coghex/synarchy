{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure tests for the zero-size (minimized window / minimize-restore)
--   projection guards introduced for issue #118.
--
--   The bug: view/projection math divides by @winW@, @winH@, or @fbH@
--   with no guard for a minimized / zero-size window or framebuffer, so a
--   minimize/restore transition feeds the aspect ratio and pixel→world
--   normalizations zero divisors and derives @Infinity@/@NaN@ cursor,
--   hover, culling, and pick values.
--
--   No engine needed: the guarded helpers and the cursor/culling
--   projection functions are pure. We feed them a zero-size viewport and
--   assert they report "no tile" (Nothing) or keep their bounds finite,
--   and that a normal viewport still resolves a result.
module Test.Headless.Render.ViewportGuard (spec) where

import UPrelude
import Test.Hspec
import Engine.Graphics.Camera (defaultCamera, Camera2D(..), CameraFacing(..))
import Engine.Graphics.Viewport (windowDegenerate, viewportDegenerate, safeAspect)
import World.Render.ViewBounds (ViewBounds(..), computeViewBounds)
import World.Render.Zoom.ViewBounds (ZoomViewBounds(..), computeZoomViewBounds)
import World.Render.Zoom.Cursor (pixelToChunkOrigin)

-- | All four corners of a view-bounds rect are finite (no Infinity/NaN).
finiteViewBounds ∷ ViewBounds → Bool
finiteViewBounds vb = all ok [vbLeft vb, vbRight vb, vbTop vb, vbBottom vb]
  where ok x = not (isInfinite x ∨ isNaN x)

finiteZoomBounds ∷ ZoomViewBounds → Bool
finiteZoomBounds vb = all ok [zvLeft vb, zvRight vb, zvTop vb, zvBottom vb]
  where ok x = not (isInfinite x ∨ isNaN x)

spec ∷ Spec
spec = do
    describe "viewport zero-size predicates" $ do
        it "windowDegenerate flags any non-positive window dimension" $ do
            windowDegenerate 0 600    `shouldBe` True
            windowDegenerate 800 0    `shouldBe` True
            windowDegenerate 0 0      `shouldBe` True
            windowDegenerate 800 600  `shouldBe` False

        it "viewportDegenerate also flags a zero framebuffer height" $ do
            viewportDegenerate 800 600 0   `shouldBe` True
            viewportDegenerate 0 600 720   `shouldBe` True
            viewportDegenerate 800 600 720 `shouldBe` False

        it "safeAspect returns 1 on a zero framebuffer height, ratio otherwise" $ do
            safeAspect 1600 0   `shouldBe` 1.0
            safeAspect 0 0      `shouldBe` 1.0
            safeAspect 1600 800 `shouldBe` 2.0

    describe "computeViewBounds under a zero-size framebuffer" $ do
        it "stays finite (no Infinity/NaN culling bounds)" $
            finiteViewBounds (computeViewBounds defaultCamera 0 0 8) `shouldBe` True
        it "is still finite for a normal framebuffer" $
            finiteViewBounds (computeViewBounds defaultCamera 1600 900 8) `shouldBe` True

    describe "computeZoomViewBounds under a zero-size framebuffer" $
        it "stays finite (no Infinity/NaN culling bounds)" $
            finiteZoomBounds (computeZoomViewBounds defaultCamera 1600 0) `shouldBe` True

    describe "pixelToChunkOrigin under a zero-size viewport" $ do
        let cam = defaultCamera { camPosition = (0, 0) }
        it "reports no chunk when the window is zero-size" $
            pixelToChunkOrigin FaceSouth cam 0 0 0 0 8 100 100
                `shouldBe` Nothing
        it "reports no chunk when only the framebuffer height is zero" $
            pixelToChunkOrigin FaceSouth cam 800 600 1600 0 8 400 300
                `shouldBe` Nothing
        it "resolves a chunk origin for a normal viewport at screen center" $
            pixelToChunkOrigin FaceSouth cam 800 600 1600 1200 8 400 300
                `shouldSatisfy` (/= Nothing)
