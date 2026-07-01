{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure tests for the pan-margin quad-cache invalidation (#447).
--   The contract under test: 'cameraChanged' allows exactly the camera
--   travel whose coverage 'expandViewBounds' + 'quadCacheMargins'
--   baked into the cached tile pass — pans inside the margin reuse the
--   cache AND still have every visible tile inside the built bounds;
--   pans beyond it invalidate.
module Test.Headless.Render.PanMargin (spec) where

import UPrelude
import Test.Hspec
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..), defaultCamera)
import Engine.Graphics.Viewport (safeAspect)
import World.Types (WorldCameraSnapshot(..))
import World.Grid (tileWidth)
import World.Render.Camera (camEpsilon, quadCacheMargins, cameraChanged)
import World.Render.ViewBounds (ViewBounds(..), computeViewBounds, expandViewBounds)

testZoom ∷ Float
testZoom = 1.2

testFb ∷ (Int, Int)
testFb = (1920, 1080)

snapAt ∷ (Float, Float) → WorldCameraSnapshot
snapAt p = WorldCameraSnapshot
    { wcsPosition = p
    , wcsZoom     = testZoom
    , wcsZSlice   = 30
    , wcsFbSize   = testFb
    , wcsFacing   = FaceSouth
    }

-- | A hair inside float-comparison noise, well under any margin.
nudge ∷ Float
nudge = 0.001

spec ∷ Spec
spec = do
    let s0       = snapAt (0, 0)
        (mX, mY) = quadCacheMargins s0

    describe "quadCacheMargins" $ do
        it "scales with zoom and framebuffer aspect" $ do
            let aspect = safeAspect (fst testFb) (snd testFb)
            mY `shouldBe` 0.25 * testZoom
            mX `shouldBe` 0.25 * testZoom * aspect

    describe "cameraChanged" $ do
        it "tolerates a pan up to margin + epsilon per axis" $ do
            cameraChanged s0 (snapAt (mX + camEpsilon - nudge, 0)) `shouldBe` False
            cameraChanged s0 (snapAt (0, mY + camEpsilon - nudge)) `shouldBe` False

        it "invalidates on a pan beyond margin + epsilon" $ do
            cameraChanged s0 (snapAt (mX + camEpsilon + nudge, 0)) `shouldBe` True
            cameraChanged s0 (snapAt (0, -(mY + camEpsilon + nudge))) `shouldBe` True

        it "still invalidates on zoom / zSlice / fbSize / facing changes" $ do
            cameraChanged s0 (s0 { wcsZoom = testZoom + camEpsilon + nudge }) `shouldBe` True
            cameraChanged s0 (s0 { wcsZSlice = 31 }) `shouldBe` True
            cameraChanged s0 (s0 { wcsFbSize = (800, 600) }) `shouldBe` True
            cameraChanged s0 (s0 { wcsFacing = FaceEast }) `shouldBe` True

    describe "expandViewBounds" $ do
        it "widens each side by the per-axis margin" $ do
            let vb  = ViewBounds { vbLeft = -1, vbRight = 1, vbTop = -2, vbBottom = 2 }
                vb' = expandViewBounds (0.5, 0.25) vb
            vbLeft vb'   `shouldBe` (-1.5)
            vbRight vb'  `shouldBe` 1.5
            vbTop vb'    `shouldBe` (-2.25)
            vbBottom vb' `shouldBe` 2.25

    describe "coverage/invalidation pairing" $ do
        -- The cached pass builds bounds at s0 expanded by the margins;
        -- the LARGEST pan cameraChanged tolerates must still keep every
        -- tile overlapping the true viewport inside those bounds. A
        -- tile overlaps the viewport when its drawX is within tileWidth
        -- left of the viewport edge, hence the tileWidth term.
        it "largest tolerated pan keeps the true viewport inside built bounds" $ do
            let cam0 = defaultCamera { camPosition = (0, 0)
                                     , camZoom = testZoom
                                     , camZSlice = 30 }
                built = expandViewBounds (quadCacheMargins s0) $
                            computeViewBounds cam0 (fst testFb) (snd testFb) 25
                aspect = safeAspect (fst testFb) (snd testFb)
                halfW  = testZoom * aspect
                dx     = mX + camEpsilon - nudge   -- tolerated per cameraChanged
                dy     = mY + camEpsilon - nudge
            -- each edge checked against the pan direction that stresses
            -- it: left/top against a negative pan, right/bottom positive
            vbLeft built  `shouldSatisfy` (≤ -dx - halfW - tileWidth)
            vbRight built `shouldSatisfy` (≥ dx + halfW)
            -- vertical (padY also carries tile height + depth padding,
            -- so this holds with slack)
            vbTop built    `shouldSatisfy` (≤ -dy - testZoom - tileWidth)
            vbBottom built `shouldSatisfy` (≥ dy + testZoom)
