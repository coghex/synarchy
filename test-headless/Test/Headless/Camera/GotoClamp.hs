{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure tests for the camera.gotoTile glacier clamp (issue #297).
--
--   The bug: @camera.gotoTile@ teleported the camera by writing camPosition
--   directly with no world-bounds clamp, so a target near the world's
--   diagonal corner parked the camera at the v-edge rim. gotoTile forces a
--   zoomed-in level that DOES load chunks (unlike panning over the rim at
--   world-map zoom), and generating a rim chunk heap-overflowed the world
--   thread permanently.
--
--   The fix fences the teleport target through 'applyGotoLimits', whose
--   buffer ('cameraGotoBufferChunks') keeps not just the camera but the
--   region the loader pulls in around it ('chunkLoadRadius' past the camera)
--   strictly inside the rim. These assertions pin that the fence is more
--   conservative than the pan path's and that the outermost loaded chunk
--   stays interior. The clamp is pure — no engine needed.
module Test.Headless.Camera.GotoClamp (spec) where

import UPrelude
import Test.Hspec
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Loop.Camera (applyGotoLimits, cameraYLimitChunks
                          , cameraGotoBufferChunks)
import World.Chunk.Types (chunkSize)
import World.Generate.Constants (chunkLoadRadius)
import World.Grid (tileHalfDiamondHeight)

-- World sizes (in chunks) to exercise.
worldSizes ∷ [Int]
worldSizes = [64, 128, 256]

facings ∷ [CameraFacing]
facings = [FaceSouth, FaceNorth, FaceWest, FaceEast]

-- The v-axis component (the one the rim clamp acts on) for a given facing.
clampedV ∷ CameraFacing → (Float, Float) → Float
clampedV FaceSouth (_, y) = y
clampedV FaceNorth (_, y) = y
clampedV FaceWest  (x, _) = x
clampedV FaceEast  (x, _) = x

spec ∷ Spec
spec = do
    describe "camera.gotoTile glacier clamp (#297)" $ do

        it "fences teleports harder than the pan/drag path" $
            -- The pan path uses a 2-chunk buffer; gotoTile must use more,
            -- because it forces a zoomed-in level that actually loads chunks.
            (cameraGotoBufferChunks > 2) `shouldBe` True

        it "keeps the outermost loaded chunk strictly inside the v-edge rim" $
            -- The camera is fenced to (halfSize - buffer) v-chunks; the loader
            -- pulls chunkLoadRadius past that. That outermost loaded chunk must
            -- stay strictly interior, or generating it heap-overflows the world
            -- thread (the #297 crash).
            forM_ worldSizes $ \ws → do
                let halfSize        = ws `div` 2
                    loadedMaxVChunk = (halfSize - cameraGotoBufferChunks)
                                        + chunkLoadRadius
                (loadedMaxVChunk < halfSize) `shouldBe` True

        it "clamps an out-of-bounds rim target onto the fence" $
            forM_ worldSizes $ \ws → forM_ facings $ \f → do
                let lim = cameraYLimitChunks cameraGotoBufferChunks ws
                    far = 1.0e6
                    out = applyGotoLimits ws f far far
                abs (clampedV f out) `shouldSatisfy`
                    (\v → v <= lim + 1.0e-3 ∧ v >= lim - 1.0e-3)

        it "leaves a deep-interior target unchanged (identity)" $
            forM_ worldSizes $ \ws → forM_ facings $ \f →
                applyGotoLimits ws f 3.0 5.0 `shouldBe` (3.0, 5.0)

        it "the fence sits inside the true rim" $
            forM_ worldSizes $ \ws → do
                let halfTiles = fromIntegral ((ws * chunkSize) `div` 2)
                                  * tileHalfDiamondHeight
                    lim = cameraYLimitChunks cameraGotoBufferChunks ws
                (lim < halfTiles) `shouldBe` True
