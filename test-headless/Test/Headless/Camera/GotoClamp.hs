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
                          , cameraGotoBufferChunks, gotoTileZoomSafe)
import World.Chunk.Types (chunkSize)
import World.Generate.Constants (chunkLoadRadius)
import World.Grid (tileHalfDiamondHeight)

-- World sizes (in chunks) to exercise. 8 is the smallest supported world
-- (World.Generate.Config.minimumWorldSize) — the case where an unbounded goto
-- buffer would drive the limit negative and invert the clamp.
worldSizes ∷ [Int]
worldSizes = [8, 16, 64, 128, 256]

facings ∷ [CameraFacing]
facings = [FaceSouth, FaceNorth, FaceWest, FaceEast]

-- The v-axis component (the one the rim clamp acts on) for a given facing.
clampedV ∷ CameraFacing → (Float, Float) → Float
clampedV FaceSouth (_, y) = y
clampedV FaceNorth (_, y) = y
clampedV FaceWest  (x, _) = x
clampedV FaceEast  (x, _) = x

-- The camera v-chunk a rim-ward teleport actually lands on for a given world
-- size, derived from the clamp output (so it reflects the effective, possibly
-- small-world-capped, buffer rather than the raw constant).
clampedCamVChunk ∷ Int → Int
clampedCamVChunk ws =
    let (_, y) = applyGotoLimits ws FaceSouth 1.0e6 1.0e6
    in round (y / tileHalfDiamondHeight / fromIntegral chunkSize)

spec ∷ Spec
spec = do
    describe "camera.gotoTile glacier clamp (#297)" $ do

        it "fences teleports at least as hard as the pan/drag path" $
            -- The pan path uses a 2-chunk buffer; gotoTile uses more (it forces
            -- a zoomed-in level that actually loads chunks), so its limit is
            -- never looser than the pan limit on any supported world size.
            forM_ worldSizes $ \ws →
                cameraYLimitChunks cameraGotoBufferChunks ws
                    `shouldSatisfy` (≤ cameraYLimitChunks 2 ws)

        it "never inverts the clamp — limit stays non-negative on every world" $
            -- On the 8-chunk minimum the raw buffer would push the limit
            -- negative (maxRow < 0), inverting the clampF range. The half-size
            -- cap keeps it ≥ 0 (and exactly 0 there — a centre pin).
            forM_ worldSizes $ \ws →
                cameraYLimitChunks cameraGotoBufferChunks ws
                    `shouldSatisfy` (≥ 0)

        it "keeps the outermost loaded chunk strictly inside the v-edge rim" $
            -- The loader pulls chunkLoadRadius past the (clamped) camera; that
            -- outermost loaded chunk must stay strictly interior. Holds even on
            -- the 8-chunk world, where the camera pins to centre and only loads
            -- the same set the initial (centre) view already loaded safely.
            forM_ worldSizes $ \ws → do
                let halfSize = ws `div` 2
                (clampedCamVChunk ws + chunkLoadRadius) `shouldSatisfy` (< halfSize)

        it "clamps an out-of-bounds rim target onto the fence" $
            forM_ worldSizes $ \ws → forM_ facings $ \f → do
                let lim = cameraYLimitChunks cameraGotoBufferChunks ws
                    out = applyGotoLimits ws f 1.0e6 1.0e6
                abs (clampedV f out) `shouldSatisfy`
                    (\v → v ≤ lim + 1.0e-3 ∧ v ≥ lim - 1.0e-3)

        it "leaves a deep-interior target unchanged (identity)" $
            -- 0.3 screen units sits inside the fence for every world big enough
            -- to have a non-zero fence (the 8-chunk world pins to centre, so
            -- nothing but the origin is interior there).
            forM_ (filter (\ws → ws ≥ 16) worldSizes) $ \ws → forM_ facings $ \f →
                applyGotoLimits ws f 0.3 0.3 `shouldBe` (0.3, 0.3)

        it "only allows tile-level zoom where the loader stays clear of the rim" $ do
            -- The 8-chunk minimum has no safe zoomed-in region (a centred load
            -- still pulls a rim corner chunk), so gotoTile must stay zoomed out
            -- there; every larger supported world is safe.
            gotoTileZoomSafe 8  `shouldBe` False
            forM_ [16, 64, 128, 256] $ \ws →
                gotoTileZoomSafe ws `shouldBe` True

        it "the fence sits inside the true rim" $
            forM_ worldSizes $ \ws → do
                let halfTiles = fromIntegral ((ws * chunkSize) `div` 2)
                                  * tileHalfDiamondHeight
                    lim = cameraYLimitChunks cameraGotoBufferChunks ws
                (lim < halfTiles) `shouldBe` True
