{-# LANGUAGE Strict #-}
-- | Pure tests for the world bounds @camera.goToTile@ retains (#1953).
--
--   History. Issue #297 gave the teleport its own, larger glacier fence
--   because generating a chunk on the glacier rim killed the world
--   thread, and it deferred the mechanism to #298. The defect turned out
--   to live in the SHARED chunk generator — a beyond-glacier neighbour's
--   @minBound@ sentinel sizing a ~2^63-tall strata column — and was
--   repaired there by PR #363, which covers the init-queue loader and
--   the camera-visible loader alike. The teleport-only fence outlived
--   its cause: it clamped four chunks further in than ordinary panning,
--   and on the 8-chunk minimum world it exceeded the half-size, pinning
--   EVERY teleport to the centre in map view with z-tracking off.
--
--   #1953 retired it. @camera.goToTile@ is now fenced by the same
--   'cameraGlacierBufferChunks' boundary the pan and drag paths use,
--   which is a framing rule (the outermost rim band would fill half the
--   screen with the void past the world edge) and not a safety one.
--
--   These examples pin the RETAINED behaviour: the fence is still the
--   pan/drag path's, a rim-ward target lands on it for both glacier
--   directions and all four facings, the cylindrical u-axis is never
--   clamped, and an interior target is the identity — on every supported
--   world size, the 8-chunk minimum included. The clamp is pure, so no
--   engine is needed here; "Test.Headless.Camera.GotoLoad" drives the
--   repaired loader end to end through the real registered API.
module Test.Headless.Camera.GotoClamp (spec) where

import UPrelude
import Test.Hspec
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Loop.Camera (applyLimits, cameraYLimitChunks
                          , cameraGlacierBufferChunks)
import World.Chunk.Types (chunkSize)
import World.Grid (tileHalfDiamondHeight)

-- World sizes (in chunks) to exercise. 8 is the smallest supported world
-- (World.Generate.Config.minimumWorldSize) — the case the retired
-- teleport buffer collapsed to a centre pin.
worldSizes ∷ [Int]
worldSizes = [8, 16, 64, 128, 256]

facings ∷ [CameraFacing]
facings = [FaceSouth, FaceNorth, FaceWest, FaceEast]

-- | The retained bound for a world size: the pan/drag path's own.
limitFor ∷ Int → Float
limitFor = cameraYLimitChunks cameraGlacierBufferChunks

-- | Screen coordinates carrying a given (u, v) pair, where v is the
--   glacier-bounded axis for this facing and u the cylindrical one that
--   wraps. Mirrors 'Engine.Loop.Camera.applyLimits'' own facing split.
axesToScreen ∷ CameraFacing → Float → Float → (Float, Float)
axesToScreen FaceSouth u v = (u, v)
axesToScreen FaceNorth u v = (u, v)
axesToScreen FaceWest  u v = (v, u)
axesToScreen FaceEast  u v = (v, u)

-- The v-axis component (the one the rim clamp acts on) for a facing.
clampedV ∷ CameraFacing → (Float, Float) → Float
clampedV FaceSouth (_, y) = y
clampedV FaceNorth (_, y) = y
clampedV FaceWest  (x, _) = x
clampedV FaceEast  (x, _) = x

-- The u-axis component (the cylinder seam), which must come back
-- untouched however far out of range it is.
wrappingU ∷ CameraFacing → (Float, Float) → Float
wrappingU FaceSouth (x, _) = x
wrappingU FaceNorth (x, _) = x
wrappingU FaceWest  (_, y) = y
wrappingU FaceEast  (_, y) = y

-- | A distinctive u offset, far enough out that a clamp acting on the
--   wrong axis could not return it unchanged.
farU ∷ Float
farU = 987654.0

nearly ∷ Float → Float → Bool
nearly want got = abs (got - want) ≤ 1.0e-3

spec ∷ Spec
spec = do
    describe "camera.gotoTile world bounds (#1953)" $ do

        it "fences teleports on the pan/drag path's own two-chunk buffer" $
            -- #1953 retired the teleport's separate buffer by adopting
            -- this one AS-IS; re-examining the pan/drag boundary itself
            -- is separate work. Changing this value now moves every
            -- camera path at once, which is the point of pinning it.
            cameraGlacierBufferChunks `shouldBe` 2

        it "never inverts the clamp — limit stays non-negative on every world" $
            -- The half-size cap keeps 'cameraYLimitChunks' ≥ 0 for a
            -- buffer wider than the world could hold.
            forM_ worldSizes $ \ws →
                limitFor ws `shouldSatisfy` (≥ 0)

        it "leaves the 8-chunk minimum a real interior to teleport into" $
            -- The retired buffer (chunkLoadRadius + 4 = 6) exceeded this
            -- world's half-size of 4, collapsing the limit to 0 so every
            -- target landed at the centre. Two chunks fit with room to
            -- spare: 4 half-chunks − 2 = 2 chunks = 32 tiles of reach.
            limitFor 8 `shouldBe`
                fromIntegral (2 * chunkSize) * tileHalfDiamondHeight

        it "clamps an out-of-bounds rim target onto the retained bound" $
            -- Both glacier directions, all four facings.
            forM_ worldSizes $ \ws → forM_ facings $ \f →
                forM_ [1.0, -1.0 ∷ Float] $ \sign → do
                    let (x, y) = axesToScreen f farU (sign * 1.0e6)
                        out    = applyLimits ws f x y
                    clampedV f out `shouldSatisfy` nearly (sign * limitFor ws)

        it "never clamps the cylindrical u-axis" $
            -- u wraps rather than ending, so the clamp must not touch it
            -- even when the v-axis is being pulled all the way back.
            forM_ worldSizes $ \ws → forM_ facings $ \f → do
                let (x, y) = axesToScreen f farU 1.0e6
                    out    = applyLimits ws f x y
                wrappingU f out `shouldBe` farU

        it "leaves an interior target unchanged (identity)" $
            -- 0.3 screen units sits inside the fence on every supported
            -- world size now that the 8-chunk minimum has an interior.
            forM_ worldSizes $ \ws → forM_ facings $ \f →
                applyLimits ws f 0.3 0.3 `shouldBe` (0.3, 0.3)

        it "the fence sits inside the true rim" $
            forM_ worldSizes $ \ws → do
                let halfTiles = fromIntegral ((ws * chunkSize) `div` 2)
                                  * tileHalfDiamondHeight
                (limitFor ws < halfTiles) `shouldBe` True
