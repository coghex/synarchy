{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure tests for scroll-to-zoom calibration (issue #596).
--
--   The bug: the old onScroll handler (duplicated in world_view.lua and
--   test_arena.lua) applied a fixed velocity impulse per scroll callback
--   based only on the SIGN of dy, ignoring its magnitude. Combined with
--   'Engine.Loop.Camera.stepCameraZoom''s friction (which decays velocity
--   every frame, roughly canceling a single notch's impulse within about
--   a frame), that made the total zoom response track callback COUNT
--   rather than scroll AMOUNT: a physical mouse wheel (one big event per
--   notch, spaced out over real time) lost almost all of its impulse to
--   friction between notches, while a trackpad (many tiny events packed
--   into the same frame) stacked its fixed-size impulses before friction
--   got a chance to act — see CLAUDE.md's testing-tiers note and
--   world_view.lua's onScroll history for the pre-fix code.
--
--   The fix ('Engine.Loop.Camera.scrollZoomImpulse') scales the impulse
--   by the delta itself, so total impulse is linear in the summed scroll
--   amount regardless of how many callbacks delivered it. These tests
--   exercise that pure function together with the also-exported
--   'stepCameraZoom' integrator — the exact functions
--   'Engine.Scripting.Lua.API.Camera.cameraApplyScrollZoomFn' and
--   'Engine.Loop.Camera.updateCameraZoom' call — under a synthetic
--   60fps frame timeline. No engine needed.
module Test.Headless.Camera.ZoomScroll (spec) where

import UPrelude
import Test.Hspec
import Engine.Loop.Camera (scrollZoomImpulse, stepCameraZoom, zoomMin)

-- | Nominal frame time the synthetic timeline steps by.
dtF ∷ Float
dtF = 1 / 60

startZoom ∷ Float
startZoom = 64.0

-- | Run friction-only frames (no further scroll input) until zoom
--   velocity snaps to rest.
settle ∷ (Float, Float) → (Float, Float)
settle (z, 0)  = (z, 0)
settle (z, zv) = settle (stepCameraZoom dtF z zv)

-- | One simulated frame: fold this frame's already-coalesced scroll
--   delta (every raw callback since the last frame, summed — see
--   cameraApplyScrollZoomFn, which does the same before touching
--   velocity) into zoom velocity, then integrate.
stepFrame ∷ (Float, Float) → Float → (Float, Float)
stepFrame (z, zv) dy = stepCameraZoom dtF z (zv + scrollZoomImpulse z dy)

-- | Sum the velocity impulse from a list of raw dy deltas that all land
--   within the same frame (i.e. before any friction/integration runs) —
--   what cameraApplyScrollZoomFn does for each raw GLFW callback between
--   two 'updateCameraZoom' ticks.
coalesce ∷ Float → [Float] → Float
coalesce z dys = sum (map (scrollZoomImpulse z) dys)

-- | Two Floats are close enough to call equal for calibration purposes
--   (well under any zoom value we compare — just soaks up Float
--   summation-order rounding).
closeTo ∷ Float → Float → Bool
closeTo a b = abs (a - b) < 1.0e-3

-- | Spread @n@ equal-sized scroll events summing to @totalDy@ evenly
--   across @frames@ simulated frames (multiple events landing on the
--   same frame index are coalesced, matching real per-frame delivery),
--   then settle. Models one continuous scroll gesture of a given total
--   amount, delivered as a coarse (few big events) or fine (many small
--   events) stream over the same real-time span.
simulateGesture ∷ Float → Int → Int → (Float, Float)
simulateGesture totalDy n frames =
    let dyEach = totalDy / fromIntegral n
        eventFrame i = round
            (fromIntegral i * fromIntegral frames / fromIntegral n ∷ Double)
        frameCounts = [ length (filter (≡ f) (map eventFrame [0 .. n-1]))
                      | f ← [0 .. frames - 1] ]
        frameDeltas = map ((dyEach *) . fromIntegral) frameCounts
        afterInput = foldl' stepFrame (startZoom, 0) frameDeltas
    in settle afterInput

spec ∷ Spec
spec = describe "camera zoom scroll calibration (#596)" $ do

    it "scales by scroll amount, not event count: one big delta and many \
       \small deltas summing to it add the same total impulse" $ do
        let impulseOne = coalesce startZoom [-1.0]
            impulseTen = coalesce startZoom (replicate 10 (-0.1))
        impulseOne `shouldSatisfy` closeTo impulseTen

    it "one dy=-1 event and ten dy=-0.1 events (same total, applied within \
       \the same frame) settle to the same zoom" $ do
        let velOne = coalesce startZoom [-1.0]
            velTen = coalesce startZoom (replicate 10 (-0.1))
            (zOne, _) = settle (startZoom, velOne)
            (zTen, _) = settle (startZoom, velTen)
        zOne `shouldSatisfy` closeTo zTen

    it "ten same-direction dy=-1 wheel-notch inputs (~100ms cadence) \
       \reduce settled zoom by at least 25%, without immediately \
       \clamping to zoomMin" $ do
        let (zFinal, zvFinal) = simulateGesture (-10.0) 10 60
        zvFinal `shouldBe` 0
        zFinal `shouldSatisfy` (≤ startZoom * 0.75)
        zFinal `shouldSatisfy` (> zoomMin * 2)

    it "splitting the same total delta into many small trackpad-style \
       \events over the same span doesn't settle more than 25% further \
       \than the coarse wheel sequence" $ do
        let (zCoarse, _) = simulateGesture (-10.0) 10 60
            (zFine, _)   = simulateGesture (-10.0) 100 60
            coarseChange = startZoom - zCoarse
            fineChange   = startZoom - zFine
        fineChange `shouldSatisfy` (≤ coarseChange * 1.25)

    it "dy > 0 zooms out and dy < 0 zooms in, with equal-magnitude impulses \
       \for equal-magnitude opposite deltas" $ do
        let impulseIn  = scrollZoomImpulse startZoom (-1.0)
            impulseOut = scrollZoomImpulse startZoom 1.0
        impulseIn `shouldBe` negate impulseOut
        let (zIn, _)  = settle (stepFrame (startZoom, 0) (-1.0))
            (zOut, _) = settle (stepFrame (startZoom, 0) 1.0)
        zIn `shouldSatisfy` (< startZoom)
        zOut `shouldSatisfy` (> startZoom)
