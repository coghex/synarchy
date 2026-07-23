{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Elevation-spike removal for the chunk timeline pipeline.
--
--   Single-tile elevation outliers can survive elevation generation
--   when geological features (mountain ridges from convergent plate
--   boundaries) align with the u-v isometric axes — they appear as
--   1-tile-wide diagonal ridges in xy with cardinal neighbors at
--   much lower elevation. The audit catches these as TERRAIN_SPIKE.
--   This pass detects them and lowers them toward neighbor max +
--   (threshold - 1) so the result is just under the audit threshold.
--
--   Lives in its own module (not in `Generate.Timeline`) so this
--   self-contained post-processing pass — depending only on vectors,
--   ST, and `MaterialId`, with no dependency on the chunk-application
--   implementation — stays independently reviewable, mirroring the
--   `Generate.Timeline.Fast` split.
module World.Generate.Timeline.Spikes
    ( removeElevationSpikes
    ) where

import UPrelude
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import World.Material (MaterialId)

removeElevationSpikes
    ∷ Int  -- ^ spike threshold (lower if delta exceeds this)
    → Int  -- ^ max iterations
    → Int  -- ^ borderSize
    → (VU.Vector Int, VU.Vector MaterialId)
    → (VU.Vector Int, VU.Vector MaterialId)
removeElevationSpikes threshold maxIters bSize (elevVec, matVec) =
    (go False spikeConvergeCap (go True maxIters elevVec), matVec)
  where
    area = bSize * bSize

    -- Two phases. Phase 1 (cliff on) is the existing combined pass:
    -- isolated-pillar spike removal AND the iteration-limited cliff
    -- ramp that tames sheer walls. Its `maxIters` budget is deliberately
    -- small so it doesn't over-flatten mountains (see project_timeline_
    -- depth). But that same budget can run out mid-ramp on a steep peak:
    -- the flank below the summit is still being lowered when the cap is
    -- hit, leaving the summit standing as an isolated pillar (#254: seed
    -- 1337 kept +16/+19 TERRAIN_SPIKE peaks because the cliff ramp never
    -- finished re-exposing them). Phase 2 (cliff off) runs spike removal
    -- ONLY, to convergence: it lowers such residual pillars to
    -- mxNbr+(threshold-1) but never touches a tile's neighbours, so it
    -- can't cascade or flatten slopes and settles in 1-2 passes.
    spikeConvergeCap = 32

    go _       0 ev = ev
    go cliffOn n ev =
        let (ev', changed) = pass cliffOn ev
        in if changed then go cliffOn (n - 1) ev' else ev'

    -- Double-buffered: read neighbors from the immutable input `ev`,
    -- write changes to a fresh mutable `em`. This makes each pass
    -- scan-order-independent — without it, north/west neighbors
    -- (already processed in row-major order) leak their post-update
    -- values into south/east tiles' decisions, biasing how adjacent
    -- spike clusters resolve (audit #20). The outer `go` loop still
    -- iterates until convergence; updates just don't cascade within
    -- a single pass.
    pass cliffOn ev = runST $ do
        em ← VUM.new area
        forM_ [0 .. area - 1] $ \i →
            VUM.write em i (ev VU.! i)
        changedRef ← VUM.new 1
        VUM.write changedRef 0 (0 ∷ Int)
        forM_ [0 .. area - 1] $ \idx → do
            let bx = idx `mod` bSize
                by = idx `div` bSize
            -- Skip the outermost ring so all 4 cardinal neighbors
            -- are in-bounds (matches the audit which requires 4
            -- valid neighbors before classifying as a spike).
            when (bx > 0 ∧ bx < bSize - 1
                 ∧ by > 0 ∧ by < bSize - 1) $ do
                let e  = ev VU.! idx
                    n' = ev VU.! (idx - bSize)
                    s' = ev VU.! (idx + bSize)
                    w' = ev VU.! (idx - 1)
                    eN = ev VU.! (idx + 1)
                    mxNbr = max n' (max s' (max w' eN))
                    mnNbr = min n' (min s' (min w' eN))
                    -- Two independent extreme-feature clamps; whichever
                    -- lowers the tile more wins.
                    --   • SPIKE: tile > `threshold` above ALL neighbours
                    --     (an isolated pillar) → lower to mxNbr+(threshold-1).
                    --   • CLIFF: tile > `cliffThreshold` above its LOWEST
                    --     neighbour (a sheer wall the spike test misses —
                    --     the plateau side keeps mxNbr high) → lower to
                    --     mnNbr+cliffThreshold, capping the step. Iterating
                    --     ramps the wall back into the slope. Coastlines and
                    --     river canyons are shaped by earlier passes (coastal
                    --     table, smoothCliffs), so this mainly tames the
                    --     plate-boundary / ridge walls left tall when fewer
                    --     Ages erode them (see project_timeline_depth).
                    cliffThreshold = 28
                    spikeTarget = if e - mxNbr > threshold
                                  then mxNbr + (threshold - 1) else e
                    cliffTarget = if cliffOn ∧ e - mnNbr > cliffThreshold
                                  then mnNbr + cliffThreshold else e
                    target = min spikeTarget cliffTarget
                when (target < e) $ do
                    VUM.write em idx target
                    VUM.write changedRef 0 (1 ∷ Int)
        ef ← VU.unsafeFreeze em
        c ← VUM.read changedRef 0
        pure (ef, c > 0)
