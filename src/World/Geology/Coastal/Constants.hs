{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Shared distance-band knobs for the coastal pass. Split out of
--   "World.Geology.Coastal" so 'World.Geology.Coastal.RiverMouth' and
--   'World.Geology.Coastal.Smoothing' can reference the same bands
--   without importing the orchestrator (which imports them back).
module World.Geology.Coastal.Constants
    ( maxCoastalDist
    , smoothBand
    ) where

import UPrelude

-- | Maximum coastal influence distance (tiles from the smoothed
--   shoreline that erosion reaches). Since the move to the global
--   pass ('World.Geology.Coastal.identifyCoastalErosion', save v25)
--   this is a pure look knob — there is no window-margin constraint
--   anymore. (The old per-chunk windowed pass documented "must be <
--   chunkBorder", an invariant its own 12-pass contour smoother
--   silently broke: the total information horizon — preDist 14 + 12
--   diffusion passes + BFS 10 — exceeded the 14-tile shared border,
--   so adjacent windows computed coastlines disagreeing by up to
--   ~18z, rendered as cliffs at chunk seams.)
--
--   #220 widened 10 → 28: gentle coasts need enough reach to ramp
--   from the beach to the plateau without a forced cliff at the old
--   10-tile edge. The table just records more deltas;
--   'World.Geology.Coastal.applyCoastalTable' replays them per-tile
--   with no reach assumption.
maxCoastalDist ∷ Int
maxCoastalDist = 28

-- | Outer edge of the contour-smoothing band: full-strength smoothing
--   within 'maxCoastalDist', fading to zero here. Purely a look knob
--   (the global pass has no window-safety margin to respect).
smoothBand ∷ Int
smoothBand = maxCoastalDist + 6
