{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | River tracing via flow-direction chain, split (issue #581) into
--   focused submodules under "World.Geology.Timeline.RiverTrace.*":
--
--     * "World.Geology.Timeline.RiverTrace.Unwrap" — u-axis wrap-boundary
--       coordinate continuity for path noise and bounding-box storage.
--     * "World.Geology.Timeline.RiverTrace.Subdivide" — long-gap and
--       long-segment splitting safety nets.
--     * "World.Geology.Timeline.RiverTrace.Coast" — coast extension past
--       the flow-grid's last waypoint down to below-sea-level terrain.
--     * "World.Geology.Timeline.RiverTrace.Noise" — coherent perpendicular
--       path noise so traced rivers don't look grid-aligned.
--     * "World.Geology.Timeline.RiverTrace.Build" — waypoint path to
--       'RiverSegment' construction.
--
--   This module keeps the flow-direction-chain trace itself, the sole
--   exported entry point.
module World.Geology.Timeline.RiverTrace
    ( traceRiverFromSource
    ) where
import UPrelude
import qualified Data.Vector.Unboxed as VU
import World.Types
import World.Hydrology.Simulation (ElevGrid(..))
import World.Weather.Types (ClimateState)
import World.Geology.Timeline.RiverTrace.Unwrap (unwrapPathCoords, rewrapPath)
import World.Geology.Timeline.RiverTrace.Coast (extendToCoast)
import World.Geology.Timeline.RiverTrace.Noise (addPathNoise)
import World.Geology.Timeline.RiverTrace.Build (buildRiverFromPath)

-- * River tracing via flow-direction chain

-- | Trace a river from a source cell to the ocean by following
--   the pre-computed flow direction grid. This is GUARANTEED to
--   reach the ocean because the flow directions are derived from
--   the depression-filled surface (priority-flood algorithm).
--
--   The grid path is augmented with hash-based perpendicular
--   noise so the river doesn't look grid-aligned.
traceRiverFromSource ∷ Word64 → Int → ElevGrid → VU.Vector Int
                     → VU.Vector Int
                     → Int → Int → Int → Int → Float
                     → ClimateState
                     → Maybe RiverParams
traceRiverFromSource seed worldSize elevGrid _filledElev flowDir
                     gx gy srcElev riverIdx flow climate =
    let gridW   = egGridW elevGrid
        spacing = egSpacing elevGrid
        halfGrid = gridW `div` 2
        totalSamples = gridW * gridW

        -- Convert tile coordinates (gx, gy) to grid index.
        -- Same math as elevFromGrid/filledElevFromGrid.
        tileToGridIdx x y =
            let u = x - y
                v = x + y
                iu = ((u + spacing `div` 2) `div` spacing) + halfGrid
                iv = ((v + spacing `div` 2) `div` spacing) + halfGrid
                iu' = ((iu `mod` gridW) + gridW) `mod` gridW
                iv' = max 0 (min (gridW - 1) iv)
            in iv' * gridW + iu'

        -- Follow the flow direction chain from a grid index to the ocean.
        -- Collects (gx, gy, rawElev) at each grid cell.
        -- Guaranteed to terminate: filled surface is monotonically
        -- descending along flowDir, so no cycles are possible.
        traceGrid startIdx = go 0 startIdx []
          where
            maxGridSteps = 5000 ∷ Int
            go step idx acc
                | step ≥ maxGridSteps = reverse acc
                | idx < 0 ∨ idx ≥ totalSamples = reverse acc
                | otherwise =
                    let tileGX  = egGX elevGrid VU.! idx
                        tileGY  = egGY elevGrid VU.! idx
                        rawElev = egElev elevGrid VU.! idx
                        nextIdx = flowDir VU.! idx
                        -- Clamp to just below sea level — don't let
                        -- the path drop to deep ocean floor.
                        clampedElev = max (seaLevel - 3) rawElev
                        acc'    = (tileGX, tileGY, clampedElev) : acc
                    in if nextIdx < 0 ∨ nextIdx ≡ idx
                       then reverse acc'  -- ocean sink or dead end
                       -- Stop once raw terrain is well below sea level —
                       -- we've reached the ocean.
                       else if rawElev < seaLevel - 3
                       then reverse acc'
                       else go (step + 1) nextIdx acc'

        startIdx = tileToGridIdx gx gy
        gridPath = traceGrid startIdx

        -- Unwrap the path so coordinates are continuous across the
        -- u-axis wrap boundary.  Without this, rivers that cross
        -- ix=0 ↔ ix=gridW-1 in the flow grid get huge coordinate
        -- jumps and appear to span the entire world.
        --
        -- IMPORTANT: use the grid's actual u-axis period (gridW * spacing),
        -- NOT worldTiles. The grid may not tile the world exactly (e.g.
        -- gridW=384, spacing=10 → 3840, but worldTiles=4096). Using
        -- worldTiles leaves a residual error of (worldTiles - gridW*spacing)
        -- at each wrap crossing, creating long straight segments.
        worldTiles = worldSize * chunkSize
        gridWrapPeriod = gridW * spacing
        unwrappedPath = unwrapPathCoords gridWrapPeriod gridPath

        -- Extend past the ocean sink to ensure the river reaches
        -- below-sea-level terrain. Without this, the grid resolution
        -- (spacing tiles) can leave a gap of above-sea-level terrain
        -- between the river mouth and the actual ocean.
        extendedPath = extendToCoast spacing elevGrid unwrappedPath

        -- Prepend the actual source point if it differs from
        -- the first grid cell (source may be between grid points).
        fullPath = case extendedPath of
            ((fx, fy, _):_)
                | fx ≠ gx ∨ fy ≠ gy → (gx, gy, srcElev) : extendedPath
            _ → (gx, gy, srcElev) : extendedPath

        -- Add perpendicular noise to each waypoint so the path
        -- looks natural instead of grid-aligned.
        noisyPath = addPathNoise seed riverIdx spacing fullPath

        -- Reject only rivers that span more than HALF the world — a
        -- continent-spanning river is physical (issue #221: long
        -- inland-origin rivers were being discarded by the old
        -- worldTiles/3 cap), but a span beyond half the world is a
        -- wrap-boundary artifact, not a real course. Uses UNWRAPPED
        -- (continuous) coordinates to measure actual path span, not
        -- canonical coordinates (which would show a huge span for rivers
        -- crossing the wrap boundary). Applies uniformly to every world
        -- size (issue #811): the source extension this cap accommodates
        -- is no longer gated to worldSize ≥ 128 either, and coupling the
        -- two matters — extending sources on Tiny/Small worlds without
        -- also relaxing this cap would trip the old /3 bound on the
        -- newly-lengthened rivers and silently discard them.
        maxSpan = worldTiles `div` 2
        pathTooLong = case (noisyPath, reverse noisyPath) of
            ((sx, sy, _):_, (mx, my, _):_) →
                abs (mx - sx) > maxSpan ∨ abs (my - sy) > maxSpan
            _ → False

        -- Re-wrap coordinates back to canonical u-space AFTER the
        -- span check. The unwrap step made coordinates continuous
        -- for noise displacement, but the resulting coords may be
        -- outside the valid world range. If stored as-is, bounding
        -- box computation wraps each point independently, producing
        -- a bbox that spans the entire world — causing every chunk
        -- to carve this river.
        rewrappedPath = rewrapPath worldSize noisyPath

    in if length rewrappedPath < 4 ∨ pathTooLong
       then Nothing
       else buildRiverFromPath seed worldSize spacing riverIdx flow climate rewrappedPath
