{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline.RiverTrace
    ( traceRiverFromSource
    ) where
import UPrelude
import Data.Word (Word64)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Base (GeoCoord(..))
import World.Constants (seaLevel)
import World.Types
import World.Fluids (fixupSegmentContinuity)
import World.Geology.Hash
import World.Hydrology.Types (RiverParams(..), RiverSegment(..))
import World.Hydrology.Simulation (ElevGrid(..))
import World.Chunk.Types (chunkSize)
import World.Weather.Types (ClimateState)
import World.Weather.Lookup (lookupLocalClimate, LocalClimate(..))

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
        extendedPath = extendToCoast spacing unwrappedPath

        -- Prepend the actual source point if it differs from
        -- the first grid cell (source may be between grid points).
        fullPath = case extendedPath of
            ((fx, fy, _):_)
                | fx ≠ gx ∨ fy ≠ gy → (gx, gy, srcElev) : extendedPath
            _ → (gx, gy, srcElev) : extendedPath

        -- Add perpendicular noise to each waypoint so the path
        -- looks natural instead of grid-aligned.
        noisyPath = addPathNoise seed riverIdx spacing fullPath

        -- Reject rivers that span more than 1/3 of the world.
        -- Uses UNWRAPPED (continuous) coordinates to measure actual
        -- path span, not canonical coordinates (which would show a
        -- huge span for rivers crossing the wrap boundary).
        maxSpan = worldTiles `div` 3
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

-- * Path unwrapping

-- | Make path coordinates continuous across the u-axis wrap.
--   The flow grid wraps ix (u-axis), so consecutive grid cells may
--   jump from ix=0 to ix=gridW-1 or vice versa.  The egGX/egGY
--   coordinates reflect this jump, creating a discontinuity.
--   Fix: for each consecutive pair, if u = gx-gy jumps by more
--   than half the world width, adjust the second point.
unwrapPathCoords ∷ Int → [(Int, Int, Int)] → [(Int, Int, Int)]
unwrapPathCoords _ [] = []
unwrapPathCoords _ [x] = [x]
unwrapPathCoords worldTiles (p0:rest) =
    p0 : go p0 rest
  where
    halfW = worldTiles `div` 2
    go _ [] = []
    go (px, py, _) ((x, y, e):xs) =
        let prevU = px - py
            curU  = x - y
            v     = x + y
            du    = curU - prevU
            -- If du > halfW, the path jumped across the wrap going positive;
            -- adjust u negative. If du < -halfW, jumped negative; adjust positive.
            adjU  | du > halfW  = curU - worldTiles
                  | du < negate halfW = curU + worldTiles
                  | otherwise   = curU
            -- Recover gx, gy from adjusted u and original v
            x' = (adjU + v) `div` 2
            y' = (v - adjU) `div` 2
            p' = (x', y', e)
        in p' : go p' xs

-- | Re-wrap path coordinates back into canonical u-space.
--   After unwrapping (for continuous noise) and noise displacement,
--   gx/gy may be outside the valid world range.
--
--   IMPORTANT: wraps using a CONSTANT u-offset derived from the
--   first point. The old code wrapped each point independently,
--   which could map consecutive smooth points to opposite sides
--   of the world (u wraps from +halfW to -halfW), creating
--   world-spanning segments that carve straight lines from
--   source to mouth.
--
--   A constant offset preserves path continuity. The path may
--   extend slightly outside [-halfW, halfW) for long rivers,
--   but the carving/fluid code uses wrappedDeltaUV which handles
--   this correctly. The bounding box may be conservatively large
--   for wrap-crossing rivers, but that only affects performance.
rewrapPath ∷ Int → [(Int, Int, Int)] → [(Int, Int, Int)]
rewrapPath _ [] = []
rewrapPath worldSize pts@((x0, y0, _) : _) =
    let w = worldSize * chunkSize
        halfW = w `div` 2
        u0 = x0 - y0
        wrappedU0 = ((u0 + halfW) `mod` w + w) `mod` w - halfW
        uOffset = wrappedU0 - u0
    in map (\(x, y, e) →
        let u = x - y + uOffset
            v = x + y
            x' = (u + v) `div` 2
            y' = (v - u) `div` 2
        in (x', y', e)) pts

-- * Long-gap subdivision

-- | Split any consecutive pair of waypoints that are farther apart
--   than maxLen into linearly interpolated sub-points. Elevations
--   are interpolated linearly. This prevents single long straight
--   segments from anomalous grid-path jumps or wrap-boundary residuals.
subdivideLongGaps ∷ Int → [(Int, Int, Int)] → [(Int, Int, Int)]
subdivideLongGaps _ [] = []
subdivideLongGaps _ [x] = [x]
subdivideLongGaps maxLen (p1@(x1, y1, e1) : p2@(x2, y2, e2) : rest) =
    let dx = x2 - x1
        dy = y2 - y1
        dist = round (sqrt (fromIntegral (dx * dx + dy * dy) ∷ Float)) ∷ Int
    in if dist ≤ maxLen
       then p1 : subdivideLongGaps maxLen (p2 : rest)
       else let n = (dist + maxLen - 1) `div` maxLen  -- number of sub-segments
                interps = [ let t = fromIntegral i / fromIntegral n ∷ Float
                                ix = x1 + round (fromIntegral dx * t)
                                iy = y1 + round (fromIntegral dy * t)
                                ie = e1 + round (fromIntegral (e2 - e1) * t)
                            in (ix, iy, ie)
                          | i ← [0 .. n - 1] ]
            in interps ⧺ subdivideLongGaps maxLen (p2 : rest)

-- * Long-segment splitting (segment-level safety net)

-- | Split a single segment into sub-segments if it's longer than maxLen.
--   Interpolates all fields linearly. Returns a singleton vector for
--   short segments (no allocation overhead).
splitLongSegment ∷ Int → RiverSegment → V.Vector RiverSegment
splitLongSegment maxLen seg =
    let GeoCoord sx sy = rsStart seg
        GeoCoord ex ey = rsEnd seg
        dx = ex - sx
        dy = ey - sy
        dist2 = dx * dx + dy * dy
    in if dist2 ≤ maxLen * maxLen
       then V.singleton seg
       else let dist = round (sqrt (fromIntegral dist2 ∷ Float)) ∷ Int
                n = max 2 ((dist + maxLen - 1) `div` maxLen)
                lerp a b t = a + round (fromIntegral (b - a) * t)
            in V.fromList
                [ seg { rsStart     = GeoCoord (lerp sx ex t0) (lerp sy ey t0)
                      , rsEnd       = GeoCoord (lerp sx ex t1) (lerp sy ey t1)
                      , rsStartElev = lerp (rsStartElev seg) (rsEndElev seg) t0
                      , rsEndElev   = lerp (rsStartElev seg) (rsEndElev seg) t1
                      , rsWaterStart = lerp (rsWaterStart seg) (rsWaterEnd seg) t0
                      , rsWaterEnd   = lerp (rsWaterStart seg) (rsWaterEnd seg) t1
                      }
                | i ← [0 .. n - 1]
                , let t0 = fromIntegral i / fromIntegral n ∷ Float
                      t1 = fromIntegral (i + 1) / fromIntegral n ∷ Float
                ]

-- * Coast extension

-- | Extend the river path from the last flow-grid waypoint to
--   below-sea-level terrain. The number of extension waypoints
--   scales with the elevation drop, creating:
--     - Deltas at flat coasts (≤3 above sea: 2 steps)
--     - Inlets at moderate coasts (4–30: 3–5 steps)
--     - Gorges at high coasts (30+: up to 10 steps)
--   The carving system then cuts through the terrain along these
--   segments, and fluid fill places water in the carved channel.
extendToCoast ∷ Int → [(Int, Int, Int)] → [(Int, Int, Int)]
extendToCoast _ [] = []
extendToCoast _ [x] = [x]
extendToCoast sp pts =
    let n = length pts
        (x1, y1, _) = pts !! max 0 (n - 2)
        (x2, y2, e2) = pts !! (n - 1)
        -- Direction from second-to-last to last waypoint.
        dx = x2 - x1
        dy = y2 - y1
        len = sqrt (fromIntegral (dx * dx + dy * dy) ∷ Float)
        halfSp = max 2 (sp `div` 2)
        (stepX, stepY) = if len < 1.0
            then (halfSp, 0)
            else ( round (fromIntegral dx / len * fromIntegral halfSp)
                 , round (fromIntegral dy / len * fromIntegral halfSp) )
        -- Elevation above sea level determines extension character.
        drop' = max 0 (e2 - seaLevel)
        -- Low coast: 2 intermediate steps (similar to old behavior).
        -- Moderate coast: 3–5 steps creating an inlet.
        -- High coast: up to 10 steps creating a gorge/fjord.
        numSteps = if drop' ≤ 3 then 2
                   else min 10 (max 3 (drop' `div` 6 + 2))
        -- Waypoints descending from mouth elevation to sea level.
        extensionPts =
            [ ( x2 + stepX * i
              , y2 + stepY * i
              , max seaLevel (e2 - (drop' * i) `div` numSteps)
              )
            | i ← [1 .. numSteps]
            ]
        -- Final 2 points at/below sea level ensure carving punches
        -- through to the ocean.
        lastX = x2 + stepX * numSteps
        lastY = y2 + stepY * numSteps
    in pts ⧺ extensionPts
            ⧺ [ (lastX + stepX,     lastY + stepY,     seaLevel)
              , (lastX + stepX * 2, lastY + stepY * 2, seaLevel - 1) ]

-- * Path noise

-- | Offset each waypoint perpendicular to the local flow direction
--   using coherent multi-scale noise. Three octaves create natural
--   river bends: large sweeping curves, medium bends, and small
--   wiggles. Noise is spatially coherent (smoothly interpolated
--   between hash values) so adjacent waypoints get similar offsets,
--   producing smooth curves instead of jagged zig-zags.
--
--   Amplitude tapers to zero at the source and mouth to keep
--   endpoints fixed. Also tapers in steep terrain to avoid
--   curving through mountain ridges.
addPathNoise ∷ Word64 → Int → Int → [(Int, Int, Int)] → [(Int, Int, Int)]
addPathNoise _ _ _ [] = []
addPathNoise _ _ _ [x] = [x]
addPathNoise seed riverIdx spacing pts =
    let len = length pts
        maxIdx = len - 1

        -- Cumulative arc length along the path (normalized to [0..len-1])
        arcLens = scanl (\acc ((x1,y1,_),(x2,y2,_)) →
            let dx = fromIntegral (x2 - x1) ∷ Float
                dy = fromIntegral (y2 - y1) ∷ Float
            in acc + sqrt (dx * dx + dy * dy)
            ) (0.0 ∷ Float) (zip pts (drop 1 pts))

        totalArc = case reverse arcLens of
            (a:_) → max 1.0 a
            []    → 1.0

        -- Flow direction at each point: vector to next point
        dirs = zipWith (\(x1, y1, _) (x2, y2, _) → (x2 - x1, y2 - y1))
                       pts (drop 1 pts)
               <> [(0, 0)]

        -- Maximum perpendicular offset scales with grid spacing.
        -- Larger amplitude creates natural-looking meanders in flat
        -- terrain. Slope taper (below) reduces this in mountains.
        maxOff = min 28.0 (fromIntegral spacing * 2.5) ∷ Float

        noisy = zipWith (\(i, (x, y, e), (dx, dy)) arcLen →
            if i ≡ 0 ∨ i ≡ maxIdx
            then (x, y, e)
            else let -- Endpoint taper: ramp from 0 at ends to 1 in middle
                     endT = fromIntegral (min i (maxIdx - i))
                          / fromIntegral (max 1 (maxIdx `div` 4))
                     taper = min 1.0 endT

                     -- Terrain steepness taper: reduce noise on steep slopes
                     -- so rivers don't curve sideways through ridges
                     slopeDelta = case drop 1 (drop (i-1) pts) of
                         ((_,_,e1):(_,_,e2):_) → abs (e1 - e2)
                         _ → 0
                     slopeTaper = max 0.3 (1.0 - fromIntegral slopeDelta / 15.0)

                     -- Coherent noise: 3 octaves at different frequencies.
                     -- Higher frequencies create more bends per river length,
                     -- preventing the "straight line" look from coarse grids.
                     t = arcLen / totalArc
                     octave1 = coherentNoise seed (riverIdx * 3)     (t * 4.0)  -- large bends
                     octave2 = coherentNoise seed (riverIdx * 3 + 1) (t * 10.0) -- medium curves
                     octave3 = coherentNoise seed (riverIdx * 3 + 2) (t * 22.0) -- small wiggles

                     -- Combine octaves with decreasing amplitude
                     noise = octave1 * 0.55 + octave2 * 0.30 + octave3 * 0.15

                     -- Final offset
                     amplitude = maxOff * taper * slopeTaper

                     -- Perpendicular to flow direction
                     dLen = sqrt (fromIntegral (dx * dx + dy * dy) ∷ Float)
                     (perpX, perpY) = if dLen < 0.001
                         then (0.0, 0.0)
                         else ( negate (fromIntegral dy) / dLen
                              , fromIntegral dx / dLen )
                     offX = round (perpX * noise * amplitude) ∷ Int
                     offY = round (perpY * noise * amplitude) ∷ Int
                 in (x + offX, y + offY, e)
            ) (zip3 [0 ∷ Int ..] pts dirs) arcLens
    in noisy

-- | 1D coherent noise: smoothly interpolated between hash values.
--   Returns a value in [-1, 1]. The input t is a continuous position;
--   hash values are computed at integer positions and interpolated
--   with smoothstep for C1 continuity.
coherentNoise ∷ Word64 → Int → Float → Float
coherentNoise seed idx t =
    let ti  = floor t ∷ Int
        frac = t - fromIntegral ti
        -- Hash at integer positions
        h0 = hashToFloatGeo (hashGeo seed (idx * 10000 + ti)     1400) * 2.0 - 1.0
        h1 = hashToFloatGeo (hashGeo seed (idx * 10000 + ti + 1) 1400) * 2.0 - 1.0
        -- Smoothstep interpolation
        s = smoothstepGeo frac
    in h0 * (1.0 - s) + h1 * s

-- * Path construction

buildRiverFromPath ∷ Word64 → Int → Int → Int → Float → ClimateState
                   → [(Int, Int, Int)] → Maybe RiverParams
buildRiverFromPath seed worldSize gridSpacing riverIdx baseFlow climate path =
    let -- Subdivide any long gaps between consecutive waypoints.
        -- This catches wrap-boundary residuals and anomalous grid-path
        -- jumps that create visible straight-line carved segments.
        -- Applied here (not earlier) so it works regardless of how the
        -- path was produced (traced rivers, tributaries, etc.).
        maxSegLen = gridSpacing * 3
        subdividedPath = subdivideLongGaps maxSegLen path
        monoPath = enforceMonotonicPath subdividedPath
        -- Keep every grid point (~10-tile segments at spacing=10).
        -- Shorter segments make the noise-displaced waypoints more
        -- visible as bends, preventing the "ruler-straight" look that
        -- comes from long straight-line segments.
        decimated0 = monoPath
        -- Clamp the last above-sea-level point near sea level so the
        -- river transitions smoothly to the coast. Points already
        -- below sea level (from coast extension) are left as-is —
        -- they ensure carving punches through to the ocean.
        decimated = clampMouthTransition decimated0
    in case decimated of
        [] → Nothing
        ((srcX, srcY, _) : _) →
            let numWP = length decimated
                (mouthX, mouthY, mouthElev) = last decimated
                -- Rivers that reach near the coast are always accepted
                -- regardless of length. Generous threshold — the coast
                -- extension will bridge the remaining gap.
                reachesCoast = mouthElev ≤ seaLevel + 15
                -- Inland rivers (feeding lakes, drying up) are fine if
                -- they're long enough to look like a real river. Short
                -- inland rivers just create blobs of water on hillsides.
                minInlandSegments = 12
                tooShortInland = not reachesCoast ∧ numWP < minInlandSegments
            in if tooShortInland
               then Nothing
               else let segments0 = fixupSegmentContinuity $ V.fromList $
                               zipWith (buildSegFromWaypoints seed worldSize
                                            numWP baseFlow climate)
                                       [0..] (zip decimated (drop 1 decimated))
                        segments1 = poolWaterSurface segments0
                        -- Split any segment longer than maxSegLen into
                        -- sub-segments by linear interpolation. This is
                        -- the final safety net — catches long segments
                        -- regardless of their origin.
                        segments = V.concatMap (splitLongSegment maxSegLen) segments1
                        totalFlow = case V.null segments of
                            True  → baseFlow
                            False → rsFlowRate (V.last segments)
                    in Just RiverParams
                        { rpSourceRegion = GeoCoord srcX srcY
                        , rpMouthRegion  = GeoCoord mouthX mouthY
                        , rpSegments     = segments
                        , rpFlowRate     = totalFlow
                        , rpMeanderSeed  = fromIntegral (hashGeo seed riverIdx 1150)
                        }

-- | Keep every Nth point from the path, always preserving
--   the first and last points.
decimatePath ∷ Int → [(Int, Int, Int)] → [(Int, Int, Int)]
decimatePath _ [] = []
decimatePath _ [x] = [x]
decimatePath n xs =
    let lastPt = last xs
        picked = go 0 xs
        -- Ensure the last point is always included
    in case picked of
        [] → [lastPt]
        _  → if last picked ≡ lastPt then picked
             else picked <> [lastPt]
  where
    go _ [] = []
    go i (x:rest)
        | i `mod` n ≡ 0 = x : go (i + 1) rest
        | otherwise       = go (i + 1) rest

-- | Clamp the last above-sea-level point to seaLevel+2 so the river
--   transitions smoothly at the coast. Below-sea-level points (from
--   coast extension) are left as-is — they carve through to the ocean.
clampMouthTransition ∷ [(Int, Int, Int)] → [(Int, Int, Int)]
clampMouthTransition [] = []
clampMouthTransition pts =
    let rev = reverse pts
        -- Split into below-sea-level tail and the rest
        (belowSea, rest) = span (\(_, _, e) → e < seaLevel) rev
    in case rest of
        -- Clamp the first above-sea-level point (= last above-sea in forward order)
        (x, y, e) : rs
            | e ≤ seaLevel + 5 →
                -- Restore original order: above-sea points, then clamped
                -- transition point, then below-sea coast extension.
                reverse rs ⧺ [(x, y, seaLevel + 2)] ⧺ reverse belowSea
        _ → pts

enforceMonotonicPath ∷ [(Int, Int, Int)] → [(Int, Int, Int)]
enforceMonotonicPath [] = []
enforceMonotonicPath [x] = [x]
enforceMonotonicPath ((x0, y0, e0) : rest) =
    (x0, y0, e0) : go e0 rest
  where
    go _ [] = []
    go maxE ((x, y, e) : xs) =
        let e' = min maxE e
        in (x, y, e') : go e' xs

buildSegFromWaypoints ∷ Word64 → Int → Int → Float → ClimateState → Int
                      → ((Int, Int, Int), (Int, Int, Int))
                      → RiverSegment
buildSegFromWaypoints seed worldSize totalSegs baseFlow climate segIdx
                      ((sx, sy, se), (ex, ey, ee)) =
    let t = fromIntegral (segIdx + 1) / fromIntegral totalSegs

        -- Climate modulation: look up precipitation at segment midpoint.
        -- Higher precipitation → more water → wider/deeper river.
        -- Normalized to 0.5 as "average" precipitation.
        midGX = (sx + ex) `div` 2
        midGY = (sy + ey) `div` 2
        LocalClimate{lcPrecip=precip} =
            lookupLocalClimate climate worldSize midGX midGY
        climateMult = max 0.3 (min 3.0 (precip / 0.5))

        flow = (baseFlow + t * baseFlow * 2.0) * climateMult

        -- Mouth proximity: the final 20% of the river widens into
        -- a delta/estuary. mouthT ramps from 0 at t=0.8 to 1 at t=1.
        mouthT = max 0.0 ((t - 0.8) / 0.2) ∷ Float
        -- Below-sea-level segments (coast extension) get full widening.
        belowSea = se ≤ seaLevel ∨ ee ≤ seaLevel
        mouthFactor = if belowSea then 1.0 else mouthT

        -- Width scales with flow, boosted at the mouth.
        -- Headwaters: 1-3 tiles. Midstream: 3-8. Mouth: up to 16.
        rawWidth = max 1 (round (flow * 6.0)) ∷ Int
        -- Mouth widening: up to 2× base width at the mouth
        mouthBoost = round (fromIntegral rawWidth * mouthFactor) ∷ Int
        width = min 16 (rawWidth + mouthBoost)

        h1 = hashGeo seed segIdx 1161
        valleyMult = 1.4 + hashToFloatGeo h1 * 0.6

        slopeDelta = abs (se - ee)
        -- Depth scales with flow: shallow headwaters, deeper downstream.
        -- Minimum 2 gives visible water in the channel center.
        -- Capped at 5 tiles — carves gentle channels, not canyons.
        -- Mouth segments are shallower (deltas are flat/wide, not deep).
        baseDepth = max 2 (slopeDelta `div` 8 + round (flow * 0.6))
        depth = min 5 (if mouthFactor > 0.5 then max 2 (baseDepth - 1) else baseDepth)
        -- Valley width: wider at the mouth to create a delta/sound.
        -- Standard: channel + modest banks. Mouth: up to 2× wider.
        minValleyW = width + depth
        rawValleyW = max (width + 2) (round (fromIntegral width * valleyMult))
        mouthValleyBoost = round (fromIntegral rawValleyW * mouthFactor * 0.5) ∷ Int
        valleyW = max minValleyW (min 32 (rawValleyW + mouthValleyBoost))
    in RiverSegment
        { rsStart       = GeoCoord sx sy
        , rsEnd         = GeoCoord ex ey
        , rsWidth       = width
        , rsValleyWidth = valleyW
        , rsDepth       = depth
        , rsFlowRate    = flow
        , rsStartElev   = se
        , rsEndElev     = ee
        -- Water surface = reference elevation (no freeboard). Every
        -- tile carved below refElev gets water; uncarved tiles at
        -- refElev don't. The carved terrain provides the depth profile.
        , rsWaterStart  = max seaLevel se
        , rsWaterEnd    = max seaLevel ee
        }

-- * Water surface pooling

-- | Pool water surfaces: where terrain forms depressions,
--   water should fill to the level of the outflow point.
--   Walk from mouth to source, propagating the downstream
--   water level upstream through any depressions. This
--   creates level pools in flat areas.
poolWaterSurface ∷ V.Vector RiverSegment → V.Vector RiverSegment
poolWaterSurface segs
    | V.length segs ≤ 1 = segs
    | otherwise =
        let segList = V.toList segs
            -- Extract n+1 water levels: [waterStart_0, waterEnd_0, waterEnd_1, ...]
            waterLevels = case segList of
                (s:_) → rsWaterStart s : map rsWaterEnd segList
                []    → []
            -- Pool: walk from mouth (last) to source (first).
            -- Any depression gets filled to the outflow level.
            pooled = reverse $ poolFill $ reverse waterLevels
            -- Write back into segments as (start, end) pairs
            pairs = zip pooled (drop 1 pooled)
        in V.fromList $ zipWith (\seg (ws, we) →
            seg { rsWaterStart = ws, rsWaterEnd = we }
            ) segList pairs
  where
    poolFill [] = []
    poolFill (x:xs) = x : goPool x xs
    goPool _ [] = []
    goPool lvl (x:xs) = let x' = max x lvl in x' : goPool x' xs
