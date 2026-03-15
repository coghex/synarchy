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
import World.Weather.Lookup (lookupLocalClimate)

-----------------------------------------------------------
-- River tracing via flow-direction chain
-----------------------------------------------------------

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
        worldTiles = worldSize * chunkSize
        unwrappedPath = unwrapPathCoords worldTiles gridPath

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
        -- These are following flow directions the wrong way around
        -- the cylindrical u-axis instead of reaching the nearest coast.
        maxSpan = worldTiles `div` 3
        pathTooLong = case (noisyPath, reverse noisyPath) of
            ((sx, sy, _):_, (mx, my, _):_) →
                abs (mx - sx) > maxSpan ∨ abs (my - sy) > maxSpan
            _ → False

    in if length noisyPath < 4 ∨ pathTooLong
       then Nothing
       else buildRiverFromPath seed worldSize riverIdx flow climate noisyPath

-----------------------------------------------------------
-- Path unwrapping
-----------------------------------------------------------

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

-----------------------------------------------------------
-- Coast extension
-----------------------------------------------------------

-- | Extrapolate 2 waypoints past the flow grid sink to push the
--   river path into below-sea-level terrain. The flow grid has
--   coarse spacing, so the sink cell can be up to `spacing` tiles
--   from the actual coastline. Without extension, there may be
--   uncarved terrain between the river mouth and the ocean.
--   Only extends if the last point is near sea level.
extendToCoast ∷ Int → [(Int, Int, Int)] → [(Int, Int, Int)]
extendToCoast _ [] = []
extendToCoast _ [x] = [x]
extendToCoast sp pts =
    let n = length pts
        (x1, y1, _) = pts !! max 0 (n - 2)
        (x2, y2, e2) = pts !! (n - 1)
    in if e2 > seaLevel + 5
       then pts  -- river doesn't reach near the coast
       else let dx = x2 - x1
                dy = y2 - y1
                len = sqrt (fromIntegral (dx * dx + dy * dy) ∷ Float)
                -- Step in the same direction at ~spacing distance
                (stepX, stepY) = if len < 1.0
                    then (sp, 0)
                    else ( round (fromIntegral dx / len * fromIntegral sp)
                         , round (fromIntegral dy / len * fromIntegral sp) )
            in pts ⧺ [ (x2 + stepX, y2 + stepY, seaLevel - 1)
                      , (x2 + stepX * 2, y2 + stepY * 2, seaLevel - 2) ]

-----------------------------------------------------------
-- Path noise
-----------------------------------------------------------

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
        -- With finer grids, paths already follow terrain well so
        -- less noise is needed. Cap at 16 tiles max.
        maxOff = min 16.0 (fromIntegral spacing * 1.5) ∷ Float

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
                     -- Each octave uses a different seed offset and frequency
                     -- along the arc length for smooth spatial variation.
                     t = arcLen / totalArc
                     octave1 = coherentNoise seed (riverIdx * 3)     (t * 2.5)  -- large bends
                     octave2 = coherentNoise seed (riverIdx * 3 + 1) (t * 6.0)  -- medium curves
                     octave3 = coherentNoise seed (riverIdx * 3 + 2) (t * 14.0) -- small wiggles

                     -- Combine octaves with decreasing amplitude
                     noise = octave1 * 0.6 + octave2 * 0.3 + octave3 * 0.1

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

-----------------------------------------------------------
-- Path construction
-----------------------------------------------------------

buildRiverFromPath ∷ Word64 → Int → Int → Float → ClimateState
                   → [(Int, Int, Int)] → Maybe RiverParams
buildRiverFromPath seed worldSize riverIdx baseFlow climate path =
    let monoPath = enforceMonotonicPath path
        -- Grid-following produces one point per grid cell (~8 tiles apart).
        -- Decimate to every 3rd to get ~24-tile segments, similar to before.
        decimated0 = decimatePath 3 monoPath
        -- Clamp the last above-sea-level point near sea level so the
        -- river transitions smoothly to the coast. Points already
        -- below sea level (from coast extension) are left as-is —
        -- they ensure carving punches through to the ocean.
        decimated = clampMouthTransition decimated0
    in case decimated of
        [] → Nothing
        ((srcX, srcY, _) : _) →
            let numWP = length decimated
                segments0 = fixupSegmentContinuity $ V.fromList $
                               zipWith (buildSegFromWaypoints seed worldSize
                                            numWP baseFlow climate)
                                       [0..] (zip decimated (drop 1 decimated))
                segments = poolWaterSurface segments0
                (mouthX, mouthY, _) = last decimated
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
            | e ≤ seaLevel + 3 →
                reverse belowSea ⧺ reverse ((x, y, seaLevel + 2) : rs)
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
        (_temp, precip, _humid, _snow) =
            lookupLocalClimate climate worldSize midGX midGY
        climateMult = max 0.3 (min 3.0 (precip / 0.5))

        flow = (baseFlow + t * baseFlow * 2.0) * climateMult

        -- Width scales with flow: narrow headwaters (1-2 tiles),
        -- widening downstream as tributaries add volume.
        rawWidth = max 1 (round (flow * 6.0)) ∷ Int
        width = min 16 rawWidth

        h1 = hashGeo seed segIdx 1161
        valleyMult = 2.0 + hashToFloatGeo h1 * 1.5

        slopeDelta = abs (se - ee)
        -- Depth scales with flow: shallow headwaters, deeper downstream.
        -- Minimum 2 ensures carved channel is below the water surface
        -- (freeboard = 1, so depth must be > 1 for water to fill).
        -- Capped at 6 tiles — carves gentle channels, not canyons.
        baseDepth = max 2 (slopeDelta `div` 6 + round (flow * 0.8))
        depth = min 6 baseDepth
        -- Valley width for water fill. Scales with river width
        -- to prevent headwater streams from carving wide valleys.
        minValleyW = max (width * 2) (depth * 2)
        rawValleyW = max (width * 2) (round (fromIntegral width * valleyMult))
        valleyW = max minValleyW (min 32 rawValleyW)

        -- Consistent freeboard: water surface sits 1 tile below
        -- the reference terrain. This keeps the water surface smooth
        -- regardless of per-segment depth variation.
        freeboard = 1 ∷ Int
    in RiverSegment
        { rsStart       = GeoCoord sx sy
        , rsEnd         = GeoCoord ex ey
        , rsWidth       = width
        , rsValleyWidth = valleyW
        , rsDepth       = depth
        , rsFlowRate    = flow
        , rsStartElev   = se
        , rsEndElev     = ee
        , rsWaterStart  = max seaLevel (se - freeboard)
        , rsWaterEnd    = max seaLevel (ee - freeboard)
        }

-----------------------------------------------------------
-- Water surface pooling
-----------------------------------------------------------

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
            waterLevels = rsWaterStart (head segList) : map rsWaterEnd segList
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
