{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline.RiverTrace.Build
    ( buildRiverFromPath
    ) where
import UPrelude
import qualified Data.Vector as V
import World.Types
import World.Fluid.River (fixupSegmentContinuity)
import World.Weather.Types (ClimateState)
import World.Weather.Lookup (lookupLocalClimate, LocalClimate(..))
import World.Geology.Hash (hashGeo, hashToFloatGeo)
import World.Geology.Timeline.RiverTrace.Subdivide (subdivideLongGaps, splitLongSegment)

-- * Path construction

buildRiverFromPath ∷ Word64 → Int → Int → Int → Float → ClimateState
                   → [(Int, Int, Int)] → Maybe RiverParams
buildRiverFromPath seed worldSize gridSpacing _riverIdx baseFlow climate path =
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
                        -- Split any segment longer than maxSegLen into
                        -- sub-segments by linear interpolation. Final
                        -- safety net — catches long segments regardless
                        -- of their origin.
                        segments = V.concatMap (splitLongSegment maxSegLen) segments0
                        totalFlow = case V.null segments of
                            True  → baseFlow
                            False → rsFlowRate (V.last segments)
                    in Just RiverParams
                        { rpSourceRegion = GeoCoord srcX srcY
                        , rpMouthRegion  = GeoCoord mouthX mouthY
                        , rpSegments     = segments
                        , rpFlowRate     = totalFlow
                        }

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
        }
