{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline.River.SourceDiversity
    ( spatiallyDiverseSources
    , filterOverlappingMouths
    ) where
import UPrelude
import World.Types
import World.Geology.Hash (wrappedDeltaUV)

-- * Spatial diversity for source selection

-- | Select river sources with spatial diversity. Instead of just
--   taking the N highest-flow sources (which cluster in one basin),
--   partition sources into spatial buckets and pick the best from
--   each bucket first. This ensures rivers spawn across the whole map.
spatiallyDiverseSources ∷ Int → Int → [(Int, Int, Int, Float)]
                        → [(Int, Int, Int, Float)]
spatiallyDiverseSources worldSize budget sources
    | budget ≤ 0 = []
    | null sources = []
    | otherwise =
        let totalTiles = worldSize * 16
            -- Bucket size: ~64 tiles per bucket → good spatial spread
            bucketSize = max 32 (totalTiles `div` 8)

            -- Assign each source to a spatial bucket
            toBucket (gx, gy, _, _) =
                let bx = (gx + totalTiles) `div` bucketSize
                    by = (gy + totalTiles) `div` bucketSize
                in (bx, by)

            -- Group by bucket, keeping flow order within each bucket
            -- (sources are already sorted by flow descending)
            addToBuckets [] buckets = buckets
            addToBuckets (s:ss) buckets =
                let b = toBucket s
                    buckets' = insertBucket b s buckets
                in addToBuckets ss buckets'

            insertBucket key val [] = [(key, [val])]
            insertBucket key val ((k, vs):rest)
                | k ≡ key   = (k, vs ⧺ [val]) : rest
                | otherwise = (k, vs) : insertBucket key val rest

            buckets = addToBuckets sources []

            -- Round-robin: take one source from each bucket in turn
            roundRobin _ 0 = []
            roundRobin [] _ = []
            roundRobin bkts n =
                let (picks, remaining) = unzip
                        [ (v, (k, vs'))
                        | (k, vs) ← bkts
                        , (v, vs') ← case vs of
                            (v':vs'') → [(v', vs'')]
                            _         → [] ]
                    remaining' = filter (not . null . snd) remaining
                in take n picks ⧺ roundRobin remaining' (n - length picks)

        in take budget (roundRobin buckets budget)

-- | Filter out rivers whose mouths are too close to existing or
--   previously-accepted river mouths. This prevents the asterisk pattern
--   where multiple rivers all converge to the same point.
filterOverlappingMouths ∷ Int → [GeoCoord] → [RiverParams] → [RiverParams]
filterOverlappingMouths worldSize existingMouths = go existingMouths
  where
    mouthThreshold = 30 ∷ Int  -- min distance between river mouths
    go _ [] = []
    go mouths (r:rs) =
        let GeoCoord mx my = rpMouthRegion r
            tooClose = any (\(GeoCoord ex ey) →
                let (dxi, dyi) = wrappedDeltaUV worldSize mx my ex ey
                in abs dxi < mouthThreshold ∧ abs dyi < mouthThreshold
                ) mouths
        in if tooClose
           then go mouths rs  -- skip this river
           else r : go (rpMouthRegion r : mouths) rs
