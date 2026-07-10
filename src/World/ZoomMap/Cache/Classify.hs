{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Chunk-summary classification helpers, split out of
--   "World.ZoomMap.Cache" (issue #573): picking a chunk's majority
--   surface material, and its climate-derived vegetation density
--   category.
module World.ZoomMap.Cache.Classify
    ( majorityMaterial
    , vegCategoryFromClimate
    ) where

import UPrelude
import qualified Data.Map.Strict as Map
import World.Types
import World.Vegetation (isBarrenMaterial, isWetlandSoil)
import World.Weather.Types (ClimateState(..))
import World.Weather.Lookup (lookupLocalClimate, LocalClimate(..))

-- * Helpers

majorityMaterial ∷ [(Int, Word8)] → Word8
majorityMaterial samples =
    let counts = foldl' (\m (_, mat) → Map.insertWith (+) mat (1 ∷ Int) m)
                        Map.empty samples
        -- Prefer land materials over ocean/air (matId=0).
        -- A coastal chunk with 40% land should show as land in the
        -- preview, since the per-chunk zoom pixel texture already
        -- renders the per-tile detail correctly.
        landCounts = Map.delete 0 counts
        pickBest m = Map.foldlWithKey' (\(bestMat, bestCount) mat count →
            if count > bestCount ∨ (count ≡ bestCount ∧ mat > bestMat)
            then (mat, count)
            else (bestMat, bestCount)
            ) (0, 0) m
        (landWinner, landCount) = pickBest landCounts
        (anyWinner, _) = pickBest counts
    in if landCount > 0 then landWinner else anyWinner

-- * Vegetation Category

-- | Climate-based vegetation density:
--   0 = none (barren rock, ocean, glacier),
--   1 = sparse (tundra, arid scrub),
--   2 = medium (temperate grassland),
--   3 = dense (lush grass, tropical),
--   4 = marsh/wetland.

vegCategoryFromClimate ∷ ClimateState → Int → Int → Int → Word8 → Word8
vegCategoryFromClimate climate worldSize baseGX baseGY matId
    | isBarrenMaterial matId = 0
    | isWetlandSoil matId   = 4
    | otherwise =
        let -- Sample at chunk center
            gx = baseGX + chunkSize `div` 2
            gy = baseGY + chunkSize `div` 2
            LocalClimate{lcTemp=temp, lcPrecip=precip, lcSnow=snow} =
                lookupLocalClimate climate worldSize gx gy
        in if snow > 0.7           then 0
           else if temp < -5.0     then 1  -- tundra
           else if precip < 0.15   then 0  -- desert
           else if precip < 0.25   then 1  -- arid
           else if temp > 20.0 ∧ precip > 0.5 then 3  -- tropical
           else if temp > 10.0 ∧ precip > 0.4 then 3  -- warm wet
           else if temp > 5.0  ∧ precip > 0.3 then 2  -- temperate
           else 1  -- cool / sparse
