{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate.Config.Normalize
    ( minimumWorldSize
    , normalizeWorldSize
    , normalizePlateCount
    , normalizeWorldGenInputs
    , normalizeWorldGenConfig
    ) where

import UPrelude
import World.Generate.Config.Types (WorldGenConfig(..))
import World.Region.Types (regionSize)
import World.Weather.Types (climateRegionSize)

-- | Smallest supported world side, in chunks. A world must contain at
--   least one complete region for every region grid used by generation.
minimumWorldSize ∷ Int
minimumWorldSize = max regionSize climateRegionSize

roundUpToMultiple ∷ Int → Int → Int
roundUpToMultiple step n =
    let r = n `mod` step
    in if r ≡ 0 then n else n + step - r

-- | Normalize world dimensions and plate counts before they reach
--   generation. These are UI-friendly clamps: impossible small values
--   snap to one full region, then round up to the next region multiple
--   so every region grid tiles the world exactly.
normalizeWorldSize ∷ Int → Int
normalizeWorldSize n =
    roundUpToMultiple minimumWorldSize (max minimumWorldSize n)

normalizePlateCount ∷ Int → Int
normalizePlateCount = max 1

normalizeWorldGenInputs ∷ Int → Int → (Int, Int)
normalizeWorldGenInputs worldSize plateCount =
    (normalizeWorldSize worldSize, normalizePlateCount plateCount)

normalizeWorldGenConfig ∷ WorldGenConfig → WorldGenConfig
normalizeWorldGenConfig cfg =
    let (worldSize, plateCount) =
            normalizeWorldGenInputs (wgcWorldSize cfg) (wgcPlateCount cfg)
    in cfg { wgcWorldSize = worldSize, wgcPlateCount = plateCount }
