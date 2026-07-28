{-# LANGUAGE Strict #-}
module World.Constants
    ( seaLevel
    ) where

import UPrelude

-- | Global sea level. Tiles at or below this elevation
--   in ocean-connected regions are submerged.
seaLevel ∷ Int
seaLevel = 0
