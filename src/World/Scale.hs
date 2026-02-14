{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Scale
    ( -- * Types
      WorldScale(..)
      -- * Construction
    , computeWorldScale
      -- * Scaling functions
    , scaleElev
    , scaleDist
    , scaleElevI
    , scaleDistI
      -- * Constants
    , referenceWorldSize
    ) where

import UPrelude

-- | Reference world size: at this size, all raw geological
--   values (in meters / tiles) are used as-is.
--   worldSize=512 → 8192 tiles across.
referenceWorldSize ∷ Int
referenceWorldSize = 512

-- | Pre-computed scaling factors for a given world size.
--   At referenceWorldSize, scale = 1.0.
--   At worldSize=256, scale = 0.5.
--   At worldSize=64, scale = 0.125.
data WorldScale = WorldScale
    { wsWorldSize ∷ !Int
    , wsScale     ∷ !Float   -- ^ worldSize / referenceWorldSize
    } deriving (Show)

-- | Compute the world scale from world size.
computeWorldScale ∷ Int → WorldScale
computeWorldScale worldSize =
    WorldScale
        { wsWorldSize = worldSize
        , wsScale     = fromIntegral worldSize
                      / fromIntegral referenceWorldSize
        }

-- | Scale a Float elevation value.
scaleElev ∷ WorldScale → Float → Float
scaleElev ws v = v * wsScale ws

-- | Scale a Float horizontal distance value.
scaleDist ∷ WorldScale → Float → Float
scaleDist ws v = v * wsScale ws

-- | Scale and round to Int.
scaleElevI ∷ WorldScale → Int → Int
scaleElevI ws v = round (fromIntegral v * wsScale ws ∷ Float)

-- | Scale a horizontal Int distance.
scaleDistI ∷ WorldScale → Int → Int
scaleDistI ws v = round (fromIntegral v * wsScale ws ∷ Float)
