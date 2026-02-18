{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate.Constants
    ( chunkLoadRadius
    , viewDepth
    , chunkBorder
    ) where

import UPrelude

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------

chunkLoadRadius ∷ Int
chunkLoadRadius = 2

-- | How many z-levels below the z-slice are rendered.
--   This is a RENDER window, not a generation limit.
viewDepth ∷ Int
viewDepth = 250

-- | Border size around each chunk for neighbor lookups.
--   Needs to be large enough for erosion neighbor access.
--   4 tiles gives comfortable margin for smoothing.
chunkBorder ∷ Int
chunkBorder = 4
