{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate.Constants
    ( chunkLoadRadius
    , viewDepth
    , chunkBorder
    ) where

import UPrelude

-- * Constants

chunkLoadRadius ∷ Int
chunkLoadRadius = 2

-- | How many z-levels below the z-slice are rendered.
--   This is a RENDER window, not a generation limit.
viewDepth ∷ Int
viewDepth = 250

-- | Border size around each chunk for neighbor lookups.
--   Must be larger than maxCoastalDist (in Coastal.hs) so that
--   adjacent chunks' bordered regions overlap at ALL ocean tiles
--   within beach distance of the chunk boundary. The extra margin
--   prevents seams where one chunk's BFS sees an ocean tile but
--   the neighbor's border doesn't include it.
chunkBorder ∷ Int
chunkBorder = 14
