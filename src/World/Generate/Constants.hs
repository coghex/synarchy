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
--   (Historically this had to exceed maxCoastalDist so adjacent
--   chunks' windowed coastal BFS agreed; since the save-v25 global
--   coastal pass, chunk gen just replays per-tile table deltas and
--   coastal reach is independent of this border — #220 widened
--   maxCoastalDist to 28 > chunkBorder with no seam risk. The border
--   still bounds every OTHER windowed neighborhood op in the chunk
--   pipeline; see the chunk-pipeline window-op rule.)
chunkBorder ∷ Int
chunkBorder = 14
