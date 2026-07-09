{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Polar glacier-border tests, split out of "World.Plate" (issue #560).
module World.Plate.Glacier
    ( isGlacierZone
    , isBeyondGlacier
    ) where

import UPrelude
import World.Chunk.Types (chunkSize)

-- * Glacier Border

glacierWidthRows ∷ Int
glacierWidthRows = chunkSize

isGlacierZone ∷ Int → Int → Int → Bool
isGlacierZone worldSize gx gy =
    let halfTiles = (worldSize * chunkSize) `div` 2
        glacierEdge = halfTiles - glacierWidthRows
        screenRow = gx + gy
    in abs screenRow ≥ glacierEdge

isBeyondGlacier ∷ Int → Int → Int → Bool
isBeyondGlacier worldSize gx gy =
    let halfTiles = (worldSize * chunkSize) `div` 2
        screenRow = gx + gy
    in abs screenRow > halfTiles
