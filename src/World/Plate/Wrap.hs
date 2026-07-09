{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Cylindrical world-wrap arithmetic, split out of "World.Plate"
--   (issue #560). Pure coordinate math shared by plate queries, the
--   coastal/rift fields, and worldgen elsewhere.
module World.Plate.Wrap
    ( worldWidthTiles
    , wrapGlobalU
    , wrapGlobalX
    , wrappedDeltaU
    ) where

import UPrelude
import World.Chunk.Types (chunkSize)

-- * Cylindrical Wrapping

worldWidthTiles ∷ Int → Int
worldWidthTiles worldSize = worldSize * chunkSize

-- | Wrap a global coordinate pair so that the isometric u-axis
--   (gx - gy, which maps to screen X) wraps by worldTiles,
--   while the v-axis (gx + gy, which maps to screen Y) stays fixed.
--
--   This produces a vertical seam on screen instead of a diagonal one.
wrapGlobalU ∷ Int → Int → Int → (Int, Int)
wrapGlobalU worldSize gx gy =
    let w = worldWidthTiles worldSize    -- worldTiles
        halfW = w `div` 2
        u = gx - gy
        v = gx + gy
        -- Wrap u into [-halfW, halfW)
        wrappedU = ((u + halfW) `mod` w + w) `mod` w - halfW
        -- Recover gx, gy from (wrappedU, v)
        gx' = (wrappedU + v) `div` 2
        gy' = (v - wrappedU) `div` 2
    in (gx', gy')

-- | Old wrap: wraps gx only. Produces diagonal seam.
--   Use wrapGlobalU instead for screen-aligned wrapping.
wrapGlobalX ∷ Int → Int → Int
wrapGlobalX worldSize gx =
    let w = worldWidthTiles worldSize
        halfW = w `div` 2
        wrapped = ((gx + halfW) `mod` w + w) `mod` w - halfW
    in wrapped

-- | Wrapped distance in the u-axis between two points
wrappedDeltaU ∷ Int → Int → Int → Int → Int → Int
wrappedDeltaU worldSize gx1 gy1 gx2 gy2 =
    let w = worldWidthTiles worldSize
        u1 = gx1 - gy1
        u2 = gx2 - gy2
        raw = u2 - u1
        halfW = w `div` 2
    in ((raw + halfW) `mod` w + w) `mod` w - halfW
