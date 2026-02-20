{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Hash
    ( hashGeo
    , hashToFloatGeo
    , hashToRangeGeo
    , smoothstepGeo
    , wrappedDeltaXGeo
    , wrappedDeltaUV
    , scaleCount
    ) where

import UPrelude
import Data.Bits (xor, shiftR, (.&.))
import Data.Word (Word32, Word64)

-----------------------------------------------------------
-- Hash Utilities
-----------------------------------------------------------

hashGeo ∷ Word64 → Int → Int → Word32
hashGeo seed idx prop =
    let h0 = fromIntegral seed ∷ Word64
        h1 = h0 `xor` (fromIntegral idx * 0x517cc1b727220a95)
        h2 = h1 `xor` (fromIntegral prop * 0x6c62272e07bb0142)
        h3 = h2 `xor` (h2 `shiftR` 33)
        h4 = h3 * 0xff51afd7ed558ccd
        h5 = h4 `xor` (h4 `shiftR` 33)
        h6 = h5 * 0xc4ceb9fe1a85ec53
        h7 = h6 `xor` (h6 `shiftR` 33)
    in fromIntegral (h7 .&. 0xFFFFFFFF)

hashToFloatGeo ∷ Word32 → Float
hashToFloatGeo h = fromIntegral (h .&. 0x00FFFFFF)
                 / fromIntegral (0x00FFFFFF ∷ Word32)

hashToRangeGeo ∷ Word32 → Int → Int → Int
hashToRangeGeo h lo hi =
    let f = hashToFloatGeo h
        span' = hi - lo + 1
    in lo + floor (f * fromIntegral span')

smoothstepGeo ∷ Float → Float
smoothstepGeo t = t * t * (3.0 - 2.0 * t)

-- | Wrapped X distance for cylindrical world (OLD — wraps gx only).
--   Produces diagonal seam. Use wrappedDeltaUV for correct u-axis wrapping.
{-# INLINE wrappedDeltaXGeo #-}
wrappedDeltaXGeo ∷ Int → Int → Int → Int
wrappedDeltaXGeo worldSize x1 x2 =
    let w = worldSize * 16
        raw = x2 - x1
        halfW = w `div` 2
    in ((raw + halfW) `mod` w + w) `mod` w - halfW

-- | Wrapped distance between two points in the isometric u-wrapped world.
--   Returns (dx, dy) where dx and dy are the shortest-path deltas
--   accounting for cylindrical wrapping along the u-axis (gx - gy).
{-# INLINE wrappedDeltaUV #-}
wrappedDeltaUV ∷ Int → Int → Int → Int → Int → (Int, Int)
wrappedDeltaUV worldSize gx1 gy1 gx2 gy2 =
    let w = worldSize * 16
        halfW = w `div` 2
        du = (gx1 - gy1) - (gx2 - gy2)
        dv = (gx1 + gy1) - (gx2 + gy2)
        wrappedDU = ((du + halfW) `mod` w + w) `mod` w - halfW
        dx = (wrappedDU + dv) `div` 2
        dy = (dv - wrappedDU) `div` 2
    in (dx, dy)

-----------------------------------------------------------
-- Feature Scaling
-----------------------------------------------------------

scaleCount ∷ Int → Int → Int
scaleCount worldSize baseCount =
    let areaRatio = (worldSize * worldSize) `div` (128 * 128)
    in max baseCount (baseCount * areaRatio)
