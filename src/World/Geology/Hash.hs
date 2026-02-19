{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Hash
    ( hashGeo
    , hashToFloatGeo
    , hashToRangeGeo
    , smoothstepGeo
    , wrappedDeltaXGeo
    , scaleCount
    ) where

import UPrelude
import Data.Bits (xor, shiftR, (.&.))
import Data.Word (Word32, Word64)

-----------------------------------------------------------
-- Hash Utilities
-----------------------------------------------------------

-- | Hash for geology — takes seed, index, and property.
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

-- | Wrapped X distance for cylindrical world.
{-# INLINE wrappedDeltaXGeo #-}
wrappedDeltaXGeo ∷ Int → Int → Int → Int
wrappedDeltaXGeo worldSize x1 x2 =
    let w = worldSize * 16
        raw = x2 - x1
        halfW = w `div` 2
    in ((raw + halfW) `mod` w + w) `mod` w - halfW

-----------------------------------------------------------
-- Feature Scaling
-----------------------------------------------------------

-- | Scale a feature count by world area relative to a 128-chunk baseline.
--   At worldSize=128 returns the base count unchanged.
--   At worldSize=256 returns 4× the base count, etc.
scaleCount ∷ Int → Int → Int
scaleCount worldSize baseCount =
    let areaRatio = (worldSize * worldSize) `div` (128 * 128)
    in max baseCount (baseCount * areaRatio)
