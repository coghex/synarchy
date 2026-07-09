{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Deterministic hashing and small interpolation helpers shared across
--   the "World.Plate.*" split (issue #560). No local dependencies beyond
--   'UPrelude' — every other Plate submodule builds on this one.
module World.Plate.Hash
    ( plateHash
    , hashCoord
    , hashToFloat'
    , hashToRange
    , smoothstep
    , lerp
    ) where

import UPrelude

-- * Noise & Hash

plateHash ∷ Word64 → Int → Int → Word32
plateHash seed plateIdx propIdx =
    hashCoord (seed + fromIntegral propIdx * 7919) plateIdx propIdx

hashCoord ∷ Word64 → Int → Int → Word32
hashCoord seed x y =
    let h0 = fromIntegral seed ∷ Word64
        h1 = h0 `xor` (fromIntegral x * 0x517cc1b727220a95)
        h2 = h1 `xor` (fromIntegral y * 0x6c62272e07bb0142)
        h3 = h2 `xor` (h2 `shiftR` 33)
        h4 = h3 * 0xff51afd7ed558ccd
        h5 = h4 `xor` (h4 `shiftR` 33)
        h6 = h5 * 0xc4ceb9fe1a85ec53
        h7 = h6 `xor` (h6 `shiftR` 33)
    in fromIntegral (h7 .&. 0xFFFFFFFF)

hashToFloat' ∷ Word32 → Float
hashToFloat' h = fromIntegral (h .&. 0x00FFFFFF) / fromIntegral (0x00FFFFFF ∷ Word32)

hashToRange ∷ Word32 → Int → Int → Int
hashToRange h lo hi =
    let f = hashToFloat' h
        span' = hi - lo + 1
    in min hi (lo + floor (f * fromIntegral span'))

smoothstep ∷ Float → Float
smoothstep t = t * t * (3.0 - 2.0 * t)

lerp ∷ Float → Float → Float → Float
lerp t a b = a + t * (b - a)
