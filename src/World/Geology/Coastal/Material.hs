{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Per-tile coastal material selection: the beach-sand depth
--   profile and the hashed material rolls for beaches, wetlands,
--   and river deltas.
module World.Geology.Coastal.Material
    ( sandProfile
    , beachMaterial
    , wetlandMaterial
    , deltaMaterial
    ) where

import UPrelude
import World.Types

-- * Sand Profile

sandProfile ∷ Int → Int
sandProfile dist
    | dist ≤ 1  = seaLevel + 1
    | dist ≤ 2  = seaLevel + 1
    | dist ≤ 3  = seaLevel + 2
    | dist ≤ 4  = seaLevel + 2
    | dist ≤ 5  = seaLevel + 3
    | dist ≤ 6  = seaLevel + 3
    | dist ≤ 7  = seaLevel + 4
    | dist ≤ 8  = seaLevel + 5
    | otherwise = seaLevel + 6

-- * Material Selection

beachMaterial ∷ Int → Word8 → Float → Word8
beachMaterial dist origMat roll
    | dist ≤ 3  = 55
    | dist ≤ 5  = if roll < 0.85 then 55 else 54
    | dist ≤ 7  = if roll < 0.6  then 55 else 54
    | otherwise = transitionMaterial origMat roll

transitionMaterial ∷ Word8 → Float → Word8
transitionMaterial origMat roll = case origMat of
    56 → if roll < 0.6 then 54 else 53
    60 → if roll < 0.6 then 54 else 53
    50 → if roll < 0.6 then 51 else 52
    57 → if roll < 0.5 then 52 else 54
    58 → if roll < 0.6 then 51 else 52
    53 → 53
    54 → 54
    55 → 55
    62 → if roll < 0.5 then 53 else 54
    64 → if roll < 0.5 then 53 else 54
    _  → if roll < 0.5 then 54 else 53

wetlandMaterial ∷ Int → Float → Word8
wetlandMaterial dist roll
    | dist ≤ 3 =
        if roll < 0.4 then 64
        else if roll < 0.7 then 62
        else 63
    | dist ≤ 5 =
        if roll < 0.3 then 62
        else if roll < 0.6 then 63
        else 64
    | otherwise =
        if roll < 0.4 then 62
        else if roll < 0.7 then 51
        else 50

deltaMaterial ∷ Int → Float → Word8
deltaMaterial dist roll
    | dist ≤ 3 =
        if roll < 0.4 then 55
        else if roll < 0.6 then 54
        else if roll < 0.8 then 64
        else 61
    | dist ≤ 5 =
        if roll < 0.3 then 53
        else if roll < 0.5 then 52
        else if roll < 0.7 then 50
        else 62
    | otherwise =
        if roll < 0.4 then 53
        else 54
