{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Direction
    ( Direction(..)
    , dirIndex
    , indexToDir
    , allDirections
    ) where

import UPrelude

-- | Eight compass directions, ordered clockwise from South.
--   The index order matters for the rotation arithmetic.
data Direction = DirS | DirSW | DirW | DirNW | DirN | DirNE | DirE | DirSE
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Map a Direction to its clockwise index (S=0, SW=1, … SE=7)
dirIndex ∷ Direction → Int
dirIndex = fromEnum

-- | Inverse of dirIndex (mod 8)
indexToDir ∷ Int → Direction
indexToDir n = toEnum (n `mod` 8)

-- | All eight directions in clockwise order
allDirections ∷ [Direction]
allDirections = [minBound .. maxBound]
