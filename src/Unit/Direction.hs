{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module Unit.Direction
    ( Direction(..)
    , dirIndex
    , indexToDir
    , allDirections
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Serialize (Serialize)

-- | Eight compass directions, ordered clockwise from South.
--
--   APPEND-ONLY. The constructor order is load-bearing in two ways:
--   (1) `Enum`-derived `fromEnum`/`toEnum` drive the rotation arithmetic
--   in `dirIndex`/`indexToDir`, and (2) `Generic`-derived `Serialize` is
--   positional by constructor tag, so reordering or inserting a
--   constructor silently maps existing saved `usFacing` values to the
--   wrong direction. If the geometry ever needs different cardinality
--   (16-way etc.), bump `currentSaveVersion` in `World.Save.Types`.
data Direction = DirS | DirSW | DirW | DirNW | DirN | DirNE | DirE | DirSE
    deriving (Show, Eq, Ord, Enum, Bounded, Generic, Serialize)

-- | Map a Direction to its clockwise index (S=0, SW=1, … SE=7)
dirIndex ∷ Direction → Int
dirIndex = fromEnum

-- | Inverse of dirIndex (mod 8)
indexToDir ∷ Int → Direction
indexToDir n = toEnum (n `mod` 8)

-- | All eight directions in clockwise order
allDirections ∷ [Direction]
allDirections = [minBound .. maxBound]
