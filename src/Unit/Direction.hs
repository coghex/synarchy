{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module Unit.Direction
    ( Direction(..)
    , dirIndex
    , indexToDir
    , allDirections
    , mirrorDir
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

-- | The "western half" of the compass (SW, W, NW) can be obtained by
--   horizontally mirroring their eastern counterparts (SE, E, NE). This
--   lets bilaterally-symmetric animations ship 5 directional sprites
--   instead of 8 — the renderer flips UVs at draw time for the western
--   directions. Returns @Nothing@ for directions that are their own
--   canonical (S, N, NE, E, SE) — those use their own asset, no flip.
--
--   Used by `Unit.Render.resolveTexture` / `pickFrame` to fall back to
--   the mirror direction's sprite + a flipX flag when the requested
--   direction has no entry. Per-animation opt-in is governed by which
--   directions get authored — if an animation needs an asymmetric prop
--   (e.g. weapon in the right hand) the asset author simply provides
--   all 8 sprites and the mirror fallback never triggers.
mirrorDir ∷ Direction → Maybe Direction
mirrorDir DirSW = Just DirSE
mirrorDir DirW  = Just DirE
mirrorDir DirNW = Just DirNE
mirrorDir _     = Nothing
