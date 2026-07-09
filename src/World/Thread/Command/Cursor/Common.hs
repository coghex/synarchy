{-# LANGUAGE UnicodeSyntax #-}

-- | Shared cursor-designation constants. Split out of
--   "World.Thread.Command.Cursor" (issue #564).
module World.Thread.Command.Cursor.Common
    ( maxDesignateSide
    ) where

import UPrelude

-- | Cap on the designation rectangle's side length. Guards against a
--   misclick across the map turning into a 100k-tile designation.
maxDesignateSide ∷ Int
maxDesignateSide = 128
