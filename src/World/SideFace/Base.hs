{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.SideFace.Base
    ( SideDecoType(..)
    , sideDecoBase
    , sideDecoVariants
    , isWaterfallDeco
    ) where

import UPrelude

-- | Types of side-face decorations on cliff walls.
data SideDecoType
    = DecoNone
    | DecoVines      -- ^ IDs 1-4
    | DecoMoss       -- ^ IDs 5-8
    | DecoIce        -- ^ IDs 9-12
    | DecoMineral    -- ^ IDs 13-16
    | DecoWaterfall  -- ^ IDs 17-20 (dynamic, from active sim)
    deriving (Show, Eq, Enum, Bounded)

-- | Number of visual variants per deco type.
sideDecoVariants ∷ Int
sideDecoVariants = 4

-- | Base deco ID for each type: typeBase + variant (0-3) = final ID.
sideDecoBase ∷ SideDecoType → Word8
sideDecoBase DecoNone      = 0
sideDecoBase DecoVines     = 1
sideDecoBase DecoMoss      = 5
sideDecoBase DecoIce       = 9
sideDecoBase DecoMineral   = 13
sideDecoBase DecoWaterfall = 17

-- | Check if a deco ID is a waterfall (17-20).
isWaterfallDeco ∷ Word8 → Bool
isWaterfallDeco d = d ≥ 17 ∧ d ≤ 20
