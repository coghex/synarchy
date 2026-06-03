{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | Per-chunk sparse storage for exposed lava and (future) dig-revealed
--   lava tiles. Lives in its own module — separate from 'World.Magma.Types'
--   — so 'World.Chunk.Types' can reference the overlay without dragging
--   in the volcano-feature types that close a module-graph cycle through
--   'World.Fluid.Lake.Types'.
module World.Magma.Overlay
    ( MagmaOverlay(..)
    , emptyMagmaOverlay
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import qualified Data.HashMap.Strict as HM
import World.Fluid.Types (FluidCell(..))

-- | Per-chunk sparse storage. Lives on 'LoadedChunk' as
--   @Maybe MagmaOverlay@. Empty / Nothing in nearly every chunk.
data MagmaOverlay = MagmaOverlay
    { moSurface  ∷ !(HM.HashMap (Int, Int) FluidCell)
        -- ^ Tiles where a chute / chamber reaches or exceeds the
        --   surface terrain AND the tile is above sea level (or in
        --   a non-ocean chunk). Keys are global tile coords; the
        --   stored 'FluidCell' is @FluidCell Lava surfaceZ@.
    , moBasaltCap ∷ !(HM.HashMap (Int, Int) Int)
        -- ^ Tiles where a chamber/chute would breach under the
        --   ocean. Instead of emerging lava (which would punch a
        --   visual hole through the ocean), 'discoverChunkLava'
        --   marks the tile here with a target terrain Z that seals
        --   the chamber. Chunk-gen raises the terrain to that Z and
        --   stamps 'matBasalt' on the top column tile; the ocean
        --   then fills the column above the cap.
    , moRevealed ∷ !(HM.HashMap (Int, Int, Int) ())
        -- ^ Reserved for future dig integration. ALWAYS EMPTY in
        --   phase 1 + 2; included now so the eventual save schema
        --   doesn't need restructuring.
    } deriving (Show, Eq, Generic, NFData)

emptyMagmaOverlay ∷ MagmaOverlay
emptyMagmaOverlay = MagmaOverlay HM.empty HM.empty HM.empty
