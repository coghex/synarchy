{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module World.Chunk.Types
    ( ChunkCoord(..)
    , Chunk
    , ColumnIndex
    , columnIndex
    , chunkSize
    , ColumnTiles(..)
    , emptyColumn
    , ColumnStrata(..)
    , LoadedChunk(..)
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Data.Hashable (Hashable(..))
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import World.Material (MaterialId(..))
import World.Fluid.Types (FluidCell(..), IceMap, emptyIceMap)
import World.Flora.Types (FloraChunkData(..), emptyFloraChunkData)
import World.Magma.Overlay (MagmaOverlay)
import Structure.Types (ChunkStructures)

data ChunkCoord = ChunkCoord !Int !Int
    deriving (Show, Eq, Ord, Generic, Serialize)
instance NFData ChunkCoord where
    rnf (ChunkCoord x y) = rnf x `seq` rnf y

instance Hashable ChunkCoord where
    hashWithSalt s (ChunkCoord x y) = s `hashWithSalt` x `hashWithSalt` y

-- | A single column's tile data: contiguous z-range of non-air tiles.
--   Tiles from csStartZ to csStartZ + VU.length csMats - 1.
--   Air gaps within the range ARE stored (as matAir / slopeId 0)
--   so indexing is O(1). Only leading/trailing air is trimmed.
data ColumnTiles = ColumnTiles
    { ctStartZ  ∷ !Int
    , ctMats    ∷ !(VU.Vector Word8)    -- ^ material IDs
    , ctSlopes  ∷ !(VU.Vector Word8)    -- ^ slope IDs
    , ctVeg     ∷ !(VU.Vector Word8)    -- ^ vegetation IDs
    } deriving (Show, Eq)

instance NFData ColumnTiles where
    rnf (ColumnTiles z ms ss v) = rnf z `seq` rnf ms `seq` rnf ss `seq` rnf v

-- | A chunk is a flat vector of 16×16 columns.
--   Index with: columnIndex lx ly = ly * chunkSize + lx
type Chunk = V.Vector ColumnTiles

emptyColumn ∷ ColumnTiles
emptyColumn = ColumnTiles 0 VU.empty VU.empty VU.empty

chunkSize ∷ Int
chunkSize = 16
type ColumnIndex = Int
columnIndex ∷ Int → Int → ColumnIndex
columnIndex lx ly = ly * chunkSize + lx

data ColumnStrata = ColumnStrata
    { csStartZ ∷ !Int
    , csMats   ∷ !(VU.Vector MaterialId)
    } deriving (Show, Eq)
instance NFData ColumnStrata where
    rnf (ColumnStrata startZ mats) =
        rnf startZ `seq` rnf mats

data LoadedChunk = LoadedChunk
    { lcCoord      ∷ !ChunkCoord
    , lcTiles      ∷ !Chunk
    , lcSurfaceMap ∷ !(VU.Vector Int)
    , lcTerrainSurfaceMap ∷ !(VU.Vector Int)
    , lcFluidMap   ∷ !(V.Vector (Maybe FluidCell))
    , lcIceMap     ∷ !IceMap               -- ^ Ice overlay (frozen ocean/lake/alpine)
    , lcFlora      ∷ !FloraChunkData
    , lcSideDeco   ∷ !(VU.Vector Word8)    -- ^ Side-face decorations per column
    , lcWaterTableMap ∷ !(VU.Vector Int)
      -- ^ Per-tile water-table elevation. Below this z value the ground
      --   is saturated. When wt ≥ terrain[t] the tile shows surface
      --   water; subsurface query for digging: position (lx, ly, z) is
      --   wet iff z ≤ lcWaterTableMap[ly*chunkSize+lx]. Computed by
      --   World.Hydrology.WaterTable, see DESIGN.md in that folder.
    , lcMagma      ∷ !(Maybe MagmaOverlay)
      -- ^ Sparse lava overlay produced by 'discoverChunkLava' at chunk
      --   gen. Nothing in nearly every chunk; when present, the
      --   overlay's moSurface map drives lava placement in
      --   composeFluidMap (highest priority above water + ice).
    , lcStructures ∷ !ChunkStructures
      -- ^ Per-chunk structure overlay (floors/walls/posts/ceilings),
      --   built by replaying this chunk's WeSetStructure edits. Keyed
      --   (gx,gy,slot-tag); pieces hold texture PALETTE IDS (resolved to
      --   handles at render). Evicts + reloads with the chunk.
    } deriving (Show, Eq)
-- Removed: lcModified :: Bool. The world's edit log
-- (WorldState.wsEditsRef) is now the source of truth for "this chunk
-- has been edited". Eviction is purely distance-based; edits survive
-- via replay on regeneration.

instance NFData LoadedChunk where
    rnf (LoadedChunk coord tiles surfMap terrainMap fluidMap iceMap flora sideDeco wtMap magma structs) =
        rnf coord `seq` rnf tiles `seq` rnf surfMap `seq`
        rnf terrainMap `seq` rnf fluidMap `seq` rnf iceMap `seq`
        rnf flora `seq` rnf sideDeco `seq` rnf wtMap `seq` rnf magma `seq`
        rnf structs
