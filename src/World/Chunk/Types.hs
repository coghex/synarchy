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
import World.Fluid.Types (FluidCell(..))

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
    , ctMats    ∷ !(VU.Vector Word8)    -- material IDs (tileType)
    , ctSlopes  ∷ !(VU.Vector Word8)    -- slope IDs (tileSlopeId)
    } deriving (Show, Eq)

instance NFData ColumnTiles where
    rnf (ColumnTiles z ms ss) = rnf z `seq` rnf ms `seq` rnf ss

-- | A chunk is a flat vector of 16×16 columns.
--   Index with: columnIndex lx ly = ly * chunkSize + lx
type Chunk = V.Vector ColumnTiles

-- | Empty column (glacier, beyond-world, etc.)
emptyColumn ∷ ColumnTiles
emptyColumn = ColumnTiles 0 VU.empty VU.empty

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
    , lcModified   ∷ !Bool
    } deriving (Show, Eq)

instance NFData LoadedChunk where
    rnf (LoadedChunk coord tiles surfMap terrainMap fluidMap modified) =
        rnf coord `seq` rnf tiles `seq` rnf surfMap `seq`
        rnf terrainMap `seq` rnf fluidMap `seq` rnf modified
