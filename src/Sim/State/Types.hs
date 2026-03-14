{-# LANGUAGE Strict, UnicodeSyntax #-}
module Sim.State.Types
    ( SimState(..)
    , SimChunkState(..)
    , emptySimState
    ) where

import UPrelude
import Data.IORef (IORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..))
import World.Fluid.Internal (FluidMap)
import World.Tile.Types (WorldTileData(..))

data SimState = SimState
    { ssChunks      ∷ !(HM.HashMap ChunkCoord SimChunkState)
    , ssTickRate    ∷ !Int              -- ^ Microseconds between ticks (default 100000)
    , ssDirtyChunks ∷ !(HS.HashSet ChunkCoord)  -- ^ Chunks modified this tick
    , ssPaused     ∷ !Bool
    , ssTilesRef   ∷ !(Maybe (IORef WorldTileData))
        -- ^ Ref to the active world's tile data (set on SimActivateWorld)
    }

data SimChunkState = SimChunkState
    { scsFluid       ∷ !FluidMap          -- ^ Live fluid state
    , scsTerrain     ∷ !(VU.Vector Int)   -- ^ Terrain surface (read-only until modified)
    , scsGenFluid    ∷ !FluidMap          -- ^ Original generated fluid (for diff on save)
    , scsSettleTicks ∷ !Int               -- ^ Remaining fast settle ticks (0 = settled)
    }

emptySimState ∷ SimState
emptySimState = SimState
    { ssChunks      = HM.empty
    , ssTickRate    = 100000  -- 10 Hz
    , ssDirtyChunks = HS.empty
    , ssPaused     = False
    , ssTilesRef   = Nothing
    }
