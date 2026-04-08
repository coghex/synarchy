{-# LANGUAGE Strict, UnicodeSyntax #-}
module Sim.Command.Types
    ( SimCommand(..)
    ) where

import UPrelude
import Control.Concurrent.MVar (MVar)
import Data.IORef (IORef)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..))
import World.Fluid.Internal (FluidMap)
import World.Tile.Types (WorldTileData(..))

data SimCommand
    = SimActivateWorld !(IORef WorldTileData)
        -- ^ Set the active world tile data ref (called on WorldShow)
    | SimDeactivateWorld
        -- ^ Clear the active world (called on WorldHide/WorldDestroy)
    | SimChunkLoaded !ChunkCoord !FluidMap !(VU.Vector Int)
        -- ^ Chunk loaded: coord, initial fluid map, terrain surface map
    | SimChunkUnloaded !ChunkCoord
        -- ^ Chunk evicted — stop simulating
    | SimTerrainModified !ChunkCoord ![(Int, Int)]
        -- ^ Terrain changed at local indices: [(localIndex, newElevation)]
    | SimSetTickRate !Int
        -- ^ Tick rate in microseconds (default 100000 = 10Hz)
    | SimPause
    | SimResume
    | SimFastSettleAll !(MVar ())
        -- ^ Synchronously run all settle ticks (no sleeping) until all
        --   chunks have scsSettleTicks == 0 and no chunks are active.
        --   Then writes the result back to wsTilesRef, sets ssPaused,
        --   and signals the MVar. Used by dump mode to get a stable
        --   simulation state without waiting for the live sim loop.

instance Show SimCommand where
    show (SimActivateWorld _)     = "SimActivateWorld"
    show SimDeactivateWorld       = "SimDeactivateWorld"
    show (SimChunkLoaded cc _ _)  = "SimChunkLoaded " <> show cc
    show (SimChunkUnloaded cc)    = "SimChunkUnloaded " <> show cc
    show (SimTerrainModified cc mods) =
        "SimTerrainModified " <> show cc <> " (" <> show (length mods) <> " tiles)"
    show (SimSetTickRate r) = "SimSetTickRate " <> show r
    show SimPause  = "SimPause"
    show SimResume = "SimResume"
    show (SimFastSettleAll _) = "SimFastSettleAll"
