{-# LANGUAGE Strict, UnicodeSyntax #-}
module Sim.Command.Types
    ( SimCommand(..)
    ) where

import UPrelude
import Control.Concurrent.MVar (MVar)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..))
import World.Page.Types (WorldPageId(..))
import World.Fluid.Internal (FluidMap)

data SimCommand
    = SimActivateWorld !WorldPageId
        -- ^ A world became visible (WorldShow): start simulating it. The
        --   sim no longer holds the tile ref — it emits 'WorldApplyFluids'
        --   (tagged with this page id) to the world thread, the sole writer
        --   of 'wsTilesRef'.
    | SimDeactivateWorld !WorldPageId
        -- ^ Drop one world's sim state (called on WorldHide/WorldDestroy).
        --   Only that world's chunks/active flag are cleared; other
        --   simulated worlds are untouched (#55, #61).
    | SimChunkLoaded !WorldPageId !ChunkCoord !FluidMap !(VU.Vector Int)
        -- ^ Chunk loaded in a world: page id, coord, initial fluid map,
        --   terrain surface map
    | SimChunkUnloaded !WorldPageId !ChunkCoord
        -- ^ Chunk evicted from a world — stop simulating it
    | SimChunkEdited !WorldPageId !ChunkCoord !FluidMap !(VU.Vector Int)
        -- ^ A live terrain/fluid edit landed in a world's chunk: page id,
        --   coord, the post-edit fluid map and terrain surface (read from
        --   the authoritative tiles). Re-seeds the sim chunk AND activates
        --   it (and its cardinal neighbours) so the new fluid actually
        --   flows/settles — re-using SimChunkLoaded here left the chunk
        --   inactive, so edited fluid sat frozen (#60).
    | SimSetTickRate !Int
        -- ^ Tick rate in microseconds (default 100000 = 10Hz). Global.
    | SimPause
    | SimResume
    | SimFastSettleAll !(MVar ())
        -- ^ Synchronously run all settle ticks (no sleeping) across every
        --   world until each chunk has scsSettleTicks == 0 and no chunk is
        --   active. Then emits a 'WorldApplyFluids' batch (per world) with
        --   an ack and waits for the world thread to apply it, sets
        --   ssPaused, and signals the MVar. Used by dump mode to get a
        --   stable simulation state without waiting for the live sim loop.

instance Show SimCommand where
    show (SimActivateWorld p)     = "SimActivateWorld " <> show p
    show (SimDeactivateWorld p)   = "SimDeactivateWorld " <> show p
    show (SimChunkLoaded p cc _ _) = "SimChunkLoaded " <> show p <> " " <> show cc
    show (SimChunkUnloaded p cc)  = "SimChunkUnloaded " <> show p <> " " <> show cc
    show (SimChunkEdited p cc _ _) =
        "SimChunkEdited " <> show p <> " " <> show cc
    show (SimSetTickRate r) = "SimSetTickRate " <> show r
    show SimPause  = "SimPause"
    show SimResume = "SimResume"
    show (SimFastSettleAll _) = "SimFastSettleAll"
