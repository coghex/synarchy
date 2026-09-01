{-# LANGUAGE Strict #-}
module Sim.Command.Types
    ( SimCommand(..)
    ) where

import UPrelude
import Control.Concurrent.MVar (MVar)
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..))
import World.Page.Types (WorldPageId(..))
import World.Fluid.Internal (FluidMap)
import Sim.Topology (SimTopology)

data SimCommand
    = SimActivateWorld !WorldPageId !SimTopology
        -- ^ A world became visible (WorldShow): start simulating it. The
        --   sim no longer holds the tile ref — it emits 'WorldApplyFluids'
        --   (tagged with this page id) to the world thread, the sole writer
        --   of 'wsTilesRef'.
        --
        --   Carries the page's seam topology (#2044): activation is what
        --   lets this world tick, so the topology its neighbour probes
        --   need is established in the same message.
    | SimDeactivateWorld !WorldPageId
        -- ^ A world was hidden: stop ticking it but KEEP its loaded chunks
        --   so a later WorldShow can resume simulating them. Dropping the
        --   chunks here left a hidden→shown world's sim inert, because
        --   ChunkLoading never re-emits SimChunkLoaded for coords already in
        --   wsTilesRef. Other worlds are untouched (#55).
    | SimDropWorld !WorldPageId
        -- ^ A world was destroyed: discard its sim state entirely (chunks +
        --   active flag). Used on WorldDestroy / destroyAll, where the
        --   chunks are gone for good (#61).
    | SimChunkLoaded !WorldPageId !SimTopology !ChunkCoord !FluidMap !(VU.Vector Int)
        -- ^ Chunk loaded in a world: page id, seam topology, coord,
        --   initial fluid map, terrain surface map. This and
        --   'SimChunkEdited' are the only two ways a chunk enters sim
        --   state, so carrying the topology here is what guarantees a
        --   world with anything to simulate — by a normal tick or by
        --   'SimFastSettleAll', which processes every stored world
        --   regardless of activation — already knows its seam (#2044).
    | SimChunkUnloaded !WorldPageId !ChunkCoord
        -- ^ Chunk evicted from a world — stop simulating it
    | SimChunkEdited !WorldPageId !SimTopology !ChunkCoord !Word64 !FluidMap !(VU.Vector Int)
        -- ^ A live terrain/fluid edit landed in a world's chunk: page id,
        --   seam topology, coord, the chunk's new LIVE-EDIT GENERATION,
        --   and the post-edit
        --   fluid map and terrain surface (read from the authoritative
        --   tiles). Re-seeds the sim chunk AND activates it (and its
        --   cardinal neighbours) so the new fluid actually flows/settles —
        --   re-using SimChunkLoaded here left the chunk inactive, so edited
        --   fluid sat frozen (#60). The cardinal neighbours are resolved
        --   through the carried topology, so an edit beside the u seam
        --   activates the physically adjacent far-side chunk rather than
        --   a raw key nothing is stored under (#2044).
        --
        --   The generation is the explicit causal provenance the two
        --   independent queues otherwise lack (#1596): the sim keeps it as
        --   'Sim.State.Types.scsEditGen' and stamps every writeback it
        --   later produces for this chunk with it, so the world thread can
        --   tell a writeback derived from the POST-edit chunk from one
        --   computed before the edit. See
        --   'World.State.Types.wsChunkEditGenRef' for the full protocol.
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
    show (SimActivateWorld p t)   = "SimActivateWorld " <> show p <> " " <> show t
    show (SimDeactivateWorld p)   = "SimDeactivateWorld " <> show p
    show (SimDropWorld p)         = "SimDropWorld " <> show p
    show (SimChunkLoaded p t cc _ _) =
        "SimChunkLoaded " <> show p <> " " <> show t <> " " <> show cc
    show (SimChunkUnloaded p cc)  = "SimChunkUnloaded " <> show p <> " " <> show cc
    show (SimChunkEdited p t cc g _ _) =
        "SimChunkEdited " <> show p <> " " <> show t <> " " <> show cc
                          <> " gen=" <> show g
    show (SimSetTickRate r) = "SimSetTickRate " <> show r
    show SimPause  = "SimPause"
    show SimResume = "SimResume"
    show (SimFastSettleAll _) = "SimFastSettleAll"
