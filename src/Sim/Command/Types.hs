{-# LANGUAGE Strict #-}
module Sim.Command.Types
    ( SimCommand(..)
    , ReactionChunkSync(..)
    , FastSettleRequest(..)
    , FastSettleOutcome(..)
    ) where

import UPrelude
import Control.Concurrent.MVar (MVar)
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Residency (ChunkGeneration)
import World.Chunk.Types (ChunkCoord(..))
import World.Page.Types (WorldPageId(..))
import World.Fluid.Internal (FluidMap)
import Sim.Topology (SimTopology)

data SimCommand
    = SimActivateWorld !WorldPageId !ChunkGeneration !SimTopology
        -- ^ A world became visible (WorldShow): start simulating it. The
        --   sim no longer holds the tile ref — it emits 'WorldApplyFluids'
        --   (tagged with this page id) to the world thread, the sole writer
        --   of 'wsTilesRef'.
        --
        --   Carries the page's seam topology (#2044): activation is what
        --   lets this world tick, so the topology its neighbour probes
        --   need is established in the same message. The
        --   'World.Chunk.Residency.ChunkGeneration' beside it is the
        --   sending page's INCARNATION epoch, read from that page's own
        --   'World.State.Types.WorldState' and stored the same way
        --   (#2477); see 'Sim.State.Types.swsIncarnation'.
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
    | SimChunkLoaded !WorldPageId !ChunkGeneration !SimTopology !ChunkCoord !FluidMap !(VU.Vector Int)
        -- ^ Chunk loaded in a world: page id, the page's incarnation
        --   epoch (#2477), seam topology, coord,
        --   initial fluid map, terrain surface map. This and
        --   'SimChunkEdited' are the only two ways a chunk enters sim
        --   state, so carrying the topology here is what guarantees a
        --   world with anything to simulate — by a normal tick or by
        --   'SimFastSettleAll', which processes every stored world
        --   regardless of activation — already knows its seam (#2044).
    | SimChunkUnloaded !WorldPageId !ChunkCoord
        -- ^ Chunk evicted from a world — stop simulating it
    | SimChunkEdited !WorldPageId !ChunkGeneration !SimTopology !ChunkCoord !Word64 !FluidMap !(VU.Vector Int)
        -- ^ A live terrain/fluid edit landed in a world's chunk: page id,
        --   the page's incarnation epoch (#2477),
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
    | SimReactionCommitted !WorldPageId !ChunkGeneration !SimTopology
                           ![ReactionChunkSync]
        -- ^ A coherent reaction result was ADMITTED and committed by the
        --   world thread (#2485): page id, the page's incarnation epoch,
        --   seam topology, and one entry per participating chunk.
        --
        --   This is the post-commit half of 'SimChunkEdited' and exists
        --   because that message cannot express it. 'SimChunkEdited'
        --   re-seeds a chunk's whole active grid from the passive
        --   'World.Fluid.Internal.FluidMap' through
        --   'Sim.Fluid.Types.fluidCellToActive', which rounds a volume up
        --   to whole surface levels: the 1 unit of water a reaction left
        --   behind would come back as 7. So this message carries the
        --   authoritative post-edit TERRAIN and the new generation, and
        --   the sim keeps the exact active volumes it already holds,
        --   emptying only the cells that became stone. The result is a
        --   sim chunk holding BOTH halves of the commit — the reaction's
        --   own fluid outcome and the terrain edit — rather than one
        --   rebuilt from a snapshot that predates either.
        --
        --   An INACTIVE or absent chunk has no exact volumes to keep, so
        --   it re-seeds from 'rcsFluid' exactly as 'SimChunkEdited'
        --   would. Both cases adopt 'rcsEditGen', which is what makes
        --   the writebacks this chunk produces from here on acceptable
        --   to the world thread again (#1596), and wake the chunk and
        --   its physical cardinal neighbours so the surviving fluid
        --   keeps flowing around the new stone.
        --
        --   A REJECTED (stale) result sends no such message: the world
        --   thread converges those chunks with an ordinary
        --   'World.Thread.Command.Edit.Sync.syncEditToSim' re-seed from
        --   the authoritative tiles instead, which is what restores the
        --   lava the discarded reaction had consumed.
    | SimSetTickRate !Int
        -- ^ Tick rate in microseconds (default 100000 = 10Hz). Global.
    | SimPause
    | SimResume
    | SimFastSettleAll !FastSettleRequest
        -- ^ Synchronously run all settle ticks (no sleeping) across every
        --   world until each chunk has scsSettleTicks == 0 and no chunk is
        --   active. Then emits a 'WorldApplyFluids' batch (per world) with
        --   an ack and waits for the world thread to apply it, sets
        --   ssPaused, and publishes the 'FastSettleOutcome'. Used by dump
        --   mode to get a stable simulation state without waiting for the
        --   live sim loop.

-- | One participating chunk of a committed reaction result (#2485).
data ReactionChunkSync = ReactionChunkSync
    { rcsCoord      ∷ !ChunkCoord
    , rcsEditGen    ∷ !Word64
      -- ^ The chunk's live-edit generation AFTER the commit: one bump
      --   for the whole result, not one per event, so sibling events
      --   admitted from the same pre-commit generation all land.
    , rcsFluid      ∷ !FluidMap
      -- ^ The authoritative post-edit passive fluid map, read from the
      --   page's own tiles. Used verbatim for an inactive or absent
      --   chunk; for an active one it is the passive mirror and the
      --   exact active volumes the sim already holds are what survive.
    , rcsTerrain    ∷ !(VU.Vector Int)
      -- ^ The authoritative post-edit terrain surface map.
    , rcsSolidified ∷ ![Int]
      -- ^ Local cell indices in this chunk that became stone. Their
      --   active fluid is emptied: the lava that was there is what the
      --   contact consumed, and the terrain now stands one z higher.
    } deriving (Show, Eq)

-- | One 'SimFastSettleAll': where the outcome is published, and the ONE
--   deadline every wait inside the settle shares (#2334).
data FastSettleRequest = FastSettleRequest
    { fsrDone     ∷ !(MVar FastSettleOutcome)
      -- ^ Filled exactly once, after every fallible step of the settle
      --   handler has run. A settle that never fills it is a sim worker
      --   that died on the way; the caller watches the worker's
      --   'Engine.Core.Thread.tsDone' for that, not this.
    , fsrDeadline ∷ !Double
      -- ^ An ABSOLUTE 'Engine.Core.Clock.monotonicSeconds' instant, not
      --   a duration, and shared end to end: every per-world
      --   acknowledgement wait spends what is left of this one budget,
      --   so N worlds cannot restart it N times and multiply the
      --   caller's total wait.
    }

-- | What one 'SimFastSettleAll' ended as (#2334).
--
--   Only 'FastSettleApplied' means every requested writeback reached
--   world-thread application; the caller must treat every other
--   constructor as a failed settle and emit nothing derived from the
--   tiles.
data FastSettleOutcome
    = FastSettleApplied
      -- ^ Every world's batch was acknowledged as applied.
    | FastSettleWorldFailed !WorldPageId !Text
      -- ^ This world's acknowledgement came back as a failure: the
      --   world thread raised inside the batch handler, and the text is
      --   what it raised.
    | FastSettleAckDeadline !WorldPageId
      -- ^ The shared 'fsrDeadline' ran out while waiting for this
      --   world's acknowledgement.
    deriving (Eq, Show)

instance Show SimCommand where
    show (SimActivateWorld p e t) =
        "SimActivateWorld " <> show p <> " " <> show e <> " " <> show t
    show (SimDeactivateWorld p)   = "SimDeactivateWorld " <> show p
    show (SimDropWorld p)         = "SimDropWorld " <> show p
    show (SimChunkLoaded p e t cc _ _) =
        "SimChunkLoaded " <> show p <> " " <> show e <> " " <> show t
                          <> " " <> show cc
    show (SimChunkUnloaded p cc)  = "SimChunkUnloaded " <> show p <> " " <> show cc
    show (SimChunkEdited p e t cc g _ _) =
        "SimChunkEdited " <> show p <> " " <> show e <> " " <> show t
                          <> " " <> show cc <> " gen=" <> show g
    show (SimReactionCommitted p e t syncs) =
        "SimReactionCommitted " <> show p <> " " <> show e <> " " <> show t
                                <> " " <> show (map rcsCoord syncs)
    show (SimSetTickRate r) = "SimSetTickRate " <> show r
    show SimPause  = "SimPause"
    show SimResume = "SimResume"
    show (SimFastSettleAll _) = "SimFastSettleAll"
