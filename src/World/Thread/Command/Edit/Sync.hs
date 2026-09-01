{-# LANGUAGE Strict #-}

-- | Shared plumbing for live world edits: re-seeding the sim after an
--   edit lands in a loaded chunk. Split out of "World.Thread.Command.Edit"
--   (issue #563) so both "World.Thread.Command.Edit.Terrain" and
--   "World.Thread.Command.Edit.Fluid" can call it without depending on
--   each other.
module World.Thread.Command.Edit.Sync
    ( syncEditToSim
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (atomicModifyIORef')
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..))
import Sim.Command.Types (SimCommand(..))
import World.Types

-- | After a live terrain/fluid edit lands in the chunk, re-seed that
--   chunk's sim state from the now-authoritative tiles AND activate it so
--   the new fluid actually flows. Without the sync the sim runs against
--   pre-edit fluid/terrain and writes its stale result back over the edit;
--   without the activation (the old SimChunkLoaded path) the edited chunk
--   kept the new snapshot but sat frozen because the volume sim only
--   advances active chunks (#60).
--
--   The re-seed alone did not close the overwrite, because the two
--   messages travel INDEPENDENT queues: this one goes to the sim with no
--   acknowledgement, while a 'World.Command.Types.WorldApplyFluids' batch
--   the sim computed from the PRE-edit chunk rides the world queue and can
--   land behind the edit. So this is also the one place a chunk's
--   live-edit generation is minted (#1596): the bump happens BEFORE the
--   message is enqueued and the new value travels with it, giving the sim
--   the provenance it stamps onto every later writeback for this chunk and
--   giving the world thread — which reads the same
--   'World.State.Types.wsChunkEditGenRef' when a batch arrives — something
--   to compare against. Every edit that changes a field
--   'World.Thread.Command.applyOneWriteback' overwrites must come through
--   here; a slope, vegetation or structure edit must not, since the
--   writeback preserves those fields and dropping one would only stall
--   the fluid sim.
--
--   Caller contract: pass the chunk AFTER the edit has been written back
--   into @wsTilesRef@, so the sim re-seeds from the authoritative tiles.
syncEditToSim ∷ WorldSimCapability → WorldPageId → WorldState → LoadedChunk
              → IO ()
syncEditToSim wsc pageId ws lc = do
    let coord = lcCoord lc
    editGen ← atomicModifyIORef' (wsChunkEditGenRef ws) $ \gens →
        let g = HM.lookupDefault 0 coord gens + 1
        in (HM.insert coord g gens, g)
    -- The page's seam topology rides along so the sim wakes the chunk
    -- physically across the u seam, not a raw neighbour key the page
    -- stores nothing under (#2044).
    topo ← pageSimTopology ws
    Q.writeQueue (wsSimQueue wsc) $
        SimChunkEdited pageId topo coord editGen
            (lcFluidMap lc) (lcTerrainSurfaceMap lc)
