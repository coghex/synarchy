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
syncEditToSim ∷ WorldSimCapability → WorldPageId → LoadedChunk → IO ()
syncEditToSim wsc pageId lc =
    Q.writeQueue (wsSimQueue wsc) $
        SimChunkEdited pageId (lcCoord lc)
            (lcFluidMap lc) (lcTerrainSurfaceMap lc)
