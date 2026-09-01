{-# LANGUAGE Strict #-}
module Sim.State.Types
    ( SimState(..)
    , SimWorldState(..)
    , SimChunkState(..)
    , emptySimState
    , emptySimWorldState
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..))
import World.Page.Types (WorldPageId(..))
import World.Fluid.Internal (FluidMap)
import Sim.Fluid.Types (ActiveFluidCell(..))
import Sim.Topology (SimTopology(..))

-- | Simulation state, scoped per world. Each visible/active world owns
--   an independent 'SimWorldState' (its own chunk map, dirty set and
--   active flag) keyed by 'WorldPageId', so one world's fluid sim can
--   never contaminate another's tiles (epic #101 / #59). Tick rate and
--   the engine-level pause are genuinely global and stay at the top.
data SimState = SimState
    { ssWorlds      ∷ !(HM.HashMap WorldPageId SimWorldState)
    , ssTickRate    ∷ !Int              -- ^ Microseconds between ticks (default 100000)
    , ssPaused      ∷ !Bool             -- ^ Engine-level (game) pause, all worlds
    }

-- | One world's simulation state.
data SimWorldState = SimWorldState
    { swsChunks      ∷ !(HM.HashMap ChunkCoord SimChunkState)
    , swsDirtyChunks ∷ !(HS.HashSet ChunkCoord)  -- ^ Chunks modified this tick
    , swsActive      ∷ !Bool
        -- ^ True between SimActivateWorld and SimDeactivateWorld for THIS
        --   world. The sim never holds the world's tile ref — it emits
        --   'WorldApplyFluids' (tagged with this world's page id) to the
        --   world thread, the sole writer of 'wsTilesRef'.
    , swsTopology    ∷ !SimTopology
        -- ^ This page's seam topology (#2044): how a chunk coord maps to
        --   the key the page STORES it under, so the sim's neighbour
        --   probes reach the physically adjacent chunk across the u seam
        --   instead of missing on a raw key. Carried by every command
        --   that seeds a chunk into this world or activates it, so a page
        --   with anything to simulate always has it.
    }

data SimChunkState = SimChunkState
    { scsFluid       ∷ !FluidMap          -- ^ Live fluid state
    , scsTerrain     ∷ !(VU.Vector Int)   -- ^ Terrain surface (read-only until modified)
    , scsSettleTicks ∷ !Int               -- ^ Remaining fast settle ticks (0 = settled)
    , scsActive      ∷ !Bool              -- ^ True when volume sim is running
    , scsActiveFluid ∷ !(V.Vector (Maybe ActiveFluidCell))  -- ^ Volume-tracked fluid (active only)
    , scsEquilTicks  ∷ !Int               -- ^ Ticks at equilibrium (for deactivation)
    , scsSideDeco    ∷ !(VU.Vector Word8) -- ^ Side-face decorations (waterfall etc.)
    , scsEditGen     ∷ !Word64
      -- ^ The owning page's live-edit generation for this chunk, as of
      --   the message this state was seeded from: 0 from 'SimChunkLoaded',
      --   the carried value from 'SimChunkEdited' (#1596). Copied verbatim
      --   into every writeback this chunk produces
      --   ('World.Command.Types.fwEditGen') so the world thread — the sole
      --   writer of the tiles — can reject output computed before an edit
      --   it would overwrite. The sim never compares it; see
      --   'World.State.Types.wsChunkEditGenRef'.
    }

emptySimState ∷ SimState
emptySimState = SimState
    { ssWorlds      = HM.empty
    , ssTickRate    = 100000  -- 10 Hz
    , ssPaused      = False
    }

emptySimWorldState ∷ SimWorldState
emptySimWorldState = SimWorldState
    { swsChunks      = HM.empty
    , swsDirtyChunks = HS.empty
    , swsActive      = False
    -- A world nothing has seeded or activated yet has no chunks, so it
    -- neither ticks nor fast-settles; 'SimFlatTopology' is the honest
    -- placeholder until the first command that carries the real one, the
    -- same answer 'World.State.Types.pageWrapWorldSize' gives a page with
    -- no gen params.
    , swsTopology    = SimFlatTopology
    }
