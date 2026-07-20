{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | The in-memory shape of a fully-staged, not-yet-published whole
--   session replacement (issue #763, save-overhaul C2). Built entirely
--   by "World.Load.Stage" without touching any live
--   'Engine.Core.State.EngineEnv' ref; consumed exactly once by
--   "World.Load.Publish", which performs the atomic swap. Kept as its
--   own Types-style module (World/Types.hs-shaped: local deps only, no
--   'Engine.Core.State' import) so 'Engine.Core.State' can name it for
--   'EngineEnv''s @pendingLoadRef@ field without a cycle through the
--   staging logic itself.
--
--   Every deferred cross-thread side effect a saved page's restoration
--   used to fire immediately (sim chunk seeding, location-stamp
--   dispatch) is captured here as plain data instead — requirement 6
--   forbids staging from sending work through a live queue, so
--   "World.Load.Publish" fires these once the new session is
--   registered.
module World.Load.Types
    ( StagedPage(..)
    , StagedSession(..)
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.ByteString (ByteString)
import Engine.Graphics.Camera (Camera2D)
import Structure.Palette (TexPalette)
import Building.Types (BuildingManager)
import Unit.Types (UnitManager, UnitId)
import Unit.Sim.Types (UnitSimState)
import World.Material (MaterialRegistry)
import World.Types
    ( WorldPageId, WorldState, ChunkCoord, FluidCell )
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

-- | One saved page, fully reconstructed but not yet registered anywhere
--   live (a fresh 'WorldState' with its own private 'Data.IORef.IORef's,
--   built exactly like init/the pre-#763 load path — chunk gen, zoom
--   cache, arena rebuild, edit replay — just never written into
--   'Engine.Core.State.worldManagerRef' until publish).
data StagedPage = StagedPage
    { spPageId         ∷ !WorldPageId
    , spWorldState      ∷ !WorldState
    , spSimSeeds        ∷ ![(ChunkCoord, V.Vector (Maybe FluidCell), VU.Vector Int)]
      -- ^ Deferred 'Sim.Command.Types.SimChunkLoaded' payloads (the
      --   synchronously-generated center chunk, or every eager chunk for
      --   an arena rebuild) — sent once this page is live.
    , spLocationStamps  ∷ ![(Text, Int, Int)]
      -- ^ Deferred location id / global tile x / global tile y triples
      --   for 'World.Thread.ChunkLoading.dispatchLocationStamps' — fired
      --   once this page is live instead of during staging.
    }

-- | The complete replacement session, ready to publish. Everything a
--   successful publish needs is already computed; publish performs only
--   'Data.IORef.IORef' writes plus the deferred sends every 'StagedPage'
--   collected.
data StagedSession = StagedSession
    { ssPages         ∷ ![StagedPage]
    , ssActivePage    ∷ !WorldPageId
    , ssVisiblePages  ∷ ![WorldPageId]
    , ssBuildings     ∷ !BuildingManager
    , ssUnits         ∷ !UnitManager
    , ssUnitSimStates ∷ !(HM.HashMap UnitId UnitSimState)
    , ssGameTime      ∷ !Double
    , ssTexPalette    ∷ !TexPalette
    , ssNextItemId    ∷ !Word64
    , ssCamera        ∷ !Camera2D
    , ssZoomAtlas     ∷ !(Maybe (Int, Int, ByteString))
    , ssPreview       ∷ !(Maybe (Int, Int, ByteString))
    , ssMaterialRegistry ∷ !MaterialRegistry
      -- ^ Round 6 review: the off-session registry staged against (see
      --   "World.Load.Stage"'s haddock) — carried through so publish is
      --   the SOLE point it ever reaches the live
      --   'Engine.Core.State.materialRegistryRef', same as every other
      --   piece of session state.
    }
