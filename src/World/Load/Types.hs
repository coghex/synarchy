{-# LANGUAGE Strict #-}

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
import Item.Knowledge (PortableKnowledge)
import Unit.Types (UnitManager, UnitId)
import Unit.Sim.Types (UnitSimState)
import World.Material (MaterialRegistry)
import World.Save.Payload (LoadReconcileContext)
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
    , ssZoomAtlas     ∷ !(Maybe (WorldPageId, Int, Int, ByteString))
      -- ^ The ONE staged page whose own zoom cache produced these
      --   pixels, paired with them (issue #1670). Staging builds a
      --   separate 'World.Types.wsZoomCacheRef' for EVERY non-arena
      --   page but atlas pixels for only one of them, so the owner id
      --   is what lets "World.Load.Publish" hand the payload to that
      --   page alone: 'World.Render.Zoom.Bake' indexes a page's own
      --   cache using its ASSIGNED atlas's layout, so a page holding
      --   another page's atlas bakes its quads against the wrong
      --   world's pixels. A page not named here keeps 'wsZoomAtlasRef'
      --   at 'Nothing' until it publishes its OWN image: since #2485
      --   every staged page retains its own atlas pixels
      --   ('World.State.Types.wsZoomLiveRef'), so a live terrain edit on
      --   one of them regenerates a tile and republishes from there.
      --   What this field decides is only which image is uploaded AT
      --   LOAD, never which page may have one.
    , ssPreview       ∷ !(Maybe (Int, Int, ByteString))
    , ssReconcile     ∷ !LoadReconcileContext
      -- ^ The restored session's item-instance / unit-page /
      --   per-page bill + ground-item context (issue #1589), computed
      --   in "World.Load.Stage" from the SAME decoded save the
      --   reference-edge cross-validator reads and carried through to
      --   publish, which hands it to Lua's @onSaveLoaded@ reconcile
      --   broadcast. Data, not a live query: the Lua side must never
      --   have to ask the ACTIVE page about a per-page id belonging to
      --   some other page.
    , ssPortableKnowledge ∷ !PortableKnowledge
      -- ^ #2512: the session-wide portable-container memory, already
      --   SCRUBBED against this replacement session's own complete live
      --   item enumeration ("World.Load.Stage") — never against the
      --   outgoing session's, which is about to be discarded. Publish
      --   installs it verbatim onto the replacement
      --   'World.State.Types.wmPortableKnowledge', which is what makes
      --   an absent payload CLEAR whatever the outgoing session
      --   remembered rather than leave it standing.
    , ssMaterialRegistry ∷ !MaterialRegistry
      -- ^ The off-session registry staged against (see
      --   "World.Load.Stage"'s haddock) — carried through so publish is
      --   the SOLE point it ever reaches the live
      --   'Engine.Core.State.materialRegistryRef', same as every other
      --   piece of session state.
    }
