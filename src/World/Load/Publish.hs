{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Whole-session LOAD publication (issue #763, save-overhaul C2): the
--   single, atomic replacement of every live gameplay ref with a
--   'World.Load.Types.StagedSession' already built by "World.Load.Stage".
--
--   'publishStagedSession' is called from the WORLD thread's
--   'World.Command.Types.WorldLoadPublish' handler ONLY while the
--   'Engine.Save.Barrier' capture lock is held (requirement 10) — every
--   other state-owner thread (Unit/Building/Combat/Simulation, and the
--   Lua thread, which drove the barrier itself and already applied its
--   own state before queuing this command — see
--   "Engine.Scripting.Lua.Thread.Dispatch") is quiesced for the
--   duration, so no gameplay consumer can observe a mixed generation.
--   Old-session teardown (dropping stale sim state, reclaiming blood
--   textures) is queued as part of the SAME call but only takes effect
--   after the new session is already live (requirement 15).
module World.Load.Publish
    ( publishStagedSession
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef)
import Control.Concurrent.STM (atomically, writeTVar)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, LogCategory(..), LoggerState)
import qualified Engine.Core.Queue as Q
import Sim.Command.Types (SimCommand(..))
import Engine.Scripting.Lua.Types (LuaMsg(..))
import World.Types
import World.Load.Types (StagedPage(..), StagedSession(..))
import World.Blood.Teardown (enqueueBloodDisposalAll)
import World.Thread.Command.UI (handleWorldShowCommand)
import World.Thread.Helpers (sendSaveLoaded, unWorldPageId)
import Building.Types (BuildingManager(..), unBuildingId)
import Unit.Types (UnitManager(..), unUnitId)
import Unit.Sim.Types (UnitThreadState(..))

-- | Publish a fully-staged session, replacing the current one entirely.
--   Never fails: every value staged is already fully forced (built and
--   validated well before this ran), so the only work left is plain
--   'Data.IORef.IORef' assignment plus queuing the deferred work each
--   staged page collected.
publishStagedSession ∷ EngineEnv → LoggerState → StagedSession → IO ()
publishStagedSession env logger staged = do
    oldMgr ← readIORef (worldManagerRef env)
    let oldPageIds = map fst (wmWorlds oldMgr)
        newPageIds = map spPageId (ssPages staged)

    logInfo logger CatWorld $
        "Publishing loaded session: " <> T.pack (show (length newPageIds))
        <> " page(s), active=" <> unWorldPageId (ssActivePage staged)

    -- Drop every OLD page's sim state FIRST (before any new page is
    -- registered/seeded below) so a coincidental id collision between an
    -- old and a new page can't have this drop race the new page's own
    -- SimChunkLoaded seeding — the sim thread drains this queue in
    -- order, so "drop old, then (maybe) reseed the same id" is always
    -- correct regardless of overlap.
    forM_ oldPageIds $ \pid → Q.writeQueue (simQueue env) (SimDropWorld pid)
    -- Reclaim every old page's blood-texture GPU resources (mirrors the
    -- existing destroy-all/exit-to-menu path) before wmWorlds replaces
    -- them below.
    enqueueBloodDisposalAll (bloodDisposeQueue env) oldMgr

    -- Genuinely global values, all at once. Every reader that matters
    -- (the other state-owner threads) is quiesced for the duration of
    -- this whole function — see the module haddock.
    writeIORef (gameTimeRef env) (ssGameTime staged)
    writeIORef (enginePausedRef env) True
    writeIORef (texPaletteRef env) (ssTexPalette staged)
    -- Runtime paletteId → texture handle table is session-local; clear it
    -- so Lua re-resolves every palette texture for this session.
    writeIORef (texPaletteHandlesRef env) HM.empty
    -- A load REPLACES the complete session (requirement 8) — the prior
    -- allocator value belongs to a session no longer live, so this is a
    -- plain assignment, never a 'max' against the discarded value.
    writeIORef (nextItemInstanceIdRef env) (ssNextItemId staged)
    writeIORef (buildingManagerRef env) (ssBuildings staged)
    writeIORef (unitManagerRef env) (ssUnits staged)
    writeIORef (utsRef env) (UnitThreadState { utsSimStates = ssUnitSimStates staged })
    writeIORef (cameraRef env) (ssCamera staged)
    forM_ (ssZoomAtlas staged) $ \zoomAtlas → writeIORef (zoomAtlasDataRef env) (Just zoomAtlas)
    forM_ (ssPreview staged)   $ \preview   → writeIORef (worldPreviewRef env) (Just preview)

    -- Register every staged page under its OWN saved id (requirement 8:
    -- no remap, no collision suffix — a load replaces the complete
    -- session, so nothing survives to collide with). wmVisible starts
    -- empty so handleWorldShowCommand below starts from a clean slate.
    writeIORef (worldManagerRef env) WorldManager
        { wmWorlds  = [ (spPageId p, spWorldState p) | p ← ssPages staged ]
        , wmVisible = []
        }

    -- Restore visibility through the real handler so its side effects
    -- fire (SimActivateWorld, quad-cache bump). Reverse order so
    -- ssVisiblePages's head — the page that was actually on screen at
    -- save time — ends up at the head of wmVisible, matching
    -- resolveActiveWorld's "first visible wins" rule.
    forM_ (reverse (ssVisiblePages staged)) $ \pid →
        handleWorldShowCommand env logger pid

    -- Fire every deferred sim-seed / location-stamp collected during
    -- staging now that each page is genuinely live (requirement 6: this
    -- work only ever touches a live queue from here, at publish, never
    -- during staging).
    forM_ (ssPages staged) $ \p → do
        forM_ (spSimSeeds p) $ \(coord, fluidMap, terrainMap) →
            Q.writeQueue (simQueue env)
                (SimChunkLoaded (spPageId p) coord fluidMap terrainMap)
        forM_ (spLocationStamps p) $ \(lid, gx, gy) →
            Q.writeQueue (luaQueue env)
                (LuaStampLocation (unWorldPageId (spPageId p)) lid gx gy)

    resetTransientState env

    -- Signal Lua with every restored id — a load replaces the complete
    -- session, so there is no "off-page survivor" distinction left to
    -- make (unlike the pre-#763 merge path): every unit/building in the
    -- new session is a survivor.
    let bIds = map (fromIntegral . unBuildingId) (HM.keys (bmInstances (ssBuildings staged)))
        uIds = map (fromIntegral . unUnitId) (HM.keys (umInstances (ssUnits staged)))
    sendSaveLoaded env uIds bIds

-- | Clear the runtime-only, per-session state requirement 13 excludes
--   from a loaded session: pending build-tool ghost, and the previous
--   session's combat/injury/thought/action-outcome/player-event streams
--   (never persisted, never meaningful to carry across a whole-session
--   replacement). Toolbar/selection/focus reset is the pre-existing
--   'onSaveLoaded' Lua broadcast's job (see 'sendSaveLoaded' above),
--   unchanged by this issue.
resetTransientState ∷ EngineEnv → IO ()
resetTransientState env = do
    writeIORef (buildingGhostRef env) Nothing
    writeIORef (combatEventsRef env) Seq.empty
    writeIORef (injuryEventsRef env) Seq.empty
    writeIORef (thoughtEventsRef env) Seq.empty
    writeIORef (actionOutcomeRef env) Seq.empty
    atomically $ writeTVar (eventStoreRef env) Seq.empty
    atomically $ writeTVar (popupQueueRef env) Seq.empty
