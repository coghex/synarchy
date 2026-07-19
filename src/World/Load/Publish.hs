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
import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Control.Concurrent.STM (atomically, writeTVar)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logWarn, LogCategory(..), LoggerState)
import qualified Engine.Core.Queue as Q
import Sim.Command.Types (SimCommand(..))
import Engine.Scripting.Lua.Types (LuaMsg(..))
import Engine.Input.Types (defaultInputState)
import UI.Focus (FocusManager(..))
import UI.Manager (clearElementFocus, clearControlFocus)
import UI.Types (UIPageManager(upmHovered))
import World.Types
import World.Load.Types (StagedPage(..), StagedSession(..))
import World.Blood.Teardown (enqueueBloodDisposalAll)
import World.Thread.Command.UI (handleWorldShowCommand)
import World.Thread.Helpers (sendSaveLoaded, sendGenLog, unWorldPageId)
import Building.Types (BuildingManager(..), unBuildingId)
import Unit.Types (UnitManager(..), unUnitId)
import Unit.Sim.Types (UnitThreadState(..))

-- | Publish a fully-staged session, replacing the current one entirely.
--   Never fails: every value staged is already fully forced (built and
--   validated well before this ran), so the only work left is plain
--   'Data.IORef.IORef' assignment plus queuing the deferred work each
--   staged page collected.
publishStagedSession ∷ EngineEnv → LoggerState → Int → StagedSession → IO ()
publishStagedSession env logger requestId staged = do
    -- Requirement 12 (round 2 review): every OTHER owner thread
    -- (Unit/Building/Combat/Simulation) is quiesced for this function's
    -- ENTIRE duration -- the same guarantee 'World.Thread.processAuthorizedSave'
    -- relies on for the world queue -- but unlike the world queue,
    -- nothing in THEIR queues is ever authorized to run inside the
    -- lock, so the fix here is unconditional discard rather than
    -- partition. Anything sitting there targeted the OLD session (a
    -- unit/building/combat/sim command issued moments before the load)
    -- and, left in place, would run against the REPLACEMENT session the
    -- instant its owner resumes -- e.g. a stale UnitKill for an id the
    -- new session's allocator happens to reuse. Done here, while every
    -- owner is still locked out, so there is no window in which a
    -- resumed owner could observe and act on a stale message. The raw
    -- input queue is included for the same reason even though Input is
    -- not itself a SaveOwner (nothing quiesces it): it's a cheap,
    -- race-free flush (STM) that closes the "stale click/key still
    -- awaiting dispatch" case for whatever had already reached this
    -- queue. The Lua engine-message queue is handled separately, on the
    -- LUA THREAD itself (see 'Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged') --
    -- flushing it from here would race the Lua thread's own recursive
    -- drain of messages that arrived during staging, which runs
    -- immediately after 'applyLuaLoad' and would very likely win.
    discardStaleQueues env logger

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
    -- fire (SimActivateWorld, quad-cache bump). ssActivePage is the
    -- save's REQUESTED primary page (engine.saveWorld is page-targeted
    -- and may target a page that wasn't even visible at save time —
    -- WriteWorld.hs's own sgActivePage/sgVisiblePages split), so it is
    -- prepended ahead of ssVisiblePages here rather than assumed to
    -- already be a member of it — dedupPageIds then keeps only its
    -- first occurrence. Reverse order so the front of that combined
    -- list — ssActivePage itself — ends up at the head of wmVisible,
    -- matching resolveActiveWorld's "first visible wins" rule: a load
    -- must always make its own primary page active, exactly like a
    -- fresh world.show would.
    let wantVisible = dedupPageIds (ssActivePage staged : ssVisiblePages staged)
    forM_ (reverse wantVisible) $ \pid →
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
    sendSaveLoaded env requestId uIds bIds
    -- The one user-facing "done" toast for the whole transaction —
    -- staging deliberately never sends one (requirement 6: no live-queue
    -- work while staging), so this is the sole place a load reports
    -- completion via the ordinary gen-log toast channel.
    sendGenLog env "Save loaded"

-- | Discard every stale message still sitting in an owner's queue at
--   the moment a load publishes (requirement 12) — see the call site's
--   haddock in 'publishStagedSession' for why this is unconditional
--   discard rather than the world queue's authorized/deferred split.
discardStaleQueues ∷ EngineEnv → LoggerState → IO ()
discardStaleQueues env logger = do
    discard "unit"      (unitQueue env)
    discard "building"  (buildingQueue env)
    discard "combat"    (combatQueue env)
    discard "simulation" (simQueue env)
    discard "input"     (inputQueue env)
  where
    discard ∷ Text → Q.Queue α → IO ()
    discard label q = do
        stale ← Q.flushQueue q
        unless (null stale) $
            logWarn logger CatWorld $
                "Load publish discarded " <> T.pack (show (length stale))
                <> " stale " <> label <> " message(s) queued before the \
                   \whole-session replacement"

-- | The first occurrence of each page id, in order — used to fold
--   ssActivePage into ssVisiblePages without risking a duplicate head
--   entry when the active page was already visible at save time.
dedupPageIds ∷ [WorldPageId] → [WorldPageId]
dedupPageIds = go HS.empty
  where
    go _    []       = []
    go seen (p : ps)
        | p `HS.member` seen = go seen ps
        | otherwise          = p : go (HS.insert p seen) ps

-- | Clear the runtime-only, per-session state requirement 13 excludes
--   from a loaded session: pending build-tool ghost, held/pending input
--   gestures (key/mouse state, pending UI click/activation — a stale
--   press or release must not act on the replacement session), UI
--   keyboard-control focus, and the previous session's combat/injury/
--   thought/action-outcome/player-event streams (never persisted, never
--   meaningful to carry across a whole-session replacement).
--   'focusManagerRef' clears only the CURRENT focus, not its registered
--   target map — the live UI tree (and the targets it registered) is
--   rebuilt by Lua on this same load, but that rebuild is a consequence
--   of the 'sendSaveLoaded' broadcast above and hasn't necessarily run
--   yet at this exact point, so wiping the whole map here could
--   transiently desync it from what's still on screen. Toolbar/
--   selection reset is the pre-existing 'onSaveLoaded' Lua broadcast's
--   job (see 'sendSaveLoaded' above), unchanged by this issue.
--
--   'uiManagerRef' additionally clears TEXT focus ('upmGlobalFocus'),
--   keyboard CONTROL focus ('upmControlFocus'), and hover
--   ('upmHovered') — round 2 review: a control that held keyboard
--   focus before the load would otherwise still fire Enter/Space's
--   'onClick' callback afterward, potentially against a closure that
--   captured old-session state (e.g. a save-slot button, a build-tool
--   ghost target). Unlike 'focusManagerRef', clearing these does not
--   touch the UI TREE itself (elements/pages), only the two
--   independent focus pointers and the hover pointer — the tree is
--   Lua-owned and rebuilt/reconciled by the same 'sendSaveLoaded'
--   broadcast as before.
resetTransientState ∷ EngineEnv → IO ()
resetTransientState env = do
    writeIORef (buildingGhostRef env) Nothing
    writeIORef (inputStateRef env) defaultInputState
    atomicModifyIORef' (focusManagerRef env) $ \fm →
        (fm { fmCurrentFocus = Nothing }, ())
    atomicModifyIORef' (uiManagerRef env) $ \mgr →
        ( (clearControlFocus ∘ clearElementFocus)
              (mgr { upmHovered = Nothing })
        , () )
    writeIORef (combatEventsRef env) Seq.empty
    writeIORef (injuryEventsRef env) Seq.empty
    writeIORef (thoughtEventsRef env) Seq.empty
    writeIORef (actionOutcomeRef env) Seq.empty
    atomically $ writeTVar (eventStoreRef env) Seq.empty
    atomically $ writeTVar (popupQueueRef env) Seq.empty
