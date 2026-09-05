{-# LANGUAGE Strict #-}

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
      -- * The load-publish transient reset, exported for its gate
    , resetTransientState
      -- * The owner-queue discard, exported for its gate (#2221): what
      --   an owner parked at the boundary left queued must be proven to
      --   die here rather than survive into the replacement session.
    , discardStaleQueues
    ) where

import Engine.Graphics.Solar (maxSolarPages)
import UPrelude
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Control.Concurrent.STM (atomically, modifyTVar')
import Engine.Core.State (EngineEnv(..))
import Engine.PlayerEvent (clearEventStoreRows)
import Engine.Core.Log (logInfo, logWarn, LogCategory(..), LoggerState)
import qualified Engine.Core.Queue as Q
import Sim.Command.Types (SimCommand(..))
import Engine.Scripting.Lua.Types (LuaMsg(..))
import Engine.Input.Types (defaultInputState)
import UI.ShellFocus (FocusManager(..))
import UI.Manager (clearElementFocus, clearControlFocus)
import UI.Tooltip (clearTooltipLock)
import UI.Types (UIPageManager(upmHovered))
import World.Types
import World.Load.Types (StagedPage(..), StagedSession(..))
import World.Pause (beginPauseEpoch)
import World.Blood.Teardown (enqueueBloodDisposalAll)
import World.Thread.Command.UI (handleWorldShowCommand)
import World.Thread.Helpers (sendSaveLoaded, sendGenLog)
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
    -- Every OTHER owner thread
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
    -- queue. The Lua engine-message queue ('luaQueue', engine → Lua) is
    -- handled separately, on the LUA THREAD itself (see
    -- 'Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged') --
    -- flushing it from here would race the Lua thread's own recursive
    -- drain of messages that arrived during staging, which runs
    -- immediately after 'applyLuaLoad' and would very likely win. The
    -- Lua-TO-engine queue ('luaToEngineQueue', drained by the main/
    -- offscreen render thread's 'Engine.Scripting.Lua.Message.processLuaMessages')
    -- is handled the same way, on ITS OWN consumer thread — see
    -- 'Engine.Loop''s save-barrier owner gate — for the identical reason:
    -- flushing it from here (an earlier attempt) raced
    -- that thread's own drain and, observed empirically, could leave a
    -- load transaction's publish-side work permanently stuck instead of
    -- merely losing one message.
    discardStaleQueues env logger

    oldMgr ← readIORef (worldManagerRef env)
    let oldPageIds = map fst (wmWorlds oldMgr)
        newPageIds = map spPageId (ssPages staged)

    logInfo logger CatWorld $
        "Publishing loaded session: " <> tshow (length newPageIds)
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
    -- The SOLE point the off-session registry
    -- "World.Load.Stage" staged against (and
    -- "Engine.Scripting.Lua.API.Save.continueLoad" validated the save's
    -- material references against) ever reaches the live ref — not at
    -- validation time, when the load isn't yet known to succeed.
    writeIORef (materialRegistryRef env) (ssMaterialRegistry staged)
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
    -- Pair the atlas with the EXACT WorldState it belongs to -- the ONE
    -- staged page whose own zoom cache produced these pixels, named by
    -- 'ssZoomAtlas' itself -- so the eventual GPU upload
    -- (Engine.Scripting.Lua.Message.WorldTexture.handleZoomAtlasUpload)
    -- never has to re-read worldManagerRef later and risk a NEWER
    -- publish's pages having already replaced these by then.
    --
    -- #1670: this is a per-PAGE association, never a per-session one.
    -- 'World.Load.Stage' gives every non-arena page its own
    -- wsZoomCacheRef but builds atlas pixels for only one of them, and
    -- 'World.Render.Zoom.Bake' indexes each visible page's own cache
    -- using that page's ASSIGNED atlas layout -- so handing this payload
    -- to every published page (which is what this did before #1670) made
    -- a second visible page bake its quads against another world's
    -- pixels, and index past the texture whenever its cache was the
    -- longer one. Pages left out keep wsZoomAtlasRef at Nothing, which
    -- ensureBakedAtlas already supports as per-material baking.
    forM_ (ssZoomAtlas staged) $ \(ownerPid, w, h, bytes) → do
        let atlasOwners = [ spWorldState p
                          | p ← ssPages staged, spPageId p ≡ ownerPid ]
        when (null atlasOwners) $ logWarn logger CatWorld $
            "Load publish: staged zoom atlas names page "
            <> unWorldPageId ownerPid
            <> ", which is not among this session's staged pages -- \
               \publishing it with no target rather than attaching it \
               \to a page whose cache did not produce it"
        writeIORef (zoomAtlasDataRef env) (Just (w, h, bytes, atlasOwners))
    -- Bump the preview generation on EVERY publish,
    -- unconditionally — never only inside the 'Just' branch below. A
    -- page staged via the arena-reconstruction path
    -- ('World.Load.Stage.stageSession', 'isArenaParams') carries no
    -- preview at all ('ssPreview' is 'Nothing'), but this publish still
    -- REPLACES the session exactly as any other load does, and MUST
    -- still invalidate whatever preview upload was in flight for the
    -- session it replaces — 'Engine.Scripting.Lua.Thread.Dispatch's
    -- delivery-time check ('LuaWorldPreviewReady') and
    -- 'Engine.Scripting.Lua.Message.WorldTexture.handleWorldPreview'
    -- rely on this counter moving on every publish to detect that.
    previewGen ← atomicModifyIORef' (worldPreviewGenerationRef env)
                    (\g → (g + 1, g + 1))
    forM_ (ssPreview staged) $ \(w, h, bytes) →
        writeIORef (worldPreviewRef env) (Just (w, h, bytes, previewGen))

    -- Register every staged page under its OWN saved id (requirement 8:
    -- no remap, no collision suffix — a load replaces the complete
    -- session, so nothing survives to collide with). wmVisible starts
    -- empty so handleWorldShowCommand below starts from a clean slate.
    --
    -- #1602: the replacement manager's page-SELECTION generation is
    -- seeded from the outgoing one rather than restarting at 0. A load
    -- replaces the whole page set, so every placement binding captured
    -- before it must read as stale afterwards — a fresh counter would
    -- hand the new session the same low numbers the old one had already
    -- issued, which is exactly the ABA hazard the generation exists to
    -- close. (The handleWorldShowCommand calls below bump it further.)
    outgoingSelectionGen ← wmSelectionGen <$> readIORef (worldManagerRef env)
    writeIORef (worldManagerRef env) WorldManager
        { wmWorlds  = [ (spPageId p, spWorldState p) | p ← ssPages staged ]
        , wmVisible = []
        , wmSelectionGen = outgoingSelectionGen + 1
        -- Reset rather than carried: a load replaces the whole session,
        -- and 'World.Thread.processAuthorizedSave' DISCARDS whatever
        -- selection-changing commands were still queued against the old
        -- one, so their requests will never be discharged by a handler.
        , wmSelectionPending = 0
        -- Settled by construction: nothing is outstanding against the
        -- replacement session, so the projection is the applied state.
        , wmProjectedGen = outgoingSelectionGen + 1
        , wmProjectedWorlds = [ spPageId p | p ← ssPages staged ]
        , wmProjectedVisible = []
        -- No teardown is outstanding against the replacement session
        -- (#2291): 'discardStaleQueues' above threw away the unit and
        -- building queues, every boundary marker included, so nothing is
        -- left to complete and nothing may fence this session's page
        -- registrations.
        , wmTeardownsPending = 0
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
    --
    -- Truncated to the visible-page limit (#1869) BEFORE the loop, not
    -- left to handleWorldShowCommand's own refusal: the loop shows in
    -- reverse so the primary lands at the head, which means the primary
    -- is shown LAST and a refusal at the limit would drop precisely the
    -- page a load must always make active. 'ssActivePage' heads
    -- 'wantVisible', so taking a prefix always keeps it. No save written
    -- by this engine can exceed the limit, since every entry got there
    -- through a show that enforced it.
    let allWanted = dedupPageIds (ssActivePage staged : ssVisiblePages staged)
        wantVisible = take maxSolarPages allWanted
    when (length allWanted > length wantVisible) $
        logWarn logger CatWorld $
            "Restoring only " <> tshow (length wantVisible) <> " of "
            <> tshow (length allWanted)
            <> " saved visible worlds: the most one frame can light "
            <> "individually"
    forM_ (reverse wantVisible) $ \pid →
        handleWorldShowCommand (toWorldSimCapability env) logger pid

    -- #1599: give the PUBLISHED session its own pause epoch, now that it
    -- has an active page to own one. Deliberately 'beginPauseEpoch' and
    -- not 'imposePause': the flag has been set since the load was
    -- accepted (on the OUTGOING session), so the transition-guarded
    -- version would preserve an epoch belonging to pages this publish
    -- just discarded and hand their speed to a page of the new session.
    -- Starting fresh here zeroes the new active page's clock and records
    -- the default 1.0 every loaded page comes up at (time scale is never
    -- persisted), which is exactly the load policy scripts/pause.lua's
    -- onSaveLoaded states: a load resumes at default speed, never at a
    -- pre-save one.
    beginPauseEpoch (toWorldSimCapability env)

    -- Fire every deferred sim-seed / location-stamp collected during
    -- staging now that each page is genuinely live (requirement 6: this
    -- work only ever touches a live queue from here, at publish, never
    -- during staging).
    forM_ (ssPages staged) $ \p → do
        -- Every seed carries the page's own seam topology (#2044); it is
        -- read from the staged page's gen params, which staging has
        -- already populated.
        topo ← pageSimTopology (spWorldState p)
        forM_ (spSimSeeds p) $ \(coord, fluidMap, terrainMap) →
            Q.writeQueue (simQueue env)
                (SimChunkLoaded (spPageId p) topo coord fluidMap terrainMap)
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
    sendSaveLoaded env requestId uIds bIds (ssReconcile staged)
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
                "Load publish discarded " <> tshow (length stale)
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
--   ('upmHovered') — a control that held keyboard
--   focus before the load would otherwise still fire Enter/Space's
--   'onClick' callback afterward, potentially against a closure that
--   captured old-session state (e.g. a save-slot button, a build-tool
--   ghost target). Unlike 'focusManagerRef', clearing these does not
--   touch the UI TREE itself (elements/pages), only the two
--   independent focus pointers and the hover pointer — the tree is
--   Lua-owned and rebuilt/reconciled by the same 'sendSaveLoaded'
--   broadcast as before.
--
--   'uiManagerRef' also runs 'clearTooltipLock' (#2156): a tooltip the
--   player had LOCKED before the load is session UI that nothing else
--   reconciles — 'UI.Tooltip.State.tickLocked' freezes it and ignores
--   hover and source validity while locked, and a page deletion does
--   not touch the separately owned tooltip state — so it would stay on
--   screen over the replacement session and
--   'UI.Tooltip.Lock.isPointInLockedTooltip' would keep swallowing
--   clicks inside its box. 'clearTooltipLock' is the existing
--   unlock-AND-hide: it releases the lock, destroys the visuals and
--   hides the tooltip page synchronously, so no locked box survives
--   the publish (a merely hovered, unlocked tooltip is hidden too, and
--   the very next tick re-evaluates hover against the new tree).
--
--   'hudActivePageRef' additionally resets to
--   'Nothing': 'World.Thread.Cursor.pollCursorInfo' compares the
--   active page id against this ref to detect an active-WORLD switch
--   and force a fresh HUD push even when the raw cursor selection
--   fields look unchanged. A load that lands on the SAME page id the
--   old session had active (reloading the same save, or a different
--   save that happens to reuse the id) would otherwise leave this ref
--   already matching post-publish, so the next poll sees
--   'activeChanged = False' and never re-pushes — the HUD keeps
--   showing whatever selected-tile/chunk text the OLD session's
--   cursor last rendered, now describing entities that no longer
--   exist. Resetting to 'Nothing' makes the very next poll unconditionally
--   detect a "switch" regardless of whether the page id matches.
resetTransientState ∷ EngineEnv → IO ()
resetTransientState env = do
    writeIORef (buildingGhostRef env) Nothing
    writeIORef (inputStateRef env) defaultInputState
    writeIORef (hudActivePageRef env) Nothing
    atomicModifyIORef' (focusManagerRef env) $ \fm →
        (fm { fmCurrentFocus = Nothing }, ())
    atomicModifyIORef' (uiManagerRef env) $ \mgr →
        ( (clearTooltipLock ∘ clearControlFocus ∘ clearElementFocus)
              (mgr { upmHovered = Nothing })
        , () )
    atomicModifyIORef' (uiManagerRef env) $ \mgr →
        ( (clearControlFocus ∘ clearElementFocus)
              (mgr { upmHovered = Nothing })
        , () )
    writeIORef (combatEventsRef env) Seq.empty
    writeIORef (injuryEventsRef env) Seq.empty
    writeIORef (thoughtEventsRef env) Seq.empty
    writeIORef (actionOutcomeRef env) Seq.empty
    -- Rows only: 'clearEventStoreRows' deliberately keeps the event
    -- store's sequence counter, so a row emitted after this load still
    -- outranks any cursor an observer retained from before it and no
    -- sequence is ever reissued in one engine process (#1714).
    atomically $ modifyTVar' (eventStoreRef env) clearEventStoreRows
