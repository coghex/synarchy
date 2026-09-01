-- | Engine-to-Lua message dispatch: drains 'LuaMsg' values off the
--   engine-to-Lua queue and broadcasts each as the matching Lua module
--   callback (onKeyDown, onMouseUp, onSaveLoaded, ...).
module Engine.Scripting.Lua.Thread.Dispatch
  ( processLuaMsg
  , processLuaMsgs
  ) where

import UPrelude
import Engine.Scripting.Lua.Types
import Engine.Scripting.Lua.Script (callModuleFunction)
import Engine.Scripting.Lua.Util (isValidRef, broadcastToModules
                                 , broadcastToModulesReportingErrors)
import Engine.Core.Log (logWarn, logDebug, LogCategory(..))
import Engine.Core.Thread
import Engine.Core.State (EngineEnv(..))
import Structure.ArtCatalog
    (ArtFailureReport(..), artAssetFailureMessage, failPackArtPath)
import World.State.Types (requestSelectionChange)
import Engine.Input.Types (keyToText, clickRouteText)
import UI.Types (ElementHandle(..))
import qualified Graphics.UI.GLFW as GLFW
import qualified Engine.Core.Queue as Q
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified HsLua as Lua
import Data.List (find)
import Data.IORef (IORef, readIORef, writeIORef, atomicModifyIORef')
import Control.Concurrent.STM (readTVarIO)
import Control.Concurrent.MVar (tryPutMVar)
import Engine.Save.Barrier
    ( SaveOwner(..), beginSave, acknowledgeSave, waitForOwners
    , reachSnapshot, failSave )
import Engine.Load.Status (advanceLoad, failLoad, finishLoad, failReconciliation
                          , ReconciliationFailure(..), LoadPhase(..))
import Engine.Scripting.Lua.API.Save (applyLuaLoad, abortLuaLoad)
import Engine.Scripting.Lua.DebugServer (DebugCommand(..), pollDebugCommand)
import World.Command.Types (WorldCommand(..))
import World.Save.Payload (LoadReconcileContext(..))

processLuaMsgs ∷ EngineEnv → LuaBackendState → IORef ThreadControl → IO ()
processLuaMsgs env ls stateRef = do
    let (_, etlq) = lbsMsgQueues ls
    mMsg ← Q.tryReadQueue etlq
    case mMsg of
        Just msg → do
            logger ← readIORef (loggerRef env)
            logDebug logger CatLua $ "Engine-to-Lua message: " <> tshow msg
            processLuaMsg env ls stateRef msg
            processLuaMsgs env ls stateRef
        Nothing → return ()

processLuaMsg ∷ EngineEnv → LuaBackendState → IORef ThreadControl → LuaMsg → IO ()
processLuaMsg env ls stateRef msg = case msg of
  LuaTextureLoaded handle assetId → do
    logger ← readIORef (loggerRef env)
    logDebug logger CatLua $
        "Texture loaded with handle " <> tshow handle
        <> " as asset " <> tshow assetId
  LuaFontLoaded handle path → do
    logger ← readIORef (loggerRef env)
    logDebug logger CatLua $
        "Font " <> tshow path
        <> " loaded with handle " <> tshow handle
  LuaFontLoadFailed err → do
    logger ← readIORef (loggerRef env)
    logWarn logger CatLua $
        "Font load failed: " <> tshow err
  LuaThreadKill → writeIORef stateRef ThreadStopped
  LuaMouseDownEvent button x y → do
    let buttonNum = case button of
          GLFW.MouseButton'1 → 1
          GLFW.MouseButton'2 → 2
          GLFW.MouseButton'3 → 3
          _                  → 0
    broadcastToModules ls "onMouseDown"
      [ScriptNumber (fromIntegral buttonNum), ScriptNumber x, ScriptNumber y]
  LuaMouseUpEvent button x y downRoute → do
    let buttonNum = case button of
          GLFW.MouseButton'1 → 1
          GLFW.MouseButton'2 → 2
          GLFW.MouseButton'3 → 3
          _                  → 0
    -- onMouseUp fires on every physical release (UI widget drags
    -- depend on it); the 4th arg says where the matching press was
    -- routed ("game"/"ui"/"swallowed") so handlers can pair with
    -- onMouseDown by filtering on "game".
    broadcastToModules ls "onMouseUp"
      [ ScriptNumber (fromIntegral buttonNum), ScriptNumber x, ScriptNumber y
      , ScriptString (clickRouteText downRoute) ]
  LuaScrollEvent dx dy → do
    broadcastToModules ls "onScroll"
      [ ScriptNumber (realToFrac dx)
      , ScriptNumber (realToFrac dy)
      ]
  LuaZSliceScroll dx dy → do
    broadcastToModules ls "onZSliceScroll"
      [ ScriptNumber (realToFrac dx)
      , ScriptNumber (realToFrac dy)
      ]
  LuaUIClickEvent elemHandle callbackName _x _y → do
    let (ElementHandle h) = elemHandle
    -- F4 (#730): NOT recorded here any more — a
    -- ClickUI-routed press's outcome is deferred to its matching
    -- release, so the gesture can be classified as a plain click or a
    -- UI-widget drag exactly once (see Engine.Input.Thread's
    -- pendingUIClickRef / inpPendingUIClick).
    broadcastToModules ls callbackName [ScriptNumber (fromIntegral h)]
  LuaUIRightClickEvent elemHandle callbackName _x _y → do
    let (ElementHandle h) = elemHandle
    -- F4 (#730): see LuaUIClickEvent above.
    broadcastToModules ls callbackName [ScriptNumber (fromIntegral h)]
  LuaUIPressBeginEvent elemHandle callbackName → do
    let (ElementHandle h) = elemHandle
    broadcastToModules ls "onUIPressBegin"
      [ScriptNumber (fromIntegral h), ScriptString callbackName]
  LuaUIControlFocusChanged mElemHandle →
    broadcastToModules ls "onUIControlFocusChanged"
      [ maybe ScriptNil (\(ElementHandle h) → ScriptNumber (fromIntegral h)) mElemHandle ]
  LuaUIStepEvent elemHandle direction → do
    let (ElementHandle h) = elemHandle
    broadcastToModules ls "onUIStep"
      [ScriptNumber (fromIntegral h), ScriptNumber (fromIntegral direction)]
  LuaUIScrollEvent elemHandle dx dy shiftHeld → do
    let (ElementHandle h) = elemHandle
    broadcastToModules ls "onUIScroll"
      [ ScriptNumber (fromIntegral h)
      , ScriptNumber (realToFrac dx)
      , ScriptNumber (realToFrac dy)
      , ScriptBool shiftHeld
      ]
  LuaUICharInput c →
    broadcastToModules ls "onUICharInput" [ScriptString (T.singleton c)]
  LuaUIBackspace →
    broadcastToModules ls "onUIBackspace" []
  LuaUIDelete →
    broadcastToModules ls "onUIDelete" []
  LuaUISubmit →
    broadcastToModules ls "onUISubmit" []
  LuaUIEscape →
    broadcastToModules ls "onUIEscape" []
  LuaUICursorLeft →
    broadcastToModules ls "onUICursorLeft" []
  LuaUICursorRight →
    broadcastToModules ls "onUICursorRight" []
  LuaUIHome →
    broadcastToModules ls "onUIHome" []
  LuaUIEnd →
    broadcastToModules ls "onUIEnd" []
  LuaUIFocusLost →
    broadcastToModules ls "onUIFocusLost" []
  LuaKeyDownEvent key glfwKey → do
    -- Expose the exact key for the duration of the (synchronous) onKeyDown
    -- broadcast so engine.keyMatchesAction can resolve the precise side of
    -- a merged modifier; clear it afterwards.
    writeIORef (currentKeyDownRef env) (Just glfwKey)
    broadcastToModules ls "onKeyDown" [ScriptString (keyToText key)]
    writeIORef (currentKeyDownRef env) Nothing
  LuaKeyUpEvent key →
    broadcastToModules ls "onKeyUp" [ScriptString (keyToText key)]
  LuaInjectFollowup evs →
    -- Fence follow-up (#697): this queue is FIFO, so every broadcast
    -- the fenced sequence queued ahead of this message has already
    -- been dispatched — its callbacks saw the modifiers still held.
    -- Re-inject the carried releases now; the input thread processes
    -- them like any other event, so nothing is left stuck down. Plain
    -- enqueue, no drain-wait: ordering is the contract, and the Lua
    -- thread must not stall on the input thread's tick.
    mapM_ (Q.writeQueue (inputQueue env)) evs
  LuaShellToggle →
    broadcastToModules ls "onShellToggle" []
  LuaArenaReady pageId →
    broadcastToModules ls "onArenaReady" [ScriptString pageId]
  LuaStampLocation pageId locId gx gy →
    broadcastToModules ls "onStampLocation"
        [ ScriptString pageId, ScriptString locId
        , ScriptNumber (fromIntegral gx), ScriptNumber (fromIntegral gy) ]
  LuaConstructInvalidated pageId gx gy attempt →
    broadcastToModules ls "onConstructInvalidated"
        [ ScriptString pageId
        , ScriptNumber (fromIntegral gx), ScriptNumber (fromIntegral gy)
        , ScriptNumber (fromIntegral attempt) ]
  LuaOpenArena →
    broadcastToModules ls "onOpenArena" []
  LuaDebugToggle → do
    logger ← readIORef (loggerRef env)
    logDebug logger CatLua "Debug overlay toggle requested"
    scriptsMap ← readTVarIO (lbsScripts ls)
    let mDebugScript = find (\s → scriptPath s ≡ "scripts/debug.lua")
                            (Map.elems scriptsMap)
    case mDebugScript of
      Just debugScript → do
        when (isValidRef (scriptModuleRef debugScript)) $ do
          _ ← callModuleFunction ls
                                 (scriptModuleRef debugScript) "toggle" []
          return ()
      Nothing →
        logWarn logger CatLua "Debug script not found"
  LuaDebugShow → do
    logger ← readIORef (loggerRef env)
    logDebug logger CatLua "Debug overlay show requested"
    scriptsMap ← readTVarIO (lbsScripts ls)
    let mDebugScript = find (\s → scriptPath s ≡ "scripts/debug.lua")
                            (Map.elems scriptsMap)
    case mDebugScript of
      Just debugScript → do
        when (isValidRef (scriptModuleRef debugScript)) $ do
          _ ← callModuleFunction ls
                                 (scriptModuleRef debugScript) "show" []
          return ()
      Nothing →
        logWarn logger CatLua "Debug script not found"
  LuaDebugHide → do
    logger ← readIORef (loggerRef env)
    logDebug logger CatLua "Debug overlay hide requested"
    scriptsMap ← readTVarIO (lbsScripts ls)
    let mDebugScript = find (\s → scriptPath s ≡ "scripts/debug.lua")
                            (Map.elems scriptsMap)
    case mDebugScript of
      Just debugScript → do
        when (isValidRef (scriptModuleRef debugScript)) $ do
          _ ← callModuleFunction ls
                                 (scriptModuleRef debugScript) "hide" []
          return ()
      Nothing →
        logWarn logger CatLua "Debug script not found"
  LuaWindowResize w h → do
    broadcastToModules ls "onWindowResize"
      [ScriptNumber (fromIntegral w), ScriptNumber (fromIntegral h)]
  LuaFramebufferResize w h → do
    broadcastToModules ls "onFramebufferResize"
      [ScriptNumber (fromIntegral w), ScriptNumber (fromIntegral h)]
  LuaAssetLoaded assetType handle path → do
    broadcastToModules ls "onAssetLoaded"
      [ ScriptString assetType
      , ScriptNumber (fromIntegral handle)
      , ScriptString path
      ]
  LuaAssetFailed assetType handle path reason → do
    -- #1690: the terminal counterpart of onAssetLoaded, on its own
    -- callback so no waiter can mistake one for the other.
    logger ← readIORef (loggerRef env)
    -- #1842: a texture the unplaced-piece art catalogue registered is
    -- reported by the CATALOGUE instead, which can name the pack, the
    -- kind and the asset role the generic line cannot — and the pack
    -- stops resolving anything, because its art is only meaningful as a
    -- complete set. Exactly ONE warning either way: the contextual line
    -- REPLACES the generic one rather than joining it, and a repeat for
    -- an asset already recorded is silent (the catalogue deduplicates
    -- per pack per path), which is what keeps a re-requested texture
    -- from warning once per attempt.
    report ← if assetType ≢ "texture"
        then pure (ArtFailureReport False Nothing)
        else atomicModifyIORef' (structureArtCatalogRef env)
                                (failPackArtPath path reason)
    -- #1844: a NEW terminal failure makes the whole pack resolve nothing,
    -- which invalidates every outstanding designation naming it —
    -- including ones over chunks that are already resident, where no
    -- later terrain edit or chunk publication would ever re-check them.
    -- Requirement 9's catalogue-reconciliation sweep, enqueued only when
    -- the catalogue actually CHANGED (a repeat for an already-recorded
    -- asset yields no report and enqueues nothing).
    forM_ (afrFailure report) $ \_ →
        Q.writeQueue (worldQueue env) WorldRevalidateConstructAll
    if afrTracked report
        then forM_ (afrFailure report) $
                 logWarn logger CatAsset ∘ artAssetFailureMessage
        else logWarn logger CatAsset $
                "Asset load failed (" <> assetType <> ", handle " <> tshow handle
                  <> "): " <> path <> " -- " <> reason
    -- The fifth argument says the diagnostic is ALREADY REPORTED. Every
    -- Lua handler still runs — a failure has to settle whatever
    -- readiness gate is waiting on it, or boot stalls — but the ones
    -- that log a line of their own skip it, so a tracked structure-art
    -- failure stays at exactly one observable warning however many
    -- modules are listening. `afrTracked`, not `afrFailure`: a repeat
    -- for an asset already recorded emits nothing here either, and Lua
    -- re-announcing it would be the duplicate by another route.
    broadcastToModules ls "onAssetFailed"
      [ ScriptString assetType
      , ScriptNumber (fromIntegral handle)
      , ScriptString path
      , ScriptString reason
      , ScriptBool (afrTracked report)
      ]
  LuaCharInput fid c →
    broadcastToModules ls "onCharInput"
      [ScriptNumber (fromIntegral fid), ScriptString (T.singleton c)]
  LuaTextBackspace fid →
    broadcastToModules ls "onTextBackspace" [ScriptNumber (fromIntegral fid)]
  LuaTextDelete fid →
    broadcastToModules ls "onTextDelete" [ScriptNumber (fromIntegral fid)]
  LuaTabPressed fid →
    broadcastToModules ls "onTabPressed" [ScriptNumber (fromIntegral fid)]
  LuaTextSubmit fid →
    broadcastToModules ls "onTextSubmit" [ScriptNumber (fromIntegral fid)]
  LuaFocusLost fid →
    broadcastToModules ls "onFocusLost" [ScriptNumber (fromIntegral fid)]
  LuaCursorUp fid →
    broadcastToModules ls "onCursorUp" [ScriptNumber (fromIntegral fid)]
  LuaCursorDown fid →
    broadcastToModules ls "onCursorDown" [ScriptNumber (fromIntegral fid)]
  LuaCursorLeft fid →
    broadcastToModules ls "onCursorLeft" [ScriptNumber (fromIntegral fid)]
  LuaCursorRight fid →
    broadcastToModules ls "onCursorRight" [ScriptNumber (fromIntegral fid)]
  LuaCursorHome fid →
    broadcastToModules ls "onCursorHome" [ScriptNumber (fromIntegral fid)]
  LuaCursorEnd fid →
    broadcastToModules ls "onCursorEnd" [ScriptNumber (fromIntegral fid)]
  LuaInterrupt fid →
    broadcastToModules ls "onInterrupt" [ScriptNumber (fromIntegral fid)]
  LuaWorldGenLog text →
    broadcastToModules ls "onWorldGenLog" [ScriptString text]
  LuaSaveLoaded requestId survUnitIds survBuildingIds reconcile → do
    -- Issue #763: the debug-console TCP server keeps
    -- accepting commands onto 'lbsDebugQueue' regardless of the save
    -- barrier's capture-lock state — while this load held the boundary
    -- ('handleLoadStaged' through the world thread's matching publish),
    -- any command that arrived is still sitting there, queued for a
    -- session that no longer exists once this handler runs (this is
    -- the FIRST point on the Lua thread reached after publish — see
    -- @Engine.Scripting.Lua.Thread.luaTick@s own comment on why
    -- 'processLuaMsgs' drains 'LuaSaveLoaded' before 'processDebugCommands'
    -- ever gets a chance to run again). Left alone, that later
    -- 'processDebugCommands' call would execute every one of them
    -- against the REPLACEMENT session — e.g. a queued
    -- @world.setDate(pageId, ...)@ whose pageId the load happens to
    -- reuse would silently mutate the new page. Cancel every command
    -- still queued at this exact handoff instead, resolving its
    -- waiting response MVar so the client (netcat, a script) doesn't
    -- hang: none of them can possibly have been issued FOR the session
    -- that's live from this point on.
    let cancelStaleDebugCommands = do
            mCmd ← pollDebugCommand (lbsDebugQueue ls)
            case mCmd of
                Nothing → pure ()
                Just (DebugCommand _ mvar) → do
                    _ ← tryPutMVar mvar
                        "REJECTED: a load transaction replaced the \
                        \session while this command was queued"
                    cancelStaleDebugCommands
    cancelStaleDebugCommands
    reconcileFailures ← broadcastToModulesReportingErrors ls "onSaveLoaded"
      [ intsToScriptArray survUnitIds
      , intsToScriptArray survBuildingIds
      -- Issue #1589: appended, never inserted — every shipped
      -- 'onSaveLoaded' callback reads the two survivor arrays
      -- positionally, and Lua ignores arguments a function does not
      -- declare, so only the callbacks that actually want the context
      -- (today @scripts/unit_ai.lua@'s) have to name it.
      , reconcileToScriptValue reconcile ]
    -- The transaction is reported
    -- 'LoadPublished' only NOW, once this reconciliation broadcast has
    -- actually run — not the instant the Haskell-side ref swap
    -- happened (World.Load.Publish.publishStagedSession, well before
    -- this message was even drained). The broadcast never
    -- throws (each module call is pcall-guarded internally), so exactly
    -- one of the two terminations below always runs.
    --
    -- Issue #1204: that pcall guard used to SWALLOW what it caught, so
    -- a callback that partially reconciled its singleton and then
    -- raised still reported an unqualified 'LoadSucceeded'. The
    -- shipped callbacks do correctness-critical work (scripts/
    -- unit_ai.lua and building_spawn.lua prune orphaned rows and scrub
    -- typed references, unit_resources.lua rebuilds derived body
    -- statistics, ui_manager_menu.lua rebinds Lua's world/HUD ids to
    -- the published session), so an incompletely reconciled session is
    -- not a presentation-only concern. It gets its own honest terminal
    -- disposition instead — see 'failReconciliation', which is
    -- deliberately not 'failLoad'.
    if null reconcileFailures
      then finishLoad (loadStatusRef env) requestId
      else failReconciliation (loadStatusRef env) requestId
             [ ReconciliationFailure (T.pack scriptPath') err
             | (scriptPath', err) ← reconcileFailures ]
  LuaHudLogInfo text1 text2 kind →
    broadcastToModules ls "onSetInfoText"
      [ScriptString text1, ScriptString text2, ScriptString kind]
  LuaHudLogWeatherInfo text →
    broadcastToModules ls "onSetWeatherInfo" [ScriptString text]
  LuaHudLogResourcesInfo text →
    broadcastToModules ls "onSetResourcesInfo" [ScriptString text]
  LuaWorldPreviewReady handleInt gen → do
    -- Issue #763: validated HERE, at delivery, not at
    -- upload-completion time — see the long comment on
    -- 'Engine.Scripting.Lua.Message.WorldTexture.handleWorldPreview'
    -- for why upload-completion time can't decide this correctly. Every
    -- 'LuaMsg' this dispatcher processes (this one included) only ever
    -- runs while the save barrier's capture lock is open, and a load
    -- transaction holds that lock for its entire duration — so by now,
    -- any publish that could have superseded this preview has
    -- unconditionally already run and bumped
    -- 'worldPreviewGenerationRef' (on EVERY publish, whether or not
    -- that publish carries its own new preview — see
    -- 'World.Load.Publish.publishStagedSession').
    latestGen ← readIORef (worldPreviewGenerationRef env)
    if gen ≢ latestGen
      then do
        logger ← readIORef (loggerRef env)
        logDebug logger CatLua
            "World preview announcement superseded by a later publish \
            \— discarding stale generation"
      else
        broadcastToModules ls "onWorldPreviewReady"
          [ScriptNumber (fromIntegral handleInt)]
  LuaShowPopup category msg r g b a mCoords mPage →
    -- #1588: the page rides alongside the coords, as the 8th argument,
    -- so a LIVE popup carries the same coordinate frame an event-log
    -- REPLAY does (scripts/event_log.lua's onRowClick forwards ev.page
    -- into the same slot). Trailing and optional: a handler written
    -- against the old seven-argument shape still binds every argument
    -- it declares, and simply sees no page.
    broadcastToModules ls "onShowPopup"
      [ ScriptString category
      , ScriptString msg
      , ScriptNumber (realToFrac r)
      , ScriptNumber (realToFrac g)
      , ScriptNumber (realToFrac b)
      , ScriptNumber (realToFrac a)
      , coordsToScriptValue mCoords
      , maybe ScriptNil ScriptString mPage
      ]
  LuaLoadStaged requestId → handleLoadStaged env ls requestId
  LuaLoadStagingFailed requestId → do
      -- This message is QUEUED (not a direct call), so
      -- it can be processed well after 'requestId' has already gone
      -- terminal and a NEWER request has been accepted and prepared its
      -- own Lua state. Passing 'requestId' through lets 'abortLuaLoad'
      -- no-op instead of clearing state that belongs to that newer
      -- request (see 'Engine.Scripting.Lua.API.Save.Bridge.abortLuaLoad').
      logger ← readIORef (loggerRef env)
      Lua.runWith (lbsLuaState ls) (abortLuaLoad logger requestId)

-- | Issue #763 (save-overhaul C2): a whole-session load transaction just
--   finished STAGING (on the world thread, touching no live ref) and is
--   ready to publish. This is the Lua thread's turn to drive the SAME
--   'Engine.Save.Barrier' owner-quiescence protocol 'engine.saveWorld'
--   uses — reused as-is rather than duplicated, since "every other
--   state-owner thread must briefly stop touching shared state" is
--   identical plumbing regardless of WHY — so that Unit/Building/Combat/
--   Simulation are all quiesced before either side of the publish
--   becomes observable (requirement 10). Once quiesced, this function
--   applies the ALREADY-VALIDATED prepared Lua state
--   ('Engine.Scripting.Lua.API.Save.Bridge.applyLuaLoad') itself (an HsLua call,
--   so it must run here, on the Lua thread) and only then queues
--   'WorldLoadPublish' for the world thread to perform the matching
--   Haskell-side ref swap — satisfying requirement 11 (no Haskell state
--   becomes observable while required Lua state can still fail: a Lua
--   apply failure aborts here, before 'WorldLoadPublish' is ever queued,
--   leaving the old session completely untouched).
handleLoadStaged ∷ EngineEnv → LuaBackendState → Int → IO ()
handleLoadStaged env ls requestId = do
    logger ← readIORef (loggerRef env)
    -- SaveInput is omitted when the input thread was
    -- never started (App.Headless boots without one — no GLFW window
    -- to poll), so waitForOwners below never times out forever waiting
    -- on an owner that can never acknowledge. See
    -- Engine.Scripting.Lua.API.Save.saveOwnerSet — the identical
    -- computation, duplicated rather than shared because the natural
    -- home ("Engine.Save.Barrier") is what Engine.Core.State already
    -- depends on for SaveBarrier itself, so importing EngineEnv there
    -- would cycle.
    --
    -- SaveRender is ALWAYS included
    -- (unlike SaveInput above, never conditioned on a thread-active
    -- flag). Every boot mode capable of reaching this function at all
    -- (i.e. running a debug console that can accept 'engine.loadSave')
    -- runs one of the three main loops, and all three acknowledge
    -- SaveRender through the single shared handshake in
    -- @Engine.Loop.Mode.runGatedByCaptureLock@ — headless included: it
    -- runs no 'Engine.Loop.mainLoop'/'mainLoopOffscreen', but it DOES
    -- run 'Engine.Loop.Headless.headlessLoop', which drains the exact
    -- same 'luaToEngineQueue' via the same 'processLuaMessages'.
    -- '--dump' is the only loop-free mode, and it has no debug
    -- console/Lua thread to ever call this in the first place. WHY that
    -- acknowledgement has to be a genuine owner handshake rather than a
    -- point-in-time 'captureLocked' pre-check is explained once, at
    -- @Engine.Loop.Mode.runGatedByCaptureLock@; a plain save omits
    -- SaveRender ('Engine.Scripting.Lua.API.Save.saveOwnerSet').
    inputActive ← readIORef (inputThreadActiveRef env)
    let baseOwners = Set.fromList
            [SaveLua, SaveWorld, SaveUnit, SaveBuilding, SaveCombat, SaveSimulation]
        withInput = if inputActive then Set.insert SaveInput baseOwners else baseOwners
        owners = Set.insert SaveRender withInput
    started ← beginSave (saveBarrierRef env) owners
    case started of
      Left err → do
        logWarn logger CatWorld $
            "load publish #" <> tshow requestId
            <> " could not begin the publish barrier: " <> err
        failLoad (loadStatusRef env) requestId err
        -- prepareLuaLoad already succeeded by the time
        -- staging (and thus this dispatch) ever runs, leaving Lua's
        -- registration guard (_loadActive) active until applyAll
        -- commits it -- which never happens on this failure path.
        Lua.runWith (lbsLuaState ls) (abortLuaLoad logger requestId)
      Right barrierRequestId → do
        -- The Lua thread is the one driving this transaction and is
        -- therefore already quiescent for its own duration (mirrors
        -- 'engine.saveWorld''s identical self-ack).
        acknowledgeSave (saveBarrierRef env) barrierRequestId SaveLua
        ready ← waitForOwners 5000000 (saveBarrierRef env) barrierRequestId
        case ready of
          Left err → do
            failSave (saveBarrierRef env) barrierRequestId err
            logWarn logger CatWorld $
                "load publish #" <> tshow requestId
                <> " timed out waiting for state owners: " <> err
            failLoad (loadStatusRef env) requestId err
            writeIORef (pendingLoadRef env) Nothing
            -- Same as the beginSave failure above --
            -- abort the prepared-but-never-applied Lua load.
            Lua.runWith (lbsLuaState ls) (abortLuaLoad logger requestId)
          Right () → do
            reachSnapshot (saveBarrierRef env) barrierRequestId
            advanceLoad (loadStatusRef env) requestId LoadWaitingPublish
            applied ← Lua.runWith (lbsLuaState ls) (applyLuaLoad logger)
            case applied of
              -- applyLuaLoad is only reachable after prepareLoad already
              -- validated every component back in loadSaveFn, so this is
              -- a genuine apply()/reset-hook bug, not a data problem —
              -- but it must still abort (requirement 6: no required
              -- failure may be partial). Failing here — BEFORE
              -- WorldLoadPublish is ever queued — means the Haskell side
              -- never changes at all.
              Left err → do
                failSave (saveBarrierRef env) barrierRequestId err
                logWarn logger CatWorld $
                    "load publish #" <> tshow requestId
                    <> " failed applying Lua state: " <> err
                failLoad (loadStatusRef env) requestId err
                writeIORef (pendingLoadRef env) Nothing
              Right () → do
                -- Every message that
                -- reached this SAME queue while staging was in flight
                -- (staging can take a while — worldgen chunk
                -- regeneration is the dominant cost) is still sitting
                -- behind the 'LuaLoadStaged' message that triggered
                -- this whole function, since 'luaQueue' is FIFO and
                -- this call runs synchronously to completion before
                -- the Lua thread's own loop returns to draining it.
                -- Discarding it here, still ON THE LUA THREAD and
                -- still before 'WorldLoadPublish' is even queued,
                -- closes that window — a stale queued UI click or
                -- debug-console call must not fire against the
                -- replacement session once the Lua thread's normal
                -- loop resumes draining this queue right after this
                -- function returns. (Flushing from the WORLD thread's
                -- 'World.Load.Publish.publishStagedSession' instead
                -- would race that same resumed drain and very likely
                -- lose — see its haddock.)
                stale ← Q.flushQueue (luaQueue env)
                when (not (null stale)) $
                    logWarn logger CatWorld $
                        "Load publish discarded " <> tshow (length stale)
                        <> " stale Lua message(s) queued during staging"
                -- #1602: a publish REPLACES the page set, so it is a
                -- selection change and must read as pending from the
                -- moment it is queued. 'publishStagedSession' resets the
                -- count outright, so this is balanced however the
                -- transaction ends.
                atomicModifyIORef' (worldManagerRef env) $ \mgr →
                    (requestSelectionChange True ([], []) mgr, ())
                Q.writeQueue (worldQueue env) (WorldLoadPublish requestId)

-- | Build a Lua array @{ id1, id2, ... }@ from a list of integer ids.
--   Used by 'LuaSaveLoaded' to hand the surviving loaded-page unit /
--   building ids to the Lua reconcile callback; it iterates with @ipairs@.
intsToScriptArray ∷ [Int] → ScriptValue
intsToScriptArray xs = ScriptTable $
    zipWith (\i x → ( ScriptNumber (fromIntegral (i ∷ Int))
                    , ScriptNumber (fromIntegral x) ))
            [1..] xs

-- | Marshal the restored session's reconciliation context (issue
--   #1589) into the one Lua table @onSaveLoaded@'s third argument
--   carries:
--
-- > { item_instance = { [iid] = true, ... },
-- >   unitPage      = { [uid] = "<page id>", ... },
-- >   byPage = { craft_bill  = { ["<page id>"] = { [bill] = true, ... } },
-- >              ground_item = { ["<page id>"] = { [gid]  = true, ... } } } }
--
--   Keyed by the same reference-KIND vocabulary
--   'World.Save.Integrity.luaEdgeResolves' and the Lua @references()@
--   hooks already speak, and split by SCOPE: the session-global kind
--   sits at the top level, the two per-page kinds under @byPage@ where
--   a page id must be supplied to reach a set at all. That shape is
--   what stops a reconcile from resolving a per-page id session-wide.
--
--   Every one of the four tables is always present, empty or not — an
--   empty session is a real value, and the Lua side treats a MISSING
--   table as an engine fault rather than as "nothing to check".
reconcileToScriptValue ∷ LoadReconcileContext → ScriptValue
reconcileToScriptValue rc = ScriptTable
    [ (ScriptString "item_instance", idSet (lrcItemInstances rc))
    , (ScriptString "unitPage", ScriptTable
        [ (ScriptNumber (fromIntegral uid), ScriptString pid)
        | (uid, pid) ← lrcUnitPages rc ])
    , (ScriptString "byPage", ScriptTable
        [ (ScriptString "craft_bill",  byPage (lrcBillsByPage rc))
        , (ScriptString "ground_item", byPage (lrcGroundItemsByPage rc)) ])
    ]
  where
    idSet ids = ScriptTable
        [ (ScriptNumber (fromIntegral i), ScriptBool True) | i ← ids ]
    byPage entries = ScriptTable
        [ (ScriptString pid, idSet ids) | (pid, ids) ← entries ]

-- | Encode the optional payload as either @{x=gx, y=gy}@ or 'nil'.
--   The Lua-side popup module makes a line clickable only when its
--   coords are non-nil.
coordsToScriptValue ∷ Maybe (Int, Int) → ScriptValue
coordsToScriptValue Nothing = ScriptNil
coordsToScriptValue (Just (gx, gy)) = ScriptTable
    [ (ScriptString "x", ScriptNumber (fromIntegral gx))
    , (ScriptString "y", ScriptNumber (fromIntegral gy))
    ]
