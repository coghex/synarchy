-- | Engine-to-Lua message dispatch: drains 'LuaMsg' values off the
--   engine-to-Lua queue and broadcasts each as the matching Lua module
--   callback (onKeyDown, onMouseUp, onSaveLoaded, ...).
module Engine.Scripting.Lua.Thread.Dispatch
  ( processLuaMsg
  , processLuaMsgs
  ) where

import UPrelude
import Engine.Scripting.Types
import Engine.Scripting.Lua.Types
import Engine.Scripting.Lua.Script (callModuleFunction)
import Engine.Scripting.Lua.Util (isValidRef, broadcastToModules)
import Engine.Core.Log (logWarn, logDebug, LogCategory(..))
import Engine.Core.Thread
import Engine.Core.State (EngineEnv(..))
import Engine.Input.Types (keyToText, clickRouteText)
import UI.Types (ElementHandle(..))
import qualified Graphics.UI.GLFW as GLFW
import qualified Engine.Core.Queue as Q
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified HsLua as Lua
import Data.List (find)
import Data.IORef (IORef, readIORef, writeIORef)
import Control.Concurrent.STM (readTVarIO)
import Engine.Save.Barrier
    ( SaveOwner(..), beginSave, acknowledgeSave, waitForOwners
    , reachSnapshot, failSave )
import Engine.Load.Status (advanceLoad, failLoad, finishLoad, LoadPhase(..))
import Engine.Scripting.Lua.API.Save (applyLuaLoad)
import World.Command.Types (WorldCommand(..))

processLuaMsgs ∷ EngineEnv → LuaBackendState → IORef ThreadControl → IO ()
processLuaMsgs env ls stateRef = do
    let (_, etlq) = lbsMsgQueues ls
    mMsg ← Q.tryReadQueue etlq
    case mMsg of
        Just msg → do
            logger ← readIORef (loggerRef env)
            logDebug logger CatLua $ "Engine-to-Lua message: " <> T.pack (show msg)
            processLuaMsg env ls stateRef msg
            processLuaMsgs env ls stateRef
        Nothing → return ()

processLuaMsg ∷ EngineEnv → LuaBackendState → IORef ThreadControl → LuaMsg → IO ()
processLuaMsg env ls stateRef msg = case msg of
  LuaTextureLoaded handle assetId → do
    logger ← readIORef (loggerRef env)
    logDebug logger CatLua $
        "Texture loaded with handle " <> T.pack (show handle)
        <> " as asset " <> T.pack (show assetId)
  LuaFontLoaded handle path → do
    logger ← readIORef (loggerRef env)
    logDebug logger CatLua $
        "Font " <> T.pack (show path)
        <> " loaded with handle " <> T.pack (show handle)
  LuaFontLoadFailed err → do
    logger ← readIORef (loggerRef env)
    logWarn logger CatLua $
        "Font load failed: " <> T.pack (show err)
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
    -- F4 (#730 review round 2): NOT recorded here any more — a
    -- ClickUI-routed press's outcome is deferred to its matching
    -- release, so the gesture can be classified as a plain click or a
    -- UI-widget drag exactly once (see Engine.Input.Thread's
    -- pendingUIClickRef / inpPendingUIClick).
    broadcastToModules ls callbackName [ScriptNumber (fromIntegral h)]
  LuaUIRightClickEvent elemHandle callbackName _x _y → do
    let (ElementHandle h) = elemHandle
    -- F4 (#730 review round 2): see LuaUIClickEvent above.
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
  LuaSaveLoaded requestId survUnitIds survBuildingIds → do
    broadcastToModules ls "onSaveLoaded"
      [ intsToScriptArray survUnitIds
      , intsToScriptArray survBuildingIds ]
    -- Round 2 review, requirement 9: the transaction is reported
    -- 'LoadPublished' only NOW, once this reconciliation broadcast has
    -- actually run — not the instant the Haskell-side ref swap
    -- happened (World.Load.Publish.publishStagedSession, well before
    -- this message was even drained). 'broadcastToModules' never
    -- throws (each module call is pcall-guarded internally), so this
    -- always runs.
    finishLoad (loadStatusRef env) requestId
  LuaHudLogInfo text1 text2 kind →
    broadcastToModules ls "onSetInfoText"
      [ScriptString text1, ScriptString text2, ScriptString kind]
  LuaHudLogWeatherInfo text →
    broadcastToModules ls "onSetWeatherInfo" [ScriptString text]
  LuaHudLogResourcesInfo text →
    broadcastToModules ls "onSetResourcesInfo" [ScriptString text]
  LuaWorldPreviewReady handleInt →
    broadcastToModules ls "onWorldPreviewReady"
      [ScriptNumber (fromIntegral handleInt)]
  LuaShowPopup category msg r g b a mCoords →
    broadcastToModules ls "onShowPopup"
      [ ScriptString category
      , ScriptString msg
      , ScriptNumber (realToFrac r)
      , ScriptNumber (realToFrac g)
      , ScriptNumber (realToFrac b)
      , ScriptNumber (realToFrac a)
      , coordsToScriptValue mCoords
      ]
  LuaLoadStaged requestId → handleLoadStaged env ls requestId

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
--   ('Engine.Scripting.Lua.API.Save.applyLuaLoad') itself (an HsLua call,
--   so it must run here, on the Lua thread) and only then queues
--   'WorldLoadPublish' for the world thread to perform the matching
--   Haskell-side ref swap — satisfying requirement 11 (no Haskell state
--   becomes observable while required Lua state can still fail: a Lua
--   apply failure aborts here, before 'WorldLoadPublish' is ever queued,
--   leaving the old session completely untouched).
handleLoadStaged ∷ EngineEnv → LuaBackendState → Int → IO ()
handleLoadStaged env ls requestId = do
    logger ← readIORef (loggerRef env)
    started ← beginSave (saveBarrierRef env)
        (Set.fromList [SaveLua, SaveWorld, SaveUnit, SaveBuilding, SaveCombat, SaveSimulation])
    case started of
      Left err → do
        logWarn logger CatWorld $
            "load publish #" <> T.pack (show requestId)
            <> " could not begin the publish barrier: " <> err
        failLoad (loadStatusRef env) requestId err
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
                "load publish #" <> T.pack (show requestId)
                <> " timed out waiting for state owners: " <> err
            failLoad (loadStatusRef env) requestId err
            writeIORef (pendingLoadRef env) Nothing
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
                    "load publish #" <> T.pack (show requestId)
                    <> " failed applying Lua state: " <> err
                failLoad (loadStatusRef env) requestId err
                writeIORef (pendingLoadRef env) Nothing
              Right () → do
                -- Round 2 review (requirement 12): every message that
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
                        "Load publish discarded " <> T.pack (show (length stale))
                        <> " stale Lua message(s) queued during staging"
                Q.writeQueue (worldQueue env) (WorldLoadPublish requestId)

-- | Build a Lua array @{ id1, id2, ... }@ from a list of integer ids.
--   Used by 'LuaSaveLoaded' to hand the surviving loaded-page unit /
--   building ids to the Lua reconcile callback; it iterates with @ipairs@.
intsToScriptArray ∷ [Int] → ScriptValue
intsToScriptArray xs = ScriptTable $
    zipWith (\i x → ( ScriptNumber (fromIntegral (i ∷ Int))
                    , ScriptNumber (fromIntegral x) ))
            [1..] xs

-- | Encode the optional payload as either @{x=gx, y=gy}@ or 'nil'.
--   The Lua-side popup module makes a line clickable only when its
--   coords are non-nil.
coordsToScriptValue ∷ Maybe (Int, Int) → ScriptValue
coordsToScriptValue Nothing = ScriptNil
coordsToScriptValue (Just (gx, gy)) = ScriptTable
    [ (ScriptString "x", ScriptNumber (fromIntegral gx))
    , (ScriptString "y", ScriptNumber (fromIntegral gy))
    ]
