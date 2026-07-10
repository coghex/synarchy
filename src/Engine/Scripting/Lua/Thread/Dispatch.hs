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
import Engine.ActionOutcome (ActionOutcome(..), pushActionOutcome)
import UI.Types (ElementHandle(..))
import qualified Graphics.UI.GLFW as GLFW
import qualified Engine.Core.Queue as Q
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.List (find)
import Data.IORef (IORef, readIORef, writeIORef)
import Control.Concurrent.STM (readTVarIO)

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
  LuaUIClickEvent elemHandle callbackName → do
    let (ElementHandle h) = elemHandle
    recordWidgetClickOutcome env "input.click" callbackName
    broadcastToModules ls callbackName [ScriptNumber (fromIntegral h)]
  LuaUIRightClickEvent elemHandle callbackName → do
    let (ElementHandle h) = elemHandle
    recordWidgetClickOutcome env "input.rightClick" callbackName
    broadcastToModules ls callbackName [ScriptNumber (fromIntegral h)]
  LuaUIScrollEvent elemHandle dx dy → do
    let (ElementHandle h) = elemHandle
    broadcastToModules ls "onUIScroll"
      [ ScriptNumber (fromIntegral h)
      , ScriptNumber (realToFrac dx)
      , ScriptNumber (realToFrac dy)
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
  LuaSaveLoaded survUnitIds survBuildingIds →
    broadcastToModules ls "onSaveLoaded"
      [ intsToScriptArray survUnitIds
      , intsToScriptArray survBuildingIds ]
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

-- | F4 (#646) Layer A: a UI element ate this click — record which
--   callback consumed it. This is the "widget" half of the input-routing
--   consolidation (deadclick / tool / world are the Lua-side
--   scripts/init_mouse.lua chain's job, since only it knows whether the
--   ClickGame route it's handed ultimately hit nothing).
recordWidgetClickOutcome ∷ EngineEnv → Text → Text → IO ()
recordWidgetClickOutcome env kind callbackName = do
    gt ← readIORef (gameTimeRef env)
    pushActionOutcome (actionOutcomeRef env) ActionOutcome
        { aoTs        = gt
        , aoKind      = kind
        , aoOutcome   = "accepted"
        , aoWhereX    = Nothing
        , aoWhereY    = Nothing
        , aoTarget    = Nothing
        , aoRequested = Nothing
        , aoApplied   = Nothing
        , aoDropped   = Nothing
        , aoReason    = Nothing
        , aoHandler   = Just callbackName
        }

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
