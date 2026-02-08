module Engine.Scripting.Lua.API
  ( registerLuaAPI
  , registerLuaFunction
  ) where

import UPrelude
import Engine.Scripting.Lua.Types (LuaBackendState)
import Engine.Scripting.Lua.API.Core (loadScriptFn, killScriptFn, 
                                      setTickIntervalFn, pauseScriptFn, 
                                      resumeScriptFn, quitFn, getFPSFn)
import Engine.Scripting.Lua.API.Debug (showDebugFn, hideDebugFn, toggleDebugFn)
import Engine.Scripting.Lua.API.Config (getVideoConfigFn, setVideoConfigFn
                                       , saveVideoConfigFn, setUIScaleFn
                                       , setFrameLimitFn, setResolutionFn
                                       , setWindowModeFn, setVSyncFn
                                       , setMSAAFn, setBrightnessFn
                                       , setPixelSnapFn, setTextureFilterFn
                                       , loadDefaultConfigFn)
import Engine.Scripting.Lua.API.Graphics (loadTextureFn, spawnSpriteFn, setPosFn,
                                           setColorFn, setSizeFn, setVisibleFn
                                           , destroyFn, getUIScaleFn)
import Engine.Scripting.Lua.API.Log (logInfoFn, logWarnFn, logDebugFn)
import Engine.Scripting.Lua.API.Input (isKeyDownFn, isActionDownFn,
                                       getMousePositionFn,
                                       isMouseButtonDownFn, getWindowSizeFn, 
                                       getFramebufferSizeFn, getWorldCoordFn)
import Engine.Scripting.Lua.API.Text (loadFontFn, spawnTextFn, setTextFn, 
                                       getTextFn, getTextWidthFn)
import Engine.Scripting.Lua.API.Focus (registerFocusableFn, requestFocusFn, 
                                        releaseFocusFn, getFocusIdFn)
import Engine.Scripting.Lua.API.Shell (shellExecuteFn)
import Engine.Scripting.Lua.API.UI
import Engine.Core.State (EngineEnv)
import qualified HsLua as Lua
import qualified Data.ByteString.Char8 as BS

-- | Helper to register a Lua function with less boilerplate
registerLuaFunction ∷ BS.ByteString → Lua.LuaE Lua.Exception Lua.NumResults
                    → Lua.LuaE Lua.Exception ()
registerLuaFunction name action = do
    Lua.pushHaskellFunction action
    Lua.setfield (-2) (Lua.Name name)

-- | Register all engine API functions with Lua
registerLuaAPI ∷ Lua.State → EngineEnv → LuaBackendState → IO ()
registerLuaAPI lst env backendState = Lua.runWith lst $ do
  Lua.newtable
  
  -- Core functions
  registerLuaFunction "quit"              (quitFn env)
  registerLuaFunction "logInfo"           (logInfoFn env)
  registerLuaFunction "logWarn"           (logWarnFn env)
  registerLuaFunction "logDebug"          (logDebugFn env)
  registerLuaFunction "showDebug"         (showDebugFn backendState)
  registerLuaFunction "hideDebug"         (hideDebugFn backendState)
  registerLuaFunction "toggleDebug"       (toggleDebugFn backendState)
  registerLuaFunction "getFPS"            (getFPSFn env)
  registerLuaFunction "loadScript"        (loadScriptFn env backendState lst)
  registerLuaFunction "killScript"        (killScriptFn env backendState lst)
  registerLuaFunction "pauseScript"       (pauseScriptFn backendState)
  registerLuaFunction "resumeScript"      (resumeScriptFn backendState)
  registerLuaFunction "setTickInterval"   (setTickIntervalFn env backendState)

  -- Config functions
  registerLuaFunction "getVideoConfig"    (getVideoConfigFn env)
  registerLuaFunction "setVideoConfig"    (setVideoConfigFn env)
  registerLuaFunction "saveVideoConfig"   (saveVideoConfigFn env)
  registerLuaFunction "loadDefaultConfig" (loadDefaultConfigFn env)
  registerLuaFunction "setUIScale"        (setUIScaleFn env)
  registerLuaFunction "setFrameLimit"     (setFrameLimitFn env)
  registerLuaFunction "setResolution"     (setResolutionFn env)
  registerLuaFunction "setWindowMode"     (setWindowModeFn env)
  registerLuaFunction "setVSync"          (setVSyncFn env)
  registerLuaFunction "setMSAA"           (setMSAAFn env)
  registerLuaFunction "setBrightness"     (setBrightnessFn env)
  registerLuaFunction "setPixelSnap"      (setPixelSnapFn env)
  registerLuaFunction "setTextureFilter"  (setTextureFilterFn env)
  
  -- Graphics functions
  registerLuaFunction "loadTexture"   (loadTextureFn backendState)
  registerLuaFunction "spawnSprite"   (spawnSpriteFn env backendState)
  registerLuaFunction "setPos"        (setPosFn env backendState)
  registerLuaFunction "setColor"      (setColorFn env backendState)
  registerLuaFunction "setSize"       (setSizeFn env backendState)
  registerLuaFunction "setVisible"    (setVisibleFn env backendState)
  registerLuaFunction "destroy"       (destroyFn env backendState)
  registerLuaFunction "getUIScale"    (getUIScaleFn env)
  
  -- Input functions
  registerLuaFunction "isKeyDown"         (isKeyDownFn backendState)
  registerLuaFunction "isActionDown"      (isActionDownFn env backendState)
  registerLuaFunction "getMousePosition"  (getMousePositionFn backendState)
  registerLuaFunction "isMouseButtonDown" (isMouseButtonDownFn backendState)
  registerLuaFunction "getWindowSize"     (getWindowSizeFn env backendState)
  registerLuaFunction "getFramebufferSize" (getFramebufferSizeFn env backendState)
  registerLuaFunction "getWorldCoord"     (getWorldCoordFn env backendState)
  
  -- Text functions
  registerLuaFunction "loadFont"     (loadFontFn env backendState)
  registerLuaFunction "spawnText"    (spawnTextFn env backendState)
  registerLuaFunction "setText"      (setTextFn env)
  registerLuaFunction "getText"      (getTextFn env)
  registerLuaFunction "getTextWidth" (getTextWidthFn env)
  
  -- Focus functions
  registerLuaFunction "registerFocusable" (registerFocusableFn env)
  registerLuaFunction "requestFocus"      (requestFocusFn env)
  registerLuaFunction "releaseFocus"      (releaseFocusFn env)
  registerLuaFunction "getFocusId"        (getFocusIdFn env)
 
  -- Shell functions
  registerLuaFunction "shellExecute" shellExecuteFn
  
  Lua.setglobal (Lua.Name "engine")
  
  -- Register UI table separately
  Lua.newtable
  
  -- UI Page functions
  registerLuaFunction "newPage"    (uiNewPageFn env)
  registerLuaFunction "deletePage" (uiDeletePageFn env)
  registerLuaFunction "showPage"   (uiShowPageFn env)
  registerLuaFunction "hidePage"   (uiHidePageFn env)
  
  -- UI Element creation functions
  registerLuaFunction "newElement" (uiNewElementFn env)
  registerLuaFunction "newBox"     (uiNewBoxFn env)
  registerLuaFunction "newText"    (uiNewTextFn env)
  registerLuaFunction "newSprite"  (uiNewSpriteFn env)
  
  -- UI Hierarchy functions
  registerLuaFunction "addToPage"      (uiAddToPageFn env)
  registerLuaFunction "addChild"       (uiAddChildFn env)
  registerLuaFunction "removeElement"  (uiRemoveElementFn env)
  registerLuaFunction "deleteElement"  (uiDeleteElementFn env)
  registerLuaFunction "findElementAt" (uiFindElementAtFn env)
  registerLuaFunction "getElementOnClick" (uiGetElementOnClickFn env)
  registerLuaFunction "findHoverTarget" (uiFindHoverTargetFn env)

  -- Text buffer operations
  registerLuaFunction "enableTextInput" (uiEnableTextInputFn env)
  registerLuaFunction "getTextInput" (uiGetTextFn env)
  registerLuaFunction "setTextInput" (uiSetTextInputFn env)
  registerLuaFunction "getCursor" (uiGetCursorFn env)
  registerLuaFunction "setCursor" (uiSetCursorFn env)
  registerLuaFunction "insertChar" (uiInsertCharFn env)
  registerLuaFunction "deleteBackward" (uiDeleteBackwardFn env)
  registerLuaFunction "deleteForward" (uiDeleteForwardFn env)
  registerLuaFunction "cursorLeft" (uiCursorLeftFn env)
  registerLuaFunction "cursorRight" (uiCursorRightFn env)
  registerLuaFunction "cursorHome" (uiCursorHomeFn env)
  registerLuaFunction "cursorEnd" (uiCursorEndFn env)

  -- UI Focus functions
  registerLuaFunction "setFocus"       (uiSetFocusFn env)
  registerLuaFunction "clearFocus"     (uiClearFocusFn env)
  registerLuaFunction "getFocus"       (uiGetFocusFn env)
  registerLuaFunction "hasFocus"       (uiHasFocusFn env)
  
  -- UI Property functions
  registerLuaFunction "setPosition"  (uiSetPositionFn env)
  registerLuaFunction "setSize"      (uiSetSizeFn env)
  registerLuaFunction "setVisible"   (uiSetVisibleFn env)
  registerLuaFunction "setClickable" (uiSetClickableFn env)
  registerLuaFunction "setZIndex"    (uiSetZIndexFn env)
  registerLuaFunction "setColor"     (uiSetColorFn env)
  registerLuaFunction "setText"      (uiSetTextFn env)
  registerLuaFunction "setSpriteTexture" (uiSetSpriteTextureFn env)
  registerLuaFunction "setOnClick"   (uiSetOnClickFn env)
  
  -- UI textures
  registerLuaFunction "setBoxTextures" (uiSetBoxTexturesFn env)
  registerLuaFunction "loadBoxTextures" (uiLoadBoxTexturesFn env)
  
  Lua.setglobal (Lua.Name "UI")
