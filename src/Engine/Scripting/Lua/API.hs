module Engine.Scripting.Lua.API
  ( registerLuaAPI
  , registerLuaFunction
  ) where

import UPrelude
import Engine.Scripting.Lua.Types (LuaBackendState)
import Engine.Scripting.Lua.API.Core (logInfoFn, loadScriptFn, killScriptFn, 
                                       setTickIntervalFn, pauseScriptFn, 
                                       resumeScriptFn)
import Engine.Scripting.Lua.API.Graphics (loadTextureFn, spawnSpriteFn, setPosFn,
                                           setColorFn, setSizeFn, setVisibleFn, destroyFn)
import Engine.Scripting.Lua.API.Input (isKeyDownFn, isActionDownFn, getMousePositionFn,
                                        isMouseButtonDownFn, getWindowSizeFn, 
                                        getFramebufferSizeFn, getWorldCoordFn)
import Engine.Scripting.Lua.API.Text (loadFontFn, spawnTextFn, setTextFn, 
                                       getTextFn, getTextWidthFn)
import Engine.Scripting.Lua.API.Focus (registerFocusableFn, requestFocusFn, 
                                        releaseFocusFn, getFocusIdFn)
import Engine.Scripting.Lua.API.Shell (shellExecuteFn)
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
  registerLuaFunction "logInfo"         (logInfoFn env)
  registerLuaFunction "loadScript"      (loadScriptFn env backendState lst)
  registerLuaFunction "killScript"      (killScriptFn backendState lst)
  registerLuaFunction "pauseScript"     (pauseScriptFn backendState)
  registerLuaFunction "resumeScript"    (resumeScriptFn backendState)
  registerLuaFunction "setTickInterval" (setTickIntervalFn env backendState)
  
  -- Graphics functions
  registerLuaFunction "loadTexture"  (loadTextureFn backendState)
  registerLuaFunction "spawnSprite"  (spawnSpriteFn env backendState)
  registerLuaFunction "setPos"       (setPosFn env backendState)
  registerLuaFunction "setColor"     (setColorFn env backendState)
  registerLuaFunction "setSize"      (setSizeFn env backendState)
  registerLuaFunction "setVisible"   (setVisibleFn env backendState)
  registerLuaFunction "destroy"      (destroyFn env backendState)
  
  -- Input functions
  registerLuaFunction "isKeyDown"         (isKeyDownFn backendState)
  registerLuaFunction "isActionDown"      (isActionDownFn env backendState)
  registerLuaFunction "getMousePosition"  (getMousePositionFn backendState)
  registerLuaFunction "isMouseButtonDown" (isMouseButtonDownFn backendState)
  registerLuaFunction "getWindowSize"     (getWindowSizeFn backendState)
  registerLuaFunction "getFramebufferSize" (getFramebufferSizeFn backendState)
  registerLuaFunction "getWorldCoord"     (getWorldCoordFn env backendState)
  
  -- Text functions
  registerLuaFunction "loadFont"     (loadFontFn backendState)
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
