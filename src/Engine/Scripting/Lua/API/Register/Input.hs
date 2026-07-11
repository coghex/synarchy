module Engine.Scripting.Lua.API.Register.Input
  ( registerInputAPI
  ) where

import Data.IORef (IORef)
import Engine.Core.State (EngineEnv)
import Engine.Core.Thread (ThreadControl)
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.InputInject
  ( inputMoveMouseFn, inputClickFn, inputMouseDownFn, inputMouseUpFn
  , inputScrollFn, inputKeyFn, inputKeyDownFn, inputKeyUpFn, inputTypeFn )
import Engine.Scripting.Lua.Types (LuaBackendState)
import qualified HsLua as Lua

-- | Populate and install the @input@ global table (#644): synthetic
--   input injection for the playtest harness. Read-side input queries
--   (isKeyDown, getMousePosition, …) stay on the @engine@ table.
--
--   Takes the same 'LuaBackendState' + thread-control ref every verb's
--   ack needs to settle its modifier lifetime synchronously (#727) —
--   see 'Engine.Scripting.Lua.API.InputInject.injectAndSettle'.
registerInputAPI ∷ EngineEnv → LuaBackendState → IORef ThreadControl
                 → Lua.LuaE Lua.Exception ()
registerInputAPI env ls stateRef = do
  Lua.newtable

  registerLuaFunction "moveMouse" (inputMoveMouseFn env ls stateRef)
  registerLuaFunction "click"     (inputClickFn env ls stateRef)
  registerLuaFunction "mouseDown" (inputMouseDownFn env ls stateRef)
  registerLuaFunction "mouseUp"   (inputMouseUpFn env ls stateRef)
  registerLuaFunction "scroll"    (inputScrollFn env ls stateRef)
  registerLuaFunction "key"       (inputKeyFn env ls stateRef)
  registerLuaFunction "keyDown"   (inputKeyDownFn env ls stateRef)
  registerLuaFunction "keyUp"     (inputKeyUpFn env ls stateRef)
  registerLuaFunction "type"      (inputTypeFn env ls stateRef)

  Lua.setglobal (Lua.Name "input")
