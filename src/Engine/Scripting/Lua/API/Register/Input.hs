module Engine.Scripting.Lua.API.Register.Input
  ( registerInputAPI
  ) where

import UPrelude
import Engine.Core.State (EngineEnv)
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.InputInject
  ( inputMoveMouseFn, inputClickFn, inputMouseDownFn, inputMouseUpFn
  , inputScrollFn, inputKeyFn, inputKeyDownFn, inputKeyUpFn, inputTypeFn )
import qualified HsLua as Lua

-- | Populate and install the @input@ global table (#644): synthetic
--   input injection for the playtest harness. Read-side input queries
--   (isKeyDown, getMousePosition, …) stay on the @engine@ table.
registerInputAPI ∷ EngineEnv → Lua.LuaE Lua.Exception ()
registerInputAPI env = do
  Lua.newtable

  registerLuaFunction "moveMouse" (inputMoveMouseFn env)
  registerLuaFunction "click"     (inputClickFn env)
  registerLuaFunction "mouseDown" (inputMouseDownFn env)
  registerLuaFunction "mouseUp"   (inputMouseUpFn env)
  registerLuaFunction "scroll"    (inputScrollFn env)
  registerLuaFunction "key"       (inputKeyFn env)
  registerLuaFunction "keyDown"   (inputKeyDownFn env)
  registerLuaFunction "keyUp"     (inputKeyUpFn env)
  registerLuaFunction "type"      (inputTypeFn env)

  Lua.setglobal (Lua.Name "input")
