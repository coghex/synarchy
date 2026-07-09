module Engine.Scripting.Lua.API.Register.Debug
  ( registerDebugAPI
  ) where

import UPrelude
import Engine.Core.State (EngineEnv)
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.Screenshot (captureScreenshotFn)
import qualified HsLua as Lua

-- | Populate the @debug@ global with engine debug verbs (#643).
--   openlibs already installed Lua's stock @debug@ stdlib table, so we
--   add fields to it rather than replacing it — the stock functions
--   (traceback etc.) stay available. Falls back to creating the table
--   if a future init path ever skips the stdlib.
registerDebugAPI ∷ EngineEnv → Lua.LuaE Lua.Exception ()
registerDebugAPI env = do
  _ ← Lua.getglobal (Lua.Name "debug")
  isTbl ← Lua.istable (-1)
  unless isTbl $ do
    Lua.pop 1
    Lua.newtable

  registerLuaFunction "captureScreenshot" (captureScreenshotFn env)

  if isTbl
    then Lua.pop 1
    else Lua.setglobal (Lua.Name "debug")
