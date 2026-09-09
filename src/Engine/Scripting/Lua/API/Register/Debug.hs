module Engine.Scripting.Lua.API.Register.Debug
  ( registerDebugAPI
  ) where

import UPrelude
import Engine.Core.State (EngineEnv)
import Engine.Scripting.Lua.CallStats (LuaCallStats)
import Engine.Scripting.Lua.API.CallStats (getLuaCallStatsFn, resetLuaCallStatsFn)
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.Input (getWindowPosFn)
import Engine.Scripting.Lua.API.Screenshot (captureScreenshotFn)
import Engine.Scripting.Lua.API.ActionOutcome
    (debugRecordOutcomeFn, debugDrainActionOutcomesFn)
import Engine.Scripting.Lua.API.LoadGate
    (armLoadStageGateFn, releaseLoadStageGateFn, getLoadStageGateFn)
import Engine.Scripting.Lua.API.QueueStats (getQueueStatsFn)
import Engine.Scripting.Lua.API.SceneStats (getSceneStatsFn)
import qualified HsLua as Lua

-- | Populate the @debug@ global with engine debug verbs
--   (#643, #646, #907, #1181).
--   openlibs already installed Lua's stock @debug@ stdlib table, so we
--   add fields to it rather than replacing it — the stock functions
--   (traceback etc.) stay available. Falls back to creating the table
--   if a future init path ever skips the stdlib.
registerDebugAPI ∷ LuaCallStats → EngineEnv → Lua.LuaE Lua.Exception ()
registerDebugAPI callStats env = do
  _ ← Lua.getglobal (Lua.Name "debug")
  isTbl ← Lua.istable (-1)
  unless isTbl $ do
    Lua.pop 1
    Lua.newtable

  registerLuaFunction callStats "debug" "captureScreenshot" (captureScreenshotFn env)
  registerLuaFunction callStats "debug" "getWindowPos" (getWindowPosFn env)
  registerLuaFunction callStats "debug" "recordOutcome" (debugRecordOutcomeFn env)
  registerLuaFunction callStats "debug" "drainActionOutcomes" (debugDrainActionOutcomesFn env)
  -- #1181: the load-staging gate. Test-only coordination, so it lives
  -- here rather than on a player-facing table.
  registerLuaFunction callStats "debug" "armLoadStageGate" (armLoadStageGateFn env)
  registerLuaFunction callStats "debug" "releaseLoadStageGate" (releaseLoadStageGateFn env)
  registerLuaFunction callStats "debug" "getLoadStageGate" (getLoadStageGateFn env)
  -- #1910: inter-thread queue telemetry. Read-only, world-free, and
  -- answerable on a bare headless boot, so a backlog can be observed
  -- from the moment the console comes up.
  registerLuaFunction callStats "debug" "getQueueStats" (getQueueStatsFn env)
  -- #1921: World.Render scene-assembly telemetry. Read-only, and
  -- published by the world thread, which runs headless — so it answers
  -- in --headless and --offscreen alike.
  registerLuaFunction callStats "debug" "getSceneStats" (getSceneStatsFn env)

  registerLuaFunction callStats "debug" "getLuaCallStats" (getLuaCallStatsFn callStats)
  registerLuaFunction callStats "debug" "resetLuaCallStats" (resetLuaCallStatsFn callStats)

  if isTbl
    then Lua.pop 1
    else Lua.setglobal (Lua.Name "debug")
