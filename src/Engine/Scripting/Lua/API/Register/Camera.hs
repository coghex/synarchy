module Engine.Scripting.Lua.API.Register.Camera
  ( registerCameraAPI
  ) where

import Engine.Scripting.Lua.CallStats (LuaCallStats)
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.Camera
import Engine.Scripting.Lua.API.Combat
import Engine.Core.State (EngineEnv)
import qualified HsLua as Lua

-- | Populate and install the @camera@, @combat@, @injury@, and
--   @thought@ global tables.
registerCameraAPI ∷ LuaCallStats → EngineEnv → Lua.LuaE Lua.Exception ()
registerCameraAPI callStats env = do
  Lua.newtable

  registerLuaFunction callStats "camera" "goToTile" (cameraGotoTileFn env)
  registerLuaFunction callStats "camera" "move" (cameraMoveFn env)
  registerLuaFunction callStats "camera" "setPosition" (cameraSetPositionFn env)
  registerLuaFunction callStats "camera" "getPosition" (cameraGetPositionFn env)
  registerLuaFunction callStats "camera" "setZoom" (cameraSetZoomFn env)
  registerLuaFunction callStats "camera" "getZoom" (cameraGetZoomFn env)
  registerLuaFunction callStats "camera" "getZoomFadeStart" cameraGetZoomFadeStartFn
  registerLuaFunction callStats "camera" "getZoomFadeEnd" cameraGetZoomFadeEndFn
  registerLuaFunction callStats "camera" "setZoomVelocity" (cameraSetZoomVelocityFn env)
  registerLuaFunction callStats "camera" "getZoomVelocity" (cameraGetZoomVelocityFn env)
  registerLuaFunction callStats "camera" "applyScrollZoom" (cameraApplyScrollZoomFn env)
  registerLuaFunction callStats "camera" "setZSlice" (cameraSetZSliceFn env)
  registerLuaFunction callStats "camera" "getZSlice" (cameraGetZSliceFn env)
  registerLuaFunction callStats "camera" "rotateCW" (cameraRotateCWFn env)
  registerLuaFunction callStats "camera" "rotateCCW" (cameraRotateCCWFn env)
  registerLuaFunction callStats "camera" "getFacing" (cameraGetFacingFn env)
  registerLuaFunction callStats "camera" "getZTracking" (cameraGetZTrackingFn env)
  registerLuaFunction callStats "camera" "setZTracking" (cameraSetZTrackingFn env)
  Lua.setglobal (Lua.Name "camera")

  -- Combat: queue attack commands + drain combat-thread events for
  -- the combat-log UI. Skeleton phase — `combat.attack` enqueues but
  -- the thread doesn't resolve yet; `combat.drainEvents` always
  -- returns empty.
  Lua.newtable
  registerLuaFunction callStats "combat" "attack"      (combatAttackFn env)
  registerLuaFunction callStats "combat" "drainEvents" (combatDrainEventsFn env)
  registerLuaFunction callStats "combat" "emitDeath"   (combatEmitDeathFn env)
  Lua.setglobal (Lua.Name "combat")

  -- Injury: NON-combat wound stream (falls / hazards / wound-caused
  -- deaths) for the injury-log UI. Mirrors the combat event stream.
  Lua.newtable
  registerLuaFunction callStats "injury" "emit"        (injuryEmitFn env)
  registerLuaFunction callStats "injury" "drainEvents" (injuryDrainEventsFn env)
  Lua.setglobal (Lua.Name "injury")

  -- Thought: per-unit thought stream (#351) for the unit-log UI's
  -- Thought tab. Purely Lua-produced (scripts/thoughts.lua) — mirrors
  -- the injury event stream's shape and drain pattern.
  Lua.newtable
  registerLuaFunction callStats "thought" "emit"        (thoughtEmitFn env)
  registerLuaFunction callStats "thought" "drainEvents" (thoughtDrainEventsFn env)
  Lua.setglobal (Lua.Name "thought")
