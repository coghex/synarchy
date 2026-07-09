module Engine.Scripting.Lua.API.Register.Camera
  ( registerCameraAPI
  ) where

import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.Camera
import Engine.Scripting.Lua.API.Combat
import Engine.Core.State (EngineEnv)
import qualified HsLua as Lua

-- | Populate and install the @camera@, @combat@, @injury@, and
--   @thought@ global tables.
registerCameraAPI ∷ EngineEnv → Lua.LuaE Lua.Exception ()
registerCameraAPI env = do
  Lua.newtable

  registerLuaFunction "goToTile" (cameraGotoTileFn env)
  registerLuaFunction "move" (cameraMoveFn env)
  registerLuaFunction "setPosition" (cameraSetPositionFn env)
  registerLuaFunction "getPosition" (cameraGetPositionFn env)
  registerLuaFunction "setZoom" (cameraSetZoomFn env)
  registerLuaFunction "getZoom" (cameraGetZoomFn env)
  registerLuaFunction "getZoomFadeStart" cameraGetZoomFadeStartFn
  registerLuaFunction "getZoomFadeEnd" cameraGetZoomFadeEndFn
  registerLuaFunction "setZoomVelocity" (cameraSetZoomVelocityFn env)
  registerLuaFunction "getZoomVelocity" (cameraGetZoomVelocityFn env)
  registerLuaFunction "applyScrollZoom" (cameraApplyScrollZoomFn env)
  registerLuaFunction "setZSlice" (cameraSetZSliceFn env)
  registerLuaFunction "getZSlice" (cameraGetZSliceFn env)
  registerLuaFunction "rotateCW" (cameraRotateCWFn env)
  registerLuaFunction "rotateCCW" (cameraRotateCCWFn env)
  registerLuaFunction "getFacing" (cameraGetFacingFn env)
  registerLuaFunction "getZTracking" (cameraGetZTrackingFn env)
  registerLuaFunction "setZTracking" (cameraSetZTrackingFn env)
  Lua.setglobal (Lua.Name "camera")

  -- Combat: queue attack commands + drain combat-thread events for
  -- the combat-log UI. Skeleton phase — `combat.attack` enqueues but
  -- the thread doesn't resolve yet; `combat.drainEvents` always
  -- returns empty.
  Lua.newtable
  registerLuaFunction "attack"      (combatAttackFn env)
  registerLuaFunction "drainEvents" (combatDrainEventsFn env)
  registerLuaFunction "emitDeath"   (combatEmitDeathFn env)
  Lua.setglobal (Lua.Name "combat")

  -- Injury: NON-combat wound stream (falls / hazards / wound-caused
  -- deaths) for the injury-log UI. Mirrors the combat event stream.
  Lua.newtable
  registerLuaFunction "emit"        (injuryEmitFn env)
  registerLuaFunction "drainEvents" (injuryDrainEventsFn env)
  Lua.setglobal (Lua.Name "injury")

  -- Thought: per-unit thought stream (#351) for the unit-log UI's
  -- Thought tab. Purely Lua-produced (scripts/thoughts.lua) — mirrors
  -- the injury event stream's shape and drain pattern.
  Lua.newtable
  registerLuaFunction "emit"        (thoughtEmitFn env)
  registerLuaFunction "drainEvents" (thoughtDrainEventsFn env)
  Lua.setglobal (Lua.Name "thought")
