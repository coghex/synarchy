-- | Populate and install the @faction@ global table (#912).
--
--   Unlike every sibling registrar this takes no 'EngineEnv': the whole
--   table is pure over faction tags, which is what lets the headless
--   suite install it into a bare Lua state and assert the model's
--   behavior from Lua itself.
module Engine.Scripting.Lua.API.Register.Faction
  ( registerFactionAPI
  ) where

import Engine.Scripting.Lua.CallStats (LuaCallStats)
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.Faction
import qualified HsLua as Lua

registerFactionAPI ∷ LuaCallStats → Lua.LuaE Lua.Exception ()
registerFactionAPI callStats = do
  Lua.newtable

  registerLuaFunction callStats "faction" "relation"              factionRelationFn
  registerLuaFunction callStats "faction" "areAllies"             factionAreAlliesFn
  registerLuaFunction callStats "faction" "canAttack"             factionCanAttackFn
  registerLuaFunction callStats "faction" "isPlayerOwned"         factionIsPlayerOwnedFn
  registerLuaFunction callStats "faction" "isPlayerCommandable"   factionIsPlayerCommandableFn
  registerLuaFunction callStats "faction" "hasUnrestrictedCombat" factionHasUnrestrictedCombatFn

  Lua.setglobal (Lua.Name "faction")
