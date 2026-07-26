-- | Populate and install the @faction@ global table (#912).
--
--   Unlike every sibling registrar this takes no 'EngineEnv': the whole
--   table is pure over faction tags, which is what lets the headless
--   suite install it into a bare Lua state and assert the model's
--   behavior from Lua itself.
module Engine.Scripting.Lua.API.Register.Faction
  ( registerFactionAPI
  ) where

import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.Faction
import qualified HsLua as Lua

registerFactionAPI ∷ Lua.LuaE Lua.Exception ()
registerFactionAPI = do
  Lua.newtable

  registerLuaFunction "relation"              factionRelationFn
  registerLuaFunction "areAllies"             factionAreAlliesFn
  registerLuaFunction "canAttack"             factionCanAttackFn
  registerLuaFunction "isPlayerOwned"         factionIsPlayerOwnedFn
  registerLuaFunction "isPlayerCommandable"   factionIsPlayerCommandableFn
  registerLuaFunction "hasUnrestrictedCombat" factionHasUnrestrictedCombatFn

  Lua.setglobal (Lua.Name "faction")
