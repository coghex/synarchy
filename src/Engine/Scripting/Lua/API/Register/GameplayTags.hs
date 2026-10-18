-- | Populate and install the @gameplayTags@ global table (#2706, SCN-06
--   of epic #2698): general gameplay tags for scripts, a namespace of
--   its own and deliberately separate from @faction@, whose tags are
--   policy (#2551).
--
--   'registerGameplayTagAPIWith' takes the test seams, so the Hspec group
--   can install the PRODUCTION verbs with a counting marshalling seam
--   (observing that @count@ and @exists@ never push a target) and with a
--   page replacement between an edit's resolution and its application.
module Engine.Scripting.Lua.API.Register.GameplayTags
  ( registerGameplayTagAPI
  , registerGameplayTagAPIWith
  ) where

import UPrelude
import Engine.Core.Capability.Building (BuildingCapability(..), toBuildingCapability)
import Engine.Core.Capability.SaveLoad (SaveLoadCapability(..), toSaveLoadCapability)
import Engine.Core.Capability.UnitCombat (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Engine.Core.State (EngineEnv)
import Engine.Scripting.Lua.CallStats (LuaCallStats)
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.GameplayTags
import qualified HsLua as Lua

registerGameplayTagAPI ∷ LuaCallStats → EngineEnv → Lua.LuaE Lua.Exception ()
registerGameplayTagAPI = registerGameplayTagAPIWith productionTagMarshal (pure ())

-- | The production verbs with their two test seams: the list marshalling
--   and the window between an edit's resolution and its application.
registerGameplayTagAPIWith ∷ TagMarshal → IO () → LuaCallStats → EngineEnv
                           → Lua.LuaE Lua.Exception ()
registerGameplayTagAPIWith marshal afterResolve callStats env = do
  let api = TagApi
        { taUnits        = ucUnitManagerRef (toUnitCombatCapability env)
        , taBuildings    = bcBuildingManagerRef (toBuildingCapability env)
        , taWorld        = toWorldSimCapability env
        , taSaveBarrier  = slSaveBarrierRef (toSaveLoadCapability env)
        , taAfterResolve = afterResolve }
  Lua.newtable

  registerLuaFunction callStats "gameplayTags" "add"     (tagAddFn api)
  registerLuaFunction callStats "gameplayTags" "remove"  (tagRemoveFn api)
  registerLuaFunction callStats "gameplayTags" "replace" (tagReplaceFn api)
  registerLuaFunction callStats "gameplayTags" "clear"   (tagClearFn api)
  registerLuaFunction callStats "gameplayTags" "tagsOf"  (tagTagsOfFn api)
  registerLuaFunction callStats "gameplayTags" "has"     (tagHasFn api)
  registerLuaFunction callStats "gameplayTags" "query"   (tagQueryFn marshal api)
  registerLuaFunction callStats "gameplayTags" "count"   (tagCountFn marshal api)
  registerLuaFunction callStats "gameplayTags" "exists"  (tagExistsFn marshal api)

  Lua.setglobal (Lua.Name "gameplayTags")
