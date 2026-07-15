module Engine.Scripting.Lua.API.Register.Building
  ( registerBuildingAPI
  ) where

import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.Buildings
import Engine.Core.State (EngineEnv)
import qualified HsLua as Lua

-- | Populate and install the @building@ global table. Mirrors @unit@
--   in shape.
registerBuildingAPI ∷ EngineEnv → Lua.LuaE Lua.Exception ()
registerBuildingAPI env = do
  Lua.newtable
  registerLuaFunction "spawn"               (buildingSpawnFn env)
  registerLuaFunction "destroy"             (buildingDestroyFn env)
  registerLuaFunction "canPlaceAt"          (buildingCanPlaceAtFn env)
  registerLuaFunction "remoteCheck"         (buildingRemoteCheckFn env)
  registerLuaFunction "setGhost"            (buildingSetGhostFn env)
  registerLuaFunction "clearGhost"          (buildingClearGhostFn env)
  registerLuaFunction "getStartingBuildings" (buildingGetStartingBuildingsFn env)
  registerLuaFunction "getInfo"             (buildingGetInfoFn env)
  registerLuaFunction "getActivity"         (buildingGetActivityFn env)
  registerLuaFunction "list"                (buildingListFn env)
  registerLuaFunction "getActiveIds"        (buildingGetActiveIdsFn env)
  registerLuaFunction "listDefs"            (buildingListDefsFn env)
  registerLuaFunction "hitTestAt"           (buildingHitTestAtFn env)
  registerLuaFunction "select"              (buildingSelectFn env)
  registerLuaFunction "deselect"            (buildingDeselectFn env)
  registerLuaFunction "getSelected"         (buildingGetSelectedFn env)
  registerLuaFunction "setSpawnRemaining"   (buildingSetSpawnRemainingFn env)
  registerLuaFunction "getSpawnRemaining"   (buildingGetSpawnRemainingFn env)
  registerLuaFunction "consumeSpawn"        (buildingConsumeSpawnFn env)
  registerLuaFunction "getBuildProgress"    (buildingGetBuildProgressFn env)
  registerLuaFunction "getBuildRequired"    (buildingGetBuildRequiredFn env)
  registerLuaFunction "addBuildProgress"    (buildingAddBuildProgressFn env)
  registerLuaFunction "getMaterialNeed"     (buildingGetMaterialNeedFn env)
  registerLuaFunction "getMaterialDelivered" (buildingGetMaterialDeliveredFn env)
  registerLuaFunction "areMaterialsSatisfied" (buildingAreMaterialsSatisfiedFn env)
  registerLuaFunction "getStorage"          (buildingGetStorageFn env)
  registerLuaFunction "getStorageCapacity"  (buildingGetStorageCapacityFn env)
  registerLuaFunction "getStorageWeight"    (buildingGetStorageWeightFn env)
  registerLuaFunction "getOperations"       (buildingGetOperationsFn env)
  registerLuaFunction "findStation"         (buildingFindStationFn env)
  Lua.setglobal (Lua.Name "building")
