module Engine.Scripting.Lua.API.Register.Building
  ( registerBuildingAPI
  ) where

import Engine.Scripting.Lua.CallStats (LuaCallStats)
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.Buildings
import Engine.Core.State (EngineEnv)
import qualified HsLua as Lua

-- | Populate and install the @building@ global table. Mirrors @unit@
--   in shape.
registerBuildingAPI ∷ LuaCallStats → EngineEnv → Lua.LuaE Lua.Exception ()
registerBuildingAPI callStats env = do
  Lua.newtable
  registerLuaFunction callStats "building" "spawn"               (buildingSpawnFn env)
  registerLuaFunction callStats "building" "destroy"             (buildingDestroyFn env)
  registerLuaFunction callStats "building" "canPlaceAt"          (buildingCanPlaceAtFn env)
  registerLuaFunction callStats "building" "remoteCheck"         (buildingRemoteCheckFn env)
  registerLuaFunction callStats "building" "setGhost"            (buildingSetGhostFn env)
  registerLuaFunction callStats "building" "clearGhost"          (buildingClearGhostFn env)
  registerLuaFunction callStats "building" "getStartingBuildings" (buildingGetStartingBuildingsFn env)
  registerLuaFunction callStats "building" "getInfo"             (buildingGetInfoFn env)
  registerLuaFunction callStats "building" "getActivity"         (buildingGetActivityFn env)
  registerLuaFunction callStats "building" "list"                (buildingListFn env)
  registerLuaFunction callStats "building" "getActiveIds"        (buildingGetActiveIdsFn env)
  registerLuaFunction callStats "building" "existsWithDef"       (buildingExistsWithDefFn env)
  registerLuaFunction callStats "building" "listDefs"            (buildingListDefsFn env)
  registerLuaFunction callStats "building" "hitTestAt"           (buildingHitTestAtFn env)
  registerLuaFunction callStats "building" "select"              (buildingSelectFn env)
  registerLuaFunction callStats "building" "deselect"            (buildingDeselectFn env)
  registerLuaFunction callStats "building" "getSelected"         (buildingGetSelectedFn env)
  registerLuaFunction callStats "building" "setSpawnRemaining"   (buildingSetSpawnRemainingFn env)
  registerLuaFunction callStats "building" "getSpawnRemaining"   (buildingGetSpawnRemainingFn env)
  registerLuaFunction callStats "building" "consumeSpawn"        (buildingConsumeSpawnFn env)
  registerLuaFunction callStats "building" "getBuildProgress"    (buildingGetBuildProgressFn env)
  registerLuaFunction callStats "building" "getBuildRequired"    (buildingGetBuildRequiredFn env)
  registerLuaFunction callStats "building" "addBuildProgress"    (buildingAddBuildProgressFn env)
  registerLuaFunction callStats "building" "getMaterialNeed"     (buildingGetMaterialNeedFn env)
  registerLuaFunction callStats "building" "getMaterialDelivered" (buildingGetMaterialDeliveredFn env)
  registerLuaFunction callStats "building" "areMaterialsSatisfied" (buildingAreMaterialsSatisfiedFn env)
  registerLuaFunction callStats "building" "getStorage"          (buildingGetStorageFn env)
  registerLuaFunction callStats "building" "getStorageCapacity"  (buildingGetStorageCapacityFn env)
  registerLuaFunction callStats "building" "getStorageWeight"    (buildingGetStorageWeightFn env)
  registerLuaFunction callStats "building" "getContainerKnowledge"
                                            (buildingGetContainerKnowledgeFn env)
  registerLuaFunction callStats "building" "getRememberedItemContents"
                                            (buildingGetRememberedItemContentsFn env)
  registerLuaFunction callStats "building" "refreshContainerKnowledge"
                                            (buildingRefreshContainerKnowledgeFn env)
  registerLuaFunction callStats "building" "getOperations"       (buildingGetOperationsFn env)
  registerLuaFunction callStats "building" "findStation"         (buildingFindStationFn env)
  Lua.setglobal (Lua.Name "building")
