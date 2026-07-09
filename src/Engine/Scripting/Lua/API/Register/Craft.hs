module Engine.Scripting.Lua.API.Register.Craft
  ( registerCraftAPI
  ) where

import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.Craft
import Engine.Scripting.Lua.API.Power
import Engine.Scripting.Lua.API.Repair
import Engine.Core.State (EngineEnv)
import qualified HsLua as Lua

-- | Populate and install the @craft@, @power@, and @repair@ global
--   tables.
registerCraftAPI ∷ EngineEnv → Lua.LuaE Lua.Exception ()
registerCraftAPI env = do
  -- Craft global — the crafting recipe catalogue (#325), loaded from
  -- data/recipes/*.yaml via engine.loadRecipeYaml. get/getNames are
  -- read-only queries; execute runs one craft against a unit's
  -- inventory (verify + consume inputs/fuel, produce outputs);
  -- executeAt (#326) additionally requires a Built work station
  -- offering the recipe's station kind with the unit adjacent.
  -- The bill verbs (#329) manage the active world's per-station
  -- standing-order queue (Craft.Bills): addBill / cancelBill /
  -- getBill / getBills are the queue surface (UI #330);
  -- claimBill / releaseBill / addBillProgress / completeBillCycle
  -- are the craft AI's job lifecycle. setBillPaused / reorderBill are
  -- the #330 station panel's pause + manual-reorder controls.
  Lua.newtable
  registerLuaFunction "get"      (craftGetFn env)
  registerLuaFunction "getNames" (craftGetNamesFn env)
  registerLuaFunction "execute"  (craftExecuteFn env)
  registerLuaFunction "executeAt" (craftExecuteAtFn env)
  registerLuaFunction "addBill"           (craftAddBillFn env)
  registerLuaFunction "cancelBill"        (craftCancelBillFn env)
  registerLuaFunction "getBill"           (craftGetBillFn env)
  registerLuaFunction "getBills"          (craftGetBillsFn env)
  registerLuaFunction "claimBill"         (craftClaimBillFn env)
  registerLuaFunction "releaseBill"       (craftReleaseBillFn env)
  registerLuaFunction "addBillProgress"   (craftAddBillProgressFn env)
  registerLuaFunction "completeBillCycle" (craftCompleteBillCycleFn env)
  registerLuaFunction "setBillPaused"     (craftSetBillPausedFn env)
  registerLuaFunction "setBillWorking"    (craftSetBillWorkingFn env)
  registerLuaFunction "reorderBill"       (craftReorderBillFn env)
  Lua.setglobal (Lua.Name "craft")

  -- Power global (#358) — the placeable power-node registry. placeNode
  -- pops a solar_panel/high_voltage_battery item out of a unit's
  -- inventory and turns it into a persistent power node; getNode /
  -- getNodeForBuilding / listNodes are read-only queries reporting each
  -- node's role + parameters. listNetworks / getNetworkForNode (#360)
  -- report the live wire-connectivity view: which nodes share a network
  -- and its current generation/drain/stored/capacity/powered status.
  Lua.newtable
  registerLuaFunction "isPlaceable"       (powerIsPlaceableFn env)
  registerLuaFunction "placeNode"         (powerPlaceNodeFn env)
  registerLuaFunction "getNode"           (powerGetNodeFn env)
  registerLuaFunction "getNodeForBuilding" (powerGetNodeForBuildingFn env)
  registerLuaFunction "listNodes"         (powerListNodesFn env)
  registerLuaFunction "listNetworks"       (powerListNetworksFn env)
  registerLuaFunction "getNetworkForNode"  (powerGetNetworkForNodeFn env)
  registerLuaFunction "isBuildingPowered"  (powerIsBuildingPoweredFn env)
  registerLuaFunction "isStationPoweredForRecipe"
                                            (powerIsStationPoweredForRecipeFn env)
  Lua.setglobal (Lua.Name "power")

  -- Repair global (#301) — the policy layer on top of unit.repairItem
  -- (#300): repair flows are recipe entries tagged with a repair axis
  -- (data/recipes/repair.yaml), gated on the same Built/adjacent
  -- station rules as craft.executeAt. get/getNames are read-only,
  -- restricted to repair-tagged recipes; repairAt runs one repair
  -- against a targeted item instance.
  Lua.newtable
  registerLuaFunction "get"      (repairGetFn env)
  registerLuaFunction "getNames" (repairGetNamesFn env)
  registerLuaFunction "repairAt" (repairAtFn env)
  Lua.setglobal (Lua.Name "repair")
