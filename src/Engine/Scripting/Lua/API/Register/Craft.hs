module Engine.Scripting.Lua.API.Register.Craft
  ( registerCraftAPI
  ) where

import Engine.Scripting.Lua.CallStats (LuaCallStats)
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.Craft
import Engine.Scripting.Lua.API.Power
import Engine.Scripting.Lua.API.Repair
import Engine.Core.State (EngineEnv, unitManagerRef)
import Engine.Core.Capability.ContentRegistries
  (toContentRegistriesCapability)
import Engine.Core.Capability.ContentRegistriesView
  (toContentRegistriesViewCapability)
import qualified HsLua as Lua

-- | Populate and install the @craft@, @power@, and @repair@ global
--   tables.
registerCraftAPI ∷ LuaCallStats → EngineEnv → Lua.LuaE Lua.Exception ()
registerCraftAPI callStats env = do
  -- craft.get/getNames and the whole repair surface read the recipe
  -- (and item) catalogue through the `content-registries` capability
  -- (#890) rather than the full EngineEnv. They take DIFFERENT records
  -- (#1896): `Engine.Scripting.Lua.API.Craft.Recipe` also owns
  -- `engine.loadRecipeYaml`'s write, so it keeps the raw writer
  -- interface, while `Engine.Scripting.Lua.API.Repair` only reads and
  -- takes the read-only view.
  let regs     = toContentRegistriesCapability env
      regsView = toContentRegistriesViewCapability env
  -- Craft global — the crafting recipe catalogue (#325), loaded from
  -- data/recipes/*.yaml via engine.loadRecipeYaml. get/getNames are
  -- read-only queries; execute runs one craft against a unit's
  -- inventory (verify + consume inputs/fuel, produce outputs);
  -- executeAt (#326) additionally requires a Built work station
  -- offering the recipe's station kind with the unit adjacent.
  -- The bill verbs (#329) manage a per-station standing-order queue
  -- (Craft.Bills) that lives PER WORLD PAGE, and #2325 splits them into
  -- two families by which page each resolves. See
  -- Engine.Scripting.Lua.API.Craft.Bill's module haddock for why.
  --
  --   * ACTOR-QUALIFIED, resolved on the ACTING UNIT's own page:
  --       getBill(uid, billId)
  --       claimBill(billId, uid, timeout)
  --       releaseBill(uid, billId)
  --       setBillWorking(uid, billId, working)
  --       addBillProgress(uid, billId, delta)
  --       completeBillCycle(uid, billId)
  --     — the craft AI's job lifecycle (scripts/unit_ai_craft.lua),
  --     which holds one numeric bill id across many ticks.
  --     craft.executeAt's optional billId is validated the same way.
  --   * ACTIVE-page, resolved on the page the player is looking at:
  --       addBill / cancelBill / setBillPaused / reorderBill / getBills
  --     — the #330 station panel's queue surface and its pause +
  --     manual-reorder controls, plus the discovery listing the AI's own
  --     scan starts from.
  Lua.newtable
  registerLuaFunction callStats "craft" "get"      (craftGetFn regs)
  registerLuaFunction callStats "craft" "getNames" (craftGetNamesFn regs)
  registerLuaFunction callStats "craft" "execute"  (craftExecuteFn env)
  registerLuaFunction callStats "craft" "executeAt" (craftExecuteAtFn env)
  registerLuaFunction callStats "craft" "addBill"           (craftAddBillFn env)
  registerLuaFunction callStats "craft" "cancelBill"        (craftCancelBillFn env)
  registerLuaFunction callStats "craft" "getBill"           (craftGetBillFn env)
  registerLuaFunction callStats "craft" "getBills"          (craftGetBillsFn env)
  registerLuaFunction callStats "craft" "claimBill"         (craftClaimBillFn env)
  registerLuaFunction callStats "craft" "releaseBill"       (craftReleaseBillFn env)
  registerLuaFunction callStats "craft" "addBillProgress"   (craftAddBillProgressFn env)
  registerLuaFunction callStats "craft" "completeBillCycle" (craftCompleteBillCycleFn env)
  registerLuaFunction callStats "craft" "setBillPaused"     (craftSetBillPausedFn env)
  registerLuaFunction callStats "craft" "setBillWorking"    (craftSetBillWorkingFn env)
  registerLuaFunction callStats "craft" "reorderBill"       (craftReorderBillFn env)
  Lua.setglobal (Lua.Name "craft")

  -- Power global (#358) — the placeable power-node registry. placeNode
  -- pops an item whose building def declares a power node (#1148) out
  -- of a unit's inventory and turns it into a persistent power node;
  -- getNode /
  -- getNodeForBuilding / listNodes are read-only queries reporting each
  -- node's role + parameters. listNetworks / getNetworkForNode (#360)
  -- report the live wire-connectivity view: which nodes share a network
  -- and its current generation/drain/stored/capacity/powered status.
  Lua.newtable
  registerLuaFunction callStats "power" "isPlaceable"       (powerIsPlaceableFn env)
  registerLuaFunction callStats "power" "placeNode"         (powerPlaceNodeFn env)
  registerLuaFunction callStats "power" "getNode"           (powerGetNodeFn env)
  registerLuaFunction callStats "power" "getNodeForBuilding" (powerGetNodeForBuildingFn env)
  registerLuaFunction callStats "power" "listNodes"         (powerListNodesFn env)
  registerLuaFunction callStats "power" "listNetworks"       (powerListNetworksFn env)
  registerLuaFunction callStats "power" "getNetworkForNode"  (powerGetNetworkForNodeFn env)
  registerLuaFunction callStats "power" "isBuildingPowered"  (powerIsBuildingPoweredFn env)
  registerLuaFunction callStats "power" "isStationPoweredForRecipe"
                                            (powerIsStationPoweredForRecipeFn env)
  Lua.setglobal (Lua.Name "power")

  -- Repair global (#301) — the policy layer on top of unit.repairItem
  -- (#300): repair flows are recipe entries tagged with a repair axis
  -- (data/recipes/repair.yaml), gated on the same Built/adjacent
  -- station rules as craft.executeAt. get/getNames are read-only,
  -- restricted to repair-tagged recipes; repairAt runs one repair
  -- against a targeted item instance.
  Lua.newtable
  registerLuaFunction callStats "repair" "get"      (repairGetFn regsView)
  registerLuaFunction callStats "repair" "getNames" (repairGetNamesFn regsView)
  registerLuaFunction callStats "repair" "repairAt"
                     (repairAtFn regsView (unitManagerRef env) env)
  Lua.setglobal (Lua.Name "repair")
