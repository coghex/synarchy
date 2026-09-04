module Engine.Scripting.Lua.API.Register.Craft
  ( registerCraftAPI
  ) where

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
registerCraftAPI ∷ EngineEnv → Lua.LuaE Lua.Exception ()
registerCraftAPI env = do
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
  registerLuaFunction "get"      (craftGetFn regs)
  registerLuaFunction "getNames" (craftGetNamesFn regs)
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
  -- pops an item whose building def declares a power node (#1148) out
  -- of a unit's inventory and turns it into a persistent power node;
  -- getNode /
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
  registerLuaFunction "get"      (repairGetFn regsView)
  registerLuaFunction "getNames" (repairGetNamesFn regsView)
  registerLuaFunction "repairAt"
                     (repairAtFn regsView (unitManagerRef env) env)
  Lua.setglobal (Lua.Name "repair")
