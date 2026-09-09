module Engine.Scripting.Lua.API.Register.Equipment
  ( registerEquipmentAPI
  ) where

import Engine.Scripting.Lua.CallStats (LuaCallStats)
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.Equipment
import Engine.Scripting.Lua.API.Substance
import Engine.Scripting.Lua.API.Infection
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.ContentRegistries
  (toContentRegistriesCapability)
import qualified HsLua as Lua

-- | Populate and install the @equipment@, @substance@, and
--   @infection@ global tables.
registerEquipmentAPI ∷ LuaCallStats → EngineEnv → Lua.LuaE Lua.Exception ()
registerEquipmentAPI callStats env = do
  -- The class / substance / infection catalogue queries read through the
  -- `content-registries` capability (#890), not the full EngineEnv.
  let regs = toContentRegistriesCapability env
  -- Equipment global.
  -- Read: getClass / getClassNames / getLoadout.
  -- Write: equip / unequip (with kind validation against slot's accepted kind).
  Lua.newtable
  registerLuaFunction callStats "equipment" "getClass"      (equipmentGetClassFn regs)
  registerLuaFunction callStats "equipment" "getClassNames" (equipmentGetClassNamesFn regs)
  registerLuaFunction callStats "equipment" "getLoadout"    (equipmentGetLoadoutFn env)
  registerLuaFunction callStats "equipment" "equip"           (equipmentEquipFn env)
  registerLuaFunction callStats "equipment" "unequip"         (equipmentUnequipFn env)
  registerLuaFunction callStats "equipment" "equipAccessory"  (equipmentEquipAccessoryFn env)
  registerLuaFunction callStats "equipment" "unequipAccessory"(equipmentUnequipAccessoryFn env)
  registerLuaFunction callStats "equipment" "getAccessories"  (equipmentGetAccessoriesFn env)
  Lua.setglobal (Lua.Name "equipment")

  -- Substance global — read-only access to material physical
  -- properties (density, tensile, fracture toughness, …). Loaded
  -- from data/substances/*.yaml via engine.loadSubstanceYaml.
  Lua.newtable
  registerLuaFunction callStats "substance" "get"      (substanceGetFn regs)
  registerLuaFunction callStats "substance" "getNames" (substanceGetNamesFn regs)
  Lua.setglobal (Lua.Name "substance")

  -- Infection global — read-only access to the infection catalogue
  -- (staph, gas gangrene, …) loaded from data/infections/*.yaml via
  -- engine.loadInfectionYaml.
  Lua.newtable
  registerLuaFunction callStats "infection" "get"      (infectionGetFn regs)
  registerLuaFunction callStats "infection" "getNames" (infectionGetNamesFn regs)
  Lua.setglobal (Lua.Name "infection")
