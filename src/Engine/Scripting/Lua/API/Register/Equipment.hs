module Engine.Scripting.Lua.API.Register.Equipment
  ( registerEquipmentAPI
  ) where

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
registerEquipmentAPI ∷ EngineEnv → Lua.LuaE Lua.Exception ()
registerEquipmentAPI env = do
  -- The class / substance / infection catalogue queries read through the
  -- `content-registries` capability (#890), not the full EngineEnv.
  let regs = toContentRegistriesCapability env
  -- Equipment global.
  -- Read: getClass / getClassNames / getLoadout.
  -- Write: equip / unequip (with kind validation against slot's accepted kind).
  Lua.newtable
  registerLuaFunction "getClass"      (equipmentGetClassFn regs)
  registerLuaFunction "getClassNames" (equipmentGetClassNamesFn regs)
  registerLuaFunction "getLoadout"    (equipmentGetLoadoutFn env)
  registerLuaFunction "equip"           (equipmentEquipFn env)
  registerLuaFunction "unequip"         (equipmentUnequipFn env)
  registerLuaFunction "equipAccessory"  (equipmentEquipAccessoryFn env)
  registerLuaFunction "unequipAccessory"(equipmentUnequipAccessoryFn env)
  registerLuaFunction "getAccessories"  (equipmentGetAccessoriesFn env)
  Lua.setglobal (Lua.Name "equipment")

  -- Substance global — read-only access to material physical
  -- properties (density, tensile, fracture toughness, …). Loaded
  -- from data/substances/*.yaml via engine.loadSubstanceYaml.
  Lua.newtable
  registerLuaFunction "get"      (substanceGetFn regs)
  registerLuaFunction "getNames" (substanceGetNamesFn regs)
  Lua.setglobal (Lua.Name "substance")

  -- Infection global — read-only access to the infection catalogue
  -- (staph, gas gangrene, …) loaded from data/infections/*.yaml via
  -- engine.loadInfectionYaml.
  Lua.newtable
  registerLuaFunction "get"      (infectionGetFn regs)
  registerLuaFunction "getNames" (infectionGetNamesFn regs)
  Lua.setglobal (Lua.Name "infection")
