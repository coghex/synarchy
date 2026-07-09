-- | Top-level Lua API registration. Each Lua global table (@engine@,
--   @UI@, @unit@, @building@, …) is populated by a dedicated
--   "Engine.Scripting.Lua.API.Register" submodule; this module just
--   sequences them into one Lua state under a single 'Lua.runWith'.
module Engine.Scripting.Lua.API
  ( registerLuaAPI
  , registerLuaFunction
  ) where

import UPrelude
import Engine.Scripting.Lua.Types (LuaBackendState)
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.Register.Engine (registerEngineAPI)
import Engine.Scripting.Lua.API.Register.UI (registerUIAPI)
import Engine.Scripting.Lua.API.Register.Unit (registerUnitAPI)
import Engine.Scripting.Lua.API.Register.Building (registerBuildingAPI)
import Engine.Scripting.Lua.API.Register.Designation (registerDesignationAPI)
import Engine.Scripting.Lua.API.Register.Equipment (registerEquipmentAPI)
import Engine.Scripting.Lua.API.Register.Craft (registerCraftAPI)
import Engine.Scripting.Lua.API.Register.Item (registerItemAPI)
import Engine.Scripting.Lua.API.Register.World (registerWorldAPI)
import Engine.Scripting.Lua.API.Register.Camera (registerCameraAPI)
import Engine.Core.State (EngineEnv)
import qualified HsLua as Lua

registerLuaAPI ∷ Lua.State → EngineEnv → LuaBackendState → IO ()
registerLuaAPI lst env backendState = Lua.runWith lst $ do
  registerEngineAPI lst env backendState
  registerUIAPI env
  registerUnitAPI env
  registerBuildingAPI env
  registerDesignationAPI env
  registerEquipmentAPI env
  registerCraftAPI env
  registerItemAPI env
  registerWorldAPI env
  registerCameraAPI env
