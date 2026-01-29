module Engine.Scripting.Lua.Backend
  ( LuaBackend(..)
  , createLuaBackend
  , startLuaThread
  ) where

import UPrelude
import Engine.Scripting.Backend
import Engine.Scripting.Types
import Engine.Scripting.Lua.Thread (startLuaThread)
import Engine.Scripting.Lua.Script (callLuaFunction)
import Engine.Core.State (EngineEnv)
import qualified HsLua as Lua
import qualified Data.Text as T
import Data.Dynamic (toDyn, fromDynamic)

-- | Lua scripting backend
data LuaBackend = LuaBackend

instance ScriptBackend LuaBackend where
  initBackend _ = do
    lst ← Lua.newstate
    _ ← Lua.runWith lst Lua.openlibs
    return $ ScriptContext (toDyn lst)
  
  closeBackend _ (ScriptContext dyn) = 
    case fromDynamic dyn of
      Just lst → Lua.close lst
      Nothing  → error "Invalid Lua context"
  
  loadScript _ (ScriptContext dyn) path =
    case fromDynamic dyn of
      Just lst → do
        result ← Lua.runWith lst $ Lua.dofileTrace $ Just path
        case result of
          Lua.OK → return $ ScriptSuccess []
          _      → do
            err ← Lua.runWith lst $ Lua.tostring (-1)
            return $ ScriptError (T.pack $ show err)
      Nothing → return $ ScriptError "Invalid Lua context"
  
  reloadScript backend ctx path = loadScript backend ctx path
  
  callFunction _ (ScriptContext dyn) funcName args =
    case fromDynamic dyn of
      Just lst → do
        result ← Lua.runWith lst $ callLuaFunction funcName args
        case result of
          Lua.OK → return $ ScriptSuccess []
          _      → return $ ScriptError "Function call failed"
      Nothing → return $ ScriptError "Invalid Lua context"
  
  backendName _    = "Lua"
  backendVersion _ = "5.5"

-- | Create Lua backend
createLuaBackend ∷ IO LuaBackend
createLuaBackend = return LuaBackend
