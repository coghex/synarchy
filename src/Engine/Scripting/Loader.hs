module Engine.Scripting.Loader
  ( createBackend
  ) where

import UPrelude
import Engine.Scripting.Backend
import Engine.Scripting.Types
import qualified Engine.Scripting.Lua.Backend as Lua
import qualified Engine.Scripting.PureScript.Backend as PureScript

-- | Create backend based on type (breaks circular dependency)
createBackend :: BackendType -> IO AnyBackend
createBackend LuaBackendType = do
  backend <- Lua.createLuaBackend
  return $ AnyBackend backend
createBackend PureScriptBackendType = do
  backend <- PureScript.createPureScriptBackend
  return $ AnyBackend backend
createBackend JavaScriptBackendType = throwUnsupportedBackend "JavaScript"
createBackend PythonBackendType     = throwUnsupportedBackend "Python"

throwUnsupportedBackend :: String -> IO a
throwUnsupportedBackend name = 
  error $ name ++ " scripting backend not yet implemented. Only Lua is currently supported."
