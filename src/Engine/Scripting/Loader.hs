module Engine.Scripting.Loader
  ( createBackend
  ) where

import UPrelude
import Engine.Scripting.Backend
import qualified Engine.Scripting.Lua.Backend as Lua

-- | Create backend based on type (breaks circular dependency)
createBackend ∷ BackendType → IO AnyBackend
createBackend LuaBackendType = do
  backend ← Lua.createLuaBackend
  return $ AnyBackend backend
createBackend PureScriptBackendType = throwUnsupportedBackend "PureScript"
createBackend JavaScriptBackendType = throwUnsupportedBackend "JavaScript"
createBackend PythonBackendType     = throwUnsupportedBackend "Python"

throwUnsupportedBackend ∷ String → IO a
throwUnsupportedBackend name = 
  error $ name ++ " scripting backend not yet implemented. Only Lua is currently supported."
