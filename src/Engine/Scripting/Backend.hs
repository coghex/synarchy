{-# LANGUAGE ExistentialQuantification, UnicodeSyntax #-}

module Engine.Scripting.Backend
  ( ScriptBackend(..)
  , AnyBackend(..)
  , BackendType(..)
  ) where

import UPrelude
import Engine.Scripting.Types
import qualified Data.Text as T

-- -----------------------------------------------------------
-- Backend types
-- -----------------------------------------------------------

data BackendType
  = LuaBackendType
  | PureScriptBackendType
  | JavaScriptBackendType
  | PythonBackendType
  deriving (Show, Eq, Read)

data AnyBackend = ∀ backend. ScriptBackend backend ⇒ AnyBackend backend

-- -----------------------------------------------------------
-- Backend interface
-- -----------------------------------------------------------

class ScriptBackend backend where
  initBackend      ∷  backend → IO ScriptContext
  closeBackend     ∷ backend → ScriptContext → IO ()
  loadScript       ∷ backend → ScriptContext → FilePath → IO ScriptResult
  reloadScript     ∷ backend → ScriptContext → FilePath → IO ScriptResult
  callFunction     ∷  backend → ScriptContext → T.Text → [ScriptValue] → IO ScriptResult
  registerFunction ∷  backend → ScriptContext → T.Text 
                   → ([ScriptValue] → IO [ScriptValue]) → IO ()
  backendName      ∷ backend → T.Text
  backendVersion   ∷ backend → T.Text

-- -----------------------------------------------------------
-- Helpers
-- -----------------------------------------------------------

throwUnsupportedBackend ∷ String → IO a
throwUnsupportedBackend name = 
  error $ name ++ " scripting backend not yet implemented.  Only Lua is currently supported."
