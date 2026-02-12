{-# LANGUAGE ExistentialQuantification, UnicodeSyntax #-}

module Engine.Scripting.Backend
  ( ScriptBackend(..)
  , AnyBackend(..)
  , BackendType(..)
  ) where

import UPrelude
import Engine.Scripting.Types
import qualified Data.Text as T

-- | Backend type selection
data BackendType
  = LuaBackendType
  | PureScriptBackendType
  | JavaScriptBackendType
  | PythonBackendType
  deriving (Show, Eq, Read)

-- | Abstract interface for scripting backends
class ScriptBackend backend where
  -- Lifecycle
  initBackend      ∷  backend → IO ScriptContext
  closeBackend     ∷ backend → ScriptContext → IO ()
  
  -- Script loading
  loadScript       ∷ backend → ScriptContext → FilePath → IO ScriptResult
  reloadScript     ∷ backend → ScriptContext → FilePath → IO ScriptResult
  
  -- Function calls
  callFunction     ∷  backend → ScriptContext → T.Text → [ScriptValue] → IO ScriptResult
  
  -- API registration
  registerFunction ∷  backend → ScriptContext → T.Text 
                   → ([ScriptValue] → IO [ScriptValue]) → IO ()
  
  -- Introspection
  backendName      ∷ backend → T.Text
  backendVersion   ∷ backend → T.Text

-- | Existential wrapper for any backend
data AnyBackend = ∀ backend. ScriptBackend backend ⇒ AnyBackend backend
