{-# LANGUAGE ExistentialQuantification, UnicodeSyntax #-}

module Engine.Scripting.Backend
  ( ScriptBackend(..)
  , AnyBackend(..)
  , BackendType(..)
  ) where

import UPrelude
import Engine.Scripting.Types
import qualified Data.Text as T

data BackendType
  = LuaBackendType
  | PureScriptBackendType
  | JavaScriptBackendType
  | PythonBackendType
  deriving (Show, Eq, Read)

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

data AnyBackend = ∀ backend. ScriptBackend backend ⇒ AnyBackend backend
