module Engine.Scripting.PureScript.Backend
  ( PureScriptBackend(..)
  , createPureScriptBackend  -- ← Add this export
  ) where

import UPrelude
import Engine.Scripting.Backend
import Engine.Scripting.Types
import qualified Data.Text as T

data PureScriptBackend = PureScriptBackend

instance ScriptBackend PureScriptBackend where
  initBackend _          = error "PureScript backend not yet implemented"
  closeBackend _ _       = error "PureScript backend not yet implemented"
  loadScript _ _ _       = error "PureScript backend not yet implemented"
  reloadScript _ _ _     = error "PureScript backend not yet implemented"
  callFunction _ _ _ _   = error "PureScript backend not yet implemented"
  registerFunction _ _ _ _ = error "PureScript backend not yet implemented"
  backendName _          = "PureScript"
  backendVersion _       = "0.15.0"

-- ← Add this function
createPureScriptBackend :: IO PureScriptBackend
createPureScriptBackend = error "PureScript backend not yet implemented"
