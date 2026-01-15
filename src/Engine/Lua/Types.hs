module Engine.Lua.Types
  ( LuaScript(..)
  , LuaScripts
  , LuaEnv(..)
  , LuaState(..)
  , LuaError(..)
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Scripting.Lua as Lua
import Control.Concurrent.STM (TVar)

-- | Represents a single Lua script with execution metadata
data LuaScript = LuaScript
  { lsName         ∷ T.Text           -- ^ Script name/identifier
  , lsFilePath     ∷ FilePath         -- ^ Path to the Lua script file
  , lsTickInterval ∷ Word64           -- ^ Ticks between executions (0 = execute every tick)
  , lsNextTick     ∷ Word64           -- ^ Next tick when this script should execute (informational, not used for scheduling)
  , lsEnabled      ∷ Bool             -- ^ Whether this script is enabled
  , lsInitialized  ∷ Bool             -- ^ Whether the init script has been executed
  } deriving (Show, Eq)

-- | Type alias for managing multiple Lua scripts
type LuaScripts = Map.Map T.Text LuaScript

-- | Lua environment containing the Lua state and configuration
data LuaEnv = LuaEnv
  { leLuaState    ∷ Lua.LuaState      -- ^ The Lua interpreter state
  , leModPath     ∷ FilePath          -- ^ Path to the mods directory
  , leScripts     ∷ TVar LuaScripts   -- ^ Thread-safe map of registered scripts
  , leCurrentTick ∷ TVar Word64       -- ^ Current game tick counter
  }

-- | Lua execution state tracking
data LuaState = LuaState
  { lstScripts     ∷ LuaScripts       -- ^ All registered scripts
  , lstCurrentTick ∷ Word64           -- ^ Current tick counter
  , lstTickLCM     ∷ Word64           -- ^ LCM of all tick intervals for optimization
  } deriving (Show, Eq)

-- | Lua error types for better error handling
data LuaError
  = LuaLoadError T.Text FilePath      -- ^ Error loading a script file
  | LuaExecutionError T.Text FilePath -- ^ Error executing a script
  | LuaScriptNotFound T.Text          -- ^ Script not found by name
  | LuaStateError T.Text              -- ^ General Lua state error
  deriving (Show, Eq)
