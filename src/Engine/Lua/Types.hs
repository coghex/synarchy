module Engine.Lua.Types where

import UPrelude
import Data.Time.Clock (UTCTime)
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TQueue (TQueue)
import qualified Data.Map.Strict as Map
import qualified HsLua as Lua

-- | represents a single Lua script's metadata
data LuaScript = LuaScript
  { scriptPath   ∷ FilePath -- ^ path to the Lua script file
  , tickInterval ∷ Double   -- ^ how often to run the script (in seconds)
  , nextTick     ∷ UTCTime  -- ^ the time when the script should next run
  } deriving (Eq, Show)

-- | a thread-safe map of Lua scripts, where the key is the file path
type LuaScripts = TVar (Map.Map FilePath LuaScript)

-- | global lua runtime environment
data LuaEnv = LuaEnv
  { luaState      ∷ Lua.State       -- ^ the Lua state
  , luaScripts    ∷ LuaScripts      -- ^ the collection of Lua scripts
  , luaEventQueue ∷ TQueue LuaEvent -- ^ queue for Lua events
  }

-- | Log levels for Lua
data LuaLogLevel = LuaLogDebug
                 | LuaLogInfo
                 | LuaLogWarn
                 | LuaLogError
                 deriving (Eq, Show)

-- | Represents an event that can be processed by Lua scripts
data LuaEvent = LuaLog LuaLogLevel String
              | LuaCmd FilePath String
              | LuaRes LuaResult
              deriving (Eq, Show)

-- | Represents the result of a Lua command execution
data LuaResult = LuaSuccess
               | LuaError String
               | LuaNoop
               deriving (Eq, Show)
