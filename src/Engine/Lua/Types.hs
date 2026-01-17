module Engine.Lua.Types where

import UPrelude
import Data.Time.Clock (UTCTime)
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Concurrent.STM.TQueue (TQueue)
import qualified Engine.Core.Queue as Q
import qualified Data.Map.Strict as Map
import qualified HsLua as Lua

-- | represents a single Lua script's metadata
data LuaScript = LuaScript
  { scriptPath   ∷ FilePath -- ^ path to the Lua script file
  } deriving (Eq, Show)

-- | a thread-safe map of Lua scripts, where the key is the file path
type LuaScripts = TVar (Map.Map FilePath LuaScript)

-- | lua data state containing the Lua interpreter and related data
data LuaState = LuaState
  { luaState    ∷ Lua.State         -- ^ the Lua state
  , luaScripts  ∷ LuaScripts        -- ^ the collection of Lua scripts
  , luaEventQ   ∷ Q.Queue LuaEvent   -- ^ queue for Lua events
  , luaCommandQ ∷ Q.Queue LuaCommand -- ^ queue for Lua commands
  }

defaultLuaState ∷ Q.Queue LuaEvent → Q.Queue LuaCommand → IO LuaState
defaultLuaState eventQueue commandQueue = do
  lState ← Lua.newstate
  scriptsVar ← newTVarIO Map.empty
  return LuaState
    { luaState    = lState
    , luaScripts  = scriptsVar
    , luaEventQ   = eventQueue
    , luaCommandQ = commandQueue
    }

-- | Log levels for Lua
data LuaLogLevel = LuaLogDebug
                 | LuaLogInfo
                 | LuaLogWarn
                 | LuaLogError
                 deriving (Eq, Show)

-- | Represents a command to be executed by Lua scripts
data LuaCommand = LuaStart | LuaStop deriving (Eq, Show)

-- | Represents an event that can be processed by Lua scripts
data LuaEvent = LuaLog LuaLogLevel String
              | LuaRes LuaResult
              deriving (Eq, Show)

-- | Represents the result of a Lua command execution
data LuaResult = LuaSuccess
               | LuaError String
               | LuaNoop
               deriving (Eq, Show)
