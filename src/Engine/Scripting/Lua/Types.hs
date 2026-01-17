module Engine.Scripting.Lua.Types where

import UPrelude
import Data.Time.Clock (UTCTime)
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import qualified Engine.Core.Queue as Q
import qualified Data.Map.Strict as Map
import qualified HsLua as Lua

-- | Represents a single Lua script's metadata
data LuaScript = LuaScript
  { scriptPath   ∷ FilePath
  } deriving (Eq, Show)

-- | Thread-safe map of Lua scripts
type LuaScripts = TVar (Map.Map FilePath LuaScript)

-- | Lua-specific state (wraps Lua.  State with script tracking)
data LuaBackendState = LuaBackendState
  { lbsLuaState    ∷ Lua.State
  , lbsScripts     ∷ LuaScripts
  , lbsEventQueue  ∷ Q.Queue LuaEvent
  , lbsCommandQueue ∷ Q.Queue LuaCommand
  }

-- | Log levels for Lua
data LuaLogLevel = LuaLogDebug
                 | LuaLogInfo
                 | LuaLogWarn
                 | LuaLogError
                 deriving (Eq, Show)

-- | Lua-specific commands
data LuaCommand = LuaStart | LuaStop 
  deriving (Eq, Show)

-- | Lua-specific events
data LuaEvent = LuaLog LuaLogLevel String
              | LuaRes LuaResult
              deriving (Eq, Show)

-- | Lua execution result
data LuaResult = LuaSuccess
               | LuaError String
               | LuaNoop
               deriving (Eq, Show)
