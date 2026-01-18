module Engine.Scripting.Lua.Types where

import UPrelude
import Data.Time.Clock (UTCTime)
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import qualified Engine.Core.Queue as Q
import qualified Data.Map.Strict as Map
import qualified HsLua as Lua

-- | Represents a single Lua script's metadata
data LuaScript = LuaScript
  { scriptPath     ∷ FilePath -- path to the Lua script
  , scriptTickRate ∷ Double   -- seconds between updates
  , scriptNextTick ∷ Double   -- next scheduled tick time
  } deriving (Eq, Show)

-- | Thread-safe map of Lua scripts
type LuaScripts = TVar (Map.Map FilePath LuaScript)

-- | Lua-specific state (wraps Lua.  State with script tracking)
data LuaBackendState = LuaBackendState
  { lbsLuaState    ∷ Lua.State
  , lbsScripts     ∷ LuaScripts
  , lbsMsgQueues   ∷ (Q.Queue LuaToEngineMsg, Q.Queue EngineToLuaMsg)
  }

-- | Log levels for Lua
data LuaLogLevel = LuaLogDebug
                 | LuaLogInfo
                 | LuaLogWarn
                 | LuaLogError
                 deriving (Eq, Show)

data LuaToEngineMsg = LuaLog LuaLogLevel String
                    | LuaLoadTexture FilePath
                    deriving (Eq, Show)
data EngineToLuaMsg = LuaTextureLoaded Int
                    | LuaThreadKill
                    deriving (Eq, Show)

-- | Lua execution result
data LuaResult = LuaSuccess
               | LuaError String
               | LuaNoop
               deriving (Eq, Show)
