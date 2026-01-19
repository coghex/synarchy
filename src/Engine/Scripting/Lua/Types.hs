module Engine.Scripting.Lua.Types where

import UPrelude
import Data.Time.Clock (UTCTime)
import Data.IORef (IORef)
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Engine.Asset.Base
import Engine.Asset.Types
import Engine.Scene.Base
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
  { lbsLuaState     ∷ Lua.State
  , lbsScripts      ∷ LuaScripts
  , lbsMsgQueues    ∷ (Q.Queue LuaToEngineMsg, Q.Queue EngineToLuaMsg)
  , lbsAssetPool    ∷ IORef AssetPool
  , lbsNextObjectId ∷ IORef Word32
  }

-- | Log levels for Lua
data LuaLogLevel = LuaLogDebug
                 | LuaLogInfo
                 | LuaLogWarn
                 | LuaLogError
                 deriving (Eq, Show)

data LuaToEngineMsg = LuaLog LuaLogLevel String
                    | LuaLoadTextureRequest TextureHandle FilePath
                    | LuaSpawnSpriteRequest
                        { lssObjectId    ∷ ObjectId -- generated in lua thread
                        , lssX           ∷ Float
                        , lssY           ∷ Float
                        , lssWidth       ∷ Float
                        , lssHeight      ∷ Float
                        , lssTextureHandle ∷ TextureHandle }
                    deriving (Eq, Show)
data EngineToLuaMsg = LuaTextureLoaded TextureHandle AssetId
                    | LuaThreadKill
                    deriving (Eq, Show)

-- | Lua execution result
data LuaResult = LuaSuccess
               | LuaError String
               | LuaNoop
               deriving (Eq, Show)
