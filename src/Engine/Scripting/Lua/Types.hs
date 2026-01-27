module Engine.Scripting.Lua.Types where

import UPrelude
import Data.Time.Clock (UTCTime)
import Data.IORef (IORef)
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Engine.Asset.Base
import Engine.Asset.Types
import Engine.Asset.Handle
import Engine.Input.Types
import Engine.Scene.Base
import Engine.Graphics.Vulkan.Types.Vertex
import qualified Graphics.UI.GLFW as GLFW
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
  , lbsMsgQueues    ∷ (Q.Queue LuaToEngineMsg, Q.Queue LuaMsg)
  , lbsAssetPool    ∷ IORef AssetPool
  , lbsNextObjectId ∷ IORef Word32
  , lbsInputState   ∷ IORef InputState
  }

-- | Log levels for Lua
data LuaLogLevel = LuaLogDebug
                 | LuaLogInfo
                 | LuaLogWarn
                 | LuaLogError
                 deriving (Eq, Show)

-- | messages from lua to the main thread
data LuaToEngineMsg = LuaLog LuaLogLevel String
                    | LuaLoadTextureRequest TextureHandle FilePath
                    | LuaLoadFontRequest FontHandle FilePath Int
                    | LuaSpawnTextRequest ObjectId Float Float FontHandle
                                                   Text LayerId
                    | LuaSpawnSpriteRequest
                        { lssObjectId    ∷ ObjectId -- generated in lua thread
                        , lssX           ∷ Float
                        , lssY           ∷ Float
                        , lssWidth       ∷ Float
                        , lssHeight      ∷ Float
                        , lssTextureHandle ∷ TextureHandle
                        , lssLayer       ∷ LayerId }
                    | LuaMoveSpriteRequest ObjectId Float Float
                    | LuaSetSpriteScaleRequest ObjectId Float Float
                    | LuaSetSpriteColorRequest ObjectId Vec4
                    | LuaSetVisibleRequest ObjectId Bool
                    | LuaSetTextRequest ObjectId Text
                    | LuaDestroySpriteRequest ObjectId
                    | LuaRequestFocus Word32
                    | LuaReleaseFocus
                    | LuaRegisterFocusable Bool Int
                    | LuaUnregisterFocusable Word32
                    deriving (Eq, Show)

-- | messages from set to lua from anywhere
data LuaMsg = LuaTextureLoaded TextureHandle AssetId
            | LuaFontLoaded FontHandle
            | LuaFontLoadFailed Text
            | LuaThreadKill
            | LuaMouseDownEvent GLFW.MouseButton Double Double
            | LuaMouseUpEvent GLFW.MouseButton Double Double
            | LuaKeyDownEvent Key
            | LuaKeyUpEvent Key
            | LuaShellToggle
            | LuaFocusGained Word32
            | LuaFocusLost Word32
            | LuaCharInput Word32 Char
            | LuaTextBackspace Word32
            | LuaTextSubmit Word32
            deriving (Eq, Show)

-- | Lua execution result
data LuaResult = LuaSuccess
               | LuaError String
               | LuaNoop
               deriving (Eq, Show)
