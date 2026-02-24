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
import Engine.Graphics.Config (WindowMode(..), TextureFilter(..))
import UI.Types (ElementHandle(..))
import qualified Graphics.UI.GLFW as GLFW
import qualified Engine.Core.Queue as Q
import qualified Data.Map.Strict as Map
import qualified HsLua as Lua

-- | Represents a single Lua script's metadata
data LuaScript = LuaScript
  { scriptId        ∷ Word32   -- unique identifier
  , scriptPath      ∷ FilePath -- path to the Lua script
  , scriptTickRate  ∷ Double   -- seconds between updates
  , scriptNextTick  ∷ Double   -- next scheduled tick time
  , scriptModuleRef ∷ Lua.Reference -- reference to the returned table
  , scriptPaused    ∷ Bool     -- is the script paused
  } deriving (Eq, Show)

-- | Thread-safe map of Lua scripts
type LuaScripts = TVar (Map.Map FilePath LuaScript)

-- | Lua-specific state (wraps Lua.  State with script tracking)
data LuaBackendState = LuaBackendState
  { lbsLuaState     ∷ Lua.State
  , lbsScripts      ∷ TVar (Map.Map Word32 LuaScript)
  , lbsNextScriptId ∷ IORef Word32
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
                    | LuaSetWindowMode WindowMode
                    | LuaSetVSync Bool
                    | LuaSetMSAA Int
                    | LuaSetResolution Int Int
                    | LuaSetBrightness Int
                    | LuaSetPixelSnap Bool
                    | LuaSetTextureFilter TextureFilter
                    | LuaLoadTextureRequest TextureHandle FilePath
                    | LuaLoadFontRequest FontHandle FilePath Int
                    | LuaSpawnTextRequest ObjectId Float Float FontHandle
                                                   Text Vec4 LayerId Float
                    | LuaSpawnSpriteRequest
                        { lssObjectId    ∷ ObjectId -- generated in lua thread
                        , lssX           ∷ Float
                        , lssY           ∷ Float
                        , lssWidth       ∷ Float
                        , lssHeight      ∷ Float
                        , lssTextureHandle ∷ TextureHandle
                        , lssLayer       ∷ LayerId }
                    | LuaSetSpriteScaleRequest ObjectId Float Float
                    | LuaSetColorRequest ObjectId Vec4
                    | LuaSetSizeRequest ObjectId Float Float
                    | LuaSetPosRequest ObjectId Float Float
                    | LuaSetVisibleRequest ObjectId Bool
                    | LuaSetTextRequest ObjectId Text
                    | LuaDestroyRequest ObjectId
                    | LuaRequestFocus Word32
                    | LuaReleaseFocus
                    | LuaRegisterFocusable Bool Int
                    | LuaUnregisterFocusable Word32
                    deriving (Eq, Show)

-- | messages to lua from anywhere
data LuaMsg = LuaTextureLoaded TextureHandle AssetId
            | LuaFontLoaded FontHandle FilePath
            | LuaFontLoadFailed Text
            | LuaThreadKill
            | LuaMouseDownEvent GLFW.MouseButton Double Double
            | LuaMouseUpEvent GLFW.MouseButton Double Double
            | LuaScrollEvent Double Double
            | LuaZSliceScroll Double Double
            | LuaKeyDownEvent Key
            | LuaKeyUpEvent Key
            | LuaShellToggle
            | LuaWindowResize Int Int
            | LuaFramebufferResize Int Int
            | LuaAssetLoaded Text Int Text
            | LuaFocusGained Word32
            | LuaFocusLost Word32
            | LuaCharInput Word32 Char
            | LuaTextBackspace Word32
            | LuaTabPressed Word32
            | LuaTextSubmit Word32
            | LuaCursorUp Word32
            | LuaCursorDown Word32
            | LuaCursorLeft Word32
            | LuaCursorRight Word32
            | LuaCursorHome Word32
            | LuaCursorEnd Word32
            | LuaTextDelete Word32
            | LuaInterrupt Word32
            | LuaUIClickEvent ElementHandle Text
            | LuaUIRightClickEvent ElementHandle Text
            | LuaUIScrollEvent ElementHandle Double Double
            | LuaUICharInput Char
            | LuaUIBackspace
            | LuaUIDelete
            | LuaUISubmit
            | LuaUIEscape
            | LuaUICursorLeft
            | LuaUICursorRight
            | LuaUIHome
            | LuaUIEnd
            | LuaUIFocusLost
            | LuaDebugShow
            | LuaDebugHide
            | LuaDebugToggle
            | LuaWorldGenLog Text
            | LuaHudLogInfo Text Text
            | LuaWorldPreviewReady Int
            deriving (Eq, Show)

-- | Lua execution result
data LuaResult = LuaSuccess
               | LuaError String
               | LuaNoop
               deriving (Eq, Show)
