module Engine.Core.Types
  ( EngineState(..)
  , EngineEnv(..)
  , EngineError(..)
  , EngineConfig(..)
  , LoggingFunc
  , TextureState(..)
  ) where

import UPrelude
import qualified Control.Monad.Logger.CallStack as Logger
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word64)
import qualified Vulkan.Core10 as Vk
import Engine.Graphics.Vulkan.Types.Texture (TextureState(..))

-- | Engine environment (read-only)
data EngineEnv = EngineEnv
  { engineConfig     ∷ EngineConfig
  , vulkanInstance   ∷ Vk.Instance
  , vulkanDevice     ∷ Vk.Device
  , vulkanCmdPool    ∷ Vk.CommandPool
  , vulkanCmdBuffers ∷ V.Vector Vk.CommandBuffer
  }

-- | Engine state (mutable)
data EngineState = EngineState
  { frameCount     ∷ Word64
  , engineRunning  ∷ Bool
  , currentTime    ∷ Double
  , deltaTime      ∷ Double
  , logFunc        ∷ LoggingFunc
  , textureState   ∷ TextureState
  }

type LoggingFunc = Logger.Loc → Logger.LogSource → Logger.LogLevel
                              → Logger.LogStr → IO ()

-- | Engine configuration
data EngineConfig = EngineConfig
  { windowWidth     ∷ Int
  , windowHeight    ∷ Int
  , enableVSync     ∷ Bool
  , enableDebug     ∷ Bool
  }

-- | Engine errors
data EngineError
  = VulkanError   T.Text
  | ResourceError T.Text
  | StateError    T.Text
  | InitError     T.Text
  deriving (Show, Eq)
