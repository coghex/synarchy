module Engine.Core.Types
  ( EngineState(..)
  , EngineEnv(..)
  , EngineError(..)
  , EngineConfig(..)
  , LoggingFunc
  ) where

import UPrelude
import qualified Control.Monad.Logger.CallStack as Logger
import qualified Data.Text as T
import Data.Word (Word64)
import qualified Vulkan.Core10 as Vk

-- | Engine environment (read-only)
data EngineEnv = EngineEnv
  { engineConfig    ∷ EngineConfig
  , vulkanInstance  ∷ Vk.Instance
  , vulkanDevice    ∷ Vk.Device
  }

-- | Engine state (mutable)
data EngineState = EngineState
  { frameCount     ∷ Word64
  , engineRunning  ∷ Bool
  , currentTime    ∷ Double
  , deltaTime      ∷ Double
  , logFunc        ∷ LoggingFunc
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
