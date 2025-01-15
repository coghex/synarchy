module Engine.Core.Types
  ( EngineConfig(..)
  ) where

import UPrelude
import qualified Control.Monad.Logger.CallStack as Logger
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word32, Word64)
import qualified Vulkan.Core10 as Vk
import Engine.Asset.Types
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Texture (TextureState(..))
import Engine.Graphics.Window.Types
import Engine.Graphics.Vulkan.Types.Descriptor
import Engine.Core.Queue
import Engine.Event.Types
import Engine.Input.Types

-- | Engine configuration
data EngineConfig = EngineConfig
  { windowWidth     ∷ Int
  , windowHeight    ∷ Int
  , enableVSync     ∷ Bool
  , enableDebug     ∷ Bool
  }

