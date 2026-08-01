module Engine.Graphics.Base where
import UPrelude

-- Basic graphics types without Vulkan dependencies
data GraphicsConfig = GraphicsConfig
  { gcAppName    ∷ Text   -- ^ Application name for Vulkan
  , gcDebugMode  ∷ Bool   -- ^ Enable Vulkan validation layers
  , gcMaxFrames  ∷ Int    -- ^ Maximum number of frames in flight
  }

