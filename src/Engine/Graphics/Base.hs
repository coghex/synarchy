module Engine.Graphics.Base where
import UPrelude
-- Basic graphics types without Vulkan dependencies
data GraphicsConfig = GraphicsConfig
  { gcAppName    ∷ Text
  , gcWidth      ∷ Int
  , gcHeight     ∷ Int
  , gcDebugMode  ∷ Bool
  , gcMaxFrames  ∷ Int
  }
