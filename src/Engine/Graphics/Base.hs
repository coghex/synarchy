module Engine.Graphics.Base where
import UPrelude
import qualified Data.Text as T
-- Basic graphics types without Vulkan dependencies
data GraphicsConfig = GraphicsConfig
  { gcAppName    ∷ T.Text
  , gcWidth      ∷ Int
  , gcHeight     ∷ Int
  , gcDebugMode  ∷ Bool
  , gcMaxFrames  ∷ Int
  }
