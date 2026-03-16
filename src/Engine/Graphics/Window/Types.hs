module Engine.Graphics.Window.Types
  ( WindowConfig(..)
  , Window(..)
  ) where

import UPrelude
import qualified Graphics.UI.GLFW as GLFW

-- | Basic window configuration
data WindowConfig = WindowConfig
  { wcWidth      ∷ Int
  , wcHeight     ∷ Int
  , wcFullscreen ∷ Bool
  , wcTitle      ∷ Text
  , wcResizable  ∷ Bool
  }

-- | Opaque window type
newtype Window = Window GLFW.Window
