-- src/Engine/Graphics/Window/Types.hs
module Engine.Graphics.Window.Types
  ( WindowConfig(..)
  , Window(..)  -- or maybe just Window if you want to keep it opaque
  ) where

import UPrelude
import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW

-- | Basic window configuration
data WindowConfig = WindowConfig
  { wcWidth     ∷ Int
  , wcHeight    ∷ Int
  , wcTitle     ∷ T.Text
  , wcResizable ∷ Bool
  }

-- | Opaque window type
newtype Window = Window GLFW.Window
