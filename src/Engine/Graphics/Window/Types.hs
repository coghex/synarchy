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
  , wcBorderless ∷ Bool
    -- ^ Ask 'Engine.Graphics.Window.GLFW.createWindow' for an
    --   undecorated, monitor-sized window (#1731). Mutually exclusive
    --   with 'wcFullscreen', which takes precedence if a caller sets
    --   both; 'Engine.Core.Defaults.defaultWindowConfig' never does.
    --   Like the fullscreen request this is a REQUEST, not an outcome —
    --   it degrades to the plain decorated window when no primary
    --   monitor or video mode is available.
  , wcTitle      ∷ Text
  , wcResizable  ∷ Bool
  , wcVisible    ∷ Bool
  , wcFocused    ∷ Bool
  }

-- | Opaque window type
newtype Window = Window GLFW.Window
