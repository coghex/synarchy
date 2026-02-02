module Engine.Graphics.Vulkan.Font
  ( getFontState
  ) where

import UPrelude
import Engine.Core.Monad (EngineM(..))
import Engine.Core.State (GraphicsState(..))
import Engine.Core.Error.Exception (ExceptionType(..), GraphicsError(..))
import Engine.Graphics.Vulkan.Types.Font (FontState(..))

getFontState ∷ GraphicsState → EngineM ε σ FontState
getFontState state = case fontState state of
  Just fs → return fs
  Nothing → logAndThrowM CatFont (ExGraphics FontError)
              "FontState is not initialized in GraphicsState"
