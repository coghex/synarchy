-- Engine/Event/Types.hs
module Engine.Event.Types
  ( Event(..)
  , LogLevel(..)
  , InputEvent(..)
  , SystemAction(..)
  ) where

import UPrelude
import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW

data Event
  = EventError T.Text T.Text            -- ^ Error event with source and message
  | EventLog LogLevel T.Text         -- ^ Logging event
  | EventInput InputEvent          -- ^ Input event
  | EventSystem SystemAction       -- ^ System-level event
  deriving (Show, Eq)

data LogLevel
  = LogDebug Int
  | LogInfo
  | LogWarn
  | LogError
  deriving (Show, Eq)

data InputEvent
  = KeyInput 
      { window ∷ GLFW.Window
      , key ∷ GLFW.Key
      , scancode ∷ Int
      , keyaction ∷ GLFW.KeyState
      , mods ∷ GLFW.ModifierKeys
      }
  | MouseButton
      { window ∷ GLFW.Window
      , button ∷ GLFW.MouseButton
      , mouseaction ∷ GLFW.MouseButtonState
      , mods ∷ GLFW.ModifierKeys
      }
  | MouseScroll
      { window ∷ GLFW.Window
      , xOffset ∷ Double
      , yOffset ∷ Double
      }
  deriving (Show, Eq)

data SystemAction
  = SysRecreateWindow
  | SysReloadResources
  | SysToggleFullscreen
  | SysResizeWindow Int Int
  | SysExit
  deriving (Show, Eq)
