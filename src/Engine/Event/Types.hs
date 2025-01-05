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
import Engine.Input.Types

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

data SystemAction
  = SysRecreateWindow
  | SysReloadResources
  | SysToggleFullscreen
  | SysResizeWindow Int Int
  | SysExit
  deriving (Show, Eq)
