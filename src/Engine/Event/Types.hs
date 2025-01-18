-- Engine/Event/Types.hs
module Engine.Event.Types
  ( Event(..)
  , LogLevel(..)
  , InputEvent(..)
  , SystemAction(..)
  ) where

import UPrelude
import Engine.Asset.Base
import Engine.Asset.Types
import qualified Graphics.UI.GLFW as GLFW
import Engine.Input.Types
import Engine.Event.Base

data Event
  = EventError Text Text           -- ^ Error event with source and message
  | EventLog LogLevel Text         -- ^ Logging event
  | EventInput InputEvent          -- ^ Input event
  | EventSystem SystemAction       -- ^ System-level event
  | EventAsset AssetEvent          -- ^ Asset event
  deriving (Show, Eq)
