-- Engine/Event/Types.hs
module Engine.Event.Types
  ( Event(..)
  , LogLevel(..)
  , InputEvent(..)
  , SystemAction(..)
  ) where

import UPrelude
import qualified Data.Text as T
import Engine.Asset.Types
import qualified Graphics.UI.GLFW as GLFW
import Engine.Input.Types
import Engine.Event.Base

data Event
  = EventError T.Text T.Text            -- ^ Error event with source and message
  | EventLog LogLevel T.Text         -- ^ Logging event
  | EventInput InputEvent          -- ^ Input event
  | EventSystem SystemAction       -- ^ System-level event
  | EventAsset AssetEvent          -- ^ Asset event
  deriving (Show, Eq)

data LogLevel
  = LogDebug Int
  | LogInfo
  | LogWarn
  | LogError
  deriving (Show, Eq)

data AssetEvent
  = AssetLoaded AssetId
  | AssetLoadError AssetId T.Text
  | AssetUnloaded AssetId
  deriving (Show, Eq)
