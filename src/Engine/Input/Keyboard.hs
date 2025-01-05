-- File: Engine/Input/Keyboard.hs
module Engine.Input.Keyboard where

import UPrelude
import qualified Data.Map as Map
import qualified Graphics.UI.GLFW as GLFW
import Engine.Input.Types
import Engine.Core.Types
import Engine.Core.Monad (EngineM', MonadIO(liftIO))
import Control.Monad.State (modify)
import Control.Monad (when)

