-- test/Test/Engine/Graphics/Vulkan/Surface.hs
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Test.Engine.Graphics.Vulkan.Surface where

import UPrelude
import Test.Hspec
import Engine.Graphics.Window.Types (Window(..))
import qualified Graphics.UI.GLFW as GLFW
import Engine.Core.State
import Engine.Core.Base
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (catchError)
import Foreign.Ptr (nullPtr)
import Data.Maybe (isJust)
import Vulkan.Dynamic (InstanceCmds(..))
import Engine.Core.Error.Exception
import Vulkan.Core10 (Instance(..))

