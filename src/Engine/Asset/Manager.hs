-- Engine/Asset/Manager.hs
module Engine.Asset.Manager where

import UPrelude
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, modify, gets)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Ptr (nullPtr)
import Vulkan.Core10
import Engine.Core.Types
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Asset.Types
import Engine.Graphics.Vulkan.Types
import Engine.Event.Types

