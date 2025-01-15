module Engine.Asset.Manager.Types where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Engine.Concurrent.Queue as Q
import Data.Word (Word32)
import Vulkan.Core10
import Engine.Graphics.Vulkan.Types (TextureInfo)
import Engine.Asset.Types
import Engine.Asset.Id


