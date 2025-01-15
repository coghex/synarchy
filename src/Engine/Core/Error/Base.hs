module Engine.Core.Error.Base
  ( EngineError(..)
  ) where

import UPrelude
import qualified Data.Text as T

-- | Engine errors
data EngineError
  = VulkanError   T.Text
  | ResourceError T.Text
  | StateError    T.Text
  | InitError     T.Text
  | AssetError    T.Text
  deriving (Show, Eq)
