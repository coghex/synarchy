module Engine.Asset.Base where

import UPrelude
import Data.Word (Word32)
import qualified Data.Text as T

-- | Unique identifier for assets
newtype AssetId = AssetId Word32
  deriving (Eq, Ord, Show)
data AssetAtatus = AssetUnloaded | AssetLoading | AssetLoaded | AssetError T.Text
  deriving (Show)
