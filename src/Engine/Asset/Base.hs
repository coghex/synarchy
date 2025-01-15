module Engine.Asset.Base where

import UPrelude
import Data.Word (Word32)
import qualified Data.Text as T

-- | Unique identifier for assets
newtype AssetId = AssetId Word32
  deriving (Eq, Ord, Show)

-- | Asset status tracking
data AssetStatus
  = AssetUnloaded
  | AssetLoading
  | AssetLoaded
  | AssetError T.Text
  deriving (Eq, Show)

data AssetEvent
  = AssetStatus AssetId
  deriving (Show, Eq)
