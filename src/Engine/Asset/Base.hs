module Engine.Asset.Base where

import UPrelude

-- | Unique identifier for assets
newtype AssetId = AssetId Word32
  deriving (Eq, Ord, Show)

-- | Asset status tracking
data AssetStatus
  = AssetUnloaded
  | AssetLoading
  | AssetLoaded
  | AssetError Text
  deriving (Eq, Show)

data AssetEvent
  = AssetStatus AssetId
  deriving (Show, Eq)
