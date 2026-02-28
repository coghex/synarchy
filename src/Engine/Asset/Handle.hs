{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Engine.Asset.Handle
  ( TextureHandle(..)
  , FontHandle(..)
  , ShaderHandle(..)
  , AssetHandle(..)
  , AssetState(..)
  , Dependent(..)
  ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Control.DeepSeq (NFData)
import Engine.Asset.Base (AssetId)

-- | Asset Handles provide opaque references to assets
newtype TextureHandle = TextureHandle Int
  deriving (Show, Eq, Ord, Generic, Serialize, NFData)

newtype FontHandle = FontHandle Int
  deriving (Show, Eq, Ord)

newtype ShaderHandle = ShaderHandle Int
  deriving (Show, Eq, Ord)

-- | Asset state tracking
data AssetState α
  = AssetLoading 
    { asPath       ∷ FilePath
    , asDependents ∷ [Dependent]
    , asProgress   ∷ Float 
    }
  | AssetReady 
    { asValue      ∷ α
    , asDependents ∷ [Dependent] 
    }
  | AssetFailed 
    { asError ∷ Text }
  deriving (Show, Eq)

-- | Dependent asset information
data Dependent = Dependent
  { depAssetId ∷ AssetId
  , depType    ∷ Text
  } deriving (Show, Eq)

-- | Asset handle type class (simplified - no pool dependencies)
class (Eq h, Ord h, Show h) ⇒ AssetHandle h where
  -- construct a handle from an integer
  fromInt ∷ Int → h
  -- extract Int from handle
  toInt ∷ h → Int

-- Instances
instance AssetHandle TextureHandle where
  fromInt       = TextureHandle
  toInt (TextureHandle n) = n

instance AssetHandle FontHandle where
  fromInt       = FontHandle
  toInt (FontHandle n) = n

instance AssetHandle ShaderHandle where
  fromInt       = ShaderHandle
  toInt (ShaderHandle n) = n
