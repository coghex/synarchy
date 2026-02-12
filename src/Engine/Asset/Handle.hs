{-# LANGUAGE TypeApplications, AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables, UnicodeSyntax #-}
module Engine.Asset.Handle
  ( TextureHandle(..)
  , FontHandle(..)
  , ShaderHandle(..)
  , AssetHandle(..)
  , AssetState(..)
  , Dependent(..)
  ) where

import UPrelude
import Engine.Asset.Base (AssetId)

-----------------------------------------------------------
-- Asset Handles
-----------------------------------------------------------

newtype TextureHandle = TextureHandle Int
  deriving (Show, Eq, Ord)

newtype FontHandle = FontHandle Int
  deriving (Show, Eq, Ord)

newtype ShaderHandle = ShaderHandle Int
  deriving (Show, Eq, Ord)

-----------------------------------------------------------
-- Asset State Tracking
-----------------------------------------------------------

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

data Dependent = Dependent
  { depAssetId ∷ AssetId
  , depType    ∷ Text
  } deriving (Show, Eq)

-----------------------------------------------------------
-- Asset Handle Type Class
-----------------------------------------------------------

class (Eq h, Ord h, Show h) ⇒ AssetHandle h where
  fromInt ∷ Int → h
  toInt ∷ h → Int

instance AssetHandle TextureHandle where
  fromInt       = TextureHandle
  toInt (TextureHandle n) = n

instance AssetHandle FontHandle where
  fromInt       = FontHandle
  toInt (FontHandle n) = n

instance AssetHandle ShaderHandle where
  fromInt       = ShaderHandle
  toInt (ShaderHandle n) = n
