{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Engine.Asset.Handle
  ( TextureHandle(..)
  , missingTextureHandle
  , firstAllocatableTextureHandle
  , isMissingTextureHandle
  , FontHandle(..)
  , AssetHandle(..)
  , AssetState(..)
  , Dependent(..)
  ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Data.Hashable (Hashable)
import Control.DeepSeq (NFData)
import Engine.Asset.Base (AssetId)

-- | Asset Handles provide opaque references to assets
newtype TextureHandle = TextureHandle Int
  deriving (Show, Eq, Ord, Generic, Serialize, NFData, Hashable)

-- | The missing\/absent-texture SENTINEL (#1696).
--
--   Dozens of engine fields spell "this texture is not set" as
--   @TextureHandle 0@ — every unset field in
--   "World.Render.Textures.Types", the unset UI separator, a flora
--   species whose YAML omits @harvested_texture@ — and the bindless
--   handle→slot table is zero-initialised so an unregistered handle id
--   resolves to bindless SLOT 0, the undefined checkerboard, in the
--   fragment shader (#286).
--
--   The handle id and the slot index are different numbers: slot 0 is
--   reserved by 'Engine.Graphics.Vulkan.Texture.Slot', and reserving
--   the HANDLE takes the two guards below.
--
--   1. 'Engine.Asset.Manager.generateTextureHandle' starts at
--      'firstAllocatableTextureHandle', so no real texture is ever
--      given this id.
--   2. Every bindless-registration path runs
--      'Engine.Graphics.Vulkan.Texture.Handle.checkRegistrableHandle'
--      first, so a producer synthesising a literal zero handle cannot
--      point table entry 0 at a real slot either. That same guard is
--      where a handle id past the END of the table is refused (#1699);
--      this reservation is its other half.
missingTextureHandle ∷ TextureHandle
missingTextureHandle = TextureHandle 0

-- | The lowest id 'Engine.Asset.Manager.generateTextureHandle' may hand
--   out: one past 'missingTextureHandle', so a fresh process's FIRST
--   texture allocation is @TextureHandle 1@ (#1696).
firstAllocatableTextureHandle ∷ Int
firstAllocatableTextureHandle = 1

-- | Is this the reserved missing-texture sentinel rather than a real,
--   allocatable texture id? (#1696)
isMissingTextureHandle ∷ TextureHandle → Bool
isMissingTextureHandle handle = handle ≡ missingTextureHandle

newtype FontHandle = FontHandle Int
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

-- | Type class for opaque asset handles backed by integer indices
class (Eq h, Ord h, Show h) ⇒ AssetHandle h where
  -- | Construct a handle from an integer index
  fromInt ∷ Int → h
  -- | Extract the underlying integer index
  toInt ∷ h → Int

instance AssetHandle TextureHandle where
  fromInt       = TextureHandle
  toInt (TextureHandle n) = n

instance AssetHandle FontHandle where
  fromInt       = FontHandle
  toInt (FontHandle n) = n
