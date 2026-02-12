-- | Bindless texture handles with slot management
module Engine.Graphics.Vulkan.Texture.Handle
  ( BindlessTextureHandle(..)
  , toBindlessHandle
  , fromBindlessHandle
  , isUndefinedHandle
  , undefinedHandle
  ) where

import UPrelude
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Vulkan.Texture.Slot (TextureSlot(..), undefinedSlot)

-- | A texture handle for the bindless system
-- Contains slot index for shader access and generation for validity checking
data BindlessTextureHandle = BindlessTextureHandle
  { bthSlot   ∷ !TextureSlot
  , bthHandle ∷ !TextureHandle  -- Original asset handle for lookup
  } deriving (Show, Eq, Ord)

-- | The handle for the undefined/missing texture
undefinedHandle ∷ BindlessTextureHandle
undefinedHandle = BindlessTextureHandle
  { bthSlot   = undefinedSlot
  , bthHandle = TextureHandle 0
  }

-- | Check if this is the undefined texture handle
isUndefinedHandle ∷ BindlessTextureHandle → Bool
isUndefinedHandle h = bthSlot h ≡ undefinedSlot

-- | Create a bindless handle from a slot and asset handle
toBindlessHandle ∷ TextureSlot → TextureHandle → BindlessTextureHandle
toBindlessHandle slot handle = BindlessTextureHandle
  { bthSlot   = slot
  , bthHandle = handle
  }

-- | Get the slot index for shader use (this is what gets passed to the GPU)
fromBindlessHandle ∷ BindlessTextureHandle → Word32
fromBindlessHandle = tsIndex . bthSlot
