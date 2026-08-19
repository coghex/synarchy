-- | Bindless texture handles with slot management
module Engine.Graphics.Vulkan.Texture.Handle
  ( BindlessTextureHandle(..)
  , toBindlessHandle
  ) where

import UPrelude
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Vulkan.Texture.Slot (TextureSlot(..))

-- | A texture handle for the bindless system
-- Contains slot index for shader access and generation for validity checking
data BindlessTextureHandle = BindlessTextureHandle
  { bthSlot   ∷ !TextureSlot
  , bthHandle ∷ !TextureHandle  -- Original asset handle for lookup
  } deriving (Show, Eq, Ord)

-- | Create a bindless handle from a slot and asset handle
toBindlessHandle ∷ TextureSlot → TextureHandle → BindlessTextureHandle
toBindlessHandle slot handle = BindlessTextureHandle
  { bthSlot   = slot
  , bthHandle = handle
  }

