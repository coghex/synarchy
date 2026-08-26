-- | Bindless texture handles with slot management
module Engine.Graphics.Vulkan.Texture.Handle
  ( BindlessTextureHandle(..)
  , toBindlessHandle
  , TextureRegistrationFailure(..)
  , checkRegistrableHandle
  , registrationFailureMessage
  ) where

import UPrelude
import Engine.Asset.Handle
  (TextureHandle(..), isMissingTextureHandle, missingTextureHandle, toInt)
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

-- | Why a bindless registration produced no handle.
--
--   The two are kept apart because they send a reader to different
--   places: exhaustion is a capacity story, while a refused sentinel is
--   a producer defect. Reporting the latter as the former is exactly
--   the misdiagnosis this type exists to make unrepresentable (#1696).
data TextureRegistrationFailure
  = TextureSlotsExhausted
    -- ^ The bindless slot allocator has no free slot left.
  | TextureHandleReserved
    -- ^ The handle named 'missingTextureHandle', the reserved
    --   missing-texture sentinel.
  deriving (Show, Eq)

-- | The ONE guard every bindless-registration path runs before it
--   allocates a slot, writes a descriptor, pokes the handle→slot table,
--   or records any @btsHandleMap@ \/ @btsImageViews@ \/ @btsPinned@,
--   asset-pool or texture-size bookkeeping (#1696).
--
--   Both registration entry points in
--   "Engine.Graphics.Vulkan.Texture.Bindless" and the cached-alias fast
--   path in "Engine.Scripting.Lua.Message.Texture" — the one that
--   writes @btsHandleMap@ directly rather than going through
--   'Engine.Graphics.Vulkan.Texture.Bindless.registerTexture' — share
--   it, so @handleToSlot[0]@ keeps resolving to the undefined slot for
--   the whole process lifetime however a zero handle is synthesised.
--
--   'Engine.Asset.Manager.generateTextureHandle' should already make
--   this unreachable; the guard is what stops a future producer
--   reintroducing the defect with a literal.
checkRegistrableHandle ∷ TextureHandle → Either TextureRegistrationFailure ()
checkRegistrableHandle handle
  | isMissingTextureHandle handle = Left TextureHandleReserved
  | otherwise                     = Right ()

-- | The diagnostic a failed registration logs: which failure, the handle
--   id it named, and where the texture came from — a file path for
--   file-backed uploads, a stable source label for procedural
--   registrations (the default face map, the world preview, the zoom
--   atlas, blood decals) (#1696).
registrationFailureMessage ∷ TextureRegistrationFailure
                           → TextureHandle
                           → Text  -- ^ Caller provenance
                           → Text
registrationFailureMessage failure handle source = case failure of
  TextureHandleReserved →
    "Refused to register reserved missing-texture sentinel handle "
      <> tshow (toInt handle) <> " for " <> source
      <> " -- handleToSlot[" <> tshow (toInt missingTextureHandle)
      <> "] stays on the undefined slot (#1696)"
  TextureSlotsExhausted →
    "Failed to allocate bindless slot for texture handle "
      <> tshow (toInt handle) <> ": " <> source
