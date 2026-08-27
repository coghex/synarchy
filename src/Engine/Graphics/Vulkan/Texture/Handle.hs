-- | Bindless texture handles with slot management
module Engine.Graphics.Vulkan.Texture.Handle
  ( BindlessTextureHandle(..)
  , toBindlessHandle
  , TextureRegistrationFailure(..)
  , HandleAddressing(..)
  , checkRegistrableHandle
  , registrationFailureMessage
  ) where

import UPrelude
import Engine.Asset.Handle
  (TextureHandle(..), isMissingTextureHandle, missingTextureHandle, toInt)
import Engine.Graphics.Vulkan.Texture.Limits (handleSlotTableSize)
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
--   The three are kept apart because they send a reader to different
--   places: exhaustion is a capacity story that a later request can
--   still win, a refused sentinel is a producer defect, and an
--   unrepresentable id is a permanently spent handle namespace.
--   Reporting any of them as another is exactly the misdiagnosis this
--   type exists to make unrepresentable (#1696, #1699).
data TextureRegistrationFailure
  = TextureSlotsExhausted
    -- ^ The bindless slot allocator has no free slot left.
  | TextureHandleReserved
    -- ^ The handle named 'missingTextureHandle', the reserved
    --   missing-texture sentinel.
  | TextureHandleUnrepresentable
    -- ^ The handle id lies outside the shader's handle→slot table
    --   ('handleSlotTableSize'), so no vertex naming it could ever
    --   resolve to this registration's slot (#1699). Distinct from
    --   'TextureSlotsExhausted' the same way that one is distinct from
    --   'TextureHandleReserved': slots are recyclable and a later
    --   request can succeed, while the handle NAMESPACE is monotonic
    --   and never reset, so this refusal is permanent for the rest of
    --   the process.
  deriving (Show, Eq)

-- | How the SHADER will reach the texture this registration is for —
--   the one thing that decides whether the handle→slot table has to be
--   able to represent its handle id (#1699).
data HandleAddressing
  = ShaderAddressable
    -- ^ Geometry names the stable handle id and the fragment shader
    --   resolves it through @handleToSlot[]@ (#286). Every ordinary
    --   texture: disk loads and their cached aliases, the world preview
    --   and the zoom atlas, blood decals. An id the table cannot hold
    --   silently resolves to slot 0 (the undefined checkerboard), so
    --   such a registration is refused rather than reported.
  | SlotOnly
    -- ^ The caller consumes the descriptor SLOT INDEX directly and no
    --   vertex ever names the handle, so the table is not on the read
    --   path at all. The default face map is the only one
    --   ("Engine.Graphics.Vulkan.Texture.DefaultFaceMap"): its slot
    --   reaches the shader as @fragDefaultFaceMapSlot@ through the UBO,
    --   and its handle id is deliberately PAST the table so it can
    --   never collide with an allocated one. Refusing it would force
    --   that uniform to slot 0 and regress the very fallback it exists
    --   to provide.
  deriving (Show, Eq)

-- | The ONE guard every bindless-registration path runs before it
--   allocates a slot, writes a descriptor, pokes the handle→slot table,
--   or records any @btsHandleMap@ \/ @btsImageViews@ \/ @btsPinned@,
--   asset-pool or texture-size bookkeeping (#1696, #1699).
--
--   Both registration entry points in
--   "Engine.Graphics.Vulkan.Texture.Bindless" and the cached-alias fast
--   path in "Engine.Scripting.Lua.Message.Texture" — the one that
--   writes @btsHandleMap@ directly rather than going through
--   'Engine.Graphics.Vulkan.Texture.Bindless.registerTexture' — share
--   it, so @handleToSlot[0]@ keeps resolving to the undefined slot for
--   the whole process lifetime however a zero handle is synthesised.
--
--   It refuses two DIFFERENT unrepresentable ids, and the caller's
--   'HandleAddressing' is what decides whether the second one applies:
--
--   * the reserved sentinel, always (#1696).
--     'Engine.Asset.Manager.generateTextureHandle' should already make
--     that unreachable; the guard is what stops a future producer
--     reintroducing the defect with a literal.
--   * for a 'ShaderAddressable' registration only, an id the
--     handle→slot table cannot hold (#1699). That is the whole of
--     #1699's detection: it happens HERE, once, rather than being
--     re-derived at each publishing call site, and it happens before
--     any mutation so a refusal leaves nothing to unwind.
--
--   A 'SlotOnly' registration is exempt from the range half by
--   construction, not by an exemption list: nothing reads the table for
--   it, so an id outside the table costs it nothing.
checkRegistrableHandle ∷ HandleAddressing → TextureHandle
                       → Either TextureRegistrationFailure ()
checkRegistrableHandle addressing handle
  | isMissingTextureHandle handle = Left TextureHandleReserved
  | readsTable ∧ not (isRepresentableHandle handle)
                                  = Left TextureHandleUnrepresentable
  | otherwise                     = Right ()
  where
    readsTable = case addressing of
      ShaderAddressable → True
      SlotOnly          → False

-- | Can @handleToSlot[]@ hold an entry for this id at all? The table is
--   indexed by the raw handle id, so only @[0, 'handleSlotTableSize')@
--   is addressable (#286, #1699).
isRepresentableHandle ∷ TextureHandle → Bool
isRepresentableHandle handle = hid ≥ 0 ∧ hid < handleSlotTableSize
  where hid = toInt handle

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
  TextureHandleUnrepresentable →
    "Refused to register texture handle "
      <> tshow (toInt handle) <> " for " <> source
      <> " -- the shader handle->slot table holds only "
      <> tshow handleSlotTableSize <> " entries (ids 0.."
      <> tshow (handleSlotTableSize - 1)
      <> "), so this handle can never resolve to its slot (#1699)"
  TextureHandleReserved →
    "Refused to register reserved missing-texture sentinel handle "
      <> tshow (toInt handle) <> " for " <> source
      <> " -- handleToSlot[" <> tshow (toInt missingTextureHandle)
      <> "] stays on the undefined slot (#1696)"
  TextureSlotsExhausted →
    "Failed to allocate bindless slot for texture handle "
      <> tshow (toInt handle) <> ": " <> source
