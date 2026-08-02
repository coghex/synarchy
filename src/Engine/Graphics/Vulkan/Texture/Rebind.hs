-- | Pure decision logic for a global texture-filter change (#983).
--
--   @setTextureFilter@ ("Engine.Graphics.Vulkan.Texture.Bindless")
--   repaints every bindless slot when the global filter toggles. WHICH
--   slot gets which image view and which sampler — and which handle→slot
--   mappings cannot be repainted at all — is decided here, with no
--   @Device@ and no descriptor writes, so it stays testable headlessly.
--
--   The distinction this module encodes is that @btsImageViews@ is
--   __canonical-image bookkeeping__, not a per-handle mirror of
--   @btsHandleMap@. Cached-atlas aliases ("Engine.Asset.Manager",
--   "Engine.Scripting.Lua.Message.Texture") deliberately add a handle to
--   @btsHandleMap@ only, sharing the original texture's slot and image
--   view. Such an alias is normal operation and must rebind through its
--   canonical owner silently. A handle whose slot has no canonical image
--   view at all — reachable when an atlas is unloaded and its aliases
--   outlive it — is genuinely unrecoverable and is reported instead.
module Engine.Graphics.Vulkan.Texture.Rebind
  ( SlotRebind(..)
  , FilterRebindPlan(..)
  , planFilterRebind
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Vulkan.Texture.Handle (BindlessTextureHandle(..))
import Engine.Graphics.Vulkan.Texture.Slot (TextureSlot(..))
import Vulkan.Core10 (ImageView, Sampler)

-- | One descriptor write: repaint @srSlot@ with @srImageView@ bound to
--   @srSampler@.
data SlotRebind = SlotRebind
  { srSlot      ∷ !Word32     -- ^ Bindless array index to repaint
  , srImageView ∷ !ImageView  -- ^ The canonical owner's image view
  , srSampler   ∷ !Sampler    -- ^ Canonical owner's pinned sampler, else the new global one
  } deriving (Show, Eq)

-- | Everything a filter change needs to do beyond the undefined-image
--   pre-pass: the slots it can repaint, and the handle→slot mappings it
--   cannot.
data FilterRebindPlan = FilterRebindPlan
  { frpRebinds       ∷ ![SlotRebind]
    -- ^ One entry per recoverable slot, ascending by slot index. Slots
    --   are deduplicated: aliases sharing a slot produce a single write
    --   driven by the canonical owner, so the result does not depend on
    --   @btsHandleMap@ iteration order.
  , frpUnrecoverable ∷ ![(TextureHandle, Word32)]
    -- ^ Handle→slot mappings whose slot has no canonical image view,
    --   ascending by handle. The caller warns about these; the slots
    --   stay on the undefined image with the new sampler that the
    --   pre-pass already wrote.
  } deriving (Show, Eq)

-- | Decide the per-slot repaint for a filter change.
--
--   A slot's canonical owner is a handle that is in @btsHandleMap@ AND
--   owns an entry in @btsImageViews@. Every other handle mapping to that
--   slot is an alias and rebinds through it — including for the sampler,
--   so a pinned canonical owner keeps its pin no matter where an alias
--   sorts relative to it.
planFilterRebind
  ∷ Map.Map TextureHandle BindlessTextureHandle
    -- ^ @btsHandleMap@: every handle, canonical or alias
  → Map.Map TextureHandle ImageView
    -- ^ @btsImageViews@: canonical owners only
  → Map.Map TextureHandle Sampler
    -- ^ @btsPinned@: handles that must keep their own sampler
  → Sampler
    -- ^ The newly acquired global sampler
  → FilterRebindPlan
planFilterRebind handleMap imageViews pinned newSampler = FilterRebindPlan
  { frpRebinds =
      [ SlotRebind slotIdx view (Map.findWithDefault newSampler owner pinned)
      | (slotIdx, (owner, view)) ← Map.toList canonicalBySlot
      ]
  , frpUnrecoverable =
      [ (texHandle, slotOf bindlessHandle)
      | (texHandle, bindlessHandle) ← Map.toList handleMap
      , not (Map.member (slotOf bindlessHandle) canonicalBySlot)
      ]
  }
  where
    -- Slot → the handle that owns its image view. 'Map.toList' is
    -- ascending, and the combining function keeps the earlier entry, so
    -- the (not expected, but representable) case of two owners on one
    -- slot resolves deterministically to the lower handle.
    canonicalBySlot ∷ Map.Map Word32 (TextureHandle, ImageView)
    canonicalBySlot = Map.fromListWith (\_new old → old)
      [ (slotOf bindlessHandle, (texHandle, view))
      | (texHandle, bindlessHandle) ← Map.toList handleMap
      , Just view ← [Map.lookup texHandle imageViews]
      ]

    slotOf ∷ BindlessTextureHandle → Word32
    slotOf = tsIndex . bthSlot
