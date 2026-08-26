-- | Pure decision logic for the FINAL release of a bindless texture
--   (#1281).
--
--   Releasing a texture is not a one-handle operation, because a stable
--   'TextureHandle' is not a one-to-one name for a bindless slot.
--   Cached-atlas deduplication ("Engine.Scripting.Lua.Message.Texture")
--   gives a second 'Engine.Asset.Base.AssetId' resolving to an
--   already-loaded atlas its own stable handle, and records it in
--   @btsHandleMap@ pointing at the CANONICAL owner's slot — deliberately
--   with no @btsImageViews@ / @btsPinned@ entry of its own (the
--   canonical-image model "Engine.Graphics.Vulkan.Texture.Rebind"
--   relies on). Every such alias resolves through the same slot the
--   canonical owner does.
--
--   So freeing that slot while any alias survives is not merely
--   untidy: "Engine.Graphics.Vulkan.Texture.Slot" hands a freed slot
--   straight to the next registration, and nothing on the shader read
--   path validates a slot generation, so a surviving alias would sample
--   whatever unrelated texture claimed the slot next — wrong content,
--   not undefined. This module decides which handles a release has to
--   invalidate and which slots it may hand back, with no @Device@ and no
--   descriptor writes, so the invariant is assertable headlessly.
module Engine.Graphics.Vulkan.Texture.Release
  ( TextureReleasePlan(..)
  , releaseOwnerHandles
  , planTextureRelease
  , dropReleasedHandles
  , freeReleasedSlots
  , resolveHandleSlot
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Engine.Asset.Base (AssetId)
import Engine.Asset.Handle (TextureHandle(..), AssetState(..))
import Engine.Graphics.Vulkan.Texture.Handle (BindlessTextureHandle(..))
import Engine.Graphics.Vulkan.Texture.Slot
  (TextureSlot(..), TextureSlotAllocator, freeSlot)

-- | Everything a final release must do to the bindless bookkeeping.
data TextureReleasePlan = TextureReleasePlan
  { trpInvalidated ∷ ![TextureHandle]
    -- ^ EVERY stable handle the release invalidates, ascending: the
    --   canonical owners it was asked for plus every cached-atlas alias
    --   resolving to a slot they own. Each of these must have its
    --   shader handle→slot entry zeroed (slot 0 = undefined) and its
    --   bindless\/pool\/size bookkeeping removed. An owner with no
    --   @btsHandleMap@ entry left is still listed — its pool-side
    --   bookkeeping needs clearing either way.
  , trpFreedSlots  ∷ ![TextureSlot]
    -- ^ The slots to hand back to the allocator, ascending by index and
    --   each present exactly ONCE however many handles named it. A
    --   double free would let one slot be allocated to two live
    --   textures.
  } deriving (Show, Eq)

-- | The complete owner set to hand 'planTextureRelease' for a release of
--   these atlases: their canonical handles
--   ('Engine.Asset.Types.taTextureHandle') plus every handle the POOL
--   records as belonging to one of them.
--
--   That second half is not redundant with the slot sweep below. A
--   POOL-ONLY alias — real in @apTextureHandles@ and the texture size
--   map, absent from @btsHandleMap@, and therefore invisible to any
--   slot-derived sweep — is swept in here because @AssetReady@ carries
--   the atlas's 'AssetId', which names it whether or not a slot does.
--
--   Until #1690 the loader MADE such aliases: @duplicateCachedTextureHandle@
--   ("Engine.Scripting.Lua.Message.Texture") wrote an alias's
--   @AssetReady@ state, size entry and refcount bump unconditionally,
--   so a cache hit against an atlas whose registration had run out of
--   bindless slots published one. #1690 stopped that at the source —
--   such an atlas is never published in the first place, and a cache hit
--   that cannot resolve a slot now FAILS
--   ("Engine.Graphics.Vulkan.Texture.Publish") instead of reporting a
--   load. This asset-id sweep stays regardless, and is not dead weight:
--   it is what keeps the release decision independent of whether a
--   handle ever held a slot, so an owner the bindless half finds nothing
--   to do for still has its pool-side bookkeeping purged rather than
--   silently kept.
--
--   Only @AssetReady@ names an asset; a handle still loading or failed
--   carries no id to match, so neither is swept in.
releaseOwnerHandles
  ∷ Set.Set AssetId
    -- ^ The atlases being released
  → [TextureHandle]
    -- ^ Their canonical handles
  → Map.Map TextureHandle (AssetState AssetId)
    -- ^ @apTextureHandles@
  → [TextureHandle]
releaseOwnerHandles assetIds canonical handleStates = Set.toAscList $
  Set.fromList canonical `Set.union` Set.fromList
    [ texHandle
    | (texHandle, AssetReady assetId _) ← Map.toList handleStates
    , assetId `Set.member` assetIds
    ]

-- | Decide what a final release invalidates.
--
--   The owners are the handles being released ('releaseOwnerHandles').
--   Their slots come out of @btsHandleMap@, and then every handle in
--   that map naming one of those slots is swept in too — which is the
--   rest of the alias set, because a cached alias is defined by sharing
--   its canonical owner's slot.
planTextureRelease
  ∷ [TextureHandle]
    -- ^ Canonical owners being released
  → Map.Map TextureHandle BindlessTextureHandle
    -- ^ @btsHandleMap@: every handle, canonical and alias alike
  → TextureReleasePlan
planTextureRelease owners handleMap = TextureReleasePlan
  { trpInvalidated = Set.toAscList (Set.fromList owners `Set.union` sharers)
  , trpFreedSlots  = Map.elems releasedSlots
  }
  where
    -- Keyed by slot INDEX, so two owners that somehow share one slot
    -- still yield a single free.
    releasedSlots ∷ Map.Map Word32 TextureSlot
    releasedSlots = Map.fromList
      [ (tsIndex slot, slot)
      | owner ← owners
      , Just bindlessHandle ← [Map.lookup owner handleMap]
      , let slot = bthSlot bindlessHandle
      ]

    sharers ∷ Set.Set TextureHandle
    sharers = Set.fromList
      [ texHandle
      | (texHandle, bindlessHandle) ← Map.toList handleMap
      , Map.member (tsIndex (bthSlot bindlessHandle)) releasedSlots
      ]

-- | Drop every invalidated handle from a handle-keyed bindless map
--   (@btsHandleMap@, @btsImageViews@, @btsPinned@).
dropReleasedHandles
  ∷ TextureReleasePlan → Map.Map TextureHandle α → Map.Map TextureHandle α
dropReleasedHandles plan m =
  foldl' (flip Map.delete) m (trpInvalidated plan)

-- | Return the released slots to the allocator, once each.
freeReleasedSlots ∷ TextureReleasePlan → TextureSlotAllocator → TextureSlotAllocator
freeReleasedSlots plan alloc =
  foldl' (flip freeSlot) alloc (trpFreedSlots plan)

-- | Resolve a handle to its bindless slot the way the shader's
--   handle→slot table does: a handle with no mapping is slot 0, the
--   undefined texture. This is the read 'trpInvalidated' has to make
--   answer 0 — before the freed slot can be handed to anything else.
resolveHandleSlot
  ∷ TextureHandle → Map.Map TextureHandle BindlessTextureHandle → Word32
resolveHandleSlot texHandle =
  maybe 0 (tsIndex ∘ bthSlot) ∘ Map.lookup texHandle
