-- | Device-free coverage for the bindless texture-filter rebind
--   decision (#983).
--
--   @setTextureFilter@ itself needs a live Vulkan 'Device' (it issues
--   @updateDescriptorSets@ and acquires/releases samplers), which the
--   headless suite has no way to provide. The DECISION it makes — which
--   slot is repainted with which image view and which sampler, and which
--   handle→slot mappings cannot be repainted at all — lives in the pure
--   'planFilterRebind' seam instead, so it is assertable here. The
--   Vulkan handle values below are fabricated 'Word64' newtypes; nothing
--   dereferences them.
--
--   The property under test is that @btsImageViews@ is CANONICAL-image
--   bookkeeping rather than a per-handle mirror of @btsHandleMap@:
--   cached-atlas aliases ("Engine.Asset.Manager",
--   "Engine.Scripting.Lua.Message.Texture") deliberately register a
--   handle in @btsHandleMap@ ONLY, sharing the original texture's slot
--   and image view. Treating that as corruption would warn about normal
--   operation; the genuinely broken case is a handle whose SLOT has no
--   canonical image view at all.
module Test.Headless.Graphics.BindlessRebind (spec) where

import UPrelude
import qualified Data.Map.Strict as Map
import Test.Hspec
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Vulkan.Texture.Handle (BindlessTextureHandle(..))
import Engine.Graphics.Vulkan.Texture.Slot (TextureSlot(..))
import Engine.Graphics.Vulkan.Texture.Rebind
  (FilterRebindPlan(..), SlotRebind(..), planFilterRebind)
import Vulkan.Core10 (ImageView(..), Sampler(..))

-- | The canonical atlas: owns slot 1 and the image view for it.
canonicalA ∷ TextureHandle
canonicalA = TextureHandle 1

-- | A cached alias of 'canonicalA'. Shares slot 1; deliberately absent
--   from @btsImageViews@ — this is what a cache hit looks like.
aliasA ∷ TextureHandle
aliasA = TextureHandle 2

-- | A handle whose slot (9) has no canonical image view at all: the
--   shape left behind when an atlas is unregistered but an alias that
--   shared its slot is not. Sorts BEFORE 'canonicalB' so the "keep
--   going" case below is real.
orphan ∷ TextureHandle
orphan = TextureHandle 3

-- | A second canonical atlas, owning slot 2.
canonicalB ∷ TextureHandle
canonicalB = TextureHandle 4

viewA, viewB ∷ ImageView
viewA = ImageView 0xA1
viewB = ImageView 0xB2

-- | The sampler a filter change has just acquired.
globalSampler ∷ Sampler
globalSampler = Sampler 0x100

-- | A sampler pinned to a specific handle (world preview / zoom atlas),
--   which must survive the filter change.
pinnedSampler ∷ Sampler
pinnedSampler = Sampler 0x200

-- | @btsHandleMap@ entry: this handle resolves to this slot index.
at ∷ TextureHandle → Word32 → (TextureHandle, BindlessTextureHandle)
at texHandle slotIdx =
  (texHandle, BindlessTextureHandle (TextureSlot slotIdx 0) texHandle)

spec ∷ Spec
spec = do
  describe "a cached alias sharing a canonical slot" $ do
    let handleMap  = Map.fromList [canonicalA `at` 1, aliasA `at` 1]
        imageViews = Map.fromList [(canonicalA, viewA)]
        plan = planFilterRebind handleMap imageViews Map.empty globalSampler

    it "rebinds the shared slot through its canonical image owner" $
      frpRebinds plan `shouldBe` [SlotRebind 1 viewA globalSampler]

    it "is not reported as unrecoverable (no warning)" $
      frpUnrecoverable plan `shouldBe` []

  describe "a handle→slot mapping with no canonical image view" $ do
    let handleMap = Map.fromList
          [canonicalA `at` 1, orphan `at` 9, canonicalB `at` 2]
        imageViews = Map.fromList [(canonicalA, viewA), (canonicalB, viewB)]
        plan = planFilterRebind handleMap imageViews Map.empty globalSampler

    it "reports that handle and that slot as unrecoverable" $
      frpUnrecoverable plan `shouldBe` [(orphan, 9)]

    it "keeps going: a later valid slot is still rebound" $
      -- 'orphan' sorts before 'canonicalB' in the handle map, so slot 2
      -- is only present if the unrecoverable entry did not abort the walk.
      frpRebinds plan `shouldBe`
        [ SlotRebind 1 viewA globalSampler
        , SlotRebind 2 viewB globalSampler
        ]

    it "does not invent a rebind for the unrecoverable slot" $
      map srSlot (frpRebinds plan) `shouldNotContain` [9]

  describe "a pinned canonical slot" $ do
    let handleMap  = Map.fromList [canonicalA `at` 1, canonicalB `at` 2]
        imageViews = Map.fromList [(canonicalA, viewA), (canonicalB, viewB)]
        pinned     = Map.fromList [(canonicalA, pinnedSampler)]
        plan = planFilterRebind handleMap imageViews pinned globalSampler

    it "keeps its pinned sampler while unpinned slots take the new one" $
      frpRebinds plan `shouldBe`
        [ SlotRebind 1 viewA pinnedSampler
        , SlotRebind 2 viewB globalSampler
        ]

    it "keeps the pin even when an alias of it sorts later" $ do
      -- The pre-#983 loop derived the sampler from the ITERATED handle,
      -- so 'aliasA' (which is not pinned and sorts after 'canonicalA')
      -- repainted the shared slot with the new global sampler and
      -- silently dropped the pin.
      let withAlias = Map.insert aliasA
            (BindlessTextureHandle (TextureSlot 1 0) aliasA) handleMap
          aliasPlan = planFilterRebind withAlias imageViews pinned globalSampler
      frpRebinds aliasPlan `shouldBe`
        [ SlotRebind 1 viewA pinnedSampler
        , SlotRebind 2 viewB globalSampler
        ]
      frpUnrecoverable aliasPlan `shouldBe` []
