-- | Device-free coverage for the final release of a bindless texture
--   (#1281).
--
--   The defect this pins: a cached-atlas ALIAS
--   ("Engine.Scripting.Lua.Message.Texture") is a second stable
--   'TextureHandle' resolving to an existing atlas's slot, recorded in
--   @btsHandleMap@ only. Releasing that atlas used to unregister the one
--   CANONICAL handle and hand the slot straight back
--   ("Engine.Graphics.Vulkan.Texture.Slot" prefers a freed slot for the
--   very next allocation), leaving every alias still resolving to it.
--   Nothing on the shader read path validates a slot generation, so once
--   'Engine.Asset.Manager.unloadAsset' gains a caller such an alias would
--   sample whatever unrelated texture claimed the slot next — wrong
--   content, not the undefined texture.
--
--   The release ITSELF needs a live 'Vulkan.Core10.Device' (descriptor
--   writes, mapped-memory pokes), which the headless suite cannot
--   provide. The DECISION — which handles are invalidated, which slots
--   go back, and how many times — is the pure
--   "Engine.Graphics.Vulkan.Texture.Release" seam, and the bookkeeping
--   transition is the same 'dropReleasedHandles' \/ 'freeReleasedSlots'
--   production applies, so all of it is assertable here.
module Test.Headless.Graphics.BindlessRelease (spec) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import Test.Hspec
import Engine.Asset.Base (AssetId(..))
import Engine.Asset.Handle (TextureHandle(..), AssetState(..))
import Engine.Graphics.Vulkan.Texture.Handle (BindlessTextureHandle(..))
import Engine.Graphics.Vulkan.Texture.Slot
  (TextureSlot(..), TextureSlotAllocator, createSlotAllocator, allocateSlot)
import Engine.Graphics.Vulkan.Texture.Release
  (TextureReleasePlan(..), releaseOwnerHandles, planTextureRelease
  , dropReleasedHandles, freeReleasedSlots, resolveHandleSlot)
import qualified Data.Set as Set
import Vulkan.Core10 (ImageView(..), Sampler(..))

-- | Atlas A's canonical owner, and two cached aliases of it. Both
--   aliases live in @btsHandleMap@ ONLY — that absence from
--   @btsImageViews@ is what a cache hit looks like — and 'aliasA2'
--   deliberately sorts AFTER atlas B's handles so an implementation that
--   stopped at the first non-A handle would miss it.
canonicalA, aliasA1, aliasA2 ∷ TextureHandle
canonicalA = TextureHandle 1
aliasA1    = TextureHandle 2
aliasA2    = TextureHandle 5

-- | A second, unrelated atlas with an alias of its own. Nothing about
--   releasing A may touch either.
canonicalB, aliasB ∷ TextureHandle
canonicalB = TextureHandle 3
aliasB     = TextureHandle 4

-- | The texture registered into the slot A's release freed.
newcomer ∷ TextureHandle
newcomer = TextureHandle 6

viewA, viewB, viewNew ∷ ImageView
viewA   = ImageView 0xA1
viewB   = ImageView 0xB2
viewNew = ImageView 0xC3

pinnedSampler ∷ Sampler
pinnedSampler = Sampler 0x200

-- | The allocator, and the two slots it really handed out — obtained
--   from the production allocator rather than fabricated, so the
--   generation bookkeeping a reuse depends on is genuine.
baseAllocator ∷ TextureSlotAllocator
slotA, slotB ∷ TextureSlot
(baseAllocator, slotA, slotB) =
  case allocateSlot (createSlotAllocator 8) of
    Nothing → error "allocateSlot: first slot unavailable"
    Just (a, alloc1) → case allocateSlot alloc1 of
      Nothing → error "allocateSlot: second slot unavailable"
      Just (b, alloc2) → (alloc2, a, b)

-- | @btsHandleMap@: five handles over two slots.
handleMap ∷ Map.Map TextureHandle BindlessTextureHandle
handleMap = Map.fromList
  [ (canonicalA, BindlessTextureHandle slotA canonicalA)
  , (aliasA1,    BindlessTextureHandle slotA aliasA1)
  , (aliasA2,    BindlessTextureHandle slotA aliasA2)
  , (canonicalB, BindlessTextureHandle slotB canonicalB)
  , (aliasB,     BindlessTextureHandle slotB aliasB)
  ]

-- | @btsImageViews@: canonical owners only, by construction.
imageViews ∷ Map.Map TextureHandle ImageView
imageViews = Map.fromList [(canonicalA, viewA), (canonicalB, viewB)]

-- | @btsPinned@: A's canonical owner keeps its own sampler.
pinned ∷ Map.Map TextureHandle Sampler
pinned = Map.fromList [(canonicalA, pinnedSampler)]

-- | @apTextureHandles@ — where @AssetReady@ actually lives. Every
--   handle has its own entry, alias included.
handleStates ∷ Map.Map TextureHandle (AssetState AssetId)
handleStates = Map.fromList
  [ (h, AssetReady aid [])
  | (h, aid) ← [ (canonicalA, AssetId 10), (aliasA1, AssetId 10)
               , (aliasA2, AssetId 10), (canonicalB, AssetId 11)
               , (aliasB, AssetId 11) ]
  ]

-- | The texture size map, likewise keyed by the stable handle.
textureSizes ∷ HM.HashMap TextureHandle (Int, Int)
textureSizes = HM.fromList [ (h, (32, 32)) | h ← Map.keys handleMap ]

-- | The shader's handle→slot table as the GPU reads it: production
--   holds it in persistently-mapped memory and pokes an entry at every
--   @btsHandleMap@ mutation. Zero-initialised, so an unwritten id
--   already means slot 0 (the undefined texture).
type ShaderTable = Map.Map TextureHandle Word32

shaderTable ∷ ShaderTable
shaderTable = fmap (tsIndex ∘ bthSlot) handleMap

-- | What @releaseTextureHandles@ pokes: zero for every invalidated
--   handle.
applyReleaseWrites ∷ TextureReleasePlan → ShaderTable → ShaderTable
applyReleaseWrites plan tbl =
  foldl' (\m texHandle → Map.insert texHandle 0 m) tbl (trpInvalidated plan)

-- | What a later registration pokes: this handle now names this slot.
registerInTable ∷ TextureHandle → Word32 → ShaderTable → ShaderTable
registerInTable = Map.insert

-- | The size-map purge production performs from the plan.
dropReleasedSizes ∷ TextureReleasePlan
                  → HM.HashMap TextureHandle (Int, Int)
                  → HM.HashMap TextureHandle (Int, Int)
dropReleasedSizes plan m = foldl' (flip HM.delete) m (trpInvalidated plan)

spec ∷ Spec
spec = do
  describe "the final release of an atlas with cached aliases" $ do
    let plan = planTextureRelease [canonicalA] handleMap
        aHandles = [canonicalA, aliasA1, aliasA2]

    it "invalidates the canonical handle AND every alias of its slot" $
      -- The whole defect in one assertion: the pre-#1281 path invalidated
      -- only 'canonicalA'.
      trpInvalidated plan `shouldBe` [canonicalA, aliasA1, aliasA2]

    it "returns the shared slot to the allocator exactly once" $
      trpFreedSlots plan `shouldBe` [slotA]

    it "zeroes the shader handle→slot entry of every old handle" $ do
      let table = applyReleaseWrites plan shaderTable
      map (\h → Map.lookup h table) aHandles
        `shouldBe` map Just [0, 0, 0]

    it "removes every old handle from the bindless bookkeeping" $ do
      let handleMap'  = dropReleasedHandles plan handleMap
          imageViews' = dropReleasedHandles plan imageViews
          pinned'     = dropReleasedHandles plan pinned
      map (`Map.member` handleMap') aHandles `shouldBe` [False, False, False]
      Map.keys imageViews' `shouldBe` [canonicalB]
      pinned' `shouldBe` Map.empty

    it "removes every old handle from the pool and size bookkeeping" $ do
      -- @AssetReady@ is the value stored in @apTextureHandles@; there is
      -- no separate ready map, and an alias has its own entry in both.
      let states' = dropReleasedHandles plan handleStates
          sizes'  = dropReleasedSizes plan textureSizes
      map (`Map.member` states') aHandles `shouldBe` [False, False, False]
      map (`HM.member` sizes') aHandles `shouldBe` [False, False, False]

    it "leaves an unrelated atlas and its own alias untouched" $ do
      let handleMap' = dropReleasedHandles plan handleMap
          states'    = dropReleasedHandles plan handleStates
          sizes'     = dropReleasedSizes plan textureSizes
      map (`resolveHandleSlot` handleMap') [canonicalB, aliasB]
        `shouldBe` [tsIndex slotB, tsIndex slotB]
      map (`Map.member` states') [canonicalB, aliasB] `shouldBe` [True, True]
      map (`HM.member` sizes') [canonicalB, aliasB] `shouldBe` [True, True]

  describe "reusing the slot a released atlas gave back" $ do
    let plan = planTextureRelease [canonicalA] handleMap
        handleMap' = dropReleasedHandles plan handleMap
        allocator' = freeReleasedSlots plan baseAllocator

    it "hands the freed slot straight to the next registration" $
      -- Not incidental — this immediacy is what makes a surviving alias
      -- resolve to unrelated CONTENT rather than to nothing.
      case allocateSlot allocator' of
        Nothing → expectationFailure "no slot available for re-registration"
        Just (slot, _) → do
          tsIndex slot `shouldBe` tsIndex slotA
          tsGeneration slot `shouldNotBe` tsGeneration slotA

    it "cannot hand that one slot out twice" $
      -- A double free (one per alias, say) would let two live textures
      -- believe they own it.
      case allocateSlot allocator' ⌦ \(_, next) → allocateSlot next of
        Nothing → expectationFailure "no second slot available"
        Just (slot, _) → tsIndex slot `shouldNotBe` tsIndex slotA

    it "still resolves every old handle to slot 0, never to the newcomer" $
      case allocateSlot allocator' of
        Nothing → expectationFailure "no slot available for re-registration"
        Just (slot, _) → do
          let reused = Map.insert newcomer
                (BindlessTextureHandle slot newcomer) handleMap'
              table = registerInTable newcomer (tsIndex slot)
                        (applyReleaseWrites plan shaderTable)
          map (`resolveHandleSlot` reused) [canonicalA, aliasA1, aliasA2]
            `shouldBe` [0, 0, 0]
          map (\h → Map.lookup h table) [canonicalA, aliasA1, aliasA2]
            `shouldBe` map Just [0, 0, 0]
          resolveHandleSlot newcomer reused `shouldBe` tsIndex slot
          -- Unrelated view bookkeeping for the reused slot is the
          -- newcomer's alone.
          Map.keys (Map.insert newcomer viewNew
                     (dropReleasedHandles plan imageViews))
            `shouldBe` [canonicalB, newcomer]

  describe "a non-final release" $ do
    -- 'unloadAsset' only reaches the release seam at refcount ≤ 0; above
    -- it, nothing is planned and nothing may move.
    let plan = planTextureRelease [] handleMap

    it "plans no invalidation and frees no slot" $ do
      trpInvalidated plan `shouldBe` []
      trpFreedSlots plan `shouldBe` []

    it "leaves every mapping, handle state and size exactly as it was" $ do
      dropReleasedHandles plan handleMap `shouldBe` handleMap
      dropReleasedHandles plan imageViews `shouldBe` imageViews
      dropReleasedHandles plan pinned `shouldBe` pinned
      dropReleasedHandles plan handleStates `shouldBe` handleStates
      dropReleasedSizes plan textureSizes `shouldBe` textureSizes

    it "leaves the slot allocated: the next registration gets a fresh one" $
      case allocateSlot (freeReleasedSlots plan baseAllocator) of
        Nothing → expectationFailure "no slot available"
        Just (slot, _) → do
          tsIndex slot `shouldNotBe` tsIndex slotA
          tsIndex slot `shouldNotBe` tsIndex slotB

  describe "releasing several atlases at once" $ do
    -- The drain path ('cleanupAssetManager') releases every atlas in one
    -- call, and must still free each slot once.
    let plan = planTextureRelease [canonicalA, canonicalB] handleMap

    it "invalidates every handle over every released slot" $
      trpInvalidated plan
        `shouldBe` [canonicalA, aliasA1, canonicalB, aliasB, aliasA2]

    it "frees each slot exactly once, ascending" $
      trpFreedSlots plan `shouldBe` [slotA, slotB]

    it "empties the bindless bookkeeping it owned" $ do
      dropReleasedHandles plan handleMap `shouldBe` Map.empty
      dropReleasedHandles plan imageViews `shouldBe` Map.empty
      dropReleasedHandles plan pinned `shouldBe` Map.empty

  describe "releasing a canonical handle with no bindless mapping left" $ do
    -- Defensive: an owner already absent from @btsHandleMap@ names no
    -- slot, so there is nothing to hand back and no alias set to derive
    -- — but its own pool-side entries must still be purged rather than
    -- silently kept because the bindless half found nothing to do.
    let plan = planTextureRelease [canonicalA]
                 (Map.delete canonicalA handleMap)

    it "frees no slot" $
      trpFreedSlots plan `shouldBe` []

    it "still invalidates the owner itself" $
      trpInvalidated plan `shouldBe` [canonicalA]

    it "purges the owner's pool and size entries" $ do
      let states' = dropReleasedHandles plan handleStates
          sizes'  = dropReleasedSizes plan textureSizes
      Map.member canonicalA states' `shouldBe` False
      HM.member canonicalA sizes' `shouldBe` False

  describe "an atlas whose registration never got a bindless slot" $ do
    -- @duplicateCachedTextureHandle@ writes an alias's @AssetReady@
    -- state, size entry and refcount bump UNCONDITIONALLY; only the
    -- @btsHandleMap@ insertion is conditional on the canonical owner
    -- holding a slot. So a cache hit against a slot-exhausted atlas (or
    -- one taken with no bindless system at all) leaves POOL-ONLY
    -- aliases, which no slot-derived sweep can see.
    let -- Neither the canonical nor its aliases ever reached the
        -- bindless map; only atlas B is registered.
        starvedMap = Map.fromList
          [ (canonicalB, BindlessTextureHandle slotB canonicalB)
          , (aliasB,     BindlessTextureHandle slotB aliasB)
          ]
        owners = releaseOwnerHandles (Set.singleton (AssetId 10))
                   [canonicalA] handleStates
        plan = planTextureRelease owners starvedMap

    it "finds the pool-only aliases through their AssetReady asset id" $
      owners `shouldBe` [canonicalA, aliasA1, aliasA2]

    it "purges every one of them from the pool and size bookkeeping" $ do
      let states' = dropReleasedHandles plan handleStates
          sizes'  = dropReleasedSizes plan textureSizes
          aHandles = [canonicalA, aliasA1, aliasA2]
      map (`Map.member` states') aHandles `shouldBe` [False, False, False]
      map (`HM.member` sizes') aHandles `shouldBe` [False, False, False]

    it "frees no slot, because the atlas never held one" $
      trpFreedSlots plan `shouldBe` []

    it "leaves the registered atlas and its own alias alone" $ do
      dropReleasedHandles plan starvedMap `shouldBe` starvedMap
      let states' = dropReleasedHandles plan handleStates
      map (`Map.member` states') [canonicalB, aliasB] `shouldBe` [True, True]

    it "ignores handles that name no asset at all" $ do
      -- Only @AssetReady@ carries an 'AssetId'; a loading or failed
      -- handle must not be swept into another atlas's release.
      let loading = TextureHandle 20
          failed  = TextureHandle 21
          states  = Map.union handleStates $ Map.fromList
            [ (loading, AssetLoading "pending.png" [] 0.5)
            , (failed,  AssetFailed "boom") ]
      releaseOwnerHandles (Set.singleton (AssetId 10)) [canonicalA] states
        `shouldBe` [canonicalA, aliasA1, aliasA2]

    it "sweeps every drained atlas at once, canonical handles included" $
      -- The 'cleanupResources' shape: every atlas id, every canonical
      -- handle. Nothing may be left behind.
      releaseOwnerHandles (Set.fromList [AssetId 10, AssetId 11])
        [canonicalA, canonicalB] handleStates
        `shouldBe` [canonicalA, aliasA1, canonicalB, aliasB, aliasA2]
