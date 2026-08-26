-- | Device-free coverage for what a FAILED bindless registration
--   publishes (#1690).
--
--   The defect this pins: 'Engine.Graphics.Vulkan.Texture.Slot' returns
--   'Nothing' at capacity and
--   'Engine.Graphics.Vulkan.Texture.Bindless.registerTextureImpl'
--   propagates that as a system with no descriptor write, no
--   @btsHandleMap@ entry and no handle→slot table entry. Three of its
--   callers then continued as though it had succeeded: the disk loader
--   wrote @AssetReady@, both pool caches, the size entry and
--   @LuaAssetLoaded@ — poisoning the path cache so every LATER request
--   for that path took a lying cache hit — and the two transient
--   uploads discarded the result outright, disposing a still-renderable
--   previous generation in favour of one nothing can sample.
--
--   The registration ITSELF needs a live 'Vulkan.Core10.Device'
--   (descriptor writes, mapped-memory pokes), which the headless suite
--   cannot provide. The DECISION — what is published, what is released,
--   and what the pool's maps retain — is the pure
--   "Engine.Graphics.Vulkan.Texture.Publish" seam every caller now
--   routes through, so all of it is assertable here, the way #1281's
--   "Engine.Graphics.Vulkan.Texture.Release" made the release decision
--   assertable.
--
--   Exhaustion here is REAL, not a hand-written 'Nothing': every case
--   drives an actual 'TextureSlotAllocator' whose capacity the batch
--   runs off the end of, so a change to the allocator's own
--   at-capacity answer fails these too.
module Test.Headless.Graphics.BindlessPublish (spec) where

import UPrelude
import qualified Data.Map.Strict as Map
import Test.Hspec
import Engine.Asset.Base (AssetId(..))
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Vulkan.Texture.Handle
  (BindlessTextureHandle, toBindlessHandle)
import Engine.Graphics.Vulkan.Texture.Slot
  (TextureSlot(..), TextureSlotAllocator, createSlotAllocator, allocateSlot)
import Engine.Graphics.Vulkan.Texture.Publish

-- * A batch, modelled exactly as the loader runs one

-- | One fresh upload request, as
--   'Engine.Scripting.Lua.Message.Texture.handleLoadTextureBatchWith'
--   sees it after the cache/alias classification has run.
data Request = Request
  { reqHandle  ∷ !TextureHandle
  , reqPath    ∷ !Text
  , reqAssetId ∷ !AssetId
  }

-- | Everything one batch leaves behind.
data BatchResult = BatchResult
  { brOutcomes ∷ ![(TextureHandle, TexturePublish)]
    -- ^ Every request's terminal outcome, in batch order. Every request
    --   appears exactly once: that is requirement 4's "one terminal,
    --   observable outcome" in list form.
  , brPaths    ∷ !(Map.Map Text AssetId)
    -- ^ @apAssetPaths@ after the batch.
  , brAtlases  ∷ !(Map.Map AssetId Text)
    -- ^ @apTextureAtlases@ after the batch, standing in for the atlas
    --   record by its path (nothing here needs a 'Vulkan.Core10.Image').
  , brCleanups ∷ ![(TextureHandle, [GpuCleanupStep])]
    -- ^ The GPU objects each FAILED request handed back, in order.
  }

-- | Run one batch the way the loader does: allocate, classify, and let
--   the outcome alone decide what is written.
runBatch
  ∷ UploadSampler
  → TextureSlotAllocator
  → Map.Map Text AssetId       -- ^ @apAssetPaths@ before the batch
  → Map.Map AssetId Text       -- ^ @apTextureAtlases@ before the batch
  → [Request]
  → BatchResult
runBatch policy allocator0 paths0 atlases0 requests = BatchResult
  { brOutcomes = [ (reqHandle r, outcome) | (r, outcome) ← results ]
  , brPaths    = publishRegisteredEntries
      [ (reqPath r, reqAssetId r, outcome) | (r, outcome) ← results ] paths0
  , brAtlases  = publishRegisteredEntries
      [ (reqAssetId r, reqPath r, outcome) | (r, outcome) ← results ] atlases0
  , brCleanups =
      [ (reqHandle r, failedUploadCleanup policy)
      | (r, PublishFailed _) ← results
      ]
  }
  where
    results = reverse (fst (foldl' step ([], allocator0) requests))
    step (acc, allocator) request =
      case allocateSlot allocator of
        Just (slot, allocator') →
          ( (request, classifyRegistration (reqPath request)
                        (Just (toBindlessHandle slot (reqHandle request)))) : acc
          , allocator' )
        Nothing →
          ( (request, classifyRegistration (reqPath request) Nothing) : acc
          , allocator )

-- | The pool holds ONE allocatable slot: slot 0 is the reserved
--   undefined texture, so a two-slot allocator hands out exactly one.
oneSlotAllocator ∷ TextureSlotAllocator
oneSlotAllocator = createSlotAllocator 2

-- | An allocator already at capacity: every request in a batch run
--   against it fails.
exhaustedAllocator ∷ TextureSlotAllocator
exhaustedAllocator = createSlotAllocator 1

request ∷ Int → Text → Word32 → Request
request handle path assetId =
  Request (TextureHandle handle) path (AssetId assetId)

isFailure ∷ TexturePublish → Bool
isFailure = \case
  PublishFailed _ → True
  PublishRegistered _ → False

-- | The bindless mapping a REGISTERED canonical owner has, for the
--   cache-hit cases.
registeredMapping ∷ BindlessTextureHandle
registeredMapping = toBindlessHandle (TextureSlot 4 0) (TextureHandle 1)

spec ∷ Spec
spec = do
  describe "a first-time load that runs out of bindless slots" $ do
    let result = runBatch UploadGlobalSampler exhaustedAllocator
                   Map.empty Map.empty [request 1 "wall.png" 10]

    it "reaches a terminal FAILURE rather than reporting a load" $
      map snd (brOutcomes result) `shouldSatisfy` all isFailure

    it "names the path in the reason, so the log and the AssetFailed \
       \state say WHICH request died" $
      map snd (brOutcomes result) `shouldBe`
        [PublishFailed "no bindless slot available to register wall.png"]

    it "publishes no atlas" $
      brAtlases result `shouldBe` Map.empty

    it "leaves the path cache empty, so the path is not poisoned" $
      brPaths result `shouldBe` Map.empty

    it "hands its GPU objects back, view before image" $
      map snd (brCleanups result) `shouldBe` [[CleanupImageView, CleanupImage]]

    it "records no slot on anything, because it holds none" $
      map (publishedSlot ∘ snd) (brOutcomes result) `shouldBe` [Nothing]

  describe "a repeat request for a path whose upload failed" $ do
    -- Requirement 5: the FIRST failure is what decides this. Because it
    -- inserted no @apAssetPaths@ entry, the classification pass that
    -- opens the next batch finds nothing to hit and the request is a
    -- fresh upload again — which succeeds the moment a slot exists.
    let firstTry = runBatch UploadGlobalSampler exhaustedAllocator
                     Map.empty Map.empty [request 1 "wall.png" 10]
        -- The next batch's cache lookup, verbatim: a path in
        -- @apAssetPaths@ is a cache hit, and nothing else is.
        takesCacheHit = Map.member "wall.png" (brPaths firstTry)
        secondTry = runBatch UploadGlobalSampler oneSlotAllocator
                      (brPaths firstTry) (brAtlases firstTry)
                      [request 2 "wall.png" 11]

    it "is not a cache hit, because the failure cached nothing" $
      takesCacheHit `shouldBe` False

    it "re-uploads and succeeds once a slot is available" $
      map snd (brOutcomes secondTry) `shouldBe` [PublishRegistered 1]

    it "now caches the path against the atlas that really registered" $
      brPaths secondTry `shouldBe` Map.fromList [("wall.png", AssetId 11)]

  describe "partial exhaustion inside one batch" $ do
    -- The batch has one slot and three requests. Everything BEFORE
    -- capacity is reached must be completely unaffected by what happens
    -- after it.
    let result = runBatch UploadGlobalSampler oneSlotAllocator
                   Map.empty Map.empty
                   [ request 1 "first.png"  10
                   , request 2 "second.png" 11
                   , request 3 "third.png"  12
                   ]

    it "registers the requests that fit and fails only the rest" $
      map snd (brOutcomes result) `shouldBe`
        [ PublishRegistered 1
        , PublishFailed "no bindless slot available to register second.png"
        , PublishFailed "no bindless slot available to register third.png"
        ]

    it "gives every request in the batch exactly one outcome" $
      map fst (brOutcomes result) `shouldBe`
        [TextureHandle 1, TextureHandle 2, TextureHandle 3]

    it "keeps the successful request's atlas and path entry" $ do
      brAtlases result `shouldBe` Map.fromList [(AssetId 10, "first.png")]
      brPaths result `shouldBe` Map.fromList [("first.png", AssetId 10)]

    it "cleans up after the failures only" $
      map fst (brCleanups result) `shouldBe` [TextureHandle 2, TextureHandle 3]

  describe "a pinned-atlas batch that runs out of slots" $ do
    let result = runBatch UploadPinnedNearest exhaustedAllocator
                   Map.empty Map.empty [request 7 "walk.png" 20]

    it "releases the sampler reference it acquired, after the image" $
      -- A REGISTERED pinned slot keeps its reference for the whole
      -- session (D-6). One that never registered is holding a
      -- reference to a sampler nothing will ever sample through.
      map snd (brCleanups result) `shouldBe`
        [[CleanupImageView, CleanupImage, ReleasePinnedSampler]]

    it "differs from the global-sampler policy only in that release" $
      -- The global policy borrows @btsTextureSampler@, which belongs to
      -- the bindless system and outlives every request.
      failedUploadCleanup UploadGlobalSampler `shouldBe`
        [CleanupImageView, CleanupImage]

    it "publishes nothing either way" $ do
      brAtlases result `shouldBe` Map.empty
      brPaths result `shouldBe` Map.empty

  describe "a cross-policy re-upload that fails" $ do
    -- @apAssetPaths@ is keyed by path alone while a slot's sampler is
    -- fixed by whichever policy first uploaded it, so a same-path entry
    -- under the OPPOSITE policy is valid and is exactly why this request
    -- is a fresh upload rather than a cache hit. Its failure must not
    -- insert, overwrite, or DELETE that entry.
    let existingPaths = Map.fromList [("walk.png", AssetId 5)]
        existingAtlases = Map.fromList [(AssetId 5, "walk.png")]
        result = runBatch UploadPinnedNearest exhaustedAllocator
                   existingPaths existingAtlases [request 9 "walk.png" 21]

    it "leaves the other policy's cache entry exactly as it found it" $
      brPaths result `shouldBe` existingPaths

    it "leaves the other policy's atlas exactly as it found it" $
      brAtlases result `shouldBe` existingAtlases

    it "still reports its own request as failed" $
      map snd (brOutcomes result) `shouldSatisfy` all isFailure

    it "lets the failed policy retry later without disturbing the other" $
      -- The retry allocates its OWN slot and its OWN asset id; the
      -- surviving entry is simply overwritten by a real registration,
      -- which is what a legitimate cross-policy re-upload does.
      brPaths (runBatch UploadPinnedNearest oneSlotAllocator
                 (brPaths result) (brAtlases result) [request 9 "walk.png" 21])
        `shouldBe` Map.fromList [("walk.png", AssetId 21)]

  describe "an in-batch deduped alias" $ do
    -- One batch can name the same path twice; the loader uploads it
    -- ONCE and hands every other request the canonical result.
    it "inherits the canonical request's success" $
      aliasPublish "wall.png" (Just (Right (AssetId 10, "atlas")))
        `shouldBe` Right (AssetId 10, "atlas")

    it "inherits the canonical request's failure, with its reason" $
      aliasPublish "wall.png"
        (Just (Left "no bindless slot available to register wall.png"
                 ∷ Either Text (AssetId, Text)))
        `shouldBe` Left "no bindless slot available to register wall.png"

    it "fails rather than staying loading when the canonical vanished" $
      -- Before #1690 this branch only logged, leaving the alias in
      -- 'AssetLoading' forever — a request with no terminal outcome.
      aliasPublish "wall.png" (Nothing ∷ Maybe (Either Text (AssetId, Text)))
        `shouldBe` Left "canonical texture missing for deduped alias: wall.png"

  describe "a cache hit against an atlas that holds no slot" $ do
    it "publishes the hit when the canonical owner really is registered" $
      cachedAliasPublish "wall.png" (Just registeredMapping)
        `shouldBe` PublishRegistered 4

    it "fails instead of reporting AssetReady for a slot-less atlas" $
      -- This is the poisoned cache hit itself: the handle would resolve
      -- to slot 0, the undefined texture, and reporting it loaded is a
      -- lie no later request could correct.
      cachedAliasPublish "wall.png" Nothing
        `shouldBe` PublishFailed "cached texture holds no bindless slot: wall.png"

    it "answers the same when there is no bindless system at all" $
      -- Both ways the resolution comes up empty mean the same thing to
      -- whoever samples the handle.
      publishFailureReason (cachedAliasPublish "wall.png" Nothing)
        `shouldSatisfy` isJust

  describe "a transient upload that fails while a generation is live" $ do
    it "retains the previous generation instead of replacing it" $
      classifyTransientRegistration "the world preview" Nothing
        `shouldBe` TransientRetain
          "no bindless slot available to register the world preview; \
          \keeping the previous generation"

    it "replaces it when the registration really took a slot" $
      classifyTransientRegistration "the world preview"
        (Just registeredMapping) `shouldBe` TransientReplace 4

    it "decides the zoom atlas the same way" $ do
      classifyTransientRegistration "the zoom atlas"
        (Just registeredMapping) `shouldBe` TransientReplace 4
      classifyTransientRegistration "the zoom atlas" Nothing
        `shouldBe` TransientRetain
          "no bindless slot available to register the zoom atlas; \
          \keeping the previous generation"

    it "releases the failed upload's own GPU objects, sampler included" $
      -- Both transients register pinned (preview NEAREST, zoom LINEAR),
      -- so both acquired a reference of their own.
      failedUploadCleanup UploadPinnedNearest `shouldBe`
        [CleanupImageView, CleanupImage, ReleasePinnedSampler]

  describe "a device with capacity behaves exactly as before" $ do
    let result = runBatch UploadGlobalSampler (createSlotAllocator 16)
                   Map.empty Map.empty
                   [ request 1 "a.png" 10
                   , request 2 "b.png" 11
                   , request 3 "c.png" 12
                   ]

    it "registers every request into consecutive slots" $
      map snd (brOutcomes result) `shouldBe`
        [PublishRegistered 1, PublishRegistered 2, PublishRegistered 3]

    it "records each slot on its own atlas" $
      map (publishedSlot ∘ snd) (brOutcomes result)
        `shouldBe` [Just 1, Just 2, Just 3]

    it "caches every path and every atlas" $ do
      brPaths result `shouldBe` Map.fromList
        [("a.png", AssetId 10), ("b.png", AssetId 11), ("c.png", AssetId 12)]
      brAtlases result `shouldBe` Map.fromList
        [(AssetId 10, "a.png"), (AssetId 11, "b.png"), (AssetId 12, "c.png")]

    it "cleans up nothing" $
      brCleanups result `shouldBe` []
