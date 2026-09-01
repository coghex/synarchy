-- | Pure decision logic for PUBLISHING a freshly uploaded bindless
--   texture (#1690).
--
--   "Engine.Graphics.Vulkan.Texture.Bindless"'s @registerTexture@ /
--   @registerPinnedTexture@ answer @Left TextureSlotsExhausted@ when
--   "Engine.Graphics.Vulkan.Texture.Slot" is out of slots: no descriptor
--   write, no @btsHandleMap@ entry, no handle→slot table entry. The
--   upload's GPU objects exist and nothing samples them. #1696's
--   @Left TextureHandleReserved@ and #1699's
--   @Left TextureHandleUnrepresentable@ leave exactly the same
--   wreckage, so all three refusals are decided here identically — what
--   differs between them is only the diagnosis a reader is handed,
--   which 'registrationFailureMessage' already owns.
--
--   Continuing from that as though registration had succeeded is worse
--   than a missing texture, because the bookkeeping a success writes is
--   what every LATER request consults. A disk load that published
--   @AssetReady@ plus an @apAssetPaths@ entry for a slot-less atlas
--   converted that path into a permanently success-reporting no-op: the
--   next request for it took the cache hit instead of retrying, and
--   published @AssetReady@ again. A transient upload (world preview,
--   zoom atlas) that published its unusable replacement first DISPOSED
--   the previous generation, destroying a texture that was still being
--   drawn.
--
--   So the outcome of a registration is a decision, and this module is
--   that decision with no 'Vulkan.Core10.Device' in it: what a request
--   publishes, what it releases, and what the pool's maps retain. Every
--   caller routes through it — @Engine.Scripting.Lua.Message.Texture@
--   for disk loads and their aliases,
--   @Engine.Scripting.Lua.Message.WorldTexture@ for the two transients —
--   so the choice is assertable headlessly
--   ("Test.Headless.Graphics.BindlessPublish"), the way #1281's
--   "Engine.Graphics.Vulkan.Texture.Release" made the release decision
--   assertable.
--
--   'World.Render.BloodQuads' keeps its own inline handling: it is the
--   precedent this module generalizes, and a decal that fails to
--   register is simply not uploaded — it publishes nothing anywhere to
--   decide about. Being the one PER-FRAME registration path, it also
--   asks 'Engine.Asset.Manager.textureHandleNamespaceSpent' before
--   uploading at all, so #1699's permanent refusal costs it one cycle
--   rather than one per frame.
--   "Engine.Graphics.Vulkan.Texture.DefaultFaceMap" deliberately falls
--   back to slot 0 for its 1×1 init-time face map and is not a
--   publication boundary either.
module Engine.Graphics.Vulkan.Texture.Publish
  ( UploadSampler(..)
  , TexturePublish(..)
  , classifyRegistration
  , cachedAliasPublish
  , aliasPublish
  , UnregistrableRequest(..)
  , classifyRequestHandle
  , publishedSlot
  , publishFailureReason
  , publishRegisteredEntries
  , GpuCleanupStep(..)
  , failedUploadCleanup
  , TransientPublish(..)
  , classifyTransientRegistration
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import Engine.Asset.Handle (TextureHandle)
import Engine.Graphics.Vulkan.Texture.Handle
  (BindlessTextureHandle(..), HandleAddressing(..)
  , TextureRegistrationFailure(..), checkRegistrableHandle
  , registrationFailureMessage)
import Engine.Graphics.Vulkan.Texture.Policy (UploadSampler(..))
import Engine.Graphics.Vulkan.Texture.Slot (TextureSlot(..))

-- | The TERMINAL outcome of one texture request. Every request reaches
--   exactly one of these, so nothing waiting on a request stalls.
data TexturePublish
  = PublishRegistered !Word32
    -- ^ Registration took this bindless slot index. Everything the
    --   success path writes is written, unchanged from before #1690.
  | PublishFailed !Text
    -- ^ Terminally failed, with the reason to record and report. NOTHING
    --   a success boundary observes is written: no @AssetReady@, no
    --   @apTextureAtlases@ entry, no @apAssetPaths@ entry, no texture
    --   size entry, and no @LuaAssetLoaded@. The handle settles on
    --   'Engine.Asset.Handle.AssetFailed' and the failure is announced
    --   on its own channel instead.
  deriving (Show, Eq)

-- | Decide the outcome of a FRESH upload's registration.
--
--   The failure reason is 'registrationFailureMessage' verbatim — the
--   SAME text
--   'Engine.Graphics.Vulkan.Texture.Bindless.registerTextureImpl'
--   already logged for this refusal (#1696). So the state a handle
--   settles on, the message Lua is told, and the line in the log all
--   read alike, and a refused sentinel is never restated here as slot
--   exhaustion.
classifyRegistration
  ∷ TextureHandle
  → Text  -- ^ Caller provenance: a path, or a stable source label
  → Either TextureRegistrationFailure BindlessTextureHandle
  → TexturePublish
classifyRegistration handle source = \case
  Right bindlessHandle → PublishRegistered (tsIndex (bthSlot bindlessHandle))
  Left failure → PublishFailed (registrationFailureMessage failure handle source)

-- | Decide the outcome of a CACHE HIT: a second stable handle for an
--   atlas the pool already holds, resolved through the CANONICAL
--   owner's @btsHandleMap@ entry.
--
--   'Nothing' covers both ways that resolution can come up empty — the
--   atlas has no mapping, or there is no bindless system at all — and
--   both are failures. Publishing @AssetReady@ for an atlas holding no
--   slot is precisely the poisoned cache hit #1690 removes: the handle
--   resolves to slot 0 (the undefined texture) on the shader read path,
--   so reporting it loaded is a lie that no later request can correct.
--
--   Scope: this decides only what a hit whose OWN handle
--   'classifyRequestHandle' already admitted does. That judgement runs
--   first, in
--   'Engine.Scripting.Lua.Message.Texture.duplicateCachedTextureHandle'
--   itself, because this path owns its own @btsHandleMap@ insertion
--   rather than going through @registerTexture@ — and a refused
--   sentinel is dropped there outright rather than settled here: it is
--   a producer defect in a handle that no request ever legitimately
--   names, not a request whose upload failed.
cachedAliasPublish
  ∷ Text  -- ^ the cached atlas's path
  → Maybe BindlessTextureHandle
     -- ^ the CANONICAL owner's @btsHandleMap@ mapping, if it has one
  → TexturePublish
cachedAliasPublish path = \case
  Just bindlessHandle → PublishRegistered (tsIndex (bthSlot bindlessHandle))
  Nothing → PublishFailed
      ("cached texture holds no bindless slot: " <> path)

-- | What a request must do about its OWN handle before it can be
--   published at all — 'Nothing' when the handle is fine.
--
--   Two paths reach a publication decision without ever calling
--   'Engine.Graphics.Vulkan.Texture.Bindless.registerTexture', so
--   nothing upstream has judged their handle for them: the cached-atlas
--   fast path, which owns its own @btsHandleMap@ insertion, and an
--   in-batch deduped ALIAS, which takes the canonical request's result
--   verbatim. Both must be judged HERE, on the handle they themselves
--   name.
--
--   That is not a formality for the alias. A batch can carry two
--   unrepresentable requests for one uncached path — @65536@ folded
--   into by @65537@ — and inheriting the canonical's reason would give
--   the second request a log line, an 'AssetFailed' state and a Lua
--   notification whose text names a DIFFERENT handle than the
--   notification's own id (#1699). Worse across kinds: a canonical that
--   ran out of SLOTS would tell an unrepresentable alias its slots ran
--   out. One terminal outcome per request means one diagnostic per
--   request too.
data UnregistrableRequest
  = RequestDropped !Text
    -- ^ #1696's reserved sentinel: log the reason and write nothing at
    --   all, not even a terminal failure. A zero handle names no real
    --   request, so there is nothing to settle.
  | RequestSettled !Text
    -- ^ #1699's unrepresentable id: log the reason AND settle the
    --   request on it. This is a real request that something is waiting
    --   on, and the id is unusable for the rest of the process, so it
    --   must end terminally rather than silently.
  deriving (Show, Eq)

-- | Judge one request's own handle. The reason is
--   'registrationFailureMessage' verbatim, on THIS handle and THIS
--   source, so it reads like every other refusal in the engine.
classifyRequestHandle
  ∷ TextureHandle
  → Text  -- ^ Caller provenance: this request's own path or label
  → Maybe UnregistrableRequest
classifyRequestHandle handle source =
  case checkRegistrableHandle ShaderAddressable handle of
    Right ()                  → Nothing
    Left TextureHandleReserved →
      Just (RequestDropped (reasonFor TextureHandleReserved))
    Left failure              → Just (RequestSettled (reasonFor failure))
  where
    reasonFor failure = registrationFailureMessage failure handle source

-- | Resolve an in-batch deduped ALIAS against the canonical request it
--   was folded into.
--
--   One batch can name the same path more than once;
--   'Engine.Scripting.Lua.Message.Texture' uploads it ONCE and gives
--   every other request the canonical result. So a canonical that failed
--   fails its aliases with the same reason, and a canonical missing from
--   the batch's results entirely — which should be unreachable — fails
--   them too rather than leaving them loading forever. An alias is a
--   request like any other and must reach a terminal outcome.
--
--   Reached only AFTER 'classifyRequestHandle' has admitted the alias's
--   own handle: what is inherited here is the canonical's UPLOAD
--   outcome, never a verdict on a handle this request does not name
--   (#1699).
aliasPublish ∷ Text → Maybe (Either Text α) → Either Text α
aliasPublish path = \case
  Just outcome → outcome
  Nothing → Left ("canonical texture missing for deduped alias: " <> path)

-- | The bindless slot to record on a published atlas
--   (@Engine.Asset.Types.taBindlessSlot@), and 'Nothing' for a failure —
--   which never reaches an atlas, because a failure builds none.
publishedSlot ∷ TexturePublish → Maybe Word32
publishedSlot = \case
  PublishRegistered slot → Just slot
  PublishFailed _ → Nothing

-- | The failure reason to settle the handle and the notification on.
publishFailureReason ∷ TexturePublish → Maybe Text
publishFailureReason = \case
  PublishRegistered _ → Nothing
  PublishFailed reason → Just reason

-- | Fold a batch's per-request outcomes into one of the pool's maps —
--   @apTextureAtlases@ keyed by 'Engine.Asset.Base.AssetId', and
--   @apAssetPaths@ keyed by
--   'Engine.Graphics.Vulkan.Texture.Policy.TextureCacheKey' (path AND
--   'UploadSampler' policy since #2075; path alone before it).
--
--   Only a 'PublishRegistered' request contributes an entry. A
--   'PublishFailed' one contributes NOTHING, which is three separate
--   prohibitions and not just the obvious one: it must not insert its
--   own entry, must not overwrite an existing entry, and must not DELETE
--   one. That last is load-bearing for @apAssetPaths@ in both eras. The
--   policy in the key now keeps the OTHER policy's same-path entry at a
--   different key entirely, so a failure cannot reach it by accident —
--   but a failed retry of a path this SAME policy already has a live
--   canonical for still shares its key exactly, and evicting that would
--   turn one failed re-request into the loss of a slot that is
--   registered, sampled and working.
publishRegisteredEntries
  ∷ Ord k ⇒ [(k, α, TexturePublish)] → Map.Map k α → Map.Map k α
publishRegisteredEntries results entries0 = foldl' step entries0 results
  where
    step entries (key, value, PublishRegistered _) = Map.insert key value entries
    step entries (_, _, PublishFailed _) = entries

-- | One GPU object a failed registration hands back.
data GpuCleanupStep
  = CleanupImageView
    -- ^ The image view created for this upload.
  | CleanupImage
    -- ^ The image (and its memory) the view referenced.
  | ReleasePinnedSampler
    -- ^ The refcounted sampler reference a pinned upload ACQUIRED.
    --   A registered pinned slot keeps its reference for the whole
    --   session; an unregistered one holds a reference to a sampler
    --   nothing will ever sample through.
  deriving (Show, Eq)

-- | Everything a failed registration releases, IN ORDER.
--
--   View before image, because the view references it — the same order
--   'World.Render.BloodQuads' and the transient-disposal path already
--   use. Only a pinned upload acquired a sampler reference of its own;
--   an 'UploadGlobalSampler' one borrowed @btsTextureSampler@, which
--   belongs to the bindless system and outlives every request.
failedUploadCleanup ∷ UploadSampler → [GpuCleanupStep]
failedUploadCleanup = \case
  UploadGlobalSampler → [CleanupImageView, CleanupImage]
  UploadPinnedNearest → [CleanupImageView, CleanupImage, ReleasePinnedSampler]

-- | What a TRANSIENT upload — the world preview thumbnail and the zoom
--   background atlas — does with its registration result.
data TransientPublish
  = TransientReplace !Word32
    -- ^ Registered into this slot: dispose the previous generation,
    --   record this one, and publish it.
  | TransientRetain !Text
    -- ^ Failed: the PREVIOUS generation stays live and still named by
    --   @previewTexture@ \/ @zoomAtlasTexture@, this upload's own GPU
    --   objects are released ('failedUploadCleanup', both transients
    --   being pinned), and nothing is published — no
    --   'Engine.Scripting.Lua.Types.LuaWorldPreviewReady', no new
    --   'World.Render.Zoom.Types.ZoomAtlasInfo'. Disposing a live,
    --   still-renderable generation in favour of one nothing can sample
    --   is strictly worse than keeping the stale picture until the next
    --   world init or load regenerates it.
  deriving (Show, Eq)

-- | Decide a transient upload's outcome. Like 'classifyRegistration',
--   the reason is 'registrationFailureMessage' verbatim.
classifyTransientRegistration
  ∷ TextureHandle
  → Text  -- ^ Caller provenance: the transient's stable source label
  → Either TextureRegistrationFailure BindlessTextureHandle
  → TransientPublish
classifyTransientRegistration handle source = \case
  Right bindlessHandle → TransientReplace (tsIndex (bthSlot bindlessHandle))
  Left failure → TransientRetain (registrationFailureMessage failure handle source)
