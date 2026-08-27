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
  (BindlessTextureHandle(..), TextureRegistrationFailure
  , registrationFailureMessage)
import Engine.Graphics.Vulkan.Texture.Slot (TextureSlot(..))

-- | Which sampler a freshly uploaded slot is registered with.
--
--   The two policies are mutually exclusive per batch, which is why
--   'Engine.Scripting.Lua.Message' bursts atlas requests separately
--   from ordinary ones.
data UploadSampler
    = UploadGlobalSampler
      -- ^ Follow the shared global sampler: the slot is repainted by a
      --   runtime 'setTextureFilter' toggle. Every ordinary texture.
    | UploadPinnedNearest
      -- ^ Pinned to NEAREST regardless of the global filter (D-6).
      --   Compiled unit-animation atlases (#1259): a filter toggle must
      --   not start bilinearly resampling unit art, and on a sheet it
      --   would additionally bleed neighbouring cells across every
      --   frame edge.
    deriving (Show, Eq)

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
--   Scope: this decides only what a hit that PASSED #1696's
--   'checkRegistrableHandle' guard does. That guard runs first, in
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
--   @apAssetPaths@ keyed by path.
--
--   Only a 'PublishRegistered' request contributes an entry. A
--   'PublishFailed' one contributes NOTHING, which is three separate
--   prohibitions and not just the obvious one: it must not insert its
--   own entry, must not overwrite an existing entry, and must not DELETE
--   one. That last is load-bearing for @apAssetPaths@, whose key is the
--   path alone while a slot's sampler is fixed by whichever policy first
--   uploaded it: a same-path entry belonging to the OPPOSITE
--   'UploadSampler' policy is valid, is what makes this request a fresh
--   upload rather than a cache hit, and must survive this request's
--   failure intact so the policy that owns it keeps working and this one
--   can retry later.
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
