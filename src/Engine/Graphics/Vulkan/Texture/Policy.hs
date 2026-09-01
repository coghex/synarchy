-- | Which sampler a texture upload's bindless slot is registered with,
--   and the cache key that keeps one slot per @(path, policy)@ pair.
--
--   A module of its own, with no local dependencies, because the
--   declaration now travels the WHOLE length of a load: a Lua caller
--   names it ("Engine.Scripting.Lua.API.Graphics"), the queued request
--   carries it ("Engine.Scripting.Lua.Types"), the message loop bursts
--   by it ("Engine.Scripting.Lua.Message"), the asset pool keys its
--   path cache on it ("Engine.Asset.Types") and the upload path acts on
--   it ("Engine.Scripting.Lua.Message.Texture"). Those namespaces
--   cannot all import "Engine.Graphics.Vulkan.Texture.Publish", where
--   'UploadSampler' used to live; that module re-exports it, so its own
--   importers are unchanged.
module Engine.Graphics.Vulkan.Texture.Policy
  ( UploadSampler(..)
  , TextureCacheKey(..)
  , parseUploadPolicy
  , uploadPolicyNames
  ) where

import UPrelude

-- | Which sampler a freshly uploaded slot is registered with.
--
--   The two policies are mutually exclusive per batch, which is why
--   'Engine.Scripting.Lua.Message' bursts a run of ordinary requests
--   only as far as the policy stays the same, and bursts atlas requests
--   separately from both.
data UploadSampler
    = UploadGlobalSampler
      -- ^ Follow the shared global sampler: the slot is repainted by a
      --   runtime 'Engine.Graphics.Vulkan.Texture.Bindless.setTextureFilter'
      --   toggle. Scene art — everything drawn in the world, and the
      --   backward-compatible answer for a load that declares nothing
      --   (#2075, D-4).
    | UploadPinnedNearest
      -- ^ Pinned to NEAREST regardless of the global filter. UI chrome
      --   and the icons the UI\/HUD layers draw use it (#2075), because
      --   they are pixel art presented at a fixed scale and were never
      --   meant to follow the player's scene-art filter. Gameplay unit
      --   atlases are scene art and use 'UploadGlobalSampler' (#2085).
    deriving (Show, Eq, Ord)

-- | The upload cache's key: one canonical slot per PATH per POLICY.
--
--   Keying on the path alone was the bug #2075 closes. A slot's sampler
--   is fixed by whichever policy uploaded it, so one entry per path
--   cannot describe a genuinely dual-use texture — @utility\/white.png@
--   is drawn by both the UI and the world — and the surviving entry
--   belonged to whichever policy uploaded LAST. Alternating policies
--   therefore re-uploaded every time and left the earlier canonical
--   unreferenced by the cache, so a scene→UI→scene→UI sequence could
--   allocate four canonical slots for one file.
--
--   With the policy in the key, each policy keeps its own reusable
--   canonical: that sequence allocates exactly two slots and every
--   later request of either policy is a cache hit aliasing its own.
data TextureCacheKey = TextureCacheKey
  { tckPath   ∷ !Text
    -- ^ The texture's filesystem path, exactly as requested.
  , tckPolicy ∷ !UploadSampler
    -- ^ The policy the slot behind this entry was registered under.
  } deriving (Show, Eq, Ord)

-- | The policy a Lua caller named, or 'Nothing' for a value that names
--   no policy at all.
--
--   Deliberately total in its refusal (#2075): an unrecognised token is
--   'Nothing', never a silent 'UploadGlobalSampler'. Only OMITTING the
--   argument selects the backward-compatible scene policy, so a typo
--   ("UI", "pinned", "nearest") is reported rather than quietly loading
--   the texture into the wrong category — which would look correct
--   until the player toggled the filter.
parseUploadPolicy ∷ Text → Maybe UploadSampler
parseUploadPolicy = \case
  "scene" → Just UploadGlobalSampler
  "ui"    → Just UploadPinnedNearest
  _       → Nothing

-- | Every token 'parseUploadPolicy' accepts, for the refusal message.
uploadPolicyNames ∷ [Text]
uploadPolicyNames = ["scene", "ui"]
