-- | Pure classification of one upload batch against the asset pool's
--   policy-scoped path cache (#2075).
--
--   The upload path ("Engine.Scripting.Lua.Message.Texture") decides
--   three things about every request before it touches a 'Device': does
--   an already-uploaded canonical slot serve it (a CACHE HIT), is it the
--   first request for its path in this batch (a FRESH upload), or does
--   an earlier request in this same batch already own the path (an
--   ALIAS)? That decision is what makes one file cost one slot per
--   policy instead of one per request, so it is the part worth proving
--   headlessly — the GPU work either side of it needs a real device.
module Engine.Asset.TextureCache
  ( BatchClassification(..)
  , classifyBatchRequests
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Engine.Asset.Base (AssetId)
import Engine.Asset.Handle (TextureHandle)
import Engine.Graphics.Vulkan.Texture.Policy
  (UploadSampler, TextureCacheKey(..))

-- | Every request in one batch, split by how it will be served. The
--   three lists partition the input and each is in BATCH ORDER.
--
--   @α@ is the cached-atlas payload — 'Engine.Asset.Types.TextureAtlas'
--   in production. Left a parameter because this decision never inspects
--   it beyond handing it to @reusable@, which keeps the type honest and
--   lets a test instantiate it at something comparable.
data BatchClassification α = BatchClassification
  { bcCached  ∷ ![(TextureHandle, AssetId, α)]
    -- ^ Served by a canonical slot this policy already uploaded: the
    --   handle is duplicated onto that slot and no image is read.
  , bcFresh   ∷ ![(TextureHandle, FilePath)]
    -- ^ The first request for its path in this batch with no reusable
    --   cache entry: it uploads, and becomes the canonical every later
    --   same-path request in this batch aliases.
  , bcAliases ∷ ![(TextureHandle, FilePath, TextureHandle)]
    -- ^ A later request for a path an EARLIER request in this same
    --   batch already claimed, paired with that claimant's handle.
  } deriving (Show, Eq)

-- | Classify one batch's requests.
--
--   The cache lookup is POLICY-SCOPED: the key is
--   @(path, this batch's policy)@, so an entry belonging to the other
--   policy is invisible here and stays untouched at its own key. That is
--   what gives each policy one stable, reusable canonical slot per path
--   — a scene→UI→scene→UI sequence for one file allocates exactly two
--   slots, and every request after the first of each policy is a hit.
--
--   @reusable@ is the caller's GPU-side consistency check on a candidate
--   hit (in production, @cacheEntryReusable@ against @btsPinned@:
--   was the canonical really registered the way its key claims?). It
--   should never refuse; when it does, the request falls through to a
--   fresh upload with its own slot rather than inheriting a sampler it
--   did not ask for.
--
--   Within-batch aliasing is keyed by PATH ALONE, and correctly so:
--   'Engine.Scripting.Lua.Message' bursts only requests that share a
--   policy, so every request reaching one call of this carries the same
--   one.
classifyBatchRequests
  ∷ UploadSampler
  → (α → Bool)                         -- ^ candidate-hit consistency check
  → Map.Map TextureCacheKey AssetId    -- ^ @apAssetPaths@
  → Map.Map AssetId α                  -- ^ @apTextureAtlases@
  → [(TextureHandle, FilePath)]
  → BatchClassification α
classifyBatchRequests policy reusable paths atlases requests =
    BatchClassification
      { bcCached  = reverse cached
      , bcFresh   = reverse fresh
      , bcAliases = reverse aliases
      }
  where
    (cached, fresh, aliases, _) =
        foldl' step ([], [], [], Map.empty) requests

    step (cached', fresh', aliases', seen) (handle, path) =
        let pathKey = T.pack path
            key = TextureCacheKey pathKey policy
            asFresh = ( cached', (handle, path) : fresh'
                      , aliases', Map.insert pathKey handle seen )
            asAliasOrFresh = case Map.lookup pathKey seen of
                Just canonical →
                    ( cached', fresh'
                    , (handle, path, canonical) : aliases', seen )
                Nothing → asFresh
        in case Map.lookup key paths of
            Just assetId → case Map.lookup assetId atlases of
                Just atlas
                    | reusable atlas →
                        ( (handle, assetId, atlas) : cached'
                        , fresh', aliases', seen )
                    | otherwise → asAliasOrFresh
                -- A path-cache entry whose atlas is gone. Not a hit and
                -- not an alias of anything: re-upload it.
                Nothing → asFresh
            Nothing → asAliasOrFresh
