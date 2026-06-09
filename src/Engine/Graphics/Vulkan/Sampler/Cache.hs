{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Deduplicated, refcounted Vulkan sampler operations.
--
--   The engine needs only a handful of DISTINCT samplers (a texture
--   filter — nearest or linear — and a font sampler), but historically
--   minted a fresh 'Sampler' per atlas, per font, and per filter-toggle,
--   none shared. This cache collapses them: a 'SamplerKind' names each
--   distinct config, 'acquireSampler' hands out the one shared 'Sampler'
--   for that kind (creating it on first use), and 'releaseSampler' drops
--   a reference, destroying the underlying 'VkSampler' only when the last
--   user lets go. Net: at most one 'VkSampler' per 'SamplerKind' alive at
--   a time, regardless of how many textures/fonts load or how often the
--   filter is toggled.
--
--   These are plain 'IO' (not 'EngineM') so they drop directly into the
--   IO cleanup closures resources already store ('taCleanup' /
--   'ttCleanup' / 'fCleanup') — a resource releases its sampler in the
--   same closure that frees its image/view. Main-thread only (like all
--   GPU resource work). The cache lives in an 'IORef' on 'EngineEnv'
--   (@samplerCacheRef@). Types come from
--   "Engine.Graphics.Vulkan.Sampler.Types".
module Engine.Graphics.Vulkan.Sampler.Cache
  ( acquireSampler
  , releaseSampler
  , destroySamplerCache
  , module Engine.Graphics.Vulkan.Sampler.Types
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import Data.IORef
import Engine.Graphics.Vulkan.Sampler.Types
import Vulkan.Core10

-- | Get the shared sampler for a kind, creating it on first use and
--   bumping its refcount otherwise.
acquireSampler ∷ Device → IORef SamplerCache → SamplerKind → IO Sampler
acquireSampler dev ref kind = do
    cache ← readIORef ref
    case Map.lookup kind cache of
        Just (s, n) → do
            writeIORef ref (Map.insert kind (s, n + 1) cache)
            pure s
        Nothing → do
            s ← createSampler dev (samplerInfoFor kind) Nothing
            writeIORef ref (Map.insert kind (s, 1) cache)
            pure s

-- | Drop a reference. When the last user releases a kind, the device is
--   idled (the sampler may still be referenced by in-flight frames —
--   acquire/release happen at load/unload/toggle, never per-frame, so the
--   stall is invisible) and the 'VkSampler' is destroyed.
releaseSampler ∷ Device → IORef SamplerCache → SamplerKind → IO ()
releaseSampler dev ref kind = do
    cache ← readIORef ref
    case Map.lookup kind cache of
        Nothing → pure ()
        Just (s, n)
            | n > 1 → writeIORef ref (Map.insert kind (s, n - 1) cache)
            | otherwise → do
                deviceWaitIdle dev
                destroySampler dev s Nothing
                writeIORef ref (Map.delete kind cache)

-- | Destroy every cached sampler (shutdown). Caller must have idled the
--   device already (shutdown does a single 'deviceWaitIdle' up front).
destroySamplerCache ∷ Device → IORef SamplerCache → IO ()
destroySamplerCache dev ref = do
    cache ← readIORef ref
    forM_ (Map.elems cache) $ \(s, _) → destroySampler dev s Nothing
    writeIORef ref emptySamplerCache
