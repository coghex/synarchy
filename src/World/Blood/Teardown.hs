{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | World-thread half of #788's blood-texture GPU teardown. When a world
--   page is removed or replaced (destroy / destroy-all / init / arena /
--   save-load), its 'wsBloodTextureHandlesRef' — the map of live bindless
--   'TextureHandle's + per-texture GPU cleanup actions #606 uploaded — is
--   no longer reachable by 'World.Render.BloodQuads.uploadBloodTextures'
--   (which only sweeps pages still in 'wmWorlds'), so those GPU resources
--   would leak. These helpers enqueue the orphaned page's LIVE handle
--   'IORef' onto 'Engine.Core.State.bloodDisposeQueue' for the render
--   thread to dispose.
--
--   Enqueuing the live ref (not a snapshot of its records) is what keeps
--   this race-free against a still-in-flight FIFO eviction of the same
--   map: that sweep frees exactly the handles it removed and rewrites the
--   ref, and the drain later reads whatever remains — the two sets are
--   disjoint, so no handle is freed twice. Enqueuing is idempotent: a ref
--   handed over twice yields its records only once (the first drain
--   empties it). No GPU access here — this is pure 'IORef'/queue plumbing
--   on the world thread; the actual Vulkan disposal happens on the render
--   thread. Deterministically testable without a device
--   ('Test.Headless.Blood.Teardown').
module World.Blood.Teardown
    ( enqueueBloodDisposal
    , enqueueBloodDisposalForPage
    , enqueueBloodDisposalAll
    , drainBloodDisposalRecords
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, atomicModifyIORef')
import Engine.Asset.Handle (TextureHandle)
import qualified Engine.Core.Queue as Q
import World.Page.Types (WorldPageId)
import World.State.Types (WorldState(..), WorldManager(..), BloodTextureHandles)

-- | Hand one to-be-orphaned world's blood-texture handle 'IORef' to the
--   render thread for disposal.
enqueueBloodDisposal ∷ Q.Queue (IORef BloodTextureHandles) → WorldState → IO ()
enqueueBloodDisposal q ws = Q.writeQueue q (wsBloodTextureHandlesRef ws)

-- | Enqueue the blood disposal for a specific page id, if it currently
--   exists in the manager. Used by the replacement paths (init / arena /
--   save-load), which drop an existing entry under a reused page id, and
--   by single-page destroy — look the OLD state up before it is dropped
--   from 'wmWorlds'.
enqueueBloodDisposalForPage
    ∷ Q.Queue (IORef BloodTextureHandles) → WorldManager → WorldPageId → IO ()
enqueueBloodDisposalForPage q mgr pid =
    forM_ (lookup pid (wmWorlds mgr)) (enqueueBloodDisposal q)

-- | Enqueue the blood disposal for every page in the manager (destroy-all
--   / Exit to Menu, which clears 'wmWorlds' wholesale).
enqueueBloodDisposalAll ∷ Q.Queue (IORef BloodTextureHandles) → WorldManager → IO ()
enqueueBloodDisposalAll q mgr =
    forM_ (map snd (wmWorlds mgr)) (enqueueBloodDisposal q)

-- | Render-thread drain, non-GPU half: flush every queued handle 'IORef'
--   and atomically empty each, returning all the disposal records to free.
--   Emptying the map here satisfies the handle-map-clearing requirement
--   and makes re-enqueue idempotent (a second flush of the same ref finds
--   it empty). The caller ('World.Render.BloodQuads.disposeQueuedBloodTextures')
--   does the Vulkan work; splitting the pure part out keeps it testable
--   without a device.
drainBloodDisposalRecords
    ∷ Q.Queue (IORef BloodTextureHandles) → IO [(TextureHandle, IO ())]
drainBloodDisposalRecords q = do
    refs ← Q.flushQueue q
    concat <$> forM refs
        (\r → atomicModifyIORef' r (\m → (HM.empty, HM.elems m)))
