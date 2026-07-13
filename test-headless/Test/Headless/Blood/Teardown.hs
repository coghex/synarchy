{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Deterministic, non-GPU coverage for the world-thread half of #788's
--   blood-texture GPU teardown (World.Blood.Teardown): every path that
--   removes or replaces a world page must hand that page's live blood
--   handle 'IORef' to the render thread's dispose queue, and the drain
--   must yield every owned cleanup record exactly once, empty the map,
--   and stay idempotent under a double-enqueue. No engine boot / no GPU —
--   the actual Vulkan disposal is the render thread's job and is covered
--   by tools/blood_gpu_lifecycle_probe.py.
module Test.Headless.Blood.Teardown (spec) where

import UPrelude
import Test.Hspec
import Data.IORef
import qualified Data.HashMap.Strict as HM
import qualified Engine.Core.Queue as Q
import Engine.Asset.Handle (TextureHandle(..))
import Blood.Types (BloodTextureId(..))
import World.Page.Types (WorldPageId(..))
import World.State.Types ( WorldState(..), WorldManager(..)
                         , emptyWorldState, emptyWorldManager
                         , BloodTextureHandles )
import World.Blood.Teardown

-- | A world page carrying @n@ synthetic blood-texture records. Each
--   record's "GPU cleanup" just bumps @counter@, so a later run of the
--   drained cleanups proves each fired exactly once.
mkPage ∷ IORef Int → Int → IO WorldState
mkPage counter n = do
    ws ← emptyWorldState
    writeIORef (wsBloodTextureHandlesRef ws) (mkRecords counter n)
    pure ws

mkRecords ∷ IORef Int → Int → BloodTextureHandles
mkRecords counter n = HM.fromList
    [ (BloodTextureId (fromIntegral i), (TextureHandle i, modifyIORef' counter (+1)))
    | i ← [1 .. n] ]

-- | Blood handle-map size of a page (the "handle-map clearing" check).
handleCount ∷ WorldState → IO Int
handleCount ws = HM.size ⊚ readIORef (wsBloodTextureHandlesRef ws)

spec ∷ Spec
spec = do
    describe "single-page destroy (enqueueBloodDisposal)" $ do
        it "drains every owned cleanup record exactly once and empties the map" $ do
            counter ← newIORef 0
            q ← Q.newQueue
            ws ← mkPage counter 4
            enqueueBloodDisposal q ws
            records ← drainBloodDisposalRecords q
            length records `shouldBe` 4
            -- map cleared as part of the drain
            handleCount ws `shouldReturn` 0
            -- running the drained cleanups fires each exactly once
            sequence_ (map snd records)
            readIORef counter `shouldReturn` 4

    describe "single-page destroy via page id (enqueueBloodDisposalForPage)" $ do
        it "enqueues the page's records when the id is present" $ do
            counter ← newIORef 0
            q ← Q.newQueue
            ws ← mkPage counter 3
            let mgr = emptyWorldManager { wmWorlds = [(WorldPageId "doomed", ws)] }
            enqueueBloodDisposalForPage q mgr (WorldPageId "doomed")
            records ← drainBloodDisposalRecords q
            length records `shouldBe` 3

        it "is a no-op when the page id is absent (nothing queued)" $ do
            counter ← newIORef 0
            q ← Q.newQueue
            ws ← mkPage counter 3
            let mgr = emptyWorldManager { wmWorlds = [(WorldPageId "other", ws)] }
            enqueueBloodDisposalForPage q mgr (WorldPageId "missing")
            records ← drainBloodDisposalRecords q
            length records `shouldBe` 0
            handleCount ws `shouldReturn` 3   -- untouched

    describe "destroy-all / Exit to Menu (enqueueBloodDisposalAll)" $ do
        it "drains every page's records once and empties every map" $ do
            counter ← newIORef 0
            q ← Q.newQueue
            wsA ← mkPage counter 2
            wsB ← mkPage counter 5
            let mgr = emptyWorldManager
                    { wmWorlds = [(WorldPageId "a", wsA), (WorldPageId "b", wsB)] }
            enqueueBloodDisposalAll q mgr
            records ← drainBloodDisposalRecords q
            length records `shouldBe` 7
            handleCount wsA `shouldReturn` 0
            handleCount wsB `shouldReturn` 0
            sequence_ (map snd records)
            readIORef counter `shouldReturn` 7

    describe "replacement / stale-page removal (init / arena / save-load)" $ do
        -- The init / arena / save-load-replacement AND LoadWorld
        -- stale-page-removal paths all reduce to the same call: enqueue
        -- the OLD state under the doomed id before the manager drops it.
        it "reclaims the replaced page's records under the reused id" $ do
            counter ← newIORef 0
            q ← Q.newQueue
            oldWs ← mkPage counter 6
            let mgr = emptyWorldManager
                    { wmWorlds = [(WorldPageId "main_world", oldWs)] }
            enqueueBloodDisposalForPage q mgr (WorldPageId "main_world")
            records ← drainBloodDisposalRecords q
            length records `shouldBe` 6
            handleCount oldWs `shouldReturn` 0

        it "reclaims a stale within-session-load page dropped from wmWorlds" $ do
            -- LoadWorld drops remapped-away / superseded pages; each must
            -- have its blood enqueued before it leaves wmWorlds.
            counter ← newIORef 0
            q ← Q.newQueue
            keep  ← mkPage counter 2       -- a page the load keeps
            stale ← mkPage counter 4       -- a stale page the load supersedes
            let mgr = emptyWorldManager
                    { wmWorlds = [ (WorldPageId "main_world", keep)
                                 , (WorldPageId "main_world#2", stale) ] }
            enqueueBloodDisposalForPage q mgr (WorldPageId "main_world#2")
            records ← drainBloodDisposalRecords q
            length records `shouldBe` 4
            handleCount stale `shouldReturn` 0
            handleCount keep  `shouldReturn` 2   -- kept page untouched

    describe "idempotency (FIFO eviction / teardown overlap)" $ do
        it "yields each record only once even if the same ref is enqueued twice" $ do
            counter ← newIORef 0
            q ← Q.newQueue
            ws ← mkPage counter 3
            enqueueBloodDisposal q ws
            enqueueBloodDisposal q ws       -- e.g. destroy racing a stray re-queue
            records ← drainBloodDisposalRecords q
            length records `shouldBe` 3     -- not 6 — first extraction emptied it
            sequence_ (map snd records)
            readIORef counter `shouldReturn` 3

        it "a re-drain of an already-emptied ref yields nothing" $ do
            counter ← newIORef 0
            q ← Q.newQueue
            ws ← mkPage counter 3
            enqueueBloodDisposal q ws
            _ ← drainBloodDisposalRecords q
            enqueueBloodDisposal q ws
            records2 ← drainBloodDisposalRecords q
            length records2 `shouldBe` 0

    describe "live-ref semantics (disjoint from in-flight FIFO eviction)" $ do
        it "drains the ref's contents AT DRAIN TIME, not an enqueue-time snapshot" $ do
            counter ← newIORef 0
            q ← Q.newQueue
            ws ← mkPage counter 2
            enqueueBloodDisposal q ws
            -- A render sweep uploads one more texture into the SAME map
            -- after teardown enqueued it: the drain must see it too.
            modifyIORef' (wsBloodTextureHandlesRef ws) $
                HM.insert (BloodTextureId 99) (TextureHandle 99, modifyIORef' counter (+1))
            records ← drainBloodDisposalRecords q
            length records `shouldBe` 3
