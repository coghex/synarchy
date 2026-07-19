{-# LANGUAGE UnicodeSyntax #-}
module Test.Headless.Load.Status (spec) where

import UPrelude
import Test.Hspec
import Engine.Load.Status

-- | Pure coverage for the whole-session LOAD transaction status surface
--   (issue #763, save-overhaul C2) — the load-side counterpart to
--   "Test.Headless.Save.Barrier". No engine boot required: this module
--   is a plain 'Data.IORef.IORef', exactly like the barrier's own STM
--   state, so its lifecycle is directly testable in isolation. The
--   real cross-thread publish coordination it hands off to
--   ("Engine.Save.Barrier"'s owner-quiescence protocol, reused as-is —
--   see "Engine.Scripting.Lua.Thread.Dispatch") is already covered by
--   "Test.Headless.Save.Barrier"; end-to-end coverage through a real
--   engine.loadSave / engine.getLoadStatus round trip lives in
--   tools/transactional_load_probe.py.
spec ∷ Spec
spec = describe "transactional load" $ do
    it "moves through every phase in order for a single successful load" $ do
        ref ← newLoadStatusRef
        Right n ← beginLoad ref "my_save"
        status0 ← readLoadStatus ref
        lsRequestId <$> status0 `shouldBe` Just n
        lsSaveName <$> status0 `shouldBe` Just "my_save"
        lsPhase <$> status0 `shouldBe` Just LoadRequested
        loadInProgress ref `shouldReturn` True
        mapM_ (advanceLoad ref n)
            [ LoadPaused, LoadSourceSelected, LoadEnvelopeValidated
            , LoadComponentsDecoded, LoadComponentsMigrated
            , LoadSnapshotAssembled, LoadContentValidated, LoadStaged
            , LoadWaitingPublish ]
        status1 ← readLoadStatus ref
        lsPhase <$> status1 `shouldBe` Just LoadWaitingPublish
        lsOutcome <$> status1 `shouldBe` Just Nothing
        loadInProgress ref `shouldReturn` True
        finishLoad ref n
        status2 ← readLoadStatus ref
        lsPhase <$> status2 `shouldBe` Just LoadPublished
        lsOutcome <$> status2 `shouldBe` Just (Just LoadSucceeded)
        loadInProgress ref `shouldReturn` False

    it "rejects a second load request while one is already in flight" $ do
        ref ← newLoadStatusRef
        Right _ ← beginLoad ref "first"
        beginLoad ref "second"
            `shouldReturn` Left "a load transaction is already active"

    it "a failed load reaches a terminal, non-published outcome naming\
       \ the reason" $ do
        ref ← newLoadStatusRef
        Right n ← beginLoad ref "broken_save"
        advanceLoad ref n LoadPaused
        failLoad ref n "envelope checksum mismatch"
        status ← readLoadStatus ref
        lsPhase <$> status `shouldBe` Just LoadFailed
        lsOutcome <$> status
            `shouldBe` Just (Just (LoadAborted "envelope checksum mismatch"))
        loadInProgress ref `shouldReturn` False

    it "a terminal load frees the slot for the next one, with a fresh,\
       \ strictly increasing id" $ do
        ref ← newLoadStatusRef
        Right first ← beginLoad ref "a"
        finishLoad ref first
        Right second ← beginLoad ref "b"
        second `shouldBe` first + 1

    it "ignores a stale signal for an id that already reached a\
       \ terminal outcome" $ do
        ref ← newLoadStatusRef
        Right n ← beginLoad ref "a"
        finishLoad ref n
        -- A late/duplicate signal for the same (now-terminal) id must
        -- not reopen or corrupt the status.
        advanceLoad ref n LoadStaged
        failLoad ref n "too late"
        status ← readLoadStatus ref
        lsPhase <$> status `shouldBe` Just LoadPublished
        lsOutcome <$> status `shouldBe` Just (Just LoadSucceeded)

    it "readLoadStatus is Nothing before any load has ever been\
       \ requested" $ do
        ref ← newLoadStatusRef
        readLoadStatus ref `shouldReturn` Nothing
        loadInProgress ref `shouldReturn` False
