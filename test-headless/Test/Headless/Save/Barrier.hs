{-# LANGUAGE UnicodeSyntax #-}
module Test.Headless.Save.Barrier (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Set as Set
import Engine.Save.Barrier

spec ∷ Spec
spec = describe "save snapshot barrier" $ do
    it "reaches the boundary only after every registered owner acknowledges" $ do
        b ← newSaveBarrier
        Right n ← beginSave b (Set.fromList [SaveLua, SaveWorld, SaveUnit])
        acknowledgeSave b n SaveLua
        acknowledgeSave b n SaveWorld
        early ← waitForOwners 1000 b n
        early `shouldSatisfy` (\value → case value of Left _ → True; Right _ → False)
        acknowledgeSave b n SaveUnit
        -- First pass drained each owner; a second pass closes causal
        -- follow-up work emitted by the last owner into an earlier queue.
        acknowledgeSave b n SaveLua
        acknowledgeSave b n SaveWorld
        acknowledgeSave b n SaveUnit
        acknowledgeSave b n SaveLua
        acknowledgeSave b n SaveWorld
        acknowledgeSave b n SaveUnit
        waitForOwners 1000 b n `shouldReturn` Right ()
        reachSnapshot b n
        status ← readSaveStatus b
        ssPhase <$> status `shouldBe` Just SaveSnapshotBoundary
        -- Worker loops continue ticking during capture.  Their stale acks
        -- must not reopen command processing after the boundary.
        acknowledgeSave b n SaveLua
        statusAgain ← readSaveStatus b
        ssPhase <$> statusAgain `shouldBe` Just SaveSnapshotBoundary

    it "fails a non-responsive owner without completing capture" $ do
        b ← newSaveBarrier
        Right n ← beginSave b (Set.singleton SaveWorld)
        failSave b n "world owner did not respond"
        waitForOwners 1000 b n `shouldReturn` Left "world owner did not respond"
        status ← readSaveStatus b
        ssPhase <$> status `shouldBe` Just SaveFailed

    it "serializes back-to-back transactions with distinct ids" $ do
        b ← newSaveBarrier
        Right first ← beginSave b Set.empty
        beginSave b Set.empty `shouldReturn` Left "a save transaction is already active"
        finishSave b first
        Right second ← beginSave b Set.empty
        second `shouldBe` first + 1

    -- #758: releaseCaptureLock lets state owners resume (captureLocked
    -- False) the instant the snapshot is captured, WITHOUT declaring the
    -- transaction terminally complete -- so a later disk-write failure
    -- can still surface as a real SaveFailed outcome.
    it "unblocks captureLocked without completing the transaction" $ do
        b ← newSaveBarrier
        Right n ← beginSave b Set.empty
        reachSnapshot b n
        captureLocked b `shouldReturn` True
        releaseCaptureLock b n
        captureLocked b `shouldReturn` False
        status ← readSaveStatus b
        ssPhase <$> status `shouldBe` Just SaveEncoding
        ssOutcome <$> status `shouldBe` Just Nothing
        -- Still open: a second overlapping save is still refused, and a
        -- status reader must not see a premature success.
        saveInProgress b `shouldReturn` True
        beginSave b Set.empty `shouldReturn` Left "a save transaction is already active"

    it "ignores a stray acknowledgement during SaveEncoding" $ do
        b ← newSaveBarrier
        Right n ← beginSave b (Set.singleton SaveWorld)
        reachSnapshot b n
        releaseCaptureLock b n
        acknowledgeSave b n SaveWorld
        status ← readSaveStatus b
        ssPhase <$> status `shouldBe` Just SaveEncoding
        ssAcknowledged <$> status `shouldBe` Just Set.empty

    it "finishSave finalizes a success after releaseCaptureLock" $ do
        b ← newSaveBarrier
        Right n ← beginSave b Set.empty
        reachSnapshot b n
        releaseCaptureLock b n
        finishSave b n
        status ← readSaveStatus b
        ssPhase <$> status `shouldBe` Just SaveCaptureComplete
        ssOutcome <$> status `shouldBe` Just (Just SaveSucceeded)
        saveInProgress b `shouldReturn` False

    it "failSave finalizes a disk-write failure discovered after releaseCaptureLock" $ do
        b ← newSaveBarrier
        Right n ← beginSave b Set.empty
        reachSnapshot b n
        releaseCaptureLock b n
        -- State owners have already resumed at this point; the write
        -- itself is what fails.
        failSave b n "disk full"
        status ← readSaveStatus b
        ssPhase <$> status `shouldBe` Just SaveFailed
        ssOutcome <$> status `shouldBe` Just (Just (SaveAborted "disk full"))
        saveInProgress b `shouldReturn` False
