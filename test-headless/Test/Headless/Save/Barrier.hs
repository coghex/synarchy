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

    -- Round 15 review, revised (issue #763): a conditionally-registered
    -- owner (SaveRender/SaveInput) may still be ticking and acking
    -- during a transaction that never listed it as an owner at all --
    -- e.g. the render thread's per-tick acknowledgeCurrent SaveRender
    -- during a plain save (saveOwnerSet never includes SaveRender).
    -- That stray ack must be a no-op, not a corruption: inserting it
    -- into ssAcknowledged unconditionally would permanently break the
    -- exact-set-equality check this whole protocol is built on,
    -- wedging the transaction until waitForOwners times out even once
    -- every REAL owner has acknowledged.
    it "an acknowledgement from an owner not registered for this transaction is a no-op" $ do
        b ← newSaveBarrier
        Right n ← beginSave b (Set.singleton SaveWorld)
        acknowledgeSave b n SaveRender
        status ← readSaveStatus b
        ssAcknowledged <$> status `shouldBe` Just Set.empty
        -- The real owner still completes all three quiescence passes
        -- normally -- the stray ack above didn't poison anything.
        acknowledgeSave b n SaveWorld
        acknowledgeSave b n SaveWorld
        acknowledgeSave b n SaveWorld
        waitForOwners 1000 b n `shouldReturn` Right ()

    -- Round 15 review, revised: SaveRender (Engine.Loop's render/
    -- offscreen thread) must gate the snapshot boundary exactly like
    -- any other real state-owner thread when it IS registered for the
    -- transaction (a load, outside headless mode) -- a bare
    -- captureLocked pre-check alone let the barrier reach the boundary
    -- and publish in the gap between that check and the camera/
    -- Lua-message work it gated, since nothing actually waited for
    -- this thread. Registering it as a genuine SaveOwner closes that
    -- window structurally: the boundary is unreachable until this
    -- thread's own acknowledgement lands, mirroring
    -- @Engine.Loop.runGatedByCaptureLock@s "check locked, do unlocked
    -- work if not locked, always ack" shape.
    it "a registered SaveRender owner gates the snapshot boundary exactly like a real state-owner thread" $ do
        b ← newSaveBarrier
        Right n ← beginSave b (Set.fromList [SaveLua, SaveWorld, SaveRender])
        let ackAll = do
                acknowledgeSave b n SaveLua
                acknowledgeSave b n SaveWorld
                acknowledgeSave b n SaveRender
        -- Two full passes with every owner participating.
        ackAll
        ackAll
        -- Third pass: every owner but SaveRender acks -- as if the
        -- render thread's own last unlocked tick hasn't run yet. The
        -- boundary must NOT be reachable even though the other two
        -- owners are done.
        acknowledgeSave b n SaveLua
        acknowledgeSave b n SaveWorld
        stillWaiting ← waitForOwners 1000 b n
        stillWaiting `shouldSatisfy` (\value → case value of Left _ → True; Right _ → False)
        -- Only once SaveRender itself acks can the barrier proceed.
        acknowledgeSave b n SaveRender
        waitForOwners 1000 b n `shouldReturn` Right ()
        reachSnapshot b n
        status ← readSaveStatus b
        ssPhase <$> status `shouldBe` Just SaveSnapshotBoundary

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

    -- Issue #2221 (#757 requirement 5, restored): an acknowledgement is
    -- a WAIT, not mutual exclusion -- the transaction still needs every
    -- OTHER owner's final acknowledgement plus the initiator's
    -- reachSnapshot before the boundary exists. An owner whose gate
    -- asked only captureLocked therefore read False in that gap and
    -- could start a fresh unlocked tick that was still running when the
    -- snapshot was captured (save) or when publishStagedSession swapped
    -- the session refs (load). 'ownerGated' parks each owner from its
    -- OWN final-pass acknowledgement instead.
    describe "post-final-acknowledgement owner park (issue #2221)" $ do
        -- Three owners, none of them SaveLua: SaveLua is the one owner
        -- acknowledgeSave deliberately carries across a pass reset (it
        -- is the blocked initiator), so leaving it out keeps every
        -- assertion below about the ordinary worker-owner protocol.
        let owners = [SaveWorld, SaveUnit, SaveRender]
            ackAll b n = mapM_ (acknowledgeSave b n) owners
            -- Drive the transaction to the START of its final pass:
            -- every owner has drained twice and none has acknowledged
            -- the third (final) pass yet.
            atFinalPass = do
                b ← newSaveBarrier
                Right n ← beginSave b (Set.fromList owners)
                ackAll b n
                ackAll b n
                pure (b, n)

        it "an earlier-pass acknowledgement parks nobody, so the \
           \multi-pass causal drain is unchanged" $ do
            b ← newSaveBarrier
            Right n ← beginSave b (Set.fromList owners)
            acknowledgeSave b n SaveWorld
            ownerGated b SaveWorld `shouldReturn` False
            ackAll b n
            -- Second pass: still not the final one.
            acknowledgeSave b n SaveWorld
            ownerGated b SaveWorld `shouldReturn` False
            ownerGated b SaveUnit  `shouldReturn` False

        it "a final-pass acknowledgement parks ONLY the owner that made \
           \it, leaving the others free to finish their own pass" $ do
            (b, n) ← atFinalPass
            mapM_ (\o → ownerGated b o `shouldReturn` False) owners
            acknowledgeSave b n SaveWorld
            ownerGated b SaveWorld  `shouldReturn` True
            ownerGated b SaveUnit   `shouldReturn` False
            ownerGated b SaveRender `shouldReturn` False
            -- An owner that is still free really can complete the pass:
            -- park it too and the barrier reaches the boundary normally.
            acknowledgeSave b n SaveUnit
            ownerGated b SaveUnit `shouldReturn` True
            acknowledgeSave b n SaveRender
            mapM_ (\o → ownerGated b o `shouldReturn` True) owners
            waitForOwners 1000 b n `shouldReturn` Right ()

        it "a parked owner is parked BEFORE the boundary exists -- the \
           \global capture lock is still open at that point" $ do
            (b, n) ← atFinalPass
            acknowledgeSave b n SaveWorld
            -- The whole point: captureLocked (the only gate before
            -- #2221) still answers False here.
            captureLocked b `shouldReturn` False
            ownerGated b SaveWorld `shouldReturn` True

        it "an acknowledgement from an owner this transaction never \
           \registered parks nothing, but the global capture lock still \
           \gates it at the boundary" $ do
            b ← newSaveBarrier
            Right n ← beginSave b (Set.singleton SaveWorld)
            acknowledgeSave b n SaveRender
            ownerGated b SaveRender `shouldReturn` False
            acknowledgeSave b n SaveWorld
            acknowledgeSave b n SaveWorld
            acknowledgeSave b n SaveWorld
            ownerGated b SaveWorld  `shouldReturn` True
            ownerGated b SaveRender `shouldReturn` False
            reachSnapshot b n
            ownerGated b SaveRender `shouldReturn` True

        it "releaseCaptureLock unparks every owner before encoding \
           \finishes (#758), and failSave unparks them on the spot" $ do
            (b, n) ← atFinalPass
            ackAll b n
            reachSnapshot b n
            mapM_ (\o → ownerGated b o `shouldReturn` True) owners
            releaseCaptureLock b n
            mapM_ (\o → ownerGated b o `shouldReturn` False) owners
            -- Still mid-transaction: the encode/disk step has not
            -- reported yet, so nothing is terminal.
            saveInProgress b `shouldReturn` True

            (b2, n2) ← atFinalPass
            acknowledgeSave b2 n2 SaveWorld
            ownerGated b2 SaveWorld `shouldReturn` True
            failSave b2 n2 "owner did not respond"
            mapM_ (\o → ownerGated b2 o `shouldReturn` False) owners

        it "a finished transaction leaves no park state behind, so the \
           \next one starts clean" $ do
            (b, n) ← atFinalPass
            ackAll b n
            reachSnapshot b n
            releaseCaptureLock b n
            finishSave b n
            mapM_ (\o → ownerGated b o `shouldReturn` False) owners
            Right n2 ← beginSave b (Set.fromList owners)
            mapM_ (\o → ownerGated b o `shouldReturn` False) owners
            -- And the fresh transaction parks on its OWN final pass,
            -- not on anything inherited from the previous one.
            ackAll b n2
            mapM_ (\o → ownerGated b o `shouldReturn` False) owners
            ackAll b n2
            acknowledgeSave b n2 SaveWorld
            ownerGated b SaveWorld  `shouldReturn` True
            ownerGated b SaveUnit   `shouldReturn` False

        -- Requirement 2: the boundary is reached only when no owner has
        -- a gated tick in flight. reachSnapshot used to check nothing
        -- but request identity and outcome, so a premature call could
        -- declare the boundary while owners were still mid-pass.
        it "reachSnapshot refuses to declare the boundary before the \
           \final pass is complete" $ do
            (b, n) ← atFinalPass
            reachSnapshot b n
            status ← readSaveStatus b
            ssPhase <$> status `shouldBe` Just SavePausing
            captureLocked b `shouldReturn` False
            acknowledgeSave b n SaveWorld
            acknowledgeSave b n SaveUnit
            reachSnapshot b n
            captureLocked b `shouldReturn` False
            -- Only the last outstanding owner's acknowledgement opens it.
            acknowledgeSave b n SaveRender
            reachSnapshot b n
            captureLocked b `shouldReturn` True

        it "reachSnapshot cannot re-close the capture window \
           \releaseCaptureLock already opened" $ do
            (b, n) ← atFinalPass
            ackAll b n
            reachSnapshot b n
            releaseCaptureLock b n
            reachSnapshot b n
            status ← readSaveStatus b
            ssPhase <$> status `shouldBe` Just SaveEncoding
            captureLocked b `shouldReturn` False

        it "a parked owner's routine per-tick acknowledgement is inert: \
           \it neither inflates the pass count nor unparks anything" $ do
            (b, n) ← atFinalPass
            ackAll b n
            before ← readSaveStatus b
            ssQuiescencePasses <$> before `shouldBe` Just 3
            ackAll b n
            ackAll b n
            after ← readSaveStatus b
            ssQuiescencePasses <$> after `shouldBe` Just 3
            ssPhase <$> after `shouldBe` Just SaveWaitingOwners
            mapM_ (\o → ownerGated b o `shouldReturn` True) owners

        -- Requirement 4: timeout semantics are kept, and a report that
        -- named only the outstanding owners could no longer distinguish
        -- an owner that never ran from one still mid-pass now that
        -- owners park at different points of the same transaction.
        it "a timeout names the outstanding owners AND the phase the \
           \transaction stalled in" $ do
            (b, n) ← atFinalPass
            acknowledgeSave b n SaveWorld
            acknowledgeSave b n SaveUnit
            timedOut ← waitForOwners 1000 b n
            timedOut `shouldBe`
                Left "timed out waiting for save state owners: \
                     \[SaveRender] in phase SavePausing"
