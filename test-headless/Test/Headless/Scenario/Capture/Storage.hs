{-# LANGUAGE Strict #-}
-- | The file a capture publishes (#2714): complete or absent, never partial;
--   an existing destination replaced only on explicit request, even when the
--   file appears while the capture runs; a failure at any storage step
--   leaves the destination, the temporary files and the shared line exactly
--   as the step's contract says.
--
--   Faults are injected through the writer's operation seam
--   ('Scenario.Capture.Types.StorageOps'), never through filesystem
--   permissions (a suite that runs as root bypasses those) and never through
--   timing.
module Test.Headless.Scenario.Capture.Storage (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent.MVar (putMVar)
import Control.Exception (AsyncException(..), SomeException, fromException, throwIO, try)
import Data.IORef (readIORef)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import System.Directory
    (createDirectory, createDirectoryIfMissing, createFileLink, doesDirectoryExist, doesFileExist, getModificationTime)
import System.FilePath ((</>))
import Engine.Core.Capability.SaveLoad (SaveLoadCapability(..), toSaveLoadCapability)
import Engine.Core.State (EngineEnv(..))
import Engine.Load.Status (captureInProgress)
import Engine.Save.Barrier (captureLocked, saveInProgress)
import Scenario.Capture.Storage
import Scenario.Types (Scenario(..))
import Scenario.Capture.Types
import Test.Headless.Scenario.Capture.Support
import Test.Headless.Scenario.Load.Harness
import Test.Headless.Scenario.Placement.Fixtures (Fx(..))
import World.Save.Storage.Durable
    ( ExclusiveOutcome(..), UniqueWriteOps(..), WriteStep(..), productionUniqueWriteOps
    , publishExclusive, writeUniqueDurablyWith )

-- | The scenario the examples capture: small, with a unit and a building.
contentDoc ∷ [String]
contentDoc =
    [ "map: {width: 20, height: 20}"
    , "units:"
    , "  - {id: scout, definition: acolyte, x: 1, y: 1, stats: {hydration: 6.0}}"
    , "buildings:"
    , "  - {id: bat, definition: high_voltage_battery, x: 5, y: 5, power_charge: 100}" ]

-- | What one capture attempt left behind.
data Attempt = Attempt
    { atStatus ∷ ScenarioCaptureStatus
    , atDest   ∷ FilePath
    , atFiles  ∷ [FilePath]
      -- ^ every name in the destination's directory afterwards
    , atFinal  ∷ Maybe BS.ByteString
      -- ^ the destination's bytes afterwards
    }

attempt ∷ H → Fault → Bool → Maybe BS.ByteString → String → IO Attempt
attempt h fault overwrite existing name = do
    dir ← freshDir name
    let dest = dir </> "out.yaml"
    forM_ existing (BS.writeFile dest)
    armCaptureOps h (faultingOps productionOps fault)
    st ← runCaptureWith h (CaptureRequest dest overwrite)
    disarmCapture h
    files ← listDir dir
    present ← doesFileExist dest
    final ← if present then Just <$> BS.readFile dest else pure Nothing
    pure (Attempt st dest files final)

failure ∷ Attempt → IO CaptureFailure
failure a = case cstOutcome (atStatus a) of
    Just (CaptureUnsuccessful f) → pure f
    other → fail ("expected an unsuccessful capture, got " ⧺ show other)

-- | After ANY handled outcome: still paused, the line free, the capture
--   lock released, and a later capture goes through.
afterwards ∷ H → IO ()
afterwards h = do
    let env = fxEnv (hFx h)
        sl = toSaveLoadCapability env
    readIORef (enginePausedRef env) `shouldReturn` True
    saveInProgress (slSaveBarrierRef sl) `shouldReturn` False
    captureLocked (slSaveBarrierRef sl) `shouldReturn` False
    captureInProgress (slLoadStatusRef sl) (slSaveBarrierRef sl) `shouldReturn` False
    dir ← freshDir "afterwards"
    s ← runCapture h (dir </> "next.yaml") False
    void (captureOf s)

old ∷ BS.ByteString
old = BC.pack "the previous destination, untouched\n"

spec ∷ SpecWith H
spec = describe "the file" $ do

    it "publishes one complete file into a new destination and leaves nothing else behind" $ \h → do
        loadDoc h "capture-store-new.yaml" contentDoc
        a ← attempt h NoFault False Nothing "new"
        report ← captureOf (atStatus a)
        atFiles a `shouldBe` ["out.yaml"]
        fmap BS.length (atFinal a) `shouldBe` Just (crpBytes report)
        crpReplaced report `shouldBe` False
        crpWarnings report `shouldBe` []
        cstPublication (atStatus a) `shouldBe` Published
        -- a complete current-format document the boundary accepts
        sc ← decodeFile h (atDest a)
        length (scUnits sc) `shouldBe` 1
        afterwards h

    it "refuses an existing destination without overwrite intent and changes none of its bytes" $ \h → do
        loadDoc h "capture-store-exists.yaml" contentDoc
        a ← attempt h NoFault False (Just old) "exists"
        f ← failure a
        (cfKind f, cfPhase f, cfPublication f) `shouldBe` (FailOverwriteRefused, Just PhaseDestinationCheck, NotPublished)
        atFinal a `shouldBe` Just old
        atFiles a `shouldBe` ["out.yaml"]
        afterwards h

    it "does not overwrite a file that appears between the preflight and the publication" $ \h → do
        loadDoc h "capture-store-late.yaml" contentDoc
        dir ← freshDir "late"
        let dest = dir </> "out.yaml"
        -- the destination is free when the request is judged, and taken just
        -- before it would be published
        armCapture h $ \p → when (p ≡ CpBeforePublish) (BS.writeFile dest old)
        s ← runCapture h dest False
        disarmCapture h
        a ← failure (Attempt s dest [] Nothing)
        (cfKind a, cfPhase a, cfPublication a) `shouldBe` (FailOverwriteRefused, Just PhasePublish, NotPublished)
        BS.readFile dest `shouldReturn` old
        -- the request's own temporary file is gone, and only it
        listDir dir `shouldReturn` ["out.yaml"]
        afterwards h

    it "replaces an existing destination in one step on explicit overwrite, and keeps no backup" $ \h → do
        loadDoc h "capture-store-overwrite.yaml" contentDoc
        a ← attempt h NoFault True (Just old) "overwrite"
        report ← captureOf (atStatus a)
        crpReplaced report `shouldBe` True
        atFinal a `shouldSatisfy` maybe False (/= old)
        atFiles a `shouldBe` ["out.yaml"]
        _ ← decodeFile h (atDest a)
        afterwards h

    it "writes a new name for Save As and leaves the original file and its source alone" $ \h → do
        dir ← freshDir "saveas"
        let src = dir </> "source.yaml"
        writeFile src (doc contentDoc ⧺ "")
        srcBefore ← BS.readFile src
        mtimeBefore ← getModificationTime src
        reloadFrom h src
        -- load, migration and capture preparation never rewrite the source
        BS.readFile src `shouldReturn` srcBefore
        getModificationTime src `shouldReturn` mtimeBefore
        s1 ← runCapture h (dir </> "copy-1.yaml") False
        void (captureOf s1)
        s2 ← runCapture h (dir </> "copy-2.yaml") False
        void (captureOf s2)
        BS.readFile src `shouldReturn` srcBefore
        getModificationTime src `shouldReturn` mtimeBefore
        listDir dir `shouldReturn` ["copy-1.yaml", "copy-2.yaml", "source.yaml"]
        -- and a failed attempt never touches it either
        armCaptureOps h (faultingOps productionOps FailLink)
        s3 ← runCapture h (dir </> "copy-3.yaml") False
        disarmCapture h
        _ ← failure (Attempt s3 "" [] Nothing)
        BS.readFile src `shouldReturn` srcBefore
        listDir dir `shouldReturn` ["copy-1.yaml", "copy-2.yaml", "source.yaml"]
        afterwards h

    it "reports the publication the moment the destination holds the new file, while the request is still finishing" $ \h → do
        loadDoc h "capture-store-recorded.yaml" contentDoc
        dir ← freshDir "recorded"
        let dest = dir </> "out.yaml"
        hold ← newHold
        -- held after the atomic publication, before the directory is synced
        armCaptureHold h CpPublished hold
        withPump h $ do
            n ← requestCapture h dest False
            awaitHold hold
            st ← captureStatusOf h
            fmap (\r → (cstStage r, cstPublication r, isNothing (cstOutcome r))) st
                `shouldBe` Just (CaptureStoring, Published, True)
            -- the complete file is already in place
            bytes ← BS.readFile dest
            fmap BS.length (Just bytes) `shouldSatisfy` maybe False (> 0)
            _ ← decodeFile h dest
            putMVar (holdRelease hold) ()
            s ← awaitCapture h n 60
            disarmCapture h
            void (captureOf s)
        afterwards h

    describe "an injected failure at each step" $ do
        let case_ label fault overwrite existing check =
                it label $ \h → do
                    loadDoc h "capture-store-fault.yaml" contentDoc
                    -- the bytes an unfaulted capture of the same arena writes
                    ref ← attempt h NoFault False Nothing "reference"
                    void (captureOf (atStatus ref))
                    reference ← maybe (fail "no reference") pure (atFinal ref)
                    a ← attempt h fault overwrite existing "fault"
                    void (check a reference)
                    afterwards h
        let beforePublication f kind phase existing a _ref = do
                (cfKind f, cfPhase f, cfPublication f) `shouldBe` (kind, Just phase, NotPublished)
                -- the destination is exactly what it was, and no temporary remains
                atFinal a `shouldBe` existing
                atFiles a `shouldBe` maybe [] (const ["out.yaml"]) existing
        case_ "the temporary file cannot be created" FailCreate False Nothing $ \a ref → do
            f ← failure a
            beforePublication f FailTemporaryWrite PhaseTemporaryCreate Nothing a ref
        case_ "the temporary file cannot be written (a partial file was created, and removed)" FailWrite False Nothing $ \a ref → do
            f ← failure a
            beforePublication f FailTemporaryWrite PhaseTemporaryWrite Nothing a ref
        case_ "the temporary file cannot be flushed" FailFlush False Nothing $ \a ref → do
            f ← failure a
            beforePublication f FailTemporaryWrite PhaseTemporaryFlush Nothing a ref
        case_ "the candidate cannot be read back" FailRead False Nothing $ \a ref → do
            f ← failure a
            beforePublication f FailVerification PhaseCandidateReread Nothing a ref
        case_ "the candidate reads back as other bytes" FailReadWrongBytes False Nothing $ \a ref → do
            f ← failure a
            beforePublication f FailVerification PhaseCandidateReread Nothing a ref
        case_ "the exclusive publication fails" FailLink False Nothing $ \a ref → do
            f ← failure a
            beforePublication f FailPublication PhasePublish Nothing a ref
        case_ "the replacing publication fails and the previous file stands" FailReplace True (Just old) $ \a ref → do
            f ← failure a
            beforePublication f FailPublication PhasePublish (Just old) a ref
        case_ "a write fails over an existing destination and the previous file stands" FailWrite True (Just old) $ \a ref → do
            f ← failure a
            beforePublication f FailTemporaryWrite PhaseTemporaryWrite (Just old) a ref
        case_ "the directory cannot be synced: published, complete, and NOT reported as saved" FailSyncDir False Nothing $ \a ref → do
            f ← failure a
            (cfKind f, cfPhase f, cfPublication f) `shouldBe` (FailDurability, Just PhaseDirectorySync, Published)
            atFinal a `shouldBe` Just ref
            atFiles a `shouldBe` ["out.yaml"]
            cstPublication (atStatus a) `shouldBe` Published
        case_ "the directory cannot be synced after a replacing publication: the NEW complete file stands" FailSyncDir True (Just old) $ \a ref → do
            f ← failure a
            (cfKind f, cfPublication f) `shouldBe` (FailDurability, Published)
            atFinal a `shouldBe` Just ref
            atFiles a `shouldBe` ["out.yaml"]
        case_ "the temporary name cannot be removed after publication: still a success, with a warning" FailRemove False Nothing $ \a ref → do
            report ← captureOf (atStatus a)
            atFinal a `shouldBe` Just ref
            length (crpWarnings report) `shouldBe` 1
            -- the leftover is the request's OWN temporary, named in the warning
            let leftovers = filter (/= "out.yaml") (atFiles a)
            length leftovers `shouldBe` 1
            crpWarnings report `shouldSatisfy` all (\w → T.pack (head' leftovers) `T.isInfixOf` w)

    it "cleans up only its own temporary file, never another request's files or the destination" $ \h → do
        loadDoc h "capture-store-own.yaml" contentDoc
        dir ← freshDir "own"
        let dest = dir </> "out.yaml"
            neighbour = dir </> "scenario-capture-tmp424242"
            other = dir </> "notes.txt"
        BS.writeFile dest old
        BS.writeFile neighbour (BC.pack "another request's candidate")
        BS.writeFile other (BC.pack "a person's note")
        armCaptureOps h (faultingOps productionOps FailReplace)
        s' ← runCaptureWith h (CaptureRequest dest True)
        disarmCapture h
        _ ← failure (Attempt s' dest [] Nothing)
        BS.readFile dest `shouldReturn` old
        BS.readFile neighbour `shouldReturn` BC.pack "another request's candidate"
        BS.readFile other `shouldReturn` BC.pack "a person's note"
        listDir dir `shouldReturn` ["notes.txt", "out.yaml", "scenario-capture-tmp424242"]
        afterwards h

    describe "the destination" $ do
        it "must be a file in an existing directory, not a symlink and not a directory" $ \h → do
            loadDoc h "capture-store-dest.yaml" contentDoc
            dir ← freshDir "dest"
            -- a directory that does not exist
            s1 ← runCapture h (dir </> "missing" </> "out.yaml") False
            f1 ← failure (Attempt s1 "" [] Nothing)
            (cfKind f1, cfPublication f1) `shouldBe` (FailDestination, NotPublished)
            doesDirectoryExist (dir </> "missing") `shouldReturn` False
            -- a directory in the destination's place, with and without overwrite
            createDirectory (dir </> "isdir")
            forM_ [False, True] $ \ow → do
                s2 ← runCapture h (dir </> "isdir") ow
                f2 ← failure (Attempt s2 "" [] Nothing)
                (cfKind f2, cfPublication f2) `shouldBe` (FailDestination, NotPublished)
            -- a symlink: never followed, never replaced
            BS.writeFile (dir </> "target.txt") old
            createFileLink (dir </> "target.txt") (dir </> "link.yaml")
            forM_ [False, True] $ \ow → do
                s3 ← runCapture h (dir </> "link.yaml") ow
                f3 ← failure (Attempt s3 "" [] Nothing)
                (cfKind f3, cfPublication f3) `shouldBe` (FailDestination, NotPublished)
            BS.readFile (dir </> "target.txt") `shouldReturn` old
            -- a destination naming no file
            s4 ← runCapture h (dir ⧺ "/") False
            f4 ← failure (Attempt s4 "" [] Nothing)
            cfKind f4 `shouldBe` FailDestination
            createDirectoryIfMissing True dir
            afterwards h

    describe "a thread cancellation (shutdown) during a write" $ do
        let cancelled = (== Just ThreadKilled) ∘ fromException
        it "is never a write failure at creation, write or flush of the owned temporary: it propagates and leaves no file" $ \_ → do
            forM_ [ ("create", productionUniqueWriteOps { uwCreate = \_ _ → throwIO ThreadKilled })
                  , ("write", productionUniqueWriteOps { uwPut = \_ _ → throwIO ThreadKilled })
                  , ("flush", productionUniqueWriteOps { uwFlush = \_ → throwIO ThreadKilled }) ] $ \(step, ops) → do
                dir ← freshDir ("cancel-" ⧺ step)
                r ← try (writeUniqueDurablyWith ops dir "scenario-capture-tmp" (BC.pack "bytes"))
                case r of
                    Left e → (step, cancelled (e ∷ SomeException)) `shouldBe` (step, True)
                    Right _ → expectationFailure ("a cancellation at " ⧺ step ⧺ " was reported as a result")
                -- the temporary the call had created was removed on the way out
                listDir dir `shouldReturn` []
        it "still reports an ordinary failure at each of those steps as a result, with the file named for cleanup" $ \_ → do
            forM_ [ ("create", StepOpen, productionUniqueWriteOps { uwCreate = \_ _ → ioError (userError "create") })
                  , ("write", StepWrite, productionUniqueWriteOps { uwPut = \_ _ → ioError (userError "write") })
                  , ("flush", StepFlush, productionUniqueWriteOps { uwFlush = \_ → ioError (userError "flush") }) ] $ \(step, expected, ops) → do
                dir ← freshDir ("fail-" ⧺ step)
                r ← writeUniqueDurablyWith ops dir "scenario-capture-tmp" (BC.pack "bytes")
                case r of
                    Left (got, mPath, _) → do
                        (step, got) `shouldBe` (step, expected)
                        -- only a failure after creation names a file
                        (step, isJust mPath) `shouldBe` (step, expected ≢ StepOpen)
                    Right _ → expectationFailure ("a failure at " ⧺ step ⧺ " was reported as success")
        it "removes the request's own temporary and propagates, wherever the cancellation arrives after creation" $ \_ → do
            let real = productionOps
            forM_ [ ("read", real { opRead = \_ → throwIO ThreadKilled })
                  , ("link", real { opLink = \_ _ → throwIO ThreadKilled })
                  , ("replace", real { opReplace = \_ _ → throwIO ThreadKilled }) ] $ \(where_, ops) → do
                dir ← freshDir ("cancel-after-" ⧺ where_)
                let dest = dir </> "out.yaml"
                    replacing = where_ ≡ "replace"
                -- a replacement has an existing file to leave alone; the others publish a new one
                when replacing (BS.writeFile dest old)
                r ← try (writeScenarioFile ops StorageInput
                    { siRequest = CaptureRequest dest replacing, siBytes = BC.pack "version: 3\n"
                    , siVerify = const (Right ()), siAt = \_ → pure (), siPublished = pure () })
                case r of
                    Left e → (where_, cancelled (e ∷ SomeException)) `shouldBe` (where_, True)
                    Right _ → expectationFailure ("a cancellation during " ⧺ where_ ⧺ " was reported as a result")
                -- the destination is exactly as it was (still absent, or the old file) and no temporary remains
                listDir dir `shouldReturn` (if replacing then ["out.yaml"] else [])
                when replacing (BS.readFile dest `shouldReturn` old)
        it "after publication leaves the COMPLETE new file, with the temporary name removed" $ \_ → do
            dir ← freshDir "cancel-after-publish"
            let dest = dir </> "out.yaml"
                bytes = BC.pack "version: 3\n"
            r ← try (writeScenarioFile productionOps { opSyncDir = \_ → throwIO ThreadKilled } StorageInput
                { siRequest = CaptureRequest dest False, siBytes = bytes
                , siVerify = const (Right ()), siAt = \_ → pure (), siPublished = pure () })
            case r of
                Left e → cancelled (e ∷ SomeException) `shouldBe` True
                Right _ → expectationFailure "a cancellation during the directory sync was reported as a result"
            BS.readFile dest `shouldReturn` bytes
            listDir dir `shouldReturn` ["out.yaml"]

    describe "the writer on its own" $ do
        it "rejects a candidate the scenario boundary refuses, before publishing anything" $ \_ → do
            dir ← freshDir "writer-validate"
            let dest = dir </> "out.yaml"
            r ← writeScenarioFile productionOps StorageInput
                { siRequest = CaptureRequest dest False, siBytes = BC.pack "version: 3\n"
                , siVerify = const (Left "not a scenario"), siAt = \_ → pure (), siPublished = pure () }
            case r of
                Left f → (sfKind f, sfPhase f, sfPublication f) `shouldBe` (FailVerification, PhaseCandidateValidate, NotPublished)
                Right _ → expectationFailure "the candidate was published"
            listDir dir `shouldReturn` []

        it "publishes without replacing: the filesystem refuses a taken name, whenever it was taken" $ \_ → do
            dir ← freshDir "writer-exclusive"
            let src = dir </> "candidate"
                dst = dir </> "dest"
            BS.writeFile src (BC.pack "new")
            BS.writeFile dst old
            r ← publishExclusive src dst
            case r of
                ExclusiveDestinationExists → pure ()
                other → expectationFailure ("expected a refusal, got " ⧺ show other)
            BS.readFile dst `shouldReturn` old
            -- and a free name is taken completely
            let free = dir </> "free"
            r' ← publishExclusive src free
            case r' of
                ExclusivePublished → pure ()
                other → expectationFailure ("expected publication, got " ⧺ show other)
            BS.readFile free `shouldReturn` BC.pack "new"
            -- a symlink at the name counts as taken (it is never followed)
            createFileLink dst (dir </> "linked")
            r'' ← publishExclusive src (dir </> "linked")
            case r'' of
                ExclusiveDestinationExists → pure ()
                other → expectationFailure ("expected a refusal, got " ⧺ show other)
            BS.readFile dst `shouldReturn` old
  where
    head' xs = case xs of (x : _) → x; [] → ""
