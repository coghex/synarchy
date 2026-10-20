{-# LANGUAGE Strict #-}
-- | Publishing a captured scenario document as one complete file (#2714,
--   SCN-14 of epic #2698). Built from "World.Save.Storage.Durable"'s
--   primitives, as the generated-world library is; it is NOT a second
--   ordinary-save transaction and shares no phase list with one.
--
--   Receives ONLY the destination, the overwrite intent and the complete,
--   already-encoded bytes. It never reads live gameplay state.
--
--   == The sequence
--
--   1. __Destination check.__ The directory must exist; the destination must
--      not be a symlink (the managed-path containment guard) or anything but
--      a regular file; an existing file is refused unless overwrite was
--      asked.
--   2. __Temporary file.__ A uniquely named file created EXCLUSIVELY next to
--      the destination (so it is on the destination's filesystem), written
--      and flushed to disk. It is owned from the moment it exists: cleanup
--      removes exactly this path and nothing else.
--   3. __Candidate verification.__ The temporary file is read back and
--      checked byte for byte against what was written, then decoded and
--      validated through the scenario boundary.
--   4. __Atomic publication.__ Without overwrite permission: a hard link,
--      which the filesystem refuses when the name is taken, so a file that
--      appears during the capture is never replaced. With it: a rename,
--      which replaces the destination in one step. The destination is never
--      streamed into or truncated, so it is never a partial document.
--   5. __Cleanup and durability.__ The temporary name is removed (a failure
--      here is a warning: the destination is complete) and the containing
--      directory is synced. Success is reported only after that sync.
--
--   == What a failure means
--
--   Before step 4 the destination is untouched: an existing file is intact
--   and a new one is absent. From step 4 on the complete new file stands,
--   and a failure reports that it was published and which step is left,
--   never that the old file survived. The durability boundary is plain POSIX
--   @fsync@ of the file and the directory, exactly as ordinary saves
--   document it ("World.Save.Storage"); nothing stronger is claimed.
module Scenario.Capture.Storage
    ( productionOps
    , writeStepPhase
    , DestinationVerdict(..)
    , checkDestination
    , StorageInput(..)
    , StorageSuccess(..)
    , StorageFailure(..)
    , writeScenarioFile
    ) where

import UPrelude
import Control.Exception
    (SomeAsyncException, SomeException, displayException, fromException, onException, throwIO, try)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import System.Directory
    (doesDirectoryExist, doesFileExist, doesPathExist, removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName)
import Scenario.Capture.Types
import World.Save.Storage.Durable
    ( ExclusiveOutcome(..), WriteStep(..), publishExclusive, rejectSymlinkedPath
    , syncDirectory, writeUniqueDurably )

-- | The real filesystem.
productionOps ∷ StorageOps
productionOps = StorageOps
    { opCreate = writeUniqueDurably
    , opRead = BS.readFile
    , opLink = publishExclusive
    , opReplace = renameFile
    , opRemove = removeFile
    , opSyncDir = syncDirectory
    , opExists = doesPathExist }

-- | The storage phase a failed 'WriteStep' belongs to.
writeStepPhase ∷ WriteStep → StoragePhase
writeStepPhase = \case
    StepOpen → PhaseTemporaryCreate
    StepWrite → PhaseTemporaryWrite
    StepFlush → PhaseTemporaryFlush

-- | What the destination check found.
data DestinationVerdict
    = DestinationFree
    | DestinationWillReplace
      -- ^ a regular file is there and overwrite was asked
    | DestinationRejected !FailureKind !Text
    deriving (Show, Eq)

-- | Judge the destination before anything is written. Pure observation: it
--   touches nothing. The verdict is advisory for the publication that
--   follows, which enforces the overwrite rule itself.
checkDestination ∷ StorageOps → CaptureRequest → IO DestinationVerdict
checkDestination ops req = do
    let dest = crDestination req
        dir = takeDirectory dest
    dirOk ← doesDirectoryExist dir
    if null (takeFileName dest)
        then pure (DestinationRejected FailDestination "the destination names no file")
    else if not dirOk
        then pure (DestinationRejected FailDestination
                ("the destination directory does not exist: " <> T.pack dir))
    else do
        symlink ← rejectSymlinkedPath dest
        case symlink of
            Left why → pure (DestinationRejected FailDestination why)
            Right () → do
                present ← opExists ops dest
                isFile ← doesFileExist dest
                pure $ case (present, isFile, crOverwrite req) of
                    (False, _, _) → DestinationFree
                    (True, False, _) → DestinationRejected FailDestination
                        "the destination exists and is not a regular file"
                    (True, True, True) → DestinationWillReplace
                    (True, True, False) → DestinationRejected FailOverwriteRefused
                        "a file already exists at the destination and overwrite was not requested"

-- | What the writer is handed.
data StorageInput = StorageInput
    { siRequest  ∷ !CaptureRequest
    , siBytes    ∷ !BS.ByteString
    , siVerify   ∷ BS.ByteString → Either Text ()
      -- ^ decode and validate the candidate through the scenario boundary
    , siAt       ∷ CapturePoint → IO ()
    , siPublished ∷ IO ()
      -- ^ told the instant the destination holds the new file
    }

data StorageSuccess = StorageSuccess
    { ssReplaced ∷ !Bool
    , ssWarnings ∷ ![Text]
    } deriving (Show, Eq)

data StorageFailure = StorageFailure
    { sfKind        ∷ !FailureKind
    , sfPhase       ∷ !StoragePhase
    , sfPublication ∷ !CapturePublication
    , sfDetail      ∷ !Text
    , sfWarnings    ∷ ![Text]
    } deriving (Show, Eq)

-- | Run one operation, turning a synchronous exception into a value and
--   letting an asynchronous one (a shutdown) through.
attempt ∷ IO α → IO (Either SomeException α)
attempt act = do
    r ← try act
    case r of
        Left e | Just (_ ∷ SomeAsyncException) ← fromException e → throwIO e
        _ → pure r

render ∷ SomeException → Text
render = T.pack ∘ displayException

-- | Publish @siBytes@ at the destination. See the module header for the
--   sequence and for what each failure promises.
writeScenarioFile ∷ StorageOps → StorageInput → IO (Either StorageFailure StorageSuccess)
writeScenarioFile ops inp = do
    let req = siRequest inp
        dest = crDestination req
        dir = takeDirectory dest
    verdict ← checkDestination ops req
    case verdict of
        DestinationRejected kind why → pure (Left (failBefore kind PhaseDestinationCheck why []))
        _ → do
            created ← opCreate ops dir "scenario-capture-tmp" (siBytes inp)
            case created of
                Left (step, mTmp, e) → do
                    warns ← maybe (pure []) (cleanup ops) mTmp
                    pure (Left (failBefore FailTemporaryWrite (writeStepPhase step)
                        ("the temporary file could not be " <> stepVerb step <> ": " <> render e) warns))
                -- a thread cancellation anywhere after the file exists removes
                -- the request's own temporary on the way out (never the
                -- destination), and propagates unchanged
                Right tmp → afterCreate ops inp tmp (verdict ≡ DestinationWillReplace)
                                `onException` void (cleanup ops tmp)
  where
    stepVerb s = case s of
        StepOpen → "created"
        StepWrite → "written"
        StepFlush → "flushed to disk"

-- | The destination is untouched until publication, so every failure before
--   it removes the request's own temporary file and reports 'NotPublished'.
failBefore ∷ FailureKind → StoragePhase → Text → [Text] → StorageFailure
failBefore kind phase why warns = StorageFailure kind phase NotPublished why warns

-- | Remove the request's own temporary file; a failure is a warning, never
--   an error (the destination does not depend on it).
cleanup ∷ StorageOps → FilePath → IO [Text]
cleanup ops tmp = do
    r ← attempt (opRemove ops tmp)
    pure $ case r of
        Right () → []
        Left e → [ "the temporary file " <> T.pack tmp <> " could not be removed: " <> render e ]

afterCreate ∷ StorageOps → StorageInput → FilePath → Bool
            → IO (Either StorageFailure StorageSuccess)
afterCreate ops inp tmp willReplace = do
    let abort kind phase why = do
            warns ← cleanup ops tmp
            pure (Left (failBefore kind phase why warns))
    reread ← attempt (opRead ops tmp)
    case reread of
        Left e → abort FailVerification PhaseCandidateReread
                    ("the candidate could not be read back: " <> render e)
        Right back
            | back ≢ siBytes inp → abort FailVerification PhaseCandidateReread
                ("the candidate read back as " <> tshow (BS.length back) <> " bytes that differ from the "
                 <> tshow (BS.length (siBytes inp)) <> " written")
            | otherwise → case siVerify inp back of
                Left why → abort FailVerification PhaseCandidateValidate
                    ("the candidate was rejected by the scenario boundary: " <> why)
                Right () → do
                    seamed ← attempt (siAt inp CpBeforePublish)
                    case seamed of
                        Left e → abort FailPublication PhasePublish
                            ("publication did not start: " <> render e)
                        Right () → publishStep
  where
    req' = siRequest inp
    dest' = crDestination req'
    dir' = takeDirectory dest'
    publishStep
        | crOverwrite req' = do
            r ← attempt (opReplace ops tmp dest')
            case r of
                Left e → do
                    warns ← cleanup ops tmp
                    pure (Left (failBefore FailPublication PhasePublish
                        ("the destination could not be replaced: " <> render e) warns))
                Right () → published []
        | otherwise = do
            r ← attempt (opLink ops tmp dest')
            case r of
                Left e → do
                    warns ← cleanup ops tmp
                    pure (Left (failBefore FailPublication PhasePublish
                        ("publication failed: " <> render e) warns))
                Right ExclusivePublished → do
                    -- the temporary name is a second name for the published
                    -- file: removing it is cleanup, never a failure
                    warns ← cleanup ops tmp
                    published warns
                Right ExclusiveDestinationExists → do
                    warns ← cleanup ops tmp
                    pure (Left (failBefore FailOverwriteRefused PhasePublish
                        "a file appeared at the destination while the capture ran and overwrite was not requested"
                        warns))
                Right (ExclusiveFailed e) → do
                    warns ← cleanup ops tmp
                    pure (Left (failBefore FailPublication PhasePublish
                        ("the filesystem refused to publish without replacing (a filesystem without hard "
                         <> "links cannot; request an explicit overwrite to publish by rename): "
                         <> tshow e) warns))
    published warns = do
        siPublished inp
        seamed ← attempt (siAt inp CpPublished)
        let afterFail why = pure (Left (StorageFailure FailPublication PhasePublish Published why warns))
        case seamed of
            Left e → afterFail ("publication completed but the request was interrupted: " <> render e)
            Right () → do
                synced ← attempt (opSyncDir ops dir')
                pure $ case synced of
                    Left e → Left StorageFailure
                        { sfKind = FailDurability, sfPhase = PhaseDirectorySync
                        , sfPublication = Published
                        , sfDetail = "the complete file was published but the directory could not be synced, so it "
                                     <> "is not reported as saved: " <> render e
                        , sfWarnings = warns }
                    Right () → Right (StorageSuccess { ssReplaced = willReplace, ssWarnings = warns })
