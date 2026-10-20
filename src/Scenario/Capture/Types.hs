{-# LANGUAGE Strict #-}
-- | The vocabulary of the scenario-capture service (#2714, SCN-14 of epic
--   #2698): what a caller asks for, the stages a request moves through, the
--   terminal result, the storage phases the file writer reports, and the
--   coordination seams the test suite drives. Free of any engine import so
--   that "Engine.Load.Status" can hold the request record beside the
--   operation record #2713 put there, and "World.Command.Types" and
--   "Engine.Scripting.Lua.Types" can carry a request.
--
--   The contract each type carries is documented once, in
--   @docs/engine_contracts.md@ (§Scenario capture). Everything here is
--   TRANSIENT coordination state: it names no saved value.
module Scenario.Capture.Types
    ( -- * What a caller supplies
      CaptureRequest(..)
      -- * The request's stages and result
    , CaptureStage(..)
    , isTerminalCaptureStage
    , CapturePublication(..)
    , FailureKind(..)
    , StoragePhase(..)
    , phaseText
    , CaptureFailure(..)
    , captureFailureText
    , CaptureReport(..)
    , CaptureOutcome(..)
    , ScenarioCaptureStatus(..)
    , initialCaptureStatus
      -- * Test coordination
    , CapturePoint(..)
    , StorageOps(..)
    , CaptureSeams(..)
    , noCaptureSeams
    ) where

import UPrelude
import Control.Exception (SomeException)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Scenario.Types (Scenario, ScenarioDiagnostic(..))
import World.Save.Storage.Durable (ExclusiveOutcome, WriteStep)

-- | What an entry-point caller hands in. The destination is RESOLVED by the
--   caller (filename shorthand, the scenario folder and overwrite
--   confirmation belong to SCN-15); overwrite intent is explicit.
data CaptureRequest = CaptureRequest
    { crDestination ∷ !FilePath
    , crOverwrite   ∷ !Bool
      -- ^ may an existing file at the destination be replaced? Without it a
      --   new file may be created and an existing one is refused, at
      --   publication as well as in preflight
    } deriving (Show, Eq)

-- | Where a request is. Every stage before 'CaptureDone' is non-terminal;
--   'CaptureDone' and 'CaptureFailed' are the two terminal ones.
data CaptureStage
    = CaptureAdmitted
      -- ^ accepted: exclusion held, simulation paused
    | CapturePreflight
      -- ^ the destination is being judged; nothing was coordinated yet
    | CaptureCoordinating
      -- ^ waiting for every state owner to reach the snapshot boundary
    | CaptureSnapshotting
      -- ^ at the boundary: the session is being read and projected
    | CaptureEncoding
      -- ^ the projection is being encoded and validated, still inside the
      --   capture window
    | CaptureStoring
      -- ^ the owners have resumed; the immutable bytes are being written
    | CaptureDone
    | CaptureFailed
    deriving (Show, Eq, Ord, Enum, Bounded)

isTerminalCaptureStage ∷ CaptureStage → Bool
isTerminalCaptureStage s = s ≡ CaptureDone ∨ s ≡ CaptureFailed

-- | Whether the destination now holds the new complete file. Exposed on
--   every failure, because it decides what recovery means: before
--   publication the previous destination (or its absence) is exactly as it
--   was; after it the complete NEW file stands and nothing promises the old
--   one survived.
data CapturePublication
    = NotPublished
    | Published
    deriving (Show, Eq)

-- | The distinguishable ways a request fails.
data FailureKind
    = FailCoordination
      -- ^ the owners did not reach the boundary, or the request is stale
    | FailNotTestingSession
      -- ^ the live session holds an ordinary gameplay page, or no arena
    | FailProjection
      -- ^ the supported snapshot is inconsistent or has content the schema
      --   cannot state: capture refuses rather than dropping it
    | FailValidation
      -- ^ the encoded document is not accepted by the shared schema boundary
    | FailEncoding
      -- ^ a deferred exception surfaced while the document was encoded
    | FailDestination
      -- ^ the destination cannot be written to at all (no such directory,
      --   not a regular file, a symlink)
    | FailOverwriteRefused
      -- ^ a file already names the destination and overwrite was not asked
    | FailTemporaryWrite
      -- ^ creating, writing or flushing the request's own temporary file
    | FailVerification
      -- ^ the candidate bytes did not read back as the captured document
    | FailPublication
      -- ^ the atomic publication itself failed
    | FailDurability
      -- ^ the file was published but the directory entry could not be made
      --   durable: it is NOT reported as saved
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Every phase the file writer can fail in, in the order a real
--   publication reaches them. Scenario-specific: ordinary-save storage's
--   own phase list is slot- and generation-specific and is not reused. Each
--   maps onto Durable's 'WriteStep' where one applies ('writeStepPhase').
data StoragePhase
    = PhaseDestinationCheck
    | PhaseTemporaryCreate
    | PhaseTemporaryWrite
    | PhaseTemporaryFlush
    | PhaseCandidateReread
    | PhaseCandidateValidate
    | PhasePublish
    | PhaseTemporaryRemove
      -- ^ after publication: a leftover temporary, never a failure
    | PhaseDirectorySync
    deriving (Show, Eq, Ord, Enum, Bounded)

phaseText ∷ StoragePhase → Text
phaseText = \case
    PhaseDestinationCheck → "destination check"
    PhaseTemporaryCreate → "temporary file creation"
    PhaseTemporaryWrite → "temporary file write"
    PhaseTemporaryFlush → "temporary file flush"
    PhaseCandidateReread → "candidate re-read"
    PhaseCandidateValidate → "candidate validation"
    PhasePublish → "publication"
    PhaseTemporaryRemove → "temporary file removal"
    PhaseDirectorySync → "directory sync"

-- | Why a request ended unsuccessful.
data CaptureFailure = CaptureFailure
    { cfStage       ∷ !CaptureStage
      -- ^ the stage the request had reached
    , cfKind        ∷ !FailureKind
    , cfPhase       ∷ !(Maybe StoragePhase)
      -- ^ the storage phase, for a failure of the file writer
    , cfDestination ∷ !FilePath
    , cfPublication ∷ !CapturePublication
    , cfDetail      ∷ !Text
      -- ^ the actionable cause
    , cfDiagnostics ∷ ![ScenarioDiagnostic]
      -- ^ every finding behind a projection or validation failure
    , cfWarnings    ∷ ![Text]
      -- ^ non-fatal findings (a leftover temporary this request could not
      --   remove)
    } deriving (Show, Eq)

-- | One line naming the stage, the kind, the destination and the cause, for
--   the log and the generic transaction status.
captureFailureText ∷ CaptureFailure → Text
captureFailureText f =
    "scenario capture failed at " <> tshow (cfStage f) <> " (" <> tshow (cfKind f)
    <> maybe "" (\p → ", " <> phaseText p) (cfPhase f) <> ") for "
    <> T.pack (cfDestination f) <> ": " <> cfDetail f
    <> (if cfPublication f ≡ Published then " [the new file WAS published]" else "")
    <> (if null (cfDiagnostics f) then ""
        else " [" <> T.intercalate "; " (map diagText (take 8 (cfDiagnostics f)))
             <> (if length (cfDiagnostics f) > 8 then "; …" else "") <> "]")
  where diagText d = sdPath d <> ": " <> tshow (sdReason d)

-- | What a successful request wrote.
data CaptureReport = CaptureReport
    { crpDestination ∷ !FilePath
    , crpBytes       ∷ !Int
    , crpReplaced    ∷ !Bool
      -- ^ an existing file was replaced (explicit overwrite)
    , crpEntries     ∷ ![(Text, Int)]
      -- ^ the number of entries written per family
    , crpWarnings    ∷ ![Text]
      -- ^ a leftover temporary this request could not remove
    } deriving (Show, Eq)

data CaptureOutcome
    = CaptureSucceeded !CaptureReport
    | CaptureUnsuccessful !CaptureFailure
    deriving (Show, Eq)

-- | The request's authoritative record, keyed by the barrier transaction's
--   request id.
data ScenarioCaptureStatus = ScenarioCaptureStatus
    { cstRequestId   ∷ !Int
    , cstRequest     ∷ !CaptureRequest
    , cstStage       ∷ !CaptureStage
    , cstOutcome     ∷ !(Maybe CaptureOutcome)
    , cstPublication ∷ !CapturePublication
      -- ^ updated the moment the destination holds the new file, so it is
      --   truthful even while the request is still finishing
    } deriving (Show, Eq)

initialCaptureStatus ∷ Int → CaptureRequest → ScenarioCaptureStatus
initialCaptureStatus rid req = ScenarioCaptureStatus
    { cstRequestId = rid, cstRequest = req, cstStage = CaptureAdmitted
    , cstOutcome = Nothing, cstPublication = NotPublished }

-- | The named instants at which the suite holds the service or injects a
--   failure. A seam is consulted at the instant the name says and is inert
--   (one read) unless armed.
data CapturePoint
    = CpAdmitted
      -- ^ accepted and paused, not yet handed to the Lua thread
    | CpPreflight
      -- ^ on the Lua thread, before the destination is judged
    | CpCoordinating
      -- ^ on the Lua thread, before it waits for the owners
    | CpBoundary
      -- ^ on the world thread: every owner has acknowledged and is parked,
      --   nothing has been read yet
    | CpSnapshotted
      -- ^ the session was read; nothing is projected yet
    | CpEncoded
      -- ^ the document is fully encoded and validated; the capture lock is
      --   still held
    | CpReleased
      -- ^ the capture lock was released; storage has not started
    | CpBeforePublish
      -- ^ the candidate is verified; the destination has not been touched
    | CpPublished
      -- ^ the destination holds the new file; the directory is not synced
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | The filesystem operations the writer performs, each replaceable so a
--   test can fail exactly one of them without depending on timing or on
--   permissions a root-run suite bypasses. The production set is
--   "Scenario.Capture.Storage"'s @productionOps@.
data StorageOps = StorageOps
    { opCreate    ∷ FilePath → String → BS.ByteString
                  → IO (Either (WriteStep, Maybe FilePath, SomeException) FilePath)
      -- ^ write and flush the bytes into a NEW uniquely named file
    , opRead      ∷ FilePath → IO BS.ByteString
    , opLink      ∷ FilePath → FilePath → IO ExclusiveOutcome
      -- ^ publish without replacing
    , opReplace   ∷ FilePath → FilePath → IO ()
      -- ^ publish over whatever is there, atomically
    , opRemove    ∷ FilePath → IO ()
    , opSyncDir   ∷ FilePath → IO ()
    , opExists    ∷ FilePath → IO Bool
    }

-- | The suite's hooks. 'seamAt' may block (a held request) or throw (an
--   injected fault); the default does neither. 'cseamOps' replaces the
--   production filesystem operations.
--   'cseamScenario' and 'cseamBytes' transform the projection and the encoded
--   bytes INSIDE the region the service evaluates before it releases the
--   capture lock, so a suite can raise a deferred exception from a thunk
--   exactly where a real one would surface.
data CaptureSeams = CaptureSeams
    { cseamAt       ∷ CapturePoint → IO ()
    , cseamOps      ∷ Maybe StorageOps
    , cseamScenario ∷ Scenario → Scenario
    , cseamBytes    ∷ BS.ByteString → BS.ByteString
    }

noCaptureSeams ∷ CaptureSeams
noCaptureSeams = CaptureSeams
    { cseamAt = \_ → pure (), cseamOps = Nothing, cseamScenario = id, cseamBytes = id }
