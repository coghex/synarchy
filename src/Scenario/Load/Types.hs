{-# LANGUAGE Strict #-}
-- | The vocabulary of the paused scenario-load service (#2713, SCN-13 of
--   epic #2698): the source a caller hands in, the stages a request moves
--   through, the terminal result, and the coordination seams the test
--   suite drives. Free of any engine import so that
--   "Engine.Load.Status" can hold the request record beside the load
--   transaction it extends.
--
--   The contract each type carries is documented once, in
--   @docs/engine_contracts.md@ (§Scenario load). Everything here is
--   TRANSIENT coordination state: it names no saved value.
module Scenario.Load.Types
    ( -- * What a caller supplies
      ScenarioSource(..)
    , sourceIdentity
      -- * The request's stages and result
    , ScenarioLoadStage(..)
    , isTerminalStage
    , ScenarioLoadOutcome(..)
    , FailureCause(..)
    , FailureContext(..)
    , failureText
    , Recovery(..)
    , ScenarioLoadStatus(..)
    , initialScenarioStatus
      -- * What the continuation carries
    , ScenarioPlan(..)
      -- * Admission
    , Busy(..)
    , busyText
      -- * Test coordination
    , ScenarioLoadPoint(..)
    , ScenarioLoadSeams(..)
    , noSeams
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Scenario.Schema (ScenarioFormat)
import Scenario.Types (Scenario, ScenarioDiagnostic, ScenarioFailure)
import World.Arena.Bounds (ArenaBounds)

-- | A scenario source an entry-point caller has already RESOLVED (path
--   shorthand and launch-directory policy belong to SCN-15). Never
--   written by the service (D-24).
data ScenarioSource
    = SourceFile !FilePath
    | SourceBytes !Text !BS.ByteString
      -- ^ a document held in memory, under the name it is reported by
    deriving (Show, Eq)

-- | The name a status reports the source by.
sourceIdentity ∷ ScenarioSource → Text
sourceIdentity (SourceFile p)    = T.pack p
sourceIdentity (SourceBytes n _) = n

-- | Where a request is. Every stage before 'StageReady' is
--   non-terminal; 'StageReady' and 'StageFailed' are the two terminal
--   ones.
data ScenarioLoadStage
    = StageAdmitted
      -- ^ accepted: exclusion held, simulation paused
    | StagePreflight
      -- ^ reading, decoding and migrating the source; nothing constructed
    | StageStaging
      -- ^ the replacement shell is being built off to the side
    | StagePublishing
      -- ^ the shell is crossing the save barrier into the live session
    | StageReconciling
      -- ^ published; the session-bound Lua callbacks are running
    | StageConstructing
      -- ^ the scenario's content is being installed into the published page
    | StageRecovering
      -- ^ construction failed; a fresh default flat arena is being published
    | StageReady
    | StageFailed
    deriving (Show, Eq, Ord, Enum, Bounded)

isTerminalStage ∷ ScenarioLoadStage → Bool
isTerminalStage s = s ≡ StageReady ∨ s ≡ StageFailed

-- | What went wrong, by cause.
data FailureCause
    = CauseSource !ScenarioFailure
      -- ^ the file could not be read or understood (D-16, D-23): nothing
      --   was constructed and the current session stands
    | CauseNotTestingSession !Text
      -- ^ the live session holds an ordinary gameplay page (D-11)
    | CauseSetup !Text
      -- ^ the replacement could not be prepared or published
    | CauseConstruction !Text
      -- ^ an adapter or the content install failed after the shell was
      --   published
    | CauseReconciliation ![(Text, Text)]
      -- ^ the module and error of every session-bound callback that raised
    | CauseException !Text
      -- ^ a synchronous exception at an ownership handoff
    deriving (Show, Eq)

data FailureContext = FailureContext
    { fcStage   ∷ !ScenarioLoadStage
      -- ^ the stage the request had reached
    , fcCause   ∷ !FailureCause
    , fcDetails ∷ ![Text]
      -- ^ every further diagnostic, the recovery's own among them
    } deriving (Show, Eq)

-- | One line naming the stage and the cause, for the generic load status
--   and the log.
failureText ∷ FailureContext → Text
failureText (FailureContext stage cause details) =
    "scenario load failed at " <> T.pack (show stage) <> ": " <> causeText cause
    <> (if null details then "" else " [" <> T.intercalate "; " details <> "]")
  where
    causeText c = case c of
        CauseSource f → T.pack (show f)
        CauseNotTestingSession t → t
        CauseSetup t → t
        CauseConstruction t → t
        CauseReconciliation fs → T.intercalate "; " [ m <> ": " <> e | (m, e) ← fs ]
        CauseException t → t

data ScenarioLoadOutcome
    = ScenarioReady
    | ScenarioUnsuccessful !FailureContext
    deriving (Show, Eq)

-- | What became of the session after an unsuccessful request.
data Recovery
    = RecoveryNone
      -- ^ nothing needed recovering (the request is in flight or ready)
    | RecoveryPreserved
      -- ^ the request failed BEFORE the session was replaced: the previous
      --   session is exactly as it was
    | RecoveryFlatArena
      -- ^ the replacement was published and construction failed; a fresh
      --   default flat arena stands, paused. NOT the requested scenario.
    | RecoveryIncomplete
      -- ^ the replacement was published and is incompletely initialized;
      --   no promise is made about the previous session
    | RecoveryFailed !Text
      -- ^ construction failed and so did recovery; the session is unusable
      --   until another load replaces it
    deriving (Show, Eq)

-- | The request's authoritative record, keyed by the load transaction's
--   request id.
data ScenarioLoadStatus = ScenarioLoadStatus
    { slsRequestId ∷ !Int
    , slsSource    ∷ !Text
    , slsStage     ∷ !ScenarioLoadStage
    , slsWarnings  ∷ ![ScenarioDiagnostic]
      -- ^ every rejection, across every phase, in the order found
    , slsOutcome   ∷ !(Maybe ScenarioLoadOutcome)
    , slsPublished ∷ !Bool
      -- ^ whether a replacement session was ever published
    , slsRecovery  ∷ !Recovery
    , slsPendingFailure ∷ !(Maybe FailureContext)
      -- ^ construction failed and recovery is publishing the flat arena:
      --   the failure the request ends with once recovery has finished
    } deriving (Show, Eq)

initialScenarioStatus ∷ Int → Text → ScenarioLoadStatus
initialScenarioStatus rid src = ScenarioLoadStatus
    { slsRequestId = rid, slsSource = src, slsStage = StageAdmitted
    , slsWarnings = [], slsOutcome = Nothing, slsPublished = False
    , slsRecovery = RecoveryNone, slsPendingFailure = Nothing }

-- | What the Lua-thread continuation needs once the replacement shell has
--   been published: the decoded scenario and the bounds its page was
--   created with. Transient, keyed by request id, never saved.
data ScenarioPlan = ScenarioPlan
    { plScenario ∷ !Scenario
    , plBounds   ∷ !(Maybe ArenaBounds)
    } deriving (Show, Eq)

-- | An immediate rejection: the request was not accepted, no identity was
--   allocated, and the operation that held the lock is untouched.
data Busy
    = BusySave
    | BusyLoad
    deriving (Show, Eq)

busyText ∷ Busy → Text
busyText BusySave = "a save transaction is already active"
busyText BusyLoad = "a load transaction is already active"

-- | The named instants at which the suite holds the service or injects a
--   failure. A seam is consulted at the instant the name says and is
--   inert (one read) unless armed.
data ScenarioLoadPoint
    = PointAdmitted
    | PointPreflight
      -- ^ before the source is read
    | PointPreflighted
      -- ^ the source decoded; nothing constructed yet
    | PointStagingHandoff
      -- ^ on the Lua thread, before the staging command is handed over
    | PointStaging
      -- ^ on the world thread, entering staging
    | PointPublishHandoff
      -- ^ on the Lua thread, before the publish barrier opens
    | PointPublished
      -- ^ the session swap happened; reconciliation has not run
    | PointReconciled
      -- ^ reconciliation ran; construction has not started
    | PointTerrain
    | PointUnits
    | PointPlacement
      -- ^ the three construction steps, each reached just before it runs
    | PointBarrier
      -- ^ on the Lua thread, the publish barrier is held: every owner has
      --   acknowledged and is parked, the Lua state is not yet applied
    | PointInitialized
      -- ^ the content is installed; readiness has not been declared
    | PointRecovery
      -- ^ construction failed; recovery is about to be staged
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | The suite's hook. 'seamAt' may block (a held request) or throw (an
--   injected fault); the default does neither. 'seamFormat' replaces the
--   shipped format for the request's decode (a migration that fails cannot
--   be authored from a shipped chain, whose steps are all the identity).
data ScenarioLoadSeams = ScenarioLoadSeams
    { seamAt     ∷ ScenarioLoadPoint → IO ()
    , seamFormat ∷ Maybe ScenarioFormat
    }

noSeams ∷ ScenarioLoadSeams
noSeams = ScenarioLoadSeams { seamAt = \_ → pure (), seamFormat = Nothing }
