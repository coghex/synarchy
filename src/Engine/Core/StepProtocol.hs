-- | Dormant, pure coordinated-step model (#2482). No worker consumes this
-- module. Rational seconds are the accounting domain; conversion to a public
-- floating-point clock is a separate, rounded presentation operation.
--
-- Constructors of 'Protocol' and 'Scoped' are private. 'inspectProtocol'
-- returns a detached diagnostic value, never a record update surface for the
-- machine. Rejected transitions return no successor and apply no model work.
module Engine.Core.StepProtocol
    ( Protocol, Mode(..), Owner(..), Stage(..), Phase(..), Grant(..)
    , Progress(..), StepView(..), View(..), Rejection(..), Scoped
    , newProtocol, replaceSession, inspectProtocol, committedSeconds
    , scopeValue, resolveScoped, sampleWall, startStep, nextGrant
    , acceptGrant, applyBatch, suspendPhase, resumePhase, acknowledgePhase
    , requestPause, resumeProtocol, enterTransaction, finishTransaction
    , faultProtocol
    ) where

import UPrelude
import Data.List (nub)
import Engine.Core.Clock (sanitiseElapsed)

-- | Logical execution owners; building commands share the unit owner today.
data Owner = UnitOwner | CombatOwner | FluidOwner | WorldOwner | LuaOwner
    deriving (Eq, Ord, Show)

-- | Semantic stages, with potentially several ordered owner phases per stage.
data Stage = Admission | Motion | Consequences | Productive | Decisions | Completion
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Position distinguishes repeated visits to the same owner or stage.
data Phase = Phase
    { phasePosition ∷ Int, phaseStage ∷ Stage, phaseOwner ∷ Owner }
    deriving (Eq, Show)

data Mode = Boundary | RunningStep | Paused | Transaction | Faulted
    deriving (Eq, Show)

-- | A complete immutable interval and routing identity. External messages
-- can be malformed: every transition validates the entire record.
data Grant = Grant
    { grantEpoch ∷ Integer
    , grantStep ∷ Integer
    , grantPhase ∷ Phase
    , grantStart ∷ Rational
    , grantEnd ∷ Rational
    , grantDuration ∷ Rational
    } deriving (Eq, Show)

data Progress = AwaitingGrant | Executing | Suspended
    deriving (Eq, Show)

-- | Captured membership, accepted external prefix, and cursor survive yields.
-- Applying a batch models one effect per member, in membership order. Domain
-- effects and publication remain the later adapters' responsibility.
data StepView = StepView
    { stepGrant ∷ Grant
    , stepMembers ∷ [Integer]
    , stepPrefix ∷ [Integer]
    , stepCursor ∷ Int
    , stepProgress ∷ Progress
    , stepAcknowledged ∷ [Phase]
    , stepApplied ∷ [(Phase, Integer)]
    } deriving (Eq, Show)

-- | Read-only copy of the machine's accounting and in-flight diagnostic state.
data View = View
    { viewEpoch ∷ Integer
    , viewCompleted ∷ Integer
    , viewSeconds ∷ Rational
    , viewMode ∷ Mode
    , viewPauseRequested ∷ Bool
    , viewDemand ∷ Rational
    , viewDiscardedSeconds ∷ Rational
    , viewDiscardCount ∷ Integer
    , viewStep ∷ Maybe StepView
    } deriving (Eq, Show)

data Rejection = InvalidParameters | WrongMode | InsufficientDemand
               | StaleScope | InvalidMembership | WrongGrant
               | WrongProgress | InvalidBatch | IncompletePhase
    deriving (Eq, Show)

-- | Runtime identity wrapper for retained command prefixes and cadence state.
-- Saved cadence progress can be wrapped afresh after replacement; an old
-- runtime cursor can never be reinterpreted as belonging to that session.
data Scoped α = Scoped Integer α deriving (Eq, Show)

data Protocol = Protocol
    { pEpoch ∷ Integer, pOrigin ∷ Rational, pRate ∷ Rational
    , pCap ∷ Rational, pPhases ∷ [Phase], pCompleted ∷ Integer
    , pMode ∷ Mode, pPause ∷ Bool, pDemand ∷ Rational
    , pDiscarded ∷ Rational, pDiscards ∷ Integer
    , pLastWall ∷ Maybe Double, pStep ∷ Maybe StepView
    } deriving (Eq, Show)

-- | All rates, origins and caps are finite rationals. A rate must be positive,
-- an origin nonnegative, and a cap must hold at least one interval. Epochs
-- are positive unbounded integers. Every semantic stage must occur, in order;
-- adjacent owner phases may share a stage. There are no shipping defaults.
newProtocol
    ∷ Integer → Rational → Rational → Rational → [(Stage, Owner)]
    → Either Rejection Protocol
newProtocol epoch origin rate cap plan
    | epoch ≤ 0 ∨ origin < 0 ∨ rate ≤ 0 = Left InvalidParameters
    | cap < 1 / rate = Left InvalidParameters
    | nub (map fst plan) ≢ [minBound .. maxBound]
        ∨ not (and (zipWith (≤) (map fst plan) (drop 1 (map fst plan))))
        = Left InvalidParameters
    | otherwise = Right Protocol
        { pEpoch = epoch, pOrigin = origin, pRate = rate, pCap = cap
        , pPhases = zipWith (\i (stage, owner) → Phase i stage owner) [0..] plan
        , pCompleted = 0, pMode = Boundary, pPause = False, pDemand = 0
        , pDiscarded = 0, pDiscards = 0, pLastWall = Nothing, pStep = Nothing }

-- | Replacement is an explicit restoration operation, even after a fault.
-- Require increasing epochs so a previously used token cannot become current
-- again. The new origin may be saved gameplay seconds; no wire format is added.
replaceSession ∷ Integer → Rational → Protocol → Either Rejection Protocol
replaceSession epoch origin p
    | epoch ≤ pEpoch p = Left StaleScope
    | otherwise = newProtocol epoch origin (pRate p) (pCap p)
        [(phaseStage phase, phaseOwner phase) | phase ← pPhases p]

committedSeconds ∷ Protocol → Rational
committedSeconds p = pOrigin p + fromInteger (pCompleted p) / pRate p

inspectProtocol ∷ Protocol → View
inspectProtocol p = View (pEpoch p) (pCompleted p) (committedSeconds p)
    (pMode p) (pPause p) (pDemand p) (pDiscarded p) (pDiscards p) (pStep p)

scopeValue ∷ Protocol → α → Scoped α
scopeValue p = Scoped (pEpoch p)

resolveScoped ∷ Protocol → Scoped α → Either Rejection α
resolveScoped p (Scoped epoch value)
    | epoch ≢ pEpoch p = Left StaleScope
    | otherwise = Right value

-- | Raw monotonic samples use the existing host-interruption sanitiser.
-- Only unstarted demand is capped. The first sample rebases, and pause,
-- transaction, resume and replacement all discard the old pacing baseline.
-- A paused sample can never create demand for later replay.
sampleWall ∷ Double → Rational → Protocol → Either Rejection Protocol
sampleWall now speed p
    | speed < 0 = Left InvalidParameters
    | otherwise = Right p
        { pLastWall = Just now, pDemand = kept
        , pDiscarded = pDiscarded p + discarded
        , pDiscards = pDiscards p + if discarded > 0 then 1 else 0 }
  where
    elapsed = maybe 0 (sanitiseElapsed . (now -)) (pLastWall p)
    eligible = not (pPause p) ∧ pMode p `elem` [Boundary, RunningStep]
    demand = if eligible then pDemand p + toRational elapsed * speed else 0
    kept = min (pCap p) demand
    discarded = max 0 (demand - kept)

-- | Admission debits exactly one interval. Its work is thereafter independent
-- of pacing demand, including when a pause or an overload clears that demand.
startStep ∷ Scoped [Integer] → [Integer] → Protocol → Either Rejection Protocol
startStep prefix members p
    | pMode p ≢ Boundary ∨ pPause p = Left WrongMode
    | pDemand p < duration = Left InsufficientDemand
    | nub members ≢ members = Left InvalidMembership
    | otherwise = do
        commands ← resolveScoped p prefix
        case pPhases p of
            [] → Left InvalidParameters
            phase:_ → Right p
                { pMode = RunningStep, pDemand = pDemand p - duration
                , pStep = Just (StepView (makeGrant p phase) members commands
                    0 AwaitingGrant [] []) }
  where
    duration = 1 / pRate p

makeGrant ∷ Protocol → Phase → Grant
makeGrant p phase = Grant (pEpoch p) (pCompleted p + 1) phase
    (committedSeconds p)
    (pOrigin p + fromInteger (pCompleted p + 1) / pRate p) (1 / pRate p)

nextGrant ∷ Protocol → Maybe Grant
nextGrant p
    | pMode p ≡ RunningStep = stepGrant <$> pStep p
    | otherwise = Nothing

-- | Match routing, epoch, ordinal, stage position AND interval. Checking only
-- epoch/ordinal would admit a future phase or a message to the wrong owner.
matchingStep ∷ Grant → Protocol → Either Rejection StepView
matchingStep grant p
    | pMode p ≢ RunningStep = Left WrongMode
    | otherwise = case pStep p of
        Just step | grant ≡ stepGrant step → Right step
        _ → Left WrongGrant

acceptGrant ∷ Grant → Protocol → Either Rejection Protocol
acceptGrant grant p = do
    step ← matchingStep grant p
    if stepProgress step ≢ AwaitingGrant
        then Left WrongProgress
        else Right p { pStep = Just step { stepProgress = Executing } }

-- | Credit the next finite prefix of the captured roster. A completed member
-- never executes twice within a phase. A zero/oversized batch is refused.
applyBatch ∷ Grant → Int → Protocol → Either Rejection Protocol
applyBatch grant count p = do
    step ← matchingStep grant p
    if stepProgress step ≢ Executing then Left WrongProgress
    else if count ≤ 0 ∨ count > length (stepMembers step) - stepCursor step
        then Left InvalidBatch
    else let members = take count (drop (stepCursor step) (stepMembers step))
         in Right p { pStep = Just step
            { stepCursor = stepCursor step + count
            , stepApplied = stepApplied step
                ⧺ [(grantPhase grant, member) | member ← members] } }

suspendPhase ∷ Grant → Protocol → Either Rejection Protocol
suspendPhase grant p = do
    step ← matchingStep grant p
    if stepProgress step ≢ Executing then Left WrongProgress
    else Right p { pStep = Just step { stepProgress = Suspended } }

resumePhase ∷ Grant → Protocol → Either Rejection Protocol
resumePhase grant p = do
    step ← matchingStep grant p
    if stepProgress step ≢ Suspended then Left WrongProgress
    else Right p { pStep = Just step { stepProgress = Executing } }

-- | Acknowledgement requires an accepted, fully processed phase. Only the last
-- phase commits time; a later phase cannot acknowledge on an earlier one's
-- behalf. Completed phases remain in the diagnostic in-flight ledger.
acknowledgePhase ∷ Grant → Protocol → Either Rejection Protocol
acknowledgePhase grant p = do
    step ← matchingStep grant p
    if stepProgress step ≢ Executing then Left WrongProgress
    else if stepCursor step ≢ length (stepMembers step) then Left IncompletePhase
    else case drop (phasePosition (grantPhase grant) + 1) (pPhases p) of
        phase:_ → Right p { pStep = Just step
            { stepGrant = makeGrant p phase, stepCursor = 0
            , stepProgress = AwaitingGrant
            , stepAcknowledged = stepAcknowledged step ⧺ [grantPhase grant] } }
        [] → Right p
            { pCompleted = pCompleted p + 1, pStep = Nothing
            , pMode = if pPause p then Paused else Boundary }

-- | Pause is immediately observable and prevents admission. Already admitted
-- work retains its grant, membership, cursor and completion obligations.
requestPause ∷ Protocol → Protocol
requestPause p = p
    { pPause = True, pDemand = 0, pLastWall = Nothing
    , pMode = if pMode p ≡ Boundary then Paused else pMode p }

resumeProtocol ∷ Protocol → Either Rejection Protocol
resumeProtocol p
    | pMode p ≢ Paused = Left WrongMode
    | otherwise = Right p
        { pMode = Boundary, pPause = False, pDemand = 0, pLastWall = Nothing }

-- | Pure mode admission only. Real save/load permits and parking are GT-2.
enterTransaction ∷ Protocol → Either Rejection Protocol
enterTransaction p
    | pMode p `notElem` [Boundary, Paused] = Left WrongMode
    | otherwise = Right p
        { pMode = Transaction, pPause = True, pDemand = 0, pLastWall = Nothing }

finishTransaction ∷ Protocol → Either Rejection Protocol
finishTransaction p
    | pMode p ≢ Transaction = Left WrongMode
    | otherwise = Right p { pMode = Paused, pLastWall = Nothing }

-- | Retain the failed phase for diagnostics but publish no incomplete time.
-- Resume, grant, batch, acknowledgement and transaction paths refuse Faulted.
faultProtocol ∷ Protocol → Protocol
faultProtocol p = p { pMode = Faulted, pDemand = 0, pLastWall = Nothing }
