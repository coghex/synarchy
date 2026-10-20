{-# LANGUAGE Strict #-}
-- | The scenario-capture service (#2714, SCN-14 of epic #2698): one engine
--   service that turns the live testing arena into a scenario file, or
--   reports honestly why it could not, and leaves the arena PAUSED either
--   way. The contract is documented once, in @docs/engine_contracts.md@
--   (§Scenario capture).
--
--   == Shape
--
--   A capture IS a save-barrier transaction ("Engine.Save.Barrier"): the
--   same owner quiescence, the same final acknowledgement and parking, the
--   same capture lock and the same encode-before-release discipline an
--   ordinary save has, because a coherent snapshot of the state owners is
--   exactly what it needs. What differs is what is read from the snapshot
--   (the supported scenario baseline, projected, not an ordinary session)
--   and what is written (one YAML file, not a save slot).
--
--   1. 'requestScenarioCapture' (any thread) admits the request through the
--      shared admission boundary ('claimCapture'), pauses, and hands the
--      request to the Lua thread. A conflicting request is answered 'Busy'
--      at once and touches nothing.
--   2. 'startScenarioCapture' (Lua thread, which is the Lua owner) judges
--      the destination, acknowledges for Lua, waits for every other owner to
--      reach the snapshot boundary and queues the world-thread command.
--   3. 'runScenarioCapture' (world thread, inside the capture window): read
--      the session once, project it, encode it, validate the encoded bytes
--      through the scenario boundary — all fully evaluated HERE, so a
--      deferred exception can never escape into storage — then release the
--      capture lock so the owners resume.
--   4. The immutable bytes are written by "Scenario.Capture.Storage" while
--      operation exclusion and the pause continue. The request ends only
--      when that resolves; the record ends before the barrier transaction
--      does, so the next operation is admitted only after a reader can see
--      the result.
--
--   Nothing the capture does changes the arena: it consumes no gameplay
--   randomness, heals, refills and realizes nothing, clears no order, and
--   never replaces the arena with a reloaded copy.
module Scenario.Capture.Service
    ( requestScenarioCapture
    , startScenarioCapture
    , runScenarioCapture
    , readCaptureStatus
    , buildDocument
    , verifyDocument
    ) where

import UPrelude
import Control.Exception
    (SomeAsyncException, SomeException, displayException, evaluate, fromException, throwIO, try)
import Data.IORef (readIORef)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.Core (CoreCapability(..), toCoreCapability)
import Engine.Core.Capability.InputView (InputViewCapability(..), toInputViewCapability)
import Engine.Core.Capability.SaveLoad (SaveLoadCapability(..), toSaveLoadCapability)
import Engine.Core.Capability.WorldSim (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Log (LoggerState, logInfo, logWarn, LogCategory(..))
import Engine.Core.State (EngineEnv)
import Engine.Load.Status
import Engine.Save.Barrier
    ( SaveOutcome(..), SaveOwner(..), SaveStatus(..), acknowledgeSave, beginSave, failSave, finishSave
    , readSaveStatus, reachSnapshot, releaseCaptureLock, standardSaveOwners, waitForOwners )
import Engine.Scripting.Lua.Types (LuaMsg(..))
import Scenario.Capture.Encode (encodeScenario)
import Scenario.Capture.Inputs
import Scenario.Capture.Project
import Scenario.Capture.Storage
import Scenario.Capture.Types
import Scenario.Load.Catalog (liveScenarioCatalog)
import Scenario.Load.Session (arenaPageId)
import Scenario.Load.Types (Busy(..))
import Scenario.Schema (currentScenarioVersion, decodeScenarioBytesPaths, scenarioFormat)
import Scenario.Types
import World.Command.Types (WorldCommand(..))
import World.Pause (imposePause)

-- * Entry

-- | Ask for the live testing arena to be captured to @crDestination@.
--   Callable from any thread.
--
--   Admission is atomic with the check of every conflicting operation
--   ('claimCapture'): while an ordinary save, an autosave, an ordinary load,
--   a scenario load or another capture is active the answer is 'Busy'
--   immediately, no identity is allocated, nothing is queued and the active
--   operation's status and pause state are untouched. On acceptance the
--   simulation is paused SYNCHRONOUSLY, the request is handed to the Lua
--   thread, and its identity is returned; the outcome is read from
--   'readCaptureStatus'.
--
--   A synchronous exception between acceptance and the hand-off ends the
--   request unsuccessful rather than leaving it permanently non-terminal; a
--   thread cancellation propagates.
requestScenarioCapture ∷ EngineEnv → CaptureRequest → IO (Either Busy Int)
requestScenarioCapture env req = do
    let sl = toSaveLoadCapability env
        lsr = slLoadStatusRef sl
    inputActive ← readIORef (ccInputThreadActiveRef (toCoreCapability env))
    claimed ← claimCapture lsr
        (beginSave (slSaveBarrierRef sl) (standardSaveOwners inputActive)) req
    forM_ claimed $ \rid → guarded (exceptionFailure env rid FailCoordination) $ do
        imposePause (toWorldSimCapability env)
        captureSeamPoint lsr CpAdmitted
        Q.writeQueue (ivLuaQueue (toInputViewCapability env)) (LuaScenarioCapture rid req)
    pure claimed

-- | The capture's record, only while it belongs to the CURRENT save-barrier
--   transaction: a later operation supersedes it, so a stale record is never
--   reported under a newer request's name. A record the barrier has ended
--   without the record having been told is settled from the barrier's own
--   outcome, so a request never stays non-terminal under a terminal
--   transaction.
readCaptureStatus ∷ EngineEnv → IO (Maybe ScenarioCaptureStatus)
readCaptureStatus env = do
    let sl = toSaveLoadCapability env
    rec' ← readCaptureRecord (slLoadStatusRef sl)
    cur ← readSaveStatus (slSaveBarrierRef sl)
    pure $ case (rec', cur) of
        (Just r, Just b) | ssRequestId b ≡ cstRequestId r → Just (settle b r)
        _ → Nothing
  where
    settle b r = case (cstOutcome r, ssOutcome b) of
        (Nothing, Just (SaveAborted why)) → r
            { cstStage = CaptureFailed
            , cstOutcome = Just (CaptureUnsuccessful CaptureFailure
                { cfStage = cstStage r, cfKind = FailCoordination, cfPhase = Nothing
                , cfDestination = crDestination (cstRequest r), cfPublication = cstPublication r
                , cfDetail = why, cfDiagnostics = [], cfWarnings = [] }) }
        _ → r

-- * The Lua thread

-- | The Lua thread's turn on an accepted request: judge the destination,
--   acknowledge for Lua, wait for the other owners to reach the snapshot
--   boundary, and hand the request to the world thread.
startScenarioCapture ∷ EngineEnv → Int → CaptureRequest → IO ()
startScenarioCapture env rid req = guarded (exceptionFailure env rid FailCoordination) $ whenCurrent env rid $ do
    let sl = toSaveLoadCapability env
        lsr = slLoadStatusRef sl
        barrier = slSaveBarrierRef sl
    stageTo env rid CapturePreflight
    captureSeamPoint lsr CpPreflight
    ops ← operations env
    verdict ← checkDestination ops req
    case verdict of
        DestinationRejected kind why →
            endFailure env rid kind (Just PhaseDestinationCheck) NotPublished why [] []
        _ → do
            stageTo env rid CaptureCoordinating
            captureSeamPoint lsr CpCoordinating
            -- The Lua thread IS the Lua owner, and it blocks here: nothing
            -- it owns can run while the other owners drain.
            acknowledgeSave barrier rid SaveLua
            ready ← waitForOwners ownerWaitMicros barrier rid
            case ready of
                Left err → endFailure env rid FailCoordination Nothing NotPublished err [] []
                Right () → do
                    reachSnapshot barrier rid
                    Q.writeQueue (wsWorldQueue (toWorldSimCapability env)) (WorldScenarioCapture rid req)

-- | How long the Lua thread waits for the other owners, as an ordinary save
--   does.
ownerWaitMicros ∷ Int
ownerWaitMicros = 5000000

-- * The world thread

-- | The world thread's turn, inside the capture window: read, project,
--   encode and validate (all fully evaluated before the lock is released),
--   release, then write. See the module header.
runScenarioCapture ∷ EngineEnv → LoggerState → Int → CaptureRequest → IO ()
runScenarioCapture env logger rid req = guarded (exceptionFailure env rid FailProjection) $ whenCurrent env rid $ do
    let lsr = slLoadStatusRef (toSaveLoadCapability env)
        barrier = slSaveBarrierRef (toSaveLoadCapability env)
        dest = crDestination req
    stageTo env rid CaptureSnapshotting
    captureSeamPoint lsr CpBoundary
    page ← liveTestingPage env arenaPageId
    case page of
        Left why → endFailure env rid FailNotTestingSession Nothing NotPublished why [] []
        Right ws → do
            inputs ← readLiveInputs env arenaPageId ws
            snapped ← syncTry (captureSeamPoint lsr CpSnapshotted)
            case snapped of
                Left e → endFailure env rid FailProjection Nothing NotPublished
                    ("the capture was interrupted: " <> render e) [] []
                Right () → do
                    stageTo env rid CaptureEncoding
                    catalog ← liveScenarioCatalog env
                    seams ← seamTransforms env
                    -- fully evaluated HERE: an exception raised by any thunk
                    -- is caught inside the capture window, never in storage
                    built ← syncTry (evaluate (forceBuilt (buildWith seams catalog inputs)))
                    case built of
                        Left e → endFailure env rid FailEncoding Nothing NotPublished
                            ("an exception surfaced while the document was encoded: " <> render e) [] []
                        Right (Left (kind, why, diags)) →
                            endFailure env rid kind Nothing NotPublished why diags []
                        Right (Right (bytes, projection)) → do
                            sealed ← syncTry (captureSeamPoint lsr CpEncoded)
                            case sealed of
                                Left e → endFailure env rid FailEncoding Nothing NotPublished
                                    ("the capture was interrupted before release: " <> render e) [] []
                                Right () → do
                                    -- the bytes are immutable and complete: the owners
                                    -- may resume; exclusion and the pause continue
                                    releaseCaptureLock barrier rid
                                    stageTo env rid CaptureStoring
                                    released ← syncTry (captureSeamPoint lsr CpReleased)
                                    case released of
                                        Left e → endFailure env rid FailPublication Nothing NotPublished
                                            ("the capture was interrupted before storage: " <> render e) [] []
                                        Right () → store env logger rid req catalog projection bytes dest
  where
    forceBuilt r = case r of
        Left (k, t, ds) → k `seq` T.length t `seq` length ds `seq` r
        Right (b, p) → BS.length b `seq` length (prCounts p) `seq` r

-- | Write the verified bytes and end the request with what storage said.
store ∷ EngineEnv → LoggerState → Int → CaptureRequest → ScenarioCatalog → Projection
      → BS.ByteString → FilePath → IO ()
store env logger rid req catalog projection bytes dest = do
    let lsr = slLoadStatusRef (toSaveLoadCapability env)
    ops ← operations env
    result ← syncTry $ writeScenarioFile ops StorageInput
        { siRequest = req, siBytes = bytes
        , siVerify = verifyDocument catalog (prScenario projection)
        , siAt = captureSeamPoint lsr
        , siPublished = updateCaptureStatus lsr rid (\s → s { cstPublication = Published }) }
    case result of
        Left e → do
            -- an unexpected exception: the record says whether the new file
            -- was published by the time it was raised
            published ← maybe NotPublished cstPublication <$> readCaptureRecord lsr
            endFailure env rid FailPublication (Just PhasePublish) published
                ("an unexpected exception ended the write: " <> render e) [] []
        Right (Left f) →
            endFailure env rid (sfKind f) (Just (sfPhase f)) (sfPublication f) (sfDetail f) [] (sfWarnings f)
        Right (Right ok) → do
            let report = CaptureReport
                    { crpDestination = dest, crpBytes = BS.length bytes
                    , crpReplaced = ssReplaced ok, crpEntries = prCounts projection
                    , crpWarnings = ssWarnings ok }
            logInfo logger CatWorld $ "Scenario captured: " <> T.pack dest
                <> " (" <> tshow (BS.length bytes) <> " bytes" <> (if ssReplaced ok then ", replaced" else "") <> ")"
            endSuccess env rid report

-- * The document

-- | The document the projection of @inputs@ is, as bytes, and the projection:
--   projected, encoded, and validated through the shared scenario boundary
--   before anything leaves the capture window. The failure names its kind.
buildDocument ∷ ScenarioCatalog → CaptureInputs
              → Either (FailureKind, Text, [ScenarioDiagnostic]) (BS.ByteString, Projection)
buildDocument = buildWith (id, id)

buildWith ∷ (Scenario → Scenario, BS.ByteString → BS.ByteString) → ScenarioCatalog → CaptureInputs
          → Either (FailureKind, Text, [ScenarioDiagnostic]) (BS.ByteString, Projection)
buildWith (onScenario, onBytes) catalog inputs = do
    projection ← either (\f → Left (FailProjection, pfSummary f, pfDiagnostics f)) Right
                        (projectScenario currentScenarioVersion inputs)
    let scenario = onScenario (prScenario projection)
        bytes = onBytes (encodeScenario scenario)
    case verifyDocument catalog scenario bytes of
        Left why → Left (FailValidation, why, [])
        Right () → Right (bytes, projection { prScenario = scenario })

-- | Does @bytes@ decode through the shared scenario boundary with no
--   diagnostic into exactly @expected@? A document the schema rejects, or
--   one that decodes into something else (an entry it could not carry), is
--   never published.
verifyDocument ∷ ScenarioCatalog → Scenario → BS.ByteString → Either Text ()
verifyDocument catalog expected bytes =
    case fst (decodeScenarioBytesPaths scenarioFormat catalog bytes) of
        ScenarioFailed f → Left ("the document is not a valid scenario: " <> tshow f)
        ScenarioLoaded _ ds@(_ : _) → Left
            (tshow (length ds) <> " finding(s) on decoding the document: "
             <> T.intercalate "; " [ sdPath d <> " " <> tshow (sdReason d) | d ← take 6 ds ])
        ScenarioLoaded back [] → case differing expected back of
            [] → Right ()
            fams → Left ("the document does not decode back to the captured scenario; these differ: "
                         <> T.intercalate ", " fams)

-- | The families in which two scenarios differ.
differing ∷ Scenario → Scenario → [Text]
differing a b = [ name | (name, same) ← checks, not same ]
  where
    checks =
        [ ("version", scSourceVersion a ≡ scSourceVersion b), ("map", scMap a ≡ scMap b)
        , ("camera", scCamera a ≡ scCamera b), ("terrain", scTerrain a ≡ scTerrain b)
        , ("fluids", scFluids a ≡ scFluids b), ("flora", scFlora a ≡ scFlora b)
        , ("structures", scStructures a ≡ scStructures b), ("buildings", scBuildings a ≡ scBuildings b)
        , ("locations", scLocations a ≡ scLocations b), ("units", scUnits a ≡ scUnits b)
        , ("ground_items", scGroundItems a ≡ scGroundItems b) ]

-- * Termination

-- | Is request @rid@ still the live capture? A command that outlived its
--   transaction (the barrier ended it, a newer operation replaced it) does
--   nothing: a stale completion can neither finish nor overwrite a newer
--   request.
whenCurrent ∷ EngineEnv → Int → IO () → IO ()
whenCurrent env rid act = do
    let sl = toSaveLoadCapability env
    rec' ← readCaptureRecord (slLoadStatusRef sl)
    cur ← readSaveStatus (slSaveBarrierRef sl)
    case (rec', cur) of
        (Just r, Just b)
            | cstRequestId r ≡ rid, isNothing (cstOutcome r)
            , ssRequestId b ≡ rid, isNothing (ssOutcome b) → act
        _ → pure ()

stageTo ∷ EngineEnv → Int → CaptureStage → IO ()
stageTo env rid st = updateCaptureStatus (slLoadStatusRef (toSaveLoadCapability env)) rid (\s → s { cstStage = st })

-- | End the request unsuccessfully: record first, then the barrier
--   transaction (which is what admits the next operation). The arena stays
--   paused: nothing here touches the pause.
endFailure ∷ EngineEnv → Int → FailureKind → Maybe StoragePhase → CapturePublication → Text
           → [ScenarioDiagnostic] → [Text] → IO ()
endFailure env rid kind phase published why diags warns = do
    let sl = toSaveLoadCapability env
        lsr = slLoadStatusRef sl
    rec' ← readCaptureRecord lsr
    forM_ rec' $ \r → when (cstRequestId r ≡ rid ∧ isNothing (cstOutcome r)) $ do
        let failure = CaptureFailure
                { cfStage = cstStage r, cfKind = kind, cfPhase = phase
                , cfDestination = crDestination (cstRequest r), cfPublication = published
                , cfDetail = why, cfDiagnostics = diags, cfWarnings = warns }
        logger ← readIORef (ccLoggerRef (toCoreCapability env))
        logWarn logger CatWorld (captureFailureText failure)
        terminateCapture lsr rid (CaptureUnsuccessful failure)
        failSave (slSaveBarrierRef sl) rid (captureFailureText failure)

endSuccess ∷ EngineEnv → Int → CaptureReport → IO ()
endSuccess env rid report = do
    let sl = toSaveLoadCapability env
    terminateCapture (slLoadStatusRef sl) rid (CaptureSucceeded report)
    finishSave (slSaveBarrierRef sl) rid

-- | A synchronous exception at a handoff: end the request unsuccessful
--   instead of leaving it permanently non-terminal.
exceptionFailure ∷ EngineEnv → Int → FailureKind → SomeException → IO ()
exceptionFailure env rid kind e = do
    published ← maybe NotPublished cstPublication <$> readCaptureRecord (slLoadStatusRef (toSaveLoadCapability env))
    endFailure env rid kind Nothing published
        ("an exception ended the request: " <> render e) [] []

-- * Plumbing

operations ∷ EngineEnv → IO StorageOps
operations env = fromMaybe productionOps <$> captureSeamOps (slLoadStatusRef (toSaveLoadCapability env))

seamTransforms ∷ EngineEnv → IO (Scenario → Scenario, BS.ByteString → BS.ByteString)
seamTransforms = captureSeamTransforms ∘ slLoadStatusRef ∘ toSaveLoadCapability

render ∷ SomeException → Text
render = T.pack ∘ displayException

syncTry ∷ IO α → IO (Either SomeException α)
syncTry act = do
    r ← try act
    case r of
        Left e | Just (ae ∷ SomeAsyncException) ← fromException e → throwIO ae
        _ → pure r

guarded ∷ (SomeException → IO ()) → IO () → IO ()
guarded onFailure act = syncTry act ≫= either onFailure pure
