{-# LANGUAGE Strict #-}
-- | What "Scenario.Capture" (#2714) drives the PRODUCTION capture service
--   with: the engine, the production Lua message pump, the real save
--   barrier, the real world thread and the stand-in owners of
--   "Test.Headless.Scenario.Load.Harness" (which acknowledge the barrier by
--   the real owners' protocol and COUNT what they did, so a test can assert
--   that a parked owner mutated nothing).
--
--   The examples act as the barrier owners the headless harness never
--   starts, exactly as @Test.Headless.Save.Barrier@ and @OwnerPark@ do:
--   they hold and release a stand-in's acknowledgement to put a mutation on
--   either side of the snapshot boundary deterministically, never by
--   sleeping.
module Test.Headless.Scenario.Capture.Support
    ( -- * Requests
      requestCapture
    , requestCaptureWith
    , awaitCapture
    , runCapture
    , runCaptureWith
    , captureStatusOf
    , captureOf
    , withPump
    , clearLine
      -- * Seams
    , armCapture
    , armCaptureHold
    , registerHold
    , awaitHold
    , armCaptureWith
    , unitIdOf
    , armCaptureOps
    , disarmCapture
    , holdCaptureAt
    , failCaptureAt
    , faultingOps
    , Fault(..)
      -- * Files
    , freshDir
    , listDir
    , decodeFile
    , decodeBytes
    , captureText
      -- * Sessions
    , loadDoc
    , reloadFrom
    , captureLive
    , liveArena
    , editUnit
    , unitNamed
    , tag
    ) where

import UPrelude
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (readMVar, tryPutMVar)
import Control.Exception (ErrorCall(..), SomeException, bracket, throwIO, toException)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import GHC.Clock (getMonotonicTime, getMonotonicTimeNSec)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (createDirectoryIfMissing, getDirectoryContents)
import System.FilePath ((</>))
import Test.Hspec (expectationFailure)
import Engine.Core.Capability.SaveLoad (SaveLoadCapability(..), toSaveLoadCapability)
import Engine.Core.State (EngineEnv(..))
import Engine.Load.Status
import Engine.Save.Barrier (SaveStatus(..), failSave, readSaveStatus)
import Gameplay.Tags.Types (GameplayTag, mkGameplayTag)
import Scenario.Capture.Service (requestScenarioCapture, readCaptureStatus)
import Scenario.Capture.Types
import Scenario.Load.Catalog (liveScenarioCatalog)
import Scenario.Load.Types (Busy, ScenarioLoadStatus(..), ScenarioLoadOutcome(..), ScenarioSource(..))
import Scenario.Schema (decodeScenarioBytes)
import Scenario.Types
import Test.Headless.Scenario.Load.Harness
import Test.Headless.Scenario.Placement.Fixtures (Fx(..))
import Unit.Types
import World.Save.Storage.Durable (ExclusiveOutcome(..), WriteStep(..))
import World.State.Types

tag ∷ Text → GameplayTag
tag t = fromMaybe (error "empty tag") (mkGameplayTag t)

-- * Requests

loadRefOf ∷ H → LoadStatusRef
loadRefOf = slLoadStatusRef ∘ toSaveLoadCapability ∘ fxEnv ∘ hFx

-- | Run the production Lua pump for the duration of the action.
withPump ∷ H → IO α → IO α
withPump h act = bracket (startPump h) stopPump (const act)

-- | Ask for a capture; the request must be accepted.
requestCapture ∷ H → FilePath → Bool → IO Int
requestCapture h dest overwrite = requestCaptureWith h (CaptureRequest dest overwrite)

requestCaptureWith ∷ H → CaptureRequest → IO Int
requestCaptureWith h req = requestScenarioCapture (fxEnv (hFx h)) req ≫= \case
    Right n → pure n
    Left busy → do
        expectationFailure ("the capture was refused: " ⧺ show (busy ∷ Busy))
        throwIO (ErrorCall "refused")

captureStatusOf ∷ H → IO (Maybe ScenarioCaptureStatus)
captureStatusOf = readCaptureStatus ∘ fxEnv ∘ hFx

-- | The terminal status of request @n@, with the pump running elsewhere.
--   Fails the example (never skips) after @secs@ seconds.
awaitCapture ∷ H → Int → Double → IO ScenarioCaptureStatus
awaitCapture h n secs = do
    start ← getMonotonicTime
    let go = do
            st ← captureStatusOf h
            case st of
                Just s | cstRequestId s ≡ n ∧ isJust (cstOutcome s) → pure s
                _ → do
                    now ← getMonotonicTime
                    when (now - start > secs) $ do
                        expectationFailure ("capture #" ⧺ show n ⧺ " did not finish: " ⧺ show st)
                        throwIO (ErrorCall "timeout")
                    threadDelay 1000
                    go
    go

-- | Ask for a capture and wait for its terminal status (pump included).
runCapture ∷ H → FilePath → Bool → IO ScenarioCaptureStatus
runCapture h dest overwrite = runCaptureWith h (CaptureRequest dest overwrite)

runCaptureWith ∷ H → CaptureRequest → IO ScenarioCaptureStatus
runCaptureWith h req = withPump h $ do
    n ← requestCaptureWith h req
    awaitCapture h n 60

-- | The outcome's report; the example fails if the request did not succeed.
captureOf ∷ ScenarioCaptureStatus → IO CaptureReport
captureOf s = case cstOutcome s of
    Just (CaptureSucceeded r) → pure r
    other → do
        expectationFailure ("the capture did not succeed: " ⧺ show other)
        throwIO (ErrorCall "unsuccessful")

-- | Put the shared line back as a fresh engine has it: no seam armed, no
--   owner held, and no operation left half-done by an example that failed
--   (or stopped a thread on purpose). Run around every example, so one
--   example's failure is reported once instead of blocking every later one.
clearLine ∷ H → IO ()
clearLine h = do
    held ← atomicModifyIORef' heldHolds (\hs → ([], hs))
    forM_ held $ \hold → tryPutMVar (holdRelease hold) ()
    let env = fxEnv (hFx h)
        sl = toSaveLoadCapability env
        lsr = slLoadStatusRef sl
        barrier = slSaveBarrierRef sl
    disarmCapture h
    disarm h
    forM_ [minBound .. maxBound] $ \o → holdOwner (hOwners h) o False
    holdSimulation (hOwners h) False
    rec' ← readCaptureRecord lsr
    forM_ rec' $ \r → when (isNothing (cstOutcome r)) $
        terminateCapture lsr (cstRequestId r) $ CaptureUnsuccessful CaptureFailure
            { cfStage = cstStage r, cfKind = FailCoordination, cfPhase = Nothing
            , cfDestination = crDestination (cstRequest r), cfPublication = NotPublished
            , cfDetail = "cleared between examples", cfDiagnostics = [], cfWarnings = [] }
    cur ← readSaveStatus barrier
    forM_ cur $ \b → when (isNothing (ssOutcome b)) $ failSave barrier (ssRequestId b) "cleared between examples"
    load ← readLoadStatus lsr
    forM_ load $ \l → when (isNothing (lsOutcome l)) $ finishLoad lsr (lsRequestId l)

-- * Seams

armCapture ∷ H → (CapturePoint → IO ()) → IO ()
armCapture h f = armCaptureSeams (loadRefOf h) noCaptureSeams { cseamAt = f }

-- | Hold the capture at @point@: the first time it arrives it signals
--   'holdReached' and waits for 'holdRelease'.
armCaptureHold ∷ H → CapturePoint → Hold → IO ()
armCaptureHold h point hold = do
    registerHold hold
    armCapture h $ \p → when (p ≡ point) $ do
        _ ← tryPutMVar (holdReached hold) ()
        readMVar (holdRelease hold)

-- | Every hold an example created, so 'clearLine' can let them all go: an
--   example that fails while a REAL thread (the world thread) is parked at a
--   seam must not leave it parked for every example after it.
heldHolds ∷ IORef [Hold]
heldHolds = unsafePerformIO (newIORef [])
{-# NOINLINE heldHolds #-}

-- | Wait (bounded) until the held thread has arrived at its seam. A request
--   that ended BEFORE the seam (a failure the example did not expect) must
--   fail the example, never hang the suite.
awaitHold ∷ Hold → IO ()
awaitHold hold = timeout 30000000 (readMVar (holdReached hold)) ≫= \case
    Just () → pure ()
    Nothing → do
        expectationFailure "the capture never reached the seam the example holds it at"
        throwIO (ErrorCall "seam not reached")

registerHold ∷ Hold → IO ()
registerHold hold = atomicModifyIORef' heldHolds (\hs → (hold : hs, ()))

-- | Arm a complete set of seams.
armCaptureWith ∷ H → CaptureSeams → IO ()
armCaptureWith h = armCaptureSeams (loadRefOf h)

armCaptureOps ∷ H → StorageOps → IO ()
armCaptureOps h ops = armCaptureSeams (loadRefOf h) noCaptureSeams { cseamOps = Just ops }

disarmCapture ∷ H → IO ()
disarmCapture h = disarmCaptureSeams (loadRefOf h)

holdCaptureAt ∷ CapturePoint → Hold → CapturePoint → IO ()
holdCaptureAt point hold p = when (p ≡ point) $ do
    _ ← tryPutMVar (holdReached hold) ()
    readMVar (holdRelease hold)

failCaptureAt ∷ CapturePoint → Text → CapturePoint → IO ()
failCaptureAt point why p = when (p ≡ point) (throwIO (ErrorCall (T.unpack why)))

-- | The filesystem operation to fail. 'FailCreate' fails before the file
--   exists; 'FailWrite' and 'FailFlush' fail after the REAL temporary file
--   was created (so the example can assert it was cleaned up, and only it).
data Fault
    = FailCreate
    | FailWrite
    | FailFlush
    | FailRead
    | FailReadWrongBytes
    | FailLink
    | FailReplace
    | FailSyncDir
    | FailRemove
    | NoFault
    deriving (Show, Eq)

-- | The production operations with exactly one of them failing.
faultingOps ∷ StorageOps → Fault → StorageOps
faultingOps real fault = case fault of
    NoFault → real
    FailCreate → real { opCreate = \_ _ _ → pure (Left (StepOpen, Nothing, boom "create")) }
    FailWrite → real { opCreate = partial StepWrite "write" }
    FailFlush → real { opCreate = partial StepFlush "flush" }
    FailRead → real { opRead = \_ → throwIO (userError "read") }
    FailReadWrongBytes → real { opRead = \p → BS.map (+ 1) <$> opRead real p }
    FailLink → real { opLink = \_ _ → pure (ExclusiveFailed (userError "link")) }
    FailReplace → real { opReplace = \_ _ → throwIO (userError "replace") }
    FailSyncDir → real { opSyncDir = \_ → throwIO (userError "syncDir") }
    FailRemove → real { opRemove = \_ → throwIO (userError "remove") }
  where
    boom ∷ String → SomeException
    boom = toException ∘ userError
    -- create the real file with PART of the bytes, then fail
    partial step what dir template bytes = do
        r ← opCreate real dir template (BS.take (BS.length bytes `div` 2) bytes)
        pure $ case r of
            Right path → Left (step, Just path, boom what)
            Left e → Left e

-- * Files

-- | A new, empty directory for one example.
freshDir ∷ String → IO FilePath
freshDir name = do
    tmp ← scratchFile ("capture-" ⧺ name)
    ns ← getMonotonicTimeNSec
    let dir = tmp ⧺ "-" ⧺ show ns
    createDirectoryIfMissing True dir
    pure dir

-- | The names in a directory (sorted, without @.@ and @..@).
listDir ∷ FilePath → IO [FilePath]
listDir dir = sortOn' <$> (filter (`notElem` [".", ".."]) <$> getDirectoryContents dir)
  where sortOn' = Map.keys ∘ Map.fromList ∘ map (\x → (x, ()))

-- | Decode a captured file through the production boundary, against the
--   LIVE registries: it must decode with no diagnostic.
decodeFile ∷ H → FilePath → IO Scenario
decodeFile h path = BS.readFile path ≫= decodeBytes h

decodeBytes ∷ H → BS.ByteString → IO Scenario
decodeBytes h bytes = do
    catalog ← liveScenarioCatalog (fxEnv (hFx h))
    case decodeScenarioBytes catalog bytes of
        ScenarioLoaded s [] → pure s
        ScenarioLoaded _ ds → do
            expectationFailure ("the captured document decoded with diagnostics: " ⧺ show ds)
            throwIO (ErrorCall "diagnostics")
        ScenarioFailed f → do
            expectationFailure ("the captured document failed to decode: " ⧺ show f)
            throwIO (ErrorCall "failed")

captureText ∷ BS.ByteString → Text
captureText = TE.decodeUtf8Lenient

-- * Sessions

-- | Replace the session with the scenario @text@ (a v3 document) and wait
--   for it to be ready.
loadDoc ∷ H → String → [String] → IO ()
loadDoc h name ls = do
    path ← writeScenario name (doc ls)
    reloadFrom h path

-- | Replace the session with the scenario file at @path@.
reloadFrom ∷ H → FilePath → IO ()
reloadFrom h path = do
    s ← runLoad h (SourceFile path)
    slsOutcome s `shouldBeReady` ()
    unless (null (slsWarnings s)) $ do
        expectationFailure ("the scenario loaded with warnings: " ⧺ show (slsWarnings s))
        throwIO (ErrorCall "warnings")
  where
    shouldBeReady o () = case o of
        Just ScenarioReady → pure ()
        other → do
            expectationFailure ("the scenario did not load: " ⧺ show other)
            throwIO (ErrorCall "not ready")

-- | Capture the live arena to a new file in a fresh directory and return
--   the file's bytes and the terminal status.
captureLive ∷ H → String → IO (FilePath, BS.ByteString, ScenarioCaptureStatus)
captureLive h name = do
    dir ← freshDir name
    let dest = dir </> "captured.yaml"
    s ← runCapture h dest False
    _ ← captureOf s
    bytes ← BS.readFile dest
    pure (dest, bytes, s)

liveArena ∷ H → IO WorldState
liveArena h = activeArena h ≫= maybe (fail "the arena page is not published") pure

-- | Rewrite one live unit, by its scenario reference, as a player's action
--   would.
editUnit ∷ H → Text → (UnitInstance → UnitInstance) → IO ()
editUnit h ref f = do
    let env = fxEnv (hFx h)
    um ← readIORef (unitManagerRef env)
    case [ u | (u, r) ← Map.toList (umScenarioRefs um), r ≡ ref ] of
        [uid] → void $ atomicModifyIORef' (unitManagerRef env) $ \m →
            (m { umInstances = HM.adjust f uid (umInstances m) }, ())
        other → fail ("expected one unit '" ⧺ T.unpack ref ⧺ "', found " ⧺ show (length other))

-- | The runtime id the unit with this scenario reference was allocated.
unitIdOf ∷ H → Text → IO UnitId
unitIdOf h ref = do
    um ← readIORef (unitManagerRef (fxEnv (hFx h)))
    case [ u | (u, r) ← Map.toList (umScenarioRefs um), r ≡ ref ] of
        [u] → pure u
        other → fail ("expected one unit '" ⧺ T.unpack ref ⧺ "', found " ⧺ show (length other))

unitNamed ∷ H → Text → IO UnitInstance
unitNamed h ref = do
    um ← readIORef (unitManagerRef (fxEnv (hFx h)))
    case [ i | (u, r) ← Map.toList (umScenarioRefs um), r ≡ ref, Just i ← [HM.lookup u (umInstances um)] ] of
        [i] → pure i
        other → fail ("expected one unit '" ⧺ T.unpack ref ⧺ "', found " ⧺ show (length other))
