{-# LANGUAGE Strict #-}
-- | The public Lua entry points of the scenario services (#2715, SCN-15 of
--   epic #2698): @engine.loadScenario@, @engine.saveScenario@ and
--   @engine.getScenarioStatus@, plus the three startup verbs a direct
--   @--scenario@ boot drives. They replace the internal @debug.*Scenario@
--   verbs #2714 shipped as a stand-in.
--
--   The verbs are thin on purpose. They decode and validate their
--   arguments, resolve the path through the ONE policy ("Scenario.Path"),
--   and hand the result to the services #2713 ("Scenario.Load.Service")
--   and #2714 ("Scenario.Capture.Service"), reusing the operation
--   identities and status records those services own. There is no second
--   loader, capture path, coordinator or session-state owner here.
--
--   == The contract (documented once, in @docs/engine_contracts.md@
--      §Scenario entry points)
--
--   * A WRONG-TYPED call is a programming error and raises a Lua error:
--     a non-string path, a path that is not valid UTF-8, a non-table or
--     non-boolean option, an unknown option, a wrong argument count.
--     Nothing is submitted.
--   * A well-typed request the engine cannot take is a REJECTION, answered
--     at once as @nil, rejection@ with @rejection.reason@ one of
--     @"busy"@, @"invalid_path"@, @"not_found"@ (the startup verb adds
--     @"already_started"@), plus @message@ and, when
--     known, the resolved @path@. A rejection allocates no identity,
--     queues nothing, replaces no status and changes no pause state.
--   * An ACCEPTED request answers its integer id. Its outcome is read from
--     @engine.getScenarioStatus()@, which names the operation (@kind@,
--     @id@), the resolved path, the stage, whether it is @done@, and — once
--     done — its @outcome@.
module Engine.Scripting.Lua.API.Scenario
    ( ScenarioApi(..)
    , productionScenarioApi
    , loadScenarioFn
    , saveScenarioFn
    , scenarioStatusFn
    , scenarioStartupFn
    , startScenarioStartupFn
    , failScenarioStartupFn
      -- * Exposed for the tests of the encoding
    , loadStageName
    , saveStageName
    ) where

import UPrelude
import Control.Exception (ErrorCall(..), IOException, throwIO, try)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import System.Directory (getCurrentDirectory)
import Engine.Core.Capability.Core (CoreCapability(..), toCoreCapability)
import Engine.Core.Capability.SaveLoad (SaveLoadCapability(..), toSaveLoadCapability)
import Engine.Core.State (EngineEnv)
import Engine.Core.Types (BootMode(..), EngineConfig(..))
import Engine.Load.Status
    ( claimStartupSubmission, readLastScenarioOperation, readScenarioStatus
    , readStartupResult, recordStartupFailure, releaseStartupSubmission )
import Scenario.Capture.Service (readCaptureStatus, requestScenarioCapture)
import Scenario.Capture.Types
import Scenario.Load.Service (requestScenarioLoad, requestStartupScenarioLoad)
import Scenario.Load.Types
import Scenario.Path
import Scenario.Types (DiagnosticEffect(..), DiagnosticReason(..), ScenarioDiagnostic(..), ScenarioFailure(..))

-- | Where in-game names resolve from. Production reads the process's
--   working directory, which IS the selected resource root once
--   "App.ResourceRoot" has run; the suite supplies a directory of its own
--   so the launch and resource roots can differ.
newtype ScenarioApi = ScenarioApi
    { saRoots ∷ IO ScenarioRoots
    }

productionScenarioApi ∷ ScenarioApi
productionScenarioApi = ScenarioApi
    { saRoots = do
        here ← getCurrentDirectory
        -- an in-game name never consults the launch directory (D-49)
        pure (ScenarioRoots { srLaunchDir = here, srResourceRoot = here }) }

-- * Argument decoding

-- | Raise a Lua error; nothing after it runs. It is a Haskell exception
--   ('ErrorCall'), which "Engine.Scripting.Lua.API.Internal"'s guard turns
--   into a regular, catchable Lua error naming the verb — the guard is the
--   one place a Lua error is raised, so no verb unwinds through its own
--   Haskell frames.
argFail ∷ Text → Lua.LuaE Lua.Exception α
argFail = Lua.liftIO ∘ throwIO ∘ ErrorCall ∘ T.unpack

-- | How many arguments the call received.
argCount ∷ Lua.LuaE Lua.Exception Int
argCount = (\(Lua.StackIndex i) → fromIntegral i) <$> Lua.gettop

-- | The number of arguments the call received must be within @[lo, hi]@.
arity ∷ Text → Int → Int → Lua.LuaE Lua.Exception ()
arity fname lo hi = do
    n ← argCount
    when (n < lo ∨ n > hi) $ argFail $
        fname <> " takes " <> expected <> " argument(s), got " <> tshow n
  where
    expected | lo ≡ hi = tshow lo
             | otherwise = tshow lo <> " to " <> tshow hi

-- | A STRING argument: the actual Lua type is checked first (a number is
--   never coerced into a path) and the bytes must be valid UTF-8 (a
--   lenient decode would silently name a different file).
stringArg ∷ Text → Text → Lua.StackIndex → Lua.LuaE Lua.Exception String
stringArg fname what idx = do
    ty ← Lua.ltype idx
    if ty ≢ Lua.TypeString
        then argFail (fname <> ": " <> what <> " must be a string")
        else do
            bytes ← fromMaybe BS.empty <$> Lua.tostring idx
            case TE.decodeUtf8' bytes of
                Right t → pure (T.unpack t)
                Left _ → argFail (fname <> ": " <> what <> " is not valid UTF-8")

-- | The options table of @engine.saveScenario@: only @overwrite@, a
--   boolean. Walked RAW, so no metamethod runs; an unknown key or a
--   wrong-typed value is refused before anything is submitted.
overwriteOption ∷ Text → Lua.StackIndex → Lua.LuaE Lua.Exception Bool
overwriteOption fname idx = do
    ty ← Lua.ltype idx
    case ty of
        Lua.TypeNil → pure False
        Lua.TypeTable → Lua.pushnil *> walk False
        _ → argFail (fname <> ": options must be a table")
  where
    walk acc = do
        more ← Lua.next idx
        if not more then pure acc else do
            keyType ← Lua.ltype (-2)
            when (keyType ≢ Lua.TypeString) $
                argFail (fname <> ": option names must be strings")
            key ← TE.decodeUtf8Lenient . fromMaybe BS.empty <$> Lua.tostring (-2)
            if key ≢ "overwrite"
                then argFail (fname <> ": unknown option " <> tshow key
                              <> " (the only option is overwrite)")
                else do
                    valueType ← Lua.ltype (-1)
                    when (valueType ≢ Lua.TypeBoolean) $
                        argFail (fname <> ": option overwrite must be a boolean")
                    flag ← Lua.toboolean (-1)
                    Lua.pop 1
                    walk flag

-- * Result encoding

setText ∷ Text → Text → Lua.LuaE Lua.Exception ()
setText k v = Lua.pushstring (TE.encodeUtf8 v) *> Lua.setfield (-2) (Lua.Name (TE.encodeUtf8 k))

setBool ∷ Text → Bool → Lua.LuaE Lua.Exception ()
setBool k b = Lua.pushboolean b *> Lua.setfield (-2) (Lua.Name (TE.encodeUtf8 k))

setInt ∷ Text → Int → Lua.LuaE Lua.Exception ()
setInt k n = Lua.pushinteger (fromIntegral n) *> Lua.setfield (-2) (Lua.Name (TE.encodeUtf8 k))

-- | Set field @k@ of the table on top to a table built by @build@.
setTable ∷ Text → Lua.LuaE Lua.Exception () → Lua.LuaE Lua.Exception ()
setTable k build = do
    Lua.newtable
    build
    Lua.setfield (-2) (Lua.Name (TE.encodeUtf8 k))

-- | Set field @k@ to a sequence of tables, one per element.
setList ∷ Text → [Lua.LuaE Lua.Exception ()] → Lua.LuaE Lua.Exception ()
setList k items = setTable k $
    forM_ (zip [1 ∷ Int ..] items) $ \(i, build) → do
        Lua.newtable
        build
        Lua.rawseti (-2) (fromIntegral i)

-- | A rejection: @nil, { reason, message [, path] }@.
reject ∷ Text → Text → Maybe FilePath → Lua.LuaE Lua.Exception Lua.NumResults
reject reason message path = do
    Lua.pushnil
    Lua.newtable
    setText "reason" reason
    setText "message" message
    forM_ path $ \p → setText "path" (T.pack p)
    pure 2

-- | An id, or the busy rejection.
answer ∷ Either Busy Int → Lua.LuaE Lua.Exception Lua.NumResults
answer = \case
    Right n → Lua.pushinteger (fromIntegral n) *> pure 1
    Left busy → reject "busy" (busyText busy) Nothing

-- | Resolve a typed path, turning a stray file-system exception into the
--   same kind of refusal the policy itself gives.
resolveOrReject ∷ ScenarioApi → ScenarioUse → String
                → Lua.LuaE Lua.Exception (Either (Lua.LuaE Lua.Exception Lua.NumResults) FilePath)
resolveOrReject api use raw = do
    roots ← Lua.liftIO (saRoots api)
    attempt ← Lua.liftIO (try (resolveScenarioPath EntryGame use roots raw)
                           ∷ IO (Either IOException (Either PathProblem FilePath)))
    pure $ case attempt of
        Left e → Left (reject "invalid_path"
                          ("the scenario path " <> tshow raw <> " could not be resolved: " <> tshow e)
                          Nothing)
        Right (Left problem) →
            Left (reject (pathProblemReason problem) (pathProblemText problem)
                         (pathProblemResolved problem))
        Right (Right path) → Right path

-- * The verbs

-- | @engine.loadScenario(path) → id | nil, rejection@
loadScenarioFn ∷ ScenarioApi → EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
loadScenarioFn api env = do
    let fname = "engine.loadScenario"
    arity fname 1 1
    raw ← stringArg fname "path" 1
    resolved ← resolveOrReject api UseLoad raw
    case resolved of
        Left refusal → refusal
        Right path → Lua.liftIO (requestScenarioLoad env (SourceFile path)) ⌦ answer

-- | @engine.saveScenario(path [, { overwrite = boolean }]) → id | nil, rejection@
saveScenarioFn ∷ ScenarioApi → EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
saveScenarioFn api env = do
    let fname = "engine.saveScenario"
    arity fname 1 2
    raw ← stringArg fname "path" 1
    n ← argCount
    overwrite ← if n ≥ 2 then overwriteOption fname 2 else pure False
    resolved ← resolveOrReject api UseCapture raw
    case resolved of
        Left refusal → refusal
        Right path →
            Lua.liftIO (requestScenarioCapture env (CaptureRequest path overwrite)) ⌦ answer

-- | @engine.getScenarioStatus() → table | nil@
--
--   The record of the scenario operation admitted LAST, while it is still
--   the current one of its kind (an ordinary load, save or autosave that
--   came after it supersedes it, so a stale record is never reported
--   under a newer request's name). A terminal result stays readable until
--   then. A poller therefore matches @kind@ and @id@ against the request
--   it made before believing the result is its own.
scenarioStatusFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
scenarioStatusFn env = do
    arity "engine.getScenarioStatus" 0 0
    let lsr = slLoadStatusRef (toSaveLoadCapability env)
    lastOp ← Lua.liftIO (readLastScenarioOperation lsr)
    case lastOp of
        Just OpLoad → Lua.liftIO (readScenarioStatus lsr) ⌦ maybe Lua.pushnil pushLoadStatus
        Just OpSave → Lua.liftIO (readCaptureStatus env) ⌦ maybe Lua.pushnil pushSaveStatus
        Nothing → Lua.pushnil
    pure 1

-- * The load record

loadStageName ∷ ScenarioLoadStage → Text
loadStageName = \case
    StageAdmitted → "admitted"
    StagePreflight → "preflight"
    StageStaging → "staging"
    StagePublishing → "publishing"
    StageReconciling → "reconciling"
    StageConstructing → "constructing"
    StageRecovering → "recovering"
    StageReady → "ready"
    StageFailed → "failed"

recoveryName ∷ Recovery → Text
recoveryName = \case
    RecoveryNone → "none"
    RecoveryPreserved → "preserved"
    RecoveryFlatArena → "flat_arena"
    RecoveryIncomplete → "incomplete"
    RecoveryFailed _ → "failed"

pushLoadStatus ∷ ScenarioLoadStatus → Lua.LuaE Lua.Exception ()
pushLoadStatus s = do
    Lua.newtable
    setText "kind" "load"
    setInt "id" (slsRequestId s)
    setText "source" (slsSource s)
    setText "stage" (loadStageName (slsStage s))
    setBool "done" (isJust (slsOutcome s))
    setBool "published" (slsPublished s)
    setText "recovery" (recoveryName (slsRecovery s))
    case slsRecovery s of
        RecoveryFailed why → setText "recoveryError" why
        _ → pure ()
    setList "warnings" (map diagnosticFields (slsWarnings s))
    forM_ (slsOutcome s) $ \case
        ScenarioReady → setText "outcome" "ready"
        ScenarioUnsuccessful ctx → do
            setText "outcome" "failed"
            setTable "failure" (failureFields ctx)

failureFields ∷ FailureContext → Lua.LuaE Lua.Exception ()
failureFields ctx = do
    setText "stage" (loadStageName (fcStage ctx))
    setText "message" (failureText ctx)
    setList "details" [ setText "message" d | d ← fcDetails ctx ]
    case fcCause ctx of
        CauseSource f → setText "kind" "source" *> setText "source" (sourceFailureCode f)
        CauseNotTestingSession _ → setText "kind" "not_testing_session"
        CauseSetup _ → setText "kind" "setup"
        CauseConstruction _ → setText "kind" "construction"
        CauseReconciliation fs → do
            setText "kind" "reconciliation"
            setList "reconciliation" [ setText "module" m *> setText "error" e | (m, e) ← fs ]
        CauseException _ → setText "kind" "exception"

sourceFailureCode ∷ ScenarioFailure → Text
sourceFailureCode = \case
    ScenarioUnreadable _ → "unreadable"
    ScenarioSyntaxError _ → "syntax_error"
    ScenarioNotAMapping → "not_a_mapping"
    ScenarioVersionMissing → "version_missing"
    ScenarioVersionMalformed _ → "version_malformed"
    ScenarioVersionUnsupported _ → "version_unsupported"
    ScenarioMigrationFailed _ _ → "migration_failed"

-- | One warning: its document path, a stable code (with the payload the
--   reason carries), and what it did to the scenario.
diagnosticFields ∷ ScenarioDiagnostic → Lua.LuaE Lua.Exception ()
diagnosticFields d = do
    setText "path" (sdPath d)
    let (code, detail) = reasonCode (sdReason d)
    setText "code" code
    forM_ detail $ setText "detail"
    setText "reason" (tshow (sdReason d))
    setText "effect" $ case sdEffect d of
        EntryRejected → "entry_rejected"
        FieldRejected → "field_rejected"
        TilesClipped → "tiles_clipped"
        CascadeRejected → "cascade_rejected"

reasonCode ∷ DiagnosticReason → (Text, Maybe Text)
reasonCode = \case
    UnknownField → ("unknown_field", Nothing)
    InvalidValue t → ("invalid_value", Just t)
    MissingRequired → ("missing_required", Nothing)
    UnknownDefinition t → ("unknown_definition", Just t)
    DuplicateId t → ("duplicate_id", Just t)
    DerivedValue → ("derived_value", Nothing)
    EmptyTag → ("empty_tag", Nothing)
    MissingReference t → ("missing_reference", Just t)
    WrongReferenceKind t → ("wrong_reference_kind", Just t)
    RejectedReference t → ("rejected_reference", Just t)
    OwnerRejected t → ("owner_rejected", Just t)
    AmbiguousBinding t → ("ambiguous_binding", Just t)
    DefinitionMismatch t → ("definition_mismatch", Just t)
    OutsideBounds → ("outside_bounds", Nothing)
    FootprintOutsideBounds → ("footprint_outside_bounds", Nothing)
    ClippedTiles n → ("clipped_tiles", Just (tshow n))
    CyclicContents t → ("cyclic_contents", Just t)
    NoEncounter t → ("no_encounter", Just t)

-- * The capture record

saveStageName ∷ CaptureStage → Text
saveStageName = \case
    CaptureAdmitted → "admitted"
    CapturePreflight → "preflight"
    CaptureCoordinating → "coordinating"
    CaptureSnapshotting → "snapshotting"
    CaptureEncoding → "encoding"
    CaptureStoring → "storing"
    CaptureDone → "done"
    CaptureFailed → "failed"

failureKindName ∷ FailureKind → Text
failureKindName = \case
    FailCoordination → "coordination"
    FailNotTestingSession → "not_testing_session"
    FailProjection → "projection"
    FailValidation → "validation"
    FailEncoding → "encoding"
    FailDestination → "destination"
    FailOverwriteRefused → "overwrite_refused"
    FailTemporaryWrite → "temporary_write"
    FailVerification → "verification"
    FailPublication → "publication"
    FailDurability → "durability"

pushSaveStatus ∷ ScenarioCaptureStatus → Lua.LuaE Lua.Exception ()
pushSaveStatus s = do
    let req = cstRequest s
    Lua.newtable
    setText "kind" "save"
    setInt "id" (cstRequestId s)
    setText "destination" (T.pack (crDestination req))
    setBool "overwrite" (crOverwrite req)
    setText "stage" (saveStageName (cstStage s))
    setBool "done" (isJust (cstOutcome s))
    setBool "published" (cstPublication s ≡ Published)
    case cstOutcome s of
        Nothing → setList "warnings" []
        Just (CaptureSucceeded r) → do
            setText "outcome" "saved"
            setInt "bytes" (crpBytes r)
            setBool "replaced" (crpReplaced r)
            setTable "entries" $ forM_ (crpEntries r) $ \(family, n) → setInt family n
            setList "warnings" [ setText "message" w | w ← crpWarnings r ]
        Just (CaptureUnsuccessful f) → do
            setText "outcome" "failed"
            setList "warnings" [ setText "message" w | w ← cfWarnings f ]
            setTable "failure" $ do
                setText "stage" (saveStageName (cfStage f))
                setText "kind" (failureKindName (cfKind f))
                forM_ (cfPhase f) $ \p → setText "phase" (phaseText p)
                setText "message" (cfDetail f)
                setText "destination" (T.pack (cfDestination f))
                setBool "published" (cfPublication f ≡ Published)
                setList "diagnostics" (map diagnosticFields (cfDiagnostics f))

-- * The startup verbs

-- | @engine.getScenarioStartup() → nil | { path, loadsDefinitions [, result] }@
--
--   The startup driver's question: did this boot ask for a scenario, and
--   must the driver bring the arena-profile definitions in itself; and,
--   once the startup load has ended, how — @result@ is that load's terminal
--   status (the same table @engine.getScenarioStatus@ gives a load), kept
--   for the boot however many operations were admitted after it (a capture
--   a script made the instant the scenario was ready would otherwise hide
--   it from the driver). The
--   path is the RESOLVED file @--scenario@ named; it comes from the engine
--   configuration and is only ever handed on as data. Headless never runs
--   the UI boot that loads definitions for the other modes, so headless
--   alone reports @loadsDefinitions = true@.
scenarioStartupFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
scenarioStartupFn env = do
    arity "engine.getScenarioStartup" 0 0
    let cfg = ccEngineConfig (toCoreCapability env)
    case ecScenarioStartup cfg of
        Nothing → Lua.pushnil
        Just path → do
            result ← Lua.liftIO (readStartupResult (slLoadStatusRef (toSaveLoadCapability env)))
            Lua.newtable
            setText "path" (T.pack path)
            setBool "loadsDefinitions" (ecBootMode cfg ≡ ModeHeadless)
            -- the startup load's OWN terminal record, kept however many
            -- operations were admitted since
            forM_ result $ \r → pushLoadStatus r *> Lua.setfield (-2) "result"
    pure 1

-- | @engine.startScenarioStartup() → id | nil, rejection@
--
--   Submit the boot's one scenario load: the file @--scenario@ resolved,
--   through the same service @engine.loadScenario@ uses, marked as the
--   startup load so an unsuccessful end is latched for the boot to exit
--   on. The engine, not the caller, keeps it to ONE: a second call after an
--   accepted submission is rejected @already_started@, and a submission
--   the service refused as @busy@ (another operation held the line) gives
--   the claim back so the caller may ask again. Refused (nothing
--   submitted) when the boot asked for no scenario. The startup driver
--   calls it after the definitions the scenario names are available.
startScenarioStartupFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
startScenarioStartupFn env = do
    arity "engine.startScenarioStartup" 0 0
    let lsr = slLoadStatusRef (toSaveLoadCapability env)
    case ecScenarioStartup (ccEngineConfig (toCoreCapability env)) of
        Nothing → reject "invalid_path" "this boot did not ask for a scenario (--scenario)" Nothing
        Just path → do
            claimed ← Lua.liftIO (claimStartupSubmission lsr)
            if not claimed
                then reject "already_started"
                        "the startup scenario was already submitted; a boot submits exactly one" Nothing
                else do
                    submitted ← Lua.liftIO (requestStartupScenarioLoad env (SourceFile path))
                    either (const (Lua.liftIO (releaseStartupSubmission lsr))) (const (pure ())) submitted
                    answer submitted

-- | @engine.failScenarioStartup(message) → true | nil, rejection@
--
--   Latch a startup failure that is not a scenario load's — the
--   arena-profile definitions would not load — so a headless or offscreen
--   boot exits nonzero through its fatal tail instead of waiting forever
--   for a scenario that cannot load. Refused when the boot asked for no
--   scenario.
failScenarioStartupFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
failScenarioStartupFn env = do
    let fname = "engine.failScenarioStartup"
    arity fname 1 1
    message ← stringArg fname "message" 1
    case ecScenarioStartup (ccEngineConfig (toCoreCapability env)) of
        Nothing → reject "invalid_path" "this boot did not ask for a scenario (--scenario)" Nothing
        Just path → do
            Lua.liftIO $ recordStartupFailure (slLoadStatusRef (toSaveLoadCapability env)) $
                "scenario startup failed for " <> T.pack path <> ": " <> T.pack message
            Lua.pushboolean True
            pure 1
