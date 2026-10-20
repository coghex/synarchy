-- | INTERNAL console verbs for the scenario load and capture services
--   (#2714, SCN-14 of epic #2698), registered on the @debug@ table beside
--   @debug.armLoadStageGate@ — deliberately NOT on a player-facing
--   namespace, following @engine.debugThrow@'s precedent for a test-only
--   binding that ships in engine code.
--
--   They exist because the services have no public entry point yet: the
--   Lua functions, the CLI boot and the Escape-menu actions belong to
--   SCN-15, which supersedes these verbs. Until then they are what lets a
--   FRESH engine process load and capture a scenario through the real
--   services over its debug console (@tools/scenario_capture_probe.py@ is
--   the caller). They take a resolved path and nothing else: no shorthand,
--   no folder policy, no confirmation. Each answers at once; completion is
--   read from the status verbs.
--
--   Each verb reaches the services through 'EngineEnv' only for the
--   request itself; the status is read through the @save-load-coordination@
--   capability's handle, so no @EngineEnv@ field was added.
module Engine.Scripting.Lua.API.ScenarioDebug
  ( loadScenarioFn
  , captureScenarioFn
  , scenarioLoadStatusFn
  , scenarioCaptureStatusFn
  ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Engine.Core.Capability.SaveLoad (SaveLoadCapability(..), toSaveLoadCapability)
import Engine.Core.State (EngineEnv)
import Engine.Load.Status (readScenarioStatus)
import Scenario.Capture.Service (readCaptureStatus, requestScenarioCapture)
import Scenario.Capture.Types
import Scenario.Load.Service (requestScenarioLoad)
import Scenario.Load.Types
import Scenario.Types (ScenarioDiagnostic(..))

-- | @debug.loadScenario(path) → id | false, reason@
--
--   Ask for the scenario file at @path@ to be loaded as the paused testing
--   session. @false@ with the busy reason when another operation holds the
--   line; otherwise the request's id, whose result is read from
--   @debug.getScenarioLoadStatus()@.
loadScenarioFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
loadScenarioFn env = do
    mPath ← Lua.tostring 1
    case mPath of
        Nothing → refuse "a path is required"
        Just p → do
            r ← Lua.liftIO $ requestScenarioLoad env (SourceFile (T.unpack (TE.decodeUtf8Lenient p)))
            answer r

-- | @debug.captureScenario(path [, overwrite]) → id | false, reason@
--
--   Ask for the live testing arena to be captured to the file at @path@.
--   @overwrite@ is the explicit intent to replace an existing file (absent
--   or false refuses one). Answers as 'loadScenarioFn' does; the result is
--   read from @debug.getScenarioCaptureStatus()@.
captureScenarioFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
captureScenarioFn env = do
    mPath ← Lua.tostring 1
    overwrite ← Lua.toboolean 2
    case mPath of
        Nothing → refuse "a path is required"
        Just p → do
            r ← Lua.liftIO $ requestScenarioCapture env
                (CaptureRequest (T.unpack (TE.decodeUtf8Lenient p)) overwrite)
            answer r

refuse ∷ Text → Lua.LuaE Lua.Exception Lua.NumResults
refuse why = do
    Lua.pushboolean False
    Lua.pushstring (TE.encodeUtf8 why)
    pure 2

answer ∷ Either Busy Int → Lua.LuaE Lua.Exception Lua.NumResults
answer = \case
    Right n → Lua.pushinteger (fromIntegral n) >> pure 1
    Left busy → refuse (busyText busy)

field ∷ Text → Text → Lua.LuaE Lua.Exception ()
field k v = Lua.pushstring (TE.encodeUtf8 v) >> Lua.setfield (-2) (Lua.Name (TE.encodeUtf8 k))

boolField ∷ Text → Bool → Lua.LuaE Lua.Exception ()
boolField k b = Lua.pushboolean b >> Lua.setfield (-2) (Lua.Name (TE.encodeUtf8 k))

intField ∷ Text → Int → Lua.LuaE Lua.Exception ()
intField k n = Lua.pushinteger (fromIntegral n) >> Lua.setfield (-2) (Lua.Name (TE.encodeUtf8 k))

-- | @debug.getScenarioLoadStatus() → table | nil@
--
--   The scenario load request's record while it is the current load:
--   @id@, @source@, @stage@, @published@, @recovery@, @warnings@ (a count),
--   and once terminal @outcome@ (@"ready"@ or @"unsuccessful"@) with, for an
--   unsuccessful one, @failure@ (the one-line cause).
scenarioLoadStatusFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
scenarioLoadStatusFn env = do
    st ← Lua.liftIO $ readScenarioStatus (slLoadStatusRef (toSaveLoadCapability env))
    case st of
        Nothing → Lua.pushnil
        Just s → do
            Lua.newtable
            intField "id" (slsRequestId s)
            field "source" (slsSource s)
            field "stage" (tshow (slsStage s))
            boolField "published" (slsPublished s)
            field "recovery" (tshow (slsRecovery s))
            intField "warnings" (length (slsWarnings s))
            forM_ (slsOutcome s) $ \o → case o of
                ScenarioReady → field "outcome" "ready"
                ScenarioUnsuccessful ctx → do
                    field "outcome" "unsuccessful"
                    field "failure" (failureText ctx)
    pure 1

-- | @debug.getScenarioCaptureStatus() → table | nil@
--
--   The capture request's record while it is the current save-barrier
--   transaction: @id@, @destination@, @overwrite@, @stage@, @published@ and
--   once terminal @outcome@ (@"succeeded"@ or @"unsuccessful"@) with, for a
--   success, @bytes@, @replaced@, one @entries.<family>@ count per family and
--   a @warnings@ count, and for a failure @kind@, @phase@ (when a storage
--   step), @detail@ and the one-line @failure@.
scenarioCaptureStatusFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
scenarioCaptureStatusFn env = do
    st ← Lua.liftIO $ readCaptureStatus env
    case st of
        Nothing → Lua.pushnil
        Just s → do
            Lua.newtable
            intField "id" (cstRequestId s)
            field "destination" (T.pack (crDestination (cstRequest s)))
            boolField "overwrite" (crOverwrite (cstRequest s))
            field "stage" (tshow (cstStage s))
            boolField "published" (cstPublication s ≡ Published)
            forM_ (cstOutcome s) $ \o → case o of
                CaptureSucceeded r → do
                    field "outcome" "succeeded"
                    intField "bytes" (crpBytes r)
                    boolField "replaced" (crpReplaced r)
                    intField "warnings" (length (crpWarnings r))
                    Lua.newtable
                    forM_ (crpEntries r) $ \(family, n) → intField family n
                    Lua.setfield (-2) "entries"
                CaptureUnsuccessful f → do
                    field "outcome" "unsuccessful"
                    field "kind" (tshow (cfKind f))
                    forM_ (cfPhase f) $ \p → field "phase" (phaseText p)
                    field "detail" (cfDetail f)
                    field "failure" (captureFailureText f)
                    intField "findings" (length (cfDiagnostics f))
                    forM_ (take 1 (cfDiagnostics f)) $ \d → field "firstFinding" (sdPath d)
    pure 1
