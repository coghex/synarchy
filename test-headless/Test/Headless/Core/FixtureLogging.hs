-- | The headless suite's fixture log backend (#1925).
--
--   'Test.Headless.Harness.Log' is the suite's engine-initialization
--   boundary; these are the properties the rest of the suite's silence
--   rests on, and none of them is observable from a spec that merely
--   boots quietly and sees nothing.
--
--     * the preference-free fixture picks a non-writing backend, and
--       picks it BEFORE initialization — the only point at which the
--       initializer's own entries can still be steered;
--     * a stdout-writing boot really does put those entries on stdout —
--       the positive control, without which the quiet case would pass
--       just as happily against a broken observation (the witness entry
--       is Debug-level since #1928, so those examples set
--       @ENGINE_DEBUG=event@ for themselves);
--     * the quiet boot puts nothing on either stream;
--     * an explicitly supplied callback sees an initialization-time
--       entry, and sees it by the time the initializer returns;
--     * @SYNARCHY_TEST_LOG@ turns the quiet default back into a logging
--       one with no source edit, and refuses a value it does not
--       recognize instead of silently staying quiet.
module Test.Headless.Core.FixtureLogging (spec) where

import UPrelude
import Test.Hspec

import Control.Exception (ErrorCall, bracket, finally)
import Data.List (isInfixOf)
import qualified Data.Text as T
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (Handle, hClose, hFlush, stderr, stdout)
import qualified System.IO as IO
import GHC.IO.Handle (hDuplicate, hDuplicateTo)

import Engine.Core.Log.Types (LogBackend(..), LogEntry(..))
import Test.Headless.Harness.Log
  ( diagnosticLogEnvVar
  , initializeEngineHeadlessLogging
  , initializeEngineHeadlessQuiet
  , newLogCapture
  , resolveFixtureLogBackend
  )

-- | An entry every headless boot emits from inside
--   'Engine.Core.Init.initializeEngineWith', before the initializer
--   returns. 'Engine.Asset.YamlNotifications.loadNotificationCfg' is
--   handed the logger directly, so this line is unreachable to any
--   fixture that installs its backend after the initializer returns —
--   which is what makes it the right witness here.
--
--   #1928 moved it from Info to Debug (an ordinary registry load is no
--   longer narrated), so the examples that need to SEE it run under
--   'withEventDebug'. What it witnesses is unchanged: the entry is
--   still emitted from the same call, at the same point, through
--   whatever backend the fixture installed.
initTimeMarker ∷ String
initTimeMarker = "Notification registry loaded"

-- | @ENGINE_DEBUG@, which 'Engine.Core.Log.initLogger' reads once per
--   boot. Named here so the examples below and the reason above cannot
--   drift apart.
engineDebugEnvVar ∷ String
engineDebugEnvVar = "ENGINE_DEBUG"

spec ∷ Spec
spec = describe "headless fixture logging" $ do

  describe "the backend a preference-free fixture selects" $ do
    it "is a non-writing callback when the diagnostic variable is unset" $
      withDiagnosticVar Nothing $ do
        backend ← resolveFixtureLogBackend
        show backend `shouldBe` "LogToCallback"

    -- Under 'withEventDebug' too, so this is not the vacuous pass it
    -- would be if initialization had nothing to emit in the first
    -- place: the very entry the two positive controls below SEE is the
    -- one neither stream receives here.
    it "is installed before initialization, so no boot entry is written" $
      withEventDebug $ withDiagnosticVar Nothing $ do
        (out, err) ← capturingOutput $ void initializeEngineHeadlessQuiet
        out `shouldBe` ""
        err `shouldBe` ""

    -- Without this, the case above would pass just as happily against
    -- an observation that never sees anything.
    it "really is what silences the stream — a handle backend still writes" $
      withEventDebug $ do
        (out, _) ← capturingOutput $
          void (initializeEngineHeadlessLogging (LogToHandle stdout))
        out `shouldSatisfy` (initTimeMarker `isInfixOf`)

  describe "an explicitly supplied backend" $
    it "observes an initialization-time entry before the initializer returns" $
      withEventDebug $ do
        (backend, drain) ← newLogCapture
        void (initializeEngineHeadlessLogging backend)
        entries ← drain
        entries `shouldSatisfy` (not ∘ null)
        map (T.unpack ∘ leMessage) entries
          `shouldSatisfy` any (initTimeMarker `isInfixOf`)

  describe "the SYNARCHY_TEST_LOG diagnostic rerun" $ do
    it "sends a quiet fixture's own boot entries to stderr, keeping stdout clean" $
      withEventDebug $ withDiagnosticVar (Just "stderr") $ do
        (out, err) ← capturingOutput $ void initializeEngineHeadlessQuiet
        err `shouldSatisfy` (initTimeMarker `isInfixOf`)
        out `shouldBe` ""

    it "restores the pre-#1925 stdout stream on request" $
      withEventDebug $ withDiagnosticVar (Just "stdout") $ do
        (out, _) ← capturingOutput $ void initializeEngineHeadlessQuiet
        out `shouldSatisfy` (initTimeMarker `isInfixOf`)

    it "treats unset, empty and 'quiet' alike, and ignores case and padding" $
      forM_ [Nothing, Just "", Just "quiet", Just "QUIET", Just " quiet "] $ \value →
        withDiagnosticVar value $ do
          backend ← resolveFixtureLogBackend
          show backend `shouldBe` "LogToCallback"

    it "refuses a value it does not recognize rather than staying quiet" $
      withDiagnosticVar (Just "verbose") $
        resolveFixtureLogBackend `shouldThrow` \e →
          let rendered = show (e ∷ ErrorCall)
          in  diagnosticLogEnvVar `isInfixOf` rendered
                ∧ "verbose" `isInfixOf` rendered
                ∧ "quiet, stderr, stdout" `isInfixOf` rendered

-- | Run an action with 'diagnosticLogEnvVar' set to an exact value (or
--   removed), restoring whatever the surrounding environment had.
withDiagnosticVar ∷ Maybe String → IO α → IO α
withDiagnosticVar = withEnvVar diagnosticLogEnvVar

-- | Run an action with @ENGINE_DEBUG=event@, which is what makes
--   'initTimeMarker' — Debug-level since #1928 — reachable at all. Set
--   here rather than left to the ambient environment so the examples
--   below neither depend on a developer's shell nor are silenced by one
--   that names some other category ('loadDebugCategoriesFromEnv'
--   REPLACES the enabled set rather than adding to it).
withEventDebug ∷ IO α → IO α
withEventDebug = withEnvVar engineDebugEnvVar (Just "event")

-- | Set (or remove) one environment variable for the duration,
--   restoring whatever the surrounding environment had. The suite is
--   sequential, so this reaches exactly the example that asked for it.
withEnvVar ∷ String → Maybe String → IO α → IO α
withEnvVar name value action =
    bracket (lookupEnv name) apply (\_ → apply value ≫ action)
  where
    apply = maybe (unsetEnv name) (setEnv name)

-- | Everything the action writes to the process's stdout and stderr.
--
--   The suite is sequential (@Spec.hs@ never marks a spec @parallel@),
--   so redirecting the process-wide handles for one example captures
--   that example and nothing else. Both streams are flushed before the
--   swap so no pending formatter output lands in the capture, and after
--   it so nothing the action wrote is still buffered when the file is
--   read.
capturingOutput ∷ IO α → IO (String, String)
capturingOutput action =
    withRedirect stdout $ \readOut →
      withRedirect stderr $ \readErr → do
        _ ← action
        (,) ⊚ readOut ⊛ readErr

-- | Point one handle at a temporary file for the duration, handing the
--   body a reader for what was written. The handle is restored, and the
--   file removed, however the body ends.
--
--   The sink is closed as soon as it has been duplicated onto the
--   handle: the duplicate keeps the descriptor alive, while holding the
--   sink open would keep GHC's per-file lock and make every read of the
--   same path fail with @resource busy@.
withRedirect ∷ Handle → (IO String → IO α) → IO α
withRedirect handle body = do
    tmpDir ← getTemporaryDirectory
    bracket (acquire tmpDir) release $ \(path, _) →
      body (hFlush handle ≫ IO.readFile' path)
  where
    acquire tmpDir = do
      hFlush handle
      saved ← hDuplicate handle
      (path, sink) ← IO.openTempFile tmpDir "synarchy-fixture-log.txt"
      hDuplicateTo sink handle
      hClose sink
      pure (path, saved)
    release (path, saved) =
      (hFlush handle ≫ hDuplicateTo saved handle ≫ hClose saved)
        `finally` removeFile path
