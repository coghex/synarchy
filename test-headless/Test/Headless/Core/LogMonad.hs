-- | Regression coverage for logging source-location attribution
--   ('Engine.Core.Log.extractCallSite').
--
--   A log entry must be attributed to the place the logging call was
--   WRITTEN, never to a frame inside 'Engine.Core.Log' or
--   'Engine.Core.Log.Monad'. Issue #889 broke that once already: the
--   new capability-scoped @*For@ layer was missing from the internal
--   function-name skip list, so attribution stopped at a Log.Monad
--   frame. #945 removed that list — attribution now reads the
--   outermost 'GHC.Stack.CallStack' frame, which is the public entry
--   point paired with its external caller — so internal helpers can be
--   added, renamed, removed, or shared freely.
--
--   What that mechanism cannot check is that each PUBLIC entry point
--   carries 'HasCallStack'. These cases are the enforcement: one per
--   live entry-point family, and a NEW public logging family must add
--   its own here.
module Test.Headless.Core.LogMonad (spec) where

import UPrelude
import Test.Hspec
import Data.List (isInfixOf)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import GHC.Stack (SrcLoc(..))
import Engine.Core.Init (EngineInitResult(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Engine.Core.Monad (runEngineM, EngineM')
import Engine.Core.State (EngineEnv, loggerRef)
import Engine.Core.Capability.Core (toCoreCapability)
import Engine.Core.Error.Exception
  (ExceptionType(..), SystemError(..))
import Engine.Core.Log
  ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
  , LogCategory(..), LogEntry(..), LoggerState
  , logInfo, logThreadInfo, logInfoS
  )
import Engine.Core.Log.Monad (logInfoFor, logInfoM, logAndThrowM)

spec ∷ Spec
spec = describe "logging source-location attribution" $ do

  it "attributes a direct logInfo call to its own call site" $ do
    entries ← withCapturingLogger True $ \logger →
      logInfo logger CatTest "direct normal probe"
    expectExternalCallSite entries

  it "attributes a direct logThreadInfo call to its own call site" $ do
    -- The thread FORMATTER blanks the location column on purpose, so
    -- this inspects the structured entry through LogToCallback.
    entries ← withCapturingLogger True $ \logger →
      logThreadInfo logger CatTest "direct thread probe"
    expectExternalCallSite entries

  it "attributes a structured logInfoS call to its own call site" $ do
    entries ← withCapturingLogger True $ \logger →
      logInfoS logger CatTest "structured probe" [("probe", "yes")]
    expectExternalCallSite entries

  it "attributes a capability-scoped logInfoFor call to its own call site" $ do
    entries ← withCapturingEngine $ \env →
      logInfoFor (toCoreCapability env) CatTest "capability-scoped probe"
    expectExternalCallSite entries

  it "attributes a logInfoM call to ITS OWN call site, not an internal Log.Monad frame (#889)" $ do
    entries ← withCapturingEngine $ \env → do
      let action ∷ EngineM' ()
          action = logInfoM CatTest "monadic probe"
      _ ← runEngineM action env pure
      pure ()
    expectExternalCallSite entries

  it "attributes a logAndThrowM call to its own call site" $ do
    entries ← withCapturingEngine $ \env → do
      let action ∷ EngineM' ()
          action = logAndThrowM CatTest (ExSystem (IOError "probe")) "exception probe"
      _ ← runEngineM action env pure
      pure ()
    expectExternalCallSite entries

  it "captures no source location when lcShowLocation is disabled" $ do
    -- Covers the gate on BOTH writers: logMessage and logThreadMessage.
    entries ← withCapturingLogger False $ \logger → do
      logInfo logger CatTest "normal probe, location disabled"
      logThreadInfo logger CatTest "thread probe, location disabled"
    length entries `shouldBe` 2
    map leSrcLoc entries `shouldSatisfy` all isNothing

-- | Install a fresh callback-backed logger, run the action against it,
--   and return the entries it captured in emission order.
withCapturingLogger ∷ Bool → (LoggerState → IO ()) → IO [LogEntry]
withCapturingLogger showLocation act = do
  capturedRef ← newIORef []
  logger ← initLogger defaultLogConfig
    { lcBackend = LogToCallback (\e → modifyIORef' capturedRef (e :))
    , lcShowLocation = showLocation
    }
  act logger
  reverse ⊚ readIORef capturedRef

-- | As 'withCapturingLogger', but for the entry points that reach the
--   logger through an 'EngineEnv' (the @*For@, @*M@ and @logAndThrow*@
--   families) — the capturing logger is installed on the env.
withCapturingEngine ∷ (EngineEnv → IO ()) → IO [LogEntry]
withCapturingEngine act = do
  EngineInitResult env ← initializeEngineHeadlessQuiet
  capturedRef ← newIORef []
  logger ← initLogger defaultLogConfig
    { lcBackend = LogToCallback (\e → modifyIORef' capturedRef (e :))
    }
  writeIORef (loggerRef env) logger
  act env
  reverse ⊚ readIORef capturedRef

-- | Exactly one entry was captured, and its location is this test
--   module's own call site rather than anywhere inside the logging
--   implementation.
expectExternalCallSite ∷ HasCallStack ⇒ [LogEntry] → Expectation
expectExternalCallSite entries = case entries of
  [entry] → case leSrcLoc entry of
    Nothing → expectationFailure "expected the entry to capture a source location"
    Just loc → do
      srcLocFile loc `shouldSatisfy` ("Test/Headless/Core/LogMonad.hs" `isInfixOf`)
      srcLocFile loc `shouldNotSatisfy` ("Engine/Core/Log.hs" `isInfixOf`)
      srcLocFile loc `shouldNotSatisfy` ("Engine/Core/Log/Monad.hs" `isInfixOf`)
  other → expectationFailure $
    "expected exactly one captured log entry, got " ⧺ show (length other)
