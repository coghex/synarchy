{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Regression coverage for issue #889's capability-record split:
--   'Engine.Core.Log.Monad's @*M@ wrappers now call through an extra
--   internal @*For@ layer before reaching 'Engine.Core.Log.logMessage'.
--   'Engine.Core.Log.extractCallSite' identifies the true external
--   caller by walking the 'GHC.Stack.CallStack' and dropping every
--   frame whose name is a KNOWN internal logging-wrapper name — the
--   new @*For@ names must be in that list too, or the reported source
--   location regresses to an internal 'Engine.Core.Log.Monad' call
--   site instead of the real caller.
module Test.Headless.Core.LogMonad (spec) where

import UPrelude
import Test.Hspec
import Data.List (isInfixOf)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import GHC.Stack (SrcLoc(..))
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.Monad (runEngineM, EngineM')
import Engine.Core.State (EngineEnv, loggerRef)
import Engine.Core.Log
  ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
  , LogCategory(..), LogEntry(..)
  )
import Engine.Core.Log.Monad (logInfoM)

spec ∷ Spec
spec = describe "capability-scoped logging source-location attribution (#889)" $
  it "attributes a logInfoM call to ITS OWN call site, not an internal Log.Monad frame" $ do
    EngineInitResult env ← initializeEngineHeadless
    capturedRef ← newIORef []
    testLogger ← initLogger defaultLogConfig
      { lcBackend = LogToCallback (\e → modifyIORef' capturedRef (e :))
      }
    writeIORef (loggerRef env) testLogger

    let action ∷ EngineM' EngineEnv ()
        action = logInfoM CatTest "probe message"
    _ ← runEngineM action env pure

    entries ← readIORef capturedRef
    case entries of
      [entry] → case leSrcLoc entry of
        Nothing → expectationFailure "expected logInfoM to capture a source location"
        Just loc → do
          srcLocFile loc `shouldSatisfy` ("Test/Headless/Core/LogMonad.hs" `isInfixOf`)
          srcLocFile loc `shouldNotSatisfy` ("Log/Monad.hs" `isInfixOf`)
      other → expectationFailure $
        "expected exactly one captured log entry, got " ⧺ show (length other)
