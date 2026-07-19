{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | The 'LuaSaveLoaded' stale-debug-command gate (round 10 review,
--   issue #763): the debug-console TCP server keeps accepting commands
--   onto 'lbsDebugQueue' regardless of the save barrier's capture-lock
--   state, so a command issued while a load transaction holds that
--   boundary can still be sitting in the queue once the transaction
--   publishes and 'Engine.Scripting.Lua.Thread.Dispatch' processes the
--   'LuaSaveLoaded' reconciliation message — at which point it targets
--   a session that no longer exists. 'processLuaMsg' must cancel any
--   such command right there (resolving its response 'MVar' rather
--   than leaving the caller hanging or, worse, letting a LATER
--   'processDebugCommands' call execute it against the replacement
--   session). No real load transaction is driven here — that full
--   engine.saveWorld / engine.loadSave round trip is
--   tools/transactional_load_probe.py's job; this is a direct,
--   isolated call against 'LuaSaveLoaded', mirroring how
--   "Test.Headless.Input.Followup" drives 'processLuaMsg' directly.
module Test.Headless.Lua.DebugQueue (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent.MVar (newEmptyMVar, tryTakeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (newTQueue, writeTQueue, tryReadTQueue)
import Data.IORef (newIORef)
import qualified Data.Text as T
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.DebugServer (DebugCommand(..))
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Dispatch (processLuaMsg)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaMsg(..))

-- | A bare Lua backend (full API registered, no script loaded — the
--   'LuaSaveLoaded' handler's 'broadcastToModules' call is a no-op
--   with zero scripts, which is fine here since this test only cares
--   about the debug-queue side effect), with its own private debug
--   queue swapped in so the test can push fake commands onto it and
--   read back what happened to their response 'MVar's.
newBareBackendWithDebugQueue ∷ EngineEnv → IO LuaBackendState
newBareBackendWithDebugQueue env = do
    ls0 ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                 (assetPoolRef env) (nextObjectIdRef env)
                                 (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls0) env ls0 stateRef
    privateQueue ← atomically newTQueue
    pure ls0 { lbsDebugQueue = privateQueue }

spec ∷ SpecWith EngineEnv
spec = describe "LuaSaveLoaded stale debug-command cancellation (round 10 review, issue #763)" $ do
    it "cancels a debug command still queued at the load handoff, \
       \resolving its response MVar instead of leaving it to execute \
       \against the replacement session" $ \env → do
        ls ← newBareBackendWithDebugQueue env
        stateRef ← newIORef ThreadRunning
        respVar ← newEmptyMVar
        atomically $ writeTQueue (lbsDebugQueue ls)
            (DebugCommand "world.setDate('some_page', 9999, 1, 1)" respVar)

        processLuaMsg env ls stateRef (LuaSaveLoaded 123456 [] [])

        resp ← tryTakeMVar respVar
        case resp of
            Nothing → expectationFailure
                "stale debug command's response MVar was never resolved \
                \-- its caller (netcat, a script) would hang"
            Just msg → do
                msg `shouldSatisfy` T.isInfixOf "REJECTED"
                -- The command itself must never have been evaluated —
                -- only its MVar was resolved with a rejection message.
                msg `shouldSatisfy` (not . T.isInfixOf "setDate")

    it "cancels every stale command queued at the handoff, not just the \
       \first" $ \env → do
        ls ← newBareBackendWithDebugQueue env
        stateRef ← newIORef ThreadRunning
        respVar1 ← newEmptyMVar
        respVar2 ← newEmptyMVar
        atomically $ writeTQueue (lbsDebugQueue ls)
            (DebugCommand "return 1" respVar1)
        atomically $ writeTQueue (lbsDebugQueue ls)
            (DebugCommand "return 2" respVar2)

        processLuaMsg env ls stateRef (LuaSaveLoaded 654321 [] [])

        r1 ← tryTakeMVar respVar1
        r2 ← tryTakeMVar respVar2
        r1 `shouldSatisfy` maybe False (T.isInfixOf "REJECTED")
        r2 `shouldSatisfy` maybe False (T.isInfixOf "REJECTED")

    it "leaves the debug queue empty afterward (no leftover stale \
       \commands for a later processDebugCommands call to pick up)" $ \env → do
        ls ← newBareBackendWithDebugQueue env
        stateRef ← newIORef ThreadRunning
        respVar ← newEmptyMVar
        atomically $ writeTQueue (lbsDebugQueue ls)
            (DebugCommand "return 1" respVar)

        processLuaMsg env ls stateRef (LuaSaveLoaded 42 [] [])

        remaining ← atomically $ tryReadTQueue (lbsDebugQueue ls)
        isNothing remaining `shouldBe` True
