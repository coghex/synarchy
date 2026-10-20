-- | engine.setPaused's load-in-flight gate (round 15 review, issue
--   #763): staging runs BEFORE the save barrier's capture lock is ever
--   entered, and the Lua thread's own tick loop keeps servicing
--   debug/script work throughout that entire window — so an unpause
--   (setPaused(false)) landing there could resume the OLD, still-live
--   session's simulation before the transaction either publishes or
--   fails. A subsequent staging failure must leave the pre-load
--   session's pause state exactly as it was, per the #763 "nothing
--   changed" contract. A pause (setPaused(true)) is never blocked.
--   Drives 'setPausedFn' directly against a real (script-less) Lua
--   backend, mirroring "Test.Headless.Lua.DebugQueue" and
--   "Test.Headless.Lua.PreviewGeneration".
module Test.Headless.Lua.PauseGate (spec) where

import UPrelude
import Test.Hspec
import qualified HsLua as Lua
import Data.IORef (readIORef, writeIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Capability.SaveLoad (SaveLoadCapability(..), toSaveLoadCapability)
import qualified Engine.Core.Queue as Q
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Engine.Core.Capability.WorldSim (toWorldSimCapability, withPlayerIntentHeld)
import Engine.Load.Status (beginLoad, claimScenarioLoad, failLoad, terminateScenario)
import Scenario.Load.Service (requestScenarioLoad)
import Scenario.Load.Types
import Engine.Scripting.Lua.API.Core (setPausedFn)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Types (LuaBackendState(..))

-- | Call @setPausedFn env@ with the boolean argument HsLua's own
--   @toboolean 1@ reads pushed onto the stack first, in a fresh
--   'Lua.LuaE' run against the given backend's real 'Lua.State'.
--   Returns the function's own boolean result (round 16 rereview:
--   'setPausedFn' now reports whether it actually applied the flag,
--   which scripts/pause.lua's caller-side fix depends on).
callSetPaused ∷ LuaBackendState → EngineEnv → Bool → IO Bool
callSetPaused ls env b =
    Lua.runWith (lbsLuaState ls) $ do
        Lua.pushboolean b
        _ ← setPausedFn env
        applied ← Lua.toboolean (-1)
        Lua.pop 1
        pure applied

spec ∷ SpecWith EngineEnv
spec = describe "engine.setPaused load-in-flight gate (round 15 review, \
                \issue #763)" $ do
    it "setPaused(false) is rejected while a load transaction is in \
       \flight, leaving the pause flag untouched" $ \env → do
        ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                    (assetPoolRef env) (nextObjectIdRef env)
                                    (inputStateRef env) (loggerRef env)
        writeIORef (enginePausedRef env) True

        Right reqId ← beginLoad (loadStatusRef env) "pausegate_probe"

        applied ← callSetPaused ls env False
        applied `shouldBe` False
        stillPaused ← readIORef (enginePausedRef env)
        stillPaused `shouldBe` True

        -- loadStatusRef is shared across every 'it' in this describe
        -- block's engine (aroundAll) -- end the transaction so it
        -- doesn't leave loadInProgress stuck true for later tests.
        failLoad (loadStatusRef env) reqId "test cleanup"

    it "setPaused(true) is never blocked, even while a load transaction \
       \is in flight" $ \env → do
        ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                    (assetPoolRef env) (nextObjectIdRef env)
                                    (inputStateRef env) (loggerRef env)
        writeIORef (enginePausedRef env) False

        Right reqId ← beginLoad (loadStatusRef env) "pausegate_probe2"

        applied ← callSetPaused ls env True
        applied `shouldBe` True
        nowPaused ← readIORef (enginePausedRef env)
        nowPaused `shouldBe` True

        failLoad (loadStatusRef env) reqId "test cleanup"

    it "setPaused(false) is accepted again once the load transaction \
       \ends (simulated here as a failed/aborted load, mirroring the \
       \#763 'nothing changed' contract)" $ \env → do
        ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                    (assetPoolRef env) (nextObjectIdRef env)
                                    (inputStateRef env) (loggerRef env)
        writeIORef (enginePausedRef env) True

        Right reqId ← beginLoad (loadStatusRef env) "pausegate_probe3"
        duringApplied ← callSetPaused ls env False
        duringApplied `shouldBe` False
        stillPausedDuring ← readIORef (enginePausedRef env)
        stillPausedDuring `shouldBe` True

        failLoad (loadStatusRef env) reqId "test abort"
        afterApplied ← callSetPaused ls env False
        afterApplied `shouldBe` True
        unpausedAfter ← readIORef (enginePausedRef env)
        unpausedAfter `shouldBe` False

    -- #2713: a SCENARIO load is a load transaction too, so the same gate
    -- must hold from the moment it is accepted until it is terminal. The
    -- request is made through the production entry point and then left
    -- un-pumped, which is exactly a request held in flight.
    it "setPaused(false) is rejected while a SCENARIO load is in flight and \
       \accepted again once it ends" $ \env → do
        ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                    (assetPoolRef env) (nextObjectIdRef env)
                                    (inputStateRef env) (loggerRef env)
        writeIORef (enginePausedRef env) False
        accepted ← requestScenarioLoad env (SourceBytes "pausegate-scenario" "version: 3\n")
        reqId ← either (\b → fail ("not accepted: " ⧺ show b)) pure accepted
        -- acceptance itself paused the session, synchronously
        readIORef (enginePausedRef env) `shouldReturn` True
        duringApplied ← callSetPaused ls env False
        duringApplied `shouldBe` False
        readIORef (enginePausedRef env) `shouldReturn` True
        -- the request ends (unsuccessfully, before any replacement) and the
        -- queued hand-off message it left is dropped with it
        terminateScenario (slLoadStatusRef (toSaveLoadCapability env)) reqId
            (ScenarioUnsuccessful (FailureContext StagePreflight (CauseSetup "test abort") []))
            RecoveryPreserved
        _ ← Q.flushQueue (luaQueue env)
        afterApplied ← callSetPaused ls env False
        afterApplied `shouldBe` True
        readIORef (enginePausedRef env) `shouldReturn` False

    -- #2713 (review round 1): the refusal of an unpause is decided INSIDE
    -- the player-intent lock, which every pause transition takes. An
    -- operation admitted on another thread between a check and that lock
    -- must still win: with the check taken before the lock, the unpause
    -- below reads "no load in flight", waits for the lock, and then clears
    -- the pause the accepted operation imposes.
    it "setPaused(false) decides under the pause lock, so a load admitted while it waits still refuses it" $ \env → do
        ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                    (assetPoolRef env) (nextObjectIdRef env)
                                    (inputStateRef env) (loggerRef env)
        writeIORef (enginePausedRef env) False
        result ← newEmptyMVar
        reqId ← withPlayerIntentHeld (toWorldSimCapability env) $ \_ → do
            -- the unpause starts and waits for the lock this thread holds
            _ ← forkIO (callSetPaused ls env False ≫= putMVar result)
            threadDelay 100000
            -- a scenario load is admitted meanwhile (its claim does not take
            -- the lock; its own pause would wait for it)
            admitted ← claimScenarioLoad (slLoadStatusRef (toSaveLoadCapability env))
                           (pure False) "pause-race"
            either (\b → fail ("not admitted: " ⧺ show b)) pure admitted
        applied ← takeMVar result
        applied `shouldBe` False
        terminateScenario (slLoadStatusRef (toSaveLoadCapability env)) reqId
            (ScenarioUnsuccessful (FailureContext StagePreflight (CauseSetup "test abort") []))
            RecoveryPreserved
