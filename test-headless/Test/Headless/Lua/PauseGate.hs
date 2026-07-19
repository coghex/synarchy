{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
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
import Engine.Load.Status (beginLoad, failLoad)
import Engine.Scripting.Lua.API.Core (setPausedFn)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Types (LuaBackendState(..))

-- | Call @setPausedFn env@ with the boolean argument HsLua's own
--   @toboolean 1@ reads pushed onto the stack first, in a fresh
--   'Lua.LuaE' run against the given backend's real 'Lua.State'.
callSetPaused ∷ LuaBackendState → EngineEnv → Bool → IO ()
callSetPaused ls env b = do
    _ ← Lua.runWith (lbsLuaState ls) $ do
        Lua.pushboolean b
        setPausedFn env
    pure ()

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

        callSetPaused ls env False
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

        callSetPaused ls env True
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
        callSetPaused ls env False
        stillPausedDuring ← readIORef (enginePausedRef env)
        stillPausedDuring `shouldBe` True

        failLoad (loadStatusRef env) reqId "test abort"
        callSetPaused ls env False
        unpausedAfter ← readIORef (enginePausedRef env)
        unpausedAfter `shouldBe` False
