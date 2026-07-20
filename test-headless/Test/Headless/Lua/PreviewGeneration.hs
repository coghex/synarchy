{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | The 'LuaWorldPreviewReady' delivery-time generation gate (round 11
--   review, issue #763): staleness for a world-preview GPU upload can
--   NOT be decided at upload-completion time
--   ("Engine.Scripting.Lua.Message.WorldTexture.handleWorldPreview") —
--   that races 'World.Load.Publish.publishStagedSession', which runs
--   asynchronously on the world thread and can supersede an upload that
--   already finished its own check. The generation the upload was
--   dequeued under now rides in the message itself and is validated
--   HERE, in "Engine.Scripting.Lua.Thread.Dispatch", which only ever
--   runs once the save barrier's capture lock has released — by which
--   point any racing publish has unconditionally already bumped
--   'worldPreviewGenerationRef'. This drives 'processLuaMsg' directly
--   against a real (but script-less) Lua backend, exactly like
--   "Test.Headless.Lua.DebugQueue" — no real load transaction, no GPU
--   upload; that end-to-end path is tools/offscreen_probe.py's job.
module Test.Headless.Lua.PreviewGeneration (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Dispatch (processLuaMsg)
import Engine.Scripting.Lua.Types (LuaMsg(..))

spec ∷ SpecWith EngineEnv
spec = describe "LuaWorldPreviewReady generation validated at delivery \
                \(round 11 review, issue #763)" $ do
    it "discards a preview announcement whose generation is stale (a \
       \publish bumped worldPreviewGenerationRef after this upload was \
       \dequeued but before its message was delivered)" $ \env → do
        ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                    (assetPoolRef env) (nextObjectIdRef env)
                                    (inputStateRef env) (loggerRef env)
        stateRef ← newIORef ThreadRunning

        -- This upload was dequeued under generation 0 (worldPreviewGenerationRef
        -- starts at 0 for a fresh engine and this test doesn't otherwise
        -- touch it), then a publish raced it and bumped the counter to 1
        -- before the announcement was ever delivered.
        staleGen ← readIORef (worldPreviewGenerationRef env)
        _ ← atomicModifyIORef' (worldPreviewGenerationRef env)
                (\g → (g + 1, g + 1))

        -- broadcastToModules with zero scripts loaded is a silent no-op
        -- either way, so this only proves the STALE branch is taken by
        -- observing it does NOT crash/throw reaching for Lua state that
        -- isn't there and the generation truly no longer matches.
        latestBefore ← readIORef (worldPreviewGenerationRef env)
        latestBefore `shouldSatisfy` (≢ staleGen)
        processLuaMsg env ls stateRef (LuaWorldPreviewReady 7 staleGen)
        -- Processing a stale announcement must never itself mutate the
        -- generation counter (only a publish does that).
        afterStale ← readIORef (worldPreviewGenerationRef env)
        afterStale `shouldBe` latestBefore

    it "accepts a preview announcement whose generation still matches \
       \the current counter (nothing superseded it)" $ \env → do
        ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                    (assetPoolRef env) (nextObjectIdRef env)
                                    (inputStateRef env) (loggerRef env)
        stateRef ← newIORef ThreadRunning

        currentGen ← readIORef (worldPreviewGenerationRef env)
        -- Must not throw, and must not itself perturb the counter —
        -- only a publish is allowed to bump it.
        processLuaMsg env ls stateRef (LuaWorldPreviewReady 7 currentGen)
        afterAccepted ← readIORef (worldPreviewGenerationRef env)
        afterAccepted `shouldBe` currentGen

    it "a fresh publish's generation always wins over an older, still- \
       \in-flight upload's generation, no matter how many other \
       \publishes raced in between" $ \env → do
        ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                    (assetPoolRef env) (nextObjectIdRef env)
                                    (inputStateRef env) (loggerRef env)
        stateRef ← newIORef ThreadRunning

        oldGen ← readIORef (worldPreviewGenerationRef env)
        -- Three publishes race the old upload in a row.
        _ ← atomicModifyIORef' (worldPreviewGenerationRef env) (\g → (g + 1, g + 1))
        _ ← atomicModifyIORef' (worldPreviewGenerationRef env) (\g → (g + 1, g + 1))
        newGen ← atomicModifyIORef' (worldPreviewGenerationRef env)
                    (\g → (g + 1, g + 1))

        -- The old upload's announcement (oldGen) must be discarded...
        processLuaMsg env ls stateRef (LuaWorldPreviewReady 1 oldGen)
        genAfterOld ← readIORef (worldPreviewGenerationRef env)
        genAfterOld `shouldBe` newGen

        -- ...while the newest publish's own announcement (newGen) is
        -- accepted, and neither call perturbs the counter itself.
        processLuaMsg env ls stateRef (LuaWorldPreviewReady 2 newGen)
        genAfterNew ← readIORef (worldPreviewGenerationRef env)
        genAfterNew `shouldBe` newGen
