-- | Regression coverage for the load publication's CUTOVER on
--   @luaToEngineQueue@ (the Lua-to-engine direction).
--
--   A whole-session load replaces the live UI/scene generation, so work
--   the REPLACED session queued must not execute afterwards. What makes
--   that a cutover rather than a flush-at-some-moment is that the
--   REPLACEMENT session legitimately queues work of its own — the
--   @LuaSaveLoaded@ reconciliation's @onSaveLoaded@ handlers do exactly
--   that — and none of it may be lost. The two sets are separable only
--   at one instant: inside
--   'Engine.Scripting.Lua.Thread.Dispatch.commitLoadPublish', on the
--   producer thread, after the Lua apply has committed and before
--   @WorldLoadPublish@ exists. Plain saves replace no generation and
--   keep their work.
module Test.Headless.Lua.RenderQueue (spec) where

import UPrelude
import Test.Hspec
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Message (discardStaleLuaToEngineWork)
import Engine.Scripting.Lua.Types (LuaToEngineMsg(..))

spec ∷ SpecWith EngineEnv
spec = describe "Lua-to-engine load-publication queue" $ do
    it "the cutover drops the replaced session's queued UI/scene work" $ \env → do
        Q.writeQueue (luaToEngineQueue env) (LuaSetBrightness 73)
        discardStaleLuaToEngineWork env `shouldReturn` 1
        Q.flushQueue (luaToEngineQueue env) `shouldReturn` []

    it "the cutover reports nothing when the replaced session left \
       \nothing queued" $ \env → do
        discardStaleLuaToEngineWork env `shouldReturn` 0

    it "work queued AFTER the cutover is the replacement session's and \
       \survives: the cutover is a boundary, not a standing flush" $ \env → do
        Q.writeQueue (luaToEngineQueue env) (LuaSetBrightness 73)
        discardStaleLuaToEngineWork env `shouldReturn` 1

        -- What publishStagedSession's LuaSaveLoaded reconciliation
        -- queues once the world thread runs the publish command this
        -- cutover was about to enqueue.
        Q.writeQueue (luaToEngineQueue env) (LuaSetBrightness 74)
        Q.flushQueue (luaToEngineQueue env)
            `shouldReturn` [LuaSetBrightness 74]
