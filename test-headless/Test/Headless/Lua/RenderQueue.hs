{-# LANGUAGE UnicodeSyntax #-}
-- | Regression coverage for the render/headless consumers' load-publication
--   boundary.  A whole-session load replaces the live UI/scene generation,
--   so Lua-to-engine work queued by the old generation must be discarded
--   while that boundary is locked rather than executed on the first tick
--   afterward.  Plain saves retain the same session and must keep their work.
module Test.Headless.Lua.RenderQueue (spec) where

import UPrelude
import Test.Hspec
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Engine.Load.Status (beginLoad, finishLoad)
import Engine.Scripting.Lua.Message (discardLuaMessagesForActiveLoad)
import Engine.Scripting.Lua.Types (LuaToEngineMsg(..))

spec ∷ SpecWith EngineEnv
spec = describe "Lua-to-engine load-publication queue" $ do
    it "discards old UI/scene work during an active whole-session load" $ \env → do
        Right requestId ← beginLoad (loadStatusRef env) "render-queue-test"
        Q.writeQueue (luaToEngineQueue env) (LuaSetBrightness 73)

        discarded ← discardLuaMessagesForActiveLoad env
        discarded `shouldBe` 1
        Q.flushQueue (luaToEngineQueue env) `shouldReturn` []
        finishLoad (loadStatusRef env) requestId

    it "preserves queued work when no load is active (including a normal save)" $ \env → do
        Q.writeQueue (luaToEngineQueue env) (LuaSetBrightness 74)

        discarded ← discardLuaMessagesForActiveLoad env
        discarded `shouldBe` 0
        Q.flushQueue (luaToEngineQueue env) `shouldReturn` [LuaSetBrightness 74]
