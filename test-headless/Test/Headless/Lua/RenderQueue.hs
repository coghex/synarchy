-- | Regression coverage for the render/headless consumers\' load-publication
--   boundary.  A whole-session load replaces the live UI/scene generation,
--   so Lua-to-engine work queued by the old generation must be discarded
--   once that load COMMITS to publishing, rather than executed on the first
--   tick afterward.  Plain saves retain the same session and must keep their
--   work — and so, since #2221, must a load that has not committed yet: the
--   discard is irreversible, and every load failure before
--   'World.Command.Types.WorldLoadPublish' is queued leaves the old session
--   live and unchanged, with this queued work still owed a run.
module Test.Headless.Lua.RenderQueue (spec) where

import UPrelude
import Test.Hspec
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Engine.Load.Status
    ( LoadPhase(..), advanceLoad, armStaleLuaDiscard, beginLoad, failLoad
    , finishLoad )
import Engine.Scripting.Lua.Message (discardLuaMessagesForActiveLoad)
import Engine.Scripting.Lua.Types (LuaToEngineMsg(..))

spec ∷ SpecWith EngineEnv
spec = describe "Lua-to-engine load-publication queue" $ do
    it "discards old UI/scene work once a whole-session load commits to \
       \publishing" $ \env → do
        Right requestId ← beginLoad (loadStatusRef env) "render-queue-test"
        Q.writeQueue (luaToEngineQueue env) (LuaSetBrightness 73)
        -- What 'Engine.Scripting.Lua.Thread.Dispatch.commitLoadPublish'
        -- does as it queues the publish command itself.
        advanceLoad (loadStatusRef env) requestId LoadWaitingPublish
        armStaleLuaDiscard (loadStatusRef env)

        discarded ← discardLuaMessagesForActiveLoad env
        discarded `shouldBe` 1
        Q.flushQueue (luaToEngineQueue env) `shouldReturn` []
        finishLoad (loadStatusRef env) requestId

    it "preserves queued work while a load is merely IN PROGRESS: it is \
       \not committed to publishing yet, and an abort must leave the old \
       \session\'s work intact (#2221)" $ \env → do
        Right requestId ← beginLoad (loadStatusRef env) "render-queue-test"
        Q.writeQueue (luaToEngineQueue env) (LuaSetBrightness 75)
        -- Staged, past the capture boundary, but 'applyLuaLoad' has not
        -- returned: the publish can still fail from here.
        advanceLoad (loadStatusRef env) requestId LoadStaged

        discarded ← discardLuaMessagesForActiveLoad env
        discarded `shouldBe` 0
        failLoad (loadStatusRef env) requestId "failed applying Lua state"
        discardLuaMessagesForActiveLoad env `shouldReturn` 0
        Q.flushQueue (luaToEngineQueue env) `shouldReturn` [LuaSetBrightness 75]

    it "preserves queued work when no load is active (including a normal save)" $ \env → do
        Q.writeQueue (luaToEngineQueue env) (LuaSetBrightness 74)

        discarded ← discardLuaMessagesForActiveLoad env
        discarded `shouldBe` 0
        Q.flushQueue (luaToEngineQueue env) `shouldReturn` [LuaSetBrightness 74]
