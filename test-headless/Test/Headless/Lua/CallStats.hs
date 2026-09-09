module Test.Headless.Lua.CallStats (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (AsyncException(UserInterrupt), SomeAsyncException
                         , throwIO, try, bracket, finally, getMaskingState)
import Data.IORef (newIORef)
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.API.CallStats (getLuaCallStatsFn, resetLuaCallStatsFn)
import Engine.Scripting.Lua.API.Descriptor (luaVerb, retNone)
import Engine.Scripting.Lua.API.Internal
    (registerLuaFunction, registerLuaVerb, guardLuaAction)
import Engine.Scripting.Lua.CallStats
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import qualified HsLua as Lua
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)

spec ∷ Spec
spec = describe "Lua.CallStats" $ do
    around withFixture $ do
        it "starts unavailable at sequence zero and excludes both diagnostic verbs" $ \(_, ls) →
            check ls
                [ "local a = debug.getLuaCallStats()"
                , "assert(not a.available and a.sequence == 0 and #a.verbs == 0)"
                , "debug.resetLuaCallStats()"
                , "local b = debug.getLuaCallStats()"
                , "assert(not b.available and b.sequence == 1 and #b.verbs == 0)"
                , "assert(debug.getLuaCallStats().sequence == 1)" ]

        it "counts both registrars once and distinguishes equal names in different namespaces" $ \(_, ls) →
            check ls
                [ "raw.same(); described.same(); raw.same()"
                , "local s = debug.getLuaCallStats()"
                , "assert(s.available and s.sequence == 3 and #s.verbs == 2)"
                , "assert(s.verbs[1].id == 'described.same' and s.verbs[1].count == 1)"
                , "assert(s.verbs[2].id == 'raw.same' and s.verbs[2].count == 2)"
                , "for _, v in ipairs(s.verbs) do"
                , "  assert(math.type(v.count) == 'integer')"
                , "  assert(math.type(v.totalDurationNs) == 'integer')"
                , "  assert(math.type(v.maxDurationNs) == 'integer')"
                , "  assert(v.maxDurationNs >= 0 and v.totalDurationNs >= v.maxDurationNs)"
                , "end" ]

        it "returns independent tables and non-consuming reads" $ \(_, ls) →
            check ls
                [ "raw.same(); local a = debug.getLuaCallStats()"
                , "local b = debug.getLuaCallStats()"
                , "assert(a ~= b and a.verbs ~= b.verbs and a.verbs[1] ~= b.verbs[1])"
                , "a.verbs[1].count = 900; a.verbs[1].id = 'changed'"
                , "raw.same(); local c = debug.getLuaCallStats()"
                , "assert(b.sequence == 1 and b.verbs[1].count == 1)"
                , "assert(c.sequence == 2 and c.verbs[1].count == 2)"
                , "assert(c.verbs[1].id == 'raw.same')"
                , "assert(c.verbs[1].totalDurationNs >= b.verbs[1].totalDurationNs)"
                , "assert(c.verbs[1].maxDurationNs >= b.verbs[1].maxDurationNs)" ]

        it "resets rows while sequence continues across windows" $ \(_, ls) →
            check ls
                [ "raw.same(); debug.resetLuaCallStats(); debug.resetLuaCallStats()"
                , "local empty = debug.getLuaCallStats()"
                , "assert(not empty.available and empty.sequence == 3 and #empty.verbs == 0)"
                , "described.same(); local s = debug.getLuaCallStats()"
                , "assert(s.sequence == 4 and #s.verbs == 1 and s.verbs[1].count == 1)" ]

        it "records Lua errors and converted synchronous exceptions through both registrars" $ \(_, ls) →
            check ls
                [ "for _, api in ipairs({raw, described}) do"
                , "  local ok, err = pcall(api.luaError)"
                , "  assert(not ok and tostring(err):find('explicit Lua failure', 1, true))"
                , "  ok, err = pcall(api.haskellError)"
                , "  assert(not ok and tostring(err):find('Haskell exception in haskellError', 1, true))"
                , "end"
                , "local s = debug.getLuaCallStats()"
                , "assert(s.sequence == 4 and #s.verbs == 4)"
                , "for _, v in ipairs(s.verbs) do assert(v.count == 1) end"
                , "raw.same(); assert(debug.getLuaCallStats().sequence == 5)" ]

        it "records nested callbacks inclusively without double-counting" $ \(_, ls) →
            check ls
                [ "raw.nested()"
                , "local s = debug.getLuaCallStats()"
                , "assert(s.sequence == 2 and #s.verbs == 2)"
                , "assert(s.verbs[1].id == 'described.same' and s.verbs[1].count == 1)"
                , "assert(s.verbs[2].id == 'raw.nested' and s.verbs[2].count == 1)"
                , "assert(s.verbs[2].totalDurationNs >= s.verbs[1].totalDurationNs)" ]

        it "drops an unfinished pre-reset call while retaining new nested work" $ \(_, ls) →
            check ls
                [ "raw.resetNested()"
                , "local s = debug.getLuaCallStats()"
                , "assert(s.sequence == 2 and #s.verbs == 1)"
                , "assert(s.verbs[1].id == 'described.same' and s.verbs[1].count == 1)" ]

        it "also drops a pre-reset call that unwinds with an error" $ \(_, ls) →
            check ls
                [ "assert(not pcall(raw.resetError))"
                , "local s = debug.getLuaCallStats()"
                , "assert(not s.available and s.sequence == 1 and #s.verbs == 0)" ]

    it "records asynchronous unwinding and preserves cancellation" $ do
        stats ← newLuaCallStats
        -- Test the same guarded instrumented action without taking an async
        -- Haskell exception through Lua's C frames (the #2479 fixture pattern).
        outcome ← try @SomeAsyncException $ Lua.run @Lua.Exception $
            guardLuaAction "cancel" $ withLuaCallStats stats "raw.cancel" $
                Lua.liftIO (throwIO UserInterrupt)
        outcome `shouldSatisfy` isLeft
        snapshot ← readLuaCallStats stats
        callSequence snapshot `shouldBe` 1
        fmap callCount (Map.lookup "raw.cancel" (callVerbs snapshot)) `shouldBe` Just 1

    it "keeps separate runtimes independent" $ do
        first ← newLuaCallStats
        second ← newLuaCallStats
        Lua.run @Lua.Exception (withLuaCallStats first "raw.same" (pure ()))
        callSequence <$> readLuaCallStats first ≫= (`shouldBe` 1)
        readLuaCallStats second ≫= (`shouldBe` CallSnapshot 0 Map.empty)

    it "preserves the incoming cancellation mask while an action runs" $ do
        stats ← newLuaCallStats
        (before, during) ← Lua.run @Lua.Exception $ do
            before ← Lua.liftIO getMaskingState
            during ← withLuaCallStats stats "raw.masking" (Lua.liftIO getMaskingState)
            pure (before, during)
        during `shouldBe` before

    it "wires the full production API to one window, including the UI descriptor registrar" $
        withIsolatedResourceRoot $ withHeadlessEngineNoWorld $ \env → do
            backend ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                (assetPoolRef env) (nextObjectIdRef env) (inputStateRef env) (loggerRef env)
            stateRef ← newIORef ThreadRunning
            let ls = lbsLuaState backend
            (do
                registerLuaAPI ls env backend stateRef
                check ls
                    [ "debug.resetLuaCallStats()"
                    , "assert(not debug.getLuaCallStats().available)"
                    , "engine.getBootProfile(); camera.getPosition(); UI.getFocus()"
                    , "assert(not pcall(engine.debugThrow))"
                    , "local s = debug.getLuaCallStats(); local counts = {}"
                    , "for _, v in ipairs(s.verbs) do counts[v.id] = v.count end"
                    , "assert(#s.verbs == 4 and counts['engine.getBootProfile'] == 1)"
                    , "assert(counts['camera.getPosition'] == 1 and counts['UI.getFocus'] == 1)"
                    , "assert(counts['engine.debugThrow'] == 1)"
                    ]) `finally` Lua.close ls

withFixture ∷ ((LuaCallStats, Lua.State) → IO ()) → IO ()
withFixture action = bracket Lua.newstate Lua.close $ \ls → do
    callStats ← newLuaCallStats
    Lua.runWith ls $ do
        Lua.openlibs
        Lua.newtable
        registerLuaFunction callStats "debug" "getLuaCallStats" (getLuaCallStatsFn callStats)
        registerLuaFunction callStats "debug" "resetLuaCallStats" (resetLuaCallStatsFn callStats)
        Lua.setglobal "debug"
        Lua.newtable
        registerLuaFunction callStats "raw" "same" (pure 0)
        registerLuaFunction callStats "raw" "uncalled" (pure 0)
        registerLuaFunction callStats "raw" "luaError" (Lua.failLua "explicit Lua failure")
        registerLuaFunction callStats "raw" "haskellError" (Lua.liftIO (throwIO (userError "boom")))
        registerLuaFunction callStats "raw" "nested" (nested "described.same()")
        registerLuaFunction callStats "raw" "resetNested"
            (nested "debug.resetLuaCallStats(); described.same()")
        registerLuaFunction callStats "raw" "resetError" $ do
            _ ← nested "debug.resetLuaCallStats()"
            Lua.failLua "after reset"
        Lua.setglobal "raw"
        Lua.newtable
        _ ← registerLuaVerb callStats "described" (luaVerb "same" [] retNone "test") (pure 0)
        _ ← registerLuaVerb callStats "described" (luaVerb "luaError" [] retNone "test")
            (Lua.failLua "explicit Lua failure")
        _ ← registerLuaVerb callStats "described" (luaVerb "haskellError" [] retNone "test")
            (Lua.liftIO (throwIO (userError "boom")))
        Lua.setglobal "described"
    action (callStats, ls)
  where
    nested code = do
        status ← Lua.dostring code
        if status ≡ Lua.OK then pure 0 else Lua.error

check ∷ Lua.State → [Text] → Expectation
check ls source = executeDebugLua ls (T.unlines (source <> ["return true"]))
    ≫= (`shouldBe` "true")
