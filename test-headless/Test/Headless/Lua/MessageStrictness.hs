-- | The blocking gate for #622's fix (#2161).
--
--   'Engine.Scripting.Lua.Types' carries @{-\# LANGUAGE Strict, StrictData \#-}@
--   so every field of a 'LuaToEngineMsg' or 'LuaMsg' is forced when the
--   message is CONSTRUCTED, on the constructing thread. That is what keeps a
--   Haskell exception hiding in a field inside @registerLuaFunction@'s catch
--   guard, where it degrades to a caught Lua error: with lazy fields the
--   thunk would first be forced by whichever thread later consumed the
--   queued message, outside the guard, where the fail-stop handler kills
--   the whole process.
--
--   The real-engine probe that used to stand in for this gate sent
--   malformed UTF-8 through @engine.setText@ and graded process survival.
--   Since #618/#665 every decoder on that path is lenient, so its stimulus
--   could no longer throw and the probe passed with the pragma deleted
--   (TH-1 in @docs/coordinated_test_harness_findings.md@). These examples
--   need no engine and no Lua: each builds ONE message with a
--   distinguishable throwing thunk in exactly one 'Text' field, every other
--   argument total, and forces only the constructor itself. Deleting the
--   pragma makes both fail, because the construction then succeeds and the
--   thunk is never forced — the observation the probe could not make.
module Test.Headless.Lua.MessageStrictness (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (ErrorCall(..), evaluate, throw, try)
import Engine.Scene.Base (ObjectId(..))
import Engine.Scripting.Lua.Types (LuaMsg(..), LuaToEngineMsg(..))

spec ∷ Spec
spec = describe "Lua message field strictness" $ do
    it "LuaSetTextRequest forces its Text field at construction, in the \
       \caller, rather than on the engine thread that consumes it (#622)" $
        forcedAtConstruction "LuaSetTextRequest"
            (LuaSetTextRequest (ObjectId 1))

    it "LuaAssetFailed forces its Text field at construction, in the \
       \caller, rather than on the Lua thread that consumes it (#622)" $
        forcedAtConstruction "LuaAssetFailed"
            (LuaAssetFailed "texture" 7 "assets/textures/missing.png")

-- | Apply the constructor to a thunk that throws a tagged 'ErrorCall' when
--   forced, then force the RESULT to weak head normal form and nothing
--   more. Only a strict field makes that raise: a lazy one leaves the
--   thunk sitting in the message unevaluated and the construction
--   succeeds. The tag is checked verbatim so nothing but this thunk can
--   satisfy the example.
forcedAtConstruction ∷ String → (Text → μ) → Expectation
forcedAtConstruction name build = do
    let tag = "lua-strict-msg: forced " <> name <> "'s Text field"
        poison ∷ Text
        poison = throw (ErrorCall tag)
    outcome ← try (evaluate (build poison))
    case outcome of
        Left (ErrorCall got) → got `shouldBe` tag
        Right _ → expectationFailure $
            name <> " was constructed without forcing its Text field, so "
            <> "an exception carried there would first surface on the "
            <> "consuming thread, outside registerLuaFunction's catch guard"
