-- | #1927 split-hold modifier OWNERSHIP — the stateful half, driven
--   against the real headless 'EngineEnv' with this spec playing both
--   thread drain loops by hand (see
--   "Test.Headless.Input.InjectHarness").
--
--   'Test.Headless.Input.Inject' asserts what the sequence BUILDERS
--   emit; that is exactly the coverage PRR-1 slipped past, because a
--   split hold's modifier lifetime is not a property of either half's
--   event list — it is a property of the ownership record the input
--   thread keeps BETWEEN the two independent verb calls. So every
--   example here asserts published held STATE
--   ('Engine.Input.Types.keyHeld', what @engine.isKeyDown@ answers
--   with) or a real Lua callback's own reading of it, never a list
--   shape.
--
--   The two defects reproduced live in @docs/project_review_693-682.md@
--   PRR-1 are the first four examples, and both fail against the
--   pre-#1927 tree: an up half that omits the modifier list used to
--   leave the gesture's modifier held forever, and a gesture ending
--   used to release a modifier an independent owner still held.
module Test.Headless.Input.InjectOwnership (spec) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.UI.GLFW as GLFW
import qualified HsLua as Lua
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Types (EngineConfig(..))
import qualified Engine.Core.Queue as Q
import Engine.Input.Inject
    ( keyDownSequence, keyUpSequence, mouseDownSequence, mouseUpSequence
    , moveSequence, noMods )
import Engine.Input.Types
import Engine.Scripting.Lua.API.InputInject (injectAndSettle, SettleResult(..))
import Engine.Scripting.Lua.Thread.Dispatch (processLuaMsg)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Input.InjectHarness
import Test.Hspec

leftButton ∷ GLFW.MouseButton
leftButton = GLFW.MouseButton'1

shiftFlags ∷ GLFW.ModifierKeys
shiftFlags = snd shiftMod

-- | The event shape 'Engine.Input.Callback' emits for a real key
--   press/release — the "physical input" clause of the issue's
--   requirement 2 is exercised through this and nothing else, so a
--   spec cannot accidentally prove ownership only against a synthetic
--   owner it also created.
physicalKey ∷ GLFW.Key → GLFW.KeyState → GLFW.ModifierKeys → InputEvent
physicalKey = InputKeyEvent

-- | Push a sequence and drain it through the real input thread.
push ∷ EngineEnv → [InputEvent] → IO ()
push env evs = do
    mapM_ (Q.writeQueue (inputQueue env)) evs
    inputTick env

-- | Resolve every pending #697/#1927 fence the way the Lua thread
--   would: dispatch each 'LuaInjectFollowup' through the REAL
--   dispatcher branch (which only re-injects into the input queue),
--   then let the input thread process the re-injection. Returns how
--   many fences were dispatched, so a spec can assert that a
--   modifier-free hold produced none.
settleFences ∷ EngineEnv → IO Int
settleFences env = do
    msgs ← drainLua env
    let fences = filter isFollowupMsg msgs
    mapM_ (processLuaMsg env
              (error "LuaInjectFollowup must not force LuaBackendState")
              (error "LuaInjectFollowup must not force ThreadControl"))
          fences
    inputTick env
    pure (length fences)

held ∷ EngineEnv → GLFW.Key → IO Bool
held env key = flip keyHeld key ⊚ readIORef (inputStateRef env)

-- | The harness env with injection ENABLED. The input.* verbs refuse
--   to run at all under a headless config, so the Lua-boundary
--   argument contract (#1927 requirement 5) is unreachable through the
--   real verb otherwise. Only the config differs: every queue, ref and
--   piece of state the verb touches is the harness's own live env.
withInjectionEnabled ∷ EngineEnv → EngineEnv
withInjectionEnabled env =
    env { engineConfig = (engineConfig env) { ecHeadless = False } }

-- | Run one line of Lua against the backend and return the @error@
--   field of the ack table it produced ('Nothing' when the ack has no
--   error — i.e. the call was accepted).
callVerbError ∷ Lua.State → String → IO (Maybe Text)
callVerbError st src = Lua.runWith st $ do
    status ← Lua.dostring (TE.encodeUtf8 (T.pack src))
    if status ≢ Lua.OK
        then do
            msg ← Lua.tostring (-1)
            Lua.pop 1
            pure (Just ("lua error: " <> maybe "?" TE.decodeUtf8Lenient msg))
        else do
            ty ← Lua.getfield (-1) (Lua.Name "error")
                    ∷ Lua.LuaE Lua.Exception Lua.Type
            result ← if ty ≡ Lua.TypeString
                then fmap TE.decodeUtf8Lenient ⊚ Lua.tostring (-1)
                else pure Nothing
            Lua.pop 2
            pure result

spec ∷ SpecWith EngineEnv
spec = do
    -- PRR-1 defect 1, key half: the leak. Pre-#1927,
    -- keyUpSequence W ([], noMods) was exactly one event — the W
    -- release — so shift stayed published held with no owner and no
    -- way to clear it.
    it "a key split hold releases its own modifier when the up half names none" $ \env → do
        resetInput env
        push env (keyDownSequence GLFW.Key'W shiftMod)
        held env GLFW.Key'W `shouldReturn` True
        held env GLFW.Key'LeftShift `shouldReturn` True

        -- Requirement 4: the halves straddle intervening cursor moves
        -- and further input-thread ticks, exactly like a real drag.
        push env (moveSequence (140, 90))
        push env (moveSequence (200, 120))
        held env GLFW.Key'LeftShift `shouldReturn` True

        push env (keyUpSequence GLFW.Key'W)
        -- #697 is unchanged for split holds: the modifier is still
        -- held while the primary release's own callbacks are pending,
        -- and only the FENCE clears it.
        held env GLFW.Key'W `shouldReturn` False
        held env GLFW.Key'LeftShift `shouldReturn` True
        settleFences env `shouldReturn` 1
        held env GLFW.Key'LeftShift `shouldReturn` False

    -- PRR-1 defect 1, mouse half.
    it "a mouse split hold releases its own modifier when the up half names none" $ \env → do
        resetInput env
        push env (mouseDownSequence (100, 50) leftButton shiftMod)
        held env GLFW.Key'LeftShift `shouldReturn` True
        push env (moveSequence (180, 110))
        push env (mouseUpSequence (220, 130) leftButton)
        held env GLFW.Key'LeftShift `shouldReturn` True
        settleFences env `shouldReturn` 1
        held env GLFW.Key'LeftShift `shouldReturn` False
        st ← readIORef (inputStateRef env)
        inpMousePos st `shouldBe` (220, 130)

    -- PRR-1 defect 2, key half, through the PHYSICAL event path: the
    -- over-release. Pre-#1927 the gesture's unconditional modifier
    -- release dropped this independent hold; the independent owner's
    -- own release then had nothing left to clear.
    it "a key split hold leaves a physically held modifier held, and its own release still clears it" $ \env → do
        resetInput env
        push env [physicalKey GLFW.Key'LeftShift GLFW.KeyState'Pressed shiftFlags]
        held env GLFW.Key'LeftShift `shouldReturn` True

        push env (keyDownSequence GLFW.Key'W shiftMod)
        push env (keyUpSequence GLFW.Key'W)
        _ ← settleFences env
        held env GLFW.Key'W `shouldReturn` False
        held env GLFW.Key'LeftShift `shouldReturn` True

        -- The independent owner is genuinely still there, not merely
        -- un-cleared bookkeeping: its own release ends the hold.
        push env [physicalKey GLFW.Key'LeftShift GLFW.KeyState'Released noMods]
        held env GLFW.Key'LeftShift `shouldReturn` False

    -- PRR-1 defect 2, mouse half.
    it "a mouse split hold leaves a physically held modifier held, and its own release still clears it" $ \env → do
        resetInput env
        push env [physicalKey GLFW.Key'LeftShift GLFW.KeyState'Pressed shiftFlags]
        push env (mouseDownSequence (100, 50) leftButton shiftMod)
        push env (mouseUpSequence (100, 50) leftButton)
        _ ← settleFences env
        held env GLFW.Key'LeftShift `shouldReturn` True
        push env [physicalKey GLFW.Key'LeftShift GLFW.KeyState'Released noMods]
        held env GLFW.Key'LeftShift `shouldReturn` False

    -- The ownership-transfer case, which a snapshot of "what was held
    -- before the gesture" cannot satisfy: the independent owner lets
    -- go WHILE the gesture is still running. The gesture must keep the
    -- modifier held on its own account until its own up half — and
    -- must then actually release it, since nothing else holds it any
    -- more.
    it "a split hold keeps its modifier held after the independent owner releases it mid-gesture" $ \env → do
        resetInput env
        push env [physicalKey GLFW.Key'LeftShift GLFW.KeyState'Pressed shiftFlags]
        push env (mouseDownSequence (100, 50) leftButton shiftMod)
        held env GLFW.Key'LeftShift `shouldReturn` True

        push env [physicalKey GLFW.Key'LeftShift GLFW.KeyState'Released noMods]
        held env GLFW.Key'LeftShift `shouldReturn` True
        push env (moveSequence (160, 80))
        held env GLFW.Key'LeftShift `shouldReturn` True

        push env (mouseUpSequence (160, 80) leftButton)
        _ ← settleFences env
        held env GLFW.Key'LeftShift `shouldReturn` False

    -- The mirror image: the independent owner ARRIVES during the
    -- gesture. Ending the gesture must not release what it did not
    -- introduce, so a snapshot taken at the down half is equally wrong
    -- in this direction.
    it "a split hold does not release a modifier an independent owner acquired mid-gesture" $ \env → do
        resetInput env
        push env (keyDownSequence GLFW.Key'W shiftMod)
        push env [physicalKey GLFW.Key'LeftShift GLFW.KeyState'Pressed shiftFlags]
        push env (keyUpSequence GLFW.Key'W)
        _ ← settleFences env
        held env GLFW.Key'LeftShift `shouldReturn` True
        push env [physicalKey GLFW.Key'LeftShift GLFW.KeyState'Released noMods]
        held env GLFW.Key'LeftShift `shouldReturn` False

    it "two overlapping split holds each own the same modifier independently" $ \env → do
        resetInput env
        push env (keyDownSequence GLFW.Key'W shiftMod)
        push env (mouseDownSequence (100, 50) leftButton shiftMod)
        push env (keyUpSequence GLFW.Key'W)
        _ ← settleFences env
        held env GLFW.Key'LeftShift `shouldReturn` True
        push env (mouseUpSequence (100, 50) leftButton)
        _ ← settleFences env
        held env GLFW.Key'LeftShift `shouldReturn` False

    it "a modifier-free split hold claims nothing and fences nothing" $ \env → do
        resetInput env
        push env (keyDownSequence GLFW.Key'W ([], noMods))
        push env (keyUpSequence GLFW.Key'W)
        settleFences env `shouldReturn` 0
        push env (mouseDownSequence (100, 50) leftButton ([], noMods))
        push env (mouseUpSequence (100, 50) leftButton)
        settleFences env `shouldReturn` 0
        held env GLFW.Key'W `shouldReturn` False

    it "focus loss drops a split hold's outstanding modifier claim" $ \env → do
        resetInput env
        push env (keyDownSequence GLFW.Key'W shiftMod)
        held env GLFW.Key'LeftShift `shouldReturn` True
        push env [InputWindowEvent (WindowFocus False)]
        -- The OS delivers no release for a synthetic claim either, and
        -- the up half may never arrive: a claim that outlived this
        -- would be held forever.
        held env GLFW.Key'LeftShift `shouldReturn` False
        _ ← drainLua env
        push env [InputWindowEvent (WindowFocus True)]

    -- Callback-level (#697's contract, preserved for split holds):
    -- driven through the REAL 'injectAndSettle' with a REAL loaded
    -- script, so these read what an actual Lua callback observed via
    -- engine.isKeyDown, not a proxy for it.
    it "a key split hold's own release callback observes its modifier held, and the ack leaves nothing held" $ \env → do
        resetInput env
        (ls, stateRef, fixtureRef) ← newTestLuaBackend env
        withFakeInputThread env $ do
            r1 ← injectAndSettle env ls stateRef settleTimeoutMicros
                     (keyDownSequence GLFW.Key'W shiftMod)
            r1 `shouldBe` SettleOk
            readFixtureBoolIn ls fixtureRef "keyDownShiftBy" "W"
                `shouldReturn` Just True

            r2 ← injectAndSettle env ls stateRef settleTimeoutMicros
                     (keyUpSequence GLFW.Key'W)
            r2 `shouldBe` SettleOk
            -- The primary release's OWN callback still saw shift held…
            readFixtureBoolIn ls fixtureRef "keyUpShiftBy" "W"
                `shouldReturn` Just True
            -- …and the fenced modifier release ran afterwards, its own
            -- callback reading the post-release state.
            readFixtureBoolIn ls fixtureRef "keyUpShiftBy" "Shift"
                `shouldReturn` Just False
            held env GLFW.Key'LeftShift `shouldReturn` False
            (drainLua env ⌦ (`shouldSatisfy` null))

    it "a mouse split hold's own release callback observes its modifier held, and the ack leaves nothing held" $ \env → do
        resetInput env
        (ls, stateRef, fixtureRef) ← newTestLuaBackend env
        withFakeInputThread env $ do
            r1 ← injectAndSettle env ls stateRef settleTimeoutMicros
                     (mouseDownSequence (100, 50) leftButton shiftMod)
            r1 `shouldBe` SettleOk
            readFixtureBool ls fixtureRef "mouseDownShift"
                `shouldReturn` Just True

            r2 ← injectAndSettle env ls stateRef settleTimeoutMicros
                     (mouseUpSequence (100, 50) leftButton)
            r2 `shouldBe` SettleOk
            readFixtureBool ls fixtureRef "mouseUpShift"
                `shouldReturn` Just True
            held env GLFW.Key'LeftShift `shouldReturn` False
            (drainLua env ⌦ (`shouldSatisfy` null))

    it "a split hold nested inside an independent modifier hold leaves that hold intact across the ack" $ \env → do
        resetInput env
        (ls, stateRef, fixtureRef) ← newTestLuaBackend env
        withFakeInputThread env $ do
            -- The independent owner is itself a raw synthetic modifier
            -- hold — the alternative tools/preview_probe.py already
            -- uses — so this covers the synthetic owner class the
            -- physical-path examples above cannot.
            outer ← injectAndSettle env ls stateRef settleTimeoutMicros
                        (keyDownSequence GLFW.Key'LeftShift ([], noMods))
            outer `shouldBe` SettleOk
            held env GLFW.Key'LeftShift `shouldReturn` True

            inner1 ← injectAndSettle env ls stateRef settleTimeoutMicros
                         (mouseDownSequence (100, 50) leftButton shiftMod)
            inner1 `shouldBe` SettleOk
            inner2 ← injectAndSettle env ls stateRef settleTimeoutMicros
                         (mouseUpSequence (100, 50) leftButton)
            inner2 `shouldBe` SettleOk
            readFixtureBool ls fixtureRef "mouseUpShift"
                `shouldReturn` Just True
            held env GLFW.Key'LeftShift `shouldReturn` True

            release ← injectAndSettle env ls stateRef settleTimeoutMicros
                          (keyUpSequence GLFW.Key'LeftShift)
            release `shouldBe` SettleOk
            held env GLFW.Key'LeftShift `shouldReturn` False

    -- Requirement 5 at the Lua boundary: the release verbs no longer
    -- accept a modifier list, and say so — naming the verb that does —
    -- rather than accepting and ignoring it.
    it "input.keyUp and input.mouseUp reject a mods argument, naming the down half" $ \env → do
        resetInput env
        (ls, _stateRef, _ref) ←
            newTestLuaBackendWith (withInjectionEnabled env) env
        keyErr ← callVerbError (lbsLuaState ls)
                     "return input.keyUp(\"W\", {\"shift\"})"
        case keyErr of
            Nothing → expectationFailure
                "input.keyUp accepted a mods argument"
            Just msg → do
                msg `shouldSatisfy` T.isInfixOf "input.keyUp"
                msg `shouldSatisfy` T.isInfixOf "input.keyDown(name, mods)"
        mouseErr ← callVerbError (lbsLuaState ls)
                       "return input.mouseUp(10, 20, \"left\", {\"shift\"})"
        case mouseErr of
            Nothing → expectationFailure
                "input.mouseUp accepted a mods argument"
            Just msg → do
                msg `shouldSatisfy` T.isInfixOf "input.mouseUp"
                msg `shouldSatisfy`
                    T.isInfixOf "input.mouseDown(x, y, button, mods)"
        -- Nothing was injected: a rejected argument must not half-run
        -- the verb.
        (drainLua env ⌦ (`shouldSatisfy` null))

    it "the release verbs still accept their own arguments" $ \env → do
        resetInput env
        (ls, _stateRef, _ref) ←
            newTestLuaBackendWith (withInjectionEnabled env) env
        withFakeInputThread env $ do
            downOk ← callVerbError (lbsLuaState ls)
                         "return input.keyDown(\"W\", {\"shift\"})"
            downOk `shouldBe` Nothing
            upOk ← callVerbError (lbsLuaState ls) "return input.keyUp(\"W\")"
            upOk `shouldBe` Nothing
            held env GLFW.Key'W `shouldReturn` False
            held env GLFW.Key'LeftShift `shouldReturn` False
            mouseDownOk ← callVerbError (lbsLuaState ls)
                              "return input.mouseDown(10, 20, \"left\", {\"shift\"})"
            mouseDownOk `shouldBe` Nothing
            mouseUpOk ← callVerbError (lbsLuaState ls)
                            "return input.mouseUp(10, 20, \"left\")"
            mouseUpOk `shouldBe` Nothing
            held env GLFW.Key'LeftShift `shouldReturn` False
        _ ← drainLua env
        _ ← settleFences env
        pure ()
