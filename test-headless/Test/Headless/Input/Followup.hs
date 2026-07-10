-- | #697 modifier-lifetime fence — the queue-relay half, driven
--   against the real headless EngineEnv with this test playing both
--   thread drain loops by hand (the harness starts neither the input
--   thread nor the Lua thread, so both queues belong to the test):
--
--     1. a shift-click sequence drained by the REAL 'processInputs'
--        leaves shift PUBLISHED AS HELD — the state every Lua callback
--        of that click will poll — because the release rides the
--        fence instead of the batch;
--     2. the input thread forwards the fence to the Lua queue BEHIND
--        the click's own broadcasts (FIFO order proves the callbacks
--        run first);
--     3. replaying the fence through the REAL dispatcher branch
--        ('processLuaMsg') re-injects the release, and the next input
--        drain publishes shift released — nothing sticks.
--
--   What this can't cover headless — an actual Lua callback observing
--   engine.isKeyDown mid-click through the real threads — is the
--   graphical half: tools/input_check.py's "shift-click observed
--   shift held in the callback".
--
--   #727 (below): sequential input.* ack overlap. A debug-console
--   command / a script's own back-to-back input.* calls can dispatch a
--   SECOND verb before the FIRST verb's callbacks and fence have been
--   dispatched (the recursive debug-command drain runs ahead of the
--   Lua-message drain — see 'Engine.Scripting.Lua.API.InputInject'\'s
--   haddock). The fix makes 'injectAndSettle' — what every input.*
--   verb's ack now runs through — synchronously resolve a fenced
--   verb's release (drain + dispatch its Lua messages, and if that
--   included the fence, wait for the re-injected release to drain too
--   and dispatch ITS broadcast) before returning, so by the time one
--   call returns, NOTHING belonging to it is left pending on either
--   queue for the next call to race. These specs drive the REAL
--   'injectAndSettle' (a background pump stands in for the un-started
--   input thread, mirroring the existing per-event fence tests above)
--   back to back, proving a second action can never observe or clip a
--   first action's temporary modifier.
module Test.Headless.Input.Followup (spec) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (finally)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Data.List (findIndex)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import qualified Engine.Core.Queue as Q
import Engine.Input.Inject (clickSequence, noMods)
import Engine.Input.Thread (processInputs)
import Engine.Input.Types
import Engine.Scripting.Lua.API.InputInject (injectAndSettle, SettleResult(..))
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Dispatch (processLuaMsg)
import Engine.Scripting.Lua.Types (LuaMsg(..), LuaBackendState)
import Test.Hspec

shiftMod ∷ ([GLFW.Key], GLFW.ModifierKeys)
shiftMod = ([GLFW.Key'LeftShift], noMods { GLFW.modifierKeysShift = True })

shiftState ∷ InputState → Maybe Bool
shiftState st = keyPressed ⊚ Map.lookup GLFW.Key'LeftShift (inpKeyStates st)

-- | Drain every queued LuaMsg (nothing else consumes this queue
--   headless; earlier specs may have left worldgen chatter behind).
drainLua ∷ EngineEnv → IO [LuaMsg]
drainLua env = go []
  where
    go acc = do
        m ← Q.tryReadQueue (luaQueue env)
        case m of
            Just msg → go (msg : acc)
            Nothing  → pure (reverse acc)

-- | Reset the input-side state this spec touches and give the click
--   path a non-degenerate viewport (headless boots with zero sizes,
--   which would route every press as swallowed).
resetInput ∷ EngineEnv → IO ()
resetInput env = do
    writeIORef (inputStateRef env) defaultInputState
    writeIORef (windowSizeRef env) (1280, 720)
    writeIORef (framebufferSizeRef env) (1280, 720)
    _ ← drainLua env
    pure ()

-- | One input-thread tick: drain the input queue through the real
--   'processInputs' (which also publishes to inputStateRef, #697).
inputTick ∷ EngineEnv → IO ()
inputTick env = do
    st ← readIORef (inputStateRef env)
    _ ← processInputs env st
    pure ()

-- | Stand in for the un-started real input thread (#727 specs below):
--   pumps the REAL 'processInputs' on a tight poll for the duration of
--   the action, so 'injectAndSettle's blocking waits on
--   'inputProcessedRef' — which nothing headless would otherwise ever
--   advance — resolve promptly instead of running out their full
--   timeout.
withFakeInputThread ∷ EngineEnv → IO α → IO α
withFakeInputThread env act = do
    stopRef ← newIORef False
    let pump = do
            stop ← readIORef stopRef
            unless stop $ do
                st ← readIORef (inputStateRef env)
                _ ← processInputs env st
                threadDelay 500
                pump
    tid ← forkIO pump
    act `finally` (writeIORef stopRef True ≫ killThread tid)

-- | A real, script-less Lua backend + thread-control ref — everything
--   'injectAndSettle' needs to drain and dispatch 'luaQueue' (#727).
--   Broadcasting to zero registered scripts is a safe no-op, so this
--   exercises the REAL dispatch path headless with no Lua script
--   loaded.
newTestLuaBackend ∷ EngineEnv → IO (LuaBackendState, IORef ThreadControl)
newTestLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    pure (ls, stateRef)

-- | Timeout for the #727 specs' 'injectAndSettle' calls — generous
--   relative to the 500us fake-pump poll interval above.
settleTimeoutMicros ∷ Int
settleTimeoutMicros = 5 * 1000 * 1000

isFollowupMsg ∷ LuaMsg → Bool
isFollowupMsg (LuaInjectFollowup _) = True
isFollowupMsg _                     = False

isMouseUpMsg ∷ LuaMsg → Bool
isMouseUpMsg (LuaMouseUpEvent _ _ _ _) = True
isMouseUpMsg _                         = False

spec ∷ SpecWith EngineEnv
spec = do
    it "shift-click publishes shift held through the batch, fence releases it after" $ \env → do
        resetInput env
        mapM_ (Q.writeQueue (inputQueue env))
              (clickSequence (100, 50) GLFW.MouseButton'1 shiftMod)
        inputTick env

        -- The whole primary batch (shift down, move, press, release,
        -- fence) is drained; the published state — what a Lua callback
        -- polling engine.isKeyDown sees — still has shift HELD.
        held ← readIORef (inputStateRef env)
        shiftState held `shouldBe` Just True

        -- The fence reached the Lua queue AFTER the click's own
        -- broadcasts: FIFO position is exactly what guarantees the
        -- callbacks run before the release is re-injected.
        msgs ← drainLua env
        length (filter isFollowupMsg msgs) `shouldBe` 1
        case (findIndex isMouseUpMsg msgs, findIndex isFollowupMsg msgs) of
            (Just up, Just fence) → up `shouldSatisfy` (< fence)
            other → expectationFailure $
                "expected mouse-up and fence messages, got indices "
                ⧺ show other ⧺ " in " ⧺ show msgs

        -- Replay the fence through the REAL dispatcher branch. It
        -- only forwards to the input queue — it must not need the Lua
        -- backend, which the headless harness never builds.
        case filter isFollowupMsg msgs of
            [fenceMsg] → processLuaMsg env
                (error "LuaInjectFollowup must not force LuaBackendState (#697)")
                (error "LuaInjectFollowup must not force ThreadControl (#697)")
                fenceMsg
            other → expectationFailure $
                "expected exactly one fence message, got " ⧺ show other
        inputTick env

        -- Release landed: shift reads up, via a RECORDED release
        -- (an entry flipped to unpressed, not a silently absent key —
        -- proves the release event was processed, not dropped).
        released ← readIORef (inputStateRef env)
        shiftState released `shouldBe` Just False

    it "plain click produces no fence message" $ \env → do
        resetInput env
        mapM_ (Q.writeQueue (inputQueue env))
              (clickSequence (100, 50) GLFW.MouseButton'1 ([], noMods))
        inputTick env
        msgs ← drainLua env
        filter isFollowupMsg msgs `shouldSatisfy` null

    -- #727: sequential input.* acks must be a real modifier-lifetime
    -- boundary — drives the REAL 'injectAndSettle' (what every input.*
    -- verb's ack now runs through) back to back, the same way a debug
    -- console command / a script's own consecutive calls would. No
    -- 'threadDelay' needed around the assertions below: 'injectAndSettle'
    -- waits on 'inputProcessedRef' (a count of FULLY processed events,
    -- not queue emptiness), so by the time it returns, everything it
    -- claims is genuinely settled — no eventual-consistency window to
    -- paper over.
    it "a shift-click settles before a later plain action can inherit its modifier (#727)" $ \env → do
        resetInput env
        (ls, stateRef) ← newTestLuaBackend env
        withFakeInputThread env $ do
            result1 ← injectAndSettle env ls stateRef settleTimeoutMicros
                (clickSequence (100, 50) GLFW.MouseButton'1 shiftMod)
            result1 `shouldBe` SettleOk

            -- The ack IS the boundary now: nothing left pending on
            -- either queue, and shift already reads released — a
            -- caller doesn't need to poll for it (unlike pre-#727).
            afterFirst ← drainLua env
            afterFirst `shouldSatisfy` null
            st1 ← readIORef (inputStateRef env)
            shiftState st1 `shouldBe` Just False

            -- A plain action issued right after the first ack must
            -- not inherit the prior verb's temporary modifier.
            result2 ← injectAndSettle env ls stateRef settleTimeoutMicros
                (clickSequence (200, 60) GLFW.MouseButton'1 ([], noMods))
            result2 `shouldBe` SettleOk
            afterSecond ← drainLua env
            afterSecond `shouldSatisfy` null
            st2 ← readIORef (inputStateRef env)
            shiftState st2 `shouldBe` Just False
            inpMousePos st2 `shouldBe` (200, 60)

    it "a shift-click settles before a later shift-click can be clipped by its cleanup (#727)" $ \env → do
        resetInput env
        (ls, stateRef) ← newTestLuaBackend env
        withFakeInputThread env $ do
            result1 ← injectAndSettle env ls stateRef settleTimeoutMicros
                (clickSequence (100, 50) GLFW.MouseButton'1 shiftMod)
            result1 `shouldBe` SettleOk
            st1 ← readIORef (inputStateRef env)
            shiftState st1 `shouldBe` Just False

            -- The second shift-click re-presses and independently
            -- releases the SAME modifier key: the first action's
            -- already-completed cleanup must not clip this one's own
            -- lifetime (it re-establishes "held" itself), and this
            -- one's own release must land exactly once — not stuck,
            -- not silently dropped.
            result2 ← injectAndSettle env ls stateRef settleTimeoutMicros
                (clickSequence (200, 60) GLFW.MouseButton'1 shiftMod)
            result2 `shouldBe` SettleOk
            afterSecond ← drainLua env
            afterSecond `shouldSatisfy` null
            st2 ← readIORef (inputStateRef env)
            shiftState st2 `shouldBe` Just False
