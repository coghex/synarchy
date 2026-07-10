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
module Test.Headless.Input.Followup (spec) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Graphics.UI.GLFW as GLFW
import Data.IORef (readIORef, writeIORef)
import Data.List (findIndex)
import Engine.Core.State (EngineEnv(..))
import qualified Engine.Core.Queue as Q
import Engine.Input.Inject (clickSequence, noMods)
import Engine.Input.Thread (processInputs)
import Engine.Input.Types
import Engine.Scripting.Lua.Thread.Dispatch (processLuaMsg)
import Engine.Scripting.Lua.Types (LuaMsg(..))
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
