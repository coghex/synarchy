{-# LANGUAGE Strict #-}
module Engine.Input.Thread where

import UPrelude
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent (threadDelay, ThreadId, killThread, forkIO)
import Control.Exception (SomeException, catch)
import Control.Monad.Logger (Loc(..), LogLevel(..), toLogStr, defaultLoc)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Engine.Core.State
import Engine.Core.Thread
import Engine.Input.Types
import Engine.Input.Callback
import Engine.Input.Bindings
import Engine.Scripting.Lua.Types
import qualified Engine.Core.Queue as Q

-- | Start the input processing thread
startInputThread ∷ EngineEnv → IO ThreadState
startInputThread env = do
    let inputSRef     = inputStateRef env
        apRef         = assetPoolRef env
        objIdRef      = nextObjectIdRef env
    stateRef ← newIORef ThreadRunning
    threadId ← catch 
        (do
            let lf = logFunc env
            lf defaultLoc "input" LevelInfo "Starting input thread..."
            tid ← forkIO $ runInputLoop env stateRef
            return tid
        ) 
        (\(e :: SomeException) → do
            let lf = logFunc env
            lf defaultLoc "input" LevelError $ "Failed to start input thread."
                                           <> (toLogStr $ show e)
            error "Input thread failed to start"
        )
    return $ ThreadState stateRef threadId

-- | Main input processing loop with timing control
runInputLoop ∷ EngineEnv → IORef ThreadControl → IO ()
  -- read control state
runInputLoop env stateRef = do
  control ← readIORef stateRef
  case control of
    ThreadStopped → do
        let lf = logFunc env
        lf defaultLoc "input" LevelInfo "Input thread stopping..."
        pure ()
    ThreadPaused  → do
        threadDelay 100000  -- 100ms pause check
        runInputLoop env stateRef
    ThreadRunning → do
        -- Start frame timing
        frameStart ← getCurrentTime
        
        -- Process all pending inputs
        let sharedInputRef = inputStateRef env
        inpSt ← readIORef sharedInputRef
        newInpSt ← processInputs env inpSt

        -- write to the shared state
        writeIORef sharedInputRef newInpSt
        
        -- Calculate frame time and delay to maintain consistent rate
        frameEnd ← getCurrentTime
        let diff  = diffUTCTime frameEnd frameStart
            usecs = floor (toRational diff * 1000000) ∷ Int
            targetFrameTime = 1000  -- 1ms target frame time
            delay = targetFrameTime - usecs
        
        -- Only delay if we're running faster than target
        when (delay > 0) $ threadDelay delay
        
        -- Continue loop
        runInputLoop env stateRef

-- | Process all queued inputs
processInputs ∷ EngineEnv → InputState → IO InputState
processInputs env inpSt = do
    mEvent ← Q.tryReadQueue (inputQueue env)
    case mEvent of
        Just event → do
            newState ← processInput env inpSt event
            processInputs env newState
        Nothing → return inpSt

-- | Process individual input events
processInput ∷ EngineEnv → InputState → InputEvent → IO InputState
processInput env inpSt event = case event of
    InputKeyEvent glfwKey keyState mods → do
        let key = fromGLFWKey glfwKey
        -- get current key bindings
        bindings ← readIORef (keyBindingsRef env)
        -- check for special key combinations
        case getKeyForAction "escape" bindings of
          Just escapeKeyName →
            when (Just key ≡ textToKey escapeKeyName
                  && keyState ≡ GLFW.KeyState'Pressed) $ do
                writeIORef (lifecycleRef env) CleaningUp
          Nothing → return ()
        -- send key events to lua
        let lq = luaQueue env
        when (keyState ≡ GLFW.KeyState'Pressed) $
            Q.writeQueue lq (LuaKeyDownEvent key)
        when (keyState ≡ GLFW.KeyState'Released) $
            Q.writeQueue lq (LuaKeyUpEvent key)
        return $ updateKeyState inpSt glfwKey keyState mods
    InputMouseEvent btn pos state → do
        -- send mouse events to lua
        let lq = luaQueue env
            (x, y) = pos
        when (state ≡ GLFW.MouseButtonState'Pressed) $
            Q.writeQueue lq (LuaMouseDownEvent btn x y)
        when (state ≡ GLFW.MouseButtonState'Released) $
            Q.writeQueue lq (LuaMouseUpEvent btn x y)
        return $ updateMouseState inpSt btn pos state
    InputCursorMove x y → 
        return $ inpSt { inpMousePos = (x, y) }
    InputScrollEvent x y →
        return $ updateScrollState inpSt x y
    InputWindowEvent winEv → do
        case winEv of
          WindowResize w h → do
            let lf = logFunc env
            lf defaultLoc "input" LevelInfo $ "Window resized to: " <> (toLogStr $ show (w, h))
            -- TODO: Trigger swapchain recreation or other necessary updates
          _ → return ()
        return $ updateWindowState inpSt winEv

-- | Helper state update functions
updateKeyState ∷ InputState → GLFW.Key → GLFW.KeyState → GLFW.ModifierKeys → InputState
updateKeyState state key keyState mods = state
    { inpKeyStates = Map.insert key newKeyState (inpKeyStates state) }
    where
        newKeyState = KeyState
            { keyPressed = keyState ≡ GLFW.KeyState'Pressed
            , keyMods = mods
            , keyTime = 0.0
            }

updateMouseState ∷ InputState → GLFW.MouseButton → (Double, Double) → GLFW.MouseButtonState → InputState
updateMouseState state btn pos btnState = state
    { inpMousePos = pos
    , inpMouseBtns = Map.insert btn (btnState ≡ GLFW.MouseButtonState'Pressed) (inpMouseBtns state)
    }

updateWindowState ∷ InputState → WindowEvent → InputState
updateWindowState state (WindowResize w h) = state { inpWindowSize = (w, h) }
updateWindowState state (WindowFocus focused) = state { inpWindowFocused = focused }
updateWindowState state _ = state

updateScrollState ∷ InputState → Double → Double → InputState
updateScrollState state x y = state { inpScrollDelta = (x, y) }
