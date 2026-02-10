{-# LANGUAGE Strict #-}
module Engine.Input.Thread where

import UPrelude
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent (threadDelay, ThreadId, killThread, forkIO)
import Control.Exception (SomeException, catch)
import Data.IORef (IORef, newIORef, writeIORef, readIORef, atomicModifyIORef')
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Engine.Core.Log (logDebug, logError, logWarn, logInfo, LogCategory(..))
import Engine.Core.State
import Engine.Core.Thread
import Engine.Core.Error.Exception (SystemError(..), ExceptionType(..))
import Engine.Event.Types
import Engine.Input.Types
import Engine.Input.Callback
import Engine.Input.Bindings
import Engine.Scripting.Lua.Types
import Engine.Graphics.Window.Types (Window(..))
import qualified Engine.Core.Queue as Q
import UI.Manager (findClickableElementAt)
import UI.Types (ElementHandle(..), UIPageManager(..), upmGlobalFocus)
import UI.Focus (FocusManager, getInputMode, InputMode(..), clearFocus
                , FocusId(..), fmCurrentFocus)

-- | Start the input processing thread
startInputThread ∷ EngineEnv → IO ThreadState
startInputThread env = do
    let inputSRef     = inputStateRef env
        apRef         = assetPoolRef env
        objIdRef      = nextObjectIdRef env
        logRef        = loggerRef env
    logger ← readIORef logRef
    stateRef ← newIORef ThreadRunning
    threadId ← catch 
        (do
            logInfo logger CatInput "Starting input thread..."
            tid ← forkIO $ runInputLoop env stateRef
            return tid
        ) 
        (\(e :: SomeException) → do
            logError logger CatInput $ "Failed starting input thread: " <> T.pack (show e)
            Q.writeQueue (eventQueue env) $ EventError
              "startInputThread:" $ T.pack (show e)
            error "Input thread start failure."
        )
    return $ ThreadState stateRef threadId

-- | Main input processing loop with timing control
runInputLoop ∷ EngineEnv → IORef ThreadControl → IO ()
  -- read control state
runInputLoop env stateRef = do
  control ← readIORef stateRef
  logger ← readIORef (loggerRef env)
  case control of
    ThreadStopped → do
        logDebug logger CatInput "Input thread stopping..."
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

        threadDelay 16666  -- Fixed delay for ~60 FPS
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
        focusMgr ← readIORef (focusManagerRef env)
        uiMgr ← readIORef (uiManagerRef env)
        let shellMode = getInputMode focusMgr
            uiFocus = upmGlobalFocus uiMgr
            key = fromGLFWKey glfwKey
        case (shellMode, uiFocus) of
          (TextInputMode (FocusId fid),_) → do
            logger ← readIORef (loggerRef env)
            logDebug logger CatInput $ "Input mode: ShellTextInput, focusId=" <> T.pack (show fid)
            when (key ≡ KeyGrave ∧ keyState ≡ GLFW.KeyState'Pressed) $ do
                Q.writeQueue (luaQueue env) LuaShellToggle
            when (key ≡ KeyEscape ∧ keyState ≡ GLFW.KeyState'Pressed) $ do
                Q.writeQueue (luaQueue env) $ LuaFocusLost fid
            when (key ≡ KeyBackspace ∧ isKeyDown keyState) $
                Q.writeQueue (luaQueue env) (LuaTextBackspace fid)
            when (key ≡ KeyEnter ∧ keyState ≡ GLFW.KeyState'Pressed) $
                Q.writeQueue (luaQueue env) (LuaTextSubmit fid)
            when (key ≡ KeyTab ∧ keyState ≡ GLFW.KeyState'Pressed) $
                Q.writeQueue (luaQueue env) (LuaTabPressed fid)
            when (key ≡ KeyUp ∧ keyState ≡ GLFW.KeyState'Pressed) $
                Q.writeQueue (luaQueue env) (LuaCursorUp fid)
            when (key ≡ KeyDown ∧ keyState ≡ GLFW.KeyState'Pressed) $
                Q.writeQueue (luaQueue env) (LuaCursorDown fid)
            when (key ≡ KeyLeft ∧ isKeyDown keyState) $
                Q.writeQueue (luaQueue env) (LuaCursorLeft fid)
            when (key ≡ KeyRight ∧ isKeyDown keyState) $
                Q.writeQueue (luaQueue env) (LuaCursorRight fid)
            when ((key ≡ KeyHome ∧ keyState ≡ GLFW.KeyState'Pressed)
                 ∨ (key ≡ KeyA ∧ keyState ≡ GLFW.KeyState'Pressed
                   ∧ (GLFW.modifierKeysControl mods))) $
                Q.writeQueue (luaQueue env) (LuaCursorHome fid)
            when ((key ≡ KeyEnd ∧ keyState ≡ GLFW.KeyState'Pressed)
                 ∨ (key ≡ KeyE ∧ keyState ≡ GLFW.KeyState'Pressed
                   ∧ (GLFW.modifierKeysControl mods))) $
                Q.writeQueue (luaQueue env) (LuaCursorEnd fid)
            when (key ≡ KeyDelete ∧ isKeyDown keyState) $
                Q.writeQueue (luaQueue env) (LuaTextDelete fid)
            when (key ≡ KeyC ∧ keyState ≡ GLFW.KeyState'Pressed
                   ∧ (GLFW.modifierKeysControl mods)) $
                Q.writeQueue (luaQueue env) (LuaInterrupt fid)
            return ()

         -- UI element has focus (no shell focus)
          (GameInputMode, Just (ElementHandle elemId)) → do
            logger ← readIORef (loggerRef env)
            logDebug logger CatInput $ "Input mode: UITextInput, elementId=" <> T.pack (show elemId)
            -- Grave key always toggles shell
            when (key ≡ KeyGrave ∧ keyState ≡ GLFW.KeyState'Pressed) $
                Q.writeQueue (luaQueue env) LuaShellToggle
            -- Escape unfocuses UI element
            when (key ≡ KeyEscape ∧ keyState ≡ GLFW.KeyState'Pressed) $
                Q.writeQueue (luaQueue env) LuaUIEscape
            -- Text input keys
            when (key ≡ KeyBackspace ∧ isKeyDown keyState) $
                Q.writeQueue (luaQueue env) LuaUIBackspace
            when (key ≡ KeyEnter ∧ keyState ≡ GLFW.KeyState'Pressed) $
                Q.writeQueue (luaQueue env) LuaUISubmit
            when (key ≡ KeyLeft ∧ isKeyDown keyState) $
                Q.writeQueue (luaQueue env) LuaUICursorLeft
            when (key ≡ KeyRight ∧ isKeyDown keyState) $
                Q.writeQueue (luaQueue env) LuaUICursorRight
            when ((key ≡ KeyHome ∧ keyState ≡ GLFW.KeyState'Pressed)
                 ∨ (key ≡ KeyA ∧ keyState ≡ GLFW.KeyState'Pressed
                   ∧ (GLFW.modifierKeysControl mods))) $
                Q.writeQueue (luaQueue env) LuaUIHome
            when ((key ≡ KeyEnd ∧ keyState ≡ GLFW.KeyState'Pressed)
                 ∨ (key ≡ KeyE ∧ keyState ≡ GLFW.KeyState'Pressed
                   ∧ (GLFW.modifierKeysControl mods))) $
                Q.writeQueue (luaQueue env) LuaUIEnd
            when (key ≡ KeyDelete ∧ isKeyDown keyState) $
                Q.writeQueue (luaQueue env) LuaUIDelete
            return ()
          
          -- Game input mode (no focus)
          (GameInputMode, Nothing) → do
            logger ← readIORef (loggerRef env)
            logDebug logger CatInput $ "Input mode: GameInputMode, key=" <> T.pack (show key)
            when (key ≡ KeyGrave ∧ keyState ≡ GLFW.KeyState'Pressed) $
                Q.writeQueue (luaQueue env) LuaShellToggle
            when (key ≠ KeyGrave) $ do
                let lq = luaQueue env
                when (keyState ≡ GLFW.KeyState'Pressed) $
                    Q.writeQueue lq (LuaKeyDownEvent key)
                when (keyState ≡ GLFW.KeyState'Released) $
                    Q.writeQueue lq (LuaKeyUpEvent key)
            -- get current key bindings
            bindings ← readIORef (keyBindingsRef env)
            -- check for special key combinations
            case getKeyForAction "escape" bindings of
              Just escapeKeyName → do
                logDebug logger CatInput $ "Key binding: action=escape, key=" <> escapeKeyName
                when (Just key ≡ textToKey escapeKeyName
                      ∧ keyState ≡ GLFW.KeyState'Pressed) $ do
                    logDebug logger CatInput "Action triggered: escape"
                    writeIORef (lifecycleRef env) CleaningUp
              Nothing → return ()

        return $ updateKeyState inpSt glfwKey keyState mods

    InputCharEvent c → do
        when (c ≠ '`') $ do
          focusMgr ← readIORef (focusManagerRef env)
          uiMgr ← readIORef (uiManagerRef env)
          
          case (fmCurrentFocus focusMgr, upmGlobalFocus uiMgr) of
            -- Shell has focus - send to shell
            (Just (FocusId fid), _) →
              Q.writeQueue (luaQueue env) (LuaCharInput fid c)
            -- UI element has focus - send to UI
            (Nothing, Just _elemHandle) →
              Q.writeQueue (luaQueue env) (LuaUICharInput c)
            -- No focus - ignore
            (Nothing, Nothing) → return ()
        return inpSt
    InputMouseEvent btn pos state → do
        let lq = luaQueue env
            (x, y) = pos
        logger ← readIORef (loggerRef env)
        
        when (state ≡ GLFW.MouseButtonState'Pressed) $ do
            logDebug logger CatInput $ "Mouse button pressed: button=" <> T.pack (show btn)
                                    <> ", pos=(" <> T.pack (show x) <> "," <> T.pack (show y) <> ")"
            
            -- Get window and framebuffer sizes from InputState
            (winW, winH) ← readIORef (windowSizeRef env)
            (fbW, fbH) ← readIORef (framebufferSizeRef env)
            
            -- Calculate scale factors
            let scaleX = fromIntegral fbW / fromIntegral winW
                scaleY = fromIntegral fbH / fromIntegral winH
                mouseX = realToFrac x * scaleX
                mouseY = realToFrac y * scaleY
            
            logDebug logger CatUI $ "Click at (" <> T.pack (show mouseX) <> ", " <> T.pack (show mouseY) <> ")"
            logDebug logger CatInput $ "Converted mouse pos: (" <> T.pack (show mouseX) 
                                    <> ", " <> T.pack (show mouseY) <> ")"
            logDebug logger CatInput $ "Window: " <> T.pack (show winW) <> "x" <> T.pack (show winH)
                                    <> ", Framebuffer: " <> T.pack (show fbW) <> "x" <> T.pack (show fbH)
            
            -- Check for UI element clicks with converted coordinates
            uiMgr ← readIORef (uiManagerRef env)
            case findClickableElementAt (mouseX, mouseY) uiMgr of
                Just (elemHandle, callback) → do
                    Q.writeQueue lq (LuaUIClickEvent elemHandle callback)
                    logDebug logger CatUI $ "UI element clicked: " <> callback
                Nothing → do
                    -- Clicked outside UI - clear UI focus
                    Q.writeQueue lq LuaUIFocusLost
                    Q.writeQueue lq (LuaMouseDownEvent btn x y)
        
        when (state ≡ GLFW.MouseButtonState'Released) $ do
            logDebug logger CatInput $ "Mouse button released: button=" <> T.pack (show btn)
                                    <> ", pos=(" <> T.pack (show x) <> "," <> T.pack (show y) <> ")"
            Q.writeQueue lq (LuaMouseUpEvent btn x y)
        
        return $ updateMouseState inpSt btn pos state
    InputCursorMove x y → 
        return $ inpSt { inpMousePos = (x, y) }
    InputScrollEvent x y → do
        logger ← readIORef (loggerRef env)
        logDebug logger CatInput $ "Scroll event: dx=" <> T.pack (show x) <> ", dy=" <> T.pack (show y)
        
        -- Check modifier keys for shift+scroll (z-slice)
        inpSt' ← readIORef (inputStateRef env)
        let shiftHeld = case Map.lookup GLFW.Key'LeftShift (inpKeyStates inpSt') of
                Just ks → keyPressed ks
                Nothing → case Map.lookup GLFW.Key'RightShift (inpKeyStates inpSt') of
                    Just ks → keyPressed ks
                    Nothing → False
        
        if shiftHeld
        then do
            -- Shift+scroll → z-slice adjustment
            logDebug logger CatInput "Shift+scroll: z-slice adjustment"
            Q.writeQueue (luaQueue env) (LuaZSliceScroll x y)
        else do
            -- Check if UI element is under cursor
            (winW, winH) ← readIORef (windowSizeRef env)
            (fbW, fbH) ← readIORef (framebufferSizeRef env)
            let (rawX, rawY) = inpMousePos inpSt
                scaleX = fromIntegral fbW / fromIntegral winW
                scaleY = fromIntegral fbH / fromIntegral winH
                mouseX = realToFrac rawX * scaleX
                mouseY = realToFrac rawY * scaleY
            
            uiMgr ← readIORef (uiManagerRef env)
            case findClickableElementAt (mouseX, mouseY) uiMgr of
                Just (elemHandle, _callback) → do
                    logDebug logger CatInput $ "Scroll on UI element: " <> T.pack (show elemHandle)
                    Q.writeQueue (luaQueue env) (LuaUIScrollEvent elemHandle x y)
                Nothing → do
                    -- No UI element under cursor, and no focus → game scroll
                    logDebug logger CatInput "Scroll: game scroll (camera zoom)"
                    Q.writeQueue (luaQueue env) (LuaScrollEvent x y)
        
        return $ updateScrollState inpSt x y
    InputWindowEvent winEv → do
        logger ← readIORef (loggerRef env)
        case winEv of
          WindowResize w h → do
            logDebug logger CatInput $ "Window resize event: width=" <> T.pack (show w) <> ", height=" <> T.pack (show h)
            writeIORef (windowSizeRef env) (w, h)
            Q.writeQueue (luaQueue env) (LuaWindowResize w h)
          FramebufferResize w h → do
            logDebug logger CatInput $ "Framebuffer resize event: width=" <> T.pack (show w) <> ", height=" <> T.pack (show h)
            writeIORef (framebufferSizeRef env) (w, h)
            Q.writeQueue (luaQueue env) (LuaFramebufferResize w h)
          WindowFocus focused →
            logDebug logger CatInput $ "Window focus event: focused=" <> T.pack (show focused)
          WindowMinimize minimized →
            logDebug logger CatInput $ "Window minimize event: minimized=" <> T.pack (show minimized)
        return $ updateWindowState inpSt winEv

-- | Helper state update functions
updateKeyState ∷ InputState → GLFW.Key → GLFW.KeyState → GLFW.ModifierKeys → InputState
updateKeyState state key keyState mods = state
    { inpKeyStates = Map.insert key newKeyState (inpKeyStates state) }
    where
        newKeyState = KeyState
            { keyPressed = keyState ≡ GLFW.KeyState'Pressed
                         ∨ keyState ≡ GLFW.KeyState'Repeating
            , keyMods = mods
            , keyTime = 0.0
            }

updateWindowState ∷ InputState → WindowEvent → InputState
updateWindowState state (WindowFocus focused) = state { inpWindowFocused = focused }
updateWindowState state _ = state

updateMouseState ∷ InputState → GLFW.MouseButton → (Double, Double) → GLFW.MouseButtonState → InputState
updateMouseState state btn pos btnState = state
    { inpMousePos = pos
    , inpMouseBtns = Map.insert btn (btnState ≡ GLFW.MouseButtonState'Pressed) (inpMouseBtns state)
    }

updateScrollState ∷ InputState → Double → Double → InputState
updateScrollState state x y = state { inpScrollDelta = (x, y) }

-- | Helper to check if key is pressed or repeating
isKeyDown ∷ GLFW.KeyState → Bool
isKeyDown GLFW.KeyState'Pressed   = True
isKeyDown GLFW.KeyState'Repeating = True
isKeyDown _                       = False
