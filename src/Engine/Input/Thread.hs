{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Input.Thread where

import UPrelude
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent (threadDelay, ThreadId, killThread, forkIO)
import Control.Exception (SomeException, catch, finally)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Data.IORef (IORef, newIORef, writeIORef, readIORef, atomicModifyIORef')
import Engine.Core.Log (logDebug, logError, logWarn, logInfo, LogCategory(..))
import Engine.Core.State
import Engine.Core.Thread
import Engine.Core.Error.Exception (SystemError(..), ExceptionType(..))
import Engine.Event.Types
import Engine.Input.Types
import Engine.Input.Callback
import Engine.Scripting.Lua.Types
import Engine.Graphics.Window.Types (Window(..))
import qualified Engine.Core.Queue as Q
import UI.Manager (findClickableElementAt, findRightClickableElementAt
                  , validateFocus)
import UI.Tooltip (isTooltipLocked, isTooltipVisible, isPointInLockedTooltip
                  , clearTooltipLock, toggleTooltipLock)
import UI.Types (ElementHandle(..))
import UI.Focus (FocusManager, getInputMode, InputMode(..), clearFocus
                , FocusId(..), fmCurrentFocus)

startInputThread ∷ EngineEnv → IO ThreadState
startInputThread env = do
    let logRef        = loggerRef env
    logger ← readIORef logRef
    stateRef ← newIORef ThreadRunning
    doneVar ← newEmptyMVar
    threadId ← catch 
        (do
            logInfo logger CatInput "Starting input thread..."
            tid ← forkIO $ runInputLoop env stateRef `finally` putMVar doneVar ()
            return tid
        ) 
        (\(e ∷ SomeException) → do
            logError logger CatInput $ "Failed starting input thread: " <> T.pack (show e)
            Q.writeQueue (eventQueue env) $ EventError
              "startInputThread:" $ T.pack (show e)
            error "Input thread start failure."
        )
    return $ ThreadState stateRef threadId doneVar

runInputLoop ∷ EngineEnv → IORef ThreadControl → IO ()
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
        -- One guarded tick per iteration; the recursive call lives
        -- OUTSIDE the catch — inside it, each tick pushes a catch
        -- frame that never pops (unbounded stack growth).
        ok ← catch
          (do
            let sharedInputRef = inputStateRef env
            inpSt ← readIORef sharedInputRef
            newInpSt ← processInputs env inpSt
            writeIORef sharedInputRef newInpSt
            threadDelay 16666  -- ~60 FPS
            pure True
          )
          (\(e ∷ SomeException) → do
            logger ← readIORef (loggerRef env)
            logError logger CatInput $ "Input thread crashed: " <> T.pack (show e)
            writeIORef (lifecycleRef env) CleaningUp
            pure False
          )
        when ok $ runInputLoop env stateRef

processInputs ∷ EngineEnv → InputState → IO InputState
processInputs env inpSt = do
    mEvent ← Q.tryReadQueue (inputQueue env)
    case mEvent of
        Just event → do
            newState ← processInput env inpSt event
            processInputs env newState
        Nothing → return inpSt

processInput ∷ EngineEnv → InputState → InputEvent → IO InputState
processInput env inpSt event = case event of
    InputKeyEvent glfwKey keyState mods → do
        -- Two independent focus systems checked here:
        --   1. FocusManager (focusManagerRef) — shell/console text input
        --   2. UIPageManager (uiManagerRef)   — UI widget text input
        -- Focus is cleared asynchronously by Lua callbacks:
        --   Shell:   LuaFocusLost → shell.onFocusLost → engine.releaseFocus()
        --   Textbox: LuaUIEscape  → uiManager.onUIEscape → UI.clearFocus()
        -- The UI focus is read through validateFocus, which clears a
        -- focus pointing at a dead/hidden element instead of letting
        -- it capture the keyboard (all keys would route to UI-text
        -- mode below and be dropped by the Lua dispatcher).
        focusMgr ← readIORef (focusManagerRef env)
        uiFocus ← atomicModifyIORef' (uiManagerRef env) validateFocus
        let shellMode = getInputMode focusMgr
            key = fromGLFWKey glfwKey
        case (shellMode, uiFocus) of
          (TextInputMode (FocusId fid),_) → do
            logger ← readIORef (loggerRef env)
            logDebug logger CatInput $ "Input mode: ShellTextInput, focusId=" <> T.pack (show fid)
            when (key ≡ KeyGrave ∧ keyState ≡ GLFW.KeyState'Pressed) $ do
                Q.writeQueue (luaQueue env) LuaShellToggle
            -- Lua side clears focus: shell.onFocusLost → engine.releaseFocus()
            when (key ≡ KeyEscape ∧ keyState ≡ GLFW.KeyState'Pressed) $ do
                Q.writeQueue (luaQueue env) $ LuaFocusLost fid
            -- isKeyDown includes Pressed+Repeating for held-key repeat;
            -- KeyState'Pressed is single-fire only. Backspace, arrows,
            -- and delete intentionally repeat for text editing UX.
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

          (GameInputMode, Just (ElementHandle elemId)) → do
            logger ← readIORef (loggerRef env)
            logDebug logger CatInput $ "Input mode: UITextInput, elementId=" <> T.pack (show elemId)
            when (key ≡ KeyGrave ∧ keyState ≡ GLFW.KeyState'Pressed) $
                Q.writeQueue (luaQueue env) LuaShellToggle
            when (key ≡ KeyEscape ∧ keyState ≡ GLFW.KeyState'Pressed) $
                Q.writeQueue (luaQueue env) LuaUIEscape
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
            when (key ≡ KeyEscape ∧ keyState ≡ GLFW.KeyState'Pressed) $ do
                logDebug logger CatInput "Action triggered: escape"
                Q.writeQueue (luaQueue env) LuaUIEscape

        -- While text input has focus, normal key presses are not
        -- recorded in inpKeyStates — the map should keep reflecting
        -- game-mode input, so pollers of it (camera arrow-pan, Lua
        -- isKeyDown/isActionDown) can't react to letters typed into
        -- the shell or a textbox. Modifiers are the exception: shift /
        -- ctrl / alt / super still need to be live so a world click
        -- that DROPS focus can immediately see the held modifier.
        -- RELEASES are always recorded: a key held across a focus
        -- change must still get its release, or it sticks "down".
        let textFocused = case (shellMode, uiFocus) of
              (TextInputMode _, _) → True
              (_, Just _)          → True
              _                    → False
        return $ if textFocused ∧ not (shouldTrackKeyStateWhileTextFocused glfwKey keyState)
            then inpSt
            else updateKeyState inpSt glfwKey keyState mods

    InputCharEvent c → do
        -- Backtick is INTENTIONALLY untypeable in every text field:
        -- it's the shell-toggle key (KeyGrave above), and letting the
        -- char through would type a '`' into the shell or textbox the
        -- press just toggled/defocused.
        when (c ≠ '`') $ do
          focusMgr ← readIORef (focusManagerRef env)
          uiFocus ← atomicModifyIORef' (uiManagerRef env) validateFocus
          case (fmCurrentFocus focusMgr, uiFocus) of
            (Just (FocusId fid), _) →
              Q.writeQueue (luaQueue env) (LuaCharInput fid c)
            (Nothing, Just _elemHandle) →
              Q.writeQueue (luaQueue env) (LuaUICharInput c)
            (Nothing, Nothing) → return ()
        return inpSt
    InputMouseEvent btn pos state → do
        let lq = luaQueue env
            (x, y) = pos
        logger ← readIORef (loggerRef env)
        
        -- Each press is routed exactly one way (ClickRoute): to the
        -- game (LuaMouseDownEvent), to a UI element (LuaUIClickEvent /
        -- right-click), or swallowed with no Lua event (tooltip lock,
        -- minimized window). The route is recorded per button so the
        -- matching release can hand it to Lua, and swallowed presses
        -- are kept out of inpMouseBtns so button pollers (camera
        -- middle-drag) don't react to clicks the tooltip ate.
        mRoute ← if state ≢ GLFW.MouseButtonState'Pressed then return Nothing else fmap Just $ do
            logDebug logger CatInput $ "Mouse button pressed: button=" <> T.pack (show btn)
                                    <> ", pos=(" <> T.pack (show x) <> "," <> T.pack (show y) <> ")"

            (winW, winH) ← readIORef (windowSizeRef env)
            (fbW, fbH) ← readIORef (framebufferSizeRef env)

            let scaleX = fromIntegral fbW / fromIntegral winW
                scaleY = fromIntegral fbH / fromIntegral winH
                mouseX = realToFrac x * scaleX
                mouseY = realToFrac y * scaleY

            logDebug logger CatUI $ "Click at (" <> T.pack (show mouseX) <> ", " <> T.pack (show mouseY) <> ")"

            uiMgr ← readIORef (uiManagerRef env)
            let mousePos = (mouseX, mouseY)

            -- Minimized window: winW/winH are 0, so the scale
            -- division above yields NaN/Infinity coords. Drop the
            -- click rather than feed NaN into hit-tests and Lua
            -- camera math.
            if not (winW > 0 ∧ winH > 0) then return ClickSwallowed else case btn of
              -- Middle button: toggle tooltip lock when a tooltip is up.
              -- Falls through to a normal mouse-down event when nothing
              -- is shown, so other middle-click behavior (panning, etc.)
              -- still reaches Lua.
              GLFW.MouseButton'3 →
                if isTooltipVisible uiMgr
                  then do
                    atomicModifyIORef' (uiManagerRef env) $ \m →
                        (toggleTooltipLock m, ())
                    return ClickSwallowed
                  else do
                    Q.writeQueue lq (LuaMouseDownEvent btn x y)
                    return ClickGame

              -- All other buttons: if a tooltip is locked, intercept the
              -- click. Inside the locked box → swallow (the locked
              -- tooltip is the topmost UI and consumes clicks on itself).
              -- Outside → release the lock + hide, then dispatch the
              -- click normally so the user-perceivable behavior is
              -- "first click anywhere off the tooltip dismisses it AND
              -- still does whatever it would have done."
              _ → do
                let locked      = isTooltipLocked uiMgr
                    clickInside = locked ∧ isPointInLockedTooltip mousePos uiMgr
                if clickInside
                  then return ClickSwallowed
                  else do
                    when locked $
                        atomicModifyIORef' (uiManagerRef env) $ \m →
                            (clearTooltipLock m, ())
                    -- Re-read manager after the unlock mutation so we
                    -- don't hit-test against the now-hidden tooltip page.
                    uiMgr' ← if locked
                                then readIORef (uiManagerRef env)
                                else return uiMgr
                    case btn of
                      GLFW.MouseButton'1 →
                        case findClickableElementAt mousePos uiMgr' of
                            Just (elemHandle, callback) → do
                                Q.writeQueue lq
                                    (LuaUIClickEvent elemHandle callback)
                                logDebug logger CatUI $
                                    "UI element left-clicked: " <> callback
                                return ClickUI
                            Nothing → do
                                Q.writeQueue lq LuaUIFocusLost
                                Q.writeQueue lq (LuaMouseDownEvent btn x y)
                                return ClickGame

                      GLFW.MouseButton'2 →
                        case findRightClickableElementAt mousePos uiMgr' of
                            Just (elemHandle, callback) → do
                                Q.writeQueue lq
                                    (LuaUIRightClickEvent elemHandle callback)
                                logDebug logger CatUI $
                                    "UI element right-clicked: " <> callback
                                return ClickUI
                            -- No right-click handler under the cursor. If
                            -- an ordinary clickable control is still there,
                            -- consume the click so it can't fall through to
                            -- gameplay (mirrors the left-click blocking
                            -- semantics); only truly empty UI space routes
                            -- the right-click to the world.
                            Nothing → case findClickableElementAt mousePos uiMgr' of
                                Just _ → do
                                    logDebug logger CatUI
                                        "Right-click consumed by clickable UI element (no handler)"
                                    return ClickUI
                                Nothing → do
                                    Q.writeQueue lq (LuaMouseDownEvent btn x y)
                                    return ClickGame

                      _ → do
                        Q.writeQueue lq (LuaMouseDownEvent btn x y)
                        return ClickGame

        -- The release ALWAYS goes to Lua — UI widget drags (slider
        -- knob, scrollbar tab) start from a LuaUIClickEvent and rely
        -- on uiManager.onMouseUp to end them. The press's route rides
        -- along so handlers wanting strict down/up pairing can filter
        -- on "game".
        when (state ≡ GLFW.MouseButtonState'Released) $ do
            logDebug logger CatInput $ "Mouse button released: button=" <> T.pack (show btn)
                                    <> ", pos=(" <> T.pack (show x) <> "," <> T.pack (show y) <> ")"
            let downRoute = Map.findWithDefault ClickGame btn (inpMouseRoutes inpSt)
            Q.writeQueue lq (LuaMouseUpEvent btn x y downRoute)

        return $ case mRoute of
            Just route → inpSt
                { inpMousePos    = pos
                , inpMouseRoutes = Map.insert btn route (inpMouseRoutes inpSt)
                , inpMouseBtns   = if route ≡ ClickSwallowed
                    then inpMouseBtns inpSt
                    else Map.insert btn True (inpMouseBtns inpSt)
                }
            Nothing → inpSt
                { inpMousePos  = pos
                , inpMouseBtns = Map.insert btn False (inpMouseBtns inpSt)
                }
    InputCursorMove x y → 
        return $ inpSt { inpMousePos = (x, y) }
    InputScrollEvent x y → do
        logger ← readIORef (loggerRef env)
        logDebug logger CatInput $ "Scroll event: dx=" <> T.pack (show x) <> ", dy=" <> T.pack (show y)
        
        -- Check both shifts independently: released keys keep a map
        -- entry with keyPressed=False, so a nested left-then-right
        -- lookup would stop consulting RightShift after the first
        -- LeftShift press of the session.
        let shiftDown k = maybe False keyPressed (Map.lookup k (inpKeyStates inpSt))
            shiftHeld = shiftDown GLFW.Key'LeftShift ∨ shiftDown GLFW.Key'RightShift
        
        if shiftHeld
        then do
            logDebug logger CatInput "Shift+scroll: z-slice adjustment"
            Q.writeQueue (luaQueue env) (LuaZSliceScroll x y)
        else do
            (winW, winH) ← readIORef (windowSizeRef env)
            (fbW, fbH) ← readIORef (framebufferSizeRef env)
            let (rawX, rawY) = inpMousePos inpSt
                scaleX = fromIntegral fbW / fromIntegral winW
                scaleY = fromIntegral fbH / fromIntegral winH
                mouseX = realToFrac rawX * scaleX
                mouseY = realToFrac rawY * scaleY
            
            uiMgr ← readIORef (uiManagerRef env)
            -- Same minimized-window guard as the click path above.
            when (winW > 0 ∧ winH > 0) $ case findClickableElementAt (mouseX, mouseY) uiMgr of
                Just (elemHandle, _callback) → do
                    logDebug logger CatInput $ "Scroll on UI element: " <> T.pack (show elemHandle)
                    Q.writeQueue (luaQueue env) (LuaUIScrollEvent elemHandle x y)
                Nothing → do
                    logDebug logger CatInput "Scroll: game scroll (camera zoom)"
                    Q.writeQueue (luaQueue env) (LuaScrollEvent x y)

        return inpSt
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
          WindowFocus focused → do
            logDebug logger CatInput $ "Window focus event: focused=" <> T.pack (show focused)
            unless focused $ releaseHeldButtons env inpSt
          WindowMinimize minimized → do
            logDebug logger CatInput $ "Window minimize event: minimized=" <> T.pack (show minimized)
            when minimized $ releaseHeldButtons env inpSt
          -- Currently never emitted (the GLFW close request is polled
          -- via shouldClose in the main loop, not routed as an input
          -- event); handled here so the match stays total if it is.
          WindowClose →
            logDebug logger CatInput "Window close event"
        return $ updateWindowState inpSt winEv

-- * State update helpers

-- | Key-state gate while shell/UI text input owns the keyboard.
-- Normal typing keys stay out of inpKeyStates so game pollers ignore
-- them, but held modifiers must still be visible because the first
-- world click after focus loss reads engine.isKeyDown immediately.
shouldTrackKeyStateWhileTextFocused ∷ GLFW.Key → GLFW.KeyState → Bool
shouldTrackKeyStateWhileTextFocused key keyState =
    keyState ≡ GLFW.KeyState'Released ∨ isModifierKey key

isModifierKey ∷ GLFW.Key → Bool
isModifierKey key = key `elem`
    [ GLFW.Key'LeftShift, GLFW.Key'RightShift
    , GLFW.Key'LeftControl, GLFW.Key'RightControl
    , GLFW.Key'LeftAlt, GLFW.Key'RightAlt
    , GLFW.Key'LeftSuper, GLFW.Key'RightSuper
    ]

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
updateWindowState state (WindowFocus focused)
    -- Losing focus (alt-tab, minimize) can swallow the matching key/
    -- button release events, so drop all held state — otherwise a drag
    -- or a held modifier stays logically down until some later release
    -- happens to arrive. Regaining focus only flips the flag.
    | focused   = state { inpWindowFocused = True }
    | otherwise = (clearHeldInput state) { inpWindowFocused = False }
updateWindowState state (WindowMinimize minimized)
    | minimized = clearHeldInput state
    | otherwise = state
updateWindowState state _ = state

-- | Clear all held mouse-button and key state. Used on focus-loss /
--   minimize transitions, where the OS may never deliver the releases
--   that would normally clear it. Cursor position is left untouched.
clearHeldInput ∷ InputState → InputState
clearHeldInput state = state
    { inpKeyStates   = Map.empty
    , inpMouseBtns   = Map.empty
    , inpMouseRoutes = Map.empty
    }

-- | Emit the mouse-up events the OS swallows on a focus-loss / minimize
--   transition. The Haskell @inpMouseBtns@ clear (see 'clearHeldInput')
--   only fixes button pollers; several drags live entirely in Lua (the
--   drag-select box, slider / scrollbar knobs) and end only on
--   @onMouseUp@, so without a synthetic release they stay latched to the
--   cursor when focus returns.
--
--   Each release fires at the last known cursor position and is routed
--   'ClickSwallowed' ("swallowed"), NOT the press's original route: a
--   focus transition cancels held input rather than completing it, so
--   handlers that act on the release (drag-select committing a unit
--   selection) skip it on this route, while handlers that merely tear
--   down drag state (slider / scrollbar / button) ignore the route and
--   release as usual.
releaseHeldButtons ∷ EngineEnv → InputState → IO ()
releaseHeldButtons env inpSt =
    forM_ (heldButtonReleases inpSt) $ \(btn, mx, my, route) →
        Q.writeQueue (luaQueue env) (LuaMouseUpEvent btn mx my route)

-- | The synthetic releases 'releaseHeldButtons' should emit: one per
--   currently-held button, at the last known cursor position, all routed
--   'ClickSwallowed' so Lua treats them as a cancel. Buttons whose press
--   was swallowed never enter @inpMouseBtns@, so they are skipped here.
heldButtonReleases ∷ InputState
                   → [(GLFW.MouseButton, Double, Double, ClickRoute)]
heldButtonReleases inpSt =
    [ (btn, mx, my, ClickSwallowed)
    | (btn, True) ← Map.toList (inpMouseBtns inpSt) ]
  where (mx, my) = inpMousePos inpSt

isKeyDown ∷ GLFW.KeyState → Bool
isKeyDown GLFW.KeyState'Pressed   = True
isKeyDown GLFW.KeyState'Repeating = True
isKeyDown _                       = False
