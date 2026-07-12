{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Input.Thread where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent (threadDelay, forkIO)
import Control.Exception (SomeException, catch, finally)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar')
import Data.IORef (IORef, newIORef, writeIORef, readIORef, atomicModifyIORef')
import Engine.Core.Log (logDebug, logError, logInfo, LogCategory(..))
import Engine.Core.State
import Engine.Core.Thread
import Engine.Input.Bindings (KeyBindings, parseKeyName)
import Engine.Input.State
import Engine.Input.Types
import Engine.Scripting.Lua.Types
import Engine.ActionOutcome (ActionOutcome(..), pushActionOutcome)
import Engine.Graphics.Viewport (viewportDegenerate)
import qualified Engine.Core.Queue as Q
import UI.Manager (findClickableElementAt, findRightClickableElementAt
                  , findElementAt, validateFocus)
import UI.Tooltip (isTooltipLocked, isTooltipVisible, isPointInLockedTooltip
                  , clearTooltipLock, toggleTooltipLock)
import UI.Types (ElementHandle(..))
import UI.Focus (getInputMode, InputMode(..), FocusId(..), fmCurrentFocus)

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
            inpSt ← readIORef (inputStateRef env)
            -- processInputs publishes to inputStateRef after each
            -- event it processes (#697) — no batch write here.
            _ ← processInputs env inpSt
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
            -- Publish after EVERY event, not once per drained batch: a
            -- synthetic modifier press (#697) must be visible to Lua
            -- pollers (engine.isKeyDown) before the following click/key
            -- event's Lua broadcast is dispatched — this write
            -- happens-before that broadcast's STM enqueue below, so a
            -- callback can never observe a state older than the events
            -- that preceded its own.
            writeIORef (inputStateRef env) newState
            processInputs env newState
        Nothing → do
            -- F4 (#730): flush a still-pending char-aggregate batch at
            -- the tail of every drain too — not just when a following
            -- non-char event interrupts it — so a lone trailing real
            -- character (nothing left in the queue to interrupt it
            -- this tick) still records promptly instead of waiting
            -- indefinitely for the next unrelated event. Must persist
            -- via inputStateRef like the Just branch above, or the
            -- next drain re-reads the unflushed batch and double-pushes it.
            flushed ← flushPendingCharBatch env inpSt
            writeIORef (inputStateRef env) flushed
            return flushed

-- | F4 (#730): flush any pending char-aggregate outcome (see
--   'flushPendingCharBatch') before dispatching a NON-char event —
--   the interleaving event (a real key's matching InputKeyEvent, a
--   mouse move, ...) is what ends the run of characters it
--   interrupted. A char event never flushes here (it's the thing
--   BEING accumulated); 'dispatchInput's own InputCharEvent case
--   folds it into the batch instead.
processInput ∷ EngineEnv → InputState → InputEvent → IO InputState
processInput env inpSt0 event = do
    inpSt ← case event of
        InputCharEvent _ → pure inpSt0
        _                → flushPendingCharBatch env inpSt0
    dispatchInput env inpSt event

-- | F4 (#730): push the pending char-batch aggregate (if any) as ONE
--   ActionOutcome record and clear it — so a synthetic multi-character
--   @input.type@ sequence (N 'InputCharEvent's with no other event
--   interleaved before its trailing 'InputBarrier') collapses to a
--   single truthful record instead of N misleadingly-separate ones
--   (#730 review). Uniform delivery (every char applied, or every char
--   dropped) reads "accepted"/"noop"; a mixed batch reads "partial"
--   with consistent requested/applied/dropped counts.
flushPendingCharBatch ∷ EngineEnv → InputState → IO InputState
flushPendingCharBatch env inpSt = case inpCharBatch inpSt of
    Nothing → return inpSt
    Just batch → do
        gt ← readIORef (gameTimeRef env)
        let req = cbRequested batch
            app = cbApplied batch
            drp = cbDropped batch
            outcome
              | drp ≡ 0   = "accepted"
              | app ≡ 0   = "noop"
              | otherwise = "partial"
        pushActionOutcome (actionOutcomeRef env) ActionOutcome
            { aoTs = gt, aoKind = "input.type", aoOutcome = outcome
            , aoWhereX = Nothing, aoWhereY = Nothing
            , aoTarget = cbTarget batch
            , aoRequested = Just req, aoApplied = Just app, aoDropped = Just drp
            , aoReason = cbDropReason batch
            , aoHandler = cbHandler batch
            }
        return inpSt { inpCharBatch = Nothing }

-- | F4 (#730): fold one char's routing outcome into the pending batch
--   (starting a fresh one if none is in flight). `domain` names the
--   delivery target's kind when `applied`, or the drop classification
--   otherwise; an applied char's domain/target always overwrite the
--   batch's reported handler (an accepted delivery is more informative
--   than a drop classification), while a drop only sets the fallback
--   handler if nothing has been applied yet.
accumulateCharOutcome ∷ InputState → Bool → Text → Maybe Word32 → InputState
accumulateCharOutcome inpSt applied domain target =
    let batch0 = case inpCharBatch inpSt of
            Just b  → b
            Nothing → emptyCharBatch
        batch1
          | applied = batch0
              { cbRequested = cbRequested batch0 + 1
              , cbApplied   = cbApplied batch0 + 1
              , cbHandler   = Just domain
              , cbTarget    = target
              }
          | otherwise = batch0
              { cbRequested  = cbRequested batch0 + 1
              , cbDropped    = cbDropped batch0 + 1
              , cbDropReason = Just domain
              , cbHandler    = case cbHandler batch0 of
                  Just h  → Just h
                  Nothing → Just domain
              }
    in inpSt { inpCharBatch = Just batch1 }

-- | F4 (#730) Layer A: one primary keyboard record per real key press
--   ('processInput's caller only invokes this via 'dispatchInput' for
--   the raw event; recording itself gates on Pressed + non-modifier —
--   see 'shouldRecord' at each call site below). `matched` is the
--   specific action name that actually fired for this key (set by the
--   very same 'when' branch that queued the Lua message it names, so
--   this can never drift from the real dispatch decision); 'Nothing'
--   records a "noop" — the key reached a recognized text-input domain
--   but matched none of its editing actions (ordinary printable keys
--   route via InputCharEvent instead, not here).
recordKeyOutcome ∷ EngineEnv → Text → Maybe Text → Maybe Word32 → IO ()
recordKeyOutcome env domain matched target = do
    gt ← readIORef (gameTimeRef env)
    let (outcome, handler, reason) = case matched of
            Just action → ("accepted", Just action, Nothing)
            Nothing     → ("noop", Just domain,
                           Just (domain <> ": key matched no recognized action"))
    pushActionOutcome (actionOutcomeRef env) ActionOutcome
        { aoTs = gt, aoKind = "input.key", aoOutcome = outcome
        , aoWhereX = Nothing, aoWhereY = Nothing, aoTarget = target
        , aoRequested = Nothing, aoApplied = Nothing, aoDropped = Nothing
        , aoReason = reason, aoHandler = handler
        }

-- | F4 (#730): the bound action name for a gameplay-domain key press,
--   derived from the SAME keybind registry
--   'Engine.Input.Bindings.keyMatchesAction' consults — domain-level
--   attribution is the contract (#730 review: "consumed-vs-ignored
--   distinction derivable from the engine-side keybind registry";
--   per-Lua-script onKeyDown-subscriber attribution is NOT required).
--   'Nothing' when no editable action binds this key — still a real,
--   accepted broadcast (every onKeyDown fires regardless of binding),
--   just with no identifiable bound consumer.
gameplayKeyHandler ∷ GLFW.Key → KeyBindings → Maybe Text
gameplayKeyHandler glfwKey bindings = case
    [ action | (action, keyNames) ← Map.toList bindings
             , any (\name → glfwKey `elem` parseKeyName name) keyNames ] of
        (a : _) → Just a
        []      → Nothing

-- | The real per-event dispatch — split out of 'processInput' so the
--   F4 char-batch flush above can wrap it without reindenting this
--   whole (pre-existing) case, which every alternative below still
--   drives via the unqualified `inpSt` its body already refers to.
dispatchInput ∷ EngineEnv → InputState → InputEvent → IO InputState
dispatchInput env inpSt event = case event of
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

        -- F4 (#730) Layer A: exactly one primary record per REAL key
        -- press — never a release/repeat (subordinate, #730 review)
        -- and never a bare modifier key (Shift/Ctrl/Alt/Super), which
        -- only ever brackets another action ("modifier presses/
        -- releases ... must not create additional primary Layer A
        -- records"). `matchedRef` is set by whichever `when` branch
        -- below actually queues a Lua message, so the recorded
        -- handler can never drift from the real dispatch decision.
        matchedRef ← newIORef (Nothing ∷ Maybe Text)
        let markMatched name = writeIORef matchedRef (Just name)
            shouldRecord = keyState ≡ GLFW.KeyState'Pressed
                         ∧ not (isModifierKey glfwKey)

        case (shellMode, uiFocus) of
          (TextInputMode (FocusId fid),_) → do
            logger ← readIORef (loggerRef env)
            logDebug logger CatInput $ "Input mode: ShellTextInput, focusId=" <> T.pack (show fid)
            when (key ≡ KeyGrave ∧ keyState ≡ GLFW.KeyState'Pressed) $ do
                Q.writeQueue (luaQueue env) LuaShellToggle
                markMatched "shellToggle"
            -- Lua side clears focus: shell.onFocusLost → engine.releaseFocus()
            when (key ≡ KeyEscape ∧ keyState ≡ GLFW.KeyState'Pressed) $ do
                Q.writeQueue (luaQueue env) $ LuaFocusLost fid
                markMatched "focusLost"
            -- isKeyDown includes Pressed+Repeating for held-key repeat;
            -- KeyState'Pressed is single-fire only. Backspace, arrows,
            -- and delete intentionally repeat for text editing UX.
            when (key ≡ KeyBackspace ∧ isKeyDown keyState) $ do
                Q.writeQueue (luaQueue env) (LuaTextBackspace fid)
                markMatched "backspace"
            when (key ≡ KeyEnter ∧ keyState ≡ GLFW.KeyState'Pressed) $ do
                Q.writeQueue (luaQueue env) (LuaTextSubmit fid)
                markMatched "submit"
            when (key ≡ KeyTab ∧ keyState ≡ GLFW.KeyState'Pressed) $ do
                Q.writeQueue (luaQueue env) (LuaTabPressed fid)
                markMatched "tab"
            when (key ≡ KeyUp ∧ keyState ≡ GLFW.KeyState'Pressed) $ do
                Q.writeQueue (luaQueue env) (LuaCursorUp fid)
                markMatched "cursorUp"
            when (key ≡ KeyDown ∧ keyState ≡ GLFW.KeyState'Pressed) $ do
                Q.writeQueue (luaQueue env) (LuaCursorDown fid)
                markMatched "cursorDown"
            when (key ≡ KeyLeft ∧ isKeyDown keyState) $ do
                Q.writeQueue (luaQueue env) (LuaCursorLeft fid)
                markMatched "cursorLeft"
            when (key ≡ KeyRight ∧ isKeyDown keyState) $ do
                Q.writeQueue (luaQueue env) (LuaCursorRight fid)
                markMatched "cursorRight"
            when ((key ≡ KeyHome ∧ keyState ≡ GLFW.KeyState'Pressed)
                 ∨ (key ≡ KeyA ∧ keyState ≡ GLFW.KeyState'Pressed
                   ∧ (GLFW.modifierKeysControl mods))) $ do
                Q.writeQueue (luaQueue env) (LuaCursorHome fid)
                markMatched "cursorHome"
            when ((key ≡ KeyEnd ∧ keyState ≡ GLFW.KeyState'Pressed)
                 ∨ (key ≡ KeyE ∧ keyState ≡ GLFW.KeyState'Pressed
                   ∧ (GLFW.modifierKeysControl mods))) $ do
                Q.writeQueue (luaQueue env) (LuaCursorEnd fid)
                markMatched "cursorEnd"
            when (key ≡ KeyDelete ∧ isKeyDown keyState) $ do
                Q.writeQueue (luaQueue env) (LuaTextDelete fid)
                markMatched "delete"
            when (key ≡ KeyC ∧ keyState ≡ GLFW.KeyState'Pressed
                   ∧ (GLFW.modifierKeysControl mods)) $ do
                Q.writeQueue (luaQueue env) (LuaInterrupt fid)
                markMatched "interrupt"
            when shouldRecord $ do
                matched ← readIORef matchedRef
                recordKeyOutcome env "shell_text" matched (Just fid)

          (GameInputMode, Just (ElementHandle elemId)) → do
            logger ← readIORef (loggerRef env)
            logDebug logger CatInput $ "Input mode: UITextInput, elementId=" <> T.pack (show elemId)
            when (key ≡ KeyGrave ∧ keyState ≡ GLFW.KeyState'Pressed) $ do
                Q.writeQueue (luaQueue env) LuaShellToggle
                markMatched "shellToggle"
            when (key ≡ KeyEscape ∧ keyState ≡ GLFW.KeyState'Pressed) $ do
                Q.writeQueue (luaQueue env) LuaUIEscape
                markMatched "uiEscape"
            when (key ≡ KeyBackspace ∧ isKeyDown keyState) $ do
                Q.writeQueue (luaQueue env) LuaUIBackspace
                markMatched "backspace"
            when (key ≡ KeyEnter ∧ keyState ≡ GLFW.KeyState'Pressed) $ do
                Q.writeQueue (luaQueue env) LuaUISubmit
                markMatched "submit"
            when (key ≡ KeyLeft ∧ isKeyDown keyState) $ do
                Q.writeQueue (luaQueue env) LuaUICursorLeft
                markMatched "cursorLeft"
            when (key ≡ KeyRight ∧ isKeyDown keyState) $ do
                Q.writeQueue (luaQueue env) LuaUICursorRight
                markMatched "cursorRight"
            when ((key ≡ KeyHome ∧ keyState ≡ GLFW.KeyState'Pressed)
                 ∨ (key ≡ KeyA ∧ keyState ≡ GLFW.KeyState'Pressed
                   ∧ (GLFW.modifierKeysControl mods))) $ do
                Q.writeQueue (luaQueue env) LuaUIHome
                markMatched "cursorHome"
            when ((key ≡ KeyEnd ∧ keyState ≡ GLFW.KeyState'Pressed)
                 ∨ (key ≡ KeyE ∧ keyState ≡ GLFW.KeyState'Pressed
                   ∧ (GLFW.modifierKeysControl mods))) $ do
                Q.writeQueue (luaQueue env) LuaUIEnd
                markMatched "cursorEnd"
            when (key ≡ KeyDelete ∧ isKeyDown keyState) $ do
                Q.writeQueue (luaQueue env) LuaUIDelete
                markMatched "delete"
            when shouldRecord $ do
                matched ← readIORef matchedRef
                recordKeyOutcome env "ui_text" matched (Just elemId)

          (GameInputMode, Nothing) → do
            logger ← readIORef (loggerRef env)
            logDebug logger CatInput $ "Input mode: GameInputMode, key=" <> T.pack (show key)
            when (key ≡ KeyGrave ∧ keyState ≡ GLFW.KeyState'Pressed) $ do
                Q.writeQueue (luaQueue env) LuaShellToggle
                markMatched "openShell"
            when (key ≠ KeyGrave) $ do
                let lq = luaQueue env
                -- Carry the exact GLFW key alongside the merged logical key:
                -- onKeyDown still gets the merged name string, but
                -- engine.keyMatchesAction uses the precise key to resolve
                -- which side of a modifier was pressed (race-free, vs. the
                -- transient shared input state).
                when (keyState ≡ GLFW.KeyState'Pressed) $
                    Q.writeQueue lq (LuaKeyDownEvent key glfwKey)
                when (keyState ≡ GLFW.KeyState'Released) $
                    Q.writeQueue lq (LuaKeyUpEvent key)
            when (key ≡ KeyEscape ∧ keyState ≡ GLFW.KeyState'Pressed) $ do
                logDebug logger CatInput "Action triggered: escape"
                Q.writeQueue (luaQueue env) LuaUIEscape
                markMatched "escape"
            -- Gameplay-domain keys always dispatch (onKeyDown always
            -- broadcasts here, Grave excepted above) — "accepted" is
            -- unconditional; the handler resolves to the bound action
            -- name when the keybind registry has one, else falls back
            -- to the domain itself (still a real, consumed broadcast —
            -- see 'gameplayKeyHandler').
            when shouldRecord $ do
                bindings ← readIORef (keyBindingsRef env)
                matched ← readIORef matchedRef
                let handler = case matched of
                        Just m  → Just m
                        Nothing → case gameplayKeyHandler glfwKey bindings of
                            Just b  → Just b
                            Nothing → Just "gameplay_key"
                recordKeyOutcome env "gameplay_key" handler Nothing

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

    InputCharEvent c →
        -- Backtick is INTENTIONALLY untypeable in every text field:
        -- it's the shell-toggle key (KeyGrave above), and letting the
        -- char through would type a '`' into the shell or textbox the
        -- press just toggled/defocused. F4 (#730): each branch folds
        -- its routing outcome into the pending char-aggregate batch
        -- (see 'accumulateCharOutcome') rather than pushing its own
        -- record — 'flushPendingCharBatch' collapses a whole run of
        -- these (a synthetic multi-character @input.type@, or one
        -- real keystroke's char) into a single truthful record.
        if c ≡ '`'
          then return $ accumulateCharOutcome inpSt False "dropped_backtick" Nothing
          else do
            focusMgr ← readIORef (focusManagerRef env)
            uiFocus ← atomicModifyIORef' (uiManagerRef env) validateFocus
            case (fmCurrentFocus focusMgr, uiFocus) of
              (Just (FocusId fid), _) → do
                Q.writeQueue (luaQueue env) (LuaCharInput fid c)
                return $ accumulateCharOutcome inpSt True "shell_text" (Just fid)
              (Nothing, Just (ElementHandle eh)) → do
                Q.writeQueue (luaQueue env) (LuaUICharInput c)
                return $ accumulateCharOutcome inpSt True "ui_text" (Just eh)
              (Nothing, Nothing) →
                return $ accumulateCharOutcome inpSt False "dropped_unfocused" Nothing
    InputMouseEvent btn pos state → do
        let lq = luaQueue env
            (x, y) = pos
        logger ← readIORef (loggerRef env)

        -- F4 (#646) Layer A: routes that consume a press WITHOUT ever
        -- queuing a Lua event (ClickSwallowed, and a ClickUI whose
        -- widget has no handler for this button) are otherwise
        -- invisible to the oracle — Dispatch.hs only sees the messages
        -- that actually get queued. Record those routes right here,
        -- where the decision is made; a route that DOES queue a
        -- LuaUIClickEvent/LuaUIRightClickEvent is recorded once, in
        -- Dispatch.hs, to avoid double-recording the same press.
        let recordRouteOutcome ∷ Text → Maybe Text → IO ()
            recordRouteOutcome outcome handler = do
                gt ← readIORef (gameTimeRef env)
                pushActionOutcome (actionOutcomeRef env) ActionOutcome
                    { aoTs = gt, aoKind = "input.click", aoOutcome = outcome
                    -- The real click position (review round 9 — these
                    -- routes previously hard-coded Nothing/Nothing,
                    -- losing the location entirely; the critic needs it
                    -- to identify a phantom affordance). Window coords,
                    -- matching what scripts/init_mouse.lua's recordClick
                    -- records for the game-chain routes.
                    , aoWhereX = Just x, aoWhereY = Just y, aoTarget = Nothing
                    , aoRequested = Nothing, aoApplied = Nothing, aoDropped = Nothing
                    , aoReason = Nothing, aoHandler = handler
                    }

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

            -- Zero-size window/framebuffer (minimize): winW/winH = 0 makes
            -- the scale division yield NaN/Infinity, and fbW/fbH = 0
            -- collapses the scaled coord to (0,0) — either way the hit-test
            -- below would dispatch a bogus UI/game event. Drop the click.
            if viewportDegenerate winW winH fbW fbH
              then do
                recordRouteOutcome "noop" (Just "degenerate_viewport")
                return ClickSwallowed
              else case btn of
              -- Middle button: toggle tooltip lock when a tooltip is up.
              -- Otherwise hit-test the UI so a middle-click over ANY UI/
              -- menu surface can't fall through to gameplay middle-click
              -- behavior (the loop uses it for camera dragging — see
              -- Engine.Loop.Camera). Unlike left/right-click (which only
              -- block on clickable controls), middle-click has no UI
              -- handler to dispatch to and exists purely to pan the
              -- camera, so a passive panel under the cursor must block it
              -- too — hence findElementAt (any sized element) rather than
              -- findClickableElementAt. Any UI surface under the cursor
              -- SWALLOWS the click: the camera middle-drag polls
              -- inpMouseBtns directly (bypassing the route), so only
              -- ClickSwallowed — which keeps the button out of
              -- inpMouseBtns — actually stops the drag. Empty (non-UI)
              -- space still routes the middle-click to the world.
              GLFW.MouseButton'3 →
                if isTooltipVisible uiMgr
                  then do
                    atomicModifyIORef' (uiManagerRef env) $ \m →
                        (toggleTooltipLock m, ())
                    recordRouteOutcome "accepted" (Just "tooltip_lock_toggle")
                    return ClickSwallowed
                  else case findElementAt mousePos uiMgr of
                    Just _ → do
                        logDebug logger CatUI
                            "Middle-click swallowed by UI surface"
                        recordRouteOutcome "noop" (Just "ui_surface_block")
                        return ClickSwallowed
                    Nothing → do
                        -- scripts/init_mouse.lua's onMouseDown only
                        -- branches on MOUSE_LEFT/MOUSE_RIGHT, so a middle
                        -- press reaching "game" falls through the whole
                        -- chain with no recordClick call of its own (it
                        -- drives camera-drag polling instead, not the
                        -- click-dispatch chain) — record it here or it's
                        -- invisible to F4 entirely (review round 2).
                        Q.writeQueue lq (LuaMouseDownEvent btn x y)
                        recordRouteOutcome "accepted" (Just "camera_drag")
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
                  then do
                    recordRouteOutcome "accepted" (Just "tooltip_lock_dismiss")
                    return ClickSwallowed
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
                                    (LuaUIClickEvent elemHandle callback x y)
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
                                    (LuaUIRightClickEvent elemHandle callback x y)
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
                                Just (_leftCallbackHandle, leftClickCallback) → do
                                    -- Consumed by a clickable control with no
                                    -- right-click handler (e.g. an ordinary
                                    -- button). Left-clicking such a control
                                    -- clears textbox/dropdown focus via the
                                    -- dispatched click event; the right-click
                                    -- has no event to ride, so clear focus
                                    -- explicitly here, otherwise a focused
                                    -- widget stays captured.
                                    Q.writeQueue lq LuaUIFocusLost
                                    logDebug logger CatUI
                                        "Right-click consumed by clickable UI element (no handler)"
                                    -- No LuaUIRightClickEvent gets queued for
                                    -- this route, so Dispatch.hs never sees
                                    -- it either — record it here or it's
                                    -- invisible to F4 entirely (review #1).
                                    -- The recorded handler is the control's
                                    -- OWN left-click callback name (its
                                    -- identity), not a generic placeholder
                                    -- (review round 8 — the acceptance
                                    -- criterion is "records the consuming
                                    -- handler", which a constant string
                                    -- can't satisfy for more than one
                                    -- control).
                                    recordRouteOutcome "accepted" (Just leftClickCallback)
                                    return ClickUI
                                Nothing → do
                                    -- A right-click that misses all UI clears
                                    -- focus before reaching gameplay, exactly
                                    -- like the left-click miss path above —
                                    -- otherwise a focused textbox/dropdown
                                    -- keeps capturing the keyboard.
                                    Q.writeQueue lq LuaUIFocusLost
                                    Q.writeQueue lq (LuaMouseDownEvent btn x y)
                                    return ClickGame

                      _ → do
                        -- GLFW mouse buttons 4-8 (side/extra buttons):
                        -- Dispatch.hs maps anything past MouseButton'3 to
                        -- Lua button 0, and init_mouse.lua's onMouseDown
                        -- only branches on MOUSE_LEFT/MOUSE_RIGHT, so this
                        -- reaches the end of that chain with no defined
                        -- behavior anywhere and no recordClick call
                        -- (review round 3) — record it here instead.
                        Q.writeQueue lq (LuaMouseDownEvent btn x y)
                        recordRouteOutcome "noop" (Just "unmapped_button")
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
    -- Synthetic-sequence fence (#697): hand the carried events (the
    -- tap's modifier releases) to the Lua thread. This message enters
    -- the Lua queue AFTER every broadcast the preceding events of the
    -- same sequence queued above, so the Lua thread re-injects the
    -- releases only once those callbacks have run — a shift-click's
    -- handler polls shift as held, and the release still lands
    -- afterwards (no stuck keys).
    InputFollowup evs → do
        Q.writeQueue (luaQueue env) (LuaInjectFollowup evs)
        return inpSt
    -- Completion marker for synthetic injection (#727): everything
    -- queued ahead of this barrier (FIFO, this is the only consumer)
    -- has already been fully processed, side effects included, by the
    -- time this token lands — see 'inputBarrierRef's haddock. 'max'
    -- (not overwrite) so an out-of-order arrival can't ever move the
    -- watermark backwards, even though allocation+push order already
    -- guarantees monotonic processing order here.
    InputBarrier tok → do
        atomically $ modifyTVar' (inputBarrierRef env) (max tok)
        return inpSt
    InputScrollEvent x y → do
        logger ← readIORef (loggerRef env)
        logDebug logger CatInput $ "Scroll event: dx=" <> T.pack (show x) <> ", dy=" <> T.pack (show y)

        -- Check both shifts independently: released keys keep a map
        -- entry with keyPressed=False, so a nested left-then-right
        -- lookup would stop consulting RightShift after the first
        -- LeftShift press of the session.
        let shiftDown k = maybe False keyPressed (Map.lookup k (inpKeyStates inpSt))
            shiftHeld = shiftDown GLFW.Key'LeftShift ∨ shiftDown GLFW.Key'RightShift
            (rawX, rawY) = inpMousePos inpSt

            -- F4 (#730) Layer A: exactly one record per H1 `scroll`
            -- action — InputScrollEvent carries no bracketing internal
            -- events (unlike key taps/types), so no batching is
            -- needed. Coordinates are the CURRENT cursor position
            -- (#730 review — a scroll event carries no position of
            -- its own), in the same window-space 'recordRouteOutcome'
            -- above uses for clicks.
            recordScrollOutcome ∷ Text → Text → Maybe Word32 → IO ()
            recordScrollOutcome outcome domain target = do
                gt ← readIORef (gameTimeRef env)
                pushActionOutcome (actionOutcomeRef env) ActionOutcome
                    { aoTs = gt, aoKind = "input.scroll", aoOutcome = outcome
                    , aoWhereX = Just rawX, aoWhereY = Just rawY, aoTarget = target
                    , aoRequested = Nothing, aoApplied = Nothing, aoDropped = Nothing
                    , aoReason = Nothing, aoHandler = Just domain
                    }

        if shiftHeld
        then do
            logDebug logger CatInput "Shift+scroll: z-slice adjustment"
            Q.writeQueue (luaQueue env) (LuaZSliceScroll x y)
            recordScrollOutcome "accepted" "z_slice" Nothing
        else do
            (winW, winH) ← readIORef (windowSizeRef env)
            (fbW, fbH) ← readIORef (framebufferSizeRef env)
            let scaleX = fromIntegral fbW / fromIntegral winW
                scaleY = fromIntegral fbH / fromIntegral winH
                mouseX = realToFrac rawX * scaleX
                mouseY = realToFrac rawY * scaleY

            uiMgr ← readIORef (uiManagerRef env)
            -- Same zero-size window/framebuffer guard as the click
            -- path — now also recording the drop (#730 review: parity
            -- with the click path's own degenerate_viewport noop). The
            -- shift+scroll z-slice branch above deliberately bypasses
            -- this guard, same as before this change (passivity —
            -- recording must not alter that routing).
            if viewportDegenerate winW winH fbW fbH
              then recordScrollOutcome "noop" "degenerate_viewport" Nothing
              else case findClickableElementAt (mouseX, mouseY) uiMgr of
                Just (elemHandle@(ElementHandle eh), _callback) → do
                    logDebug logger CatInput $ "Scroll on UI element: " <> T.pack (show elemHandle)
                    Q.writeQueue (luaQueue env) (LuaUIScrollEvent elemHandle x y)
                    recordScrollOutcome "accepted" "ui_scroll" (Just eh)
                Nothing → do
                    logDebug logger CatInput "Scroll: game scroll (camera zoom)"
                    Q.writeQueue (luaQueue env) (LuaScrollEvent x y)
                    recordScrollOutcome "accepted" "game_scroll" Nothing

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
