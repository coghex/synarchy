{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Physical + synthetic KEYBOARD dispatch (#787): routes a raw GLFW
--   key event to shell-text, UI-text, or gameplay handling depending
--   on focus, and records the F4 (#730) primary keyboard outcome.
--   Split out of 'Engine.Input.Thread' to keep that facade a thin
--   thread-loop entrypoint; reached only through
--   'Engine.Input.Thread.Dispatch.dispatchInput'.
module Engine.Input.Thread.Keyboard
  ( dispatchKeyEvent
  , recordKeyOutcome
  , gameplayKeyHandler
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW
import Data.IORef (readIORef, writeIORef, newIORef, atomicModifyIORef')
import Engine.Core.Log (logDebug, LogCategory(..))
import Engine.Core.State
import Engine.Input.Bindings (KeyBindings, parseKeyName)
import Engine.Input.State
import Engine.Input.Types
import Engine.Scripting.Lua.Types
import Engine.ActionOutcome (ActionOutcome(..), pushActionOutcome)
import qualified Engine.Core.Queue as Q
import UI.Manager (validateFocus)
import UI.Types (ElementHandle(..))
import UI.Focus (getInputMode, InputMode(..), FocusId(..))

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

-- | The real per-event dispatch for 'InputKeyEvent' — split out of
--   'dispatchInput' so the F4 char-batch flush wrapping it (in
--   'Engine.Input.Thread.Dispatch') doesn't have to reindent this
--   whole (pre-existing) body, which still drives everything below via
--   the unqualified `inpSt` its own body already refers to.
dispatchKeyEvent ∷ EngineEnv → InputState → GLFW.Key → GLFW.KeyState → GLFW.ModifierKeys → IO InputState
dispatchKeyEvent env inpSt glfwKey keyState mods = do
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
