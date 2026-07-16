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
import qualified Data.Set as Set
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
import UI.Manager (validateFocus, validateControlFocusIn, setControlFocus
                  , clearControlFocus, getElement, getControlFocus)
import UI.Types (ElementHandle(..), UIElement(..))
import UI.Focus (getInputMode, InputMode(..), FocusId(..))
import UI.FocusNavigation (nextFocus, prevFocus)

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
--   'Nothing' when no editable action binds this key — the broadcast
--   still fires (every onKeyDown fires regardless of binding), but
--   with no identifiable bound consumer, so #771 records it as
--   ignored/noop rather than a false "accepted".
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
        -- #745 review round 3: was this physical key ALREADY consumed
        -- by the control-focus layer at an earlier Pressed dispatch,
        -- still held (Repeating) or now releasing? See
        -- 'inpControlFocusConsumedKeys'.
        alreadyConsumed = Set.member glfwKey (inpControlFocusConsumedKeys inpSt)

    -- F4 (#730) Layer A: exactly one primary record per REAL key
    -- press — never a release/repeat (subordinate, #730 review)
    -- and never a bare modifier key (Shift/Ctrl/Alt/Super), which
    -- only ever brackets another action ("modifier presses/
    -- releases ... must not create additional primary Layer A
    -- records"). `matchedRef` is set by whichever `when` branch
    -- below actually queues a Lua message, so the recorded
    -- handler can never drift from the real dispatch decision.
    matchedRef ← newIORef (Nothing ∷ Maybe Text)
    -- #745: set only when THIS key was actually consumed by the
    -- control-focus layer (Tab/Shift+Tab, Enter/Space, or an arrow on
    -- a steppable focused control) — distinct from 'matchedRef', which
    -- also gets set by "openShell"/"escape" without those withholding
    -- key-state polling. Drives the same withholding text focus
    -- already gets, at the bottom of this function.
    controlFocusConsumedRef ← newIORef False
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

        -- #745: keyboard CONTROL focus — Tab/Shift+Tab traversal,
        -- Enter/Space activation, Escape clears, and arrow-key
        -- stepping on a steppable (slider) focused control. Only
        -- reachable here — neither shell text mode nor UI text mode
        -- above ever runs this, so text focus keeps its pre-existing
        -- priority automatically. Validated the same way text focus
        -- already is (belt-and-suspenders repair against a
        -- hidden/deleted/disabled/detached/out-of-scope control).
        -- #745 review round 4: one atomic transition, not a separate
        -- readIORef/writeIORef pair — the Lua thread mutates this same
        -- uiManagerRef concurrently (element create/delete/visibility
        -- etc.), and a plain write-back here would silently DISCARD
        -- any such concurrent mutation landed between the read and the
        -- write, not just ones touching control focus.
        (mgr0, mgr1, controlFocus) ← atomicModifyIORef' (uiManagerRef env) $ \old →
            let (validated, cf) = validateControlFocusIn old
            in (validated, (old, validated, cf))
        -- #745 review round 2: report a repair-triggered clear too —
        -- not just the explicit Tab/Escape transitions below — so a
        -- Lua focus-ring consumer never sees a stale handle linger
        -- after the control it names went invalid.
        when (controlFocus ≢ getControlFocus mgr0) $
            Q.writeQueue (luaQueue env) (LuaUIControlFocusChanged controlFocus)

        when (key ≡ KeyTab ∧ keyState ≡ GLFW.KeyState'Pressed) $ do
            let step = if GLFW.modifierKeysShift mods then prevFocus else nextFocus
            -- #745 review round 5: traverse AND assign in the SAME
            -- atomic transition, computed from whatever is actually
            -- current at mutation time (not the earlier validation
            -- snapshot) — otherwise a concurrent Lua-thread mutation
            -- between that snapshot and this write could invalidate
            -- the chosen target before it's installed.
            newFocus ← atomicModifyIORef' (uiManagerRef env) $ \m →
                case step m (getControlFocus m) of
                    Nothing     → (m, Nothing)
                    Just target → (setControlFocus target m, Just target)
            when (isJust newFocus) $ do
                Q.writeQueue (luaQueue env) (LuaUIControlFocusChanged newFocus)
                markMatched "controlFocusTab"
                writeIORef controlFocusConsumedRef True

        when (key ≡ KeyEscape ∧ keyState ≡ GLFW.KeyState'Pressed ∧ isJust controlFocus) $ do
            atomicModifyIORef' (uiManagerRef env) $ \m → (clearControlFocus m, ())
            Q.writeQueue (luaQueue env) (LuaUIControlFocusChanged Nothing)
            markMatched "controlFocusEscape"
            writeIORef controlFocusConsumedRef True

        case controlFocus of
            Nothing → return ()
            Just elemHandle → do
                let mFocusedEl = getElement elemHandle mgr1
                    -- #745 review round 3: a drag-activation control's
                    -- onClick STARTS A DRAG (slider knob/track,
                    -- scrollbar thumb — see 'UI.Types.ueDragActivation')
                    -- rather than performing a discrete action; firing
                    -- it from Enter/Space would leave that drag state
                    -- latched forever (only a real mouse-up ends it).
                    -- Keyboard activation is for discrete controls only.
                    dragActivation = maybe False ueDragActivation mFocusedEl
                when ((key ≡ KeyEnter ∨ key ≡ KeySpace) ∧ keyState ≡ GLFW.KeyState'Pressed
                     ∧ not dragActivation) $
                    case ueOnClick =≪ mFocusedEl of
                        Nothing → return ()
                        Just callback → do
                            -- Enter/Space activates through the SAME
                            -- callback a real click fires — "their
                            -- logical action" — so every widget family
                            -- already wired via UI.setOnClick gets
                            -- keyboard activation with no widget-side
                            -- change at all.
                            Q.writeQueue (luaQueue env) (LuaUIClickEvent elemHandle callback 0 0)
                            markMatched "controlFocusActivate"
                            writeIORef controlFocusConsumedRef True
                let steppable = maybe False ueSteppable mFocusedEl
                when ((key ≡ KeyLeft ∨ key ≡ KeyRight) ∧ keyState ≡ GLFW.KeyState'Pressed ∧ steppable) $ do
                    Q.writeQueue (luaQueue env)
                        (LuaUIStepEvent elemHandle (if key ≡ KeyLeft then (-1) else 1))
                    markMatched "controlFocusStep"
                    writeIORef controlFocusConsumedRef True

        when (key ≡ KeyGrave ∧ keyState ≡ GLFW.KeyState'Pressed) $ do
            Q.writeQueue (luaQueue env) LuaShellToggle
            markMatched "openShell"
        -- #745 review round 2: a key the control-focus layer above
        -- just consumed (Tab/Shift+Tab, Enter/Space, or a steppable
        -- arrow) must take precedence over gameplay dispatch, exactly
        -- like text focus already does by never reaching this
        -- broadcast at all for the keys ITS branches handle (compare
        -- the TextInputMode/UITextInput cases above, which have no
        -- unconditional onKeyDown broadcast of their own). Mirror that
        -- here: skip the broadcast for a consumed key so no gameplay
        -- onKeyDown handler can also act on it; every unconsumed key
        -- keeps reaching gameplay exactly as before #745.
        controlFocusConsumedFresh ← readIORef controlFocusConsumedRef
        when (key ≠ KeyGrave ∧ not (controlFocusConsumedFresh ∨ alreadyConsumed)) $ do
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
        -- Gameplay-domain keys always broadcast onKeyDown (Grave
        -- excepted above), but "accepted" (#771) is conditional on an
        -- actual consumer: a matched built-in route (escape,
        -- openShell — highest priority, set via markMatched above) or
        -- a bound keybind-registry action. A key with neither is
        -- passed through as genuine 'Nothing', letting
        -- 'recordKeyOutcome' emit its ignored/noop record (domain
        -- "gameplay_key", descriptive reason) instead of a false
        -- "accepted".
        when shouldRecord $ do
            bindings ← readIORef (keyBindingsRef env)
            matched ← readIORef matchedRef
            let handler = case matched of
                    Just m  → Just m
                    Nothing → gameplayKeyHandler glfwKey bindings
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
    -- #745: a key the control-focus layer itself consumed — this
    -- dispatch (Tab/Shift+Tab, Enter/Space, or an arrow on a steppable
    -- focused control) OR an earlier Pressed dispatch for the SAME
    -- held key ('alreadyConsumed') — is withheld from inpKeyStates the
    -- same way a text-focused key already is.
    controlFocusConsumedFresh ← readIORef controlFocusConsumedRef
    let controlFocusConsumed = controlFocusConsumedFresh ∨ alreadyConsumed
        -- #745 review round 3: carry "consumed" through Repeating,
        -- start it on a freshly-consumed Pressed, and drop it on
        -- Released regardless of outcome — mirrors inpPendingUIClick/
        -- inpPendingActivation's own press-to-release lifecycle.
        outgoingConsumedKeys
            | keyState ≡ GLFW.KeyState'Released = Set.delete glfwKey (inpControlFocusConsumedKeys inpSt)
            | controlFocusConsumedFresh = Set.insert glfwKey (inpControlFocusConsumedKeys inpSt)
            | otherwise = inpControlFocusConsumedKeys inpSt
        result = if (textFocused ∨ controlFocusConsumed)
                    ∧ not (shouldTrackKeyStateWhileTextFocused glfwKey keyState)
            then inpSt
            else updateKeyState inpSt glfwKey keyState mods
    return result { inpControlFocusConsumedKeys = outgoingConsumedKeys }
