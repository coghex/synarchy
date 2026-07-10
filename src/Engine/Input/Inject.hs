{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Synthetic input injection for the input.* console verbs (#644) —
--   the actor-output channel of the UX playtest harness (#641).
--
--   Events are synthesized in the engine's internal 'InputEvent'
--   representation and pushed into the SAME queue the GLFW callbacks
--   populate ("Engine.Input.Callback"), so the input thread — and
--   everything downstream of it — cannot tell a synthetic event from a
--   real one. GLFW itself is never touched (its cursor/event APIs are
--   main-thread-only; we inject from the Lua thread, above the OS
--   layer that real-OS-click testing would cover).
--
--   Positions given to the sequence builders are WINDOW coordinates —
--   the space GLFW callbacks report and every internal consumer
--   expects. Callers converting from screenshot/framebuffer pixels
--   (F1's space, #643) go through 'fbToWindow' first.
--
--   Routing invariants are preserved by construction: every click
--   carries its cursor move (hover/pick state stays coherent), every
--   press has a matching release available, and modifier lists
--   bracket the action with real modifier-key events — so both
--   consumers that read the event's 'GLFW.ModifierKeys' field and
--   consumers that poll held-key state (engine.isKeyDown's shift-click
--   checks) observe the modifier exactly as they would a physical one.
--
--   Modifier LIFETIME (#697): the releases are not injected inline —
--   the input thread drains a whole sequence before the Lua thread
--   runs any callback it produced, so inline releases would already be
--   the published state by callback time. Instead the releases ride an
--   'InputFollowup' fence at the end of the sequence: input thread →
--   'LuaInjectFollowup' → Lua thread → back into the input queue. Both
--   queues are FIFO, so the releases re-enter strictly after the
--   action's callbacks have run, and are then processed like any other
--   key events (no stuck state).
--
--   Sequential ack boundary (#727): the fence relay above only orders
--   messages WITHIN one sequence — the public ack (see
--   'Engine.Scripting.Lua.API.InputInject.injectAndSettle') used to
--   fire as soon as the primary batch drained, before the fence had
--   even reached the Lua queue, let alone been dispatched. A second
--   verb issued right after that ack — the very next debug-console
--   command, or the next line of a script calling input.* twice in a
--   row — could then run while the first verb's modifier was still
--   published held (a plain second action inheriting it) or get its
--   own hold clipped by the first verb's still-pending release. Fixed
--   by making the ack synchronously resolve the whole lifetime: it now
--   drains and dispatches every Lua message the sequence produced,
--   and — if that included a fence — waits for the re-injected release
--   to drain too and dispatches its broadcast, all before returning.
--   So by the time one input.* call acks, nothing belonging to it is
--   left pending on either queue for the next call to race.
module Engine.Input.Inject
  ( resolveButton
  , resolveMods
  , resolveKeyName
  , fbToWindow
  , noMods
  , moveSequence
  , clickSequence
  , mouseDownSequence
  , mouseUpSequence
  , scrollSequence
  , keyTapSequence
  , keyDownSequence
  , keyUpSequence
  , typeSequence
  , injectEvents
  , waitEventsProcessed
  ) where

import UPrelude
import Control.Monad (foldM)
import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TVar (TVar, readTVarIO)
import qualified Engine.Core.Queue as Q
import Engine.Input.Bindings (parseKeyName)
import Engine.Input.Types (InputEvent(..))

-- | \"left\" | \"right\" | \"middle\" (case-insensitive) → GLFW button.
resolveButton ∷ Text → Maybe GLFW.MouseButton
resolveButton t = case T.toLower t of
    "left"   → Just GLFW.MouseButton'1
    "right"  → Just GLFW.MouseButton'2
    "middle" → Just GLFW.MouseButton'3
    _        → Nothing

noMods ∷ GLFW.ModifierKeys
noMods = GLFW.ModifierKeys False False False False False False

-- | Modifier names (\"shift\" | \"ctrl\" | \"alt\" | \"super\",
--   case-insensitive) → the physical keys to hold around the action
--   plus the 'GLFW.ModifierKeys' flags to stamp on key events. Left
--   variants are pressed, matching what a physical press of a merged
--   modifier does. Nothing on any unknown name.
resolveMods ∷ [Text] → Maybe ([GLFW.Key], GLFW.ModifierKeys)
resolveMods = foldM step ([], noMods)
  where
    step (ks, m) name = case T.toLower name of
        "shift" → Just (ks ⧺ [GLFW.Key'LeftShift]
                       , m { GLFW.modifierKeysShift = True })
        "ctrl"  → Just (ks ⧺ [GLFW.Key'LeftControl]
                       , m { GLFW.modifierKeysControl = True })
        "alt"   → Just (ks ⧺ [GLFW.Key'LeftAlt]
                       , m { GLFW.modifierKeysAlt = True })
        "super" → Just (ks ⧺ [GLFW.Key'LeftSuper]
                       , m { GLFW.modifierKeysSuper = True })
        _       → Nothing

-- | Key name → the GLFW key to synthesize, via the canonical keybind
--   vocabulary ('parseKeyName' — 'keyToText' names, merged modifiers,
--   and the side-specific aliases). A merged name (\"Shift\") presses
--   its first (left) physical key, like a real keystroke would.
resolveKeyName ∷ Text → Maybe GLFW.Key
resolveKeyName name = case parseKeyName name of
    (k:_) → Just k
    []    → Nothing

-- | Framebuffer pixels (F1's screenshot space) → window coordinates
--   (the input pipeline's space). On HiDPI displays these differ by
--   the DPI scale; injecting framebuffer coords raw would land every
--   click at 2× the intended position. Nothing when either size is
--   degenerate (minimized window / headless zeros).
fbToWindow ∷ (Int, Int) → (Int, Int) → (Double, Double)
           → Maybe (Double, Double)
fbToWindow (winW, winH) (fbW, fbH) (x, y)
    | winW ≤ 0 ∨ winH ≤ 0 ∨ fbW ≤ 0 ∨ fbH ≤ 0 = Nothing
    | otherwise = Just
        ( x * fromIntegral winW / fromIntegral fbW
        , y * fromIntegral winH / fromIntegral fbH )

modDowns ∷ ([GLFW.Key], GLFW.ModifierKeys) → [InputEvent]
modDowns (ks, m) = [ InputKeyEvent k GLFW.KeyState'Pressed m | k ← ks ]

modUps ∷ ([GLFW.Key], GLFW.ModifierKeys) → [InputEvent]
modUps (ks, _) =
    [ InputKeyEvent k GLFW.KeyState'Released noMods | k ← reverse ks ]

-- | Modifier releases, deferred behind the sequence's Lua broadcasts
--   (#697). Inline releases are drained in the same batch as the
--   action, so by the time the Lua thread runs the click/key callbacks
--   the published state already shows the modifier up — a synthetic
--   shift-click reads as a plain click to engine.isKeyDown pollers.
--   Wrapping the releases in 'InputFollowup' routes them input thread →
--   Lua thread → input thread; both queues are FIFO, so they are
--   re-injected only after every callback of the fenced action has
--   run, then processed normally (nothing sticks). Empty when there
--   are no modifiers, keeping plain sequences identical.
deferModUps ∷ ([GLFW.Key], GLFW.ModifierKeys) → [InputEvent]
deferModUps mm = case modUps mm of
    []  → []
    ups → [InputFollowup ups]

-- | Cursor move only — updates hover/pick/tooltip state like a real
--   mouse move.
moveSequence ∷ (Double, Double) → [InputEvent]
moveSequence (x, y) = [InputCursorMove x y]

-- | Full click: mods down → move → press → release → mods up (the
--   ups deferred behind the click's own Lua callbacks, #697). The
--   move rides along so the hit-test and any hover-derived state see
--   the cursor where the click lands.
clickSequence ∷ (Double, Double) → GLFW.MouseButton
              → ([GLFW.Key], GLFW.ModifierKeys) → [InputEvent]
clickSequence pos@(x, y) btn mm =
    modDowns mm
    ⧺ [ InputCursorMove x y
      , InputMouseEvent btn pos GLFW.MouseButtonState'Pressed
      , InputMouseEvent btn pos GLFW.MouseButtonState'Released
      ]
    ⧺ deferModUps mm

-- | Press without release (drags, holds). Modifier keys go down here
--   and come back up in the matching 'mouseUpSequence', so a
--   shift-drag holds shift across the whole gesture.
mouseDownSequence ∷ (Double, Double) → GLFW.MouseButton
                  → ([GLFW.Key], GLFW.ModifierKeys) → [InputEvent]
mouseDownSequence pos@(x, y) btn mm =
    modDowns mm
    ⧺ [ InputCursorMove x y
      , InputMouseEvent btn pos GLFW.MouseButtonState'Pressed
      ]

-- | Release (ends a drag started by 'mouseDownSequence'). Modifier
--   ups are deferred so the release's own callbacks still observe the
--   modifier held (#697) — like a human letting go of shift only after
--   the mouse button.
mouseUpSequence ∷ (Double, Double) → GLFW.MouseButton
                → ([GLFW.Key], GLFW.ModifierKeys) → [InputEvent]
mouseUpSequence pos@(x, y) btn mm =
    [ InputCursorMove x y
    , InputMouseEvent btn pos GLFW.MouseButtonState'Released
    ]
    ⧺ deferModUps mm

-- | Wheel scroll. Routed against the CURRENT cursor position (like a
--   real wheel event) — callers targeting a specific element should
--   moveSequence there first.
scrollSequence ∷ Double → Double → [InputEvent]
scrollSequence dx dy = [InputScrollEvent dx dy]

-- | Press + release of one key, bracketed by its modifiers (releases
--   deferred behind the tap's Lua callbacks, #697).
keyTapSequence ∷ GLFW.Key → ([GLFW.Key], GLFW.ModifierKeys) → [InputEvent]
keyTapSequence k mm@(_, m) =
    modDowns mm
    ⧺ [ InputKeyEvent k GLFW.KeyState'Pressed m
      , InputKeyEvent k GLFW.KeyState'Released m
      ]
    ⧺ deferModUps mm

-- | Hold a key down (camera pan etc.). Modifiers go down first and
--   stay held until the matching 'keyUpSequence'.
keyDownSequence ∷ GLFW.Key → ([GLFW.Key], GLFW.ModifierKeys) → [InputEvent]
keyDownSequence k mm@(_, m) =
    modDowns mm ⧺ [InputKeyEvent k GLFW.KeyState'Pressed m]

-- | Release a held key (and its modifiers after it, deferred behind
--   the release's own Lua callbacks, #697).
keyUpSequence ∷ GLFW.Key → ([GLFW.Key], GLFW.ModifierKeys) → [InputEvent]
keyUpSequence k mm@(_, m) =
    [InputKeyEvent k GLFW.KeyState'Released m] ⧺ deferModUps mm

-- | Character input, one char event per character — the GLFW *char*
--   path text fields listen on, distinct from key events. Only a
--   focused text field consumes these (same as real typing).
typeSequence ∷ Text → [InputEvent]
typeSequence = map InputCharEvent . T.unpack

-- | Block until 'Engine.Core.State.inputProcessedRef' has counted at
--   least 'target' fully-processed events (or the timeout, in
--   microseconds, elapses; True = reached). Deliberately NOT a wait on
--   the input queue becoming empty (#727): the input thread dequeues
--   an event in one STM transaction and only afterwards, in IO, runs
--   its side effects (state publish + any 'luaQueue' write) — "queue
--   empty" can be observed in the gap between those two, before the
--   LAST event's effects have actually landed. The counter increments
--   only once those effects are done
--   (see 'Engine.Input.Thread.processInputs'), so waiting on it is a
--   genuine completion signal instead of a proxy that can race ahead.
waitEventsProcessed ∷ TVar Int → Int → Int → IO Bool
waitEventsProcessed counterRef target timeoutMicros = do
    delayVar ← STM.registerDelay timeoutMicros
    STM.atomically $ STM.orElse
        (do n ← STM.readTVar counterRef
            STM.check (n ≥ target)
            return True)
        (do timedOut ← STM.readTVar delayVar
            STM.check timedOut
            return False)

-- | Push a synthesized sequence into the input queue and block until
--   the input thread has fully PROCESSED it (or the timeout, in
--   microseconds, elapses; True = processed) — see
--   'waitEventsProcessed' for why this counts events rather than
--   checking queue emptiness. Processed means the events entered the
--   normal processing path — downstream Lua broadcasts they trigger
--   are dispatched on the Lua thread afterwards, exactly like real
--   input (and cannot be awaited from a console verb, which runs on
--   that same thread).
injectEvents ∷ TVar Int → Q.Queue InputEvent → Int → [InputEvent] → IO Bool
injectEvents counterRef q timeoutMicros evs = do
    before ← readTVarIO counterRef
    mapM_ (Q.writeQueue q) evs
    waitEventsProcessed counterRef (before + length evs) timeoutMicros
