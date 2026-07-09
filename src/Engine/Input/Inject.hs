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
  ) where

import UPrelude
import Control.Monad (foldM)
import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW
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

-- | Cursor move only — updates hover/pick/tooltip state like a real
--   mouse move.
moveSequence ∷ (Double, Double) → [InputEvent]
moveSequence (x, y) = [InputCursorMove x y]

-- | Full click: mods down → move → press → release → mods up. The
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
    ⧺ modUps mm

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

-- | Release (ends a drag started by 'mouseDownSequence').
mouseUpSequence ∷ (Double, Double) → GLFW.MouseButton
                → ([GLFW.Key], GLFW.ModifierKeys) → [InputEvent]
mouseUpSequence pos@(x, y) btn mm =
    [ InputCursorMove x y
    , InputMouseEvent btn pos GLFW.MouseButtonState'Released
    ]
    ⧺ modUps mm

-- | Wheel scroll. Routed against the CURRENT cursor position (like a
--   real wheel event) — callers targeting a specific element should
--   moveSequence there first.
scrollSequence ∷ Double → Double → [InputEvent]
scrollSequence dx dy = [InputScrollEvent dx dy]

-- | Press + release of one key, bracketed by its modifiers.
keyTapSequence ∷ GLFW.Key → ([GLFW.Key], GLFW.ModifierKeys) → [InputEvent]
keyTapSequence k mm@(_, m) =
    modDowns mm
    ⧺ [ InputKeyEvent k GLFW.KeyState'Pressed m
      , InputKeyEvent k GLFW.KeyState'Released m
      ]
    ⧺ modUps mm

-- | Hold a key down (camera pan etc.). Modifiers go down first and
--   stay held until the matching 'keyUpSequence'.
keyDownSequence ∷ GLFW.Key → ([GLFW.Key], GLFW.ModifierKeys) → [InputEvent]
keyDownSequence k mm@(_, m) =
    modDowns mm ⧺ [InputKeyEvent k GLFW.KeyState'Pressed m]

-- | Release a held key (and its modifiers, after it).
keyUpSequence ∷ GLFW.Key → ([GLFW.Key], GLFW.ModifierKeys) → [InputEvent]
keyUpSequence k mm@(_, m) =
    [InputKeyEvent k GLFW.KeyState'Released m] ⧺ modUps mm

-- | Character input, one char event per character — the GLFW *char*
--   path text fields listen on, distinct from key events. Only a
--   focused text field consumes these (same as real typing).
typeSequence ∷ Text → [InputEvent]
typeSequence = map InputCharEvent . T.unpack

-- | Push a synthesized sequence into the input queue and block until
--   the input thread has drained it (or the timeout, in microseconds,
--   elapses; True = consumed). Draining means the events entered the
--   normal processing path — downstream Lua broadcasts they trigger
--   are dispatched on the Lua thread afterwards, exactly like real
--   input (and cannot be awaited from a console verb, which runs on
--   that same thread).
injectEvents ∷ Q.Queue InputEvent → Int → [InputEvent] → IO Bool
injectEvents q timeoutMicros evs = do
    mapM_ (Q.writeQueue q) evs
    Q.waitQueueEmpty timeoutMicros q
