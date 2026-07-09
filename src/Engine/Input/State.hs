{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Input.State where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Graphics.UI.GLFW as GLFW
import Engine.Core.State
import Engine.Scripting.Lua.Types
import qualified Engine.Core.Queue as Q
import Engine.Input.Types

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
