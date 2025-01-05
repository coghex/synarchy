-- File: Engine/Input/Types.hs
{-# LANGUAGE Strict #-}
module Engine.Input.Types where

import UPrelude
import qualified Data.Map as Map
import qualified Graphics.UI.GLFW as GLFW

data InputState = InputState 
    { inpKeyStates ∷ Map.Map GLFW.Key KeyState
    , inpMousePos  ∷ (Double, Double)    -- ^ Current mouse position
    , inpMouseDelta ∷ (Double, Double)   -- ^ Mouse movement since last frame
    , inpMouseBtns ∷ Map.Map GLFW.MouseButton Bool
    , inpScrollDelta ∷ (Double, Double)  -- ^ Scroll wheel delta
    , inpWindowSize ∷ (Int, Int)         -- ^ Current window dimensions
    , inpWindowFocused ∷ Bool            -- ^ Is window currently focused
    } deriving (Show)

-- | Main type for all input events
data InputEvent 
    = InputKeyEvent 
        { inpKey      ∷ GLFW.Key        -- ^ The key being pressed/released
        , inpKeyState ∷ GLFW.KeyState   -- ^ Whether it's pressed or released
        , inpMods     ∷ GLFW.ModifierKeys -- ^ Modifier keys (shift, ctrl, etc)
        }
    | InputWindowEvent
        { inpWinEvent ∷ WindowEvent     -- ^ Window-related events
        }
    | InputMouseEvent
        { inpMouseBtn   ∷ GLFW.MouseButton  -- ^ Mouse button
        , inpMousePos   ∷ (Double, Double)  -- ^ Cursor position
        , inpMouseState ∷ GLFW.MouseButtonState -- ^ Button state
        }
    | InputScrollEvent
        { inpScrollX   ∷ Double         -- ^ Scroll X offset
        , inpScrollY   ∷ Double         -- ^ Scroll Y offset
        }
    deriving (Show, Eq)

-- | Window-specific events
data WindowEvent 
    = WindowResize Int Int        -- ^ New width and height
    | WindowClose                 -- ^ Window close request
    | WindowFocus Bool           -- ^ Window focus gained/lost
    | WindowMinimize Bool        -- ^ Window minimized/restored
    deriving (Show, Eq)

-- | Helper type for keyboard state tracking
data KeyState = KeyState
    { keyPressed ∷ Bool         -- ^ Is the key currently pressed
    , keyMods    ∷ GLFW.ModifierKeys  -- ^ Active modifiers when pressed
    , keyTime    ∷ Double      -- ^ Time of last state change
    } deriving (Show, Eq)

data Key
    = KeyA | KeyB | KeyC | KeyD | KeyE | KeyF | KeyG | KeyH | KeyI | KeyJ
    | KeyK | KeyL | KeyM | KeyN | KeyO | KeyP | KeyQ | KeyR | KeyS | KeyT
    | KeyU | KeyV | KeyW | KeyX | KeyY | KeyZ
    | Key0 | Key1 | Key2 | Key3 | Key4 | Key5 | Key6 | Key7 | Key8 | Key9
    | KeySpace | KeyEscape | KeyEnter | KeyTab | KeyBackspace
    | KeyUp | KeyDown | KeyLeft | KeyRight
    | KeyLShift | KeyRShift | KeyLCtrl | KeyRCtrl | KeyLAlt | KeyRAlt
    | KeyNULL
    deriving (Eq, Show, Ord)

defaultKeyState ∷ KeyState
defaultKeyState = KeyState
    { keyPressed = False
    , keyMods    = GLFW.ModifierKeys
        { GLFW.modifierKeysShift    = False
        , GLFW.modifierKeysControl  = False
        , GLFW.modifierKeysAlt      = False
        , GLFW.modifierKeysSuper    = False
        , GLFW.modifierKeysCapsLock = False
        , GLFW.modifierKeysNumLock  = False
        }
    , keyTime    = 0.0
    }

defaultInputState ∷ InputState
defaultInputState = InputState
    { inpKeyStates = Map.empty
    , inpMousePos = (0.0, 0.0)
    , inpMouseDelta = (0.0, 0.0)
    , inpMouseBtns = Map.empty
    , inpScrollDelta = (0.0, 0.0)
    , inpWindowSize = (800, 600)         -- default size, adjust as needed
    , inpWindowFocused = True
    }
