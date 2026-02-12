{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Input.Types where

import UPrelude
import qualified Data.Map as Map
import qualified Graphics.UI.GLFW as GLFW

----------- Input State -----------------------------------

data InputState = InputState 
    { inpKeyStates ∷ Map.Map GLFW.Key KeyState
    , inpMousePos  ∷ (Double, Double)    -- ^ Current mouse position
    , inpMouseDelta ∷ (Double, Double)   -- ^ Mouse movement since last frame
    , inpMouseBtns ∷ Map.Map GLFW.MouseButton Bool
    , inpScrollDelta ∷ (Double, Double)  -- ^ Scroll wheel delta
    , inpWindowFocused ∷ Bool            -- ^ Is window currently focused
    } deriving (Show)

----------- Input Events ----------------------------------

data InputEvent 
    = InputKeyEvent 
        { ikeKey      ∷ GLFW.Key        -- ^ The key being pressed/released
        , ikeKeyState ∷ GLFW.KeyState   -- ^ Whether it's pressed or released
        , ikeMods     ∷ GLFW.ModifierKeys -- ^ Modifier keys (shift, ctrl, etc)
        }
    | InputCharEvent
        { iceChar ∷ Char                 -- ^ Character input
        }
    | InputWindowEvent
        { iweWinEvent ∷ WindowEvent     -- ^ Window-related events
        }
    | InputMouseEvent
        { imeMouseBtn   ∷ GLFW.MouseButton  -- ^ Mouse button
        , imeMousePos   ∷ (Double, Double)  -- ^ Cursor position
        , imeMouseState ∷ GLFW.MouseButtonState -- ^ Button state
        }
    | InputCursorMove Double Double
    | InputScrollEvent
        { iseScrollX   ∷ Double         -- ^ Scroll X offset
        , iseScrollY    ∷ Double        -- ^ Scroll Y offset
        }
    deriving (Show, Eq)

data WindowEvent 
    = WindowResize Int Int        -- ^ New width and height
    | FramebufferResize Int Int -- ^ New framebuffer width and height
    | WindowClose                 -- ^ Window close request
    | WindowFocus Bool           -- ^ Window focus gained/lost
    | WindowMinimize Bool        -- ^ Window minimized/restored
    deriving (Show, Eq)

----------- Key State -------------------------------------

data KeyState = KeyState
    { keyPressed ∷ Bool         -- ^ Is the key currently pressed
    , keyMods    ∷ GLFW.ModifierKeys  -- ^ Active modifiers when pressed
    , keyTime    ∷ Double       -- ^ Time of last state change
    } deriving (Show, Eq)

----------- Key Types -------------------------------------

data Key
    = KeyA | KeyB | KeyC | KeyD | KeyE | KeyF | KeyG | KeyH | KeyI | KeyJ
    | KeyK | KeyL | KeyM | KeyN | KeyO | KeyP | KeyQ | KeyR | KeyS | KeyT
    | KeyU | KeyV | KeyW | KeyX | KeyY | KeyZ
    | Key0 | Key1 | Key2 | Key3 | Key4 | Key5 | Key6 | Key7 | Key8 | Key9
    | KeySpace | KeyEnter | KeyEscape | KeyTab | KeyBackspace | KeyDelete
    | KeyUp | KeyDown | KeyLeft | KeyRight | KeyHome | KeyEnd
    | KeyShift | KeyCtrl | KeyAlt | KeySuper
    | KeyGrave | KeyMinus | KeyEqual | KeyComma | KeyPeriod
    | KeyF1 | KeyF2 | KeyF3 | KeyF4 | KeyF5 | KeyF6
    | KeyF7 | KeyF8 | KeyF9 | KeyF10 | KeyF11 | KeyF12
    | KeyUnknown
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

----------- Conversions -----------------------------------

fromGLFWKey ∷ GLFW.Key → Key
fromGLFWKey GLFW.Key'A = KeyA
fromGLFWKey GLFW.Key'B = KeyB
fromGLFWKey GLFW.Key'C = KeyC
fromGLFWKey GLFW.Key'D = KeyD
fromGLFWKey GLFW.Key'E = KeyE
fromGLFWKey GLFW.Key'F = KeyF
fromGLFWKey GLFW.Key'G = KeyG
fromGLFWKey GLFW.Key'H = KeyH
fromGLFWKey GLFW.Key'I = KeyI
fromGLFWKey GLFW.Key'J = KeyJ
fromGLFWKey GLFW.Key'K = KeyK
fromGLFWKey GLFW.Key'L = KeyL
fromGLFWKey GLFW.Key'M = KeyM
fromGLFWKey GLFW.Key'N = KeyN
fromGLFWKey GLFW.Key'O = KeyO
fromGLFWKey GLFW.Key'P = KeyP
fromGLFWKey GLFW.Key'Q = KeyQ
fromGLFWKey GLFW.Key'R = KeyR
fromGLFWKey GLFW.Key'S = KeyS
fromGLFWKey GLFW.Key'T = KeyT
fromGLFWKey GLFW.Key'U = KeyU
fromGLFWKey GLFW.Key'V = KeyV
fromGLFWKey GLFW.Key'W = KeyW
fromGLFWKey GLFW.Key'X = KeyX
fromGLFWKey GLFW.Key'Y = KeyY
fromGLFWKey GLFW.Key'Z = KeyZ
fromGLFWKey GLFW.Key'0 = Key0
fromGLFWKey GLFW.Key'1 = Key1
fromGLFWKey GLFW.Key'2 = Key2
fromGLFWKey GLFW.Key'3 = Key3
fromGLFWKey GLFW.Key'4 = Key4
fromGLFWKey GLFW.Key'5 = Key5
fromGLFWKey GLFW.Key'6 = Key6
fromGLFWKey GLFW.Key'7 = Key7
fromGLFWKey GLFW.Key'8 = Key8
fromGLFWKey GLFW.Key'9 = Key9
fromGLFWKey GLFW.Key'Space = KeySpace
fromGLFWKey GLFW.Key'Enter = KeyEnter
fromGLFWKey GLFW.Key'Escape = KeyEscape
fromGLFWKey GLFW.Key'Tab = KeyTab
fromGLFWKey GLFW.Key'Backspace = KeyBackspace
fromGLFWKey GLFW.Key'Delete = KeyDelete
fromGLFWKey GLFW.Key'Up = KeyUp
fromGLFWKey GLFW.Key'Down = KeyDown
fromGLFWKey GLFW.Key'Left = KeyLeft
fromGLFWKey GLFW.Key'Right = KeyRight
fromGLFWKey GLFW.Key'LeftShift = KeyShift
fromGLFWKey GLFW.Key'RightShift = KeyShift
fromGLFWKey GLFW.Key'LeftControl = KeyCtrl
fromGLFWKey GLFW.Key'RightControl = KeyCtrl
fromGLFWKey GLFW.Key'LeftAlt = KeyAlt
fromGLFWKey GLFW.Key'RightAlt = KeyAlt
fromGLFWKey GLFW.Key'GraveAccent = KeyGrave
fromGLFWKey GLFW.Key'Minus = KeyMinus
fromGLFWKey GLFW.Key'Equal = KeyEqual
fromGLFWKey GLFW.Key'F1 = KeyF1
fromGLFWKey GLFW.Key'F2 = KeyF2
fromGLFWKey GLFW.Key'F3 = KeyF3
fromGLFWKey GLFW.Key'F4 = KeyF4
fromGLFWKey GLFW.Key'F5 = KeyF5
fromGLFWKey GLFW.Key'F6 = KeyF6
fromGLFWKey GLFW.Key'F7 = KeyF7
fromGLFWKey GLFW.Key'F8 = KeyF8
fromGLFWKey GLFW.Key'F9 = KeyF9
fromGLFWKey GLFW.Key'F10 = KeyF10
fromGLFWKey GLFW.Key'F11 = KeyF11
fromGLFWKey GLFW.Key'F12 = KeyF12
fromGLFWKey _ = KeyUnknown

keyToText ∷ Key → Text
keyToText KeyA = "A"
keyToText KeyB = "B"
keyToText KeyC = "C"
keyToText KeyD = "D"
keyToText KeyE = "E"
keyToText KeyF = "F"
keyToText KeyG = "G"
keyToText KeyH = "H"
keyToText KeyI = "I"
keyToText KeyJ = "J"
keyToText KeyK = "K"
keyToText KeyL = "L"
keyToText KeyM = "M"
keyToText KeyN = "N"
keyToText KeyO = "O"
keyToText KeyP = "P"
keyToText KeyQ = "Q"
keyToText KeyR = "R"
keyToText KeyS = "S"
keyToText KeyT = "T"
keyToText KeyU = "U"
keyToText KeyV = "V"
keyToText KeyW = "W"
keyToText KeyX = "X"
keyToText KeyY = "Y"
keyToText KeyZ = "Z"
keyToText Key0 = "0"
keyToText Key1 = "1"
keyToText Key2 = "2"
keyToText Key3 = "3"
keyToText Key4 = "4"
keyToText Key5 = "5"
keyToText Key6 = "6"
keyToText Key7 = "7"
keyToText Key8 = "8"
keyToText Key9 = "9"
keyToText KeySpace = "Space"
keyToText KeyEnter = "Enter"
keyToText KeyEscape = "Escape"
keyToText KeyTab = "Tab"
keyToText KeyBackspace = "Backspace"
keyToText KeyDelete = "Delete"
keyToText KeyUp = "Up"
keyToText KeyDown = "Down"
keyToText KeyLeft = "Left"
keyToText KeyRight = "Right"
keyToText KeyShift = "Shift"
keyToText KeyCtrl = "Ctrl"
keyToText KeyAlt = "Alt"
keyToText KeySuper = "Super"
keyToText KeyGrave = "Grave"
keyToText KeyMinus = "Minus"
keyToText KeyEqual = "Equal"
keyToText KeyComma = "Comma"
keyToText KeyPeriod = "Period"
keyToText KeyF1 = "F1"
keyToText KeyF2 = "F2"
keyToText KeyF3 = "F3"
keyToText KeyF4 = "F4"
keyToText KeyF5 = "F5"
keyToText KeyF6 = "F6"
keyToText KeyF7 = "F7"
keyToText KeyF8 = "F8"
keyToText KeyF9 = "F9"
keyToText KeyF10 = "F10"
keyToText KeyF11 = "F11"
keyToText KeyF12 = "F12"
keyToText KeyUnknown = "Unknown"

textToKey ∷ Text → Maybe Key
textToKey "A" = Just KeyA
textToKey "B" = Just KeyB
textToKey "C" = Just KeyC
textToKey "D" = Just KeyD
textToKey "E" = Just KeyE
textToKey "F" = Just KeyF
textToKey "G" = Just KeyG
textToKey "H" = Just KeyH
textToKey "I" = Just KeyI
textToKey "J" = Just KeyJ
textToKey "K" = Just KeyK
textToKey "L" = Just KeyL
textToKey "M" = Just KeyM
textToKey "N" = Just KeyN
textToKey "O" = Just KeyO
textToKey "P" = Just KeyP
textToKey "Q" = Just KeyQ
textToKey "R" = Just KeyR
textToKey "S" = Just KeyS
textToKey "T" = Just KeyT
textToKey "U" = Just KeyU
textToKey "V" = Just KeyV
textToKey "W" = Just KeyW
textToKey "X" = Just KeyX
textToKey "Y" = Just KeyY
textToKey "Z" = Just KeyZ
textToKey "Space" = Just KeySpace
textToKey "Enter" = Just KeyEnter
textToKey "Escape" = Just KeyEscape
textToKey "Tab" = Just KeyTab
textToKey "Backspace" = Just KeyBackspace
textToKey "Grave" = Just KeyGrave
textToKey _ = Nothing

----------- Defaults --------------------------------------

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
    , inpWindowFocused = True
    }
