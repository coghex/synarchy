{-# LANGUAGE Strict #-}
module Engine.Input.Callback where

import UPrelude
import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent.STM.TQueue
import Engine.Input.Types
import Engine.Core.Queue

-- | Sets up all GLFW callbacks for a window
setupCallbacks ∷ GLFW.Window → Queue InputEvent → IO ()
setupCallbacks window queue = do
    GLFW.setKeyCallback window 
        (Just $ keyCallback queue)
    GLFW.setMouseButtonCallback window 
        (Just $ mouseCallback queue)
    GLFW.setCursorPosCallback window 
        (Just $ cursorCallback queue)
    GLFW.setScrollCallback window 
        (Just $ scrollCallback queue)
    GLFW.setWindowSizeCallback window 
        (Just $ resizeCallback queue)
    GLFW.setWindowFocusCallback window 
        (Just $ focusCallback queue)

-- | Keyboard input callback
keyCallback ∷ Queue InputEvent → GLFW.Window → GLFW.Key → Int
            → GLFW.KeyState → GLFW.ModifierKeys → IO ()
keyCallback queue _win key _scancode keyState mods = 
    writeQueue queue $ InputKeyEvent key keyState mods

-- | Mouse button callback
mouseCallback ∷ Queue InputEvent → GLFW.Window → GLFW.MouseButton
              → GLFW.MouseButtonState → GLFW.ModifierKeys → IO ()
mouseCallback queue _win btn state mods = do
    (x, y) ← GLFW.getCursorPos _win
    writeQueue queue $ InputMouseEvent btn (x, y) state

-- | Cursor position callback
cursorCallback ∷ Queue InputEvent → GLFW.Window → Double → Double → IO ()
cursorCallback queue _win x y = do
    let pos = (x, y)
    writeQueue queue $ InputMouseEvent GLFW.MouseButton'1 pos GLFW.MouseButtonState'Released

-- | Scroll wheel callback
scrollCallback ∷ Queue InputEvent → GLFW.Window → Double → Double → IO ()
scrollCallback queue _win x y =
    writeQueue queue $ InputScrollEvent x y

-- | Window resize callback
resizeCallback ∷ Queue InputEvent → GLFW.Window → Int → Int → IO ()
resizeCallback queue _win w h =
    writeQueue queue $ InputWindowEvent $ WindowResize w h

-- | Window focus callback
focusCallback ∷ Queue InputEvent → GLFW.Window → Bool → IO ()
focusCallback queue _win focused =
    writeQueue queue $ InputWindowEvent $ WindowFocus focused

-- | Helper function to create empty callbacks for initialization
emptyCallback ∷ IO ()
emptyCallback = return ()
