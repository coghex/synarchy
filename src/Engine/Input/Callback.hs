{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Input.Callback where

import UPrelude
import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent.STM.TQueue
import Data.IORef (IORef, readIORef)
import Engine.Input.Types
import Engine.Core.State
import Engine.Core.Queue

-- | Sets up all GLFW callbacks for a window
setupCallbacks ∷ GLFW.Window → IORef EngineLifecycle → Queue InputEvent → IO ()
setupCallbacks window el queue = do
    GLFW.setKeyCallback window
        (Just $ keyCallback queue el)
    GLFW.setCharCallback window
        (Just $ charCallback queue)
    GLFW.setMouseButtonCallback window 
        (Just $ mouseCallback queue)
    GLFW.setCursorPosCallback window 
        (Just $ cursorCallback queue)
    GLFW.setScrollCallback window 
        (Just $ scrollCallback queue)
    GLFW.setWindowSizeCallback window 
        (Just $ resizeCallback queue)
    GLFW.setFramebufferSizeCallback window
        (Just $ framebufferSizeCallback queue)
    GLFW.setWindowFocusCallback window 
        (Just $ focusCallback queue)

-- | clears all GLFW callbacks for a window
clearGLFWCallbacks ∷ GLFW.Window → IO ()
clearGLFWCallbacks window = do
    GLFW.setKeyCallback window Nothing
    GLFW.setCharCallback window Nothing
    GLFW.setMouseButtonCallback window Nothing
    GLFW.setCursorPosCallback window Nothing
    GLFW.setScrollCallback window Nothing
    GLFW.setWindowSizeCallback window Nothing
    GLFW.setFramebufferSizeCallback window Nothing
    GLFW.setWindowFocusCallback window Nothing

-- | Keyboard input callback
keyCallback ∷ Queue InputEvent → IORef EngineLifecycle → GLFW.Window
            → GLFW.Key → Int → GLFW.KeyState → GLFW.ModifierKeys → IO ()
keyCallback queue el _win key _scancode keyState mods = do
    -- discard any input pressed during startup
  lifecycle ← readIORef el
  case lifecycle of
    EngineRunning → writeQueue queue $ InputKeyEvent key keyState mods
    _             → return ()

-- | Character input callback
charCallback ∷ Queue InputEvent → GLFW.Window → Char → IO ()
charCallback queue _win char = do
    writeQueue queue $ InputCharEvent char

-- | Mouse button callback
mouseCallback ∷ Queue InputEvent → GLFW.Window → GLFW.MouseButton
              → GLFW.MouseButtonState → GLFW.ModifierKeys → IO ()
mouseCallback queue _win btn state mods = do
    (x, y) ← GLFW.getCursorPos _win
    writeQueue queue $ InputMouseEvent btn (x, y) state

-- | Cursor position callback
cursorCallback ∷ Queue InputEvent → GLFW.Window → Double → Double → IO ()
cursorCallback queue _win x y = do
    writeQueue queue $ InputCursorMove x y

-- | Scroll wheel callback
scrollCallback ∷ Queue InputEvent → GLFW.Window → Double → Double → IO ()
scrollCallback queue _win x y =
    writeQueue queue $ InputScrollEvent x y

-- | Window resize callback
resizeCallback ∷ Queue InputEvent → GLFW.Window → Int → Int → IO ()
resizeCallback queue _win w h =
    writeQueue queue $ InputWindowEvent $ WindowResize w h

-- | Framebuffer size callback
framebufferSizeCallback ∷ Queue InputEvent → GLFW.Window → Int → Int → IO ()
framebufferSizeCallback queue _win w h =
    writeQueue queue $ InputWindowEvent $ FramebufferResize w h

-- | Window focus callback
focusCallback ∷ Queue InputEvent → GLFW.Window → Bool → IO ()
focusCallback queue _win focused =
    writeQueue queue $ InputWindowEvent $ WindowFocus focused

-- | Helper function to create empty callbacks for initialization
emptyCallback ∷ IO ()
emptyCallback = return ()
