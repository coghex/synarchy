{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Input.Callback where

import UPrelude
import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent.STM.TQueue
import Data.IORef (IORef, readIORef)
import Engine.Input.Types
import Engine.Core.State
import Engine.Core.Queue

setupCallbacks ∷ GLFW.Window → IORef EngineLifecycle → Queue InputEvent → IO ()
setupCallbacks window el queue = do
    GLFW.setKeyCallback window
        (Just $ keyCallback queue el)
    GLFW.setCharCallback window
        (Just $ charCallback queue el)
    GLFW.setMouseButtonCallback window
        (Just $ mouseCallback queue el)
    GLFW.setCursorPosCallback window
        (Just $ cursorCallback queue)
    GLFW.setScrollCallback window
        (Just $ scrollCallback queue el)
    GLFW.setWindowSizeCallback window 
        (Just $ resizeCallback queue)
    GLFW.setFramebufferSizeCallback window
        (Just $ framebufferSizeCallback queue)
    GLFW.setWindowFocusCallback window
        (Just $ focusCallback queue)
    GLFW.setWindowIconifyCallback window
        (Just $ iconifyCallback queue)

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
    GLFW.setWindowIconifyCallback window Nothing

-- | Discard user-intent input (keys, chars, clicks, scroll) unless the
--   engine is running, to prevent stale events queued during startup.
--   Window-state callbacks (cursor position, resize, focus) stay
--   ungated — they sync state, and dropping them would leave it stale.
whenRunning ∷ IORef EngineLifecycle → IO () → IO ()
whenRunning el act = do
    lifecycle ← readIORef el
    when (lifecycle ≡ EngineRunning) act

keyCallback ∷ Queue InputEvent → IORef EngineLifecycle → GLFW.Window
            → GLFW.Key → Int → GLFW.KeyState → GLFW.ModifierKeys → IO ()
keyCallback queue el _win key _scancode keyState mods = whenRunning el $
    writeQueue queue $ InputKeyEvent key keyState mods

charCallback ∷ Queue InputEvent → IORef EngineLifecycle → GLFW.Window → Char → IO ()
charCallback queue el _win char = whenRunning el $
    writeQueue queue $ InputCharEvent char

mouseCallback ∷ Queue InputEvent → IORef EngineLifecycle → GLFW.Window
              → GLFW.MouseButton → GLFW.MouseButtonState → GLFW.ModifierKeys → IO ()
mouseCallback queue el win btn state mods = whenRunning el $ do
    (x, y) ← GLFW.getCursorPos win
    writeQueue queue $ InputMouseEvent btn (x, y) state

cursorCallback ∷ Queue InputEvent → GLFW.Window → Double → Double → IO ()
cursorCallback queue _win x y = do
    writeQueue queue $ InputCursorMove x y

scrollCallback ∷ Queue InputEvent → IORef EngineLifecycle → GLFW.Window
               → Double → Double → IO ()
scrollCallback queue el _win x y = whenRunning el $
    writeQueue queue $ InputScrollEvent x y

resizeCallback ∷ Queue InputEvent → GLFW.Window → Int → Int → IO ()
resizeCallback queue _win w h =
    writeQueue queue $ InputWindowEvent $ WindowResize w h

framebufferSizeCallback ∷ Queue InputEvent → GLFW.Window → Int → Int → IO ()
framebufferSizeCallback queue _win w h =
    writeQueue queue $ InputWindowEvent $ FramebufferResize w h

focusCallback ∷ Queue InputEvent → GLFW.Window → Bool → IO ()
focusCallback queue _win focused =
    writeQueue queue $ InputWindowEvent $ WindowFocus focused

iconifyCallback ∷ Queue InputEvent → GLFW.Window → Bool → IO ()
iconifyCallback queue _win iconified =
    writeQueue queue $ InputWindowEvent $ WindowMinimize iconified

emptyCallback ∷ IO ()
emptyCallback = return ()
