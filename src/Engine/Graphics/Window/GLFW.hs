{-# LANGUAGE Strict #-}
module Engine.Graphics.Window.GLFW
  ( -- * Window Management
    createWindow
  , createRawWindow
    -- * Window State
  , windowShouldClose
  , setWindowShouldClose
  , getWindowSize
  , getFramebufferSize
  , GLFW.setWindowSize
  , GLFW.setWindowPos
  , GLFW.getWindowPos
    -- * Event Handling
  , pollEvents
  , GLFW.postEmptyEvent
    -- * Keyboard and Mouse Input
  , GLFW.setKeyCallback
  , GLFW.setErrorCallback
    -- * Vulkan Integration
  , vulkanSupported
  , getRequiredInstanceExtensions
  , createWindowSurface
    -- * Raw init since tests run in IO
  , GLFW.init
  , GLFW.terminate
  , GLFW.Window
  ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.IORef (writeIORef, modifyIORef')
import qualified Graphics.UI.GLFW as GLFW
import Engine.Core.Monad
import Engine.Core.State (WindowState(..), appliedModeAtCreation, luaQueue)
import Engine.Core.Capability.Render
  (RenderCapability(..), toRenderCapability)
import Engine.Core.Resource
import qualified Engine.Core.Queue as Q
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logAndThrowM, logDebugM, logInfoM)
import Engine.Core.Error.Exception (ExceptionType(..), GraphicsError(..)
                                   , InitError(..))
import Engine.Graphics.Window.Types
import Engine.Scripting.Lua.Types (LuaMsg(..))
import Vulkan.Core10 (Instance(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR, destroySurfaceKHR)

-- | Creates a GLFW window with given configuration
createWindow ∷ WindowConfig → EngineM σ Window
createWindow config = do
  allocResource (\_ → do
                  liftIO GLFW.terminate
                  logDebugM CatGraphics "GLFW terminated")
                (do
                  success ← liftIO $ GLFW.init
                  case success of
                    True  → logDebugM CatGraphics "GLFW initialized"
                    False → logAndThrowM CatGraphics (ExInit WindowCreationFailed)
                                 "Failed to initialize GLFW"
                  -- Set necessary window hints for Vulkan
                  liftIO $ do
                    GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
                    GLFW.windowHint $ GLFW.WindowHint'Resizable True)

  liftIO $ do
    GLFW.windowHint $ GLFW.WindowHint'Resizable (wcResizable config)
    GLFW.windowHint $ GLFW.WindowHint'Visible (wcVisible config)
    GLFW.windowHint $ GLFW.WindowHint'Focused (wcFocused config)
    -- A hidden automated window must also stay non-activating if a
    -- platform shows it as part of native surface setup. For ordinary
    -- configs this preserves GLFW's normal focus-on-show behavior.
    GLFW.windowHint $ GLFW.WindowHint'FocusOnShow (wcFocused config)

  window ← allocResource (\(Window w0) → liftIO $ GLFW.destroyWindow w0) $ do
    mw ← liftIO $ GLFW.createWindow (wcWidth config) (wcHeight config)
                                    (T.unpack $ wcTitle config) Nothing Nothing
    case mw of
      Nothing → logAndThrowM CatGraphics (ExInit WindowCreationFailed) $
                 T.pack $ "Failed to create GLFW window with dimensions: "
                 ⧺ show (wcWidth config) ⧺ "x" ⧺ show (wcHeight config)
      Just win → do
        actualSize ← liftIO $ GLFW.getWindowSize win
        logInfoM CatGraphics $ "Window created with actual size: " <> tshow actualSize
        pure $ Window win
  let Window win = window
  -- A fullscreen request degrades gracefully to the plain window GLFW
  -- just created, so the outcome — not the config — is what gets
  -- recorded as the applied mode below (#907).
  fullscreenApplied ← if wcFullscreen config
    then do
      primaryMonitor ← liftIO $ GLFW.getPrimaryMonitor
      case primaryMonitor of
        Nothing → do
          logInfoM CatGraphics "Failed to get primary monitor for fullscreen"
          pure False
        Just monitor → do
          videoMode ← liftIO $ GLFW.getVideoMode monitor
          case videoMode of
            Nothing → do
              logInfoM CatGraphics "Failed to get video mode for fullscreen"
              pure False
            Just vm → do
              liftIO $ GLFW.setFullscreen win monitor vm
              pure True
    else pure False

  env ← ask
  liftIO $ do
    windowSize ← GLFW.getWindowSize win
    framebufferSize ← GLFW.getFramebufferSize win
    windowPos ← GLFW.getWindowPos win
    writeIORef (rcWindowSizeRef (toRenderCapability env)) windowSize
    writeIORef (rcFramebufferSizeRef (toRenderCapability env)) framebufferSize
    writeIORef (rcWindowPosRef (toRenderCapability env)) windowPos
    -- Seed the window-mode tracker from what GLFW actually did. Nothing
    -- else establishes it: 'Engine.Core.Init' cannot know whether this
    -- fullscreen request succeeded, and 'defaultWindowConfig' never asks
    -- for borderless at creation, so any non-fullscreen outcome is a
    -- plain decorated window.
    modifyIORef' (rcWindowStateRef (toRenderCapability env)) $ \ws →
      ws { wsAppliedMode = appliedModeAtCreation fullscreenApplied }
    Q.writeQueue (luaQueue env) (LuaWindowResize (fst windowSize) (snd windowSize))
    Q.writeQueue (luaQueue env) (LuaFramebufferResize (fst framebufferSize) (snd framebufferSize))
    
  pure window
 


-- | Creates a GLFW window in an IO context for testing
createRawWindow ∷ WindowConfig → IO (Maybe Window)
createRawWindow config = do

  GLFW.windowHint $ GLFW.WindowHint'Resizable (wcResizable config)
  GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
  GLFW.windowHint $ GLFW.WindowHint'Visible (wcVisible config)
  GLFW.windowHint $ GLFW.WindowHint'Focused (wcFocused config)
  GLFW.windowHint $ GLFW.WindowHint'FocusOnShow (wcFocused config)
  mw ← liftIO $ GLFW.createWindow (wcWidth config) (wcHeight config)
                                  (T.unpack $ wcTitle config) Nothing Nothing
  pure $ case mw of
    Nothing → Nothing
    Just win → Just $ Window win

-- | Check if a window should close
windowShouldClose ∷ GLFW.Window → EngineM σ Bool
windowShouldClose = liftIO ∘ GLFW.windowShouldClose

-- | Set whether a window should close
setWindowShouldClose ∷ GLFW.Window → Bool → EngineM σ ()
setWindowShouldClose win = liftIO ∘ GLFW.setWindowShouldClose win

-- | Get the current window size
getWindowSize ∷ GLFW.Window → EngineM σ (Int, Int)
getWindowSize = liftIO ∘ GLFW.getWindowSize

-- | Get the current framebuffer size
getFramebufferSize ∷ GLFW.Window → EngineM σ (Int, Int)
getFramebufferSize = liftIO ∘ GLFW.getFramebufferSize

-- | Poll for pending events
pollEvents ∷ EngineM σ ()
pollEvents = liftIO GLFW.pollEvents

-- | Get required Vulkan instance extensions
getRequiredInstanceExtensions ∷ EngineM σ [BS.ByteString]
getRequiredInstanceExtensions = do
  exts ← liftIO GLFW.getRequiredInstanceExtensions
  liftIO $ traverse BS.packCString exts

-- | Create a Vulkan surface for a window
createWindowSurface ∷ Window 
                   → Instance  -- ^ Raw Vulkan instance handle
                   → EngineM σ SurfaceKHR  -- ^ Raw Vulkan surface handle
createWindowSurface (Window win) inst = allocResource
  (\surface → do
      logDebugM CatVulkan "Destroying window surface"
      liftIO $ destroySurfaceKHR inst surface Nothing)
  $ do
    surfaceOrError ← liftIO $ alloca $ \surfacePtr → do
      result ← GLFW.createWindowSurface 
        (instanceHandle inst)
        win
        nullPtr
        surfacePtr
      if result ≡ 0  -- VK_SUCCESS
        then Right ⊚ peek surfacePtr
        else pure $ Left $ "Failed to create window surface, error code: "
                         ⧺ show result

    case surfaceOrError of
      Right surface → pure surface
      Left err → logAndThrowM CatVulkan (ExGraphics VulkanSurfaceLost) $
                   T.pack $ "Failed to create window surface: " ⧺ err

vulkanSupported ∷ EngineM σ Bool
vulkanSupported = liftIO GLFW.vulkanSupported
