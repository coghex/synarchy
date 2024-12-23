{-# LANGUAGE Strict #-}
module Engine.Graphics.Window.GLFW
  ( -- * Window Management
    createWindow
  , destroyWindow
  , showWindow
  , hideWindow
    -- * Window State
  , windowShouldClose
  , setWindowShouldClose
  , getWindowSize
    -- * Event Handling
  , pollEvents
  , waitEvents
  , waitEventsTimeout
    -- * Vulkan Integration
  , getRequiredInstanceExtensions
  , createWindowSurface
    -- * Initialization
  , initializeGLFW
  , terminateGLFW
  ) where

import UPrelude
import Control.Exception (IOException, catch, ioError)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import qualified Graphics.UI.GLFW as GLFW
import Engine.Core.Monad
import Engine.Core.Resource
import Engine.Core.Error.Exception (throwEngineException, EngineException(..)
                                   , ExceptionType(..))
import Engine.Graphics.Types (GraphicsConfig(..))
import Engine.Graphics.Window.Types
import Vulkan.Core10 (Instance(..), AllocationCallbacks)
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR)

-- | Initialize GLFW with error handling
initializeGLFW ∷ EngineM ε σ ()
initializeGLFW = do
  success ← liftIO GLFW.init
  unless success $
    throwEngineException $ EngineException ExGraphics "Failed to initialize GLFW"
  
  -- Set necessary window hints for Vulkan
  liftIO $ do
    GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
    GLFW.windowHint $ GLFW.WindowHint'Resizable True

-- | Creates a GLFW window with given configuration
createWindow ∷ WindowConfig → EngineM ε σ Window
createWindow config = EngineM $ \e s c → do
  -- Set window hints
  GLFW.windowHint $ GLFW.WindowHint'Resizable (wcResizable config)
  GLFW.windowHint $ GLFW.WindowHint'Visible False
  
  -- Create the window
  mbWindow ← GLFW.createWindow 
    (wcWidth config) 
    (wcHeight config) 
    (T.unpack $ wcTitle config)
    Nothing  -- monitor (None for windowed mode)
    Nothing  -- share (None for no shared context)
    
  case mbWindow of
    Nothing → c $ Left $ error "Failed to create GLFW window"
    Just window → c $ Right $ Window window

-- | Clean up GLFW window resources
destroyWindow ∷ Window → EngineM' ε ()
destroyWindow (Window win) = liftIO $ GLFW.destroyWindow win

-- | Safely create window with automatic cleanup
initWindow ∷ WindowConfig → EngineM ε σ Window
initWindow config = allocResource destroyWindow $ createWindow config

-- | Show a window
showWindow ∷ GLFW.Window → EngineM ε σ ()
showWindow win = liftIO $ GLFW.showWindow win

-- | Hide a window
hideWindow ∷ GLFW.Window → EngineM ε σ ()
hideWindow win = liftIO $ GLFW.hideWindow win

-- | Check if a window should close
windowShouldClose ∷ GLFW.Window → EngineM ε σ Bool
windowShouldClose = liftIO ∘ GLFW.windowShouldClose

-- | Set whether a window should close
setWindowShouldClose ∷ GLFW.Window → Bool → EngineM ε σ ()
setWindowShouldClose win = liftIO ∘ GLFW.setWindowShouldClose win

-- | Get the current window size
getWindowSize ∷ GLFW.Window → EngineM ε σ (Int, Int)
getWindowSize = liftIO ∘ GLFW.getWindowSize

-- | Set a window's title
setWindowTitle ∷ GLFW.Window → T.Text → EngineM ε σ ()
setWindowTitle win title = liftIO $ GLFW.setWindowTitle win ( T.unpack title )

-- | Poll for pending events
pollEvents ∷ EngineM ε σ ()
pollEvents = liftIO GLFW.pollEvents

-- | Wait for events
waitEvents ∷ EngineM ε σ ()
waitEvents = liftIO GLFW.waitEvents

-- | Wait for events with timeout
waitEventsTimeout ∷ Double → EngineM ε σ ()
waitEventsTimeout = liftIO ∘ GLFW.waitEventsTimeout

-- | Get required Vulkan instance extensions
getRequiredInstanceExtensions ∷ EngineM ε σ [BS.ByteString]
getRequiredInstanceExtensions = do
  exts ← liftIO GLFW.getRequiredInstanceExtensions
  liftIO $ traverse BS.packCString exts

-- | Create a Vulkan surface for a window
createWindowSurface ∷ Window 
                   → Instance  -- ^ Raw Vulkan instance handle
                   → EngineM ε σ SurfaceKHR  -- ^ Raw Vulkan surface handle
createWindowSurface (Window win) inst = do
  surfaceOrError ← liftIO $ alloca $ \surfacePtr → do
    result ← GLFW.createWindowSurface 
      (instanceHandle inst)
      win
      nullPtr
      surfacePtr
    if result == 0  -- VK_SUCCESS
      then Right <$> peek surfacePtr
      else pure $ Left "Failed to create window surface"

  case surfaceOrError of
    Right surface → pure surface
    Left err → throwEngineException $ EngineException 
      { errorType = ExGraphics
      , errorMsg = err
      }

-- | Terminate GLFW
terminateGLFW ∷ EngineM ε σ ()
terminateGLFW = liftIO GLFW.terminate
