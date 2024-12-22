{-# LANGUAGE Strict #-}
module Engine.Graphics.Window.GLFW
  ( -- * Types
    Window
  , WindowConfig(..)
    -- * Window Management
  , createWindow
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
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import qualified Graphics.UI.GLFW as GLFW
import Engine.Core.Monad (EngineM)
import Engine.Core.Error.Exception (throwEngineException, EngineException(..)
                                   , ExceptionType(..))
import Engine.Graphics.Types (GraphicsConfig(..))
import Vulkan.Core10 (Instance(..), AllocationCallbacks)
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR)

-- | Window configuration
data WindowConfig = WindowConfig
  { wcWidth     ∷ Int      -- ^ Window width
  , wcHeight    ∷ Int      -- ^ Window title
  , wcTitle     ∷ String   -- ^ Window title
  , wcResizable ∷ Bool     -- ^ Whether window can be resized
  }

-- | Re-export GLFW Window type
type Window = GLFW.Window

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

-- | Create a window with the given configuration
createWindow ∷ WindowConfig → EngineM ε σ Window
createWindow WindowConfig{..} = do
  -- Set window hints
  liftIO $ do
    GLFW.windowHint $ GLFW.WindowHint'Resizable wcResizable
    GLFW.windowHint $ GLFW.WindowHint'Visible False  -- Start hidden
    
  -- Create the window
  maybeWindow ← liftIO $ GLFW.createWindow 
    wcWidth 
    wcHeight 
    wcTitle 
    Nothing     -- monitor (Nothing = windowed mode)
    Nothing     -- share (Nothing = no shared context)

  case maybeWindow of
    Just window → return window
    Nothing     → throwEngineException $ EngineException
                    ExGraphics "Failed to create GLFW window"

-- | Destroy a window and free its resources
destroyWindow ∷ Window → EngineM ε σ ()
destroyWindow = liftIO ∘ GLFW.destroyWindow

-- | Show a window
showWindow ∷ Window → EngineM ε σ ()
showWindow = liftIO ∘ GLFW.showWindow

-- | Hide a window
hideWindow ∷ Window → EngineM ε σ ()
hideWindow = liftIO ∘ GLFW.hideWindow

-- | Check if a window should close
windowShouldClose ∷ Window → EngineM ε σ Bool
windowShouldClose = liftIO ∘ GLFW.windowShouldClose

-- | Set whether a window should close
setWindowShouldClose ∷ Window → Bool → EngineM ε σ ()
setWindowShouldClose win = liftIO ∘ GLFW.setWindowShouldClose win

-- | Get the current window size
getWindowSize ∷ Window → EngineM ε σ (Int, Int)
getWindowSize = liftIO ∘ GLFW.getWindowSize

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
createWindowSurface win inst = do
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
