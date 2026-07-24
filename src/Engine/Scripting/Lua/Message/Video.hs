-- | Window / video-config Lua message handlers (split out of
--   'Engine.Scripting.Lua.Message', #558): resolution, window mode,
--   VSync, MSAA, brightness, pixel snap, and the live texture-filter
--   swap. No GPU texture upload lives here — see
--   'Engine.Scripting.Lua.Message.Texture' for that.
module Engine.Scripting.Lua.Message.Video
    ( handleSetResolution
    , handleSetWindowMode
    , handleSetVSync
    , handleSetMSAA
    , handleSetBrightness
    , handleSetPixelSnap
    , handleSetTextureFilter
    ) where

import UPrelude
import qualified Data.Text as T
import Data.IORef (readIORef, atomicModifyIORef', writeIORef)
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logInfoM, logWarnM)
import Engine.Core.Monad
import Engine.Core.State (EngineState(..), GraphicsState(..)
  , WindowState(..), luaQueue )
import Engine.Core.Capability.Render
  (RenderCapability(..), toRenderCapability)
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Config (WindowMode(..)
                               , VideoConfig(..)
                               , TextureFilter(..)
                               , textureFilterToText
                               , textureFilterToVulkan)
import Engine.Graphics.Vulkan.Recreate (recreateSwapchain)
import Engine.Graphics.Vulkan.Texture.Bindless (setTextureFilter)
import Engine.Graphics.Window.Types (Window(..))
import Engine.Scripting.Lua.Types
import qualified Graphics.UI.GLFW as GLFW

handleSetResolution ∷ Int → Int → EngineM ε σ ()
handleSetResolution w h = do
    state ← gets graphicsState
    case glfwWindow state of
        Nothing → logWarnM CatGraphics "Cannot set resolution: no window"
        Just (Window win) → do
            -- GLFW.setWindowSize expects logical (screen-coordinate) pixels;
            -- the OS scales to framebuffer size on HiDPI displays.
            liftIO $ GLFW.setWindowSize win w h
            env ← ask
            liftIO $ do
                (winW, winH) ← GLFW.getWindowSize win
                (fbW, fbH) ← GLFW.getFramebufferSize win
                writeIORef (rcWindowSizeRef (toRenderCapability env)) (winW, winH)
                writeIORef (rcFramebufferSizeRef (toRenderCapability env)) (fbW, fbH)

                Q.writeQueue (luaQueue env) (LuaWindowResize winW winH)
                Q.writeQueue (luaQueue env) (LuaFramebufferResize fbW fbH)

            logInfoM CatGraphics $ "Window resized to "
                <> T.pack (show w) <> "x" <> T.pack (show h) <> " (logical pixels)"

handleSetWindowMode ∷ WindowMode → EngineM ε σ ()
handleSetWindowMode mode = do
    state ← gets graphicsState
    case glfwWindow state of
        Nothing → logWarnM CatGraphics "Cannot set window mode: no window"
        Just (Window win) → do
            env ← ask
            liftIO $ do
                -- Cache windowed geometry before switching away from it
                currentConfig ← readIORef (rcVideoConfigRef (toRenderCapability env))
                when (vcWindowMode currentConfig ≡ Windowed) $ do
                    (wx, wy) ← GLFW.getWindowPos win
                    (ww, wh) ← GLFW.getWindowSize win
                    writeIORef (rcWindowStateRef (toRenderCapability env)) $ WindowState
                        { wsWindowedPos  = (wx, wy)
                        , wsWindowedSize = (ww, wh)
                        }

                case mode of
                    Fullscreen → do
                        mMonitor ← GLFW.getPrimaryMonitor
                        case mMonitor of
                            Nothing → pure ()
                            Just monitor → do
                                mMode ← GLFW.getVideoMode monitor
                                case mMode of
                                    Nothing → pure ()
                                    Just vm → do
                                      GLFW.setFullscreen win monitor vm
                                      (winW, winH) ← GLFW.getWindowSize win
                                      (fbW, fbH) ← GLFW.getFramebufferSize win
                                      let rc = toRenderCapability env
                                      writeIORef (rcWindowSizeRef rc) (winW, winH)
                                      writeIORef (rcFramebufferSizeRef rc) (fbW, fbH)
                                      Q.writeQueue (luaQueue env)
                                                   (LuaWindowResize winW winH)
                                      Q.writeQueue (luaQueue env)
                                                   (LuaFramebufferResize fbW fbH)

                    BorderlessWindowed → do
                        mMonitor ← GLFW.getPrimaryMonitor
                        case mMonitor of
                            Nothing → pure ()
                            Just monitor → do
                                mMode ← GLFW.getVideoMode monitor
                                case mMode of
                                    Nothing → pure ()
                                    Just vm → do
                                        let monW = GLFW.videoModeWidth vm
                                            monH = GLFW.videoModeHeight vm
                                        GLFW.setWindowed win monW monH 0 0
                                        GLFW.setWindowAttrib win GLFW.WindowAttrib'Decorated False
                                        (winW, winH) ← GLFW.getWindowSize win
                                        (fbW, fbH) ← GLFW.getFramebufferSize win
                                        let rc = toRenderCapability env
                                        writeIORef (rcWindowSizeRef rc) (winW, winH)
                                        writeIORef (rcFramebufferSizeRef rc) (fbW, fbH)
                                        Q.writeQueue (luaQueue env)
                                                     (LuaWindowResize winW winH)
                                        Q.writeQueue (luaQueue env)
                                                     (LuaFramebufferResize fbW fbH)



                    Windowed → do
                        let rc = toRenderCapability env
                        ws ← readIORef (rcWindowStateRef rc)
                        let (wx, wy) = wsWindowedPos ws
                            (ww, wh) = wsWindowedSize ws
                        GLFW.setWindowAttrib win GLFW.WindowAttrib'Decorated True
                        GLFW.setWindowed win ww wh wx wy
                        (winW, winH) ← GLFW.getWindowSize win
                        (fbW, fbH) ← GLFW.getFramebufferSize win
                        writeIORef (rcWindowSizeRef rc) (winW, winH)
                        writeIORef (rcFramebufferSizeRef rc) (fbW, fbH)
                        Q.writeQueue (luaQueue env)
                                     (LuaWindowResize winW winH)
                        Q.writeQueue (luaQueue env)
                                     (LuaFramebufferResize fbW fbH)


handleSetVSync ∷ Bool → EngineM ε σ ()
handleSetVSync vsync = do
    env ← ask
    liftIO $ atomicModifyIORef' (rcVideoConfigRef (toRenderCapability env)) $ \c →
        (c { vcVSync = vsync }, ())

    state ← gets graphicsState
    case glfwWindow state of
        Nothing → logWarnM CatGraphics "Cannot set VSync: no window"
        Just window → do
            logInfoM CatGraphics $ "Recreating swapchain for VSync change: "
                <> if vsync then "enabled" else "disabled"
            recreateSwapchain window

handleSetMSAA ∷ Int → EngineM ε σ ()
handleSetMSAA msaa = do
    env ← ask
    liftIO $ atomicModifyIORef' (rcVideoConfigRef (toRenderCapability env)) $ \c →
        (c { vcMSAA = msaa }, ())

    state ← gets graphicsState
    case glfwWindow state of
        Nothing → logWarnM CatGraphics "Cannot set MSAA: no window"
        Just window → do
            logInfoM CatGraphics $ "Recreating swapchain for MSAA change: "
                <> T.pack (show msaa) <> "x"
            recreateSwapchain window

handleSetBrightness ∷ Int → EngineM ε σ ()
handleSetBrightness pct = do
    env ← ask
    let brightness = max 50 (min 300 pct)
    liftIO $ writeIORef (rcBrightnessRef (toRenderCapability env)) brightness
    logDebugM CatGraphics $ "Brightness set to " <> T.pack (show pct) <> "%"

handleSetPixelSnap ∷ Bool → EngineM ε σ ()
handleSetPixelSnap enabled = do
    env ← ask
    liftIO $ writeIORef (rcPixelSnapRef (toRenderCapability env)) enabled
    logDebugM CatGraphics $ "Pixel snap " <> if enabled then "enabled" else "disabled"

-- | Live-swap every bound texture sampler to a new filter mode
--   ('LuaSetTextureFilter'). No-op (besides the config write) when no
--   Vulkan device/bindless system is up yet.
handleSetTextureFilter ∷ TextureFilter → EngineM ε σ ()
handleSetTextureFilter tf = do
    logInfoM CatTexture $ "Texture filter changed to: " <> textureFilterToText tf
    env ← ask
    liftIO $ writeIORef (rcTextureFilterRef (toRenderCapability env)) tf
    gs ← gets graphicsState
    mBindless ← liftIO $ readIORef (rcTextureSystemRef (toRenderCapability env))
    case (vulkanDevice gs, mBindless) of
        (Just dev, Just bindless) → do
            newBindless ← setTextureFilter dev (textureFilterToVulkan tf) bindless
            liftIO $ writeIORef (rcTextureSystemRef (toRenderCapability env)) (Just newBindless)
            logInfoM CatTexture "All texture samplers updated live"
        _ → pure ()
