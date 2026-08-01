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
import Data.IORef (readIORef, atomicModifyIORef', modifyIORef', writeIORef)
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logInfoM, logWarnM)
import Engine.Core.Monad
import Engine.Core.State (EngineState(..), GraphicsState(..)
  , WindowState(..), applyWindowModeTransition, windowModeAlreadyApplied
  , luaQueue )
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

handleSetResolution ∷ Int → Int → EngineM σ ()
handleSetResolution w h = do
    state ← gets graphicsState
    case glfwWindow state of
        Nothing → logWarnM CatGraphics "Cannot set resolution: no window"
        Just (Window win) → do
            -- GLFW.setWindowSize expects logical (screen-coordinate) pixels;
            -- the OS scales to framebuffer size on HiDPI displays.
            liftIO $ GLFW.setWindowSize win w h
            env ← ask
            liftIO $ publishWindowGeometry (toRenderCapability env)
                                           (luaQueue env) win

            logInfoM CatGraphics $ "Window resized to "
                <> T.pack (show w) <> "x" <> T.pack (show h) <> " (logical pixels)"

-- | Republish the live GLFW window\/framebuffer geometry: both size refs,
--   the position ref, and both Lua resize notifications. Every successful
--   mode branch (and 'handleSetResolution') ends here, exactly as each
--   did inline before — #907 changed only the cache DECISION, never what
--   a successful switch publishes.
publishWindowGeometry ∷ RenderCapability → Q.Queue LuaMsg → GLFW.Window → IO ()
publishWindowGeometry rc lq win = do
    (winW, winH) ← GLFW.getWindowSize win
    (fbW, fbH) ← GLFW.getFramebufferSize win
    (wx, wy) ← GLFW.getWindowPos win
    writeIORef (rcWindowSizeRef rc) (winW, winH)
    writeIORef (rcFramebufferSizeRef rc) (fbW, fbH)
    writeIORef (rcWindowPosRef rc) (wx, wy)
    Q.writeQueue lq (LuaWindowResize winW winH)
    Q.writeQueue lq (LuaFramebufferResize fbW fbH)

-- | Apply a window-mode switch on the main render thread.
--
--   The windowed geometry cache is keyed off 'wsAppliedMode' — the mode
--   THIS handler last applied — never off @vcWindowMode@. The Lua thread
--   publishes the target mode into the video config the moment it
--   enqueues the message, so reading it back here saw the mode being
--   entered rather than the one being left: leaving @windowed@ skipped
--   the cache entirely, and returning to @windowed@ overwrote it with the
--   borderless monitor geometry and then "restored" that (#907).
--
--   'applyWindowModeTransition' is folded in only after the switch
--   actually succeeded, from geometry sampled before it, so a branch that
--   bails out (no monitor, no video mode) leaves both the cache and the
--   applied mode untouched. A request for the mode already applied is
--   inert ('windowModeAlreadyApplied') — see that function for why a
--   redundant @windowed@ request must not re-run the restore.
handleSetWindowMode ∷ WindowMode → EngineM σ ()
handleSetWindowMode mode = do
    state ← gets graphicsState
    case glfwWindow state of
        Nothing → logWarnM CatGraphics "Cannot set window mode: no window"
        Just (Window win) → do
            env ← ask
            liftIO $ do
                let rc = toRenderCapability env
                    publish = publishWindowGeometry rc (luaQueue env) win
                -- Sampled while the window is still in the mode being left
                livePos ← GLFW.getWindowPos win
                liveSize ← GLFW.getWindowSize win
                ws0 ← readIORef (rcWindowStateRef rc)

                if windowModeAlreadyApplied ws0 mode
                  -- Nothing to switch. Republishing the unchanged live
                  -- geometry keeps this exactly as inert as it was before
                  -- #907, when the guard's own cache write made the
                  -- Windowed branch's restore a no-op.
                  then publish
                  else do
                    applied ← case mode of
                        Fullscreen → do
                            mMonitor ← GLFW.getPrimaryMonitor
                            case mMonitor of
                                Nothing → pure False
                                Just monitor → do
                                    mMode ← GLFW.getVideoMode monitor
                                    case mMode of
                                        Nothing → pure False
                                        Just vm → do
                                          GLFW.setFullscreen win monitor vm
                                          publish
                                          pure True

                        BorderlessWindowed → do
                            mMonitor ← GLFW.getPrimaryMonitor
                            case mMonitor of
                                Nothing → pure False
                                Just monitor → do
                                    mMode ← GLFW.getVideoMode monitor
                                    case mMode of
                                        Nothing → pure False
                                        Just vm → do
                                            let monW = GLFW.videoModeWidth vm
                                                monH = GLFW.videoModeHeight vm
                                            GLFW.setWindowed win monW monH 0 0
                                            GLFW.setWindowAttrib win GLFW.WindowAttrib'Decorated False
                                            publish
                                            pure True

                        Windowed → do
                            let (wx, wy) = wsWindowedPos ws0
                                (ww, wh) = wsWindowedSize ws0
                            GLFW.setWindowAttrib win GLFW.WindowAttrib'Decorated True
                            GLFW.setWindowed win ww wh wx wy
                            publish
                            pure True

                    when applied $
                        modifyIORef' (rcWindowStateRef rc)
                                     (applyWindowModeTransition mode livePos liveSize)


handleSetVSync ∷ Bool → EngineM σ ()
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

handleSetMSAA ∷ Int → EngineM σ ()
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

handleSetBrightness ∷ Int → EngineM σ ()
handleSetBrightness pct = do
    env ← ask
    let brightness = max 50 (min 300 pct)
    liftIO $ writeIORef (rcBrightnessRef (toRenderCapability env)) brightness
    logDebugM CatGraphics $ "Brightness set to " <> T.pack (show pct) <> "%"

handleSetPixelSnap ∷ Bool → EngineM σ ()
handleSetPixelSnap enabled = do
    env ← ask
    liftIO $ writeIORef (rcPixelSnapRef (toRenderCapability env)) enabled
    logDebugM CatGraphics $ "Pixel snap " <> if enabled then "enabled" else "disabled"

-- | Live-swap every bound texture sampler to a new filter mode
--   ('LuaSetTextureFilter'). No-op (besides the config write) when no
--   Vulkan device/bindless system is up yet.
handleSetTextureFilter ∷ TextureFilter → EngineM σ ()
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
