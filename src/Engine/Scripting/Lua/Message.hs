module Engine.Scripting.Lua.Message
  ( processLuaMessages
  ) where

import UPrelude
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.IORef (readIORef, atomicModifyIORef', writeIORef)
import Data.Time.Clock (getCurrentTime)
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Utils (copyBytes)
import System.FilePath (takeBaseName)
import Engine.Asset.Handle
import Engine.Asset.Manager
import Engine.Asset.Types
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logInfoM, logWarnM, logDebugSM, logInfoSM, logWarnSM)
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Resource (locally)
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Config (WindowMode(..), VideoConfig(..), TextureFilter(..)
                              , textureFilterToText, textureFilterToVulkan)
import Engine.Graphics.Font.Load (loadSDFFont)
import Engine.Graphics.Types (DevQueues(..))
import Engine.Graphics.Vulkan.Image (createVulkanImage, createVulkanImageView
                                    , copyBufferToImage, VulkanImage(..))
import Engine.Graphics.Vulkan.Buffer (createVulkanBuffer)
import Engine.Graphics.Vulkan.Command (runCommandsOnce)
import Engine.Graphics.Vulkan.Types.Vertex (Vec4(..))
import Engine.Graphics.Vulkan.Recreate (recreateSwapchain)
import Engine.Graphics.Vulkan.Texture (createTextureSampler, transitionImageLayout
                                      , ImageLayoutTransition(..))
import Engine.Graphics.Vulkan.Texture.Bindless (rewriteAllSamplers, registerTexture)
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Window.Types (Window(..))
import Engine.Scene.Base
import Engine.Scene.Graph (modifySceneNode, deleteSceneNode)
import Engine.Scene.Manager (addObjectToScene)
import Engine.Scene.Types
import Engine.Scripting.Lua.Types
import qualified Graphics.UI.GLFW as GLFW
import Vulkan.Core10
import Vulkan.Zero (zero)

processLuaMessages ∷ EngineM ε σ ()
processLuaMessages = do
    env ← ask
    messages ← liftIO $ Q.flushQueue (luaToEngineQueue env)
    
    when (not $ null messages) $
        logDebugSM CatLua "Processing Lua messages"
            [("count", T.pack $ show $ length messages)]
    
    forM_ messages handleLuaMessage
    -- poll for world preview image
    handleWorldPreview

handleLuaMessage ∷ LuaToEngineMsg → EngineM ε σ ()
handleLuaMessage msg = do
    case msg of
        LuaSetWindowMode mode → do
            logDebugM CatLua $ "Setting window mode: " <> T.pack (show mode)
            handleSetWindowMode mode

        LuaSetResolution w h → do
            logDebugSM CatLua "Setting resolution"
                [("width", T.pack $ show w)
                ,("height", T.pack $ show h)]
            handleSetResolution w h

        LuaSetVSync enabled → do
            logDebugSM CatLua "Setting VSync"
                [("enabled", if enabled then "true" else "false")]
            handleSetVSync enabled

        LuaSetMSAA msaa → do
            logDebugSM CatLua "Setting MSAA"
                [("samples", T.pack $ show msaa)]
            handleSetMSAA msaa

        LuaSetBrightness brightness → do
            logDebugSM CatLua "Setting brightness"
                [("brightness", T.pack $ show brightness)]
            handleSetBrightness brightness

        LuaSetPixelSnap enabled → do
            logDebugSM CatLua "Setting pixel snap"
                [("enabled", if enabled then "true" else "false")]
            handleSetPixelSnap enabled

        LuaSetTextureFilter tf → do
            logInfoM CatTexture $ "Texture filter changed to: " <> textureFilterToText tf
            env ← ask
            liftIO $ writeIORef (textureFilterRef env) tf
            -- Live-update all existing texture samplers
            gs ← gets graphicsState
            case (vulkanDevice gs, vulkanPDevice gs, textureSystem gs) of
                (Just dev, Just pdev, Just bindless) → do
                    let vkFilter = textureFilterToVulkan tf
                    -- Create one new sampler with the desired filter
                    newSampler ← createTextureSampler dev pdev vkFilter
                    -- Rewrite every bindless slot to use it
                    rewriteAllSamplers dev newSampler bindless
                    logInfoM CatTexture "All texture samplers updated live"
                _ → pure ()

        LuaLoadFontRequest handle path size → do
            logDebugSM CatLua "Loading font"
                [("path", T.pack path)
                ,("size", T.pack $ show size)
                ,("handle", T.pack (show handle))]
            handleLoadFont handle path size
        
        LuaLoadTextureRequest handle path → do
            logDebugSM CatLua "Loading texture"
                [("path", T.pack path)
                ,("handle", T.pack (show handle))]
            handleLoadTexture handle path
        
        LuaSpawnTextRequest objId x y font text color layer size → do
            logDebugSM CatLua "Spawning text"
                [("objId", T.pack (show objId))
                ,("pos", T.pack (show x) <> "," <> T.pack (show y))
                ,("text", T.take 20 text)
                ,("layer", T.pack (show layer))
                ,("size", T.pack (show size))]
            handleSpawnText objId x y font text color layer size
        
        LuaSetTextRequest objId text → do
            logDebugSM CatLua "Setting text"
                [("objId", T.pack (show objId))
                ,("text", T.take 20 text)]
            handleSetText objId text
        
        LuaSpawnSpriteRequest objId x y w h tex layer → do
            logDebugSM CatLua "Spawning sprite"
                [("objId", T.pack (show objId))
                ,("pos", T.pack (show x) <> "," <> T.pack (show y))
                ,("size", T.pack (show w) <> "x" <> T.pack (show h))
                ,("layer", T.pack (show layer))]
            handleSpawnSprite objId x y w h tex layer
        
        LuaSetPosRequest objId x y → do
            logDebugSM CatLua "Moving object"
                [("objId", T.pack $ show objId)
                ,("pos", T.pack (show x) <> "," <> T.pack (show y))]
            handleSetPos objId x y
        
        LuaSetColorRequest objId color → do
            logDebugM CatLua $ "Setting color for object " <> T.pack (show objId)
            handleSetColor objId color
        
        LuaSetSizeRequest objId w h → do
            logDebugSM CatLua "Setting size"
                [("objId", T.pack $ show objId)
                ,("size", T.pack (show w) <> "x" <> T.pack (show h))]
            handleSetSize objId w h
        
        LuaSetVisibleRequest objId visible → do
            logDebugSM CatLua "Setting visibility"
                [("objId", T.pack $ show objId)
                ,("visible", if visible then "true" else "false")]
            handleSetVisible objId visible
        
        LuaDestroyRequest objId → do
            logDebugM CatLua $ "Destroying object " <> T.pack (show objId)
            handleDestroy objId

handleSetResolution ∷ Int → Int → EngineM ε σ ()
handleSetResolution w h = do
    state ← gets graphicsState
    case glfwWindow state of
        Nothing → logWarnM CatGraphics "Cannot set resolution: no window"
        Just (Window win) → do
            -- w, h are logical window dimensions (screen coordinates)
            -- On HiDPI displays, GLFW.setWindowSize expects logical pixels (screen coordinates)
            -- The framebuffer will automatically be scaled by the OS content scale
            liftIO $ GLFW.setWindowSize win w h
            -- Update our refs with the actual sizes after resize
            env ← ask
            liftIO $ do
                (winW, winH) ← GLFW.getWindowSize win
                (fbW, fbH) ← GLFW.getFramebufferSize win
                writeIORef (windowSizeRef env) (winW, winH)
                writeIORef (framebufferSizeRef env) (fbW, fbH)
                
                -- Notify Lua of the actual sizes
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
                -- Before changing mode, cache current windowed geometry
                -- (only if we're currently in windowed mode)
                currentConfig ← readIORef (videoConfigRef env)
                when (vcWindowMode currentConfig ≡ Windowed) $ do
                    (wx, wy) ← GLFW.getWindowPos win
                    (ww, wh) ← GLFW.getWindowSize win
                    writeIORef (windowStateRef env) $ WindowState
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
                                      writeIORef (windowSizeRef env) (winW, winH)
                                      writeIORef (framebufferSizeRef env) (fbW, fbH)
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
                                        -- Switch to windowed first (unset monitor)
                                        GLFW.setWindowed win monW monH 0 0
                                        -- Remove decorations
                                        GLFW.setWindowAttrib win GLFW.WindowAttrib'Decorated False
                                        (winW, winH) ← GLFW.getWindowSize win
                                        (fbW, fbH) ← GLFW.getFramebufferSize win
                                        writeIORef (windowSizeRef env) (winW, winH)
                                        writeIORef (framebufferSizeRef env) (fbW, fbH)
                                        Q.writeQueue (luaQueue env)
                                                     (LuaWindowResize winW winH)
                                        Q.writeQueue (luaQueue env)
                                                     (LuaFramebufferResize fbW fbH)
  


                    Windowed → do
                        ws ← readIORef (windowStateRef env)
                        let (wx, wy) = wsWindowedPos ws
                            (ww, wh) = wsWindowedSize ws
                        -- Restore decorations
                        GLFW.setWindowAttrib win GLFW.WindowAttrib'Decorated True
                        -- Switch to windowed with cached geometry
                        GLFW.setWindowed win ww wh wx wy
                        (winW, winH) ← GLFW.getWindowSize win
                        (fbW, fbH) ← GLFW.getFramebufferSize win
                        writeIORef (windowSizeRef env) (winW, winH)
                        writeIORef (framebufferSizeRef env) (fbW, fbH)
                        Q.writeQueue (luaQueue env)
                                     (LuaWindowResize winW winH)
                        Q.writeQueue (luaQueue env)
                                     (LuaFramebufferResize fbW fbH)


handleSetVSync ∷ Bool → EngineM ε σ ()
handleSetVSync vsync = do
    -- Update the config so recreateSwapchain reads the new value
    env ← ask
    liftIO $ do
        oldConfig ← readIORef (videoConfigRef env)
        writeIORef (videoConfigRef env) $ oldConfig { vcVSync = vsync }
    
    -- Recreate the swapchain with the new present mode
    state ← gets graphicsState
    case glfwWindow state of
        Nothing → logWarnM CatGraphics "Cannot set VSync: no window"
        Just window → do
            logInfoM CatGraphics $ "Recreating swapchain for VSync change: "
                <> if vsync then "enabled" else "disabled"
            recreateSwapchain window

-- | Handle MSAA change — same pattern as VSync
handleSetMSAA ∷ Int → EngineM ε σ ()
handleSetMSAA msaa = do
    env ← ask
    liftIO $ do
        oldConfig ← readIORef (videoConfigRef env)
        writeIORef (videoConfigRef env) $ oldConfig { vcMSAA = msaa }
    
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
    liftIO $ writeIORef (brightnessRef env) brightness
    logDebugM CatGraphics $ "Brightness set to " <> T.pack (show pct) <> "%"

handleSetPixelSnap ∷ Bool → EngineM ε σ ()
handleSetPixelSnap enabled = do
    env ← ask
    liftIO $ writeIORef (pixelSnapRef env) enabled
    logDebugM CatGraphics $ "Pixel snap " <> if enabled then "enabled" else "disabled"

handleSetTextureFilter ∷ TextureFilter → EngineM ε σ ()
handleSetTextureFilter tf = do
    env ← ask
    liftIO $ writeIORef (textureFilterRef env) tf
    state ← gets graphicsState
    case (vulkanDevice state, vulkanPDevice state, textureSystem state) of
        (Just dev, Just pdev, Just bindless) → do
            logInfoM CatTexture $ "Texture filter set to: " 
                <> textureFilterToText tf
                <> " (takes effect on next texture load or restart)"
        _ → pure ()

-- | Handle texture load request
handleLoadTexture ∷ TextureHandle → FilePath → EngineM ε σ ()
handleLoadTexture handle path = do
    logDebugM CatLua $ "Loading texture from Lua: " <> T.pack path
                    <> " (handle: " <> T.pack (show handle) <> ")"
    assetId ← loadTextureAtlasWithHandle handle (T.pack $ takeBaseName path) path "default"
    -- notify lua
    env ← ask
    let (TextureHandle h) = handle
    liftIO $ Q.writeQueue (luaQueue env)
      (LuaAssetLoaded "texture" (fromIntegral h) (T.pack path))
    logDebugM CatLua $ "Texture loaded successfully: " <> T.pack path

-- | Handle font load request
handleLoadFont ∷ FontHandle → FilePath → Int → EngineM ε σ ()
handleLoadFont handle path size = do
    logDebugM CatLua $ "Loading font from Lua: " <> T.pack path
    actualHandle ← loadSDFFont handle path
    env ← ask
    let etlq = luaQueue env
    liftIO $ Q.writeQueue etlq (LuaFontLoaded actualHandle path)
    let (FontHandle h) = actualHandle
    liftIO $ Q.writeQueue etlq (LuaAssetLoaded "font" (fromIntegral h) (T.pack path))
    logDebugM CatLua $ "Font loaded successfully: " <> T.pack path

-- | Handle spawn text request
handleSpawnText ∷ ObjectId → Float → Float → FontHandle → Text
                → Vec4 → LayerId → Float → EngineM ε σ ()
handleSpawnText oid x y fontHandle text color layer size = do
    sceneMgr ← gets sceneManager
    case smActiveScene sceneMgr of
      Just sceneId → do
        let node = (createSceneNode TextObject)
              { nodeId = oid
              , nodeTransform = defaultTransform { position = (x, y) }
              , nodeFont = Just fontHandle
              , nodeFontSize = Just size
              , nodeText = Just text
              , nodeColor = color
              , nodeVisible = True
              , nodeLayer = layer
              }
        case addObjectToScene sceneId node sceneMgr of
          Just (addedObjId, newSceneMgr) → do
            modify $ \s → s { sceneManager = newSceneMgr }
            env ← ask
            liftIO $ atomicModifyIORef' (textBuffersRef env) $ \m →
              (Map.insert oid text m, ())
          Nothing → logDebugM CatLua $ "Failed to add text object " <> T.pack (show oid)
      Nothing → logDebugM CatLua "Cannot spawn text: no active scene"

-- | Handle set text request
handleSetText ∷ ObjectId → Text → EngineM ε σ ()
handleSetText objId text = do
    env ← ask
    liftIO $ atomicModifyIORef' (textBuffersRef env) $ \m →
      (Map.insert objId text m, ())
    modifySceneNode objId $ \node → node { nodeText = Just text }
    return ()

-- | Handle spawn sprite request
handleSpawnSprite ∷ ObjectId → Float → Float → Float → Float 
                  → TextureHandle → LayerId → EngineM ε σ ()
handleSpawnSprite objId x y width height texHandle layer = do
    sceneMgr ← gets sceneManager
    case smActiveScene sceneMgr of
      Just sceneId → do
        let node = (createSceneNode SpriteObject)
              { nodeId = objId
              , nodeTransform = defaultTransform { position = (x, y) }
              , nodeTexture = Just texHandle
              , nodeSize = (width, height)
              , nodeColor = Vec4 1 1 1 1
              , nodeVisible = True
              , nodeLayer = layer
              }
        case addObjectToScene sceneId node sceneMgr of
          Just (addedObjId, newSceneMgr) → do
            modify $ \s → s { sceneManager = newSceneMgr }
          Nothing → logDebugM CatLua $ "Failed to add sprite " <> T.pack (show objId)
      Nothing → logDebugM CatLua "Cannot spawn sprite: no active scene"

handleSetPos ∷ ObjectId → Float → Float → EngineM ε σ ()
handleSetPos objId x y =
    void $ modifySceneNode objId $ \node →
      node { nodeTransform = (nodeTransform node) { position = (x, y) } }

handleSetColor ∷ ObjectId → Vec4 → EngineM ε σ ()
handleSetColor objId color =
    void $ modifySceneNode objId $ \node → node { nodeColor = color }

handleSetSize ∷ ObjectId → Float → Float → EngineM ε σ ()
handleSetSize objId width height =
    void $ modifySceneNode objId $ \node → node { nodeSize = (width, height) }

handleSetVisible ∷ ObjectId → Bool → EngineM ε σ ()
handleSetVisible objId visible =
    void $ modifySceneNode objId $ \node → node { nodeVisible = visible }

handleDestroy ∷ ObjectId → EngineM ε σ ()
handleDestroy objId = void $ deleteSceneNode objId

-- Add this new function:
handleWorldPreview ∷ EngineM ε σ ()
handleWorldPreview = do
    env ← ask
    mPreview ← liftIO $ atomicModifyIORef' (worldPreviewRef env) $ \v → (Nothing, v)
    case mPreview of
        Nothing → pure ()
        Just (w, h, rgbaData) → do
            logInfoM CatWorld $ "Creating world preview texture: "
                <> T.pack (show w) <> "×" <> T.pack (show h)

            gs ← gets graphicsState
            case ( vulkanDevice gs
                 , vulkanPDevice gs
                 , vulkanCmdPool gs
                 , deviceQueues gs
                 , textureSystem gs ) of
                (Just dev, Just pdev, Just cmdPool, Just queues, Just bindless) → do
                    -- Generate a texture handle
                    poolRef ← asks assetPoolRef
                    pool ← liftIO $ readIORef poolRef
                    texHandle ← liftIO $ generateTextureHandle pool

                    let width  = fromIntegral w ∷ Word32
                        height = fromIntegral h ∷ Word32
                        bufSize = fromIntegral (BS.length rgbaData)
                        queue  = graphicsQueue queues

                    -- Create GPU image
                    image ← createVulkanImage dev pdev
                        (width, height)
                        FORMAT_R8G8B8A8_UNORM
                        IMAGE_TILING_OPTIMAL
                        (IMAGE_USAGE_TRANSFER_DST_BIT .|. IMAGE_USAGE_SAMPLED_BIT)
                        MEMORY_PROPERTY_DEVICE_LOCAL_BIT

                    -- Upload via staging buffer
                    locally $ do
                        (stagingMem, stagingBuf) ← createVulkanBuffer dev pdev bufSize
                            BUFFER_USAGE_TRANSFER_SRC_BIT
                            (MEMORY_PROPERTY_HOST_VISIBLE_BIT
                             .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)

                        stagingPtr ← mapMemory dev stagingMem 0 bufSize zero
                        liftIO $ BS.useAsCStringLen rgbaData $ \(srcPtr, len) →
                            copyBytes (castPtr stagingPtr) srcPtr len
                        unmapMemory dev stagingMem

                        runCommandsOnce dev cmdPool queue $ \cmdBuf → do
                            transitionImageLayout image FORMAT_R8G8B8A8_UNORM
                                Undef_TransDst 1 cmdBuf
                            copyBufferToImage cmdBuf stagingBuf image width height
                            transitionImageLayout image FORMAT_R8G8B8A8_UNORM
                                TransDst_ShaderRO 1 cmdBuf

                    -- Create image view and sampler
                    imageView ← createVulkanImageView dev image
                        FORMAT_R8G8B8A8_UNORM IMAGE_ASPECT_COLOR_BIT

                    sampler ← createTextureSampler dev pdev FILTER_NEAREST

                    -- Register in bindless system
                    (_, newBindless) ← registerTexture dev texHandle
                        imageView sampler bindless
                    modify $ \s → s { graphicsState = (graphicsState s) {
                        textureSystem = Just newBindless } }

                    -- Notify Lua
                    let (TextureHandle h) = texHandle
                    liftIO $ Q.writeQueue (luaQueue env)
                        (LuaWorldPreviewReady (fromIntegral h))

                    logInfoM CatWorld $ "World preview texture created: handle="
                        <> T.pack (show h)

                _ → logWarnM CatWorld
                        "Cannot create preview texture: Vulkan not ready"
