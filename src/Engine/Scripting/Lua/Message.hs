module Engine.Scripting.Lua.Message
  ( processLuaMessages
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Storable as Vec
import Control.Monad (foldM)
import Data.IORef (readIORef, atomicModifyIORef', writeIORef)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import System.FilePath (takeBaseName)
import qualified Codec.Picture as JP
import Engine.Asset.Base (AssetId)
import Engine.Asset.Handle
import Engine.Asset.Manager
import Engine.Asset.Types
import Engine.Core.Error.Exception (ExceptionType(..), GraphicsError(..))
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logAndThrowM, logDebugM, logInfoM, logWarnM, logDebugSM)
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Types (ecHeadless)
import Engine.Core.Resource (locally)
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Config (WindowMode(..)
                               , VideoConfig(..)
                               , textureFilterToText
                               , textureFilterToVulkan)
import Engine.Graphics.Font.Load (loadSDFFont)
import Engine.Graphics.Types (DevQueues(..))
import Engine.Graphics.Vulkan.Base (TextureInfo(..))
import Engine.Graphics.Vulkan.Image (createVulkanImage', createVulkanImageView'
                                    , copyBufferToImage, VulkanImage(..))
import Engine.Graphics.Vulkan.Buffer (createVulkanBuffer)
import Engine.Graphics.Vulkan.Command (runCommandsOnce)
import Engine.Graphics.Vulkan.Types.Vertex (Vec4(..))
import Engine.Graphics.Vulkan.Recreate (recreateSwapchain)
import Engine.Graphics.Vulkan.Texture (transitionImageLayout
                                      , ImageLayoutTransition(..))
import Engine.Graphics.Vulkan.Sampler.Cache ( acquireSampler, releaseSampler
                                            , SamplerKind(..))
import Engine.Graphics.Vulkan.Texture.Bindless (setTextureFilter, registerPinnedTexture
                                              , registerTexture
                                               , unregisterTexture, writeHandleSlotEntry)
import Engine.Graphics.Vulkan.Texture.Handle (BindlessTextureHandle(..))
import Engine.Graphics.Vulkan.Texture.Slot (TextureSlot(..))
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Window.Types (Window(..))
import Engine.Scene.Base
import Engine.Scene.Graph (modifySceneNode, deleteSceneNode)
import Engine.Scene.Manager (addObjectToScene)
import Engine.Scene.Types
import Engine.Scripting.Lua.Types
import World.Render.Zoom.Types (ZoomAtlasInfo(..), zoomTileSize)
import World.Render.BloodQuads (uploadBloodTextures)
import World.State.Types (WorldManager(..), WorldState(..), bumpQuadCacheGen)
import qualified Graphics.UI.GLFW as GLFW
import Vulkan.Core10
import Vulkan.Zero (zero)

data TextureUploadPrep = TextureUploadPrep
    { tupHandle    ∷ !TextureHandle
    , tupPath      ∷ !FilePath
    , tupAssetId   ∷ !AssetId
    , tupWidth     ∷ !Word32
    , tupHeight    ∷ !Word32
    , tupPixels    ∷ !(ForeignPtr Word8)
    , tupPixelLen  ∷ !Int
    , tupImage     ∷ !VulkanImage
    , tupCleanImage ∷ !(IO ())
    }

-- | Invalidate every loaded world's render caches after a texture load.
--   Iterates ALL worlds in 'wmWorlds', not just 'wmVisible', so a world
--   whose dependent textures finish loading *before* it is shown still
--   refreshes when displayed (the old visible-only sweep missed it). The
--   close-up quad cache is invalidated via the race-safe generation
--   counter ('bumpQuadCacheGen'); the zoom/background caches are nulled
--   directly (they have a single writer and no cross-thread rebuild race).
invalidateAllWorldRenderCaches ∷ EngineEnv → IO ()
invalidateAllWorldRenderCaches env = do
    mgr ← readIORef (worldManagerRef env)
    forM_ (wmWorlds mgr) $ \(_, ws) → do
        bumpQuadCacheGen ws
        writeIORef (wsZoomQuadCacheRef ws) Nothing
        writeIORef (wsBgQuadCacheRef ws) Nothing

processLuaMessages ∷ EngineM ε σ ()
processLuaMessages = do
    env ← ask
    messages ← liftIO $ Q.flushQueue (luaToEngineQueue env)
    
    when (not $ null messages) $
        logDebugSM CatLua "Processing Lua messages"
            [("count", T.pack $ show $ length messages)]
    
    process messages
    whenGraphical handleWorldPreview
    whenGraphical handleZoomAtlasUpload
    whenGraphical uploadBloodTextures
  where
    process [] = pure ()
    process (LuaLoadTextureRequest handle path : rest) = do
        let (burst, rest') = span isTextureLoad rest
            requests = (handle, path) : unwrapTextureLoads burst
        whenGraphical $ handleLoadTextureBatch requests
        process rest'
    process (msg : rest) = do
        handleLuaMessage msg
        process rest

    isTextureLoad (LuaLoadTextureRequest _ _) = True
    isTextureLoad _                           = False

    unwrapTextureLoads msgs =
        [ (handle, path) | LuaLoadTextureRequest handle path ← msgs ]

-- | Run a GPU-touching action only in graphical mode; skip it when
--   headless (no device). Lets the single 'handleLuaMessage' serve both
--   the graphical loop and the headless/dump loop — the scene-graph and
--   pure-IORef cases run in both; only GPU operations are gated. (Before
--   this, a separate 'handleLuaMessageHeadless' duplicated every
--   scene-graph case and had already drifted from this one.)
whenGraphical ∷ EngineM ε σ () → EngineM ε σ ()
whenGraphical act = do
    env ← ask
    if ecHeadless (engineConfig env) then pure () else act

handleLuaMessage ∷ LuaToEngineMsg → EngineM ε σ ()
handleLuaMessage msg = do
    case msg of
        LuaSetWindowMode mode → whenGraphical $ do
            logDebugM CatLua $ "Setting window mode: " <> T.pack (show mode)
            handleSetWindowMode mode

        LuaSetResolution w h → whenGraphical $ do
            logDebugSM CatLua "Setting resolution"
                [("width", T.pack $ show w)
                ,("height", T.pack $ show h)]
            handleSetResolution w h

        LuaSetVSync enabled → whenGraphical $ do
            logDebugSM CatLua "Setting VSync"
                [("enabled", if enabled then "true" else "false")]
            handleSetVSync enabled

        LuaSetMSAA msaa → whenGraphical $ do
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

        LuaSetTextureFilter tf → whenGraphical $ do
            logInfoM CatTexture $ "Texture filter changed to: " <> textureFilterToText tf
            env ← ask
            liftIO $ writeIORef (textureFilterRef env) tf
            gs ← gets graphicsState
            mBindless ← liftIO $ readIORef (textureSystemRef env)
            case (vulkanDevice gs, mBindless) of
                (Just dev, Just bindless) → do
                    newBindless ← setTextureFilter dev (textureFilterToVulkan tf) bindless
                    liftIO $ writeIORef (textureSystemRef env) (Just newBindless)
                    logInfoM CatTexture "All texture samplers updated live"
                _ → pure ()

        LuaLoadFontRequest handle path size → whenGraphical $ do
            logDebugSM CatLua "Loading font"
                [("path", T.pack path)
                ,("size", T.pack $ show size)
                ,("handle", T.pack (show handle))]
            handleLoadFont handle path size

        LuaLoadTextureRequest handle path → whenGraphical $ do
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

        -- The remaining 'LuaToEngineMsg' constructors (logging, focus,
        -- sprite-scale, etc.) are consumed by the Lua thread's own
        -- 'processLuaMsg' on a different queue, never this engine queue.
        -- If one ever arrives here it's a routing bug — log it rather
        -- than crash (this case used to be a partial match).
        other →
            logWarnM CatLua $
                "handleLuaMessage: unexpected message on engine queue: "
                <> T.pack (show other)

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
                writeIORef (windowSizeRef env) (winW, winH)
                writeIORef (framebufferSizeRef env) (fbW, fbH)
                
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
                                        GLFW.setWindowed win monW monH 0 0
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
                        GLFW.setWindowAttrib win GLFW.WindowAttrib'Decorated True
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
    env ← ask
    liftIO $ atomicModifyIORef' (videoConfigRef env) $ \c →
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
    liftIO $ atomicModifyIORef' (videoConfigRef env) $ \c →
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
    liftIO $ writeIORef (brightnessRef env) brightness
    logDebugM CatGraphics $ "Brightness set to " <> T.pack (show pct) <> "%"

handleSetPixelSnap ∷ Bool → EngineM ε σ ()
handleSetPixelSnap enabled = do
    env ← ask
    liftIO $ writeIORef (pixelSnapRef env) enabled
    logDebugM CatGraphics $ "Pixel snap " <> if enabled then "enabled" else "disabled"

duplicateCachedTextureHandle ∷ EngineEnv → TextureHandle → AssetId
                           → TextureAtlas → EngineM ε σ ()
duplicateCachedTextureHandle env handle assetId atlas = do
    poolRef ← asks assetPoolRef
    pool ← liftIO $ readIORef poolRef
    mBindless ← liftIO $ readIORef (textureSystemRef env)
    case mBindless of
        Just bindless →
            case Map.lookup (taTextureHandle atlas) (btsHandleMap bindless) of
                Just existingBindlessHandle → do
                    liftIO $ writeIORef (textureSystemRef env) (Just bindless
                        { btsHandleMap =
                            Map.insert handle existingBindlessHandle
                                (btsHandleMap bindless)
                        })
                    -- Atlas-share path: sync the shader handle→slot table
                    -- too (the ptr is shared across the immutable copy) (#286).
                    liftIO $ writeHandleSlotEntry bindless (toInt handle)
                        (tsIndex (bthSlot existingBindlessHandle))
                Nothing → logWarnM CatAsset $
                    "Cached texture missing bindless slot for "
                        <> taPath atlas
        Nothing →
            logWarnM CatAsset "No bindless system available for cached texture reuse"

    liftIO $ do
        updateTextureState handle (AssetReady assetId []) pool
        atomicModifyIORef' poolRef $ \p →
            ( p { apTextureAtlases =
                    Map.adjust (\a → a { taRefCount = taRefCount a + 1 })
                        assetId (apTextureAtlases p)
              }
            , ()
            )
        let (w, h) = amDimensions (taMetadata atlas)
            (TextureHandle rawHandle) = handle
        atomicModifyIORef' (textureSizeRef env) $ \m →
            (HM.insert handle (fromIntegral w, fromIntegral h) m, ())
        Q.writeQueue (luaQueue env)
            (LuaAssetLoaded "texture" (fromIntegral rawHandle) (taPath atlas))

prepareTextureUpload ∷ AssetPool → Device → PhysicalDevice
                     → (TextureHandle, FilePath)
                     → EngineM ε σ TextureUploadPrep
prepareTextureUpload pool dev pdev (handle, path) = do
    assetId ← liftIO $ generateAssetId pool
    JP.Image { JP.imageWidth, JP.imageHeight, JP.imageData }
      ← liftIO (JP.readImage path) ⌦ \case
        Left err → logAndThrowM CatTexture (ExGraphics TextureLoadFailed)
                     $ "cannot load texture image: " <> T.pack err
        Right dynImg → pure $ JP.convertRGBA8 dynImg

    let (pixelPtr, pixelLen) = Vec.unsafeToForeignPtr0 imageData
        width = fromIntegral imageWidth
        height = fromIntegral imageHeight

    (image, cleanImage) ← createVulkanImage' dev pdev
        (width, height)
        FORMAT_R8G8B8A8_UNORM
        IMAGE_TILING_OPTIMAL
        (IMAGE_USAGE_TRANSFER_DST_BIT .|. IMAGE_USAGE_SAMPLED_BIT)
        MEMORY_PROPERTY_DEVICE_LOCAL_BIT

    pure TextureUploadPrep
        { tupHandle = handle
        , tupPath = path
        , tupAssetId = assetId
        , tupWidth = width
        , tupHeight = height
        , tupPixels = pixelPtr
        , tupPixelLen = pixelLen
        , tupImage = image
        , tupCleanImage = cleanImage
        }

handleLoadTextureBatch ∷ [(TextureHandle, FilePath)] → EngineM ε σ ()
handleLoadTextureBatch [] = pure ()
handleLoadTextureBatch requests = do
    env ← ask
    poolRef ← asks assetPoolRef
    pool ← liftIO $ readIORef poolRef

    let (cachedReqs, freshReqs, aliasReqs, _) =
            foldl'
                (\(cached, fresh, aliases, seen) (handle, path) →
                    let key = T.pack path
                    in case Map.lookup key (apAssetPaths pool) of
                        Just assetId →
                            case Map.lookup assetId (apTextureAtlases pool) of
                                Just atlas →
                                    ((handle, assetId, atlas) : cached,
                                     fresh, aliases, seen)
                                Nothing →
                                    (cached, (handle, path) : fresh,
                                     aliases, Map.insert key handle seen)
                        Nothing →
                            case Map.lookup key seen of
                                Just canonical →
                                    (cached, fresh, (handle, canonical) : aliases, seen)
                                Nothing →
                                    (cached, (handle, path) : fresh,
                                     aliases, Map.insert key handle seen)
                )
                ([], [], [], Map.empty)
                requests

    forM_ (reverse cachedReqs) $ \(handle, assetId, atlas) →
        duplicateCachedTextureHandle env handle assetId atlas

    let invalidateRenderCaches = liftIO $ invalidateAllWorldRenderCaches env

    when (not (null freshReqs)) $ do
        gs ← gets graphicsState
        mBindless ← liftIO $ readIORef (textureSystemRef env)
        case (vulkanDevice gs, vulkanPDevice gs, vulkanCmdPool gs, deviceQueues gs, mBindless) of
            (Just dev, Just pdev, Just cmdPool, Just queues, Just bindless0) → do
                preps ← mapM (prepareTextureUpload pool dev pdev) (reverse freshReqs)
                let queue = graphicsQueue queues

                locally $ do
                    stagingBuffers ← forM preps $ \prep → do
                        let bufSize = fromIntegral (tupPixelLen prep)
                        (stagingMem, stagingBuf) ← createVulkanBuffer dev pdev bufSize
                            BUFFER_USAGE_TRANSFER_SRC_BIT
                            (MEMORY_PROPERTY_HOST_VISIBLE_BIT
                             .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
                        stagingPtr ← mapMemory dev stagingMem 0 bufSize zero
                        liftIO $ withForeignPtr (tupPixels prep) $ \srcPtr →
                            copyBytes (castPtr stagingPtr) srcPtr (tupPixelLen prep)
                        unmapMemory dev stagingMem
                        pure (stagingMem, stagingBuf)

                    runCommandsOnce dev cmdPool queue $ \cmdBuf →
                        forM_ (zip preps stagingBuffers) $ \(prep, (_, stagingBuf)) → do
                            transitionImageLayout (tupImage prep) FORMAT_R8G8B8A8_UNORM
                                Undef_TransDst 1 cmdBuf
                            copyBufferToImage cmdBuf stagingBuf (tupImage prep)
                                (tupWidth prep) (tupHeight prep)
                            transitionImageLayout (tupImage prep) FORMAT_R8G8B8A8_UNORM
                                TransDst_ShaderRO 1 cmdBuf

                (loaded, bindlessN) ← foldM
                    (\(acc, bindless) prep → do
                        let VulkanImage image imageMemory = tupImage prep
                        (imageView, cleanView) ← createVulkanImageView' dev (tupImage prep)
                            FORMAT_R8G8B8A8_UNORM IMAGE_ASPECT_COLOR_BIT
                        (mbHandle, bindless') ← registerTexture dev (tupHandle prep)
                            imageView (btsTextureSampler bindless) bindless
                        when (isNothing mbHandle) $
                            logWarnM CatTexture $
                                "Failed to allocate bindless slot for texture: "
                                    <> T.pack (tupPath prep)
                        let bindlessSlot = fmap (tsIndex ∘ bthSlot) mbHandle
                            atlas = TextureAtlas
                                { taId = tupAssetId prep
                                , taName = T.pack (takeBaseName (tupPath prep))
                                , taPath = T.pack (tupPath prep)
                                , taMetadata = AtlasMetadata
                                    (tupWidth prep, tupHeight prep)
                                    FORMAT_R8G8B8A8_UNORM
                                    Map.empty
                                , taInfo = Just TextureInfo
                                    { tiImage = image
                                    , tiView = imageView
                                    , tiMemory = imageMemory
                                    , tiLayout = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                                    }
                                , taRefCount = 1
                                , taCleanup = Just (cleanView >> tupCleanImage prep)
                                , taBindlessSlot = bindlessSlot
                                , taTextureHandle = tupHandle prep
                                }
                            (TextureHandle rawHandle) = tupHandle prep
                        liftIO $ do
                            updateTextureState (tupHandle prep)
                                (AssetReady (tupAssetId prep) []) pool
                            atomicModifyIORef' poolRef $ \p →
                                ( p { apTextureAtlases =
                                        Map.insert (tupAssetId prep) atlas
                                            (apTextureAtlases p)
                                    , apAssetPaths =
                                        Map.insert (T.pack (tupPath prep))
                                            (tupAssetId prep) (apAssetPaths p)
                                    }
                                , ()
                                )
                            atomicModifyIORef' (textureSizeRef env) $ \m →
                                ( HM.insert (tupHandle prep)
                                    (fromIntegral (tupWidth prep), fromIntegral (tupHeight prep)) m
                                , ()
                                )
                            Q.writeQueue (luaQueue env)
                                (LuaAssetLoaded "texture" (fromIntegral rawHandle)
                                    (T.pack (tupPath prep)))
                        pure (((tupHandle prep, tupAssetId prep, atlas) : acc), bindless'))
                    ([], bindless0)
                    preps

                liftIO $ writeIORef (textureSystemRef env) (Just bindlessN)

                let loadedMap = Map.fromList
                        [ (handle, (assetId, atlas))
                        | (handle, assetId, atlas) ← loaded
                        ]
                forM_ (reverse aliasReqs) $ \(handle, canonical) →
                    case Map.lookup canonical loadedMap of
                        Just (assetId, atlas) →
                            duplicateCachedTextureHandle env handle assetId atlas
                        Nothing →
                            logWarnM CatTexture $
                                "Missing canonical texture for deduped alias: "
                                    <> T.pack (show handle)
                invalidateRenderCaches

            _ → logWarnM CatTexture "Cannot batch-load textures: Vulkan not ready"

handleLoadTexture ∷ TextureHandle → FilePath → EngineM ε σ ()
handleLoadTexture handle path = do
    logDebugM CatLua $ "Loading texture from Lua: " <> T.pack path
                    <> " (handle: " <> T.pack (show handle) <> ")"
    handleLoadTextureBatch [(handle, path)]
    logDebugM CatLua $ "Texture loaded successfully: " <> T.pack path

handleLoadFont ∷ FontHandle → FilePath → Int → EngineM ε σ ()
handleLoadFont handle path _size = do
    logDebugM CatLua $ "Loading font from Lua: " <> T.pack path
    actualHandle ← loadSDFFont handle path
    env ← ask
    let etlq = luaQueue env
    liftIO $ Q.writeQueue etlq (LuaFontLoaded actualHandle path)
    let (FontHandle h) = actualHandle
    liftIO $ Q.writeQueue etlq (LuaAssetLoaded "font" (fromIntegral h) (T.pack path))
    logDebugM CatLua $ "Font loaded successfully: " <> T.pack path

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
          Just (_addedObjId, newSceneMgr) → do
            modify $ \s → s { sceneManager = newSceneMgr }
            env ← ask
            liftIO $ atomicModifyIORef' (textBuffersRef env) $ \m →
              (Map.insert oid text m, ())
          Nothing → logDebugM CatLua $ "Failed to add text object " <> T.pack (show oid)
      Nothing → logDebugM CatLua "Cannot spawn text: no active scene"

handleSetText ∷ ObjectId → Text → EngineM ε σ ()
handleSetText objId text = do
    env ← ask
    liftIO $ atomicModifyIORef' (textBuffersRef env) $ \m →
      (Map.insert objId text m, ())
    -- Bool result ignored: setText on a missing node is a no-op by design.
    _ ← modifySceneNode objId $ \node → node { nodeText = Just text }
    return ()

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
          Just (_addedObjId, newSceneMgr) → do
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

-- | Dispose a superseded transient texture (zoom atlas / world
--   preview): free its bindless slot and destroy its GPU objects.
--   The old image may still be sampled by in-flight frames
--   (UPDATE_AFTER_BIND descriptor writes race with pending
--   execution), so wait for the device to idle first — callers run
--   once per world init/load, where the stall is invisible.
--   unregisterTexture points the slot at the undefined texture and
--   recycles it.
disposeTransientTexture ∷ Device → TransientTexture → EngineM ε σ ()
disposeTransientTexture dev old = do
    env ← ask
    liftIO $ deviceWaitIdle dev
    mSys ← liftIO $ readIORef (textureSystemRef env)
    case mSys of
        Just sys → do
            sys' ← unregisterTexture dev (ttHandle old) sys
            liftIO $ writeIORef (textureSystemRef env) (Just sys')
        Nothing → pure ()
    liftIO $ ttCleanup old

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
            mBindless ← liftIO $ readIORef (textureSystemRef env)
            case ( vulkanDevice gs
                 , vulkanPDevice gs
                 , vulkanCmdPool gs
                 , deviceQueues gs
                 , mBindless ) of
                (Just dev, Just pdev, Just cmdPool, Just queues, Just bindless) → do
                    poolRef ← asks assetPoolRef
                    pool ← liftIO $ readIORef poolRef
                    texHandle ← liftIO $ generateTextureHandle pool

                    let width  = fromIntegral w ∷ Word32
                        height = fromIntegral h ∷ Word32
                        bufSize = fromIntegral (BS.length rgbaData)
                        queue  = graphicsQueue queues

                    -- Prime variants: explicit cleanups, NOT exit-time
                    -- allocResource — this texture is replaced on every
                    -- world init/load and must be destroyable then.
                    (image, cleanImage) ← createVulkanImage' dev pdev
                        (width, height)
                        FORMAT_R8G8B8A8_UNORM
                        IMAGE_TILING_OPTIMAL
                        (IMAGE_USAGE_TRANSFER_DST_BIT .|. IMAGE_USAGE_SAMPLED_BIT)
                        MEMORY_PROPERTY_DEVICE_LOCAL_BIT

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

                    (imageView, cleanView) ← createVulkanImageView' dev image
                        FORMAT_R8G8B8A8_UNORM IMAGE_ASPECT_COLOR_BIT

                    -- Preview registers with NEAREST (shares the cached
                    -- nearest sampler). A live filter toggle repaints all
                    -- slots to the global sampler until the next regen —
                    -- same as the pre-cache behaviour.
                    let cacheRef = samplerCacheRef env
                    sampler ← liftIO $ acquireSampler dev cacheRef SamplerTextureNearest
                    let cleanSampler = releaseSampler dev cacheRef SamplerTextureNearest

                    (_, newBindless) ← registerPinnedTexture dev texHandle
                        imageView sampler bindless
                    liftIO $ writeIORef (textureSystemRef env) (Just newBindless)

                    -- Dispose the previous preview generation (slot
                    -- recycled, GPU objects destroyed) and record this
                    -- one. View before image: the view references it.
                    forM_ (previewTexture gs) (disposeTransientTexture dev)
                    let cleanupAll = cleanView >> cleanImage >> cleanSampler
                    modify $ \st → st { graphicsState =
                        (graphicsState st)
                            { previewTexture =
                                Just (TransientTexture texHandle cleanupAll) } }

                    let (TextureHandle h) = texHandle
                    liftIO $ Q.writeQueue (luaQueue env)
                        (LuaWorldPreviewReady (fromIntegral h))

                    logInfoM CatWorld $ "World preview texture created: handle="
                        <> T.pack (show h)

                _ → logWarnM CatWorld
                        "Cannot create preview texture: Vulkan not ready"

-- | Poll for pending zoom atlas pixel data and upload to GPU.
--   Called every frame.  When the world thread produces atlas data,
--   this creates a GPU texture and stores the ZoomAtlasInfo on all
--   visible world states.
handleZoomAtlasUpload ∷ EngineM ε σ ()
handleZoomAtlasUpload = do
    env ← ask
    mAtlas ← liftIO $ atomicModifyIORef' (zoomAtlasDataRef env) $ \v → (Nothing, v)
    case mAtlas of
        Nothing → pure ()
        Just (w, h, rgbaData) → do
            logInfoM CatWorld $ "Uploading zoom atlas texture: "
                <> T.pack (show w) <> "×" <> T.pack (show h)

            gs ← gets graphicsState
            mBindless ← liftIO $ readIORef (textureSystemRef env)
            case ( vulkanDevice gs
                 , vulkanPDevice gs
                 , vulkanCmdPool gs
                 , deviceQueues gs
                 , mBindless ) of
                (Just dev, Just pdev, Just cmdPool, Just queues, Just bindless) → do
                    poolRef ← asks assetPoolRef
                    pool ← liftIO $ readIORef poolRef
                    texHandle ← liftIO $ generateTextureHandle pool

                    let width  = fromIntegral w ∷ Word32
                        height = fromIntegral h ∷ Word32
                        bufSize = fromIntegral (BS.length rgbaData)
                        queue  = graphicsQueue queues

                    -- Prime variants: explicit cleanups, NOT exit-time
                    -- allocResource — this texture is replaced on every
                    -- world init/load and must be destroyable then.
                    (image, cleanImage) ← createVulkanImage' dev pdev
                        (width, height)
                        FORMAT_R8G8B8A8_UNORM
                        IMAGE_TILING_OPTIMAL
                        (IMAGE_USAGE_TRANSFER_DST_BIT .|. IMAGE_USAGE_SAMPLED_BIT)
                        MEMORY_PROPERTY_DEVICE_LOCAL_BIT

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

                    -- Create image view and sampler (LINEAR for smooth zoom)
                    (imageView, cleanView) ← createVulkanImageView' dev image
                        FORMAT_R8G8B8A8_UNORM IMAGE_ASPECT_COLOR_BIT

                    -- Zoom atlas registers with LINEAR for smooth scaling
                    -- (shares the cached linear sampler). A live filter
                    -- toggle repaints all slots to the global sampler
                    -- until the next regen — same as pre-cache behaviour.
                    let cacheRef = samplerCacheRef env
                    sampler ← liftIO $ acquireSampler dev cacheRef SamplerTextureLinear
                    let cleanSampler = releaseSampler dev cacheRef SamplerTextureLinear

                    (_, newBindless) ← registerPinnedTexture dev texHandle
                        imageView sampler bindless
                    liftIO $ writeIORef (textureSystemRef env) (Just newBindless)

                    -- Dispose the previous atlas generation (slot
                    -- recycled, GPU objects destroyed) and record this
                    -- one. View before image: the view references it.
                    forM_ (zoomAtlasTexture gs) (disposeTransientTexture dev)
                    let cleanupAll = cleanView >> cleanImage >> cleanSampler
                    modify $ \st → st { graphicsState =
                        (graphicsState st)
                            { zoomAtlasTexture =
                                Just (TransientTexture texHandle cleanupAll) } }

                    let chunksPerRow = w `div` zoomTileSize
                        atlasInfo = ZoomAtlasInfo
                            { zaiTexture     = texHandle
                            , zaiWidth       = w
                            , zaiHeight      = h
                            , zaiChunksPerRow = chunksPerRow
                            }

                    worldManager ← liftIO $ readIORef (worldManagerRef env)
                    forM_ (wmWorlds worldManager) $ \(_pageId, ws) →
                        liftIO $ writeIORef (wsZoomAtlasRef ws) (Just atlasInfo)

                    logInfoM CatWorld $ "Zoom atlas uploaded: handle="
                        <> T.pack (show texHandle) <> ", chunksPerRow="
                        <> T.pack (show chunksPerRow)

                _ → logWarnM CatWorld
                        "Cannot upload zoom atlas: Vulkan not ready"
