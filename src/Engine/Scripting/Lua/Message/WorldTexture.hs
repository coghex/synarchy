-- | World-generated GPU texture upload Lua message handlers (split out
--   of 'Engine.Scripting.Lua.Message', #558): the world-preview
--   thumbnail and the zoom-level background atlas. Both upload raw
--   RGBA pixel bytes handed over by the world thread (not a file load
--   — see 'Engine.Scripting.Lua.Message.Texture' for that) and share
--   the same superseded-generation disposal path.
module Engine.Scripting.Lua.Message.WorldTexture
    ( disposeTransientTexture
    , handleWorldPreview
    , handleZoomAtlasUpload
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.IORef (readIORef, atomicModifyIORef', writeIORef)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Asset.Manager (generateTextureHandle)
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logInfoM, logWarnM)
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Resource (locally)
import qualified Engine.Core.Queue as Q
import Foreign.Marshal.Utils (copyBytes)
import Engine.Graphics.Vulkan.Image (createVulkanImage', createVulkanImageView'
                                    , copyBufferToImage)
import Engine.Graphics.Vulkan.Buffer (createVulkanBuffer)
import Engine.Graphics.Vulkan.Command (runCommandsOnce)
import Engine.Graphics.Vulkan.Texture (transitionImageLayout
                                      , ImageLayoutTransition(..))
import Engine.Graphics.Vulkan.Sampler.Cache ( acquireSampler, releaseSampler
                                            , SamplerKind(..))
import Engine.Graphics.Vulkan.Texture.Bindless (registerPinnedTexture, unregisterTexture)
import Engine.Graphics.Types (DevQueues(..))
import Engine.Scripting.Lua.Types
import World.Render.Zoom.Types (ZoomAtlasInfo(..), zoomTileSize)
import World.State.Types (WorldManager(..), wsZoomAtlasRef)
import Vulkan.Core10
import Vulkan.Zero (zero)

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

                    -- Round 8 review: the same stale-generation window
                    -- 'handleZoomAtlasUpload' guards against below — a
                    -- load's own preview ('World.Load.Publish
                    -- .publishStagedSession', which writes
                    -- 'worldPreviewRef' before swapping
                    -- 'worldManagerRef') can overtake an upload already
                    -- in flight for the OLD session. Re-check
                    -- 'worldPreviewRef' before announcing this handle
                    -- to Lua: if something newer is already pending,
                    -- this preview is stale — skip the broadcast and
                    -- let the next poll (which dequeues the newer data)
                    -- announce the right generation instead.
                    stillCurrent ← liftIO $ readIORef (worldPreviewRef env)
                    case stillCurrent of
                        Just _ →
                            logInfoM CatWorld
                                "World preview upload superseded before \
                                \publish — discarding stale generation"
                        Nothing → do
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

                    -- Round 8 review (issue #763): this upload is async
                    -- and can take multiple frames (staging buffer +
                    -- Vulkan copy above), so a LOAD's own atlas
                    -- (World.Load.Publish.publishStagedSession, which
                    -- writes 'zoomAtlasDataRef' before swapping
                    -- 'worldManagerRef') can be queued and even
                    -- overtake an upload that was ALREADY in flight for
                    -- the OLD session. Re-check 'zoomAtlasDataRef'
                    -- right before writing to every current world: if
                    -- something newer is already pending, the atlas
                    -- this call just finished uploading is stale —
                    -- skip the write and let the NEXT poll (which will
                    -- dequeue that newer data) do it instead, rather
                    -- than momentarily pointing a freshly-published
                    -- session's pages at the wrong generation's
                    -- texture. 'zoomAtlasDataRef' is written strictly
                    -- BEFORE 'worldManagerRef' is swapped at publish,
                    -- so by the time 'worldManagerRef' shows the new
                    -- pages, this ref is guaranteed to already be
                    -- non-empty if a load raced this upload.
                    stillCurrent ← liftIO $ readIORef (zoomAtlasDataRef env)
                    case stillCurrent of
                        Just _ →
                            logInfoM CatWorld
                                "Zoom atlas upload superseded before \
                                \publish — discarding stale generation"
                        Nothing → do
                            worldManager ← liftIO $ readIORef (worldManagerRef env)
                            forM_ (wmWorlds worldManager) $ \(_pageId, ws) →
                                liftIO $ writeIORef (wsZoomAtlasRef ws) (Just atlasInfo)

                            logInfoM CatWorld $ "Zoom atlas uploaded: handle="
                                <> T.pack (show texHandle) <> ", chunksPerRow="
                                <> T.pack (show chunksPerRow)

                _ → logWarnM CatWorld
                        "Cannot upload zoom atlas: Vulkan not ready"
