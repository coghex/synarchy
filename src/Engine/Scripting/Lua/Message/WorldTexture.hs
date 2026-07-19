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
import World.State.Types (wsZoomAtlasRef)
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
        Just (w, h, rgbaData, myGen) → do
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
                    -- in flight for the OLD session.
                    --
                    -- Round 8's fix (re-reading 'worldPreviewRef' here
                    -- and skipping the broadcast if something newer was
                    -- ALREADY dequeued) was itself flagged round 9/10:
                    -- a NEWER preview enqueued strictly BETWEEN that read
                    -- and this point would still be missed, since this
                    -- upload had already committed to announcing. Compare
                    -- against 'worldPreviewGenerationRef' instead — a
                    -- monotonic counter bumped once per enqueue and never
                    -- read back down (see 'Engine.Core.State') — rather
                    -- than re-reading 'worldPreviewRef' itself: if that
                    -- counter still equals the generation this upload was
                    -- dequeued under, NOTHING has been enqueued since,
                    -- full stop; no window remains to race, and a plain
                    -- 'readIORef' suffices since the comparison is a pure
                    -- read against a value that only ever increases.
                    latestGen ← liftIO $ readIORef (worldPreviewGenerationRef env)
                    if myGen ≢ latestGen
                      then
                        logInfoM CatWorld
                            "World preview upload superseded before \
                            \publish — discarding stale generation"
                      else do
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
        Just (w, h, rgbaData, targetStates) → do
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

                    -- Round 8/9 review (issue #763): this upload is
                    -- async and can take multiple frames (staging
                    -- buffer + Vulkan copy above), so re-reading
                    -- 'worldManagerRef' HERE to find "every current
                    -- world" would race a load publish that swaps it
                    -- in the meantime — a peek-then-act check on
                    -- 'zoomAtlasDataRef' narrows that window but can't
                    -- close it (round 8's attempt was itself flagged
                    -- non-atomic in round 9). Writing to 'targetStates'
                    -- — the EXACT 'WorldState's captured back when this
                    -- atlas was enqueued (see 'EngineEnv.zoomAtlasDataRef'
                    -- and 'World.Load.Publish'/'World.Thread.Command.Init')
                    -- — needs no live ref re-read at all, so there is no
                    -- window left to race: whichever session enqueued
                    -- this atlas is exactly who receives it, regardless
                    -- of what 'worldManagerRef' holds by the time the
                    -- upload finishes.
                    forM_ targetStates $ \ws →
                        liftIO $ writeIORef (wsZoomAtlasRef ws) (Just atlasInfo)

                    logInfoM CatWorld $ "Zoom atlas uploaded: handle="
                        <> T.pack (show texHandle) <> ", chunksPerRow="
                        <> T.pack (show chunksPerRow)

                _ → logWarnM CatWorld
                        "Cannot upload zoom atlas: Vulkan not ready"
