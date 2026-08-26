-- | GPU texture (and font) upload Lua message handlers (split out of
--   'Engine.Scripting.Lua.Message', #558): batched image-file loads,
--   cached-atlas / deduped-alias fast paths, render-cache invalidation
--   after a load, and SDF font loads. World-preview and zoom-atlas
--   uploads (raw pixel bytes, not file loads) live in
--   'Engine.Scripting.Lua.Message.WorldTexture' instead.
module Engine.Scripting.Lua.Message.Texture
    ( handleLoadTextureBatch
    , handleLoadAtlasTextureBatch
    , UploadSampler(..)
    , cacheEntryReusable
    , handleLoadTexture
    , handleLoadFont
    ) where

import UPrelude
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
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
import Engine.Core.Log.Monad (logAndThrowM, logDebugM, logWarnM)
import Engine.Core.Monad
import Engine.Core.State (EngineEnv, EngineState(..), GraphicsState(..)
  , luaQueue )
import Engine.Core.Capability.Render
  (RenderCapability(..), toRenderCapability)
import Engine.Core.Resource (locally)
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Font.Load (loadSDFFont)
import Engine.Graphics.Vulkan.Base (TextureInfo(..))
import Engine.Graphics.Vulkan.Image (createVulkanImage', createVulkanImageView'
                                    , copyBufferToImage, VulkanImage(..))
import Engine.Graphics.Vulkan.Buffer (createVulkanBuffer)
import Engine.Graphics.Vulkan.Command (runCommandsOnce)
import Engine.Graphics.Vulkan.Texture (transitionImageLayout
                                      , ImageLayoutTransition(..))
import Engine.Graphics.Vulkan.Sampler.Cache (acquireSampler, releaseSampler)
import Engine.Graphics.Vulkan.Sampler.Types (SamplerKind(..))
import Engine.Graphics.Vulkan.Texture.Bindless (registerPinnedTexture
                                               , registerTexture
                                               , writeHandleSlotEntry)
import Engine.Graphics.Vulkan.Texture.Publish
  (UploadSampler(..), TexturePublish(..), GpuCleanupStep(..)
  , classifyRegistration, cachedAliasPublish, aliasPublish, publishedSlot
  , publishFailureReason, publishRegisteredEntries, failedUploadCleanup)
import Engine.Graphics.Vulkan.Texture.Handle (BindlessTextureHandle(..))
import Engine.Graphics.Vulkan.Texture.Slot (TextureSlot(..))
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Types (DevQueues(..))
import Engine.Scripting.Lua.Types
import World.State.Types (WorldManager(..), WorldState(..), bumpQuadCacheGen)
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
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    forM_ (wmWorlds mgr) $ \(_, ws) → do
        bumpQuadCacheGen ws
        writeIORef (wsZoomQuadCacheRef ws) Nothing
        writeIORef (wsBgQuadCacheRef ws) Nothing

-- | Settle a texture request on its TERMINAL FAILURE (#1690).
--
--   The counterpart of the success publication below, and deliberately
--   the only other way a request can end. It writes 'AssetFailed' —
--   until #1690 that state had no producer anywhere in the tree — and
--   announces the failure on its OWN queue message. It must never route
--   through 'LuaAssetLoaded', which is the success-only protocol Lua
--   sees as @onAssetLoaded@: a waiter that treated a failure as a load
--   would then read a handle resolving to the undefined texture.
--
--   Nothing a success writes is written: no @apTextureAtlases@ entry, no
--   @apAssetPaths@ entry (so the path is NOT poisoned and a later
--   request re-uploads instead of taking a lying cache hit), and no
--   texture size entry.
publishTextureFailure ∷ EngineEnv → AssetPool → TextureHandle → Text → Text
                      → EngineM σ ()
publishTextureFailure env pool handle path reason = do
    logWarnM CatTexture $ "Texture request failed for " <> path
        <> ": " <> reason
    liftIO $ do
        updateTextureState handle (AssetFailed reason) pool
        let (TextureHandle rawHandle) = handle
        Q.writeQueue (luaQueue env)
            (LuaAssetFailed "texture" (fromIntegral rawHandle) path reason)

duplicateCachedTextureHandle ∷ EngineEnv → TextureHandle → AssetId
                           → TextureAtlas → EngineM σ ()
duplicateCachedTextureHandle env handle assetId atlas = do
    poolRef ← asks (rcAssetPoolRef . toRenderCapability)
    pool ← liftIO $ readIORef poolRef
    mBindless ← liftIO $ readIORef (rcTextureSystemRef (toRenderCapability env))
    -- Both ways this resolution can come up empty — no bindless system
    -- at all, or an atlas with no @btsHandleMap@ entry — mean the same
    -- thing to the caller of this handle: it would resolve to slot 0,
    -- the undefined texture. #1690 stopped reporting that as a load.
    let resolved = mBindless ⌦ \bindless →
            (\existing → (bindless, existing))
              ⊚ Map.lookup (taTextureHandle atlas) (btsHandleMap bindless)
    case cachedAliasPublish (taPath atlas) (snd ⊚ resolved) of
        PublishFailed reason →
            publishTextureFailure env pool handle (taPath atlas) reason
        PublishRegistered _ → do
            forM_ resolved $ \(bindless, existingBindlessHandle) → do
                let rc = toRenderCapability env
                liftIO $ writeIORef (rcTextureSystemRef rc) (Just bindless
                    { btsHandleMap =
                        Map.insert handle existingBindlessHandle
                            (btsHandleMap bindless)
                    })
                -- Atlas-share path: sync the shader handle→slot table
                -- too (the ptr is shared across the immutable copy) (#286).
                liftIO $ writeHandleSlotEntry bindless (toInt handle)
                    (tsIndex (bthSlot existingBindlessHandle))

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
                atomicModifyIORef' (rcTextureSizeRef (toRenderCapability env)) $ \m →
                    (HM.insert handle (fromIntegral w, fromIntegral h) m, ())
                Q.writeQueue (luaQueue env)
                    (LuaAssetLoaded "texture" (fromIntegral rawHandle) (taPath atlas))

prepareTextureUpload ∷ AssetPool → Device → PhysicalDevice
                     → (TextureHandle, FilePath)
                     → EngineM σ TextureUploadPrep
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
        (IMAGE_USAGE_TRANSFER_DST_BIT ⌄ IMAGE_USAGE_SAMPLED_BIT)
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

-- | May a cached texture entry be reused for a request under this
--   upload policy?
--
--   Only when the canonical texture's pinned-ness matches what the
--   policy asks for. The path cache ('apAssetPaths') is keyed by path
--   alone, but a slot's sampler was fixed by whichever policy first
--   uploaded it, so reuse across that boundary hands the new handle the
--   wrong filtering in BOTH directions: an atlas inheriting an ordinary
--   slot follows global filter toggles and stops being nearest (#1259,
--   D-6), and an ordinary texture inheriting a pinned slot is stuck on
--   a filter it never asked for. @btsPinned@ is already the
--   authoritative record, so answering this stores nothing new.
cacheEntryReusable
    ∷ UploadSampler
    → Map.Map TextureHandle Sampler   -- ^ @btsPinned@
    → TextureHandle                   -- ^ the cached entry's CANONICAL handle
    → Bool
cacheEntryReusable policy pinned canonical =
    Map.member canonical pinned ≡ wantPinned
  where
    wantPinned = case policy of
        UploadGlobalSampler → False
        UploadPinnedNearest → True

handleLoadTextureBatch ∷ [(TextureHandle, FilePath)] → EngineM σ ()
handleLoadTextureBatch = handleLoadTextureBatchWith UploadGlobalSampler

-- | Upload compiled unit-animation atlases (#1259) — ONE image, one
--   handle, and one bindless slot per animation (D-2/D-10), pinned to
--   the nearest sampler. The image allocator already creates exactly
--   one mip level, so no mipmapped sampling of unit art is possible.
handleLoadAtlasTextureBatch ∷ [(TextureHandle, FilePath)] → EngineM σ ()
handleLoadAtlasTextureBatch = handleLoadTextureBatchWith UploadPinnedNearest

handleLoadTextureBatchWith
    ∷ UploadSampler → [(TextureHandle, FilePath)] → EngineM σ ()
handleLoadTextureBatchWith _ [] = pure ()
handleLoadTextureBatchWith samplerPolicy requests = do
    env ← ask
    poolRef ← asks (rcAssetPoolRef . toRenderCapability)
    pool ← liftIO $ readIORef poolRef
    mCacheBindless ← liftIO $ readIORef (rcTextureSystemRef (toRenderCapability env))

    -- The path cache is not policy-aware on its own: 'apAssetPaths' is
    -- keyed by path alone, while a slot's SAMPLER was fixed by whichever
    -- policy first uploaded it. Reusing across a policy boundary would
    -- silently give the new handle the wrong filtering — an atlas
    -- reusing a slot some ordinary load already created would follow
    -- global filter toggles and stop being nearest (#1259, D-6), and an
    -- ordinary texture reusing a pinned slot would be stuck on it. So a
    -- cache hit is only taken when the canonical texture's pinned-ness
    -- MATCHES what this batch asks for; otherwise the request falls
    -- through to a fresh upload with its own slot. 'btsPinned' is
    -- already the authoritative record of that, so nothing new is
    -- stored to answer it.
    let reusable atlas = case mCacheBindless of
            Nothing  → False
            Just bts → cacheEntryReusable samplerPolicy (btsPinned bts)
                           (taTextureHandle atlas)

    let (cachedReqs, freshReqs, aliasReqs, _) =
            foldl'
                (\(cached, fresh, aliases, seen) (handle, path) →
                    let key = T.pack path
                        asFresh = (cached, (handle, path) : fresh,
                                   aliases, Map.insert key handle seen)
                    in case Map.lookup key (apAssetPaths pool) of
                        Just assetId →
                            case Map.lookup assetId (apTextureAtlases pool) of
                                Just atlas
                                    | reusable atlas →
                                        ((handle, assetId, atlas) : cached,
                                         fresh, aliases, seen)
                                    -- A same-path entry under the other
                                    -- policy: re-upload rather than
                                    -- inherit its sampler. Within-batch
                                    -- aliasing below still dedupes,
                                    -- because one batch carries one
                                    -- policy.
                                    | otherwise → case Map.lookup key seen of
                                        Just canonical →
                                            (cached, fresh,
                                             (handle, path, canonical) : aliases, seen)
                                        Nothing → asFresh
                                Nothing → asFresh
                        Nothing →
                            case Map.lookup key seen of
                                Just canonical →
                                    (cached, fresh,
                                     (handle, path, canonical) : aliases, seen)
                                Nothing → asFresh
                )
                ([], [], [], Map.empty)
                requests

    forM_ (reverse cachedReqs) $ \(handle, assetId, atlas) →
        duplicateCachedTextureHandle env handle assetId atlas

    let invalidateRenderCaches = liftIO $ invalidateAllWorldRenderCaches env

    when (not (null freshReqs)) $ do
        gs ← gets graphicsState
        mBindless ← liftIO $ readIORef (rcTextureSystemRef (toRenderCapability env))
        case (vulkanDevice gs, vulkanPDevice gs, vulkanCmdPool gs, deviceQueues gs, mBindless) of
            (Just dev, Just pdev, Just cmdPool, Just queues, Just bindless0) → do
                preps ← mapM (prepareTextureUpload pool dev pdev) (reverse freshReqs)
                let queue = dqGraphicsQueue queues

                locally $ do
                    stagingBuffers ← forM preps $ \prep → do
                        let bufSize = fromIntegral (tupPixelLen prep)
                        (stagingMem, stagingBuf) ← createVulkanBuffer dev pdev bufSize
                            BUFFER_USAGE_TRANSFER_SRC_BIT
                            (MEMORY_PROPERTY_HOST_VISIBLE_BIT
                             ⌄ MEMORY_PROPERTY_HOST_COHERENT_BIT)
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

                -- One entry per prep, in batch order: its path, its
                -- would-be asset id, and the TERMINAL outcome its
                -- registration reached. Failures stay in this list on
                -- purpose — 'publishRegisteredEntries' is what proves
                -- they contribute nothing to the path cache below.
                (results, bindlessN) ← foldM
                    (\(acc, bindless) prep → do
                        let VulkanImage image imageMemory = tupImage prep
                            pathText = T.pack (tupPath prep)
                        (imageView, cleanView) ← createVulkanImageView' dev (tupImage prep)
                            FORMAT_R8G8B8A8_UNORM IMAGE_ASPECT_COLOR_BIT
                        (mbHandle, bindless', releasePinnedSampler) ← case samplerPolicy of
                            UploadGlobalSampler → do
                                -- 'btsTextureSampler' belongs to the
                                -- bindless system and outlives every
                                -- request, so this path acquires no
                                -- reference of its own to hand back.
                                (mbH, bl) ← registerTexture dev (tupHandle prep)
                                    imageView (btsTextureSampler bindless) bindless
                                pure (mbH, bl, pure ())
                            UploadPinnedNearest → do
                                -- Acquired from the shared refcounted
                                -- cache. A slot that REGISTERS keeps its
                                -- reference for the whole session,
                                -- exactly like the unit textures it
                                -- replaces; only a registration that
                                -- FAILED hands it back (#1690), because
                                -- nothing will ever sample through it.
                                let cacheRef = rcSamplerCacheRef (toRenderCapability env)
                                nearest ← liftIO $ acquireSampler dev cacheRef
                                    SamplerTextureNearest
                                (mbH, bl) ← registerPinnedTexture dev (tupHandle prep)
                                    imageView nearest bindless
                                pure ( mbH, bl
                                     , releaseSampler dev cacheRef SamplerTextureNearest )
                        let outcome = classifyRegistration pathText mbHandle
                        case publishFailureReason outcome of
                            -- #1690: the bindless allocator is out of
                            -- slots. Nothing sampling this handle could
                            -- ever see this image, so the request ends
                            -- HERE — the GPU objects go back in
                            -- 'failedUploadCleanup' order and the handle
                            -- settles on 'AssetFailed'.
                            Just reason → do
                                forM_ (failedUploadCleanup samplerPolicy) $ \case
                                    CleanupImageView     → liftIO cleanView
                                    CleanupImage         → liftIO (tupCleanImage prep)
                                    ReleasePinnedSampler → liftIO releasePinnedSampler
                                publishTextureFailure env pool (tupHandle prep)
                                    pathText reason
                                pure ( (tupHandle prep, pathText, tupAssetId prep
                                       , outcome, Left reason) : acc
                                     , bindless' )
                            Nothing → do
                                let atlas = TextureAtlas
                                        { taId = tupAssetId prep
                                        , taName = T.pack (takeBaseName (tupPath prep))
                                        , taPath = pathText
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
                                        , taBindlessSlot = publishedSlot outcome
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
                                            }
                                        , ()
                                        )
                                    atomicModifyIORef' (rcTextureSizeRef (toRenderCapability env)) $ \m →
                                        ( HM.insert (tupHandle prep)
                                            (fromIntegral (tupWidth prep), fromIntegral (tupHeight prep)) m
                                        , ()
                                        )
                                    Q.writeQueue (luaQueue env)
                                        (LuaAssetLoaded "texture" (fromIntegral rawHandle)
                                            pathText)
                                pure ( (tupHandle prep, pathText, tupAssetId prep
                                       , outcome, Right (tupAssetId prep, atlas)) : acc
                                     , bindless' )
                        )
                    ([], bindless0)
                    preps

                liftIO $ writeIORef (rcTextureSystemRef (toRenderCapability env)) (Just bindlessN)

                -- The path cache is the poisoning surface #1690 closes,
                -- so every request's outcome goes through the one funnel
                -- that decides it. A failed request neither inserts nor
                -- overwrites — and, just as load-bearing, does not
                -- DELETE: a same-path entry left by the OPPOSITE sampler
                -- policy is valid, is exactly why this request was a
                -- fresh upload rather than a cache hit, and must survive
                -- so that policy keeps working and this one can retry.
                let batchResults = reverse results
                liftIO $ atomicModifyIORef' poolRef $ \p →
                    ( p { apAssetPaths = publishRegisteredEntries
                            [ (path, assetId, outcome)
                            | (_, path, assetId, outcome, _) ← batchResults
                            ]
                            (apAssetPaths p)
                        }
                    , ()
                    )

                let canonicalResults = Map.fromList
                        [ (handle, result)
                        | (handle, _, _, _, result) ← batchResults
                        ]
                forM_ (reverse aliasReqs) $ \(handle, path, canonical) →
                    case aliasPublish (T.pack path)
                             (Map.lookup canonical canonicalResults) of
                        Right (assetId, atlas) →
                            duplicateCachedTextureHandle env handle assetId atlas
                        -- An alias is a request like any other: when the
                        -- canonical upload it was folded into failed, it
                        -- inherits that failure instead of being left in
                        -- 'AssetLoading' with only a log line (#1690).
                        Left reason →
                            publishTextureFailure env pool handle
                                (T.pack path) reason
                invalidateRenderCaches

            -- No device, command pool, queue or bindless system at
            -- all: nothing in this batch was uploaded or even
            -- attempted. Deliberately unchanged by #1690, which is
            -- about the outcome of a registration that RAN and found no
            -- slot. This branch is also the normal, expected state of
            -- --headless (no GPU), where announcing a terminal failure
            -- per request would invent a failure protocol for a mode
            -- that never renders.
            _ → logWarnM CatTexture "Cannot batch-load textures: Vulkan not ready"

handleLoadTexture ∷ TextureHandle → FilePath → EngineM σ ()
handleLoadTexture handle path = do
    logDebugM CatLua $ "Loading texture from Lua: " <> T.pack path
                    <> " (handle: " <> tshow handle <> ")"
    -- No "loaded successfully" line here: this returns once the batch
    -- has been PROCESSED, which since #1690 can equally mean the request
    -- terminally failed. 'publishTextureFailure' and the LuaAssetLoaded
    -- queue message are the outcome-bearing reports.
    handleLoadTextureBatch [(handle, path)]
    logDebugM CatLua $ "Texture load request processed: " <> T.pack path

handleLoadFont ∷ FontHandle → FilePath → Int → EngineM σ ()
handleLoadFont handle path _size = do
    logDebugM CatLua $ "Loading font from Lua: " <> T.pack path
    actualHandle ← loadSDFFont handle path
    env ← ask
    let etlq = luaQueue env
    liftIO $ Q.writeQueue etlq (LuaFontLoaded actualHandle path)
    let (FontHandle h) = actualHandle
    liftIO $ Q.writeQueue etlq (LuaAssetLoaded "font" (fromIntegral h) (T.pack path))
    logDebugM CatLua $ "Font loaded successfully: " <> T.pack path
