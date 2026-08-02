-- | What is left of the asset manager after #1007.
--
--   This module is now three things, and nothing else:
--
--   * handle and asset-id allocation ('generateTextureHandle',
--     'generateFontHandle', 'generateAssetId') — the counters every
--     texture\/font upload path draws its ids from;
--   * texture\/font handle-state setters ('updateTextureState',
--     'updateFontState') — the only writers of 'apTextureHandles' and
--     'apFontHandles';
--   * a teardown path ('unloadAsset', 'cleanupAssetManager', and its
--     helper @cleanupResources@) that nothing currently calls.
--
--   The teardown path is kept deliberately. The live texture upload
--   path ('Engine.Scripting.Lua.Message.Texture') registers atlases
--   through 'Engine.Graphics.Vulkan.Texture.Bindless.registerTexture'
--   directly and stores a cleanup closure on every atlas it loads
--   ('taCleanup', freeing that atlas's image view, image, and device
--   memory — an atlas owns no sampler, it shares the bindless system's
--   one). The functions below are the ONLY code that ever runs those
--   closures, so they are the only path capable of releasing a loaded
--   texture's GPU resources. Whether to wire them up is a separate
--   GPU-resource-lifetime question.
module Engine.Asset.Manager
  ( generateTextureHandle
  , generateFontHandle
  , generateAssetId
  , updateTextureState
  , updateFontState
  , unloadAsset
  , cleanupAssetManager
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (readIORef, atomicModifyIORef', writeIORef)
import Engine.Core.Monad
import Engine.Core.State (EngineState(..), GraphicsState(..))
import Engine.Core.Capability.RenderView
  (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Error.Exception (ExceptionType(..), GraphicsError(..)
                                   , AssetError(..))
import Engine.Core.Log.Monad (logDebugM, logInfoM, logAndThrowM, logDebugSM)
import Engine.Core.Log (LogCategory(..))
import Engine.Asset.Base
import Engine.Asset.Types
import Engine.Asset.Handle
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Texture.Bindless (unregisterTexture)
import qualified Vulkan.Core10 as Vk

generateTextureHandle ∷ AssetPool → IO TextureHandle
generateTextureHandle pool =
  atomicModifyIORef' (apNextTextureHandle pool) $ \n →
    (n + 1, TextureHandle n)

generateFontHandle ∷ AssetPool → IO FontHandle
generateFontHandle pool =
  atomicModifyIORef' (apNextFontHandle pool) $ \n →
    (n + 1, FontHandle n)

generateAssetId ∷ AssetPool → IO AssetId
generateAssetId pool =
  atomicModifyIORef' (apNextAssetId pool) $ \n →
    (n + 1, AssetId $ fromIntegral n)

updateTextureState ∷ TextureHandle → AssetState AssetId → AssetPool → IO ()
updateTextureState handle newState pool =
  atomicModifyIORef' (apTextureHandles pool) $ \m →
    (Map.insert handle newState m, ())

updateFontState ∷ FontHandle → AssetState AssetId → AssetPool → IO ()
updateFontState handle newState pool =
  atomicModifyIORef' (apFontHandles pool) $ \m →
    (Map.insert handle newState m, ())

-- | Decrement an asset's ref count; if it reaches zero, run its cleanup
--   action and remove it from the pool
unloadAsset ∷ AssetId → EngineM' ()
unloadAsset aid = do
  poolRef ← asks (rvAssetPoolRef . toRenderViewCapability)
  pool ← liftIO $ readIORef poolRef

  case Map.lookup aid (apTextureAtlases pool) of
    Just atlas → do
      let refCount = taRefCount atlas - 1
      if refCount ≤ 0 then do
          -- Free GPU resources safely, mirroring 'disposeTransientTexture':
          -- idle the device (the atlas may still be sampled by an in-flight
          -- frame), unregister the bindless slot (repoints it at the
          -- undefined texture AND frees the slot for reuse), THEN destroy
          -- the image/view/memory — so no descriptor still references the
          -- imageView when it is destroyed. Skipping either step risks a
          -- use-after-free / a dangling descriptor + leaked slot.
          env ← ask
          gs  ← gets graphicsState
          forM_ (vulkanDevice gs) $ \dev → do
            liftIO $ Vk.deviceWaitIdle dev
            mSys ← liftIO $ readIORef (rvTextureSystemRef (toRenderViewCapability env))
            forM_ mSys $ \sys → do
              sys' ← unregisterTexture dev (taTextureHandle atlas) sys
              liftIO $ writeIORef (rvTextureSystemRef (toRenderViewCapability env)) (Just sys')
          liftIO $ maybe (pure ()) id (taCleanup atlas)
          liftIO $ atomicModifyIORef' poolRef $ \p → (p
            { apTextureAtlases = Map.delete aid (apTextureAtlases p)
            , apAssetPaths = Map.filter (≢ aid) (apAssetPaths p)
            }, ())
      else
          liftIO $ atomicModifyIORef' poolRef $ \p → (p
            { apTextureAtlases = Map.adjust (\a → a { taRefCount = refCount }) aid (apTextureAtlases p)
            }, ())
      pure ()

    Nothing → logAndThrowM CatAsset (ExAsset (AssetNotFound aid))
                "Attempted to unload non-existent asset"

-- | Drain all assets: wait for the device to idle, run every cleanup action,
--   and reset the pool. Throws if cleanup is already in progress.
cleanupAssetManager ∷ EngineM' ()
cleanupAssetManager = do
    logInfoM CatAsset "Asset cleanup phase started"
    state ← gets graphicsState
    poolRef ← asks (rvAssetPoolRef . toRenderViewCapability)
    _pool ← liftIO $ readIORef poolRef

    when (cleanupStatus state ≡ InProgress) $
      logAndThrowM CatAsset (ExGraphics CleanupError) $ "Cleanup already in progress"

    modify $ \s → s { graphicsState = (graphicsState s) { cleanupStatus = InProgress } }

    device ← case vulkanDevice state of
        Nothing → logAndThrowM CatAsset (ExGraphics VulkanDeviceLost) "No device during cleanup"
        Just d → pure d
    queues ← case deviceQueues state of
        Nothing → logAndThrowM CatAsset (ExGraphics CleanupError) "No device queues during cleanup"
        Just q → pure q

    logDebugM CatAsset "Waiting for device to be idle..."
    liftIO $ do
        Vk.queueWaitIdle (graphicsQueue queues)
        Vk.queueWaitIdle (presentQueue queues)
        Vk.deviceWaitIdle device

    cleanupResources device state
    modify $ \s → s { graphicsState = (graphicsState s) { cleanupStatus = Completed } }
    logInfoM CatAsset "Asset cleanup completed successfully"

cleanupResources ∷ Vk.Device → GraphicsState → EngineM' ()
cleanupResources device _state = do
    poolRef ← asks (rvAssetPoolRef . toRenderViewCapability)
    pool ← liftIO $ readIORef poolRef
    -- The device is already fully idle here ('cleanupAssetManager' waits
    -- on both queues + the device before calling us), and 'taCleanup'
    -- only destroys image/view/memory — no GPU submission — so the device
    -- stays idle through the loop. No per-texture idle needed (that was N
    -- full CPU↔GPU stalls for N atlases). One trailing barrier below.
    forM_ (Map.elems $ apTextureAtlases pool) $ \atlas → do
        logDebugSM CatAsset "Cleaning up texture"
          [("name", taName atlas)
          ,("path", taPath atlas)
          ,("asset_id", T.pack $ show $ taId atlas)]
        liftIO $ maybe (pure ()) id (taCleanup atlas)

    liftIO $ writeIORef (apNextAssetId pool) 0
    liftIO $ atomicModifyIORef' poolRef $ \poolRef' →
        let clearedPool = poolRef'
              { apTextureAtlases = Map.empty
              , apAssetPaths = Map.empty
              }
        in (clearedPool, ())
    modify $ \s → s
        { graphicsState = (graphicsState s)
            { cleanupStatus = Completed
            }
        }

    (liftIO $ Vk.deviceWaitIdle device)
    logDebugM CatAsset "Asset manager cleanup complete"
