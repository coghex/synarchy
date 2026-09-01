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
--     helper @cleanupResources@).
--
--   The live texture upload path
--   ('Engine.Scripting.Lua.Message.Texture') registers atlases through
--   'Engine.Graphics.Vulkan.Texture.Bindless.registerTexture'
--   directly so gameplay unit art follows the global sampler (#2085),
--   and stores a
--   cleanup closure on every atlas it loads ('taCleanup', freeing that
--   atlas's image view, image, and device memory; no sampler, since a
--   slot's sampler always comes from the shared refcounted cache rather
--   than being owned by the atlas). The functions below are the ONLY
--   code that ever runs those closures, so they are the only path
--   capable of releasing a loaded texture's GPU resources.
--
--   'cleanupAssetManager' is the shutdown drain, and since #1691
--   'Engine.Loop.Shutdown.shutdownEngine' calls it — after its
--   device-idle wait and before the generic Vulkan cleanup sweep, in a
--   boot mode that has a device and its queues. Until then nothing
--   called it, so every atlas loaded from disk was still holding a
--   live @VkImage@ \/ @VkImageView@ \/ @VkDeviceMemory@ when the
--   logical device was destroyed.
--
--   'unloadAsset' is the mid-session single-atlas release and still has
--   no caller. #1281 cleared its alias-safety blocker, but WHO would
--   release a texture while the session runs — a refcount, a Lua verb,
--   or nobody — is a separate policy question (#1691 put it out of
--   scope explicitly), so it stays retained and uncalled.
module Engine.Asset.Manager
  ( generateTextureHandle
  , TextureHandleReservation(..)
  , reserveTextureHandle
  , generateFontHandle
  , generateAssetId
  , updateTextureState
  , updateFontState
  , unloadAsset
  , cleanupAssetManager
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import Data.IORef (IORef, readIORef, atomicModifyIORef', writeIORef)
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
import Engine.Graphics.Vulkan.Texture.Bindless
  (HandleAddressing(..), checkRegistrableHandle, releaseTextureHandles)
import Engine.Graphics.Vulkan.Texture.Release
  (TextureReleasePlan(..), releaseOwnerHandles)
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem)
import qualified Vulkan.Core10 as Vk

-- | Allocate the next texture handle: dense and monotonic, and never
--   'missingTextureHandle' — the counter is seeded at
--   'firstAllocatableTextureHandle', so a fresh pool's first result is
--   @TextureHandle 1@ (#1696).
generateTextureHandle ∷ AssetPool → IO TextureHandle
generateTextureHandle pool =
  atomicModifyIORef' (apNextTextureHandle pool) $ \n →
    (n + 1, TextureHandle n)

-- | The outcome of reserving a SHADER-ADDRESSABLE texture handle
--   (#1699).
data TextureHandleReservation
  = TextureHandleAllocated !TextureHandle
    -- ^ This id was allocated and the shader's handle→slot table can
    --   resolve it. Nobody else can be handed it.
  | TextureHandlesSpent !(Maybe TextureHandle)
    -- ^ The namespace has run past the table and never recovers, so
    --   NOTHING was allocated and the counter did not move. 'Just' the
    --   id that would have been handed out, for the ONE caller that
    --   claims the single report; 'Nothing' for every caller after it.
  deriving (Show, Eq)

-- | Allocate the next texture handle for a caller that needs the SHADER
--   to resolve it, or report that the namespace is spent.
--
--   The decision and the allocation are ONE 'atomicModifyIORef'' on the
--   counter, which is what makes them trustworthy: 'generateTextureHandle'
--   is a shared allocator that the Lua worker, the transients and this
--   caller all draw from concurrently, so a separate "is there room?"
--   read could be overtaken between answering and allocating — handing
--   back an id past the cap that the caller had already decided was
--   safe.
--
--   The boundary is 'checkRegistrableHandle', the same guard every
--   bindless registration runs, so there is one definition of it rather
--   than a second copy that could drift. A spent namespace advances
--   nothing: there is no point burning ids nobody can use.
--
--   The report claim is deliberately separate and is NOT part of that
--   atomic step. It only decides who prints a line about a condition
--   that is already permanent, and it is a monotone one-way flag, so
--   exactly one caller wins it however the two interleave.
--
--   Only a PER-FRAME caller needs this at all —
--   'World.Render.BloodQuads', whose diff would otherwise re-upload,
--   re-refuse and re-log the same decal every frame for the rest of the
--   session. A one-shot request keeps using 'generateTextureHandle' and
--   lets the registration boundary refuse it terminally; asking first
--   would only duplicate that. Fonts must keep using it too: they take
--   a handle purely as an atlas identity and bind through their own
--   descriptor set, so a spent table is not their failure.
reserveTextureHandle ∷ AssetPool → IO TextureHandleReservation
reserveTextureHandle pool = do
  reserved ← atomicModifyIORef' (apNextTextureHandle pool) $ \n →
    let handle = TextureHandle n
    in case checkRegistrableHandle ShaderAddressable handle of
         Right () → (n + 1, Right handle)
         Left _   → (n, Left handle)
  case reserved of
    Right handle → pure (TextureHandleAllocated handle)
    Left handle  → do
      alreadyReported ← atomicModifyIORef' (apHandlesSpentReported pool)
                            (\r → (True, r))
      pure (TextureHandlesSpent
              (if alreadyReported then Nothing else Just handle))

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

-- | Purge every handle a release invalidated from the pool-side
--   bookkeeping: its 'AssetState' entry in @apTextureHandles@ (which is
--   where @AssetReady@ lives — there is no separate ready map) and its
--   dimensions in the texture size map. Both are keyed by the stable
--   handle, so an alias has its own entry in each and neither is
--   reachable from the atlas alone (#1281).
purgeReleasedHandles ∷ TextureReleasePlan
                     → IORef (Map.Map TextureHandle (AssetState AssetId))
                     → IORef (HM.HashMap TextureHandle (Int, Int))
                     → IO ()
purgeReleasedHandles plan handleStatesRef sizeRef = do
  atomicModifyIORef' handleStatesRef $ \m →
    (foldl' (flip Map.delete) m (trpInvalidated plan), ())
  atomicModifyIORef' sizeRef $ \m →
    (foldl' (flip HM.delete) m (trpInvalidated plan), ())

-- | Read the live bindless system, or fail loudly. A release cannot
--   proceed without it: destroying an atlas image while handles may
--   still resolve to its slot is the stale-alias defect (#1281), and a
--   silent skip is what made that possible.
requireTextureSystem ∷ IORef (Maybe BindlessTextureSystem) → Text
                     → EngineM σ BindlessTextureSystem
requireTextureSystem sysRef whatFor =
  liftIO (readIORef sysRef) ⌦ \case
    Just sys → pure sys
    Nothing  → logAndThrowM CatAsset (ExGraphics CleanupError) $
      "Refusing to destroy " <> whatFor <> ": no bindless texture system, "
        <> "so its stable texture handles cannot be invalidated first"

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
          -- Free GPU resources safely, mirroring @disposeTransientTexture@:
          -- idle the device (the atlas may still be sampled by an in-flight
          -- frame), invalidate the bindless slot (repoints it at the
          -- undefined texture AND frees the slot for reuse), THEN destroy
          -- the image/view/memory — so no descriptor still references the
          -- imageView when it is destroyed. Skipping either step risks a
          -- use-after-free / a dangling descriptor + leaked slot.
          --
          -- Both steps are all-or-nothing (#1281). The device and the
          -- bindless system used to be optional 'forM_' branches that
          -- silently skipped invalidation and destroyed the image anyway,
          -- leaving live handles resolving to a slot whose image was
          -- gone. Now a missing one aborts the release with the atlas and
          -- every mapping still intact, so it can be retried.
          env ← ask
          gs  ← gets graphicsState
          let rv = toRenderViewCapability env
              atlasName = "texture atlas " <> taName atlas
                            <> " (" <> taPath atlas <> ")"
          dev ← case vulkanDevice gs of
            Just d  → pure d
            Nothing → logAndThrowM CatAsset (ExGraphics VulkanDeviceLost) $
              "Refusing to destroy " <> atlasName <> ": no Vulkan device, "
                <> "so its stable texture handles cannot be invalidated first"
          sys ← requireTextureSystem (rvTextureSystemRef rv) atlasName
          liftIO $ Vk.deviceWaitIdle dev
          -- Invalidates the canonical handle AND every cached-atlas alias
          -- of it: those sharing its slot, plus any the pool records
          -- against this asset id but that never got a bindless mapping
          -- ('releaseOwnerHandles'). The shader table is zeroed for all
          -- of them inside this call, before 'sys'' (whose allocator can
          -- hand the slot out again) is published below.
          handleStates ← liftIO $ readIORef (apTextureHandles pool)
          let owners = releaseOwnerHandles (Set.singleton aid)
                         [taTextureHandle atlas] handleStates
          (plan, sys') ← releaseTextureHandles dev owners sys
          liftIO $ do
            purgeReleasedHandles plan (apTextureHandles pool) (rvTextureSizeRef rv)
            atomicModifyIORef' poolRef $ \p → (p
              { apTextureAtlases = Map.delete aid (apTextureAtlases p)
              , apAssetPaths = Map.filter (≢ aid) (apAssetPaths p)
              }, ())
            writeIORef (rvTextureSystemRef rv) (Just sys')
            maybe (pure ()) id (taCleanup atlas)
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
    
    modifyGraphicsState $ \gs → gs { cleanupStatus = InProgress }
    
    device ← case vulkanDevice state of
        Nothing → logAndThrowM CatAsset (ExGraphics VulkanDeviceLost) "No device during cleanup"
        Just d → pure d
    queues ← case deviceQueues state of
        Nothing → logAndThrowM CatAsset (ExGraphics CleanupError) "No device queues during cleanup"
        Just q → pure q

    logDebugM CatAsset "Waiting for device to be idle..."
    liftIO $ do
        Vk.queueWaitIdle (dqGraphicsQueue queues)
        Vk.queueWaitIdle (dqPresentQueue queues)
        Vk.deviceWaitIdle device

    cleanupResources device state
    modifyGraphicsState $ \gs → gs { cleanupStatus = Completed }
    logInfoM CatAsset "Asset cleanup completed successfully"

cleanupResources ∷ Vk.Device → GraphicsState → EngineM' ()
cleanupResources device _state = do
    env ← ask
    let rv = toRenderViewCapability env
    poolRef ← asks (rvAssetPoolRef . toRenderViewCapability)
    pool ← liftIO $ readIORef poolRef
    let atlases = Map.elems (apTextureAtlases pool)
    -- Same invariant the single-atlas path holds (#1281), applied to the
    -- whole drain: every stable handle naming a slot we are about to
    -- release — canonical owner and cached-atlas aliases alike — is
    -- invalidated, and every slot is handed back exactly once, BEFORE any
    -- image is destroyed. Draining used to skip this entirely.
    unless (null atlases) $ do
      sys ← requireTextureSystem (rvTextureSystemRef rv) "the loaded texture atlases"
      handleStates ← liftIO $ readIORef (apTextureHandles pool)
      let owners = releaseOwnerHandles (Map.keysSet (apTextureAtlases pool))
                     (map taTextureHandle atlases) handleStates
      (plan, sys') ← releaseTextureHandles device owners sys
      liftIO $ do
        purgeReleasedHandles plan (apTextureHandles pool) (rvTextureSizeRef rv)
        writeIORef (rvTextureSystemRef rv) (Just sys')
    -- The device is already fully idle here ('cleanupAssetManager' waits
    -- on both queues + the device before calling us), and 'taCleanup'
    -- only destroys image/view/memory — no GPU submission — so the device
    -- stays idle through the loop. No per-texture idle needed (that was N
    -- full CPU↔GPU stalls for N atlases). One trailing barrier below.
    forM_ atlases $ \atlas → do
        logDebugSM CatAsset "Cleaning up texture"
          [("name", taName atlas)
          ,("path", taPath atlas)
          ,("asset_id", tshow $ taId atlas)]
        liftIO $ maybe (pure ()) id (taCleanup atlas)

    liftIO $ writeIORef (apNextAssetId pool) 0
    liftIO $ atomicModifyIORef' poolRef $ \poolRef' →
        let clearedPool = poolRef'
              { apTextureAtlases = Map.empty
              , apAssetPaths = Map.empty
              }
        in (clearedPool, ())
    modifyGraphicsState $ \gs → gs
        { cleanupStatus = Completed
        }

    (liftIO $ Vk.deviceWaitIdle device)
    logDebugM CatAsset "Asset manager cleanup complete"
