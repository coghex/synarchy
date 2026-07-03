module Engine.Asset.Manager
  ( generateTextureHandle
  , generateFontHandle
  , generateShaderHandle
  , generateAssetId
  , updateTextureState
  , updateFontState
  , updateShaderState
  , deleteTextureState
  , deleteFontState
  , deleteShaderState
  , getAllTextureHandles
  , getAllFontHandles
  , getAllShaderHandles
  , lookupTextureAsset
  , lookupFontAsset
  , lookupShaderAsset
  , loadTextureAtlas
  , loadTextureAtlasWithHandle
  , unloadAsset
  , getTextureAtlas
  , getShaderProgram
  , cleanupAssetManager
  , getTextureStateMap
  , getFontStateMap
  , getShaderStateMap
  , getTextureHandleState
  , getFontHandleState
  , getShaderHandleState
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (readIORef, atomicModifyIORef', writeIORef)
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Error.Exception (ExceptionType(..), GraphicsError(..)
                                   , AssetError(..))
import Engine.Core.Log.Monad (logDebugM, logWarnM, logInfoM, logAndThrowM
                             , logDebugSM)
import Engine.Core.Log (LogCategory(..))
import Engine.Asset.Base
import Engine.Asset.Types
import Engine.Asset.Handle
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Base
import Engine.Graphics.Vulkan.Texture
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.Texture.Bindless (registerTexture, unregisterTexture, writeHandleSlotEntry)
import Engine.Graphics.Vulkan.Texture.Slot (TextureSlot(..))
import Engine.Graphics.Vulkan.Texture.Handle (BindlessTextureHandle(..))
import qualified Vulkan.Core10 as Vk

generateTextureHandle ∷ AssetPool → IO TextureHandle
generateTextureHandle pool =
  atomicModifyIORef' (apNextTextureHandle pool) $ \n →
    (n + 1, TextureHandle n)

generateFontHandle ∷ AssetPool → IO FontHandle
generateFontHandle pool =
  atomicModifyIORef' (apNextFontHandle pool) $ \n →
    (n + 1, FontHandle n)

generateShaderHandle ∷ AssetPool → IO ShaderHandle
generateShaderHandle pool =
  atomicModifyIORef' (apNextShaderHandle pool) $ \n →
    (n + 1, ShaderHandle n)

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

updateShaderState ∷ ShaderHandle → AssetState AssetId → AssetPool → IO ()
updateShaderState handle newState pool =
  atomicModifyIORef' (apShaderHandles pool) $ \m →
    (Map.insert handle newState m, ())

deleteTextureState ∷ TextureHandle → AssetPool → IO ()
deleteTextureState handle pool =
  atomicModifyIORef' (apTextureHandles pool) $ \m →
    (Map.delete handle m, ())

deleteFontState ∷ FontHandle → AssetPool → IO ()
deleteFontState handle pool =
  atomicModifyIORef' (apFontHandles pool) $ \m →
    (Map.delete handle m, ())

deleteShaderState ∷ ShaderHandle → AssetPool → IO ()
deleteShaderState handle pool =
  atomicModifyIORef' (apShaderHandles pool) $ \m →
    (Map.delete handle m, ())

getAllTextureHandles ∷ AssetPool → IO [TextureHandle]
getAllTextureHandles pool = do
  stateMap ← readIORef (apTextureHandles pool)
  return $ Map.keys stateMap

getAllFontHandles ∷ AssetPool → IO [FontHandle]
getAllFontHandles pool = do
  stateMap ← readIORef (apFontHandles pool)
  return $ Map.keys stateMap

getAllShaderHandles ∷ AssetPool → IO [ShaderHandle]
getAllShaderHandles pool = do
  stateMap ← readIORef (apShaderHandles pool)
  return $ Map.keys stateMap

lookupTextureAsset ∷ TextureHandle → AssetPool → IO (Maybe (AssetState AssetId))
lookupTextureAsset handle pool = do
  stateMap ← readIORef (apTextureHandles pool)
  return $ Map.lookup handle stateMap

lookupFontAsset ∷ FontHandle → AssetPool → IO (Maybe (AssetState AssetId))
lookupFontAsset handle pool = do
  stateMap ← readIORef (apFontHandles pool)
  return $ Map.lookup handle stateMap

lookupShaderAsset ∷ ShaderHandle → AssetPool → IO (Maybe (AssetState AssetId))
lookupShaderAsset handle pool = do
  stateMap ← readIORef (apShaderHandles pool)
  return $ Map.lookup handle stateMap

-- | Convenience wrapper that auto-generates a 'TextureHandle' before loading
loadTextureAtlas ∷ Text → FilePath → Text → EngineM ε σ AssetId
loadTextureAtlas name path arrayName = do
  poolRef ← asks assetPoolRef
  pool ← liftIO $ readIORef poolRef
  texHandle ← liftIO $ generateTextureHandle pool
  loadTextureAtlasWithHandle texHandle name path arrayName

-- | Load a texture atlas from disk, register it in the bindless system, and
--   insert it into the global asset pool. If the path is already loaded, the
--   existing slot is reused and the ref count is bumped.
loadTextureAtlasWithHandle ∷ TextureHandle  -- ^ Pre-generated handle
                          → Text            -- ^ Name of the atlas
                          → FilePath        -- ^ Path to the atlas file
                          → Text            -- ^ Array name
                          → EngineM ε σ AssetId
loadTextureAtlasWithHandle texHandle name path _arrayName = do
  logDebugSM CatAsset "Asset loading started"
    [("path", T.pack path)
    ,("handle", T.pack $ show texHandle)
    ,("name", name)]
  
  -- Read from the global IORef (the ONLY source of truth)
  poolRef ← asks assetPoolRef
  pool ← liftIO $ readIORef poolRef
  
  let pathKey = T.pack path
  
  logDebugM CatAsset $ "Current atlas count: " <> T.pack (show $ Map.size $ apTextureAtlases pool)
  
  -- check if the texture is already loaded
  case Map.lookup pathKey (apAssetPaths pool) of
    Just existingId → do
      logDebugSM CatAsset "Texture found in cache"
        [("asset_id", T.pack $ show existingId)
        ,("ref_count", T.pack $ show $ taRefCount $ (apTextureAtlases pool) Map.! existingId)]
      
      -- Get the existing atlas to find its bindless slot
      let existingAtlas = (apTextureAtlases pool) Map.! existingId
      
      -- Register the NEW handle in the bindless system, pointing it at
      -- the same slot as the cached atlas. textureSystemRef is the
      -- single source of truth (no graphicsState mirror).
      env ← ask
      mBindless ← liftIO $ readIORef (textureSystemRef env)
      case mBindless of
        Just bindless →
          case Map.lookup (taTextureHandle existingAtlas) (btsHandleMap bindless) of
            Just existingBindlessHandle → do
              let newBindless = bindless
                    { btsHandleMap = Map.insert texHandle existingBindlessHandle (btsHandleMap bindless) }
              liftIO $ writeIORef (textureSystemRef env) (Just newBindless)
              -- Atlas-share path: the new handle reuses an existing slot
              -- without going through registerTexture, so sync the shader
              -- handle→slot table here too (#286).
              liftIO $ writeHandleSlotEntry newBindless (toInt texHandle)
                          (tsIndex (bthSlot existingBindlessHandle))
              logDebugM CatAsset $ "New handle " <> T.pack (show texHandle)
                          <> " successfully mapped to existing bindless slot"
            Nothing → logWarnM CatAsset $ "Failed to duplicate handle " <> T.pack (show texHandle)
        Nothing → logWarnM CatAsset "No bindless system available, skipping handle mapping"
      
      logDebugM CatAsset $ "Duplicate handle " <> T.pack (show texHandle) 
                        <> " mapped to existing slot"
      
      -- Increment ref count in the IORef
      liftIO $ atomicModifyIORef' poolRef $ \p → (p
        { apTextureAtlases = Map.adjust (\a → a { taRefCount = taRefCount a + 1 })
                                        existingId (apTextureAtlases p) }, ())
      
      return existingId
    
    Nothing → do
      -- Generate unique asset ID atomically
      nextId ← liftIO $ generateAssetId pool
      
      logDebugSM CatAsset "Creating new texture asset"
        [("asset_id", T.pack $ show nextId)
        ,("path", pathKey)
        ,("handle", T.pack $ show texHandle)]
      
      state ← get
      pDevice ← case (vulkanPDevice $ graphicsState state) of
        Nothing → logAndThrowM CatAsset (ExGraphics VulkanDeviceLost)
                    "No physical device found"
        Just pdev → pure pdev
      device ← case (vulkanDevice $ graphicsState state) of
        Nothing → logAndThrowM CatAsset (ExGraphics VulkanDeviceLost)
                    "No logical device found"
        Just dev → pure dev
      cmdPool ← case (vulkanCmdPool $ graphicsState state) of
        Nothing → logAndThrowM CatAsset (ExGraphics VulkanDeviceLost)
                    "No command pool found"
        Just cmdP → pure cmdP
      cmdQueue ← case (deviceQueues (graphicsState state)) of
        Nothing → logAndThrowM CatAsset (ExGraphics VulkanDeviceLost)
                    "No device queues found"
        Just queues → pure $ graphicsQueue queues
          
      -- Create the texture image, view and sampler
      ((_vulkanImage@(VulkanImage image imageMemory), imageView), imageCleanup) ←
        createTextureImageView' pDevice device cmdPool cmdQueue path
        
      env ← ask
      -- Register with bindless system using the provided handle
      -- textureSystemRef is the single source of truth. The atlas does
      -- not own a sampler — it shares the bindless system's single
      -- texture sampler (governed by the global filter).
      mBindless ← liftIO $ readIORef (textureSystemRef env)
      bindlessSlot ← case mBindless of
        Just bindless → do
          logDebugM CatAsset "Registering texture with bindless system"
          (mbHandle, newBindless) ←
            registerTexture device texHandle imageView (btsTextureSampler bindless) bindless
          liftIO $ writeIORef (textureSystemRef env) (Just newBindless)
          case mbHandle of
            Just bHandle → do
              let slot = tsIndex $ bthSlot bHandle
              logDebugSM CatAsset "Bindless texture slot assigned"
                [("slot", T.pack $ show slot)]
              let mapSize = Map.size $ btsHandleMap newBindless
              logDebugM CatAsset $ "Bindless handleMap now has " <> T.pack (show mapSize) <> " entries"
              pure $ Just slot
            Nothing → do
              logWarnM CatAsset $
                "Failed to register texture in bindless system: " <> name
              pure Nothing
        _ → do
          logWarnM CatAsset "No bindless system available, using legacy path"
          pure Nothing
    
      let atlas = TextureAtlas
            { taId = nextId
            , taName = name
            , taPath = T.pack path
            , taMetadata = AtlasMetadata (0, 0) Vk.FORMAT_R8G8B8A8_UNORM Map.empty
            , taInfo = Just $ TextureInfo
                { tiImage = image
                , tiView = imageView
                , tiMemory = imageMemory
                , tiLayout = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                }
            , taRefCount = 1
            , taCleanup = Just imageCleanup
            , taBindlessSlot = bindlessSlot
            , taTextureHandle = texHandle
            }
    
      logDebugSM CatTexture "Texture loaded and registered"
        [("name", name)
        ,("asset_id", T.pack $ show nextId)
        ,("handle", T.pack $ show texHandle)
        ,("bindless_slot", maybe "none" (T.pack . show) bindlessSlot)]

      liftIO $ updateTextureState texHandle (AssetReady nextId []) pool
      
      -- Update the global IORef (the ONLY copy)
      liftIO $ atomicModifyIORef' poolRef $ \p →
        let updatedPool = p
              { apTextureAtlases = Map.insert nextId atlas (apTextureAtlases p)
              , apAssetPaths = Map.insert pathKey nextId (apAssetPaths p)
              }
        in (updatedPool, ())
      
      logDebugM CatAsset $ "Atlas inserted into global pool"
    
      pure nextId

-- | Decrement an asset's ref count; if it reaches zero, run its cleanup
--   action and remove it from the pool
unloadAsset ∷ AssetId → EngineM' ε ()
unloadAsset aid = do
  poolRef ← asks assetPoolRef
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
            mSys ← liftIO $ readIORef (textureSystemRef env)
            forM_ mSys $ \sys → do
              sys' ← unregisterTexture dev (taTextureHandle atlas) sys
              liftIO $ writeIORef (textureSystemRef env) (Just sys')
          liftIO $ maybe (pure ()) id (taCleanup atlas)
          liftIO $ atomicModifyIORef' poolRef $ \p → (p
            { apTextureAtlases = Map.delete aid (apTextureAtlases p)
            , apAssetPaths = Map.filter (/= aid) (apAssetPaths p)
            }, ())
      else
          liftIO $ atomicModifyIORef' poolRef $ \p → (p
            { apTextureAtlases = Map.adjust (\a → a { taRefCount = refCount }) aid (apTextureAtlases p)
            }, ())
      pure ()

    Nothing →
      case Map.lookup aid (apShaders pool) of
        Just program → do
          let newRefCount = spRefCount program - 1
          if newRefCount ≤ 0 then do
                -- Pipeline/modules may still be referenced by an in-flight
                -- frame — idle before destroying them.
                gs ← gets graphicsState
                forM_ (vulkanDevice gs) $ \dev → liftIO $ Vk.deviceWaitIdle dev
                liftIO $ maybe (pure ()) id (spCleanup program)
                liftIO $ atomicModifyIORef' poolRef $ \p → (p
                  { apShaders = Map.delete aid (apShaders p)
                  }, ())
            else
                liftIO $ atomicModifyIORef' poolRef $ \p → (p
                  { apShaders = Map.adjust (\pr → pr { spRefCount = newRefCount }) aid (apShaders p)
                  }, ())
          pure ()
          
        Nothing → logAndThrowM CatAsset (ExAsset (AssetNotFound aid))
                    "Attempted to unload non-existent asset"

getTextureAtlas ∷ AssetId → EngineM ε σ TextureAtlas
getTextureAtlas aid = do
  poolRef ← asks assetPoolRef
  pool ← liftIO $ readIORef poolRef
  case Map.lookup aid (apTextureAtlases pool) of
    Nothing → logAndThrowM CatAsset (ExAsset (AssetNotFound aid))
                "Texture atlas not found"
    Just atlas → pure atlas

getShaderProgram ∷ AssetId → EngineM' ε ShaderProgram
getShaderProgram aid = do
  poolRef ← asks assetPoolRef
  pool ← liftIO $ readIORef poolRef
  case Map.lookup aid (apShaders pool) of
    Nothing → logAndThrowM CatAsset (ExAsset (AssetNotFound aid))
                "Shader program not found"
    Just shader → pure shader

-- | Drain all assets: wait for the device to idle, run every cleanup action,
--   and reset the pool. Throws if cleanup is already in progress.
cleanupAssetManager ∷ EngineM' ε ()
cleanupAssetManager = do
    logInfoM CatAsset "Asset cleanup phase started"
    state ← gets graphicsState
    poolRef ← asks assetPoolRef
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

cleanupResources ∷ Vk.Device → GraphicsState → EngineM' ε ()
cleanupResources device _state = do
    poolRef ← asks assetPoolRef
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
              , apShaders = Map.empty
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

getTextureStateMap ∷ AssetPool → IO (Map.Map TextureHandle (AssetState AssetId))
getTextureStateMap pool = readIORef (apTextureHandles pool)

getFontStateMap ∷ AssetPool → IO (Map.Map FontHandle (AssetState AssetId))
getFontStateMap pool = readIORef (apFontHandles pool)

getShaderStateMap ∷ AssetPool → IO (Map.Map ShaderHandle (AssetState AssetId))
getShaderStateMap pool = readIORef (apShaderHandles pool)

getTextureHandleState ∷ TextureHandle → AssetPool → IO (Maybe (AssetState AssetId))
getTextureHandleState handle pool = do
  stateMap ← readIORef (apTextureHandles pool)
  return $ Map.lookup handle stateMap

getFontHandleState ∷ FontHandle → AssetPool → IO (Maybe (AssetState AssetId))
getFontHandleState handle pool = do
  stateMap ← readIORef (apFontHandles pool)
  return $ Map.lookup handle stateMap

getShaderHandleState ∷ ShaderHandle → AssetPool → IO (Maybe (AssetState AssetId))
getShaderHandleState handle pool = do
  stateMap ← readIORef (apShaderHandles pool)
  return $ Map.lookup handle stateMap
