-- Engine/Asset/Manager.hs
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
import Control.Concurrent.MVar
import Control.Exception (finally, catch, SomeException)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef', writeIORef)
import Engine.Core.Monad
import Engine.Core.Resource (allocResource, allocResource')
import Engine.Core.State
import Engine.Core.Error.Exception (ExceptionType(..), GraphicsError(..)
                                   , AssetError(..))
import Engine.Core.Log.Monad (logDebugM, logWarnM, logInfoM, logAndThrowM
                             , logDebugSM, logInfoSM)
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Var
import Engine.Asset.Base
import Engine.Asset.Types
import Engine.Asset.Handle
import Engine.Graphics.Types
import Engine.Graphics.Config (textureFilterToVulkan)
import Engine.Graphics.Vulkan.Base
import Engine.Graphics.Vulkan.Descriptor
import Engine.Graphics.Vulkan.Image (VulkanImage(..))
import Engine.Graphics.Vulkan.Texture
import Engine.Graphics.Vulkan.Types.Texture
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.ShaderCode
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.Texture.Bindless (registerTexture)
import Engine.Graphics.Vulkan.Texture.Slot (TextureSlot(..))
import Engine.Graphics.Vulkan.Texture.Handle (BindlessTextureHandle(..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Zero

-- | Generate a texture handle
generateTextureHandle ∷ AssetPool → IO TextureHandle
generateTextureHandle pool =
  atomicModifyIORef' (apNextTextureHandle pool) $ \n →
    (n + 1, TextureHandle n)

-- | Generate a font handle
generateFontHandle ∷ AssetPool → IO FontHandle
generateFontHandle pool =
  atomicModifyIORef' (apNextFontHandle pool) $ \n →
    (n + 1, FontHandle n)

-- | Generate a shader handle
generateShaderHandle ∷ AssetPool → IO ShaderHandle
generateShaderHandle pool =
  atomicModifyIORef' (apNextShaderHandle pool) $ \n →
    (n + 1, ShaderHandle n)

-- | Generate an asset ID atomically
generateAssetId ∷ AssetPool → IO AssetId
generateAssetId pool =
  atomicModifyIORef' (apNextAssetId pool) $ \n →
    (n + 1, AssetId $ fromIntegral n)

-- | Update texture handle state
updateTextureState ∷ TextureHandle → AssetState AssetId → AssetPool → IO ()
updateTextureState handle newState pool =
  atomicModifyIORef' (apTextureHandles pool) $ \m →
    (Map.insert handle newState m, ())

-- | Update font handle state
updateFontState ∷ FontHandle → AssetState AssetId → AssetPool → IO ()
updateFontState handle newState pool =
  atomicModifyIORef' (apFontHandles pool) $ \m →
    (Map.insert handle newState m, ())

-- | Update shader handle state
updateShaderState ∷ ShaderHandle → AssetState AssetId → AssetPool → IO ()
updateShaderState handle newState pool =
  atomicModifyIORef' (apShaderHandles pool) $ \m →
    (Map.insert handle newState m, ())

-- | Delete texture handle state
deleteTextureState ∷ TextureHandle → AssetPool → IO ()
deleteTextureState handle pool =
  atomicModifyIORef' (apTextureHandles pool) $ \m →
    (Map.delete handle m, ())

-- | Delete font handle state
deleteFontState ∷ FontHandle → AssetPool → IO ()
deleteFontState handle pool =
  atomicModifyIORef' (apFontHandles pool) $ \m →
    (Map.delete handle m, ())

-- | Delete shader handle state
deleteShaderState ∷ ShaderHandle → AssetPool → IO ()
deleteShaderState handle pool =
  atomicModifyIORef' (apShaderHandles pool) $ \m →
    (Map.delete handle m, ())

-- | Get all texture handles
getAllTextureHandles ∷ AssetPool → IO [TextureHandle]
getAllTextureHandles pool = do
  stateMap ← readIORef (apTextureHandles pool)
  return $ Map.keys stateMap

-- | Get all font handles
getAllFontHandles ∷ AssetPool → IO [FontHandle]
getAllFontHandles pool = do
  stateMap ← readIORef (apFontHandles pool)
  return $ Map.keys stateMap

-- | Get all shader handles
getAllShaderHandles ∷ AssetPool → IO [ShaderHandle]
getAllShaderHandles pool = do
  stateMap ← readIORef (apShaderHandles pool)
  return $ Map.keys stateMap

-- | Lookup texture handle state
lookupTextureAsset ∷ TextureHandle → AssetPool → IO (Maybe (AssetState AssetId))
lookupTextureAsset handle pool = do
  stateMap ← readIORef (apTextureHandles pool)
  return $ Map.lookup handle stateMap

-- | Lookup font handle state
lookupFontAsset ∷ FontHandle → AssetPool → IO (Maybe (AssetState AssetId))
lookupFontAsset handle pool = do
  stateMap ← readIORef (apFontHandles pool)
  return $ Map.lookup handle stateMap

-- | Lookup shader handle state
lookupShaderAsset ∷ ShaderHandle → AssetPool → IO (Maybe (AssetState AssetId))
lookupShaderAsset handle pool = do
  stateMap ← readIORef (apShaderHandles pool)
  return $ Map.lookup handle stateMap

-- | Load a texture atlas from file (generates a new handle)
loadTextureAtlas ∷ Text → FilePath → Text → EngineM ε σ AssetId
loadTextureAtlas name path arrayName = do
  poolRef ← asks assetPoolRef
  pool ← liftIO $ readIORef poolRef
  texHandle ← liftIO $ generateTextureHandle pool
  loadTextureAtlasWithHandle texHandle name path arrayName

-- | Load a texture atlas from file with a specific TextureHandle
-- This is used when the handle was already generated (e.g., from Lua)
loadTextureAtlasWithHandle ∷ TextureHandle  -- ^ Pre-generated handle
                          → Text            -- ^ Name of the atlas
                          → FilePath        -- ^ Path to the atlas file
                          → Text            -- ^ Array name
                          → EngineM ε σ AssetId
loadTextureAtlasWithHandle texHandle name path arrayName = do
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
      
      -- Register the NEW handle in bindless system pointing to the SAME slot
      -- Use atomic modify to avoid race conditions
      modify $ \s → 
        case textureSystem (graphicsState s) of
          Just bindless →
            case Map.lookup (taTextureHandle existingAtlas) (btsHandleMap bindless) of
              Just existingBindlessHandle →
                let newBindless = bindless { 
                      btsHandleMap = Map.insert texHandle existingBindlessHandle (btsHandleMap bindless)
                    }
                in s { graphicsState = (graphicsState s) { textureSystem = Just newBindless } }
              Nothing → s
          Nothing → s
      
      currentState ← gets graphicsState
      case textureSystem currentState of
        Just bs →
          case Map.lookup texHandle (btsHandleMap bs) of
            Just _ → logDebugM CatAsset $ "New handle " <> T.pack (show texHandle) 
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
      ((vulkanImage@(VulkanImage image imageMemory), imageView, mipLevels), imageCleanup) ←
        createTextureImageView' pDevice device cmdPool cmdQueue path
        
      env ← ask
      filterMode ← liftIO $ readIORef (textureFilterRef env)
      let vkFilter = textureFilterToVulkan filterMode
      (sampler, samplerCleanup) ←
        createTextureSampler' device pDevice vkFilter

      -- Register with bindless system using the provided handle
      bindlessSlot ← case textureSystem (graphicsState state) of
        Just bindless → do
          logDebugM CatAsset "Registering texture with bindless system"
          (mbHandle, newBindless) ← registerTexture device texHandle imageView sampler bindless
          modify $ \s → s { graphicsState = (graphicsState s) {
            textureSystem = Just newBindless
            } }
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
            , taMetadata = AtlasMetadata (0, 0) Vk.FORMAT_R8G8B8A8_UNORM mipLevels Map.empty
            , taStatus = AssetLoaded
            , taInfo = Just $ TextureInfo
                { tiImage = image
                , tiView = imageView
                , tiSampler = sampler
                , tiMemory = imageMemory
                , tiLayout = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                }
            , taRefCount = 1
            , taCleanup = Just $ do
                samplerCleanup
                imageCleanup
            , taBindlessSlot = bindlessSlot
            , taTextureHandle = texHandle
            }
    
      logDebugSM CatTexture "Texture loaded and registered"
        [("name", name)
        ,("asset_id", T.pack $ show nextId)
        ,("handle", T.pack $ show texHandle)
        ,("mip_levels", T.pack $ show mipLevels)
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

-- | Unload an asset and cleanup its resources
unloadAsset ∷ AssetId → EngineM' ε ()
unloadAsset aid = do
  poolRef ← asks assetPoolRef
  pool ← liftIO $ readIORef poolRef
  
  case Map.lookup aid (apTextureAtlases pool) of
    Just atlas → do
      let refCount = taRefCount atlas - 1
      if refCount ≤ 0 then do
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

-- | Get a texture atlas by ID
getTextureAtlas ∷ AssetId → EngineM ε σ TextureAtlas
getTextureAtlas aid = do
  poolRef ← asks assetPoolRef
  pool ← liftIO $ readIORef poolRef
  case Map.lookup aid (apTextureAtlases pool) of
    Nothing → logAndThrowM CatAsset (ExAsset (AssetNotFound aid))
                "Texture atlas not found"
    Just atlas → pure atlas

-- | Get a shader program by ID
getShaderProgram ∷ AssetId → EngineM' ε ShaderProgram
getShaderProgram aid = do
  poolRef ← asks assetPoolRef
  pool ← liftIO $ readIORef poolRef
  case Map.lookup aid (apShaders pool) of
    Nothing → logAndThrowM CatAsset (ExAsset (AssetNotFound aid))
                "Shader program not found"
    Just shader → pure shader

-- | Clean up all asset resources
cleanupAssetManager ∷ EngineM' ε ()
cleanupAssetManager = do
    logInfoM CatAsset "Asset cleanup phase started"
    state ← gets graphicsState
    poolRef ← asks assetPoolRef
    pool ← liftIO $ readIORef poolRef

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
cleanupResources device state = do
    poolRef ← asks assetPoolRef
    pool ← liftIO $ readIORef poolRef
    forM_ (Map.elems $ apTextureAtlases pool) $ \atlas → do
        logDebugSM CatAsset "Cleaning up texture"
          [("name", taName atlas)
          ,("path", taPath atlas)
          ,("asset_id", T.pack $ show $ taId atlas)]
        case taStatus atlas of
            AssetLoaded → do
                liftIO $ Vk.deviceWaitIdle device
                (liftIO $ maybe (pure ()) id (taCleanup atlas))
            _ → pure ()

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

-- | Get texture handle state map
getTextureStateMap ∷ AssetPool → IO (Map.Map TextureHandle (AssetState AssetId))
getTextureStateMap pool = readIORef (apTextureHandles pool)

-- | Get font handle state map
getFontStateMap ∷ AssetPool → IO (Map.Map FontHandle (AssetState AssetId))
getFontStateMap pool = readIORef (apFontHandles pool)

-- | Get shader handle state map
getShaderStateMap ∷ AssetPool → IO (Map.Map ShaderHandle (AssetState AssetId))
getShaderStateMap pool = readIORef (apShaderHandles pool)

-- | Get specific texture handle state
getTextureHandleState ∷ TextureHandle → AssetPool → IO (Maybe (AssetState AssetId))
getTextureHandleState handle pool = do
  stateMap ← readIORef (apTextureHandles pool)
  return $ Map.lookup handle stateMap

-- | Get specific font handle state
getFontHandleState ∷ FontHandle → AssetPool → IO (Maybe (AssetState AssetId))
getFontHandleState handle pool = do
  stateMap ← readIORef (apFontHandles pool)
  return $ Map.lookup handle stateMap

-- | Get specific shader handle state
getShaderHandleState ∷ ShaderHandle → AssetPool → IO (Maybe (AssetState AssetId))
getShaderHandleState handle pool = do
  stateMap ← readIORef (apShaderHandles pool)
  return $ Map.lookup handle stateMap
