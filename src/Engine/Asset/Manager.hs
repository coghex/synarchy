-- Engine/Asset/Manager.hs
module Engine.Asset.Manager
  ( initAssetManager
  -- * Asset Loading
  , loadTextureAtlas
  , loadShaderProgram
  -- * Asset Management
  , unloadAsset
  , reloadAsset
  , getTextureAtlas
  , getShaderProgram
  , generateHandle
  , updateAssetState
  -- * Resource Cleanup
  , cleanupAssetManager
  ) where

import UPrelude
import Control.Concurrent.MVar
import Control.Exception (finally, catch, SomeException)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Engine.Core.Monad
import Engine.Core.Resource (allocResource, allocResource')
import Engine.Core.State
import Engine.Core.Error.Exception
import Engine.Core.Var
import Engine.Asset.Base
import Engine.Asset.Types
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Base
import Engine.Graphics.Vulkan.Descriptor
import Engine.Graphics.Vulkan.Image (VulkanImage(..))
import Engine.Graphics.Vulkan.Texture
import Engine.Graphics.Vulkan.Types.Texture
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.ShaderCode
import qualified Vulkan.Core10 as Vk
import Vulkan.Zero

-- | Initialize the asset manager
initAssetManager ∷ AssetConfig → EngineM ε σ AssetPool
initAssetManager config = do
  -- Create empty asset pool with initial configuration
  ap ← liftIO defaultAssetPool
  pure ap

generateHandle ∷ ∀ h. AssetHandle h ⇒ AssetPool → IO h
generateHandle pool = fromInt <$>
  atomicModifyIORef' (getCounterRef @h pool) (\n → (n + 1, n))

lookupAsset ∷ ∀ h. AssetHandle h ⇒ h → AssetPool → IO (Maybe (AssetState AssetId))
lookupAsset handle pool = do
  stateMap ← readIORef (getStateMap @h pool)
  return $ Map.lookup handle stateMap

updateAssetState ∷ ∀ h. AssetHandle h ⇒ h → AssetState AssetId → AssetPool → IO ()
updateAssetState handle newState pool =
  atomicModifyIORef' (getStateMap @h pool) $ \m → (Map.insert handle newState m, ())

-- | Delete asset state for a handle
deleteAssetState ∷ ∀ h.  AssetHandle h ⇒ h → AssetPool → IO ()
deleteAssetState handle pool =
  atomicModifyIORef' (getStateMap @h pool) $ \m →
    (Map.delete handle m, ())

-- | Get all handles of a specific type
getAllHandles ∷ ∀ h. AssetHandle h ⇒ AssetPool → IO [h]
getAllHandles pool = do
  stateMap ← readIORef (getStateMap @h pool)
  return $ Map.keys stateMap

initTextureArrayManager ∷ Vk.Device → EngineM ε σ TextureArrayManager
initTextureArrayManager device = do
  defaultArray ← createTextureArrayState device
  pure $ TextureArrayManager
    { tamArrays = Map.singleton "default" defaultArray
    , tamTextureMap = Map.empty
    }

-- | Load a texture atlas from file
loadTextureAtlas ∷ Text      -- ^ Name of the atlas
                → FilePath     -- ^ Path to the atlas file
                → Text       -- ^ Array name
                → EngineM ε σ AssetId
loadTextureAtlas name path arrayName = do
  state ← get
  let pool = assetPool state
      pathKey = T.pack path
  -- check if the texture is already loaded
  case Map.lookup pathKey (apAssetPaths pool) of
    Just existingId → do
      -- texture is already loaded, increment refcount and reuse
      modify $ \s → s { assetPool = (assetPool s) {
        apTextureAtlases = Map.adjust (\a → a { taRefCount = taRefCount a + 1 })
                                      existingId (apTextureAtlases pool) } }
      return existingId
    Nothing → do
      -- First generate a new asset ID
      let nextId = AssetId $ fromIntegral $ apNextAssetId pool
      -- Get required Vulkan resources
      pDevice ← case (vulkanPDevice $ graphicsState state) of
        Nothing → throwAssetError (AssetLoadFailed path "loadTextureAtlas: ") 
                    "No physical device found"  
        Just pdev → pure pdev
      device ← case (vulkanDevice $ graphicsState state) of
        Nothing → throwAssetError (AssetLoadFailed path "loadTextureAtlas: ")
                    "No device found"
        Just dev → pure dev
      cmdPool ← case (vulkanCmdPool $ graphicsState state) of
        Nothing → throwAssetError (AssetLoadFailed path "loadTextureAtlas: ")
                    "No command pool found"
        Just pool → pure pool
      cmdQueue ← case (deviceQueues (graphicsState state)) of
        Nothing → throwAssetError (AssetLoadFailed path "loadTextureAtlas: ")
                    "No device queues found"
        Just queues → pure $ graphicsQueue queues
          
      -- Create the texture image, view and sampler
      ((vulkanImage@(VulkanImage image imageMemory), imageView, mipLevels), imageCleanup) ←
        createTextureImageView' pDevice device cmdPool cmdQueue path
        
      (sampler, samplerCleanup) ←
        createTextureSampler' device pDevice
    
      -- Get or create texture array state
      let texArrays = textureArrayStates $ graphicsState state
      texArray ← case Map.lookup arrayName texArrays of
        Just array → pure array
        Nothing → createTextureArrayState device
    
      -- Create texture data 
      let textureData = TextureData
            { tdImageView = imageView
            , tdSampler = sampler
            , tdMipLevels = mipLevels
            , tdDescriptorSet = error "Descriptor set not yet created" -- Will be updated
            }
          
      -- Update texture array state with new texture
      let newTexArray = texArray
            { tasActiveTextures = V.snoc (tasActiveTextures texArray) textureData
            }
      updatedTexArray ← updateTextureArrayDescriptors device newTexArray
    
      -- Create initial atlas entry
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
            }
    
      -- Update state
      modify $ \s → s 
        { assetPool = (assetPool s)
            { apTextureAtlases = Map.insert nextId atlas (apTextureAtlases pool)
            , apAssetPaths = Map.insert pathKey nextId (apAssetPaths pool)
            , apNextAssetId = apNextAssetId pool + 1
            }
        , graphicsState = (graphicsState s)
            { textureArrayStates = Map.insert arrayName updatedTexArray texArrays
            }
        }
    
      pure nextId

-- | Load a shader program
loadShaderProgram ∷ Text            -- ^ Name of the program
                 → V.Vector ShaderStageInfo  -- ^ Shader stages
                 → EngineM' ε AssetId
loadShaderProgram name stages = do
    undefined

-- | Unload an asset and cleanup its resources
unloadAsset ∷ AssetId → EngineM' ε ()
unloadAsset aid = do
  pool ← gets assetPool
  
  -- First check if it's a texture atlas
  case Map.lookup aid (apTextureAtlases pool) of
    Just atlas → do
      -- Decrement reference count
      let refCount = taRefCount atlas - 1
      if refCount <= 0 then do
          -- Execute cleanup if it exists
          liftIO $ maybe (pure ()) id (taCleanup atlas)
          -- Remove from pool
          modify $ \s → s { assetPool = (assetPool s) {
            apTextureAtlases = Map.delete aid (apTextureAtlases pool)
            , apAssetPaths = Map.filter (/= aid) (apAssetPaths pool)
          } }
      else modify $ \s → s { assetPool = (assetPool s) {
            apTextureAtlases = Map.adjust (\a → a { taRefCount = refCount }) aid (apTextureAtlases pool) }}
      pure ()
      
    Nothing →
      -- If not a texture, check if it's a shader program
      case Map.lookup aid (apShaders pool) of
        Just program → do
          -- decrement reference count
          let newRefCount = spRefCount program - 1
          if newRefCount <= 0 then do
                -- Execute cleanup if it exists
                liftIO $ maybe (pure ()) id (spCleanup program)
                -- Remove from pool
                modify $ \s → s { assetPool = (assetPool s) {
                  apShaders = Map.delete aid (apShaders pool)
                } }
            else modify $ \s → s { assetPool = (assetPool s) {
                  apShaders = Map.adjust (\p → p { spRefCount = newRefCount }) aid (apShaders pool) }}
          pure ()
          
        Nothing →
          throwAssetError (AssetNotFound "unloadAsset: ") $ T.pack $
            "Asset not found: " ⧺ show aid

-- | Reload an asset (useful for hot reloading)
reloadAsset ∷ AssetId → EngineM' ε ()
reloadAsset aid = do
  -- 1. Get existing asset info
  -- 2. Clean up old resources
  -- 3. Load new resources
  -- 4. Update pool
  undefined

-- | Get a texture atlas by ID
getTextureAtlas ∷ AssetId → EngineM ε σ TextureAtlas
getTextureAtlas aid = do
  pool ← gets assetPool
  case Map.lookup aid (apTextureAtlases pool) of
    Nothing → throwAssetError (AssetNotFound "getTextureAtlas: ") 
                "Texture atlas not found"
    Just atlas → pure atlas

-- | Get a shader program by ID
getShaderProgram ∷ AssetId → EngineM' ε ShaderProgram
getShaderProgram aid = do
  pool ← gets assetPool
  case Map.lookup aid (apShaders pool) of
    Nothing → throwAssetError (AssetNotFound "getShaderProgram: ")
                "Shader program not found"
    Just shader → pure shader

-- | Clean up all asset resources
cleanupAssetManager ∷ AssetPool → EngineM' ε ()
cleanupAssetManager pool = do
    state ← gets graphicsState

    when (cleanupStatus state == InProgress) $
      throwGraphicsError CleanupError "Cleanup already in progress"
    
    modify $ \s → s { graphicsState = (graphicsState s) { cleanupStatus = InProgress } }
    
    device ← case vulkanDevice state of
        Nothing → throwGraphicsError VulkanDeviceLost "No device during cleanup"
        Just d → pure d
    queues ← case deviceQueues state of
        Nothing → throwGraphicsError VulkanDeviceLost "No device queues during cleanup"
        Just q → pure q

    logDebug "Waiting for device to be idle..."
    liftIO $ do
        Vk.queueWaitIdle (graphicsQueue queues)
        Vk.queueWaitIdle (presentQueue queues)
        Vk.deviceWaitIdle device

    cleanupResources device state pool `catchError` \e → do
        logDebug $ "Cleanup error: " ⧺ (show e)
        modify $ \s → s { graphicsState = (graphicsState s) { cleanupStatus = Completed } }
        throwError e

cleanupResources ∷ Vk.Device → GraphicsState → AssetPool → EngineM' ε ()
cleanupResources device state pool = do
    forM_ (Map.toList $ textureArrayStates state) $ \(arrayName, arrayState) → do
        logDebug $ "Invalidating descriptor sets for: " ⧺ T.unpack arrayName
        when (isJust $ tasDescriptorSet arrayState) $ do
            modify $ \s → s { graphicsState = (graphicsState s) {
                textureArrayStates = Map.adjust (\as → as { 
                    tasDescriptorSet = Nothing 
                }) arrayName (textureArrayStates $ graphicsState s)
            }}

    forM_ (Map.toList $ textureArrayStates state) $ \(arrayName, arrayState) → do
        logDebug $ "Freeing descriptor sets for: " ⧺ T.unpack arrayName
        when (isJust $ tasDescriptorSet arrayState) $ do
            handleExceptions
              (liftIO $ Vk.freeDescriptorSets device 
                (tasDescriptorPool arrayState)
                (V.singleton $ fromJust $ tasDescriptorSet arrayState))
              "Freeing descriptor sets error: "

    forM_ (Map.elems $ apTextureAtlases pool) $ \atlas → do
        logDebug $ "Cleaning up texture atlas: " ⧺ T.unpack (taName atlas)
        case taStatus atlas of
            AssetLoaded → do
                liftIO $ Vk.deviceWaitIdle device
                handleExceptions
                  (liftIO $ maybe (pure ()) id (taCleanup atlas))
                  "Cleaning up texture atlas error: "
            _ → pure ()

    modify $ \s → s 
        { graphicsState = (graphicsState s) 
            { textureArrayStates = Map.empty
            , cleanupStatus = Completed
            }
        , assetPool = (assetPool s) 
            { apTextureAtlases = Map.empty
            , apShaders = Map.empty
            , apNextAssetId = 0
            }
        }

    handleExceptions
      (liftIO $ Vk.deviceWaitIdle device)
      "Final device wait error: "
    logDebug "Asset manager cleanup complete"

handleExceptions ∷ EngineM' ε () → Text → EngineM' ε ()
handleExceptions action errorMsg = action `catchError` \e → do
    logDebug $ (show errorMsg) ⧺ (show e)
    throwError e

freeVulkanDescriptorSets ∷ Vk.Device → Vk.DescriptorPool → V.Vector Vk.DescriptorSet → EngineM' ε ()
freeVulkanDescriptorSets device pool sets = do
    logDebug $ "Freeing descriptor sets: " ⧺ (show (V.length sets))
    handleExceptions
      (liftIO $ Vk.freeDescriptorSets device pool sets)
      "Error freeing descriptor sets: "
    logDebug "Descriptor sets freed successfully"
