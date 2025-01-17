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
  -- * Resource Cleanup
  , cleanupAssetManager
  ) where

import UPrelude
import Control.Monad (void, forM_, when)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, modify, gets)
import Data.Maybe (fromMaybe, isJust, fromJust)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Monad.IO.Class (MonadIO(..))
import Engine.Core.Monad
import Engine.Core.Resource (allocResource, allocResource')
import Engine.Core.State
import Engine.Core.Error.Exception
import Engine.Asset.Base
import Engine.Asset.Types
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Base
import Engine.Graphics.Vulkan.Descriptor
import Engine.Graphics.Vulkan.Image (VulkanImage(..))
import Engine.Graphics.Vulkan.Texture
import Engine.Graphics.Vulkan.Types.Texture
import qualified Vulkan.Core10 as Vk

-- | Initialize the asset manager
initAssetManager ∷ AssetConfig → EngineM ε σ AssetPool
initAssetManager config = do
  -- Create empty asset pool with initial configuration
  pure $ AssetPool
    { apTextureAtlases = Map.empty
    , apShaderPrograms = Map.empty
    , apNextId = 0
    }

-- Add to Engine/Asset/Manager.hs
initTextureArrayManager ∷ Vk.Device → EngineM ε σ TextureArrayManager
initTextureArrayManager device = do
  defaultArray ← createTextureArrayState device
  pure $ TextureArrayManager
    { tamArrays = Map.singleton "default" defaultArray
    , tamTextureMap = Map.empty
    }

-- | Load a texture atlas from file
loadTextureAtlas ∷ T.Text      -- ^ Name of the atlas
                → FilePath     -- ^ Path to the atlas file
                → T.Text       -- ^ Array name
                → EngineM ε σ AssetId
loadTextureAtlas name path arrayName = do
  -- First generate a new asset ID
  state ← get
  let pool = assetPool state
      nextId = AssetId $ apNextId pool
  
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
        , apNextId = apNextId pool + 1
        }
    , graphicsState = (graphicsState s)
        { textureArrayStates = Map.insert arrayName updatedTexArray texArrays
        }
    }

  pure nextId

-- | Load a shader program
loadShaderProgram ∷ T.Text            -- ^ Name of the program
                 → V.Vector ShaderStageInfo  -- ^ Shader stages
                 → EngineM' ε AssetId
loadShaderProgram name stages = do
  -- 1. Generate new asset ID
  -- 2. Load and compile shader modules
  -- 3. Add to asset pool
  undefined

-- | Unload an asset and cleanup its resources
unloadAsset ∷ AssetId → EngineM' ε ()
unloadAsset aid = do
  pool ← gets assetPool
  
  -- First check if it's a texture atlas
  case Map.lookup aid (apTextureAtlases pool) of
    Just atlas → do
      -- Execute cleanup if it exists
      liftIO $ maybe (pure ()) id (taCleanup atlas)
      -- Remove from pool
      modify $ \s → s { assetPool = (assetPool s) {
        apTextureAtlases = Map.delete aid (apTextureAtlases pool)
      } }
      pure ()
      
    Nothing →
      -- If not a texture, check if it's a shader program
      case Map.lookup aid (apShaderPrograms pool) of
        Just program → do
          -- Execute cleanup if it exists
          liftIO $ maybe (pure ()) id (spCleanup program)
          -- Remove from pool
          modify $ \s → s { assetPool = (assetPool s) {
            apShaderPrograms = Map.delete aid (apShaderPrograms pool)
          } }
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
  case Map.lookup aid (apShaderPrograms pool) of
    Nothing → throwAssetError (AssetNotFound "getShaderProgram: ")
                "Shader program not found"
    Just shader → pure shader

-- | Clean up all asset resources
cleanupAssetManager ∷ AssetPool → EngineM' ε ()
cleanupAssetManager pool = do
    -- Get device for cleanup
    state ← gets graphicsState
    device ← case vulkanDevice state of
        Nothing → throwGraphicsError VulkanDeviceLost "No device during cleanup"
        Just d → pure d
    queues ← case deviceQueues state of
        Nothing → throwGraphicsError VulkanDeviceLost "No device queues during cleanup"
        Just q → pure q
        
    -- Wait for device to be idle before cleanup
    liftIO $ do
      Vk.queueWaitIdle (graphicsQueue queues)
      Vk.queueWaitIdle (presentQueue queues)
      Vk.deviceWaitIdle device
    
    -- 1. Free descriptor sets first
    forM_ (Map.toList $ textureArrayStates state) $ \(arrayName, arrayState) → do
        logDebug $ "Cleaning up texture array state: " ⧺ T.unpack arrayName
        when (isJust $ tasDescriptorSet arrayState) $ do
            -- Free descriptor sets
            freeVulkanDescriptorSets device 
                (tasDescriptorPool arrayState)
                (V.singleton $ fromJust $ tasDescriptorSet arrayState)
            -- Destroy pool and layout
            --destroyDescriptorPool device (tasDescriptorPool arrayState) Nothing
            --destroyDescriptorSetLayout device (tasDescriptorSetLayout arrayState) Nothing

    -- 2. Clean up textures
    forM_ (Map.elems $ apTextureAtlases pool) $ \atlas → do
        logDebug $ "Cleaning up texture atlas: " ⧺ T.unpack (taName atlas)
        case taStatus atlas of
            AssetLoaded → do
                -- Wait for device to be idle again before each texture cleanup
                liftIO $ Vk.deviceWaitIdle device
                liftIO $ maybe (pure ()) id (taCleanup atlas)
            _ → pure ()

    -- Clear states
    modify $ \s → s 
        { graphicsState = (graphicsState s) 
            { textureArrayStates = Map.empty }
        , assetPool = (assetPool s) 
            { apTextureAtlases = Map.empty
            , apShaderPrograms = Map.empty
            , apNextId = 0
            }
        }

    logDebug "Asset manager cleanup complete"
