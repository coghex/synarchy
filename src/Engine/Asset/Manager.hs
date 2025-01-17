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
import Control.Monad (void, forM_)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, modify, gets)
import Data.Maybe (fromMaybe)
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
      
  -- Create initial atlas entry
  let atlas = TextureAtlas
        { taId = nextId
        , taName = name
        , taPath = T.pack path
        , taMetadata = AtlasMetadata (0, 0) Vk.FORMAT_UNDEFINED 0 Map.empty
        , taStatus = AssetLoading
        , taInfo = Nothing
        , taRefCount = 1
        , taCleanup = Nothing
        }

  -- Update pool with initial entry
  modify $ \s → s { assetPool = (assetPool s)
    { apTextureAtlases = Map.insert nextId atlas (apTextureAtlases pool)
    , apNextId = apNextId pool + 1
    } }
  -- Load the texture with proper cleanup handling
  ((vulkanImage@(VulkanImage image imageMemory),imageView,mipLevels),imagecleanup) ←
    (createTextureImageView' pDevice device cmdPool cmdQueue path)

  (sampler, samplercleanup) ←
    (createTextureSampler' device pDevice)

  -- Create the TextureInfo
  let textureInfo = TextureInfo
        { tiImage = image
        , tiView = imageView
        , tiSampler = sampler
        , tiMemory = imageMemory
        , tiLayout = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
        }

--  -- Get current texture array state
--  (TexturePoolState pool layout, textures) ← gets (textureState . graphicsState)
--  
--  -- Create descriptor set for this texture
--  descriptorSet ← createTextureDescriptorSet device pool layout imageView sampler
--
--  -- Update texture array state with new texture
--  let textureData = TextureData
--        { tdImageView = imageView
--        , tdSampler = sampler
--        , tdMipLevels = mipLevels
--        , tdDescriptorSet = descriptorSet
--        }
--      newTextures = V.snoc textures textureData
--
--  -- Update engine state with new texture array
--  modify $ \s → s { graphicsState = (graphicsState s) {
--    textureState = (TexturePoolState pool layout, newTextures)
--  } }

  -- Combine cleanup actions
  let cleanup = do
        samplercleanup
        imagecleanup

  -- Update atlas with loaded texture
  modify $ \s → s { assetPool = (assetPool s)
    { apTextureAtlases = Map.adjust
        (\a → a { taMetadata = AtlasMetadata (0, 0)
                                 Vk.FORMAT_R8G8B8A8_UNORM mipLevels Map.empty
                , taStatus = AssetLoaded
                , taInfo = Just textureInfo
                , taCleanup = Just cleanup
                })
        nextId
        (apTextureAtlases $ assetPool s)
    } }

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
  -- First clean up all texture atlases
  forM_ (Map.elems $ apTextureAtlases pool) $ \atlas → do
    logDebug $ "Cleaning up texture atlas: " ⧺ T.unpack (taName atlas)
    case taStatus atlas of
      AssetLoaded → do
        -- Execute cleanup if it exists
        liftIO $ maybe (pure ()) id (taCleanup atlas)
      AssetLoading →
        logDebug $ "Warning: Cleaning up texture that was still loading: " 
                   ⧺ T.unpack (taName atlas)
      _ → pure ()

  -- Then clean up all shader programs
  forM_ (Map.elems $ apShaderPrograms pool) $ \program → do
    logDebug $ "Cleaning up shader program: " ⧺ T.unpack (spName program)
    case spStatus program of
      AssetLoaded → do
        -- Execute cleanup if it exists
        liftIO $ maybe (pure ()) id (spCleanup program)
      AssetLoading →
        logDebug $ "Warning: Cleaning up shader that was still loading: "
                   ⧺ T.unpack (spName program)
      _ → pure ()

  -- Clear the asset pool
  modify $ \s → s { assetPool = AssetPool
    { apTextureAtlases = Map.empty
    , apShaderPrograms = Map.empty
    , apNextId = 0
    }
  }

  logDebug "Asset manager cleanup complete"
