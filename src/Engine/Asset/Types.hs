module Engine.Asset.Types where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Word (Word32)
import Vulkan.Core10
import Engine.Graphics.Vulkan.Types (TextureInfo)

-- | Unique identifier for assets
newtype AssetId = AssetId Word32
  deriving (Eq, Ord, Show)

-- | Asset status tracking
data AssetStatus
  = AssetUnloaded
  | AssetLoading
  | AssetLoaded
  | AssetError T.Text
  deriving (Eq, Show)

-- | Metadata for texture atlases
data AtlasMetadata = AtlasMetadata
  { amDimensions    ∷ (Word32, Word32)  -- width and height
  , amFormat        ∷ Format
  , amMipLevels     ∷ Word32
  , amSubTextures   ∷ Map.Map T.Text SubTextureInfo
  } deriving (Show)

-- | Information about a sub-texture in an atlas
data SubTextureInfo = SubTextureInfo
  { stiPosition     ∷ (Float, Float)    -- x, y position in atlas
  , stiDimensions   ∷ (Float, Float)    -- width, height in atlas
  , stiRotated      ∷ Bool              -- whether the subtexture is rotated
  } deriving (Show)

-- | Texture atlas resource
data TextureAtlas = TextureAtlas
  { taId           ∷ AssetId
  , taName         ∷ T.Text
  , taPath         ∷ T.Text
  , taMetadata     ∷ AtlasMetadata
  , taStatus       ∷ AssetStatus
  , taInfo         ∷ Maybe TextureInfo  -- Vulkan resources
  , taRefCount     ∷ Word32
  }

-- | Shader stage specification
data ShaderStageInfo = ShaderStageInfo
  { ssiStage       ∷ ShaderStageFlags
  , ssiEntryPoint  ∷ T.Text
  , ssiPath        ∷ T.Text
  } deriving (Show)

-- | Shader program resource
data ShaderProgram = ShaderProgram
  { spId           ∷ AssetId
  , spName         ∷ T.Text
  , spStages       ∷ V.Vector ShaderStageInfo
  , spStatus       ∷ AssetStatus
  , spModules      ∷ V.Vector ShaderModule
  , spRefCount     ∷ Word32
  } deriving (Show)

-- | Asset pool containing all loaded assets
data AssetPool = AssetPool
  { apTextureAtlases    ∷ Map.Map AssetId TextureAtlas
  , apShaderPrograms    ∷ Map.Map AssetId ShaderProgram
  , apNextId            ∷ Word32
  }

-- | Asset loading configuration
data AssetConfig = AssetConfig
  { acMaxTextureAtlases ∷ Word32
  , acMaxShaderPrograms ∷ Word32
  , acPreloadAssets     ∷ Bool
  , acEnableHotReload   ∷ Bool
  } deriving (Show)
