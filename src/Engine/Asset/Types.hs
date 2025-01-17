module Engine.Asset.Types where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Word (Word32)
import Vulkan.Core10
import Engine.Core.Base
import Engine.Asset.Base (AssetId, AssetStatus)
import Engine.Graphics.Vulkan.Base (TextureInfo)
import Engine.Graphics.Vulkan.Types.Texture (TextureData)

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
  , taInfo         ∷ Maybe TextureInfo   -- Vulkan resources
  , taRefCount     ∷ Word32
  , taCleanup      ∷ Maybe (IO ()) -- cleanup function
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
  , spCleanup      ∷ Maybe (IO ())      -- cleanup function
  }
-- | tracks texture array state
data TextureArrayState = TextureArrayState
  { tasDescriptorPool      ∷ DescriptorPool
  , tasDescriptorSetLayout ∷ DescriptorSetLayout
  , tasActiveTextures      ∷ V.Vector TextureData
  , tasDescriptorSet       ∷ Maybe DescriptorSet
  }
data TextureArrayManager = TextureArrayManager
  { tamArrays     ∷ Map.Map T.Text TextureArrayState
  , tamTextureMap ∷ Map.Map AssetId T.Text
  }
