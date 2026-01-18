{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Engine.Asset.Types where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.IORef (IORef, newIORef)
import Vulkan.Core10
import Engine.Core.Base
import Engine.Asset.Base (AssetId, AssetStatus)
import Engine.Scene.Base (ObjectId)
import Engine.Graphics.Vulkan.Base (TextureInfo)
import Engine.Graphics.Vulkan.Types.Texture (TextureData)

-- | Asset pool containing all loaded assets
data AssetPool = AssetPool
  { apTextureAtlases    ∷ Map.Map AssetId TextureAtlas
  , apFonts             ∷ Map.Map AssetId Font
  , apShaders           ∷ Map.Map AssetId ShaderProgram
  , apAssetPaths        ∷ Map.Map Text AssetId
  , apNextAssetId       ∷ Int
  , apNextTextureHandle ∷ IORef Int
  , apNextFontHandle    ∷ IORef Int
  , apNextShaderHandle  ∷ IORef Int
  , apTextureHandles    ∷ IORef (Map.Map TextureHandle (AssetState AssetId))
  , apFontHandles       ∷ IORef (Map.Map FontHandle (AssetState AssetId))
  , apShaderHandles     ∷ IORef (Map.Map ShaderHandle (AssetState AssetId))
  }

-- | Create default asset pool with initialized IORefs
defaultAssetPool ∷ IO AssetPool
defaultAssetPool = do
  nextTextureHandleRef ← newIORef 0
  nextFontHandleRef ← newIORef 0
  nextShaderHandleRef ← newIORef 0
  textureHandlesRef ← newIORef Map.empty
  fontHandlesRef ← newIORef Map.empty
  shaderHandlesRef ← newIORef Map.empty
  
  pure $ AssetPool
    { apTextureAtlases = Map.empty
    , apFonts          = Map.empty
    , apShaders        = Map.empty
    , apAssetPaths     = Map.empty
    , apNextAssetId    = 0
    , apNextTextureHandle = nextTextureHandleRef
    , apNextFontHandle    = nextFontHandleRef
    , apNextShaderHandle  = nextShaderHandleRef
    , apTextureHandles    = textureHandlesRef
    , apFontHandles       = fontHandlesRef
    , apShaderHandles     = shaderHandlesRef
    }

-- | Asset loading configuration
data AssetConfig = AssetConfig
  { acMaxTextureAtlases ∷ Word32
  , acMaxShaderPrograms ∷ Word32
  , acPreloadAssets     ∷ Bool
  , acEnableHotReload   ∷ Bool
  } deriving (Show)

-- | Asset Handles provide opaque references to assets
newtype TextureHandle = TextureHandle Int
  deriving (Show, Eq, Ord, Num)
newtype FontHandle = FontHandle Int
  deriving (Show, Eq, Ord, Num)
newtype ShaderHandle = ShaderHandle Int
  deriving (Show, Eq, Ord, Num)
-- | asset handle type class
class (Eq h, Ord h, Num h) ⇒ AssetHandle h where
  -- get the counter reference for this handle type
  getCounterRef ∷ AssetPool → IORef Int
  -- get the state map for this handle type
  getStateMap ∷ AssetPool → IORef (Map.Map h (AssetState AssetId))
  -- construct a handle from an integer
  fromInt ∷ Int → h
  -- extract Int from handle
  toInt ∷ h → Int
-- Instances
instance AssetHandle TextureHandle where
  getCounterRef = apNextTextureHandle
  getStateMap   = apTextureHandles
  fromInt       = TextureHandle
  toInt (TextureHandle n) = n
instance AssetHandle FontHandle where
  getCounterRef = apNextFontHandle
  getStateMap   = apFontHandles
  fromInt       = FontHandle
  toInt (FontHandle n) = n
instance AssetHandle ShaderHandle where
  getCounterRef = apNextShaderHandle
  getStateMap   = apShaderHandles
  fromInt       = ShaderHandle
  toInt (ShaderHandle n) = n

-- | Asset Loading States
data AssetState α
  = AssetLoading { asPath       ∷ FilePath
                 , asDependents ∷ [Dependent]
                 , asProgress   ∷ Float }
  | AssetReady { asValue      ∷ α
               , asDependents ∷ [Dependent] }
  | AssetFailed { asError ∷ Text }
  deriving (Show, Eq)

-- | Dependent asset information
data Dependent = DependentEntity ObjectId -- scene entity waiting for asset
               | DependentLuaCallback Int -- lua callback id
               deriving (Show, Eq)

-- | Metadata for texture atlases
data AtlasMetadata = AtlasMetadata
  { amDimensions    ∷ (Word32, Word32)  -- width and height
  , amFormat        ∷ Format
  , amMipLevels     ∷ Word32
  , amSubTextures   ∷ Map.Map Text SubTextureInfo
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
  , taName         ∷ Text
  , taPath         ∷ Text
  , taMetadata     ∷ AtlasMetadata
  , taStatus       ∷ AssetStatus
  , taInfo         ∷ Maybe TextureInfo   -- Vulkan resources
  , taRefCount     ∷ Word32
  , taCleanup      ∷ Maybe (IO ()) -- cleanup function
  }

-- | Font resource (stub for future implementation)
data Font = Font
  { fId         ∷ AssetId
  , fName       ∷ Text
  , fPath       ∷ Text
  , fSize       ∷ Word32          -- Font size in pixels
  , fStatus     ∷ AssetStatus
  , fAtlasId    ∷ Maybe AssetId   -- References texture atlas with glyph data
  , fGlyphMap   ∷ Map.Map Char GlyphInfo  -- Character → glyph lookup
  , fRefCount   ∷ Word32
  , fCleanup    ∷ Maybe (IO ())
  }

-- | Information about a single glyph in the font atlas
data GlyphInfo = GlyphInfo
  { giChar      ∷ Char            -- The character
  , giUVRect    ∷ (Float, Float, Float, Float)  -- UV coords (x, y, w, h)
  , giSize      ∷ (Word32, Word32)  -- Glyph size in pixels
  , giOffset    ∷ (Int, Int)      -- Offset from baseline
  , giAdvance   ∷ Word32          -- Horizontal advance to next glyph
  } deriving (Show, Eq)

-- | Shader stage specification
data ShaderStageInfo = ShaderStageInfo
  { ssiStage       ∷ ShaderStageFlags
  , ssiEntryPoint  ∷ Text
  , ssiPath        ∷ Text
  } deriving (Show)

-- | Shader program resource
data ShaderProgram = ShaderProgram
  { spId           ∷ AssetId
  , spName         ∷ Text
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
  { tamArrays     ∷ Map.Map Text TextureArrayState
  , tamTextureMap ∷ Map.Map AssetId Text
  }
