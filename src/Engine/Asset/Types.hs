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
import Engine.Asset.Base (AssetId, AssetStatus)
import Engine.Asset.Handle
import Engine.Scene.Base (ObjectId)
import Engine.Graphics.Vulkan.Base (TextureInfo)
import Engine.Graphics.Vulkan.Types.Texture (TextureData, TextureArrayState)

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

data GlyphInfo = GlyphInfo
  { giUVRect    ∷ (Float, Float, Float, Float)  -- ^ UV coordinates (u0, v0, u1, v1) in atlas
  , giSize      ∷ (Float, Float)                -- ^ Glyph dimensions (width, height) in pixels
  , giBearing   ∷ (Float, Float)                -- ^ Offset from baseline (x, y)
  , giAdvance   ∷ Float                         -- ^ Horizontal advance to next glyph
  } deriving (Show, Eq)

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
  , taBindlessSlot ∷ Maybe Word32  -- bindless texture slot index
  , taTextureHandle ∷ TextureHandle -- handle for binless lookup
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
instance Show Font where
  show f = "Font { fId = " <> show (fId f)
         <> ", fName = " <> show (fName f)
         <> ", fPath = " <> show (fPath f)
         <> ", fSize = " <> show (fSize f)
         <> ", fStatus = " <> show (fStatus f)
         <> ", fAtlasId = " <> show (fAtlasId f)
         <> ", fGlyphMap = <" <> show (Map.size (fGlyphMap f)) <> " glyphs>"
         <> ", fRefCount = " <> show (fRefCount f)
         <> ", fCleanup = " <> if isJust (fCleanup f) then "<present>" else "<absent> }"
        
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
data TextureArrayManager = TextureArrayManager
  { tamArrays     ∷ Map.Map Text TextureArrayState
  , tamTextureMap ∷ Map.Map AssetId Text
  }
