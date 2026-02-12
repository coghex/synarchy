{-# LANGUAGE TypeApplications, AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables, UnicodeSyntax #-}
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
import Engine.Graphics.Vulkan.Types.Texture
  (TextureData, TextureArrayState)

-----------------------------------------------------------
-- Asset Pool
-----------------------------------------------------------

data AssetPool = AssetPool
  { apTextureAtlases    ∷ Map.Map AssetId TextureAtlas
  , apFonts             ∷ Map.Map AssetId Font
  , apShaders           ∷ Map.Map AssetId ShaderProgram
  , apAssetPaths        ∷ Map.Map Text AssetId
  , apNextAssetId       ∷ IORef Int
  , apNextTextureHandle ∷ IORef Int
  , apNextFontHandle    ∷ IORef Int
  , apNextShaderHandle  ∷ IORef Int
  , apTextureHandles    ∷ IORef
      (Map.Map TextureHandle (AssetState AssetId))
  , apFontHandles       ∷ IORef
      (Map.Map FontHandle (AssetState AssetId))
  , apShaderHandles     ∷ IORef
      (Map.Map ShaderHandle (AssetState AssetId))
  }

defaultAssetPool ∷ IO AssetPool
defaultAssetPool = do
  nextAssetIdRef ← newIORef 0
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
    , apNextAssetId    = nextAssetIdRef
    , apNextTextureHandle = nextTextureHandleRef
    , apNextFontHandle    = nextFontHandleRef
    , apNextShaderHandle  = nextShaderHandleRef
    , apTextureHandles    = textureHandlesRef
    , apFontHandles       = fontHandlesRef
    , apShaderHandles     = shaderHandlesRef
    }

data AssetConfig = AssetConfig
  { acMaxTextureAtlases ∷ Word32
  , acMaxShaderPrograms ∷ Word32
  , acPreloadAssets     ∷ Bool
  , acEnableHotReload   ∷ Bool
  } deriving (Show)

-----------------------------------------------------------
-- Texture Types
-----------------------------------------------------------

data AtlasMetadata = AtlasMetadata
  { amDimensions    ∷ (Word32, Word32)
  , amFormat        ∷ Format
  , amMipLevels     ∷ Word32
  , amSubTextures   ∷ Map.Map Text SubTextureInfo
  } deriving (Show)

data SubTextureInfo = SubTextureInfo
  { stiPosition     ∷ (Float, Float)
  , stiDimensions   ∷ (Float, Float)
  , stiRotated      ∷ Bool
  } deriving (Show)

data TextureAtlas = TextureAtlas
  { taId           ∷ AssetId
  , taName         ∷ Text
  , taPath         ∷ Text
  , taMetadata     ∷ AtlasMetadata
  , taStatus       ∷ AssetStatus
  , taInfo         ∷ Maybe TextureInfo
  , taRefCount     ∷ Word32
  , taCleanup      ∷ Maybe (IO ())
  , taBindlessSlot ∷ Maybe Word32
  , taTextureHandle ∷ TextureHandle
  }

-----------------------------------------------------------
-- Font Types
-----------------------------------------------------------

data GlyphInfo = GlyphInfo
  { giUVRect    ∷ (Float, Float, Float, Float)  -- ^ (u0, v0, u1, v1)
  , giSize      ∷ (Float, Float)                -- ^ (width, height)
  , giBearing   ∷ (Float, Float)                -- ^ (x, y) from baseline
  , giAdvance   ∷ Float                         -- ^ Horizontal advance
  } deriving (Show, Eq)

data Font = Font
  { fId         ∷ AssetId
  , fName       ∷ Text
  , fPath       ∷ Text
  , fSize       ∷ Word32
  , fStatus     ∷ AssetStatus
  , fAtlasId    ∷ Maybe AssetId
  , fGlyphMap   ∷ Map.Map Char GlyphInfo
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
         <> ", fGlyphMap = <" <> show (Map.size (fGlyphMap f))
         <> " glyphs>"
         <> ", fRefCount = " <> show (fRefCount f)
         <> ", fCleanup = " <> if isJust (fCleanup f)
            then "<present>" else "<absent> }"

-----------------------------------------------------------
-- Shader Types
-----------------------------------------------------------

data ShaderStageInfo = ShaderStageInfo
  { ssiStage       ∷ ShaderStageFlags
  , ssiEntryPoint  ∷ Text
  , ssiPath        ∷ Text
  } deriving (Show)

data ShaderProgram = ShaderProgram
  { spId           ∷ AssetId
  , spName         ∷ Text
  , spStages       ∷ V.Vector ShaderStageInfo
  , spStatus       ∷ AssetStatus
  , spModules      ∷ V.Vector ShaderModule
  , spRefCount     ∷ Word32
  , spCleanup      ∷ Maybe (IO ())
  }

data TextureArrayManager = TextureArrayManager
  { tamArrays     ∷ Map.Map Text TextureArrayState
  , tamTextureMap ∷ Map.Map AssetId Text
  }
