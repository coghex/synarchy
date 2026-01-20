module Engine.Graphics.Font.Data where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Data.Word (Word8, Word32)
import Engine.Asset.Types (TextureHandle, FontHandle, GlyphInfo)
import Engine.Scene.Base (ObjectId)
import Vulkan.Core10

-- | a complete font atlas texture with glyph metadata
data FontAtlas = FontAtlas
  { faTexture   ∷ TextureHandle
  , faGlyphData ∷ Map.Map Char GlyphInfo
  , faAtlasWidth  ∷ Int
  , faAtlasHeight ∷ Int
  , faFontSize    ∷ Int
  , faLineHeight  ∷ Float       
  , faBaseline    ∷ Float
  , faAtlasBitmap ∷ [Word8]
  , faDescriptorSet ∷ Maybe DescriptorSet
  , faImageView     ∷ Maybe ImageView
  , faSampler       ∷ Maybe Sampler
  } deriving (Eq, Show)

-- | add instance data
data GlyphInstance = GlyphInstance
  { giPosition ∷ (Float, Float)
  , giSize     ∷ (Float, Float)
  , giUVRect   ∷ (Float, Float, Float, Float)
  , giColor    ∷ (Float, Float, Float, Float)
  } deriving (Eq, Show)

-- | batch of text instances
data TextBatch = TextBatch
  { tbFontHandle   ∷ FontHandle
  , tbInstances    ∷ V.Vector GlyphInstance
  } deriving (Eq, Show)

-- | Cache of loaded fonts to avoid re-loading
data FontCache = FontCache
    { fcFonts       ∷ Map.Map FontHandle FontAtlas        -- ^ Loaded font atlases
    , fcNextHandle  ∷ Word32                              -- ^ Next available handle ID
    , fcPathCache   ∷ Map.Map (FilePath, Int) FontHandle  -- ^ (path, size) → handle lookup
    } deriving (Show)

-- | Empty font cache
defaultFontCache ∷ FontCache
defaultFontCache = FontCache
    { fcFonts = Map.empty
    , fcNextHandle = 1
    , fcPathCache = Map.empty
    }
