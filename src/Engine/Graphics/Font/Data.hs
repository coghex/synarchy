module Engine.Graphics.Font.Data where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Data.Word (Word8, Word32)
import Engine.Asset.Types (TextureHandle, FontHandle, GlyphInfo)
import Engine.Scene.Base (ObjectId)
import Foreign.Storable (peekByteOff, pokeByteOff, Storable(..))
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

-- | batch of text instances
data TextBatch = TextBatch
  { tbFontHandle   ∷ FontHandle
  , tbInstances    ∷ V.Vector GlyphInstance
  } deriving (Eq, Show)

data GlyphInstance = GlyphInstance
    { instancePosition ∷ (Float, Float)
    , instanceSize     ∷ (Float, Float)
    , instanceUVRect   ∷ (Float, Float, Float, Float)
    , instanceColor    ∷ (Float, Float, Float, Float)
    } deriving (Show, Eq)

instance Storable GlyphInstance where
    sizeOf _ = 48  -- 2*4 + 2*4 + 4*4 + 4*4 bytes
    alignment _ = 4
    
    peek ptr = do
        -- Read position (2 floats)
        px ← peekByteOff ptr 0 ∷ IO Float
        py ← peekByteOff ptr 4 ∷ IO Float
        
        -- Read size (2 floats)
        sw ← peekByteOff ptr 8 ∷ IO Float
        sh ← peekByteOff ptr 12 ∷ IO Float
        
        -- Read UV (4 floats)
        u0 ← peekByteOff ptr 16 ∷ IO Float
        v0 ← peekByteOff ptr 20 ∷ IO Float
        u1 ← peekByteOff ptr 24 ∷ IO Float
        v1 ← peekByteOff ptr 28 ∷ IO Float
        
        -- Read color (4 floats)
        r ← peekByteOff ptr 32 ∷ IO Float
        g ← peekByteOff ptr 36 ∷ IO Float
        b ← peekByteOff ptr 40 ∷ IO Float
        a ← peekByteOff ptr 44 ∷ IO Float
        
        return $ GlyphInstance (px, py) (sw, sh) (u0, v0, u1, v1) (r, g, b, a)
    
    poke ptr (GlyphInstance (px, py) (sw, sh) (u0, v0, u1, v1) (r, g, b, a)) = do
        -- Write position
        pokeByteOff ptr 0 (px ∷ Float)
        pokeByteOff ptr 4 (py ∷ Float)
        
        -- Write size
        pokeByteOff ptr 8 (sw ∷ Float)
        pokeByteOff ptr 12 (sh ∷ Float)
        
        -- Write UV
        pokeByteOff ptr 16 (u0 ∷ Float)
        pokeByteOff ptr 20 (v0 ∷ Float)
        pokeByteOff ptr 24 (u1 ∷ Float)
        pokeByteOff ptr 28 (v1 ∷ Float)
        
        -- Write color
        pokeByteOff ptr 32 (r ∷ Float)
        pokeByteOff ptr 36 (g ∷ Float)
        pokeByteOff ptr 40 (b ∷ Float)
        pokeByteOff ptr 44 (a ∷ Float)


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
