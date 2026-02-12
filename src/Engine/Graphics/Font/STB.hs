{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Graphics.Font.STB
    ( STBFont
    , STBFontInfo
    , loadSTBFont
    , freeSTBFont
    , renderSTBGlyph
    , getSTBGlyphMetrics
    , getSTBFontMetrics
    , renderSTBGlyphSDF
    , scaleForPixelHeight
    ) where

import UPrelude
import qualified Data.Text as T
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types (CInt(..), CFloat(..))
import Foreign.C.String (CString, withCString)
import Foreign.Marshal.Alloc (alloca, free, mallocBytes)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (Storable(peek, poke))
import Engine.Core.Log (LoggerState, logWarn, LogCategory(CatFont))
import Data.Word (Word8)
import Data.Char (ord)

-----------------------------------------------------------
-- C FFI Declarations
-----------------------------------------------------------

-- Opaque type for stbtt_fontinfo
data STBFontInfo

-- Font data and info
data STBFont = STBFont
    { stbFontData ∷ Ptr Word8      -- Font file data
    , stbFontInfo ∷ Ptr STBFontInfo -- Font info struct
    , stbFontSize ∷ Int             -- Size of font data
    }

foreign import ccall "stb_load_font"
    c_stb_load_font ∷ CString → Ptr CInt → IO (Ptr Word8)

foreign import ccall "stb_init_font"
    c_stb_init_font ∷ Ptr Word8 → Ptr STBFontInfo → IO CInt

foreign import ccall "stb_get_font_vmetrics"
    c_stb_get_font_vmetrics ∷ Ptr STBFontInfo → Ptr CInt → Ptr CInt → Ptr CInt → IO ()

foreign import ccall "stb_get_glyph_metrics"
    c_stb_get_glyph_metrics ∷ Ptr STBFontInfo → CInt → CFloat 
                           → Ptr CInt → Ptr CInt → Ptr CInt → Ptr CInt 
                           → Ptr CFloat → IO ()

foreign import ccall "stb_render_glyph"
    c_stb_render_glyph ∷ Ptr STBFontInfo → CInt → CFloat 
                       → Ptr CInt → Ptr CInt → Ptr CInt → Ptr CInt 
                       → IO (Ptr Word8)

foreign import ccall "stb_render_glyph_sdf"
    c_stb_render_glyph_sdf ∷ Ptr STBFontInfo → CInt → CFloat → CInt
                           → Ptr CInt → Ptr CInt → Ptr CInt → Ptr CInt
                           → IO (Ptr Word8)

foreign import ccall "stb_free_sdf"
    c_stb_free_sdf ∷ Ptr Word8 → IO ()

foreign import ccall "stb_free_bitmap"
    c_stb_free_bitmap ∷ Ptr Word8 → IO ()

foreign import ccall "stb_free_font"
    c_stb_free_font ∷ Ptr Word8 → IO ()

foreign import ccall "stb_scale_for_pixel_height"
    c_stb_scale_for_pixel_height ∷ Ptr STBFontInfo → CFloat → IO CFloat

-----------------------------------------------------------
-- Haskell API
-----------------------------------------------------------

-- | Load a TTF font file
loadSTBFont ∷ LoggerState → FilePath → IO (Maybe STBFont)
loadSTBFont logger path = withCString path $ \cpath → do
    alloca $ \sizePtr → do
        fontData ← c_stb_load_font cpath sizePtr
        if fontData ≡ nullPtr
            then do
                logWarn logger CatFont $ "Failed to load font: " <> T.pack path
                return Nothing
            else do
                size ← peek sizePtr
                
                -- Allocate fontinfo struct (opaque, ~512 bytes is safe)
                fontInfo ← mallocBytes 512 ∷ IO (Ptr STBFontInfo)
                
                -- Initialize font
                result ← c_stb_init_font fontData fontInfo
                if result ≡ 0
                    then do
                        logWarn logger CatFont
                          $ "Failed to initialize font info: " <> T.pack path
                        c_stb_free_font fontData
                        free fontInfo
                        return Nothing
                    else do
                        return $ Just $ STBFont fontData fontInfo (fromIntegral size)

-- | Free font resources
freeSTBFont ∷ STBFont → IO ()
freeSTBFont font = do
    c_stb_free_font (stbFontData font)
    free (stbFontInfo font)

-- | Get scale factor for desired pixel height
scaleForPixelHeight ∷ STBFont → Float → IO Float
scaleForPixelHeight font pixels = do
    scale ← c_stb_scale_for_pixel_height (stbFontInfo font) (realToFrac pixels)
    return (realToFrac scale)

-- | Get font vertical metrics
getSTBFontMetrics ∷ STBFont → Float → IO (Float, Float, Float)
getSTBFontMetrics font scale = do
    alloca $ \ascentPtr →
        alloca $ \descentPtr →
            alloca $ \lineGapPtr → do
                c_stb_get_font_vmetrics (stbFontInfo font) ascentPtr descentPtr lineGapPtr
                ascent ← peek ascentPtr
                descent ← peek descentPtr
                lineGap ← peek lineGapPtr
                return ( fromIntegral ascent * scale
                       , fromIntegral descent * scale
                       , fromIntegral lineGap * scale
                       )

-- | Get glyph metrics
getSTBGlyphMetrics ∷ STBFont → Char → Float → IO (Int, Int, Int, Int, Float)
getSTBGlyphMetrics font char scale = do
    let codepoint = fromIntegral $ ord char
    alloca $ \wPtr →
        alloca $ \hPtr →
            alloca $ \xoffPtr →
                alloca $ \yoffPtr →
                    alloca $ \advPtr → do
                        c_stb_get_glyph_metrics (stbFontInfo font) codepoint (realToFrac scale)
                                               wPtr hPtr xoffPtr yoffPtr advPtr
                        w ← peek wPtr
                        h ← peek hPtr
                        xoff ← peek xoffPtr
                        yoff ← peek yoffPtr
                        adv ← peek advPtr
                        return ( fromIntegral w
                               , fromIntegral h
                               , fromIntegral xoff
                               , fromIntegral yoff
                               , realToFrac adv
                               )

-- | Render a glyph to a bitmap
renderSTBGlyph ∷ STBFont → Char → Float → IO (Maybe (Int, Int, Int, Int, [Word8]))
renderSTBGlyph font char scale = do
    let codepoint = fromIntegral $ ord char
    alloca $ \wPtr →
        alloca $ \hPtr →
            alloca $ \xoffPtr →
                alloca $ \yoffPtr → do
                    bitmap ← c_stb_render_glyph (stbFontInfo font) codepoint (realToFrac scale)
                                               wPtr hPtr xoffPtr yoffPtr
                    if bitmap ≡ nullPtr
                        then return Nothing
                        else do
                            w ← peek wPtr
                            h ← peek hPtr
                            xoff ← peek xoffPtr
                            yoff ← peek yoffPtr
                            
                            let size = fromIntegral w * fromIntegral h
                            pixels ← peekArray size bitmap
                            
                            c_stb_free_bitmap bitmap
                            
                            return $ Just ( fromIntegral w
                                         , fromIntegral h
                                         , fromIntegral xoff
                                         , fromIntegral yoff
                                         , pixels
                                         )


-- | Render a glyph as SDF (Signed Distance Field)
renderSTBGlyphSDF ∷ STBFont → Char → Float → Int → IO (Maybe (Int, Int, Int, Int, [Word8]))
renderSTBGlyphSDF font char scale padding = do
    let codepoint = fromIntegral $ ord char
    alloca $ \wPtr →
        alloca $ \hPtr →
            alloca $ \xoffPtr →
                alloca $ \yoffPtr → do
                    bitmap ← c_stb_render_glyph_sdf (stbFontInfo font) codepoint 
                                                     (realToFrac scale) (fromIntegral padding)
                                                     wPtr hPtr xoffPtr yoffPtr
                    if bitmap ≡ nullPtr
                        then return Nothing
                        else do
                            w ← peek wPtr
                            h ← peek hPtr
                            xoff ← peek xoffPtr
                            yoff ← peek yoffPtr
                            let size = fromIntegral w * fromIntegral h
                            pixels ← peekArray size bitmap
                            c_stb_free_sdf bitmap
                            return $ Just (fromIntegral w, fromIntegral h,
                                          fromIntegral xoff, fromIntegral yoff, pixels)

