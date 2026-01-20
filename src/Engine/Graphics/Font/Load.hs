module Engine.Graphics.Font.Load where

import UPrelude
import Engine.Asset.Types
import Engine.Graphics.Font.Data
import Engine.Graphics.Font.STB
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Error.Exception
import Control.Monad (forM_, when, foldM)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Data.Word (Word8)
import Data.Char (ord)
import Data.Array.IO (IOArray, newArray, writeArray, getElems)

-- | Load a TTF font at specified size
loadFont ∷ FilePath → Int → EngineM ε σ FontHandle
loadFont fontPath fontSize = do
    gs ← gets graphicsState
    let cache = fontCache gs
    case Map.lookup (fontPath, fontSize) (fcPathCache cache) of
        Just handle → do
            logInfo $ "Font already loaded: " ⧺ (show fontPath)
            return handle
        Nothing → do
            logInfo $ "Loading font: " ⧺ (show fontPath)
                    ⧺ " size=" ⧺ (show fontSize)
            
            -- Generate atlas
            atlas ← liftIO $ generateFontAtlas fontPath fontSize
            
            -- Upload to GPU
            texHandle ← uploadFontAtlasToGPU atlas
            
            let newAtlas = atlas { faTexture = texHandle }
                handle = FontHandle (fromIntegral (fcNextHandle cache))
            
            modify $ \s → s 
                { graphicsState = gs { fontCache = cache
                    { fcFonts = Map.insert handle newAtlas (fcFonts cache)
                    , fcNextHandle = fcNextHandle cache + 1
                    , fcPathCache = Map.insert (fontPath, fontSize) handle (fcPathCache cache) } } }
            
            logInfo $ "Font loaded: handle=" ⧺ (show handle)
            return handle

-----------------------------------------------------------
-- Atlas Generation with STB
-----------------------------------------------------------

-- | Generate font atlas from TTF file
generateFontAtlas ∷ FilePath → Int → IO FontAtlas
generateFontAtlas fontPath fontSize = do
    putStrLn $ "Generating font atlas for: " ++ fontPath ++ " size=" ++ show fontSize
    
    -- Load font
    maybeFont ← loadSTBFont fontPath
    case maybeFont of
        Nothing → error $ "Failed to load font: " ++ fontPath
        Just font → do
            -- Get scale for desired pixel height
            scale ← scaleForPixelHeight font (fromIntegral fontSize)
            
            -- Get font metrics
            (ascent, descent, lineGap) ← getSTBFontMetrics font scale
            
            -- ASCII printable characters
            let chars = [' '..'~']
                numChars = length chars
            
            -- Render all glyphs
            glyphData ← mapM (\c → renderGlyphWithMetrics font c scale) chars
            
            -- Calculate atlas size
            let charsPerRow = 16
                maxWidth = maximum $ map (\(w,_,_,_,_) → w) glyphData
                maxHeight = maximum $ map (\(_,h,_,_,_) → h) glyphData
                cellWidth = maxWidth + 2
                cellHeight = maxHeight + 2
                atlasWidth = nextPowerOf2 (charsPerRow * cellWidth)
                numRows = (numChars + charsPerRow - 1) `div` charsPerRow
                atlasHeight = nextPowerOf2 (numRows * cellHeight)
            
            putStrLn $ "Atlas size: " ++ show atlasWidth ++ "x" ++ show atlasHeight
            
            -- Pack glyphs
            (atlasBitmap, glyphMap) ← packGlyphsSTB atlasWidth atlasHeight charsPerRow glyphData chars
            
            -- Free font
            freeSTBFont font
            
            putStrLn $ "Atlas generated:  " ++ show (Map.size glyphMap) ++ " glyphs"
            
            return $ FontAtlas
                { faTexture = TextureHandle 0
                , faGlyphData = glyphMap
                , faAtlasWidth = atlasWidth
                , faAtlasHeight = atlasHeight
                , faFontSize = fontSize
                , faLineHeight = ascent - descent + lineGap
                , faBaseline = ascent
                , faAtlasBitmap = atlasBitmap
                }

-- | Render glyph and get metrics
renderGlyphWithMetrics ∷ STBFont → Char → Float 
                       → IO (Int, Int, Int, Int, [Word8])
renderGlyphWithMetrics font char scale = do
    result ← renderSTBGlyph font char scale
    case result of
        Nothing → return (0, 0, 0, 0, [])
        Just glyph → return glyph

-- | Pack glyphs into atlas
packGlyphsSTB ∷ Int → Int → Int → [(Int, Int, Int, Int, [Word8])] → [Char]
              → IO ([Word8], Map.Map Char GlyphInfo)
packGlyphsSTB atlasWidth atlasHeight charsPerRow glyphData chars = do
    atlasArray ← newArray (0, atlasWidth * atlasHeight - 1) 0 ∷ IO (IOArray Int Word8)
    
    glyphMap ← foldM (packGlyph atlasArray) Map.empty (zip glyphData (zip chars [0..]))
    
    finalBitmap ← getElems atlasArray
    return (finalBitmap, glyphMap)
  where
    packGlyph atlasArray gmap ((w, h, xoff, yoff, pixels), (char, idx)) = do
        let col = idx `mod` charsPerRow
            row = idx `div` charsPerRow
            cellWidth = atlasWidth `div` charsPerRow
            cellHeight = 32
            atlasX = col * cellWidth + 1
            atlasY = row * cellHeight + 1
        
        -- Copy pixels
        forM_ [0.. h-1] $ \y →
            forM_ [0.. w-1] $ \x → do
                let srcIdx = y * w + x
                    dstIdx = (atlasY + y) * atlasWidth + (atlasX + x)
                when (srcIdx < length pixels) $
                    writeArray atlasArray dstIdx (pixels !! srcIdx)
        
        -- Get advance
        (_, _, _, _, advance) ← getSTBGlyphMetrics (error "font freed") char 1.0  -- Placeholder
        
        let u0 = fromIntegral atlasX / fromIntegral atlasWidth
            v0 = fromIntegral atlasY / fromIntegral atlasHeight
            u1 = fromIntegral (atlasX + w) / fromIntegral atlasWidth
            v1 = fromIntegral (atlasY + h) / fromIntegral atlasHeight
            
            glyphInfo = GlyphInfo
                { giUVRect = (u0, v0, u1, v1)
                , giSize = (fromIntegral w, fromIntegral h)
                , giBearing = (fromIntegral xoff, fromIntegral yoff)
                , giAdvance = advance
                }
        
        return $ Map.insert char glyphInfo gmap

-- | Round up to next power of 2
nextPowerOf2 ∷ Int → Int
nextPowerOf2 n = head $ dropWhile (< n) powersOf2
  where powersOf2 = iterate (*2) 1

-----------------------------------------------------------
-- GPU Upload (same as before)
-----------------------------------------------------------

uploadFontAtlasToGPU ∷ FontAtlas → EngineM ε σ TextureHandle
uploadFontAtlasToGPU atlas = do
    -- Same implementation as before
    state ← gets graphicsState
    -- ... (use faAtlasBitmap to upload)
    return (TextureHandle 0)  -- Placeholder

