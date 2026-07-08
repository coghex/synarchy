module Engine.Graphics.Font.Atlas where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import Data.Array.IO (IOArray, newArray, writeArray, getElems)
import Engine.Asset.Types
import Engine.Asset.Handle
import Engine.Graphics.Font.Data
import Engine.Graphics.Font.STB
import Engine.Core.Log (logDebug, logWarn, LogCategory(..), LoggerState)
import Control.Monad (foldM)

-- * Atlas Generation with STB

generateFontAtlas ∷ LoggerState → FilePath → Int → IO FontAtlas
generateFontAtlas logger fontPath fontSize = do
    logDebug logger CatFont $ "Generating font atlas for: " <> T.pack fontPath
                            <> " size=" <> T.pack (show fontSize)

    maybeFont ← loadSTBFont logger fontPath
    case maybeFont of
        Nothing → error $ "Failed to load font: " ⧺ fontPath
        Just font → do
            scale ← scaleForPixelHeight font (fromIntegral fontSize)
            (ascent, descent, lineGap) ← getSTBFontMetrics font scale

            let chars = [' '..'~']
                numChars = length chars

            glyphDataWithMetrics ← forM (zip chars [0..]) $ \(c, idx) → do
                (w,h,xoff,yoff,pixels) ← renderGlyphWithMetrics logger font c scale
                (_,_,_,_,advance) ← getSTBGlyphMetrics font c scale
                -- Log metrics for first few glyphs
                when (idx < 3) $
                    logDebug logger CatFont $ "Glyph metrics: char='" <> T.singleton c <> "' "
                        <> "size=" <> T.pack (show w) <> "x" <> T.pack (show h)
                        <> " bearing=(" <> T.pack (show xoff) <> "," <> T.pack (show yoff) <> ")"
                        <> " advance=" <> T.pack (show advance)
                return (w, h, xoff, yoff, pixels, advance)

            freeSTBFont font

            let charsPerRow = 16
                maxWidth = maximum $ map (\(w,_,_,_,_,_) → w) glyphDataWithMetrics
                maxHeight = maximum $ map (\(_,h,_,_,_,_) → h) glyphDataWithMetrics
                cellWidth = maxWidth + 2
                cellHeight = maxHeight + 2
                atlasWidth = nextPowerOf2 (charsPerRow * cellWidth)
                numRows = (numChars + charsPerRow - 1) `div` charsPerRow
                atlasHeight = nextPowerOf2 (numRows * cellHeight)

            logDebug logger CatFont $
                "Font atlas size: " <> T.pack (show atlasWidth)
                <> "x" <> T.pack (show atlasHeight)

            (atlasBitmap, glyphMap) ← packGlyphsSTBWithMetrics atlasWidth atlasHeight charsPerRow cellWidth cellHeight glyphDataWithMetrics chars

            logDebug logger CatFont $
                "Font atlas generated with " <> T.pack (show $ Map.size glyphMap)
                                             <> " glyphs."

            return $ FontAtlas
                { faTexture = TextureHandle 0
                , faGlyphData = glyphMap
                , faAtlasWidth = atlasWidth
                , faAtlasHeight = atlasHeight
                , faFontSize = fontSize
                , faLineHeight = ascent - descent + lineGap
                , faBaseline = ascent
                , faAtlasBitmap = atlasBitmap
                , faDescriptorSet = Nothing
                , faImageView = Nothing
                , faSampler = Nothing
                }

renderGlyphWithMetrics ∷ LoggerState → STBFont → Char → Float
                       → IO (Int, Int, Int, Int, [Word8])
renderGlyphWithMetrics logger font char scale = do
    result ← renderSTBGlyph font char scale
    case result of
        Nothing → do
            -- Warn when specific glyphs fail to rasterize
            when (not $ isExpectedEmptyGlyph char) $
                logWarn logger CatFont $ "Failed to rasterize glyph: '" <> T.singleton char <> "'"
            return (0, 0, 0, 0, [])
        Just glyph → return glyph
  where
    isExpectedEmptyGlyph c = c `elem` [' ', '\n', '\t', '\r']

-- | Pack glyphs into atlas bitmap, producing glyph metadata map.
-- Uses metrics stored before font was freed.
packGlyphsSTBWithMetrics ∷ Int → Int → Int → Int → Int
                         → [(Int, Int, Int, Int, [Word8], Float)] → [Char]
                         → IO ([Word8], Map.Map Char GlyphInfo)
packGlyphsSTBWithMetrics atlasWidth atlasHeight charsPerRow cellWidth cellHeight glyphData chars = do
    atlasArray ← newArray (0, atlasWidth * atlasHeight - 1) 0 ∷ IO (IOArray Int Word8)

    glyphMap ← foldM (packGlyph atlasArray) Map.empty (zip glyphData (zip chars [0..]))

    finalBitmap ← getElems atlasArray
    return (finalBitmap, glyphMap)
  where
    packGlyph atlasArray gmap ((w, h, xoff, yoff, pixels, advance), (char, idx)) = do
        let col = idx `mod` charsPerRow
            row = idx `div` charsPerRow
            atlasX = col * cellWidth + 1
            atlasY = row * cellHeight + 1
            pixelVec = VU.fromList pixels

        forM_ [0..h-1] $ \y →
            forM_ [0..w-1] $ \x → do
                let srcIdx = y * w + x
                    dstIdx = (atlasY + y) * atlasWidth + (atlasX + x)
                when (srcIdx < VU.length pixelVec) $
                    writeArray atlasArray dstIdx (pixelVec VU.! srcIdx)

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

nextPowerOf2 ∷ Int → Int
nextPowerOf2 n = fromMaybe 1 $ listToMaybe $ dropWhile (< n) powersOf2
  where powersOf2 = iterate (*2) 1
