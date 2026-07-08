module Engine.Graphics.Font.SDF where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Engine.Asset.Handle
import Engine.Graphics.Font.Atlas (nextPowerOf2, packGlyphsSTBWithMetrics)
import Engine.Graphics.Font.Data
import Engine.Graphics.Font.STB
import Engine.Core.Log (logDebug, logWarn, LogCategory(..), LoggerState)

-- * SDF Atlas Generation

-- | The base size for SDF generation (atlas is generated once at this size)
sdfBaseSize ∷ Int
sdfBaseSize = 48

-- | Padding around each SDF glyph (for distance field spread)
sdfPadding ∷ Int
sdfPadding = 6

-- | Generate an SDF font atlas (scalable to any size)
generateSDFFontAtlas ∷ LoggerState → FilePath → IO FontAtlas
generateSDFFontAtlas logger fontPath = do
    logDebug logger CatFont $ "Generating SDF font atlas for: " <> T.pack fontPath
                            <> " base_size=" <> T.pack (show sdfBaseSize)

    maybeFont ← loadSTBFont logger fontPath
    case maybeFont of
        Nothing → error $ "Failed to load font: " ⧺ fontPath
        Just font → do
            scale ← scaleForPixelHeight font (fromIntegral sdfBaseSize)
            (ascent, descent, lineGap) ← getSTBFontMetrics font scale

            let chars = [' '..'~']
                numChars = length chars

            glyphDataWithMetrics ← forM (zip chars [0..]) $ \(c, idx) → do
                result ← renderSTBGlyphSDF font c scale sdfPadding
                (_, _, _, _, advance) ← getSTBGlyphMetrics font c scale
                case result of
                    Nothing → do
                        when (c `notElem` [' ', '\n', '\t', '\r']) $
                            logWarn logger CatFont $ "Failed to rasterize SDF glyph: '" <> T.singleton c <> "'"
                        return (0, 0, 0, 0, [], advance)
                    Just (w, h, xoff, yoff, pixels) → do
                        when (idx < 3) $
                            logDebug logger CatFont $ "SDF Glyph: char='" <> T.singleton c <> "' "
                                <> "size=" <> T.pack (show w) <> "x" <> T.pack (show h)
                                <> " (includes " <> T.pack (show sdfPadding) <> "px padding)"
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

            logDebug logger CatFont $ "SDF Atlas size: " <> T.pack (show atlasWidth)
                                    <> "x" <> T.pack (show atlasHeight)

            (atlasBitmap, glyphMap) ← packGlyphsSTBWithMetrics atlasWidth atlasHeight
                                         charsPerRow cellWidth cellHeight glyphDataWithMetrics chars

            logDebug logger CatFont $ "SDF Atlas generated with "
                                    <> T.pack (show $ Map.size glyphMap) <> " glyphs"

            return $ FontAtlas
                { faTexture = TextureHandle 0
                , faGlyphData = glyphMap
                , faAtlasWidth = atlasWidth
                , faAtlasHeight = atlasHeight
                , faFontSize = sdfBaseSize
                , faLineHeight = ascent - descent + lineGap
                , faBaseline = ascent
                , faAtlasBitmap = atlasBitmap
                , faDescriptorSet = Nothing
                , faImageView = Nothing
                , faSampler = Nothing
                }
