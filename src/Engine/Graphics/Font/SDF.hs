module Engine.Graphics.Font.SDF where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Engine.Asset.Handle
import Engine.Graphics.Font.Atlas
    ( AtlasLayout(..), BakedGlyph(..), atlasLayout, glyphIsCovered
    , packGlyphsSTBWithMetrics )
import Engine.Graphics.Font.Data
import Engine.Graphics.Font.Fallback (fallbackMark, isIntentionallyEmpty)
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

            glyphs ← forM (zip chars [0..]) $ \(c, idx ∷ Int) → do
                inCmap ← hasSTBCodepoint font c
                result ← renderSTBGlyphSDF font c scale sdfPadding
                (_, _, _, _, advance) ← getSTBGlyphMetrics font c scale
                (w, h, xoff, yoff, pixels) ← case result of
                    Nothing → do
                        when (not $ isIntentionallyEmpty c) $
                            logWarn logger CatFont $ "Failed to rasterize SDF glyph: '" <> T.singleton c <> "'"
                        return (0, 0, 0, 0, [])
                    Just glyph@(w, h, _, _, _) → do
                        when (idx < 3) $
                            logDebug logger CatFont $ "SDF Glyph: char='" <> T.singleton c <> "' "
                                <> "size=" <> T.pack (show w) <> "x" <> T.pack (show h)
                                <> " (includes " <> T.pack (show sdfPadding) <> "px padding)"
                        return glyph
                return $ BakedGlyph
                    { bgChar = c, bgWidth = w, bgHeight = h
                    , bgBearingX = xoff, bgBearingY = yoff
                    , bgPixels = pixels, bgAdvance = advance
                    , bgCovered = glyphIsCovered c inCmap w h }

            freeSTBFont font

            let layout = atlasLayout glyphs
                -- The mark carries the same distance-field ramp the
                -- glyphs around it do, so it thresholds the same way.
                mark = fallbackMark sdfBaseSize sdfPadding
                                    (alMaxWidth layout) (alMaxHeight layout)

            logDebug logger CatFont $ "SDF Atlas size: " <> T.pack (show (alAtlasWidth layout))
                                    <> "x" <> T.pack (show (alAtlasHeight layout))

            (atlasBitmap, glyphMap, fallbackInfo) ←
                packGlyphsSTBWithMetrics layout mark glyphs

            logDebug logger CatFont $ "SDF Atlas generated with "
                                    <> T.pack (show $ Map.size glyphMap) <> " glyphs"

            return $ FontAtlas
                { faTexture = TextureHandle 0
                , faGlyphData = glyphMap
                , faFallbackGlyph = fallbackInfo
                , faAtlasWidth = alAtlasWidth layout
                , faAtlasHeight = alAtlasHeight layout
                , faFontSize = sdfBaseSize
                , faLineHeight = ascent - descent + lineGap
                , faBaseline = ascent
                , faAtlasBitmap = atlasBitmap
                , faDescriptorSet = Nothing
                , faImageView = Nothing
                , faSampler = Nothing
                }
