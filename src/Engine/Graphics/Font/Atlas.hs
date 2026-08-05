module Engine.Graphics.Font.Atlas where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import Data.Array.IO (IOArray, newArray, writeArray, getElems)
import Engine.Asset.Types
import Engine.Asset.Handle
import Engine.Graphics.Font.Data
import Engine.Graphics.Font.Fallback
    (FallbackMark(..), fallbackMark, isIntentionallyEmpty)
import Engine.Graphics.Font.STB
import Engine.Core.Log (logDebug, logWarn, LogCategory(..), LoggerState)
import Control.Monad (foldM)

-- * Baked glyphs

-- | One character's rasterized pixels and metrics, plus whether the font
--   actually draws it. stb resolves an uncovered codepoint to glyph 0
--   and rasterizes that, so coverage has to be carried alongside the
--   pixels rather than inferred from them (#1097).
data BakedGlyph = BakedGlyph
  { bgChar    ∷ Char
  , bgWidth   ∷ Int
  , bgHeight  ∷ Int
  , bgBearingX ∷ Int
  , bgBearingY ∷ Int
  , bgPixels  ∷ [Word8]
  , bgAdvance ∷ Float
  , bgCovered ∷ Bool  -- ^ Font draws this character; false ⇒ fallback mark
  }

-- | Should this character be published in the atlas's glyph map, rather
--   than left to resolve to the fallback mark?
--
--   A character the cmap does not map fails here even though stb handed
--   back .notdef pixels, and so does one whose rasterization came back
--   empty. The deliberately blank characters are published
--   unconditionally: a space carries a real advance and no mark, and
--   that has to stay true whatever the font's coverage is.
glyphIsCovered ∷ Char → Bool → Int → Int → Bool
glyphIsCovered char inCmap w h
    | isIntentionallyEmpty char = True
    | otherwise                 = inCmap ∧ w > 0 ∧ h > 0

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

            glyphs ← forM (zip chars [0..]) $ \(c, idx ∷ Int) → do
                inCmap ← hasSTBCodepoint font c
                (w,h,xoff,yoff,pixels) ← renderGlyphWithMetrics logger font c scale
                (_,_,_,_,advance) ← getSTBGlyphMetrics font c scale
                -- Log metrics for first few glyphs
                when (idx < 3) $
                    logDebug logger CatFont $ "Glyph metrics: char='" <> T.singleton c <> "' "
                        <> "size=" <> T.pack (show w) <> "x" <> T.pack (show h)
                        <> " bearing=(" <> T.pack (show xoff) <> "," <> T.pack (show yoff) <> ")"
                        <> " advance=" <> T.pack (show advance)
                return $ BakedGlyph
                    { bgChar = c, bgWidth = w, bgHeight = h
                    , bgBearingX = xoff, bgBearingY = yoff
                    , bgPixels = pixels, bgAdvance = advance
                    , bgCovered = glyphIsCovered c inCmap w h }

            freeSTBFont font

            let layout = atlasLayout glyphs
                mark = fallbackMark fontSize 0 (alMaxWidth layout) (alMaxHeight layout)

            logDebug logger CatFont $
                "Font atlas size: " <> T.pack (show (alAtlasWidth layout))
                <> "x" <> T.pack (show (alAtlasHeight layout))

            (atlasBitmap, glyphMap, fallbackInfo) ←
                packGlyphsSTBWithMetrics layout mark glyphs

            logDebug logger CatFont $
                "Font atlas generated with " <> T.pack (show $ Map.size glyphMap)
                                             <> " glyphs."

            return $ FontAtlas
                { faTexture = TextureHandle 0
                , faGlyphData = glyphMap
                , faFallbackGlyph = fallbackInfo
                , faAtlasWidth = alAtlasWidth layout
                , faAtlasHeight = alAtlasHeight layout
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
            when (not $ isIntentionallyEmpty char) $
                logWarn logger CatFont $ "Failed to rasterize glyph: '" <> T.singleton char <> "'"
            return (0, 0, 0, 0, [])
        Just glyph → return glyph

-- * Packing

-- | The atlas grid, derived from the glyphs that have to fit in it.
data AtlasLayout = AtlasLayout
  { alCharsPerRow ∷ Int
  , alCellWidth   ∷ Int
  , alCellHeight  ∷ Int
  , alMaxWidth    ∷ Int
  , alMaxHeight   ∷ Int
  , alAtlasWidth  ∷ Int
  , alAtlasHeight ∷ Int
  , alFallbackIdx ∷ Int  -- ^ Cell the fallback mark occupies
  }

-- | Size the grid. The cell dimensions come from the packed glyphs
--   alone, and the row count reserves ONE extra cell for the fallback
--   mark — with the baked range that lands in space the last row already
--   had, so no glyph moves and the atlas does not grow (#1097).
atlasLayout ∷ [BakedGlyph] → AtlasLayout
atlasLayout glyphs = AtlasLayout
    { alCharsPerRow = charsPerRow
    , alCellWidth   = cellWidth
    , alCellHeight  = cellHeight
    , alMaxWidth    = maxWidth
    , alMaxHeight   = maxHeight
    , alAtlasWidth  = nextPowerOf2 (charsPerRow * cellWidth)
    , alAtlasHeight = nextPowerOf2 (numRows * cellHeight)
    , alFallbackIdx = numChars
    }
  where
    charsPerRow = 16
    numChars = length glyphs
    maxWidth = maximum (1 : map bgWidth glyphs)
    maxHeight = maximum (1 : map bgHeight glyphs)
    cellWidth = maxWidth + 2
    cellHeight = maxHeight + 2
    numRows = (numChars + 1 + charsPerRow - 1) `div` charsPerRow

-- | Pack glyphs into atlas bitmap, producing glyph metadata map.
--   Uses metrics stored before font was freed.
--
--   Every glyph is packed, but only the covered ones are published in
--   the map: an uncovered character is meant to resolve to the fallback
--   mark, and leaving its .notdef pixels in place keeps the cell
--   dimensions — and therefore every other glyph's UVs — exactly what
--   they were.
packGlyphsSTBWithMetrics ∷ AtlasLayout → FallbackMark → [BakedGlyph]
                         → IO ([Word8], Map.Map Char GlyphInfo, GlyphInfo)
packGlyphsSTBWithMetrics layout mark glyphs = do
    let atlasWidth = alAtlasWidth layout
        atlasHeight = alAtlasHeight layout
    atlasArray ← newArray (0, atlasWidth * atlasHeight - 1) 0 ∷ IO (IOArray Int Word8)

    glyphMap ← foldM (packGlyph atlasArray) Map.empty (zip glyphs [0..])

    fallbackInfo ← blit atlasArray (alFallbackIdx layout)
                        (fmWidth mark) (fmHeight mark) (fmPixels mark)
                        (fmBearing mark) (fmAdvance mark)

    finalBitmap ← getElems atlasArray
    return (finalBitmap, glyphMap, fallbackInfo)
  where
    atlasWidth = alAtlasWidth layout

    packGlyph atlasArray gmap (glyph, idx) = do
        info ← blit atlasArray idx (bgWidth glyph) (bgHeight glyph)
                    (bgPixels glyph)
                    ( fromIntegral (bgBearingX glyph)
                    , fromIntegral (bgBearingY glyph) )
                    (bgAdvance glyph)
        return $ if bgCovered glyph
                    then Map.insert (bgChar glyph) info gmap
                    else gmap

    blit atlasArray idx w h pixels bearing advance = do
        let col = idx `mod` alCharsPerRow layout
            row = idx `div` alCharsPerRow layout
            atlasX = col * alCellWidth layout + 1
            atlasY = row * alCellHeight layout + 1
            pixelVec = VU.fromList pixels

        forM_ [0..h-1] $ \y →
            forM_ [0..w-1] $ \x → do
                let srcIdx = y * w + x
                    dstIdx = (atlasY + y) * atlasWidth + (atlasX + x)
                when (srcIdx < VU.length pixelVec) $
                    writeArray atlasArray dstIdx (pixelVec VU.! srcIdx)

        let u0 = fromIntegral atlasX / fromIntegral atlasWidth
            v0 = fromIntegral atlasY / fromIntegral (alAtlasHeight layout)
            u1 = fromIntegral (atlasX + w) / fromIntegral atlasWidth
            v1 = fromIntegral (atlasY + h) / fromIntegral (alAtlasHeight layout)

        return $ GlyphInfo
            { giUVRect = (u0, v0, u1, v1)
            , giSize = (fromIntegral w, fromIntegral h)
            , giBearing = bearing
            , giAdvance = advance
            }

nextPowerOf2 ∷ Int → Int
nextPowerOf2 n = fromMaybe 1 $ listToMaybe $ dropWhile (< n) powersOf2
  where powersOf2 = iterate (*2) 1
