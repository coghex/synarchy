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
import Data.List (minimumBy)
import Data.Ord (comparing)

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
                            <> " size=" <> tshow fontSize

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
                        <> "size=" <> tshow w <> "x" <> tshow h
                        <> " bearing=(" <> tshow xoff <> "," <> tshow yoff <> ")"
                        <> " advance=" <> tshow advance
                return $ BakedGlyph
                    { bgChar = c, bgWidth = w, bgHeight = h
                    , bgBearingX = xoff, bgBearingY = yoff
                    , bgPixels = pixels, bgAdvance = advance
                    , bgCovered = glyphIsCovered c inCmap w h }

            freeSTBFont font

            let layout = atlasLayout glyphs
                mark = fallbackMark fontSize 0 (alMaxWidth layout) (alMaxHeight layout)

            logDebug logger CatFont $
                "Font atlas size: " <> tshow (alAtlasWidth layout)
                <> "x" <> tshow (alAtlasHeight layout)

            (atlasBitmap, glyphMap, fallbackInfo) ←
                packGlyphsSTBWithMetrics layout mark glyphs

            logDebug logger CatFont $
                "Font atlas generated with " <> tshow (Map.size glyphMap)
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

-- | The largest glyph in the set, floored at 1 so an all-blank set
--   still describes a real extent.
atlasMaxGlyph ∷ [BakedGlyph] → (Int, Int)
atlasMaxGlyph glyphs =
    ( maximum (1 : map bgWidth glyphs)
    , maximum (1 : map bgHeight glyphs) )

-- | The uniform cell those glyphs force: the largest of them plus one
--   pixel of gutter on each side, so no glyph bleeds into a
--   neighbour's UV rect.
atlasCellSize ∷ [BakedGlyph] → (Int, Int)
atlasCellSize glyphs =
    let (maxWidth, maxHeight) = atlasMaxGlyph glyphs
    in (maxWidth + 2, maxHeight + 2)

-- | Size the grid at a fixed 16 columns. The cell dimensions come from
--   the packed glyphs alone, and the row count reserves ONE extra cell
--   for the fallback mark — with the baked range that lands in space
--   the last row already had, so no glyph moves and the atlas does not
--   grow (#1097).
--
--   Used by 'generateFontAtlas' only. The SDF path picks its column
--   count with 'planAtlasLayout' instead (#1098).
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
    (maxWidth, maxHeight) = atlasMaxGlyph glyphs
    (cellWidth, cellHeight) = atlasCellSize glyphs
    numRows = (numChars + 1 + charsPerRow - 1) `div` charsPerRow

-- * Deterministic grid selection (#1098)

-- | One candidate uniform grid.
data AtlasPlan = AtlasPlan
  { apColumns     ∷ Int
  , apRows        ∷ Int
  , apCellWidth   ∷ Int
  , apCellHeight  ∷ Int
  , apAtlasWidth  ∷ Int
  , apAtlasHeight ∷ Int
  } deriving (Eq, Show)

-- | What the plan costs in texture memory. The atlas is uploaded as
--   @FORMAT_R8_UNORM@, one byte per texel, so area IS the payload.
atlasPayloadBytes ∷ AtlasPlan → Int
atlasPayloadBytes plan = apAtlasWidth plan * apAtlasHeight plan

-- | Choose the column count for @glyphCount@ glyphs of the given cell
--   size, subject to a device @maxImageDimension2D@.
--
--   Every column count from 1 through @glyphCount@ is evaluated, the
--   ones whose power-of-two dimensions exceed the device limit are
--   discarded, and the cheapest R8 payload wins. Ties go to the smaller
--   longest side, then to the smaller column count — and since the
--   candidates are indexed BY column count that last step is always
--   decisive, so the choice is a total order over a fixed candidate
--   list and cannot depend on evaluation order.
--
--   The grid holds one cell more than there are glyphs: the fallback
--   mark occupies a cell of its own (#1097).
--
--   'Nothing' means either an empty glyph set or no feasible candidate;
--   the caller distinguishes them, since it is the one that knows which
--   font and which repertoire produced the set.
planAtlasGrid ∷ Int → Int → Int → Int → Maybe AtlasPlan
planAtlasGrid glyphCount cellWidth cellHeight maxDimension
    | glyphCount ≤ 0 = Nothing
    | otherwise      = case feasible of
        []    → Nothing
        plans → Just (minimumBy (comparing rank) plans)
  where
    cells = glyphCount + 1
    candidate columns = AtlasPlan
        { apColumns     = columns
        , apRows        = rows
        , apCellWidth   = cellWidth
        , apCellHeight  = cellHeight
        , apAtlasWidth  = nextPowerOf2 (columns * cellWidth)
        , apAtlasHeight = nextPowerOf2 (rows * cellHeight)
        }
      where rows = (cells + columns - 1) `div` columns
    feasible =
        [ plan
        | columns ← [1 .. glyphCount]
        , let plan = candidate columns
        , apAtlasWidth plan ≤ maxDimension
        , apAtlasHeight plan ≤ maxDimension
        ]
    rank plan = ( atlasPayloadBytes plan
                , max (apAtlasWidth plan) (apAtlasHeight plan)
                , apColumns plan )

-- | 'planAtlasGrid' applied to a real glyph set, as an 'AtlasLayout'
--   'packGlyphsSTBWithMetrics' can consume.
--
--   The cell size is derived from the SUPPLIED glyphs — the ones the
--   font actually draws — because a character the font has no glyph for
--   has no extent to measure.
planAtlasLayout ∷ [BakedGlyph] → Int → Maybe AtlasLayout
planAtlasLayout glyphs maxDimension
    | null glyphs = Nothing
    | otherwise   = toLayout ⊚ planAtlasGrid (length glyphs)
                                             cellWidth cellHeight maxDimension
  where
    (maxWidth, maxHeight) = atlasMaxGlyph glyphs
    (cellWidth, cellHeight) = atlasCellSize glyphs
    toLayout plan = AtlasLayout
        { alCharsPerRow = apColumns plan
        , alCellWidth   = apCellWidth plan
        , alCellHeight  = apCellHeight plan
        , alMaxWidth    = maxWidth
        , alMaxHeight   = maxHeight
        , alAtlasWidth  = apAtlasWidth plan
        , alAtlasHeight = apAtlasHeight plan
        , alFallbackIdx = length glyphs
        }

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
