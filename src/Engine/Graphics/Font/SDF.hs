-- | SDF atlas generation — the shipped UI font path.
--
--   @engine.loadFont@ discards the size it is given and lands here, so
--   this is where a font's repertoire is decided in practice. Since
--   #1098 that repertoire is a parameter rather than a hardcoded
--   @[' '..'~']@, it is intersected with the font's own cmap before
--   anything is rasterized, and the grid it lands on is chosen for the
--   smallest R8 payload instead of a fixed 16 columns.
module Engine.Graphics.Font.SDF
  ( -- * Generation
    sdfBaseSize
  , sdfPadding
  , generateSDFFontAtlas
    -- * Failure
  , SDFAtlasError(..)
  , sdfAtlasErrorMessage
    -- * Coverage
  , CoverageReport(..)
  , coverageReport
  , coverageReportMessage
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Engine.Asset.Handle
import Engine.Graphics.Font.Atlas
    ( AtlasLayout(..), BakedGlyph(..), atlasCellSize, glyphIsCovered
    , packGlyphsSTBWithMetrics, planAtlasLayout )
import Engine.Graphics.Font.Data
import Engine.Graphics.Font.Fallback (codepointHex, fallbackMark)
import Engine.Graphics.Font.Repertoire (Repertoire, repertoireChars, repertoireSize)
import Engine.Graphics.Font.STB
import Engine.Core.Log (logDebug, logInfo, LogCategory(..), LoggerState)
import Control.Monad (filterM)

-- * SDF Atlas Generation

-- | The base size for SDF generation (atlas is generated once at this size)
sdfBaseSize ∷ Int
sdfBaseSize = 48

-- | Padding around each SDF glyph (for distance field spread)
sdfPadding ∷ Int
sdfPadding = 6

-- * Failure

-- | Why an atlas could not be generated. Every case names the font
--   path, because the caller that has to report it is several layers
--   away from the one that chose the file.
data SDFAtlasError
  = SDFFontLoadFailed FilePath
    -- | Path, requested count — the font's cmap supplies none of them.
  | SDFNoSuppliedGlyphs FilePath Int
    -- | Path — the requested repertoire was empty to begin with.
  | SDFEmptyRepertoire FilePath
    -- | Path, supplied glyphs, cell width, cell height, device limit.
  | SDFNoFeasibleLayout FilePath Int Int Int Int
  deriving (Eq, Show)

sdfAtlasErrorMessage ∷ SDFAtlasError → Text
sdfAtlasErrorMessage err = case err of
    SDFFontLoadFailed path →
        prefix path <> "failed to load the font file"
    SDFEmptyRepertoire path →
        prefix path <> "the requested repertoire is empty"
    SDFNoSuppliedGlyphs path requested →
        prefix path <> "none of the " <> tshow requested
            <> " requested characters are in the font's cmap"
    SDFNoFeasibleLayout path supplied cellW cellH limit →
        prefix path <> "no uniform grid for " <> tshow supplied
            <> " glyphs in " <> tshow cellW <> "x" <> tshow cellH
            <> " cells fits maxImageDimension2D=" <> tshow limit
  where
    prefix path = "SDF atlas for " <> T.pack path <> ": "

tshow ∷ Show α ⇒ α → Text
tshow = T.pack . show

-- * Coverage

-- | What one generated atlas ended up covering, reported once per
--   generation — never per rasterization, and never again on a cache
--   hit, since a cache hit generates nothing.
data CoverageReport = CoverageReport
  { crFontPath  ∷ FilePath
  , crRequested ∷ Int
  , crSupplied  ∷ Int
  , crMissing   ∷ [Char]  -- ^ Requested but absent, ascending
  } deriving (Eq, Show)

-- | Ordering is inherited: the requested repertoire is canonical, so
--   filtering it preserves ascending codepoint order.
coverageReport ∷ FilePath → Repertoire → [Char] → CoverageReport
coverageReport path requested supplied = CoverageReport
    { crFontPath  = path
    , crRequested = repertoireSize requested
    , crSupplied  = length supplied
    , crMissing   = filter (\c → not (Set.member c suppliedSet))
                           (repertoireChars requested)
    }
  where suppliedSet = Set.fromList supplied

coverageReportMessage ∷ CoverageReport → Text
coverageReportMessage report =
    "Font atlas coverage: path=" <> T.pack (crFontPath report)
        <> " requested=" <> tshow (crRequested report)
        <> " supplied=" <> tshow (crSupplied report)
        <> " missing=" <> tshow (length (crMissing report))
        <> " [" <> T.intercalate " " (map label (crMissing report)) <> "]"
  where label c = "U+" <> codepointHex c

-- * Generation

-- | Generate an SDF font atlas (scalable to any size) for exactly the
--   characters @repertoire@ requests that the font genuinely supplies.
--
--   @maxDimension@ is the device's @maxImageDimension2D@; it is a
--   parameter rather than a constant so the production path can pass
--   the real limit and a test can drive a synthetic one.
generateSDFFontAtlas ∷ LoggerState → FilePath → Repertoire → Int
                     → IO (Either SDFAtlasError FontAtlas)
generateSDFFontAtlas logger fontPath repertoire maxDimension = do
    logDebug logger CatFont $ "Generating SDF font atlas for: " <> T.pack fontPath
                            <> " base_size=" <> tshow sdfBaseSize
                            <> " requested=" <> tshow (repertoireSize repertoire)

    let requested = repertoireChars repertoire
    if null requested
        then return $ Left $ SDFEmptyRepertoire fontPath
        else do
            maybeFont ← loadSTBFont logger fontPath
            case maybeFont of
                Nothing → return $ Left $ SDFFontLoadFailed fontPath
                Just font → withFont font requested
  where
    withFont font requested = do
        scale ← scaleForPixelHeight font (fromIntegral sdfBaseSize)
        (ascent, descent, lineGap) ← getSTBFontMetrics font scale

        -- The cmap decides membership, not rasterization: a supplied
        -- SPACE legitimately produces no bitmap, while an unsupplied
        -- character rasterizes .notdef perfectly happily.
        supplied ← filterM (hasSTBCodepoint font) requested

        logInfo logger CatFont $ coverageReportMessage $
            coverageReport fontPath repertoire supplied

        case supplied of
            [] → do
                freeSTBFont font
                return $ Left $ SDFNoSuppliedGlyphs fontPath (length requested)
            _ → do
                glyphs ← forM supplied (bakeGlyph font scale)
                freeSTBFont font
                let (cellWidth, cellHeight) = atlasCellSize glyphs
                case planAtlasLayout glyphs maxDimension of
                    Nothing → return $ Left $ SDFNoFeasibleLayout
                        fontPath (length glyphs) cellWidth cellHeight maxDimension
                    Just layout → Right ⊚ buildAtlas layout glyphs
                                                     ascent descent lineGap

    bakeGlyph font scale c = do
        result ← renderSTBGlyphSDF font c scale sdfPadding
        (_, _, _, _, advance) ← getSTBGlyphMetrics font c scale
        -- A NULL bitmap is the normal outcome for a supplied glyph with
        -- no outline, so it is not reported: the request was already
        -- intersected with the cmap, and there is nothing left for a
        -- per-glyph failure warning to mean.
        let (w, h, xoff, yoff, pixels) = fromMaybe (0, 0, 0, 0, []) result
        return $ BakedGlyph
            { bgChar = c, bgWidth = w, bgHeight = h
            , bgBearingX = xoff, bgBearingY = yoff
            , bgPixels = pixels, bgAdvance = advance
            -- Cmap membership is settled above; what remains is whether
            -- the glyph drew anything, which still decides publication.
            , bgCovered = glyphIsCovered c True w h }

    buildAtlas layout glyphs ascent descent lineGap = do
        -- The mark carries the same distance-field ramp the
        -- glyphs around it do, so it thresholds the same way.
        let mark = fallbackMark sdfBaseSize sdfPadding
                                (alMaxWidth layout) (alMaxHeight layout)

        logDebug logger CatFont $ "SDF Atlas size: " <> tshow (alAtlasWidth layout)
                                <> "x" <> tshow (alAtlasHeight layout)
                                <> " columns=" <> tshow (alCharsPerRow layout)

        (atlasBitmap, glyphMap, fallbackInfo) ←
            packGlyphsSTBWithMetrics layout mark glyphs

        logDebug logger CatFont $ "SDF Atlas generated with "
                                <> tshow (Map.size glyphMap) <> " glyphs"

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
