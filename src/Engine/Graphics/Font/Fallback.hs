-- | What the text renderer does with a character the font cannot draw
--   (#1097).
--
--   Before this module a missing character produced no quad, no advance
--   and no log line: measurement and both layout passes agreed on
--   nothing, so the text was silently wrong. The three of them now share
--   'resolveGlyph', which substitutes a synthesized mark — so an
--   unavailable character is visible as a problem and still consumes the
--   same width everywhere it is measured or drawn.
--
--   The mark is generated from atlas geometry ('fallbackMark') rather
--   than taken from the font, because a font narrow enough to be missing
--   the character may equally be missing whatever glyph a font-sourced
--   fallback would rely on.
module Engine.Graphics.Font.Fallback
  ( -- * Glyph resolution
    resolveGlyph
  , isMissingGlyph
  , isIntentionallyEmpty
  , missingGlyphs
    -- * Once-per-pair diagnostics
  , takeUnreportedMissingGlyphs
  , missingGlyphMessage
    -- * The synthesized mark
  , FallbackMark(..)
  , fallbackMark
  , fallbackOnEdge
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Char (ord, toUpper)
import Data.IORef (IORef, atomicModifyIORef')
import Numeric (showHex)
import Engine.Asset.Handle (FontHandle)
import Engine.Asset.Types (GlyphInfo(..))
import Engine.Graphics.Font.Data (FontAtlas(..), FontCache(..))

-- * Glyph resolution

-- | Characters that draw nothing on purpose. They keep whatever the
--   atlas says about them — a space carries a real advance, tab\/CR\/LF
--   are outside the baked range and carry none — and must never pick up
--   the fallback mark.
isIntentionallyEmpty ∷ Char → Bool
isIntentionallyEmpty c = c ≡ ' ' ∨ c ≡ '\t' ∨ c ≡ '\r' ∨ c ≡ '\n'

-- | The one lookup measurement and both layout passes go through, so
--   they cannot disagree about a missing character's advance. 'Nothing'
--   now means only "draws nothing on purpose"; an unavailable character
--   resolves to 'faFallbackGlyph'.
resolveGlyph ∷ FontAtlas → Char → Maybe GlyphInfo
resolveGlyph atlas char
    | isIntentionallyEmpty char = Map.lookup char (faGlyphData atlas)
    | otherwise = Just $ fromMaybe (faFallbackGlyph atlas)
                                   (Map.lookup char (faGlyphData atlas))

-- | Would this character render as the fallback mark? Atlas generation
--   omits every character the font does not actually draw, so absence
--   from 'faGlyphData' is the whole test.
isMissingGlyph ∷ FontAtlas → Char → Bool
isMissingGlyph atlas char =
    not (isIntentionallyEmpty char)
      ∧ not (Map.member char (faGlyphData atlas))

-- | The distinct unavailable characters in a string, in first-occurrence
--   order.
missingGlyphs ∷ FontAtlas → Text → [Char]
missingGlyphs atlas = dedupe Set.empty . filter (isMissingGlyph atlas) . T.unpack
  where
    dedupe _ [] = []
    dedupe seen (c:cs)
        | Set.member c seen = dedupe seen cs
        | otherwise         = c : dedupe (Set.insert c seen) cs

-- * Once-per-pair diagnostics

-- | Claim the @(font, codepoint)@ pairs in this string that have not
--   been reported yet, returning the ones the caller should now log.
--
--   Text layout runs every frame, so the report has to be deduplicated
--   or it floods. The claim is a single 'atomicModifyIORef'' over the
--   shared font cache: a pair is handed to exactly one caller even when
--   the Lua measurement thread and the render thread hit it at once, and
--   a different handle or a different codepoint is its own pair.
takeUnreportedMissingGlyphs ∷ IORef FontCache → FontHandle → FontAtlas
                            → Text → IO [Char]
takeUnreportedMissingGlyphs cacheRef fontH atlas text =
    case missingGlyphs atlas text of
        -- Fully covered text is the common case; keep it off the IORef.
        []      → return []
        missing → atomicModifyIORef' cacheRef $ \cache →
            let seen  = fcMissingReported cache
                fresh = filter (\c → not (Set.member (fontH, c) seen)) missing
                seen' = foldl' (\s c → Set.insert (fontH, c) s) seen fresh
            in (cache { fcMissingReported = seen' }, fresh)

-- | The diagnostic text for one claimed pair, naming both identities.
missingGlyphMessage ∷ FontHandle → Char → Text
missingGlyphMessage fontH char =
    "Missing glyph, drawing fallback mark: font=" <> T.pack (show fontH)
      <> " codepoint=U+" <> codepointHex char

codepointHex ∷ Char → Text
codepointHex c =
    let digits = map toUpper (showHex (ord c) "")
    in T.pack (replicate (4 - length digits) '0' ⧺ digits)

-- * The synthesized mark

-- | stb's SDF on-edge value, mirrored from @stb_render_glyph_sdf@ in
--   @cbits/font_stb.c@. Both atlas kinds are sampled by the same
--   fragment shader, which thresholds at 0.7 — this value — so the mark
--   has to use the encoding the glyphs beside it use.
fallbackOnEdge ∷ Double
fallbackOnEdge = 180

-- | A rasterized missing-glyph mark: a hollow box, sized and placed like
--   an ordinary glyph so the packer and the layout passes need no
--   special case for it.
data FallbackMark = FallbackMark
  { fmWidth   ∷ Int             -- ^ Cell width in px, ramp margin included
  , fmHeight  ∷ Int             -- ^ Cell height in px, ramp margin included
  , fmBearing ∷ (Float, Float)  -- ^ Offset from the baseline, stb convention
  , fmAdvance ∷ Float           -- ^ Horizontal advance
  , fmPixels  ∷ [Word8]         -- ^ Row-major, @fmWidth * fmHeight@ samples
  } deriving (Show, Eq)

-- | Build the mark for one atlas.
--
--   @fallbackMark baseSize margin maxCellW maxCellH@ — @margin@ is the
--   distance-field ramp width (the SDF atlas's padding; 0 for the bitmap
--   atlas), and the two maxima are the largest glyph the atlas already
--   packs. The mark is clamped to those maxima rather than allowed to
--   grow the cell, so adding it cannot repack an existing glyph or move
--   a single UV.
fallbackMark ∷ Int → Int → Int → Int → FallbackMark
fallbackMark baseSize margin maxCellW maxCellH = FallbackMark
    { fmWidth   = w
    , fmHeight  = h
    , fmBearing = ( fromIntegral (sideBearing - m)
                  , fromIntegral (negate (boxH + m)) )
    , fmAdvance = fromIntegral (boxW + 2 * sideBearing)
    , fmPixels  = pixels
    }
  where
    -- Roughly half an em wide by two thirds tall: the proportions a
    -- .notdef box conventionally has.
    em f = max 1 (round (fromIntegral baseSize * (f ∷ Double)))
    w = clamp 1 (max 1 maxCellW) (em 0.46 + 2 * margin)
    h = clamp 1 (max 1 maxCellH) (em 0.68 + 2 * margin)
    -- Keep at least one pixel of box after the ramp margin is taken off
    -- both sides, however small the clamp above made the cell.
    m = clamp 0 margin (min ((w - 1) `div` 2) ((h - 1) `div` 2))
    boxW = w - 2 * m
    boxH = h - 2 * m
    stroke = fromIntegral (max 1 (min boxW boxH `div` 7)) ∷ Double
    sideBearing = max 1 (boxW `div` 8)
    distScale | m > 0     = fallbackOnEdge / fromIntegral m
              | otherwise = 255
    x0 = fromIntegral m ∷ Double
    y0 = fromIntegral m ∷ Double
    x1 = fromIntegral (m + boxW) ∷ Double
    y1 = fromIntegral (m + boxH) ∷ Double
    pixels = [ encode (frameDistance (fromIntegral x + 0.5) (fromIntegral y + 0.5))
             | y ← [0 .. h - 1], x ← [0 .. w - 1] ]
    -- Signed distance to the box OUTLINE: positive inside the stroke,
    -- negative both in the hole and outside the box. @inside@ is the
    -- distance into the filled box, so the stroke is the band
    -- @0 ≤ inside ≤ stroke@.
    frameDistance px py =
        let inside = minimum [px - x0, x1 - px, py - y0, y1 - py]
        in min inside (stroke - inside)
    encode d = fromIntegral
        (clamp 0 255 (round (fallbackOnEdge + d * distScale) ∷ Int))
