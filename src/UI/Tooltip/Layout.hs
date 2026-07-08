{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure tooltip layout math: section-gap constants, hint-text line
--   splitting, vertical section stacking, and box-size / text-width
--   measurement. No 'UIPageManager' element mutation lives here — see
--   "UI.Tooltip.Render" for that.
module UI.Tooltip.Layout
  ( -- * Section gaps
    hintLineGap
  , titleLeadingPx
  , hintLeadingPx
    -- * Hint text
  , hintLines
  , hintLineHeight
    -- * Section stacking
  , stackSections
  , defaultSectionGaps
    -- * Box size
  , computeBoxSize
  , textPixelWidth
  , hintPixelWidth
  , measureText
    -- * Style predicates
  , isFontSet
  , isBoxTextureSet
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Engine.Asset.Handle (FontHandle, toInt)
import Engine.Graphics.Font.Data (FontCache(..), fcFonts)
import Engine.Graphics.Font.Util (calculateTextWidthScaled)
import UI.Types

-- | Per-line interline gap below the title-baseline rule. Picked to
--   match the visual rhythm of body-text in a 14px font; recompute if
--   the hint font ever scales.
hintLineGap ∷ Float
hintLineGap = 2

-- | Title-line leading: fudge added past 'tsFontSize' to cover descender
--   overshoot. Shared by the size pass ('computeBoxSize') and the layout
--   pass ('UI.Tooltip.Render.repositionAndAnimate') so they can't drift
--   apart.
titleLeadingPx ∷ Float
titleLeadingPx = 4

-- | Hint-line leading, same purpose as 'titleLeadingPx' but for the
--   (smaller) hint font.
hintLeadingPx ∷ Float
hintLeadingPx = 3

-- | Split the hint on '\n' so multi-line hints render as a stack of
--   text elements instead of a single overflow line. Empty Maybe ⇒
--   no lines.
hintLines ∷ TooltipContent → [Text]
hintLines content = case ttHint content of
    Nothing → []
    Just t  → T.splitOn "\n" t

-- | Per-line height (cap + leading). Used both for layout (computing
--   total hint section height) and for the per-line Y stepping.
hintLineHeight ∷ TooltipStyle → Float
hintLineHeight style = tsHintFontSize style + hintLeadingPx

------------------------------------------------------------
-- Section stacking
------------------------------------------------------------

-- | Stack four optional sections (title, separator, hint, sprite-row)
--   vertically, inserting a per-boundary gap between consecutive
--   non-empty ones. Gaps are @(titleSep, sepHint, hintSprite)@.
--   Empty sections are skipped and don't trigger their adjacent gaps.
--   Returns each section's top-Y within the stack plus the total height.
stackSections ∷ (Float, Float, Float) → Float → Float → Float → Float
              → (Float, Float, Float, Float, Float)
stackSections (gTSep, gSepH, gHSpr) titleH sepH hintH spriteH =
    let advance prevHadContent gapHere y h
          | h ≤ 0 = (y, y, prevHadContent)
          | prevHadContent = (y + gapHere, y + gapHere + h, True)
          | otherwise = (y, y + h, True)
        (titleY, after1, h1) = advance False 0     0      titleH
        (sepY,   after2, h2) = advance h1    gTSep after1 sepH
        (hintY,  after3, h3) = advance h2    gSepH after2 hintH
        (sprY,   after4, _h4) = advance h3   gHSpr after3 spriteH
    in (titleY, sepY, hintY, sprY, after4)

-- | Default per-boundary gaps for the content stack. The sep→hint gap
--   is intentionally larger so the rule reads as a separator between
--   the title block and the hint line, not as a thin element glued
--   to the hint.
defaultSectionGaps ∷ (Float, Float, Float)
defaultSectionGaps = (4, 10, 4)

------------------------------------------------------------
-- Box size computation
------------------------------------------------------------

computeBoxSize ∷ FontCache → TooltipStyle → TooltipContent → (Float, Float)
computeBoxSize fontCache style content =
    let titleW = textPixelWidth fontCache style content
        titleH = case ttText content of
            Just _ | isFontSet (tsFont style) → tsFontSize style + titleLeadingPx
            _ → 0
        hintW = hintPixelWidth fontCache style content
        hintLineN = length (hintLines content)
        hintH = if hintLineN > 0 ∧ isFontSet (tsFont style)
                  then fromIntegral hintLineN * hintLineHeight style
                       + fromIntegral (max 0 (hintLineN - 1)) * hintLineGap
                  else 0
        -- Separator only renders when there's both a title and a hint
        -- AND we have *some* texture to render it with (either a
        -- dedicated 'separatorTexture' or the centre tile of a
        -- configured box-texture set).
        hasSep = isJust (ttText content)
              ∧ isJust (ttHint content)
              ∧ isFontSet (tsFont style)
              ∧ (toInt (tsSeparatorTexture style) /= 0
                   ∨ isBoxTextureSet (tsBoxTextures style))
        sepH = if hasSep then tsSeparatorThickness style else 0
        sprites = ttSprites content
        spriteRowW = case sprites of
            [] → 0
            xs → sum (map (fst . tsSize) xs)
                 + fromIntegral (length xs - 1) * tsSpriteGap style
        spriteRowH = case sprites of
            [] → 0
            xs → maximum (map (snd . tsSize) xs)
        -- Bitmap / arcade fonts often have glyph quads that extend well
        -- past their advance width. The buffer is scaled relative to
        -- fontSize because larger fonts tend to have proportionally
        -- larger glyph-extent overshoot. ~1.5x fontSize covers most
        -- arcade-style fonts I've tested.
        textWBuffer = if isFontSet (tsFont style)
                         ∧ (isJust (ttText content) ∨ isJust (ttHint content))
                        then tsFontSize style * 1.5
                        else 0
        contentW = max (max titleW hintW + textWBuffer) spriteRowW
        (_, _, _, _, contentH) =
            stackSections defaultSectionGaps titleH sepH hintH spriteRowH
        pad = tsPadding style
        rawW = contentW + 2 * pad
        rawH = contentH + 2 * pad
        -- 9-patch corners are tsBoxTileSize px on each side. If the box
        -- is smaller than 2 * tileSize in either axis, opposing corners
        -- overlap and the *visual* box ends up larger than the logical
        -- one — text positioned for the logical box then sits in the
        -- top-left quadrant of the visual box. Enforce a min so visual
        -- and logical dimensions agree.
        minDim = if isBoxTextureSet (tsBoxTextures style)
                    then 2 * tsBoxTileSize style
                    else 0
        cappedW = case ttMaxWidth content of
            Just m | m > 0 → min rawW m
            _ → rawW
    in (max minDim cappedW, max minDim rawH)

textPixelWidth ∷ FontCache → TooltipStyle → TooltipContent → Float
textPixelWidth fontCache style content =
    measureText fontCache (tsFont style) (tsFontSize style) (ttText content)

-- | Hint width = widest line. Multi-line hints would otherwise pack
--   the whole '\n'-joined string into a single measureText call, which
--   the font renderer doesn't split — so we measure each line and take
--   the max.
hintPixelWidth ∷ FontCache → TooltipStyle → TooltipContent → Float
hintPixelWidth fontCache style content =
    case hintLines content of
        [] → 0
        ls → maximum (map (\l →
                measureText fontCache (tsFont style)
                            (tsHintFontSize style) (Just l)) ls)

measureText ∷ FontCache → FontHandle → Float → Maybe Text → Float
measureText _ _ _ Nothing = 0
measureText fontCache fontH size (Just txt) =
    case Map.lookup fontH (fcFonts fontCache) of
        Nothing    → 0  -- font not loaded yet; text won't render either
        Just atlas →
            realToFrac
              (calculateTextWidthScaled atlas size (T.unpack txt))

------------------------------------------------------------
-- Style predicates
------------------------------------------------------------

isFontSet ∷ FontHandle → Bool
isFontSet h = toInt h /= 0

isBoxTextureSet ∷ BoxTextureHandle → Bool
isBoxTextureSet (BoxTextureHandle n) = n /= 0
