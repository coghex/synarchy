{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Tooltip visual construction: creating and tearing down the
--   'UIPageManager' elements (box, title, separator, hint lines,
--   sprites) that back a shown tooltip, and per-frame repositioning /
--   sprite-frame animation. Dimension math lives in "UI.Tooltip.Layout".
module UI.Tooltip.Render
  ( destroyVisuals
  , rebuildVisuals
  , repositionAndAnimate
  , pickFrame
  ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Vector as V
import Engine.Asset.Handle (TextureHandle(..), toInt)
import Engine.Graphics.Font.Data (FontCache)
import UI.Types
import UI.Manager
import UI.Tooltip.Layout
    ( computeBoxSize, textPixelWidth, measureText, hintLines
    , hintLineHeight, hintLineGap, stackSections, defaultSectionGaps
    , titleLeadingPx, isFontSet, isBoxTextureSet
    )

------------------------------------------------------------
-- Visuals: build, destroy, reposition
------------------------------------------------------------

destroyVisuals ∷ UIPageManager → UIPageManager
destroyVisuals mgr =
    let tts = upmTooltip mgr
        handles = maybeToList (ttsBoxHandle tts)
                ⧺ maybeToList (ttsTextHandle tts)
                ⧺ ttsHintHandles tts
                ⧺ maybeToList (ttsSeparatorHandle tts)
                ⧺ ttsSpriteHandles tts
        mgr1 = foldr deleteElement mgr handles
        tts' = (upmTooltip mgr1)
                 { ttsBoxHandle       = Nothing
                 , ttsTextHandle      = Nothing
                 , ttsHintHandles     = []
                 , ttsSeparatorHandle = Nothing
                 , ttsSpriteHandles   = []
                 }
    in mgr1 { upmTooltip = tts' }

rebuildVisuals ∷ PageHandle → TooltipContent → FontCache → UIPageManager
               → UIPageManager
rebuildVisuals pageH content fontCache mgr0 =
    let mgr1 = destroyVisuals mgr0
        tts = upmTooltip mgr1
        style = ttsStyle tts
        sz = computeBoxSize fontCache style content
        (boxW, boxH) = sz
        -- Box: a geometry element for the tooltip panel is ALWAYS
        -- created, sized to the full content box. When a box-texture set
        -- is configured it renders as the textured backdrop; when not
        -- (the game hasn't called setTooltipStyle{ boxTextures = ... }),
        -- it's an invisible 'RenderNone' element so the text/sprites
        -- still appear to float, but the panel's bounds remain tracked.
        -- Keeping it present even when boxless gives a locked tooltip a
        -- correct click-swallow region (#117).
        (boxHandle, mgr2) =
            let (h, m1) =
                  if isBoxTextureSet (tsBoxTextures style)
                    then createBox "__tooltip_bg" boxW boxH
                             (tsBoxTextures style)
                             (tsBoxTileSize style)
                             (tsBgColor style)
                             0  -- overflow
                             pageH mgr1
                    else createElement "__tooltip_bg" boxW boxH pageH mgr1
                m2 = addElementToPage pageH h 0 0 m1
                m3 = setElementZIndex h 0 m2
            in (Just h, m3)
        -- Title text; created only when both text and a valid font
        -- are present.
        (textHandle, mgr3) = case ttText content of
            Nothing → (Nothing, mgr2)
            Just txt | not (isFontSet (tsFont style)) → (Nothing, mgr2)
                     | otherwise →
                let (h, m1) = createText "__tooltip_text" txt
                                 (tsFont style)
                                 (tsFontSize style)
                                 (tsTextColor style)
                                 pageH mgr2
                    m2 = addElementToPage pageH h 0 0 m1
                    m3 = setElementZIndex h 1 m2
                in (Just h, m3)
        -- Separator between title and hint. Rendered as a thin sprite.
        -- Prefers an explicit `tsSeparatorTexture` (typically a 1×1
        -- white pixel so the colour tint produces the exact requested
        -- colour); falls back to the centre tile of the configured
        -- box-texture set when no separator texture is configured.
        -- Skipped entirely when there's nothing to separate.
        (sepHandle, mgr4) = case (ttText content, ttHint content) of
            (Just _, Just _) | isFontSet (tsFont style) →
                let sepTex = pickSeparatorTexture style mgr3
                in if toInt sepTex ≡ 0
                     then (Nothing, mgr3)
                     else
                       let (h, m1) = createSprite "__tooltip_sep"
                                        1.0  -- placeholder; reposition sets real width
                                        (tsSeparatorThickness style)
                                        sepTex
                                        (tsSeparatorColor style)
                                        pageH mgr3
                           m2 = addElementToPage pageH h 0 0 m1
                           m3 = setElementZIndex h 1 m2
                       in (Just h, m3)
            _ → (Nothing, mgr3)
        -- Hint text beneath the separator. Split on '\n' so multi-line
        -- hints render as a stack of text elements; each line gets its
        -- own ElementHandle and positions independently.
        (hintHandles, mgr5) =
            if not (isFontSet (tsFont style))
              then ([], mgr4)
              else
                let lns = hintLines content
                    step (acc, m) (i, lineTxt) =
                        let (h, m1) = createText
                                ("__tooltip_hint_" <> T.pack (show i))
                                lineTxt
                                (tsFont style)
                                (tsHintFontSize style)
                                (tsHintColor style)
                                pageH m
                            m2 = addElementToPage pageH h 0 0 m1
                            m3 = setElementZIndex h 1 m2
                        in (h : acc, m3)
                    (rev, mFinal) =
                        foldl' step ([], mgr4) (zip [(0 ∷ Int)..] lns)
                in (reverse rev, mFinal)
        -- Sprite elements, one per TooltipSprite. Texture is set to the
        -- first frame; reposition/animate handles cycling animated frames.
        (spriteHandles, mgr6) =
            foldl' (createSpriteElem pageH style)
                   ([], mgr5)
                   (zip [(0 ∷ Int)..] (ttSprites content))
        tts' = (upmTooltip mgr6)
                 { ttsBoxHandle       = boxHandle
                 , ttsTextHandle      = textHandle
                 , ttsHintHandles     = hintHandles
                 , ttsSeparatorHandle = sepHandle
                 , ttsSpriteHandles   = reverse spriteHandles
                 }
    in mgr6 { upmTooltip = tts' }

createSpriteElem ∷ PageHandle → TooltipStyle
                 → ([ElementHandle], UIPageManager)
                 → (Int, TooltipSprite)
                 → ([ElementHandle], UIPageManager)
createSpriteElem pageH _style (acc, mgr) (idx, sprite) =
    let firstFrame =
            if V.null (tsFrames sprite)
              then TextureHandle 0
              else tsFrames sprite V.! 0
        (w, h) = tsSize sprite
        name = T.pack ("__tooltip_sprite_" ⧺ show idx)
        (eh, m1) = createSprite name w h firstFrame
                       (1.0, 1.0, 1.0, 1.0) pageH mgr
        m2 = addElementToPage pageH eh 0 0 m1
        m3 = setElementZIndex eh 1 m2
    in (eh : acc, m3)

repositionAndAnimate ∷ PageHandle → TooltipContent → (Float, Float)
                     → (Float, Float) → FontCache → UIPageManager
                     → UIPageManager
repositionAndAnimate _pageH content (mx, my) (fbW, fbH) fontCache mgr =
    let tts   = upmTooltip mgr
        style = ttsStyle tts
        (boxW, boxH) = computeBoxSize fontCache style content
        ox = tsMouseOffsetX style
        oy = tsMouseOffsetY style
        pad = tsPadding style
        -- Clamp the tooltip into the viewport so it never spills past
        -- the framebuffer edge.
        boxX = clamp 0 (max 0 (fbW - boxW)) (mx + ox)
        boxY = clamp 0 (max 0 (fbH - boxH)) (my + oy)
        -- Box at top-left.
        mgr1 = case ttsBoxHandle tts of
            Nothing → mgr
            Just bh → setElementPosition bh boxX boxY mgr

        -- Section dimensions. Anything with a 0 height is treated as
        -- "missing" and skipped from the stack.
        titleH = case ttText content of
            Just _ | isFontSet (tsFont style) → tsFontSize style + titleLeadingPx
            _ → 0
        hintLineN = length (hintLines content)
        hintH = if hintLineN > 0 ∧ isFontSet (tsFont style)
                  then fromIntegral hintLineN * hintLineHeight style
                       + fromIntegral (max 0 (hintLineN - 1)) * hintLineGap
                  else 0
        hasSep = isJust (ttsSeparatorHandle tts)
        sepH = if hasSep then tsSeparatorThickness style else 0
        spriteRowH = case ttSprites content of
            [] → 0
            xs → maximum (map (snd . tsSize) xs)
        -- Lay out sections from top, inserting per-boundary gaps
        -- between consecutive non-empty sections. Each returned Y is
        -- the top edge of that section within the content stack.
        (titleY0, sepY0, hintY0, spriteY0, stackH) =
            stackSections defaultSectionGaps titleH sepH hintH spriteRowH
        stackTop = boxY + (boxH - stackH) / 2

        -- Title (baseline offset ~0.85 * fontSize past the top accounts
        -- for typical ascender/descender split).
        titleBaseY = stackTop + titleY0 + tsFontSize style * 0.85
        titleW = textPixelWidth fontCache style content
        titleX = boxX + max pad ((boxW - titleW) / 2)
        mgr2 = case ttsTextHandle tts of
            Nothing → mgr1
            Just th → setElementPosition th titleX titleBaseY mgr1

        -- Separator: spans (boxW - 2*pad), positioned by top-left. The
        -- sprite element's size is set here (rebuildVisuals creates it
        -- with a placeholder width since the actual width depends on
        -- the box that the layout settles on this frame).
        sepW = boxW - 2 * pad
        mgr3 = case ttsSeparatorHandle tts of
            Nothing → mgr2
            Just sh →
                let m' = setElementSize sh sepW (tsSeparatorThickness style) mgr2
                in setElementPosition sh (boxX + pad) (stackTop + sepY0) m'

        -- Hint text under the separator. One element per line, each
        -- horizontally centred against ITS OWN width (not the panel's
        -- max-line width) so the block reads as a ragged column rather
        -- than a single fat centred run.
        hintBaseY0 = stackTop + hintY0 + tsHintFontSize style * 0.85
        lineStep   = hintLineHeight style + hintLineGap
        positionHint (m, i) lineTxt h =
            let lineW = measureText fontCache (tsFont style)
                                    (tsHintFontSize style) (Just lineTxt)
                lineX = boxX + max pad ((boxW - lineW) / 2)
                lineY = hintBaseY0 + fromIntegral i * lineStep
                m'    = setElementPosition h lineX lineY m
            in (m', i + 1)
        mgr4 = fst $ foldl
                 (\acc (lineTxt, h) → positionHint acc lineTxt h)
                 (mgr3, 0 ∷ Int)
                 (zip (hintLines content) (ttsHintHandles tts))

        -- Sprite row, horizontally centred as a group.
        spriteRowW = case ttSprites content of
            [] → 0
            xs → sum (map (fst . tsSize) xs)
                 + fromIntegral (length xs - 1) * tsSpriteGap style
        spriteRowX = boxX + max pad ((boxW - spriteRowW) / 2)
        mgr5 = layoutSprites spriteRowX (stackTop + spriteY0)
                             (ttsSpriteHandles tts) (ttSprites content)
                             (tsSpriteGap style) (ttsAnimTimeMs tts) mgr4
    in mgr5

layoutSprites ∷ Float → Float → [ElementHandle] → [TooltipSprite] → Float
              → Float → UIPageManager → UIPageManager
layoutSprites startX rowY handles sprites gap animMs mgr0 =
    go startX (zip handles sprites) mgr0
  where
    go _ [] m = m
    go x ((h, s) : rest) m =
        let (w, _) = tsSize s
            -- Animation: pick a frame from the animMs counter modulo
            -- the loop length. Static sprites already have the right
            -- texture from rebuild so we skip the swap.
            tex = pickFrame animMs s
            m1  = setSpriteTexture h tex m
            m2  = setElementPosition h x rowY m1
        in go (x + w + gap) rest m2

pickFrame ∷ Float → TooltipSprite → TextureHandle
pickFrame animMs sprite =
    let frames = tsFrames sprite
        n = V.length frames
    in if n ≤ 1
         then if n ≡ 0 then TextureHandle 0 else frames V.! 0
         else
           let dur = max 1 (tsFrameDurMs sprite)
               idx = floor (animMs / fromIntegral dur) `mod` n
           in frames V.! idx

-- | Pick the texture used for the separator strip. Explicit
--   'tsSeparatorTexture' wins; otherwise fall back to the centre tile
--   of the box-texture set. Returns 'TextureHandle 0' when neither is
--   available (in which case the separator is skipped).
pickSeparatorTexture ∷ TooltipStyle → UIPageManager → TextureHandle
pickSeparatorTexture style mgr
    | toInt (tsSeparatorTexture style) /= 0 = tsSeparatorTexture style
    | otherwise = case getBoxTextureSet (tsBoxTextures style) mgr of
        Just bts → btsCenter bts
        Nothing  → TextureHandle 0
