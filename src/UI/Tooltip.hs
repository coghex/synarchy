{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Tooltip runtime. Hovers over UI elements whose 'ueTooltip' is set;
--   after a dwell delay, a transient page on 'LayerTooltip' renders the
--   content as a box + text + sprite row that follows the cursor.
--
--   The page is created lazily on the first show and reused across
--   subsequent shows. Visuals are rebuilt only when the active content
--   actually changes; per-frame work while showing is just position
--   updates and (for animated sprites) texture swaps.
module UI.Tooltip
  ( updateTooltipState
  , setTooltipStyle
    -- * Lock control
  , lockActiveTooltip
  , clearTooltipLock
  , toggleTooltipLock
  , isTooltipLocked
  , isTooltipVisible
  , isPointInLockedTooltip
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.IORef (atomicModifyIORef', readIORef)
import Engine.Asset.Handle (FontHandle(..), TextureHandle(..), toInt)
import Engine.Core.Monad
import Engine.Core.State (EngineEnv(..), EngineState(..), TimingState(..))
import Engine.Graphics.Font.Data (FontCache(..), fcFonts)
import Engine.Graphics.Font.Util (calculateTextWidthScaled)
import Engine.Input.Types (InputState(..))
import UI.Types
import UI.Manager

-- | Per-line interline gap below the title-baseline rule. Picked to
--   match the visual rhythm of body-text in a 14px font; recompute if
--   the hint font ever scales.
hintLineGap ∷ Float
hintLineGap = 2

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
hintLineHeight style = tsHintFontSize style + 3

-- | Run one frame of the tooltip subsystem. Called from the render
--   loop after input has been applied and before 'renderUIPages'.
updateTooltipState ∷ EngineM ε σ ()
updateTooltipState = do
    env ← ask
    inp ← liftIO $ readIORef (inputStateRef env)
    fontCache ← liftIO $ readIORef (fontCacheRef env)
    (winW, winH) ← liftIO $ readIORef (windowSizeRef env)
    (fbW, fbH)   ← liftIO $ readIORef (framebufferSizeRef env)
    ts ← gets timingState
    let dtMs = realToFrac (deltaTime ts * 1000.0) ∷ Float
        (mxD, myD) = inpMousePos inp
        -- GLFW reports mouse in window points; UI elements live in
        -- framebuffer pixels. The click thread does the same scaling
        -- (Engine/Input/Thread.hs); without it, hover hit-tests are
        -- offset on HiDPI displays.
        scaleX = if winW > 0
                    then fromIntegral fbW / fromIntegral winW ∷ Float
                    else 1.0
        scaleY = if winH > 0
                    then fromIntegral fbH / fromIntegral winH ∷ Float
                    else 1.0
        mouse = ( realToFrac mxD * scaleX
                , realToFrac myD * scaleY ) ∷ (Float, Float)
        fbSize = (fromIntegral fbW, fromIntegral fbH) ∷ (Float, Float)
    liftIO $ atomicModifyIORef' (uiManagerRef env) $ \mgr →
        (tickTooltip mouse fbSize dtMs fontCache mgr, ())

-- | Replace the active tooltip style. Exposed to Lua via
--   UI.setTooltipStyle so games can plug their own font + box-textures.
setTooltipStyle ∷ TooltipStyle → UIPageManager → UIPageManager
setTooltipStyle style mgr =
    mgr { upmTooltip = (upmTooltip mgr) { ttsStyle = style } }

------------------------------------------------------------
-- Pure tick
------------------------------------------------------------

tickTooltip ∷ (Float, Float) → (Float, Float) → Float → FontCache
            → UIPageManager → UIPageManager
tickTooltip mouse fbSize dtMs fontCache mgr
  | ttsLocked (upmTooltip mgr) = tickLocked dtMs mgr
  | otherwise =
    let tts = upmTooltip mgr
        ignored = maybe Set.empty Set.singleton (ttsActivePage tts)
        -- The hovered element is what the cursor is over, ignoring the
        -- tooltip page itself (otherwise it would hijack its own hover).
        hovered = findElementAtExcept ignored mouse mgr
        hoveredContent = hovered >>= \h → getElementTooltip h mgr
        prevHovered = ttsHoveredElem tts
        -- Reset dwell on hover-target change; otherwise count down by dt.
        dwellRemaining'
          | hovered ≡ prevHovered = max 0 (ttsDwellRemaining tts - dtMs)
          | otherwise             = tsDwellMs (ttsStyle tts)
        shouldShow = case hoveredContent of
            Just _  → dwellRemaining' ≤ 0
            Nothing → False
        -- Hint-stage timer: counts down only while the SAME tooltip
        -- continues to be shown. Resets to 'tsHintDelayMs' whenever the
        -- tooltip isn't showing or the hovered element changed, so a
        -- fresh element always starts at stage 1 (title only) and has
        -- to wait for the full hint delay before the rich form appears.
        sameAsLast = shouldShow ∧ ttsActiveElem tts ≡ hovered
        hintRemaining'
          | sameAsLast = max 0 (ttsHintRemainingMs tts - dtMs)
          | otherwise  = tsHintDelayMs (ttsStyle tts)
        animTime' = ttsAnimTimeMs tts + dtMs
        tts1 = tts { ttsHoveredElem     = hovered
                   , ttsDwellRemaining  = dwellRemaining'
                   , ttsHintRemainingMs = hintRemaining'
                   , ttsAnimTimeMs      = animTime'
                   }
        mgr1 = mgr { upmTooltip = tts1 }
    in if shouldShow
         then case (hovered, hoveredContent) of
                (Just eh, Just content) →
                    let display = stageContent hintRemaining' content
                    in showTooltip eh display mouse fbSize fontCache mgr1
                _ → hideTooltip mgr1
         else hideTooltip mgr1

-- | Stage 1 (still waiting for hint delay) strips the hint, so only
--   the title (+ sprites) renders. Stage 2 (hint timer at 0) returns
--   the full content. Empty / hint-less content is unchanged.
stageContent ∷ Float → TooltipContent → TooltipContent
stageContent hintRemainingMs content
  | hintRemainingMs ≤ 0 = content
  | otherwise           = content { ttHint = Nothing }

-- | Tick for a locked tooltip: position is frozen, hover/dwell are
--   ignored, but animation time still advances and per-sprite frame
--   textures are updated so animated icons keep playing.
tickLocked ∷ Float → UIPageManager → UIPageManager
tickLocked dtMs mgr =
    let tts = upmTooltip mgr
        animTime' = ttsAnimTimeMs tts + dtMs
        tts1 = tts { ttsAnimTimeMs = animTime' }
        mgr1 = mgr { upmTooltip = tts1 }
    in case ttsActiveContent tts of
        Nothing → mgr1  -- locked with nothing shown — defensive no-op
        Just content →
            foldl' (animateSprite animTime')
                   mgr1
                   (zip (ttsSpriteHandles tts) (ttSprites content))
  where
    animateSprite animTime acc (h, s) =
        setSpriteTexture h (pickFrame animTime s) acc

------------------------------------------------------------
-- Show / hide
------------------------------------------------------------

showTooltip ∷ ElementHandle → TooltipContent → (Float, Float)
            → (Float, Float) → FontCache → UIPageManager → UIPageManager
showTooltip elemH content mouse fbSize fontCache mgr =
    let tts = upmTooltip mgr
        sameContent =
            ttsActiveElem tts ≡ Just elemH ∧
            ttsActiveContent tts ≡ Just content
        -- Make sure the transient tooltip page exists + is visible.
        (pageH, mgrP) = ensureTooltipPage mgr
        mgrPV = showPage pageH mgrP
        -- Only rebuild visuals when the content (or owner) changed.
        mgrR = if sameContent
                  then mgrPV
                  else rebuildVisuals pageH content fontCache mgrPV
        -- Reposition + animation frame swap happens every frame so the
        -- tooltip follows the cursor and animated sprites tick.
        mgrA = repositionAndAnimate pageH content mouse fbSize fontCache mgrR
        tts' = (upmTooltip mgrA)
                 { ttsActiveElem    = Just elemH
                 , ttsActiveContent = Just content
                 }
    in mgrA { upmTooltip = tts' }

hideTooltip ∷ UIPageManager → UIPageManager
hideTooltip mgr =
    let tts = upmTooltip mgr
    in case ttsActivePage tts of
        Nothing → mgr
        Just pageH →
            -- Tear down visuals but keep the page handle around for reuse.
            let mgr1 = destroyVisuals mgr
                mgr2 = hidePage pageH mgr1
                tts' = (upmTooltip mgr2)
                         { ttsActiveElem    = Nothing
                         , ttsActiveContent = Nothing
                         }
            in mgr2 { upmTooltip = tts' }

ensureTooltipPage ∷ UIPageManager → (PageHandle, UIPageManager)
ensureTooltipPage mgr =
    case ttsActivePage (upmTooltip mgr) of
        Just ph → (ph, mgr)
        Nothing →
            let (ph, m1) = createPage "__tooltip" LayerTooltip mgr
                tts' = (upmTooltip m1) { ttsActivePage = Just ph }
            in (ph, m1 { upmTooltip = tts' })

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
        -- Box: only created when a box-texture set is configured. An
        -- unset handle (= 0) means the game hasn't called
        -- setTooltipStyle{ boxTextures = ... } yet — render text/sprites
        -- as floating instead of warning every frame.
        (boxHandle, mgr2) =
            if not (isBoxTextureSet (tsBoxTextures style))
              then (Nothing, mgr1)
              else
                let (h, m1) = createBox "__tooltip_bg" boxW boxH
                                 (tsBoxTextures style)
                                 (tsBoxTileSize style)
                                 (tsBgColor style)
                                 0  -- overflow
                                 pageH mgr1
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
            Just _ | isFontSet (tsFont style) → tsFontSize style + 4
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

-- | Stack four optional sections (title, separator, hint, sprite-row)
--   vertically, inserting a per-boundary gap between consecutive
--   non-empty ones. Gaps are @(titleSep, sepHint, hintSprite)@.
--   Empty sections are skipped and don't trigger their adjacent gaps.
--   Returns each section's top-Y within the stack plus the total height.
stackSections ∷ (Float, Float, Float) → Float → Float → Float → Float
              → (Float, Float, Float, Float, Float)
stackSections (gTSep, gSepH, gHSpr) titleH sepH hintH spriteH =
    let advance prevHadContent gapHere y h
          | h ≤ 0 = (y, y, False ∨ prevHadContent)
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

------------------------------------------------------------
-- Layout: dimensions
------------------------------------------------------------

computeBoxSize ∷ FontCache → TooltipStyle → TooltipContent → (Float, Float)
computeBoxSize fontCache style content =
    let titleW = textPixelWidth fontCache style content
        titleH = case ttText content of
            Just _ | isFontSet (tsFont style) → tsFontSize style + 4
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
-- Small helpers
------------------------------------------------------------

isFontSet ∷ FontHandle → Bool
isFontSet h = toInt h /= 0

isBoxTextureSet ∷ BoxTextureHandle → Bool
isBoxTextureSet (BoxTextureHandle n) = n /= 0

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

------------------------------------------------------------
-- Lock control
------------------------------------------------------------

-- | True when the tooltip is currently locked (frozen) in place.
isTooltipLocked ∷ UIPageManager → Bool
isTooltipLocked mgr = ttsLocked (upmTooltip mgr)

-- | True when a tooltip is currently being shown (locked or not).
isTooltipVisible ∷ UIPageManager → Bool
isTooltipVisible mgr = isJust (ttsActiveContent (upmTooltip mgr))

-- | Bounds (x, y, w, h) of the locked tooltip's background box.
--   Returns Nothing when the tooltip isn't locked, or when it's
--   locked without a background box (style with no box-textures set).
lockedTooltipBox ∷ UIPageManager → Maybe (Float, Float, Float, Float)
lockedTooltipBox mgr =
    let tts = upmTooltip mgr
    in if not (ttsLocked tts)
         then Nothing
         else case ttsBoxHandle tts of
                Nothing → Nothing
                Just bh → case Map.lookup bh (upmElements mgr) of
                    Nothing → Nothing
                    Just elem →
                        let (x, y) = uePosition elem
                            (w, h) = ueSize elem
                        in Just (x, y, w, h)

-- | True iff the point lies inside the currently locked tooltip's box.
--   False whenever the tooltip isn't locked (so callers can use it as
--   a guarded "should I treat this click as in-tooltip?").
isPointInLockedTooltip ∷ (Float, Float) → UIPageManager → Bool
isPointInLockedTooltip (px, py) mgr =
    case lockedTooltipBox mgr of
        Nothing → False
        Just (bx, by, bw, bh) →
            px ≥ bx ∧ px ≤ bx + bw ∧ py ≥ by ∧ py ≤ by + bh

-- | Lock the active tooltip in place. No-op if no tooltip is showing.
lockActiveTooltip ∷ UIPageManager → UIPageManager
lockActiveTooltip mgr =
    let tts = upmTooltip mgr
    in if isJust (ttsActiveContent tts)
         then mgr { upmTooltip = tts { ttsLocked = True } }
         else mgr

-- | Unlock and hide. Combines releasing the lock with the standard
--   hide path so the visuals tear down cleanly.
clearTooltipLock ∷ UIPageManager → UIPageManager
clearTooltipLock mgr =
    let tts = upmTooltip mgr
        tts' = tts { ttsLocked = False }
        mgr' = mgr { upmTooltip = tts' }
    in hideTooltip mgr'

-- | Toggle: lock if a tooltip is showing and unlocked, otherwise
--   unlock + hide.
toggleTooltipLock ∷ UIPageManager → UIPageManager
toggleTooltipLock mgr =
    if isTooltipLocked mgr
       then clearTooltipLock mgr
       else lockActiveTooltip mgr
