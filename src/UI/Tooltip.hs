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
        animTime' = ttsAnimTimeMs tts + dtMs
        tts1 = tts { ttsHoveredElem    = hovered
                   , ttsDwellRemaining = dwellRemaining'
                   , ttsAnimTimeMs     = animTime'
                   }
        mgr1 = mgr { upmTooltip = tts1 }
    in if shouldShow
         then case (hovered, hoveredContent) of
                (Just eh, Just content) →
                    showTooltip eh content mouse fbSize fontCache mgr1
                _ → hideTooltip mgr1
         else hideTooltip mgr1

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
                ⧺ ttsSpriteHandles tts
        mgr1 = foldr deleteElement mgr handles
        tts' = (upmTooltip mgr1)
                 { ttsBoxHandle     = Nothing
                 , ttsTextHandle    = Nothing
                 , ttsSpriteHandles = []
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
        -- Text element above sprites; created only when both text and
        -- a valid font are present.
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
        -- Sprite elements, one per TooltipSprite. Texture is set to the
        -- first frame; reposition/animate handles cycling animated frames.
        (spriteHandles, mgr4) =
            foldl' (createSpriteElem pageH style)
                   ([], mgr3)
                   (zip [(0 ∷ Int)..] (ttSprites content))
        tts' = (upmTooltip mgr4)
                 { ttsBoxHandle     = boxHandle
                 , ttsTextHandle    = textHandle
                 , ttsSpriteHandles = reverse spriteHandles
                 }
    in mgr4 { upmTooltip = tts' }

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
        -- Content layout: stack text on top of sprite row, then center
        -- the whole stack vertically in the box. Single-line text uses
        -- a baseline offset of ~0.25 * fontSize past the line center so
        -- the visible glyph mass sits where the eye expects (typical
        -- ascender / descender split is ~75 / 25).
        textH = case ttText content of
            Just _ | isFontSet (tsFont style) → tsFontSize style + 4
            _ → 0
        spriteRowH = case ttSprites content of
            [] → 0
            xs → maximum (map (snd . tsSize) xs)
        innerGap = if textH > 0 ∧ spriteRowH > 0 then 4 else 0
        stackH = textH + spriteRowH + innerGap
        stackTop = boxY + (boxH - stackH) / 2
        textBaseY = stackTop + tsFontSize style * 0.85
        -- Horizontally center the text inside the box. Centering keeps
        -- padding symmetric whether the box ended up at its natural
        -- width or got bumped to the 9-patch minimum, and degrades
        -- gracefully if the measured text width turns out to be a
        -- slight under-estimate (overflow appears on both sides
        -- equally, not lopsided to the right).
        textW = realToFrac (textPixelWidth fontCache style content) ∷ Float
        textX = boxX + max pad ((boxW - textW) / 2)
        mgr2 = case ttsTextHandle tts of
            Nothing → mgr1
            Just th → setElementPosition th textX textBaseY mgr1
        -- Sprite row, also horizontally centered as a group.
        spriteRowW = case ttSprites content of
            [] → 0
            xs → sum (map (fst . tsSize) xs)
                 + fromIntegral (length xs - 1) * tsSpriteGap style
        spriteRowX = boxX + max pad ((boxW - spriteRowW) / 2)
        spriteRowY = stackTop + textH + innerGap
        mgr3 = layoutSprites spriteRowX spriteRowY
                             (ttsSpriteHandles tts) (ttSprites content)
                             (tsSpriteGap style) (ttsAnimTimeMs tts) mgr2
    in mgr3

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
    let textW = textPixelWidth fontCache style content
        textH = case ttText content of
            Just _ | isFontSet (tsFont style) → tsFontSize style + 4
            _ → 0
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
        textWBuffer = case ttText content of
            Just _ | isFontSet (tsFont style) → tsFontSize style * 1.5
            _ → 0
        contentW = max (textW + textWBuffer) spriteRowW
        contentH = textH + spriteRowH
                 + (if textH > 0 ∧ spriteRowH > 0 then 4 else 0)
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
textPixelWidth fontCache style content = case ttText content of
    Nothing → 0
    Just txt → case Map.lookup (tsFont style) (fcFonts fontCache) of
        Nothing    → 0  -- font not loaded yet; text won't render either
        Just atlas →
            realToFrac
              (calculateTextWidthScaled atlas
                                        (tsFontSize style)
                                        (T.unpack txt))

------------------------------------------------------------
-- Small helpers
------------------------------------------------------------

isFontSet ∷ FontHandle → Bool
isFontSet h = toInt h /= 0

isBoxTextureSet ∷ BoxTextureHandle → Bool
isBoxTextureSet (BoxTextureHandle n) = n /= 0

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
