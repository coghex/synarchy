{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Tooltip dwell/hover state machine and the engine-facing per-frame
--   tick. Owns the show/hide transitions (which drive
--   "UI.Tooltip.Render"'s visual build/teardown) but not the visuals
--   themselves.
module UI.Tooltip.State
  ( updateTooltipState
  , setTooltipStyle
  , showTooltip
  , hideTooltip
  ) where

import UPrelude
import qualified Data.Set as Set
import Data.IORef (atomicModifyIORef', readIORef)
import Engine.Core.Monad
import Engine.Core.State (EngineEnv(..), EngineState(..), TimingState(..))
import Engine.Graphics.Font.Data (FontCache)
import Engine.Input.Types (InputState(..))
import UI.Types
import UI.Manager
import UI.Tooltip.Render
    (rebuildVisuals, destroyVisuals, repositionAndAnimate, pickFrame)

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
