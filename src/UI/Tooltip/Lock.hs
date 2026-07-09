{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Tooltip lock API: freezing the active tooltip in place and
--   querying its locked bounds (used to swallow clicks inside a
--   pinned tooltip panel).
module UI.Tooltip.Lock
  ( lockActiveTooltip
  , clearTooltipLock
  , toggleTooltipLock
  , isTooltipLocked
  , isTooltipVisible
  , isPointInLockedTooltip
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import UI.Types
import UI.Tooltip.State (hideTooltip)

-- | True when the tooltip is currently locked (frozen) in place.
isTooltipLocked ∷ UIPageManager → Bool
isTooltipLocked mgr = ttsLocked (upmTooltip mgr)

-- | True when a tooltip is currently being shown (locked or not).
isTooltipVisible ∷ UIPageManager → Bool
isTooltipVisible mgr = isJust (ttsActiveContent (upmTooltip mgr))

-- | Bounds (x, y, w, h) of the locked tooltip's background box.
--   Returns Nothing when the tooltip isn't locked. The box geometry
--   element is always present while a tooltip is shown — even for styles
--   with no box-textures set, where it is an invisible 'RenderNone'
--   element carrying the same (position, size) as a textured box would
--   (see 'UI.Tooltip.Render.rebuildVisuals'). That keeps a locked,
--   boxless tooltip's click-swallow region correct (#117) instead of
--   collapsing to nothing.
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
