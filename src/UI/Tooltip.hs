{-# LANGUAGE UnicodeSyntax #-}
-- | Tooltip runtime. Hovers over UI elements whose 'ueTooltip' is set;
--   after a dwell delay, a transient page on 'LayerTooltip' renders the
--   content as a box + text + sprite row that follows the cursor.
--
--   The page is created lazily on the first show and reused across
--   subsequent shows. Visuals are rebuilt only when the active content
--   actually changes; per-frame work while showing is just position
--   updates and (for animated sprites) texture swaps.
--
--   Split (issue #572) into focused submodules under "UI.Tooltip.*":
--
--     * "UI.Tooltip.Layout" — pure dimension math: section gaps, hint
--       line splitting, section stacking, box-size / text-width
--       measurement.
--     * "UI.Tooltip.Render" — visual element construction, teardown,
--       and per-frame reposition / sprite-frame animation.
--     * "UI.Tooltip.State" — the dwell/hover state machine and the
--       engine-facing per-frame tick.
--     * "UI.Tooltip.Lock" — the lock API (freeze in place, click-swallow
--       bounds).
--
--   This module re-exports the public API unchanged.
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
    -- * Exposed for testing
  , rebuildVisuals
  ) where

import UI.Tooltip.State (updateTooltipState, setTooltipStyle)
import UI.Tooltip.Render (rebuildVisuals)
import UI.Tooltip.Lock
    ( lockActiveTooltip
    , clearTooltipLock
    , toggleTooltipLock
    , isTooltipLocked
    , isTooltipVisible
    , isPointInLockedTooltip
    )
