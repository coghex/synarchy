{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Shared viewport-aware floating-placement contract (#747, Phase C
--   child C1 of #741) — one framebuffer-coordinate placement algorithm
--   for every trigger-anchored popup (dropdown option lists, context
--   menu roots/submenus). Pre-#747, dropdown.lua opened its option
--   list unconditionally below the display box with no framebuffer
--   check, and context_menu.lua clamped its own position independently
--   via @engine.getFramebufferSize()@ — two divergent
--   implementations for the same underlying problem. Tooltip placement
--   ("UI.Tooltip.Layout" / "UI.Tooltip.Render") deliberately stays
--   separate (its own cursor-relative clamp), per the issue's "existing
--   tooltip placement may remain separate but cannot regress".
--
--   This module is pure — no Vulkan, window, or Lua engine involved —
--   so placement is Hspec-testable in isolation, mirroring
--   "UI.InputOwnership"/"UI.Clipping"; see @Test.Headless.UI.PopupPlacement@.
module UI.PopupPlacement
  ( PopupDirection(..)
  , PlacementRequest(..)
  , Placement(..)
  , placePopup
  , fitVisibleRows
  ) where

import UPrelude

-- | Which side of the anchor the popup prefers to open on.
--   'PopupAnchored' has no directional preference at all — it's placed
--   exactly at the anchor point (anchor size ignored) and only the
--   final two-axis clamp applies; this is the context-menu ROOT's
--   behavior (opens wherever the user clicked, clamped on-screen, no
--   flip).
data PopupDirection
    = PopupBelow
    | PopupAbove
    | PopupRight
    | PopupLeft
    | PopupAnchored
    deriving (Eq, Show)

-- | One placement request: the triggering control's own on-screen
--   rect (a zero-size point for 'PopupAnchored'), the FULL interactive
--   content size (including any scrollbar strip — "accounts for full
--   interactive size including scrollbars"), the preferred opening
--   direction, and the framebuffer to clamp within.
data PlacementRequest = PlacementRequest
    { prAnchorX      ∷ Float
    , prAnchorY      ∷ Float
    , prAnchorW      ∷ Float
    , prAnchorH      ∷ Float
    , prContentW     ∷ Float
    , prContentH     ∷ Float
    , prPreferred    ∷ PopupDirection
    , prFramebufferW ∷ Float
    , prFramebufferH ∷ Float
    } deriving (Eq, Show)

-- | The resolved top-left position, plus whether the OPPOSITE-direction
--   fallback was used instead of the preferred one (e.g. a dropdown
--   that flipped above because there wasn't room below).
data Placement = Placement
    { plX       ∷ Float
    , plY       ∷ Float
    , plFlipped ∷ Bool
    } deriving (Eq, Show)

clamp1 ∷ Float → Float → Float → Float
clamp1 lo hi v
    | hi < lo   = lo
    | otherwise = max lo (min hi v)

-- | Resolve one placement: preferred direction, opposite-direction
--   fallback when the preferred side doesn't fit, then a final
--   two-axis clamp to the framebuffer regardless of which direction
--   was used — so a popup can never end up fully or partially
--   off-screen even in a degenerate (framebuffer smaller than content)
--   case.
--
--   The fallback only ever tries the OPPOSITE side of the preferred
--   axis (below↔above, right↔left) — this is what backs "dropdowns
--   prefer below, flip above" and "submenus flip horizontally" while
--   leaving the other axis to the final clamp alone (a submenu's
--   vertical position is never flipped, only clamped — "clamp
--   vertically").
placePopup ∷ PlacementRequest → Placement
placePopup req =
    let fbW = prFramebufferW req
        fbH = prFramebufferH req
        cw  = prContentW req
        ch  = prContentH req
        ax  = prAnchorX req
        ay  = prAnchorY req
        aw  = prAnchorW req
        ah  = prAnchorH req

        fitsV y = y ≥ 0 ∧ y + ch ≤ fbH
        fitsH x = x ≥ 0 ∧ x + cw ≤ fbW

        (rawX, rawY, flipped) = case prPreferred req of
            PopupAnchored → (ax, ay, False)
            PopupBelow →
                let below = ay + ah
                in if fitsV below then (ax, below, False)
                   else (ax, ay - ch, True)
            PopupAbove →
                let above = ay - ch
                in if fitsV above then (ax, above, False)
                   else (ax, ay + ah, True)
            PopupRight →
                let right = ax + aw
                in if fitsH right then (right, ay, False)
                   else (ax - cw, ay, True)
            PopupLeft →
                let left = ax - cw
                in if fitsH left then (left, ay, False)
                   else (ax + aw, ay, True)

        clampedX = clamp1 0 (fbW - cw) rawX
        clampedY = clamp1 0 (fbH - ch) rawY
    in Placement clampedX clampedY flipped

-- | How many rows actually fit vertically given how many would
--   ideally show, the height of one row, and the vertical space
--   available for popup content — backs "dropdowns ... reduce visible
--   rows when necessary" so an oversized option list still ends up
--   fully on-screen rather than overflowing the framebuffer. Never
--   returns fewer than 1 (a degenerate zero/negative available height
--   still shows a single row rather than vanishing — the final
--   two-axis clamp in 'placePopup' handles keeping it on-screen).
fitVisibleRows ∷ Int → Float → Float → Int
fitVisibleRows preferredCount rowHeight availableHeight
    | preferredCount ≤ 0 = 0
    | rowHeight ≤ 0 = preferredCount
    | otherwise =
        let fits = floor (availableHeight / rowHeight)
        in max 1 (min preferredCount fits)
