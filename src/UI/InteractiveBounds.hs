{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Logical / visual / interactive bounds for a 'UI.Types.UIElement'
--   (#749, Phase C child C3 of #741, on top of element policy #743,
--   release activation #745, and clipping #747).
--
--   A box's 'UI.Types.ubsOverflow' expands what it RENDERS on every
--   side without changing its stored layout ('uePosition'/'ueSize'),
--   which is exactly what draws a visible border of decorative bleed
--   that the pre-#749 hit-test could never reach. C3 names the three
--   bounds this splits apart:
--
--     * LOGICAL / content bounds — 'uePosition' + 'ueSize'. What layout
--       and every pre-#749 hit-test used, unchanged.
--     * VISUAL / render bounds — content expanded by the (clamped)
--       overflow on every side. The exact rect "UI.Render" draws the
--       box within.
--     * INTERACTIVE bounds — what pointer / hover / tooltip / scroll /
--       release hit-testing actually uses. Content bounds by DEFAULT
--       (existing boxes stay content-only); the expanded VISUAL bounds
--       only when the element opts its visible border in via
--       'UI.Types.ueInteractiveOverflow'. Overflow alone never enlarges
--       the target — a decorative box keeps bleeding without becoming a
--       blocker.
--
--   This module is pure — no Vulkan, window, or Lua engine — so the
--   geometry is Hspec-testable in isolation, mirroring "UI.Clipping"
--   and "UI.InputOwnership"; see @Test.Headless.UI.InteractiveBounds@.
--   It is the ONE shared source both the render path ("UI.Render", via
--   'elementOverflow') and every hit-test entry point
--   ('UI.Manager.Query.isPointInElement', via 'interactiveRect')
--   consult, so visual and interactive geometry can never drift apart —
--   the same discipline 'UI.Types.uiLayerBand' enforces for z-order and
--   'UI.Clipping.effectiveClip' for clipping.
--
--   Deliberately does NOT depend on "UI.Manager.Query": that module
--   depends on THIS one (to share the interactive-bounds rect with
--   hit-testing), so this module recomputes absolute position locally
--   from "UI.Types" alone (mirroring "UI.Clipping") to avoid an import
--   cycle.
module UI.InteractiveBounds
  ( clampOverflow
  , elementRawOverflow
  , elementOverflow
  , contentRect
  , visualRect
  , interactiveRect
  , effectiveInteractiveBounds
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import UI.Types
import UI.Clipping (ClipRect, effectiveClip, intersectRect, hasArea)

-- | Clamp an overflow value so expanding a @(w, h)@ content rect by it
--   on every side can never invert geometry: the visual extents
--   @w + 2*ovf@ and @h + 2*ovf@ stay @≥ 0@. Invalid overflow (@≤@ minus
--   half the SMALLER content extent, which would invert the box) clamps
--   UP to exactly that limit — a zero-extent, non-inverted degenerate
--   rect that 'UI.Clipping.hasArea' already treats as hitting/rendering
--   nothing, never a negative-size rect. A validly-negative overflow (a
--   genuine shrink that keeps positive area) passes through unchanged,
--   so a box opted into expanded-visual interaction with a negative
--   overflow shrinks its interactive bounds below content bounds, in
--   lockstep with what it renders. Deterministic and pure — the #749
--   "invalid overflow cannot create inverted/unbounded geometry" guard.
clampOverflow ∷ (Float, Float) → Float → Float
clampOverflow (w, h) ovf = max ovf lowerLimit
  where lowerLimit = negate (min (max 0 w) (max 0 h) / 2)

-- | The raw box overflow of an element — 'ubsOverflow' for a
--   'RenderBox', @0@ for anything else (text, sprite, plain container).
--   A non-box element never expands, so its visual and content bounds
--   always coincide regardless of any interaction-policy flag.
elementRawOverflow ∷ UIElement → Float
elementRawOverflow el = case ueRenderData el of
    RenderBox style → ubsOverflow style
    _               → 0

-- | The element's effective overflow: its raw overflow clamped against
--   its CURRENT size (via 'clampOverflow'), recomputed live so a resize
--   takes effect on the very next query with nothing cached to go
--   stale. Both "UI.Render" (visual expansion) and 'visualRect'/
--   'interactiveRect' (hit-testing) read through here, so an
--   inverted-geometry overflow is guarded identically wherever it's
--   used.
elementOverflow ∷ UIElement → Float
elementOverflow el = clampOverflow (ueSize el) (elementRawOverflow el)

-- | Logical / layout / content bounds: the given absolute position plus
--   'ueSize'. Exactly what layout and the pre-#749 hit-test used.
contentRect ∷ (Float, Float) → UIElement → ClipRect
contentRect (ax, ay) el = let (w, h) = ueSize el in (ax, ay, w, h)

-- | Visual / render bounds: content expanded by the (clamped)
--   'elementOverflow' on every side — the exact rect "UI.Render" draws
--   the box within. For a non-box element or a zero overflow this
--   equals 'contentRect'.
visualRect ∷ (Float, Float) → UIElement → ClipRect
visualRect abs' el =
    let (cx, cy, cw, ch) = contentRect abs' el
        ovf              = elementOverflow el
    in (cx - ovf, cy - ovf, cw + ovf * 2, ch + ovf * 2)

-- | Interactive bounds: 'contentRect' by default, or the expanded
--   'visualRect' when the element opts its visible border into
--   interaction ('UI.Types.ueInteractiveOverflow'). This is the single
--   rect every hit-test entry point resolves membership against (via
--   'UI.Manager.Query.isPointInElement'), so #743 click/scroll routing,
--   hover/tooltips, and #745 press/release all agree by construction.
interactiveRect ∷ (Float, Float) → UIElement → ClipRect
interactiveRect abs' el
    | ueInteractiveOverflow el = visualRect abs' el
    | otherwise                = contentRect abs' el

-- | This element's absolute position, computed locally (mirrors
--   'UI.Manager.Query.getElementAbsolutePosition' and
--   'UI.Clipping.absolutePosition' exactly) to avoid a
--   'UI.Manager.Query' → 'UI.InteractiveBounds' → 'UI.Manager.Query'
--   import cycle. Depth-capped as belt-and-suspenders against parent
--   cycles.
absolutePosition ∷ ElementHandle → UIPageManager → (Float, Float)
absolutePosition handle mgr = go (0 ∷ Int) handle
  where
    go depth h = case Map.lookup h (upmElements mgr) of
        Nothing → (0, 0)
        Just el →
            let (ex, ey) = uePosition el
                (px, py) = case ueParent el of
                    _ | depth ≥ 64 → (0, 0)
                    Nothing → (0, 0)
                    Just parentH → go (depth + 1) parentH
            in (px + ex, py + ey)

-- | The EFFECTIVE interactive bounds actually used for hit-testing: the
--   element's 'interactiveRect' (content or expanded-visual per its
--   opt-in) intersected with every #747 ancestor clip
--   ('UI.Clipping.effectiveClip'). 'Nothing' when the handle is
--   unknown, or the interactive rect is entirely clipped away (no
--   positive-area intersection) — clipped overflow neither renders nor
--   interacts, so such an element can never be hit. Exposed to Lua as
--   @interactiveBounds@ on @UI.getElementInfo@ so introspection
--   (@ui.dumpWidgets@) and the playtest oracle join clicks against the
--   same rect a real hit resolves, not the raw content bounds.
effectiveInteractiveBounds ∷ ElementHandle → UIPageManager → Maybe ClipRect
effectiveInteractiveBounds handle mgr = do
    el ← Map.lookup handle (upmElements mgr)
    let rect = interactiveRect (absolutePosition handle mgr) el
    case effectiveClip handle mgr of
        Nothing   → Just rect
        Just clip →
            let region = intersectRect rect clip
            in if hasArea region then Just region else Nothing
