{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Opt-in rectangular descendant clipping (#747, Phase C child C1 of
--   #741). A container opts in via 'UI.Types.ueClipChildren'; the
--   EFFECTIVE clip a given element is subject to is the intersection
--   of every clipping ANCESTOR's own current absolute bounds — nested
--   clips intersect, and an ancestor never clips itself, only its
--   descendants (overflow:hidden semantics). 'Nothing' means
--   unclipped.
--
--   This module is pure — no Vulkan, window, or Lua engine involved —
--   so the clip geometry is Hspec-testable in isolation, mirroring
--   "UI.InputOwnership"; see @Test.Headless.UI.Clipping@.
--
--   Single source of truth: both the render path ("UI.Render") and the
--   hit-test walk ('UI.Manager.Query.isPointInElement') consult
--   'effectiveClip' (via 'pointInClip' for hit-testing and
--   'clipQuadUV' for rendering) so paint and hit-test can never drift
--   apart — the same discipline 'UI.Types.uiLayerBand' already
--   enforces for z-order.
--
--   Deliberately does NOT depend on "UI.Manager.Query": that module
--   needs to depend on THIS one (to share the effective-clip check
--   with hit-testing), so this module recomputes absolute position
--   locally from "UI.Types" alone to avoid a import cycle.
module UI.Clipping
  ( ClipRect
  , hasArea
  , intersectRect
  , effectiveClip
  , pointInClip
  , clipQuadUV
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import UI.Types

-- | An axis-aligned rectangle: (x, y, width, height) in the same
--   framebuffer-pixel space as 'uePosition'/'ueSize'.
type ClipRect = (Float, Float, Float, Float)

-- | True iff the rect has strictly positive width and height — an
--   empty (or degenerate/negative) intersection renders and hits
--   nothing, per the #747 contract.
hasArea ∷ ClipRect → Bool
hasArea (_, _, w, h) = w > 0 ∧ h > 0

-- | Intersect two rects. May return a rect with zero/negative width or
--   height when they don't overlap at all — callers test 'hasArea'
--   before treating the result as a visible region.
intersectRect ∷ ClipRect → ClipRect → ClipRect
intersectRect (x1, y1, w1, h1) (x2, y2, w2, h2) =
    let left   = max x1 x2
        top    = max y1 y2
        right  = min (x1 + w1) (x2 + w2)
        bottom = min (y1 + h1) (y2 + h2)
    in (left, top, right - left, bottom - top)

-- | This element's absolute position, computed locally (mirrors
--   'UI.Manager.Query.getElementAbsolutePosition' exactly) to avoid a
--   'UI.Manager.Query' → 'UI.Clipping' → 'UI.Manager.Query' import
--   cycle now that hit-testing consults this module.
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

-- | The effective clip in force for the given element, derived from
--   its ANCESTORS only (an element's own 'ueClipChildren' restricts
--   its descendants, never itself). Nested clipping ancestors
--   intersect; 'Nothing' when no ancestor clips (unclipped, matching
--   pre-#747 behavior). Depth-capped like every other ancestor walk in
--   "UI.Manager.Query" as belt-and-suspenders against parent cycles
--   (which 'UI.Manager.Hierarchy.addChildElement' refuses to create).
effectiveClip ∷ ElementHandle → UIPageManager → Maybe ClipRect
effectiveClip handle mgr = case Map.lookup handle (upmElements mgr) of
    Nothing → Nothing
    Just el → goAncestors (0 ∷ Int) (ueParent el)
  where
    goAncestors depth mParent
        | depth ≥ 64 = Nothing
        | otherwise = case mParent of
            Nothing → Nothing
            Just parentH → case Map.lookup parentH (upmElements mgr) of
                Nothing → Nothing
                Just parent →
                    let rest = goAncestors (depth + 1) (ueParent parent)
                    in if not (ueClipChildren parent) then rest
                       else
                           let (px, py) = absolutePosition parentH mgr
                               (pw, ph) = ueSize parent
                               ownRect  = (px, py, pw, ph)
                           in case rest of
                               Nothing → Just ownRect
                               Just r  → Just (intersectRect ownRect r)

-- | True iff the point lies within the clip (or there is no clip at
--   all). Shared by every hit-test entry point via
--   'UI.Manager.Query.isPointInElement'.
pointInClip ∷ Maybe ClipRect → (Float, Float) → Bool
pointInClip Nothing _ = True
pointInClip (Just (x, y, w, h)) (px, py) =
    w > 0 ∧ h > 0 ∧ px ≥ x ∧ px ≤ x + w ∧ py ≥ y ∧ py ≤ y + h

-- | Clip an axis-aligned textured quad (screen rect + its full UV
--   rect) against an optional clip, returning the intersected screen
--   rect together with the UV sub-rect that keeps the texture mapping
--   correct for the visible portion — so a partially-clipped sprite,
--   box tile, or glyph shows only its visible slice rather than being
--   squashed or stretched. 'Nothing' clip passes the quad through
--   unchanged; 'Nothing' result means the quad and the clip don't
--   overlap at all (nothing to draw — matches "empty intersections
--   render nothing").
clipQuadUV ∷ Maybe ClipRect → ClipRect → (Float, Float, Float, Float)
           → Maybe (ClipRect, (Float, Float, Float, Float))
clipQuadUV Nothing screenRect uv = Just (screenRect, uv)
clipQuadUV (Just clip) screenRect@(sx, sy, sw, sh) (u0, v0, u1, v1)
    | sw ≤ 0 ∨ sh ≤ 0 = Nothing
    | otherwise =
        let clipped@(cx, cy, cw, ch) = intersectRect clip screenRect
        in if not (hasArea clipped) then Nothing
           else
               let fracLeft   = (cx - sx) / sw
                   fracTop    = (cy - sy) / sh
                   fracRight  = (cx + cw - sx) / sw
                   fracBottom = (cy + ch - sy) / sh
                   u0' = u0 + fracLeft   * (u1 - u0)
                   u1' = u0 + fracRight  * (u1 - u0)
                   v0' = v0 + fracTop    * (v1 - v0)
                   v1' = v0 + fracBottom * (v1 - v0)
               in Just (clipped, (u0', v0', u1', v1'))
