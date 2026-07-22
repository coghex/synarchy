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
  , BoxTile(..)
  , boxTileRects
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
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

-- | Which of the nine 3x3 box tiles a rect covers — used to look up
--   its matching texture in a 'UI.Types.BoxTextureSet'.
data BoxTile
    = TileNW | TileN | TileNE
    | TileW  | TileCenter | TileE
    | TileSW | TileS | TileSE
    deriving (Eq, Show)

-- | Compute the nine 3x3 box tile rects for a box at the given
--   position/size/tile-size, each already clipped against an optional
--   ancestor clip (via 'clipQuadUV') and paired with its UV sub-rect.
--   A tile fully outside the clip is omitted entirely — this is the
--   actual per-tile geometry "UI.Render.makeBoxBatches" draws, pulled
--   out as a pure function (no 'Engine.Graphics.Vulkan.Texture.Types.
--   BindlessTextureSystem'/'Engine.Core.Monad.EngineM' involved) so
--   the real box-clipping wiring — not just the underlying
--   'clipQuadUV' helper in isolation — is directly Hspec-testable; see
--   @Test.Headless.UI.Clipping@.
boxTileRects ∷ Float → Float → Float → Float → Float → Maybe ClipRect
             → [(BoxTile, ClipRect, (Float, Float, Float, Float))]
boxTileRects x y w h tileSize clip =
    let ts = tileSize
        -- #749: fold the box's OWN visual rect into the effective clip,
        -- so a box smaller than 2*tileSize (e.g. a valid-negative
        -- overflow shrinking the visual rect below the corner size)
        -- doesn't tile its FIXED-size corner quads PAST its own visual/
        -- interactive bounds — render and interactive geometry would
        -- otherwise disagree. A no-op for a normal box (its tiles
        -- already exactly fill (x,y,w,h)); composes with any ancestor
        -- clip via intersection.
        selfClip = case clip of
            Nothing → (x, y, w, h)
            Just c  → intersectRect (x, y, w, h) c
        effClip  = Just selfClip
        -- Mirrors UI.Render.makeBoxBatches exactly: tiles overlap from
        -- the origin when the box is smaller than 2*tileSize.
        midW = max 0 (w - ts * 2)
        midH = max 0 (h - ts * 2)

        nwX = x;          nwY = y
        nX  = x + ts;     nY  = y
        neX = x + ts + midW; neY = y

        wX  = x;          wY  = y + ts
        cX  = x + ts;     cY  = y + ts
        eX  = x + ts + midW; eY  = y + ts

        swX = x;          swY = y + ts + midH
        sX  = x + ts;     sY  = y + ts + midH
        seX = x + ts + midW; seY = y + ts + midH

        tiles =
            [ (TileNW,     nwX, nwY, ts,   ts)
            , (TileN,      nX,  nY,  midW, ts)
            , (TileNE,     neX, neY, ts,   ts)
            , (TileW,      wX,  wY,  ts,   midH)
            , (TileCenter, cX,  cY,  midW, midH)
            , (TileE,      eX,  eY,  ts,   midH)
            , (TileSW,     swX, swY, ts,   ts)
            , (TileS,      sX,  sY,  midW, ts)
            , (TileSE,     seX, seY, ts,   ts)
            ]

        clipOne (tile, tx, ty, tw, th) = case clipQuadUV effClip (tx, ty, tw, th) (0, 0, 1, 1) of
            Nothing → Nothing
            Just (rect, uv) → Just (tile, rect, uv)
    in mapMaybe clipOne tiles
