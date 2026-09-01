{-# LANGUAGE Strict #-}
-- | The ONE bounded-drag helper structure planning uses (#1844).
--
--   Preview and commit used to plan a structure drag by two different
--   rules and could therefore disagree about what the player drew:
--
--     * The preview clamped OUTWARD from the anchor to 64 cells, so the
--       anchor was always in the result. Commit went through the generic
--       128-cell 'World.Thread.Command.Cursor.Common.designateRect',
--       which clamps from the LOW-coordinate end — so a long drag toward
--       lower coordinates could clamp the anchor itself out of the
--       committed rectangle. The two also disagreed on reach.
--     * The wire path tool picked its dominant axis from the CLAMPED
--       delta in the preview and from the RAW localized delta in
--       @scripts\/build_tool.lua@, so near the clamp boundary a diagonal
--       drag could preview along one axis and commit along the other.
--
--   Both callers now go through 'structureDragExtent', so neither
--   disagreement is expressible. Mine, Till and Chop are untouched and
--   keep their 128-cell 'designateRect' behaviour — this helper is
--   structure-specific by design, not a replacement for that one.
--
--   The rules, in the order they are applied:
--
--     1. The endpoint is re-expressed in the ANCHOR's local alias frame
--        (#1175's 'localizeTileToAnchor'). Everything after this is
--        arithmetic in that frame; identity away from the cylindrical
--        seam and in arenas.
--     2. A WIRE path picks its dominant axis from that RAW localized
--        delta, BEFORE any clamping. Ties (@|dx| == |dy|@) go to x, which
--        is what both previous implementations did.
--     3. Each dragged axis is clamped OUTWARD from the anchor to at most
--        'maxStructureDragSide' cells IN TOTAL, the anchor included —
--        i.e. the anchor through @anchor ± 63@. This is not 64 cells on
--        each side, and the anchor is therefore always present.
module World.Construct.Extent
    ( maxStructureDragSide
    , clampStructureSide
    , structureDragBounds
    , structureDragExtent
    ) where

import UPrelude
import World.Generate.Coordinates (localizeTileToAnchor)

-- | Cells a structure drag may span along ONE axis, the anchor
--   included. Matches the shipped preview clamp
--   (@World.Render.CursorQuads.maxMinePreviewSide@, which structure
--   planning no longer borrows) and design decision D-26.
--
--   D-26's prose says "at most 64 tiles inclusive on either side", which
--   can be read as @anchor ± 64@ (129 cells). The tighter reading is the
--   correct one: it is what the shipped clamp arithmetic does, and it is
--   what issue #1844 requirement 1 states. Do not "align" this to the
--   looser wording.
maxStructureDragSide ∷ Int
maxStructureDragSide = 64

-- | Clamp one axis endpoint outward from the anchor. Always returns a
--   value on the same side of the anchor as its input, so the anchor is
--   never clamped out of the span.
clampStructureSide ∷ Int    -- ^ anchor coordinate on this axis
                   → Int    -- ^ endpoint coordinate, anchor-local frame
                   → Int
clampStructureSide a b
    | b ≥ a     = min b (a + maxStructureDragSide - 1)
    | otherwise = max b (a - maxStructureDragSide + 1)

-- | The drag's inclusive bounds in the ANCHOR's local alias frame:
--   @((xLo, yLo), (xHi, yHi))@.
--
--   Corners stay LOCAL, exactly as 'designateRect' leaves its own:
--   canonicalisation is per enumerated tile, at lookup and storage only.
--   That is also the frame a preview quad's screen position is computed
--   in.
structureDragBounds
    ∷ Int          -- ^ world size in chunks (0 / arena ⇒ no wrapping)
    → Bool         -- ^ line mode: a 1-wide wire PATH rather than a rectangle
    → (Int, Int)   -- ^ anchor (first click), any alias
    → (Int, Int)   -- ^ second endpoint, any alias
    → ((Int, Int), (Int, Int))
structureDragBounds worldSize lineMode anchor@(ax, ay) end =
    let (lx, ly) = localizeTileToAnchor worldSize anchor end
        -- Requirement 1: the wire axis is chosen from the RAW localized
        -- delta, before the clamp. Choosing after it lets a long
        -- diagonal drag past the boundary flip axis, because clamping
        -- shortens the dominant axis toward the other one.
        (ex, ey)
            | not lineMode              = (lx, ly)
            | abs (lx - ax) ≥ abs (ly - ay) = (lx, ay)
            | otherwise                 = (ax, ly)
        ex' = clampStructureSide ax ex
        ey' = clampStructureSide ay ey
    in ((min ax ex', min ay ey'), (max ax ex', max ay ey'))

-- | Every candidate tile of the drag, in the anchor's local alias
--   frame, ascending by x then y. Never empty: the anchor is always in
--   it.
structureDragExtent ∷ Int → Bool → (Int, Int) → (Int, Int) → [(Int, Int)]
structureDragExtent worldSize lineMode anchor end =
    let ((xLo, yLo), (xHi, yHi)) =
            structureDragBounds worldSize lineMode anchor end
    in [ (gx, gy) | gx ← [xLo .. xHi], gy ← [yLo .. yHi] ]
