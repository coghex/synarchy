{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure spatial-bounds arithmetic for location definitions (#777): an
--   inclusive, axis-aligned tile box, either relative to a location's
--   anchor tile (as authored in a definition's YAML @bounds:@ block) or
--   translated to absolute world tile coordinates. This is the one
--   authoritative spatial contract every later location feature (portal
--   placement exclusion #778, remote-start distance warnings #779,
--   persistent discovery state #780) shares, replacing the old implicit
--   footprint that only existed as a Lua-side radius constant.
--
--   Seam-aware variants generalise
--   'World.Chunk.Types.chunkSeamChebyshev' 's u-alias trick — try the
--   query box shifted by one world-width step along the cylindrical u
--   axis each way, then take the best result — from chunk to tile
--   granularity, via 'World.Plate.worldWidthTiles'. A plain translation
--   preserves axis-alignment, so "shift the box" is a direct
--   generalisation of "shift the coordinate".
module Location.Bounds
    ( RelBounds(..)
    , AbsBounds(..)
    , validRelBounds
    , translateBounds
    , expandBounds
    , boundsContainsPoint
    , boundsIntersect
    , distancePointToBounds
    , distanceBoundsToBounds
    , nearestBoundsDistance
    , remotePortalThresholdTiles
    ) where

import UPrelude
import World.Plate (worldWidthTiles)

-- | An inclusive, axis-aligned tile box in offsets relative to a
--   location's anchor tile.
data RelBounds = RelBounds
    { rbMinX ∷ !Int
    , rbMinY ∷ !Int
    , rbMaxX ∷ !Int
    , rbMaxY ∷ !Int
    } deriving (Show, Eq)

-- | An inclusive, axis-aligned tile box in absolute world tile
--   coordinates — a 'RelBounds' anchored somewhere via 'translateBounds'.
data AbsBounds = AbsBounds
    { abMinX ∷ !Int
    , abMinY ∷ !Int
    , abMaxX ∷ !Int
    , abMaxY ∷ !Int
    } deriving (Show, Eq)

-- | True iff min ≤ max on both axes — the shape every location's
--   authored bounds must satisfy. 'Engine.Asset.YamlLocations' rejects
--   any definition whose bounds fail this at YAML load time.
validRelBounds ∷ RelBounds → Bool
validRelBounds b = rbMinX b ≤ rbMaxX b ∧ rbMinY b ≤ rbMaxY b

-- | Anchor a relative bounds box at an absolute tile.
translateBounds ∷ (Int, Int) → RelBounds → AbsBounds
translateBounds (gx, gy) (RelBounds minX minY maxX maxY) =
    AbsBounds (gx + minX) (gy + minY) (gx + maxX) (gy + maxY)

-- | Expand a bounds box outward by a non-negative margin on all four
--   sides — a location's discovery-margin halo.
expandBounds ∷ Int → AbsBounds → AbsBounds
expandBounds margin (AbsBounds minX minY maxX maxY) =
    AbsBounds (minX - margin) (minY - margin) (maxX + margin) (maxY + margin)

rawContainsPoint ∷ AbsBounds → (Int, Int) → Bool
rawContainsPoint (AbsBounds minX minY maxX maxY) (px, py) =
    px ≥ minX ∧ px ≤ maxX ∧ py ≥ minY ∧ py ≤ maxY

rawIntersect ∷ AbsBounds → AbsBounds → Bool
rawIntersect a b =
    abMinX a ≤ abMaxX b ∧ abMinX b ≤ abMaxX a ∧
    abMinY a ≤ abMaxY b ∧ abMinY b ≤ abMaxY a

-- | Chebyshev distance from a point to a bounds box's nearest edge — 0
--   when the point is inside (mirrors 'Building.Types.footprintDist').
rawDistancePoint ∷ AbsBounds → (Int, Int) → Int
rawDistancePoint (AbsBounds minX minY maxX maxY) (px, py) =
    max (maximum [minX - px, 0, px - maxX])
        (maximum [minY - py, 0, py - maxY])

-- | Chebyshev distance between two bounds boxes — 0 when they touch or
--   overlap.
rawDistanceBounds ∷ AbsBounds → AbsBounds → Int
rawDistanceBounds a b =
    max (maximum [abMinX a - abMaxX b, 0, abMinX b - abMaxX a])
        (maximum [abMinY a - abMaxY b, 0, abMinY b - abMaxY a])

-- | A box's own images under the world's cylindrical u-wrap: itself,
--   and one shift each way by a half-world-width step along (+u, -v) —
--   the exact translation 'World.Chunk.Types.wrapChunkCoordU' applies to
--   a single coordinate, generalised here to a whole box and to tile
--   instead of chunk granularity. Degenerates to just the box for a
--   non-wrapping (arena / zero-size) world, matching 'chunkSeamChebyshev'.
seamAliases ∷ Int → AbsBounds → [AbsBounds]
seamAliases worldSize b
    | halfW ≤ 0 = [b]
    | otherwise = [ shiftBounds (k * halfW) (negate (k * halfW)) b
                  | k ← [-1, 0, 1] ]
  where
    halfW = worldWidthTiles worldSize `div` 2
    shiftBounds dx dy (AbsBounds minX minY maxX maxY) =
        AbsBounds (minX + dx) (minY + dy) (maxX + dx) (maxY + dy)

-- | Inclusive point containment, seam-aware: true if the point falls
--   inside the box or any of its u-wrap images.
boundsContainsPoint ∷ Int → AbsBounds → (Int, Int) → Bool
boundsContainsPoint worldSize b p =
    any (`rawContainsPoint` p) (seamAliases worldSize b)

-- | Inclusive rectangle intersection, seam-aware — true whenever the
--   two boxes (or a u-wrap image of the first) share at least one tile,
--   including a shared edge or single corner tile.
boundsIntersect ∷ Int → AbsBounds → AbsBounds → Bool
boundsIntersect worldSize a b = any (`rawIntersect` b) (seamAliases worldSize a)

-- | Seam-aware Chebyshev distance from a point to a bounds box's
--   nearest edge (0 if inside).
distancePointToBounds ∷ Int → AbsBounds → (Int, Int) → Int
distancePointToBounds worldSize b p =
    minimum (map (`rawDistancePoint` p) (seamAliases worldSize b))

-- | Seam-aware Chebyshev distance between two bounds boxes (0 if
--   touching or overlapping).
distanceBoundsToBounds ∷ Int → AbsBounds → AbsBounds → Int
distanceBoundsToBounds worldSize a b =
    minimum (map (`rawDistanceBounds` b) (seamAliases worldSize a))

-- | The seam-aware nearest distance from a footprint to any of a list
--   of bounds boxes (#779) — 'Nothing' when the list is empty, since
--   "no placed locations" is itself the remote condition rather than a
--   degenerate minimum.
nearestBoundsDistance ∷ Int → AbsBounds → [AbsBounds] → Maybe Int
nearestBoundsDistance _ _ [] = Nothing
nearestBoundsDistance worldSize footprint boxes =
    Just (minimum (map (distanceBoundsToBounds worldSize footprint) boxes))

-- | #779: the minimum footprint-to-nearest-placed-location distance
--   (tiles, seam-aware Chebyshev) beyond which a starting-portal
--   placement is classified remote and needs an explicit
--   remote-settlement confirmation before it spawns. Eight 16-tile
--   chunks ('World.Chunk.Types.chunkSize') — the single named,
--   documented source; nothing else should hardcode 128 for this
--   purpose. A placement exactly at this distance is NOT remote — the
--   warning begins only strictly beyond it.
remotePortalThresholdTiles ∷ Int
remotePortalThresholdTiles = 128
