{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
-- | Pure spatial-bounds arithmetic for location definitions (#777): an
--   inclusive, axis-aligned tile box, either relative to a location's
--   anchor tile (as authored in a definition's YAML @bounds:@ block) or
--   translated to absolute world tile coordinates. This is the one
--   authoritative spatial contract every later location feature (portal
--   placement exclusion #778, remote-start distance warnings #779,
--   persistent discovery state #780, sight-based reveal #1230) shares,
--   replacing the old implicit footprint that only existed as a
--   Lua-side radius constant. Since #1230 it is the ONLY location
--   footprint: the @discovery_margin@ halo 'expandBounds' used to build
--   is gone with the proximity trigger it existed for.
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
    , LocationGeometryFailure(..)
    , narrowTileCoordinate
    , translateBounds
    , translateBoundsChecked
    , boundsContainsPoint
    , boundsIntersect
    , distancePointToBounds
    , distanceBoundsToBounds
    , nearestBoundsDistance
    , remotePortalThresholdTiles
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import World.Plate (worldWidthTiles)

-- | An inclusive, axis-aligned tile box in offsets relative to a
--   location's anchor tile.
--
--   Every authored box satisfies min ≤ max on both axes, and for a
--   'RelBounds' that rule has exactly ONE implementation in the tree:
--   the inverted-bounds rejection in 'Engine.Asset.YamlLocations' 's
--   'LocationYamlDef' parser, which fails the whole file's load naming
--   the def and the offending field (#777). A 'RelBounds' only ever
--   exists downstream of that gate — the single production
--   construction site is the API loader's @toBounds@ conversion from an
--   already-validated 'Engine.Asset.YamlLocations.LocationYamlBounds' —
--   so nothing here re-states or re-checks the rule (#1151).
--
--   The same rule over an 'AbsBounds' has a SECOND implementation, and
--   deliberately so (#1668): 'Location.Instance.locationInstanceBoundsErrors',
--   run by "World.Save.Component.PageCore" 's @validatePages@. A
--   persisted
--   location instance's box is rebuilt by
--   'World.Save.Component.WorldGenNaming.fromAbsBoundsDTO' from four
--   unrestricted wire 'Int's — the one 'AbsBounds' construction site
--   entirely outside the loader gate above — so the save boundary must
--   state the rule itself. That is not a duplicate that drifted from
--   this one: the two guard different types at different boundaries,
--   and it is not #1151's deleted shared @validRelBounds@ predicate
--   reinstated.
--
--   Being downstream of the gate is not by itself a PROOF of ordering
--   either, and since #1796 the proof has two halves, neither of which
--   is this one. First, the loader gate now ALSO constrains RANGE: all
--   four authored coordinates must lie in
--   'Engine.Asset.YamlLocations.authoredLocationCoordinateLimit' 's
--   inclusive @±(2^31 - 1)@ domain, which keeps authored data sane and
--   attributable to a field. Second — and this is the part that
--   actually proves it, because chunk coordinates remain unrestricted
--   'Int's — instance geometry is CONSTRUCTED CHECKED:
--   'Location.Instance.locationInstanceGeometry' computes the chunk
--   centre and all four translated bounds in 'Integer' and refuses the
--   placement before any 'Location.Instance.LocationInstance' exists
--   unless every component is representable as an 'Int'.
--   'translateBoundsChecked' below is that translation half.
--
--   'translateBounds' itself stays as it was — plain, unchecked 'Int'
--   addition — because it is the shared expectation ORACLE the bounds
--   and worldgen suites compare placed geometry against, and because
--   every representable translation must still come out identical to
--   it. Production code reaches geometry through the checked route, not
--   through this function.
data RelBounds = RelBounds
    { rbMinX ∷ !Int
    , rbMinY ∷ !Int
    , rbMaxX ∷ !Int
    , rbMaxY ∷ !Int
    } deriving (Show, Eq, Generic, NFData, Serialize)

-- | An inclusive, axis-aligned tile box in absolute world tile
--   coordinates — a 'RelBounds' anchored somewhere via 'translateBounds'.
data AbsBounds = AbsBounds
    { abMinX ∷ !Int
    , abMinY ∷ !Int
    , abMaxX ∷ !Int
    , abMaxY ∷ !Int
    } deriving (Show, Eq, Generic, NFData, Serialize)

-- | Anchor a relative bounds box at an absolute tile. Unchecked 'Int'
--   addition: for any translation that is representable at all this
--   agrees exactly with 'translateBoundsChecked', which is why it stays
--   the suites' expectation oracle (#1796). Production geometry is
--   built through the checked route.
translateBounds ∷ (Int, Int) → RelBounds → AbsBounds
translateBounds (gx, gy) (RelBounds minX minY maxX maxY) =
    AbsBounds (gx + minX) (gy + minY) (gx + maxX) (gy + maxY)

-- | One coordinate component that could not be represented, and the
--   exact 'Integer' value it would have had (#1796). Carried out of the
--   checked arithmetic below and given its definition id and chunk
--   coordinate by 'Location.Instance.locationInstanceGeometry', which
--   is the only place this is produced with attribution.
--
--   'lgfComponent' names the component in authored\/derived terms —
--   @"anchor.x"@, @"anchor.y"@, @"bounds.min_x"@, @"bounds.min_y"@,
--   @"bounds.max_x"@ or @"bounds.max_y"@.
data LocationGeometryFailure = LocationGeometryFailure
    { lgfComponent ∷ !Text
    , lgfValue     ∷ !Integer
    } deriving (Show, Eq, Generic, NFData)

-- | Narrow an exactly-computed 'Integer' tile coordinate to an 'Int',
--   or report it as unrepresentable (#1796).
--
--   This is where the whole checked path's safety actually lives: every
--   caller does its arithmetic in 'Integer', which cannot wrap, and
--   only ever reaches an 'Int' through here. The comparison is a direct
--   two-sided test against 'Int' 's own bounds — never @abs@, which is
--   'minBound' at 'minBound' and would admit exactly the value it was
--   meant to exclude.
narrowTileCoordinate ∷ Text → Integer → Either LocationGeometryFailure Int
narrowTileCoordinate component n
    | n < toInteger (minBound ∷ Int) ∨ n > toInteger (maxBound ∷ Int)
        = Left (LocationGeometryFailure component n)
    | otherwise = Right (fromInteger n)

-- | The checked counterpart of 'translateBounds' (#1796): anchor a
--   relative box at an absolute tile given as exact 'Integer's, and
--   build the 'AbsBounds' only once all four translated coordinates are
--   proven representable.
--
--   The anchor arrives as 'Integer' rather than 'Int' deliberately, so
--   a caller whose anchor is itself the result of unchecked
--   multiplication ('Location.Instance.locationAnchorTileInteger') can
--   hand it over WITHOUT narrowing it first — narrowing is the one
--   operation that could wrap, and it happens here, after the check.
--
--   Failure reports the FIRST offending component in
--   @min_x, min_y, max_x, max_y@ order. Nothing is clamped, saturated,
--   or returned inverted: an unrepresentable component yields no box at
--   all.
translateBoundsChecked
    ∷ (Integer, Integer) → RelBounds
    → Either LocationGeometryFailure AbsBounds
translateBoundsChecked (gx, gy) (RelBounds minX minY maxX maxY) =
    AbsBounds
        ⊚ narrowTileCoordinate "bounds.min_x" (gx + toInteger minX)
        ⊛ narrowTileCoordinate "bounds.min_y" (gy + toInteger minY)
        ⊛ narrowTileCoordinate "bounds.max_x" (gx + toInteger maxX)
        ⊛ narrowTileCoordinate "bounds.max_y" (gy + toInteger maxY)

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
