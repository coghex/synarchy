{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Pure placement validation for buildings. Given the world tile
--   data + a def's footprint + an anchor (gx, gy), return whether the
--   placement is valid, plus a reason string when it isn't.
module Building.Placement
    ( canPlaceAt
    , PlacementResult(..)
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import Building.Types
import World.Tile.Types (WorldTileData, lookupChunk)
import World.Chunk.Types (LoadedChunk(..), columnIndex)
import World.Generate (globalToChunk)
import Location.Types (LocationRegistry)
import Location.Overlay.Types (LocationOverlay)
import Location.Placement (placedLocationBounds)
import Location.Bounds (AbsBounds(..), boundsIntersect)

data PlacementResult
    = Placeable
    | NotPlaceable !Text
    deriving (Show, Eq)

-- | Validation: every tile in the footprint @[anchorX..anchorX+w-1]
--   × [anchorY..anchorY+h-1]@ must
--     1. be in a loaded chunk (else we can't tell)
--     2. share the same terrain surface Z (flat footprint)
--     3. have no fluid (water / lava / river)
--     4. not be occupied by an existing building
--     5. for a `bdIsStarting` def only (#778): not intersect the
--        absolute bounds of any location placed on this world page —
--        the starting portal can't land inside a ruin. Ordinary
--        construction is unaffected, so locations remain occupiable/
--        repairable/incorporable later.
--   For the "flat_ground" placement kind, all five. Other kinds may
--   come later (water-only for docks, sheer cliffs for towers, etc).
canPlaceAt
    ∷ BuildingManager
    → WorldTileData
    → LocationRegistry  -- ^ registered location defs (for #778 bounds)
    → LocationOverlay   -- ^ this world page's placed-location overlay
    → Int               -- ^ world size in chunks (seam-aware bounds check)
    → BuildingDef
    → Int           -- ^ anchor gx
    → Int           -- ^ anchor gy
    → PlacementResult
canPlaceAt bm wtd locs overlay worldSize def gx gy
    | bdPlacement def ≡ "flat_ground" =
        checkFlatGround bm wtd locs overlay worldSize def gx gy
    | otherwise = NotPlaceable
        ("unknown placement kind: " <> bdPlacement def)

checkFlatGround
    ∷ BuildingManager
    → WorldTileData
    → LocationRegistry
    → LocationOverlay
    → Int
    → BuildingDef
    → Int
    → Int
    → PlacementResult
checkFlatGround bm wtd locs overlay worldSize def gx gy =
    let tiles = footprintTiles gx gy (bdTileW def) (bdTileH def)
        zs    = traverse (lookupSurfaceZ wtd) tiles
    in case zs of
        Nothing → NotPlaceable "chunk not loaded"
        Just (z0:rest)
            | any (≠ z0) rest → NotPlaceable "ground is uneven"
            | any (tileHasBuilding bm) tiles → NotPlaceable "tile already occupied"
            | bdIsStarting def ∧ overlapsAnyLocation worldSize locs overlay def gx gy →
                NotPlaceable "inside a location's bounds"
            | otherwise → Placeable
        Just [] → NotPlaceable "empty footprint"   -- defensive; tileW/H≥1

-- | True if the def's footprint, anchored at (gx, gy), intersects any
--   placed location's absolute bounds on this page (#778). A separate
--   top-level function (not a `where`-bound value) so it's only
--   evaluated when 'bdIsStarting' short-circuits to it — this module's
--   Strict pragma would otherwise force it unconditionally.
overlapsAnyLocation
    ∷ Int → LocationRegistry → LocationOverlay → BuildingDef → Int → Int → Bool
overlapsAnyLocation worldSize locs overlay def gx gy =
    any (boundsIntersect worldSize footprint) (placedLocationBounds locs overlay)
  where
    footprint = AbsBounds gx gy (gx + bdTileW def - 1) (gy + bdTileH def - 1)

-- | Surface Z = top of whatever's there (terrain, ice, frozen fluid).
--   This is what units walk on, and the right reference for "flat
--   ground" — ice over ocean is still a flat surface you can put a
--   portal on. The blanket fluid-rejection check we used initially
--   was too strict for this world's geology (mostly ice/glaciers).
lookupSurfaceZ ∷ WorldTileData → (Int, Int) → Maybe Int
lookupSurfaceZ wtd (gx, gy) =
    let (chunkCoord, (lx, ly)) = globalToChunk gx gy
    in case lookupChunk chunkCoord wtd of
        Nothing → Nothing
        Just lc → Just (lcSurfaceMap lc VU.! columnIndex lx ly)

-- | True if any existing building's footprint covers this tile.
--   Linear scan — fine for the handful of buildings expected in early
--   game; if this grows, switch to a per-chunk occupancy set.
tileHasBuilding ∷ BuildingManager → (Int, Int) → Bool
tileHasBuilding bm (gx, gy) =
    any inFootprint (HM.elems (bmInstances bm))
  where
    inFootprint inst =
        let ax = biAnchorX inst
            ay = biAnchorY inst
            w  = biTileW inst
            h  = biTileH inst
        in gx ≥ ax ∧ gx < ax + w ∧ gy ≥ ay ∧ gy < ay + h
