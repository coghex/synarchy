{-# LANGUAGE Strict #-}
-- | Pure placement validation for buildings. Given the world tile
--   data + a def's footprint + an anchor (gx, gy), return whether the
--   placement is valid, plus a reason string when it isn't.
module Building.Placement
    ( canPlaceAt
    , buildingAnchorZ
    , PlacementResult(..)
    , RemoteCheck(..)
    , remoteCheck
    , isRemote
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import Building.Types
import World.Tile.Types (WorldTileData, lookupChunk)
import World.Chunk.Types (LoadedChunk(..), columnIndex)
import World.Generate.Coordinates (canonicalTile, canonicalTileFrame)
import Location.Instance (LocationInstances)
import Location.Placement (placedLocationBounds, nearestLocationDistance)
import Location.Bounds (AbsBounds(..), boundsIntersect, remotePortalThresholdTiles)

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
--        stored absolute bounds of any location placed on this page —
--        the starting portal can't land inside a ruin. Ordinary
--        construction is unaffected, so locations remain occupiable/
--        repairable/incorporable later.
--   For the "flat_ground" placement kind, all five. Other kinds may
--   come later (water-only for docks, sheer cliffs for towers, etc).
canPlaceAt
    ∷ BuildingManager
    → WorldTileData
    → LocationInstances -- ^ this page's placed locations (for #778 bounds)
    → Int               -- ^ world size in chunks (seam-aware bounds check)
    → BuildingDef
    → Int           -- ^ anchor gx
    → Int           -- ^ anchor gy
    → PlacementResult
canPlaceAt bm wtd instances worldSize def gx gy
    | bdPlacement def ≡ "flat_ground" =
        checkFlatGround bm wtd instances worldSize def gx gy
    | otherwise = NotPlaceable
        ("unknown placement kind: " <> bdPlacement def)

checkFlatGround
    ∷ BuildingManager
    → WorldTileData
    → LocationInstances
    → Int
    → BuildingDef
    → Int
    → Int
    → PlacementResult
checkFlatGround bm wtd instances worldSize def gx gy =
    -- #1175: a footprint is enumerated by stepping off its anchor, so
    -- even a canonical anchor in the last column produces tiles past the
    -- canonical u range — and the anchor itself can be an alias when it
    -- came from a pre-#1175 construct job. Both the terrain read and the
    -- occupancy test therefore resolve each tile into the stored frame.
    -- Identity away from the seam and for non-wrapping worlds.
    let tiles = footprintTiles gx gy (bdTileW def) (bdTileH def)
        zs    = traverse (lookupSurfaceZ worldSize wtd) tiles
    in case zs of
        Nothing → NotPlaceable "chunk not loaded"
        Just (z0:rest)
            | any (≢ z0) rest → NotPlaceable "ground is uneven"
            | any (tileHasBuilding worldSize bm) tiles →
                NotPlaceable "tile already occupied"
            | bdIsStarting def ∧ overlapsAnyLocation worldSize instances def gx gy →
                NotPlaceable "inside a location's bounds"
            | otherwise → Placeable
        Just [] → NotPlaceable "empty footprint"   -- defensive; tileW/H≥1

-- | True if the def's footprint, anchored at (gx, gy), intersects any
--   placed location's absolute bounds on this page (#778). A separate
--   top-level function (not a `where`-bound value) so it's only
--   evaluated when 'bdIsStarting' short-circuits to it — this module's
--   Strict pragma would otherwise force it unconditionally.
overlapsAnyLocation
    ∷ Int → LocationInstances → BuildingDef → Int → Int → Bool
overlapsAnyLocation worldSize instances def gx gy =
    any (boundsIntersect worldSize footprint) (placedLocationBounds instances)
  where
    footprint = AbsBounds gx gy (gx + bdTileW def - 1) (gy + bdTileH def - 1)

-- | #779: remote-settlement distance classification for a placement.
--   Only a starting building (the acolyte portal) ever receives the
--   warning — mirrors 'canPlaceAt's own #778 gate on 'bdIsStarting',
--   so ordinary construction is unaffected. A page with no placed
--   locations at all reports 'RemoteDistance Nothing'
--   (still remote — see 'isRemote'), distinct from a placement that
--   simply couldn't find a nearer location than the threshold.
data RemoteCheck
    = NotStartingBuilding
    | RemoteDistance (Maybe Int)
    deriving (Show, Eq)

-- | Classify a def's placement at (gx, gy) against every location
--   placed on this page.
remoteCheck
    ∷ LocationInstances → Int → BuildingDef → Int → Int
    → RemoteCheck
remoteCheck instances worldSize def gx gy
    | not (bdIsStarting def) = NotStartingBuilding
    | otherwise = RemoteDistance
        (nearestLocationDistance worldSize instances footprint)
  where
    footprint = AbsBounds gx gy (gx + bdTileW def - 1) (gy + bdTileH def - 1)

-- | True when a 'RemoteCheck' warrants the remote-settlement
--   confirmation: no placed locations at all, or the nearest one is
--   strictly beyond 'remotePortalThresholdTiles'. A non-starting
--   building never warrants it, regardless of distance.
isRemote ∷ RemoteCheck → Bool
isRemote NotStartingBuilding      = False
isRemote (RemoteDistance Nothing) = True
isRemote (RemoteDistance (Just d)) = d > remotePortalThresholdTiles

-- | Surface Z = top of whatever's there (terrain, ice, frozen fluid).
--   This is what units walk on, and the right reference for "flat
--   ground" — ice over ocean is still a flat surface you can put a
--   portal on. The blanket fluid-rejection check we used initially
--   was too strict for this world's geology (mostly ice/glaciers).
-- | The grid z a building anchored at @(gx, gy)@ lands on: that tile's
--   own TERRAIN surface, resolved into the stored frame. 'Nothing' when
--   the chunk is not resident, which is the one state nobody can answer
--   for.
--
--   The single answer @building.spawn@ stamps onto the instance
--   ('biGridZ') and the committed-designation ghost draws at (#1845), so
--   a planned building cannot be drawn at a z it will not land on. That
--   is not a tidiness point: a terrain edit under a live designation
--   would otherwise leave the ghost at its stored @cdZ@ while the stake
--   landed somewhere else, and the hand-off — which must be invisible —
--   would move the building.
--
--   Deliberately the TERRAIN surface and not 'lookupSurfaceZ's
--   max(terrain, fluid): that is the map the spawn has always read, and
--   the placement check's own flat-ground test is a separate question
--   asked of a separate map.
buildingAnchorZ ∷ Int → WorldTileData → Int → Int → Maybe Int
buildingAnchorZ worldSize wtd gx gy =
    let (chunkCoord, (lx, ly), _) = canonicalTileFrame worldSize gx gy
    in case lookupChunk chunkCoord wtd of
        Nothing → Nothing
        Just lc → Just (lcTerrainSurfaceMap lc VU.! columnIndex lx ly)

lookupSurfaceZ ∷ Int → WorldTileData → (Int, Int) → Maybe Int
lookupSurfaceZ worldSize wtd (gx, gy) =
    let (chunkCoord, (lx, ly), _) = canonicalTileFrame worldSize gx gy
    in case lookupChunk chunkCoord wtd of
        Nothing → Nothing
        Just lc → Just (lcSurfaceMap lc VU.! columnIndex lx ly)

-- | True if any existing building's footprint covers this tile.
--   Linear scan — fine for the handful of buildings expected in early
--   game; if this grows, switch to a per-chunk occupancy set.
--
--   #1175: compared in the CANONICAL frame on both sides. A placed
--   building's own footprint is stepped off its anchor the same way, so
--   at the seam its far columns are aliases; comparing raw would let a
--   new building land on top of one.
tileHasBuilding ∷ Int → BuildingManager → (Int, Int) → Bool
tileHasBuilding worldSize bm tile =
    any inFootprint (HM.elems (bmInstances bm))
  where
    canon = canonicalTile worldSize
    (cgx, cgy) = uncurry canon tile
    inFootprint inst =
        let ax = biAnchorX inst
            ay = biAnchorY inst
        in or [ canon x y ≡ (cgx, cgy)
              | x ← [ax .. ax + biTileW inst - 1]
              , y ← [ay .. ay + biTileH inst - 1] ]
