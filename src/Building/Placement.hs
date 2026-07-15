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
--   For the "flat_ground" placement kind, all four. Other kinds may
--   come later (water-only for docks, sheer cliffs for towers, etc).
canPlaceAt
    ∷ BuildingManager
    → WorldTileData
    → BuildingDef
    → Int           -- ^ anchor gx
    → Int           -- ^ anchor gy
    → PlacementResult
canPlaceAt bm wtd def gx gy
    | bdPlacement def ≡ "flat_ground" = checkFlatGround bm wtd def gx gy
    | otherwise = NotPlaceable
        ("unknown placement kind: " <> bdPlacement def)

checkFlatGround
    ∷ BuildingManager
    → WorldTileData
    → BuildingDef
    → Int
    → Int
    → PlacementResult
checkFlatGround bm wtd def gx gy =
    let tiles = footprintTiles gx gy (bdTileW def) (bdTileH def)
        zs    = traverse (lookupSurfaceZ wtd) tiles
    in case zs of
        Nothing → NotPlaceable "chunk not loaded"
        Just (z0:rest)
            | any (≠ z0) rest → NotPlaceable "ground is uneven"
            | any (tileHasBuilding bm) tiles → NotPlaceable "tile already occupied"
            | otherwise → Placeable
        Just [] → NotPlaceable "empty footprint"   -- defensive; tileW/H≥1

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
