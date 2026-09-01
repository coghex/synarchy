{-# LANGUAGE Strict #-}
-- | The WORLD CONTEXT an unplaced structure piece's art depends on
--   (#1842): the cap state a wall would be built with, and the
--   connection shape a wire would be built with.
--
--   "Structure.ArtCatalog" answers "what art does this pack have for
--   this slot?" from a registration alone. Two kinds need more than the
--   descriptor to pick their slot, and both rules already exist — in
--   Lua, where the render pass cannot reach them:
--
--     * A WALL's cap facemap comes from the two end corners of that
--       edge, testing posts on THAT TILE ONLY, ordered left\/right
--       (@scripts/structures.lua@'s @placeWall@ + @WALL_ENDS@, which is
--       'Structure.Facing.wallEdgeEnds' here). Same-tile is deliberate:
--       a post on a neighbouring tile shares the end node but must not
--       cap this wall.
--     * A WIRE's connection shape comes from which of its four cardinal
--       neighbours carry wire ('Structure.Wire.wireShapeFor').
--
--   Every lookup here canonicalizes across the cylindrical seam the way
--   @structure.hasAt@ does (#1175): chunks are stored u-wrapped, so one
--   physical tile has two names near the seam, and a seam-blind lookup
--   would read a wall's own post as absent. Away from the seam every
--   step is the identity.
--
--   The DESIGNATION half is what the render pass adds and the placer
--   must not have: 'wireNeighborsAt' takes the designation map as an
--   explicit 'Maybe', so the placement path ('Nothing' — placed and
--   staged wire only, exactly @scripts/wire.lua@'s current behaviour)
--   and the render path ('Just' — a designated neighbour connects too)
--   cannot be confused for one another.
module World.Construct.Art
    ( structurePresentAt
    , structureGridZAt
    , postCornerSlot
    , wallCapsAt
    , wireDesignatedAt
    , wireNeighborsAt
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Structure.Facing (WallEdge, WallCaps(..), PostCorner(..), wallEdgeEnds)
import Structure.Types
    ( StructureSlot(..), StructureStage(..), StagedStructurePiece(..)
    , StructurePieceData(..) )
import Structure.Wire (WireNeighbors(..))
import World.Chunk.Types (LoadedChunk(..))
import World.Construct.Types
    (ConstructDesignations, ConstructDesignation(..), ConstructTarget(..)
    , StructurePiece(..))
import World.Generate.Coordinates (canonicalTile, canonicalTileFrame)
import World.Tile.Types (WorldTileData, lookupChunk)

-- | Is a piece present in this slot of tile @(gx, gy)@? The staging
--   cache first, then the authoritative per-chunk overlay — the same
--   two authorities, in the same order, that @structure.hasAt@ consults,
--   so a piece placed earlier in the same Lua call is visible here too.
structurePresentAt ∷ Int → WorldTileData → StructureStage → StructureSlot
                   → Int → Int → Bool
structurePresentAt worldSize tileData stage slot gx gy =
    HM.member key (ssEntries stage)
      ∨ maybe False (HM.member key ∘ lcStructures) (lookupChunk coord tileData)
  where
    (coord, _, (dgx, dgy)) = canonicalTileFrame worldSize gx gy
    key = (gx + dgx, gy + dgy, fromIntegral (fromEnum slot) ∷ Word8)

-- | The grid z of the piece in this slot, or 'Nothing' when there is
--   none. Same two authorities in the same order as 'structurePresentAt'
--   — which is what @structure.floorZAt@ reads, and it is read for the
--   same reason: a post takes its supporting FLOOR's z, not a z derived
--   from terrain (#1844).
structureGridZAt ∷ Int → WorldTileData → StructureStage → StructureSlot
                 → Int → Int → Maybe Int
structureGridZAt worldSize tileData stage slot gx gy =
    case HM.lookup key (ssEntries stage) of
        Just staged → Just (spdGridZ (stgPiece staged))
        Nothing     → spdGridZ ⊚ (lookupChunk coord tileData
                                    ⌦ HM.lookup key ∘ lcStructures)
  where
    (coord, _, (dgx, dgy)) = canonicalTileFrame worldSize gx gy
    key = (gx + dgx, gy + dgy, fromIntegral (fromEnum slot) ∷ Word8)

-- | A tile corner → the post slot that stands at it.
postCornerSlot ∷ PostCorner → StructureSlot
postCornerSlot c = case c of
    CornerN → SPostN
    CornerE → SPostE
    CornerS → SPostS
    CornerW → SPostW

-- | The cap facemap state a wall on @edge@ of tile @(gx, gy)@ would be
--   BUILT with: this tile's own posts at the edge's two end corners,
--   in 'wallEdgeEnds' order — canvas-left first, which is what makes the
--   result the @"\<left\>\<right\>"@ suffix the pack YAML keys its four
--   cap facemaps by.
wallCapsAt ∷ Int → WorldTileData → StructureStage → WallEdge → Int → Int
           → WallCaps
wallCapsAt worldSize tileData stage edge gx gy = WallCaps (post l) (post r)
  where
    (l, r) = wallEdgeEnds edge
    post c = structurePresentAt worldSize tileData stage (postCornerSlot c) gx gy

-- | Is tile @(gx, gy)@ DESIGNATED for wire? Any outstanding wire
--   designation counts, whatever its status: a claimed or part-built
--   tile is still going to be wire, which is what the run being drawn
--   will connect to. The map is keyed by the canonical tile
--   ('World.Thread.Command.Cursor.Construct' inserts under
--   'canonicalTile'), so the lookup canonicalizes too.
wireDesignatedAt ∷ Int → ConstructDesignations → Int → Int → Bool
wireDesignatedAt worldSize designations gx gy =
    case HM.lookup (canonicalTile worldSize gx gy) designations of
        Just cd → case cdTarget cd of
            CtStructure piece → spKind piece ≡ "wire"
            CtBuilding _      → False
        Nothing → False

-- | Which of tile @(gx, gy)@'s four cardinal neighbours a wire there
--   would connect to, in @N, E, S, W@ order.
--
--   'Nothing' for the designations is the PLACEMENT answer — placed and
--   staged wire only, which is what @scripts/wire.lua@ has always used
--   and must keep using, or laying a run would recap neighbours against
--   wire that is not there yet. 'Just' is the RENDER answer, where a
--   designated neighbour connects because that is what the finished run
--   will look like.
wireNeighborsAt ∷ Int → WorldTileData → StructureStage
                → Maybe ConstructDesignations → Int → Int → WireNeighbors
wireNeighborsAt worldSize tileData stage mDesignations gx gy = WireNeighbors
    { wnNorth = wired gx       (gy - 1)
    , wnEast  = wired (gx + 1) gy
    , wnSouth = wired gx       (gy + 1)
    , wnWest  = wired (gx - 1) gy
    }
  where
    wired nx ny =
        structurePresentAt worldSize tileData stage SWire nx ny
          ∨ maybe False (\d → wireDesignatedAt worldSize d nx ny) mDesignations
