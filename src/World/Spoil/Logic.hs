{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Spoil routing glued to world data: the tile-legality predicate
--   and the capacity / blocked queries built on the pure router in
--   "World.Spoil.Types". Shared by the dig command handler (deposit
--   + refusal gate) and the Lua dig-info query (so the AI can skip
--   spoil-blocked designations instead of digging in place forever).
module World.Spoil.Logic
    ( spoilTileOk
    , spoilStartVertex
    , spoilCapacityAt
    , spoilBlockedAt
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.List (sortOn)
import World.Chunk.Types (LoadedChunk(..), columnIndex)
import World.Generate.Coordinates (globalToChunk)
import World.Material.Id (MaterialId(..))
import World.Mine.Types (MineDesignations)
import World.Spoil.Types (SpoilPiles, spoilCapacity)
import World.Tile.Types (WorldTileData, lookupChunk)

-- | May spoil land on this tile? Loaded chunk, not designated for
--   mining (that includes the dig tile itself), dry, and within TWO
--   z of the dig level ("toss it up onto the bank") — piles don't
--   pour off cliffs or into water. ±2 (user decision) lets a
--   full-rect quarry go two levels per cycle before the rim is out
--   of reach.
spoilTileOk ∷ WorldTileData → MineDesignations → Int → (Int, Int) → Bool
spoilTileOk td desigs digZ (tx, ty) =
    not (HM.member (tx, ty) desigs) ∧
    case lookupChunk coord td of
        Nothing → False
        Just lc →
            let idx   = columnIndex lx ly
                surfZ = lcTerrainSurfaceMap lc VU.! idx
                fluid = lcFluidMap lc V.! idx
            in case fluid of
                Just _  → False
                Nothing → abs (surfZ - digZ) ≤ 2
  where
    (coord, (lx, ly)) = globalToChunk tx ty

-- | Preferred first vertex for a dig's spoil: the dig tile's corner
--   farthest from the digger, so the mound starts behind the tile
--   relative to whoever is working it.
spoilStartVertex ∷ (Float, Float) → (Int, Int) → (Int, Int)
spoilStartVertex (ux, uy) (gx, gy) =
    fst $ last $ sortOn snd
        [ (v, dist2 v)
        | v ← [ (gx, gy), (gx + 1, gy)
              , (gx + 1, gy + 1), (gx, gy + 1) ] ]
  where
    dist2 (cx, cy) =
        let dx = fromIntegral cx - ux
            dy = fromIntegral cy - uy
        in dx * dx + dy * dy ∷ Float

-- | Corner-units of spoil the area around a dig can still absorb.
spoilCapacityAt ∷ WorldTileData → MineDesignations → SpoilPiles
                → MaterialId → Int → (Float, Float) → (Int, Int)
                → Float
spoilCapacityAt td desigs piles mat digZ upos tile =
    spoilCapacity (spoilTileOk td desigs digZ) mat
                  (spoilStartVertex upos tile) piles

-- | Is the dig effectively blocked for lack of spoil room? A small
--   epsilon of headroom is required so the per-tick deposit always
--   fits what the refusal gate approved.
spoilBlockedAt ∷ WorldTileData → MineDesignations → SpoilPiles
               → MaterialId → Int → (Float, Float) → (Int, Int)
               → Bool
spoilBlockedAt td desigs piles mat digZ upos tile =
    spoilCapacityAt td desigs piles mat digZ upos tile < 0.25
