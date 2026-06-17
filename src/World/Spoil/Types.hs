{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | Spoil piles: material displaced by digging, mounding up at tile
--   VERTICES near the excavation.
--
--   A pile lives at a lattice vertex — the point where four tiles
--   meet. Each of those tiles contributes exactly one corner to the
--   vertex, and every tile corner belongs to exactly one vertex, so
--   piles at different vertices can never fight over a corner. The
--   pile raises those four corners one at a time ("expand before
--   stack": all deposit routing fills level 1 across nearby vertices
--   before anything stacks to level 2; hard cap 'spoilMaxLevels').
--
--   Fills are stored RELATIVE to each touching tile's current terrain
--   surface. When a tile ends up with all four of its corners filled
--   one full level (fed by up to four different piles), the cell is
--   promoted to real terrain via a WeAddTile edit and one level is
--   debited from each contributing corner — the overlay only ever
--   holds the partial fringe.
--
--   Pure module: the deposit router takes a tile-legality predicate
--   as a closure so it stays unit-testable without world state.
module World.Spoil.Types
    ( SpoilPile(..)
    , SpoilPiles
    , emptySpoilPiles
    , spoilSlotTiles
    , tileCornerVertices
    , spoilRaiseThreshold
    , spoilMaxLevels
    , spoilSearchRadius
    , spoilCapacity
    , depositSpoil
    , candidateVertices
    , promotableTiles
    , debitPromotedTile
    , spoilLevelAt
    ) where

import UPrelude hiding (get)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.List (sortOn, nub)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM
import World.Material.Id (MaterialId(..))

-- | One pile at a vertex. Slot order is fixed:
--   slot 0 = tile (vx-1, vy-1) — its SE corner
--   slot 1 = tile (vx,   vy-1) — its SW corner
--   slot 2 = tile (vx-1, vy  ) — its NE corner
--   slot 3 = tile (vx,   vy  ) — its NW corner
--   Fill is in corner-units relative to the slot tile's current
--   terrain surface: 0..1 = first level, 1..2 = stacked second level.
data SpoilPile = SpoilPile
    { spMat  ∷ !MaterialId
    , spFill ∷ !(Float, Float, Float, Float)
    } deriving (Show, Eq, Generic, Serialize, NFData)

-- | Vertex (lattice point) → pile.
type SpoilPiles = HM.HashMap (Int, Int) SpoilPile

emptySpoilPiles ∷ SpoilPiles
emptySpoilPiles = HM.empty

-- | A slot's corner renders raised once its fill (within the level)
--   crosses this — mirrors the dig system's 'digThreshold'.
spoilRaiseThreshold ∷ Float
spoilRaiseThreshold = 0.5

-- | Vertical cap: a vertex can hold at most this many stacked levels
--   (4 corner-units each). Expansion always runs first; stacking is
--   the boxed-in fallback.
spoilMaxLevels ∷ Int
spoilMaxLevels = 2

-- | How far (in vertices, Chebyshev) the deposit router searches from
--   the preferred vertex before giving up. Capacity inside this
--   radius is what the dig-refusal gate checks.
spoilSearchRadius ∷ Int
spoilSearchRadius = 4

-- | The four (tile, slot) pairs touching a vertex, in slot order.
spoilSlotTiles ∷ (Int, Int) → [((Int, Int), Int)]
spoilSlotTiles (vx, vy) =
    [ ((vx - 1, vy - 1), 0)
    , ((vx,     vy - 1), 1)
    , ((vx - 1, vy    ), 2)
    , ((vx,     vy    ), 3)
    ]

-- | Inverse view: a tile's four corners as (vertex, slot) pairs, in
--   the designation corner order NW, NE, SE, SW.
tileCornerVertices ∷ (Int, Int) → [((Int, Int), Int)]
tileCornerVertices (tx, ty) =
    [ ((tx,     ty    ), 3)   -- NW corner
    , ((tx + 1, ty    ), 2)   -- NE corner
    , ((tx + 1, ty + 1), 0)   -- SE corner
    , ((tx,     ty + 1), 1)   -- SW corner
    ]

slotGet ∷ Int → (Float, Float, Float, Float) → Float
slotGet 0 (a, _, _, _) = a
slotGet 1 (_, b, _, _) = b
slotGet 2 (_, _, c, _) = c
slotGet _ (_, _, _, d) = d

slotSet ∷ Int → Float → (Float, Float, Float, Float)
        → (Float, Float, Float, Float)
slotSet 0 v (_, b, c, d) = (v, b, c, d)
slotSet 1 v (a, _, c, d) = (a, v, c, d)
slotSet 2 v (a, b, _, d) = (a, b, v, d)
slotSet _ v (a, b, c, _) = (a, b, c, v)

-- | Candidate vertices ordered by distance from the start (then by
--   coords for determinism), within the search radius.
candidateVertices ∷ (Int, Int) → [(Int, Int)]
candidateVertices (sx, sy) =
    map snd $ sortOn fst
        [ (dx * dx + dy * dy ∷ Int, (sx + dx, sy + dy))
        | dx ← [-r .. r], dy ← [-r .. r] ]
  where r = spoilSearchRadius

-- | Can this slot accept the material? Tiles are vetted by the
--   caller's predicate; the vertex additionally refuses to mix
--   materials (a granite-gravel mound stays gravel). The no-mixing
--   rule extends to the whole TILE the slot belongs to: a tile's four
--   corners live in four different vertices, and promotion converts
--   the whole cell to one material, so a slot is refused if any other
--   corner of its tile already holds a different spoil material.
slotUsable ∷ ((Int, Int) → Bool) → MaterialId → SpoilPiles
           → (Int, Int) → Int → Bool
slotUsable tileOk mat piles v slot =
    let tiles = spoilSlotTiles v
        tileOf = fst (tiles !! slot)
        matOk = case HM.lookup v piles of
            Nothing → True
            Just p  → spMat p ≡ mat ∨ spFill p ≡ (0, 0, 0, 0)
    in matOk ∧ tileOk tileOf ∧ not (tileMatConflict piles mat tileOf)

-- | Does the tile already hold spoil of a different material in any of
--   its four corners? Any positive fill claims the tile for its
--   material — promotion picks a single material per cell, so corners
--   must agree.
tileMatConflict ∷ SpoilPiles → MaterialId → (Int, Int) → Bool
tileMatConflict piles mat tile = any conflicting (tileCornerVertices tile)
  where
    conflicting (v, slot) = case HM.lookup v piles of
        Nothing → False
        Just p  → slotGet slot (spFill p) > 0 ∧ spMat p ≢ mat

-- | Total corner-units the piles around @start@ can still absorb.
--   The dig-refusal gate: if this is less than the spoil a dig tick
--   would produce, the dig must not start.
spoilCapacity ∷ ((Int, Int) → Bool) → MaterialId → (Int, Int)
              → SpoilPiles → Float
spoilCapacity tileOk mat start piles =
    sum
        [ fromIntegral spoilMaxLevels - fill
        | v ← candidateVertices start
        , slot ← [0 .. 3]
        , slotUsable tileOk mat piles v slot
        , let fill = maybe 0 (slotGet slot ∘ spFill) (HM.lookup v piles)
        , fill < fromIntegral spoilMaxLevels
        ]

-- | Route @amount@ corner-units of spoil into the piles around
--   @start@. Expand-before-stack: pass 1 fills every reachable slot
--   to one level; pass 2 (only if material remains) stacks toward
--   'spoilMaxLevels'. Returns the new piles and any unplaced
--   leftover (0 when the caller pre-checked 'spoilCapacity').
depositSpoil ∷ ((Int, Int) → Bool) → MaterialId → (Int, Int) → Float
             → SpoilPiles → (SpoilPiles, Float)
depositSpoil tileOk mat start amount0 piles0 =
    let passes = [1.0, fromIntegral spoilMaxLevels]
        step (piles, amt) cap
            | amt ≤ 0   = (piles, amt)
            | otherwise = foldl' (pourVertex cap) (piles, amt)
                                 (candidateVertices start)
    in foldl' step (piles0, amount0) passes
  where
    pourVertex cap (piles, amt) v
        | amt ≤ 0   = (piles, amt)
        | otherwise = foldl' (pourSlot cap v) (piles, amt) [0 .. 3]
    pourSlot cap v (piles, amt) slot
        | amt ≤ 0 = (piles, amt)
        | not (slotUsable tileOk mat piles v slot) = (piles, amt)
        | otherwise =
            let pile = HM.lookupDefault (SpoilPile mat (0, 0, 0, 0))
                                        v piles
                fill = slotGet slot (spFill pile)
                room = cap - fill
            in if room ≤ 0
               then (piles, amt)
               else
                   let d = min room amt
                       pile' = pile { spMat  = mat
                                    , spFill = slotSet slot (fill + d)
                                                       (spFill pile) }
                   in (HM.insert v pile' piles, amt - d)

-- | Tiles (among those touching the given vertices) whose four
--   corners are all at least one full level — ready to compact into
--   real terrain.
promotableTiles ∷ SpoilPiles → [(Int, Int)] → [(Int, Int)]
promotableTiles piles vs =
    [ t
    | t ← nub [ tile | v ← vs, (tile, _) ← spoilSlotTiles v ]
    , all cornerFull (tileCornerVertices t)
    ]
  where
    cornerFull (v, slot) =
        case HM.lookup v piles of
            Nothing → False
            Just p  → slotGet slot (spFill p) ≥ 1.0

-- | Debit one full level from each of a promoted tile's corners (its
--   surface just rose one z, so the remaining fill is again relative
--   to the new surface). Empty piles are dropped to keep the map
--   tidy.
debitPromotedTile ∷ (Int, Int) → SpoilPiles → SpoilPiles
debitPromotedTile t piles = foldl' debit piles (tileCornerVertices t)
  where
    debit m (v, slot) = case HM.lookup v m of
        Nothing → m
        Just p  →
            let fill' = max 0 (slotGet slot (spFill p) - 1.0)
                p'    = p { spFill = slotSet slot fill' (spFill p) }
            in if spFill p' ≡ (0, 0, 0, 0)
               then HM.delete v m
               else HM.insert v p' m

-- | Render levels for one slot fill: how many z the corner shows
--   raised (0, 1 or 2). Crosses at the half-level thresholds.
spoilLevelAt ∷ Float → Int
spoilLevelAt f
    | f ≥ 1.0 + spoilRaiseThreshold = 2
    | f ≥ spoilRaiseThreshold       = 1
    | otherwise                     = 0
