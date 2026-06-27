{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Bounded local A* on the tile grid.
--
-- Triggered by `Unit.Thread.Movement` when greedy stepping is blocked
-- or when the next step's cost crosses `replanCostThreshold`. The
-- search is bounded by `maxRadius` tiles (Chebyshev distance from
-- src). If the destination is outside that bound or fully walled off,
-- we return a partial path to the closest reached tile by Euclidean
-- distance to the destination — the unit walks that far, then
-- replans from its new position. This is the "naive but eventual"
-- navigation philosophy in code form.
module Unit.Pathing.AStar
    ( localAStar
    , defaultMaxRadius
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.Set            as Set
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Unit.Pathing.Cost (stepCost, PathingConfig)
import World.Tile.Types (WorldTileData)

-- | Default search radius. Roughly the size of a chunk (16×16 tiles).
defaultMaxRadius ∷ Int
defaultMaxRadius = 16

-- | Local bounded A* from `src` to `dst`.
--
--   * `maxRadius` caps the Chebyshev distance from src to any
--     expanded tile, so the search stays local even when dst is far.
--   * Returns the list of tile coordinates *after* src, in order. The
--     last element is dst if reachable, otherwise the closest reached
--     tile by Euclidean distance to dst.
--   * Returns `[]` if `src == dst` or if no neighbor of src is
--     passable.
localAStar ∷ PathingConfig → WorldTileData → (Int, Int) → (Int, Int) → Int → [(Int, Int)]
localAStar pc wtd src dst maxRadius
    | src ≡ dst = []
    | otherwise =
        let h0 = euclid src dst
            initial = AS
                { asOpen   = Set.singleton (h0, src)
                , asGScore = HM.singleton src 0
                , asParent = HM.empty
                , asClosed = HS.empty
                , asBest   = src
                , asBestH  = h0
                }
            budget = max 64 (maxRadius * maxRadius * 4)
            final  = run pc wtd src dst maxRadius budget initial
            endTile
                | HS.member dst (asClosed final) = dst
                | otherwise                      = asBest final
        in reconstruct (asParent final) src endTile

-- | A* state. The open set uses `(f-score, tile)` so `Set.minView`
--   pops the lowest f-cost (with the tile as a tiebreaker — fine for
--   our purposes since any ordering of equal-f tiles is admissible).
data AS = AS
    { asOpen   ∷ !(Set.Set (Float, (Int, Int)))
    , asGScore ∷ !(HM.HashMap (Int, Int) Float)
    , asParent ∷ !(HM.HashMap (Int, Int) (Int, Int))
    , asClosed ∷ !(HS.HashSet (Int, Int))
    , asBest   ∷ !(Int, Int)
    , asBestH  ∷ !Float
    }

-- | Drive the A* loop. Recurses until dst is closed, the open set
--   empties, or the node budget runs out.
run ∷ PathingConfig → WorldTileData → (Int, Int) → (Int, Int) → Int → Int → AS → AS
run pc wtd src dst maxR budget st
    | HS.size (asClosed st) ≥ budget = st
    | otherwise = case Set.minView (asOpen st) of
        Nothing -> st
        Just ((_, cur), open')
            | cur ≡ dst ->
                st { asOpen = open', asClosed = HS.insert cur (asClosed st) }
            | HS.member cur (asClosed st) ->
                run pc wtd src dst maxR budget (st { asOpen = open' })
            | otherwise ->
                let st1 = st { asOpen   = open'
                             , asClosed = HS.insert cur (asClosed st)
                             }
                    st2 = expand pc wtd src dst maxR cur st1
                in  run pc wtd src dst maxR budget st2

-- | Relax all 8 neighbors of `cur`.
expand ∷ PathingConfig → WorldTileData → (Int, Int) → (Int, Int) → Int → (Int, Int) → AS → AS
expand pc wtd src dst maxR cur st0 =
    foldl' (relax pc wtd src dst maxR cur) st0 (neighbors cur)

-- | Standard A* relaxation with two extra guards:
--   * Skip neighbors farther than `maxR` (Chebyshev) from src.
--   * Track the best-h-so-far so we can return a partial path on
--     failure to reach dst.
relax
    ∷ PathingConfig
    → WorldTileData
    → (Int, Int)  -- ^ src
    → (Int, Int)  -- ^ dst
    → Int         -- ^ maxR
    → (Int, Int)  -- ^ cur
    → AS
    → (Int, Int)  -- ^ nbr
    → AS
relax pc wtd src dst maxR cur st nbr
    | HS.member nbr (asClosed st)    = st
    | chebyshev src nbr > maxR       = st
    | otherwise = case stepCost pc wtd cur nbr of
        Nothing → st
        Just c  →
            let curG  = fromMaybe 0 (HM.lookup cur (asGScore st))
                g'    = curG + c
                bestG = HM.lookup nbr (asGScore st)
            in case bestG of
                Just bg | bg ≤ g' → st  -- already have a better route to nbr
                _ →
                    let h = euclid nbr dst
                        f = g' + h
                        (newBest, newBestH)
                            | h < asBestH st = (nbr, h)
                            | otherwise      = (asBest st, asBestH st)
                    in st { asOpen   = Set.insert (f, nbr) (asOpen st)
                          , asGScore = HM.insert nbr g'  (asGScore st)
                          , asParent = HM.insert nbr cur (asParent st)
                          , asBest   = newBest
                          , asBestH  = newBestH
                          }

-- | 8-connected neighbors.
neighbors ∷ (Int, Int) → [(Int, Int)]
neighbors (gx, gy) =
    [ (gx + dx, gy + dy)
    | dx ← [-1, 0, 1]
    , dy ← [-1, 0, 1]
    , (dx, dy) /= (0, 0)
    ]

-- | Walk the parent chain from `end` back to `src` and emit the
--   forward-order list (waypoints after src).
reconstruct
    ∷ HM.HashMap (Int, Int) (Int, Int)
    → (Int, Int)
    → (Int, Int)
    → [(Int, Int)]
reconstruct parents src end
    | end ≡ src = []
    | otherwise = go end []
  where
    go cur acc
        | cur ≡ src = acc
        | otherwise = case HM.lookup cur parents of
            Nothing → acc  -- broken chain (shouldn't happen if asBest is correct)
            Just p  → go p (cur : acc)

euclid ∷ (Int, Int) → (Int, Int) → Float
euclid (ax, ay) (bx, by) =
    let dx = fromIntegral (bx - ax) ∷ Float
        dy = fromIntegral (by - ay) ∷ Float
    in sqrt (dx * dx + dy * dy)

chebyshev ∷ (Int, Int) → (Int, Int) → Int
chebyshev (ax, ay) (bx, by) = max (abs (bx - ax)) (abs (by - ay))
