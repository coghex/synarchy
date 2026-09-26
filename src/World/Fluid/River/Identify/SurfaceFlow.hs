{-# LANGUAGE Strict #-}

-- | Generation-only surface connections. Widening can replace a centre's
-- plane with a lower intersecting reach. Its old uphill edge then describes
-- neither the water's route nor a reason to excavate the intersected reach.
-- Resolve the entire affected level patch toward its downhill wet outlets.
-- Rainfall accumulation, river identity and the selected footprint remain
-- owned by the original hydrological graph.
module World.Fluid.River.Identify.SurfaceFlow (resolveSurfaceFlow) where

import UPrelude
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as VU
import World.Fluid.River.Identify.Common
    (stepDir, dirNorth, dirEast, dirSouth, dirWest)

-- | Heights are exact, boundary-normalized planes; edges have wet endpoints.
-- Retain ordinary edges. For each plateau containing an uphill source,
-- multi-source breadth-first routing gives each tile one outlet or parent.
-- Within that plateau distance to an outlet strictly decreases; leaving it
-- strictly lowers the plane. A closed depression is a terminal pool, never
-- an invented drain or a cut through the surrounding higher channel.
resolveSurfaceFlow
    ∷ Int → VU.Vector Bool → VU.Vector Int → [(Int,Int)] → [(Int,Int)]
resolveSurfaceFlow n mask heights raw =
    let (changed,routed) = go IS.empty IM.empty uphill
    in Set.toAscList $ Set.fromList $
        [(a,b) | (a,b) ← raw, not (IS.member a changed)] <> IM.toAscList routed
  where
    original = IM.fromListWith min raw
    uphill = IS.toAscList $ IS.fromList
        [a | (a,b) ← raw, heights VU.! a < heights VU.! b]
    neighbours i = [j | d ← [dirNorth,dirEast,dirSouth,dirWest],
        Just j ← [stepDir n i d], mask VU.! j]
    plateau start = flood (IS.singleton start) [start]
      where
        flood seen [] = seen
        flood seen (i:rest) =
            let next = [j | j ← neighbours i, heights VU.! j ≡ heights VU.! start,
                            not (IS.member j seen)]
            in flood (foldr IS.insert seen next) (next <> rest)
    outlet i =
        let lower = [j | j ← neighbours i, heights VU.! j < heights VU.! i]
        in case IM.lookup i original of
            Just j | j `elem` lower → Just j
            _ → case lower of
                [] → Nothing
                _ → Just $ snd $ minimum [(heights VU.! j,j) | j ← lower]
    route members =
        let roots = [(i,j) | i ← IS.toAscList members, Just j ← [outlet i]]
            initial = IM.fromList roots
            bfs seen queue edges = case Seq.viewl queue of
                Seq.EmptyL → edges
                i Seq.:< rest →
                    let next = [j | j ← neighbours i, IS.member j members,
                                    not (IS.member j seen)]
                    in bfs (foldr IS.insert seen next)
                        (rest Seq.>< Seq.fromList next)
                        (foldr (\j → IM.insert j i) edges next)
        in bfs (IS.fromList (map fst roots)) (Seq.fromList (map fst roots)) initial
    go seen edges [] = (seen,edges)
    go seen edges (i:rest)
        | IS.member i seen = go seen edges rest
        | otherwise =
            let members = plateau i
            in go (IS.union seen members) (IM.union (route members) edges) rest
