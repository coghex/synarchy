{-# LANGUAGE Strict #-}

-- | Exact river surfaces over an already selected footprint. Selection
-- continues to use the historical whole-z bank tests and culling. Width
-- claims at the same historical plane stay flat. Conflicting planes meet
-- as junctions, with surface connections resolved independently of rainfall
-- accumulation. Directed flow and lateral bounds are relaxed together.
module World.Fluid.River.Identify.Surface
    ( exactRiverSurfaces
    , riverSurfaceBaseline
    ) where

import UPrelude
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef')
import World.Constants (seaLevel)
import World.Fluid.Exact (exactSurfaceOfZ, fluidUnitsPerZ)
import World.Fluid.River.Identify.Common
    (stepDir, dirNorth, dirEast, dirSouth, dirWest)
import World.Fluid.River.Identify.SurfaceFlow (resolveSurfaceFlow)

-- | Inputs are the original centre mask, final retained footprint,
-- historical FINAL whole-z surface field, width-claim pairs, and actual
-- breakthrough paths (mouth through ocean endpoint). Non-river slots
-- retain minBound. No footprint, width, component or flow field is changed.
exactRiverSurfaces
    ∷ Int → VU.Vector Word8 → VU.Vector Bool → VU.Vector Bool
    → VU.Vector Int → [(Int, Int)] → [[Int]] → Int → VU.Vector Int
exactRiverSurfaces worldTiles dirs centres mask wholeSurfaces sections paths quantum =
    let wet i = mask VU.! i
        tiles = [i | i ← [0 .. VU.length mask - 1], wet i]
        centreEdges =
            [(i, j) | i ← tiles, centres VU.! i,
                Just j ← [stepDir worldTiles i (dirs VU.! i)],
                wet j, centres VU.! j]
        pathEdges = concatMap (\p → zip p (drop 1 p)) paths
        anchors = IM.fromList [(last p, exactSurfaceOfZ seaLevel)
                              | p ← paths, not (null p)]
        originals = riverSurfaceBaseline worldTiles mask wholeSurfaces paths quantum
        original i = originals VU.! i
        flowEdges = resolveSurfaceFlow worldTiles mask originals (centreEdges <> pathEdges)
        downstream = IM.fromList flowEdges
        ahead 0 _ = []
        ahead n i = original i : case IM.lookup i downstream of
            Just j → ahead (n - 1) j
            Nothing → replicate (n - 1) (original i)
        -- A lookahead average spreads steps upstream; cap its own
        -- adjustment below one z so a distant cliff cannot lower an
        -- entire approach. The baseline already fits sink constraints;
        -- compatible equality and downhill flow cannot amplify this cap.
        target i
            | IM.member i anchors = original i
            | centres VU.! i ∨ IM.member i downstream =
                min (original i) $ max (original i - (fluidUnitsPerZ - 1))
                    (sum (ahead fluidUnitsPerZ i) `div` fluidUnitsPerZ)
            | otherwise = original i
        -- (a,b,w) means H[b] <= H[a] + w. Equality is two
        -- zero-weight edges; monotonicity is upstream → downstream.
        flatClaims = [(a,b) | (a,b) ← sections, wet a, wet b,
                             original a ≡ original b]
        equalities = [(a,b,0) | (a,b) ← flatClaims]
                  <> [(b,a,0) | (a,b) ← flatClaims]
        -- Do not introduce a steeper neighbouring step while making
        -- a section flat. An old flat pair may gain one eighth step.
        bounds = surfaceBounds worldTiles mask wholeSurfaces quantum
        edges = equalities <> bounds <> [(a,b,0) | (a,b) ← flowEdges]
        initial = VU.generate (VU.length mask) $ \i → if wet i then target i else minBound
    in lowerSurfaces mask initial edges

-- | Fit explicit breakthrough endpoints to sea level before classifying
-- level junctions. Ocean elsewhere overrides this latent table at composition;
-- those overridden entries are not extra sinks in the surface graph.
riverSurfaceBaseline
    ∷ Int → VU.Vector Bool → VU.Vector Int
    → [[Int]] → Int → VU.Vector Int
riverSurfaceBaseline n mask whole paths quantum =
    let anchors = IM.fromList [(last p, exactSurfaceOfZ seaLevel)
                              | p ← paths, not (null p)]
        initial = VU.imap (\i h → if mask VU.! i
            then IM.findWithDefault (exactSurfaceOfZ h) i anchors else minBound) whole
    in lowerSurfaces mask initial (surfaceBounds n mask whole quantum)

surfaceBounds ∷ Int → VU.Vector Bool → VU.Vector Int → Int → [(Int,Int,Int)]
surfaceBounds n mask whole quantum =
    [(i,j, min (exactSurfaceOfZ (max 1 quantum)) $
        max 1 (exactSurfaceOfZ (abs (whole VU.! i - whole VU.! j))))
    | i ← [0 .. VU.length mask - 1], mask VU.! i,
      d ← [dirNorth,dirEast,dirSouth,dirWest],
      Just j ← [stepDir n i d], mask VU.! j]

lowerSurfaces
    ∷ VU.Vector Bool → VU.Vector Int
    → [(Int,Int,Int)] → VU.Vector Int
lowerSurfaces mask initial constraints = VU.create $ do
        let tiles = [i | i ← [0 .. VU.length mask - 1], mask VU.! i]
            edges = IM.fromListWith (<>) [(a,[(b,w)]) | (a,b,w) ← constraints]
        surface ← VU.thaw initial
        pending ← newSTRef tiles
        let loop = do
                queue ← readSTRef pending
                case queue of
                    [] → pure ()
                    i:rest → do
                        writeSTRef pending rest
                        h ← VUM.read surface i
                        forM_ (IM.findWithDefault [] i edges) $ \(j,w) → do
                            hj ← VUM.read surface j
                            let lowered = h + w
                            when (hj > lowered) $ do
                                VUM.write surface j lowered
                                modifySTRef' pending (j:)
                        loop
        loop
        pure surface
