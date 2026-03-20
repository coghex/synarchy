{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Fluid.Overflow
    ( handleOverflow
    ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.HashSet as HS
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Fluid.Internal (FluidMap)
import World.Constants (seaLevel)

-- | Detect river overflow after an initial fill, carve overflow
--   channels into the terrain, and place water directly along the
--   carved paths.
--
--   Returns modified terrain and a modified fluid map with overflow
--   water placed. No RiverParams are created — the water is placed
--   directly to follow the actual traced path (avoiding straight-line
--   artifacts from single-segment rivers).
handleOverflow ∷ ChunkCoord → VU.Vector Int → FluidMap
              → (VU.Vector Int, FluidMap)
handleOverflow _coord surfaceMap fluidMap =
    let candidates = detectOverflow surfaceMap fluidMap
        deduped = deduplicateCandidates candidates
    in if null deduped
       then (surfaceMap, fluidMap)
       else
        let (carvedTerrain, filledFluid) =
                foldl' (\(terr, fluid) cand →
                    applyOverflow terr fluid cand
                    ) (surfaceMap, fluidMap) deduped
        in (carvedTerrain, filledFluid)

-- | Detect overflow candidates: dry tiles adjacent to river water
--   whose terrain is at least 2 below the adjacent water surface.
detectOverflow ∷ VU.Vector Int → FluidMap → [(Int, Int, Int)]
detectOverflow surfaceMap fluidMap =
    [ (lx, ly, maxWater)
    | idx ← [0 .. chunkSize * chunkSize - 1]
    , isNothing (fluidMap V.! idx)
    , let lx = idx `mod` chunkSize
          ly = idx `div` chunkSize
          terrZ = surfaceMap VU.! idx
    , let waterNbrs = [ fcSurface fc
                      | (nx, ny) ← cardinals lx ly
                      , let nIdx = ny * chunkSize + nx
                      , Just fc ← [fluidMap V.! nIdx]
                      , fcType fc ≡ River
                      ]
    , not (null waterNbrs)
    , let maxWater = foldl' max minBound waterNbrs
    , terrZ + 1 < maxWater
    , terrZ > seaLevel
    ]

-- | Remove candidates that are within 3 tiles of each other.
deduplicateCandidates ∷ [(Int, Int, Int)] → [(Int, Int, Int)]
deduplicateCandidates [] = []
deduplicateCandidates (c : cs) = c : deduplicateCandidates (filter (farFrom c) cs)
  where
    farFrom (x1, y1, _) (x2, y2, _) =
        abs (x1 - x2) > 3 ∨ abs (y1 - y2) > 3

-- | Trace a path downhill from an overflow point, carve terrain,
--   and place water directly along the path.
applyOverflow ∷ VU.Vector Int → FluidMap → (Int, Int, Int)
              → (VU.Vector Int, FluidMap)
applyOverflow surfaceMap fluidMap (startLX, startLY, waterSurf) =
    let path = tracePath surfaceMap fluidMap startLX startLY
    in if length path < 2
       then (surfaceMap, fluidMap)
       else let -- Carve terrain along path
                terrainUpdates = carveAlongPath surfaceMap waterSurf path
                carvedTerrain = if null terrainUpdates
                                then surfaceMap
                                else surfaceMap VU.// terrainUpdates
                -- Place water directly along the path, descending
                -- from the parent river's water surface
                fluidUpdates = placeWaterAlongPath carvedTerrain waterSurf path
                filledFluid = if null fluidUpdates
                              then fluidMap
                              else fluidMap V.// fluidUpdates
            in (carvedTerrain, filledFluid)

-- | Follow steepest descent from the start tile.
tracePath ∷ VU.Vector Int → FluidMap → Int → Int → [(Int, Int)]
tracePath surfaceMap fluidMap startLX startLY =
    go startLX startLY HS.empty (64 ∷ Int)
  where
    go lx ly visited remaining
        | remaining ≤ 0 = [(lx, ly)]
        | HS.member (lx, ly) visited = [(lx, ly)]
        | lx ≤ 0 ∨ lx ≥ chunkSize - 1 ∨ ly ≤ 0 ∨ ly ≥ chunkSize - 1
            = [(lx, ly)]
        | otherwise =
            let terrZ = surfaceMap VU.! (ly * chunkSize + lx)
                visited' = HS.insert (lx, ly) visited
                nbrs = [ (nx, ny, surfaceMap VU.! (ny * chunkSize + nx))
                       | (nx, ny) ← cardinals lx ly
                       ]
            in if hasRiverNeighbor fluidMap lx ly
               then [(lx, ly)]
               else if terrZ ≤ seaLevel
               then [(lx, ly)]
               else case nbrs of
                   [] → [(lx, ly)]
                   _  →
                       let (bx, by, bz) = foldl' (\a@(_, _, az) b@(_, _, bTz) →
                               if bTz < az then b else a) (head nbrs) (tail nbrs)
                       in if bz ≥ terrZ
                          then [(lx, ly)]
                          else (lx, ly) : go bx by visited' (remaining - 1)

-- | Carve terrain along the traced path.
carveAlongPath ∷ VU.Vector Int → Int → [(Int, Int)] → [(Int, Int)]
carveAlongPath surfaceMap waterSurf path =
    [ (idx, newTerr)
    | (lx, ly) ← path
    , let idx = ly * chunkSize + lx
          terrZ = surfaceMap VU.! idx
          target = waterSurf - 2
          newTerr = min terrZ target
    , newTerr < terrZ
    ]

-- | Place river water along the traced path. Water surface descends
--   from the parent river surface, lowering by 1 for each tile where
--   terrain drops. This creates a natural cascade/waterfall effect
--   rather than a flat water plane.
placeWaterAlongPath ∷ VU.Vector Int → Int → [(Int, Int)] → [(Int, Maybe FluidCell)]
placeWaterAlongPath carvedTerrain startWaterSurf path =
    go startWaterSurf path []
  where
    go _ [] acc = reverse acc
    go prevWater ((lx, ly) : rest) acc =
        let idx = ly * chunkSize + lx
            terrZ = carvedTerrain VU.! idx
            -- Water surface: at least terrain+1, but never higher than
            -- the previous tile's water (flows downhill). Lower when
            -- terrain drops to create cascading effect.
            waterHere = max (terrZ + 1) (min prevWater (terrZ + 2))
        in if waterHere ≤ terrZ ∨ waterHere ≤ seaLevel
           then go prevWater rest acc  -- skip this tile
           else go waterHere rest ((idx, Just (FluidCell River waterHere)) : acc)

-- * Helpers

cardinals ∷ Int → Int → [(Int, Int)]
cardinals lx ly =
    [ (nx, ny)
    | (nx, ny) ← [(lx-1, ly), (lx+1, ly), (lx, ly-1), (lx, ly+1)]
    , nx ≥ 0, nx < chunkSize, ny ≥ 0, ny < chunkSize
    ]

hasRiverNeighbor ∷ FluidMap → Int → Int → Bool
hasRiverNeighbor fluidMap lx ly =
    any (\(nx, ny) →
        case fluidMap V.! (ny * chunkSize + nx) of
            Just fc → fcType fc ≡ River
            Nothing → False
        ) (cardinals lx ly)
