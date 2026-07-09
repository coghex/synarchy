{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Weather.Generate.OceanRegions
    ( oceanRegionsFromChunkMap
    , oceanRegionsFromGrid
    ) where

import UPrelude
import qualified Data.HashSet as HS
import qualified Data.Vector.Unboxed as VU
import World.Weather.Types
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Ocean.Types (OceanMap)
import World.Hydrology.Simulation (ElevGrid(..))

-- * Ocean region derivation

-- | Derive a set of ocean ClimateCoords from a chunk-resolution
--   OceanMap. Used at true world init (initEarlyClimate), where the
--   chunk-level ocean flood fill has already run.
oceanRegionsFromChunkMap ∷ Int → OceanMap → HS.HashSet ClimateCoord
oceanRegionsFromChunkMap worldSize oceanMap =
    let regionsPerSide = climateRegionCount worldSize
        halfChunks = worldSize `div` 2

        allCoords = [ ClimateCoord ru rv
                    | ru ← [0 .. regionsPerSide - 1]
                    , rv ← [0 .. regionsPerSide - 1]
                    ]

        -- Check if a climate region overlaps any ocean chunk.
        isOceanRegion (ClimateCoord ru rv) =
            let u0 = ru * climateRegionSize - halfChunks
                v0 = rv * climateRegionSize - halfChunks
            in any (\(du, dv) →
                    let u = u0 + du
                        v = v0 + dv
                    in if even (u + v)
                       then let cx = (u + v) `div` 2
                                cy = (v - u) `div` 2
                            in HS.member (ChunkCoord cx cy) oceanMap
                       else False
                   )
                   [ (du, dv) | du ← [0 .. climateRegionSize - 1]
                               , dv ← [0 .. climateRegionSize - 1] ]

    in HS.fromList [ coord | coord ← allCoords, isOceanRegion coord ]

-- | Derive a set of ocean ClimateCoords from the coarse ElevGrid.
--   Used during timeline construction to approximate ocean coverage
--   without running the expensive chunk-level BFS flood fill.
--
--   Each grid sample below seaLevel and not flagged as land maps
--   to a ClimateCoord via (u, v) → (ru, rv) conversion.
oceanRegionsFromGrid ∷ ElevGrid → Int → HS.HashSet ClimateCoord
oceanRegionsFromGrid grid worldSize =
    let gridW   = egGridW grid
        spacing = egSpacing grid
        halfGrid = gridW `div` 2
        regionsPerSide = climateRegionCount worldSize
        halfChunks = worldSize `div` 2
        -- Region size in tiles (climateRegionSize chunks × chunkSize tiles)
        regionSizeTiles = climateRegionSize * chunkSize
        halfW = halfChunks * chunkSize  -- half-world in tiles
    in HS.fromList
        [ ClimateCoord ru rv
        | ix ← [0 .. gridW - 1]
        , iy ← [0 .. gridW - 1]
        , let idx = iy * gridW + ix
        , not (egLand grid VU.! idx)
          -- Map grid sample → (u,v) tile coords → ClimateCoord
        , let u = (ix - halfGrid) * spacing
              v = (iy - halfGrid) * spacing
              -- Convert tile-space to region indices
              ru = (u + halfW) `div` regionSizeTiles
              rv = (v + halfW) `div` regionSizeTiles
        , ru ≥ 0, ru < regionsPerSide
        , rv ≥ 0, rv < regionsPerSide
        ]
