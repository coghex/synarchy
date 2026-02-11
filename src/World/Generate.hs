{-# LANGUAGE Strict #-}
module World.Generate
    ( -- * Generation
      generateChunk
      -- * Coordinate helpers
    , chunkSize
    , globalToChunk
    , chunkToGlobal
    , chunkWorldBounds
    , chunkLoadRadius
    , cameraChunkCoord
      -- * Constants
    , viewDepth
      -- * Types re-export
    , ChunkCoord(..)
    ) where

import UPrelude
import Data.Bits (xor, shiftR, (.&.))
import Data.Word (Word32, Word64)
import qualified Data.HashMap.Strict as HM
import World.Types (Tile(..), ChunkCoord(..), Chunk, WorldGenParams(..))
import World.Material (MaterialId(..))
import World.Plate (TectonicPlate(..), generatePlates
                   , elevationAtGlobal, isBeyondGlacier, wrapGlobalX)
import World.Grid (worldToGrid)

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------

chunkSize :: Int
chunkSize = 16

chunkLoadRadius :: Int
chunkLoadRadius = 2

-- | How many z-levels below the z-slice are rendered.
--   This is a RENDER window, not a generation limit.
viewDepth :: Int
viewDepth = 50

-----------------------------------------------------------
-- Coordinate Helpers
-----------------------------------------------------------

globalToChunk :: Int -> Int -> (ChunkCoord, (Int, Int))
globalToChunk gx gy =
    let cx = floorDiv gx chunkSize
        cy = floorDiv gy chunkSize
        lx = floorMod gx chunkSize
        ly = floorMod gy chunkSize
    in (ChunkCoord cx cy, (lx, ly))

chunkToGlobal :: ChunkCoord -> Int -> Int -> (Int, Int)
chunkToGlobal (ChunkCoord cx cy) lx ly =
    (cx * chunkSize + lx, cy * chunkSize + ly)

chunkWorldBounds :: ChunkCoord -> ((Int, Int), (Int, Int))
chunkWorldBounds (ChunkCoord cx cy) =
    let minX = cx * chunkSize
        minY = cy * chunkSize
        maxX = minX + chunkSize - 1
        maxY = minY + chunkSize - 1
    in ((minX, minY), (maxX, maxY))

cameraChunkCoord :: Float -> Float -> ChunkCoord
cameraChunkCoord camX camY =
    let (gx, gy) = worldToGrid camX camY
        (coord, _) = globalToChunk gx gy
    in coord

floorDiv :: Int -> Int -> Int
floorDiv a b = floor (fromIntegral a / fromIntegral b :: Double)

floorMod :: Int -> Int -> Int
floorMod a b = a - floorDiv a b * b

-----------------------------------------------------------
-- Chunk Generation
-----------------------------------------------------------

-- | Generate a single chunk. Pure and deterministic.
--   Returns (tiles, surfaceMap) where surfaceMap maps (lx,ly) -> surfaceZ.
--   Only produces tiles that are exposed to the surface:
--   the top tile of each column, plus any tiles whose side
--   face is visible because a neighbor column is shorter.
generateChunk :: WorldGenParams -> ChunkCoord -> (Chunk, HM.HashMap (Int, Int) Int)
generateChunk params coord =
    let seed = wgpSeed params
        worldSize = wgpWorldSize params
        plates = generatePlates seed worldSize (wgpPlateCount params)

        -- Wrap global X for border tiles that may cross the seam
        wrapGX gx = wrapGlobalX worldSize gx

        columns = [ ((lx, ly), elevationAtGlobal seed plates worldSize (wrapGX gx) gy)
                  | lx <- [-1 .. chunkSize]
                  , ly <- [-1 .. chunkSize]
                  , let (gx, gy) = chunkToGlobal coord lx ly
                  , not (isBeyondGlacier worldSize (wrapGX gx) gy)
                  ]
        elevMap = HM.fromList columns

        lookupElev lx ly = case HM.lookup (lx, ly) elevMap of
            Just (z, _) -> z
            Nothing     -> 0

        -- Build the surface elevation map for this chunk's own columns
        surfaceMap = HM.fromList
            [ ((lx, ly), surfZ)
            | lx <- [0 .. chunkSize - 1]
            , ly <- [0 .. chunkSize - 1]
            , let (gx, gy) = chunkToGlobal coord lx ly
            , not (isBeyondGlacier worldSize (wrapGX gx) gy)
            , let surfZ = lookupElev lx ly
            ]

        tiles = [ tile
                | lx <- [0 .. chunkSize - 1]
                , ly <- [0 .. chunkSize - 1]
                , let (gx, gy) = chunkToGlobal coord lx ly
                , not (isBeyondGlacier worldSize (wrapGX gx) gy)
                , let (surfZ, mat) = case HM.lookup (lx, ly) elevMap of
                          Just v  -> v
                          Nothing -> (0, MaterialId 1)
                      neighborMinZ = minimum
                          [ lookupElev (lx - 1) ly
                          , lookupElev (lx + 1) ly
                          , lookupElev lx (ly - 1)
                          , lookupElev lx (ly + 1)
                          ]
                      -- Generate tiles all the way down to the shortest neighbor.
                      -- No viewDepth cap here — viewDepth only limits rendering.
                      exposeFrom = min surfZ neighborMinZ
                , tile <- generateExposedColumn lx ly surfZ exposeFrom (unMaterialId mat)
                ]
    in (HM.fromList tiles, surfaceMap)

-- | Generate only the exposed tiles for a column.
--   Always includes the surface tile. Below that, includes
--   tiles down to exposeFrom (where a neighbor's surface is lower).
generateExposedColumn :: Int -> Int -> Int -> Int -> Word8
                      -> [((Int, Int, Int), Tile)]
generateExposedColumn lx ly surfaceZ exposeFrom material =
    [ ((lx, ly, z), Tile material 0)
    | z <- [exposeFrom .. surfaceZ]
    ]
