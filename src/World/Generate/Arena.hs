{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate.Arena
    ( generateFlatChunk
    ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Types
import World.Constants (seaLevel)

-- | Generate a single flat loam chunk at seaLevel.
--   Pure and trivial — no geology, no fluids, no flora.
generateFlatChunk ∷ ChunkCoord → LoadedChunk
generateFlatChunk coord =
    let loamId    = 56 ∷ Word8
        arenaZ    = seaLevel
        chunkArea = chunkSize * chunkSize
        flatColumn = ColumnTiles
            { ctStartZ = arenaZ
            , ctMats   = VU.singleton loamId
            , ctSlopes = VU.singleton 0
            , ctVeg    = VU.singleton 0
            }
    in LoadedChunk
        { lcCoord             = coord
        , lcTiles             = V.replicate chunkArea flatColumn
        , lcSurfaceMap        = VU.replicate chunkArea arenaZ
        , lcTerrainSurfaceMap = VU.replicate chunkArea arenaZ
        , lcFluidMap          = V.replicate chunkArea Nothing
        , lcFlora             = emptyFloraChunkData
        , lcModified          = False
        }
