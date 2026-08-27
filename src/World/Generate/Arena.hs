{-# LANGUAGE Strict #-}
module World.Generate.Arena
    ( generateFlatChunk
    , generateArenaChunks
    , arenaGenForSeed
    ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.Random (StdGen, mkStdGen, randomR)
import World.Types
import World.Material
import World.Vegetation
import Structure.Types (emptyChunkStructures)

-- | Generate a single flat loam chunk at seaLevel.
--   Pure and trivial — no geology, no fluids, no flora.
generateFlatChunk ∷ ChunkCoord → LoadedChunk
generateFlatChunk coord =
    let MaterialId loamId = matLoam
        vegId = vegMediumGrass
        arenaZ    = seaLevel
        chunkArea = chunkSize * chunkSize
        flatColumn = ColumnTiles
            { ctStartZ = arenaZ
            , ctMats   = VU.singleton loamId
            , ctSlopes = VU.singleton 0
            , ctVeg    = VU.singleton vegId
            }
    in LoadedChunk
        { lcCoord             = coord
        , lcTiles             = V.replicate chunkArea flatColumn
        , lcSurfaceMap        = VU.replicate chunkArea arenaZ
        , lcTerrainSurfaceMap = VU.replicate chunkArea arenaZ
        , lcFluidMap          = V.replicate chunkArea Nothing
        , lcIceMap            = emptyIceMap
        , lcFlora             = emptyFloraChunkData
        , lcSideDeco          = VU.replicate chunkArea 0
        , lcWaterTableMap     = VU.replicate chunkArea (arenaZ - 2)
        , lcMagma             = Nothing
        , lcStructures        = emptyChunkStructures
        }

-- | The generator an arena page's base is built from, derived from the
--   seed that page RECORDS in its own 'WorldGenParams' (#1718). Fresh
--   creation and the save-load restore path both go through this one
--   derivation, so the base a loaded arena is rebuilt with is the base
--   its recorded seed actually produced. Kept here, beside the only
--   consumer, so neither caller can invent its own mapping; 'wgpSeed' is
--   a 'Word64' and 'mkStdGen' takes an 'Int', and the truncation is
--   irrelevant because the canonical arena seed is 0.
arenaGenForSeed ∷ Word64 → StdGen
arenaGenForSeed seed = mkStdGen (fromIntegral seed)

-- | Arena footprint: (2r+1)² chunks centred on the origin (5×5).
arenaRadius ∷ Int
arenaRadius = 2

-- | Build the full eager arena chunk set — the 5×5 flat loam-over-granite
--   region 'handleWorldInitArenaCommand' stamps at init. Moved here (#365)
--   so the save-load path can rebuild an arena page identically instead of
--   running the real generator on the arena's synthetic gen params (which
--   wedges the world thread). The StdGen only varies the surface grass
--   sprites; every chunk shares one column layout.
--
--   A pure function of its generator, deliberately (#1718): the base is
--   never persisted, so a loaded arena is reconstructed rather than
--   restored. Both callers must therefore pass 'arenaGenForSeed' applied
--   to the page's OWN recorded 'wgpSeed' — passing anything else (an
--   ambient 'newStdGen', a hardcoded constant) re-rolls every surface
--   tile's grass variant across a save/load round trip.
generateArenaChunks ∷ StdGen → [LoadedChunk]
generateArenaChunks gen =
    let MaterialId loamId    = matLoam
        MaterialId graniteId = matGranite
        -- Surface veg base id: + rand 0..3 spreads across the grass/moss
        -- variants (ids 5..8), matching the original arena look.
        grassId    = vegMediumGrass
        arenaZ     = seaLevel    -- z = 0 (surface)
        loamLayers    = 4        -- top 4 tiles
        graniteLayers = 12       -- 12 tiles below
        arenaDepth    = loamLayers + graniteLayers
        columnStartZ  = arenaZ - arenaDepth + 1   -- bottom of the column
        chunkArea     = chunkSize * chunkSize     -- 256

        -- Column material stack (shared across all columns): index 0 is
        -- the bottom of the column, index (arenaDepth - 1) is the top.
        -- Loam fills the top loamLayers; granite fills below.
        columnMats   = VU.generate arenaDepth (\i →
                         if i ≥ graniteLayers then loamId else graniteId)
        columnSlopes = VU.replicate arenaDepth 0

        buildColumns ∷ Int → V.Vector ColumnTiles → StdGen → V.Vector ColumnTiles
        buildColumns 0    acc _g = acc
        buildColumns area acc g = buildColumns (area - 1)
                                      (V.cons newelem acc) newg
            where (rand, newg) = randomR (0, 3) g
                  actualId = grassId + rand
                  -- Vegetation only on the top tile (the visible surface);
                  -- granite + lower loam tiles get 0 so the renderer
                  -- doesn't sprout grass underground after a dig.
                  columnVeg = VU.generate arenaDepth (\i →
                                if i ≡ arenaDepth - 1 then actualId else 0)
                  newelem = (ColumnTiles
                             { ctStartZ = columnStartZ
                             , ctMats   = columnMats
                             , ctSlopes = columnSlopes
                             , ctVeg    = columnVeg
                             })
        flatSurfaceMap = VU.replicate chunkArea arenaZ
        flatFluidMap   = V.replicate chunkArea Nothing
        flatFlora      = emptyFloraChunkData
        flatChunk      = buildColumns chunkArea V.empty gen

        mkChunk cx cy = LoadedChunk
            { lcCoord             = ChunkCoord cx cy
            , lcTiles             = flatChunk
            , lcSurfaceMap        = flatSurfaceMap
            , lcTerrainSurfaceMap = flatSurfaceMap
            , lcFluidMap          = flatFluidMap
            , lcIceMap            = emptyIceMap
            , lcFlora             = flatFlora
            , lcSideDeco          = VU.replicate (chunkSize * chunkSize) 0
            , lcWaterTableMap    = VU.replicate (chunkSize * chunkSize) (arenaZ - 2)
            , lcMagma             = Nothing
            , lcStructures        = emptyChunkStructures
            }

    in [ mkChunk cx cy
       | cx ← [-arenaRadius .. arenaRadius]
       , cy ← [-arenaRadius .. arenaRadius]
       ]
