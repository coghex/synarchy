{-# LANGUAGE Strict #-}
-- | The ONE arena terrain authority (#2707, SCN-07 of epic #2698).
--
--   An arena page's base is never persisted: a save stores the page's
--   generation parameters and its edit overlay, so every column no edit
--   touched is RECONSTRUCTED from the page's recorded seed and the
--   requested canonical chunk coordinate. 'generateArenaChunk' is that
--   reconstruction, and every path that makes an arena chunk — initial
--   creation, the camera loader, the explicit\/queued demand drain,
--   eviction regeneration and save restoration — goes through it
--   (through "World.Thread.ChunkLoading".'generateDemandedChunks' for the
--   streaming paths). There is no second recipe: the 5×5 startup set is
--   only 'generateArenaChunks', a batch wrapper that maps the authority
--   over 'arenaFootprint'.
--
--   __The base.__ Every unedited column has sixteen cells: twelve granite
--   under four loam, the top at 'seaLevel'. Fluids, flora, structures,
--   ice and magma are empty, and vegetation sits on the top cell only (a
--   dug column must not sprout grass underground). No indefinite depth is
--   generated, and the startup footprint is NOT a horizontal boundary:
--   the authority answers for any canonical coordinate, and expansion
--   stays bounded only by the existing numeric and loading limits.
--
--   __Reconstruction is independent of demand order.__ The chunk is a
--   pure function of @(seed, coordinate)@. The seed-blind material and
--   topology are shared constants; the seed-sensitive surface-vegetation
--   variant of each column is drawn from a FRESH generator derived from
--   the seed and the coordinate alone, so batch size, request order,
--   repeated demand and eviction cannot change a chunk (no ambient or
--   shared RNG, no sequential stream across chunks). Grass-variant
--   placement is deliberately not a cross-revision compatibility surface
--   (#2065): within one build a seed reconstructs its own base, and a
--   later build may map the same seed to different variants.
module World.Generate.Arena
    ( generateArenaChunk
    , generateArenaChunks
    , arenaFootprint
    , arenaFootprintRadius
    , arenaLoamLayers
    , arenaGraniteLayers
    ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.Random (StdGen, mkStdGen, randomRs)
import World.Types
import World.Material
import World.Vegetation
import Structure.Types (emptyChunkStructures)

-- | Loam cells at the top of every unedited arena column.
arenaLoamLayers ∷ Int
arenaLoamLayers = 4

-- | Granite cells below the loam of every unedited arena column.
arenaGraniteLayers ∷ Int
arenaGraniteLayers = 12

-- | Startup footprint radius: (2r+1)² chunks centred on the origin (5×5).
--   A STARTUP set, not an extent — see the module header.
arenaFootprintRadius ∷ Int
arenaFootprintRadius = 2

-- | The chunk coordinates of the startup footprint, in the order the
--   batch wrapper produces them.
arenaFootprint ∷ [ChunkCoord]
arenaFootprint =
    [ ChunkCoord cx cy
    | cx ← [-arenaFootprintRadius .. arenaFootprintRadius]
    , cy ← [-arenaFootprintRadius .. arenaFootprintRadius]
    ]

-- | The eager startup set: the authority mapped over 'arenaFootprint'.
--   It carries no terrain recipe of its own. @seed@ must be the page's
--   OWN recorded 'wgpSeed' — an ambient generator or a constant here
--   would re-roll every surface tile's grass variant across a save/load
--   round trip (#1718).
generateArenaChunks ∷ Word64 → [LoadedChunk]
generateArenaChunks seed = map (generateArenaChunk seed) arenaFootprint

-- | One arena chunk, from the owning page's recorded seed and the
--   requested CANONICAL chunk coordinate (the arena's canonical
--   coordinate is the identity: 'World.Chunk.Residency.canonicalChunkCoord'
--   never wraps an arena, so the sentinel world size never reaches this
--   function).
generateArenaChunk ∷ Word64 → ChunkCoord → LoadedChunk
generateArenaChunk seed coord =
    let chunkArea = chunkSize * chunkSize
        columns = V.fromListN chunkArea
            [ arenaColumn variant
            | variant ← take chunkArea (randomRs (0, 3) (chunkGen seed coord)) ]
    in LoadedChunk
        { lcCoord             = coord
        , lcTiles             = columns
        , lcSurfaceMap        = VU.replicate chunkArea seaLevel
        , lcTerrainSurfaceMap = VU.replicate chunkArea seaLevel
        , lcFluidMap          = V.replicate chunkArea Nothing
        , lcIceMap            = emptyIceMap
        , lcFlora             = emptyFloraChunkData
        , lcSideDeco          = VU.replicate chunkArea 0
        , lcWaterTableMap     = VU.replicate chunkArea (seaLevel - 2)
        , lcMagma             = Nothing
        , lcStructures        = emptyChunkStructures
        }

-- | Cells in one unedited column.
arenaDepth ∷ Int
arenaDepth = arenaLoamLayers + arenaGraniteLayers

-- | One column: index 0 is the bottom cell, index @arenaDepth - 1@ the
--   top. @variant@ (0..3) selects the grass/moss sprite of the top cell
--   (ids 'vegMediumGrass' .. +3); every other cell carries no vegetation.
arenaColumn ∷ Word8 → ColumnTiles
arenaColumn variant = ColumnTiles
    { ctStartZ = seaLevel - arenaDepth + 1
    , ctMats   = arenaColumnMats
    , ctSlopes = arenaColumnSlopes
    , ctVeg    = VU.generate arenaDepth $ \i →
                    if i ≡ arenaDepth - 1 then vegMediumGrass + variant else 0
    }

-- | The seed-blind material stack, shared by every column.
arenaColumnMats ∷ VU.Vector Word8
arenaColumnMats = VU.generate arenaDepth $ \i →
    if i ≥ arenaGraniteLayers then loamId else graniteId
  where MaterialId loamId    = matLoam
        MaterialId graniteId = matGranite

arenaColumnSlopes ∷ VU.Vector Word8
arenaColumnSlopes = VU.replicate arenaDepth 0

-- | The vegetation generator of ONE chunk: a pure function of the seed
--   and the coordinate. A SplitMix64-style finalizer is applied after
--   folding in each input, so neighbouring coordinates (and seeds) draw
--   unrelated streams.
chunkGen ∷ Word64 → ChunkCoord → StdGen
chunkGen seed (ChunkCoord cx cy) = mkStdGen (fromIntegral h3)
  where
    h1 = mix (seed + 0x9E3779B97F4A7C15)
    h2 = mix (h1 `xor` fromIntegral cx)
    h3 = mix (h2 `xor` fromIntegral cy)
    mix ∷ Word64 → Word64
    mix z0 =
        let z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xBF58476D1CE4E5B9
            z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
        in z2 `xor` (z2 `shiftR` 31)
