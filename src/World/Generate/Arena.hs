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
    , generateArenaChunkIn
    , generateArenaChunks
    , generateArenaChunksIn
    , arenaFootprint
    , arenaStartupChunks
    , arenaFootprintRadius
    , arenaLoamLayers
    , arenaGraniteLayers
    , absentColumn
    , absentSurfaceZ
    , clipArenaChunk
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.Random (StdGen, mkStdGen, randomRs)
import World.Arena.Bounds
    (ArenaBounds, arenaAdmitsTile, chunkColumnMask, chunkInsideArena
    , boundedChunkCoords)
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
generateArenaChunks = generateArenaChunksIn Nothing

-- | The startup set of a page with these bounds (#2708): the footprint
--   restricted to the chunks that intersect the rectangle. An expandable
--   arena starts with the whole footprint; a finite one smaller than it
--   starts with only what intersects (a 1×1 arena: exactly one chunk),
--   and one larger than it loads the rest on demand through the same
--   authority.
arenaStartupChunks ∷ Maybe ArenaBounds → [ChunkCoord]
arenaStartupChunks mb = boundedChunkCoords mb arenaFootprint

-- | 'generateArenaChunks' for a page with these bounds.
generateArenaChunksIn ∷ Maybe ArenaBounds → Word64 → [LoadedChunk]
generateArenaChunksIn mb seed =
    map (generateArenaChunkIn mb seed) (arenaStartupChunks mb)

-- | One arena chunk, from the owning page's recorded seed and the
--   requested CANONICAL chunk coordinate (the arena's canonical
--   coordinate is the identity: 'World.Chunk.Residency.canonicalChunkCoord'
--   never wraps an arena, so the sentinel world size never reaches this
--   function).
generateArenaChunk ∷ Word64 → ChunkCoord → LoadedChunk
generateArenaChunk = generateArenaChunkIn Nothing

-- | THE authority (#2708): one arena chunk of a page with these bounds
--   ('Nothing': expandable). On a finite page a chunk that only partly
--   intersects the rectangle carries its in-rectangle columns EXACTLY as
--   the expandable arena would build them — the vegetation draw is made
--   for every column index regardless, so a valid cell's variant never
--   depends on the rectangle — and the rest as ABSENT columns
--   ('absentColumn'): no cells, no surface, no vegetation, no fluid slot.
--
--   A chunk wholly OUTSIDE the rectangle must never be asked for (callers
--   filter with 'World.Arena.Bounds.boundedChunkCoords' BEFORE claiming
--   it); this function would answer with an all-absent chunk, which is
--   deliberately not something any path admits.
generateArenaChunkIn ∷ Maybe ArenaBounds → Word64 → ChunkCoord → LoadedChunk
generateArenaChunkIn mb seed coord =
    let chunkArea = chunkSize * chunkSize
        mask = fmap (`chunkColumnMask` coord) mb
        present i = maybe True (VU.! i) mask
        columns = V.fromListN chunkArea
            [ if present i then arenaColumn variant else absentColumn
            | (i, variant) ←
                zip [0 ..] (take chunkArea (randomRs (0, 3) (chunkGen seed coord))) ]
        surface = VU.generate chunkArea $ \i →
            if present i then seaLevel else absentSurfaceZ
    in LoadedChunk
        { lcCoord             = coord
        , lcTiles             = columns
        , lcSurfaceMap        = surface
        , lcTerrainSurfaceMap = surface
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

-- | The column of a cell OUTSIDE a finite arena's rectangle (#2708): no
--   cells at all. It is the engine's existing BEYOND-THE-WORLD shape — the
--   one a generated world gives the tiles past its glacier rim — an empty,
--   unrendered column whose terrain surface is the 'minBound' sentinel
--   ('absentSurfaceZ'). Every pass that looks across a column boundary
--   already special-cases that sentinel (slopes, edge strata, side
--   decorations, fluid placement, ice, the magma overlay), so a partial
--   chunk needs no new storage form and the padding is treated as what it
--   is: absent, not a lower neighbouring column. It can never change:
--   every authoritative mutation refuses an out-of-rectangle tile.
absentColumn ∷ ColumnTiles
absentColumn = ColumnTiles
    { ctStartZ = seaLevel - arenaDepth + 1
    , ctMats   = VU.empty
    , ctSlopes = VU.empty
    , ctVeg    = VU.empty
    }

-- | Re-assert a finite arena's rectangle on a chunk that edits have
--   just been replayed onto (#2708): every column outside it goes back
--   to the absent shape, with no fluid, side decoration, flora or
--   structure. A well-formed save never carries an out-of-rectangle edit
--   — every authoritative mutation refuses one — so this is the
--   guarantee that a hand-edited or damaged overlay still cannot make
--   terrain outside the map through the replay path.
clipArenaChunk ∷ Maybe ArenaBounds → LoadedChunk → LoadedChunk
clipArenaChunk Nothing lc = lc
clipArenaChunk mb@(Just ab) lc
    | chunkInsideArena ab (lcCoord lc) = lc
    | otherwise =
        let mask = chunkColumnMask ab (lcCoord lc)
            present i = mask VU.! i
        in lc
            { lcTiles = V.imap (\i c → if present i then c else absentColumn)
                               (lcTiles lc)
            , lcSurfaceMap = VU.imap
                (\i z → if present i then z else absentSurfaceZ) (lcSurfaceMap lc)
            , lcTerrainSurfaceMap = VU.imap
                (\i z → if present i then z else absentSurfaceZ)
                (lcTerrainSurfaceMap lc)
            , lcFluidMap = V.imap (\i f → if present i then f else Nothing)
                                  (lcFluidMap lc)
            , lcSideDeco = VU.imap (\i d → if present i then d else 0)
                                   (lcSideDeco lc)
            , lcFlora = FloraChunkData
                [ fi | fi ← fcdInstances (lcFlora lc)
                     , present (fromIntegral (fiTileY fi) * chunkSize
                                + fromIntegral (fiTileX fi)) ]
            , lcStructures = HM.filterWithKey
                (\(gx, gy, _) _ → arenaAdmitsTile mb gx gy) (lcStructures lc)
            }

-- | The surface z recorded for an absent column: the 'minBound' sentinel
--   the generator already uses for tiles beyond the world's edge.
absentSurfaceZ ∷ Int
absentSurfaceZ = minBound

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
