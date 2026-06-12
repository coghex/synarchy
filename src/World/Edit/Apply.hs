{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure replay of `WorldEdit` operations onto a `LoadedChunk`.
--
--   Both code paths use this:
--   - Live edits (in `World.Thread.Command.Edit`) apply via `applyEdit`
--     then append to the world's edit log.
--   - Chunk regeneration after eviction (in `World.Thread.ChunkLoading`)
--     calls `replayEdits` to overlay the saved edits onto the freshly
--     generated chunk.
--
--   Single-source-of-truth for the edit semantics — if the live path
--   and the replay path ever diverge, every chunk eviction round-trip
--   silently mutates the world.
module World.Edit.Apply
    ( applyEdit
    , replayEdits
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (LoadedChunk(..), ColumnTiles(..), columnIndex)
import World.Flora.Types (FloraChunkData(..), FloraInstance(..))
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Edit.Types (WorldEdit(..), WorldEdits)
import World.Generate.Coordinates (globalToChunk)
import World.Material.Id (MaterialId(..))
import World.Hydrology.WaterTable (waterTableAtTile)

-- | Apply one edit to a chunk. Out-of-bounds edits (the column index
--   doesn't fall in this chunk, or a delete pops an already-empty
--   column) silently no-op rather than crash — keeps the replay
--   resilient against player edits at chunk-boundary corner cases.
applyEdit ∷ WorldEdit → LoadedChunk → LoadedChunk
applyEdit (WeDeleteTile gx gy) lc
    | not (edgeBelongsTo gx gy lc) = lc
    | otherwise =
        let (_, (lx, ly)) = globalToChunk gx gy
            idx     = columnIndex lx ly
            -- Dig operates on TERRAIN top, not surface top, so that
            -- successive digs through revealed water continue to
            -- advance terrain instead of no-oping against the water
            -- column we just exposed.
            oldTopZ = lcTerrainSurfaceMap lc VU.! idx
            col     = lcTiles lc V.! idx
            colLen  = VU.length (ctMats col)
            i       = oldTopZ - ctStartZ col
        in if i < 0 ∨ i ≥ colLen
           then lc                              -- nothing left to dig
           else
               let col' = col
                       { ctMats   = ctMats   col VU.// [(i, 0)]
                       , ctSlopes = ctSlopes col VU.// [(i, 0)]
                       , ctVeg    = ctVeg    col VU.// [(i, 0)]
                       }
                   -- Terrain changed: any flora rooted on this tile
                   -- goes with it (a tree can't stand over the hole).
                   floraKept = FloraChunkData
                       [ fi | fi ← fcdInstances (lcFlora lc)
                            , fromIntegral (fiTileX fi) ≠ lx
                              ∨ fromIntegral (fiTileY fi) ≠ ly ]
                   newTopZ  = oldTopZ - 1
                   wt       = waterTableAtTile lc lx ly
                   curFluid = lcFluidMap lc V.! idx
                   -- If the newly exposed surface is at or below the
                   -- water table and the column has no fluid yet,
                   -- reveal groundwater. The fluid surface sits at the
                   -- water table — successive digs further down expose
                   -- more space beneath the same flat water surface.
                   newFluid = case curFluid of
                       Just _                        → curFluid
                       Nothing | newTopZ ≤ wt        →
                           Just (FluidCell Lake wt)
                       _                             → Nothing
                   newSurface = case newFluid of
                       Just fc → max newTopZ (fcSurface fc)
                       Nothing → newTopZ
               in lc
                   { lcTiles             = lcTiles lc V.// [(idx, col')]
                   , lcSurfaceMap        = lcSurfaceMap        lc VU.// [(idx, newSurface)]
                   , lcTerrainSurfaceMap = lcTerrainSurfaceMap lc VU.// [(idx, newTopZ)]
                   , lcFluidMap          = lcFluidMap lc V.// [(idx, newFluid)]
                   , lcFlora             = floraKept
                   }
applyEdit (WeAddTile gx gy mat) lc
    | not (edgeBelongsTo gx gy lc) = lc
    | otherwise =
        let (_, (lx, ly)) = globalToChunk gx gy
            idx     = columnIndex lx ly
            -- Raise on TERRAIN top, mirroring WeDeleteTile: a spoil
            -- cell promoted under revealed groundwater displaces the
            -- water column upward via the surface recompute below.
            oldTopZ = lcTerrainSurfaceMap lc VU.! idx
            col     = lcTiles lc V.! idx
            colLen  = VU.length (ctMats col)
            newTopZ = oldTopZ + 1
            i       = newTopZ - ctStartZ col
        in if i < 0 ∨ i > colLen
           then lc                          -- malformed column; no-op
           else
               -- Columns are allocated with no headroom above the
               -- generated surface, so adding at the top usually
               -- means GROWING the vectors by one cell (i == colLen).
               -- Without this, WeAddTile silently no-ops on flat
               -- ground and spoil promotion would leak material.
               let col' = if i ≡ colLen
                       then col
                           { ctMats   = VU.snoc (ctMats   col)
                                                (unMaterialId mat)
                           , ctSlopes = VU.snoc (ctSlopes col) 0
                           , ctVeg    = VU.snoc (ctVeg    col) 0
                           }
                       else col
                           { ctMats   = ctMats   col VU.// [(i, unMaterialId mat)]
                           , ctSlopes = ctSlopes col VU.// [(i, 0)]
                           , ctVeg    = ctVeg    col VU.// [(i, 0)]
                           }
                   -- A pile burying the tile takes any rooted flora
                   -- with it, same rule as digging it out.
                   floraKept = FloraChunkData
                       [ fi | fi ← fcdInstances (lcFlora lc)
                            , fromIntegral (fiTileX fi) ≠ lx
                              ∨ fromIntegral (fiTileY fi) ≠ ly ]
                   curFluid = lcFluidMap lc V.! idx
                   -- Filling at or above the fluid surface displaces
                   -- the fluid cell entirely (promotion legality
                   -- excludes fluid tiles, but replay must stay
                   -- total for resilience).
                   newFluid = case curFluid of
                       Just fc | newTopZ ≥ fcSurface fc → Nothing
                       _                                → curFluid
                   newSurface = case newFluid of
                       Just fc → max newTopZ (fcSurface fc)
                       Nothing → newTopZ
               in lc
                   { lcTiles             = lcTiles lc V.// [(idx, col')]
                   , lcSurfaceMap        = lcSurfaceMap        lc VU.// [(idx, newSurface)]
                   , lcTerrainSurfaceMap = lcTerrainSurfaceMap lc VU.// [(idx, newTopZ)]
                   , lcFluidMap          = lcFluidMap lc V.// [(idx, newFluid)]
                   , lcFlora             = floraKept
                   }
applyEdit (WeSetFluidTile gx gy ft) lc
    | not (edgeBelongsTo gx gy lc) = lc
    | otherwise =
        let idx        = columnIdx gx gy
            surfZ      = lcTerrainSurfaceMap lc VU.! idx
            newSurface = surfZ + 1
            cell       = FluidCell { fcType = ft, fcSurface = newSurface }
            oldTop     = lcSurfaceMap lc VU.! idx
            -- River renders flat at fcSurface to hide protrusions; other
            -- fluid types use max(terrain/old, fluid). Mirrors the rule in
            -- World.Generate.Chunk.mkSurfaceMap and Sim.Thread.
            renderedSurf = case ft of
                River → newSurface
                _     → max oldTop newSurface
        in lc
            { lcFluidMap   = lcFluidMap lc V.// [(idx, Just cell)]
            , lcSurfaceMap = lcSurfaceMap lc VU.// [(idx, renderedSurf)]
            }

-- | Replay every edit recorded for this chunk, in stored order.
--   Defensive `HM.lookup` — chunks with no edits round-trip unchanged.
replayEdits ∷ WorldEdits → LoadedChunk → LoadedChunk
replayEdits edits lc = case HM.lookup (lcCoord lc) edits of
    Nothing → lc
    Just es → foldl (flip applyEdit) lc es

-- Helpers ---------------------------------------------------------

edgeBelongsTo ∷ Int → Int → LoadedChunk → Bool
edgeBelongsTo gx gy lc =
    let (coord, _) = globalToChunk gx gy
    in coord ≡ lcCoord lc

-- | Convert (gx, gy) global coords to the local column index in this
--   chunk's flat vector. Pre-condition: edgeBelongsTo is true.
columnIdx ∷ Int → Int → Int
columnIdx gx gy =
    let (_, (lx, ly)) = globalToChunk gx gy
    in columnIndex lx ly
