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
import World.Fluid.Types (FluidCell(..))
import World.Edit.Types (WorldEdit(..), WorldEdits)
import World.Generate.Coordinates (globalToChunk)

-- | Apply one edit to a chunk. Out-of-bounds edits (the column index
--   doesn't fall in this chunk, or a delete pops an already-empty
--   column) silently no-op rather than crash — keeps the replay
--   resilient against player edits at chunk-boundary corner cases.
applyEdit ∷ WorldEdit → LoadedChunk → LoadedChunk
applyEdit (WeDeleteTile gx gy) lc
    | not (edgeBelongsTo gx gy lc) = lc
    | otherwise =
        let idx     = columnIdx gx gy
            oldTopZ = lcSurfaceMap lc VU.! idx
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
               in lc
                   { lcTiles             = lcTiles lc V.// [(idx, col')]
                   , lcSurfaceMap        = lcSurfaceMap        lc VU.// [(idx, oldTopZ - 1)]
                   , lcTerrainSurfaceMap = lcTerrainSurfaceMap lc VU.// [(idx, oldTopZ - 1)]
                   }
applyEdit (WeSetFluidTile gx gy ft) lc
    | not (edgeBelongsTo gx gy lc) = lc
    | otherwise =
        let idx        = columnIdx gx gy
            surfZ      = lcTerrainSurfaceMap lc VU.! idx
            newSurface = surfZ + 1
            cell       = FluidCell { fcType = ft, fcSurface = newSurface }
            oldTop     = lcSurfaceMap lc VU.! idx
        in lc
            { lcFluidMap   = lcFluidMap lc V.// [(idx, Just cell)]
            , lcSurfaceMap = lcSurfaceMap lc VU.// [(idx, max oldTop newSurface)]
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
