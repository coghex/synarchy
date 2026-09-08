{-# LANGUAGE Strict #-}
-- | The world-generation cell source for the map pyramid (issue #2298,
--   WML-5).
--
--   This is the only module in the pyramid that knows how a finest cell
--   is filled, and it fills it the way the map has always been filled:
--   through "World.ZoomMap.Cache.ChunkPass", which is the shipping
--   builder's own two passes. A page therefore carries the SAME bytes
--   'World.ZoomMap.Cache.Pixels.generateChunkPixels' produces today for
--   every chunk it covers, including the cross-boundary ocean
--   extension, rather than a second implementation that has to be kept
--   in agreement with it.
--
--   == The halo
--
--   Pass two reads the neighbouring chunks' pass-one fluid maps, so a
--   page footprint is not self-sufficient. Every batch therefore runs
--   pass one over the requested chunks PLUS
--   'zoomChunkHaloNeighbours' of each, and pass two over the requested
--   chunks alone. Because the halo is derived from each chunk rather
--   than from the batch, a chunk's bytes do not depend on what else was
--   asked for in the same batch: a page generated alone is the page
--   generated inside a larger region.
--
--   == Purity and totality
--
--   Pure: same parameters, same registry, same palette, same bytes,
--   with no clock, no filesystem and no chunk residency involved.
--   Total over NORMALIZED, INTERNALLY CONSISTENT
--   'World.Generate.Types.WorldGenParams' — the same precondition
--   'World.ZoomMap.Cache.BuildPixels' has always generated under.
--   'WorldGenParams' is publicly constructible, so the one consistency
--   this module can check, it checks: parameters whose world size
--   disagrees with the geometry they were paired with are refused
--   rather than silently addressed against the wrong lattice.
module World.ZoomMap.Pyramid.Cells
    ( worldGenCellSource
      -- * The halo, as its own two steps
    , mapCellHaloTable
    , renderCellsFromHaloTable
    ) where

import UPrelude
import Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import World.Chunk.Types (ChunkCoord(..))
import World.Generate.InitTerrain (BorderedTerrainCache)
import World.Generate.Types (WorldGenParams(..))
import World.Material (MaterialRegistry)
import World.ZoomMap.Cache.ChunkPass
    ( ZoomChunkPass(..), zoomChunkHaloNeighbours, zoomChunkPass
    , zoomChunkPixels )
import World.ZoomMap.ColorPalette (ZoomColorPalette)
import World.ZoomMap.Pyramid.Address
import World.ZoomMap.Pyramid.Inventory (MapPyramidRefusal(..))
import World.ZoomMap.Pyramid.Page (MapCellSource(..))

-- | Pass one over a batch of chunks AND the halo their pass two reads:
--   every requested chunk plus each one's
--   'zoomChunkHaloNeighbours'.
--
--   The halo is derived from each chunk, never from the batch, which is
--   what makes a chunk's bytes independent of what else was asked for
--   alongside it.
mapCellHaloTable ∷ WorldGenParams → MaterialRegistry
                 → Maybe BorderedTerrainCache → [ChunkCoord]
                 → Map.Map ChunkCoord ZoomChunkPass
mapCellHaloTable params registry mBorderedCache chunks =
    Map.fromList (zip needed passes)
  where
    worldSize = wgpWorldSize params
    halo = concatMap (zoomChunkHaloNeighbours worldSize) chunks
    needed = Set.toList (Set.fromList (chunks ⧺ halo))
    batch = max 1 (length needed `div` 128)
    passes = map (zoomChunkPass params registry mBorderedCache) needed
                 `using` parListChunk batch rdeepseq

-- | Render chunks out of such a table.
--
--   The TABLE is the halo: a chunk's pass two reads its neighbours'
--   pass-one fluid maps out of it, and a neighbour the table does not
--   hold answers dry — exactly how the whole-world builder's missing
--   map key behaves at the latitude edge. Dropping that lookup, or
--   handing this a table without the neighbours, changes the bytes of
--   every chunk whose shoreline gap the extension would have closed.
renderCellsFromHaloTable ∷ ZoomColorPalette → Int
                         → Map.Map ChunkCoord ZoomChunkPass
                         → [ChunkCoord] → Either Text [BS.ByteString]
renderCellsFromHaloTable palette worldSize table = traverse renderOne
  where
    haloFluid coord = zcpRawFluid ⊚ Map.lookup coord table
    renderOne coord = case Map.lookup coord table of
        Just pass →
            Right $ zoomChunkPixels palette worldSize haloFluid coord pass
        -- Reachable only from a hand-built table: 'mapCellHaloTable'
        -- always holds every chunk it was asked about. Answering rather
        -- than pattern-matching partially is what keeps this total.
        Nothing → Left $ "has no generated chunk for " <> tshow coord

-- | A cell source that generates finest cells from world-generation
--   parameters.
--
--   The 'BorderedTerrainCache' is the same optional init-time
--   accelerator 'World.ZoomMap.Cache.BuildPixels' takes: present on
--   fresh world init, absent on a loaded save. It changes speed, not
--   output — the @fresh\/cache and load\/scratch zoom identity@ spec
--   pins that — so a page is the same page either way.
worldGenCellSource ∷ MapGeometry → WorldGenParams → MaterialRegistry
                   → ZoomColorPalette → Maybe BorderedTerrainCache
                   → Either MapPyramidRefusal MapCellSource
worldGenCellSource geom params registry palette mBorderedCache
    | mgWorldSize geom ≢ wgpWorldSize params =
        Left $ MapPyramidInconsistentParams $
            "map geometry for worldSize " <> tshow (mgWorldSize geom)
            <> " was paired with generation parameters for worldSize "
            <> tshow (wgpWorldSize params)
    | otherwise = Right $ MapCellSource generate
  where
    worldSize = wgpWorldSize params

    generate cells = do
        chunks ← traverse chunkOf cells
        let table = mapCellHaloTable params registry mBorderedCache chunks
        renderCellsFromHaloTable palette worldSize table chunks

    chunkOf ∷ MapCell → Either Text ChunkCoord
    chunkOf cell = case chunkOfFinestCell geom cell of
        Right coord → Right coord
        Left refusal →
            Left $ "was asked for an unaddressable cell — "
                 <> mapAddressRefusalText refusal
