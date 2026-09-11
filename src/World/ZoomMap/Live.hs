{-# LANGUAGE Strict #-}

-- | Regenerating ONE chunk's zoom-map tile after a live terrain edit,
--   and patching it into the atlas the renderer samples (#2485).
--
--   Clearing the quad caches does not do this. 'World.Render.Zoom.Quads'
--   reads 'World.State.Types.wsZoomCacheRef' and
--   'World.State.Types.wsZoomAtlasRef', and
--   'World.Render.Zoom.Bake.ensureBakedAtlas' only re-derives QUADS from
--   that data — the terrain pixels themselves are produced once, at page
--   initialization, by 'World.ZoomMap.Cache.buildZoomCacheWithPixels'.
--   A tile that changed after that point is invisible on the zoom map
--   until those pixels are regenerated, however many caches are dropped.
--
--   The regeneration runs the SAME two passes the page's own atlas was
--   built from ('World.ZoomMap.Cache.ChunkPass'), so every unedited tile
--   comes out byte-identical to what is already on screen, and then
--   overrides the handful of tiles the edit actually changed with what
--   the live chunk now holds. Reproducing the passes from the loaded
--   chunk instead would mean re-deriving the climate, ice and
--   vegetation rules a second time, and any drift between the two
--   derivations would show as a seam around every refreshed chunk.
module World.ZoomMap.Live
    ( ZoomTileOverride(..)
    , liveChunkZoom
    , liveTileOverrides
    , zoomVisibleEditTile
    , patchAtlasTile
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Foreign.Marshal.Utils (copyBytes)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Fluid.Types (FluidCell(..), IceCell(..))
import World.Edit.Types (WorldEdit(..))
import World.Generate.Coordinates (globalToChunk)
import World.Generate.Types (WorldGenParams(..))
import World.Material (MaterialRegistry)
import World.ZoomMap.Cache.Classify (majorityMaterial)
import World.ZoomMap.Cache.ChunkPass
    (ZoomChunkPass(..), zoomChunkPass, zoomChunkPixels
    , zoomChunkHaloNeighbours)
import World.ZoomMap.ColorPalette (ZoomColorPalette)
import World.ZoomMap.Types (ZoomChunkEntry(..), zoomTileSize)

-- | One tile of a regenerated chunk, replaced with what the LIVE chunk
--   holds there rather than what generation would produce.
data ZoomTileOverride = ZoomTileOverride
    { ztoIndex    ∷ !Int
      -- ^ Local cell index, @ly * chunkSize + lx@.
    , ztoElev     ∷ !Int
      -- ^ The live terrain top ('World.Types.lcTerrainSurfaceMap').
    , ztoMaterial ∷ !Word8
      -- ^ The material at that top.
    , ztoVeg      ∷ !Word8
      -- ^ …and its vegetation byte, read from the live column rather
      --   than assumed: the pass injects SNOW vegetation on ice-covered
      --   tiles, and an override that left the pass's value in place
      --   would colour an edited cell by an ice decision the live chunk
      --   may no longer agree with.
    , ztoFluid    ∷ !(Maybe FluidCell)
      -- ^ The live fluid cell. A solidified column has none, and the
      --   pass-one map would still show the lava the contact consumed.
    , ztoIce      ∷ !(Maybe IceCell)
      -- ^ …and the live ice cell. Carried rather than cleared: no world
      --   edit clears 'World.Types.lcIceMap', so the detailed render
      --   still shows whatever ice is there, and a zoom refresh that
      --   dropped it would make the two presentations disagree about
      --   every cell this chunk has ever edited.
    } deriving (Show, Eq)

-- | The tile a recorded edit changes ON THE ZOOM MAP, if it changes one.
--
--   Total over every constructor with no catch-all, so a new kind of
--   edit is a compile error here rather than a tile that silently keeps
--   its generation-time pixels. The zoom pass colours a tile from its
--   elevation, its top material, its vegetation and its fluid, so those
--   are the edits that answer 'Just'; structure and flora edits change
--   neither, and answer 'Nothing' deliberately.
zoomVisibleEditTile ∷ WorldEdit → Maybe (Int, Int)
zoomVisibleEditTile e = case e of
    WeDeleteTile gx gy             → Just (gx, gy)
    WeSetFluidTile gx gy _         → Just (gx, gy)
    WeAddTile gx gy _              → Just (gx, gy)
    WeSetSlope{}                   → Nothing
    WeSetCell gx gy _ _            → Just (gx, gy)
    WeSetStructure{}               → Nothing
    WeClearStructure{}             → Nothing
    WeSetVeg gx gy _ _             → Just (gx, gy)
    WePlaceFlora{}                 → Nothing
    WeSetFluidSnapshot gx gy _ _   → Just (gx, gy)
    WeClearFluidSnapshot gx gy     → Just (gx, gy)
    WePlaceFloraWithId{}           → Nothing
    WePlaceFloraRef{}              → Nothing

-- | What the LIVE chunk holds at each of the given local cells, as zoom
--   tile overrides.
--
--   The caller supplies the cells from the chunk's own EDIT LOG rather
--   than from one delivery, and that is the whole point: the pixels are
--   regenerated from generation-time data, so overriding only the cells
--   one commit touched would repaint every EARLIER edit in the chunk
--   back to its generated appearance. The log carries them all,
--   cumulatively, however many commits and player edits have landed
--   since the page came up.
--
--   Diffing live against generated across the whole chunk would carry
--   them too, but it is not the same thing: a loaded chunk and
--   'World.Generate.Chunk.generateZoomTerrain' disagree on far more
--   tiles than any edit touched, so that would repaint the entire block
--   and leave the refreshed chunk looking unlike every chunk beside it.
--
--   A cell whose column cannot answer — an index out of range, or a
--   terrain top outside its column — is left to the generated pass
--   rather than guessed at.
liveTileOverrides ∷ LoadedChunk → [Int] → [ZoomTileOverride]
liveTileOverrides lc indices =
    [ ZoomTileOverride { ztoIndex    = i
                       , ztoElev     = liveElev
                       , ztoMaterial = ctMats col VU.! relZ
                       , ztoVeg      = ctVeg  col VU.! relZ
                       , ztoFluid    = lcFluidMap lc V.! i
                       , ztoIce      = lcIceMap lc V.! i
                       }
    | i ← indices
    , i ≥ 0, i < VU.length (lcTerrainSurfaceMap lc)
    , let liveElev = lcTerrainSurfaceMap lc VU.! i
    , let col  = lcTiles lc V.! i
    , let relZ = liveElev - ctStartZ col
    , relZ ≥ 0, relZ < VU.length (ctMats col)
    ]

-- | The local cells of @coord@ that this page's edit log has changed on
--   the zoom map.
editedZoomCells ∷ ChunkCoord → [WorldEdit] → [Int]
editedZoomCells coord edits =
    [ ly * chunkSize + lx
    | e ← edits
    , Just (gx, gy) ← [zoomVisibleEditTile e]
    , let (cc, (lx, ly)) = globalToChunk gx gy
    , cc ≡ coord
    ]

-- | Regenerate one chunk's @zoomTileSize × zoomTileSize@ RGBA block from
--   the LIVE chunk, and the summary entry that goes with it.
--
--   The entry matters as much as the pixels, and is produced WITHOUT a
--   palette for exactly that reason: a page with no atlas of its own has
--   no palette to regenerate pixels with, and renders the zoom map per
--   material from 'World.ZoomMap.Types.zceTexIndex' and 'zceElev'. Gating
--   the entry on the palette would leave precisely those pages reading
--   generation-time data forever, which is the case this split exists to
--   serve.
--
--   The halo pass-two extension ('World.ZoomMap.Cache.OceanFill') reads
--   the four cardinal neighbours' pass-one fluid, so those are generated
--   too. They are NOT overridden: an edit in this chunk cannot change
--   what its neighbour composed, and a neighbour that was itself edited
--   gets its own refresh.
liveChunkZoom ∷ WorldGenParams → MaterialRegistry → Maybe ZoomColorPalette
              → ChunkCoord → LoadedChunk → [WorldEdit]
              → (ZoomChunkEntry, Maybe BS.ByteString)
liveChunkZoom params registry mPalette coord lc edits =
    ( entry
    , (\palette → zoomChunkPixels palette worldSize haloFluid coord overridden)
      <$> mPalette )
  where
    worldSize = wgpWorldSize params

    generated = zoomChunkPass params registry Nothing coord
    overridden = foldl' overrideOne generated
        (liveTileOverrides lc (editedZoomCells coord edits))

    -- The same summary rule 'World.ZoomMap.Cache.ChunkPass' applies,
    -- re-run over the overridden tiles: beyond-glacier cells are left
    -- out so they cannot drag the average or contaminate the material.
    liveMats = [ (e, m) | (e, m, _, _, _) ← V.toList (zcpTiles overridden)
                        , e > minBound ]
    entry = (zcpEntry generated)
        { zceTexIndex = if null liveMats
                        then zceTexIndex (zcpEntry generated)
                        else majorityMaterial liveMats
        , zceElev     = if null liveMats
                        then zceElev (zcpEntry generated)
                        else sum (map fst liveMats) `div` length liveMats
        }

    haloHere = Map.fromList
        [ (nc, zcpRawFluid (zoomChunkPass params registry Nothing nc))
        | nc ← zoomChunkHaloNeighbours worldSize coord ]
    haloFluid nc
        | nc ≡ coord = Just (zcpRawFluid overridden)
        | otherwise  = Map.lookup nc haloHere

-- | Substitute one live tile into a generated pass.
--
--   All four of the pass's per-tile products move together — the tile
--   tuple the renderer colours from, the elevation vector the ocean
--   dilation admits against, the composed fluid map, and the ice
--   overlay — and all four come from the LIVE chunk. The ice in
--   particular is copied, never cleared: no world edit clears
--   'World.Types.lcIceMap', so the detailed render goes on showing
--   whatever ice a cell has, and a zoom refresh that dropped it would
--   make the two presentations disagree about every cell the chunk has
--   ever edited — not only the one this commit touched, since the
--   override set is the whole edit log.
overrideOne ∷ ZoomChunkPass → ZoomTileOverride → ZoomChunkPass
overrideOne pass o
    | ztoIndex o < 0 ∨ ztoIndex o ≥ V.length (zcpTiles pass) = pass
    | otherwise = pass
        { zcpTiles    = zcpTiles pass V.// [(i, (ztoElev o, ztoMaterial o,
                                                 ztoVeg o, gx, gy))]
        , zcpElevs    = zcpElevs pass VU.// [(i, ztoElev o)]
        , zcpRawFluid = zcpRawFluid pass V.// [(i, ztoFluid o)]
        , zcpIceMap   = zcpIceMap pass V.// [(i, ztoIce o)]
        }
  where
    i = ztoIndex o
    (_, _, _, gx, gy) = zcpTiles pass V.! i

-- | Copy one regenerated chunk block over its tile in an existing atlas.
--
--   Row by row, because the atlas is packed in atlas-row-major order and
--   a chunk tile is a @zoomTileSize@-wide window into each of
--   @zoomTileSize@ consecutive rows — the same layout
--   'World.ZoomMap.ChunkTexture.buildZoomAtlas' assembles.
--
--   Refuses rather than writes when the block is the wrong size or the
--   tile does not lie inside the atlas: the alternative is a
--   'copyBytes' past the end of the buffer.
patchAtlasTile ∷ Int          -- ^ Atlas width in pixels.
               → Int          -- ^ Atlas tiles per row.
               → Int          -- ^ Tile index within the atlas.
               → BS.ByteString -- ^ The regenerated block.
               → BS.ByteString -- ^ The atlas as it stands.
               → Either Text BS.ByteString
patchAtlasTile atlasW chunksPerRow idx tile atlas
    | BS.length tile ≢ tileBytes =
        Left $ "zoom tile block is " <> tshow (BS.length tile)
               <> " bytes, expected " <> tshow tileBytes
    | chunksPerRow ≤ 0 ∨ atlasW ≤ 0 =
        Left "zoom atlas has no usable layout"
    | idx < 0 =
        Left $ "zoom tile index " <> tshow idx <> " is negative"
    | lastRowOff + rowBytes > BS.length atlas =
        Left $ "zoom tile index " <> tshow idx
               <> " does not lie inside a " <> tshow (BS.length atlas)
               <> "-byte atlas"
    | otherwise = Right $ BSI.unsafeCreate (BS.length atlas) $ \dest → do
        BS.useAsCStringLen atlas $ \(src, len) →
            copyBytes dest (castPtr src) len
        BS.useAsCStringLen tile $ \(tsrc, _) →
            forM_ [0 .. zoomTileSize - 1] $ \ty →
                copyBytes (dest       `plusPtr` rowOffset ty)
                          (castPtr tsrc `plusPtr` (ty * rowBytes))
                          rowBytes
  where
    rowBytes   = zoomTileSize * 4
    tileBytes  = zoomTileSize * rowBytes
    col        = idx `mod` chunksPerRow
    row        = idx `div` chunksPerRow
    rowOffset ty = ((row * zoomTileSize + ty) * atlasW + col * zoomTileSize) * 4
    lastRowOff = rowOffset (zoomTileSize - 1)
