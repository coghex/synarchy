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
    , liveChunkPixels
    , patchAtlasTile
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Foreign.Marshal.Utils (copyBytes)
import World.Chunk.Types (ChunkCoord(..))
import World.Fluid.Types (FluidCell(..))
import World.Generate.Types (WorldGenParams(..))
import World.Material (MaterialRegistry)
import World.ZoomMap.Cache.ChunkPass
    (ZoomChunkPass(..), zoomChunkPass, zoomChunkPixels
    , zoomChunkHaloNeighbours)
import World.ZoomMap.ColorPalette (ZoomColorPalette)
import World.ZoomMap.Types (zoomTileSize)

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
      -- ^ …and its vegetation byte. Carried rather than assumed zero:
      --   the pass injects SNOW vegetation on ice-covered tiles, and an
      --   override that left that in place would paint fresh stone
      --   white.
    , ztoFluid    ∷ !(Maybe FluidCell)
      -- ^ The live fluid cell. A solidified column has none, and the
      --   pass-one map would still show the lava the contact consumed.
    } deriving (Show, Eq)

-- | Regenerate one chunk's @zoomTileSize × zoomTileSize@ RGBA block with
--   the given tiles overridden.
--
--   The halo pass-two extension ('World.ZoomMap.Cache.OceanFill') reads
--   the four cardinal neighbours' pass-one fluid, so those are generated
--   too. They are NOT overridden: an edit in this chunk cannot change
--   what its neighbour composed, and a neighbour that was itself edited
--   gets its own refresh.
liveChunkPixels ∷ WorldGenParams → MaterialRegistry → ZoomColorPalette
                → ChunkCoord → [ZoomTileOverride] → BS.ByteString
liveChunkPixels params registry palette coord overrides =
    zoomChunkPixels palette worldSize haloFluid coord overridden
  where
    worldSize = wgpWorldSize params

    overridden = foldl' overrideOne (zoomChunkPass params registry Nothing coord)
                                    overrides

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
--   overlay. Fresh stone is not iced: the column just rose out of a
--   contact hot enough to boil the water off it, and leaving a drape
--   there would colour it as snow no matter what material it is.
overrideOne ∷ ZoomChunkPass → ZoomTileOverride → ZoomChunkPass
overrideOne pass o
    | ztoIndex o < 0 ∨ ztoIndex o ≥ V.length (zcpTiles pass) = pass
    | otherwise = pass
        { zcpTiles    = zcpTiles pass V.// [(i, (ztoElev o, ztoMaterial o,
                                                 ztoVeg o, gx, gy))]
        , zcpElevs    = zcpElevs pass VU.// [(i, ztoElev o)]
        , zcpRawFluid = zcpRawFluid pass V.// [(i, ztoFluid o)]
        , zcpIceMap   = zcpIceMap pass V.// [(i, Nothing)]
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
