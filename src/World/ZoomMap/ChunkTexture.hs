{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Generate a texture atlas for the zoom map.
--   Each chunk contributes a zoomTileSize×zoomTileSize pixel tile to the atlas.
--   The atlas is a single large RGBA8 image packed in row-major order.
module World.ZoomMap.ChunkTexture
    ( ZoomAtlasData(..)
    , buildZoomAtlas
    , chunkAtlasUVs
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector as V
import Control.DeepSeq (NFData(..))
import Foreign.Ptr (plusPtr)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import World.Types (ChunkCoord(..), chunkSize, zoomTileSize)
import World.ZoomMap.ColorPalette

-- | The atlas image data ready for GPU upload.
data ZoomAtlasData = ZoomAtlasData
    { zadWidth       ∷ !Int              -- ^ Atlas width in pixels
    , zadHeight      ∷ !Int              -- ^ Atlas height in pixels
    , zadChunksPerRow ∷ !Int             -- ^ Number of chunk tiles per row
    , zadPixelData   ∷ !BS.ByteString    -- ^ RGBA8 pixel data
    } deriving (Show)

instance NFData ZoomAtlasData where
    rnf (ZoomAtlasData w h cpr d) =
        rnf w `seq` rnf h `seq` rnf cpr `seq` rnf d

-- * Atlas Construction

-- | Build the zoom atlas from per-chunk color data.
--   Each chunk's 16×16 pixel tile is generated from the
--   material and vegetation at each tile position.
--
--   Input: a vector of per-chunk pixel data (each 16×16×4 bytes)
--   plus the total number of chunks.
buildZoomAtlas ∷ Int → V.Vector BS.ByteString → ZoomAtlasData
buildZoomAtlas numChunks chunkPixels =
    let chunksPerRow = ceilSqrt numChunks
        atlasRowCount = (numChunks + chunksPerRow - 1) `div` chunksPerRow
        atlasW = chunksPerRow * zoomTileSize
        atlasH = atlasRowCount * zoomTileSize
        -- Allocate the full atlas
        atlasSize = atlasW * atlasH * 4  -- RGBA8
        atlas = assembleAtlas atlasW atlasH chunksPerRow chunkPixels atlasSize
    in ZoomAtlasData
        { zadWidth       = atlasW
        , zadHeight      = atlasH
        , zadChunksPerRow = chunksPerRow
        , zadPixelData   = atlas
        }

-- | Assemble per-chunk pixel blocks into a single atlas ByteString.
assembleAtlas ∷ Int → Int → Int → V.Vector BS.ByteString → Int → BS.ByteString
assembleAtlas atlasW _atlasH chunksPerRow chunkPixels totalSize =
    BSI.unsafeCreate totalSize $ \destPtr → do
        -- Zero-fill (for any padding chunks at the end)
        fillBytes destPtr 0 totalSize
        -- Copy each chunk's pixel data into the atlas
        V.iforM_ chunkPixels $ \i chunkBS → do
            let col = i `mod` chunksPerRow
                row = i `div` chunksPerRow
            -- Copy row-by-row within the chunk tile
            BS.useAsCStringLen chunkBS $ \(srcBasePtr, _srcLen) → do
                let tileStride = zoomTileSize * 4  -- bytes per row within the chunk
                    atlasStride = atlasW * 4        -- bytes per row in the atlas
                forM_ [0 .. zoomTileSize - 1] $ \ty → do
                    let srcOff  = ty * tileStride
                        destX   = col * zoomTileSize
                        destY   = row * zoomTileSize + ty
                        destOff = destY * atlasStride + destX * 4
                    copyBytes (destPtr `plusPtr` destOff)
                              (srcBasePtr `plusPtr` srcOff)
                              tileStride

-- * UV Computation

-- | Compute UV coordinates for a chunk at the given index
--   within the atlas.  Returns (u0, v0, u1, v1).
chunkAtlasUVs ∷ ZoomAtlasData → Int → (Float, Float, Float, Float)
chunkAtlasUVs atlas idx =
    let cpr = zadChunksPerRow atlas
        col = idx `mod` cpr
        row = idx `div` cpr
        aw  = fromIntegral (zadWidth atlas)
        ah  = fromIntegral (zadHeight atlas)
        u0  = fromIntegral (col * zoomTileSize) / aw
        v0  = fromIntegral (row * zoomTileSize) / ah
        u1  = fromIntegral ((col + 1) * zoomTileSize) / aw
        v1  = fromIntegral ((row + 1) * zoomTileSize) / ah
    in (u0, v0, u1, v1)

-- * Helpers

ceilSqrt ∷ Int → Int
ceilSqrt n = ceiling (sqrt (fromIntegral n ∷ Double))
