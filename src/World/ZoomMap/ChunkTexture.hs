{-# LANGUAGE Strict #-}
-- | Generate a texture atlas for the zoom map.
--   Each chunk contributes a zoomTileSize×zoomTileSize pixel tile to the
--   atlas. The atlas is a single large RGBA8 image packed in row-major
--   order.
--
--   Since #2020 this module does not decide its own dimensions. An
--   accepted 'MapImagePlan' — produced by "World.Map.ImagePlan" and
--   admitted against the device's real @maxImageDimension2D@ before a
--   single chunk pixel was generated — is the allocation authority, and
--   this module verifies the supplied blocks against it before it
--   allocates or copies anything.
module World.ZoomMap.ChunkTexture
    ( ZoomAtlasData(..)
    , buildZoomAtlas
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector as V
import Control.DeepSeq (NFData(..))
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import World.Map.ImagePlan
    ( MapImageLayout(..), MapImagePlan(..), MapImageRefusal
    , checkPlannedBlocks, checkPlannedCount )
import World.ZoomMap.Types (zoomTileSize)

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

-- | Build the zoom atlas from per-chunk color data, against an already
--   accepted plan.
--
--   Inputs: the accepted 'MapImagePlan' for this world's atlas, the
--   number of entries in the page's zoom CACHE, and one pixel block per
--   chunk (each @zoomTileSize * zoomTileSize * 4@ bytes).
--
--   Nothing is allocated or copied until all three agree with the plan:
--   the cache count, the block count, and every block's exact byte
--   length. A disagreement is a typed refusal, because the alternative
--   is a @copyBytes@ reading past a short block into whatever follows
--   it.
buildZoomAtlas ∷ MapImagePlan → Int → V.Vector BS.ByteString
               → Either MapImageRefusal ZoomAtlasData
buildZoomAtlas plan cacheCount chunkPixels = do
    checkPlannedCount plan "zoom cache entr(ies)" cacheCount
    checkPlannedBlocks plan chunkPixels
    let atlasW = mipWidth plan
        atlasH = mipHeight plan
        chunksPerRow = case mipLayout plan of
            LayoutTiled { milTilesPerRow = n } → n
            -- Unreachable: 'checkPlannedCount' above already refused
            -- every non-tiled plan. Answering honestly rather than
            -- partially, so this cannot become a pattern-match failure.
            LayoutWhole → 1
        atlasSize = mipByteCount plan
    pure ZoomAtlasData
        { zadWidth        = atlasW
        , zadHeight       = atlasH
        , zadChunksPerRow = chunksPerRow
        , zadPixelData    =
            assembleAtlas atlasW chunksPerRow chunkPixels atlasSize
        }

-- | Assemble per-chunk pixel blocks into a single atlas ByteString.
assembleAtlas ∷ Int → Int → V.Vector BS.ByteString → Int → BS.ByteString
assembleAtlas atlasW chunksPerRow chunkPixels totalSize =
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
