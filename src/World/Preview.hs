{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
module World.Preview
    ( buildPreviewFromPixels
    , PreviewImage(..)
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import World.Types (WorldGenParams(..))
import World.ZoomMap.Types (ZoomChunkEntry(..), zoomTileSize)

-- * Preview Image

data PreviewImage = PreviewImage
    { piWidth  ∷ !Int
    , piHeight ∷ !Int
    , piData   ∷ !BS.ByteString   -- ^ RGBA pixel data, length = w*h*4
    } deriving (Show, Generic, NFData)

-- * Build Preview

-- | Samples the center pixel of each chunk's zoom texture to
--   produce a preview that exactly matches the zoom map.

buildPreviewFromPixels ∷ WorldGenParams → V.Vector ZoomChunkEntry
                       → V.Vector BS.ByteString → PreviewImage
buildPreviewFromPixels params cache pixels =
    let worldSize = wgpWorldSize params
        halfSize  = worldSize `div` 2
        imgW      = worldSize * 2
        imgH      = worldSize * 2
        totalBytes = imgW * imgH * 4

        -- Sample two horizontal positions from each 32×32 chunk
        -- texture. The left and right halves of the isometric
        -- diamond correspond to different tiles along the u-axis,
        -- giving real additional horizontal resolution.
        midY    = zoomTileSize `div` 2
        leftX   = zoomTileSize `div` 4       -- col 8 of 32
        rightX  = 3 * zoomTileSize `div` 4   -- col 24 of 32
        leftOff  = (midY * zoomTileSize + leftX) * 4
        rightOff = (midY * zoomTileSize + rightX) * 4

        sampleAt chunkPx off =
            if off + 3 < BS.length chunkPx
            then ( BS.index chunkPx off
                 , BS.index chunkPx (off + 1)
                 , BS.index chunkPx (off + 2)
                 , BS.index chunkPx (off + 3) )
            else (0, 0, 0, 255)

        pixelData = BSI.unsafeCreate totalBytes $ \ptr → do
                -- Fill background
                forM_ [0 .. imgW * imgH - 1] $ \i → do
                    pokeByteOff ptr (i * 4 + 0) (0 ∷ Word8)
                    pokeByteOff ptr (i * 4 + 1) (0 ∷ Word8)
                    pokeByteOff ptr (i * 4 + 2) (0 ∷ Word8)
                    pokeByteOff ptr (i * 4 + 3) (255 ∷ Word8)

                V.iforM_ cache $ \i entry → do
                    let cx = zceChunkX entry
                        cy = zceChunkY entry
                        u = cx - cy
                        v = cx + cy
                        uWrapped = (((u + halfSize) `mod` worldSize) + worldSize)
                                   `mod` worldSize
                        px = uWrapped * 2
                        py = (v + halfSize) * 2

                    when (py >= 0 ∧ py + 1 < imgH ∧ i < V.length pixels) $ do
                        let chunkPx = pixels V.! i
                            (lr, lg, lb, la) = sampleAt chunkPx leftOff
                            (rr, rg, rb, ra) = sampleAt chunkPx rightOff

                            writePixel x y r g b a = do
                                let wx = x `mod` imgW
                                when (wx >= 0 ∧ wx < imgW ∧ y >= 0 ∧ y < imgH) $ do
                                    let idx = (y * imgW + wx) * 4
                                    pokeByteOff ptr (idx + 0) r
                                    pokeByteOff ptr (idx + 1) g
                                    pokeByteOff ptr (idx + 2) b
                                    pokeByteOff ptr (idx + 3) a
                        -- Left half of chunk → first 2 pixel columns
                        writePixel px       py lr lg lb la
                        writePixel (px + 1) py lr lg lb la
                        writePixel px       (py + 1) lr lg lb la
                        writePixel (px + 1) (py + 1) lr lg lb la
                        -- Right half of chunk → next 2 pixel columns
                        writePixel (px + 2) py rr rg rb ra
                        writePixel (px + 3) py rr rg rb ra
                        writePixel (px + 2) (py + 1) rr rg rb ra
                        writePixel (px + 3) (py + 1) rr rg rb ra

    in PreviewImage imgW imgH pixelData
