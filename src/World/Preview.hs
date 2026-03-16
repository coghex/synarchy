{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module World.Preview
    ( buildPreviewImage
    , buildPreviewFromPixels
    , PreviewImage(..)
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (pokeByteOff)
import System.IO.Unsafe (unsafePerformIO)
import World.Types (ZoomChunkEntry(..), WorldGenParams(..))
import World.Render.Zoom.Types (zoomTileSize)

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

        pixelData = unsafePerformIO $ do
            fptr ← BSI.mallocByteString totalBytes
            withForeignPtr fptr $ \ptr → do
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

            return $ BSI.fromForeignPtr fptr 0 totalBytes

    in PreviewImage imgW imgH pixelData

-- * Fallback: Build Preview from ZoomChunkEntry summary (used when per-chunk pixel data is not available)

buildPreviewImage ∷ WorldGenParams → V.Vector ZoomChunkEntry → PreviewImage
buildPreviewImage params cache =
    let worldSize = wgpWorldSize params
        halfSize  = worldSize `div` 2
        imgW      = worldSize
        imgH      = worldSize
        totalBytes = imgW * imgH * 4

        pixelData = unsafePerformIO $ do
            fptr ← BSI.mallocByteString totalBytes
            withForeignPtr fptr $ \ptr → do
                forM_ [0 .. imgW * imgH - 1] $ \i → do
                    pokeByteOff ptr (i * 4 + 0) (0 ∷ Word8)
                    pokeByteOff ptr (i * 4 + 1) (0 ∷ Word8)
                    pokeByteOff ptr (i * 4 + 2) (0 ∷ Word8)
                    pokeByteOff ptr (i * 4 + 3) (255 ∷ Word8)

                V.forM_ cache $ \entry → do
                    let cx = zceChunkX entry
                        cy = zceChunkY entry
                        u = cx - cy
                        v = cx + cy
                        px = (((u + halfSize) `mod` imgW) + imgW) `mod` imgW
                        py = v + halfSize

                    when (py >= 0 ∧ py < imgH) $ do
                        let matId   = zceTexIndex entry
                            elev    = zceElev entry
                            hasIce  = zceHasIce entry
                            (r, g, b, a) = fallbackColor matId elev hasIce
                            writePixel x = when (x >= 0 ∧ x < imgW) $ do
                                let idx = (py * imgW + x) * 4
                                pokeByteOff ptr (idx + 0) r
                                pokeByteOff ptr (idx + 1) g
                                pokeByteOff ptr (idx + 2) b
                                pokeByteOff ptr (idx + 3) a
                        writePixel px
                        writePixel ((px + 1) `mod` imgW)

            return $ BSI.fromForeignPtr fptr 0 totalBytes

    in PreviewImage imgW imgH pixelData

-- | Simple color for fallback preview (no per-chunk pixels available)
fallbackColor ∷ Word8 → Int → Bool → (Word8, Word8, Word8, Word8)
fallbackColor matId elev hasIce
    | matId ≡ 250 = (220, 225, 245, 255)   -- glacier
    | hasIce      = (200, 215, 235, 255)   -- ice
    | matId ≡ 0   = (30, 60, 120, 255)     -- ocean
    | otherwise   =                         -- land
        let shift = clamp (-30) 60 (elev `div` 20)
            r = clampByte (140 + shift)
            g = clampByte (130 + shift)
            b = clampByte (100 + shift)
        in (r, g, b, 255)

clampByte ∷ Int → Word8
clampByte x
    | x < 0     = 0
    | x > 255   = 255
    | otherwise  = fromIntegral x
