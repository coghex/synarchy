{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Preview
    ( buildPreviewImage
    , PreviewImage(..)
    ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (pokeByteOff)
import System.IO.Unsafe (unsafePerformIO)
import World.Types (ZoomChunkEntry(..), WorldGenParams(..))

-----------------------------------------------------------
-- Preview Image
-----------------------------------------------------------

data PreviewImage = PreviewImage
    { piWidth  ∷ !Int
    , piHeight ∷ !Int
    , piData   ∷ !BS.ByteString   -- ^ RGBA pixel data, length = w*h*4
    } deriving (Show)

-----------------------------------------------------------
-- Build Preview Image from Zoom Cache
--
-- The zoom cache is in grid-space chunk coordinates (ccx, ccy).
-- The isometric world's screen axes are rotated 45° from grid axes:
--   screen-X (east-west)   ∝ (ccx - ccy)  — wraps cylindrically
--   screen-Y (north-south) ∝ (ccx + ccy)  — bounded by glacier
--
-- Image dimensions = worldSize × worldSize
-- Background is transparent black (areas beyond the map).
-----------------------------------------------------------

buildPreviewImage ∷ WorldGenParams → V.Vector ZoomChunkEntry → PreviewImage
buildPreviewImage params cache =
    let worldSize = wgpWorldSize params
        imgW      = worldSize
        imgH      = worldSize
        totalBytes = imgW * imgH * 4

        pixelData = unsafePerformIO $ do
            fptr ← BSI.mallocByteString totalBytes
            withForeignPtr fptr $ \ptr → do
                -- Fill with transparent black (beyond-the-map areas)
                forM_ [0 .. imgW * imgH - 1] $ \i → do
                    pokeByteOff ptr (i * 4 + 0) (0 ∷ Word8)
                    pokeByteOff ptr (i * 4 + 1) (0 ∷ Word8)
                    pokeByteOff ptr (i * 4 + 2) (0 ∷ Word8)
                    pokeByteOff ptr (i * 4 + 3) (255 ∷ Word8)

                -- Paint each cache entry
                V.forM_ cache $ \entry → do
                    let cx = zceChunkX entry
                        cy = zceChunkY entry
                        -- Rotate from grid space to screen-aligned space
                        u = cx - cy   -- east-west (wraps)
                        v = cx + cy   -- north-south (bounded)

                        -- Wrap u into [0, worldSize) for cylindrical wrapping
                        px = ((u `mod` imgW) + imgW) `mod` imgW

                        -- Map v to pixel row, centered
                        -- v ranges from roughly -worldSize to +worldSize
                        -- Scale down by 2 to fit in imgH pixels
                        py = (v + worldSize) `div` 2

                    when (px >= 0 ∧ px < imgW ∧ py >= 0 ∧ py < imgH) $ do
                        let matId = zceTexIndex entry
                            elev  = zceElev entry
                            (r, g, b, a) = tileColor matId elev
                            idx = (py * imgW + px) * 4
                        pokeByteOff ptr (idx + 0) r
                        pokeByteOff ptr (idx + 1) g
                        pokeByteOff ptr (idx + 2) b
                        pokeByteOff ptr (idx + 3) a

            return $ BSI.fromForeignPtr fptr 0 totalBytes

    in PreviewImage imgW imgH pixelData

-----------------------------------------------------------
-- Tile Color
--
-- Ocean detection: the zoom cache stores material + elevation.
-- Ocean tiles keep their plate material (granite/diorite/gabbro)
-- but have deeply negative elevation from the ocean plate's
-- baseElev (-6000 to -3000) plus boundary effects.
-- We treat any non-special material with elev < 0 as ocean.
-----------------------------------------------------------

tileColor ∷ Word8 → Int → (Word8, Word8, Word8, Word8)
tileColor matId elev
    -- Glacier: always render as ice regardless of elevation
    | matId ≡ 250 = glacierColor elev
    -- Lava: always orange-red
    | matId ≡ 100 = (220, 100, 30, 255)
    -- Ocean: negative elevation with a normal rock material
    | elev < 0    = oceanColor elev
    -- Land: positive elevation, color by material
    | otherwise   = landColor matId elev

-- | Ocean color: deeper = darker blue
oceanColor ∷ Int → (Word8, Word8, Word8, Word8)
oceanColor elev =
    let depth = min 6000 (abs elev)  -- clamp for very deep trenches
        -- Normalize depth to 0.0-1.0 range (6000 = max expected depth)
        t = fromIntegral depth / 6000.0 ∷ Float
        r = clampByte $ round (70.0  - 50.0 * t)
        g = clampByte $ round (110.0 - 70.0 * t)
        b = clampByte $ round (200.0 - 40.0 * t)
    in (r, g, b, 255)

-- | Glacier: white with slight blue tint
glacierColor ∷ Int → (Word8, Word8, Word8, Word8)
glacierColor elev =
    let shift = clamp 0 40 (elev `div` 2)
        r = clampByte (220 + shift `div` 2)
        g = clampByte (225 + shift `div` 2)
        b = 245
    in (r, g, b, 255)

-- | Land color by material ID and elevation
landColor ∷ Word8 → Int → (Word8, Word8, Word8, Word8)
landColor matId elev = case matId of
    -- Igneous intrusive
    1   → elevTint (160, 140, 130) elev   -- granite: grey-brown
    2   → elevTint (180, 180, 175) elev   -- diorite: light grey
    3   → elevTint ( 90,  90,  95) elev   -- gabbro: dark grey

    -- Igneous extrusive
    4   → elevTint ( 60,  60,  65) elev   -- basalt: very dark
    5   → elevTint ( 30,  25,  35) elev   -- obsidian: near black

    -- Sedimentary
    10  → elevTint (210, 190, 140) elev   -- sandstone: sandy
    11  → elevTint (200, 200, 180) elev   -- limestone: pale
    12  → elevTint (130, 120, 110) elev   -- shale: dark brown

    -- Impact
    20  → elevTint (100, 110, 100) elev   -- impactite: dark green-grey

    -- Meteorite minerals
    30  → elevTint (140, 120, 100) elev   -- iron: rusty
    31  → elevTint (120, 150,  80) elev   -- olivine: olive green
    32  → elevTint ( 80, 100,  70) elev   -- pyroxene: dark green
    33  → elevTint (180, 170, 155) elev   -- feldspar: pale tan

    -- Unknown material
    _   → elevTint (150, 140, 130) elev

-- | Tint a base color by elevation (higher = brighter, capped)
elevTint ∷ (Int, Int, Int) → Int → (Word8, Word8, Word8, Word8)
elevTint (br, bg, bb) elev =
    let -- Scale elevation to a brightness shift
        -- Low land (~0-100) gets slight darkening, high (1000+) gets brightening
        shift = clamp (-30) 60 (elev `div` 20)
        r = clampByte (br + shift)
        g = clampByte (bg + shift)
        b = clampByte (bb + shift)
    in (r, g, b, 255)

-- | Clamp an Int to valid byte range
clampByte ∷ Int → Word8
clampByte x
    | x < 0     = 0
    | x > 255   = 255
    | otherwise  = fromIntegral x
