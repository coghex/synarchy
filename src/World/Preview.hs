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
--   screen-X (east-west)   ∝ (ccx - ccy)
--   screen-Y (north-south) ∝ (ccx + ccy)
--
-- We rotate into screen-aligned coordinates so the preview
-- looks like the zoomed-out map the player sees in-game,
-- rather than a 45°-tilted diamond.
-----------------------------------------------------------

buildPreviewImage ∷ WorldGenParams → V.Vector ZoomChunkEntry → PreviewImage
buildPreviewImage params cache =
    let worldSize = wgpWorldSize params
        halfSize  = worldSize `div` 2

        -- Screen-aligned coordinates:
        --   u = ccx - ccy  (east-west, wraps with period worldSize)
        --   v = ccx + ccy  (north-south, bounded by glacier)
        --
        -- The world wraps cylindrically in X (grid-space ccx).
        -- In screen-aligned space, this means u wraps with period worldSize.
        -- Image width = worldSize (one full wrap around the cylinder).
        --
        -- The north-south range v = ccx + ccy spans [-2*halfSize+1 .. 2*halfSize-1]
        -- but isBeyondGlacier clips to |v*chunkSize| <= halfTiles,
        -- so the actual populated range is roughly [-worldSize .. worldSize].
        -- We use worldSize for height too (matching the playable area).
        imgW      = worldSize
        imgH      = worldSize
        totalBytes = imgW * imgH * 4

        -- v offset: v ranges from roughly -worldSize to +worldSize
        -- but the playable area (after glacier clipping) fits in worldSize rows.
        -- Center it: py = (v + worldSize) / 2, clamped to [0, imgH)
        vOffset   = worldSize `div` 2

        pixelData = unsafePerformIO $ do
            fptr ← BSI.mallocByteString totalBytes
            -- Fill with ocean blue default
            withForeignPtr fptr $ \ptr → do
                forM_ [0 .. imgW * imgH - 1] $ \i → do
                    let (r, g, b, a) = oceanColor (-5)
                    pokeByteOff ptr (i * 4 + 0) r
                    pokeByteOff ptr (i * 4 + 1) g
                    pokeByteOff ptr (i * 4 + 2) b
                    pokeByteOff ptr (i * 4 + 3) a

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
                            (r, g, b, a) = materialColor matId elev
                            idx = (py * imgW + px) * 4
                        pokeByteOff ptr (idx + 0) r
                        pokeByteOff ptr (idx + 1) g
                        pokeByteOff ptr (idx + 2) b
                        pokeByteOff ptr (idx + 3) a

            return $ BSI.fromForeignPtr fptr 0 totalBytes

    in PreviewImage imgW imgH pixelData

-----------------------------------------------------------
-- Material → Color Mapping
-----------------------------------------------------------

materialColor ∷ Word8 → Int → (Word8, Word8, Word8, Word8)
materialColor matId elev
    -- Ocean: elevation below sea level
    | elev < 0  = oceanColor elev
    | otherwise = case matId of
        -- Igneous intrusive
        1   → elevTint ( 160, 140, 130 ) elev   -- granite: grey-brown
        2   → elevTint ( 180, 180, 175 ) elev   -- diorite: light grey
        3   → elevTint (  90,  90,  95 ) elev   -- gabbro: dark grey

        -- Igneous extrusive
        4   → elevTint (  60,  60,  65 ) elev   -- basalt: very dark
        5   → elevTint (  30,  25,  35 ) elev   -- obsidian: near black

        -- Sedimentary
        10  → elevTint ( 210, 190, 140 ) elev   -- sandstone: sandy
        11  → elevTint ( 200, 200, 180 ) elev   -- limestone: pale
        12  → elevTint ( 130, 120, 110 ) elev   -- shale: dark brown

        -- Impact
        20  → elevTint ( 100, 110, 100 ) elev   -- impactite: dark green-grey

        -- Meteorite minerals
        30  → elevTint ( 140, 120, 100 ) elev   -- iron: rusty
        31  → elevTint ( 120, 150,  80 ) elev   -- olivine: olive green
        32  → elevTint (  80, 100,  70 ) elev   -- pyroxene: dark green
        33  → elevTint ( 180, 170, 155 ) elev   -- feldspar: pale tan

        -- Special
        100 → ( 220, 100,  30, 255 )            -- lava: orange-red
        250 → glacierColor elev                  -- glacier: white-blue

        -- Unknown
        _   → elevTint ( 150, 140, 130 ) elev

-- | Tint a base color by elevation (higher = brighter, capped)
elevTint ∷ (Int, Int, Int) → Int → (Word8, Word8, Word8, Word8)
elevTint (br, bg, bb) elev =
    let shift = clamp (-40) 60 (elev * 2)
        r = clamp 0 255 (br + shift)
        g = clamp 0 255 (bg + shift)
        b = clamp 0 255 (bb + shift)
    in (fromIntegral r, fromIntegral g, fromIntegral b, 255)

-- | Ocean color: deeper = darker blue
oceanColor ∷ Int → (Word8, Word8, Word8, Word8)
oceanColor elev =
    let depth = abs elev
        r = clamp 0 255 (40  - depth * 2)
        g = clamp 0 255 (80  - depth * 2)
        b = clamp 0 255 (180 - depth * 1)
    in (fromIntegral r, fromIntegral g, fromIntegral b, 255)

-- | Glacier: white with slight blue tint at higher elevations
glacierColor ∷ Int → (Word8, Word8, Word8, Word8)
glacierColor elev =
    let shift = clamp 0 40 (elev `div` 2)
        r = clamp 180 255 (230 + shift `div` 2)
        g = clamp 180 255 (235 + shift `div` 2)
        b = 255
    in (fromIntegral r, fromIntegral g, fromIntegral b, 255)

-- | Clamp a value to [lo, hi]
clamp ∷ Int → Int → Int → Int
clamp lo hi x
    | x < lo    = lo
    | x > hi    = hi
    | otherwise = x
