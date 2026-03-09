{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module World.Preview
    ( buildPreviewImage
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

-----------------------------------------------------------
-- Preview Image
-----------------------------------------------------------

data PreviewImage = PreviewImage
    { piWidth  ∷ !Int
    , piHeight ∷ !Int
    , piData   ∷ !BS.ByteString   -- ^ RGBA pixel data, length = w*h*4
    } deriving (Show, Generic, NFData)

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
        halfSize  = worldSize `div` 2
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

                        -- Wrap u into [0, worldSize), centered so u=0 is mid-image
                        px = (((u + halfSize) `mod` imgW) + imgW) `mod` imgW

                        -- Map v to pixel row, centered
                        -- v ranges from roughly -worldSize to +worldSize
                        -- Scale down by 2 to fit in imgH pixels
                        py = (v + worldSize) `div` 2

                    when (px >= 0 ∧ px < imgW ∧ py >= 0 ∧ py < imgH) $ do
                        let matId    = zceTexIndex entry
                            elev     = zceElev entry
                            isOcean  = zceIsOcean entry
                            hasRiver = zceHasRiver entry
                            hasLake  = zceHasLake entry
                            vegCat   = zceVegCategory entry
                            (r, g, b, a) = tileColor matId elev isOcean
                                               hasRiver hasLake vegCat
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
-- Uses the isOcean/hasLava flags from the zoom cache for
-- accurate ocean and lava detection.
-----------------------------------------------------------

tileColor ∷ Word8 → Int → Bool → Bool → Bool → Word8
          → (Word8, Word8, Word8, Word8)
tileColor matId elev isOcean hasRiver hasLake vegCat
    -- Glacier: always render as ice regardless of elevation
    | matId ≡ 250 = glacierColor elev
    -- Lava material: orange-red
    | matId ≡ 100 = (220, 100, 30, 255)
    -- Lake: calm blue (checked before ocean so inland lakes stand out)
    | hasLake ∧ not isOcean = (60, 100, 180, 255)
    -- Ocean: depth-based blue
    | isOcean     = oceanColor elev
    -- River: blue-tinted land
    | hasRiver    = (50, 90, 170, 255)
    -- Land: material + elevation + vegetation tint
    | otherwise   = vegTint vegCat (landColor matId elev)

-- | Blend vegetation green onto a base land color
vegTint ∷ Word8 → (Word8, Word8, Word8, Word8) → (Word8, Word8, Word8, Word8)
vegTint 0 base = base  -- no vegetation
vegTint cat (br, bg, bb, ba) =
    let -- Vegetation green targets by category
        (gr, gg, gb, t) = case cat of
            1 → ( 90, 120,  60, 0.20 ∷ Float)  -- sparse: subtle olive
            2 → ( 70, 130,  50, 0.35 ∷ Float)  -- medium: green
            3 → ( 40, 110,  35, 0.50 ∷ Float)  -- dense: lush
            4 → ( 60, 100,  70, 0.40 ∷ Float)  -- marsh: dark teal-green
            _ → ( 90, 120,  60, 0.20 ∷ Float)  -- fallback
        s = 1.0 - t
        r = clampByte $ round (fromIntegral br * s + fromIntegral gr * t)
        g = clampByte $ round (fromIntegral bg * s + fromIntegral gg * t)
        b = clampByte $ round (fromIntegral bb * s + fromIntegral gb * t)
    in (r, g, b, ba)

-- | Ocean color: deeper = darker blue
oceanColor ∷ Int → (Word8, Word8, Word8, Word8)
oceanColor elev =
    let depth = min 6000 (abs elev)
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

    -- Sedimentary rock
    10  → elevTint (210, 190, 140) elev   -- sandstone: sandy
    11  → elevTint (200, 200, 180) elev   -- limestone: pale
    12  → elevTint (130, 120, 110) elev   -- shale: dark brown

    -- Sedimentary (additional)
    20  → elevTint (210, 190, 140) elev   -- sandstone (alt ID)
    21  → elevTint (170, 160, 140) elev   -- siltstone
    22  → elevTint (130, 120, 110) elev   -- shale
    23  → elevTint (180, 160, 130) elev   -- conglomerate
    24  → elevTint (150, 130, 110) elev   -- mudstone
    25  → elevTint (160, 140, 120) elev   -- claystone
    26  → elevTint (200, 200, 180) elev   -- limestone
    27  → elevTint (220, 215, 200) elev   -- chalk
    28  → elevTint (140, 130, 110) elev   -- chert

    -- Meteorite minerals
    30  → elevTint (140, 120, 100) elev   -- iron: rusty
    31  → elevTint (120, 150,  80) elev   -- olivine: olive green
    32  → elevTint ( 80, 100,  70) elev   -- pyroxene: dark green
    33  → elevTint (180, 170, 155) elev   -- feldspar: pale tan

    -- Metamorphic
    40  → elevTint (140, 135, 140) elev   -- marble
    41  → elevTint (110, 105, 115) elev   -- slate
    42  → elevTint (120, 115, 120) elev   -- quartzite
    43  → elevTint (130, 120, 130) elev   -- schist
    44  → elevTint (140, 130, 135) elev   -- gneiss
    45  → elevTint (120, 115, 110) elev   -- phyllite

    -- Soil types
    50  → elevTint (160, 120,  80) elev   -- clay: reddish-brown
    51  → elevTint (170, 130,  85) elev   -- sandy clay
    52  → elevTint (175, 140,  90) elev   -- sandy clay loam
    53  → elevTint (185, 155, 110) elev   -- sandy loam
    54  → elevTint (190, 165, 120) elev   -- loamy sand
    55  → elevTint (210, 195, 150) elev   -- sand: pale yellow
    56  → elevTint (150, 120,  80) elev   -- loam: rich brown
    57  → elevTint (155, 115,  75) elev   -- clay loam
    58  → elevTint (145, 110,  75) elev   -- silty clay
    59  → elevTint (150, 115,  80) elev   -- silty clay loam
    60  → elevTint (165, 140, 100) elev   -- silt loam
    61  → elevTint (170, 150, 115) elev   -- silt
    62  → elevTint ( 80,  70,  50) elev   -- peat: very dark brown
    63  → elevTint ( 70,  60,  45) elev   -- mucky peat
    64  → elevTint ( 60,  55,  40) elev   -- muck: near black

    -- Gravel / salt
    65  → elevTint (150, 145, 135) elev   -- heavy gravel
    66  → elevTint (175, 170, 160) elev   -- light gravel
    67  → elevTint (230, 225, 215) elev   -- salt flat: near white

    -- Glacial drift
    110 → elevTint (155, 145, 130) elev   -- till
    111 → elevTint (160, 150, 135) elev   -- moraine
    112 → elevTint (140, 125, 110) elev   -- glacial clay
    113 → elevTint (170, 165, 150) elev   -- outwash gravel

    -- Unknown material
    _   → elevTint (150, 140, 130) elev

-- | Tint a base color by elevation (higher = brighter, capped)
elevTint ∷ (Int, Int, Int) → Int → (Word8, Word8, Word8, Word8)
elevTint (br, bg, bb) elev =
    let shift = clamp (-30) 60 (elev `div` 20)
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
