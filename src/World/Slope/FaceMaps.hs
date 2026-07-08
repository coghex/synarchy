{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Slope.FaceMaps
    ( -- * Slope → Face Map Index Mapping
      slopeToFaceMapIndex
      -- * Procedural Face Map Generation
    , SlopeFaceMaps(..)
    , generateSlopeFaceMaps
      -- * Side Face Maps (left/right only, no top face)
    , generateSideFaceMapLeft
    , generateSideFaceMapRight
    ) where

import UPrelude
import qualified Data.Vector.Storable as VS

tilePixelWidth ∷ Int
tilePixelWidth = 96

tilePixelHeight ∷ Int
tilePixelHeight = 64

diamondRows ∷ Int
diamondRows = 48

-- * Slope → Face Map Index Mapping

slopeToFaceMapIndex ∷ Word8 → Int
slopeToFaceMapIndex  = fromIntegral

-- * Procedural Face Map Generation

data SlopeFaceMaps = SlopeFaceMaps
    { sfmFlat  ∷ !(VS.Vector Word8)
    , sfmNorth ∷ !(VS.Vector Word8)
    , sfmEast  ∷ !(VS.Vector Word8)
    , sfmSouth ∷ !(VS.Vector Word8)
    , sfmWest  ∷ !(VS.Vector Word8)
    } deriving (Show)

generateSlopeFaceMaps ∷ SlopeFaceMaps
generateSlopeFaceMaps = SlopeFaceMaps
    { sfmFlat  = generateFlatFaceMap
    , sfmNorth = generateRampFaceMap RampNorth
    , sfmEast  = generateRampFaceMap RampEast
    , sfmSouth = generateRampFaceMap RampSouth
    , sfmWest  = generateRampFaceMap RampWest
    }

data RampDirection = RampNorth | RampEast | RampSouth | RampWest

generateFlatFaceMap ∷ VS.Vector Word8
generateFlatFaceMap = VS.generate (tilePixelWidth * tilePixelHeight * 4) $ \i →
    let px  = i `div` 4
        col = px `mod` tilePixelWidth
        row = px `div` tilePixelWidth
        chan = i `mod` 4
    in if row < diamondRows
       then case chan of
                0 → 0
                1 → 255
                2 → 0
                _ → 255
       else let halfW = tilePixelWidth `div` 2
            in if col < halfW
               then case chan of
                   0 → 0
                   1 → 0
                   2 → 255
                   _ → 255
               else case chan of
                   0 → 255
                   1 → 0
                   2 → 0
                   _ → 255

generateRampFaceMap ∷ RampDirection → VS.Vector Word8
generateRampFaceMap dir = VS.generate (tilePixelWidth * tilePixelHeight * 4) $ \i →
    let px  = i `div` 4
        col = px `mod` tilePixelWidth
        row = px `div` tilePixelWidth
        chan = i `mod` 4
    in if row < diamondRows
       then let t = rampGradient dir col row
                green = round (255.0 * (1.0 - t * 0.7)) ∷ Int
                red   = round (255.0 * t * 0.5) ∷ Int
                blue  = round (255.0 * t * 0.5) ∷ Int
            in case chan of
                0 → clampByte red
                1 → clampByte green
                2 → clampByte blue
                _ → 255
       else let halfW = tilePixelWidth `div` 2
            in if col < halfW
               then case chan of
                   0 → 0
                   1 → 0
                   2 → 255
                   _ → 255
               else case chan of
                   0 → 255
                   1 → 0
                   2 → 0
                   _ → 255

rampGradient ∷ RampDirection → Int → Int → Float
rampGradient dir col row =
    let cx = fromIntegral tilePixelWidth / 2.0 ∷ Float
        cy = fromIntegral diamondRows / 2.0 ∷ Float
        nx = (fromIntegral col - cx) / cx
        ny = (fromIntegral row - cy) / cy
    in clamp01 $ case dir of
        RampNorth → (1.0 - ny) / 2.0
        RampSouth → (1.0 + ny) / 2.0
        RampEast  → (1.0 + nx) / 2.0
        RampWest  → (1.0 - nx) / 2.0

clampByte ∷ Int → Word8
clampByte x = fromIntegral (max 0 (min 255 x))

-- * Side Face Maps (left/right only, no top face)

-- | Left side face map: G=0 (no top), B=255 for left-side pixels only.
--   The left side face occupies the bottom-left portion of the tile sprite.
generateSideFaceMapLeft ∷ VS.Vector Word8
generateSideFaceMapLeft = VS.generate (tilePixelWidth * tilePixelHeight * 4) $ \i →
    let px  = i `div` 4
        col = px `mod` tilePixelWidth
        row = px `div` tilePixelWidth
        chan = i `mod` 4
        halfW = tilePixelWidth `div` 2
    in if row < diamondRows
       then 0  -- no top face at all
       else if col < halfW
            then case chan of  -- left side face
                0 → 0
                1 → 0
                2 → 255
                _ → 255
            else 0  -- no right side face

-- | Right side face map: G=0 (no top), R=255 for right-side pixels only.
generateSideFaceMapRight ∷ VS.Vector Word8
generateSideFaceMapRight = VS.generate (tilePixelWidth * tilePixelHeight * 4) $ \i →
    let px  = i `div` 4
        col = px `mod` tilePixelWidth
        row = px `div` tilePixelWidth
        chan = i `mod` 4
        halfW = tilePixelWidth `div` 2
    in if row < diamondRows
       then 0  -- no top face at all
       else if col ≥ halfW
            then case chan of  -- right side face
                0 → 255
                1 → 0
                2 → 0
                _ → 255
            else 0  -- no left side face
