{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Per-chunk RGBA pixel generation, split out of
--   "World.ZoomMap.Cache" (issue #573).
module World.ZoomMap.Cache.Pixels
    ( generateChunkPixels
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import World.Types
import World.Fluid.Internal (FluidMap)
import World.Vegetation (vegVariants)
import World.ZoomMap.ColorPalette (ZoomColorPalette, lookupMatColor
                                  , lookupVegColorById
                                  , defaultOceanColor, defaultLavaColor)

-- | Generate zoomTileSize×zoomTileSize RGBA pixel data for a single chunk.
--   Uses inverse isometric transform to map screen-space pixels
--   back to grid tiles, producing a diamond-shaped image.
generateChunkPixels ∷ ZoomColorPalette → Bool
                    → Int → FluidMap → IceMap
                    → V.Vector (Int, Word8, Word8, Int, Int)
                    → BS.ByteString
generateChunkPixels palette hasLava _worldSize fluidMap iceMap tileVec =
    BL.toStrict $ BB.toLazyByteString $ mconcat
        [ pixelAt px py
        | py ← [0 .. zoomTileSize - 1]
        , px ← [0 .. zoomTileSize - 1]
        ]
  where
    cs ∷ Float
    cs = fromIntegral chunkSize
    ts ∷ Float
    ts = fromIntegral zoomTileSize

    pixelAt ∷ Int → Int → BB.Builder
    pixelAt px py =
        let u = (fromIntegral px + 0.5) / ts
            v = (fromIntegral py + 0.5) / ts
            -- Inverse isometric: UV bounding box → grid-local coords
            gxF = cs * (u + v - 0.5)
            gyF = cs * (v - u + 0.5)
            lx = floor gxF ∷ Int
            ly = floor gyF ∷ Int
        in if lx < 0 ∨ lx ≥ chunkSize ∨ ly < 0 ∨ ly ≥ chunkSize
           then BB.word8 0 <> BB.word8 0 <> BB.word8 0 <> BB.word8 0
           else let idx = ly * chunkSize + lx
                    (elev, matId, vegId, gx, gy) = tileVec V.! idx
               -- Beyond-glacier tiles (minBound elevation) are outside the
               -- world diamond — render them transparent so ocean fluid
               -- doesn't bleed through at the south pole boundary.
               in if elev ≡ minBound
               -- Beyond-glacier tiles are outside the world diamond —
               -- render as glacier ice (snow veg color) so the boundary
               -- is solid and matches adjacent glacier zone tiles.
               then let (gr, gg, gb, ga) = case lookupVegColorById palette 65 of
                            Just c  → c
                            Nothing → (169, 189, 199, 255)
                    in BB.word8 gr <> BB.word8 gg <> BB.word8 gb <> BB.word8 ga
               else
                let tileIsOcean = case fluidMap V.! idx of
                        Just (FluidCell Ocean surf) → elev ≤ surf
                        _ → False
                    hasIce = isJust (iceMap V.! idx)
                    baseColor
                      | tileIsOcean ∧ not hasIce = defaultOceanColor
                      | otherwise = tileColor palette hasLava
                                        matId vegId elev gx gy
                    (r, g, b, a)
                        | hasIce = baseColor
                        | otherwise = case fluidMap V.! idx of
                            Just fc | fcType fc ≢ Ocean →
                                let blend = 0.7 ∷ Float
                                    (lr, lg, lb, la) = baseColor
                                    -- River + lake blend toward water blue;
                                    -- lava blends toward the lava palette
                                    -- color so volcanic chunks are visible
                                    -- on the zoom map as red-orange instead
                                    -- of looking like another lake.
                                    (tr, tg, tb) = case fcType fc of
                                        Lava → defaultLavaColor3
                                        _    → waterBlue3
                                in ( round (fromIntegral lr * (1.0 - blend)
                                          + fromIntegral tr * blend ∷ Float)
                                   , round (fromIntegral lg * (1.0 - blend)
                                          + fromIntegral tg * blend ∷ Float)
                                   , round (fromIntegral lb * (1.0 - blend)
                                          + fromIntegral tb * blend ∷ Float)
                                   , la )
                            _ → baseColor
                in BB.word8 r <> BB.word8 g <> BB.word8 b <> BB.word8 a

-- | Blend target for river / lake tiles: deep water blue.
waterBlue3 ∷ (Word8, Word8, Word8)
waterBlue3 = (50, 90, 170)

-- | Blend target for lava tiles: 'defaultLavaColor' minus the alpha.
defaultLavaColor3 ∷ (Word8, Word8, Word8)
defaultLavaColor3 = let (r, g, b, _) = defaultLavaColor in (r, g, b)

-- | Determine the color for a single non-ocean tile.
--   Ocean rendering is handled per tile in generateChunkPixels via
--   the compose fluid map, not here.
tileColor ∷ ZoomColorPalette → Bool → Word8 → Word8 → Int → Int → Int
          → (Word8, Word8, Word8, Word8)
tileColor palette _hasLava matId vegId _elev _gx _gy
    -- Snow-covered tiles (including frozen ocean) use snow color
    | isSnowVeg vegId =
        case lookupVegColorById palette vegId of
            Just vegColor → vegColor
            Nothing       → (220, 225, 235, 255)
    | otherwise =
        case lookupVegColorById palette vegId of
            Just vegColor → blendVegMat vegId vegColor (lookupMatColor palette matId)
            Nothing       → lookupMatColor palette matId

-- | Blend vegetation color with material color based on veg type density.
blendVegMat ∷ Word8 → (Word8,Word8,Word8,Word8) → (Word8,Word8,Word8,Word8)
            → (Word8,Word8,Word8,Word8)
blendVegMat vegId (vr,vg,vb,va) (mr,mg,mb,ma) =
    let vegWeight = vegDensityWeight vegId
        matWeight = 1.0 - vegWeight
        blend v m = round (fromIntegral v * vegWeight + fromIntegral m * matWeight)
    in (blend vr mr, blend vg mg, blend vb mb, max va ma)

-- | Determine vegetation blend weight from the veg type.
vegDensityWeight ∷ Word8 → Float
vegDensityWeight 0 = 0.0
vegDensityWeight vegId =
    let base = ((vegId - 1) `div` vegVariants) * vegVariants + 1
    in case base of
        1  → 0.3   -- sparse grass
        5  → 0.5   -- medium grass
        9  → 0.8   -- dense grass
        13 → 0.6   -- tall grass
        17 → 0.3   -- thin moss
        21 → 0.6   -- thick moss
        25 → 0.3   -- light ivy
        29 → 0.6   -- heavy ivy
        33 → 0.2   -- lichen
        37 → 0.2   -- desert scrub
        41 → 0.6   -- marsh grass
        45 → 0.3   -- dead grass
        49 → 0.4   -- fallen leaves
        53 → 0.5   -- pine needles
        57 → 0.3   -- mushroom patch
        61 → 0.4   -- wildflowers
        65 → 0.9   -- snow
        69 → 0.8   -- desert sand
        73 → 0.7   -- gravel tundra
        _  → 0.3

-- | Check if a vegetation ID is snow (IDs 65-68).
isSnowVeg ∷ Word8 → Bool
isSnowVeg v = v ≥ 65 ∧ v ≤ 68
