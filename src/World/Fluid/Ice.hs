{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Fluid.Ice
    ( computeChunkIce
    ) where

import UPrelude
import Data.Bits (xor, shiftR, (.&.))
import Data.Word (Word64)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Constants (seaLevel)
import World.Fluid.Types (FluidCell(..), IceCell(..), IceMode(..), IceMap
                         , IceLevelGrid)
import World.Fluid.IceLevel (lookupIceLevel)
import World.Plate (TectonicPlate, isBeyondGlacier, isGlacierZone
                   , wrapGlobalU, elevationAtGlobal)
import World.Weather.Types (ClimateState)
import World.Weather.Lookup (lookupLocalClimate, LocalClimate(..))

-- * Chunk-Level Ice Computation

-- | Compute the ice overlay for a chunk.
--   Two ice modes:
--     - Basin ice: flat sheet filling valleys at the global ice level
--     - Drape ice: thin coating (terrain+1) on peaks above basin level
--
--   The global ice level grid (from fillDepressions on frozen cells)
--   determines basin fill heights. Tiles below the fill level get
--   basin ice; tiles above get drape ice.
computeChunkIce ∷ Word64 → [TectonicPlate] → ClimateState → Int → ChunkCoord
                → IceLevelGrid                 -- ^ global ice fill levels
                → VU.Vector Int                -- ^ terrain surface map
                → V.Vector (Maybe FluidCell)   -- ^ fluid map
                → IceMap
computeChunkIce seed plates climate worldSize coord ilGrid terrainSurfMap fluidMap =
    let ChunkCoord cx cy = coord
        area = chunkSize * chunkSize
        maxBasinThickness = 20 ∷ Int
    in runST $ do
        mv ← MV.replicate area Nothing
        forM_ [0 .. area - 1] $ \idx → do
            let lx = idx `mod` chunkSize
                ly = idx `div` chunkSize
                gx = cx * chunkSize + lx
                gy = cy * chunkSize + ly
                (gx', gy') = wrapGlobalU worldSize gx gy
            -- Skip beyond-glacier and glacier-zone tiles
            when (not (isBeyondGlacier worldSize gx' gy')
                  ∧ not (isGlacierZone worldSize gx' gy')) $ do
                let LocalClimate{ lcTemp=meanT
                                , lcSummerTemp=summerT
                                , lcWinterTemp=winterT } =
                        lookupLocalClimate climate worldSize gx' gy'
                    terrainZ = terrainSurfMap VU.! idx

                    (globalElev, _) = elevationAtGlobal seed plates worldSize gx' gy'
                    altAboveSeaLevel = max 0 (globalElev - seaLevel)
                    lapseRate = 0.065 ∷ Float
                    altCooling = fromIntegral altAboveSeaLevel * lapseRate

                    oceanPenalty = if globalElev < seaLevel
                                   then 5.0 else 0.0 ∷ Float

                    noise = iceNoise seed gx' gy'

                    effectiveT = meanT + oceanPenalty - altCooling + noise

                    hasIce = effectiveT < -2.0
                           ∨ (winterT - altCooling < -10.0
                              ∧ summerT - altCooling < 5.0)

                when (hasIce ∧ terrainZ > minBound) $ do
                    let fluidZ = case fluidMap V.! idx of
                            Just fc → fcSurface fc
                            Nothing → minBound
                        baseZ = max terrainZ fluidZ
                        mIceLevel = lookupIceLevel ilGrid worldSize gx' gy'
                        cell = case mIceLevel of
                            Just iceLevel | baseZ < iceLevel →
                                -- Basin ice: flat at the fill level
                                IceCell (min iceLevel (baseZ + maxBasinThickness))
                                        BasinIce
                            _ →
                                -- Drape ice: thin coating on terrain
                                IceCell (baseZ + 1) DrapeIce
                    MV.write mv idx (Just cell)
        V.freeze mv

-- * Noise

iceNoise ∷ Word64 → Int → Int → Float
iceNoise seed gx gy =
    let h1 = iceHash seed gx gy 12
        h2 = iceHash seed gx gy 5
        h3 = iceHash seed gx gy 2
        n1 = (hashToFloat h1 - 0.5) * 3.0
        n2 = (hashToFloat h2 - 0.5) * 1.0
        n3 = (hashToFloat h3 - 0.5) * 1.0
    in n1 + n2 + n3

iceHash ∷ Word64 → Int → Int → Int → Word64
iceHash seed gx gy scale =
    let fx = fromIntegral gx / fromIntegral scale ∷ Float
        fy = fromIntegral gy / fromIntegral scale ∷ Float
        ix = floor fx ∷ Int
        iy = floor fy ∷ Int
        tx = fx - fromIntegral ix
        ty = fy - fromIntegral iy
        sx = smoothstep tx
        sy = smoothstep ty
        v00 = tileHash seed ix       iy
        v10 = tileHash seed (ix + 1) iy
        v01 = tileHash seed ix       (iy + 1)
        v11 = tileHash seed (ix + 1) (iy + 1)
        f00 = hashToFloat v00
        f10 = hashToFloat v10
        f01 = hashToFloat v01
        f11 = hashToFloat v11
        top    = f00 + sx * (f10 - f00)
        bottom = f01 + sx * (f11 - f01)
        result = top + sy * (bottom - top)
    in round (result * 0xFFFFFF) ∷ Word64

tileHash ∷ Word64 → Int → Int → Word64
tileHash seed x y =
    let h0 = seed `xor` 0x1CE1CE1CE
        h1 = h0 `xor` (fromIntegral x * 0x517cc1b727220a95)
        h2 = h1 `xor` (fromIntegral y * 0x6c62272e07bb0142)
        h3 = h2 `xor` (h2 `shiftR` 33)
        h4 = h3 * 0xff51afd7ed558ccd
        h5 = h4 `xor` (h4 `shiftR` 33)
    in h5

hashToFloat ∷ Word64 → Float
hashToFloat h = fromIntegral (h .&. 0x00FFFFFF) / fromIntegral (0x00FFFFFF ∷ Word64)

smoothstep ∷ Float → Float
smoothstep t = t * t * (3.0 - 2.0 * t)
