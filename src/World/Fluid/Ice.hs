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
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef')
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Constants (seaLevel)
import World.Fluid.Types (FluidCell(..), FluidType(..), IceCell(..), IceMap)
import World.Plate (isBeyondGlacier, isGlacierZone, wrapGlobalU)
import World.Weather.Types (ClimateState(..))
import World.Weather.Lookup (lookupLocalClimate, LocalClimate(..))

-----------------------------------------------------------
-- Chunk-Level Ice Computation
-----------------------------------------------------------

-- | Compute the ice overlay for a chunk based on climate data.
--   Ice forms based on effective temperature which accounts for:
--     - Regional climate (mean, summer, winter temps)
--     - Altitude (lapse rate: higher = colder)
--     - Ocean thermal inertia (ocean needs colder temps to freeze)
--     - Noise (breaks up straight latitude lines)
--
--   Glacier-zone tiles are skipped — the glacier wall is solid
--   terrain and does not need an ice overlay.
computeChunkIce ∷ Word64 → ClimateState → Int → ChunkCoord
                → VU.Vector Int                   -- ^ terrain surface map
                → V.Vector (Maybe FluidCell)      -- ^ fluid map
                → IceMap
computeChunkIce seed climate worldSize coord terrainSurfMap fluidMap =
    let ChunkCoord cx cy = coord
        area = chunkSize * chunkSize

        -- Pass 1: compute raw ice surface per tile
        rawIce = runST $ do
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

                        -- Altitude-adjusted temperature: lapse rate of
                        -- 6.5°C per 1000m (0.065°C per tile at 10m/tile).
                        -- Higher terrain is effectively colder.
                        altAboveSeaLevel = max 0 (terrainZ - seaLevel)
                        lapseRate = 0.065 ∷ Float
                        altCooling = fromIntegral altAboveSeaLevel * lapseRate

                        -- Ocean thermal inertia: ocean resists freezing.
                        -- Effectively makes ocean 5°C warmer for ice
                        -- threshold purposes.
                        isOcean = case fluidMap V.! idx of
                            Just fc → fcType fc ≡ Ocean
                            Nothing → False
                        oceanPenalty = if isOcean then 5.0 else 0.0 ∷ Float

                        -- Deterministic noise: ±2°C variation to break
                        -- up the straight latitude lines.
                        noise = iceNoise seed gx' gy'

                        -- Effective temperature for ice threshold
                        effectiveT = meanT + oceanPenalty - altCooling + noise

                        -- Ice forms where effective temp is cold enough
                        hasIce = effectiveT < -2.0
                               ∨ (winterT - altCooling < -10.0
                                  ∧ summerT - altCooling < 5.0)

                    when (hasIce ∧ terrainZ > minBound) $ do
                        let fluidZ = case fluidMap V.! idx of
                                Just fc → fcSurface fc
                                Nothing → minBound
                            baseZ = max terrainZ fluidZ
                            iceSurf = baseZ + 1
                        MV.write mv idx (Just (IceCell iceSurf))
            V.freeze mv

    in smoothIceSurface terrainSurfMap fluidMap rawIce

-----------------------------------------------------------
-- Noise
-----------------------------------------------------------

-- | Deterministic noise for ice boundary variation.
--   Returns a value in [-2.0, 2.0] to add natural irregularity.
iceNoise ∷ Word64 → Int → Int → Float
iceNoise seed gx gy =
    let -- Two octaves of hash-based noise at different scales
        h1 = iceHash seed gx gy 12     -- broad variation
        h2 = iceHash seed gx gy 5      -- fine detail
        n1 = (hashToFloat h1 - 0.5) * 3.0   -- ±1.5
        n2 = (hashToFloat h2 - 0.5) * 1.0   -- ±0.5
    in n1 + n2

-- | Simple value noise: hash at grid cell corners, bilinear interpolate.
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

-----------------------------------------------------------
-- Ice Surface Smoothing
-----------------------------------------------------------

-- | Smooth the ice surface to form a continuous sheet.
--   Averages each ice tile's surface with its ice-covered neighbors,
--   clamped to never go below the base (terrain or fluid surface).
--   Runs multiple iterations for convergence.
smoothIceSurface ∷ VU.Vector Int
                 → V.Vector (Maybe FluidCell)
                 → IceMap → IceMap
smoothIceSurface terrainSurfMap fluidMap iceMap = runST $ do
    mv ← V.thaw iceMap
    let area = chunkSize * chunkSize

        -- Compute the base Z for a tile (ice cannot go below this)
        baseAt idx =
            let terrainZ = terrainSurfMap VU.! idx
                fluidZ = case fluidMap V.! idx of
                    Just fc → fcSurface fc
                    Nothing → minBound
            in max terrainZ fluidZ

        smoothPass = do
            changed ← newSTRef False
            forM_ [0 .. area - 1] $ \idx → do
                val ← MV.read mv idx
                case val of
                    Nothing → pure ()
                    Just (IceCell mySurf) → do
                        let lx = idx `mod` chunkSize
                            ly = idx `div` chunkSize
                        -- Gather ice-covered neighbor surfaces
                        nbrSum ← newSTRef (0 ∷ Int)
                        nbrCnt ← newSTRef (0 ∷ Int)
                        let checkNbr nx ny
                              | nx < 0 ∨ nx ≥ chunkSize
                                ∨ ny < 0 ∨ ny ≥ chunkSize = pure ()
                              | otherwise = do
                                  nv ← MV.read mv (ny * chunkSize + nx)
                                  case nv of
                                      Just (IceCell ns) → do
                                          modifySTRef' nbrSum (+ ns)
                                          modifySTRef' nbrCnt (+ 1)
                                      Nothing → pure ()
                        checkNbr lx (ly - 1)
                        checkNbr lx (ly + 1)
                        checkNbr (lx + 1) ly
                        checkNbr (lx - 1) ly
                        cnt ← readSTRef nbrCnt
                        when (cnt > 0) $ do
                            total ← readSTRef nbrSum
                            let avg = (mySurf * 2 + total)
                                      `div` (2 + cnt)
                                base = baseAt idx
                                -- Never go below base + 1 (minimum
                                -- ice thickness of 1 tile)
                                newSurf = max (base + 1) avg
                            when (newSurf ≠ mySurf) $ do
                                MV.write mv idx (Just (IceCell newSurf))
                                writeSTRef changed True
            readSTRef changed

        loop 0 = pure ()
        loop n = do
            didChange ← smoothPass
            when didChange $ loop (n - 1)

    loop (5 ∷ Int)
    V.freeze mv
