{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Magma.Field
    ( mantleZ
    , mantleNoise
    , sumHotspots
    , mantleBaseDepth
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import World.Constants (seaLevel)
import World.Base (GeoCoord(..))
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Fluid.Internal (wrapChunkCoordU)
import World.Geology.Hash (valueNoise2D, wrappedDeltaUV)
import World.Magma.Types
    ( MagmaSource(..)
    , VolcanoCtx(..)
    , msSurfaceRadius
    )

-- | Base mantle depth in tiles below seaLevel. 1 tile = 10 m, so
--   −250 ≈ 2.5 km — shallow as actual mantle but accessible for
--   gameplay (dig + chamber visualisation).
mantleBaseDepth ∷ Int
mantleBaseDepth = seaLevel - 250

-- | Ceiling Z of the molten interior at @(gx, gy)@. Below this z,
--   @lavaAt@ is always True.
mantleZ ∷ VolcanoCtx → Int → Int → Int
mantleZ ctx gx gy =
    let noise  = mantleNoise gx gy (vcSeed ctx)
        uplift = sumHotspots ctx gx gy
    in mantleBaseDepth + noise + round uplift

-- | Multivariable noise for natural variation in the mantle ceiling.
--   Three octaves of 2D value noise at scales 200 / 80 / 30 tiles.
--   Output is in @±30@ tiles.
mantleNoise ∷ Int → Int → Word64 → Int
mantleNoise gx gy seed =
    let fx = fromIntegral gx ∷ Float
        fy = fromIntegral gy ∷ Float
        -- valueNoise2D returns in [-0.5, 0.5]
        n1 = valueNoise2D seed 0xA1 fx fy 200.0
        n2 = valueNoise2D seed 0xA2 fx fy  80.0
        n3 = valueNoise2D seed 0xA3 fx fy  30.0
        combined = 0.6 * n1 + 0.3 * n2 + 0.1 * n3   -- ∈ [-0.5, 0.5]
    in round (combined * 60.0)                       -- ±30 tile amplitude

-- | Gaussian uplift contribution from every source whose hotspot
--   bbox (padded by 3σ) covers the chunk containing @(gx, gy)@.
--   σ = 2× the surface radius; beyond 3σ the contribution is <1%
--   of 'msHotspotBoost' and we drop the source from the sum.
sumHotspots ∷ VolcanoCtx → Int → Int → Float
sumHotspots ctx gx gy =
    let worldSize = vcWorldSize ctx
        cc = wrapChunkCoordU worldSize
               (ChunkCoord (gx `floorDiv` chunkSize)
                           (gy `floorDiv` chunkSize))
        candidates = HM.lookupDefault [] cc (vcHotspotIndex ctx)
    in sum [ contrib (vcSources ctx V.! i) | i ← candidates ]
  where
    floorDiv a b = let (q, r) = a `divMod` b
                   in if r < 0 then q - 1 else q
    contrib s =
        let GeoCoord cx cy = msCenter s
            (dx, dy) = wrappedDeltaUV (vcWorldSize ctx) gx gy cx cy
            d2  = fromIntegral (dx*dx + dy*dy) ∷ Float
            r   = msSurfaceRadius s
            sigma = 2.0 * max 1.0 r
            sigma2 = sigma * sigma
        in msHotspotBoost s * exp (negate d2 / sigma2)
