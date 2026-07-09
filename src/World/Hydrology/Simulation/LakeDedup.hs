{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.Simulation.LakeDedup
    ( dedupLakes
    ) where

import UPrelude
import World.Types
import World.Geology.Hash (wrappedDeltaUV)

-- * Lake Deduplication

-- | Radius-aware dedup: a new lake is rejected if it falls within
--   the exclusion zone of any already-accepted lake.  The exclusion
--   radius is the *larger* of the two lakes' radii × 1.5, so big
--   lakes claim more territory and prevent noisy clusters.
dedupLakes ∷ Int → [LakeParams] → [LakeParams]
dedupLakes worldSize = go []
  where
    go acc [] = acc
    go acc (lk:rest) =
        let dominated = any (\existing →
                let GeoCoord ex ey = lkCenter existing
                    GeoCoord lx ly = lkCenter lk
                    (dxi, dyi) = wrappedDeltaUV worldSize lx ly ex ey
                    -- Exclusion zone: 1.5× the larger radius
                    exclR = round (fromIntegral (max (lkRadius existing)
                                                     (lkRadius lk))
                                   * (1.5 ∷ Float))
                in abs dxi < exclR ∧ abs dyi < exclR
                ) acc
        in if dominated
           then go acc rest
           else go (lk : acc) rest
