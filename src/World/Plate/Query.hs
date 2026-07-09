{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Nearest-plate lookup and boundary classification, split out of
--   "World.Plate" (issue #560).
module World.Plate.Query
    ( -- * Queries
      twoNearestPlates
      -- * Boundary classification
    , BoundaryType(..)
    , classifyBoundary
    ) where

import UPrelude
import World.Plate.Types (TectonicPlate(..))
import World.Plate.Wrap (wrappedDeltaU)
import World.Plate.Noise (wrappedValueNoise2D)

-- * Boundary Classification

data BoundaryType
    = Convergent !Float
    | Divergent  !Float
    | Transform  !Float
    deriving (Show)

-- * Plate Queries

twoNearestPlates ∷ Word64 → Int → [TectonicPlate] → Int → Int
                 → ((TectonicPlate, Float), (TectonicPlate, Float))
twoNearestPlates seed worldSize plates gx gy =
    let -- Cheap raw distance: 2 mults + 1 sqrt, no noise lookups.
        rawDist plate =
            let du = fromIntegral (wrappedDeltaU worldSize gx gy
                        (plateCenterX plate) (plateCenterY plate)) ∷ Float
                dv = fromIntegral ((gx + gy) - (plateCenterX plate + plateCenterY plate)) ∷ Float
            in sqrt (du * du + dv * dv)

        -- plateJitter outputs in [-40, +40]. A plate's actual
        -- distance is at least rawDist - 40, so any plate with
        -- rawDist - 40 > best2.dist cannot beat the runner-up —
        -- we skip its expensive jitter computation (audit #18,
        -- which was the hot path: 2 noise lookups per plate per
        -- elevationAtGlobal call).
        jitterMax = 40.0 ∷ Float

        jitter plate =
            plateJitter seed worldSize gx gy
                (plateCenterX plate) (plateCenterY plate)

        -- best2Real flag tracks whether best2 holds a real plate or
        -- the initial placeholder. The skip predicate only fires once
        -- best2 is real — otherwise the placeholder distance (matching
        -- the original `maxDist = fromIntegral worldSize`) is a small
        -- finite "infinity" specifically chosen to feed downstream
        -- boundary calculations correctly when the first plate has no
        -- competitor. Preserving that placeholder keeps elevation
        -- bit-identical to the pre-optimization output.
        go !best1 !best2 !_best2Real [] = (best1, best2)
        go !best1 !best2 !best2Real (p:rest) =
            let !raw = rawDist p
            in if best2Real ∧ raw - jitterMax > snd best2
               then go best1 best2 best2Real rest
               else let !d = raw + jitter p
                        !cand = (p, d)
                    in if d < snd best1
                       then go cand best1 True rest
                       else if d < snd best2
                            then go best1 cand True rest
                            else go best1 best2 best2Real rest

        maxDist = fromIntegral worldSize ∷ Float
    in case plates of
        (p:ps) →
            let !raw0 = rawDist p
                !d0   = raw0 + jitter p
                first  = (p, d0)
                second = (p, maxDist)
            in go first second False ps
        _ → error $ "twoNearestPlates: no plates (seed=" ⧺ show seed
                  ⧺ " worldSize=" ⧺ show worldSize
                  ⧺ " tile=(" ⧺ show gx ⧺ "," ⧺ show gy ⧺ "))"

-- * Boundary Classification

classifyBoundary ∷ Int → TectonicPlate → TectonicPlate → BoundaryType
classifyBoundary worldSize plateA plateB =
    let duRaw = fromIntegral (wrappedDeltaU worldSize
                    (plateCenterX plateA) (plateCenterY plateA)
                    (plateCenterX plateB) (plateCenterY plateB)) ∷ Float
        dvRaw = fromIntegral ((plateCenterX plateB + plateCenterY plateB)
                            - (plateCenterX plateA + plateCenterY plateA)) ∷ Float
        nLen  = sqrt (duRaw * duRaw + dvRaw * dvRaw)
        (nx, ny) = if nLen > 0.001
                   then (duRaw / nLen, dvRaw / nLen)
                   else (1.0, 0.0)

        (tx, ty) = (-ny, nx)

        approachA = plateDriftX plateA * nx + plateDriftY plateA * ny
        approachB = plateDriftX plateB * nx + plateDriftY plateB * ny
        approach  = approachA - approachB

        shearA = plateDriftX plateA * tx + plateDriftY plateA * ty
        shearB = plateDriftX plateB * tx + plateDriftY plateB * ty
        shear  = abs (shearA - shearB)

        convergentThreshold = 0.3
        divergentThreshold  = -0.3

    in if approach > convergentThreshold
       then Convergent approach
       else if approach < divergentThreshold
            then Divergent (abs approach)
            else Transform shear

-- * Jitter

-- | Per-plate jitter: each plate gets a different noise field
--   keyed by its center coordinates. This creates irregular
--   plate boundaries instead of straight Voronoi edges.
plateJitter ∷ Word64 → Int → Int → Int → Int → Int → Float
plateJitter seed worldSize gx gy plateCX plateCY =
    let plateSeed = seed `xor` fromIntegral (plateCX * 7919 + plateCY * 6271)
        n1 = wrappedValueNoise2D plateSeed worldSize gx gy 20
        n2 = wrappedValueNoise2D (plateSeed + 99) worldSize gx gy 8
        combined = n1 * 0.7 + n2 * 0.3
    in (combined - 0.5) * 80.0
