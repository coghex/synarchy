{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Multi-pass Jacobi smoothing used by the coastal pass: contour
--   flattening that carves the gentle coastal-band gradient out of
--   raw plate-noise terrain, and a final beach-smoothing pass that
--   polishes the eroded shoreline.
module World.Geology.Coastal.Smoothing
    ( smoothCoast
    , smoothCoastalContour
    ) where

import UPrelude
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import World.Types
import World.Material (MaterialId(..), getMaterialProps, MaterialProps(..)
                      , MaterialRegistry, matGlacier)
import World.Geology.Coastal.Constants (maxCoastalDist, smoothBand)

-- * Beach Smoothing

smoothCoast ∷ Int → Int → VU.Vector Int → VU.Vector Float → VU.Vector Int
            → VU.Vector Int
smoothCoast 0 _ _ _ elev = elev
smoothCoast iters borderSize distF steepF elev =
    let borderArea = borderSize * borderSize
        -- Jacobi iteration: read from immutable previous pass, write
        -- to new mutable vector. This ensures overlapping border tiles
        -- in adjacent chunks produce identical results regardless of
        -- processing order — eliminating chunk boundary seams.
        smoothed = runST $ do
            em ← VUM.new borderArea
            forM_ [0 .. borderArea - 1] $ \i →
                VUM.write em i (elev VU.! i)
            forM_ [0 .. borderArea - 1] $ \idx → do
                let d = distF VU.! idx
                    bx = idx `mod` borderSize
                    by = idx `div` borderSize
                -- Smooth tiles in the border area, but skip the outermost
                -- ring. Edge tiles have out-of-bounds neighbors that fall
                -- back to self, producing different results in adjacent
                -- chunks. Skipping them ensures all processed tiles have
                -- valid neighbors → identical results across chunks.
                -- Steep (cliff) stretches are skipped entirely — this
                -- min-with-average pass would eat the cliff base the
                -- erosion stage deliberately left standing.
                when (d > 0 ∧ d ≤ maxCoastalDist
                     ∧ steepF VU.! idx < 0.6
                     ∧ bx > 0 ∧ bx < borderSize - 1
                     ∧ by > 0 ∧ by < borderSize - 1) $ do
                    let e = elev VU.! idx
                        readN nx ny
                            | nx ≥ 0 ∧ nx < borderSize
                              ∧ ny ≥ 0 ∧ ny < borderSize
                                = let nIdx = ny * borderSize + nx
                                      nd = distF VU.! nIdx
                                  in if nd ≡ 0
                                     then seaLevel + 1
                                     else elev VU.! nIdx
                            | otherwise = e
                        n  = readN bx       (by - 1)
                        s  = readN bx       (by + 1)
                        w  = readN (bx - 1) by
                        eN = readN (bx + 1) by
                        avg = (n + s + w + eN + e) `div` 5
                    VUM.write em idx (min e avg)
            VU.unsafeFreeze em
    in smoothCoast (iters - 1) borderSize distF steepF smoothed

-- * Coastline Contour Smoothing

-- | Multi-pass coastal terrain flattening.
--
--   The elevation noise in Plate.hs creates ±100-tile swings at
--   5-tile wavelength. Where this noise straddles seaLevel, it
--   produces a jagged, speckled coastline with steep gradients
--   right at the coast — no room for flat beaches.
--
--   This function does two things:
--     1. Smooths the sea-level isoline (removes speckle)
--     2. Flattens the coastal gradient for soft rock, creating
--        the gentle slope needed for wide sandy beaches
--
--   The flattening works by pulling each tile toward the minimum
--   of its neighbors plus a small step. Over many iterations this
--   propagates the low ocean elevation inland, creating a gradual
--   ramp from seaLevel to natural terrain. Hard rock resists this
--   pull, preserving cliffs and headlands.
--
--   Spatially bounded by a pre-computed distance field: only tiles
--   within 'World.Geology.Coastal.Constants.smoothBand' of the coast
--   are smoothed, with a linear fade from 'maxCoastalDist' outward.
--   (Historically the bound was chunkBorder for window-overlap
--   safety; the pass has been global since save v25, so the bound is
--   purely a look knob now.)
--
--   Steepness-aware (#220): the coherent tectonic field scales the
--   pull to zero on convergent stretches, so their natural gradient
--   survives to the waterline — that surviving gradient IS the
--   cliff. Flattening everything here was the root cause of the
--   uniform beach-to-cliff coastline.
--
--   Uses Jacobi iteration (reads from immutable previous pass) so
--   overlapping border tiles in adjacent chunks produce identical
--   results regardless of processing order — no seams.
smoothCoastalContour ∷ Int → Int → VU.Vector MaterialId → MaterialRegistry
                     → VU.Vector Int → VU.Vector Float → VU.Vector Int
                     → VU.Vector Int
smoothCoastalContour 0 _ _ _ _ _ elev = elev
smoothCoastalContour iters borderSize matVec registry preDistField steepF elev =
    let borderArea = borderSize * borderSize
        bandWidth  = 60 ∷ Int  -- process tiles within ±60 elev of seaLevel
        fadeStart  = 45 ∷ Int  -- full strength below this, fade above
        -- Spatial bound: smoothing fades from full strength at
        -- maxCoastalDist to zero at smoothBand.
        smoothDistMax = smoothBand
        fadeDenom = smoothDistMax - maxCoastalDist
        smoothed = runST $ do
            em ← VUM.new borderArea
            forM_ [0 .. borderArea - 1] $ \i →
                VUM.write em i (elev VU.! i)
            forM_ [0 .. borderArea - 1] $ \idx → do
                let e  = elev VU.! idx
                    bx = idx `mod` borderSize
                    by = idx `div` borderSize
                    absD = abs (e - seaLevel)
                    preDist = preDistField VU.! idx
                -- Skip outermost border ring (inconsistent between
                -- chunks), tiles outside the elevation band, and
                -- tiles beyond the spatial distance bound.
                -- Only smooth land tiles (above seaLevel). Ocean tiles
                -- must stay ≤ seaLevel so the BFS distance field can
                -- seed from them — raising them kills coastal processing.
                let mat = matVec VU.! idx
                when (e > seaLevel ∧ absD < bandWidth
                     ∧ mat ≢ matGlacier
                     ∧ preDist ≤ smoothDistMax
                     ∧ bx > 0 ∧ bx < borderSize - 1
                     ∧ by > 0 ∧ by < borderSize - 1) $ do
                    let hardness = mpHardness (getMaterialProps registry mat)
                        -- Full strength within fadeStart, linear fade
                        -- to zero at bandWidth edge.
                        elevFade = if absD < fadeStart then 1.0
                                   else 1.0 - fromIntegral (absD - fadeStart)
                                            / fromIntegral (bandWidth - fadeStart) ∷ Float
                        -- Hardness reduces smoothing but never
                        -- eliminates it — even granite erodes over
                        -- geological time. Soft rock (≤0.3): 100%.
                        -- Hard rock (0.9 granite): 25%. This ensures
                        -- flat beaches form on all coast types, just
                        -- narrower on harder rock.
                        hardFade = max 0.25 (min 1.0
                                    (1.0 - (hardness - 0.3) * 1.0)) ∷ Float
                        -- Spatial fade: full strength within maxCoastalDist,
                        -- linear fade to zero at smoothBand distance.
                        spatialFade
                          | preDist ≤ maxCoastalDist = 1.0
                          | fadeDenom ≤ 0            = 0.0
                          | otherwise = max 0.0
                              (fromIntegral (smoothDistMax - preDist)
                              / fromIntegral fadeDenom) ∷ Float
                        -- Steepness gate (#220): convergent stretches
                        -- keep their natural gradient — no flattening.
                        steepFade = 1.0 - steepF VU.! idx
                        strength = elevFade * hardFade * spatialFade * steepFade
                    when (strength > 0.01) $ do
                        let readN nx ny
                                | nx ≥ 0 ∧ nx < borderSize
                                  ∧ ny ≥ 0 ∧ ny < borderSize
                                    = elev VU.! (ny * borderSize + nx)
                                | otherwise = e
                            n  = readN bx       (by - 1)
                            s  = readN bx       (by + 1)
                            w  = readN (bx - 1) by
                            eN = readN (bx + 1) by
                            minN = min n (min s (min w eN))
                            avg  = (n + s + w + eN + e) `div` 5
                        -- For tiles above sea level: use the minimum
                        -- neighbor + 2 as an erosion target. This pulls
                        -- elevated coastal land downhill toward the
                        -- ocean, creating the gentle slope for beaches.
                        -- Blend with neighbor average for smoothing.
                        -- Clamped at sea+1: a land tile bordering a
                        -- deep floor must not plunge below sea in one
                        -- step — that digs flooded pockets behind the
                        -- shoreline (deep "floating" lakes, terrain
                        -- dropping out from under coastal lava pools).
                        -- Below sea level: just average (fill small holes).
                        let target
                              | e > seaLevel = max (seaLevel + 1)
                                                   (min avg (minN + 2))
                              | otherwise    = avg
                            delta = fromIntegral (target - e) ∷ Float
                            newE = e + round (strength * delta)
                        VUM.write em idx newE
            VU.unsafeFreeze em
    in smoothCoastalContour (iters - 1) borderSize matVec registry
           preDistField steepF smoothed
