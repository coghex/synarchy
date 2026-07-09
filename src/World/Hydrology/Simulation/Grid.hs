{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.Simulation.Grid
    ( buildInitialElevGrid
    , updateElevGrid
    ) where

import UPrelude
import qualified Data.Vector.Unboxed as VU
import World.Types
import World.Plate (elevationAtGlobal, isBeyondGlacier, wrapGlobalU, worldWidthTiles)
import World.Geology.Hash (hashGeo, hashToFloatGeo, wrappedDeltaUV)
import World.Hydrology.Simulation.Types (ElevGrid(..), baseSampleSpacing, maxGridDim)

-- * Grid Construction

buildInitialElevGrid ∷ Word64 → Int → [TectonicPlate] → ElevGrid
buildInitialElevGrid seed worldSize plates =
    let totalTiles = worldSize * 16
        -- The grid lives in (u, v) space and wraps ix as a torus, so
        -- spacing must DIVIDE the world's u-period exactly: with the
        -- old floor-division spacing, gridW capped at maxGridDim left
        -- gridW·spacing < totalTiles — a never-sampled stripe at the
        -- seam (128 tiles wide at worldSize 128, 256 at 256) where no
        -- per-age rivers, lakes, or valley carving could originate,
        -- and a torus wrap stitching flow across a phantom
        -- discontinuity. Picking the smallest divisor of totalTiles
        -- ≥ ceil(totalTiles / maxGridDim) restores exact coverage
        -- (totalTiles = 16·worldSize, so divisors are dense). The
        -- [..totalTiles] bound makes the search total; spacing =
        -- totalTiles (gridW 1) is unreachable for any real world
        -- size. Regression test: Test.Headless.WorldGen.WrapSeam.
        minSpacing = max baseSampleSpacing
                         ((totalTiles + maxGridDim - 1) `div` maxGridDim)
        spacing = fromMaybe totalTiles
                      (listToMaybe [ s | s ← [minSpacing .. totalTiles]
                                       , totalTiles `mod` s ≡ 0 ])
        gridW = max 4 (totalTiles `div` spacing)
        halfGrid = gridW `div` 2
        totalSamples = gridW * gridW
        fromIdx idx = (idx `mod` gridW, idx `div` gridW)

        -- ix maps to u-axis, iy maps to v-axis
        -- u = (ix - halfGrid) * spacing
        -- v = (iy - halfGrid) * spacing
        -- gx = (u + v) / 2, gy = (v - u) / 2
        gxV = VU.generate totalSamples $ \idx →
            let (ix, iy) = fromIdx idx
                u = (ix - halfGrid) * spacing
                v = (iy - halfGrid) * spacing
            in (u + v) `div` 2

        gyV = VU.generate totalSamples $ \idx →
            let (ix, iy) = fromIdx idx
                u = (ix - halfGrid) * spacing
                v = (iy - halfGrid) * spacing
            in (v - u) `div` 2

        rawElevV = VU.generate totalSamples $ \idx →
            let gx = gxV VU.! idx
                gy = gyV VU.! idx
                (gx', gy') = wrapGlobalU worldSize gx gy
            in if isBeyondGlacier worldSize gx' gy'
               then seaLevel + 500
               else fst (elevationAtGlobal seed plates worldSize gx' gy')

        -- Add meander-inducing micro-noise to the elevation grid.
        -- On flat terrain, the D8 flow direction creates straight paths
        -- because the gradient is nearly uniform. Small coherent noise
        -- breaks the symmetry and forces flow to curve naturally,
        -- creating meandering drainage patterns. The noise amplitude
        -- scales with local flatness — steep terrain keeps its natural
        -- gradient, flat terrain gets noise to induce curvature.
        elevV = VU.imap (\idx rawE →
            if rawE ≤ seaLevel then rawE  -- don't perturb ocean/sub-sea
            else let gx = gxV VU.! idx
                     gy = gyV VU.! idx
                     -- Two octaves of coherent noise at different scales
                     n1 = meanderNoise seed worldSize gx gy 40 1300
                     n2 = meanderNoise seed worldSize gx gy 18 1301
                     noise = n1 * 0.6 + n2 * 0.4
                     -- Local slope: max elevation diff to any neighbor
                     (ix, iy) = (idx `mod` gridW, idx `div` gridW)
                     maxSlope = foldl' (\acc (dx, dy) →
                         let nx = ((ix + dx) `mod` gridW + gridW) `mod` gridW
                             ny = iy + dy
                         in if ny < 0 ∨ ny ≥ gridW then acc
                            else max acc (abs (rawE - rawElevV VU.! (ny * gridW + nx)))
                         ) 0 [(-1,0),(1,0),(0,-1),(0,1)]
                     -- Flat terrain (slope < 3) gets full noise;
                     -- steep terrain (slope > 10) gets none
                     flatness = clamp01 (1.0 - fromIntegral maxSlope / 10.0)
                     -- Amplitude: up to 1 elevation level on flat terrain
                     amplitude = 1.0 * flatness
                 in rawE + round (noise * amplitude)
            ) rawElevV

        landV = VU.generate totalSamples $ \idx →
            let gx = gxV VU.! idx
                gy = gyV VU.! idx
                (gx', gy') = wrapGlobalU worldSize gx gy
            in elevV VU.! idx > seaLevel
             ∧ not (isBeyondGlacier worldSize gx' gy')

    in ElevGrid gridW spacing elevV gxV gyV landV

-- | 2D coherent noise for meander induction. Returns [-1, 1].
--   Value noise: hash at integer lattice points, bilinear interp with
--   smoothstep. Operates in (u, v) space so the lattice can be made
--   periodic on the u-axis — the world is a cylinder along u, and the
--   raw (gx, gy) hash was discontinuous across that seam, producing
--   uncorrelated noise on physically adjacent tiles. The u-axis lattice
--   index is wrapped modulo `latU = w / wavelength`, where w is the
--   u-axis tile period. If `wavelength` doesn't divide w evenly, the
--   effective u-axis wavelength drifts by `(w / latU - wavelength)` —
--   <1% at typical world sizes, visually indistinguishable.
meanderNoise ∷ Word64 → Int → Int → Int → Int → Int → Float
meanderNoise seed worldSize gx gy wavelength prop =
    let (gx', gy') = wrapGlobalU worldSize gx gy
        w  = worldWidthTiles worldSize
        u  = gx' - gy'
        v  = gx' + gy'
        fu = fromIntegral u / fromIntegral wavelength ∷ Float
        fv = fromIntegral v / fromIntegral wavelength ∷ Float
        iu = floor fu ∷ Int
        iv = floor fv ∷ Int
        fracU = fu - fromIntegral iu
        fracV = fv - fromIntegral iv
        -- Smoothstep for C1 continuity
        su = fracU * fracU * (3.0 - 2.0 * fracU)
        sv = fracV * fracV * (3.0 - 2.0 * fracV)
        -- Periodic lattice on u-axis (v-axis doesn't wrap)
        latU = max 1 (w `div` wavelength)
        wrapU k = ((k `mod` latU) + latU) `mod` latU
        i0 = wrapU iu
        i1 = wrapU (iu + 1)
        -- Hash at four corners
        h00 = hashToFloatGeo (hashGeo seed (i0 * 7919 + iv * 6271) prop) * 2.0 - 1.0
        h10 = hashToFloatGeo (hashGeo seed (i1 * 7919 + iv * 6271) prop) * 2.0 - 1.0
        h01 = hashToFloatGeo (hashGeo seed (i0 * 7919 + (iv+1) * 6271) prop) * 2.0 - 1.0
        h11 = hashToFloatGeo (hashGeo seed (i1 * 7919 + (iv+1) * 6271) prop) * 2.0 - 1.0
        -- Bilinear interpolation
        top    = h00 * (1.0 - su) + h10 * su
        bottom = h01 * (1.0 - su) + h11 * su
    in top * (1.0 - sv) + bottom * sv

-- * Incremental Update

updateElevGrid ∷ Int → ElevGrid → GeoPeriod → ElevGrid
updateElevGrid worldSize grid period =
    let events = gpEvents period
    in if null events
       then grid
       else
       let gridW = egGridW grid
           totalSamples = gridW * gridW
           oldElev = egElev grid

           newElev = VU.generate totalSamples $ \idx →
               let gx = egGX grid VU.! idx
                   gy = egGY grid VU.! idx
                   e0 = oldElev VU.! idx
               in foldl' (\e event →
                      let mod' = applyGeoEventSimple event worldSize gx gy e
                      in e + gmElevDelta mod'
                  ) e0 events

           newLand = VU.generate totalSamples $ \idx →
               let gx = egGX grid VU.! idx
                   gy = egGY grid VU.! idx
                   (gx', gy') = wrapGlobalU worldSize gx gy
               in newElev VU.! idx > seaLevel
                ∧ not (isBeyondGlacier worldSize gx' gy')

       in grid { egElev = newElev, egLand = newLand }

-- * Simplified Event Application

applyGeoEventSimple ∷ GeoEvent → Int → Int → Int → Int → GeoModification
applyGeoEventSimple (CraterEvent params) ws gx gy _e =
    let GeoCoord cx cy = cpCenter params
        (dxi, dyi) = wrappedDeltaUV ws gx gy cx cy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)
        r = fromIntegral (cpRadius params) ∷ Float
    in if dist > r * 1.5
       then noModification
       else if dist < r * 0.8
            then GeoModification (negate (cpDepth params)) Nothing 0
            else if dist < r
                 then GeoModification (cpRimHeight params) Nothing 0
                 else noModification
applyGeoEventSimple (VolcanicEvent feature) ws gx gy _e =
    applyVolcanicSimple feature ws gx gy
applyGeoEventSimple _ _ _ _ _ = noModification

applyVolcanicSimple ∷ FeatureShape → Int → Int → Int → GeoModification
applyVolcanicSimple (VolcanicShape (ShieldVolcano p)) ws gx gy =
    let GeoCoord cx cy = shCenter p
        (dxi, dyi) = wrappedDeltaUV ws gx gy cx cy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)
        r = fromIntegral (shBaseRadius p) ∷ Float
    in if dist > r then noModification
       else let t = 1.0 - dist / r
            in GeoModification (round (fromIntegral (shPeakHeight p) * t * t)) Nothing 0
applyVolcanicSimple (VolcanicShape (SuperVolcano p)) ws gx gy =
    let GeoCoord cx cy = svCenter p
        (dxi, dyi) = wrappedDeltaUV ws gx gy cx cy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)
        outerR = fromIntegral (svEjectaRadius p) ∷ Float
        calderaR = fromIntegral (svCalderaRadius p) ∷ Float
    in if dist > outerR then noModification
       else if dist < calderaR
            then GeoModification (negate (svFloorDepth p)) Nothing 0
            else let denom = outerR - calderaR
                     t = if denom > 0.0 then 1.0 - (dist - calderaR) / denom else 1.0
                 in GeoModification (round (fromIntegral (svRimHeight p) * t)) Nothing 0
applyVolcanicSimple (VolcanicShape (CinderCone p)) ws gx gy =
    let GeoCoord cx cy = ccCenter p
        (dxi, dyi) = wrappedDeltaUV ws gx gy cx cy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)
        r = fromIntegral (ccBaseRadius p) ∷ Float
    in if dist > r then noModification
       else let t = 1.0 - dist / r
            in GeoModification (round (fromIntegral (ccPeakHeight p) * t)) Nothing 0
applyVolcanicSimple _ _ _ _ = noModification
