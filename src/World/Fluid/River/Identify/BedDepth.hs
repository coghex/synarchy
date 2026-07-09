{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Channel-bed depth model (#223) and the final per-tile carve delta
--   for the river-identification pipeline. Ordinary reaches keep a
--   flat width-derived depth; confined (canyon-walled) and
--   tectonically-rifted reaches deepen, with a thalweg cross-section
--   and pool/riffle variation along the channel. Called once from
--   'World.Fluid.River.Identify.traceRivers'. See that module's header
--   comment for the full pipeline overview.
module World.Fluid.River.Identify.BedDepth
    ( computeBedDepth
    , maxBedDepth
    , computeCarveDelta
    ) where

import UPrelude
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (chunkSize)
import World.Constants (seaLevel)
import World.Fluid.River.Identify.Components (depthFromRadius)
import World.Plate (wrappedValueNoise2D)

-- | Per-tile carve delta in z — the FINAL channel-bed fit, the second
--   of the two-stage hydrology (see the note at the 'identifyWorldRivers'
--   call in Timeline.hs). This is NOT the geological river carve: that
--   happens per-age in 'reconcileHydrology' and shapes the valley over
--   time. This runs once on the settled terrain, given the finalized
--   river path/width/surface, and only tops the bed up to channel depth
--   — a bounded fit, not a re-carve.
--
--   Per-tile carve delta in z. Water surface is the centre's pre-carve
--   elevation; post-carve terrain at a river tile is
--   @min(preCarve, surface − depthFromRadius)@. The delta lowers the
--   terrain enough to ensure water visible at every river tile and
--   keep the cross-section flat under the same water plane.
--
--   For tiles whose pre-carve terrain is already below the channel
--   floor (downhill banks), delta = 0; the natural depression fills
--   with water.
computeCarveDelta
    ∷ Int                  -- ^ worldTiles
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Bool       -- ^ isRiverTile
    → VU.Vector Int        -- ^ per-tile bed depth ('computeBedDepth')
    → VU.Vector Int        -- ^ surfaceZ
    → VU.Vector Int
computeCarveDelta worldTiles terrain isRiverTile bedDepth surfZ =
    let nTiles = worldTiles * worldTiles
    in VU.generate nTiles $ \i →
        if not (isRiverTile VU.! i)
        then 0
        else
          let preCarve = terrain VU.! i
              surf     = surfZ   VU.! i
              depth    = bedDepth VU.! i
          in max 0 (preCarve - surf + depth)

-- * Bed depth model (#223)

-- | Valley-wall height (above the water surface) at which a reach
--   starts counting as confined; each 'canyonConfSlope' z above that
--   adds one z of bed depth, up to 'canyonConfCap'. Calibrated on the
--   seed-42 w128 wall-height census: at 12/4 the first boost lands at
--   walls 16 z above the water surface (~15% of river mileage), the
--   cap at 32+ z — ordinary hilly valleys stay on the flat shallow
--   fit and only genuine gorges deepen.
canyonConfMin, canyonConfSlope, canyonConfCap ∷ Int
canyonConfMin   = 12
canyonConfSlope = 4
canyonConfCap   = 5

-- | Chebyshev radius of the wall-height scan around each river tile.
--   Deliberately tight: a wall 2 tiles from the water IS the channel
--   wall; wider scans read nearby hills as confinement and boost
--   every mountain-adjacent reach.
canyonScanRadius ∷ Int
canyonScanRadius = 2

-- | Rift-intensity floor below which the tectonic bed bonus is zero,
--   and the maximum bonus (z) a fully-rifted reach adds.
riftBedThreshold ∷ Float
riftBedThreshold = 0.25

riftBedCap ∷ Int
riftBedCap = 3

-- | Hard cap on the water column a canyon/rift reach can carve.
maxBedDepth ∷ Int
maxBedDepth = 9

-- | Wavelength (tiles) of the pool/riffle bed noise on deep reaches.
poolRiffleScale ∷ Int
poolRiffleScale = 12

-- | Per-tile channel bed depth (z of water column the carve fits).
--
--   Ordinary reaches keep the historical behaviour EXACTLY: depth =
--   'depthFromRadius' width, flat across the cross-section — the
--   issue #223 requirement is that lowland rivers stay shallow and
--   flat. A reach only deepens when it has a canyon/rift *boost*:
--
--     * confinement — the highest terrain within 'canyonScanRadius'
--       tiles stands ≥ 'canyonConfMin' z above the water surface
--       (gorges through plateaus, breakthrough cuts, rift shoulders);
--     * rift intensity — the tile sits on a divergent plate boundary
--       ('riftTectonicIntensity' ≥ 'riftBedThreshold').
--
--   The boost is smoothed along the channel (two Jacobi passes over
--   river-tile 4-neighbours) so depth never pops tile-to-tile, then
--   shaped by a thalweg profile (deepest at the centreline, one z
--   shallower per wing tile out) and pool/riffle value noise (±1–2 z
--   at 'poolRiffleScale' wavelength) so deep beds read as carved
--   channels rather than flat boxes. Result clamped to
--   [1, 'maxBedDepth']. The carve stays lower-only downstream —
--   deeper targets only ever LOWER the bed further below the fixed
--   water surface.
computeBedDepth
    ∷ Word64               -- ^ world seed
    → Int                  -- ^ worldSize (chunks per side)
    → VU.Vector Int        -- ^ terrain (pre-carve)
    → VU.Vector Bool       -- ^ isRiverTile
    → VU.Vector Int        -- ^ widthRadius
    → VU.Vector Int        -- ^ perpDist (0 = centreline)
    → VU.Vector Int        -- ^ surfaceZ
    → (Int → Int → Float)  -- ^ rift-intensity field
    → VU.Vector Int
computeBedDepth seed worldSize terrain isRiverTile widthRadius perpDist
                surfZ riftAt =
    let worldTiles = worldSize * chunkSize
        nTiles     = worldTiles * worldTiles
        half       = worldTiles `div` 2

        -- Highest terrain within the scan window, minus the water
        -- surface. Grid edges clamp; near-seam correctness comes from
        -- the stitched grid's double coverage (same convention as the
        -- rest of this module).
        wallAbove i =
            let bx = i `mod` worldTiles
                by = i `div` worldTiles
                x0 = max 0 (bx - canyonScanRadius)
                x1 = min (worldTiles - 1) (bx + canyonScanRadius)
                y0 = max 0 (by - canyonScanRadius)
                y1 = min (worldTiles - 1) (by + canyonScanRadius)
                goY y !acc
                    | y > y1 = acc
                    | otherwise = goY (y + 1) (goX y x0 acc)
                goX y x !acc
                    | x > x1 = acc
                    | otherwise =
                        let t = terrain VU.! (y * worldTiles + x)
                            acc' = if t ≡ minBound then acc else max acc t
                        in goX y (x + 1) acc'
            in goY y0 minBound - surfZ VU.! i

        rawBoost = VU.generate nTiles $ \i →
            if not (isRiverTile VU.! i)
            then 0
            else
              let confB = min canyonConfCap
                              (max 0 ((wallAbove i - canyonConfMin)
                                      `div` canyonConfSlope))
                  gx = (i `mod` worldTiles) - half
                  gy = (i `div` worldTiles) - half
                  rift = riftAt gx gy
                  riftB = if rift < riftBedThreshold
                          then 0
                          else min riftBedCap
                                   (round (fromIntegral riftBedCap * rift))
              in confB + riftB

        -- Two smoothing passes over river 4-neighbours: kills
        -- tile-to-tile depth pops where the wall scan enters/leaves
        -- a cliff's window.
        smoothPass bv = VU.generate nTiles $ \i →
            if not (isRiverTile VU.! i)
            then 0
            else
              let bx = i `mod` worldTiles
                  by = i `div` worldTiles
                  neigh = [ j
                          | (dx, dy) ← [(1,0),(-1,0),(0,1),(0,-1)]
                          , let nx = bx + dx
                                ny = by + dy
                          , nx ≥ 0, nx < worldTiles
                          , ny ≥ 0, ny < worldTiles
                          , let j = ny * worldTiles + nx
                          , isRiverTile VU.! j
                          ]
                  s = sum (map (bv VU.!) neigh)
                  n = length neigh
              in (2 * (bv VU.! i) + s) `div` (2 + n)

        boost = smoothPass (smoothPass rawBoost)

    in VU.generate nTiles $ \i →
        if not (isRiverTile VU.! i)
        then 0
        else
          let base = depthFromRadius (max 0 (widthRadius VU.! i))
              b    = boost VU.! i
          in if b ≤ 0
             then base
             else
               let gx = (i `mod` worldTiles) - half
                   gy = (i `div` worldTiles) - half
                   -- [0,1] noise → {−1, 0, +1, +2}
                   pool = round (wrappedValueNoise2D seed worldSize gx gy
                                                     poolRiffleScale
                                 * 3.0 ∷ Float) - 1
                   perp = min 8 (perpDist VU.! i)
                   raw  = max 1 (min maxBedDepth (base + b + pool - perp))
                   -- The boost may only deepen while the bed stays
                   -- ABOVE sea level — a boosted bed at seaLevel or
                   -- below reclassifies the channel as ocean in an
                   -- oceanic chunk. Where the water surface is too
                   -- low to afford any extra depth, fall back to the
                   -- exact unboosted fit (master-identical mouths).
                   aboveSea = surfZ VU.! i - (seaLevel + 1)
               in max base (min raw aboveSea)
