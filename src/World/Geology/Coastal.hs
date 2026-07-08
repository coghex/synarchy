{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Coastal
    ( identifyCoastalErosion
    , applyCoastalTable
    , breachSealedBasins
    , beachMaterial
    , wetlandMaterial
    , deltaMaterial
    , coastHash
    , sandProfile
    , maxCoastalDist
    , filterNearbyMouths
    , isNearRiverMouth
    , shorelineOffset
    ) where

import UPrelude
import Control.Monad.ST (runST)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import World.Types
import World.Material (MaterialId(..), getMaterialProps, MaterialProps(..)
                      , MaterialRegistry, matGlacier)
import World.Plate (coastCellSize, coastCellsInU, coastCellSteepness
                   , coastSteepAt, wrappedValueNoise2D)
import World.Geology.Coastal.Types (CoastalTable(..))
import World.Geology.Coastal.Constants (maxCoastalDist, smoothBand)
import World.Geology.Coastal.RiverMouth
    (filterNearbyMouths, isNearRiverMouth)
import World.Geology.Coastal.Noise (coastHash, shorelineOffset)
import World.Geology.Coastal.Material
    (sandProfile, beachMaterial, wetlandMaterial, deltaMaterial)
import World.Geology.Coastal.Distance (buildDistField)
import World.Geology.Coastal.Smoothing (smoothCoast, smoothCoastalContour)
import World.Geology.Coastal.Breach (breachSealedBasins)
import World.Generate.Constants (chunkBorder)
import World.Generate.Coordinates (chunkToGlobal)

-- * Coastal Erosion Pass

-- | Global coastal pass. Runs ONCE at world init on the stitched
--   pre-coastal terrain (per-chunk timeline windows are
--   window-position-independent, so the stitch is the unambiguous
--   pre-coastal world), and records the result as per-chunk deltas.
--   Chunk gen applies the table ('applyCoastalTable') instead of
--   re-running a windowed pass — every chunk reads the same
--   coastline answer, eliminating the seam-cliff divergence class.
--
--   Pipeline: preDist BFS → coherent tectonic steepness field (#220)
--   → steepness-aware 12-pass contour smoothing → dist BFS →
--   per-tile erosion (steepness-driven profile, river-mouth marshes,
--   hash rolls, shoreline noise) → steepness-gated 10-pass beach
--   smoothing. All terrain writes are LOWER-ONLY — cliffs come from
--   NOT flattening convergent stretches, never from raising (raising
--   desyncs lake/ocean classification: floating lakes, spikes).
--
--   Grid convention: @(gy + half) * worldTiles + (gx + half)@ with
--   'minBound' on beyond-glacier tiles (the 'stitchWorldTerrain'
--   shape). The sentinel is substituted with @seaLevel + 100@
--   internally — the exact value the old window pipeline used for
--   beyond-glacier tiles — so every helper behaves identically.
identifyCoastalErosion
    ∷ Word64
    → Int                    -- ^ worldSize (chunks per side)
    → [TectonicPlate]
    → MaterialRegistry
    → [(Int, Int)]           -- ^ river mouths (global coords)
    → VU.Vector Int          -- ^ stitched pre-coastal elevations
    → VU.Vector MaterialId   -- ^ stitched pre-coastal materials
    → CoastalTable
identifyCoastalErosion seed worldSize plates registry allMouths
                       globalElev globalMat =
    let worldTiles = worldSize * chunkSize
        halfWorld  = worldTiles `div` 2
        halfChunks = worldSize `div` 2
        chunkArea  = chunkSize * chunkSize
        chunkCoords = [ ChunkCoord ccx ccy
                      | ccx ← [-halfChunks .. halfChunks - 1]
                      , ccy ← [-halfChunks .. halfChunks - 1]
                      ]
        worldIdx gx gy = (gy + halfWorld) * worldTiles + (gx + halfWorld)

        -- Sentinel substitution: beyond-glacier tiles take the same
        -- high-land value the window pipeline gave them, so they are
        -- never BFS seeds, never smoothed (elevation-band check), and
        -- never poison a neighbour average.
        elevG = VU.map (\e → if e ≡ minBound then seaLevel + 100 else e)
                       globalElev

        preDistField = buildDistField smoothBand worldTiles elevG

        -- Coherent tectonic steepness (#220). The coarse cell samples
        -- (World.Plate.coastCellSteepness) are memoized into a small
        -- grid — only cells actually touched by coastal-band tiles are
        -- sampled — then interpolated per tile via the SAME
        -- 'coastSteepAt' the base-terrain shelf modulation uses.
        cellsU = coastCellsInU worldSize
        vCells = 2 * (worldTiles `div` coastCellSize) + 4
        jvOff  = worldTiles `div` coastCellSize + 2
        wrapJu ju = ((ju `mod` cellsU) + cellsU) `mod` cellsU
        clampJv jv = max (negate jvOff) (min (vCells - jvOff - 1) jv)
        cellIdx ju jv = (clampJv jv + jvOff) * cellsU + wrapJu ju

        neededCells = runST $ do
            nm ← VUM.replicate (cellsU * vCells) False
            let effCell = fromIntegral (worldTiles)
                        / fromIntegral cellsU ∷ Float
            forM_ [0 .. worldTiles * worldTiles - 1] $ \idx →
                when (preDistField VU.! idx ≤ smoothBand) $ do
                    let gx = idx `mod` worldTiles - halfWorld
                        gy = idx `div` worldTiles - halfWorld
                        iu = floor (fromIntegral (gx - gy) / effCell) ∷ Int
                        iv = floor (fromIntegral (gx + gy)
                                   / fromIntegral coastCellSize ∷ Float) ∷ Int
                    -- Mark a 4x4 block around the interpolation cell —
                    -- generous on purpose so float edge cases can never
                    -- read an unsampled (zero) cell.
                    forM_ [iu - 1 .. iu + 2] $ \ju →
                        forM_ [iv - 1 .. iv + 2] $ \jv →
                            VUM.write nm (cellIdx ju jv) True
            VU.unsafeFreeze nm

        steepCells = VU.generate (cellsU * vCells) $ \i →
            if neededCells VU.! i
            then let (jvI, juI) = i `divMod` cellsU
                 in coastCellSteepness seed worldSize plates juI (jvI - jvOff)
            else 0.0

        tectAt = coastSteepAt (\ju jv → steepCells VU.! cellIdx ju jv) worldSize

        -- Per-tile combined steepness in [0,1]: tectonic base, coarse
        -- along-shore noise (so one margin alternates coves and heads),
        -- polar bias (glacial coasts read rocky/steep), and a small
        -- hardness term (rocky headlands on hard rock).
        steepField = VU.generate (worldTiles * worldTiles) $ \idx →
            if preDistField VU.! idx > smoothBand then 0.0
            else
              let gx = idx `mod` worldTiles - halfWorld
                  gy = idx `div` worldTiles - halfWorld
                  tect = tectAt gx gy
                  n = wrappedValueNoise2D (seed + 77) worldSize gx gy 96
                  vAbs = fromIntegral (abs (gx + gy)) ∷ Float
                  pT = clamp01 ((vAbs / fromIntegral halfWorld - 0.7) / 0.3)
                  polar = 0.35 * pT * pT * (3.0 - 2.0 * pT)
                  hardness = mpHardness (getMaterialProps registry
                                (globalMat VU.! idx))
              in clamp01 (tect + (n - 0.5) * 0.35 + polar
                          + (hardness - 0.45) * 0.25)

        -- Multi-pass contour smoothing near sea level, hardness- and
        -- steepness-aware: gentle stretches flatten into beach ramps,
        -- convergent stretches keep their natural gradient.
        smoothedContour = smoothCoastalContour 12 worldTiles globalMat
                              registry preDistField steepField elevG

        distField = buildDistField maxCoastalDist worldTiles smoothedContour

        (passElev, passMat) = runST $ do
            -- Start from the contour-smoothed elevation so the
            -- erosion pass is consistent with the distance field.
            elevM ← VU.thaw smoothedContour
            matM  ← VU.thaw globalMat
            forM_ chunkCoords $ \coord@(ChunkCoord cx cy) → do
              let -- Pre-filter river mouths per chunk — same perf
                  -- trick the windowed pass used (300+ mouths → 0-3).
                  nearbyMouths = filterNearbyMouths worldSize coord allMouths
              forM_ [0 .. chunkArea - 1] $ \i → do
                let lx = i `mod` chunkSize
                    ly = i `div` chunkSize
                    gx = cx * chunkSize + lx
                    gy = cy * chunkSize + ly
                    idx = worldIdx gx gy
                    dist = distField VU.! idx
                when (dist > 0 ∧ dist ≤ maxCoastalDist) $ do
                    let elev = smoothedContour VU.! idx
                        mat  = globalMat VU.! idx
                    -- Never erode glacier boundary tiles — they mark the
                    -- impassable world edge and must not be replaced with
                    -- coastal materials.
                    when (mat ≢ matGlacier) $ do
                      let steep = steepField VU.! idx
                          nearRiver = isNearRiverMouth worldSize nearbyMouths
                              gx gy
                          localHash = coastHash seed gx gy
                          roll = fromIntegral (localHash .&. 0xFF) / 255.0 ∷ Float
                          offset = shorelineOffset seed gx gy
                          effDist = max 1 (dist + round offset)

                      if nearRiver
                      then do
                          -- Marsh / delta at river mouths: the gentle
                          -- sandProfile gradient (sea+1 at the waterline
                          -- to sea+6 at the reach edge), lower-only.
                          -- NOT flatter than that: the final rivers are
                          -- fitted to this terrain later, and banks cut
                          -- well below the river surface read as water
                          -- standing above land (WATER_CLIFF family).
                          let target = sandProfile effDist
                          when (target < elev) $ VUM.write elevM idx target
                          when (effDist ≤ 10) $ do
                              let wm | effDist ≤ 6 = wetlandMaterial effDist roll
                                     | otherwise   = deltaMaterial effDist roll
                              VUM.write matM idx (MaterialId wm)
                      else do
                          -- Profile from the coherent steepness field.
                          -- Gentle: wide beach, then a long smoothstep
                          -- ramp toward the tile's own natural elevation
                          -- (low hinterland ⇒ beach runs into plains;
                          -- high hinterland ⇒ even climb, no forced
                          -- cliff). Steep: cliffBlend fades the target
                          -- into "leave the natural terrain alone" —
                          -- cliffs by NOT lowering, never by raising.
                          let beachW = max 1 (round ((1.0 - steep) * 6.0)) ∷ Int
                              riseSpan = max 4 (round ((1.0 - steep)
                                  * fromIntegral (maxCoastalDist - beachW) ∷ Float))
                              beachTop = sandProfile beachW
                              rampT
                                | dist ≤ beachW = fromIntegral (sandProfile effDist)
                                | otherwise =
                                    let rf0 = clamp01 (fromIntegral (dist - beachW)
                                                / fromIntegral riseSpan)
                                        rf = rf0 * rf0 * (3.0 - 2.0 * rf0)
                                    in fromIntegral beachTop
                                       + rf * fromIntegral (elev - beachTop)
                              cliffBlend = clamp01 ((steep - 0.55) / 0.20)
                              target = round (rampT + cliffBlend
                                          * (fromIntegral elev - rampT)) ∷ Int

                          -- Lower terrain toward the target (never raise)
                          when (target < elev) $ VUM.write elevM idx target

                          -- Materials: sand stays confined to the
                          -- waterline (wide sand ribbons across the full
                          -- reach read as "snakes"); steep shores get
                          -- gravel/rock only where the shore is actually
                          -- low (cliff tops keep their plate material).
                          if steep < 0.45
                          then when (effDist ≤ 8) $
                              VUM.write matM idx (MaterialId
                                  (beachMaterial effDist (unMaterialId mat) roll))
                          else when (dist ≤ 3 ∧ elev ≤ seaLevel + 8) $ do
                              let gravelMat
                                    | roll < 0.5 = 54  -- gravel
                                    | otherwise  = 53  -- coarse sand
                              VUM.write matM idx (MaterialId gravelMat)
            elevF ← VU.unsafeFreeze elevM
            matF  ← VU.unsafeFreeze matM
            pure (elevF, matF)

        smoothedElev = breachSealedBasins worldSize globalMat
                           (smoothCoast 10 worldTiles distField steepField passElev)

        -- Sparse per-chunk tables: delta vs the (sanitised) input
        -- grid; chunks the pass never touched are absent.
        chunkEntries =
            [ (coord, deltas, mats)
            | coord@(ChunkCoord cx cy) ← chunkCoords
            , let deltas = VU.generate chunkArea $ \i →
                    let lx = i `mod` chunkSize
                        ly = i `div` chunkSize
                        idx = worldIdx (cx * chunkSize + lx)
                                       (cy * chunkSize + ly)
                    in smoothedElev VU.! idx - elevG VU.! idx
                  mats = VU.generate chunkArea $ \i →
                    let lx = i `mod` chunkSize
                        ly = i `div` chunkSize
                        idx = worldIdx (cx * chunkSize + lx)
                                       (cy * chunkSize + ly)
                        newM = passMat VU.! idx
                    in if newM ≡ globalMat VU.! idx
                       then 0 ∷ Word8
                       else unMaterialId newM
            , VU.any (≠ 0) deltas ∨ VU.any (≠ 0) mats
            ]

    in CoastalTable
        { coElevDelta   = HM.fromList
            [ (cc, dv) | (cc, dv, _) ← chunkEntries, VU.any (≠ 0) dv ]
        , coMatOverride = HM.fromList
            [ (cc, mv) | (cc, _, mv) ← chunkEntries, VU.any (≠ 0) mv ]
        }

-- | Apply the global coastal table to one chunk's BORDERED
--   pre-coastal vectors (timeline-stage output). Border tiles map to
--   their owning chunk's table entry — the same cross-chunk lookup
--   convention as the river/lake 'carveAt' — so every chunk sees the
--   identical coastline regardless of which window asks.
applyCoastalTable
    ∷ CoastalTable
    → ChunkCoord
    → (VU.Vector Int, VU.Vector MaterialId)
    → (VU.Vector Int, VU.Vector MaterialId)
applyCoastalTable table coord (elevVec, matVec) =
    let borderSize = chunkSize + 2 * chunkBorder
        borderArea = borderSize * borderSize
        fromIndex idx =
            let (by, bx) = idx `divMod` borderSize
            in (bx - chunkBorder, by - chunkBorder)
        lookupTile gx gy =
            let cx = gx `div` chunkSize
                cy = gy `div` chunkSize
                ilx = ((gx `mod` chunkSize) + chunkSize) `mod` chunkSize
                ily = ((gy `mod` chunkSize) + chunkSize) `mod` chunkSize
                li  = ily * chunkSize + ilx
                d   = case HM.lookup (ChunkCoord cx cy) (coElevDelta table) of
                        Just dv → dv VU.! li
                        Nothing → 0
                m   = case HM.lookup (ChunkCoord cx cy) (coMatOverride table) of
                        Just mv → mv VU.! li
                        Nothing → 0
            in (d, m)
        elev' = VU.generate borderArea $ \idx →
            let z = elevVec VU.! idx
            in if z ≡ minBound
               then z
               else
                 let (lx, ly) = fromIndex idx
                     (gx, gy) = chunkToGlobal coord lx ly
                     (d, _)   = lookupTile gx gy
                 in z + d
        mat' = VU.generate borderArea $ \idx →
            let m0 = matVec VU.! idx
                (lx, ly) = fromIndex idx
                (gx, gy) = chunkToGlobal coord lx ly
                (_, mo)  = lookupTile gx gy
            in if mo ≡ 0 ∨ m0 ≡ matGlacier
               then m0
               else MaterialId mo
    in (elev', mat')
