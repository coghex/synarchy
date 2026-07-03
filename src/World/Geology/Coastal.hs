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
import Control.Monad (foldM)
import Control.Monad.ST (runST)
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import World.Types
import World.Material (MaterialId(..), getMaterialProps, MaterialProps(..)
                      , MaterialRegistry, matGlacier)
import World.Plate (coastCellSize, coastCellsInU, coastCellSteepness
                   , coastSteepAt, wrapGlobalU, wrappedValueNoise2D)
import World.Geology.Coastal.Types (CoastalTable(..))
import World.Geology.Hash (wrappedDeltaUV)
import World.Generate.Constants (chunkBorder)
import World.Generate.Coordinates (chunkToGlobal)

-- * River Mouth Proximity

riverMouthRadius ∷ Int
riverMouthRadius = 24

-- | Pre-filter river mouths to those within range of a chunk,
--   then check proximity per tile. Avoids iterating 300+ mouths
--   per tile when most are far away.
filterNearbyMouths ∷ Int → ChunkCoord → [(Int, Int)] → [(Int, Int)]
filterNearbyMouths worldSize (ChunkCoord cx cy) mouths =
    let -- Chunk center in global coords
        centerX = cx * chunkSize + chunkSize `div` 2
        centerY = cy * chunkSize + chunkSize `div` 2
        -- Max distance: riverMouthRadius + half chunk + maxCoastalDist + border
        maxDist = riverMouthRadius + chunkSize + maxCoastalDist
        maxDist2 = maxDist * maxDist
    in filter (\(mx, my) →
        let (dx, dy) = wrappedDeltaUV worldSize centerX centerY mx my
            d2 = dx * dx + dy * dy
        in d2 ≤ maxDist2
        ) mouths

isNearRiverMouth ∷ Int → [(Int, Int)] → Int → Int → Bool
isNearRiverMouth worldSize mouths gx gy =
    any (\(mx, my) →
        let (dx, dy) = wrappedDeltaUV worldSize gx gy mx my
            d2 = dx * dx + dy * dy
        in d2 ≤ riverMouthRadius * riverMouthRadius
        ) mouths

-- * Sand Profile

sandProfile ∷ Int → Int
sandProfile dist
    | dist ≤ 1  = seaLevel + 1
    | dist ≤ 2  = seaLevel + 1
    | dist ≤ 3  = seaLevel + 2
    | dist ≤ 4  = seaLevel + 2
    | dist ≤ 5  = seaLevel + 3
    | dist ≤ 6  = seaLevel + 3
    | dist ≤ 7  = seaLevel + 4
    | dist ≤ 8  = seaLevel + 5
    | otherwise = seaLevel + 6

-- * Material Selection

beachMaterial ∷ Int → Word8 → Float → Word8
beachMaterial dist origMat roll
    | dist ≤ 3  = 55
    | dist ≤ 5  = if roll < 0.85 then 55 else 54
    | dist ≤ 7  = if roll < 0.6  then 55 else 54
    | otherwise = transitionMaterial origMat roll

transitionMaterial ∷ Word8 → Float → Word8
transitionMaterial origMat roll = case origMat of
    56 → if roll < 0.6 then 54 else 53
    60 → if roll < 0.6 then 54 else 53
    50 → if roll < 0.6 then 51 else 52
    57 → if roll < 0.5 then 52 else 54
    58 → if roll < 0.6 then 51 else 52
    53 → 53
    54 → 54
    55 → 55
    62 → if roll < 0.5 then 53 else 54
    64 → if roll < 0.5 then 53 else 54
    _  → if roll < 0.5 then 54 else 53

wetlandMaterial ∷ Int → Float → Word8
wetlandMaterial dist roll
    | dist ≤ 3 =
        if roll < 0.4 then 64
        else if roll < 0.7 then 62
        else 63
    | dist ≤ 5 =
        if roll < 0.3 then 62
        else if roll < 0.6 then 63
        else 64
    | otherwise =
        if roll < 0.4 then 62
        else if roll < 0.7 then 51
        else 50

deltaMaterial ∷ Int → Float → Word8
deltaMaterial dist roll
    | dist ≤ 3 =
        if roll < 0.4 then 55
        else if roll < 0.6 then 54
        else if roll < 0.8 then 64
        else 61
    | dist ≤ 5 =
        if roll < 0.3 then 53
        else if roll < 0.5 then 52
        else if roll < 0.7 then 50
        else 62
    | otherwise =
        if roll < 0.4 then 53
        else 54

-- * Coastal Erosion Pass

-- | Maximum coastal influence distance (tiles from the smoothed
--   shoreline that erosion reaches). Since the move to the global
--   pass ('identifyCoastalErosion', save v25) this is a pure look
--   knob — there is no window-margin constraint anymore. (The old
--   per-chunk windowed pass documented "must be < chunkBorder", an
--   invariant its own 12-pass contour smoother silently broke: the
--   total information horizon — preDist 14 + 12 diffusion passes +
--   BFS 10 — exceeded the 14-tile shared border, so adjacent windows
--   computed coastlines disagreeing by up to ~18z, rendered as
--   cliffs at chunk seams.)
--
--   #220 widened 10 → 28: gentle coasts need enough reach to ramp
--   from the beach to the plateau without a forced cliff at the old
--   10-tile edge. The table just records more deltas; 'applyCoastalTable'
--   replays them per-tile with no reach assumption.
maxCoastalDist ∷ Int
maxCoastalDist = 28

-- | Outer edge of the contour-smoothing band: full-strength smoothing
--   within 'maxCoastalDist', fading to zero here. Purely a look knob
--   (the global pass has no window-safety margin to respect).
smoothBand ∷ Int
smoothBand = maxCoastalDist + 6

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

-- * Distance Field

buildDistField ∷ Int → Int → VU.Vector Int → VU.Vector Int
buildDistField maxDist borderSize elevVec = runST $ do
    let borderArea = borderSize * borderSize
        sentinel = maxDist + 1
    distM ← VUM.replicate borderArea sentinel

    seeds ← VUM.new borderArea
    seedCount ← VUM.new 1
    VUM.write seedCount 0 (0 ∷ Int)

    forM_ [0 .. borderArea - 1] $ \idx → do
        let elev = elevVec VU.! idx
        when (elev ≤ seaLevel) $ do
            VUM.write distM idx 0
            sc ← VUM.read seedCount 0
            VUM.write seeds sc idx
            VUM.write seedCount 0 (sc + 1)

    sc0 ← VUM.read seedCount 0
    let bfsLevel currentSeeds currentCount level = do
            when (level ≤ maxDist ∧ currentCount > 0) $ do
                nextSeeds ← VUM.new borderArea
                nextCount ← VUM.new 1
                VUM.write nextCount 0 (0 ∷ Int)
                forM_ [0 .. currentCount - 1] $ \si → do
                    idx ← VUM.read currentSeeds si
                    let bx = idx `mod` borderSize
                        by = idx `div` borderSize
                        tryNeighbor nx ny =
                            when (nx ≥ 0 ∧ nx < borderSize
                                 ∧ ny ≥ 0 ∧ ny < borderSize) $ do
                                let nIdx = ny * borderSize + nx
                                old ← VUM.read distM nIdx
                                when (old > level + 1) $ do
                                    VUM.write distM nIdx (level + 1)
                                    nc ← VUM.read nextCount 0
                                    VUM.write nextSeeds nc nIdx
                                    VUM.write nextCount 0 (nc + 1)
                    tryNeighbor (bx - 1) by
                    tryNeighbor (bx + 1) by
                    tryNeighbor bx       (by - 1)
                    tryNeighbor bx       (by + 1)
                nc ← VUM.read nextCount 0
                bfsLevel nextSeeds nc (level + 1)

    bfsLevel seeds sc0 0
    VU.unsafeFreeze distM

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

-- * Sealed-Basin Breaching

-- | Minimum tile count for a sealed sub-sea basin to earn a breach.
breachMinSize ∷ Int
breachMinSize = 12

-- | Minimum depth below sea for a sealed basin to earn a breach.
breachMinDepth ∷ Int
breachMinDepth = 4

-- | Longest land channel (tiles) a breach may carve to reach the sea.
--   Generous on purpose: a long narrow mouth through a headland reads
--   as a fjord inlet, and an unbreached deep basin reads as a bug
--   (a sea-level "lake" 40z deep behind the shore).
breachMaxChannel ∷ Int
breachMaxChannel = 16

-- | Connect large, deep sub-sea basins to the open ocean by carving a
--   narrow channel through the shoreline ridge that seals them (#220).
--   Steep coasts keep their natural terrain, which frequently seals a
--   deep pre-coastal basin behind the shore; unbreached, those flood
--   later as sea-level "lakes" tens of z deep (the FLOATING_LAKE
--   family), half-classified as ocean by the oceanic-chunk rule. The
--   old universal flatten eroded those ridges below sea by accident —
--   this makes the same connection deliberately and minimally, and the
--   resulting one-tile mouths read as natural deep inlets on cliff
--   coasts. Channel tiles are LOWERED to sea-1 (they are land above
--   sea by construction, so the pass stays lower-only). Small or
--   shallow sealed ponds are left alone — a lagoon behind a sandbar
--   is a feature.
--
--   Topology note: decisions are made on CANONICAL tiles only (their
--   own 'wrapGlobalU' image) and carves are projected onto every grid
--   copy at the end — running naively on the square grid gives
--   near-seam basins independent canonical/alias component ids, and
--   one copy carves while its twin doesn't (a seam artifact, verified
--   on seed 12321). Adjacency deliberately does NOT wrap across the
--   u-seam: connectivity here must match what the engine's
--   'computeWorldEdgeOcean' (edges-as-walls, no wrap) will later
--   compute, or a basin whose only sub-sea route to the ocean crosses
--   the seam is "connected" to this pass yet sealed to the engine —
--   and floods as a deep sea-level lake anyway. A physically
--   cross-seam-connected basin just gains a redundant local inlet.
breachSealedBasins ∷ Int → VU.Vector MaterialId → VU.Vector Int
                   → VU.Vector Int
breachSealedBasins worldSize matVec elev = runST $ do
    let worldTiles = worldSize * chunkSize
        halfWorld = worldTiles `div` 2
        area = worldTiles * worldTiles
        inGrid gx gy = gx ≥ -halfWorld ∧ gx < halfWorld
                     ∧ gy ≥ -halfWorld ∧ gy < halfWorld
        toIdx gx gy = (gy + halfWorld) * worldTiles + (gx + halfWorld)
        isCanonical i =
            let gx = i `mod` worldTiles - halfWorld
                gy = i `div` worldTiles - halfWorld
                (gx', gy') = wrapGlobalU worldSize gx gy
            in gx' ≡ gx ∧ gy' ≡ gy
        -- Canonical neighbors under the ENGINE's connectivity model:
        -- no u-seam wrap (see topology note above).
        neighborIdxs idx =
            let gx = idx `mod` worldTiles - halfWorld
                gy = idx `div` worldTiles - halfWorld
            in [ nIdx
               | (nx, ny) ← [ (gx - 1, gy), (gx + 1, gy)
                            , (gx, gy - 1), (gx, gy + 1) ]
               , inGrid nx ny
               , let nIdx = toIdx nx ny
               , isCanonical nIdx
               ]

    -- Pass 1: label canonical sub-sea connected components
    -- (4-adjacency, wrap-aware).
    comp ← VUM.replicate area (-1 ∷ Int)
    queue ← VUM.new area
    nComps ← VUM.replicate 1 (0 ∷ Int)
    forM_ [0 .. area - 1] $ \i →
        when (isCanonical i ∧ elev VU.! i ≤ seaLevel) $ do
            ci ← VUM.read comp i
            when (ci ≡ -1) $ do
                cid ← VUM.read nComps 0
                VUM.write nComps 0 (cid + 1)
                VUM.write comp i cid
                VUM.write queue 0 i
                let bfs !qHead !qTail =
                        when (qHead < qTail) $ do
                            idx ← VUM.read queue qHead
                            newTail ← foldM
                                (\ !t nIdx → do
                                    nc ← VUM.read comp nIdx
                                    if nc ≡ -1 ∧ elev VU.! nIdx ≤ seaLevel
                                    then do
                                        VUM.write comp nIdx cid
                                        VUM.write queue t nIdx
                                        pure (t + 1)
                                    else pure t)
                                qTail (neighborIdxs idx)
                            bfs (qHead + 1) newTail
                bfs 0 1

    -- Pass 2: per-component size and minimum elevation.
    n ← VUM.read nComps 0
    if n ≤ 1
    then pure elev  -- zero or one water body: nothing to connect
    else do
        sizes ← VUM.replicate n (0 ∷ Int)
        minEs ← VUM.replicate n (maxBound ∷ Int)
        forM_ [0 .. area - 1] $ \i → do
            ci ← VUM.read comp i
            when (ci ≥ 0) $ do
                sz ← VUM.read sizes ci
                VUM.write sizes ci (sz + 1)
                me ← VUM.read minEs ci
                when (elev VU.! i < me) $
                    VUM.write minEs ci (elev VU.! i)

        -- Open ocean = largest component (first on ties — scan order
        -- is deterministic, and alias copies of near-seam water carry
        -- identical geometry, so the seam sees consistent choices).
        oceanRef ← VUM.replicate 1 (0 ∷ Int)
        forM_ [1 .. n - 1] $ \c → do
            best ← VUM.read oceanRef 0
            bsz ← VUM.read sizes best
            csz ← VUM.read sizes c
            when (csz > bsz) $ VUM.write oceanRef 0 c
        oceanId ← VUM.read oceanRef 0

        -- Pass 3: for each qualifying sealed basin, multi-source BFS
        -- through carvable land (level-order, deterministic) until a
        -- tile adjacent to the open ocean is found, then carve the
        -- parent chain to sea-1. Basins with no route within
        -- 'breachMaxChannel' stay sealed (deep inland depressions).
        qual ← VUM.replicate n False
        forM_ [0 .. n - 1] $ \c →
            when (c ≢ oceanId) $ do
                sz ← VUM.read sizes c
                me ← VUM.read minEs c
                when (sz ≥ breachMinSize ∧ me ≤ seaLevel - breachMinDepth) $
                    VUM.write qual c True

        -- One scan: collect the carvable-land ring around every
        -- qualifying basin, grouped per basin (ascending tile order).
        seedMap ← foldM
            (\ !m i → do
                ci ← VUM.read comp i
                if ci < 0 then pure m
                else do
                    q ← VUM.read qual ci
                    if not q then pure m
                    else do
                        ring ← foldM
                            (\ !acc nIdx →
                                if elev VU.! nIdx > seaLevel
                                   ∧ matVec VU.! nIdx ≢ matGlacier
                                then pure (nIdx : acc)
                                else pure acc)
                            [] (neighborIdxs i)
                        pure (if null ring then m
                              else IM.insertWith (⧺) ci ring m))
            IM.empty [0 .. area - 1]

        carveMask ← VUM.replicate area False
        -- visited/parent are generation-stamped by basin id so they
        -- need no clearing between basins.
        visited ← VUM.replicate area (-1 ∷ Int)
        parent  ← VUM.replicate area (-1 ∷ Int)
        let carvable i =
                pure (elev VU.! i > seaLevel ∧ matVec VU.! i ≢ matGlacier)
            carveChain i = do
                VUM.write carveMask i True
                p ← VUM.read parent i
                when (p ≥ 0) $ carveChain p

        forM_ (IM.toAscList seedMap) $ \(c, ring) → do
            -- Seed the queue (dedupe via the visited stamp).
            seedTail ← foldM
                (\ !t nIdx → do
                    vs ← VUM.read visited nIdx
                    ok ← carvable nIdx
                    if ok ∧ vs ≢ c
                    then do
                        VUM.write visited nIdx c
                        VUM.write parent nIdx (-1)
                        VUM.write queue t nIdx
                        pure (t + 1)
                    else pure t)
                0 ring
            -- Level-order search for a tile touching the ocean.
            let search !qHead !qTail !depth
                  | qHead ≥ qTail ∨ depth > breachMaxChannel = pure (-1)
                  | otherwise = do
                      (newTail, hit) ← foldM
                          (\(!t, !h) qi → do
                              idx ← VUM.read queue qi
                              if h ≥ 0 then pure (t, h)
                              else do
                                touches ← foldM
                                    (\ !acc nIdx → do
                                        nc ← VUM.read comp nIdx
                                        pure (acc ∨ nc ≡ oceanId))
                                    False (neighborIdxs idx)
                                if touches then pure (t, idx)
                                else do
                                  t' ← foldM
                                      (\ !tt nIdx → do
                                          ok ← carvable nIdx
                                          vs ← VUM.read visited nIdx
                                          if ok ∧ vs ≢ c
                                          then do
                                              VUM.write visited nIdx c
                                              VUM.write parent nIdx idx
                                              VUM.write queue tt nIdx
                                              pure (tt + 1)
                                          else pure tt)
                                      t (neighborIdxs idx)
                                  pure (t', -1))
                          (qTail, -1) [qHead .. qTail - 1]
                      if hit ≥ 0
                      then pure hit
                      else search qTail newTail (depth + 1)
            hit ← search 0 seedTail 0
            when (hit ≥ 0) $ carveChain hit

        -- Project canonical carves onto every grid copy (canonical +
        -- u-seam alias) so the per-chunk tables stay alias-consistent.
        out ← VU.thaw elev
        forM_ [0 .. area - 1] $ \i → do
            let gx = i `mod` worldTiles - halfWorld
                gy = i `div` worldTiles - halfWorld
                (gx', gy') = wrapGlobalU worldSize gx gy
            when (inGrid gx' gy') $ do
                m ← VUM.read carveMask (toIdx gx' gy')
                when m $ VUM.write out i (seaLevel - 1)
        VU.unsafeFreeze out

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
--   within 'smoothBand' of the coast are smoothed, with a linear
--   fade from maxCoastalDist outward. (Historically the bound was
--   chunkBorder for window-overlap safety; the pass has been global
--   since save v25, so the bound is purely a look knob now.)
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

-- * Shoreline Noise

-- | Coherent 2D value noise for smoothing the depositional beach edge.
--   Wavelength 48 tiles (~3 chunks), amplitude ±2 tiles.
--   Returns a smooth float that shifts effective distance from ocean.
shorelineOffset ∷ Word64 → Int → Int → Float
shorelineOffset seed gx gy =
    let fx = fromIntegral gx / 48.0 ∷ Float
        fy = fromIntegral gy / 48.0 ∷ Float
        ix = floor fx ∷ Int
        iy = floor fy ∷ Int
        fracX = fx - fromIntegral ix
        fracY = fy - fromIntegral iy
        sx = fracX * fracX * (3.0 - 2.0 * fracX)
        sy = fracY * fracY * (3.0 - 2.0 * fracY)
        h00 = shoreHash seed ix       iy
        h10 = shoreHash seed (ix + 1) iy
        h01 = shoreHash seed ix       (iy + 1)
        h11 = shoreHash seed (ix + 1) (iy + 1)
        top    = h00 + sx * (h10 - h00)
        bottom = h01 + sx * (h11 - h01)
        val    = top  + sy * (bottom - top)
    in (val * 2.0 - 1.0) * 2.0  -- range ±2

shoreHash ∷ Word64 → Int → Int → Float
shoreHash seed x y =
    let h = coastHash seed (x * 7919) (y * 6271)
    in fromIntegral (h .&. 0xFFFF) / 65535.0

-- * Hash Utility

{-# INLINE coastHash #-}
coastHash ∷ Word64 → Int → Int → Word64
coastHash seed gx gy =
    let h0 = seed `xor` 0x9E3779B97F4A7C15
        h1 = h0 `xor` (fromIntegral gx * 0x517cc1b727220a95)
        h2 = h1 `xor` (fromIntegral gy * 0x6c62272e07bb0142)
        h3 = h2 `xor` (h2 `shiftR` 33)
        h4 = h3 * 0xff51afd7ed558ccd
        h5 = h4 `xor` (h4 `shiftR` 33)
    in h5
