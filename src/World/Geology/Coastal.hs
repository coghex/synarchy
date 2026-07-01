{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Coastal
    ( identifyCoastalErosion
    , applyCoastalTable
    -- * Zoom map coastal support
    , CoastType(..)
    , classifyCoast
    , isDepositional
    , beachMaterial
    , wetlandMaterial
    , deltaMaterial
    , coastHash
    , sandProfile
    , outcroppHardness
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
import World.Plate (twoNearestPlates, BoundaryType(..), classifyBoundary, wrapGlobalU)
import World.Geology.Coastal.Types (CoastalTable(..))
import World.Geology.Hash (wrappedDeltaUV)
import World.Generate.Constants (chunkBorder)
import World.Generate.Coordinates (chunkToGlobal)

-- * Coast Classification

data CoastType
    = ErosionalRocky
    | ErosionalGravel
    | DepositionalSandy
    | DepositionalWetland
    | DeltaicCoast
    | SubmergentRocky
    | TransformCoast
    deriving (Eq, Show)

isDepositional ∷ CoastType → Bool
isDepositional DepositionalSandy   = True
isDepositional DepositionalWetland = True
isDepositional DeltaicCoast        = True
isDepositional _                   = False

classifyCoast ∷ BoundaryType → Bool → Bool → Float → Float → Float
              → Bool → CoastType
classifyCoast boundary aIsLand bIsLand hardness slope boundaryFade nearRiver =
    let activity = case boundary of
            Convergent str → str * (1.0 - boundaryFade * 0.5)
            Divergent  str → str * (1.0 - boundaryFade * 0.5)
            Transform  str → str * (1.0 - boundaryFade * 0.7)
        mixedMargin = aIsLand /= bIsLand
    in if nearRiver
       then if slope < 0.1 then DepositionalWetland else DeltaicCoast
       else case boundary of
           Convergent _ →
               if mixedMargin ∧ activity > 0.8 ∧ hardness > 0.7
               then ErosionalRocky
               else if mixedMargin ∧ activity > 0.7 ∧ hardness > 0.5
               then ErosionalGravel
               else DepositionalSandy
           Divergent _ →
               if activity > 0.7 ∧ hardness > 0.6
               then SubmergentRocky
               else DepositionalSandy
           Transform _ →
               if activity > 0.7 ∧ hardness > 0.6
               then TransformCoast
               else DepositionalSandy

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

outcroppHardness ∷ Float
outcroppHardness = 0.7

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
maxCoastalDist ∷ Int
maxCoastalDist = 10

-- | Global coastal pass. Runs ONCE at world init on the stitched
--   pre-coastal terrain (per-chunk timeline windows are
--   window-position-independent, so the stitch is the unambiguous
--   pre-coastal world), and records the result as per-chunk deltas.
--   Chunk gen applies the table ('applyCoastalTable') instead of
--   re-running a windowed pass — every chunk reads the same
--   coastline answer, eliminating the seam-cliff divergence class.
--
--   Identical math to the old per-chunk pass — preDist BFS, 12-pass
--   contour smoothing, dist BFS, per-tile erosion (plate
--   classification, river-mouth proximity, hash rolls, shoreline
--   noise), 10-pass beach smoothing — just on the world grid, where
--   there are no window edges. Per-tile hash/plate coords are
--   canonical, which also fixes the old hairline seam at the wrap
--   meridian (windows crossing the wrap hashed unwrapped coords).
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

        preDistField = buildDistField chunkBorder worldTiles elevG

        -- Multi-pass contour smoothing near sea level, hardness-aware
        -- (soft rock coasts become smooth, hard rock retains rugged
        -- headlands). Same constants as ever; preDistField now only
        -- bounds the visual band, not a seam-safety margin.
        smoothedContour = smoothCoastalContour 12 worldTiles globalMat
                              registry preDistField elevG

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
                    idx = worldIdx (cx * chunkSize + lx) (cy * chunkSize + ly)
                    dist = distField VU.! idx
                when (dist > 0 ∧ dist ≤ maxCoastalDist) $ do
                    let elev = smoothedContour VU.! idx
                        mat  = globalMat VU.! idx
                    -- Never erode glacier boundary tiles — they mark the
                    -- impassable world edge and must not be replaced with
                    -- coastal materials.
                    when (mat ≢ matGlacier) $ do
                      let hardness = mpHardness (getMaterialProps registry mat)

                          -- Per-tile plate classification. Computing at the
                          -- chunk center caused hard seams where adjacent
                          -- chunks straddled a plate boundary. Per-tile is
                          -- correct and only runs for coastal tiles (~10-20%
                          -- of a coastal chunk).
                          gxPlate = cx * chunkSize + lx
                          gyPlate = cy * chunkSize + ly
                          (gxP', gyP') = wrapGlobalU worldSize gxPlate gyPlate
                          ((plateA, distA), (plateB, distB)) =
                              twoNearestPlates seed worldSize plates gxP' gyP'
                          tileBoundary = classifyBoundary worldSize plateA plateB
                          tileBoundaryDist = (distB - distA) / 2.0
                          maxBDist = fromIntegral worldSize * 4.0 ∷ Float
                          tileBoundaryFade = min 1.0 (abs tileBoundaryDist / maxBDist)
                          nearRiver = isNearRiverMouth worldSize nearbyMouths
                              gxPlate gyPlate
                          coastType = classifyCoast tileBoundary
                              (plateIsLand plateA) (plateIsLand plateB)
                              hardness 0.0 tileBoundaryFade nearRiver

                          localHash = coastHash seed
                              (cx * chunkSize + lx) (cy * chunkSize + ly)
                          roll = fromIntegral (localHash .&. 0xFF) / 255.0 ∷ Float

                          gx = cx * chunkSize + lx
                          gy = cy * chunkSize + ly
                          offset = shorelineOffset seed gx gy
                          effDist = max 1 (dist + round offset)
                          sandLevel = sandProfile effDist

                      -- Terrain target: every coast type gets a slope.
                      -- Depositional coasts get flat sandy beaches.
                      -- Erosional coasts get steeper rocky slopes.
                      -- Outcrops (hard rock headlands) get the steepest.
                      let isOutcrop = hardness ≥ outcroppHardness
                                    ∧ elev > sandLevel + 8
                          terrainTarget
                            | isDepositional coastType ∧ not isOutcrop
                                = sandLevel
                            | isDepositional coastType ∧ isOutcrop
                                = sandLevel + 8 + dist * 2
                            | otherwise
                                = seaLevel + dist * 2

                      -- Lower terrain toward the target (never raise)
                      when (terrainTarget < elev) $
                          VUM.write elevM idx terrainTarget

                      -- Material: depositional coasts get beach material,
                      -- erosional coasts get gravel/rock near the waterline.
                      if isDepositional coastType ∧ not isOutcrop
                      then do
                          let chooseMat = case coastType of
                                  DepositionalWetland → wetlandMaterial effDist roll
                                  DeltaicCoast → deltaMaterial effDist roll
                                  _ → beachMaterial effDist (unMaterialId mat) roll
                          VUM.write matM idx (MaterialId chooseMat)
                      else when (dist ≤ 4) $ do
                          let gravelMat
                                | roll < 0.5 = 54  -- gravel
                                | otherwise  = 53  -- coarse sand
                          VUM.write matM idx (MaterialId gravelMat)
            elevF ← VU.unsafeFreeze elevM
            matF  ← VU.unsafeFreeze matM
            pure (elevF, matF)

        smoothedElev = smoothCoast 10 worldTiles distField passElev

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

smoothCoast ∷ Int → Int → VU.Vector Int → VU.Vector Int → VU.Vector Int
smoothCoast 0 _ _ elev = elev
smoothCoast iters borderSize distF elev =
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
                when (d > 0 ∧ d ≤ maxCoastalDist
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
    in smoothCoast (iters - 1) borderSize distF smoothed

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
--   within chunkBorder distance of the coast are smoothed, with a
--   linear fade from maxCoastalDist to chunkBorder. This ensures
--   the smoothing zone stays within the bordered region overlap,
--   so adjacent chunks produce identical results. Without this
--   bound, the elevation-only check (bandWidth=60) would smooth
--   inland terrain far from the coast, creating hard seams at
--   chunk boundaries where one chunk runs coastal erosion and
--   its neighbor doesn't.
--
--   Uses Jacobi iteration (reads from immutable previous pass) so
--   overlapping border tiles in adjacent chunks produce identical
--   results regardless of processing order — no seams.
smoothCoastalContour ∷ Int → Int → VU.Vector MaterialId → MaterialRegistry
                     → VU.Vector Int → VU.Vector Int → VU.Vector Int
smoothCoastalContour 0 _ _ _ _ elev = elev
smoothCoastalContour iters borderSize matVec registry preDistField elev =
    let borderArea = borderSize * borderSize
        bandWidth  = 60 ∷ Int  -- process tiles within ±60 elev of seaLevel
        fadeStart  = 45 ∷ Int  -- full strength below this, fade above
        -- Spatial bound: smoothing fades from full strength at
        -- maxCoastalDist to zero at chunkBorder. This keeps the
        -- effect within the bordered region overlap.
        smoothDistMax = chunkBorder
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
                        -- linear fade to zero at chunkBorder distance.
                        spatialFade
                          | preDist ≤ maxCoastalDist = 1.0
                          | fadeDenom ≤ 0            = 0.0
                          | otherwise = max 0.0
                              (fromIntegral (smoothDistMax - preDist)
                              / fromIntegral fadeDenom) ∷ Float
                        strength = elevFade * hardFade * spatialFade
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
                        -- Below sea level: just average (fill small holes).
                        let target
                              | e > seaLevel = min avg (minN + 2)
                              | otherwise    = avg
                            delta = fromIntegral (target - e) ∷ Float
                            newE = e + round (strength * delta)
                        VUM.write em idx newE
            VU.unsafeFreeze em
    in smoothCoastalContour (iters - 1) borderSize matVec registry preDistField smoothed

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
