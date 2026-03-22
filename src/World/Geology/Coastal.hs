{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Coastal
    ( applyCoastalErosion
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
import Control.Monad (when, forM_)
import Control.Monad.ST (runST)
import Data.Bits (xor, shiftR, (.&.))
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import World.Types
import World.Material (MaterialId(..), getMaterialProps, MaterialProps(..)
                      , MaterialRegistry(..))
import World.Plate (TectonicPlate(..), twoNearestPlates
                   , BoundaryType(..), classifyBoundary, wrapGlobalU)
import World.Fluid.Ocean (hasAnyOceanFluid)
import World.Geology.Hash (wrappedDeltaUV)
import World.Constants (seaLevel)
import World.Chunk.Types (chunkSize)
import World.Generate.Constants (chunkBorder)
import World.Generate.Coordinates (chunkToGlobal)

-- * Coast Classification

data CoastType
    = ErosionalRocky
    | ErosionalGravel
    | DepositionalSandy
    | DepositionalWetland
    | DeltaicCoast
    | SubmergenRocky
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
               if mixedMargin ∧ activity > 0.7 ∧ hardness > 0.5
               then ErosionalRocky
               else if mixedMargin ∧ activity > 0.6 ∧ hardness > 0.35
               then ErosionalGravel
               else DepositionalSandy
           Divergent _ →
               if activity > 0.5 ∧ hardness > 0.4
               then SubmergenRocky
               else DepositionalSandy
           Transform _ →
               if activity > 0.6 ∧ hardness > 0.4
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
    | dist ≤ 2  = seaLevel + 2
    | dist ≤ 3  = seaLevel + 3
    | otherwise = seaLevel + 4

outcroppHardness ∷ Float
outcroppHardness = 0.7

-- * Material Selection

beachMaterial ∷ Int → Word8 → Float → Word8
beachMaterial dist origMat roll
    | dist ≤ 2  = 55
    | dist ≤ 3  = if roll < 0.85 then 55 else 54
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

-- | Maximum coastal influence distance. Must not exceed chunkBorder
--   (currently 4) — otherwise the BFS distance field can't see ocean
--   tiles beyond the border overlap and produces wrong distances near
--   chunk boundaries, causing visible seams.
maxCoastalDist ∷ Int
maxCoastalDist = chunkBorder

applyCoastalErosion ∷ Word64 → Int → [TectonicPlate] → MaterialRegistry
                    → GeoTimeline → OceanMap → ChunkCoord
                    → (VU.Vector Int, VU.Vector MaterialId)
                    → (VU.Vector Int, VU.Vector MaterialId)
applyCoastalErosion seed worldSize plates registry timeline oceanMap coord (elevVec, matVec) =
    if not (hasAnyOceanFluid oceanMap coord)
    then (elevVec, matVec)
    else
    let borderSize = chunkSize + 2 * chunkBorder
        borderArea = borderSize * borderSize

        fromIndex idx =
            let (by, bx) = idx `divMod` borderSize
            in (bx - chunkBorder, by - chunkBorder)

        -- Pre-filter river mouths to those near this chunk.
        -- Typical: 300+ mouths → 0-3 nearby. Saves ~30K distance
        -- calculations per coastal chunk.
        allMouths = concatMap extractMouths (gtPeriods timeline)
        extractMouths period =
            [ (mx, my)
            | HydroEvent (RiverFeature rp) ← gpEvents period
            , let GeoCoord mx my = rpMouthRegion rp
            ]
        nearbyMouths = filterNearbyMouths worldSize coord allMouths

        ChunkCoord cx cy = coord

        -- Single-pass outlier removal near sea level. Eliminates
        -- isolated land tiles surrounded by ocean and vice versa.
        -- Only 1 tile of propagation — stays within the chunk border
        -- overlap so neighboring chunks produce identical results (no seams).
        smoothedBaseElev = removeCoastalOutliers borderSize elevVec

        distField = buildDistField borderSize smoothedBaseElev

        (passElev, passMat) = runST $ do
            elevM ← VUM.new borderArea
            matM  ← VUM.new borderArea
            forM_ [0 .. borderArea - 1] $ \idx → do
                -- Start from ORIGINAL timeline elevation. The smoothed
                -- version is only used for BFS ocean detection — writing
                -- it here would leak outlier-removal artifacts into
                -- non-coastal tiles, creating chunk boundary seams.
                VUM.write elevM idx (elevVec VU.! idx)
                VUM.write matM  idx (matVec  VU.! idx)

            forM_ [0 .. borderArea - 1] $ \idx → do
                let dist = distField VU.! idx
                    (lx, ly) = fromIndex idx
                -- Process ALL tiles in the border area, not just interior.
                -- Border tiles must be eroded identically by both chunks
                -- that share them, so the smoothing pass sees consistent
                -- values and doesn't create seams at chunk boundaries.
                when (dist > 0 ∧ dist ≤ maxCoastalDist) $ do
                    let elev = elevVec VU.! idx
                        mat  = matVec  VU.! idx
                        hardness = mpHardness (getMaterialProps registry mat)

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

                    if isDepositional coastType
                    then do
                        let isOutcrop = hardness ≥ outcroppHardness
                                      ∧ elev > sandLevel + 2
                            chooseMat = case coastType of
                                DepositionalWetland → wetlandMaterial effDist roll
                                DeltaicCoast → deltaMaterial effDist roll
                                _ → beachMaterial effDist (unMaterialId mat) roll
                        if isOutcrop
                        then pure ()
                        else do
                            let newElev = min elev sandLevel
                            VUM.write elevM idx newElev
                            VUM.write matM idx (MaterialId chooseMat)
                    else do
                        let aboveSea = elev - seaLevel
                        case coastType of
                            ErosionalRocky → do
                                when (dist ≤ 2 ∧ aboveSea > 0) $ do
                                    let cut = min aboveSea
                                              (round (2.0 * (1.0 - hardness)))
                                    VUM.write elevM idx (elev - cut)
                                when (dist ≤ 2 ∧ hardness < 0.3) $
                                    VUM.write matM idx (MaterialId
                                        (if roll < 0.6 then 65 else 66))
                            ErosionalGravel → do
                                when (dist ≤ 3 ∧ aboveSea > 0) $ do
                                    let cut = min aboveSea
                                              (round (3.0 * (1.0 - hardness)))
                                    VUM.write elevM idx (elev - cut)
                                when (dist ≤ 3 ∧ hardness < 0.4) $
                                    VUM.write matM idx (MaterialId
                                        (if roll < 0.5 then 65 else 66))
                            SubmergenRocky → do
                                when (dist ≤ 2 ∧ aboveSea > 0) $ do
                                    let cut = min aboveSea
                                              (round (2.5 * (1.0 - hardness)))
                                    VUM.write elevM idx (elev - cut)
                                when (dist ≤ 2 ∧ hardness < 0.4) $
                                    VUM.write matM idx (MaterialId
                                        (if roll < 0.5 then 65 else 66))
                            TransformCoast → do
                                when (dist ≤ 3 ∧ aboveSea > 0) $ do
                                    let cut = min aboveSea
                                              (round (2.0 * (1.0 - hardness)))
                                    VUM.write elevM idx (elev - cut)
                                when (dist ≤ 3 ∧ hardness < 0.4) $
                                    VUM.write matM idx (MaterialId
                                        (if roll < 0.4 then 65
                                         else if roll < 0.6 then 55
                                         else 66))
                            _ → pure ()

            elevF ← VU.unsafeFreeze elevM
            matF  ← VU.unsafeFreeze matM
            pure (elevF, matF)

        smoothedElev = smoothCoast 5 borderSize distField passElev

    in (smoothedElev, passMat)

-- * Distance Field

buildDistField ∷ Int → VU.Vector Int → VU.Vector Int
buildDistField borderSize elevVec = runST $ do
    let borderArea = borderSize * borderSize
        sentinel = maxCoastalDist + 1
    distM ← VUM.replicate borderArea sentinel

    seeds ← VUM.new borderArea
    seedCount ← VUM.new 1
    VUM.write seedCount 0 (0 ∷ Int)

    -- Seed BFS from ocean tiles, but skip the outermost border ring.
    -- The timeline erosion produces slightly inconsistent elevations
    -- at the border edge (neighbor fallback to self). If those tiles
    -- become BFS seeds in one chunk but not the adjacent chunk, the
    -- distance fields diverge → different beach processing → seam.
    forM_ [0 .. borderArea - 1] $ \idx → do
        let elev = elevVec VU.! idx
            bx = idx `mod` borderSize
            by = idx `div` borderSize
        when (elev ≤ seaLevel
             ∧ bx > 0 ∧ bx < borderSize - 1
             ∧ by > 0 ∧ by < borderSize - 1) $ do
            VUM.write distM idx 0
            sc ← VUM.read seedCount 0
            VUM.write seeds sc idx
            VUM.write seedCount 0 (sc + 1)

    sc0 ← VUM.read seedCount 0
    let bfsLevel currentSeeds currentCount level = do
            when (level ≤ maxCoastalDist ∧ currentCount > 0) $ do
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
                        -- Don't read from the outermost border ring — those
                        -- tiles have potentially inconsistent values from
                        -- timeline erosion (neighbor fallback to self at
                        -- the border edge). Reading them would propagate
                        -- the inconsistency inward across smoothing iterations.
                        readN nx ny
                            | nx > 0 ∧ nx < borderSize - 1
                              ∧ ny > 0 ∧ ny < borderSize - 1
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

-- * Coastline Pre-Smoothing

-- | Single-pass outlier removal at the land/ocean boundary.
--   For tiles near sea level, count how many of the 8 neighbors
--   are land vs ocean. If a tile disagrees with the strong majority
--   of its neighbors, adjust it to match.
--
--   Only 1 tile of propagation per pass, so it stays within the
--   chunk border overlap and produces identical results in
--   neighboring chunks (no seams).
removeCoastalOutliers ∷ Int → VU.Vector Int → VU.Vector Int
removeCoastalOutliers borderSize elev =
    let borderArea = borderSize * borderSize
        threshold  = 10  -- only consider tiles within ±10 of sea level
    in runST $ do
        em ← VUM.new borderArea
        forM_ [0 .. borderArea - 1] $ \i →
            VUM.write em i (elev VU.! i)
        forM_ [0 .. borderArea - 1] $ \idx → do
            let e = elev VU.! idx
            let bx = idx `mod` borderSize
                by = idx `div` borderSize
            -- Skip outermost border ring and tiles far from sea level.
            -- Edge tiles have out-of-bounds neighbors that differ
            -- between chunks — skipping ensures determinism.
            when (abs (e - seaLevel) ≤ threshold
                 ∧ bx > 0 ∧ bx < borderSize - 1
                 ∧ by > 0 ∧ by < borderSize - 1) $ do
                let readE nx ny
                        | nx ≥ 0 ∧ nx < borderSize
                          ∧ ny ≥ 0 ∧ ny < borderSize
                            = elev VU.! (ny * borderSize + nx)
                        | otherwise = e
                    neighbors =
                        [ readE (bx + dx) (by + dy)
                        | dx ← [-1, 0, 1], dy ← [-1, 0, 1]
                        , dx ≢ 0 ∨ dy ≢ 0
                        ]
                    landCount = length (filter (> seaLevel) neighbors)
                    oceanCount = 8 - landCount
                -- Isolated land tile surrounded mostly by ocean:
                -- pull it below sea level (average of ocean neighbors)
                if e > seaLevel ∧ oceanCount ≥ 6
                then do
                    let oceanSum = sum (filter (≤ seaLevel) neighbors)
                    VUM.write em idx (oceanSum `div` max 1 oceanCount)
                -- Isolated ocean tile surrounded mostly by land:
                -- push it above sea level (average of land neighbors)
                else if e ≤ seaLevel ∧ landCount ≥ 6
                then do
                    let landSum = sum (filter (> seaLevel) neighbors)
                    VUM.write em idx (landSum `div` max 1 landCount)
                else pure ()
        VU.unsafeFreeze em

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
