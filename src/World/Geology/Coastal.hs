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
    | dist ≤ 3  = seaLevel + 1
    | dist ≤ 5  = seaLevel + 2
    | otherwise = seaLevel + 3

outcroppHardness ∷ Float
outcroppHardness = 0.6

-- * Material Selection

beachMaterial ∷ Int → Word8 → Float → Word8
beachMaterial dist origMat roll
    | dist ≤ 4  = 55
    | dist ≤ 6  = if roll < 0.85 then 55 else 54
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

maxCoastalDist ∷ Int
maxCoastalDist = 8

applyCoastalErosion ∷ Word64 → Int → [TectonicPlate] → MaterialRegistry
                    → GeoTimeline → OceanMap → ChunkCoord
                    → (VU.Vector Int, VU.Vector MaterialId)
                    → (VU.Vector Int, VU.Vector MaterialId)
applyCoastalErosion seed worldSize plates registry timeline oceanMap coord (elevVec, matVec) =
    -- Early exit for chunks far from the coast. Only skip if
    -- this chunk AND all 8 neighbors are non-oceanic — those
    -- chunks can't have coastal tiles even in their border zone.
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

        -- Cache plate classification at chunk center. Plate boundaries
        -- change slowly over 16 tiles, so one lookup suffices for the
        -- entire chunk instead of per-tile twoNearestPlates calls.
        ChunkCoord cx cy = coord
        chunkCenterGX = cx * chunkSize + chunkSize `div` 2
        chunkCenterGY = cy * chunkSize + chunkSize `div` 2
        (chunkCenterGX', chunkCenterGY') =
            wrapGlobalU worldSize chunkCenterGX chunkCenterGY
        ((chunkPlateA, chunkDistA), (chunkPlateB, chunkDistB)) =
            twoNearestPlates seed worldSize plates chunkCenterGX' chunkCenterGY'
        chunkBoundary = classifyBoundary worldSize chunkPlateA chunkPlateB
        chunkBoundaryDist = (chunkDistB - chunkDistA) / 2.0
        maxBDist = fromIntegral worldSize * 4.0 ∷ Float
        chunkBoundaryFade = min 1.0 (abs chunkBoundaryDist / maxBDist)

        distField = buildDistField borderSize elevVec

        (passElev, passMat) = runST $ do
            elevM ← VUM.new borderArea
            matM  ← VUM.new borderArea
            forM_ [0 .. borderArea - 1] $ \idx → do
                VUM.write elevM idx (elevVec VU.! idx)
                VUM.write matM  idx (matVec  VU.! idx)

            forM_ [0 .. borderArea - 1] $ \idx → do
                let dist = distField VU.! idx
                    (lx, ly) = fromIndex idx
                    isInterior = lx ≥ 0 ∧ lx < chunkSize
                               ∧ ly ≥ 0 ∧ ly < chunkSize
                when (dist > 0 ∧ dist ≤ maxCoastalDist ∧ isInterior) $ do
                    let elev = elevVec VU.! idx
                        mat  = matVec  VU.! idx
                        hardness = mpHardness (getMaterialProps registry mat)

                        -- Use cached chunk-level plate classification
                        nearRiver = isNearRiverMouth worldSize nearbyMouths
                            (cx * chunkSize + lx) (cy * chunkSize + ly)
                        coastType = classifyCoast chunkBoundary
                            (plateIsLand chunkPlateA) (plateIsLand chunkPlateB)
                            hardness 0.0 chunkBoundaryFade nearRiver

                        localHash = coastHash seed
                            (cx * chunkSize + lx) (cy * chunkSize + ly)
                        roll = fromIntegral (localHash .&. 0xFF) / 255.0 ∷ Float

                        sandLevel = sandProfile dist

                    if isDepositional coastType
                    then do
                        let isOutcrop = hardness ≥ outcroppHardness
                                      ∧ elev > sandLevel
                            chooseMat = case coastType of
                                DepositionalWetland → wetlandMaterial dist roll
                                DeltaicCoast → deltaMaterial dist roll
                                _ → beachMaterial dist (unMaterialId mat) roll
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

        smoothedElev = smoothCoast 3 borderSize distField passElev

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

    forM_ [0 .. borderArea - 1] $ \idx → do
        let elev = elevVec VU.! idx
        when (elev ≤ seaLevel) $ do
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
        smoothed = runST $ do
            em ← VUM.new borderArea
            forM_ [0 .. borderArea - 1] $ \i →
                VUM.write em i (elev VU.! i)
            forM_ [0 .. borderArea - 1] $ \idx → do
                let d = distF VU.! idx
                    bx = idx `mod` borderSize
                    by = idx `div` borderSize
                    lx = bx - chunkBorder
                    ly = by - chunkBorder
                    isInt = lx ≥ 0 ∧ lx < chunkSize
                          ∧ ly ≥ 0 ∧ ly < chunkSize
                when (d > 0 ∧ d ≤ maxCoastalDist ∧ isInt) $ do
                    e ← VUM.read em idx
                    let readN nx ny
                            | nx ≥ 0 ∧ nx < borderSize
                              ∧ ny ≥ 0 ∧ ny < borderSize
                                = pure (elev VU.! (ny * borderSize + nx))
                            | otherwise = pure e
                    n ← readN bx (by - 1)
                    s ← readN bx (by + 1)
                    w ← readN (bx - 1) by
                    eN ← readN (bx + 1) by
                    let avg = (n + s + w + eN + e) `div` 5
                    VUM.write em idx (min e avg)
            VU.unsafeFreeze em
    in smoothCoast (iters - 1) borderSize distF smoothed

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
