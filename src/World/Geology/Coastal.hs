{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Coastal
    ( applyCoastalErosion
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
import World.Geology.Hash (wrappedDeltaUV)
import World.Constants (seaLevel)
import World.Chunk.Types (chunkSize)
import World.Generate.Constants (chunkBorder)
import World.Generate.Coordinates (chunkToGlobal)

-----------------------------------------------------------
-- Coast Classification
-----------------------------------------------------------

data CoastType
    = ErosionalRocky      -- ^ Active convergent margin → cliffs
    | ErosionalGravel     -- ^ Active margin, softer rock → gravel shores
    | DepositionalSandy   -- ^ Passive margin → sandy beaches
    | DepositionalWetland -- ^ Near river mouth, low-energy → wetlands
    | DeltaicCoast        -- ^ At river mouth → deltaic sediment fan
    | SubmergenRocky      -- ^ Divergent margin → fjord-like
    | TransformCoast      -- ^ Transform fault → mixed
    deriving (Eq, Show)

-- | Is this coast type one where sand gets deposited?
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
               -- Only strongly convergent mixed margins with hard rock
               -- produce erosional cliffs. Most coastlines, even at
               -- convergent boundaries, accumulate sand over time.
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

-----------------------------------------------------------
-- River Mouth Proximity
-----------------------------------------------------------

riverMouthRadius ∷ Int
riverMouthRadius = 24

isNearRiverMouth ∷ Int → [(Int, Int)] → Int → Int → Bool
isNearRiverMouth worldSize mouths gx gy =
    any (\(mx, my) →
        let (dx, dy) = wrappedDeltaUV worldSize gx gy mx my
            d2 = dx * dx + dy * dy
        in d2 ≤ riverMouthRadius * riverMouthRadius
        ) mouths

-----------------------------------------------------------
-- Sand Profile
-----------------------------------------------------------

-- | The sand deposition surface: a smooth profile rising gently
--   from sea level inland. Sand accumulates up to this height.
--   Rises ~1 tile per 3 tiles of distance — very gentle.
--
--   dist 1   → seaLevel + 1
--   dist 2-3 → seaLevel + 1
--   dist 4-5 → seaLevel + 2
--   dist 6-8 → seaLevel + 3
sandProfile ∷ Int → Int
sandProfile dist
    | dist ≤ 3  = seaLevel + 1
    | dist ≤ 5  = seaLevel + 2
    | otherwise = seaLevel + 3

-- | How far above the sand profile a tile can be and still get
--   covered by sand (soft materials only). Hard rock always
--   pokes through if above the profile.
sandCoverMargin ∷ Int
sandCoverMargin = 3

-- | Hardness threshold for rock outcrops. Materials harder than
--   this resist sand coverage and create outcrops.
-- | Only igneous and metamorphic rock (granite 0.7, basalt 0.6,
--   gneiss 0.65, quartzite 0.8) create outcrops on beaches.
--   Sedimentary rock (sandstone 0.5, limestone 0.45) gets eroded.
outcroppHardness ∷ Float
outcroppHardness = 0.6

-----------------------------------------------------------
-- Material Selection
-----------------------------------------------------------

-- | Choose the beach material based on distance from ocean and
--   the original inland material. Creates natural transitions:
--   pure sand near water, blending to sandy variants at the edge.
beachMaterial ∷ Int → Word8 → Float → Word8
beachMaterial dist origMat roll
    -- Close to water: pure sand
    | dist ≤ 4  = 55   -- sand
    -- Middle beach: mostly sand, occasional loamy sand
    | dist ≤ 6  = if roll < 0.85 then 55 else 54  -- sand / loamy sand
    -- Beach margin: transition depends on what's inland
    | otherwise = transitionMaterial origMat roll

-- | At the beach margin, blend sand with whatever material is inland.
--   Creates realistic transitions: sand→loamy_sand→loam, etc.
transitionMaterial ∷ Word8 → Float → Word8
transitionMaterial origMat roll = case origMat of
    -- Loam family → loamy sand
    56 → if roll < 0.6 then 54 else 53   -- loamy sand / sandy loam
    60 → if roll < 0.6 then 54 else 53   -- silt loam → loamy sand / sandy loam
    -- Clay family → sandy clay
    50 → if roll < 0.6 then 51 else 52   -- clay → sandy clay / sandy clay loam
    57 → if roll < 0.5 then 52 else 54   -- clay loam → sandy clay loam / loamy sand
    58 → if roll < 0.6 then 51 else 52   -- silty clay → sandy clay / sandy clay loam
    -- Already sandy → keep sandy
    53 → 53   -- sandy loam stays
    54 → 54   -- loamy sand stays
    55 → 55   -- sand stays
    -- Peat/organic → sandy loam (sand mixing into organics)
    62 → if roll < 0.5 then 53 else 54   -- peat → sandy loam / loamy sand
    64 → if roll < 0.5 then 53 else 54   -- muck → sandy loam / loamy sand
    -- Rock → loamy sand / sandy loam (weathered rock + sand)
    _  → if roll < 0.5 then 54 else 53   -- default loamy sand / sandy loam

-- | Wetland material near river mouths.
wetlandMaterial ∷ Int → Float → Word8
wetlandMaterial dist roll
    | dist ≤ 3 =
        if roll < 0.4 then 64       -- muck
        else if roll < 0.7 then 62  -- peat
        else 63                      -- mucky peat
    | dist ≤ 5 =
        if roll < 0.3 then 62       -- peat
        else if roll < 0.6 then 63  -- mucky peat
        else 64                      -- muck
    | otherwise =
        if roll < 0.4 then 62       -- peat
        else if roll < 0.7 then 51  -- sandy clay
        else 50                      -- clay

-- | Deltaic material: mix of fluvial sand and mud.
deltaMaterial ∷ Int → Float → Word8
deltaMaterial dist roll
    | dist ≤ 3 =
        if roll < 0.4 then 55       -- sand
        else if roll < 0.6 then 54  -- loamy sand
        else if roll < 0.8 then 64  -- muck
        else 61                      -- silt
    | dist ≤ 5 =
        if roll < 0.3 then 53       -- sandy loam
        else if roll < 0.5 then 52  -- sandy clay loam
        else if roll < 0.7 then 50  -- clay
        else 62                      -- peat
    | otherwise =
        if roll < 0.4 then 53       -- sandy loam
        else 54                      -- loamy sand

-----------------------------------------------------------
-- Coastal Erosion Pass
-----------------------------------------------------------

maxCoastalDist ∷ Int
maxCoastalDist = 8

applyCoastalErosion ∷ Word64 → Int → [TectonicPlate] → MaterialRegistry
                    → GeoTimeline → ChunkCoord
                    → (VU.Vector Int, VU.Vector MaterialId)
                    → (VU.Vector Int, VU.Vector MaterialId)
applyCoastalErosion seed worldSize plates registry timeline coord (elevVec, matVec) =
    -- No early exit: even inland chunks must run the BFS to check if
    -- their border zone overlaps with a coastal chunk. Without this,
    -- chunk boundaries between beach and inland terrain create gaps
    -- (black voids) because the inland chunk's strata don't extend
    -- down to the beach level.
    let borderSize = chunkSize + 2 * chunkBorder
        borderArea = borderSize * borderSize

        fromIndex idx =
            let (by, bx) = idx `divMod` borderSize
            in (bx - chunkBorder, by - chunkBorder)

        riverMouths = concatMap extractMouths (gtPeriods timeline)
        extractMouths period =
            [ (mx, my)
            | HydroEvent (RiverFeature rp) ← gpEvents period
            , let GeoCoord mx my = rpMouthRegion rp
            ]

        distField = buildDistField borderSize elevVec

        -- Step 1: Deposit sand / apply erosion per tile.
        -- For depositional coasts: build sand surface outward from
        -- the waterline. Sand fills up to sandProfile, covering soft
        -- terrain. Hard rock pokes through as outcrops.
        -- For erosional coasts: minor cliff-base erosion + gravel.
        (passElev, passMat) = runST $ do
            elevM ← VUM.new borderArea
            matM  ← VUM.new borderArea
            forM_ [0 .. borderArea - 1] $ \idx → do
                VUM.write elevM idx (elevVec VU.! idx)
                VUM.write matM  idx (matVec  VU.! idx)

            forM_ [0 .. borderArea - 1] $ \idx → do
                let dist = distField VU.! idx
                    (lx, ly) = fromIndex idx
                    -- Only modify tiles in the 16×16 interior.
                    -- Border tiles are shared with adjacent chunks which
                    -- compute different BFS distances (different ocean
                    -- visibility). Modifying border tiles causes elevation
                    -- mismatches at chunk boundaries → black voids and
                    -- ocean level discontinuities.
                    isInterior = lx ≥ 0 ∧ lx < chunkSize
                               ∧ ly ≥ 0 ∧ ly < chunkSize
                when (dist > 0 ∧ dist ≤ maxCoastalDist ∧ isInterior) $ do
                    let
                        (gx, gy) = chunkToGlobal coord lx ly
                        (gx', gy') = wrapGlobalU worldSize gx gy
                        elev = elevVec VU.! idx
                        mat  = matVec  VU.! idx
                        hardness = mpHardness (getMaterialProps registry mat)

                        ((plateA, distA'), (plateB, distB)) =
                            twoNearestPlates seed worldSize plates gx' gy'
                        boundary = classifyBoundary worldSize plateA plateB
                        boundaryDist = (distB - distA') / 2.0
                        maxBDist = fromIntegral worldSize * 4.0 ∷ Float
                        boundaryFade = min 1.0 (abs boundaryDist / maxBDist)
                        nearRiver = isNearRiverMouth worldSize riverMouths gx' gy'
                        coastType = classifyCoast boundary
                            (plateIsLand plateA) (plateIsLand plateB)
                            hardness 0.0 boundaryFade nearRiver
                            -- slope=0 for classification; we don't need it
                            -- since wetland is gated by nearRiver now

                        localHash = coastHash seed gx' gy'
                        roll = fromIntegral (localHash .&. 0xFF) / 255.0 ∷ Float

                        sandLevel = sandProfile dist

                    if isDepositional coastType
                    then do
                        -- === DEPOSITIONAL: flatten + deposit ===
                        -- Hard rock above the sand profile creates outcrops.
                        -- Everything else gets flattened to the sand profile
                        -- (erosion brings high terrain down, deposition fills
                        -- low terrain up). The result is a smooth surface at
                        -- sandLevel with occasional rock outcrops.
                        let isOutcrop = hardness ≥ outcroppHardness
                                      ∧ elev > sandLevel
                            chooseMat = case coastType of
                                DepositionalWetland → wetlandMaterial dist roll
                                DeltaicCoast → deltaMaterial dist roll
                                _ → beachMaterial dist (unMaterialId mat) roll
                        if isOutcrop
                        then pure ()
                        else do
                            -- Only LOWER terrain or keep same. Never raise
                            -- above current elevation — raising creates
                            -- strata gaps and ocean fill artifacts.
                            let newElev = min elev sandLevel
                            VUM.write elevM idx newElev
                            VUM.write matM idx (MaterialId chooseMat)
                    else do
                        -- === EROSIONAL: minor cliff-base erosion ===
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

        -- Step 2: Smooth beach elevations. 3 passes of neighbor
        -- averaging on depositional coastal tiles. This eliminates
        -- noise and creates the smooth, even surfaces of real beaches.
        -- Only adjusts toward the average (never above current).
        smoothedElev = smoothCoast 3 borderSize distField passElev

    in (smoothedElev, passMat)

-----------------------------------------------------------
-- Distance Field
-----------------------------------------------------------

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

-----------------------------------------------------------
-- Beach Smoothing
-----------------------------------------------------------

-- | Iterative smoothing on coastal tiles. Each pass averages
--   a tile's elevation with its 4 neighbors, only lowering
--   (never raising above current). Multiple passes propagate
--   smoothness outward from the flat beach core.
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
                    let
                        readN nx ny
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

-----------------------------------------------------------
-- Hash Utility
-----------------------------------------------------------

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

