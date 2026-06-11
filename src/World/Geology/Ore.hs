{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Flow-routed sedimentary ore deposition.
--
--   Runs once per geological Age (from 'World.Geology.Timeline.buildAge'),
--   after that age's hydrology simulation. Every volcanic feature
--   matching a 'DepositSpec' sheds sediment flux proportional to its
--   size, activity and the age's duration; the flux is routed down the
--   age's 'frFlowDir' drainage field (so it follows river valleys
--   downhill) and drops out of suspension where the coarse terrain
--   flattens, accumulating flat sheets of varying thickness — fans at
--   the foot of volcano flanks, ribbons widening into basins.
--
--   Each source emits at most one 'OreSheetEvent' per age with a tight
--   bbox. Sheets are TRUE deposition (they raise terrain and the strata
--   replay records the rise as ore material), so later ages bury them:
--   deep ore is old ore.
--
--   Provenance is the design rule: no flux without a volcanic source,
--   never a random scatter. Abundance levers ('OreLevers') are purely
--   mechanistic — they scale source flux, and ore-poor worlds on
--   low-volcanism seeds are accepted by design.
--
--   Deferred extensions (deposit-spec rows, not new machinery): gold /
--   tin river placers, glacial-outwash transport, bog iron, crater
--   ejecta sources.
module World.Geology.Ore
    ( buildOreSheets
    , buildOreDepositTable
    , oreMaterialIds
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Geology.Hash (hashGeo, hashToFloatGeo)
import World.Geology.Ore.Types
import World.Geology.Timeline.Types
import World.Hydrology.Simulation (ElevGrid(..), FlowResult(..))
import World.Material (matIronOre, matCopperOre, unMaterialId)
import World.Plate (wrapGlobalU)

-- * Deposit specs

-- | How one ore material is sourced and where it settles. Adding a
--   resource means adding a row here (plus its lever in 'OreLevers').
data DepositSpec = DepositSpec
    { dsMat      ∷ !Word8
      -- ^ Ore material deposited
    , dsLever    ∷ OreLevers → Float
      -- ^ Per-material abundance lever
    , dsFlatMax  ∷ !Int
      -- ^ Coarse-grid slope (z per cell) at or below which flux
      --   settles; steeper cells carry the flux onward
    , dsThickCap ∷ !Int
      -- ^ Max sheet thickness (z) per age
    , dsSource   ∷ VolcanicFeature → Maybe Float
      -- ^ Size factor for a source feature (Nothing = not a source
      --   for this material). Roughly radius·height normalised so a
      --   typical source lands near 1.0.
    }

-- | Iron: shed by mafic / extrusive edifices (shields, cinder cones,
--   fissure ridges, lava tubes) — basalt weathers to iron-rich
--   sediment that settles in basins downslope.
ironSpec ∷ DepositSpec
ironSpec = DepositSpec
    { dsMat      = unMaterialId matIronOre
    , dsLever    = olIron
    , dsFlatMax  = 2
    , dsThickCap = 5
    , dsSource   = ironSource
    }
  where
    ironSource (ShieldVolcano p) =
        Just (fromIntegral (shBaseRadius p * shPeakHeight p) / 6000.0)
    ironSource (CinderCone p) =
        Just (fromIntegral (ccBaseRadius p * ccPeakHeight p) / 1500.0)
    ironSource (FissureVolcano p) =
        Just (featureLineFactor (fpStart p) (fpEnd p) (fpRidgeHeight p) 3000.0)
    ironSource (LavaTube p) =
        Just (featureLineFactor (ltStart p) (ltEnd p) (ltRidgeHeight p) 3000.0)
    ironSource _ = Nothing

-- | Copper: shed by felsic / intrusive features (domes, calderas,
--   supervolcanoes). Cinder cones contribute a reduced share as the
--   stand-in for porphyry-style arc volcanism so copper reliably
--   occurs on land (hydrothermal vents are ocean-floor only and their
--   flux is dropped by the land check at the walk's first step).
copperSpec ∷ DepositSpec
copperSpec = DepositSpec
    { dsMat      = unMaterialId matCopperOre
    , dsLever    = olCopper
    , dsFlatMax  = 2
    , dsThickCap = 4
    , dsSource   = copperSource
    }
  where
    -- Felsic features are rare (often one caldera / supervolcano per
    -- world), so their factors run hot relative to iron's, and cinder
    -- cones carry full weight — they're the only COMMON land source
    -- copper has. Calibrated to land copper near 1/4 of iron.
    copperSource (LavaDome p) =
        Just (fromIntegral (ldBaseRadius p * ldHeight p) / 400.0)
    copperSource (Caldera p) =
        Just (fromIntegral (caOuterRadius p * caRimHeight p) / 800.0)
    copperSource (SuperVolcano p) =
        Just (fromIntegral (svCalderaRadius p * svRimHeight p) / 400.0)
    copperSource (HydrothermalVent p) =
        Just (fromIntegral (htRadius p * htChimneyHeight p) / 400.0)
    copperSource (CinderCone p) =
        Just (fromIntegral (ccBaseRadius p * ccPeakHeight p) / 1500.0)
    copperSource _ = Nothing

depositSpecs ∷ [DepositSpec]
depositSpecs = [ironSpec, copperSpec]

-- | Ore material ids the deposit system places (used by the dump
--   layer and report tooling to recognise ore cells in strata).
oreMaterialIds ∷ [Word8]
oreMaterialIds = map dsMat depositSpecs

-- | Size factor for line features (fissures, tubes): length·height.
featureLineFactor ∷ GeoCoord → GeoCoord → Int → Float → Float
featureLineFactor (GeoCoord sx sy) (GeoCoord ex ey) height norm =
    let dx = fromIntegral (ex - sx) ∷ Float
        dy = fromIntegral (ey - sy) ∷ Float
        len = sqrt (dx * dx + dy * dy)
    in len * fromIntegral height / norm

-- * Tunables

-- | Flux a size-1.0 source sheds per million years, in z·cells of
--   eventual sheet volume. Calibrated with tools/ore_report.py
--   (4.0 gave 0.39% of land as iron with crumb-sized deposits; the
--   gameplay target is iron in abundance with large sheets).
fluxPerMy ∷ Float
fluxPerMy = 12.0

-- | Sheets smaller than this many cells after quantisation are
--   dropped — single-cell crumbs read as noise, not deposits.
minSheetCells ∷ Int
minSheetCells = 3

-- | Flux below which a walk stops (the tail is lost to suspension).
fluxEpsilon ∷ Float
fluxEpsilon = 0.5

-- | Safety cap on walk length; 'frFlowDir' chains are guaranteed to
--   reach the ocean, so this only guards against pathological grids.
maxWalkSteps ∷ Int
maxWalkSteps = 4096

-- | Deposition is episodic: each (source, age) pair only sheds a
--   sheet when its hash roll passes, with flux scaled by 1/chance so
--   total ore volume is unchanged. Keeps the timeline's event count
--   (and the per-column strata replay cost) ~3× lower than
--   every-feature-every-age, and reads geologically as pulsed
--   sedimentation — fewer, thicker, more distinct sheets.
episodeChance ∷ Float
episodeChance = 0.35

-- | How much of the eroding edifice still sheds sediment in each
--   activity state. Dormant and extinct volcanoes keep eroding — the
--   edifice is there regardless — just less energetically.
activityFactor ∷ FeatureActivity → Float
activityFactor FActive    = 1.0
activityFactor FDormant   = 0.7
activityFactor FExtinct   = 0.5
activityFactor FCollapsed = 0.3

-- | Fraction of carried flux that settles on a cell of the given
--   coarse slope (z drop to the downstream neighbour). Flat cells
--   capture strongly; anything steeper than the spec's flatMax
--   carries everything onward.
depositFrac ∷ Int → Int → Float
depositFrac flatMax slope
    | slope > flatMax = 0.0
    | otherwise =
        0.12 + 0.28 * (1.0 - fromIntegral slope / fromIntegral (flatMax + 1))

-- * Per-age deposition pass

-- | Emit this age's ore-sheet events. One event per (source feature ×
--   matching spec) whose episode roll passes and that actually
--   deposited at least one cell.
buildOreSheets ∷ Word64 → Int → OreLevers → Int → Float
               → [PersistentFeature] → ElevGrid → FlowResult → [GeoEvent]
buildOreSheets seed periodIdx levers worldSize duration features grid fr =
    [ OreSheetEvent sheet
    | spec ← depositSpecs
    , pf ← features
    , VolcanicShape vf ← [pfFeature pf]
    , Just sizeFactor ← [dsSource spec vf]
    , let GeoFeatureId fidInt = pfId pf
          roll = hashToFloatGeo
              (hashGeo seed (fidInt * 1021 + periodIdx) 7777)
    , roll < episodeChance
    , Just (srcIx, srcIy) ← [sourceCell worldSize grid (volcanicCenter vf)]
    , let flux0 = sizeFactor
                * activityFactor (pfActivity pf)
                * duration * (fluxPerMy / episodeChance)
                * olGlobal levers * dsLever spec levers
    , flux0 > fluxEpsilon
    , let acc = runWalk grid fr (dsFlatMax spec) (dsThickCap spec)
                        (toIdx grid srcIx srcIy) flux0
    , Just sheet ← [sheetFromAccum grid spec srcIx acc]
    ]

volcanicCenter ∷ VolcanicFeature → GeoCoord
volcanicCenter (ShieldVolcano p)    = shCenter p
volcanicCenter (CinderCone p)       = ccCenter p
volcanicCenter (LavaDome p)         = ldCenter p
volcanicCenter (Caldera p)          = caCenter p
volcanicCenter (FissureVolcano p)   = midpoint (fpStart p) (fpEnd p)
volcanicCenter (LavaTube p)         = midpoint (ltStart p) (ltEnd p)
volcanicCenter (SuperVolcano p)     = svCenter p
volcanicCenter (HydrothermalVent p) = htCenter p

midpoint ∷ GeoCoord → GeoCoord → GeoCoord
midpoint (GeoCoord ax ay) (GeoCoord bx by) =
    GeoCoord ((ax + bx) `div` 2) ((ay + by) `div` 2)

-- | Map a global tile coordinate to its nearest coarse-grid cell.
--   Returns Nothing if the v-axis index falls off the (non-wrapping)
--   grid edge.
sourceCell ∷ Int → ElevGrid → GeoCoord → Maybe (Int, Int)
sourceCell worldSize grid (GeoCoord gx0 gy0) =
    let (gx, gy) = wrapGlobalU worldSize gx0 gy0
        gridW = egGridW grid
        halfGrid = gridW `div` 2
        s = fromIntegral (egSpacing grid) ∷ Float
        ix0 = round (fromIntegral (gx - gy) / s) + halfGrid ∷ Int
        iy  = round (fromIntegral (gx + gy) / s) + halfGrid ∷ Int
        ix = ((ix0 `mod` gridW) + gridW) `mod` gridW
    in if iy < 0 ∨ iy ≥ gridW
       then Nothing
       else Just (ix, iy)

{-# INLINE toIdx #-}
toIdx ∷ ElevGrid → Int → Int → Int
toIdx grid ix iy = iy * egGridW grid + ix

-- | Walk the drainage chain from the source cell, settling flux on
--   flat cells into a per-cell thickness accumulator. Deposition is
--   CAPACITY-CLAMPED: a cell holds at most the spec's thickness cap
--   per sheet, and surplus stays in the flux and flows on downstream —
--   so saturated basins fill outward into wide flat sheets instead of
--   discarding the excess at quantisation. On very flat ground
--   (slope ≤ 1) the settled share also spreads over the 3×3
--   neighbourhood so fans widen instead of marching as 1-cell ribbons.
runWalk ∷ ElevGrid → FlowResult → Int → Int → Int → Float
        → HM.HashMap Int Float
runWalk grid fr flatMax thickCap start flux0 =
    go HM.empty start flux0 (0 ∷ Int)
  where
    gridW  = egGridW grid
    filled = frFilledElev fr
    dirs   = frFlowDir fr
    landV  = egLand grid
    cap    = fromIntegral thickCap ∷ Float

    go acc cur flux steps
        | flux < fluxEpsilon ∨ steps ≥ maxWalkSteps = acc
        | not (landV VU.! cur) = acc
        | otherwise =
            let next = dirs VU.! cur
            in if next < 0
               then acc
               else
               let slope = max 0 (filled VU.! cur - filled VU.! next)
                   frac  = depositFrac flatMax slope
                   want  = flux * frac
                   (acc', deposited) =
                       if want > 0.0
                       then spread acc cur want slope
                       else (acc, 0.0)
               in go acc' next (flux - deposited) (steps + 1)

    spread acc cur want slope
        | slope > 1 = depositInto acc cur want
        | otherwise =
            let ix = cur `mod` gridW
                iy = cur `div` gridW
                (acc1, d1) = depositInto acc cur (want * 0.6)
                neigh = [ (-1, 0, 0.075), (1, 0, 0.075)
                        , (0, -1, 0.075), (0, 1, 0.075)
                        , (-1, -1, 0.025), (1, -1, 0.025)
                        , (-1, 1, 0.025), (1, 1, 0.025) ]
            in foldl' (\(a, dTot) (dx, dy, share) →
                   let nx = ((ix + dx) `mod` gridW + gridW) `mod` gridW
                       ny = iy + dy
                   in if ny < 0 ∨ ny ≥ gridW
                      then (a, dTot)
                      else let nIdx = ny * gridW + nx
                           in if landV VU.! nIdx
                              then let (a', d') =
                                          depositInto a nIdx (want * share)
                                   in (a', dTot + d')
                              else (a, dTot)  -- share lost into water
                   ) (acc1, d1) neigh

    -- Clamp by the cell's remaining capacity; report what stuck.
    depositInto acc idx amt =
        let have = HM.lookupDefault 0.0 idx acc
            d = min amt (max 0.0 (cap - have))
        in if d ≤ 0.0
           then (acc, 0.0)
           else (HM.insertWith (+) idx d acc, d)

-- | Quantise an accumulator into a windowed thickness raster and wrap
--   it in 'OreSheetParams'. Window indices are relativised around the
--   source column so seam-straddling fans stay contiguous (the origin
--   may go negative / past gridW; sampling wraps). A one-cell zero
--   margin on every side gives the bilinear sampler its edge taper.
sheetFromAccum ∷ ElevGrid → DepositSpec → Int
               → HM.HashMap Int Float → Maybe OreSheetParams
sheetFromAccum grid spec ixRef acc =
    let gridW = egGridW grid
        halfG = gridW `div` 2
        cells = [ (dx, iy, t)
                | (idx, amt) ← HM.toList acc
                , let t = min (dsThickCap spec) (round amt)
                , t ≥ 1
                , let ix = idx `mod` gridW
                      iy = idx `div` gridW
                      dx = ((ix - ixRef + halfG) `mod` gridW + gridW)
                               `mod` gridW - halfG
                ]
    in if length cells < minSheetCells
       then Nothing
       else
            let minDx = minimum [ dx | (dx, _, _) ← cells ]
                maxDx = maximum [ dx | (dx, _, _) ← cells ]
                minIy = minimum [ iy | (_, iy, _) ← cells ]
                maxIy = maximum [ iy | (_, iy, _) ← cells ]
                w = maxDx - minDx + 3
                h = maxIy - minIy + 3
                thickMap = HM.fromList
                    [ ((dx - minDx + 1, iy - minIy + 1), t)
                    | (dx, iy, t) ← cells ]
                bytes = BS.pack
                    [ fromIntegral
                        (HM.lookupDefault (0 ∷ Int) (cx, cy) thickMap)
                    | cy ← [0 .. h - 1], cx ← [0 .. w - 1] ]
            in Just OreSheetParams
                { osMat     = dsMat spec
                , osGridW   = gridW
                , osSpacing = egSpacing grid
                , osIX0     = ixRef + minDx - 1
                , osIY0     = minIy - 1
                , osW       = w
                , osH       = h
                , osThick   = bytes
                }

-- * Global deposit table

-- | Aggregate every sheet in the final timeline into a per-chunk
--   (material → volume) summary for the zoom-map info panel. Each
--   coarse cell covers ~spacing²/2 tiles (the (u,v)→(x,y) map halves
--   areas), attributed to the chunk containing the cell centre.
buildOreDepositTable ∷ Int → [GeoPeriod] → WorldOreDeposits
buildOreDepositTable worldSize periods =
    let cellEntries =
            [ (ChunkCoord (gx `div` chunkSize) (gy `div` chunkSize)
              , HM.singleton (osMat os) vol)
            | p ← periods
            , OreSheetEvent os ← gpEvents p
            , let gridW = osGridW os
                  halfG = gridW `div` 2
                  s = osSpacing os
                  cellTiles = max 1 ((s * s) `div` 2)
            , cy ← [0 .. osH os - 1]
            , cx ← [0 .. osW os - 1]
            , let t = fromIntegral (BS.index (osThick os) (cy * osW os + cx))
            , t > (0 ∷ Int)
            , let u = (osIX0 os + cx - halfG) * s
                  v = (osIY0 os + cy - halfG) * s
                  (gx, gy) = wrapGlobalU worldSize
                                 ((u + v) `div` 2) ((v - u) `div` 2)
                  vol = t * cellTiles
            ]
        byChunk = HM.fromListWith (HM.unionWith (+)) cellEntries
    in WorldOreDeposits (HM.map HM.toList byChunk)
