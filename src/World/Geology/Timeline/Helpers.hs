{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Timeline.Helpers
    ( -- * Period construction
      mkGeoPeriod
      -- * Feature helpers
    , featureCenter
    , isHydroFeature
    , isRiverFeature
    , isGlacierFeature
    , isActiveRiver
    , isLakeFeature
    , isSuperVolcano
    , isSourceNew
    , getRiverParamsFromPf
      -- * Erosion
    , erosionFromGeoState
    , regionalErosionMap
      -- * Glacier evolution wrapper
    , evolveGlacierCapped
      -- * Elev grid lookup
    , elevFromGrid
      -- * Lake reconciliation
    , reconcileLakes
    ) where
import UPrelude
import Data.List (foldl')
import Data.Ord (comparing)
import Data.Word (Word64)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Unboxed as VU
import qualified Data.HashMap.Strict as HM
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Types
import World.Plate (wrapGlobalU)
import World.Geology.Types
import World.Geology.Hash
import World.Weather.Types
import World.Hydrology.Glacier (evolveGlacier)
import World.Hydrology.Types (HydroFeature(..), GlacierParams(..)
                             , RiverParams(..), LakeParams(..))
import World.Hydrology.Simulation (ElevGrid(..))

-----------------------------------------------------------
-- Elev grid lookup
-----------------------------------------------------------

{-# INLINE elevFromGrid #-}
elevFromGrid ∷ ElevGrid → Int → Int → Int → Int
elevFromGrid grid worldSize gx gy =
    let spacing = egSpacing grid
        gridW = egGridW grid
        halfGrid = gridW `div` 2
        -- Convert (gx, gy) to (u, v) space
        u = gx - gy
        v = gx + gy
        -- Invert the grid formula: u = (ix - halfGrid) * spacing
        -- => ix = u / spacing + halfGrid
        iu = ((u + spacing `div` 2) `div` spacing) + halfGrid
        iv = ((v + spacing `div` 2) `div` spacing) + halfGrid
        -- Wrap iu (u-axis wraps), clamp iv (v-axis bounded)
        iu' = ((iu `mod` gridW) + gridW) `mod` gridW
        iv' = max 0 (min (gridW - 1) iv)
        idx = iv' * gridW + iu'
    in if idx ≥ 0 ∧ idx < VU.length (egElev grid)
       then egElev grid VU.! idx
       else seaLevel

-----------------------------------------------------------
-- Feature classification helpers
-----------------------------------------------------------

isHydroFeature ∷ FeatureShape → Bool
isHydroFeature (HydroShape _) = True
isHydroFeature _              = False

isRiverFeature ∷ FeatureShape → Bool
isRiverFeature (HydroShape (RiverFeature _)) = True
isRiverFeature _                             = False

isGlacierFeature ∷ FeatureShape → Bool
isGlacierFeature (HydroShape (GlacierFeature _)) = True
isGlacierFeature _                               = False

isActiveRiver ∷ FeatureShape → Bool
isActiveRiver (HydroShape (RiverFeature _)) = True
isActiveRiver _                             = False

isLakeFeature ∷ FeatureShape → Bool
isLakeFeature (HydroShape (LakeFeature _)) = True
isLakeFeature _                            = False

isSuperVolcano ∷ PersistentFeature → Bool
isSuperVolcano pf = case pfFeature pf of
    (VolcanicShape (SuperVolcano _)) → True
    _                                → False

getRiverParamsFromPf ∷ PersistentFeature → RiverParams
getRiverParamsFromPf pf = case pfFeature pf of
    HydroShape (RiverFeature r) → r
    _ → error "getRiverParamsFromPf: not a river"

isSourceNew ∷ Int → [PersistentFeature] → (Int, Int, Int, Float) → Bool
isSourceNew worldSize existingRivers (sx, sy, _, _) =
    let threshold = 60  -- was 150 — allow rivers closer together
    in not $ any (\pf →
        let river = getRiverParamsFromPf pf
            GeoCoord ex ey = rpSourceRegion river
            (dxi, dyi) = wrappedDeltaUV worldSize sx sy ex ey
            dx = abs dxi
            dy = abs dyi
        in dx < threshold ∧ dy < threshold
        ) existingRivers

-----------------------------------------------------------
-- Erosion
-----------------------------------------------------------

erosionFromGeoState ∷ GeoState → ClimateState → Word64 → Int → Bool → ErosionParams
erosionFromGeoState gs climate seed ageIdx isLastAge =
    let co2 = gsCO2 gs
        chemical = min 1.0 (0.2 + (co2 - 1.0) * 0.3)

        globalTemp = csGlobalTemp climate
        regions = cgRegions (csClimate climate)

        avgPrecip = if HM.null regions then 0.5
                    else let total = HM.foldl' (\acc rc →
                               let SeasonalClimate s w = rcPrecipitation rc
                               in acc + (s + w) / 2.0
                               ) 0.0 regions
                         in total / fromIntegral (HM.size regions)

        avgHumidity = if HM.null regions then 0.5
                      else let total = HM.foldl' (\acc rc → acc + rcHumidity rc) 0.0 regions
                           in total / fromIntegral (HM.size regions)

        avgSnowFrac = if HM.null regions then 0.0
                      else let total = HM.foldl' (\acc rc → acc + rcPrecipType rc) 0.0 regions
                           in total / fromIntegral (HM.size regions)

        hydraulicScale = 0.3 + 0.7 * min 1.0 (avgPrecip / 0.5)

        thermalScale =
            let t = globalTemp
            in if t < -10.0 then 0.15
               else if t < 0.0 then 0.3 + 0.7 * ((t + 10.0) / 10.0)
               else if t < 10.0 then 1.0
               else if t < 25.0 then 1.0 - 0.6 * ((t - 10.0) / 15.0)
               else 0.15

        windScale = 0.1 + 0.9 * max 0.0 (1.0 - avgPrecip * 2.0)

    in ErosionParams
        { epIntensity     = 0.5
        , epHydraulic     = 0.5 * hydraulicScale
        , epThermal       = 0.3 * thermalScale
        , epWind          = 0.2 * windScale
        , epChemical      = chemical
        , epSeed          = seed + fromIntegral ageIdx * 7
        -- NEW: baked climate for erosionSediment
        , epTemperature   = globalTemp
        , epPrecipitation = avgPrecip
        , epHumidity      = avgHumidity
        , epSnowFraction  = avgSnowFrac
        , epIsLastAge     = isLastAge
        }

-- | Build per-region erosion parameters from the climate grid.
--   Each climate region gets its own ErosionParams derived from
--   its local temperature, precipitation, humidity, and snow.
regionalErosionMap ∷ GeoState → ClimateState → Word64 → Int → Bool
                   → HM.HashMap ClimateCoord ErosionParams
regionalErosionMap gs climate seed ageIdx isLastAge =
    let co2 = gsCO2 gs
        chemical = min 1.0 (0.2 + (co2 - 1.0) * 0.3)
        regions = cgRegions (csClimate climate)
    in HM.map (buildRegionErosion co2 chemical) regions
  where
    buildRegionErosion co2 chemical rc =
        let SeasonalClimate summerT winterT = rcAirTemp rc
            regionTemp = (summerT + winterT) / 2.0
            SeasonalClimate summerP winterP = rcPrecipitation rc
            regionPrecip = (summerP + winterP) / 2.0
            regionHumidity = rcHumidity rc
            regionSnow = rcPrecipType rc

            hydraulicScale = 0.3 + 0.7 * min 1.0 (regionPrecip / 0.5)
            thermalScale =
                let t = regionTemp
                in if t < -10.0 then 0.15
                   else if t < 0.0 then 0.3 + 0.7 * ((t + 10.0) / 10.0)
                   else if t < 10.0 then 1.0
                   else if t < 25.0 then 1.0 - 0.6 * ((t - 10.0) / 15.0)
                   else 0.15
            windScale = 0.1 + 0.9 * max 0.0 (1.0 - regionPrecip * 2.0)
            windBoost = 0.5 + 0.5 * rcWindSpeed rc

        in ErosionParams
            { epIntensity     = 0.5
            , epHydraulic     = 0.5 * hydraulicScale
            , epThermal       = 0.3 * thermalScale
            , epWind          = 0.2 * windScale * windBoost
            , epChemical      = chemical
            , epSeed          = seed + fromIntegral ageIdx * 7
            , epTemperature   = regionTemp
            , epPrecipitation = regionPrecip
            , epHumidity      = regionHumidity
            , epSnowFraction  = regionSnow
            , epIsLastAge     = isLastAge
            }

-----------------------------------------------------------
-- Feature center
-----------------------------------------------------------

featureCenter ∷ FeatureShape → (Int, Int)
featureCenter (VolcanicShape (ShieldVolcano p))
    = let GeoCoord x y = shCenter p in (x, y)
featureCenter (VolcanicShape (CinderCone p))
    = let GeoCoord x y = ccCenter p in (x, y)
featureCenter (VolcanicShape (LavaDome p))
    = let GeoCoord x y = ldCenter p in (x, y)
featureCenter (VolcanicShape (Caldera p))
    = let GeoCoord x y = caCenter p in (x, y)
featureCenter (VolcanicShape (FissureVolcano p))
    = let GeoCoord sx sy = fpStart p
          GeoCoord ex ey = fpEnd p
      in ((sx + ex) `div` 2, (sy + ey) `div` 2)
featureCenter (VolcanicShape (LavaTube p))
    = let GeoCoord sx sy = ltStart p
          GeoCoord ex ey = ltEnd p
      in ((sx + ex) `div` 2, (sy + ey) `div` 2)
featureCenter (VolcanicShape (SuperVolcano p))
    = let GeoCoord x y = svCenter p in (x, y)
featureCenter (VolcanicShape (HydrothermalVent p))
    = let GeoCoord x y = htCenter p in (x, y)
featureCenter (HydroShape (RiverFeature r))
    = let GeoCoord x y = rpSourceRegion r in (x, y)
featureCenter (HydroShape (GlacierFeature g))
    = let GeoCoord x y = glCenter g in (x, y)
featureCenter (HydroShape (LakeFeature l))
    = let GeoCoord x y = lkCenter l in (x, y)

-----------------------------------------------------------
-- Period construction
-----------------------------------------------------------

mkGeoPeriod ∷ Int → Text → GeoScale → Int → Float → [GeoEvent]
  → ErosionParams → HM.HashMap ClimateCoord ErosionParams → GeoPeriod
mkGeoPeriod worldSize name scale duration date events erosion regErosion =
    let tagged = tagEventsWithBBox worldSize events
        exploded = concatMap (\(evt, _bb) →
            let evts = explodeRiverEvent evt
            in map (\e → (e, eventBBox e worldSize)) evts
            ) tagged
        explodedVec = V.modify (VA.sortBy (comparing (bbMinY . snd)))
                               (V.fromList exploded)
        periodBB = if V.null explodedVec
                   then EventBBox maxBound maxBound minBound minBound
                   else V.foldl' (\(EventBBox ax ay bx by) (_, EventBBox cx cy dx dy) →
                            EventBBox (min ax cx) (min ay cy) (max bx dx) (max by dy)
                        ) (snd (V.head explodedVec)) (V.tail explodedVec)
    in GeoPeriod
        { gpName            = name
        , gpScale           = scale
        , gpDuration        = duration
        , gpDate            = date
        , gpEvents          = events
        , gpErosion         = erosion
        , gpRegionalErosion = regErosion
        , gpTaggedEvents    = tagged
        , gpExplodedEvents  = explodedVec
        , gpPeriodBBox      = periodBB
        }

-----------------------------------------------------------
-- Glacier evolution wrapper
-----------------------------------------------------------

evolveGlacierCapped ∷ Word64 → Bool → Int → GeoState
                    → ([GeoEvent], TimelineBuildState)
                    → PersistentFeature
                    → ([GeoEvent], TimelineBuildState)
evolveGlacierCapped seed canBranch periodIdx gs (events, tbs) pf =
    if canBranch
    then evolveGlacier seed periodIdx gs (events, tbs) pf
    else let fid = pfId pf
             GeoFeatureId fidInt = fid
             h1 = hashGeo seed fidInt 900
             roll = hashToFloatGeo h1
             temp = gsCO2 gs
         in if (temp < 0.8 ∧ roll ≥ 0.50 ∧ roll < 0.65)
             ∨ (temp ≥ 0.8 ∧ temp ≤ 1.2 ∧ roll ≥ 0.20 ∧ roll < 0.30)
            then (events, tbs)
            else evolveGlacier seed periodIdx gs (events, tbs) pf

-----------------------------------------------------------
-- Lake reconciliation
-----------------------------------------------------------

reconcileLakes ∷ Word64 → Int → Int → Int
              → [PersistentFeature] → [LakeParams]
              → TimelineBuildState
              → ([PersistentFeature], [GeoEvent], TimelineBuildState)
reconcileLakes _seed _ageIdx periodIdx worldSize existingLakes simLakes tbs =
    let lakeMatchRadius = 60

        isNearExisting lk =
            let GeoCoord lx ly = lkCenter lk
            in any (\epf → case pfFeature epf of
                HydroShape (LakeFeature elk) →
                    let GeoCoord ex ey = lkCenter elk
                        (dxi, dyi) = wrappedDeltaUV worldSize lx ly ex ey
                    in abs dxi < lakeMatchRadius
                     ∧ abs dyi < lakeMatchRadius
                _ → False
                ) existingLakes

        newLakes = filter (not . isNearExisting) simLakes

        (pfs, evts, tbs') = foldl' (\(ps, es, st) lake →
            let (fid, st') = allocFeatureId st
                pf = PersistentFeature
                    { pfId               = fid
                    , pfFeature          = HydroShape $ LakeFeature lake
                    , pfActivity         = FActive
                    , pfFormationPeriod   = periodIdx
                    , pfLastActivePeriod  = periodIdx
                    , pfEruptionCount     = 0
                    , pfParentId          = Nothing
                    }
                st'' = registerFeature pf st'
            in (pf : ps, HydroEvent (LakeFeature lake) : es, st'')
            ) ([], [], tbs) newLakes

    in (pfs, evts, tbs')
