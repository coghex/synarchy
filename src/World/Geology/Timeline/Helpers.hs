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
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Types
import World.Plate (wrapGlobalU)
import World.Geology.Types
import World.Geology.Hash
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
        (gx', gy') = wrapGlobalU worldSize gx gy
        ix = (gx' `div` spacing) + halfGrid
        iy = (gy' `div` spacing) + halfGrid
        ix' = max 0 (min (gridW - 1) ix)
        iy' = max 0 (min (gridW - 1) iy)
        idx = iy' * gridW + ix'
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
    let threshold = 150  -- tiles
    in not $ any (\pf →
        let river = getRiverParamsFromPf pf
            GeoCoord ex ey = rpSourceRegion river
            dx = abs (wrappedDeltaXGeo worldSize sx ex)
            dy = abs (sy - ey)
        in dx < threshold ∧ dy < threshold
        ) existingRivers

-----------------------------------------------------------
-- Erosion
-----------------------------------------------------------

erosionFromGeoState ∷ GeoState → Word64 → Int → ErosionParams
erosionFromGeoState gs seed ageIdx =
    let co2 = gsCO2 gs
        chemical = min 1.0 (0.2 + (co2 - 1.0) * 0.3)
    in ErosionParams
        { epIntensity = 0.5
        , epHydraulic = 0.5
        , epThermal   = 0.3
        , epWind      = 0.2
        , epChemical  = chemical
        , epSeed      = seed + fromIntegral ageIdx * 7
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

mkGeoPeriod ∷ Int → Text → GeoScale → Int → Float → [GeoEvent] → ErosionParams → GeoPeriod
mkGeoPeriod worldSize name scale duration date events erosion =
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

reconcileLakes ∷ Word64 → Int → Int
              → [PersistentFeature] → [LakeParams]
              → TimelineBuildState
              → ([PersistentFeature], [GeoEvent], TimelineBuildState)
reconcileLakes _seed _ageIdx periodIdx existingLakes simLakes tbs =
    let lakeMatchRadius = 60

        isNearExisting lk =
            let GeoCoord lx ly = lkCenter lk
            in any (\epf → case pfFeature epf of
                HydroShape (LakeFeature elk) →
                    let GeoCoord ex ey = lkCenter elk
                    in abs (lx - ex) < lakeMatchRadius
                     ∧ abs (ly - ey) < lakeMatchRadius
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
