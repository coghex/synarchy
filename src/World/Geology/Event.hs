{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Event
    ( applyGeoEvent
    , applyEvolution
    ) where

import UPrelude
import World.Base (GeoCoord(..))
import World.Types
import World.Material
import World.Geology.Types
import World.Geology.Ore.Types (OreSheetParams(..), oreSheetThicknessAt)
import World.Geology.Crater (applyCrater)
import World.Geology.Volcano (applyVolcanicFeature, perturbDist)
import World.Geology.Hash (smoothstepGeo, wrappedDeltaUV)
import World.Hydrology.Types (HydroFeature(..))
import World.Hydrology.Event (applyHydroEvolution, applyHydroFeature)
import World.Hydrology.River (applyRiverCarve)
import World.Hydrology.River.Carving (carveFromSegment, computeDeltaDeposit')
import World.Hydrology.Glacier (applyGlacierCarve)

-- * Event Application

applyGeoEvent ∷ GeoEvent → Int → Int → Int → Int → Float → GeoModification
applyGeoEvent (CraterEvent params)  worldSize gx gy baseElev hardness =
    applyCrater params worldSize gx gy baseElev hardness
applyGeoEvent (VolcanicEvent (VolcanicShape feature)) worldSize gx gy baseElev hardness =
    applyVolcanicFeature feature worldSize gx gy baseElev hardness
applyGeoEvent (VolcanicEvent _) _ _ _ _ _ =
    noModification
applyGeoEvent (VolcanicModify _fid evolution) worldSize gx gy baseElev hardness =
    applyEvolution evolution worldSize gx gy baseElev hardness
applyGeoEvent (HydroEvent feature) worldSize gx gy baseElev _ =
    applyHydroFeature feature worldSize gx gy baseElev
applyGeoEvent (HydroModify _fid evolution) worldSize gx gy baseElev _ =
    applyHydroEvolution evolution worldSize gx gy baseElev
applyGeoEvent (RiverSegmentEvent rsc) worldSize gx gy baseElev _ =
    carveFromSegment worldSize gx gy
                            (rscMeanderSeed rsc) (rscSegment rsc) baseElev
applyGeoEvent (RiverDeltaEvent rdp) worldSize gx gy baseElev _ =
    computeDeltaDeposit' (rdpLastSegment rdp) (rdpFlowRate rdp)
                         worldSize gx gy baseElev
applyGeoEvent (OreSheetEvent os) _worldSize gx gy _baseElev _ =
    -- True deposition: the sheet raises the surface by its sampled
    -- thickness and the whole rise is new ore material, so the strata
    -- replay writes an ore band that later periods bury or erode.
    let delta = round (oreSheetThicknessAt os gx gy)
    in if delta ≤ 0
       then noModification
       else GeoModification delta (Just (osMat os)) delta
applyGeoEvent (LandslideEvent _)    _ _ _ _ _ = noModification
applyGeoEvent (GlaciationEvent _)   _ _ _ _ _ = noModification
applyGeoEvent (FloodEvent _)        _ _ _ _ _ = noModification

-- * Feature Evolution Application

applyEvolution ∷ FeatureEvolution → Int → Int → Int → Int → Float → GeoModification
applyEvolution (Reactivate heightGain _lavaExt center radius cElev) ws gx gy baseElev hardness =
    let GeoCoord cx cy = center
        (dxi, dyi) = wrappedDeltaUV ws gx gy cx cy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        rr = fromIntegral radius ∷ Float
        dist = perturbDist cx cy dx dy rr baseElev cElev hardness
    in if dist > rr
       then noModification
       else let t = dist / rr
                profile = 1.0 - smoothstepGeo t
                elevDelta = round (fromIntegral heightGain * profile)
                (mat, intrusion) = if t < 0.4
                    then (Just (unMaterialId matBasalt), abs elevDelta)
                    else (Nothing, 0)
            in GeoModification elevDelta mat intrusion

applyEvolution (GoDormant _center _radius) _ _ _ _ _ = noModification
applyEvolution (GoExtinct _center _radius) _ _ _ _ _ = noModification

applyEvolution (CollapseToCaldera depth _ratio center radius cElev) ws gx gy baseElev hardness =
    let GeoCoord cx cy = center
        (dxi, dyi) = wrappedDeltaUV ws gx gy cx cy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        rr = fromIntegral radius ∷ Float
        dist = perturbDist cx cy dx dy rr baseElev cElev hardness
    in if dist > rr
       then noModification
       else let t = dist / rr
                rimZone = t > 0.8
                bowlT = smoothstepGeo (t / 0.8)
                elevDelta = if rimZone
                    then 0
                    else round (negate (fromIntegral depth) * (1.0 - bowlT))
                mat = if rimZone
                    then Nothing
                    else Just (unMaterialId matObsidian)
            in GeoModification elevDelta mat 0
applyEvolution (ParasiticEruption childFeature _childId _center _radius) ws gx gy e hardness =
    applyVolcanicFeature childFeature ws gx gy e hardness
applyEvolution (FlankCollapse collapseAngle collapseWidth debrisRadius center _radius) ws gx gy _e _ =
    let GeoCoord cx cy = center
        (dxi, dyi) = wrappedDeltaUV ws gx gy cx cy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)
        debrisR = fromIntegral debrisRadius ∷ Float

    in if dist > debrisR
       then noModification
       else
       let tileAngle = atan2 dy dx
           angleDiff = abs (wrapAngle (tileAngle - collapseAngle))
           halfWidth = collapseWidth / 2.0

       in if angleDiff > halfWidth
          then let debrisFanHalf = halfWidth * 1.5
               in if angleDiff > debrisFanHalf ∨ dist < debrisR * 0.3
                  then noModification
                  else let radialT = dist / debrisR
                           angularT = (angleDiff - halfWidth) / (debrisFanHalf - halfWidth)
                           elevDelta = round (8.0 * (1.0 - radialT) * (1.0 - angularT))
                       in if elevDelta ≤ 0
                          then noModification
                          else GeoModification elevDelta Nothing 0
          else
          let radialT = dist / debrisR
              angularFade = 1.0 - (angleDiff / halfWidth) ** 2.0

          in if radialT < 0.3
             then let scarT = radialT / 0.3
                      scarDepth = min 40.0 (debrisR * 0.15) * angularFade
                      elevDelta = round (negate scarDepth * (1.0 - scarT * 0.5))
                  in GeoModification elevDelta (Just (unMaterialId matBasalt)) 0

             else if radialT < 0.7
             then let debrisT = (radialT - 0.3) / 0.4
                      peakDebris = min 15.0 (debrisR * 0.05) * angularFade
                      humpProfile = sin (debrisT * π) * (1.0 - debrisT * 0.3)
                      elevDelta = round (peakDebris * humpProfile)
                      mat = if debrisT < 0.5
                            then unMaterialId matBasalt
                            else unMaterialId matObsidian
                  in GeoModification elevDelta (Just mat) (abs elevDelta)

             else let apronT = (radialT - 0.7) / 0.3
                      apronHeight = min 5.0 (debrisR * 0.02) * angularFade
                      elevDelta = round (apronHeight * (1.0 - apronT))
                  in if elevDelta ≤ 0
                     then noModification
                     else GeoModification elevDelta Nothing 0

wrapAngle ∷ Float → Float
wrapAngle ang
    | ang > π    = ang - 2.0 * π
    | ang < (-π) = ang + 2.0 * π
    | otherwise   = ang
