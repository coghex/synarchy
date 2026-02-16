{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Event
    ( applyGeoEvent
    , applyEvolution
    ) where

import UPrelude
import World.Types
import World.Material
import World.Geology.Types
import World.Geology.Crater (applyCrater)
import World.Geology.Volcano (applyVolcanicFeature)
import World.Geology.Hash (wrappedDeltaXGeo, smoothstepGeo)

-----------------------------------------------------------
-- Event Application
-----------------------------------------------------------

applyGeoEvent ∷ GeoEvent → Int → Int → Int → Int → GeoModification
applyGeoEvent (CraterEvent params)  worldSize gx gy baseElev =
    applyCrater params worldSize gx gy baseElev
applyGeoEvent (VolcanicEvent feature) worldSize gx gy baseElev =
    applyVolcanicFeature feature worldSize gx gy baseElev
applyGeoEvent (VolcanicModify _fid evolution) worldSize gx gy baseElev =
    applyEvolution evolution worldSize gx gy baseElev
applyGeoEvent (EruptionEvent _fid flow) worldSize gx gy baseElev =
    applyLavaFlow flow worldSize gx gy baseElev
applyGeoEvent (LandslideEvent _)    _ _ _ _ = noModification
applyGeoEvent (GlaciationEvent _)   _ _ _ _ = noModification
applyGeoEvent (FloodEvent _)        _ _ _ _ = noModification

-----------------------------------------------------------
-- Lava Flow Application
-----------------------------------------------------------

-- | Apply a lava flow to a single column.
--   Lava flows radially from the source, losing elevation
--   with distance based on viscosity. If the lava surface
--   at this column is above the current terrain, material
--   is deposited to fill the gap.
--
--   viscosity=1: runny basalt, loses 1 tile of height per tile distance
--   viscosity=2: moderate, loses 2 per tile
--   viscosity=3: viscous obsidian, loses 3 per tile (piles up near source)
--
--   The flow only deposits material where the lava surface
--   is above the existing terrain — it fills valleys and
--   pools in depressions rather than coating hilltops.
applyLavaFlow ∷ LavaFlow → Int → Int → Int → Int → GeoModification
applyLavaFlow flow worldSize gx gy baseElev =
    let sx = lfSourceX flow
        sy = lfSourceY flow
        dx = fromIntegral (wrappedDeltaXGeo worldSize gx sx) ∷ Float
        dy = fromIntegral (gy - sy) ∷ Float
        dist = sqrt (dx * dx + dy * dy)
        maxR = fromIntegral (lfRadius flow) ∷ Float

    in if dist > maxR
       then noModification
       else
       let -- Lava surface elevation at this distance from source
           visc = fromIntegral (lfViscosity flow) ∷ Float
           lavaSurface = lfElevation flow - round (dist * visc)

           -- How much material to deposit: difference between
           -- lava surface and current terrain
           deposit = lavaSurface - baseElev

       in if deposit ≤ 0
          then noModification
          else -- Deposit is entirely new volcanic material
               GeoModification deposit (Just (lfMaterial flow)) deposit

-----------------------------------------------------------
-- Feature Evolution Application
-----------------------------------------------------------

applyEvolution ∷ FeatureEvolution → Int → Int → Int → Int → GeoModification
applyEvolution (Reactivate heightGain _lavaExt center radius) ws gx gy _e =
    let GeoCoord cx cy = center
        dx = fromIntegral (wrappedDeltaXGeo ws gx cx) ∷ Float
        dy = fromIntegral (gy - cy) ∷ Float
        dist = sqrt (dx * dx + dy * dy)
        rr = fromIntegral radius ∷ Float
    in if dist > rr
       then noModification
       else let t = dist / rr
                profile = 1.0 - smoothstepGeo t
                elevDelta = round (fromIntegral heightGain * profile)
                -- Inner 40%: fresh basalt deposition (full intrusion)
                -- Outer 60%: pure uplift from magma pressure
                (mat, intrusion) = if t < 0.4
                    then (Just (unMaterialId matBasalt), abs elevDelta)
                    else (Nothing, 0)
            in GeoModification elevDelta mat intrusion

applyEvolution (GoDormant _center _radius) _ _ _ _ = noModification
applyEvolution (GoExtinct _center _radius) _ _ _ _ = noModification

applyEvolution (CollapseToCaldera depth _ratio center radius) ws gx gy _e =
    let GeoCoord cx cy = center
        dx = fromIntegral (wrappedDeltaXGeo ws gx cx) ∷ Float
        dy = fromIntegral (gy - cy) ∷ Float
        dist = sqrt (dx * dx + dy * dy)
        rr = fromIntegral radius ∷ Float
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
                -- Collapse is depression/rearrangement, not deposition
            in GeoModification elevDelta mat 0
applyEvolution (ParasiticEruption childFeature _childId _center _radius) ws gx gy e =
    applyVolcanicFeature childFeature ws gx gy e
applyEvolution (FlankCollapse collapseAngle collapseWidth debrisRadius center _radius) ws gx gy _e =
    let GeoCoord cx cy = center
        dx = fromIntegral (wrappedDeltaXGeo ws gx cx) ∷ Float
        dy = fromIntegral (gy - cy) ∷ Float
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
             then -- Scar zone: moderate gouge
                  let scarT = radialT / 0.3
                      scarDepth = min 40.0 (debrisR * 0.15) * angularFade
                      elevDelta = round (negate scarDepth * (1.0 - scarT * 0.5))
                  in GeoModification elevDelta (Just (unMaterialId matBasalt)) 0

             else if radialT < 0.7
             then -- Debris field: low hummocky mounds
                  let debrisT = (radialT - 0.3) / 0.4
                      peakDebris = min 15.0 (debrisR * 0.05) * angularFade
                      humpProfile = sin (debrisT * π) * (1.0 - debrisT * 0.3)
                      elevDelta = round (peakDebris * humpProfile)
                      mat = if debrisT < 0.5
                            then unMaterialId matBasalt
                            else unMaterialId matObsidian
                  in GeoModification elevDelta (Just mat) (abs elevDelta)

             else -- Debris apron: thin rubble
                  let apronT = (radialT - 0.7) / 0.3
                      apronHeight = min 5.0 (debrisR * 0.02) * angularFade
                      elevDelta = round (apronHeight * (1.0 - apronT))
                  in if elevDelta ≤ 0
                     then noModification
                     else GeoModification elevDelta Nothing 0

-- | Wrap an angle difference to [-π, π]
wrapAngle ∷ Float → Float
wrapAngle ang
    | ang > π    = ang - 2.0 * π
    | ang < (-π) = ang + 2.0 * π
    | otherwise   = ang
