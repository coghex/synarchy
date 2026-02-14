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
applyGeoEvent (LandslideEvent _)    _ _ _ _ = noModification
applyGeoEvent (GlaciationEvent _)   _ _ _ _ = noModification
applyGeoEvent (FloodEvent _)        _ _ _ _ = noModification

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
       let -- Angle from center to this tile
           tileAngle = atan2 dy dx
           -- Angular distance from collapse direction
           angleDiff = abs (wrapAngle (tileAngle - collapseAngle))
           halfWidth = collapseWidth / 2.0

       in if angleDiff > halfWidth
          -- Outside the collapse sector — check if in the debris fan
          then let debrisFanHalf = halfWidth * 1.5
               in if angleDiff > debrisFanHalf ∨ dist < debrisR * 0.3
                  then noModification
                  else -- Thin debris fringe: small uplift from scattered rubble
                       let radialT = dist / debrisR
                           angularT = (angleDiff - halfWidth) / (debrisFanHalf - halfWidth)
                           elevDelta = round (15.0 * (1.0 - radialT) * (1.0 - angularT))
                       in if elevDelta ≤ 0
                          then noModification
                          else GeoModification elevDelta Nothing 0
          else
          -- Inside the collapse sector
          let radialT = dist / debrisR
              -- Angular fade: strongest at the center of the sector,
              -- weaker at the edges
              angularFade = 1.0 - (angleDiff / halfWidth) ** 2.0

              -- Three zones along the radial axis:
              --   Inner (< 0.3): the scar — deep depression, exposed rock
              --   Middle (0.3-0.7): the hummocky debris field
              --   Outer (> 0.7): thinning debris apron

          in if radialT < 0.3
             then -- Scar zone: deep gouge into the volcanic edifice
                  let scarT = radialT / 0.3
                      -- Deepest at the center, shallower at the rim of the scar
                      scarDepth = debrisR * 0.4 * angularFade
                      elevDelta = round (negate scarDepth * (1.0 - scarT * 0.5))
                      -- Exposed interior rock — basalt from the volcano's guts
                  in GeoModification elevDelta (Just (unMaterialId matBasalt)) 0

             else if radialT < 0.7
             then -- Debris field: hummocky mounds of collapsed material
                  -- Elevation is slightly positive (debris piled up)
                  -- but less than the original volcano height
                  let debrisT = (radialT - 0.3) / 0.4
                      peakDebris = debrisR * 0.15 * angularFade
                      -- Hump profile: rises then falls
                      humpProfile = sin (debrisT * π) * (1.0 - debrisT * 0.3)
                      elevDelta = round (peakDebris * humpProfile)
                      -- Mix of basalt rubble and obsidian fragments
                      mat = if debrisT < 0.5
                            then unMaterialId matBasalt
                            else unMaterialId matObsidian
                  in GeoModification elevDelta (Just mat) (abs elevDelta)

             else -- Debris apron: thinning blanket of rubble
                  let apronT = (radialT - 0.7) / 0.3
                      apronHeight = debrisR * 0.05 * angularFade
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
