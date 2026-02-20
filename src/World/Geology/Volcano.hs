{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Volcano
    ( applyVolcanicFeature
    , applyShieldVolcano
    , applyCinderCone
    , applyLavaDome
    , applyCaldera
    , applyFissure
    , applyLavaTube
    , applySuperVolcano
    , applyHydrothermal
    ) where

import UPrelude
import World.Base (GeoCoord(..))
import World.Types
import World.Material
import World.Geology.Types
import World.Geology.Hash

-----------------------------------------------------------
-- Volcanic Feature Dispatch
-----------------------------------------------------------

applyVolcanicFeature ∷ VolcanicFeature → Int → Int → Int → Int → GeoModification
applyVolcanicFeature (ShieldVolcano p)    ws gx gy e = applyShieldVolcano p ws gx gy e
applyVolcanicFeature (CinderCone p)       ws gx gy e = applyCinderCone p ws gx gy e
applyVolcanicFeature (LavaDome p)         ws gx gy e = applyLavaDome p ws gx gy e
applyVolcanicFeature (Caldera p)          ws gx gy e = applyCaldera p ws gx gy e
applyVolcanicFeature (FissureVolcano p)   ws gx gy e = applyFissure p ws gx gy e
applyVolcanicFeature (LavaTube p)         ws gx gy e = applyLavaTube p ws gx gy e
applyVolcanicFeature (SuperVolcano p)     ws gx gy e = applySuperVolcano p ws gx gy e
applyVolcanicFeature (HydrothermalVent p) ws gx gy e = applyHydrothermal p ws gx gy e

-----------------------------------------------------------
-- Shield Volcano
-----------------------------------------------------------

applyShieldVolcano ∷ ShieldParams → Int → Int → Int → Int → GeoModification
applyShieldVolcano params worldSize gx gy _baseElev =
    let GeoCoord cx cy = shCenter params
        (dxi, dyi) = wrappedDeltaUV worldSize gx gy cx cy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)

        baseR = fromIntegral (shBaseRadius params) ∷ Float
        peakH = fromIntegral (shPeakHeight params) ∷ Float
        pitR  = fromIntegral (shPitRadius params) ∷ Float
        pitD  = fromIntegral (shPitDepth params) ∷ Float

    in if dist > baseR
       then noModification

       else if shSummitPit params ∧ dist < pitR
       then let t = dist / pitR
                rimElev = peakH
                floorElev = peakH - pitD
                bowlT = smoothstepGeo t
                elevDelta = round (floorElev + (rimElev - floorElev) * bowlT)
            in GeoModification elevDelta (Just (unMaterialId matBasalt)) (abs elevDelta)

       else
       let t = dist / baseR
           profile = cos (t * π / 2.0)
           elevDelta = round (peakH * profile)
       in if t < 0.3
          then
            let intrusionFrac = 0.3 ∷ Float
                intrusion = max 1 (round (fromIntegral elevDelta * intrusionFrac))
            in GeoModification elevDelta (Just (unMaterialId matBasalt)) intrusion
          else
            GeoModification elevDelta Nothing 0

-----------------------------------------------------------
-- Cinder Cone
-----------------------------------------------------------

applyCinderCone ∷ CinderConeParams → Int → Int → Int → Int → GeoModification
applyCinderCone params worldSize gx gy _baseElev =
    let GeoCoord cx cy = ccCenter params
        (dxi, dyi) = wrappedDeltaUV worldSize gx gy cx cy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)

        baseR   = fromIntegral (ccBaseRadius params) ∷ Float
        peakH   = fromIntegral (ccPeakHeight params) ∷ Float
        craterR = fromIntegral (ccCraterRadius params) ∷ Float
        craterD = fromIntegral (ccCraterDepth params) ∷ Float

    in if dist > baseR
       then noModification

       else if dist < craterR
       then let t = dist / craterR
                rimElev = peakH
                floorElev = peakH - craterD
                bowlT = smoothstepGeo t
                elevDelta = round (floorElev + (rimElev - floorElev) * bowlT)
            in GeoModification elevDelta (Just (unMaterialId matObsidian)) (abs elevDelta)

       else
       let t = (dist - craterR) / (baseR - craterR)
           elevDelta = round (peakH * (1.0 - t))
           mat = if t < 0.3
                 then unMaterialId matObsidian
                 else unMaterialId matBasalt
       in GeoModification elevDelta (Just mat) (abs elevDelta)

-----------------------------------------------------------
-- Lava Dome
-----------------------------------------------------------

applyLavaDome ∷ LavaDomeParams → Int → Int → Int → Int → GeoModification
applyLavaDome params worldSize gx gy _baseElev =
    let GeoCoord cx cy = ldCenter params
        (dxi, dyi) = wrappedDeltaUV worldSize gx gy cx cy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)

        baseR = fromIntegral (ldBaseRadius params) ∷ Float
        height = fromIntegral (ldHeight params) ∷ Float

    in if dist > baseR
       then noModification

       else
       let t = dist / baseR
           profile = 1.0 - t ** 6.0
           elevDelta = round (height * profile)
           mat = if t < 0.5
                 then unMaterialId matObsidian
                 else unMaterialId matFeldspar
       in GeoModification elevDelta (Just mat) (abs elevDelta)

-----------------------------------------------------------
-- Caldera
-----------------------------------------------------------

applyCaldera ∷ CalderaParams → Int → Int → Int → Int → GeoModification
applyCaldera params worldSize gx gy _baseElev =
    let GeoCoord cx cy = caCenter params
        (dxi, dyi) = wrappedDeltaUV worldSize gx gy cx cy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)

        outerR = fromIntegral (caOuterRadius params) ∷ Float
        innerR = fromIntegral (caInnerRadius params) ∷ Float
        rimH   = fromIntegral (caRimHeight params) ∷ Float
        floorD = fromIntegral (caFloorDepth params) ∷ Float

        ejectaR = outerR * 1.5

    in if dist > ejectaR
       then noModification

       else if dist > outerR
       then let t = (dist - outerR) / (ejectaR - outerR)
                t' = 1.0 - smoothstepGeo t
                elevDelta = round (rimH * 0.3 * t')
            in GeoModification elevDelta Nothing 0

       else if dist > innerR
       then let rimMid = (innerR + outerR) / 2.0
                distFromMid = abs (dist - rimMid) / ((outerR - innerR) / 2.0)
                t' = 1.0 - smoothstepGeo (min 1.0 distFromMid)
                elevDelta = round (rimH * t')
            in GeoModification elevDelta (Just (unMaterialId matObsidian)) (abs elevDelta)

       else
       let t = dist / innerR
           bowlT = smoothstepGeo t
           elevDelta = round (negate (min floorD 60.0) * (1.0 - bowlT * 0.5))
           mat = if t < 0.3
                 then unMaterialId matMagma
                 else unMaterialId matObsidian
           fillDepth = max 3 (abs elevDelta `div` 3)
       in GeoModification elevDelta (Just mat) fillDepth

-----------------------------------------------------------
-- Fissure
-----------------------------------------------------------

applyFissure ∷ FissureParams → Int → Int → Int → Int → GeoModification
applyFissure params worldSize gx gy _baseElev =
    let GeoCoord sx sy = fpStart params
        GeoCoord ex ey = fpEnd params

        -- Vector from start to query point
        (pxi, pyi) = wrappedDeltaUV worldSize gx gy sx sy
        px = fromIntegral pxi ∷ Float
        py = fromIntegral pyi ∷ Float

        -- Vector from start to end
        (fxi, fyi) = wrappedDeltaUV worldSize ex ey sx sy
        fdx = fromIntegral fxi ∷ Float
        fdy = fromIntegral fyi ∷ Float
        fLen = sqrt (fdx * fdx + fdy * fdy)

    in if fLen < 0.001
       then noModification
       else
       let nx = fdx / fLen
           ny = fdy / fLen

           dot = px * nx + py * ny
           perpX = px - dot * nx
           perpY = py - dot * ny
           perpDist = sqrt (perpX * perpX + perpY * perpY)

           halfW   = fromIntegral (fpWidth params) ∷ Float
           ridgeH  = fromIntegral (fpRidgeHeight params) ∷ Float

           alongT = dot / fLen
           endTaper = min 1.0 (min (alongT * 5.0) ((1.0 - alongT) * 5.0))
           withinLength = alongT ≥ -0.05 ∧ alongT ≤ 1.05

       in if not withinLength ∨ perpDist > halfW
          then noModification

          else
          let crossT = perpDist / halfW
              profile = cos (crossT * π / 2.0) * endTaper
              elevDelta = round (ridgeH * profile)

          in if fpHasMagma params ∧ crossT < 0.15
             then GeoModification elevDelta (Just (unMaterialId matMagma)) (abs elevDelta)
             else if crossT < 0.3
             then let intrusion = max 1 (round (fromIntegral elevDelta * (0.3 ∷ Float)))
                  in GeoModification elevDelta (Just (unMaterialId matBasalt)) intrusion
             else GeoModification elevDelta Nothing 0

-----------------------------------------------------------
-- Lava Tube
-----------------------------------------------------------

applyLavaTube ∷ LavaTubeParams → Int → Int → Int → Int → GeoModification
applyLavaTube params worldSize gx gy _baseElev =
    let GeoCoord sx sy = ltStart params
        GeoCoord ex ey = ltEnd params

        (pxi, pyi) = wrappedDeltaUV worldSize gx gy sx sy
        px = fromIntegral pxi ∷ Float
        py = fromIntegral pyi ∷ Float

        (fxi, fyi) = wrappedDeltaUV worldSize ex ey sx sy
        fdx = fromIntegral fxi ∷ Float
        fdy = fromIntegral fyi ∷ Float
        fLen = sqrt (fdx * fdx + fdy * fdy)

    in if fLen < 0.001
       then noModification
       else
       let nx = fdx / fLen
           ny = fdy / fLen

           dot = px * nx + py * ny
           perpX = px - dot * nx
           perpY = py - dot * ny
           perpDist = sqrt (perpX * perpX + perpY * perpY)

           halfW  = fromIntegral (ltWidth params) ∷ Float
           ridgeH = fromIntegral (ltRidgeHeight params) ∷ Float

           alongT = dot / fLen
           withinLength = alongT ≥ 0.0 ∧ alongT ≤ 1.0

       in if not withinLength ∨ perpDist > halfW
          then noModification

          else
          let numCollapses = ltCollapses params
              collapseSeed = ltCollapseSeed params
              collapsePositions =
                  [ hashToFloatGeo (hashGeo collapseSeed i 60) | i ← [0 .. numCollapses - 1] ]
              collapseRadius = halfW * 0.8
              nearCollapse = any (\cPos →
                  let cDist = abs (alongT - cPos) * fLen
                  in cDist < collapseRadius ∧ perpDist < collapseRadius
                  ) collapsePositions

              crossT = perpDist / halfW
              ridgeProfile = cos (crossT * π / 2.0)

          in if nearCollapse
             then let pitDepth = ridgeH * 2.0
                      elevDelta = round (negate pitDepth * (1.0 - crossT))
                  in GeoModification elevDelta (Just (unMaterialId matBasalt)) 0
             else
             let elevDelta = round (ridgeH * ridgeProfile)
             in GeoModification elevDelta Nothing 0

-----------------------------------------------------------
-- Super Volcano
-----------------------------------------------------------

applySuperVolcano ∷ SuperVolcanoParams → Int → Int → Int → Int → GeoModification
applySuperVolcano params worldSize gx gy baseElev =
    let GeoCoord cx cy = svCenter params
        (dxi, dyi) = wrappedDeltaUV worldSize gx gy cx cy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)

        calderaR = fromIntegral (svCalderaRadius params) ∷ Float
        rimH     = fromIntegral (svRimHeight params) ∷ Float
        floorD   = fromIntegral (svFloorDepth params) ∷ Float
        ejectaR  = fromIntegral (svEjectaRadius params) ∷ Float
        ejectaD  = fromIntegral (svEjectaDepth params) ∷ Float

        rimWidth = calderaR * 0.15
        rimOuterR = calderaR + rimWidth
        rimInnerR = calderaR - rimWidth

        maxFloorDrop = max 10.0 (fromIntegral baseElev + 50.0)
        clampedFloorD = min floorD maxFloorDrop

    in if dist > ejectaR
       then noModification

       else if dist > rimOuterR
       then let t = (dist - rimOuterR) / (ejectaR - rimOuterR)
                t' = (1.0 - t) ** 2.0
                elevDelta = round (ejectaD * t')
            in GeoModification elevDelta Nothing 0

       else if dist > rimInnerR
       then let rimMid = (rimInnerR + rimOuterR) / 2.0
                distFromMid = abs (dist - rimMid) / rimWidth
                t' = 1.0 - smoothstepGeo (min 1.0 distFromMid)
                elevDelta = round (rimH * t')
            in GeoModification elevDelta (Just (unMaterialId matObsidian)) (abs elevDelta)

       else
       let t = dist / rimInnerR
           bowlT = smoothstepGeo t
           elevDelta = round (negate clampedFloorD * (1.0 - bowlT * 0.7))
           mat = if t < 0.15
                 then unMaterialId matMagma
                 else if t < 0.4
                 then unMaterialId matObsidian
                 else unMaterialId matBasalt
           fillDepth = max 3 (abs elevDelta `div` 3)
       in GeoModification elevDelta (Just mat) fillDepth

-----------------------------------------------------------
-- Hydrothermal Vent
-----------------------------------------------------------

applyHydrothermal ∷ HydrothermalParams → Int → Int → Int → Int → GeoModification
applyHydrothermal params worldSize gx gy _baseElev =
    let GeoCoord cx cy = htCenter params
        (dxi, dyi) = wrappedDeltaUV worldSize gx gy cx cy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)

        radius = fromIntegral (htRadius params) ∷ Float
        chimneyH = fromIntegral (htChimneyHeight params) ∷ Float

    in if dist > radius
       then noModification

       else
       let t = dist / radius
           chimneyR = 0.15

       in if t < chimneyR
          then let chimneyT = t / chimneyR
                   profile = 1.0 - chimneyT * 0.3
                   elevDelta = round (chimneyH * profile)
               in GeoModification elevDelta (Just (unMaterialId matMagma)) (abs elevDelta)
          else
          let moundT = (t - chimneyR) / (1.0 - chimneyR)
              moundH = chimneyH * 0.3
              profile = (1.0 - moundT) ** 2.0
              elevDelta = round (moundH * profile)
              mat = if moundT < 0.3
                    then unMaterialId matMagma
                    else unMaterialId matObsidian
          in GeoModification elevDelta (Just mat) (abs elevDelta)
