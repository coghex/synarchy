{-# LANGUAGE Strict #-}
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
import World.Types
import World.Material
import World.Geology.Types
import World.Geology.Hash

-----------------------------------------------------------
-- Volcanic Feature Dispatch
-----------------------------------------------------------

applyVolcanicFeature :: VolcanicFeature -> Int -> Int -> Int -> Int -> GeoModification
applyVolcanicFeature (ShieldVolcano p)    ws gx gy e = applyShieldVolcano p ws gx gy e
applyVolcanicFeature (CinderCone p)       ws gx gy e = applyCinderCone p ws gx gy e
applyVolcanicFeature (LavaDome p)         ws gx gy e = applyLavaDome p ws gx gy e
applyVolcanicFeature (Caldera p)          ws gx gy e = applyCaldera p ws gx gy e
applyVolcanicFeature (FissureVolcano p)   ws gx gy e = applyFissure p ws gx gy e
applyVolcanicFeature (LavaTube p)         ws gx gy e = applyLavaTube p ws gx gy e
applyVolcanicFeature (SuperVolcano p)     ws gx gy e = applySuperVolcano p ws gx gy e
applyVolcanicFeature (HydrothermalVent p) ws gx gy e = applyHydrothermal p ws gx gy e

-----------------------------------------------------------
-- Volcanic Feature Application
-----------------------------------------------------------

-- | Shield Volcano
--   Very wide, gently sloping, convex profile.
--   Think Mauna Loa — the slope is so gentle you barely
--   notice you're on a volcano.
--
--   Profile:
--       ___
--      /   \       <- optional summit pit
--    ./     \.     <- convex flanks (gentle, wide)
--   /.........\
--
applyShieldVolcano :: ShieldParams -> Int -> Int -> Int -> Int -> GeoModification
applyShieldVolcano params worldSize gx gy _baseElev =
    let GeoCoord cx cy = shCenter params
        dx = fromIntegral (wrappedDeltaXGeo worldSize gx cx) :: Float
        dy = fromIntegral (gy - cy) :: Float
        dist = sqrt (dx * dx + dy * dy)

        baseR = fromIntegral (shBaseRadius params) :: Float
        peakH = fromIntegral (shPeakHeight params) :: Float
        pitR  = fromIntegral (shPitRadius params) :: Float
        pitD  = fromIntegral (shPitDepth params) :: Float

    in if dist > baseR
       then noModification

       else if shSummitPit params && dist < pitR
       -- Summit pit: small depression at the very top
       then let t = dist / pitR  -- 0 at center, 1 at pit edge
                -- Pit floor to rim transition
                rimElev = peakH
                floorElev = peakH - pitD
                bowlT = smoothstepGeo t
                elevDelta = round (floorElev + (rimElev - floorElev) * bowlT)
            in GeoModification elevDelta (Just (unMaterialId matBasalt))

       else
       -- Flanks: convex profile using cosine curve
       -- Cosine gives a very gentle approach at the base
       -- and a flatter top — exactly what shield volcanoes look like
       let t = dist / baseR  -- 0 at center, 1 at base edge
            -- Cosine profile: cos(t * pi/2) gives 1.0 at center, 0.0 at edge
            -- with a convex (bulging outward) shape
           profile = cos (t * pi / 2.0)
           elevDelta = round (peakH * profile)
       in GeoModification elevDelta (Just (unMaterialId matBasalt))

-- | Cinder Cone
--   Small, steep, perfectly conical with a crater at the top.
--   Always has a crater — that's what defines a cinder cone.
--
--   Profile:
--      /\
--     /  \      <- steep linear slopes
--    / __ \     <- always has a crater
--   /|    |\
--  / |    | \
--
applyCinderCone :: CinderConeParams -> Int -> Int -> Int -> Int -> GeoModification
applyCinderCone params worldSize gx gy _baseElev =
    let GeoCoord cx cy = ccCenter params
        dx = fromIntegral (wrappedDeltaXGeo worldSize gx cx) :: Float
        dy = fromIntegral (gy - cy) :: Float
        dist = sqrt (dx * dx + dy * dy)

        baseR   = fromIntegral (ccBaseRadius params) :: Float
        peakH   = fromIntegral (ccPeakHeight params) :: Float
        craterR = fromIntegral (ccCraterRadius params) :: Float
        craterD = fromIntegral (ccCraterDepth params) :: Float

    in if dist > baseR
       then noModification

       else if dist < craterR
       -- Crater bowl
       then let t = dist / craterR  -- 0 at center, 1 at rim
                rimElev = peakH
                floorElev = peakH - craterD
                bowlT = smoothstepGeo t
                elevDelta = round (floorElev + (rimElev - floorElev) * bowlT)
            in GeoModification elevDelta (Just (unMaterialId matObsidian))

       else
       -- Flanks: linear cone (cinder cones are very regular)
       let t = (dist - craterR) / (baseR - craterR)  -- 0 at rim, 1 at base
           elevDelta = round (peakH * (1.0 - t))
           -- Upper portion is obsidian (scoria), lower is basalt
           mat = if t < 0.3
                 then unMaterialId matObsidian
                 else unMaterialId matBasalt
       in GeoModification elevDelta (Just mat)

-- | Lava Dome
--   Rounded mound with very steep sides and a flattish top.
--   Formed by viscous lava that piles up rather than flowing.
--   Think Mt. Lassen or the dome inside Mt. St. Helens crater.
--
--   Profile:
--      ____
--     /    \     <- flat-ish top
--    |      |    <- near-vertical sides
--    |      |
--   _/      \_   <- abrupt base
--
applyLavaDome :: LavaDomeParams -> Int -> Int -> Int -> Int -> GeoModification
applyLavaDome params worldSize gx gy _baseElev =
    let GeoCoord cx cy = ldCenter params
        dx = fromIntegral (wrappedDeltaXGeo worldSize gx cx) :: Float
        dy = fromIntegral (gy - cy) :: Float
        dist = sqrt (dx * dx + dy * dy)

        baseR = fromIntegral (ldBaseRadius params) :: Float
        height = fromIntegral (ldHeight params) :: Float

    in if dist > baseR
       then noModification

       else
       -- Super-steep sigmoid profile
       -- Maps dist/baseR through a steep sigmoid so the sides
       -- are nearly vertical and the top is nearly flat
       let t = dist / baseR  -- 0 at center, 1 at edge
           -- Raise to a high power for the steep-sided effect
           -- t^6 gives nearly flat top with sharp dropoff at edges
           profile = 1.0 - t ** 6.0
           elevDelta = round (height * profile)
           -- Obsidian throughout — domes are viscous silicic lava
           mat = if t < 0.5
                 then unMaterialId matObsidian
                 else unMaterialId matFeldspar
       in GeoModification elevDelta (Just mat)

-- | Caldera
--   Collapsed volcanic structure. Broad ring of raised rim
--   surrounding a depressed floor. Can be enormous.
--   Think Crater Lake, Yellowstone, Santorini.
--
--   Profile:
--          __          __
--         /  \        /  \      <- rim peaks
--        /    \______/    \     <- flat caldera floor
--   ____/                  \____
--
applyCaldera :: CalderaParams -> Int -> Int -> Int -> Int -> GeoModification
applyCaldera params worldSize gx gy _baseElev =
    let GeoCoord cx cy = caCenter params
        dx = fromIntegral (wrappedDeltaXGeo worldSize gx cx) :: Float
        dy = fromIntegral (gy - cy) :: Float
        dist = sqrt (dx * dx + dy * dy)

        outerR = fromIntegral (caOuterRadius params) :: Float
        innerR = fromIntegral (caInnerRadius params) :: Float
        rimH   = fromIntegral (caRimHeight params) :: Float
        floorD = fromIntegral (caFloorDepth params) :: Float

        -- Ejecta/slope extends 1.5x beyond the rim
        ejectaR = outerR * 1.5

    in if dist > ejectaR
       then noModification

       else if dist > outerR
       -- Outer slope: gentle rise from surroundings to rim
       then let t = (dist - outerR) / (ejectaR - outerR)  -- 0 at rim, 1 at edge
                t' = 1.0 - smoothstepGeo t
                elevDelta = round (rimH * 0.3 * t')
            in GeoModification elevDelta (Just (unMaterialId matBasalt))

       else if dist > innerR
       -- Rim zone: the raised ring
       -- Peak is at the midpoint between inner and outer radius
       then let rimMid = (innerR + outerR) / 2.0
                -- Distance from the rim midpoint, normalized
                distFromMid = abs (dist - rimMid) / ((outerR - innerR) / 2.0)
                t' = 1.0 - smoothstepGeo (min 1.0 distFromMid)
                elevDelta = round (rimH * t')
            in GeoModification elevDelta (Just (unMaterialId matObsidian))

       else
       -- Caldera floor: flat depression
       -- Slight bowl shape — deeper at center, rising toward inner rim
       let t = dist / innerR  -- 0 at center, 1 at inner rim edge
           -- Gentle bowl with smoothstep
           bowlT = smoothstepGeo t
           -- Floor is below ground level (negative delta)
           -- Rises from -floorD at center toward 0 at inner rim
           elevDelta = round (negate floorD * (1.0 - bowlT * 0.5))
           mat = if t < 0.3
                 then unMaterialId matMagma  -- hot center
                 else unMaterialId matObsidian
       in GeoModification elevDelta (Just mat)

-- | Fissure Volcano
--   Linear ridge rather than a point feature.
--   Lava erupts along a crack in the crust.
--   Think Laki in Iceland, or the East African Rift.
--
--   Profile (cross-section perpendicular to fissure):
--      ____
--     / || \     <- central channel (magma if active)
--    /  ||  \    <- gentle ridge slopes
--   /   ||   \
--
applyFissure :: FissureParams -> Int -> Int -> Int -> Int -> GeoModification
applyFissure params worldSize gx gy _baseElev =
    let GeoCoord sx sy = fpStart params
        GeoCoord ex ey = fpEnd params

        -- Current tile position relative to fissure start
        -- Use wrapped X for cylindrical world
        px = fromIntegral (wrappedDeltaXGeo worldSize gx sx) :: Float
        py = fromIntegral (gy - sy) :: Float

        -- Fissure direction vector
        fdx = fromIntegral (wrappedDeltaXGeo worldSize ex sx) :: Float
        fdy = fromIntegral (ey - sy) :: Float
        fLen = sqrt (fdx * fdx + fdy * fdy)

    in if fLen < 0.001
       then noModification
       else
       let -- Normalized fissure direction
           nx = fdx / fLen
           ny = fdy / fLen

           -- Project point onto fissure line
           -- dot = how far along the line
           -- perp = perpendicular distance from line
           dot = px * nx + py * ny
           perpX = px - dot * nx
           perpY = py - dot * ny
           perpDist = sqrt (perpX * perpX + perpY * perpY)

           halfW   = fromIntegral (fpWidth params) :: Float
           ridgeH  = fromIntegral (fpRidgeHeight params) :: Float

           -- Check if we're within the fissure's length
           -- Allow some taper at the ends
           alongT = dot / fLen  -- 0 at start, 1 at end
           endTaper = min 1.0 (min (alongT * 5.0) ((1.0 - alongT) * 5.0))
           withinLength = alongT >= -0.05 && alongT <= 1.05

       in if not withinLength || perpDist > halfW
          then noModification

          else
          let -- Cross-section profile: ridge shape perpendicular to fissure
              crossT = perpDist / halfW  -- 0 at center, 1 at edge
              profile = cos (crossT * pi / 2.0) * endTaper
              elevDelta = round (ridgeH * profile)

              -- Central channel has magma if active
              mat = if fpHasMagma params && crossT < 0.15
                    then unMaterialId matMagma
                    else unMaterialId matBasalt
          in GeoModification elevDelta (Just mat)

-- | Lava Tube
--   Subsurface tunnel formed by flowing lava that crusted over.
--   On the surface: a subtle ridge with occasional collapse pits
--   (skylights) where the ceiling has fallen in.
--
--   Profile (cross-section):
--      __          ___         __
--     /  \        /   \       /  \     <- subtle ridge
--    /    \__  __/     \__  _/    \
--             \/                       <- collapse pits (skylights)
--
applyLavaTube :: LavaTubeParams -> Int -> Int -> Int -> Int -> GeoModification
applyLavaTube params worldSize gx gy _baseElev =
    let GeoCoord sx sy = ltStart params
        GeoCoord ex ey = ltEnd params

        px = fromIntegral (wrappedDeltaXGeo worldSize gx sx) :: Float
        py = fromIntegral (gy - sy) :: Float

        fdx = fromIntegral (wrappedDeltaXGeo worldSize ex sx) :: Float
        fdy = fromIntegral (ey - sy) :: Float
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

           halfW  = fromIntegral (ltWidth params) :: Float
           ridgeH = fromIntegral (ltRidgeHeight params) :: Float

           alongT = dot / fLen
           withinLength = alongT >= 0.0 && alongT <= 1.0

       in if not withinLength || perpDist > halfW
          then noModification

          else
          -- Check for collapse pits along the tube
          let numCollapses = ltCollapses params
              collapseSeed = ltCollapseSeed params
              -- Generate collapse positions along the tube (0.0 to 1.0)
              collapsePositions =
                  [ hashToFloatGeo (hashGeo collapseSeed i 60) | i <- [0 .. numCollapses - 1] ]
              -- Check if we're near any collapse
              collapseRadius = halfW * 0.8
              nearCollapse = any (\cPos ->
                  let cDist = abs (alongT - cPos) * fLen
                  in cDist < collapseRadius && perpDist < collapseRadius
                  ) collapsePositions

              crossT = perpDist / halfW
              ridgeProfile = cos (crossT * pi / 2.0)

          in if nearCollapse
             -- Collapse pit: depression where the roof fell in
             then let pitDepth = ridgeH * 2.0  -- drops below surface
                      elevDelta = round (negate pitDepth * (1.0 - crossT))
                  in GeoModification elevDelta (Just (unMaterialId matBasalt))
             else
             -- Normal tube: subtle surface ridge
             let elevDelta = round (ridgeH * ridgeProfile)
             in GeoModification elevDelta (Just (unMaterialId matBasalt))

-- | Super Volcano
--   Enormous caldera with a massive ejecta/ash field.
--   The caldera itself is similar to a regular caldera but
--   much larger, and the ejecta field extends hundreds of tiles.
--   Think Yellowstone — the caldera is so large you can't see
--   it from the ground.
--
--   Profile:
--                ____________________
--     __________/                    \__________    <- ejecta blanket
--    /     _____                      _____     \
--   /     /     \____________________/     \     \  <- massive rim
--  /     /                                  \     \
-- /     /           caldera floor            \     \
--
applySuperVolcano :: SuperVolcanoParams -> Int -> Int -> Int -> Int -> GeoModification
applySuperVolcano params worldSize gx gy _baseElev =
    let GeoCoord cx cy = svCenter params
        dx = fromIntegral (wrappedDeltaXGeo worldSize gx cx) :: Float
        dy = fromIntegral (gy - cy) :: Float
        dist = sqrt (dx * dx + dy * dy)

        calderaR = fromIntegral (svCalderaRadius params) :: Float
        rimH     = fromIntegral (svRimHeight params) :: Float
        floorD   = fromIntegral (svFloorDepth params) :: Float
        ejectaR  = fromIntegral (svEjectaRadius params) :: Float
        ejectaD  = fromIntegral (svEjectaDepth params) :: Float

        -- Rim is a band around the caldera edge
        rimWidth = calderaR * 0.15
        rimOuterR = calderaR + rimWidth
        rimInnerR = calderaR - rimWidth

    in if dist > ejectaR
       then noModification

       else if dist > rimOuterR
       -- Ejecta blanket: ash and debris tapering off with distance
       then let t = (dist - rimOuterR) / (ejectaR - rimOuterR)
                -- Inverse square falloff for ejecta
                t' = (1.0 - t) ** 2.0
                elevDelta = round (ejectaD * t')
            in GeoModification elevDelta (Just (unMaterialId matBasalt))

       else if dist > rimInnerR
       -- Rim zone
       then let rimMid = (rimInnerR + rimOuterR) / 2.0
                distFromMid = abs (dist - rimMid) / (rimWidth)
                t' = 1.0 - smoothstepGeo (min 1.0 distFromMid)
                elevDelta = round (rimH * t')
            in GeoModification elevDelta (Just (unMaterialId matObsidian))

       else
       -- Caldera floor
       let t = dist / rimInnerR  -- 0 at center, 1 at inner rim
           bowlT = smoothstepGeo t
           -- Deeper at center, gradual rise toward rim
           elevDelta = round (negate floorD * (1.0 - bowlT * 0.7))
           -- Magma features at the center, obsidian further out
           mat = if t < 0.15
                 then unMaterialId matMagma
                 else if t < 0.4
                 then unMaterialId matObsidian
                 else unMaterialId matBasalt
       in GeoModification elevDelta (Just mat)

-- | Hydrothermal Vent
--   Small mound on the ocean floor with a chimney.
--   Black smoker / white smoker style.
--   Very small feature — only a few tiles across.
--
--   Profile:
--       |
--      /|\       <- chimney spike
--     / | \
--    / _|_ \     <- mound base
--   /       \
--
applyHydrothermal :: HydrothermalParams -> Int -> Int -> Int -> Int -> GeoModification
applyHydrothermal params worldSize gx gy _baseElev =
    let GeoCoord cx cy = htCenter params
        dx = fromIntegral (wrappedDeltaXGeo worldSize gx cx) :: Float
        dy = fromIntegral (gy - cy) :: Float
        dist = sqrt (dx * dx + dy * dy)

        radius = fromIntegral (htRadius params) :: Float
        chimneyH = fromIntegral (htChimneyHeight params) :: Float

    in if dist > radius
       then noModification

       else
       let t = dist / radius  -- 0 at center, 1 at edge

           -- Two-part profile:
           -- Inner chimney spike (narrow, tall)
           -- Outer mound (wide, low)
           chimneyR = 0.15  -- chimney is 15% of total radius
           
       in if t < chimneyR
          -- Chimney: sharp spike
          then let chimneyT = t / chimneyR  -- 0 at center, 1 at chimney edge
                   -- Linear taper for the chimney
                   profile = 1.0 - chimneyT * 0.3
                   elevDelta = round (chimneyH * profile)
               in GeoModification elevDelta (Just (unMaterialId matMagma))
          else
          -- Mound: gentle rise
          let moundT = (t - chimneyR) / (1.0 - chimneyR)  -- 0 at chimney edge, 1 at base
              -- Mound is much lower than chimney
              moundH = chimneyH * 0.3
              profile = (1.0 - moundT) ** 2.0
              elevDelta = round (moundH * profile)
              mat = if moundT < 0.3
                    then unMaterialId matMagma
                    else unMaterialId matObsidian
          in GeoModification elevDelta (Just mat)
