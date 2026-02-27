{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Crater
    ( generateCraters
    , generateCraterAttempt
    , determineMeteoriteType
    , applyCrater
    ) where

import UPrelude
import World.Base (GeoCoord(..))
import World.Types
import World.Material
import World.Plate (isBeyondGlacier, elevationAtGlobal, TectonicPlate
                   , wrapGlobalU)
import World.Geology.Types
import World.Geology.Hash

-----------------------------------------------------------
-- Crater Generation
-----------------------------------------------------------

-- | Generate a set of crater events for a bombardment era.
--   Picks random land positions, avoids ocean and glacier,
--   determines size and whether a meteorite survives.
generateCraters ∷ Word64 → Int → [TectonicPlate] → CraterEra → [CraterParams]
generateCraters seed worldSize plates era =
    let (baseCount, minRadius, maxRadius, minDepth, maxDepth, rimMin, rimMax) = case era of
            CraterEra_Primordial → (6, 20, 60, 15, 50, 5, 20)
            CraterEra_Late       → (10,  5, 25,  4, 20, 2, 10)

        count = scaleCount worldSize baseCount
        halfTiles = (worldSize * 16) `div` 2

        attempts = map (generateCraterAttempt seed worldSize plates
                            halfTiles minRadius maxRadius
                            minDepth maxDepth rimMin rimMax) [0 .. count * 3 - 1]
        valid = take count [ cp | Just cp ← attempts ]

    in valid

-- | Try to place a single crater. Returns Nothing if it
--   lands in ocean or glacier.
generateCraterAttempt ∷ Word64 → Int → [TectonicPlate]
                      → Int → Int → Int → Int → Int → Int → Int
                      → Int → Maybe CraterParams
generateCraterAttempt seed worldSize plates halfTiles
                      minRadius maxRadius minDepth maxDepth
                      rimMin rimMax attemptIdx =
    let -- Hash the attempt index to get position and size
        h1 = hashGeo seed attemptIdx 1
        h2 = hashGeo seed attemptIdx 2
        h3 = hashGeo seed attemptIdx 3
        h4 = hashGeo seed attemptIdx 4
        h5 = hashGeo seed attemptIdx 5
        h6 = hashGeo seed attemptIdx 6

        gx' = hashToRangeGeo h1 (-halfTiles) (halfTiles - 1)
        gy' = hashToRangeGeo h2 (-halfTiles) (halfTiles - 1)
        (gx, gy) = wrapGlobalU worldSize gx' gy'

        -- Check that we're on land
        (elev, _mat) = elevationAtGlobal seed plates worldSize gx gy

        -- Reject ocean (negative elevation) and glacier
        isOcean   = elev < -100
        isGlacier = isBeyondGlacier worldSize gx gy

        radius      = hashToRangeGeo h3 minRadius maxRadius
        depth       = hashToRangeGeo h4 minDepth maxDepth
        rimHeight   = hashToRangeGeo h5 rimMin rimMax
        ejectaScale = hashToFloatGeo h6

        -- Ejecta extends 1.2-1.6x the crater radius
        ejectaRadius = radius + round (fromIntegral radius * (0.2 + ejectaScale))

        -- Meteorite survives in larger impacts
        meteoriteType = determineMeteoriteType seed attemptIdx radius

    in if isOcean ∨ isGlacier
       then Nothing
       else Just CraterParams
            { cpCenter       = GeoCoord gx gy
            , cpRadius       = radius
            , cpDepth        = depth
            , cpRimHeight    = rimHeight
            , cpEjectaRadius = ejectaRadius
            , cpMeteorite    = meteoriteType
            }

-- | Determine what type of meteorite (if any) survives the impact.
--   Larger craters are more likely to leave meteorite fragments.
--   Returns the material ID of the meteorite, or Nothing.
determineMeteoriteType ∷ Word64 → Int → Int → Maybe Word8
determineMeteoriteType seed attemptIdx radius =
    let h = hashGeo seed attemptIdx 10
        chance = hashToFloatGeo h
        -- Smaller craters are more likely to have surviving meteorites
        -- (large impacts vaporize the impactor)
        survivalChance = if radius < 20 then 0.7
                         else if radius < 50 then 0.4
                         else if radius < 80 then 0.15
                         else 0.05
    in if chance > survivalChance
       then Nothing
       else let typeHash = hashGeo seed attemptIdx 11
                typeVal  = hashToRangeGeo typeHash 0 3
            in Just $ case typeVal of
                0 → unMaterialId matIronOre   -- Iron meteorite
                1 → unMaterialId matOlivine   -- Stony (olivine-rich)
                2 → unMaterialId matPyroxene  -- Stony (pyroxene-rich)
                _ → unMaterialId matFeldspar  -- Stony (feldspar-rich)

-----------------------------------------------------------
-- Crater Application
-----------------------------------------------------------

--   Profile (cross section):
--
--     ejecta      rim
--       \         /|
--        \_______/ |
--                  |     <- surrounding terrain
--                  |____
--                       \
--                        \____  <- bowl floor
--
-- | Apply a crater's effect to a single tile position.
--   Ejecta blanket: pure uplift (no new material, strata pushed up).
--   Rim: impactite, full intrusion (impact-melted rock).
--   Bowl: depression (erosion), impactite at the surface.
--   Center: meteorite material if present.
applyCrater ∷ CraterParams → Int → Int → Int → Int → GeoModification
applyCrater params worldSize gx gy _baseElev =
    let GeoCoord cx cy = cpCenter params
        (dxi, dyi) = wrappedDeltaUV worldSize gx gy cx cy
        dx = fromIntegral dxi ∷ Float
        dy = fromIntegral dyi ∷ Float
        dist = sqrt (dx * dx + dy * dy)

        radius      = fromIntegral (cpRadius params) ∷ Float
        depth       = fromIntegral (cpDepth params) ∷ Float
        rimHeight   = fromIntegral (cpRimHeight params) ∷ Float
        ejectaR     = fromIntegral (cpEjectaRadius params) ∷ Float

    in if dist > ejectaR
       then noModification

       else if dist > radius
       -- Ejecta blanket: pure uplift, no new material
       then let t = (dist - radius) / (ejectaR - radius)
                t' = 1.0 - smoothstepGeo t
                elevDelta = round (rimHeight * t' * 0.5)
            in GeoModification elevDelta Nothing 0

       else if dist > radius * 0.9
       -- Rim zone: impactite, full intrusion (impact melt)
       then let t = (dist - radius * 0.9) / (radius * 0.1)
                t' = smoothstepGeo t
                elevDelta = round (rimHeight * t')
            in GeoModification elevDelta (Just (unMaterialId matImpactite)) (abs elevDelta)

       else if dist > radius * 0.15
       -- Bowl zone: depression, impactite surface, no intrusion
       then let t = (dist - radius * 0.15) / (radius * 0.75)
                t' = smoothstepGeo t
                elevDelta = round (negate depth * (1.0 - t'))
            in GeoModification elevDelta (Just (unMaterialId matImpactite)) 0

       else
       -- Central floor: depression, meteorite or impactite
       let elevDelta = round (negate depth)
           centerDist = dist / (radius * 0.15)
           mat = case cpMeteorite params of
               Just meteoriteMat | centerDist < 0.5 → Just meteoriteMat
               _ → Just (unMaterialId matImpactite)
       in GeoModification elevDelta mat 0
