{-# LANGUAGE Strict #-}
module World.Geology
    ( -- * Timeline construction
      buildTimeline
      -- * Event application (called from generateChunk)
    , applyGeoEvent
    , applyErosion
      -- * Crater
    , applyCrater
    , generateCraters
      -- * Types re-export
    , GeoModification(..)
    ) where

import UPrelude
import Data.Bits (xor, shiftR, (.&.))
import Data.Word (Word32, Word64)
import World.Types
import World.Material
import World.Plate (isBeyondGlacier, wrapGlobalX, elevationAtGlobal
                   , generatePlates, TectonicPlate)

-----------------------------------------------------------
-- Timeline Build State & Helpers
-----------------------------------------------------------

-- | State threaded through timeline construction.
data TimelineBuildState = TimelineBuildState
    { tbsFeatures   :: ![PersistentFeature]
    , tbsNextId     :: !Int
    , tbsPeriods    :: ![GeoPeriod]       -- ^ Accumulated in reverse
    , tbsPeriodIdx  :: !Int
    }

-- | Helper to allocate a new feature ID.
allocFeatureId :: TimelineBuildState -> (GeoFeatureId, TimelineBuildState)
allocFeatureId tbs =
    let fid = GeoFeatureId (tbsNextId tbs)
    in (fid, tbs { tbsNextId = tbsNextId tbs + 1 })

-- | Helper to add a period and advance the index.
addPeriod :: GeoPeriod -> TimelineBuildState -> TimelineBuildState
addPeriod period tbs = tbs
    { tbsPeriods   = period : tbsPeriods tbs
    , tbsPeriodIdx = tbsPeriodIdx tbs + 1
    }

-- | Helper to register a new persistent feature.
registerFeature :: PersistentFeature -> TimelineBuildState -> TimelineBuildState
registerFeature pf tbs = tbs
    { tbsFeatures = pf : tbsFeatures tbs
    }

-- | Helper to update an existing feature's state.
updateFeature :: GeoFeatureId -> (PersistentFeature -> PersistentFeature)
              -> TimelineBuildState -> TimelineBuildState
updateFeature fid f tbs = tbs
    { tbsFeatures = map (\pf -> if pfId pf == fid then f pf else pf)
                        (tbsFeatures tbs)
    }

-----------------------------------------------------------
-- Volcanic Evolution
-----------------------------------------------------------

-- | Evolve a single persistent feature between geological periods.
--   Decides whether it stays active, goes dormant, collapses,
--   spawns parasitic cones, etc.
evolveOneFeature :: Word64 -> Int
                 -> ([GeoEvent], TimelineBuildState)
                 -> PersistentFeature
                 -> ([GeoEvent], TimelineBuildState)
evolveOneFeature seed periodIdx (events, tbs) pf =
    let fid = pfId pf
        GeoFeatureId fidInt = fid
        h1 = hashGeo seed fidInt 40
        roll = hashToFloatGeo h1
    in case pfActivity pf of
        Active ->
            if roll < 0.2
            -- 20%: collapse into caldera
            then let h2 = hashGeo seed fidInt 41
                     h3 = hashGeo seed fidInt 42
                     depth = hashToRangeGeo h2 50 200
                     ratio = 0.3 + hashToFloatGeo h3 * 0.5
                     evt = VolcanicModify fid (CollapseToCaldera depth ratio)
                     tbs' = updateFeature fid
                         (\p -> p { pfActivity = Collapsed
                                  , pfLastActivePeriod = periodIdx }) tbs
                 in (evt : events, tbs')

            else if roll < 0.5
            -- 30%: go dormant
            then let evt = VolcanicModify fid GoDormant
                     tbs' = updateFeature fid
                         (\p -> p { pfActivity = Dormant
                                  , pfLastActivePeriod = periodIdx }) tbs
                 in (evt : events, tbs')

            else if roll < 0.7
            -- 20%: parasitic eruption (spawn cinder cone on flank)
            then let (childId, tbs') = allocFeatureId tbs
                     h3 = hashGeo seed fidInt 43
                     h4 = hashGeo seed fidInt 44
                     -- Place child on the flank of the parent
                     parentCenter = getFeatureCenter (pfFeature pf)
                     GeoCoord px py = parentCenter
                     angle = hashToFloatGeo h3 * 2.0 * 3.14159
                     parentRadius = getFeatureRadius (pfFeature pf)
                     dist = fromIntegral parentRadius * (0.5 + hashToFloatGeo h4 * 0.4)
                     childX = px + round (dist * cos angle)
                     childY = py + round (dist * sin angle)
                     childFeature = CinderCone CinderConeParams
                         { ccCenter       = GeoCoord childX childY
                         , ccBaseRadius   = hashToRangeGeo (hashGeo seed fidInt 45) 5 12
                         , ccPeakHeight   = hashToRangeGeo (hashGeo seed fidInt 46) 50 150
                         , ccCraterRadius = hashToRangeGeo (hashGeo seed fidInt 47) 2 5
                         , ccCraterDepth  = hashToRangeGeo (hashGeo seed fidInt 48) 10 40
                         }
                     childPf = PersistentFeature
                         { pfId               = childId
                         , pfFeature          = childFeature
                         , pfActivity         = Active
                         , pfFormationPeriod   = periodIdx
                         , pfLastActivePeriod  = periodIdx
                         , pfEruptionCount     = 1
                         , pfParentId          = Just fid
                         }
                     evt = VolcanicModify fid (ParasiticEruption childFeature childId)
                     tbs'' = registerFeature childPf tbs'
                     tbs''' = updateFeature fid
                         (\p -> p { pfEruptionCount = pfEruptionCount p + 1 }) tbs''
                 in (evt : events, tbs''')

            else
            -- 30%: stays active, grows taller
            let h5 = hashGeo seed fidInt 49
                heightGain = hashToRangeGeo h5 20 100
                evt = VolcanicModify fid (Reactivate heightGain 0)
                tbs' = updateFeature fid
                    (\p -> p { pfEruptionCount = pfEruptionCount p + 1
                             , pfLastActivePeriod = periodIdx }) tbs
            in (evt : events, tbs')

        Dormant ->
            if roll < 0.3
            -- 30%: reactivate
            then let h5 = hashGeo seed fidInt 50
                     h6 = hashGeo seed fidInt 51
                     heightGain = hashToRangeGeo h5 30 150
                     lavaExt = hashToRangeGeo h6 5 20
                     evt = VolcanicModify fid (Reactivate heightGain lavaExt)
                     tbs' = updateFeature fid
                         (\p -> p { pfActivity = Active
                                  , pfEruptionCount = pfEruptionCount p + 1
                                  , pfLastActivePeriod = periodIdx }) tbs
                 in (evt : events, tbs')

            else if roll < 0.5
            -- 20%: go extinct
            then let evt = VolcanicModify fid GoExtinct
                     tbs' = updateFeature fid
                         (\p -> p { pfActivity = Extinct }) tbs
                 in (evt : events, tbs')

            else
            -- 50%: stays dormant, no event emitted
            (events, tbs)

        Extinct   -> (events, tbs)
        Collapsed -> (events, tbs)

-----------------------------------------------------------
-- Feature Query Helpers
-----------------------------------------------------------

-- | Get center coordinates from any volcanic feature.
getFeatureCenter :: VolcanicFeature -> GeoCoord
getFeatureCenter (ShieldVolcano p)    = shCenter p
getFeatureCenter (CinderCone p)       = ccCenter p
getFeatureCenter (LavaDome p)         = ldCenter p
getFeatureCenter (Caldera p)          = caCenter p
getFeatureCenter (FissureVolcano p)   = fpStart p
getFeatureCenter (LavaTube p)         = ltStart p
getFeatureCenter (SuperVolcano p)     = svCenter p
getFeatureCenter (HydrothermalVent p) = htCenter p

-- | Get approximate radius from any volcanic feature.
getFeatureRadius :: VolcanicFeature -> Int
getFeatureRadius (ShieldVolcano p)    = shBaseRadius p
getFeatureRadius (CinderCone p)       = ccBaseRadius p
getFeatureRadius (LavaDome p)         = ldBaseRadius p
getFeatureRadius (Caldera p)          = caOuterRadius p
getFeatureRadius (FissureVolcano _)   = 50
getFeatureRadius (LavaTube _)         = 30
getFeatureRadius (SuperVolcano p)     = svCalderaRadius p
getFeatureRadius (HydrothermalVent p) = htRadius p

buildTimeline :: Word64 -> Int -> Int -> GeoTimeline
buildTimeline seed worldSize plateCount =
    let plates = generatePlates seed worldSize plateCount

        initialState = TimelineBuildState
            { tbsFeatures  = []
            , tbsNextId    = 0
            , tbsPeriods   = []
            , tbsPeriodIdx = 0
            }

        s1 = buildPrimordialBombardment seed worldSize plates initialState
        s2 = buildLateBombardment seed worldSize plates s1
        s3 = buildEarlyVolcanism seed worldSize plates s2
        s4 = buildVolcanicEvolution seed worldSize plates s3
        s5 = buildLateVolcanism seed worldSize plates s4
        s6 = buildStabilization seed s5

    in GeoTimeline
        { gtSeed      = seed
        , gtWorldSize = worldSize
        , gtPeriods   = reverse (tbsPeriods s6)
        , gtFeatures  = tbsFeatures s6
        }

-- Hex literals for xor seeds (spelled with digits)
-- 0xCRATER → use a real hex constant
-- Using plain numeric constants instead:
_craterXor :: Word64
_craterXor = 0x43524154

-----------------------------------------------------------
-- Crater Generation
-----------------------------------------------------------

-- | Which era of bombardment — controls crater count and size.
data CraterEra
    = CraterEra_Primordial   -- ^ Few but huge
    | CraterEra_Late         -- ^ More but smaller
    deriving (Show, Eq)

-- | Generate a set of crater events for a bombardment era.
--   Picks random land positions, avoids ocean and glacier,
--   determines size and whether a meteorite survives.
generateCraters :: Word64 -> Int -> [TectonicPlate] -> CraterEra -> [CraterParams]
generateCraters seed worldSize plates era =
    let (count, minRadius, maxRadius, minDepth, maxDepth, rimMin, rimMax) = case era of
            CraterEra_Primordial -> (3,  40, 120, 30, 80, 10, 30)
            CraterEra_Late       -> (8,  10, 50,  8,  40, 3,  15)

        halfTiles = (worldSize * 16) `div` 2

        attempts = map (generateCraterAttempt seed worldSize plates
                            halfTiles minRadius maxRadius
                            minDepth maxDepth rimMin rimMax) [0 .. count * 3 - 1]
        -- Take up to 'count' successful placements
        -- (some attempts will land in ocean/glacier and be rejected)
        valid = take count [ cp | Just cp <- attempts ]

    in valid

-- | Try to place a single crater. Returns Nothing if it
--   lands in ocean or glacier.
generateCraterAttempt :: Word64 -> Int -> [TectonicPlate]
                      -> Int -> Int -> Int -> Int -> Int -> Int -> Int
                      -> Int -> Maybe CraterParams
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

        gx = hashToRangeGeo h1 (-halfTiles) (halfTiles - 1)
        gy = hashToRangeGeo h2 (-halfTiles) (halfTiles - 1)

        -- Check that we're on land
        (elev, _mat) = elevationAtGlobal seed plates worldSize gx gy

        -- Reject ocean (negative elevation) and glacier
        isOcean   = elev < -100
        isGlacier = isBeyondGlacier worldSize gx gy

        radius      = hashToRangeGeo h3 minRadius maxRadius
        depth       = hashToRangeGeo h4 minDepth maxDepth
        rimHeight   = hashToRangeGeo h5 rimMin rimMax
        ejectaScale = hashToFloatGeo h6

        -- Ejecta extends 1.5-2.5x the crater radius
        ejectaRadius = radius + round (fromIntegral radius * (0.5 + ejectaScale))

        -- Meteorite survives in larger impacts
        meteoriteType = determineMeteoriteType seed attemptIdx radius

    in if isOcean || isGlacier
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
determineMeteoriteType :: Word64 -> Int -> Int -> Maybe Word8
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
                0 -> unMaterialId matIron      -- Iron meteorite
                1 -> unMaterialId matOlivine   -- Stony (olivine-rich)
                2 -> unMaterialId matPyroxene  -- Stony (pyroxene-rich)
                _ -> unMaterialId matFeldspar  -- Stony (feldspar-rich)

-----------------------------------------------------------
-- Event Application
-----------------------------------------------------------

-- | Apply a single geological event to a tile position.
--   Returns the elevation and material modification.
--   Called per-tile during chunk generation.
data GeoModification = GeoModification
    { gmElevDelta   :: !Int         -- ^ Add to elevation
    , gmMaterialOverride :: !(Maybe Word8) -- ^ Replace material if Just
    } deriving (Show)

noModification :: GeoModification
noModification = GeoModification 0 Nothing

-- | Apply any geo event to a position.
applyGeoEvent :: GeoEvent -> Int -> Int -> Int -> Int -> GeoModification
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
-- Feature Evolution Application
-----------------------------------------------------------

applyEvolution :: FeatureEvolution -> Int -> Int -> Int -> Int -> GeoModification
applyEvolution (Reactivate heightGain _lavaExt) _ws _gx _gy _e =
    GeoModification heightGain Nothing
applyEvolution GoDormant _ _ _ _ = noModification
applyEvolution GoExtinct _ _ _ _ = noModification
applyEvolution (CollapseToCaldera depth _ratio) _ws _gx _gy _e =
    GeoModification (negate depth) (Just (unMaterialId matObsidian))
applyEvolution (ParasiticEruption childFeature _childId) ws gx gy e =
    applyVolcanicFeature childFeature ws gx gy e
applyEvolution (FlankCollapse _ _ _) _ _ _ _ = noModification

-----------------------------------------------------------
-- Crater Application
-----------------------------------------------------------

-- | Apply a crater's effect to a single tile position.
--   The crater is a bowl shape: rim at the outer edge,
--   sloping down to the center. Ejecta blanket beyond the rim
--   tapers off with distance.
--
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
applyCrater :: CraterParams -> Int -> Int -> Int -> Int -> GeoModification
applyCrater params worldSize gx gy _baseElev =
    let GeoCoord cx cy = cpCenter params
        -- Wrapped distance in X for cylindrical world
        dx = fromIntegral (wrappedDeltaXGeo worldSize gx cx) :: Float
        dy = fromIntegral (gy - cy) :: Float
        dist = sqrt (dx * dx + dy * dy)

        radius      = fromIntegral (cpRadius params) :: Float
        depth       = fromIntegral (cpDepth params) :: Float
        rimHeight   = fromIntegral (cpRimHeight params) :: Float
        ejectaR     = fromIntegral (cpEjectaRadius params) :: Float

    in if dist > ejectaR
       -- Outside ejecta radius — no effect
       then noModification

       else if dist > radius
       -- Ejecta blanket zone: rim to ejecta edge
       -- Tapers from rimHeight at the rim to 0 at ejecta edge
       then let t = (dist - radius) / (ejectaR - radius)
                t' = 1.0 - smoothstepGeo t  -- 1.0 at rim, 0.0 at edge
                elevDelta = round (rimHeight * t' * 0.5)
            in GeoModification elevDelta (Just (unMaterialId matImpactite))

       else if dist > radius * 0.9
       -- Rim zone: the raised lip of the crater
       -- Peaks at the rim edge, drops off slightly inward
       then let t = (dist - radius * 0.9) / (radius * 0.1)
                t' = smoothstepGeo t  -- 0.0 at inner rim, 1.0 at rim edge
                elevDelta = round (rimHeight * t')
            in GeoModification elevDelta (Just (unMaterialId matImpactite))

       else if dist > radius * 0.15
       -- Bowl zone: slopes from rim down to floor
       -- Smooth curve from 0 at inner rim to -depth at center region
       then let t = (dist - radius * 0.15) / (radius * 0.75)
                -- t: 0.0 at the flat floor edge, 1.0 at inner rim
                t' = smoothstepGeo t
                elevDelta = round (negate depth * (1.0 - t'))
            in GeoModification elevDelta (Just (unMaterialId matImpactite))

       else
       -- Central floor: flat depression, possible meteorite
       -- Check for meteorite deposit at the very center
       let elevDelta = round (negate depth)
           centerDist = dist / (radius * 0.15)
           mat = case cpMeteorite params of
               Just meteoriteMat | centerDist < 0.5 -> Just meteoriteMat
               _ -> Just (unMaterialId matImpactite)
       in GeoModification elevDelta mat

-----------------------------------------------------------
-- Erosion Application (stub for now)
-----------------------------------------------------------

-- | Apply erosion to a tile position.
--   Takes the post-event elevation and returns a modification.
applyErosion :: ErosionParams -> Int -> Int -> Int -> Int -> GeoModification
applyErosion _params _worldSize _gx _gy _elev = noModification

-----------------------------------------------------------
-- Hash Utilities (local to this module)
-----------------------------------------------------------

-- | Hash for geology — takes seed, index, and property.
hashGeo :: Word64 -> Int -> Int -> Word32
hashGeo seed idx prop =
    let h0 = fromIntegral seed :: Word64
        h1 = h0 `xor` (fromIntegral idx * 0x517cc1b727220a95)
        h2 = h1 `xor` (fromIntegral prop * 0x6c62272e07bb0142)
        h3 = h2 `xor` (h2 `shiftR` 33)
        h4 = h3 * 0xff51afd7ed558ccd
        h5 = h4 `xor` (h4 `shiftR` 33)
        h6 = h5 * 0xc4ceb9fe1a85ec53
        h7 = h6 `xor` (h6 `shiftR` 33)
    in fromIntegral (h7 .&. 0xFFFFFFFF)

hashToFloatGeo :: Word32 -> Float
hashToFloatGeo h = fromIntegral (h .&. 0x00FFFFFF)
                 / fromIntegral (0x00FFFFFF :: Word32)

hashToRangeGeo :: Word32 -> Int -> Int -> Int
hashToRangeGeo h lo hi =
    let f = hashToFloatGeo h
        span' = hi - lo + 1
    in lo + floor (f * fromIntegral span')

smoothstepGeo :: Float -> Float
smoothstepGeo t = t * t * (3.0 - 2.0 * t)

-- | Wrapped X distance for cylindrical world.
wrappedDeltaXGeo :: Int -> Int -> Int -> Int
wrappedDeltaXGeo worldSize x1 x2 =
    let w = worldSize * 16
        raw = x2 - x1
        halfW = w `div` 2
    in ((raw + halfW) `mod` w + w) `mod` w - halfW

-----------------------------------------------------------
-- Volcano Generation
-----------------------------------------------------------

data VolcanoEra
    = VolcanoEra_Boundary    -- ^ Tied to convergent boundaries
    | VolcanoEra_Hotspot     -- ^ Random placement on land
    deriving (Show, Eq)

-- | Generate volcanoes for a geological period.
generateVolcanoes :: Word64 -> Int -> [TectonicPlate] -> VolcanoEra -> [VolcanoParams]
generateVolcanoes seed worldSize plates era =
    let (count, minBaseR, maxBaseR, minHeight, maxHeight) = case era of
            VolcanoEra_Boundary -> (6,  20, 60, 200, 800)
            VolcanoEra_Hotspot  -> (4,  15, 45, 150, 600)

        halfTiles = (worldSize * 16) `div` 2

        attempts = map (generateVolcanoAttempt seed worldSize plates
                            halfTiles era minBaseR maxBaseR
                            minHeight maxHeight) [0 .. count * 4 - 1]
        valid = take count [ vp | Just vp <- attempts ]
    in valid

generateVolcanoAttempt :: Word64 -> Int -> [TectonicPlate]
                       -> Int -> VolcanoEra -> Int -> Int -> Int -> Int
                       -> Int -> Maybe VolcanoParams
generateVolcanoAttempt seed worldSize plates halfTiles era
                       minBaseR maxBaseR minHeight maxHeight attemptIdx =
    let h1 = hashGeo seed attemptIdx 20
        h2 = hashGeo seed attemptIdx 21
        h3 = hashGeo seed attemptIdx 22
        h4 = hashGeo seed attemptIdx 23
        h5 = hashGeo seed attemptIdx 24
        h6 = hashGeo seed attemptIdx 25

        gx = hashToRangeGeo h1 (-halfTiles) (halfTiles - 1)
        gy = hashToRangeGeo h2 (-halfTiles) (halfTiles - 1)

        (elev, _mat) = elevationAtGlobal seed plates worldSize gx gy

        isOcean   = elev < -100
        isGlacier = isBeyondGlacier worldSize gx gy

        -- For boundary volcanoes, require being on land
        -- For hotspot, also require land (ocean hotspots are a future feature)
        validPlacement = not isOcean && not isGlacier

        baseRadius  = hashToRangeGeo h3 minBaseR maxBaseR
        peakHeight  = hashToRangeGeo h4 minHeight maxHeight
        hasErupted  = hashToFloatGeo h5 > 0.4  -- 60% have erupted
        craterRatio = hashToFloatGeo h6

        craterRadius = if hasErupted
                       then max 3 (round (fromIntegral baseRadius * (0.1 + craterRatio * 0.15)))
                       else 0
        craterDepth  = if hasErupted
                       then round (fromIntegral peakHeight * (0.2 + craterRatio * 0.3))
                       else 0

        -- Lava flows extend beyond the base for erupted volcanoes
        lavaRadius = if hasErupted
                     then baseRadius + round (fromIntegral baseRadius * (0.3 + craterRatio * 0.4))
                     else 0

    in if validPlacement
       then Just VolcanoParams
            { vpCenter       = GeoCoord gx gy
            , vpBaseRadius   = baseRadius
            , vpPeakHeight   = peakHeight
            , vpCraterRadius = craterRadius
            , vpCraterDepth  = craterDepth
            , vpMaterial     = unMaterialId matBasalt
            , vpPeakMaterial = unMaterialId matObsidian
            , vpHasErupted   = hasErupted
            , vpLavaRadius   = lavaRadius
            , vpLavaMaterial = unMaterialId matBasalt
            }
       else Nothing

-----------------------------------------------------------
-- Volcano Application
-----------------------------------------------------------

-- | Apply a volcano's effect to a single tile position.
--
--   Profile (cross section):
--
--              /\          <- peak (or caldera rim)
--             /  \
--            / __ \        <- caldera floor (if erupted)
--           /|    |\
--          /.|    |.\      <- concave flanks (steeper near top)
--         /..|    |..\
--        /...|    |...\
--   ____/....|    |....\____   <- lava apron (if erupted)
--
applyVolcano :: VolcanoParams -> Int -> Int -> Int -> Int -> GeoModification
applyVolcano params worldSize gx gy _baseElev =
    let GeoCoord cx cy = vpCenter params
        dx = fromIntegral (wrappedDeltaXGeo worldSize gx cx) :: Float
        dy = fromIntegral (gy - cy) :: Float
        dist = sqrt (dx * dx + dy * dy)

        baseR    = fromIntegral (vpBaseRadius params) :: Float
        peakH    = fromIntegral (vpPeakHeight params) :: Float
        craterR  = fromIntegral (vpCraterRadius params) :: Float
        craterD  = fromIntegral (vpCraterDepth params) :: Float
        lavaR    = fromIntegral (vpLavaRadius params) :: Float

    in if dist > lavaR && dist > baseR
       -- Outside all influence
       then noModification

       else if dist > baseR
       -- Lava apron zone: thin deposit beyond the base
       -- Only present if erupted
       then if vpHasErupted params
            then let t = (dist - baseR) / (lavaR - baseR)
                     t' = 1.0 - smoothstepGeo t
                     -- Thin lava deposit, thicker near base
                     elevDelta = round (peakH * 0.05 * t')
                 in GeoModification elevDelta (Just (vpLavaMaterial params))
            else noModification

       else if dist > craterR || craterR < 1.0
       -- Flank zone: conical rise from base to peak
       -- Using a concave profile (square root) — steep near top, gentle at base
       then let t = 1.0 - (dist / baseR)  -- 0.0 at base, 1.0 at center
                -- Concave profile: sqrt gives steeper flanks near peak
                profile = sqrt t
                elevDelta = round (peakH * profile)
                -- Obsidian near the peak, basalt on lower flanks
                mat = if t > 0.85
                      then vpPeakMaterial params
                      else vpMaterial params
            in GeoModification elevDelta (Just mat)

       else
       -- Caldera zone (only if erupted and craterR > 0)
       let t = dist / craterR  -- 0.0 at center, 1.0 at rim edge
           -- Caldera floor: bowl shape inside the crater
           -- Rim is at full peak height, floor drops by craterDepth
           rimHeight = peakH
           floorHeight = peakH - craterD
           -- Smooth bowl: deeper at center, rising to rim
           bowlProfile = smoothstepGeo t
           elevDelta = round (floorHeight + (rimHeight - floorHeight) * bowlProfile)
           mat = vpPeakMaterial params
       in GeoModification elevDelta (Just mat)

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

-----------------------------------------------------------
-- Feature Generation Helpers
-----------------------------------------------------------

-- | Generate and register a batch of volcanic features.
--   Returns the list of new PersistentFeatures and updated state.
generateAndRegister :: Word64 -> Int -> [TectonicPlate]
                    -> VolcanoEra
                    -> (Word64 -> Int -> [TectonicPlate] -> Int -> Int
                        -> Maybe VolcanicFeature)
                    -> Int  -- ^ period index
                    -> TimelineBuildState
                    -> ([PersistentFeature], TimelineBuildState)
generateAndRegister seed worldSize plates _era mkFeature periodIdx tbs0 =
    let halfTiles = (worldSize * 16) `div` 2
        maxAttempts = 40 :: Int
        maxFeatures = 8 :: Int

        go attemptIdx count tbs acc
            | attemptIdx >= maxAttempts = (acc, tbs)
            | count >= maxFeatures     = (acc, tbs)
            | otherwise =
                let h1 = hashGeo seed attemptIdx 70
                    h2 = hashGeo seed attemptIdx 71
                    gx = hashToRangeGeo h1 (-halfTiles) (halfTiles - 1)
                    gy = hashToRangeGeo h2 (-halfTiles) (halfTiles - 1)
                in case mkFeature seed worldSize plates gx gy of
                    Nothing -> go (attemptIdx + 1) count tbs acc
                    Just feature ->
                        let (fid, tbs') = allocFeatureId tbs
                            pf = PersistentFeature
                                { pfId               = fid
                                , pfFeature          = feature
                                , pfActivity         = Active
                                , pfFormationPeriod   = periodIdx
                                , pfLastActivePeriod  = periodIdx
                                , pfEruptionCount     = 1
                                , pfParentId          = Nothing
                                }
                            tbs'' = registerFeature pf tbs'
                        in go (attemptIdx + 1) (count + 1) tbs'' (pf : acc)

    in go 0 0 tbs0 []

-----------------------------------------------------------
-- Feature Constructors (called by generateAndRegister)
-----------------------------------------------------------

-- | Try to place a shield volcano. Requires land.
generateShieldVolcano :: Word64 -> Int -> [TectonicPlate]
                      -> Int -> Int -> Maybe VolcanicFeature
generateShieldVolcano seed worldSize plates gx gy =
    let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
    in if elev < -100 || isBeyondGlacier worldSize gx gy
       then Nothing
       else let h1 = hashGeo seed gx 80
                h2 = hashGeo seed gy 81
                h3 = hashGeo seed (gx + gy) 82
                baseR  = hashToRangeGeo h1 60 120
                peakH  = hashToRangeGeo h2 100 400
                hasPit = hashToFloatGeo h3 > 0.5
                pitR   = if hasPit then hashToRangeGeo (hashGeo seed gx 83) 3 8 else 0
                pitD   = if hasPit then hashToRangeGeo (hashGeo seed gy 84) 20 60 else 0
            in Just $ ShieldVolcano ShieldParams
                { shCenter     = GeoCoord gx gy
                , shBaseRadius = baseR
                , shPeakHeight = peakH
                , shSummitPit  = hasPit
                , shPitRadius  = pitR
                , shPitDepth   = pitD
                }

-- | Try to place a cinder cone. Requires land.
generateCinderCone :: Word64 -> Int -> [TectonicPlate]
                   -> Int -> Int -> Maybe VolcanicFeature
generateCinderCone seed worldSize plates gx gy =
    let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
    in if elev < -100 || isBeyondGlacier worldSize gx gy
       then Nothing
       else let h1 = hashGeo seed gx 90
                h2 = hashGeo seed gy 91
                h3 = hashGeo seed (gx + gy) 92
                h4 = hashGeo seed (gx * gy) 93
                baseR   = hashToRangeGeo h1 5 15
                peakH   = hashToRangeGeo h2 50 200
                craterR = hashToRangeGeo h3 2 (max 3 (baseR `div` 3))
                craterD = hashToRangeGeo h4 10 (max 15 (peakH `div` 3))
            in Just $ CinderCone CinderConeParams
                { ccCenter      = GeoCoord gx gy
                , ccBaseRadius  = baseR
                , ccPeakHeight  = peakH
                , ccCraterRadius = craterR
                , ccCraterDepth  = craterD
                }

-- | Try to place a lava dome. Requires land, prefers convergent boundaries.
generateLavaDome :: Word64 -> Int -> [TectonicPlate]
                 -> Int -> Int -> Maybe VolcanicFeature
generateLavaDome seed worldSize plates gx gy =
    let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
    in if elev < -100 || isBeyondGlacier worldSize gx gy
       then Nothing
       else let h1 = hashGeo seed gx 100
                h2 = hashGeo seed gy 101
                baseR = hashToRangeGeo h1 10 25
                height = hashToRangeGeo h2 50 150
            in Just $ LavaDome LavaDomeParams
                { ldCenter     = GeoCoord gx gy
                , ldBaseRadius = baseR
                , ldHeight     = height
                }

-- | Try to place a caldera. Requires land.
generateCaldera :: Word64 -> Int -> [TectonicPlate]
                -> Int -> Int -> Maybe VolcanicFeature
generateCaldera seed worldSize plates gx gy =
    let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
    in if elev < -100 || isBeyondGlacier worldSize gx gy
       then Nothing
       else let h1 = hashGeo seed gx 110
                h2 = hashGeo seed gy 111
                h3 = hashGeo seed (gx + gy) 112
                h4 = hashGeo seed (gx * gy) 113
                h5 = hashGeo seed (abs gx + abs gy) 114
                outerR  = hashToRangeGeo h1 30 80
                innerR  = hashToRangeGeo h2 (outerR `div` 2) (outerR * 3 `div` 4)
                rimH    = hashToRangeGeo h3 30 120
                floorD  = hashToRangeGeo h4 40 150
                hasLake = hashToFloatGeo h5 > 0.6
            in Just $ Caldera CalderaParams
                { caCenter      = GeoCoord gx gy
                , caOuterRadius = outerR
                , caInnerRadius = innerR
                , caRimHeight   = rimH
                , caFloorDepth  = floorD
                , caHasLake     = hasLake
                }

-- | Try to place a fissure. Works on land or ocean divergent boundaries.
generateFissure :: Word64 -> Int -> [TectonicPlate]
                -> Int -> Int -> Maybe VolcanicFeature
generateFissure seed worldSize plates gx gy =
    if isBeyondGlacier worldSize gx gy
    then Nothing
    else let h1 = hashGeo seed gx 120
             h2 = hashGeo seed gy 121
             h3 = hashGeo seed (gx + gy) 122
             h4 = hashGeo seed (gx * gy) 123
             h5 = hashGeo seed (abs gx) 124
             -- Fissure direction: random angle
             angle = hashToFloatGeo h1 * 2.0 * pi
             fissureLen = hashToRangeGeo h2 80 200
             halfLen = fromIntegral fissureLen / 2.0 :: Float
             ex = gx + round (halfLen * cos angle)
             ey = gy + round (halfLen * sin angle)
             sxCoord = gx - round (halfLen * cos angle)
             syCoord = gy - round (halfLen * sin angle)
             width   = hashToRangeGeo h3 5 10
             ridgeH  = hashToRangeGeo h4 20 80
             hasMagma = hashToFloatGeo h5 > 0.5
         in Just $ FissureVolcano FissureParams
             { fpStart       = GeoCoord sxCoord syCoord
             , fpEnd         = GeoCoord ex ey
             , fpWidth       = width
             , fpRidgeHeight = ridgeH
             , fpHasMagma    = hasMagma
             }

-- | Try to place a lava tube. Should be on the flank of an existing
--   shield volcano, but can also be standalone on basaltic terrain.
generateLavaTube :: Word64 -> Int -> [TectonicPlate]
                 -> Int -> Int -> Maybe VolcanicFeature
generateLavaTube seed worldSize plates gx gy =
    let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
    in if elev < -100 || isBeyondGlacier worldSize gx gy
       then Nothing
       else let h1 = hashGeo seed gx 130
                h2 = hashGeo seed gy 131
                h3 = hashGeo seed (gx + gy) 132
                h4 = hashGeo seed (gx * gy) 133
                h5 = hashGeo seed (abs gx) 134
                angle = hashToFloatGeo h1 * 2.0 * pi
                tubeLen = hashToRangeGeo h2 40 120
                halfLen = fromIntegral tubeLen / 2.0 :: Float
                ex = gx + round (halfLen * cos angle)
                ey = gy + round (halfLen * sin angle)
                sxCoord = gx - round (halfLen * cos angle)
                syCoord = gy - round (halfLen * sin angle)
                width = hashToRangeGeo h3 3 6
                ridgeH = hashToRangeGeo h4 5 15
                collapses = hashToRangeGeo h5 2 6
            in Just $ LavaTube LavaTubeParams
                { ltStart        = GeoCoord sxCoord syCoord
                , ltEnd          = GeoCoord ex ey
                , ltWidth        = width
                , ltRidgeHeight  = ridgeH
                , ltCollapses    = collapses
                , ltCollapseSeed = fromIntegral (gx * 31 + gy * 17) `xor` seed
                }

-- | Try to place a supervolcano. Very rare. Requires land.
generateSuperVolcano :: Word64 -> Int -> [TectonicPlate]
                     -> Int -> Int -> Maybe VolcanicFeature
generateSuperVolcano seed worldSize plates gx gy =
    let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
    in if elev < -100 || isBeyondGlacier worldSize gx gy
       then Nothing
       else let h1 = hashGeo seed gx 140
                h2 = hashGeo seed gy 141
                h3 = hashGeo seed (gx + gy) 142
                h4 = hashGeo seed (gx * gy) 143
                h5 = hashGeo seed (abs gx) 144
                calderaR = hashToRangeGeo h1 100 200
                rimH     = hashToRangeGeo h2 20 60
                floorD   = hashToRangeGeo h3 80 250
                ejectaR  = hashToRangeGeo h4 250 500
                ejectaD  = hashToRangeGeo h5 5 30
            in Just $ SuperVolcano SuperVolcanoParams
                { svCenter        = GeoCoord gx gy
                , svCalderaRadius = calderaR
                , svRimHeight     = rimH
                , svFloorDepth    = floorD
                , svEjectaRadius  = ejectaR
                , svEjectaDepth   = ejectaD
                }

-- | Try to place a hydrothermal vent. Requires ocean floor.
generateHydrothermalVent :: Word64 -> Int -> [TectonicPlate]
                         -> Int -> Int -> Maybe VolcanicFeature
generateHydrothermalVent seed worldSize plates gx gy =
    let (elev, _) = elevationAtGlobal seed plates worldSize gx gy
    in if elev >= -100 || isBeyondGlacier worldSize gx gy
       then Nothing  -- Must be in deep ocean
       else let h1 = hashGeo seed gx 150
                h2 = hashGeo seed gy 151
                radius   = hashToRangeGeo h1 3 8
                chimneyH = hashToRangeGeo h2 10 30
            in Just $ HydrothermalVent HydrothermalParams
                { htCenter        = GeoCoord gx gy
                , htRadius        = radius
                , htChimneyHeight = chimneyH
                }

-----------------------------------------------------------
-- Timeline Phase Builders
-----------------------------------------------------------

buildPrimordialBombardment :: Word64 -> Int -> [TectonicPlate]
                           -> TimelineBuildState -> TimelineBuildState
buildPrimordialBombardment seed worldSize plates tbs =
    let craterSeed = seed `xor` 0xDEADBEEF
        craters = generateCraters craterSeed worldSize plates CraterEra_Primordial
        period = GeoPeriod
            { gpName     = "Primordial Bombardment"
            , gpScale    = Eon
            , gpDuration = 100
            , gpEvents   = map CraterEvent craters
            , gpErosion  = ErosionParams 0.8 0.3 0.6 0.4 0.1 (seed + 1000)
            }
    in addPeriod period tbs

buildLateBombardment :: Word64 -> Int -> [TectonicPlate]
                     -> TimelineBuildState -> TimelineBuildState
buildLateBombardment seed worldSize plates tbs =
    let craterSeed = (seed `xor` 0xDEADBEEF) + 1
        craters = generateCraters craterSeed worldSize plates CraterEra_Late
        period = GeoPeriod
            { gpName     = "Late Bombardment"
            , gpScale    = Era
            , gpDuration = 50
            , gpEvents   = map CraterEvent craters
            , gpErosion  = ErosionParams 0.6 0.5 0.4 0.2 0.3 (seed + 2000)
            }
    in addPeriod period tbs

buildEarlyVolcanism :: Word64 -> Int -> [TectonicPlate]
                    -> TimelineBuildState -> TimelineBuildState
buildEarlyVolcanism seed worldSize plates tbs0 =
    let volcSeed = seed `xor` 0xB45A1F1C
        periodIdx = tbsPeriodIdx tbs0

        -- Shield volcanoes at hotspots
        (shields, tbs1) = generateAndRegister volcSeed worldSize plates
                              VolcanoEra_Hotspot generateShieldVolcano periodIdx tbs0

        -- Fissures at rift zones
        (fissures, tbs2) = generateAndRegister (volcSeed + 1) worldSize plates
                               VolcanoEra_Boundary generateFissure periodIdx tbs1

        -- Hydrothermal vents on ocean floor
        (vents, tbs3) = generateAndRegister (volcSeed + 2) worldSize plates
                            VolcanoEra_Boundary generateHydrothermalVent periodIdx tbs2

        events = map (\pf -> VolcanicEvent (pfFeature pf)) (shields <> fissures <> vents)

        period = GeoPeriod
            { gpName     = "Early Volcanism"
            , gpScale    = Era
            , gpDuration = 70
            , gpEvents   = events
            , gpErosion  = ErosionParams 0.6 0.6 0.4 0.2 0.3 (seed + 4000)
            }
    in addPeriod period tbs3

buildVolcanicEvolution :: Word64 -> Int -> [TectonicPlate]
                       -> TimelineBuildState -> TimelineBuildState
buildVolcanicEvolution seed _worldSize _plates tbs0 =
    let periodIdx = tbsPeriodIdx tbs0
        evolSeed = seed `xor` 0xEF01F100

        (events, tbs1) = foldl' (evolveOneFeature evolSeed periodIdx)
                                ([], tbs0) (tbsFeatures tbs0)

        period = GeoPeriod
            { gpName     = "Volcanic Evolution"
            , gpScale    = Period
            , gpDuration = 50
            , gpEvents   = events
            , gpErosion  = ErosionParams 0.5 0.5 0.4 0.2 0.3 (seed + 5000)
            }
    in addPeriod period tbs1

buildLateVolcanism :: Word64 -> Int -> [TectonicPlate]
                   -> TimelineBuildState -> TimelineBuildState
buildLateVolcanism seed worldSize plates tbs0 =
    let lateSeed = seed `xor` 0x1A7EF10D
        periodIdx = tbsPeriodIdx tbs0

        -- Cinder cones (common, small)
        (cinders, tbs1) = generateAndRegister lateSeed worldSize plates
                              VolcanoEra_Boundary generateCinderCone periodIdx tbs0

        -- Lava domes at convergent boundaries
        (domes, tbs2) = generateAndRegister (lateSeed + 1) worldSize plates
                            VolcanoEra_Boundary generateLavaDome periodIdx tbs1

        -- Lava tubes (on existing volcanic terrain)
        (tubes, tbs3) = generateAndRegister (lateSeed + 2) worldSize plates
                            VolcanoEra_Hotspot generateLavaTube periodIdx tbs2

        -- Rare: one supervolcano attempt
        (supers, tbs4) = generateAndRegister (lateSeed + 3) worldSize plates
                             VolcanoEra_Hotspot generateSuperVolcano periodIdx tbs3

        -- Calderas from collapsed earlier features
        (calderas, tbs5) = generateAndRegister (lateSeed + 4) worldSize plates
                               VolcanoEra_Hotspot generateCaldera periodIdx tbs4

        allNew = cinders <> domes <> tubes <> supers <> calderas
        events = map (\pf -> VolcanicEvent (pfFeature pf)) allNew

        period = GeoPeriod
            { gpName     = "Late Volcanism"
            , gpScale    = Period
            , gpDuration = 40
            , gpEvents   = events
            , gpErosion  = ErosionParams 0.3 0.4 0.3 0.2 0.2 (seed + 6000)
            }
    in addPeriod period tbs5

buildStabilization :: Word64
                   -> TimelineBuildState -> TimelineBuildState
buildStabilization seed tbs =
    let period = GeoPeriod
            { gpName     = "Crustal Stabilization"
            , gpScale    = Period
            , gpDuration = 80
            , gpEvents   = []
            , gpErosion  = ErosionParams 0.9 0.8 0.3 0.2 0.5 (seed + 7000)
            }
    in addPeriod period tbs
