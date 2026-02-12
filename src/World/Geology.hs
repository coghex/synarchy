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
-- Timeline Construction
-----------------------------------------------------------

-- | Build the full geological timeline from the world seed.
--   Deterministic — same seed always produces the same history.
buildTimeline :: Word64 -> Int -> Int -> GeoTimeline
buildTimeline seed worldSize plateCount =
    let plates = generatePlates seed worldSize plateCount

        -- Sub-seeds for each phase of timeline generation
        craterSeed   = seed `xor` 0xDEADBEEF
        -- erosionSeed  = seed `xor` 0xER051
        -- volcanoSeed  = seed `xor` 0xB45A1

        -- Phase 1: Primordial bombardment (Eon scale)
        --   Large craters on land, sets the gross terrain features
        primordialCraters = generateCraters craterSeed worldSize plates
                                CraterEra_Primordial

        primordial = GeoPeriod
            { gpName     = "Primordial Bombardment"
            , gpScale    = Eon
            , gpDuration = 100
            , gpEvents   = map CraterEvent primordialCraters
            , gpErosion  = ErosionParams
                { epIntensity = 0.8
                , epHydraulic = 0.3
                , epThermal   = 0.6
                , epWind      = 0.4
                , epChemical  = 0.1
                , epSeed      = seed + 1000
                }
            }

        -- Phase 2: Late bombardment (Era scale)
        --   Smaller, fewer craters
        lateCraters = generateCraters (craterSeed + 1) worldSize plates
                          CraterEra_Late

        lateBombardment = GeoPeriod
            { gpName     = "Late Bombardment"
            , gpScale    = Era
            , gpDuration = 50
            , gpEvents   = map CraterEvent lateCraters
            , gpErosion  = ErosionParams
                { epIntensity = 0.6
                , epHydraulic = 0.5
                , epThermal   = 0.4
                , epWind      = 0.2
                , epChemical  = 0.3
                , epSeed      = seed + 2000
                }
            }

        -- Phase 3: Stabilization (Period scale)
        --   No impacts, heavy erosion smooths out the bombardment
        stabilization = GeoPeriod
            { gpName     = "Crustal Stabilization"
            , gpScale    = Period
            , gpDuration = 80
            , gpEvents   = []
            , gpErosion  = ErosionParams
                { epIntensity = 0.9
                , epHydraulic = 0.8
                , epThermal   = 0.3
                , epWind      = 0.2
                , epChemical  = 0.5
                , epSeed      = seed + 3000
                }
            }

    in GeoTimeline
        { gtSeed      = seed
        , gtWorldSize = worldSize
        , gtPeriods   = [primordial, lateBombardment, stabilization]
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
applyGeoEvent (CraterEvent params) worldSize gx gy baseElev =
    applyCrater params worldSize gx gy baseElev
applyGeoEvent (VolcanoEvent _)     _ _ _ _ = noModification -- TODO
applyGeoEvent (LandslideEvent _)   _ _ _ _ = noModification -- TODO
applyGeoEvent (GlaciationEvent _)  _ _ _ _ = noModification -- TODO
applyGeoEvent (FloodEvent _)       _ _ _ _ = noModification -- TODO

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
