{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Magma.Kit
    ( perTypeKit
    , hotspotBoostFor
    , hashRange
    , TerrainSampler
    ) where

import UPrelude
import World.Base (GeoCoord(..))
import World.Geology.Timeline.Types
    ( VolcanicFeature(..)
    , ShieldParams(..)
    , CinderConeParams(..)
    , LavaDomeParams(..)
    , CalderaParams(..)
    , FissureParams(..)
    , SuperVolcanoParams(..)
    , HydrothermalParams(..)
    )
import World.Magma.Types (LavaShape(..))

-- | Surface elevation lookup, supplied by the caller in
--   'World.Magma.Init.buildMagmaSource'. Used by kits whose
--   surface placement depends on the local terrain — fissures,
--   currently the only ones (other variants carry their own
--   per-feature @CenterElev@ field).
type TerrainSampler = Int → Int → Int

-- | Hash-derived integer in @[lo, hi]@ used for per-volcano
--   variability (chamber depth, chute width, etc.) within type
--   bounds.
hashRange ∷ Word64 → Int → Int → Int
hashRange seed lo hi
    | hi ≤ lo  = lo
    | otherwise = lo + fromIntegral (seed `mod` fromIntegral (hi - lo + 1))

-- | Per-type kit: maps a volcano variant to its @[LavaShape]@. The
--   per-source seed @s@ is derived from @(msFeatureId, vcSeed)@ at
--   build time so the kit is deterministic but varies per volcano.
--   The mantle ceiling @mZ@ is sampled at the volcano centre.
--   'TerrainSampler' lets variants without a CenterElev field
--   (fissures) look up the surface elevation at their endpoints.
perTypeKit ∷ TerrainSampler → Word64 → Int → VolcanicFeature → [LavaShape]
perTypeKit terrainAt s mZ vf = case vf of
    CinderCone       p → cinderKit       s mZ p
    LavaDome         p → lavaDomeKit     s mZ p
    ShieldVolcano    p → shieldKit       s mZ p
    Caldera          p → calderaKit      s mZ p
    SuperVolcano     p → superKit        s mZ p
    FissureVolcano   p → fissureKit      terrainAt s mZ p
    HydrothermalVent p → hydrothermalKit s mZ p
    -- Lava tubes are horizontal geometry; deferred to a separate phase.
    LavaTube         _ → []

-- * Per-type kits

cinderKit ∷ Word64 → Int → CinderConeParams → [LavaShape]
cinderKit _seed mZ p =
    let GeoCoord cx cy = ccCenter p
        zTop = ccCenterElev p + ccPeakHeight p - ccCraterDepth p
    in [ Cylindrical cx cy mZ zTop 2.0 ]

lavaDomeKit ∷ Word64 → Int → LavaDomeParams → [LavaShape]
lavaDomeKit _seed mZ p =
    let GeoCoord cx cy = ldCenter p
        zTop = ldCenterElev p + ldHeight p
    in [ Cylindrical cx cy mZ zTop 3.0 ]

shieldKit ∷ Word64 → Int → ShieldParams → [LavaShape]
shieldKit seed mZ p =
    let GeoCoord cx cy = shCenter p
        peakZ  = shCenterElev p + shPeakHeight p
        chTopZ = shCenterElev p - hashRange (seed `xor` 1) 10 20
        chBotZ = chTopZ - hashRange (seed `xor` 2) 5 10
        chMidZ = (chTopZ + chBotZ) `div` 2
        chR    = max 2.0 (fromIntegral (shBaseRadius p) / 4.0)
        chHalf = max 1.0 (fromIntegral (chTopZ - chBotZ) / 2.0)
    in [ EllipsoidChamber cx cy chMidZ chR chR chHalf
       , Conical cx cy chTopZ peakZ 4.0 2.0
       , Cylindrical cx cy mZ chBotZ 2.0
       ]

calderaKit ∷ Word64 → Int → CalderaParams → [LavaShape]
calderaKit seed mZ p =
    let GeoCoord cx cy = caCenter p
        floorZ = caCenterElev p - caFloorDepth p
        chTopZ = floorZ - hashRange (seed `xor` 3) 30 60
        chBotZ = chTopZ - hashRange (seed `xor` 4) 20 40
        chMidZ = (chTopZ + chBotZ) `div` 2
        chR    = max 3.0 (fromIntegral (caInnerRadius p) * 0.8)
        chHalf = max 1.0 (fromIntegral (chTopZ - chBotZ) / 2.0)
    in [ EllipsoidChamber cx cy chMidZ chR chR chHalf
       , Cylindrical cx cy chTopZ floorZ 3.0
       , Cylindrical cx cy mZ chBotZ 1.5
       ]

superKit ∷ Word64 → Int → SuperVolcanoParams → [LavaShape]
superKit seed mZ p =
    let GeoCoord cx cy = svCenter p
        floorZ = svCenterElev p - svFloorDepth p
        chMidZ = floorZ - hashRange (seed `xor` 11) 80 120
        chR    = max 30.0 (fromIntegral (svCalderaRadius p) * 1.5)
        chH    = hashRange (seed `xor` 12) 40 60
        chTopZ = chMidZ + chH `div` 2
        chBotZ = chMidZ - chH `div` 2
        hasSurfaceChute = (seed `xor` 13) `mod` 2 ≡ 0
        surfaceChute =
            [ Cylindrical cx cy chTopZ floorZ 2.0 | hasSurfaceChute ]
    in [ IrregularChamber cx cy chMidZ chR (chR * 0.2) 0.05 (seed `xor` 0xC1) ]
       ⧺ surfaceChute
       ⧺ [ Cylindrical cx cy mZ chBotZ 2.0 ]

fissureKit ∷ TerrainSampler → Word64 → Int → FissureParams → [LavaShape]
fissureKit terrainAt _seed mZ p
    | not (fpHasMagma p) = []
    | otherwise =
        let GeoCoord sx sy = fpStart p
            GeoCoord ex ey = fpEnd p
            -- 'fpRidgeHeight' is a HEIGHT relative to the local
            -- surface, not an absolute Z. Sample the endpoints and
            -- place the slot just below the ridge crest. Using both
            -- endpoints (averaged) keeps short fissures stable when
            -- the surface elevation varies along their length.
            e1 = terrainAt sx sy
            e2 = terrainAt ex ey
            surfaceZ = (e1 + e2) `div` 2
            zTop = surfaceZ + fpRidgeHeight p
            w    = max 1.0 (fromIntegral (fpWidth p))
        in [ Slot sx sy ex ey mZ zTop w ]

hydrothermalKit ∷ Word64 → Int → HydrothermalParams → [LavaShape]
hydrothermalKit seed mZ p =
    let GeoCoord cx cy = htCenter p
        zTop = htCenterElev p + htChimneyHeight p
        phaseHash = fromIntegral (seed `xor` 0x7F00) ∷ Float
        phase = (phaseHash / 4294967296.0) * 2.0 * 3.14159265
    in [ Perturbed cx cy mZ zTop 2.0 3.0 0.1 phase ]

-- * Hotspot uplift contribution

-- | Per-type additive Gaussian boost to @mantleZ@. Sized so that
--   the bulge is visible (a few tens of metres) but doesn't overwhelm
--   the @mantleBaseDepth@ baseline.
hotspotBoostFor ∷ VolcanicFeature → Float
hotspotBoostFor vf = case vf of
    ShieldVolcano    p → fromIntegral (shBaseRadius p)     * 0.20
    CinderCone       p → fromIntegral (ccBaseRadius p)     * 0.30
    LavaDome         p → fromIntegral (ldBaseRadius p)     * 0.20
    Caldera          p → fromIntegral (caOuterRadius p)    * 0.50
    SuperVolcano     p → fromIntegral (svCalderaRadius p)  * 1.50
    HydrothermalVent p → fromIntegral (htRadius p)         * 0.10
    FissureVolcano   p → fromIntegral (fpWidth p)          * 0.20
    LavaTube         _ → 0.0
