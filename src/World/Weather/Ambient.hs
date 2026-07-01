{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Elevation-corrected ambient air temperature.
--
-- The single source of truth for the atmospheric lapse rate that cools a tile's
-- ambient temperature with altitude. Worldgen's ice / ice-level systems
-- (@World.Fluid.Ice@, @World.Fluid.IceLevel@) and the live unit thermo sim
-- (@scripts/thermo.lua@, via the @world.getAmbientAt@ Lua API) all read the
-- correction from here so the air a unit feels can't disagree with where the
-- world decides ice forms (see issue #308).
--
-- This is the natural seam for a richer ambient model later (day/night and
-- seasonal swing, proximity to lava/fire, shelter insulation): the lapse rate
-- is just the first term. Those inputs would fold into 'ambientTempAt' without
-- reworking the callers.
module World.Weather.Ambient
    ( lapseRate
    , altitudeCooling
    , ambientTempAt
    ) where

import UPrelude
import World.Constants (seaLevel)
import World.Plate (TectonicPlate, elevationAtGlobal, wrapGlobalU)
import World.Weather.Types (ClimateState)
import World.Weather.Lookup (lookupLocalClimate, LocalClimate(..))

-- | Atmospheric lapse rate: °C of cooling per elevation unit above sea level.
lapseRate ∷ Float
lapseRate = 0.065

-- | Cooling (°C, never negative) applied to a tile's ambient from its altitude.
--   Takes a global (sub-sea-clamped) elevation; below sea level there is no
--   cooling. Shared by ice formation, ice-level, and thermo so the lapse rate
--   has exactly one definition.
altitudeCooling ∷ Int → Float
altitudeCooling globalElev =
    fromIntegral (max 0 (globalElev - seaLevel)) * lapseRate

-- | Elevation-corrected ambient air temperature (°C) at a global tile:
--   the regional climate mean minus the altitude lapse. This is what a unit
--   standing on the tile actually feels — a valley floor is warm, the peak
--   directly above it is colder, matching where the world forms ice.
--
--   With no geology (empty plate list — the flat 'World.initArena' test world)
--   there is no elevation to sample, so the lapse term is skipped and the
--   regional mean is returned unchanged. Calling 'elevationAtGlobal' there
--   would error (@twoNearestPlates@: "no plates"); the arena is flat anyway,
--   so the mean IS the correct ambient.
ambientTempAt ∷ Word64 → [TectonicPlate] → ClimateState → Int → Int → Int → Float
ambientTempAt seed plates climate worldSize gx gy =
    let (gx', gy')  = wrapGlobalU worldSize gx gy
        meanT       = lcTemp (lookupLocalClimate climate worldSize gx' gy')
    in if null plates then meanT
       else let (globalElev, _) = elevationAtGlobal seed plates worldSize gx' gy'
            in meanT - altitudeCooling globalElev
