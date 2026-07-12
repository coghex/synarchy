{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Weather.Generate
    ( -- * Final climate refinement (see haddock below re: the misleading name)
      initEarlyClimate
      -- * Timeline-internal climate update from ElevGrid
    , updateClimateFromGrid
    , oceanRegionsFromGrid
    ) where

import UPrelude
import qualified Data.HashSet as HS
import World.Weather.Types
import World.Ocean.Types (OceanMap)
import World.Geology.Timeline.Types (GeoTimeline(..), PersistentFeature(..))
import World.Weather.Generate.OceanRegions
    (oceanRegionsFromChunkMap, oceanRegionsFromGrid)
import World.Weather.Generate.Freshwater (extractFreshwaterSources)
import World.Weather.Generate.ClimateBuilder (buildClimateFromOceanSet)

-- * Public: init from chunk-resolution OceanMap (used in Init.hs)

-- | Build the final regional climate grid. Despite the "early" name
--   (a holdover from before the timeline co-evolved climate), this is
--   the LAST climate pass: Init.hs calls it once the timeline, ocean
--   map, and freshwater features are all final, to rebuild the
--   regional grid at full (chunk-resolution ocean, completed
--   freshwater) precision. The caller supplies the timeline's own
--   final CO2 and solar constant so that pass and this one agree on
--   forcing — the regional grid, csGlobalCO2, and csSolarConst all
--   come out of the SAME buildClimateFromOceanSet call, and
--   csGlobalTemp emerges as that grid's own mean rather than being
--   patched in afterward from a different grid (#785).
--
--   Climate regions are indexed in (u, v) space where:
--     u = cx - cy   (east-west, wraps cylindrically)
--     v = cx + cy   (north-south, bounded by glacier)
--
--   This ensures latitude bands run horizontally on screen
--   instead of diagonally along the cx or cy axis.
--
--   ClimateCoord (ru, rv) covers chunk-u in
--     [ru * climateRegionSize - halfChunks .. (ru+1)*climateRegionSize - 1 - halfChunks]
--   and chunk-v in the same range for rv.
initEarlyClimate ∷ Int          -- ^ worldSize (in chunks)
                 → OceanMap     -- ^ which chunks are ocean
                 → GeoTimeline  -- ^ completed timeline (for lake/river moisture)
                 → Float        -- ^ final CO2 (from completed timeline evolution)
                 → Float        -- ^ final solar constant (from completed timeline)
                 → ClimateState
initEarlyClimate worldSize oceanMap timeline finalCO2 finalSolarConst =
    let oceanSet = oceanRegionsFromChunkMap worldSize oceanMap

        -- Extract freshwater moisture sources (lakes, rivers) from the
        -- completed timeline. These act as secondary moisture sources
        -- that create green corridors through dry continental interiors.
        freshwater = extractFreshwaterSources worldSize (gtFeatures timeline)

    -- globalTempOffset is 0.0 for the same reason updateClimateFromGrid
    -- below passes 0.0: csGlobalTemp is a computed *mean*, not an
    -- offset, so reapplying a previously-derived mean here would
    -- double-count it instead of letting it emerge fresh from this
    -- grid.
    in buildClimateFromOceanSet worldSize oceanSet freshwater
        finalCO2 0.0 finalSolarConst

-- * Public: lightweight climate update during timeline

-- | Update climate from the current ElevGrid's ocean distribution.
--   Called at each Age boundary inside buildTimeline.
--
--   Carries forward CO2 (driver of temperature evolution) and solar
--   constant. Temperature is *recomputed* from CO2 + ocean shape
--   each call; we deliberately pass 0.0 as the globalTempOffset
--   parameter rather than threading csGlobalTemp back through —
--   csGlobalTemp stores the computed *mean* temperature, not an
--   offset, so feeding it back in would double-count and drift up
--   exponentially with call frequency. The latent bug only mattered
--   at low (Era) cadence; per-Age updates surface it sharply.
updateClimateFromGrid ∷ Int                        -- ^ worldSize
                      → HS.HashSet ClimateCoord    -- ^ coarse ocean regions
                      → [PersistentFeature]        -- ^ current persistent features
                      → ClimateState               -- ^ previous climate state
                      → ClimateState
updateClimateFromGrid worldSize coarseOcean features prevClimate =
    let freshwater = extractFreshwaterSources worldSize features
    in buildClimateFromOceanSet worldSize coarseOcean freshwater
        (csGlobalCO2  prevClimate)
        0.0
        (csSolarConst prevClimate)
