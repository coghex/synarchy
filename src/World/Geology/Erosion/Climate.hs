{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Erosion.Climate
    ( lookupRegionalErosion
    , lerpErosionParams
    , erosionCornerLookup
    ) where

import UPrelude
import World.Types
import qualified Data.HashMap.Strict as HM
import World.Weather.Types (ClimateCoord(..))
import World.Weather.Lookup (RegionGridCoords(..), regionGridCoords)

-- | Look up erosion params for a tile, using regional climate
--   data when available, falling back to the global params.
--
--   Uses bilinear interpolation between the 4 nearest region
--   centers to eliminate hard grid boundaries.
--
--   Climate regions are keyed by (ru, rv) in the rotated
--   (u, v) = (gx - gy, gx + gy) coordinate system, divided
--   into climateRegionSize chunks per side.
{-# INLINE lookupRegionalErosion #-}
lookupRegionalErosion ∷ ErosionParams
                      → HM.HashMap ClimateCoord ErosionParams
                      → Int → Int → Int
                      → ErosionParams
lookupRegionalErosion fallback regMap worldSize gx gy =
    if HM.null regMap
    then fallback
    else let RegionGridCoords ru0 ru1 rv0 rv1 tu tv =
                 regionGridCoords 16 worldSize gx gy
             (ep00, ep10, ep01, ep11) =
                 erosionCornerLookup fallback regMap ru0 ru1 rv0 rv1
         in lerpErosionParams ep00 ep10 ep01 ep11 tu tv

-- | Probe the regional-erosion HashMap for the 4 cells at
--   @(ru0,rv0), (ru1,rv0), (ru0,rv1), (ru1,rv1)@. Pulled out so the
--   per-chunk caller in 'World.Generate.Timeline.applyTimelineChunk'
--   can do the 4 probes ONCE for the whole chunk-period and then
--   just call 'lerpErosionParams' per tile — replaces the per-tile
--   HashMap lookup chain that used to be 55% of init time.
{-# INLINE erosionCornerLookup #-}
erosionCornerLookup
    ∷ ErosionParams                            -- ^ fallback
    → HM.HashMap ClimateCoord ErosionParams
    → Int → Int → Int → Int                    -- ^ ru0 ru1 rv0 rv1
    → (ErosionParams, ErosionParams, ErosionParams, ErosionParams)
erosionCornerLookup fallback regMap ru0 ru1 rv0 rv1 =
    let lookupEP ru rv = HM.lookupDefault fallback (ClimateCoord ru rv) regMap
    in ( lookupEP ru0 rv0
       , lookupEP ru1 rv0
       , lookupEP ru0 rv1
       , lookupEP ru1 rv1
       )

-- | Bilinear interpolation across 4 corner 'ErosionParams' records,
--   using @(tu, tv) ∈ [0, 1]^2@ as the weights inside the 2×2 cell.
{-# INLINE lerpErosionParams #-}
lerpErosionParams
    ∷ ErosionParams                            -- ^ ep00 (low-u, low-v)
    → ErosionParams                            -- ^ ep10 (high-u, low-v)
    → ErosionParams                            -- ^ ep01 (low-u, high-v)
    → ErosionParams                            -- ^ ep11 (high-u, high-v)
    → Float                                    -- ^ tu
    → Float                                    -- ^ tv
    → ErosionParams
lerpErosionParams ep00 ep10 ep01 ep11 tu tv =
    let lerpF a b t = a + t * (b - a)
        lerpField f = lerpF (lerpF (f ep00) (f ep10) tu)
                            (lerpF (f ep01) (f ep11) tu) tv
    in ErosionParams
        { epIntensity     = lerpField epIntensity
        , epHydraulic     = lerpField epHydraulic
        , epThermal       = lerpField epThermal
        , epWind          = lerpField epWind
        , epChemical      = lerpField epChemical
        , epSeed          = epSeed ep00
        , epTemperature   = lerpField epTemperature
        , epPrecipitation = lerpField epPrecipitation
        , epHumidity      = lerpField epHumidity
        , epSnowFraction  = lerpField epSnowFraction
        , epIsLastAge     = epIsLastAge ep00
        }
