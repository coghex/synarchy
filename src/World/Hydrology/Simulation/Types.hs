{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.Simulation.Types
    ( baseSampleSpacing
    , minRiverDrainageCells
    , effRiverThreshold
    , maxGridDim
    , minLakeDepth
    , calderaGuardCeiling
    , FlowResult(..)
    , ElevGrid(..)
    ) where

import UPrelude
import qualified Data.Vector.Unboxed as VU
import qualified Data.HashMap.Strict as HM
import World.Types
import World.Weather.Types (ClimateState(..), ClimateGrid(..)
                           , RegionClimate(..), SeasonalClimate(..))

-- * Configuration

baseSampleSpacing ∷ Int
baseSampleSpacing = 4

-- | Drainage-area target (cells of average-wetness) to qualify as a
--   river. The actual flow threshold is derived per-world from
--   climate (see `effRiverThreshold`) so river density per area
--   stays consistent across arid / balanced / wet climates rather
--   than scaling with per-cell water input (audit #15).
--
--   NB (issue #221): do NOT lower this to add inland rivers. Lowering
--   it admits many *small-catchment* (short) headwaters which, against
--   the fixed per-age river budget (`maxTotalRivers`), displace the
--   large-catchment rivers we actually want long — empirically it cut
--   median river length. Inland origins come from `walkToDivide`
--   rooting the selected (largest-catchment) rivers at their divides,
--   not from a lower qualification bar.
minRiverDrainageCells ∷ Int
minRiverDrainageCells = 16

-- | World-specific river threshold scaled by climate. Uses the same
--   per-cell water formula as `accumVec`: avg-cell water units ≈
--   avgPrecip * 10 (ignoring snowmelt for the average). For a
--   balanced world (avgPrecip ≈ 0.5) this gives 80 — the constant
--   the old `minRiverTotalFlow` was calibrated for. Floor at 16
--   matches the per-cell `max 1` floor — even extreme deserts can't
--   form rivers from <16 cells of drainage.
effRiverThreshold ∷ ClimateState → Int
effRiverThreshold climate =
    let regions = cgRegions (csClimate climate)
        avgPrecip = if HM.null regions
                    then 0.5
                    else HM.foldl' (\acc rc →
                              let SeasonalClimate s w = rcPrecipitation rc
                              in acc + (s + w) / 2.0
                              ) 0.0 regions
                         / fromIntegral (HM.size regions)
    in max minRiverDrainageCells
         $ round (fromIntegral minRiverDrainageCells * avgPrecip * 10.0 ∷ Float)

maxGridDim ∷ Int
maxGridDim = 384

minLakeDepth ∷ Int
minLakeDepth = 9

-- | Worlds below this size are the only ones affected by issue #811's
--   removal of the old @worldSize ≥ 128@ inland-source-extension gate
--   — 128+ already had extension enabled and PR #288 verified that
--   path lava-neutral without any caldera-aware guard. The new
--   caldera/supervolcano hazard check in `walkToDivide` (see
--   `calderaHazardsFor` / `isCalderaHazardAt`) is therefore scoped to
--   worlds below this ceiling only: it exists purely to make
--   extension SAFE on the newly-affected Tiny(32)/Small(64) sizes, and
--   applying it above 128 as well would be an unrequested behavior
--   change to output issue #811 explicitly requires stay intact.
calderaGuardCeiling ∷ Int
calderaGuardCeiling = 128

-- * Types

-- | Result of flow simulation. Instead of full RiverParams,
--   we now export river SOURCES — high-flow land cells that
--   should be traced at tile resolution.
data FlowResult = FlowResult
    { frRiverSources ∷ ![(Int, Int, Int, Float)]
      -- ^ (gx, gy, elevation, flowStrength) — sorted by flow descending
    , frLakes        ∷ ![LakeParams]
    , frFilledElev   ∷ !(VU.Vector Int)
      -- ^ Depression-filled elevation surface (same indexing as ElevGrid)
    , frFlowDir      ∷ !(VU.Vector Int)
      -- ^ Flow direction: index of downstream neighbor, −1 for ocean/sink.
      --   Following this chain is guaranteed to reach the ocean.
    } deriving (Show)

data ElevGrid = ElevGrid
    { egGridW   ∷ !Int
    , egSpacing ∷ !Int
    , egElev    ∷ !(VU.Vector Int)
    , egGX      ∷ !(VU.Vector Int)
    , egGY      ∷ !(VU.Vector Int)
    , egLand    ∷ !(VU.Vector Bool)
    } deriving (Show)
