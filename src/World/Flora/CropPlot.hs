{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | Planted groundcover-crop state (#334).
--
--   A groundcover crop (wheat, barley, ...) draws as the surface
--   'World.Chunk.Types.ctVeg' tile-fill rather than a floating sprite
--   (row crops use ordinary 'World.Flora.Types.FloraInstance's for
--   that instead), so it has no per-instance record in chunk data —
--   'ctVeg' is just a static byte id with no room for an age. This is
--   the small world-level record that fills that gap: which species,
--   and the world day it was planted (the age-0 baseline the #332
--   runtime measures elapsed growth from), so a planted tile can share
--   the exact same growth/texture/harvest logic as wild flora instead
--   of duplicating it as a parallel set of static vegetation ids.
--
--   World-level sparse map, same shape as 'World.Flora.Harvest.
--   FloraHarvests' / 'World.Till.Types.TillDesignations' — tile-keyed,
--   written by the Lua planting primitive, read by the render pass and
--   harvest queries. Persisted per page ('wpsCropPlots').
module World.Flora.CropPlot
    ( CropPlot(..)
    , CropPlots
    , emptyCropPlots
    , newCropPlot
    , cropPlotElapsedDays
    , cropPlotInstance
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM
import World.Flora.Types (FloraId, FloraInstance(..))

-- | One planted tile. Field order is load-bearing (positional Generic
--   Serialize — append, don't reorder).
data CropPlot = CropPlot
    { cpSpecies    ∷ !FloraId
    , cpPlantedDay ∷ !Int
      -- ^ Absolute world day ('World.Time.worldAbsoluteDay') the crop
      --   was planted — the age-0 baseline. The #332 runtime measures
      --   this plot's growth as days ELAPSED SINCE PLANTING, not
      --   calendar day-of-year, so a crop's own lifecycle timeline
      --   starts fresh regardless of what day of the year it went in.
    , cpHealth     ∷ !Float
      -- ^ 0.0 dead … 1.0 full, same meaning as FloraInstance's
      --   fiHealth — scales growth speed via World.Flora.Growth.
    } deriving (Show, Eq, Generic, Serialize, NFData)

type CropPlots = HM.HashMap (Int, Int) CropPlot

emptyCropPlots ∷ CropPlots
emptyCropPlots = HM.empty

newCropPlot ∷ FloraId → Int → Float → CropPlot
newCropPlot = CropPlot

-- | Days elapsed since planting, clamped to non-negative (a plot can't
--   have gone negative-age even if queried before its planted day).
cropPlotElapsedDays ∷ Int → CropPlot → Int
cropPlotElapsedDays absDay cp = max 0 (absDay - cpPlantedDay cp)

-- | Synthesize a placement-shaped instance so 'World.Flora.Growth' /
--   'World.Flora.Render' can derive this plot's growth state and
--   texture without duplicating their logic. Callers pass
--   'cropPlotElapsedDays' as the runtime's absDay argument (fiAge
--   stays 0), which is what makes the plot's timeline start at zero on
--   its planted day rather than being pinned to an absolute placement
--   baseline like a naturally-placed FloraInstance.
cropPlotInstance ∷ CropPlot → FloraInstance
cropPlotInstance cp = FloraInstance
    { fiSpecies   = cpSpecies cp
    , fiTileX     = 0
    , fiTileY     = 0
    , fiOffU      = 0
    , fiOffV      = 0
    , fiZ         = 0
    , fiAge       = 0
    , fiHealth    = cpHealth cp
    , fiVariant   = 0
    , fiBaseWidth = 0
    }
