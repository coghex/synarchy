{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
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
    ( CropPlotOf(..)
    , CropPlot
    , CropPlots
    , SavedCropPlot
    , SavedCropPlots
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
import World.Flora.Identity (floraInstanceIdNone)
import World.Flora.Reference (FloraRef)
import World.Flora.Types (FloraId, FloraInstance(..))

-- | One planted tile, parameterized by HOW it names its species
--   (#2243). Field order is load-bearing (positional Generic
--   Serialize — append, don't reorder).
--
--   The live session instantiates it at 'FloraId' ('CropPlot', below):
--   that is the runtime handle every growth, render and harvest path
--   already uses, and nothing about them changes. A SAVE instantiates
--   it at 'World.Flora.Reference.FloraRef' ('SavedCropPlot'), because
--   the runtime handle means nothing outside the session that minted
--   it. Parameterizing the one record — rather than mirroring it — is
--   what keeps the two spellings from drifting: a field added for
--   gameplay reasons appears on both, and the two conversions
--   ('World.Thread.Command.Save.WriteWorld' out,
--   'World.Load.Stage' back) stay one-line species swaps.
data CropPlotOf s = CropPlot
    { cpSpecies    ∷ !s
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

-- | The LIVE plot: species by runtime handle.
type CropPlot = CropPlotOf FloraId

type CropPlots = HM.HashMap (Int, Int) CropPlot

-- | The PERSISTED plot: species by durable reference (#2243).
type SavedCropPlot = CropPlotOf FloraRef

type SavedCropPlots = HM.HashMap (Int, Int) SavedCropPlot

emptyCropPlots ∷ CropPlots
emptyCropPlots = HM.empty

newCropPlot ∷ FloraId → Int → Float → CropPlot
newCropPlot = CropPlot

-- | Days elapsed since planting, clamped to non-negative (a plot can't
--   have gone negative-age even if queried before its planted day).
cropPlotElapsedDays ∷ Int → CropPlotOf s → Int
cropPlotElapsedDays absDay cp = max 0 (absDay - cpPlantedDay cp)

-- | Synthesize a placement-shaped instance so 'World.Flora.Growth' /
--   'World.Flora.Render' can derive this plot's growth state and
--   texture without duplicating their logic. Callers pass
--   'cropPlotElapsedDays' as the runtime's absDay argument (fiAge
--   stays 0), which is what makes the plot's timeline start at zero on
--   its planted day rather than being pinned to an absolute placement
--   baseline like a naturally-placed FloraInstance.
--
--   #1854: this value is deliberately NOT identity-bearing. It is not a
--   plant — it is a growth-math adapter over a tile-keyed plot, it never
--   enters chunk data, and a plot never coexists with wild
--   'FloraInstance's on the same tile (tilled soil excludes natural
--   flora placement). It therefore carries the reserved
--   'World.Flora.Identity.floraInstanceIdNone', which belongs to
--   neither the generated nor the planted namespace and so can collide
--   with no real plant's id; no durable designation, claim or harvest
--   timer may ever key on it. Crop PLOTS stay tile-keyed and unchanged
--   by #1854 — one plot per tile is true by construction.
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
    , fiInstanceId = floraInstanceIdNone
    , fiChopDesignated = False
    }
