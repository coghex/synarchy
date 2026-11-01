{-# LANGUAGE Strict #-}
module World.Flora.Render
    ( resolveFloraTexture
    , resolveCropPlotTexture
    , OccurrenceAppearance(..)
    , occurrenceAppearance
    , appearanceTexture
    , plotAppearance
    , floraVisualSelectorFor
    , findActiveCycleStage  -- re-exported from World.Flora.Growth for
                            -- annual-cycle selection tests
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Engine.Asset.Handle (TextureHandle(..))
import World.Flora.Condition
    (FloraConditionRecordOf(..), FloraConditions, GrowthCondition)
import World.Flora.CropPlot
    (CropPlot, cropPlotElapsedDays, cropPlotGrowthCondition, cropPlotInstance)
import World.Flora.Harvest (FloraHarvests)
import World.Flora.Types
import World.Flora.Growth
    (floraGrowth, growthVisualPhase, growthVisualStage,
     findActiveCycleStage, instanceGrowthCondition, FloraGrowth(..))
import World.Flora.Visual

-- | The semantic request an occurrence presents (#2555: the one place
--   it is built). Context is the occurrence's own ('fiContext', #2547);
--   phase and stage are the LAST LIVING ones when it is dead, whether by
--   the natural lifespan or by an explicit condition (a record's frozen
--   snapshot); the cause is the record's, or 'CauseNatural' for the
--   lifespan's own dead window. Gameplay keeps its existing dead-phase
--   age; only the visual selector uses these.
floraVisualSelectorFor
    ∷ FloraSpecies → Int → Int → Maybe GrowthCondition → FloraInstance
    → (FloraGrowth, FloraVisualSelector)
floraVisualSelectorFor species daysPerYear absDay mCond inst =
    let growth = floraGrowth mCond species absDay inst
        condition = if fgDead growth then ConditionDead else ConditionAlive
        cause
            | not (fgDead growth) = Nothing
            | otherwise = Just (maybe CauseNatural fcrCause (fgCondition growth))
    in ( growth
       , floraVisualSelector (fiContext inst)
             (growthVisualPhase species inst growth)
             (growthVisualStage species daysPerYear absDay inst growth)
             condition cause )

-- | What an occurrence draws. Harvest depletion is the ONE state outside
--   the five semantic axes and it belongs to LIVING occurrences only: a
--   dead occurrence (explicitly conditioned or in its natural dead
--   window) is resolved through the condition order whether or not a
--   regrowth timer is running, and setting or clearing a condition never
--   touches that timer (contract §Flora visual state and fallback).
data OccurrenceAppearance
    = AppearanceHarvested !TextureHandle
      -- ^ The species' depleted art.
    | AppearanceResolved !FloraTextureResolution
      -- ^ The semantic resolver's answer, with its trace.
    deriving (Show, Eq)

appearanceTexture ∷ OccurrenceAppearance → TextureHandle
appearanceTexture (AppearanceHarvested t) = t
appearanceTexture (AppearanceResolved r)  = ftrTexture r

-- | The appearance of a wild or row-flora occurrence at the occurrence's
--   clock day, or 'Nothing' for a species the catalog does not hold.
--   Draws, the Chop picker, the cursor markers and the visual trace all
--   ask this, so they cannot disagree.
occurrenceAppearance
    ∷ FloraCatalog → Int → Int → FloraConditions → FloraHarvests
    → FloraInstance → Maybe OccurrenceAppearance
occurrenceAppearance catalog daysPerYear absDay conditions harvests inst = do
    species ← lookupSpecies (fiSpecies inst) catalog
    let (growth, selector) = floraVisualSelectorFor species daysPerYear absDay
            (instanceGrowthCondition conditions inst) inst
    pure $ case fsHarvest species of
        Just fh
            | HM.member (fiInstanceId inst) harvests ∧ not (fgDead growth)
            → AppearanceHarvested (fhHarvestedTexture fh)
        _ → AppearanceResolved (resolveFloraVisual species selector)

-- | The appearance of a groundcover plot. Growth and the annual cycle
--   deliberately use elapsed days since planting, preserving the existing
--   CropPlot render clock; the plot's embedded condition (#2552) is
--   rebased onto that clock.
plotAppearance
    ∷ FloraCatalog → Int → Int → CropPlot → Maybe FloraTextureResolution
plotAppearance catalog daysPerYear absDay plot = do
    species ← lookupSpecies (fiSpecies inst) catalog
    pure $ resolveFloraVisual species $ snd $ floraVisualSelectorFor species
        daysPerYear (cropPlotElapsedDays absDay plot)
        (cropPlotGrowthCondition plot) inst
  where inst = cropPlotInstance plot

-- | The occurrence adapter for the pure semantic resolver.
resolveFloraTexture
    ∷ FloraCatalog → Int → Int → FloraConditions → FloraInstance
    → TextureHandle
resolveFloraTexture catalog daysPerYear absDay conditions inst =
    case lookupSpecies (fiSpecies inst) catalog of
        Nothing → TextureHandle 0
        Just species →
            ftrTexture $ resolveFloraVisual species $ snd $
                floraVisualSelectorFor species daysPerYear absDay
                    (instanceGrowthCondition conditions inst) inst

-- | Groundcover adapter: see 'plotAppearance'.
resolveCropPlotTexture ∷ FloraCatalog → Int → Int → CropPlot → TextureHandle
resolveCropPlotTexture catalog daysPerYear absDay plot =
    maybe (TextureHandle 0) ftrTexture
        (plotAppearance catalog daysPerYear absDay plot)
