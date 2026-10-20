{-# LANGUAGE Strict #-}
module World.Flora.Render
    ( resolveFloraTexture
    , resolveCropPlotTexture
    , findActiveCycleStage  -- re-exported from World.Flora.Growth for
                            -- annual-cycle selection tests
    ) where

import UPrelude
import Engine.Asset.Handle (TextureHandle(..))
import World.Flora.CropPlot (CropPlot, cropPlotElapsedDays, cropPlotInstance)
import World.Flora.Types
import World.Flora.Growth
    (floraGrowth, growthVisualPhase, growthVisualStage,
     findActiveCycleStage, FloraGrowth(..))
import World.Flora.Visual

-- | The occurrence adapter for the pure semantic resolver. EFM-3 requests
-- wild context for every caller. Gameplay keeps its existing dead-phase age;
-- only the visual selector uses the last living phase and frozen death stage.
resolveFloraTexture ∷ FloraCatalog → Int → Int → FloraInstance → TextureHandle
resolveFloraTexture catalog daysPerYear absDay inst =
    case lookupSpecies (fiSpecies inst) catalog of
        Nothing → TextureHandle 0
        Just species →
            let growth = floraGrowth species absDay inst
                condition = if fgDead growth then ConditionDead else ConditionAlive
                cause = if fgDead growth then Just CauseNatural else Nothing
                selector = floraVisualSelector ContextWild
                    (growthVisualPhase species inst growth)
                    (growthVisualStage species daysPerYear absDay inst growth)
                    condition cause
            in ftrTexture (resolveFloraVisual species selector)

-- | Groundcover adapter: both growth and the annual cycle deliberately use
-- elapsed days since planting, preserving the existing CropPlot render clock.
resolveCropPlotTexture ∷ FloraCatalog → Int → Int → CropPlot → TextureHandle
resolveCropPlotTexture catalog daysPerYear absDay plot =
    resolveFloraTexture catalog daysPerYear (cropPlotElapsedDays absDay plot)
        (cropPlotInstance plot)
