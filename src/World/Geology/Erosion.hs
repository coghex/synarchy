{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Re-exports the erosion API split across 'World.Geology.Erosion.Math'
--   (elevation-smoothing math), 'World.Geology.Erosion.Sediment'
--   (material selection for eroded/deposited tiles), and
--   'World.Geology.Erosion.Climate' (regional-erosion-param lookup +
--   bilinear interpolation). Public API unchanged from before the split.
module World.Geology.Erosion
    ( applyErosion
    , applyErosionLerp4
    , erosionSediment
    , lookupRegionalErosion
    , lerpErosionParams
    , erosionCornerLookup
    ) where

import World.Geology.Erosion.Math (applyErosion, applyErosionLerp4)
import World.Geology.Erosion.Sediment (erosionSediment)
import World.Geology.Erosion.Climate
    (lookupRegionalErosion, lerpErosionParams, erosionCornerLookup)
