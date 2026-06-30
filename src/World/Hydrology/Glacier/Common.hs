{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.Glacier.Common
    ( getGlacierParams
    ) where

import UPrelude
import World.Types
import World.Hydrology.Types

-- * Extract glacier params from PersistentFeature

getGlacierParams ∷ PersistentFeature → GlacierParams
getGlacierParams pf = case pfFeature pf of
    (HydroShape (GlacierFeature g)) → g
    other → error $ "getGlacierParams: not a glacier (featureId=" ⧺ show (pfId pf)
                  ⧺ " formationPeriod=" ⧺ show (pfFormationPeriod pf)
                  ⧺ " actualShape=" ⧺ featureShapeTag other ⧺ ")"
