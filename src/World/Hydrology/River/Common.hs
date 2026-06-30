{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.River.Common
    ( getRiverParams
    ) where

import UPrelude
import World.Types
import World.Hydrology.Types

-- * Extract river params from PersistentFeature

getRiverParams ∷ PersistentFeature → RiverParams
getRiverParams pf = case pfFeature pf of
    (HydroShape (RiverFeature r)) → r
    other → error $ "getRiverParams: not a river (featureId=" ⧺ show (pfId pf)
                  ⧺ " formationPeriod=" ⧺ show (pfFormationPeriod pf)
                  ⧺ " actualShape=" ⧺ show other ⧺ ")"
