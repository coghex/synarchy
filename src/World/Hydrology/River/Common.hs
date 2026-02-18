{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.River.Common
    ( getRiverParams
    ) where

import UPrelude
import World.Types
import World.Hydrology.Types

-----------------------------------------------------------
-- Extract river params from PersistentFeature
-----------------------------------------------------------

getRiverParams ∷ PersistentFeature → RiverParams
getRiverParams pf = case pfFeature pf of
    (HydroShape (RiverFeature r)) → r
    _ → error "getRiverParams: not a river"
