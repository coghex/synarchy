{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Geology.Types
    ( GeoModification(..)
    , noModification
    , CraterEra(..)
    , TimelineBuildState(..)
    , allocFeatureId
    , addPeriod
    , registerFeature
    , updateFeature
    ) where

import UPrelude
import World.Types

-----------------------------------------------------------
-- GeoModification
-----------------------------------------------------------

-- | Apply a single geological event to a tile position.
--   Returns the elevation and material modification.
--   Called per-tile during chunk generation.
data GeoModification = GeoModification
    { gmElevDelta   ∷ !Int         -- ^ Add to elevation
    , gmMaterialOverride ∷ !(Maybe Word8) -- ^ Replace material if Just
    } deriving (Show)

noModification ∷ GeoModification
noModification = GeoModification 0 Nothing

-----------------------------------------------------------
-- Crater Era
-----------------------------------------------------------

-- | Which era of bombardment — controls crater count and size.
data CraterEra
    = CraterEra_Primordial   -- ^ Few but huge
    | CraterEra_Late         -- ^ More but smaller
    deriving (Show, Eq)

-----------------------------------------------------------
-- Timeline Build State & Helpers
-----------------------------------------------------------

-- | State threaded through timeline construction.
data TimelineBuildState = TimelineBuildState
    { tbsFeatures   ∷ ![PersistentFeature]
    , tbsNextId     ∷ !Int
    , tbsPeriods    ∷ ![GeoPeriod]       -- ^ Accumulated in reverse
    , tbsPeriodIdx  ∷ !Int
    }

-- | Helper to allocate a new feature ID.
allocFeatureId ∷ TimelineBuildState → (GeoFeatureId, TimelineBuildState)
allocFeatureId tbs =
    let fid = GeoFeatureId (tbsNextId tbs)
    in (fid, tbs { tbsNextId = tbsNextId tbs + 1 })

-- | Helper to add a period and advance the index.
addPeriod ∷ GeoPeriod → TimelineBuildState → TimelineBuildState
addPeriod period tbs = tbs
    { tbsPeriods   = period : tbsPeriods tbs
    , tbsPeriodIdx = tbsPeriodIdx tbs + 1
    }

-- | Helper to register a new persistent feature.
registerFeature ∷ PersistentFeature → TimelineBuildState → TimelineBuildState
registerFeature pf tbs = tbs
    { tbsFeatures = pf : tbsFeatures tbs
    }

-- | Helper to update an existing feature's state.
updateFeature ∷ GeoFeatureId → (PersistentFeature → PersistentFeature)
              → TimelineBuildState → TimelineBuildState
updateFeature fid f tbs = tbs
    { tbsFeatures = map (\pf → if pfId pf ≡ fid then f pf else pf)
                        (tbsFeatures tbs)
    }
