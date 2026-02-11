{-# LANGUAGE Strict #-}
module World.Material
    ( -- * Material IDs
      MaterialId(..)
    , matGranite
    , matDiorite
    , matGabbro
    , matGlacier
      -- * Material Properties
    , MaterialProps(..)
    , getMaterialProps
    ) where

import UPrelude

-----------------------------------------------------------
-- Material IDs
-----------------------------------------------------------

newtype MaterialId = MaterialId { unMaterialId :: Word8 }
    deriving (Show, Eq, Ord)

-- Igneous intrusive
matGranite :: MaterialId
matGranite = MaterialId 1

matDiorite :: MaterialId
matDiorite = MaterialId 2

matGabbro :: MaterialId
matGabbro = MaterialId 3

-- Special
matGlacier :: MaterialId
matGlacier = MaterialId 250

-----------------------------------------------------------
-- Material Properties
-----------------------------------------------------------

data MaterialProps = MaterialProps
    { matName       :: !Text
    , matHardness   :: !Float
    , matDensity    :: !Float
    } deriving (Show)

getMaterialProps :: MaterialId -> MaterialProps
getMaterialProps (MaterialId mid) = case mid of
    1   -> MaterialProps "granite" 0.9 2.7
    2   -> MaterialProps "diorite" 0.85 2.8
    3   -> MaterialProps "gabbro" 0.8 3.0
    250 -> MaterialProps "glacier" 1.0 0.9
    _   -> MaterialProps "unknown" 0.5 2.5
