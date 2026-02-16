{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Material
    ( -- * Material IDs
      MaterialId(..)
    , matAir
    , matGranite
    , matDiorite
    , matGabbro
    , matGlacier
    , matBasalt
    , matObsidian
    , matSandstone
    , matLimestone
    , matShale
    , matImpactite
    , matIron
    , matOlivine
    , matPyroxene
    , matFeldspar
    , matMagma
      -- * Material Properties
    , MaterialProps(..)
    , getMaterialProps
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))

-----------------------------------------------------------
-- Material IDs
-----------------------------------------------------------

newtype MaterialId = MaterialId { unMaterialId ∷ Word8 }
    deriving (Show, Eq, Ord)
instance NFData MaterialId where
    rnf (MaterialId mid) = rnf mid

-- | Air is the default material for empty space.
matAir ∷ MaterialId
matAir = MaterialId 0

-- Igneous intrusive
matGranite ∷ MaterialId
matGranite = MaterialId 1

matDiorite ∷ MaterialId
matDiorite = MaterialId 2

matGabbro ∷ MaterialId
matGabbro = MaterialId 3

-- Igneous extrusive (volcanic)
matBasalt ∷ MaterialId
matBasalt = MaterialId 4

matObsidian ∷ MaterialId
matObsidian = MaterialId 5

-- Sedimentary
matSandstone ∷ MaterialId
matSandstone = MaterialId 10

matLimestone ∷ MaterialId
matLimestone = MaterialId 11

matShale ∷ MaterialId
matShale = MaterialId 12

-- Impact
matImpactite ∷ MaterialId
matImpactite = MaterialId 20

-- Meteorite
matIron ∷ MaterialId
matIron = MaterialId 30

matOlivine ∷ MaterialId
matOlivine = MaterialId 31

matPyroxene ∷ MaterialId
matPyroxene = MaterialId 32

matFeldspar ∷ MaterialId
matFeldspar = MaterialId 33

-- Special
matGlacier ∷ MaterialId
matGlacier = MaterialId 250

matMagma ∷ MaterialId
matMagma = MaterialId 100

-----------------------------------------------------------
-- Material Properties
-----------------------------------------------------------

data MaterialProps = MaterialProps
    { matName       ∷ !Text
    , matHardness   ∷ !Float
    , matDensity    ∷ !Float
    } deriving (Show)

getMaterialProps ∷ MaterialId → MaterialProps
getMaterialProps (MaterialId mid) = case mid of
    1   → MaterialProps "granite"    0.9  2.7
    2   → MaterialProps "diorite"    0.85 2.8
    3   → MaterialProps "gabbro"     0.8  3.0
    4   → MaterialProps "basalt"     0.75 2.9
    5   → MaterialProps "obsidian"   0.95 2.4
    10  → MaterialProps "sandstone"  0.4  2.3
    11  → MaterialProps "limestone"  0.35 2.5
    12  → MaterialProps "shale"      0.25 2.4
    20  → MaterialProps "impactite"  0.7  2.6
    30  → MaterialProps "iron"       0.6  7.8
    31  → MaterialProps "olivine"    0.7  3.3
    32  → MaterialProps "pyroxene"   0.65 3.2
    33  → MaterialProps "feldspar"   0.6  2.6
    100 → MaterialProps "lava"       0.5  3.0
    250 → MaterialProps "glacier"    1.0  0.9
    _   → MaterialProps "unknown"    0.5  2.5
