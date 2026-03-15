{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module World.Material
    ( -- * Core types
      MaterialId(..)
      -- * Named constants (compile-time, zero-cost)
    , matAir
    , matGranite, matDiorite, matGabbro
    , matAnorthosite, matPeridotite, matPegmatite
    , matBasalt, matObsidian, matRhyolite, matAndesite
    , matTuff, matPumice, matScoria
    , matSandstone, matSiltstone, matShale
    , matConglomerate, matMudstone, matClaystone
    , matLimestone, matChalk, matChert
    , matRockSalt, matGypsum, matDolomite
    , matMarble, matQuartzite, matSlate
    , matSchist, matGneiss, matPhyllite
    , matClay, matSandyClay, matSandyClayLoam
    , matSandyLoam, matLoamySand, matSand
    , matLoam, matClayLoam, matSiltyClay, matSiltyClayLoam
    , matSiltLoam, matSilt, matPeat, matMuckyPeat, matMuck
    , matHeavyGravel, matLightGravel, matSaltFlat
    , matLignite, matBituminousCoal, matAnthracite
    , matIronOre, matOlivine, matPyroxene, matFeldspar
    , matCopperOre, matTinOre, matGoldOre
    , matImpactite, matTektite
    , matLava, matMagma, matVolcanicAsh, matTephra
    , matTill, matMoraine, matGlacialClay, matOutwashGravel
    , matGlacier, matMantle, matIce, matOcean
      -- * Properties (runtime, loaded from YAML)
    , MaterialProps(..)
    , MaterialRegistry
    , emptyMaterialRegistry
    , registerMaterial
    , getMaterialProps
    , defaultMaterialProps
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Control.DeepSeq (NFData(..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.IORef as IORef
import Data.Vector.Unboxed.Deriving (derivingUnbox)

-----------------------------------------------------------
-- Material ID (unchanged)
-----------------------------------------------------------

newtype MaterialId = MaterialId { unMaterialId ∷ Word8 }
    deriving stock (Show, Eq, Ord)
    deriving newtype (NFData)
    deriving anyclass (Serialize)
    deriving stock (Generic)

derivingUnbox "MaterialId"
    [t| MaterialId -> Word8 |]
    [| unMaterialId |]
    [| MaterialId |]

-----------------------------------------------------------
-- Named Constants (keep — used everywhere, cost nothing)
-----------------------------------------------------------

matAir ∷ MaterialId
matAir = MaterialId 0

matGranite, matDiorite, matGabbro ∷ MaterialId
matGranite = MaterialId 1
matDiorite = MaterialId 2
matGabbro  = MaterialId 3

matAnorthosite, matPeridotite, matPegmatite ∷ MaterialId
matAnorthosite = MaterialId 6
matPeridotite  = MaterialId 7
matPegmatite   = MaterialId 8

matBasalt, matObsidian, matRhyolite, matAndesite ∷ MaterialId
matBasalt   = MaterialId 10
matObsidian = MaterialId 11
matRhyolite = MaterialId 12
matAndesite = MaterialId 13

matTuff, matPumice, matScoria ∷ MaterialId
matTuff   = MaterialId 14
matPumice = MaterialId 15
matScoria = MaterialId 16

matSandstone, matSiltstone, matShale ∷ MaterialId
matSandstone = MaterialId 20
matSiltstone = MaterialId 21
matShale     = MaterialId 22

matConglomerate, matMudstone, matClaystone ∷ MaterialId
matConglomerate = MaterialId 23
matMudstone     = MaterialId 24
matClaystone    = MaterialId 25

matLimestone, matChalk, matChert ∷ MaterialId
matLimestone = MaterialId 30
matChalk     = MaterialId 31
matChert     = MaterialId 32

matRockSalt, matGypsum, matDolomite ∷ MaterialId
matRockSalt = MaterialId 33
matGypsum   = MaterialId 34
matDolomite = MaterialId 35

matMarble, matQuartzite, matSlate ∷ MaterialId
matMarble    = MaterialId 40
matQuartzite = MaterialId 41
matSlate     = MaterialId 42

matSchist, matGneiss, matPhyllite ∷ MaterialId
matSchist   = MaterialId 43
matGneiss   = MaterialId 44
matPhyllite = MaterialId 45

matClay, matSandyClay, matSandyClayLoam ∷ MaterialId
matClay          = MaterialId 50
matSandyClay     = MaterialId 51
matSandyClayLoam = MaterialId 52

matSandyLoam, matLoamySand, matSand ∷ MaterialId
matSandyLoam = MaterialId 53
matLoamySand = MaterialId 54
matSand      = MaterialId 55

matLoam, matClayLoam, matSiltyClay, matSiltyClayLoam ∷ MaterialId
matLoam          = MaterialId 56
matClayLoam      = MaterialId 57
matSiltyClay     = MaterialId 58
matSiltyClayLoam = MaterialId 59

matSiltLoam, matSilt, matPeat, matMuckyPeat, matMuck ∷ MaterialId
matSiltLoam  = MaterialId 60
matSilt      = MaterialId 61
matPeat      = MaterialId 62
matMuckyPeat = MaterialId 63
matMuck      = MaterialId 64

matHeavyGravel, matLightGravel, matSaltFlat ∷ MaterialId
matHeavyGravel = MaterialId 65
matLightGravel = MaterialId 66
matSaltFlat    = MaterialId 67

matLignite, matBituminousCoal, matAnthracite ∷ MaterialId
matLignite       = MaterialId 70
matBituminousCoal = MaterialId 71
matAnthracite    = MaterialId 72

matIronOre, matOlivine, matPyroxene, matFeldspar ∷ MaterialId
matIronOre = MaterialId 80
matOlivine = MaterialId 81
matPyroxene = MaterialId 82
matFeldspar = MaterialId 83

matCopperOre, matTinOre, matGoldOre ∷ MaterialId
matCopperOre = MaterialId 84
matTinOre    = MaterialId 85
matGoldOre   = MaterialId 86

matImpactite, matTektite ∷ MaterialId
matImpactite = MaterialId 90
matTektite   = MaterialId 91

matLava, matMagma, matVolcanicAsh, matTephra ∷ MaterialId
matLava       = MaterialId 100
matMagma      = MaterialId 101
matVolcanicAsh = MaterialId 102
matTephra     = MaterialId 103

matTill, matMoraine, matGlacialClay, matOutwashGravel ∷ MaterialId
matTill          = MaterialId 110
matMoraine       = MaterialId 111
matGlacialClay   = MaterialId 112
matOutwashGravel = MaterialId 113

matGlacier ∷ MaterialId
matGlacier = MaterialId 250

matMantle ∷ MaterialId
matMantle = MaterialId 251

matIce ∷ MaterialId
matIce = MaterialId 252

matOcean ∷ MaterialId
matOcean = MaterialId 255

-----------------------------------------------------------
-- Material Properties (runtime, populated from YAML)
-----------------------------------------------------------

data MaterialProps = MaterialProps
    { mpName     ∷ !Text
    , mpHardness ∷ !Float
    , mpDensity  ∷ !Float
    , mpAlbedo   ∷ !Float
    } deriving (Show)

defaultMaterialProps ∷ MaterialProps
defaultMaterialProps = MaterialProps "unknown" 0.5 2.5 0.5

-- | 256-slot vector, one per possible Word8 material ID.
--   Starts as all defaults; YAML loading fills in real values.
newtype MaterialRegistry = MaterialRegistry (V.Vector MaterialProps)

emptyMaterialRegistry ∷ MaterialRegistry
emptyMaterialRegistry = MaterialRegistry (V.replicate 256 defaultMaterialProps)

-- | Register a single material's properties into the registry.
registerMaterial ∷ Word8 → MaterialProps → MaterialRegistry → MaterialRegistry
registerMaterial idx props (MaterialRegistry vec) =
    MaterialRegistry (vec V.// [(fromIntegral idx, props)])

-- | Look up properties by ID.
getMaterialProps ∷ MaterialRegistry → MaterialId → MaterialProps
getMaterialProps (MaterialRegistry vec) (MaterialId mid) =
    vec V.! fromIntegral mid
