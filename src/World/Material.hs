{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module World.Material where

import UPrelude
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Control.DeepSeq (NFData(..))
import World.Texture.Types (WorldTextureType(..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed.Deriving (derivingUnbox)

-----------------------------------------------------------
-- Material IDs
-----------------------------------------------------------

newtype MaterialId = MaterialId { unMaterialId ∷ Word8 }
    deriving stock (Show, Eq, Ord)
    deriving newtype (NFData)
    deriving (Serialize, Generic)

derivingUnbox "MaterialId"
    [t| MaterialId -> Word8 |]
    [| unMaterialId |]
    [| MaterialId |]

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

matAnorthosite ∷ MaterialId
matAnorthosite = MaterialId 6

matPeridotite ∷ MaterialId
matPeridotite = MaterialId 7

matPegmatite ∷ MaterialId
matPegmatite = MaterialId 8

-- Igneous extrusive
matBasalt ∷ MaterialId
matBasalt = MaterialId 10

matObsidian ∷ MaterialId
matObsidian = MaterialId 11

matRhyolite ∷ MaterialId
matRhyolite = MaterialId 12

matAndesite ∷ MaterialId
matAndesite = MaterialId 13

matTuff ∷ MaterialId
matTuff = MaterialId 14

matPumice ∷ MaterialId
matPumice = MaterialId 15

matScoria ∷ MaterialId
matScoria = MaterialId 16

-- Sedimentary
matSandstone ∷ MaterialId
matSandstone = MaterialId 20

matSiltstone ∷ MaterialId
matSiltstone = MaterialId 21

matShale ∷ MaterialId
matShale = MaterialId 22

matConglomerate ∷ MaterialId
matConglomerate = MaterialId 23

matMudstone ∷ MaterialId
matMudstone = MaterialId 24

matClaystone ∷ MaterialId
matClaystone = MaterialId 25

-- sedimentary chemical/organic
matLimestone ∷ MaterialId
matLimestone = MaterialId 30

matChalk ∷ MaterialId
matChalk = MaterialId 31

matChert ∷ MaterialId
matChert = MaterialId 32

matRockSalt ∷ MaterialId
matRockSalt = MaterialId 33

matGypsum ∷ MaterialId
matGypsum = MaterialId 34

matDolomite ∷ MaterialId
matDolomite = MaterialId 35

-- metamorphic
matMarble ∷ MaterialId
matMarble = MaterialId 40

matQuartzite ∷ MaterialId
matQuartzite = MaterialId 41

matSlate ∷ MaterialId
matSlate = MaterialId 42

matSchist ∷ MaterialId
matSchist = MaterialId 43

matGneiss ∷ MaterialId
matGneiss = MaterialId 44

matPhyllite ∷ MaterialId
matPhyllite = MaterialId 45

-- soils
matClay ∷ MaterialId
matClay = MaterialId 50

matSandyClay ∷ MaterialId
matSandyClay = MaterialId 51

matSandyClayLoam ∷ MaterialId
matSandyClayLoam = MaterialId 52

matSandyLoam ∷ MaterialId
matSandyLoam = MaterialId 53

matLoamySand ∷ MaterialId
matLoamySand = MaterialId 54

matSand ∷ MaterialId
matSand = MaterialId 55

matLoam ∷ MaterialId
matLoam = MaterialId 56

matClayLoam ∷ MaterialId
matClayLoam = MaterialId 57

matSiltyClay ∷ MaterialId
matSiltyClay = MaterialId 58

matSiltyClayLoam ∷ MaterialId
matSiltyClayLoam = MaterialId 59

matSiltLoam ∷ MaterialId
matSiltLoam = MaterialId 60

matSilt ∷ MaterialId
matSilt = MaterialId 61

matPeat ∷ MaterialId
matPeat = MaterialId 62

matMuckyPeat ∷ MaterialId
matMuckyPeat = MaterialId 63

matMuck ∷ MaterialId
matMuck = MaterialId 64

matHeavyGravel ∷ MaterialId
matHeavyGravel = MaterialId 65

matLightGravel ∷ MaterialId
matLightGravel = MaterialId 66

matSaltFlat ∷ MaterialId
matSaltFlat = MaterialId 67

-- carbonaceous
matLignite ∷ MaterialId
matLignite = MaterialId 70

matBituminousCoal ∷ MaterialId
matBituminousCoal = MaterialId 71

matAnthracite ∷ MaterialId
matAnthracite = MaterialId 72

-- ores/metals
matIronOre ∷ MaterialId
matIronOre = MaterialId 80

matOlivine ∷ MaterialId
matOlivine = MaterialId 81

matPyroxene ∷ MaterialId
matPyroxene = MaterialId 82

matFeldspar ∷ MaterialId
matFeldspar = MaterialId 83

matCopperOre ∷ MaterialId
matCopperOre = MaterialId 84

matTinOre ∷ MaterialId
matTinOre = MaterialId 85

matGoldOre ∷ MaterialId
matGoldOre = MaterialId 86

-- Impact
matImpactite ∷ MaterialId
matImpactite = MaterialId 90

matTektite ∷ MaterialId
matTektite = MaterialId 91

-- volcanic
matLava ∷ MaterialId
matLava = MaterialId 100

matMagma ∷ MaterialId
matMagma = MaterialId 101

matVolcanicAsh ∷ MaterialId
matVolcanicAsh = MaterialId 102

matTephra ∷ MaterialId
matTephra = MaterialId 103

-- glacial
matTill ∷ MaterialId
matTill = MaterialId 110

matMoraine ∷ MaterialId
matMoraine = MaterialId 111

matGlacialClay ∷ MaterialId
matGlacialClay = MaterialId 112

matOutwashGravel ∷ MaterialId
matOutwashGravel = MaterialId 113

matGlacier ∷ MaterialId
matGlacier = MaterialId 250

-- special
matMantle ∷ MaterialId
matMantle = MaterialId 251

matOcean ∷ MaterialId
matOcean = MaterialId 255

-----------------------------------------------------------
-- Material Properties
-----------------------------------------------------------

data MaterialProps = MaterialProps
    { matName       ∷ !Text
    , matHardness   ∷ !Float
    , matDensity    ∷ !Float
    , matAlbedo     ∷ !Float
    } deriving (Show)

defaultMaterialProps ∷ MaterialProps
defaultMaterialProps = MaterialProps "unknown" 0.5 2.5 0.5

materialPropsTable ∷ V.Vector MaterialProps
materialPropsTable =
    let size = 256
        base = V.replicate size defaultMaterialProps
        updates =
            [ (1,   MaterialProps "granite"     0.9  2.7 0.5)
            , (2,   MaterialProps "diorite"     0.85 2.8 0.5)
            , (3,   MaterialProps "gabbro"      0.8  3.0 0.5)
            , (6,   MaterialProps "anorthosite" 0.9  2.6 0.5)
            , (7,   MaterialProps "peridotite"  0.8  3.3 0.5)
            , (8,   MaterialProps "pegmatite"   0.9  2.7 0.5)
            , (10,  MaterialProps "basalt"       0.7  3.0 0.5)
            , (11,  MaterialProps "obsidian"     0.95 2.4 0.5)
            , (12,  MaterialProps "rhyolite"     0.75 2.5 0.5)
            , (13,  MaterialProps "andesite"     0.7  2.6 0.5)
            , (14,  MaterialProps "tuff"         0.6  2.3 0.5)
            , (15,  MaterialProps "pumice"       0.3  0.9 0.5)
            , (16,  MaterialProps "scoria"       0.5  2.0 0.5)
            , (20,  MaterialProps "sandstone"    0.6  2.2 0.5)
            , (21,  MaterialProps "siltstone"    0.5  2.3 0.5)
            , (22,  MaterialProps "shale"        0.5  2.4 0.5)
            , (23,  MaterialProps "conglomerate" 0.7  2.5 0.5)
            , (24,  MaterialProps "mudstone"     0.4  2.1 0.5)
            , (25,  MaterialProps "claystone"    0.3  2.0 0.5)
            , (30,  MaterialProps "limestone"    0.7  2.5 0.5)
            , (31,  MaterialProps "chalk"         0.5  2.3 0.5)
            , (32,  MaterialProps "chert"         0.8  2.6 0.5)
            , (33,  MaterialProps "rock salt"     0.4  2.2 0.5)
            , (34,  MaterialProps "gypsum"        0.3  2.3 0.5)
            , (35,  MaterialProps "dolomite"      0.6  2.4 0.5)
            , (40,  MaterialProps "marble"       0.8  2.7 0.5)
            , (41,  MaterialProps "quartzite"     0.9  2.6 0.5)
            , (42,  MaterialProps "slate"         0.7  2.5 0.5)
            , (43,  MaterialProps "schist"        0.6  2.8 0.5)
            , (44,  MaterialProps "gneiss"        0.8  2.7 0.5)
            , (45,  MaterialProps "phyllite"      0.6  2.5 0.5)
            , (50,  MaterialProps "clay"          0.3  1.6 0.5)
            , (51,  MaterialProps "sandy_clay"    0.4  1.8 0.5)
            , (52,  MaterialProps "sandy_clay_loam" 0.5 1.9 0.5)
            , (53,  MaterialProps "sandy_loam"    0.5  1.7 0.5)
            , (54,  MaterialProps "loamy_sand"    0.4  1.5 0.5)
            , (55,  MaterialProps "sand"          0.4  1.6 0.5)
            , (56,  MaterialProps "loam"          0.5  1.8 0.5)
            , (57,  MaterialProps "clay_loam"     0.4  1.7 0.5)
            , (58,  MaterialProps "silty_clay"    0.3  1.5 0.5)
            , (59,  MaterialProps "silty_clay_loam" 0.4 1.6 0.5)
            , (60,  MaterialProps "silt_loam"     0.4  1.7 0.5)
            , (61,  MaterialProps "silt"          0.3  1.6 0.5)
            , (62,  MaterialProps "peat"          0.2  0.9 0.5)
            , (63,  MaterialProps "mucky_peat"    0.2  1.0 0.5)
            , (64,  MaterialProps "muck"          0.2  1.2 0.5)
            , (65,  MaterialProps "heavy_gravel"  0.5  2.0 0.5)
            , (66,  MaterialProps "light_gravel"  0.4  1.8 0.5)
            , (67,  MaterialProps "salt_flat"     0.3  1.5 0.5)
            , (70,  MaterialProps "lignite"       0.2  1.3 0.5)
            , (71,  MaterialProps "bituminous_coal" 0.3 1.4 0.5)
            , (72,  MaterialProps "anthracite"    0.4  1.5 0.5)
            , (80,  MaterialProps "iron_ore"      0.6  3.5 0.5)
            , (81,  MaterialProps "olivine"       0.7  3.3 0.5)
            , (82,  MaterialProps "pyroxene"      0.6  3.2 0.5)
            , (83,  MaterialProps "feldspar"      0.7  2.6 0.5)
            , (84,  MaterialProps "copper_ore"    0.5  3.0 0.5)
            , (85,  MaterialProps "tin_ore"       0.5  3.0 0.5)
            , (86,  MaterialProps "gold_ore"      0.5  3.0 0.5)
            , (90,  MaterialProps "impactite"     0.7  2.5 0.5)
            , (91,  MaterialProps "tektite"       0.6  2.4 0.5)
            , (100, MaterialProps "lava"          0.9  3.0 0.5)
            , (101, MaterialProps "magma"         0.9  3.0 0.5)
            , (102, MaterialProps "volcanic_ash"  0.4  2.0 0.5)
            , (103, MaterialProps "tephra"        0.5  2.2 0.5)
            , (110, MaterialProps "till"          0.5  2.0 0.5)
            , (111, MaterialProps "moraine"       0.5  2.2 0.5)
            , (112, MaterialProps "glacial_clay"  0.4  1.8 0.5)
            , (113, MaterialProps "outwash_gravel" 0.5 2.5 0.5)
            , (250, MaterialProps "glacier"       0.4  0.9 0.5)
            , (251, MaterialProps "mantle"        0.8  3.3 0.5)
            , (255, MaterialProps "ocean"         0.5  1.0 0.5)
            ]
    in base V.// updates

getMaterialProps ∷ MaterialId → MaterialProps
getMaterialProps (MaterialId mid) =
    materialPropsTable V.! fromIntegral mid

