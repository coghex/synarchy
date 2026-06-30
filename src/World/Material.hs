{-# LANGUAGE Strict, UnicodeSyntax #-}

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
    , materialIdByName
    ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.IORef as IORef
-- MaterialId (+ its Unbox instance) lives in the NON-Strict
-- World.Material.Id — see the note there for why the derivingUnbox
-- splice must not be compiled under {-# LANGUAGE Strict #-}.
import World.Material.Id (MaterialId(..))

-- * Named Constants

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

-- * Material Properties

data MaterialProps = MaterialProps
    { mpName     ∷ !Text
    , mpHardness ∷ !Float
    , mpDensity  ∷ !Float
    , mpAlbedo   ∷ !Float
    , mpDrainage ∷ !Float
      -- ^ Hydraulic drainage 0.0 (impermeable bedrock — water sits on
      --   top) to 1.0 (ultra-permeable, e.g. karst — water table dives
      --   deep). 0.4 is the neutral baseline; used by the water-table
      --   compute in @World.Hydrology.WaterTable@.
    , mpPickSpeed   ∷ !Float
      -- ^ Dig-rate multiplier when excavating this material with a
      --   pick (1.0 = baseline rate; higher = faster). Picks excel on
      --   hard rock, struggle in loose soils.
    , mpShovelSpeed ∷ !Float
      -- ^ Dig-rate multiplier with a shovel. Shovels excel in loose
      --   soils (sand, loam), barely scratch igneous rock.
    , mpDigSpoil ∷ !(Maybe Text)
      -- ^ Material NAME the digger's spoil piles are made of (yaml
      --   @dig_spoil@). Nothing = digging this material produces no
      --   spoil (the pre-yields behaviour — it just vanishes).
      --   Resolved to a MaterialId at dig time via the registry so
      --   yaml load order doesn't matter.
    , mpDigBulking ∷ !Float
      -- ^ Spoil volume per excavated volume (yaml @dig_bulking@,
      --   default 1.0). Broken hard rock bulks >1 (granite 1.25);
      --   loose soils move 1:1.
    , mpDigChunk ∷ !(Maybe Text)
      -- ^ Item def NAME spawned by the chunk-yield accumulator while
      --   digging this material (yaml @dig_chunk@ — granite →
      --   "granite_chunk"). Nothing = no chunk yields.
    , mpDigGems ∷ !Bool
      -- ^ Does the seeded gem region field (World.Gem) apply while
      --   digging this material? yaml @dig_gems@, default False.
      --   Granite (pegmatite host) is the pilot.
    , mpMoveCost ∷ !Float
      -- ^ Surface-traversal cost multiplier for unit pathing (yaml
      --   @move_cost@, default 1.0 = firm ground). >1.0 slows a unit
      --   crossing this material AND makes A* prefer firmer routes
      --   (loose/soft soils — sand, silt, mud — are >1.0; bare rock
      --   stays 1.0). Read by @Unit.Pathing.Cost@. See issue #312.
    } deriving (Show)

defaultMaterialProps ∷ MaterialProps
defaultMaterialProps =
    MaterialProps "unknown" 0.5 2.5 0.5 0.4 0.5 0.5 Nothing 1.0 Nothing
                  False 1.0

-- | Find a material's id by its registered yaml name. Linear scan of
--   the 256-slot registry — called at dig frequency, not per-frame.
materialIdByName ∷ MaterialRegistry → Text → Maybe MaterialId
materialIdByName (MaterialRegistry vec) name =
    MaterialId ∘ fromIntegral
        <$> V.findIndex (\p → mpName p ≡ name) vec

-- | 256-slot vector indexed by 'Word8'; starts as defaults, filled by YAML.
newtype MaterialRegistry = MaterialRegistry (V.Vector MaterialProps)

emptyMaterialRegistry ∷ MaterialRegistry
emptyMaterialRegistry = MaterialRegistry (V.replicate 256 defaultMaterialProps)

registerMaterial ∷ Word8 → MaterialProps → MaterialRegistry → MaterialRegistry
registerMaterial idx props (MaterialRegistry vec) =
    MaterialRegistry (vec V.// [(fromIntegral idx, props)])

getMaterialProps ∷ MaterialRegistry → MaterialId → MaterialProps
getMaterialProps (MaterialRegistry vec) (MaterialId mid) =
    vec V.! fromIntegral mid
