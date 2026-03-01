{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module World.Flora.Types
    ( -- * Species Identity
      FloraId(..)
      -- * Lifecycle
    , LifecycleType(..)
      -- * Life Phases
    , LifePhase(..)
    , LifePhaseTag(..)
    , lifePhaseOrder
      -- * Annual Cycle
    , AnnualStage(..)
    , AnnualStageTag(..)
    , AnnualCycleKey(..)
      -- * Species Definition
    , FloraSpecies(..)
    , newFloraSpecies
      -- * World Generation
    , FloraWorldGen(..)
      -- * Per-Instance Data
    , FloraInstance(..)
    , FloraChunkData(..)
    , emptyFloraChunkData
      -- * Runtime Catalog
    , FloraCatalog(..)
    , emptyFloraCatalog
    , insertSpecies
    , lookupSpecies
    , nextFloraId
    , insertWorldGen
    , worldGenSpecies
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable(..))
import Engine.Asset.Handle (TextureHandle(..))

-----------------------------------------------------------
-- Species Identity
-----------------------------------------------------------

newtype FloraId = FloraId { unFloraId ∷ Word16 }
    deriving stock (Show, Eq, Ord)
    deriving newtype (NFData, Hashable)
    deriving (Serialize, Generic)

-----------------------------------------------------------
-- Lifecycle Types
-----------------------------------------------------------

data LifecycleType
    = Evergreen
    | Perennial
        { lcMinLifespan ∷ !Float
        , lcMaxLifespan ∷ !Float
        , lcDeathChance ∷ !Float
        }
    | Annual
    | Biennial
    deriving (Show, Eq, Generic, Serialize)
instance NFData LifecycleType where
    rnf Evergreen = ()
    rnf (Perennial a b c) = rnf a `seq` rnf b `seq` rnf c
    rnf Annual = ()
    rnf Biennial = ()

-----------------------------------------------------------
-- Life Phases (age-driven, happens once)
-----------------------------------------------------------

data LifePhaseTag
    = PhaseSprout
    | PhaseSeedling
    | PhaseVegetating
    | PhaseBudding
    | PhaseFlowering
    | PhaseRipening
    | PhaseMatured
    | PhaseWithering
    | PhaseDead
    deriving (Show, Eq, Ord, Enum, Bounded, Generic, Serialize)
instance NFData LifePhaseTag where rnf x = x `seq` ()
instance Hashable LifePhaseTag where
    hashWithSalt s t = hashWithSalt s (fromEnum t)

lifePhaseOrder ∷ LifePhaseTag → Int
lifePhaseOrder PhaseSprout     = 0
lifePhaseOrder PhaseSeedling   = 1
lifePhaseOrder PhaseVegetating = 2
lifePhaseOrder PhaseBudding    = 3
lifePhaseOrder PhaseFlowering  = 4
lifePhaseOrder PhaseRipening   = 5
lifePhaseOrder PhaseMatured    = 6
lifePhaseOrder PhaseWithering  = 7
lifePhaseOrder PhaseDead       = 8

data LifePhase = LifePhase
    { lpTag     ∷ !LifePhaseTag
    , lpAge     ∷ !Float
    , lpTexture ∷ !TextureHandle
    } deriving (Show, Eq, Generic, Serialize)
instance NFData LifePhase where
    rnf (LifePhase t a tex) = rnf t `seq` rnf a `seq` rnf tex

-----------------------------------------------------------
-- Annual Cycle (day-of-year-driven, repeats)
-----------------------------------------------------------

data AnnualStageTag
    = CycleDormant
    | CycleBudding
    | CycleFlowering
    | CycleFruiting
    | CycleSenescing
    deriving (Show, Eq, Ord, Enum, Bounded, Generic, Serialize)
instance NFData AnnualStageTag where rnf x = x `seq` ()
instance Hashable AnnualStageTag where
    hashWithSalt s t = hashWithSalt s (fromEnum t)

data AnnualStage = AnnualStage
    { asTag      ∷ !AnnualStageTag
    , asStartDay ∷ !Int
    , asTexture  ∷ !TextureHandle
    } deriving (Show, Eq, Generic, Serialize)
instance NFData AnnualStage where
    rnf (AnnualStage t d tex) = rnf t `seq` rnf d `seq` rnf tex

data AnnualCycleKey = AnnualCycleKey !LifePhaseTag !AnnualStageTag
    deriving (Show, Eq, Ord, Generic, NFData, Serialize)
instance Hashable AnnualCycleKey where
    hashWithSalt s (AnnualCycleKey p c) =
        s `hashWithSalt` fromEnum p `hashWithSalt` fromEnum c

-----------------------------------------------------------
-- Species Definition
-----------------------------------------------------------

data FloraSpecies = FloraSpecies
    { fsName           ∷ !Text
    , fsBaseTexture    ∷ !TextureHandle
    , fsLifecycle      ∷ !LifecycleType
    , fsPhases         ∷ !(HM.HashMap LifePhaseTag LifePhase)
    , fsAnnualCycle    ∷ ![AnnualStage]
    , fsCycleOverrides ∷ !(HM.HashMap AnnualCycleKey TextureHandle)
    } deriving (Show, Eq, Generic, Serialize, NFData)

newFloraSpecies ∷ Text → TextureHandle → FloraSpecies
newFloraSpecies name baseTex = FloraSpecies
    { fsName           = name
    , fsBaseTexture    = baseTex
    , fsLifecycle      = Evergreen
    , fsPhases         = HM.empty
    , fsAnnualCycle    = []
    , fsCycleOverrides = HM.empty
    }

-----------------------------------------------------------
-- World Generation Registration
-----------------------------------------------------------

data FloraWorldGen = FloraWorldGen
    { fwCategory  ∷ !Text
    , fwMinTemp   ∷ !Float
    , fwMaxTemp   ∷ !Float
    , fwIdealTemp ∷ !Float
    , fwMinPrecip ∷ !Float
    , fwMaxPrecip ∷ !Float
    , fwIdealPrecip ∷ !Float
    , fwMinAlt      ∷ !Int
    , fwMaxAlt      ∷ !Int
    , fwIdealAlt    ∷ !Int
    , fwMinHumidity ∷ !Float
    , fwMaxHumidity ∷ !Float
    , fwIdealHumidity ∷ !Float
    , fwMaxSlope  ∷ !Word8
    , fwDensity   ∷ !Float
    , fwSoils     ∷ ![Word8]
    , fwFootprint ∷ !Float
    } deriving (Show, Eq, Generic, Serialize, NFData)

-----------------------------------------------------------
-- Per-Instance Data (Saved per chunk)
-----------------------------------------------------------

-- | A single placed flora instance in the world.
--
--   Position is stored as:
--     fiTileX, fiTileY: which column in the chunk (0–15)
--     fiOffU,  fiOffV:  sub-tile offset within that column
--       (0.0, 0.0) = tile center
--       range roughly (-0.5 .. 0.5) in each axis
--     fiZ: integer z-slice the plant sits on
--
--   Multiple instances can share the same tile. A meadow
--   tile might have 3-4 dandelions at different offsets;
--   a forest tile has one oak at (0,0).
data FloraInstance = FloraInstance
    { fiSpecies ∷ !FloraId
    , fiTileX   ∷ !Word8         -- ^ column X within chunk (0–15)
    , fiTileY   ∷ !Word8         -- ^ column Y within chunk (0–15)
    , fiOffU    ∷ !Float         -- ^ sub-tile U offset (-0.5 .. 0.5)
    , fiOffV    ∷ !Float         -- ^ sub-tile V offset (-0.5 .. 0.5)
    , fiZ       ∷ !Int           -- ^ z-slice this plant sits on
    , fiAge     ∷ !Float         -- ^ current age in game-days
    , fiHealth  ∷ !Float         -- ^ 0.0 dead … 1.0 full
    , fiVariant ∷ !Word8         -- ^ visual variant (0–3)
    , fiBaseWidth  ∷ !Float         -- ^ base width in pixels for offset clamp
    } deriving (Show, Eq, Generic, Serialize)
instance NFData FloraInstance where
    rnf (FloraInstance s tx ty ou ov z a h v bw) =
        rnf s `seq` rnf tx `seq` rnf ty `seq`
        rnf ou `seq` rnf ov `seq` rnf z `seq`
        rnf a `seq` rnf h `seq` rnf v `seq` rnf bw

-- | All flora placed in one chunk.
data FloraChunkData = FloraChunkData
    { fcdInstances ∷ ![FloraInstance]
    } deriving (Show, Eq, Generic, Serialize)
instance NFData FloraChunkData where
    rnf (FloraChunkData is) = rnf is

emptyFloraChunkData ∷ FloraChunkData
emptyFloraChunkData = FloraChunkData []

-----------------------------------------------------------
-- Runtime Catalog
-----------------------------------------------------------

data FloraCatalog = FloraCatalog
    { fcSpecies  ∷ !(HM.HashMap Word16 FloraSpecies)
    , fcWorldGen ∷ !(HM.HashMap Word16 FloraWorldGen)
    , fcNextId   ∷ !Word16
    } deriving (Show, Eq, Generic, Serialize, NFData)

emptyFloraCatalog ∷ FloraCatalog
emptyFloraCatalog = FloraCatalog
    { fcSpecies  = HM.empty
    , fcWorldGen = HM.empty
    , fcNextId   = 1
    }

nextFloraId ∷ FloraCatalog → (FloraId, FloraCatalog)
nextFloraId cat =
    let fid = FloraId (fcNextId cat)
    in (fid, cat { fcNextId = fcNextId cat + 1 })

insertSpecies ∷ FloraId → FloraSpecies → FloraCatalog → FloraCatalog
insertSpecies (FloraId fid) species cat =
    cat { fcSpecies = HM.insert fid species (fcSpecies cat) }

lookupSpecies ∷ FloraId → FloraCatalog → Maybe FloraSpecies
lookupSpecies (FloraId fid) cat = HM.lookup fid (fcSpecies cat)

insertWorldGen ∷ FloraId → FloraWorldGen → FloraCatalog → FloraCatalog
insertWorldGen (FloraId fid) wg cat =
    cat { fcWorldGen = HM.insert fid wg (fcWorldGen cat) }

worldGenSpecies ∷ FloraCatalog → [(FloraId, FloraWorldGen)]
worldGenSpecies cat =
    map (\(k, v) → (FloraId k, v)) $ HM.toList (fcWorldGen cat)
