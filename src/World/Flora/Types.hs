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

-- | How a plant's annual cycle behaves.
--
--   Evergreen:  No annual cycle. Phase texture used year-round.
--               Douglas fir, cactus, most shrubs.
--
--   Perennial:  Repeats annual cycle each year within eligible
--               life phases. Has a lifespan after which it has
--               a chance to die. Dandelion, wildflowers, oak.
--               lcMinLifespan / lcMaxLifespan control when
--               death chance begins (in game-days).
--               lcDeathChance is probability per year once
--               past min lifespan.
--
--   Annual:     One annual cycle, then dead at end of year.
--               Wheat, some wildflowers.
--
--   Biennial:   Vegetative first year, flowers second year,
--               then dead.
data LifecycleType
    = Evergreen
    | Perennial
        { lcMinLifespan ∷ !Float   -- ^ game-days before death possible
        , lcMaxLifespan ∷ !Float   -- ^ game-days, guaranteed dead by here
        , lcDeathChance ∷ !Float   -- ^ per-year chance once past min
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
    , lpAge     ∷ !Float           -- ^ game-days to enter this phase
    , lpTexture ∷ !TextureHandle   -- ^ default texture for this phase
    } deriving (Show, Eq, Generic, Serialize)
instance NFData LifePhase where
    rnf (LifePhase t a tex) = rnf t `seq` rnf a `seq` rnf tex

-----------------------------------------------------------
-- Annual Cycle (day-of-year-driven, repeats)
-----------------------------------------------------------

-- | Tags for stages within one annual cycle.
data AnnualStageTag
    = CycleDormant     -- ^ winter / off-season
    | CycleBudding     -- ^ early growth
    | CycleFlowering   -- ^ in bloom
    | CycleFruiting    -- ^ bearing fruit / seeds
    | CycleSenescing   -- ^ autumn color / wilting
    deriving (Show, Eq, Ord, Enum, Bounded, Generic, Serialize)
instance NFData AnnualStageTag where rnf x = x `seq` ()
instance Hashable AnnualStageTag where
    hashWithSalt s t = hashWithSalt s (fromEnum t)

-- | One stage in the annual cycle.
data AnnualStage = AnnualStage
    { asTag      ∷ !AnnualStageTag
    , asStartDay ∷ !Int             -- ^ day-of-year this stage begins (0-359)
    , asTexture  ∷ !TextureHandle   -- ^ texture for this cycle stage
    } deriving (Show, Eq, Generic, Serialize)
instance NFData AnnualStage where
    rnf (AnnualStage t d tex) = rnf t `seq` rnf d `seq` rnf tex

-- | Key for looking up a cycle texture override per life phase.
--   This lets "matured + flowering" have a different texture than
--   "vegetating + flowering" if you want.
data AnnualCycleKey = AnnualCycleKey !LifePhaseTag !AnnualStageTag
    deriving (Show, Eq, Ord, Generic)
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
    } deriving (Show)

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
    , fwMinPrecip ∷ !Float
    , fwMaxPrecip ∷ !Float
    , fwMaxSlope  ∷ !Word8
    , fwDensity   ∷ !Float
    , fwSoils     ∷ ![Word8]
    } deriving (Show)

-----------------------------------------------------------
-- Per-Instance Data (Saved per chunk)
-----------------------------------------------------------

data FloraInstance = FloraInstance
    { fiSpecies ∷ !FloraId
    , fiLocalX  ∷ !Word8
    , fiLocalY  ∷ !Word8
    , fiAge     ∷ !Float         -- ^ current age in game-days
    , fiHealth  ∷ !Float         -- ^ 0.0 dead … 1.0 full
    , fiVariant ∷ !Word8         -- ^ visual variant (0–3)
    } deriving (Show, Eq, Generic, Serialize)
instance NFData FloraInstance where
    rnf (FloraInstance s x y a h v) =
        rnf s `seq` rnf x `seq` rnf y `seq`
        rnf a `seq` rnf h `seq` rnf v

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
    } deriving (Show)

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
