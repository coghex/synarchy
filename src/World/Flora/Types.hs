{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module World.Flora.Types
    ( -- * Species Identity
      FloraId(..)
      -- * Life Phases
    , LifePhase(..)
    , LifePhaseTag(..)
    , lifePhaseOrder
      -- * Seasonal Variants
    , Season(..)
    , SeasonKey(..)
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

-- | Opaque handle for a registered flora species.
--   Word16 gives 65535 species (0 = no flora).
newtype FloraId = FloraId { unFloraId ∷ Word16 }
    deriving stock (Show, Eq, Ord)
    deriving newtype (NFData, Hashable)
    deriving (Serialize, Generic)

-----------------------------------------------------------
-- Life Phases
-----------------------------------------------------------

-- | The canonical life phase tags, in biological order.
--   A species can use any subset of these.
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

-- | Canonical ordering for sorting phases.
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

-- | One registered life phase.
data LifePhase = LifePhase
    { lpTag     ∷ !LifePhaseTag
    , lpAge     ∷ !Float           -- ^ age in game-days to enter this phase
    , lpTexture ∷ !TextureHandle   -- ^ default texture for this phase
    } deriving (Show, Eq, Generic, Serialize)
instance NFData LifePhase where
    rnf (LifePhase t a tex) = rnf t `seq` rnf a `seq` rnf tex

-----------------------------------------------------------
-- Seasonal Variants
-----------------------------------------------------------

data Season = Spring | Summer | Autumn | Winter
    deriving (Show, Eq, Ord, Enum, Bounded, Generic, Serialize)
instance NFData Season where rnf x = x `seq` ()
instance Hashable Season where
    hashWithSalt s t = hashWithSalt s (fromEnum t)

-- | Key for looking up a seasonal texture override.
data SeasonKey = SeasonKey !LifePhaseTag !Season
    deriving (Show, Eq, Ord, Generic)
instance Hashable SeasonKey where
    hashWithSalt s (SeasonKey p sn) =
        s `hashWithSalt` fromEnum p `hashWithSalt` fromEnum sn

-----------------------------------------------------------
-- Species Definition
-----------------------------------------------------------

-- | A fully described flora species.
--   Built incrementally: starts with just a name and base texture,
--   then phases, seasonal overrides, etc. are added one at a time.
data FloraSpecies = FloraSpecies
    { fsName           ∷ !Text
    , fsBaseTexture    ∷ !TextureHandle
    , fsPhases         ∷ !(HM.HashMap LifePhaseTag LifePhase)
    , fsSeasonOverride ∷ !(HM.HashMap SeasonKey TextureHandle)
    } deriving (Show)

-- | Create a minimal species with just a name and base texture.
newFloraSpecies ∷ Text → TextureHandle → FloraSpecies
newFloraSpecies name baseTex = FloraSpecies
    { fsName           = name
    , fsBaseTexture    = baseTex
    , fsPhases         = HM.empty
    , fsSeasonOverride = HM.empty
    }

-----------------------------------------------------------
-- World Generation Registration
-----------------------------------------------------------

-- | Biome constraints for world-generation placement.
--   Stored separately from FloraSpecies so that species
--   can exist without being placed during generation.
data FloraWorldGen = FloraWorldGen
    { fwCategory  ∷ !Text       -- ^ "tree", "shrub", "bush", "cactus", etc.
    , fwMinTemp   ∷ !Float
    , fwMaxTemp   ∷ !Float
    , fwMinPrecip ∷ !Float
    , fwMaxPrecip ∷ !Float
    , fwMaxSlope  ∷ !Word8
    , fwDensity   ∷ !Float      -- ^ placement probability per eligible tile
    , fwSoils     ∷ ![Word8]    -- ^ material IDs (empty = all non-barren)
    } deriving (Show)

-----------------------------------------------------------
-- Per-Instance Data (Saved per chunk)
-----------------------------------------------------------

-- | A single placed flora instance in the world.
data FloraInstance = FloraInstance
    { fiSpecies ∷ !FloraId       -- ^ which species
    , fiLocalX  ∷ !Word8         -- ^ column X within chunk (0–15)
    , fiLocalY  ∷ !Word8         -- ^ column Y within chunk (0–15)
    , fiAge     ∷ !Float         -- ^ current age in game-days
    , fiHealth  ∷ !Float         -- ^ 0.0 dead … 1.0 full health
    , fiVariant ∷ !Word8         -- ^ visual variant (0–3, for hash jitter)
    } deriving (Show, Eq, Generic, Serialize)
instance NFData FloraInstance where
    rnf (FloraInstance s x y a h v) =
        rnf s `seq` rnf x `seq` rnf y `seq`
        rnf a `seq` rnf h `seq` rnf v

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
    { fcSpecies   ∷ !(HM.HashMap Word16 FloraSpecies)
    , fcWorldGen  ∷ !(HM.HashMap Word16 FloraWorldGen)
    , fcNextId    ∷ !Word16
    } deriving (Show)

emptyFloraCatalog ∷ FloraCatalog
emptyFloraCatalog = FloraCatalog
    { fcSpecies  = HM.empty
    , fcWorldGen = HM.empty
    , fcNextId   = 1
    }

-- | Allocate the next available FloraId.
nextFloraId ∷ FloraCatalog → (FloraId, FloraCatalog)
nextFloraId cat =
    let fid = FloraId (fcNextId cat)
    in (fid, cat { fcNextId = fcNextId cat + 1 })

-- | Insert a species into the catalog.
insertSpecies ∷ FloraId → FloraSpecies → FloraCatalog → FloraCatalog
insertSpecies (FloraId fid) species cat =
    cat { fcSpecies = HM.insert fid species (fcSpecies cat) }

-- | Look up a species by its ID.
lookupSpecies ∷ FloraId → FloraCatalog → Maybe FloraSpecies
lookupSpecies (FloraId fid) cat = HM.lookup fid (fcSpecies cat)

-- | Register a species for world generation.
insertWorldGen ∷ FloraId → FloraWorldGen → FloraCatalog → FloraCatalog
insertWorldGen (FloraId fid) wg cat =
    cat { fcWorldGen = HM.insert fid wg (fcWorldGen cat) }

-- | Get all species registered for world generation.
worldGenSpecies ∷ FloraCatalog → [(FloraId, FloraWorldGen)]
worldGenSpecies cat =
    map (\(k, v) → (FloraId k, v)) $ HM.toList (fcWorldGen cat)
