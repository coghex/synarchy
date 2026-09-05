{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
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
    , FloraHarvest(..)
    , newFloraSpecies
      -- * World Generation
    , FloraWorldGen(..)
      -- * Per-Instance Data
    , FloraInstance(..)
    , floraInstanceOnTile
    , FloraChunkData(..)
    , emptyFloraChunkData
      -- * Runtime Catalog
    , FloraCatalog(..)
    , emptyFloraCatalog
    , insertSpecies
    , lookupSpecies
    , findSpeciesByName
    , nextFloraId
    , insertWorldGen
    , worldGenSpecies
    , floraWorldGenKey
    , isPlantableCropCategory
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Data.Hashable (Hashable(..))
import Engine.Asset.Handle (TextureHandle(..))
import World.Flora.Identity (FloraInstanceId)

-- * Species Identity

newtype FloraId = FloraId { unFloraId ∷ Word16 }
    deriving stock (Show, Eq, Ord)
    deriving newtype (NFData, Hashable)
    deriving anyclass (Serialize)
    deriving stock (Generic)

-- * Lifecycle Types

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

-- * Life Phases (age-driven, happens once)

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

-- * Annual Cycle (day-of-year-driven, repeats)

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

-- * Species Definition

-- | Harvestable-species data (#94): what foraging a tile of this plant
--   yields, and how long the plant takes to regrow. Species without this
--   block are decorative only. Yields reference item-registry def names;
--   the count is rolled uniformly in [min, max] per harvest.
data FloraHarvest = FloraHarvest
    { fhTags             ∷ ![Text]
      -- ^ Harvest-part tags: fruit / nuts / leaves / roots / wood. A
      --   TAGGED harvest verb ('world.harvestFlora' with a tag, the
      --   chop AI's @"wood"@) only takes a species listing that tag.
    , fhUngatedTags      ∷ ![Text]
      -- ^ The subset of 'fhTags' whose harvests may take this plant
      --   OUTSIDE 'World.Flora.Growth.harvestOpen''s window (#2212).
      --
      --   Before #2212 every tagged call skipped the window
      --   unconditionally, which was a wood-removal policy written as
      --   a property of "being tagged at all": a future @fruit@ or
      --   @grain@ tag would have inherited it and silently disabled the
      --   #332 lifecycle and seasonal gates. Now the exemption is
      --   AUTHORED. Absent (the empty list) means growth-gated, so a
      --   tagged call against a block that declares none is refused in
      --   exactly the growth states a bare call is.
      --
      --   Every entry is required to appear in 'fhTags' at the
      --   authoring boundary ('Engine.Asset.YamlFlora'): an exemption
      --   for a tag the species does not carry could never be selected.
    , fhYield            ∷ ![(Text, Int, Int)]
      -- ^ (item def name, min count, max count) per harvest — the
      --   block's DEFAULT roll, inherited by every life phase that
      --   authors no override of its own.
    , fhPhaseYields      ∷ !(HM.HashMap LifePhaseTag [(Text, Int, Int)])
      -- ^ Per-life-phase yield overrides (#2212). An ABSENT phase
      --   inherits 'fhYield'; a phase mapped to the EMPTY list yields
      --   nothing at all, which is what makes a felled sprout cost the
      --   colony a fell without paying it a mature tree's logs.
      --
      --   Absent and explicitly-empty are therefore different authored
      --   statements, and the decoder keeps them apart. The lookup key
      --   is the plant's derived life phase
      --   ('World.Flora.Growth.growthPhaseTag'), so a species with no
      --   @phases:@ at all has no key to hit and always inherits.
    , fhRegrowth         ∷ !Float
      -- ^ GAME-seconds until the tile is harvestable again (86400 = one
      --   game-day ≈ 24 real-minutes at timeScale 1). Always finite and
      --   strictly positive: the value is constrained where it is
      --   AUTHORED ('Engine.Asset.YamlFlora.requireRegrowthTime', #1711)
      --   and deliberately not clamped at any action site, so a
      --   non-positive one — which the harvest gate would read as
      --   already expired, making the very next call spawn the full
      --   yield again — cannot reach here at all.
    , fhHarvestedTexture ∷ !TextureHandle
      -- ^ Depleted visual drawn while regrowing (a berry bush with no
      --   fruit). Handle 0 = no depleted art; the plant is hidden
      --   (bare tile) until regrowth instead.
    } deriving (Show, Eq, Generic, Serialize, NFData)

data FloraSpecies = FloraSpecies
    { fsName           ∷ !Text
    , fsBaseTexture    ∷ !TextureHandle
    , fsLifecycle      ∷ !LifecycleType
    , fsPhases         ∷ !(HM.HashMap LifePhaseTag LifePhase)
    , fsAnnualCycle    ∷ ![AnnualStage]
    , fsCycleOverrides ∷ !(HM.HashMap AnnualCycleKey TextureHandle)
    , fsHarvest        ∷ !(Maybe FloraHarvest)
      -- ^ Present ⇒ foraging units can harvest this species (#94).
    } deriving (Show, Eq, Generic, Serialize, NFData)

newFloraSpecies ∷ Text → TextureHandle → FloraSpecies
newFloraSpecies name baseTex = FloraSpecies
    { fsName           = name
    , fsBaseTexture    = baseTex
    , fsLifecycle      = Evergreen
    , fsPhases         = HM.empty
    , fsAnnualCycle    = []
    , fsCycleOverrides = HM.empty
    , fsHarvest        = Nothing
    }

-- * World Generation Registration

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

-- * Per-Instance Data (Saved per chunk)

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
--   a forest tile has one oak at (0,0), but two wood-tagged
--   trees on one tile are legitimate too. Since #1854 that
--   co-tenancy is addressable: 'fiInstanceId' names ONE
--   plant, and every mutable per-plant authority (Chop
--   designations, regrowth timers, the Lua chop claims) is
--   keyed by it rather than by the tile — designating or
--   felling one plant leaves its co-tenants alone.
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
    , fiInstanceId ∷ !FloraInstanceId
      -- ^ Stable per-plant identity (#1854). Assigned by
      --   'World.Flora.Placement.computeChunkFlora' for generated
      --   flora (deterministic, so it survives chunk eviction and
      --   reload) and by the page-scoped planted allocator for a
      --   'World.Edit.Types.WePlaceFlora' row crop. Opaque — see
      --   "World.Flora.Identity".
    , fiChopDesignated ∷ !Bool
      -- ^ Is THIS plant slated for felling (#1854 requirement 7)? The
      --   loaded mirror of the durable identity-keyed authority
      --   ('World.Chop.Types.ChopDesignations'); the two are only ever
      --   written together, by "World.Flora.Designation" (requirement
      --   8), so they cannot drift. Chunk data is regenerated on every
      --   eviction, which is exactly why the DURABLE side is the
      --   authority and this is the mirror, never the other way round.
    } deriving (Show, Eq, Generic, Serialize)
instance NFData FloraInstance where
    rnf (FloraInstance s tx ty ou ov z a h v bw i cd) =
        rnf s `seq` rnf tx `seq` rnf ty `seq`
        rnf ou `seq` rnf ov `seq` rnf z `seq`
        rnf a `seq` rnf h `seq` rnf v `seq` rnf bw `seq`
        rnf i `seq` rnf cd

-- | Does this instance stand on the given LOCAL chunk column? The one
--   spelling of the @fiTileX@\/@fiTileY@ comparison every tile-scoped
--   flora walk makes, so a co-tenancy filter cannot drift between the
--   render pass, the forage lookups and the designation commits.
floraInstanceOnTile ∷ Int → Int → FloraInstance → Bool
floraInstanceOnTile lx ly fi =
    fromIntegral (fiTileX fi) ≡ lx ∧ fromIntegral (fiTileY fi) ≡ ly

-- | All flora placed in one chunk.
data FloraChunkData = FloraChunkData
    { fcdInstances ∷ ![FloraInstance]
    } deriving (Show, Eq, Generic, Serialize)
instance NFData FloraChunkData where
    rnf (FloraChunkData is) = rnf is

emptyFloraChunkData ∷ FloraChunkData
emptyFloraChunkData = FloraChunkData []

-- * Runtime Catalog

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

-- | Find a registered flora species by its YAML @name@. Catalogs are
--   small (tens of species), so a linear scan needs no index. Shared by
--   any caller that only has a player/script-facing crop name (#334's
--   world.plantCropAt, #335's plant designation + suitability query).
findSpeciesByName ∷ Text → FloraCatalog → Maybe (FloraId, FloraSpecies)
findSpeciesByName name cat =
    listToMaybe [ (FloraId k, sp)
                | (k, sp) ← HM.toList (fcSpecies cat), fsName sp ≡ name ]

insertWorldGen ∷ FloraId → FloraWorldGen → FloraCatalog → FloraCatalog
insertWorldGen (FloraId fid) wg cat =
    cat { fcWorldGen = HM.insert fid wg (fcWorldGen cat) }

-- | Every world-generating species, in CANONICAL AUTHORED-NAME ORDER
--   (#2241).
--
--   Sorted rather than handed back in 'HM.toList' order because that
--   order is a hash-table artefact: it moves when an unrelated species
--   is registered, and worldgen walks this list to decide which plants
--   a tile holds. Sorting it is only half the fix — the rolls
--   themselves are salted from 'floraWorldGenKey' rather than from a
--   position in this list ('World.Flora.Placement') — but it is the
--   half that makes the VISIT order, and so the shared-occupancy
--   competition between species that do place, deterministic.
--
--   The 'FloraId' is the tie-break, which is what keeps this a TOTAL
--   order: two entries can share a key only when neither has a species
--   record, and ids are unique by construction.
worldGenSpecies ∷ FloraCatalog → [(FloraId, FloraWorldGen)]
worldGenSpecies cat =
    sortOn (\(fid, _) → (floraWorldGenKey cat fid, unFloraId fid))
        [ (FloraId k, v) | (k, v) ← HM.toList (fcWorldGen cat) ]

-- | The stable authored key one world-gen entry is ordered and salted
--   by (#2241): the species' own YAML @name@.
--
--   'fcWorldGen' and 'fcSpecies' are independent maps, and
--   @flora.registerForWorldGen@ accepts any numeric id, so an entry
--   with NO species record is reachable and has no authored name. Such
--   an entry falls back to a synthetic key built from its numeric id —
--   the only stable thing it has — rather than collapsing every
--   nameless entry onto one shared key and therefore one shared
--   placement roll. The @\SOH@ prefix is not a legal character in an
--   authored YAML name, so the two spaces cannot collide.
floraWorldGenKey ∷ FloraCatalog → FloraId → Text
floraWorldGenKey cat fid@(FloraId n) = case lookupSpecies fid cat of
    Just sp → fsName sp
    Nothing → "\SOHworldgen:" <> tshow n

-- | The two worldGen category tags (World.Flora.Placement) that mark a
--   species as a plantable crop — a row_crop 'FloraInstance' (#334) or a
--   groundcover 'World.Flora.CropPlot' tile-fill (#334). The planting
--   designation tool (#335) and its suitability query accept both forms
--   symmetrically (recording intent only); execution asymmetry between
--   the two forms is #336's concern, not this predicate's.
isPlantableCropCategory ∷ Text → Bool
isPlantableCropCategory cat = cat ≡ "row_crop" ∨ cat ≡ "groundcover_crop"
