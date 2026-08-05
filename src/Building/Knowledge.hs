{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
-- | The player's remembered view of what a container holds (#1087,
--   epic #1013 phase A3) — the EXPERIENTIAL layer beside the live
--   'Building.Types.biStorage' truth, in the same spirit as #915's
--   per-unit location memory beside #911's cartographic lifecycle.
--   Deliberately simplified relative to that pair: knowledge here is
--   __player-global__, never per-unit (epic decision 2). One record per
--   known container; every acolyte and every panel reads the same one.
--
--   __Scope: building endpoints only.__ A unit endpoint reports live and
--   has no record here at all — an entity always knows what it is
--   carrying, and the player can only reach a player-commandable unit as
--   a transfer endpoint in the first place, so there is no reachable
--   observation gap to model. The rule that follows needs no proximity
--   trigger: an entity knows its own contents; a container must be
--   inspected.
--
--   Everything in this module is PURE — the live per-page owner is
--   'World.State.Types.wsContainerKnowledgeRef' and the IO glue that
--   drives it from the real reveal triggers lives in
--   "Building.Knowledge.Live", so the model below is directly
--   exercisable from hspec with no engine boot.
--
--   __Three states, never conflated__ ('ContainerKnowledgeState'):
--   never-inspected (no record — the UI must say "unknown", not draw an
--   empty list), known-empty (inspected, and there was nothing in it),
--   and known-contents. A storage building the player has just finished
--   constructing seeds as known-empty: they watched it go up.
--
--   __A reveal replaces the whole record__ ('recordObservation'). There
--   is no partial or incremental update, and no id-resolution at read
--   time — the remembered items are full 'ItemInstance' COPIES taken at
--   reveal time (quality, condition, fill, sharpness, nested contents,
--   instance id as they were THEN). Resolving live ids at read time
--   would defeat the entire feature.
--
--   __Remembered instance ids are historical observations, not live
--   entities.__ They deliberately do NOT participate in
--   'World.Save.Snapshot.allItemInstanceIds' (allocator bounds,
--   duplicate-live-id checks) or in live @item_instance@ reference
--   resolution: staleness REQUIRES a remembered id to stay valid after
--   the live item has moved, changed or ceased to exist. Their def
--   names are still ordinary persisted content references and follow
--   the usual missing-item-definition load contract
--   ('World.Save.Types.missingItemDefReferences').
--
--   __Capacity is never remembered.__ The player knows how big a thing
--   they built, so every consumer reads 'Building.Types.bdStorageCapacity'
--   live; only the contents go stale.
module Building.Knowledge
    ( ContainerRecord(..)
    , ContainerKnowledge(..)
    , ContainerKnowledgeState(..)
    , SeedTrigger(..)
    , seedTriggerFor
    , emptyContainerKnowledge
    , containerKnowledgeStateId
    , lookupContainer
    , containerState
    , recordState
    , observeContainer
    , recordObservation
    , forgetContainer
    , knownContainerIds
    , retainContainers
    , prunedContainerIds
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Building.Types (BuildingId(..), BuildingDef(..))
import Item.Types (ItemInstance, ItemManager, itemTotalWeight)

-- | One container's remembered contents, exactly as they were at the
--   moment of the last reveal. Field order is load-bearing for the
--   frozen DTO that mirrors it
--   ("World.Save.Component.Knowledge"), not for this record itself.
data ContainerRecord = ContainerRecord
    { crItems        ∷ ![ItemInstance]
      -- ^ Full instance COPIES, not references. Empty ⇒ the container
      --   was observed and found empty (which is why the ABSENCE of a
      --   record, not an empty list, is what "never inspected" means).
    , crStoredWeight ∷ !Float
      -- ^ Total remembered mass (kg), derived from 'crItems' with the
      --   recursive 'itemTotalWeight' measure at reveal time — so the
      --   remembered weight always matches the remembered list, and a
      --   stored canteen counts at its then-current fill. Stored rather
      --   than recomputed on read so it can never drift from the list
      --   it summarizes if an item def's fill weight later changes.
    , crRevealedAt   ∷ !Double
      -- ^ GAME-TIME seconds (the 'Engine.Core.State.gameTimeRef' clock
      --   'Building.Types.biSpawnedAt' and 'Craft.Bills.cbClaimedAt'
      --   already use) at which this observation was taken — NOT the
      --   world calendar. Game time is monotonic, page-independent and
      --   already the engine's one "when did this happen" currency;
      --   the calendar is a per-page presentation of it. C1/D1's
      --   "as of…" rendering derives from this.
    } deriving (Show, Eq, Generic, Serialize)

-- | Every container the player knows something about, on ONE world
--   page. Keyed by container identity ('BuildingId'), which is stable
--   across save/load and chunk eviction.
--
--   Page-scoped rather than session-global: it rides on
--   'World.State.Types.WorldState' beside the other per-page gameplay
--   layers (craft bills, power nodes), so a page destroyed or replaced
--   takes its knowledge with it and a load can never merge a previous
--   live session's memories into the restored one.
newtype ContainerKnowledge = ContainerKnowledge
    { ckRecords ∷ HM.HashMap BuildingId ContainerRecord
    } deriving stock (Generic)
      deriving newtype (Show, Eq, Serialize)

emptyContainerKnowledge ∷ ContainerKnowledge
emptyContainerKnowledge = ContainerKnowledge HM.empty

-- | The three observable states, which must never be conflated. In
--   particular 'NeverInspected' is NOT 'KnownEmpty': the first means
--   "no unit has ever interacted with this container", the second
--   "somebody looked, and there was nothing in it".
data ContainerKnowledgeState
    = NeverInspected
    | KnownEmpty
    | KnownContents
    deriving (Show, Eq)

-- | The stable identifier the Lua surface reports. Distinct strings for
--   all three states so a consumer can never accidentally render an
--   unknown container as an empty one.
containerKnowledgeStateId ∷ ContainerKnowledgeState → Text
containerKnowledgeStateId NeverInspected = "unknown"
containerKnowledgeStateId KnownEmpty     = "empty"
containerKnowledgeStateId KnownContents  = "known"

-- | WHEN a storage-capable building's record is seeded as known-empty —
--   requirement 2's "a storage building the player has just finished
--   constructing"; they watched it go up, so reporting it as
--   never-inspected would be wrong.
--
--   'Building.Types.currentActivity' has two arms, and the "first
--   transition to Built" they describe happens at a different MOMENT in
--   each. This classifier is the one place that mapping lives, so the
--   two call sites can never disagree about which def seeds where.
data SeedTrigger
    = SeedAtBuildCompletion
      -- ^ WORKER-BUILT (@bdBuildWork > 0@): the building is created at
      --   zero progress and only becomes Built when 'biBuildProgress'
      --   reaches 'bdBuildWork'. Seeded by
      --   @building.addBuildProgress@'s crossing of that threshold —
      --   deliberately NOT at spawn, which would fire while the thing
      --   is still a construction site.
    | SeedAtSpawn
      -- ^ INSTANT-BUILT (@bdBuildWork == 0@, the portal/solar-panel
      --   shape): there is no construction work at all, so the
      --   time-based arm carries it to Built with nothing to observe
      --   the transition — no tick ever revisits the building, and the
      --   progress verb is never called for it. Placement IS the
      --   completion event for this class, so it seeds there. (The
      --   appearing animation is a visual flourish over an
      --   already-decided outcome, not construction: the container is
      --   empty either way, and any deposit during it reveals on its
      --   own.) Safe against load, which rebuilds
      --   'Building.Types.BuildingManager' directly in
      --   "World.Load.Publish" and never replays a @BuildingSpawn@ — so
      --   restoring an already-built container cannot masquerade as a
      --   new construction event.
    | NeverSeed
      -- ^ No storage at all: nothing to remember, ever.
    deriving (Show, Eq)

seedTriggerFor ∷ BuildingDef → SeedTrigger
seedTriggerFor def
    | bdStorageCapacity def ≤ 0 = NeverSeed
    | bdBuildWork def > 0       = SeedAtBuildCompletion
    | otherwise                 = SeedAtSpawn

lookupContainer ∷ BuildingId → ContainerKnowledge → Maybe ContainerRecord
lookupContainer bid = HM.lookup bid ∘ ckRecords

-- | Derive the state from a record's presence and emptiness — the ONE
--   place that mapping lives, so no reader can invent a fourth answer
--   or collapse the first two into each other.
recordState ∷ Maybe ContainerRecord → ContainerKnowledgeState
recordState Nothing = NeverInspected
recordState (Just r)
    | null (crItems r) = KnownEmpty
    | otherwise        = KnownContents

containerState ∷ BuildingId → ContainerKnowledge → ContainerKnowledgeState
containerState bid = recordState ∘ lookupContainer bid

-- | Build the record one observation of @items@ at @now@ produces.
--   Exposed separately from 'recordObservation' so a caller holding a
--   post-commit storage list can weigh it exactly once.
observeContainer ∷ ItemManager → Double → [ItemInstance] → ContainerRecord
observeContainer itemMgr now items = ContainerRecord
    { crItems        = items
    , crStoredWeight = sum (map (itemTotalWeight itemMgr) items)
    , crRevealedAt   = now
    }

-- | Apply a reveal: REPLACE this container's whole record with what was
--   just observed. Never merges with, or partially updates, a previous
--   record — a reveal is a fresh look, not a diff.
recordObservation
    ∷ ItemManager → Double → BuildingId → [ItemInstance]
    → ContainerKnowledge → ContainerKnowledge
recordObservation itemMgr now bid items (ContainerKnowledge m) =
    ContainerKnowledge (HM.insert bid (observeContainer itemMgr now items) m)

-- | Drop one container's record entirely — demolition. Afterwards the
--   container reads as 'NeverInspected' again, so a later building that
--   somehow reused the id could never inherit the old memory.
forgetContainer ∷ BuildingId → ContainerKnowledge → ContainerKnowledge
forgetContainer bid (ContainerKnowledge m) = ContainerKnowledge (HM.delete bid m)

knownContainerIds ∷ ContainerKnowledge → [BuildingId]
knownContainerIds = HM.keys ∘ ckRecords

-- | Keep only the records whose container is in @live@. Used at the
--   LOAD boundary against the page's own restored building set: a
--   record whose building no longer exists is a tolerated, non-blocking
--   diagnostic (a demolished cargo's lingering memory is gameplay, not
--   corruption — the same contract the integrity graph applies to a
--   dangling craft-bill station), scrubbed here rather than rejected.
retainContainers
    ∷ HS.HashSet BuildingId → ContainerKnowledge → ContainerKnowledge
retainContainers live (ContainerKnowledge m) =
    ContainerKnowledge (HM.filterWithKey (\bid _ → HS.member bid live) m)

-- | The ids 'retainContainers' would drop, so the caller can report them
--   as the diagnostic they are instead of scrubbing silently.
prunedContainerIds ∷ HS.HashSet BuildingId → ContainerKnowledge → [BuildingId]
prunedContainerIds live (ContainerKnowledge m) =
    [ bid | bid ← HM.keys m, not (HS.member bid live) ]
