{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
-- | The player's remembered view of a PORTABLE container — a crate, a
--   toolbox, a kit — keyed by the item's own stable
--   'Item.Types.iiInstanceId' (#2512, epic #1231 PLC-7,
--   @docs/portable_loot_containers.md@ D-7\/D-13).
--
--   The building-keyed sibling is "Building.Knowledge", and the two are
--   deliberately NOT one type. A building never moves, is owned by one
--   page, and is revealed by a single all-or-nothing look; a crate is
--   carried between pages and owners, and the player learns about it in
--   TWO independent steps — they pick it up and feel how heavy it is,
--   and separately they open it and see what is inside. That is the one
--   structural difference this module exists for: a portable record
--   carries two SEPARATELY-STAMPED observations rather than one.
--
--   __Session-scoped, never page-scoped.__ The live map is a
--   'World.State.Types.WorldManager' field (D-24), so a record follows
--   its crate across pages and owners without ever being copied on a
--   move — the crate's identity is the key, and nothing about where it
--   currently sits is part of it. "Building.Knowledge" is page-scoped
--   for the opposite reason: its key only means anything on one page.
--
--   Everything here is PURE. The live owner is
--   'World.State.Types.wmPortableKnowledge', located through
--   "World.Item.Locate" and driven from Lua by
--   "Engine.Scripting.Lua.API.Items.Knowledge"; nothing in gameplay
--   observes a container yet (PLC-8 is what calls these from pickup and
--   open).
--
--   __Four states, never conflated__ ('PortableKnowledgeState'):
--   never-inspected (nothing is known), weight-only (it has been
--   hefted, never opened), known-empty (opened, and there was nothing
--   in it) and known-contents. As in the building layer, absence of a
--   record and 'KnownEmpty' are DIFFERENT facts and must never collapse
--   onto each other.
--
--   __Remembered instance ids are historical observations, not live
--   entities__ — the same contract 'Building.Knowledge' documents at
--   length: they are excluded from
--   'World.Save.Snapshot.allItemInstanceIds', the allocator bound, the
--   duplicate-live-id check and live @item_instance@ reference
--   resolution, because staleness REQUIRES a remembered id to stay
--   meaningful after the live item has moved, changed or ceased to
--   exist. Their def names remain ordinary content references validated
--   by 'World.Save.Types.missingItemDefReferences'.
--
--   __Capacity is never remembered.__ It is read live from the located
--   instance's 'Item.Types.iiStorage', exactly as the building layer
--   reads 'Building.Types.bdStorageCapacity' live.
module Item.Knowledge
    ( WeightObservation(..)
    , ContentsObservation(..)
    , PortableRecord(..)
    , PortableKnowledge(..)
    , PortableKnowledgeState(..)
    , emptyPortableKnowledge
    , portableKnowledgeStateId
    , lookupPortable
    , portableRecordState
    , portableState
    , observePortableWeightAt
    , observePortableContentsAt
    , observePortableWeight
    , observePortableContents
    , forgetPortable
    , knownPortableIds
    , retainPortables
    , prunedPortableIds
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Item.Types (ItemInstance(..), ItemManager, itemTotalWeight)

-- | "Somebody weighed this crate, and this is what it came to." The
--   value is the recursive 'itemTotalWeight' of the WHOLE instance —
--   its own empty weight, its fill, and everything nested inside it —
--   which is the number a unit picking the crate up actually feels.
--
--   Note this differs from 'Building.Knowledge.ContainerRecord''s
--   @crStoredWeight@, which is the weight of a building's stored
--   CONTENTS alone: a building is not a thing you can lift, so its own
--   mass is not part of the observation. A crate is.
data WeightObservation = WeightObservation
    { woWeight ∷ !Float
      -- ^ Kilograms, as measured THEN. Stored rather than recomputed on
      --   read, so it can never drift from the observation it records
      --   if an item def's weight or fill weight later changes.
    , woAt     ∷ !Double
      -- ^ GAME-TIME seconds (the 'Engine.Core.State.gameTimeRef' clock
      --   'Building.Knowledge.ContainerRecord''s @crRevealedAt@ also
      --   uses), not the world calendar.
    } deriving (Show, Eq, Generic, Serialize)

-- | "Somebody opened this crate, and this is what was in it." Full
--   instance COPIES taken at observation time — quality, condition,
--   fill, sharpness, nested contents and instance id as they were THEN.
--   Resolving live ids at read time would defeat the whole feature.
data ContentsObservation = ContentsObservation
    { coItems ∷ ![ItemInstance]
      -- ^ Empty ⇒ observed and found empty, which is why the ABSENCE of
      --   this observation, not an empty list, is what "never opened"
      --   means.
    , coAt    ∷ !Double
      -- ^ Game-time seconds, same clock as 'woAt' and independent of
      --   it: a crate hefted long after it was last opened keeps the
      --   older contents stamp.
    } deriving (Show, Eq, Generic, Serialize)

-- | What the player knows about ONE portable container. Both
--   observations are independently optional, and every combination is
--   reachable in play: nothing (a crate merely seen on the ground), a
--   weight alone (picked up, never opened), or both.
--
--   A contents observation always arrives WITH a weight observation
--   (requirement 3: opening records the weight too), so
--   \"contents without weight\" is not producible by
--   'observePortableContentsAt'. It is still representable — a decoded
--   payload could carry it — and every reader treats it honestly:
--   'portableRecordState' answers from the contents, and the projection
--   simply omits the weight fields.
data PortableRecord = PortableRecord
    { prWeight   ∷ !(Maybe WeightObservation)
    , prContents ∷ !(Maybe ContentsObservation)
    } deriving (Show, Eq, Generic, Serialize)

-- | Every portable container the player knows something about, across
--   the WHOLE session. Keyed by 'Item.Types.iiInstanceId', which is
--   stamped once at creation and preserved verbatim through every move.
newtype PortableKnowledge = PortableKnowledge
    { pkRecords ∷ HM.HashMap Word64 PortableRecord
    } deriving stock (Generic)
      deriving newtype (Show, Eq, Serialize)

emptyPortableKnowledge ∷ PortableKnowledge
emptyPortableKnowledge = PortableKnowledge HM.empty

-- | The four observable states, which must never be conflated.
data PortableKnowledgeState
    = NeverInspected
      -- ^ Nothing is known: no record at all, or a record carrying
      --   neither observation.
    | WeightOnly
      -- ^ Hefted, never opened. The player knows roughly how loaded it
      --   is and nothing about what is in it.
    | KnownEmpty
      -- ^ Opened, and there was nothing in it. NOT 'NeverInspected'.
    | KnownContents
    deriving (Show, Eq)

-- | The stable identifier the Lua surface reports. Four DISTINCT
--   strings, and the two the building layer also has
--   ('Building.Knowledge.containerKnowledgeStateId') carry the SAME
--   spellings, so one window renderer consumes either projection.
portableKnowledgeStateId ∷ PortableKnowledgeState → Text
portableKnowledgeStateId NeverInspected = "unknown"
portableKnowledgeStateId WeightOnly     = "weight-only"
portableKnowledgeStateId KnownEmpty     = "empty"
portableKnowledgeStateId KnownContents  = "known"

lookupPortable ∷ Word64 → PortableKnowledge → Maybe PortableRecord
lookupPortable iid = HM.lookup iid ∘ pkRecords

-- | Derive the state from which observations a record carries — the ONE
--   place that mapping lives, so no reader can invent a fifth answer or
--   collapse two of these into each other.
--
--   A CONTENTS observation wins outright: once the player has looked
--   inside, "empty" or "has things in it" is what they know, whatever
--   the weight says. A weight alone is 'WeightOnly'. Neither — including
--   a record that somehow carries neither — is 'NeverInspected'.
portableRecordState ∷ Maybe PortableRecord → PortableKnowledgeState
portableRecordState Nothing = NeverInspected
portableRecordState (Just r) = case prContents r of
    Just c  | null (coItems c) → KnownEmpty
            | otherwise        → KnownContents
    Nothing | isJust (prWeight r) → WeightOnly
            | otherwise           → NeverInspected

portableState ∷ Word64 → PortableKnowledge → PortableKnowledgeState
portableState iid = portableRecordState ∘ lookupPortable iid

-- | Record that this instance was WEIGHED at @now@ — and nothing else.
--   Any existing contents observation is preserved untouched, with its
--   own older stamp: hefting a crate you already opened tells you
--   nothing new about what is inside it.
observePortableWeightAt
    ∷ ItemManager → Double → ItemInstance → PortableRecord → PortableRecord
observePortableWeightAt itemMgr now inst r =
    r { prWeight = Just (WeightObservation (itemTotalWeight itemMgr inst) now) }

-- | Record that this instance was OPENED at @now@: the contents as they
--   are, AND the weight, both stamped @now@.
--
--   Both stamps move because both facts were genuinely just observed —
--   you cannot see inside a crate without also holding it. A later
--   weighing then advances 'prWeight' alone, which is what makes the
--   two stamps diverge in the honest direction (a fresh weight over
--   older contents), never the other way around.
--
--   Takes no prior record, which is how "replaces outright" is spelled
--   in the type rather than left as a rule to remember.
observePortableContentsAt
    ∷ ItemManager → Double → ItemInstance → PortableRecord
observePortableContentsAt itemMgr now inst = PortableRecord
    { prWeight   = Just (WeightObservation (itemTotalWeight itemMgr inst) now)
    , prContents = Just (ContentsObservation (iiContents inst) now)
    }

-- | Apply a weight observation to the whole map, creating the record if
--   this is the first thing ever learned about the instance.
observePortableWeight
    ∷ ItemManager → Double → ItemInstance
    → PortableKnowledge → PortableKnowledge
observePortableWeight itemMgr now inst (PortableKnowledge m) =
    PortableKnowledge $ HM.insert (iiInstanceId inst)
        (observePortableWeightAt itemMgr now inst existing) m
  where existing = fromMaybe emptyRecord (HM.lookup (iiInstanceId inst) m)

-- | Apply a contents observation to the whole map. REPLACES the record
--   outright — an open is a fresh look, never a diff against what was
--   remembered before.
observePortableContents
    ∷ ItemManager → Double → ItemInstance
    → PortableKnowledge → PortableKnowledge
observePortableContents itemMgr now inst (PortableKnowledge m) =
    PortableKnowledge $ HM.insert (iiInstanceId inst)
        (observePortableContentsAt itemMgr now inst) m

emptyRecord ∷ PortableRecord
emptyRecord = PortableRecord Nothing Nothing

-- | Drop one instance's record entirely. Afterwards it reads as
--   'NeverInspected' again.
forgetPortable ∷ Word64 → PortableKnowledge → PortableKnowledge
forgetPortable iid (PortableKnowledge m) = PortableKnowledge (HM.delete iid m)

knownPortableIds ∷ PortableKnowledge → [Word64]
knownPortableIds = HM.keys ∘ pkRecords

-- | Keep only the records whose instance is in @live@ — the load
--   boundary's scrub, run against the REPLACEMENT session's own
--   complete live item enumeration (never the outgoing session's, which
--   is being discarded, and never one page's: a crate is session-scoped
--   and may sit on any page).
--
--   A record whose instance is gone is a tolerated, non-blocking
--   diagnostic rather than a load failure, exactly as
--   'Building.Knowledge.retainContainers' is for a demolished
--   container: it has no surface left to be cleared from, so nothing
--   would ever clear it.
retainPortables ∷ HS.HashSet Word64 → PortableKnowledge → PortableKnowledge
retainPortables live (PortableKnowledge m) =
    PortableKnowledge (HM.filterWithKey (\iid _ → HS.member iid live) m)

-- | The ids 'retainPortables' would drop, so the caller can report them
--   as the diagnostic they are instead of scrubbing silently.
prunedPortableIds ∷ HS.HashSet Word64 → PortableKnowledge → [Word64]
prunedPortableIds live (PortableKnowledge m) =
    [ iid | iid ← HM.keys m, not (HS.member iid live) ]
