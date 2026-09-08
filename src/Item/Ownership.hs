{-# LANGUAGE Strict #-}
-- | The nested ownership-move boundary (#2487, epic #1231 PLC-4;
--   design authority @docs/portable_loot_containers.md@ D-1, D-4, D-5,
--   D-16).
--
--   ONE pure policy decides whether an 'ItemInstance' may be inserted
--   into, or removed from, another instance's 'iiContents'. Every
--   production path that MOVES nested ownership goes through here.
--   Materialization ("Item.Materialize"), save reconstruction
--   ("World.Save.Component.PageActivity"), temperature relaxation
--   ("Item.Temperature") and the two medical kit draws
--   ("Engine.Scripting.Lua.API.Units.Medical") write 'iiContents'
--   without being moves — they mint, rebuild, re-value or CONSUME a
--   tree rather than re-owning an instance — and are the only other
--   permitted writers. The @Item.Ownership structural writer guard@
--   hspec group holds that line.
--
--   __Pure and EngineEnv-free__, the same shape "Unit.Transfer" has:
--   the caller projects the live managers into an 'OwnershipScene' and
--   applies the resulting list. That is deliberate — PLC-8 (unit-
--   mediated opening, capacity-aware pickup) and PLC-9 (the item-
--   container transfer endpoints) are the first production callers,
--   and both need the verdict before they touch state.
--
--   __The flat precedent this generalizes.__ 'Unit.Transfer' resolves
--   an exact instance by identity, weighs the ACTUAL instance, refuses
--   over capacity and records the source index so a rollback splices
--   the instance back where it was. All of that holds here; what is
--   new is depth. A nested tree has a weight-bearing ancestor CHAIN
--   above the target, it can be made cyclic, and the same instance id
--   can be made to appear twice. Those three are what this module adds
--   to the flat contract.
--
--   __Absence fails closed__ (the signed-off rule for this slice, and
--   the answer 'Item.Types' deferred here). A target whose 'iiStorage'
--   is 'Nothing' declares no internal capacity, so it accepts no
--   insert — that is every shipped kit and toolbox today, none of
--   which authors @storage:@. A candidate whose 'iiBulk' is 'Nothing'
--   has no external bulk to charge, so it is never a candidate. The
--   same reasoning reaches the target's existing contents and its
--   ancestor chain: a sum that cannot be computed is not a sum that
--   passes. Each of those is its OWN refusal, because PLC-9 surfaces
--   the reason verbatim and "it didn't fit" would be a lie.
--
--   __Removal has no capacity__, deliberately. Taking a bandage out of
--   a first-aid kit is legal even though putting one back is not; the
--   asymmetry is D-30's, and it is why 'restoreRemoval' exists as a
--   distinct entry point from 'insertInstance' — see its haddock.
module Item.Ownership
    ( -- * Refusal vocabulary
      OwnershipRefusal(..)
    , ownershipRefusalId
    , allOwnershipRefusals
      -- * The tree a move is evaluated against
    , RootCapacity(..)
    , OwnershipScene(..)
    , sceneLoad
      -- * Tree queries
    , findInstance
    , subtreeIds
    , treeIds
      -- * Removal
    , OwnershipRemoval(..)
    , removeInstance
    , restoreRemoval
      -- * Insertion
    , insertInstance
      -- * The two composed
    , OwnershipMove(..)
    , moveInstance
    ) where

import UPrelude
import qualified Data.HashSet as HS
import Item.Types
    ( ItemInstance(..), ItemStorage(..) )

-- | Why a nested ownership move was refused. Every constructor is a
--   DISTINCT, reportable reason: PLC-9's transfer endpoints surface
--   these verbatim rather than collapsing them into one failure, so
--   two refusals that a player would act on differently must never
--   share a constructor.
--
--   Constructor ORDER is not a wire format — nothing here is
--   serialized — but 'ownershipRefusalId' is the stable spelling a
--   Lua or UI surface may key on, exactly as
--   'Unit.Transfer.transferReasonId' is for that policy.
data OwnershipRefusal
    = NoSuchInstance
      -- ^ No instance with the requested id exists anywhere in the
      --   tree (requirement 8).
    | NoSuchTarget
      -- ^ The destination container id resolves to nothing in the
      --   tree. Distinct from 'NoSuchInstance' so a caller can tell
      --   which half of a move went stale.
    | TargetNotStorage
      -- ^ The destination's 'iiStorage' is 'Nothing': it declares no
      --   internal capacity, so it accepts no insert (fail closed).
    | CandidateNoBulk
      -- ^ The candidate's 'iiBulk' is 'Nothing': it has no external
      --   bulk to charge against the destination (fail closed).
    | ContentsBulkUnknown
      -- ^ A direct child ALREADY in the destination has no 'iiBulk',
      --   so the destination's used bulk cannot be computed. Distinct
      --   from 'CandidateNoBulk': nothing is wrong with the candidate,
      --   and the caller's repair is different.
    | AncestorNotStorage
      -- ^ An ancestor of the destination declares no 'iiStorage', so
      --   the weight this move adds to it clears no stated limit
      --   (fail closed). Only reachable for a tree assembled outside
      --   this boundary.
    | OverTargetWeight
      -- ^ The destination's own 'isWeightCapacity' would be exceeded
      --   by the recursive weight of its direct children after the
      --   move (requirement 3).
    | OverTargetBulk
      -- ^ The destination's own 'isBulkCapacity' would be exceeded by
      --   the external bulk of its direct children after the move
      --   (requirement 3). Bulk is charged at the immediate parent
      --   ONLY (D-5).
    | OverAncestorWeight
      -- ^ A weight-bearing ancestor ABOVE the destination would be
      --   over its own 'isWeightCapacity' (requirement 4). Distinct
      --   from 'OverTargetWeight': the destination itself has room.
    | OverRootCapacity
      -- ^ The root owner (the carrying unit, the holding building)
      --   would be over its weight capacity (requirement 5).
    | WouldCycle
      -- ^ The destination IS the moved instance, or sits inside it, so
      --   the move would make the tree cyclic (requirement 6).
    | DuplicateInstanceId
      -- ^ An id in the moved subtree — its root or any descendant —
      --   already exists in the destination tree (requirement 6).
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | The stable external spelling of a refusal.
ownershipRefusalId ∷ OwnershipRefusal → Text
ownershipRefusalId r = case r of
    NoSuchInstance      → "no_such_instance"
    NoSuchTarget        → "no_such_target"
    TargetNotStorage    → "target_not_storage"
    CandidateNoBulk     → "candidate_no_bulk"
    ContentsBulkUnknown → "contents_bulk_unknown"
    AncestorNotStorage  → "ancestor_not_storage"
    OverTargetWeight    → "over_target_weight"
    OverTargetBulk      → "over_target_bulk"
    OverAncestorWeight  → "over_ancestor_weight"
    OverRootCapacity    → "over_root_capacity"
    WouldCycle          → "would_cycle"
    DuplicateInstanceId → "duplicate_instance_id"

-- | Every refusal, in constructor order. Requirement 9's
--   enumerability: a surface that renders reasons can be checked
--   exhaustive against this rather than against a hand-kept list.
allOwnershipRefusals ∷ [OwnershipRefusal]
allOwnershipRefusals = [minBound .. maxBound]

-- | The root owner's weight limit, in the sense @Unit.Transfer.fits@
--   already uses.
data RootCapacity
    = RootLimited !Float
      -- ^ Kilograms the owner can hold, TOTAL. @0@ is "the owner has
      --   no such stat", which reads as no room at all and refuses
      --   every insert — the same reading @uevCapacity 0@ has in
      --   @Unit.Transfer.fits@, not "unlimited".
    | RootUnlimited
      -- ^ No capacity concept at all: the ground. Not weight-checked.
      --   Deliberately a CONSTRUCTOR rather than a large number, so
      --   "unlimited" can never be confused with a big limit and a
      --   'RootLimited' 0 can never be confused with unlimited.
    deriving (Show, Eq)

-- | Everything the policy reads about one root owner, captured at one
--   instant.
--
--   The scene must describe the owner's COMPLETE load, not only the
--   sub-tree being edited: a unit whose equipment and accessories
--   already fill its capacity has no room for a nested insert either.
--   'oscItems' carries the ownership tree this move edits and
--   'oscOther' everything else the owner is already carrying, so
--   'sceneLoad' is the whole measure.
data OwnershipScene = OwnershipScene
    { oscItems    ∷ ![ItemInstance]
      -- ^ The root owner's own top-level items — a unit's inventory, a
      --   building's loose storage, a ground pile. The ownership tree
      --   this move reads and rewrites.
    , oscOther    ∷ ![ItemInstance]
      -- ^ Everything else the owner carries that this move never
      --   touches: equipment slots and accessories for a unit, empty
      --   for a building or the ground. Weighed into the carrier load
      --   and searched by NOTHING — an id here is not resolvable and
      --   not a duplicate, because it is not part of this ownership
      --   tree.
    , oscCapacity ∷ !RootCapacity
    , oscWeigh    ∷ !(ItemInstance → Float)
      -- ^ 'Item.Types.itemTotalWeight' partially applied to the live
      --   'Item.Types.ItemManager', exactly as
      --   'Unit.Transfer.tscWeigh' is: fill and nested contents count
      --   recursively, so a stocked kit weighs its bandages.
    }

-- | The owner's complete carried weight for a given tree: the edited
--   tree plus everything else the owner holds.
sceneLoad ∷ OwnershipScene → [ItemInstance] → Float
sceneLoad scene items =
    sum (map (oscWeigh scene) items)
      + sum (map (oscWeigh scene) (oscOther scene))

-- * Tree queries

-- | The instance with this id, searched depth-first through the whole
--   forest. Sibling subtrees ARE searched — contents are an ordinary
--   recursive list with no id index — but only the matched node and
--   its ancestor path are ever REWRITTEN (requirement 8).
findInstance ∷ Word64 → [ItemInstance] → Maybe ItemInstance
findInstance iid = go
  where
    go []       = Nothing
    go (i : is)
        | iiInstanceId i ≡ iid = Just i
        | otherwise            = case go (iiContents i) of
            Just found → Just found
            Nothing    → go is

-- | Every instance id in one subtree, its root included.
subtreeIds ∷ ItemInstance → HS.HashSet Word64
subtreeIds i = HS.insert (iiInstanceId i) (treeIds (iiContents i))

-- | Every instance id in a whole forest, at every depth.
treeIds ∷ [ItemInstance] → HS.HashSet Word64
treeIds = foldl' (\acc i → HS.union acc (subtreeIds i)) HS.empty

-- | Is @needle@ the root of, or anywhere inside, @holder@'s subtree?
--   Answered from the VALUE, so it serves both cycle checks: the
--   pre-detach one in 'moveInstance', which resolves @holder@ from the
--   original tree, and 'insertInstance''s, whose candidate may be
--   attached to no tree at all.
withinSubtree ∷ Word64 → ItemInstance → Bool
withinSubtree needle holder = HS.member needle (subtreeIds holder)

-- * Removal

-- | A completed removal, and everything a rollback needs to undo it.
--
--   Generalizes 'Unit.Transfer.TransferPlan''s flat @tpIndex@: a
--   nested source is an index WITHIN a named parent, so restoring it
--   needs both.
data OwnershipRemoval = OwnershipRemoval
    { orInstance ∷ !ItemInstance
      -- ^ The exact value removed — same 'iiInstanceId', same
      --   descendants, in the same authored order (requirement 2).
    , orParent   ∷ !(Maybe Word64)
      -- ^ The container it came out of, or 'Nothing' when it was a
      --   top-level item of the root owner.
    , orIndex    ∷ !Int
      -- ^ Its index within that parent's 'iiContents' (or within the
      --   root list). Order is gameplay- and UI-visible, so
      --   "unchanged" means order-preserving, not same-multiset.
    , orItems    ∷ ![ItemInstance]
      -- ^ The root item list AFTER the removal.
    } deriving (Show, Eq)

-- | Take one instance out of the tree by exact id, at whatever depth it
--   sits. No capacity applies to a removal — see the module haddock.
--
--   Siblings are searched but never altered: the rewrite touches the
--   matched node's parent and that parent's own ancestor path, and
--   nothing else.
removeInstance ∷ OwnershipScene → Word64 → Either OwnershipRefusal OwnershipRemoval
removeInstance scene iid = case takeFrom Nothing (oscItems scene) of
    Nothing → Left NoSuchInstance
    Just (found, parent, ix, items') → Right OwnershipRemoval
        { orInstance = found
        , orParent   = parent
        , orIndex    = ix
        , orItems    = items'
        }
  where
    -- Depth-first: the shallowest match in the earliest branch wins.
    -- Ids are unique by construction, so "shallowest earliest" and
    -- "the one match" coincide for every tree this boundary accepts.
    takeFrom parent = go 0
      where
        go _  [] = Nothing
        go ix (i : is)
            | iiInstanceId i ≡ iid = Just (i, parent, ix, is)
            | otherwise = case takeFrom (Just (iiInstanceId i)) (iiContents i) of
                Just (found, p, cix, kids') →
                    Just (found, p, cix, i { iiContents = kids' } : is)
                Nothing → case go (ix + 1) is of
                    Just (found, p, six, rest') → Just (found, p, six, i : rest')
                    Nothing                     → Nothing

-- | Undo a removal: splice the instance back at the exact parent and
--   index it came from.
--
--   __This is NOT an insert, and deliberately checks no capacity.__ An
--   ordinary remove-then-reinsert round trip goes through
--   'insertInstance' and therefore needs an insertion-eligible parent;
--   a rollback after a downstream failure must restore the exact
--   arrangement that existed BEFORE the removal even when that parent
--   would refuse an insert — a first-aid kit with no @storage:@ is
--   exactly that case, and refusing to put the bandage back would turn
--   a failed transaction into a lost item.
--
--   The guarantee is against the CORRESPONDING post-removal snapshot:
--   given 'orItems' (or a tree still holding that parent at that
--   depth) this restores the pre-removal tree exactly. It promises
--   nothing about reconciling arbitrary intervening mutations — a
--   caller that mutated the tree between the removal and the rollback
--   owns that reconciliation itself. Only a vanished parent is
--   reported ('NoSuchTarget'); an index past the end of a parent that
--   shrank appends rather than failing, which keeps a rollback from
--   dropping the instance.
restoreRemoval ∷ OwnershipRemoval → [ItemInstance]
               → Either OwnershipRefusal [ItemInstance]
restoreRemoval removal items = case orParent removal of
    Nothing  → Right (spliceAt (orIndex removal) (orInstance removal) items)
    Just pid → case rewriteInstance pid addTo items of
        Nothing     → Left NoSuchTarget
        Just items' → Right items'
  where
    addTo p = p { iiContents =
        spliceAt (orIndex removal) (orInstance removal) (iiContents p) }

-- | Insert @x@ at index @ix@, appending when the index is past the end.
spliceAt ∷ Int → a → [a] → [a]
spliceAt ix x xs
    | ix ≤ 0    = x : xs
    | otherwise = case xs of
        []        → [x]
        (y : ys)  → y : spliceAt (ix - 1) x ys

-- | Rewrite exactly the instance with this id, wherever it sits, and
--   nothing else. 'Nothing' when no such instance exists.
rewriteInstance ∷ Word64 → (ItemInstance → ItemInstance)
                → [ItemInstance] → Maybe [ItemInstance]
rewriteInstance iid f = go
  where
    go [] = Nothing
    go (i : is)
        | iiInstanceId i ≡ iid = Just (f i : is)
        | otherwise = case go (iiContents i) of
            Just kids' → Just (i { iiContents = kids' } : is)
            Nothing    → (i :) <$> go is

-- * Insertion

-- | Put an instance INTO a container already in the tree, subject to
--   every limit in requirements 3 through 6.
--
--   The candidate must not already be in this tree: an id anywhere in
--   its subtree that also appears in the destination is
--   'DuplicateInstanceId'. To relocate an instance that IS in the tree,
--   use 'moveInstance', which detaches it first so the final
--   arrangement — not a tree briefly holding it twice — is what gets
--   measured.
--
--   Checks run in a fixed order so the reported reason is the most
--   specific true one: structural resolution, then fail-closed
--   absence, then identity, then capacity from the inside out
--   (destination, ancestors, root).
insertInstance ∷ OwnershipScene → Word64 → ItemInstance
               → Either OwnershipRefusal [ItemInstance]
insertInstance scene targetId candidate = do
    -- Cycle first: a destination inside the candidate is a cycle
    -- whether or not it resolves in this tree, and saying "no such
    -- target" for it would be misleading.
    when (withinSubtree targetId candidate) (Left WouldCycle)
    (target, above) ← maybe (Left NoSuchTarget) Right
                            (ancestorChain targetId (oscItems scene))
    storage ← maybe (Left TargetNotStorage) Right (iiStorage target)
    candidateBulk ← maybe (Left CandidateNoBulk) Right (iiBulk candidate)
    -- Duplicate: every id in the moved subtree against the ENTIRE
    -- destination tree, not just its root against the target's
    -- children. A unique crate holding a bandage whose id is already
    -- in the owner's pack is still a duplicate.
    let present = treeIds (oscItems scene)
    when (any (`HS.member` present) (HS.toList (subtreeIds candidate)))
         (Left DuplicateInstanceId)
    -- Bulk, at the immediate parent only (D-5): a container's external
    -- bulk is fixed, so filling it consumes nothing further above.
    siblingBulk ← maybe (Left ContentsBulkUnknown) Right
                        (sumBulk (iiContents target))
    when (siblingBulk + candidateBulk > isBulkCapacity storage)
         (Left OverTargetBulk)
    -- Weight, recursively, at the destination itself…
    let moved      = oscWeigh scene candidate
        targetLoad = sum (map (oscWeigh scene) (iiContents target))
    when (targetLoad + moved > isWeightCapacity storage)
         (Left OverTargetWeight)
    -- …and at every weight-bearing ancestor above it (requirement 4):
    -- the immediate parent alone fitting proves nothing.
    mapM_ (checkAncestor scene moved) above
    items' ← maybe (Left NoSuchTarget) Right
                   (rewriteInstance targetId push (oscItems scene))
    -- The carrier last: it is the only limit outside the tree.
    checkRoot scene items'
    pure items'
  where
    push t = t { iiContents = iiContents t ⧺ [candidate] }

-- | One weight-bearing ancestor, revalidated with the moved subtree's
--   recursive weight added to what it already holds.
checkAncestor ∷ OwnershipScene → Float → ItemInstance
              → Either OwnershipRefusal ()
checkAncestor scene moved ancestor = do
    storage ← maybe (Left AncestorNotStorage) Right (iiStorage ancestor)
    let held = sum (map (oscWeigh scene) (iiContents ancestor))
    when (held + moved > isWeightCapacity storage) (Left OverAncestorWeight)

-- | The root owner's own limit, measured against the POST-move tree so
--   a relocation within one owner nets to zero.
checkRoot ∷ OwnershipScene → [ItemInstance] → Either OwnershipRefusal ()
checkRoot scene items' = case oscCapacity scene of
    RootUnlimited     → Right ()
    RootLimited limit
        | limit ≤ 0                    → Left OverRootCapacity
        | sceneLoad scene items' > limit → Left OverRootCapacity
        | otherwise                    → Right ()

-- | Σ external bulk of these direct children, or 'Nothing' when any of
--   them has none to contribute — an unknowable sum fails closed
--   rather than under-counting.
sumBulk ∷ [ItemInstance] → Maybe Float
sumBulk = foldl' step (Just 0)
  where
    step acc i = (+) <$> acc <*> iiBulk i

-- | The instance with this id, paired with its ancestors innermost
--   first: @(target, [parent, grandparent, …])@ up to (but not
--   including) the root owner, which has no 'iiStorage' of its own and
--   is checked separately by 'checkRoot'. 'Nothing' when the id is not
--   in the forest.
ancestorChain ∷ Word64 → [ItemInstance] → Maybe (ItemInstance, [ItemInstance])
ancestorChain iid = go
  where
    go [] = Nothing
    go (i : is)
        | iiInstanceId i ≡ iid = Just (i, [])
        | otherwise = case go (iiContents i) of
            Just (t, above) → Just (t, above ⧺ [i])
            Nothing         → go is

-- * The two composed

-- | A completed relocation within one root owner.
data OwnershipMove = OwnershipMove
    { omInstance ∷ !ItemInstance
      -- ^ The exact value that moved — identity and descendants
      --   preserved (requirement 2).
    , omRemoval  ∷ !OwnershipRemoval
      -- ^ The removal half, kept whole so a downstream failure can
      --   'restoreRemoval' it against 'orItems'.
    , omItems    ∷ ![ItemInstance]
      -- ^ The root item list after remove AND insert.
    } deriving (Show, Eq)

-- | Relocate an instance already in this owner's tree into a container
--   also in it.
--
--   Detach, THEN measure. Requirements 4 through 6 are about the FINAL
--   arrangement, so an ancestor shared by source and destination must
--   not be charged for the subtree twice, the carrier's net load must
--   not double-count it, and the instance's own id must not read as a
--   duplicate of itself. Removing first makes all three fall out:
--   every later check sees a tree that no longer holds the subtree.
--
--   The cycle is decided BEFORE the detach, against the instance as it
--   stands in the original tree (correction to requirements 4-6). A
--   self or descendant target must report 'WouldCycle', and the tree it
--   is resolved against has to be the one that still holds both ends of
--   the move — after the detach the destination is simply gone from it.
--
--   'insertInstance' independently rejects a target inside the
--   CANDIDATE VALUE, which is a genuinely different check: it needs no
--   tree at all, so it also covers a candidate arriving from outside
--   this owner. Neither subsumes the other, and the two agreeing on a
--   verdict is the point rather than a redundancy.
moveInstance ∷ OwnershipScene → Word64 → Word64
             → Either OwnershipRefusal OwnershipMove
moveInstance scene movedId targetId = do
    moved ← maybe (Left NoSuchInstance) Right
                  (findInstance movedId (oscItems scene))
    when (withinSubtree targetId moved) (Left WouldCycle)
    removal ← removeInstance scene movedId
    let after = scene { oscItems = orItems removal }
    items' ← insertInstance after targetId (orInstance removal)
    pure OwnershipMove
        { omInstance = orInstance removal
        , omRemoval  = removal
        , omItems    = items'
        }
