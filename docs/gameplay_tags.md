# Gameplay tags: registry and query contract

Status: the pure foundation (#2700, SCN-02 of epic #2698), and live
ownership, lifecycle and persistence for units, buildings and locations
(#2701, SCN-03; see [Live tags](#live-tags-units-buildings-and-locations)).
Plants, items, tiles and Lua access are later work (see
[Not yet integrated](#not-yet-integrated)).

Gameplay tags are optional, many-to-many labels that scripts attach to
units, buildings, locations, plants, physical items, and map tiles.
A tag has no gameplay effect of its own. The tag system stores
assignments and answers membership queries; scripts decide what a
`meeting_point` or `defender` means. Tags carry no uniqueness, no
required role, and no implicit behaviour, and whole worlds cannot be
tagged.

The design decisions behind this contract are in
[predefined_arena_worlds_design.md](designs/predefined_arena_worlds_design.md)
(D-29 and D-32 to D-42). This page is the implementation contract and
does not depend on that document.

## Modules

| Module | Owns |
|---|---|
| `Gameplay.Tags.Tag` | `GameplayTag` and `NonEmptySet` (re-exported by `Gameplay.Tags.Types`) |
| `Gameplay.Tags.Index` | `CategoryIndex`, one category's forward and reverse maps and their one mutation |
| `Gameplay.Tags.Types` | the typed targets and `TagCategory` |
| `Gameplay.Tags.Registry` | `TagRegistry`, the mutations, inspection, rebuild, invariant check |
| `Gameplay.Tags.Query` | `TagQuery`, `TagFilter`, universes/results (`TargetSets`), evaluation |
| `Gameplay.Tags.Live` | the internal edit, inspect and query operations over the live owners (#2701), physical items included (#2703) |
| `Gameplay.Tags.ItemLifecycle` | forgetting physical-item tags when items actually cease to exist (#2703) |

The first five are pure. `Gameplay.Tags.Tag` and `Gameplay.Tags.Index`
import no object identity, which is what lets the unit and building
managers and the location table hold an index without an import cycle.
`Gameplay.Tags.Live` is the only module that reads or edits live
assignments; `Gameplay.Tags.ItemLifecycle` only removes item
assignments, at the production sites that destroy items.

## Tags are not faction tags

`GameplayTag` is a separate opaque type from
`Unit.Faction.Profile.FactionTag`. Faction tags decide hostility and
alliance. Gameplay tags decide nothing, so neither type can be passed
where the other is expected, and the two share no validation code.
`mkGameplayTag` refuses only the empty string. Any other text is a
valid label, and the tag system never interprets a tag's spelling.

## Targets and identity scoping

A `TagTarget` names both the category and the identity, so unit 12
and building 12 are different targets.

| Category | Target | Key | Scope | Refused |
|---|---|---|---|---|
| Units | `TargetUnit` | `UnitId` | process-global | — |
| Buildings | `TargetBuilding` | `BuildingId` | process-global | — |
| Locations | `TargetLocation` | `LocationTarget` (`WorldPageId`, `LocationInstanceId`) | per page (the allocator restarts at 1 on each page) | — |
| Plants | `TargetPlant` | `PlantTarget` (`WorldPageId`, `FloraInstanceId`) | per page (the planted-id cursor is per page) | `floraInstanceIdNone` |
| Items | `TargetItem` | `ItemTarget` (`iiInstanceId`) | process-global | 0 (no identity) |
| Tiles | `TargetTile` | `TileTarget` (`WorldPageId`, canonical `gx`, `gy`, `z`) | per page | — |

A plant target is one real flora occurrence, never a species id.
`mkPlantTarget` refuses the reserved non-identity that the crop-plot
adapter uses. `mkItemTarget` refuses 0 because the item counter starts
at 1, and `Item.Types.itemMatches` treats only a positive id as an
identity.

The only way to build a `TileTarget` is `mkTileTarget canon page gx gy z`.
It passes the raw coordinate through
`World.Generate.Coordinates.canonicalTileFrameWith canon`. `canon` is
normally `World.Chunk.Residency.canonicalChunkCoord params` for the
page, which is arena-aware, as the tile-coordinate seam contract
requires (engine contracts §Tile-coordinate seam frame). As a result,
every seam alias of one physical tile produces the same target and the
same single registry entry.

## Storage and index invariants

The registry keeps each category in its own ordered index pair:

- **Forward** — `Map key (NonEmptySet GameplayTag)`, target → tags.
  This is the authority.
- **Reverse** — `Map GameplayTag (NonEmptySet key)`, tag → targets.
  It is derived from the forward map and exists so a tag's members are
  one lookup away.

Both maps are `Data.Map.Strict` balanced trees: lookup, insertion, and
deletion take O(log n) comparisons in the index's size.

The invariants:

1. **Sparse.** Only tagged targets have a forward entry, and only tags
   in use have a reverse entry. No object or tile record carries a tag
   field, pointer, or optional list.
2. **No empty records.** `NonEmptySet`'s constructor is not exported,
   so an empty set cannot enter either map. When a mutation removes a
   target's last tag, it deletes that forward entry. When it removes a
   tag's last member, it deletes that reverse entry.
3. **Coherence.** Every mutation reduces to setting one target's
   complete tag set, which updates both directions together. For every
   registry, the reverse index equals the one derived from the forward
   assignments. `registryInvariantViolations` checks this, and the
   public API cannot violate it.

## Operations

| Function | Effect |
|---|---|
| `addTags` / `addTag` | Union the tags into the target's set. Repeats are idempotent. |
| `removeTags` / `removeTag` | Remove those tags. Absent tags are ignored. The last removal deletes the entry. |
| `replaceTags` | Make the given set the target's complete set. The empty set clears the target. |
| `clearTags` | Remove the target's entry and its reverse memberships. |
| `tagsOf`, `hasTag` | Inspect one target. An untagged target has the empty set. |
| `assignments` | The authoritative forward assignments, in result order. |
| `fromAssignments` | Rebuild from forward assignments only, deriving every reverse index. |
| `registryCounts` | Forward entries, reverse entries, and memberships per category. |

A mutation touches only its own target. Other targets that share the
same tags keep them. `fromAssignments` rebuilds a whole registry from
forward assignments only, so a caller cannot supply a competing reverse
index; the persistence layer rebuilds each owner's index the same way,
with `Gameplay.Tags.Index.categoryFromAssignments`. A target that appears
more than once receives the union of its sets, and empty sets add
nothing. For every registry `r`,
`fromAssignments (assignments r) ≡ r`. This is an in-memory
foundation, not a save codec.

## Queries

A query is a typed expression:

```haskell
data TagQuery
    = QMatch TagFilter            -- tag-set leaf
    | QUniverse                   -- the whole selected universe
    | QUnion TagQuery TagQuery
    | QIntersection TagQuery TagQuery
    | QDifference TagQuery TagQuery
    | QComplement TagQuery        -- relative to the selected universe
```

`TagFilter` is the `all`/`any`/`none` shorthand. A target matches when
it carries every `all` tag, at least one `any` tag, and no `none` tag.
Nonempty groups combine conjunctively, and an empty group imposes no
restriction, so the empty filter matches the whole universe. The
groups are sets, so a repeated tag counts once. A tag nobody carries
denotes the empty membership set: in `all` or `any` it matches nothing,
and in `none` it excludes nothing. `tagged t` is the one-tag filter,
and `unionQueries` and `intersectQueries` fold lists.

Evaluation runs entirely in Haskell over the reverse indexes, using
`Data.Set` operations. There is no string syntax to parse and no
persistent cache.

### The universe is the caller's

The registry knows only tagged targets, but a query ranges over the
objects that exist. The caller therefore supplies a `QueryUniverse`
with every call: one ordered set per category, filled by the objects'
owners. A category left empty is not selected. Build one with
`unitUniverse`, `buildingUniverse`, `locationUniverse`,
`plantUniverse`, `itemUniverse`, or `tileUniverse`, combine them with
`<>` to select several categories, or narrow one with
`selectCategories`.

- Complement and every other result are relative to that universe, so
  untagged objects appear in negative results.
- Every result is a subset of the universe. A positive lookup is also
  clipped to it, so a tag held in an unselected category, or by a
  target outside the universe, never appears in the result.
- Evaluation adds no registry entries and does not store the universe.

For tiles, the universe is the set of currently loaded tiles. An
unloaded tagged tile drops out of results and returns when its chunk is
loaded again. Its assignment stays in the registry, because unloading
is not deletion.

### Example

A is tagged `defender`, B is tagged `reserve`, and C has no tags:

```haskell
queryList r (unitUniverse (Set.fromList [a, b, c]))
            (QComplement (tagged defender))
  -- [TargetUnit b, TargetUnit c]
```

"Units minus defenders" returns B and C. C has no registry entry, and
it matches because it is in the universe.

### Result modes and ordering

| Function | Result |
|---|---|
| `evaluateQuery` | The native result, as one `Set` per category. |
| `queryList` | Every match, complete and duplicate-free, never truncated. |
| `queryCount` | The sum of the native result sets' sizes. No list is built. |
| `queryExists` | Evaluates category by category and stops at the first nonempty one. |

For the same registry, universe, and expression, the count equals the
list's length and exists equals the list's nonemptiness. An empty
result is `[]`, `0`, and `False`.

Lists follow a stable order: category first (units, buildings,
locations, plants, items, tiles, which is `TagCategory`'s declaration
order), then the key's `Ord` within a category. This is `TagTarget`'s
derived `Ord`.

No positive tag is required, no spatial bound is imposed, and results
are not truncated. A broad query such as a complement over a million
objects is legal, but it needs temporary native storage proportional to
its result. Choosing useful filters is the script's responsibility.

## Memory

The registry's retained heap scales with actual assignments. An
untagged object costs the registry nothing. The registry does have a
fixed overhead: the six empty index pairs. It is not zero, and this
page does not claim a fixed byte cost beyond what was measured. Each
membership (target, tag) costs space in both indexes: a forward-set
element and a reverse-set element, plus a map node for each distinct
target and each distinct tag.

Tag text is not interned. The registry keeps a reference to whichever
`Text` value it receives. The forward set and the reverse key share
that value, so text that scripts or authored data already own costs
only the reference. Text that nothing else holds is retained by the
registry.

Measured numbers, the reproduction command, and limitations are in
[pr-proofs/issue-2700-gameplay-tags-memory.md](pr-proofs/issue-2700-gameplay-tags-memory.md).
The measurement is opt-in and runs inside the headless suite:

```sh
SYNARCHY_TAG_MEMORY=1 cabal test synarchy-test-headless \
  --test-options='--match "Gameplay.Tags memory" +RTS -T -RTS'
```

## Live tags: units, buildings and locations

Since #2701 the unit, building and location assignments are held by the
state that owns the objects, following capability inventory §6.4(a).
No instance carries a tag field, and `EngineEnv` gained no field.

| Category | Owner field | Key |
|---|---|---|
| Units | `UnitManager.umTags` | `UnitId` (global) |
| Buildings | `BuildingManager.bmTags` | `BuildingId` (global) |
| Locations | `LocationInstances.lisTags`, in each page's `WorldGenParams` | the page-local `LocationInstanceId` |

Each is a `CategoryIndex`: the forward map is the authority and the
reverse map is derived. A page owns its own location table, so two
pages' location 7 are different keys by construction. A query lifts
each page's keys to `LocationTarget` (`mapCategoryKeysMonotonic`) and
assembles one registry with `registryFromCategories`.

### Operations and synchronization

`Gameplay.Tags.Live` is the internal Haskell API. It has no Lua
binding; SCN-06 owns the public verbs.

- `mutateUnitTags`, `mutateBuildingTags` and `mutateLocationTags` apply
  a `TagEdit` (add, remove, replace, clear) as one `atomicModifyIORef'`
  of the owning ref. Every in-session writer of those refs already uses
  `atomicModifyIORef'`, so an edit cannot lose a concurrent update. The
  same modify checks that the target exists: an edit naming a missing
  unit, building or location answers `TagEditNoTarget` and creates
  nothing.
- A location edit resolves its page once through `wmWorlds` and
  modifies that page's `wsGenParamsRef`. Every page initialization
  builds a fresh `WorldState`, so an edit that resolved a page before it
  was destroyed or replaced lands on the departed incarnation and never
  reaches the replacement.
- `unitTagsIn`, `buildingTagsIn` and `locationTagsIn` inspect one
  target, returning `Nothing` for a target that does not exist.
- `captureLiveTagView` reads each owner ref once. Within a category the
  two maps and the query universe come from one value, so a query never
  sees an index pair mid-update or a tag outside its universe. Across
  categories and pages the reads are separate, the same guarantee every
  other cross-manager read has. `runLiveTagQuery` evaluates against
  that view, with every existing target, tagged or not, in the universe.

### Lifecycle

| Event | Effect on tags |
|---|---|
| A unit moves, changes pose, enters the dead pose, or is otherwise changed in place | kept: the instance still exists |
| A building is changed in place | kept |
| A location moves through any lifecycle state (hinted, discovered, active, cleared, depleted) | kept: an in-place change to a surviving instance |
| `UnitDestroy`, `BuildingDestroy` | that target's entry leaves both maps in the same modify that removes the instance; other targets keep shared tag names |
| `UnitClearAll`, `BuildingClearAll` | every unit or building entry goes |
| A page is destroyed or replaced (`world.destroy`, `world.init`/`world.initArena` over a live id) | `retirePageUnits`/`retirePageBuildings` remove the old incarnation's entries immediately, under the lifecycle lock, and again in the queued cutoff-bounded clear; entities admitted at or above the cutoff keep theirs. The page's location table, tags and all, goes with its `WorldState` |
| Destroy-all (exit to menu) | every page's table goes; the clear-alls take the rest |
| A load publishes | the managers and pages are replaced whole by the restored ones |

New objects inherit nothing. Unit and building ids are monotonic within
a session, and a load replaces every outgoing assignment. Location ids
restart at 1 on each page, so a replacement page's location 1 is a new
key in a new table.

General tags are not faction tags. Tagging changes nothing about an
instance: not its faction membership, ownership, combat or location
clearance.

### Persistence

| Component | Version | Carries |
|---|---|---|
| `units` | v5 | each page slice's `puTags`, the forward assignments of that page's units |
| `buildings` | v3 | each page slice's `pbTags` |
| `world-pages` | v14 | each page core's `pcLocationTags`, beside the frozen location-table DTO |

- **Capture.** The save barrier captures `usnTags`/`bsnTags` by the same
  page filter as the instances, so each assignment travels with its
  target's page. The location tags ride the page's own table.
- **Wire.** `World.Save.Component.GameplayTags` defines the one shape:
  `[(key, [Text])]`, ascending, forward assignments only. A malformed
  entry fails the owning component's validation: an empty tag, an empty
  or repeated tag list, or a key listed twice.
- **Migration.** `units` v1–v4, `buildings` v1–v2 and `world-pages`
  v1–v13 migrate explicitly with no assignments. The outgoing v4, v2
  and v13 slices are frozen as `UnitsDTOv4`, `BuildingsDTOv2` and
  `PageCoreDTOv13`. Faction tags never populate general tags.
- **Integrity.** A unit or building assignment whose target exists only
  on another page is a `wrong-page` error at both the save and the load
  boundary. An assignment naming no existing target is a
  `dangling-reference` diagnostic (`sessionIntegrityWarnings`), and the
  restore drops it with a log line rather than install a tag on nothing.
- **Restore.** `fromUnitSnapshot` and `fromBuildingSnapshot` rebuild
  each index from the restored forward assignments. Staging merges the
  pages and prunes the location tags (`pruneLocationTags`) before
  publication, so no query ever sees a restored reverse map that was not
  derived. The load transaction is unchanged: a failure before
  publication leaves the old session and its tags intact and paused, and
  a load never merges registries or reads scenario YAML.
- **Tooling.** `exe:synarchy-save-codec`'s canonical summary lists each
  page's assignments by target id, and its `seed-tags` operation writes
  assignments into a save through the real codec, for fixtures and the
  probe only.

### Gates

- Hspec `--match "Gameplay.Tags"`, which includes
  `Gameplay.Tags.Persistence`: every live edit and lifecycle path above
  through the real handlers, page replacement through all three
  transitions, nonempty codec round trips, migrated empty defaults,
  dangling and wrong-page handling, and pre-publication failure.
- `--match "save migrations"`: the tracked `z9-gameplay-tags` fixture
  (nonempty assignments in all three categories) and every older
  fixture's empty default, compared exactly.
- `python3 tools/test_save_compat_audit.py --only-reproducibility`:
  seeded fixtures are byte-reproducible, and the summary reads none off
  `z8` and nonempty ones off `z9`.
- `tools/gameplay_tag_persistence_probe.py` (manual-only): generated
  world, autosave and arena fresh-process cycles over codec-seeded tags.
- `tools/persistence_inventory_audit.py` for the inventory rows.

## Live tags: physical items

Since #2703 (SCN-04) physical-item assignments have a live owner. Items
move between owners and pages without changing identity, so their
assignments are held by the SESSION owner rather than by whichever
container holds the item today.

| Category | Owner field | Key |
|---|---|---|
| Items | `WorldManager.wmItemTags` | `ItemTarget`, the item's `iiInstanceId` (process-global within a session; 0 refused) |

The key is never a ground-row id (ground ids are page-local and
zero-based), a definition name or a display-group key. No
`ItemInstance` carries a tag field and `EngineEnv` gained no field
(capability inventory §6.4(a)). `ItemTarget` lives in
`Gameplay.Tags.Tag` so the session owner can hold it without importing
any object identity.

### The live item universe

Edits and queries resolve through the canonical live enumeration,
`World.Item.Locate`: ground items, unit inventory, equipment and
accessories, building storage and delivered materials, and every item
nested in any of those at any depth. Remembered copies (container and
portable knowledge), definitions, display groups and id 0 are not in it.
Same-definition items are separate targets, and a query result names the
physical item.

- `mutateItemTags` applies a `TagEdit` to one live item. With a page
  scope it also requires the item to be on that page. The item's owners
  sit behind several refs, so the existence check cannot share one
  atomic step with the write. The guard is
  `WorldManager.wmItemDestroyGen` instead. Every destruction's forget
  bumps it in the same atomic step that removes the destroyed ids'
  tags. An edit reads it before it locates the item, and commits only
  if it has not moved; if it has, the edit retries against a fresh view.
  So a target destroyed after the check has its own forget run after
  the write, which removes the write, and one destroyed before the
  commit forces the retry, which then finds it gone. A missing item is
  never treated as destroyed. An item a transfer has momentarily
  detached between its pop and its push is simply not editable at that
  instant (`TagEditNoTarget`), and the tags it already carries are
  untouched. The session epoch is read once for the whole operation, so
  a retry that crosses a session replacement fails instead of adopting
  the new session, where the same ids name other items.
- `itemTagsIn` inspects one item (`Nothing` when no live item has the
  id), `liveItemTargets` is the item universe, and `onPage` narrows a
  universe (every category) to the caller's selected page.
- `commitItemTreeTags` is the attachment boundary later scenario
  construction needs. It installs assignments for a newly materialized
  tree only if every target is already committed and live, under the
  same guard as an edit. Otherwise it installs nothing and returns the
  uncommitted ids; a session replacement returns every target.

### Item lifecycle

A move never touches the index: the id is unchanged, so the tags follow.
That makes every move tag-neutral by construction, including a
transfer's temporary detach, a refused move and a rolled-back batch.

| Event | Effect on item tags |
|---|---|
| Pickup, drop, ground relocation, equip/unequip, accessory changes, unit-to-unit and unit/building transfers (strict player and lax AI), construction delivery into delivered materials, nested ownership moves (`Item.Ownership`) | kept, for the item and every descendant |
| Fill, temperature, quality, condition and sharpness changes; a bulk sack eaten from; a container emptied of its last item | kept: identity survives |
| A refused or rolled-back operation, a partial batch | kept on moved and unmoved items alike |
| A failed transfer whose source vanished too, so the restore has nowhere to put the item (`commitCross`, and the lax cargo verbs' lost branches) | the discarded tree is gone, so its assignments go |
| Consumption: a discrete meal, a bulk sack eaten dry (`unit.feed`), `unit.removeItem`, craft and repair inputs, a bandage drawn from a kit, a power item placed as a node | the consumed items' assignments go; the kit, the surviving siblings and their shared tag names stay |
| Construction-designation payment | the paid items are consumed and their assignments go. A refund mints new, untagged items (owner decision, 2026-10-17) |
| `item.removeGround`; lava solidifying over ground items | the destroyed tree's assignments go. A pickup reaches the same ground removal but forgets nothing |
| `UnitDestroy`, `BuildingDestroy`, both clear-alls, page retirement (immediate and queued), page destroy or replace | everything the removed owner held goes, and on a page teardown its ground too. These paths spill nothing. A teardown reads the doomed ground only after the page is unreachable (out of `wmWorlds`, or replaced) and forgets after that, so a write that validated against the old ground is either retried or removed |
| A drop or a pickup rollback that lands on a page torn down after the item was popped | the departed `WorldState` takes the insert but is unreachable, so the item is destroyed. `placeOnGround` re-checks, after inserting, that the page's ground ref is still live and forgets the tree if not; every interleaving with the teardown's read is covered by that order |
| A unit dies | kept: the corpse keeps its items |
| Destroy-all (exit to menu) | the index is emptied with the page set |
| A load publishes | the restored index replaces the outgoing one |

Each destruction site works out which item ids its own atomic step
removed (`destroyedItemIds`), so it forgets exactly those, and only
after the removal. That is the same order the transfer-order and
container-knowledge cleanups use. It reads `wmSessionEpoch` before the
removal and skips the forget if a session replacement has bumped it
since. Item ids are never reused within a session, but a restored
session reuses them, so a cleanup computed in the outgoing session can
never strip a tag in the incoming one.

New items inherit nothing: not from their definition, their inputs,
their container or a previous holder of the id. Crafted outputs and
refunds are new identities.

### Item persistence

| Component | Version | Carries |
|---|---|---|
| `core-session` | v2 | `csItemTags`, the session's forward item assignments in the shared wire shape |

- **Capture.** `World.Thread.Command.Save.WriteWorld` captures
  `sgItemTags` from the same manager read as the pages, so the
  assignments and the item trees they name are one capture.
  `captureSessionSnapshot` keeps only assignments for items in those
  captured trees, so a save can never carry a tag on a dead item, even
  one a teardown race left behind for a moment.
- **Migration.** `core-session` v1 is frozen as `CoreSessionDTOv1` and
  migrates explicitly to no assignments. `currentSaveVersion` is
  unchanged.
- **Integrity.** An assignment naming no item that any page holds, in
  any owner and at any depth, is a `dangling-reference` warning
  attributed to `core-session`. Staging keeps an assignment only if its
  item was in the SAVED session's ownership AND survives staging. Both
  tests are needed: staging can mint items (a self-cleared designation's
  refund), and a refund can take the id a dangling assignment names, the
  saved allocator cursor. Staging drops the rest with a log line and
  rebuilds the reverse index from the forward assignments that remain,
  so a remembered copy can neither duplicate a membership nor keep a
  destroyed target live. A failure before publication leaves the
  outgoing session and its item tags intact.
- **Tooling.** The codec summary lists `itemTags` at session level, and
  `seed-tags` accepts an `items` list (`{"id": N, "tags": [...]}`, no
  page).

### Item gates

- Hspec `--match "Gameplay.Tags.Items"`: every owner category and depth,
  a three-level tree, same-definition siblings, every move and in-place
  change above, refused, rolled-back and partial operations, every
  consumption and destruction site, the payment and refund decision, the
  epoch guard, the attachment boundary, and save, load, dangling,
  pre-publication failure and v1 migration. Both index directions are
  checked after each transition.
- `--match "save migrations"`: the tracked `z10-item-tags` fixture
  (nonempty, `core-session` v2) and every older fixture's empty default.
- `tools/gameplay_tag_persistence_probe.py` (manual-only): item
  assignments across owner categories and nested contents, through
  ownership changes, consumption and ground destruction, fresh-process
  load and save, an autosave and an arena.

## Not yet integrated

Later slices of epic #2698 still owe:

- **Plants and tiles.** Their categories exist in the registry but have
  no live owner yet (SCN-05). Their universes come from the flora tables
  and loaded tile storage. Chunk eviction must not look like deletion.
- **Lua access.** This includes argument decoding and result
  marshalling. `count` and `exists` must return scalars, not lists.

If any of these steps uncovers a new identity or behaviour choice, it
goes to the owner before implementation.
