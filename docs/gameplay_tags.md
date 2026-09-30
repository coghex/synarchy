# Gameplay tags: registry and query contract

Status: the pure foundation (#2700, SCN-02 of epic #2698). Runtime
ownership, persistence, lifecycle hooks, and Lua access are later work
(see [Not yet integrated](#not-yet-integrated)).

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
| `Gameplay.Tags.Types` | `GameplayTag`, `NonEmptySet`, the typed targets and `TagCategory` |
| `Gameplay.Tags.Registry` | `TagRegistry`, the mutations, inspection, rebuild, invariant check |
| `Gameplay.Tags.Query` | `TagQuery`, `TagFilter`, universes/results (`TargetSets`), evaluation |

All three are pure. Nothing in them holds live state.

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
same tags keep them. `fromAssignments` is the path a later
persistence layer will use. It takes forward assignments only, so a
caller cannot supply a competing reverse index. A target that appears
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

## Not yet integrated

This slice is a pure value. Later slices of epic #2698 still owe:

- **Runtime ownership.** A manager or world owner holds the live
  registry, not `EngineEnv`, following capability inventory §6.4(a).
  Universes are enumerated from the object managers and from loaded
  tile storage.
- **Persistence.** The forward assignments are classified in the
  persistence inventory and saved with their owners, and the reverse
  index is rebuilt with `fromAssignments` on load. A save version or
  component migration follows the save rules.
- **Lifecycle hooks.** Destroying an object, or actually removing a
  tile, clears its assignments. Moves, equipping, and storage keep
  them. New objects never inherit tags. Chunk eviction must not look
  like deletion.
- **Lua access.** This includes argument decoding and result
  marshalling. `count` and `exists` must return scalars, not lists.

If any of these steps uncovers a new identity or behaviour choice, it
goes to the owner before implementation.
