# Portable loot containers

## Status

This document records the agreed foundation and first vertical slice for
portable, data-driven loot containers. It is the design authority from which
implementation issues will be drafted, not itself a one-PR implementation
contract. The gameplay decisions are settled; dependency-ordered issue
boundaries and the disposition of existing issues remain to be discussed.

The system supports the expedition arc in
[`expedition_gameplay_loop.md`](expedition_gameplay_loop.md), principally the
**discover**, **extract**, **return**, and **invest** verbs. Ruins may contain
rations when that result is appropriate to their cargo, but they do not
guarantee repayment of expedition supplies. The value of an expedition comes
from the options its salvage creates.

## Core model

A loot container is a portable item, not a separate class of static world
furniture.

A crate therefore has one durable item-instance identity through all of these
states:

1. lazily spawned on the ground as an unopened, unrolled shell;
2. realized exactly once by the first successful `Open` or physical `Pick up`
   attempt;
3. left on the ground with hidden realized contents after a failed lift, or
   moved into the unit's inventory after a successful one;
4. opened, partly emptied, or filled from a unit's inventory;
5. carried with its current contents;
6. dropped somewhere else; and
7. saved and loaded in any live state.

Moving the crate must never convert it into a different storage entity,
recreate its contents, or reroll its loot. After its first realization action,
the crate owns exact nested item instances, and transfers move those instances
between inventories.

The existing `ItemInstance.iiContents` model is the intended foundation.
Ground items already carry full item instances, so a portable container should
remain the same value tree when it moves between ground and unit ownership.

## Data ownership

Container appearance and capacity are independent from the cargo appropriate
to a particular location. The data model therefore has four responsibilities:

| Data concept | Responsibility |
| --- | --- |
| Item definition | An item's identity, display data, unit weight, and unit bulk in liters |
| Container component | Optional physical-storage fields on the canonical item definition: internal weight capacity and bulk capacity in liters |
| Loot profile | Candidate item definitions, appearance chances, and quantity factors |
| Location content entry | The position and the particular container-definition/loot-profile pairing to instantiate |

A container is registered in the same item registry and materializes as the
same `ItemInstance` type as every other item. "Container definition" in this
document means the canonical item definition plus its optional container
component; it never means a second registry, a parallel identity, or a
different runtime entity.

Container YAML files live under a logical item subdirectory such as
`data/items/containers/` so content and associated assets remain easy to find.
They retain the normal `items:` YAML shape, pass through the normal item
loader, and register ordinary item definition IDs. Item directories are
organizational only:

- a directory name does not become part of an item ID;
- moving an item definition between logical item directories does not change
  its type or behavior;
- locations, loot profiles, inventories, and saves continue to refer only to
  the canonical item definition ID; and
- no code may infer gameplay semantics from the definition file's directory.

The current startup loader enumerates only flat `data/items/*.yaml`. The first
slice must extend item YAML discovery to include logical subdirectories while
still routing every discovered file through `engine.loadItemYaml`. Discovery
must have an explicit deterministic ordering, and duplicate item IDs across
directories must follow the item loader's normal validation policy rather
than becoming directory-scoped definitions.

A container definition does not hardcode one contextual loot distribution.
The same crate may hold industrial salvage in a ruin, food in a storehouse, or
medical supplies in a clinic. Conversely, a loot profile may be used with
different compatible containers, whose capacities produce different final
cargo.

An explicitly handcrafted empty container may omit a loot profile. A container
whose contents are generated must name its loot profile explicitly; the first
slice should not rely on a hidden default profile. A generated container may
still end up empty when none of its profile entries appear.

## Lazy container shells and first-interaction population

A location selects both:

- which physical container definition to instantiate; and
- which loot profile will populate it on its first successful `Open` or
  physical `Pick up` attempt.

Location content remains lazy. World generation and game start retain the
location instance and authored content source; they do not create or roll
every container in the world. When the location's content is first
materialized by the existing chunk-loading flow, it creates an unrolled
container item shell with:

- its canonical container item definition and stable item-instance ID;
- its ground position or later current owner;
- a pending loot-profile ID; and
- a stable loot-source identity derived from the placed location instance and
  content entry.

Creating the shell does not roll its entries or create its generated contents.
The shell and its pending descriptor persist together until a realization
action consumes them.

The first successful `Open` or physical `Pick up` attempt consumes the pending
descriptor and realizes loot as a function of the complete pair:

```text
pending stable loot source
    + container definition
    + loot profile
    + stable random source
    -> one exact populated container item instance
```

The loot profile proposes cargo. The selected container definition supplies
the weight and bulk limits that decide which proposed cargo can physically be
placed. A small container can therefore reject quantity lots that the same
loot profile could place in a larger crate.

Population is an atomic state transition:

```text
Pending(profile ID, stable source ID) -> Realized(exact current contents)
```

For `Open`, only an effect result that reaches the contents performs this
transition. A future lock that denies access does not roll the loot. A future
trap outcome that still permits the unit to open and see inside may proceed to
realization; the exact effect outcome contract belongs to that future work.
A `Pick up` attempt realizes in the background when the unit reaches and
physically handles the crate, before its total carried weight is tested.

Concurrent open and pickup attempts cannot roll twice. The first transition
wins; every other action uses the same realized contents. A roll that produces
no items still completes the transition and leaves an independently real,
permanently empty container. Empty is not permission to try again.

After realization, the container discards its pending profile and source
association. It becomes an independent game entity whose current exact
contents are authoritative and change only through gameplay mutations.
Loading a save, moving, reopening, or emptying it never consults a profile.

The stable source makes the delayed roll seed-stable. Saving and loading an
unopened live shell preserves its pending descriptor; saving and loading a
realized container preserves its exact current contents and absence of pending
population.

This does not require tracking every container at game start. There are three
storage cases:

1. A distant location whose content has never materialized has no container
   item instance yet.
2. A lazily spawned but unrealized container is a live ground item and
   persists its identity, position, and lightweight pending descriptor.
3. A container realized by either `Open` or `Pick up` persists its independent
   item tree. Its player-knowledge entry may contain contents knowledge,
   weight knowledge, or both.

Retaining all realized containers is the first-slice contract. Any later
unloading, compaction, or caching scheme must preserve exactly the same
observable state and is deferred.

## Illustrative authoring shape

The final YAML schema must follow the repository's loader conventions; this
example documents ownership and references rather than fixing field spelling.

```yaml
# data/items/containers/wooden_crate.yaml
items:
  - name: wooden_crate
    display_name: Wooden Crate
    sprite: assets/textures/items/containers/wooden_crate.png
    kind: container
    weight: 4
    bulk: 30 # liters of external bulk
    container:
      capacity:
        weight: 30
        bulk: 23 # liters of usable internal bulk capacity
```

```yaml
# data/loot_profiles/ruin_industrial_salvage.yaml
id: ruin_industrial_salvage

quantity_multiplier:
  min: 1
  max: 4

entries:
  - { item: steel_bar,            chance: 0.30, quantity_factor: 5  }
  - { item: electric_motor,       chance: 0.05, quantity_factor: 1  }
  - { item: steel_hardware,       chance: 0.30, quantity_factor: 10 }
  - { item: high_voltage_battery, chance: 0.01, quantity_factor: 1  }
  - { item: steel_plate,          chance: 0.30, quantity_factor: 5  }
  - { item: processing_unit,      chance: 0.05, quantity_factor: 1  }
  - { item: wiring,               chance: 0.30, quantity_factor: 5  }
  - { item: rations,              chance: 0.10, quantity_factor: 5  }
```

```yaml
# Within data/locations/ruin_small.yaml
contents:
  - kind: container
    container: wooden_crate
    loot_profile: ruin_industrial_salvage
    position: { x: <corner-x>, y: <corner-y> }
```

Loot entries refer to canonical item definition IDs such as `steel_bar` and
`steel_hardware`, not texture filenames such as `bar_steel.png`.

## Weight and bulk

Weight and bulk are separate authored quantities with explicit units:

- **Weight, in kilograms:** can a unit carry this object, and can a container
  structurally support its contents?
- **Bulk, in liters:** an abstract scalar measure of how much practical
  packing space the object consumes.

Bulk is not the object's geometric volume, material displacement, or a value
from which its physical density can be calculated. It includes an authored
allowance for casing, awkward dimensions, fragility, and ordinary packing
space. A compact heavy battery can therefore have less bulk than a lighter,
broad steel plate. Liters keep item and container values intuitive and
comparable; they do not turn bulk into a scientific volume measurement.

The first slice does not model shape dynamically. An item's one authored bulk
value already summarizes its packing inconvenience; runtime fit does not
consider length, orientation, rigidity, container openings, or a particular
packing arrangement. If the summed bulk is within the container's bulk
capacity, the objects fit. A later shape-aware system may replace that scalar
abstraction if needed.

The intended physical rules are:

- Every physical item definition explicitly authors a finite, positive unit
  bulk in liters. There is no silent default and bulk is not inferred from
  weight or material density.
- The item loader rejects missing, zero, negative, or non-finite bulk for a
  physical item.
- A quantity consumes its count multiplied by its unit weight and unit bulk.
- A container's carried weight is its empty weight plus the complete recursive
  weight of its contents.
- Every container separately authors its external bulk and its internal bulk
  capacity. They are different physical values, not aliases and not derived
  from one another.
- A container's external bulk is the storage space occupied by the container
  itself. Properly filling it does not increase that external bulk.
- Internal weight usage is the total weight of the contents.
- Internal bulk usage is the sum of the external bulk values of the objects
  placed directly inside it. A nested container's contents add weight but do
  not add bulk outside that container's fixed external bulk.
- Internal capacities apply to contents, not to the empty container itself.
- Insert, transfer, pickup, drop, and load operations may not leave a
  container over either capacity.

The first slice keeps unit carrying capacity weight-based. Adding bulk limits
to unit inventories is a separate design decision and is not required merely
to make container packing physically coherent.

Bulk must participate in persistence with the same stability expected of the
other materialized physical properties of an item. All existing item
definitions need explicit liter values as part of the migration. The exact
instance representation and component-version migration belong in the
implementation design and must follow
[`persistence_contract.md`](persistence_contract.md).

### First industrial-profile bulk calibration

The first profile uses these accepted authored bulk values:

| Item | Weight | Bulk |
| --- | ---: | ---: |
| Steel bar | 0.5 kg | 0.75 L |
| Steel plate | 1.2 kg | 1.50 L |
| Electric motor | 2.5 kg | 1.25 L |
| High-voltage battery | 15.0 kg | 6.00 L |
| Steel hardware | 0.2 kg | 0.15 L |
| Processing unit | 0.4 kg | 0.40 L |
| Wiring | 0.5 kg | 0.75 L |
| Rations | 0.1 kg | 0.20 L |

The motor and battery are compact for their mass. Bars and plates receive
higher bulk relative to weight because their dimensions make them awkward to
pack. These comparisons describe gameplay bulk only; they are not claims
about any item's material density or geometric volume.

## Arbitrary-depth container nesting

Portable containers may contain other portable containers to any finite
number of levels. The data model and runtime APIs must not impose a fixed
gameplay nesting depth; weight and bulk capacities are the ordinary
limits. A crate can therefore be placed inside a larger container when the
crate's external bulk and complete recursive weight fit that destination,
regardless of the crate's own internal liter capacity.

The runtime ownership shape is an acyclic tree, never a graph:

- Every item instance has exactly one owner or root placement.
- A nested container and its complete subtree move as one exact value.
- An item instance cannot appear in two inventories or containers.
- A container cannot be inserted into itself or any of its descendants.
- Every instance ID remains unique across the complete nested tree and the
  rest of the session.

An insertion into a nested container must validate the whole ownership path:

- the immediate destination must have enough remaining internal bulk for the
  inserted item's external bulk;
- the immediate destination and every containing ancestor must remain within
  their internal weight capacities after the inserted subtree's complete
  recursive weight is added; and
- if the outermost container is carried by a unit, that unit must remain
  within its carrying-weight limit.

Adding contents to a nested container does not consume more bulk in its
ancestors because the nested container's external bulk is fixed. It does
increase weight through every ancestor.

Insertions and moves are atomic: a failed validation leaves the item, every
container, and every inventory unchanged. Save/load must preserve the complete
tree and reject invalid duplicate ownership, duplicate IDs, or cycles rather
than truncating or silently dropping nested contents. Default-content
authoring must likewise reject direct or indirect definition cycles that would
recurse forever during materialization.

## Population algorithm

This algorithm runs only while the container has a pending population
descriptor and only when a unit reaches the crate to complete a successful
`Open` or attempt a physical `Pick up`. Location discovery, map visibility,
chunk loading, inspection of an old observation, and movement toward the
container do not trigger it.

1. Atomically confirm that the container still has its pending profile and
   stable source. If another realization action already consumed them, use the
   realized contents instead.
2. Derive the container source's random stream from the world seed, stable
   location-instance ID, and stable content-source ID.
3. Independently roll each loot entry's appearance chance once. This is the
   chance that the entry proposes cargo before capacity filtering.
4. For every successful entry, roll a quantity multiplier from one through
   four.
5. The proposed quantity is `quantity_factor × quantity_multiplier`. For
   steel bars with a factor of five, the possible proposed quantities are 5,
   10, 15, or 20.
6. Represent that proposal as factor-sized generation lots and consider those
   lots in a deterministic seed-shuffled order.
7. Add a generation lot only if both the selected container's weight and bulk
   capacities still permit it; otherwise reject that lot.
8. Commit the exact accepted contents and remove the pending descriptor as one
   atomic transition. The calling action then reveals either contents
   knowledge (`Open`) or total-weight knowledge (`Pick up`).

Lot-level admission preserves authored quantity increments while allowing a
partially fitting roll to keep some lots. It also avoids a permanent
"discard the heaviest" bias against dense or rare items. The deterministic
ordering must include an explicit tie/order contract so refactoring map or
registry iteration cannot alter results.

The seed-shuffled order is the only admission priority. YAML order, item
weight, item bulk, rarity, and monetary value do not otherwise make a
successfully rolled lot win over another lot. A lot that cannot fit is
rejected without evicting cargo already admitted.

`quantity_factor` is an atomic population increment: a steel-bar lot of five
is admitted or rejected as five bars. It describes loot-generation
granularity, not a player inventory stack, stack limit, or permanent transfer
unit.

Empty and saturated containers are both emergent outcomes. Loot profiles do
not have `empty_chance`, `full_chance`, or a stock-state roll:

- a container is naturally empty when no entry appears, or when no proposed
  lot fits;
- its degree of fullness follows from the entries that appear, their quantity
  rolls, and the selected container's two capacities; and
- adding or changing profile entries intentionally changes that distribution
  and requires the profile to be retuned.

The first industrial-salvage profile targets roughly one naturally empty crate
in five by tuning its entry chances. With its four common entries at `0.30`
and the rarer entries unchanged, the chance that no entry appears before
capacity filtering is approximately:

`0.70^4 × 0.95 × 0.99 × 0.95 × 0.90 = 0.193`

That approximately nineteen-percent result is an emergent property of these
item entries. It is neither a system-wide guarantee nor a separately authored
empty-container roll. The schema must not add `empty_chance` to preserve it.

The same profile targets roughly one capacity-saturated crate in five by
tuning item bulk, quantity factors, and the crate's weight and bulk capacities.
For this measurement, a crate is saturated when it accepted at least one lot
but remaining capacity forced it to reject at least one rolled lot that would
have fit in the empty crate. A lot that is intrinsically too large for an
empty crate indicates an incompatible profile/container pairing; it does not
make an empty or sparse crate count as full.

The approximately twenty-percent saturation target is also only a measured
property of this first profile/container pairing. The schema must not add
`full_chance`, a forced stock state, or a post-roll fill operation to reach it.

The accepted first calibration is a `4 kg` empty crate with `30 L` external
bulk, `30 kg` internal weight capacity, and `23 L` internal bulk capacity.
Combined with the accepted item bulk values, quantity factors, and appearance
chances, a calibration run produced approximately `19.2%` naturally empty
crates and `19.3%` capacity-saturated crates. These are tuning baselines, not
exact runtime quotas.

The implementation's pure deterministic simulation should report
the distribution of:

- naturally empty containers;
- nonempty containers by weight and bulk occupancy;
- containers where no remaining generated lot can fit; and
- rejected lots by item.

The designer tunes the entries, quantity factors, and container capacities
until that measured distribution has the desired mix of empty, sparse, and
dense outcomes. The first slice does not add a separate control that overrides
those item-level rolls.

## Inventory stacking and first-slice quantity transfer

`quantity_factor` does not settle the separate inventory-stack design.

The first-slice contract is:

- Stacking is a presentation over exact item instances, not a counted runtime
  representation.
- Items share an actionable displayed stack only when they are genuinely
  interchangeable under the existing inventory grouping rule. Definition,
  quality, condition, fill, per-instance weight, visible weapon state, and
  nested-contents identity must not be collapsed in a way that loses
  information or makes the chosen instances ambiguous.
- Right-clicking a displayed contents stack offers `Grab all`.
- `Grab all` transfers the whole displayed stack; the first slice has no
  numeric quantity picker or `Grab amount` action.

The current item model already retains exact per-instance identity, and the
unit and cargo inventory interfaces already group interchangeable instances
for display. The generalized container contents view should reuse that
contract rather than its current definition-name-only grouping, which was
written for small supply kits and can merge materially different instances.

The exact-quantity interaction is deliberately deferred. A later issue may add
`Grab amount` over the same exact instances without changing the stored item
model or loot-generation factors.

`Grab all` is snapshot-bounded and atomic. The order targets the exact item
instances represented by the observed displayed stack when the player issues
it. Newly added matching items that the player has not observed are not swept
into the order. At execution, every requested instance must still be present
and the complete batch must satisfy all current destination and ancestor
capacity invariants. Otherwise nothing transfers, the player-controlled unit
refreshes the contents observation it successfully inspected, and the failure
is reported through the event log. The first slice never silently degrades
`Grab all` into a partial transfer.

No implementation issue may treat `quantity_factor` as the answer to
inventory stacking.

## Interaction contract

Unit-mediated world interactions use one standard assignment rule, including
`Open`, `Pick up`, and future interaction verbs:

- If one or more player-controlled units are selected, the interaction is
  assigned to the nearest eligible selected unit.
- If no player-controlled units are selected, it is assigned to the nearest
  eligible player-controlled unit on the target's world page.
- A nonempty selected set is the player's explicit candidate set. If none of
  those units is eligible, the interaction is unavailable with clear feedback;
  it does not silently fall back to an unselected unit.
- "Nearest" is chosen only by the current straight-line three-dimensional
  distance between unit and target:
  `sqrt((x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2)`. Assignment does not run a
  pathfinding query or try to predict reachability.

New interactions should inherit this assignment contract rather than define
verb-specific selection behavior.

The first usable container version should expose the same container through
ground and inventory interactions:

- Right-clicking a ground container always offers `Open` when at least one
  player-controlled unit can receive the order under the standard assignment
  rule.
- The assigned unit travels to the container. Arrival is a configurable
  three-dimensional floating-distance range between the unit and container,
  not tile-adjacency or integer grid-neighbor math. The first-slice value is
  `1.2` world tiles, matching the existing ground-pickup tuning; it remains a
  tunable rather than a data-format constant.
- The open attempt resolves only after the unit is in range.
- Right-clicking a ground container also offers `Pick up`, which sends a unit
  to the same floating interaction range before resolving the lift.
- A unit in range can transfer exact items into or out of the container.
- A nested container can be opened and traversed without flattening its
  contents into its parent.
- A carried container can transfer items between its contents and its
  carrier's top-level inventory.
- Dropping it preserves its identity and contents.
- Every insertion checks both container capacities.

If normal movement cannot bring the assigned unit into interaction range, the
open task cancels instead of being reassigned. Cancellation emits a
player-visible event-log warning identifying the unit, container, and failed
open order. The existing `unit_warning` category and
`unit_ai_core.reportFailure` path already cover failed jobs and unreachable
targets, so this behavior should reuse them rather than add a new notification
category.

When a unit reaches a crate for `Pick up`:

1. If it still has a pending population descriptor, realize and persist its
   exact contents in the background.
2. Compute and reveal the crate's total weight, including all newly realized
   and recursively nested contents.
3. Recheck the unit's live carrying capacity against that total.
4. If it fits, move the exact realized container tree into the unit's
   inventory without revealing its contents.
5. If it does not fit, the unit does not retain the crate; it remains or is
   dropped on the ground at the interaction point, and the existing
   `unit_warning` path reports that the pickup was cancelled because the crate
   is too heavy.

An unrealized crate has no truthful total weight yet, so the existing
ground-pickup command-time capacity gate cannot reject it before travel.
Pending containers bypass that first gate and are checked after realization
at the moment of handling. Already-realized containers retain command-time and
arrival-time checks when their weight is known to the player. The load-bearing
two-gate explanation in `scripts/unit_ai_pickup.lua` must be updated when this
exception lands.

The open action has an extensible result boundary. Future implementations may
detect or trigger a trap, discover that the container is locked, unlock or
force it, or deny access for another reason. Those effects and checks are
deferred, including the intended use of unit perception to notice traps, but
they occur at this boundary before contents are revealed. The first slice has
the ordinary successful-open result while preserving room for the other
outcomes.

## Actual state and player knowledge

Each portable container separates authoritative state from player knowledge:

1. **Actual contents** are the authoritative nested item-instance tree used by
   simulation, capacity checks, and mutations.
2. **Known total weight** is the player's last physical weight observation,
   which may exist without contents knowledge.
3. **Observed contents** are the player's last successful contents observation:
   a durable snapshot plus the game-time when a player-controlled unit made
   it.

Observed contents are knowledge, never authority. A command may be issued from
an old observation, but it cannot remove, duplicate, or otherwise mutate an
item that is absent from the actual container.

The observation contract is:

- Before the first successful open, the player cannot inspect the contents.
- Every `Pick up` attempt records the container's current total weight and
  game-time, whether the unit successfully carries the crate or drops it as
  too heavy. If the container still has pending loot, the attempt realizes it
  first. Picking up a previously realized container refreshes the weight
  observation in the same way. A pickup does not create or refresh a contents
  observation.
- A successful first `Open` realizes pending loot before copying the
  container's one resulting set of visible contents into the player's
  observed snapshot and recording the current game-time. It also records the
  resulting total weight.
- A successful later `Open` only refreshes the observation from current actual
  contents and weight; it never invokes population again.
- After the first observation, right-clicking the container offers both
  `Open` and `Contents`.
- `Contents` may be used remotely and displays the observed snapshot, not a
  live read of actual contents.
- The contents screen displays relative age beneath the container name, for
  example `Contents last updated 5 min ago`.
- Known total weight is exposed only as a numeric value where the interface
  ordinarily reports weight. Its observation time is persisted silently; the
  first-slice UI does not display `Weight last updated...` or any equivalent
  freshness text.
- `Open` remains available after observation so a player-controlled unit can
  refresh stale information.
- Every successful insertion, removal, or other contents-using action by a
  player-controlled unit refreshes the snapshot to the resulting actual
  contents, refreshes known weight, and records new observation times.
- Mutations by non-player units or simulation do not grant knowledge; the
  previous contents and weight observations remain visible and become stale.
- The observation timestamp uses persisted game-time, not wall-clock time, so
  pause and save/load do not age knowledge incorrectly.

Weight and contents have independent observation times. In particular, a
pickup can make the known weight current while leaving an older contents
snapshot untouched. The `Contents last updated...` label is derived only from
the contents timestamp and must not imply that the weight was observed at the
same time.

Every nested container has an independent observation snapshot and timestamp,
keyed by its own stable item-instance ID. Observing an outer crate reveals that
a nested container is present but does not automatically reveal or refresh
that nested container's contents. Opening the nested container does.

The actual tree and observed snapshot must not share mutable references.
Player knowledge must survive the container moving between ground, unit, and
nested ownership and must persist across save/load.

Container player knowledge is one separate persisted map keyed by stable
container item-instance ID. An absent key means the player has neither
inspected nor physically weighed that container. Each map value can contain:

- a known total weight and the game-time it was physically observed; and
- an optional snapshot of the container's immediately visible contents plus
  the game-time they were observed.

A weight-only entry created by `Pick up` does not enable the `Contents` menu.
Only a contents snapshot does.

The map is not stored inside the physical container and does not alter the
actual item tree. A container keeps the same knowledge entry when it moves
between ground, unit, and nested ownership because its instance ID is
preserved. Nested containers receive their own keys and values rather than
embedding recursively revealed knowledge in the outer container's snapshot.
The exact root owner and frozen save representation remain implementation
details governed by the repository persistence contract.

## Commands issued from stale observations

An item-transfer order issued from `Contents` records the target container
instance and the exact item-instance IDs represented by the selected observed
stack, but resolves against actual state only when the assigned unit reaches
interaction range.

At execution time:

- the container must still exist and be accessible;
- the requested item and quantity must be resolved against its current actual
  contents;
- all destination weight, bulk, ownership, and ancestor invariants are
  rechecked; and
- success mutates actual state atomically and then refreshes the observation.

If the unit reaches and opens the container but the requested item is gone,
the command performs no invalid mutation, refreshes the observation to what
the unit now sees, and reports the failure to the player. A multi-item `Grab
all` order follows the same rule atomically: one missing requested instance or
one failed capacity invariant means that none of the requested instances move.

If the container itself has moved, been destroyed, or become inaccessible
before the unit can inspect it, the order fails with feedback and does not
refresh the snapshot—the unit did not learn its current contents.

## First vertical slice

The content goal is one portable wooden crate placed in a fixed corner of
`ruin_small`. The location entry selects the crate definition and an
industrial-salvage loot profile containing existing material and supply item
definitions. The slice also adds the crate texture and makes the full
ground/open/transfer/pickup/carry/drop interaction legible.

The vertical slice includes:

- item bulk data and validation;
- an optional container component on canonical item definitions and
  dual-capacity rules;
- logical item-subdirectory discovery without a second registry;
- profile-driven, pair-aware, seed-stable population on the first successful
  `Open` or physical `Pick up` attempt only;
- exact persistence of unrealized live shells, pending descriptors, and
  realized containers with their current hidden or observed contents;
- persistent per-container weight and contents knowledge with independent
  game-time timestamps;
- unit-mediated floating-range opening and live revalidation of stale orders;
- ground and carried container interactions;
- one new crate texture;
- the `ruin_small` content integration; and
- focused deterministic, persistence, capacity, and interaction verification.

This is a coherent player-facing slice but is too broad for one implementation
issue. Its dependency-ordered issue breakdown will be designed in the
follow-up issue-planning discussion.

## Existing foundations and required changes

No merged system needs to be discarded. Relevant foundations already exist:

- `ItemInstance` supports recursively nested exact contents and total weight.
- `GroundItem` stores a complete item instance and persists it.
- Location instances have stable identity and exactly-once content spawning.
- Ground items can be picked up and carried.
- Carried item-containers already have a read-only contents view.
- Ground-item pickup already uses a floating arrival threshold, targets a
  stable ground-item ID, and rechecks live state on arrival.
- Ground-item pickup already has a progress-based stall timeout and reports an
  unreachable target through the existing `unit_warning` event-log path; the
  open command can follow that established cancellation contract.
- Stable item-instance IDs and persisted game-time provide identities and
  timestamps for durable container observations.

The slice must extend or correct these boundaries:

- Ground spawning must use one canonical item materializer; the current direct
  ground-spawn path can create an item with empty `iiContents` instead of
  materializing its authored default contents.
- Location container spawning must create an unrolled shell with a pending
  profile/source descriptor rather than invoke the profile during chunk load.
- The first successful `Open` or physical `Pick up` attempt must atomically
  consume that descriptor, commit one result, and remain exactly-once under
  concurrent actions and save/load.
- Pending ground-container pickup must bypass the current command-time weight
  gate, realize on arrival, reveal total weight, and either move the exact tree
  or report an over-capacity cancellation through `unit_warning`.
- Ground-item `Pick up` currently requires a selected unit and chooses among
  selected units using two-dimensional distance. It must adopt the standard
  interaction assignment rule, including the no-selection fallback and
  three-dimensional straight-line comparison.
- Ground-item context menus currently lack an open/contents action.
- The carried item-contents panel is currently inspection-only.
- The current contents view reads live state and must gain a distinct
  player-observed snapshot source with an age indicator.
- A persistent player container-knowledge owner and save representation do not
  yet exist.
- Container insert/remove APIs need exact-instance and capacity-safe semantics.
- Nested insertion must validate every weight-bearing ancestor and preserve
  acyclic single ownership.
- Bulk and container-capacity state must be represented, validated, and
  persisted compatibly.
- The current flat `loot_table` location content kind does not express the
  container-definition/loot-profile pair.
- Item startup loading currently enumerates only the flat `data/items`
  directory and must learn to discover logical item subdirectories without
  changing item identity semantics.

## Explicitly outside this slice

- locks, keys, lockpicking, and forced entry;
- traps and trap detection;
- large-chest rarity rules;
- global unique-item allocation;
- category-specific unique pools;
- ordered relic or lore-discovery tracks;
- procedural dungeon layout;
- bulk-limited unit inventories;
- profile-level empty/full probabilities or stock-state rolls;
- unloading, compacting, or caching realized-container state;
- numeric quantity selection and the deferred `Grab amount` action;
- counted runtime inventory stacks;
- a guarantee that ruins reimburse rations or other expedition inputs; and
- replacing the expedition arc's separate guaranteed progression reward with
  random container loot.

Although locks, traps, perception checks, and forced entry are outside the
slice, the successful-open action must expose a result boundary where those
effects can be added without bypassing or replacing the unit-mediated command.

## Remaining planning work

- Divide the vertical slice into dependency-ordered, one-PR child issues.
- Align #948 and #921 with those final boundaries.
