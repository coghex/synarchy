# Portable loot containers design

This document is the durable design authority for portable, data-driven loot
containers. It preserves the original physical, population, interaction, and
knowledge model while separating it from the newer unified-transfer work that
has landed since the design was written.

Design state: `ready for issue processing`

> **2026-08-11 — PLC-3 was split, and the new boundaries were signed off the same
> day.** Repository investigation showed the original slice spanned eight
> workstreams across seven files, three of which are subsystems that do not exist
> at all, and could not be one reviewable PR (D-16). It is now PLC-3 (converge
> creation, no new invariant), PLC-4 (invariant enforcement on mutation) and
> PLC-5 (integrity enumeration only). The previously unprocessed PLC-4 through
> PLC-9 moved to PLC-6 through PLC-11 so every stable child key ends in a number
> and satisfies the canonical processing contract. Completed PLC-1 and PLC-2,
> including their linked issues, are unchanged. Epic #1231's earlier split
> wording may use the former PLC-3A/PLC-3B/PLC-3C working labels; this document's
> numeric mapping is authoritative for subsequent processing.

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [x] EPIC. Add portable, data-driven loot containers — [#1231]
- [x] PLC-1. Load item definitions from logical subdirectories — [#1232]
- [x] PLC-2. Add physical bulk and portable-storage capacity data — [#1233]
- [x] PLC-3. Converge every item-creation path on one materializer — [#1418]
- [ ] PLC-4. Enforce capacity-safe, acyclic nested ownership moves
- [ ] PLC-5. Enumerate nested item trees in the save integrity graph
- [ ] PLC-6. Add lazy, deterministic loot-profile realization
- [ ] PLC-7. Persist player knowledge of portable containers
- [ ] PLC-8. Add unit-mediated opening and capacity-aware ground pickup
- [ ] PLC-9. Extend unified transfers to portable item-container endpoints
- [ ] PLC-10. Author the first wooden-crate ruin content
- [ ] PLC-11. Gate the complete portable-container lifecycle

## Epic contract

- **Goal:** Let a location contain a portable physical container whose exact,
  contextual cargo is realized once, can be inspected and transferred through
  ordinary unit commands, and remains the same nested item tree wherever the
  player carries, drops, stores, or saves it.
- **Done when:** A wooden crate spawned from `ruin_small` retains one stable
  item identity from unopened ground shell through realization, opening,
  transfer, pickup, carrying, nesting, dropping, and save/load; its cargo is
  seed-stable and capacity-valid; player knowledge is distinct from live state;
  and the shared transfer UI moves exact contents without a container-specific
  parallel interaction system.
- **Users and operators:** Players recovering and organizing expedition
  salvage; content authors pairing physical containers with contextual loot;
  maintainers of item identity, persistence, transfer, and location-content
  integrity.
- **Arc label:** `item-instance`

## Current state and evidence

- `Item.Types.ItemInstance` already owns stable `iiInstanceId` identity and a
  recursively nested `iiContents` tree. `itemTotalWeight` includes nested
  contents, and `Item.Ground.GroundItem` persists the complete instance when it
  is on the ground.
- Ground pickup preserves the exact instance, uses the real recursive weight,
  and rechecks carrying capacity at command time and arrival. Its current
  player gesture requires selected units, compares only two-dimensional
  distance, and has no `Open` path.
- `item.spawnGround` constructs an instance directly with empty `iiContents`.
  It therefore does not materialize an item definition's authored
  `idDefaultContents`, while unit spawning has a separate default-content
  materializer. There is no single canonical creation boundary today.
- The existing YAML `container:` component describes fillable fluid or pill
  capacity (`capacity`, `holds`, `fill_weight`, and `default_fill`). There is no
  authored physical bulk, internal item-storage weight limit, or internal item-
  storage bulk limit.
- `scripts/startup_loader.lua` walks the whole `data/items` tree recursively
  (`engine.listFilesRecursive`) and loads every file it finds, one
  `engine.loadItemYaml` call each, in one canonical order: ascending UTF-8
  bytes of the `/`-normalized path relative to `data/items`. Logical item
  subdirectories therefore load, at any depth, and a definition's id still
  comes from its own `name:` and never from its path. A symlink at any depth
  is skipped, so the walk terminates on any tree shape and never reaches a
  file outside `data/items`. Every other data family keeps flat, OS-ordered
  `engine.listFiles` enumeration (PLC-1, #1232).
- Location content supports independent `item` and `loot_table` entries. It
  cannot author a container-definition/loot-profile pair or persist a pending,
  unrealized profile descriptor. Closed #948 supplies seed-stable per-location
  loot selection, but not delayed container realization.
- Closed #921 made `ruin_small` random-only and guarantees no specific salvage.
  Portable crate cargo remains contextual random loot and does not replace the
  separate guaranteed expedition progression reward.
- Closed #1085 generalized the strict player transfer core to ordered exact-
  instance batches, but its endpoint vocabulary remains units and built
  buildings. Closed #1088 supplies the shared item-list widget.
- Closed #1087 added page-scoped, player-global stale knowledge for building
  containers keyed by `BuildingId`. Portable items can move between pages and
  owners, and no equivalent knowledge owner keyed by item-instance ID exists.
- **Closed epic #1013 delivered the unified unit/building transfer windows and
  movement modes**, all 17 children through the end-to-end gate #1255: the
  container-window stack with nested levels, Mode A escort sessions, Mode B
  persisted orders, failure handling, and the shared commit policy. Its
  as-built contract is `docs/engine_contracts.md` § "Player transfers: the
  three player-facing modes", and its design authority is
  `docs/unified_item_transfers.md`. It explicitly excludes item-containers as
  transfer endpoints, so it is the settled upstream substrate PLC-9 extends
  rather than a duplicate portable-container epic. D-14's external
  precondition is therefore met.
- **Eight independent item-creation sites exist** and none materializes authored
  default contents: `Unit/Thread/Command/Spawn.hs:314/386/449`,
  `World/Thread/Command/Edit/Dig.hs:248`,
  `Engine/Scripting/Lua/API/Forage/Harvest.hs:180`,
  `.../Craft/Execute.hs:250`, `.../Units/Inventory.hs:81`, and
  `.../Items/Ground.hs:103`. Each hardcodes `iiContents = []` and duplicates the
  fill/quality/condition/weight-roll assembly with small divergences.
- **ID allocation is already centralised.** Every one of those sites calls
  `Engine.Core.State.freshItemInstanceId` (`:688`), whose counter persists as
  `sdNextItemInstanceId`.
- **`idDefaultContents` is flat and unused.** It is
  `[(Text, Int, Maybe Float)]` (`Item/Types.hs:192`), read in exactly one place
  (`Spawn.hs:309`), with three references repo-wide and **no `data/` file
  authoring `default_contents`**.
- **Nothing enforces acyclicity or ancestry.** No cycle, acyclic, or ancestor
  logic exists under `src/Item/` or in `Unit/Transfer.hs`.
- **The integrity graph does not see nested items.** `World/Save/Integrity.hs`
  (511 lines) never walks `iiContents`, though `RefItemInstance` already exists
  in the reference vocabulary.
- **Capacity machinery exists but only flat and only by weight.**
  `Unit/Transfer.hs` (690 lines) has per-item capacity remeasurement and
  `tpIndex` splice-back rollback, scoped to unit and building endpoints.
- No open tracker epic currently owns portable physical containers, lazy
  container loot realization, or their ground/carried interaction lifecycle.

## Desired experience

A player reaches a ruin and finds a wooden crate whose contents have not been
precomputed merely because its chunk loaded. The player may send an appropriate
unit to open it or attempt to carry it. The first successful physical handling
realizes one deterministic cargo set for that exact crate.

Opening reveals its actual contents and weight. Pickup reveals its total weight
without granting contents knowledge; a crate that proves too heavy stays on the
ground with the same now-realized hidden cargo. The player can later inspect,
empty, refill, carry, nest, drop, and reopen it without rerolls or identity
changes. Stale remembered contents never override current physical truth.

The crate uses the same item list, transfer policy, feedback, and eventual
container-window conventions as ordinary unit/building transfers. Portable
containers add endpoint and lifecycle behavior; they do not create a competing
inventory interface.

## Scope

### In scope

- Portable item-container definitions in the canonical item registry.
- Logical item subdirectories whose paths do not affect definition identity.
- Explicit physical bulk plus internal weight and bulk capacity.
- Arbitrary finite, acyclic nesting with stable instance identity and atomic
  ownership moves.
- A contextual loot-profile registry and location-authored pairing with a
  physical container definition.
- Pending shells realized exactly once on first successful `Open` or physical
  `Pick up`, with deterministic lot admission and exact persistence.
- Player-global, persistent weight and contents observations keyed by portable
  item-instance identity.
- Ground, carried, and nested opening plus transfer interactions through shared
  UI and strict player-transfer policy.
- One wooden crate, one industrial-salvage profile, `ruin_small` integration,
  and focused lifecycle verification.

### Out of scope

- Locks, keys, lockpicking, traps, trap detection, and forced entry beyond a
  stable future-result boundary on `Open`.
- Bulk-limited unit inventories, dynamic packing geometry, item orientation, or
  shape-aware fit.
- Counted runtime stacks, numeric `Grab amount`, unique-item allocation,
  rarity tracks, lore sequences, or procedural dungeon layout.
- Profile-level `empty_chance`/`full_chance`, forced stock states, or guaranteed
  repayment of expedition supplies.
- Unloading or compacting realized-container state.
- Replacing #917's guaranteed progression reward with random crate loot.
- Changing the lax AI transfer verbs or making all ground piles generalized
  transfer endpoints.
- Reimplementing the unit/building transfer modes epic #1013 already
  delivered.

## Design

### Identity and ownership

A portable loot container is one ordinary `ItemInstance` with a physical-
storage capability. Its stable item-instance ID survives ground, unit,
building, and nested ownership. Actual contents form an acyclic tree: every
instance has one owner, an insertion cannot create self/descendant cycles, and
moving a container moves its complete subtree without minting new identities.

The mutation boundary validates the immediate destination's remaining internal
bulk, every containing storage ancestor's internal weight capacity, and an
outer carrier's weight capacity. A failed move leaves the source order,
destination, ancestors, and instance allocator unchanged.

### Physical data

Every physical item has explicit positive finite external bulk in liters.
Portable storage separately authors empty weight, external bulk, internal
weight capacity, and internal bulk capacity. Direct children consume their own
external bulk; descendant contents add weight through the ancestor chain but do
not expand a nested container's fixed external bulk.

The present `container:` YAML component is a fillable-substance model, not an
item-storage model. Physical item storage therefore uses a sibling `storage:`
component rather than overloading fluid/pill capacity semantics, per D-12.

### Lazy contextual population

A location content entry pairs one physical container definition with one loot
profile and a stable source identity. Chunk materialization creates a live
unrealized shell with its normal item identity plus a lightweight pending
descriptor; it does not roll contents.

The first successful `Open` or physical `Pick up` atomically changes
`Pending(profile, source) → Realized(exact contents)`. The random stream is a
pure function of the world seed, placed-location identity, content-entry
identity, and profile inputs. Entry appearance and quantity lots are rolled
once, lots are considered in deterministic seed-shuffled order, and only whole
lots that fit both capacities are admitted. Empty is a realized outcome, not a
reason to reroll.

After realization the profile/source descriptor is discarded. The exact item
tree is authoritative thereafter. Concurrent actions converge on one committed
result, while save/load preserves either the pending descriptor or the realized
tree without consulting current chunk-load order.

### Actual state and knowledge

Physical contents are authoritative. Player knowledge is a separate durable
record keyed by the crate's stable item-instance ID:

- a pickup attempt realizes the shell when necessary and records total weight,
  even if the carrier cannot lift it;
- successful `Open` records current contents and total weight;
- later player-controlled insert/remove interactions refresh both observations;
- simulation or non-player mutation changes actual state without granting new
  knowledge; and
- contents and weight retain independent game-time observation timestamps.

The owner is a session-level portable-item knowledge component rather than
`Building.Knowledge`'s page-scoped `BuildingId` map. Both expose a common UI
projection, but a carried item does not have to migrate a knowledge record each
time its page or owner changes, per D-13.

### Commands and transfer integration

`Open` and `Pick up` follow one unit-assignment rule. A nonempty selected set is
the explicit candidate set and chooses its nearest eligible member by current
three-dimensional straight-line distance with a deterministic unit-ID
tiebreak. With no selected player unit, the nearest eligible player-controlled
unit on the target page is assigned. An unreachable job cancels with the
existing unit-warning path; it is not silently reassigned.

Pending pickup bypasses the command-time weight refusal because truthful weight
does not exist yet. On arrival it realizes the crate, records the observed
weight, and then either moves the exact tree or leaves it at the interaction
point with a capacity warning. Already-realized crates keep both capacity
checks.

Portable contents should extend #1085's exact endpoint identity and #1088's
shared item-list widget. They should not add a fourth transfer policy or list
renderer. PLC-9 extends #1013's delivered transfer surfaces and adopts its
partial-batch semantics, per D-14 and D-15.

### Extensible open boundary

The successful-open result owns the transition that reveals contents. Future
locks, traps, perception checks, or forced entry may deny or alter that result
before knowledge is granted, without moving realization into chunk loading or
bypassing the unit-mediated command. Those outcomes are not part of this arc.

### First authored content

The first vertical slice places one wooden crate in a fixed corner of
`ruin_small` and pairs it with an industrial-salvage profile over existing
materials and supplies. Capacity filtering follows the accepted original
calibration: a `4 kg` empty crate, `30 L` external bulk, `30 kg` internal weight
capacity, and `23 L` internal bulk capacity. The approximately 19% empty and
19% saturated results remain measured tuning baselines, not runtime quotas or
schema fields.

## Decisions

### D-1. Model a portable container as one ordinary item instance

The container keeps one stable identity and one exact recursive contents tree
through every owner and placement. There is no parallel container registry or
static-furniture runtime type.

### D-2. Pair physical containers with contextual loot profiles at locations

Container appearance/capacity and contextual cargo distribution are separate
authored concepts. A location selects both; neither definition path becomes
part of an item ID.

### D-3. Realize generated cargo lazily and exactly once

Chunk loading creates a pending shell. First successful open or physical pickup
consumes its descriptor atomically; movement, save/load, and reopening never
reroll it.

### D-4. Use explicit weight and bulk as separate physical constraints

Bulk is authored packing space in liters, not geometric volume. Containers
have distinct external bulk and internal weight/bulk capacities, while unit
inventories remain weight-limited in this arc.

### D-5. Permit arbitrary finite acyclic nesting

No gameplay depth limit is imposed. Unique ownership, cycle rejection, direct-
child bulk, recursive weight, and atomic failure are the invariants.

### D-6. Admit deterministic whole generation lots

Loot profiles roll entry appearance and factor-sized lots. Seed-shuffled order
is the sole admission priority; a rejected lot does not evict accepted cargo,
and empty/full distributions emerge from entries and capacity.

### D-7. Separate actual contents from durable player observations

Pickup reveals weight, open reveals contents, and each observation has its own
persisted game-time. Knowledge may be stale and never authorizes mutation.

### D-8. Use one deterministic unit-assignment rule for world interactions

Selected player units constrain the candidates; otherwise the nearest eligible
player unit on the page is used. Distance is three-dimensional and ties break
by stable unit ID.

### D-9. Keep `Open` extensible without implementing locks or traps now

Access effects run at a result boundary before contents become known. The first
slice implements only ordinary successful opening.

### D-10. Prove the system with contextual industrial salvage

One wooden crate in `ruin_small` provides the first vertical slice. Its random
cargo creates options but guarantees neither expedition-supply repayment nor
the separate progression reward.

### D-11. Extend the unified player-transfer substrate

Portable containers reuse exact-instance endpoint requests, structured
outcomes, the shared item-list widget, and the container-window manager epic
#1013 delivered. Container-specific interaction code owns realization and
knowledge refresh, not a competing transfer stack.

### D-12. Keep fillable containers and physical item storage separate

Top-level `bulk` records the external packing space of every physical item. A
sibling optional `storage:` component records internal item-storage weight and
bulk capacities. The existing `container:` component and `iiCurrentFill` retain
their homogeneous fluid/pill meaning unchanged. An item may eventually possess
both components without either capacity inheriting the other's defaults or
validation.

### D-13. Persist portable-container knowledge at session scope

A separately versioned session component maps stable item-instance IDs to
independent weight and contents observations with their own game-time
timestamps. The record follows the crate across page and owner changes, remains
outside live ownership/allocator accounting, and is pruned when the live crate
is permanently gone. Building and portable knowledge expose one presentation
projection without sharing an incompatible durable owner.

### D-14. Make the shared transfer surfaces an external precondition for PLC-9

As filed this read *"PLC-1 through PLC-8 may proceed while epic #1013 remains
open. PLC-9 stops until #1013 has supplied its container window and persisted
unit/building order lifecycle, unless those phases are explicitly reassigned.
Portable containers then extend the settled surfaces with an item-container
endpoint instead of shipping temporary or duplicate transfer UI."*

**The precondition was satisfied on its own terms, not reassigned.** PLC-9
extends the settled unit/building transfer surfaces with an item-container
endpoint instead of shipping temporary or duplicate transfer UI — unchanged.
What has ended is the external wait.

> **2026-09-06 — the precondition is met.** Epic #1013 closed with all 17
> children complete, supplying exactly what this decision waited on: the
> generalized container window and its nested level stack (#1234, #1237,
> #1238), the persisted Mode B order lifecycle (#1246, #1247, #1249), the
> Mode A escort session (#1250, #1251), failure handling (#1253, #1254), and
> the end-to-end gate (#1255). No phase was reassigned to this arc. PLC-9's
> remaining dependencies are the internal `PLC-4`, `PLC-7` and `PLC-8`, and
> PLC-1 through PLC-8 were never gated on #1013 in the first place.

### D-15. Use per-item atomic partial batches for portable transfers

`Grab all` targets the observed row's exact instance IDs in stable order. Every
individually valid item moves; missing, stale, or over-capacity items remain and
receive structured outcomes. The batch reports complete, partial, or no
fulfillment. No individual item is partly moved, newly matching items are never
swept in, and a failed move restores that item to its original source position.

### D-16. Split PLC-3 into creation, ownership rules, and integrity

PLC-3 as originally written could not be one reviewable PR. Measured against the
current tree it spanned eight workstreams across seven files (~2,680 lines of
touched surface), three of which are subsystems that do not exist at all. It is
split into PLC-3 (converge creation), PLC-4 (capacity-safe acyclic moves) and
PLC-5 (integrity enumeration).

The seam is real rather than arithmetic: PLC-3 is a behaviour-preserving
convergence that introduces no new invariant, while PLC-4 is invariant
enforcement on mutation and is the first thing that makes PLC-2's bulk actually
bite. PLC-5 only observes; it enforces nothing.

*Consequence:* only unprocessed slice IDs were normalized. PLC-6 depends on
PLC-3 and PLC-4 (its "every accepted lot fits" signal needs capacity
enforcement); PLC-7 on PLC-3 alone (it keys on instance identity, not on
capacity); PLC-9 on PLC-4 (its endpoint checks are the ancestor and capacity
rules); PLC-11 on all three. Completed PLC-1 and PLC-2 are untouched.

*Two findings that made the split cheaper than expected, and are recorded so
they are not rediscovered:* item-instance ID allocation is ALREADY centralised —
all eight mint sites call `Engine.Core.State.freshItemInstanceId` — so PLC-3
preserves that discipline rather than building it. And `default_contents` has
ZERO authored users in `data/`, so reshaping `idDefaultContents` from a flat
tuple list to a recursive one carries no migration cost, though it also means
"no path drops defaults" is currently unobservable in shipped data and needs a
fixture to test against.

## Accepted proposals and rejected alternatives

### P-1. Add a sibling `storage:` item component

Accepted by D-12.

Keep the existing fillable `container:` vocabulary intact and put physical
item-storage capacity in a distinct optional component. The two concepts use
some of the same words but have different state and rules:

- today's `container:` means one homogeneous scalar fill such as two liters of
  water or sixty pills; it drives `iiCurrentFill`, `holds`, `fill_weight`, and
  `default_fill`;
- proposed `storage:` means an inventory of exact nested `ItemInstance` values;
  it drives `iiContents`, internal weight capacity, internal bulk capacity,
  ownership, and transfer validation; and
- top-level `bulk` remains the external packing space the item itself occupies,
  whether or not the item can store anything.

An illustrative definition would therefore read:

```yaml
weight: 4
bulk: 30
storage:
  weight_capacity: 30
  bulk_capacity: 23
```

The spelling remains subject to the repository's loader conventions, but the
separation is the decision. A future item could legally have both capabilities
if the design ever needs it—for example, a liquid tank with a separate tool
compartment—without making one capacity field mean two things.

The alternative is to extend the existing `container:` object with nested-item
fields. That saves one top-level YAML key and one optional Haskell component,
but every consumer must then distinguish fill-only, storage-only, and combined
containers. Validation defaults become hazardous: a canteen's two-liter liquid
capacity must never become two liters of nested inventory, and a wooden crate
must not acquire `iiCurrentFill` semantics merely because it stores items.
That coupling would also make later schema and error messages harder to read.

### P-2. Give portable knowledge a session-level sibling component

Accepted by D-13.

Store item-container observations by stable item-instance ID in one separately
versioned persisted session component. A record needs four independently
optional facts: known total weight, when that weight was learned, an observed
contents snapshot, and when those contents were seen. The record is player
knowledge only; it neither owns the live item nor participates in duplicate-ID
or allocator checks as though its historical snapshot were another inventory.

Session scope matches the lifecycle. The same crate may move from ground on
page A, into a unit, into a building, inside another crate, through a portal,
and onto page B without changing its item-instance ID. Its observation should
follow that identity without copying a record between `WorldState` values on
every move. When the live crate is permanently destroyed, the record can be
pruned against the canonical live-item enumeration with a diagnostic rather
than making load fail.

This does not require duplicating UI behavior. Building and portable knowledge
can expose the same presentation projection—state, remembered items and weight,
observation times, and live capacity—even though their durable owners differ.
The shared container window consumes that projection and does not need to know
which save component supplied it.

The alternative is to generalize the landed `Building.Knowledge` component.
That component is page-scoped, keyed only by `BuildingId`, assumes capacity is
always live and known, and stores one contents-derived weight with one reveal
time. Supporting portable items there would require a tagged key, independent
weight/content timestamps, cross-page movement semantics, and a migration of
the already-shipped building save component. It would produce one type named
"container knowledge," but the apparent unification would sit above two
different lifecycles and make the completed building feature riskier.

### P-3. Follow #1013's partial-batch transfer semantics

Accepted by D-15.

When an observed row names several exact instances and only some still fit,
process them in stable request order, move each capacity-valid instance, and
report each instance that remained. "Partial" applies to the batch, never to an
individual item: one ration, tool, or nested crate is still moved atomically or
not moved at all.

For example, suppose an observed row represents twelve exact ration instances:

- if the unit has room for eight at command time, eight queue and four report
  `receiver_full`; the unit makes one trip for the accepted eight;
- if capacity changes en route and only six fit on arrival, those six commit
  individually and the remaining two accepted requests fail as stale capacity;
- if one observed instance has already gone, that instance fails without
  retargeting another ration, while other still-valid instances may move; and
- the event log reports complete, partial, or no fulfillment so the player
  knows whether anything remains.

Stable request order matters because it decides which exact instances win when
capacity runs out. The request uses the observed row's explicit instance-ID
order; it never sweeps in newly added matching items. Source and destination
order remain deterministic, and a failed individual move restores that item to
its original source position.

The legacy alternative is a batch-wide transaction: if any requested instance
is missing or any capacity check fails, nothing moves. That is easy to explain
as "all means all," but it forces the player to estimate capacity, refresh the
container, and issue smaller requests. More importantly, #1085 has already
implemented per-item atomic partial batches for unit/building endpoints. Making
portable endpoints all-or-nothing would require a second policy or new batch-
wide prepare/rollback machinery solely for crates.

### P-4. Let #1013 finish the shared window and movement modes first

Accepted by D-14.

Portable foundations can land independently, but PLC-9 should extend the
settled container-window and order lifecycle after #1013 supplies them. The
completed #1013 foundations already provide exact endpoint requests (#1085),
building-container knowledge (#1087), and the shared item-list widget (#1088).
Its remaining phases own the window manager, persisted walk-then-transfer
orders, paired adjacent transfer session, cancellation/error presentation, and
unit↔unit coordination.

PLC-1 through PLC-8 do not need those surfaces. They can deliver data loading,
physical capacity, safe nested ownership, lazy realization, portable
knowledge, and ground `Open`/`Pick up`. PLC-9 is the first slice that needs a
player to move individual contents, so it is the natural dependency boundary.
Once the shared surfaces exist, PLC-9 adds an `item-container` endpoint kind,
its accessibility/capacity rules, and its knowledge refresh hooks without
inventing another window or order state machine.

The cost is schedule coupling: the portable arc can reach an openable and
carryable crate but cannot finish contents transfer until #1013 reaches the
needed phases. The alternative is to move those #1013 phases—or a temporary
subset of them—into this epic. That could reach the crate UI sooner if #1013 is
stalled, but it would split ownership of one transfer experience across two
epics, create duplicate tracker scope, and likely leave temporary UI to remove
later. Reassignment is viable only if it is explicit; silent duplication is
not.

> **2026-09-06 — the scheduling risk did not materialize.** #1013 reached and
> passed the needed phases, so the rejected alternative was never invoked: no
> #1013 phase was moved into this epic, no temporary transfer UI was built,
> and ownership of the transfer experience stayed in one epic. The proposal's
> reasoning stands as the record of why the boundary was drawn where it was.

## Open questions

### Q-1. Does physical item storage use a sibling component?

Resolved by D-12: use top-level external `bulk` plus a separate optional
physical-storage component. Existing fillable definitions and `iiCurrentFill`
keep their meaning unchanged.

### Q-2. Does portable-container knowledge use a session-level sibling owner?

Resolved by D-13: use a separately versioned durable map keyed by item-instance
ID, independent weight/content observations, and one common read shape for the
shared UI.

### Q-3. Does PLC-9 wait for epic #1013's shared transfer surfaces?

Resolved by D-14: PLC-1 through PLC-8 can proceed independently, but processing
PLC-9 stops until #1013's container window and persisted unit/building order
lifecycle exist or are explicitly reassigned to this arc.

> **2026-09-06:** answered in practice — #1013's container window and persisted
> order lifecycle now exist, so the wait D-14 imposed has ended. PLC-9 remains
> gated on its own `PLC-4`, `PLC-7` and `PLC-8` dependencies.

### Q-4. Is `Grab all` partial or all-or-nothing when capacity changed?

Resolved by D-15: process the snapshot's exact instance IDs in stable order,
commit every individually valid move, and report what remained. This preserves
one contract across every endpoint without weakening per-item atomicity.

## Verification strategy

- Add focused pure coverage for bulk/capacity validation, recursive weight,
  cycle rejection, ancestor checks, exact-instance preservation, source-order
  rollback, and deterministic materialization.
- Add fixed-vector population coverage for stable context derivation,
  appearance/quantity rolls, shuffled lot order, capacity rejection, empty
  realization, and concurrent exactly-once transitions.
- Exercise pending and realized shells, nested item trees, portable knowledge,
  independent timestamps, allocator bounds, and invalid ownership through the
  repository's persistence inventory, save compatibility, and integrity gates.
- Preserve and extend the focused item-instance, location-content, pickup,
  cargo-capacity, transfer-contract, and shared item-list suites rather than
  relying on a whole-suite run as an iteration loop.
- Add headless interaction coverage for selected/no-selection assignment,
  three-dimensional nearest choice and ties, arrival-range revalidation,
  pending over-capacity pickup, stale observed contents, and warning paths.
- Add a manual offscreen UI probe for ground and carried `Open`, relative-age
  presentation, nested traversal, exact transfer actions, partial feedback if
  D-15 applies, and resize/focus preservation through the shared widget.
- Extend `tools/location_content_probe.py` for the authored container/profile
  pair and exactly-once save/load behavior. Extend the expedition regression
  only after the portable lifecycle is independently pinned.
- Run `world_check.py` or regenerate world baselines only if the authored
  location change alters worldgen output; do not treat new randomized crate
  cargo as a replacement for #948's deterministic mapping contract.

## Delivery plan

### PLC-1. Load item definitions from logical subdirectories

- **Outcome:** Item YAML can be organized recursively without changing item
  IDs, loader routing, duplicate validation, or startup determinism.
- **Scope:** Recursive deterministic discovery for normal and arena startup,
  canonical relative paths, duplicate-ID behavior, loader diagnostics, and
  focused registry/probe coverage.
- **Phase:** Data loading foundation
- **Depends on:** `none`
- **Ordering:** `can land first`
- **Relevant decisions:** D-2
- **Acceptance signals:** Nested YAML is loaded through `engine.loadItemYaml` in
  deterministic order; moving a definition between item directories does not
  change its ID; duplicate IDs retain one documented validation policy.
- **Out of scope:** New container data or gameplay behavior.
- **Open questions:** None

### PLC-2. Add physical bulk and portable-storage capacity data

- **Outcome:** Every physical item has validated bulk and portable storage can
  author distinct external bulk plus internal weight/bulk limits.
- **Scope:** Definition/YAML representation, explicit item-data migration,
  finite-positive validation, runtime projection, compatibility/persistence
  treatment, documentation, and calibration fixtures.
- **Phase:** Physical data foundation
- **Depends on:** `none`
- **Ordering:** `can land first`
- **Relevant decisions:** D-4, D-12
- **Acceptance signals:** Missing/invalid physical bulk is rejected; fillable
  containers retain their behavior; the wooden-crate calibration is representable;
  save/load cannot silently reinterpret a materialized physical item.
- **Out of scope:** Ownership mutation, loot realization, and UI.
- **Open questions:** None

### PLC-3. Converge every item-creation path on one materializer

- **Outcome:** Every path that mints an item does so through one boundary that
  materializes the definition's authored default contents.
- **Scope:** One canonical materializer; recursive default contents replacing
  the flat `idDefaultContents` tuple list; migration of all eight mint sites;
  preservation of the existing `freshItemInstanceId` allocation discipline; and
  unification of the divergent fill/quality/condition/weight-roll logic those
  sites currently duplicate.
- **Phase:** Runtime ownership foundation
- **Depends on:** `PLC-2`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-4, D-5, D-16
- **Acceptance signals:** Ground, unit, crafted, dug, foraged and
  content-created instances all mint through the materializer; no path drops
  authored defaults; every minted id is unique and comes from the existing
  allocator; recursive default trees materialize to the authored depth; and
  existing shipped content produces the same instances it does today apart from
  defaults now being present.
- **Out of scope:** Capacity enforcement, ancestor and cycle rules, rollback,
  integrity enumeration, loot profiles, knowledge, and UI.
- **Open questions:** None

### PLC-4. Enforce capacity-safe, acyclic nested ownership moves

- **Outcome:** Every nested insert and remove preserves exact instance identity,
  respects both capacities, and fails atomically.
- **Scope:** Capacity-safe insert/remove enforcing PLC-2's internal weight AND
  bulk limits; weight-bearing ancestor and carrier revalidation; cycle and
  duplicate-instance rejection; and atomic rollback generalized from flat
  inventories to nested trees.
- **Phase:** Runtime ownership foundation
- **Depends on:** `PLC-2`, `PLC-3`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-4, D-5, D-16
- **Acceptance signals:** A move that would exceed either capacity is refused;
  a move that would create a cycle or a duplicate id is refused; a refused move
  mutates nothing and never duplicates or loses an instance; every
  weight-bearing ancestor and the carrying unit are revalidated, not just the
  immediate parent; and valid nested trees round-trip unchanged.
- **Out of scope:** The materializer itself, integrity enumeration, transfer UI
  and endpoints, and loot realization.
- **Open questions:** None

### PLC-5. Enumerate nested item trees in the save integrity graph

- **Outcome:** The shared integrity graph sees every nested item instance rather
  than only top-level ones.
- **Scope:** Walking `iiContents` during integrity enumeration at both the save
  and load boundaries, duplicate-instance-id detection across the whole tree,
  and invalid-ownership diagnostics consistent with the existing reference
  vocabulary.
- **Phase:** Runtime ownership foundation
- **Depends on:** `PLC-3`
- **Ordering:** `not on the critical path` — lands in parallel with PLC-4
- **Relevant decisions:** D-1, D-5, D-16
- **Acceptance signals:** A nested instance is enumerated at the same depth it
  is stored; a duplicate id anywhere in a tree is reported; enumeration is
  deterministic; and the existing wrong-page hard error versus dangling-target
  tolerance is unchanged for the references that already had it.
- **Out of scope:** Enforcing the invariants it reports (PLC-4 owns that),
  and any new reference kind.
- **Open questions:** None

### PLC-6. Add lazy, deterministic loot-profile realization

- **Outcome:** A location-authored container shell realizes one seed-stable,
  capacity-valid cargo set only when physically handled.
- **Scope:** Loot-profile data/registry, container/profile location entry,
  pending descriptor and source identity, deterministic lot algorithm, atomic
  realization, concurrent callers, and pending/realized persistence.
- **Phase:** Population
- **Depends on:** `PLC-1`, `PLC-2`, `PLC-3`, `PLC-4`
- **Ordering:** `critical path`
- **Relevant decisions:** D-2, D-3, D-6
- **Acceptance signals:** Chunk loading does not roll; open/pickup yields the
  same fixed-vector contents across process and load order; one empty result is
  permanent; concurrent attempts cannot roll twice; every accepted lot fits.
- **Out of scope:** Player-facing commands, knowledge UI, and crate tuning.
- **Open questions:** None

### PLC-7. Persist player knowledge of portable containers

- **Outcome:** Weight and contents observations follow a portable crate's
  stable identity without becoming authoritative state.
- **Scope:** Durable record/model, independent game-time timestamps, weight-
  only and contents observations, stale snapshots, nested-container isolation,
  query/refresh surfaces, lifecycle cleanup, and persistence documentation.
- **Phase:** Knowledge
- **Depends on:** `PLC-3`
- **Ordering:** `can land in parallel with PLC-6`
- **Relevant decisions:** D-1, D-7, D-13
- **Acceptance signals:** Never-weighed, weight-only, known-empty, and known-
  contents states remain distinct; records survive moves and save/load; stale
  IDs never enter live allocator/integrity ownership; destroyed instances clean
  up without corrupting loads.
- **Out of scope:** Windows and transfer actions.
- **Open questions:** None

### PLC-8. Add unit-mediated opening and capacity-aware ground pickup

- **Outcome:** Ground containers can be opened or physically picked up through
  deterministic unit jobs with correct realization and knowledge effects.
- **Scope:** Context actions, unit assignment, floating arrival range, progress-
  based cancellation, open result boundary, pending-pickup exception, arrival
  capacity check, knowledge refresh, event feedback, and carried/nested open.
- **Phase:** World interaction
- **Depends on:** `PLC-6`, `PLC-7`
- **Ordering:** `critical path`
- **Relevant decisions:** D-3, D-7, D-8, D-9
- **Acceptance signals:** No-selection and selected-set assignment are
  deterministic; unrealized weight is not pre-rejected; arrival realizes once;
  an overweight crate stays put but becomes weighed; open reveals exact
  contents; unreachable jobs warn and do not reassign.
- **Out of scope:** Moving individual contents between endpoints.
- **Open questions:** None

### PLC-9. Extend unified transfers to portable item-container endpoints

- **Outcome:** Players move exact items between units and ground, carried, or
  nested portable containers through the shared transfer experience.
- **Scope:** Tagged item-container endpoints, accessibility/proximity, strict
  request validation, item-container capacity and ancestor checks, stale
  observation revalidation, shared container window/list, feedback, and
  knowledge refresh after player-controlled commits.
- **Phase:** Transfer integration
- **Depends on:** `PLC-4`, `PLC-7`, `PLC-8`
- **Ordering:** `critical path` — D-14's external transfer-surface
  precondition is met (epic #1013 closed); only the three dependencies above
  remain
- **Relevant decisions:** D-1, D-5, D-7, D-11, D-14, D-15
- **Acceptance signals:** Ground and carried containers use the same endpoint
  request/outcome vocabulary and item-list widget as unit/building transfers;
  stale missing items cannot mutate live state; capacity failures follow one
  settled batch policy; nested traversal never flattens ownership.
- **Out of scope:** Rebuilding the unit/building modes #1013 delivered, lax AI
  verbs, numeric quantity pickers, and generalized ground piles.
- **Open questions:** None

### PLC-10. Author the first wooden-crate ruin content

- **Outcome:** `ruin_small` produces one legible portable wooden crate with a
  tuned industrial-salvage profile.
- **Scope:** Crate definition/texture, accepted capacities and bulk values,
  profile entries/quantity factors, fixed location position, distribution
  simulator/fixture, and authored-data validation.
- **Phase:** Content vertical slice
- **Depends on:** `PLC-1`, `PLC-2`, `PLC-6`, `PLC-8`, `PLC-9`
- **Ordering:** `critical path`
- **Relevant decisions:** D-2, D-4, D-6, D-10
- **Acceptance signals:** The profile references canonical item IDs; the crate
  appears exactly once at the authored ruin position; measured empty/saturated
  distributions remain near the documented tuning baselines without quota
  fields; cargo is not a guaranteed expedition reimbursement.
- **Out of scope:** Additional container art, profile families, and progression
  rewards.
- **Open questions:** None

### PLC-11. Gate the complete portable-container lifecycle

- **Outcome:** Focused automated and UI-capable scenarios prove one crate's
  identity, realization, knowledge, transfer, movement, nesting, and persistence.
- **Scope:** Targeted hspec groups, integrity/save fixtures, one focused
  headless lifecycle probe, one manual offscreen interaction probe, probe
  registration, and load-bearing documentation updates.
- **Phase:** Integration gate
- **Depends on:** `PLC-1`, `PLC-2`, `PLC-3`, `PLC-4`, `PLC-5`, `PLC-6`,
  `PLC-7`, `PLC-8`, `PLC-9`, `PLC-10`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2, D-3, D-4, D-5, D-6, D-7, D-8, D-9,
  D-10, D-11, D-12, D-13, D-14, D-15
- **Acceptance signals:** A pending shell survives save/load, realizes once,
  rejects an overweight pickup without rerolling, opens to an exact observed
  snapshot, transfers contents safely, survives carry/drop/nesting, and reloads
  with the same identities, actual state, and knowledge.
- **Out of scope:** Broad balance gates and unrelated full-probe sweeps.
- **Open questions:** None

## Source notes

The detailed original design follows. It remains authoritative where it does
not conflict with the canonical decisions and open questions above. In
particular, its all-or-nothing `Grab all` rule is intentionally unresolved by
Q-4 because the later signed-off unified-transfer contract uses partial
batches.

### Legacy status

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

Item YAML discovery is recursive (PLC-1, #1232): `scripts/startup_loader.lua`
walks the whole `data/items` tree at any depth and routes every discovered file
through `engine.loadItemYaml`, one call per file, in one explicit deterministic
order — ascending UTF-8 bytes of the `/`-normalized path relative to
`data/items`. Duplicate item IDs across directories follow the item loader's
normal last-write-wins policy, the later definition in that order winning, with
a diagnostic naming the ID and both files; they never become directory-scoped
definitions.

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
- Item startup loading already discovers logical item subdirectories: it walks
  the whole `data/items` tree recursively, in one canonical order, without
  changing item identity semantics (PLC-1, #1232).

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
