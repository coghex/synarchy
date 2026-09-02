# Unlike-fluid reaction design

When lava and water meet in the runtime fluid simulation, the engine today
silently converts one into the other. The owner has decided contact should
instead REACT: lava + water produces stone terrain. This document designs that
reaction — the rule, its durable terrain product, and its presentation — so the
arc can be delivered as dependency-ordered one-PR slices.

Design state: `ready for issue processing`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [ ] EPIC. Make unlike-fluid contact react: lava + water solidifies to stone
- [ ] FR-1. Detect unlike-fluid contact and resolve it with the reaction rule in every transfer path
- [ ] FR-2. Solidify the reaction product into durable stone terrain through the world edit log
- [ ] FR-3. Resolve units and items caught at a solidifying cell
- [ ] FR-4. Present the reaction: contact effects and map refresh

## Epic contract

- **Goal:** Lava and water in the active fluid simulation never silently
  change identity; where they meet, the contact consumes fluid by an explicit
  rule and leaves stone terrain that persists like any player tile edit.
- **Done when:** Water flowing into lava (and lava into water) produces stone
  at the contact site in-session, the stone survives chunk eviction and a
  fresh-process save/load, no transfer path (gravity, lateral, waterfall,
  cross-chunk seam) can convert one fluid type into another, and total
  volume accounting is explicit (consumed by reaction, never duplicated or
  silently retyped).
- **Users and operators:** Players (lava becomes a real terrain-forming
  hazard/tool instead of a paint-by-arrival accident); worldgen/sim
  maintainers (the fluid identity invariant becomes checkable).
- **Arc label:** None proposed (existing `bug` fits FR-1 only; the arc itself
  is a feature).

## Current state and evidence

Verified on master `b4631ca6e` (2026-08-31):

- Every active-fluid transfer keeps the destination's type when the
  destination is occupied: cross-chunk `transferCell`
  (`src/Sim/Fluid/Active.hs:180-181`), gravity (`:306-308`), lateral
  (`:346-352`), waterfall (`:425-431`). A pure GHCi tick with `Lava 7` above
  an occupied `Lake 1` produced `Lake 8` — the lava became water
  (`docs/holistic_project_audit_findings.md` HPA-3).
- Fluid identity is gameplay-load-bearing: `fluidPenalty`
  (`src/Unit/Pathing/Cost.hs:272-277`) makes lava and ocean impassable while
  rivers and lakes are wadeable.
- Runtime fluid types are exactly `Ocean | Lake | River | Lava`
  (`src/World/Fluid/Types.hs:21`, `Serialize` via `Generic` — append-only
  enum, guarded by `tools/enum_append_only_audit.py`). Ice is a worldgen
  drape, not a runtime fluid, so a lava+ice runtime reaction has no
  representation to react with.
- Sim state is rebuild-only scratch (`docs/persistence_state_inventory.md`
  §`simStateRef`): nothing under `SimState` persists. Durable world mutations
  live in the per-chunk edit log (`World.Edit.Types.WorldEdit`, replayed over
  regenerated chunks) — `WeAddTile gx gy matId` already raises a column by
  one z of a named material, and `WeSetCell` writes an arbitrary cell.
- The sim reaches the world only via `FluidWritebackBatch`
  (`src/World/Command/Types.hs:35-60`): per-chunk fluid/terrain/surface/deco
  vectors, applied by the world thread (sole writer of `wsTilesRef`) as a
  dumb insert, fenced per chunk by `fwEditGen` against live edits (#1596).
  A fluid writeback mutates live tiles only — it never appends to the edit
  log, which is why the reaction's stone cannot be produced by the sim
  writing terrain vectors alone: it would vanish on eviction/reload.
- Stone materials with complete art already exist: `basalt` and `obsidian`
  in `data/materials/igneous_extrusive.yaml`, each with tile and zoom-map
  textures. No art blocker.
- Adjacent open work on the same code: #2042 (lateral phase must cap
  transfers at remaining source volume) and #2044 (sim must exchange across
  the cylindrical U seam) both modify the same transfer sites the reaction
  hooks into.

## Desired experience

A player channels a river into a lava pool: where the water arrives, the lava
solidifies into stone, the water is spent, and the new stone is walkable
terrain that persists forever — reloading the save shows the same stone. Lava
flowing into a lake does the same from the other side: the advancing lava
front freezes into stone at the shoreline instead of the lake turning into
lava. No fluid ever changes type; it either stays what it is or is consumed
by the reaction.

## Scope

### In scope

- The reaction rule for lava vs water (`Ocean`/`Lake`/`River`) in every
  active-sim transfer path: gravity, lateral, waterfall, and the cross-chunk
  seam exchange.
- The durable stone product: edit-log-backed terrain that survives eviction,
  save, and load, with correct edit-generation fencing and sim re-seeding.
- Volume accounting for the reaction (explicit consumption, no duplication).
- Presentation: what the player sees at the contact site, and zoom-map/live
  render refresh of the new terrain.
- Regression coverage in the headless suite.

### Out of scope

- Water-vs-water mixing (Ocean/Lake/River into each other) — retains current
  behavior; a separate finding can address it if it ever matters.
- Ice, steam as a simulated fluid, or any new `FluidType` constructor.
- Passive/worldgen fluid (generated lava pools vs generated water are placed
  disjointly by worldgen; this arc governs the runtime active sim only).
- The #2042 conservation fix and #2044 seam fix themselves (separate issues;
  this arc depends on them).
- New art (obsidian/basalt assets already shipped).

## Design

**Where the reaction lives.** Contact happens inside the sim's transfer
sites — the four in-chunk/cross-chunk points that today retype fluid. The
reaction replaces the silent retype: when a transfer would deliver fluid into
a cell occupied by the unlike class (lava vs any water type), the transfer
instead feeds the reaction rule. The sim resolves the fluid-side outcome
immediately in its own grids (volumes consumed), and emits a solidification
event for the terrain product rather than writing terrain itself.

**How stone becomes durable.** The sim cannot append to the edit log; the
world thread owns it. Solidification events ride to the world thread (with
the producing chunk's `scsEditGen`), which validates freshness, appends the
stone edit (`WeAddTile`-shaped: raise the contact column by one z of the
product material), bumps the chunk's edit generation, and re-seeds the sim
via the existing `SimChunkEdited` path — the same causal fence live edits
already use, so an in-flight stale writeback cannot overwrite the new stone.
The exact event/edit representation is FR-2's implementation choice; the
observable contract is durability plus fence correctness.

**The rule and product** are decided: contact annihilation (D-3) yielding a
contextual material — obsidian above ground, basalt below ground or undersea
(D-2, predicate D-5). **Occupants** of a solidifying cell are destroyed
instantly (D-6, FR-3).

**Ordering against adjacent fixes.** #2042 (conservation) and #2044 (seam)
change the same transfer sites. The reaction slices land after both to avoid
building the rule on unconserved arithmetic or a seam that cannot exchange.

## Decisions

### D-1. Unlike-fluid contact reacts: lava + water produces stone

Owner decision 2026-08-31 (process-report HPA-3 disposition). Rejected
alternatives: blocking (each fluid keeps identity and piles up — conservative
but inert), precedence (one type always wins — still destroys material
silently), typed composition. Consequence: the arc needs a durable terrain
product and explicit volume consumption, which is what makes it epic-sized.

### D-2. The product material is contextual: obsidian above ground, basalt below ground or undersea

Owner decision 2026-08-31 (resolves Q-1). Both materials ship with full tile
and zoom art (`data/materials/igneous_extrusive.yaml`), so no art work is
implied. Geologically apt: subaerial rapid quench → obsidian, submerged or
subterranean solidification → (pillow) basalt. The exact machine predicate
for "above ground" vs "below ground / undersea" is Q-5.

### D-3. Volume arithmetic is contact annihilation

Owner decision 2026-08-31 (resolves Q-2). The smaller of the two contacting
volumes is consumed 1:1 from both sides; when a cell's lava reaches zero this
way, that cell's column gains one z of the product material. Rejected:
transfer-quench, threshold solidification. The owner additionally flagged
that anything occupying the solidifying cell — units, items — must be
handled, leaning toward instant destruction; that is Q-6 and its own slice
(FR-3).

### D-4. No interim stopgap ships first

Owner decision 2026-08-31 (resolves Q-3). FR-1 ships the reaction directly;
the silent-retype defect is fixed by the reaction itself rather than by a
preliminary blocking behavior.

### D-5. The product predicate is local submersion/subterranean state

Owner decision 2026-08-31 (resolves Q-5). After annihilation, the new stone
cell forms as **basalt** when it is still submerged (a water-type fluid cell
remains directly above the new stone top, or the water side of the contact
was `Ocean`) or subterranean (the new stone top sits below the column's
terrain surface, inside carved interior space); otherwise **obsidian**.
Deterministic, purely local to the contact cell, no climate/biome lookup.
Rejected: Ocean-only basalt (coarser).

### D-6. Units and items at a solidifying cell are destroyed instantly

Owner decision 2026-08-31 (resolves Q-6). A unit occupying the solidifying
cell dies immediately with an event-log entry; ground items there are
destroyed. Rejected: displacement to an adjacent tile (needs a displacement
rule, can cascade), damage-and-lift (most machinery). Today's `WeAddTile`
path has no occupant handling at all, so FR-3 is new mechanism, not a
change to existing behavior.

## Open questions

### Q-1. Which material is the reaction product?

Resolved by D-2.

### Q-2. What is the volume arithmetic and solidification site?

Resolved by D-3.

### Q-3. Does a conservative interim behavior ship first?

Resolved by D-4.

### Q-5. What is the exact predicate for obsidian vs basalt at a contact?

Resolved by D-5.

### Q-6. What happens to units and items at a solidifying cell?

Resolved by D-6.

### Q-4. What does the player see at the contact site?

The side-deco channel (`scsSideDeco`, waterfall-style decals) could carry a
steam/sizzle marker, or the contact could be visually silent (stone just
appears). Affects FR-3's size only. Resolved by owner preference; can stay
deliberately open until FR-3 is drafted.

## Verification strategy

- Pure hspec over `simulateActiveTick`: reaction fixtures for each transfer
  path (gravity drop into unlike fluid, lateral equalization, waterfall,
  seam exchange), both orderings (lava→water, water→lava), asserting exact
  volume consumption, no retype, and the emitted solidification events.
  Extends the existing `Test.Headless.Sim.Seam` pattern.
- World-thread integration: an edit-fence spec asserting a solidification
  event landed as a durable edit, bumped the generation, and re-seeded the
  sim (the `--match "fluid writeback staleness"` family is the model).
- Persistence: the stone product present after a fresh-process save→load
  (persistence-contract style; NB #365 — never on an arena page).
- A headless probe driving `world.setFluidTile` lava against water on a real
  page end-to-end is a candidate FR-3-adjacent gate; CI eligibility per
  `tools/ci_probes.py` rules.

## Delivery plan

### FR-1. Detect unlike-fluid contact and resolve it with the reaction rule in every transfer path

- **Outcome:** No transfer path can change a fluid's type; unlike-fluid
  contact consumes volume per the chosen rule and yields solidification
  events (consumed in FR-2; inert but observable in tests until then).
- **Scope:** All four transfer sites (gravity, lateral, waterfall,
  `transferCell`/seam), both orderings, volume accounting, pure hspec
  coverage.
- **Phase:** 1
- **Depends on:** `none` (externally: #2042 and #2044 should land first —
  same code)
- **Ordering:** critical path
- **Relevant decisions:** D-1, D-3, D-4
- **Acceptance signals:** reaction fixtures per path pass; a
  lava-above-water gravity tick produces no retype and exact consumption;
  existing seam/conservation specs still pass.
- **Out of scope:** terrain mutation, persistence, entity handling,
  presentation.
- **Open questions:** None.

### FR-2. Solidify the reaction product into durable stone terrain through the world edit log

- **Outcome:** Solidification events become stone terrain — obsidian or
  basalt per D-2's predicate — that survives chunk eviction and a
  fresh-process save/load, fenced correctly against concurrent live edits.
- **Scope:** Sim→world event transport, product-material selection,
  edit-log append, edit-generation bump + sim re-seed, persistence coverage.
- **Phase:** 2
- **Depends on:** FR-1
- **Ordering:** critical path
- **Relevant decisions:** D-1, D-2, D-3, D-5
- **Acceptance signals:** edit-fence spec passes; stone present after
  save→load in a fresh process; a stale in-flight writeback cannot erase
  new stone; submerged and subaerial contacts yield their respective
  materials per D-5.
- **Out of scope:** entity handling; visuals beyond what edit replay
  already renders.
- **Open questions:** None.

### FR-3. Resolve units and items caught at a solidifying cell

- **Outcome:** A unit or ground item occupying a cell whose column
  solidifies is destroyed instantly per D-6, with event-log visibility and
  no stuck or embedded entities.
- **Scope:** Occupant detection at the solidification site, instant unit
  death with its event, ground-item destruction, regression coverage.
- **Phase:** 3
- **Depends on:** FR-2
- **Ordering:** critical path
- **Relevant decisions:** D-1, D-3, D-6
- **Acceptance signals:** a unit standing at a solidifying cell dies
  immediately and an event records it; ground items at the cell do not
  survive embedded in stone; no pathing corruption afterward.
- **Out of scope:** presentation effects.
- **Open questions:** None.

### FR-4. Present the reaction: contact effects and map refresh

- **Outcome:** The contact site reads as a reaction to the player (per Q-4's
  answer), and live render + zoom map reflect the new stone promptly.
- **Scope:** Side-deco/effect at contact, cache invalidation verification,
  optional end-to-end probe.
- **Phase:** 4
- **Depends on:** FR-2
- **Ordering:** not on the critical path
- **Relevant decisions:** D-1
- **Acceptance signals:** per Q-4's resolution; zoom/live render show stone
  without a reload.
- **Open questions:** Q-4 — may stay open until this slice is drafted.

## Source notes

Origin: `docs/holistic_project_audit_findings.md` HPA-3 (marked `[deferred]`
pending this arc's epic). The owner chose "Reaction (lava+water→stone)" over
blocking, precedence, and deferral on 2026-08-31.
