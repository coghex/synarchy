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

- [x] EPIC. Make unlike-fluid contact react: lava + water solidifies to stone — [#2480]
- [x] FR-1. Detect unlike-fluid contact and resolve it with the reaction rule in every transfer path — [#2481]
- [x] FR-2. Solidify the reaction product into durable stone terrain through the world edit log — [#2485]
- [x] FR-3. Resolve units and items caught at a solidifying cell — [#2490]
- [x] FR-4. Present the reaction: contact effects and map refresh — [no-issue]: visually silent (D-7); the zoom refresh and its evidence are implemented in #2485

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

**Amended 2026-09-07 (refill policy, implemented by FR-1 in #2481).** The
identity invariant is an *occupied-contact* rule, not a rule about every
later state of a coordinate within a tick. A cell whose volume reaches zero
by annihilation is empty, and is an ordinary empty destination for later
phases and requests in the SAME tick: a later transfer may refill it with
any fluid under the normal empty-cell rule, and that refill neither cancels
nor duplicates the solidification event the coordinate already emitted. At
most one event is emitted per canonical coordinate per tick; a later tick
may emit another there once new lava has arrived and been exhausted again.
Rejected: a tick-wide tombstone forbidding refill (needs a per-coordinate
prohibition set threaded through all five branches, and would strand fluid
that has nowhere else to go).

**The five protected branches.** There are four transfer *mechanisms* but
FIVE occupied-destination *write* branches, and the reaction and the
bounded-transfer rules apply to all of them: seam exchange
(`transferCell`), `phaseGravity`, `phaseLateral` with a snapshot-occupied
destination, `phaseLateral` with a snapshot-empty destination that an
earlier source filled live in the same phase, and `phaseWaterfall`. FR-1
routes all five through one applier (`Sim.Fluid.Reaction.applyTransfer`),
so each is independently protected by its own regression fixture. Because a
reaction can consume more than the planned transfer, every request planned
from a frozen snapshot is resolved against the LIVE cells at the moment it
is applied, and ordinary transfers are additionally bounded by the
destination's remaining `Word16` capacity.

### D-4. No interim stopgap ships first

Owner decision 2026-08-31 (resolves Q-3). FR-1 ships the reaction directly;
the silent-retype defect is fixed by the reaction itself rather than by a
preliminary blocking behavior.

### D-5. The product predicate is local submersion, decided at the reaction

Owner decision 2026-08-31, amended 2026-09-07 (resolves Q-5). The sim and
the world model fluid per column on top of the terrain surface
(`Sim.Fluid.Types.volumeToSurface`, `World.Chunk.Types.lcTerrainSurfaceMap`
= topmost non-air z), and the reaction consumes the solidifying column's
own lava, so the original wording — water "directly above the new stone
top", or a stone top "below the column's terrain surface" — could never
hold at the reaction and would have collapsed to the rejected Ocean-only
rule. The predicate is therefore evaluated inside the sim at the moment of
annihilation, from the two contacting cells plus the page-wide constant
`World.Constants.seaLevel`. The new stone top is the lava column's terrain
top plus one. The stone forms as **basalt** when any of these holds:

- the water side of the contact was `Ocean`;
- the contacting water cell's fluid surface after annihilation
  (`volumeToSurface` of its terrain and remaining volume) is above the new
  stone top — the stone will be under the lake or river once the water
  equalizes onto it;
- the new stone top is at or below `seaLevel`, the codebase's own subsea
  test.

Otherwise **obsidian**. Deterministic, no climate or biome lookup, and no
dependence on writeback ordering. Rejected: Ocean-only basalt (coarser);
evaluating submersion later against the live chunk (timing-dependent); a
subterranean clause (fluid cannot sit below a column's terrain surface in
the column model, so it is unreachable).

### D-6. Units and items at a solidifying cell are destroyed instantly

Owner decision 2026-08-31 (resolves Q-6). A unit occupying the solidifying
cell dies immediately with an event-log entry; ground items there are
destroyed. Rejected: displacement to an adjacent tile (needs a displacement
rule, can cascade), damage-and-lift (most machinery). Today's `WeAddTile`
path has no occupant handling at all, so FR-3 is new mechanism, not a
change to existing behavior.

### D-7. The contact site is visually silent

Owner decision 2026-09-07 (resolves Q-4). The stone simply appears; no
side-deco marker, effect, or notification accompanies a solidification.
A steam or sizzle marker would need a new `SideDecoType`, four new decal
textures (an art issue with owner signoff; no steam, smoke, or bubble asset
exists), a render hookup, and a lifetime rule, none of which the arc needs
to deliver its outcome.

**Corrected 2026-09-08 (#2485).** This decision originally recorded that
the refresh half of FR-4 was already provided by the add-tile path FR-2
reuses, "which invalidates the live quad cache and both zoom-map quad
caches". That is true of the invalidation and FALSE of the conclusion for
the zoom map. `handleWorldAddTileCommand` clears `wsZoomQuadCacheRef` and
`wsBgQuadCacheRef`, but `World.Render.Zoom.Quads.renderFromBaked` samples
`wsZoomCacheRef` and `wsZoomAtlasRef`, and
`World.Render.Zoom.Bake.ensureBakedAtlas` only re-derives QUADS from
them — the terrain pixels themselves are produced once, at page
initialization, by `World.ZoomMap.Cache.buildZoomCacheWithPixels`. No
number of dropped quad caches changes one pixel of the zoom map, and
clearing `wsZoomAtlasRef` to fall back to per-material baking does not
either: that path colours a whole chunk by its majority material, in
which a single new stone tile cannot appear.

So #2485 performs an ACTUAL zoom terrain-data refresh: it regenerates the
affected chunk's `zoomTileSize`-square block from the live post-edit
chunk, patches it into the atlas the page retains (`wsZoomLiveRef`), and
republishes the whole image through the same `zoomAtlasDataRef` handoff a
fresh init and a load publish use. The detailed tile render is unaffected
by this correction — it really does need nothing beyond the quad-cache
invalidation, because it rebuilds from the chunk the edit replaced. See
`docs/engine_contracts.md` §Fluid reaction for the rule and
`tools/fluid_reaction_visual_probe.py` for the evidence.

Rejected: a side-deco steam/sizzle marker; a per-solidification
event-log entry (not adopted; a later finding may revisit it).

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

Resolved by D-7.

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
  (persistence-contract style, on an ordinary generated page — this gate
  wants the real generated-world path, which an arena's flat rebuild is
  not; #365's arena-load hang is fixed and is no longer the reason).
- A headless probe driving `world.setFluidTile` lava against water on a real
  page end-to-end is a candidate FR-3-adjacent gate; CI eligibility per
  `tools/ci_probes.py` rules.

## Delivery plan

### FR-1. Detect unlike-fluid contact and resolve it with the reaction rule in every transfer path

- **Outcome:** No transfer path can change a fluid's type; unlike-fluid
  contact consumes volume per the chosen rule and yields solidification
  events (consumed in FR-2; inert but observable in tests until then).
- **Scope:** All FIVE occupied-destination write branches (gravity,
  `phaseLateral` snapshot-occupied, `phaseLateral` snapshot-empty /
  live-occupied, waterfall, `transferCell`/seam), both orderings, live
  resolution of snapshot-planned requests, `Word16`-bounded ordinary
  transfers, volume accounting, event accumulation, pure hspec coverage.
- **Phase:** 1
- **Depends on:** `none` (externally: #2042 and #2044 should land first —
  same code)
- **Ordering:** critical path
- **Relevant decisions:** D-1, D-3 (including its 2026-09-07 refill
  amendment), D-4, D-5
- **Acceptance signals:** reaction fixtures per branch pass independently
  in both orderings; a lava-above-water gravity tick produces no retype and
  exact consumption; a coordinate emptied by annihilation can be refilled
  in the same tick without losing or duplicating its event; existing
  seam/conservation specs still pass.
- **Out of scope:** terrain mutation, persistence, entity handling,
  presentation.
- **Open questions:** None.

### FR-2. Solidify the reaction product into durable stone terrain through the world edit log

- **Outcome:** Solidification events become stone terrain — obsidian or
  basalt per D-2's predicate — that survives chunk eviction and a
  fresh-process save/load, fenced correctly against concurrent live edits.
- **Scope:** Sim→world event transport, coherent multi-chunk admission,
  product-material selection, edit-log append, one edit-generation bump per
  commit + an exact-active-volume sim handoff, both live presentations,
  persistence coverage.
- **Phase:** 2
- **Depends on:** FR-1
- **Ordering:** critical path
- **Relevant decisions:** D-1, D-2, D-3, D-5, D-7
- **Acceptance signals:** edit-fence spec passes; stone present after
  save→load in a fresh process; a stale in-flight writeback cannot erase
  new stone; submerged and subaerial contacts yield their respective
  materials per D-5; a water remainder that is not a multiple of
  `volumePerLevel` survives the commit handoff; both live presentations
  show the stone without a reload (FR-4's evidence, folded in here per
  D-7's 2026-09-08 correction).
- **Out of scope:** entity handling; contact effects and side-deco markers.
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

> Disposition 2026-09-07: `[no-issue]` — the contact is visually silent
> (D-7); the refresh evidence is requested on #2485.
>
> Amended 2026-09-08: D-7's original claim that the add-tile path's cache
> invalidation already refreshes the ZOOM map was wrong, so the refresh is
> not a no-op #2485 merely evidences — #2485 implements an actual zoom
> terrain-pixel regeneration and atlas republication. The disposition
> stands: no separate issue, and the evidence is
> `tools/fluid_reaction_visual_probe.py`.

- **Outcome:** The contact site reads as a reaction to the player (per Q-4's
  answer), and live render + zoom map reflect the new stone promptly.
- **Scope:** Side-deco/effect at contact, cache invalidation verification,
  optional end-to-end probe.
- **Phase:** 4
- **Depends on:** FR-2
- **Ordering:** not on the critical path
- **Relevant decisions:** D-1
- **Acceptance signals:** per Q-4's resolution; zoom/live render show stone
  without a reload — delivered by #2485's own zoom terrain refresh, not by
  quad-cache invalidation alone (D-7, corrected).
- **Open questions:** None (D-7).

## Source notes

Origin: `docs/holistic_project_audit_findings.md` HPA-3 (marked `[deferred]`
pending this arc's epic). The owner chose "Reaction (lava+water→stone)" over
blocking, precedence, and deferral on 2026-08-31.
