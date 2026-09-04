# Hydrology and fluid pipeline

River, lake, ocean and ice logic is spread across five namespaces. This document
states which stage each namespace owns, so "where does river X live?" is
answerable without grepping five trees.

It is a **namespace-ownership map**, not a design rationale for any single
algorithm — each module's own haddock remains the authority on how it works.
Section numbers here are cited from production haddock; keep them stable.

## 1. Scope

Covered: fresh water (rivers, lakes, the water table), ocean, sea ice, and the
runtime fluid simulation. Glaciers are covered only where they share the
hydrology stages, because they ride the same per-Age event mechanism.

Not covered: lava. `World.Magma` owns lava placement end to end;
`World.Fluid.Lava` is a thin "does this chunk have any lava?" predicate over
`lcMagma`, and the lava-pool table on the timeline is built by
`World.Magma.Pool.identifyLavaPools`. It is listed here only because it lives
under `World.Fluid`.

## 2. Stage map

The pipeline is **not** one linear evolution → identification → carving
sequence. There are two distinct hydrology stages running on two distinct
inputs — a per-Age geological stage inside the timeline loop, and a global
identification stage whose main identifiers run on the settled terrain that
loop produced — followed by per-chunk composition and, only at runtime, an
actual fluid simulation. Two of stage 3's products are prepared from earlier
grids rather than the settled terrain; §5 separates them.

| # | Stage | Runs | Owning namespace | Entry point |
|---|---|---|---|---|
| 1 | Per-Age geological hydrology | once per Age, inside the eon loop | `World.Hydrology.Simulation`, `World.Geology.Timeline.River`/`RiverTrace`, `World.Hydrology.River`, `World.Hydrology.Glacier` | `World.Geology.Timeline.Loop.buildEonLoop` |
| 2 | Compaction + stitched terrain | once, after the loop | `World.Geology.Timeline.Compact`, `World.Geology.Timeline.Stitch` | `World.Geology.Timeline.buildTimeline` |
| 3 | Global identification (mostly on settled terrain — see §5) | once, after the loop | `World.Fluid.*` (`Ocean`, `Lake.Identify`, `River.Identify`, `Seabed`, `OceanMask`, `IceLevel`) | `World.Geology.Timeline.buildTimeline` |
| 4 | Per-chunk composition | per chunk, at chunk gen | `World.Generate.Chunk`, `World.Generate.Chunk.Fluid`, `World.Hydrology.WaterTable`, `World.Fluid.Ice` | `World.Generate.Chunk.generateChunk` |
| 5 | Runtime fluid simulation | per tick, on loaded chunks | `Sim.Thread`, `Sim.Fluid.Active` | `Sim.Fluid.Active.simulateActiveTick` |

`World.Fluid` does **not** own runtime simulation. Stages 1–4 are all
worldgen; stage 5 is the only place fluid actually moves, and it lives under
`Sim`.

## 3. Stage 1 — per-Age geological hydrology

Inside `World.Geology.Timeline.Loop.buildEonLoop`, each Age runs, in order:

1. **`World.Hydrology.Simulation.simulateHydrology`** — coarse flow
   accumulation over the evolving `ElevGrid`. This is *geological* flow for one
   Age (where water would run given this Age's terrain and climate), not a
   fluid simulation. The implementation is
   `World.Hydrology.Simulation.Flow`; depression filling is
   `.PriorityFlood`, grid construction and update `.Grid`, shared types
   `.Types`, and lake dedup `.LakeDedup`.
2. **`World.Geology.Timeline.River.reconcileHydrology`** — turns that flow
   result into river features: evolve existing rivers, spawn new ones from
   flow-sim sources, fold in lake reconciliation. Split across
   `.River.Reconcile` (reconciliation), `.River.Evolve` (per-river per-Age
   meander / branch / deepen / widen / hold), and `.River.SourceDiversity`
   (spatially diverse source selection).
   Tracing a source down the flow-direction chain into a `RiverSegment`
   polyline is `World.Geology.Timeline.RiverTrace` and its submodules
   (`.Unwrap`, `.Subdivide`, `.Coast`, `.Noise`, `.Build`).
3. **`World.Geology.Timeline.River.mergeConvergingRivers`** — tributary
   merging (`.River.Merge`), a `TimelineBuildState → TimelineBuildState`
   rewrite of the river features.
4. **Glaciers** — `World.Hydrology.Glacier.generateGlaciers` /
   `evolveGlacier` (`.Glacier.Generation`, `.Glacier.Evolution`).

Only some of those produce events. `reconcileHydrology` returns a `[GeoEvent]`
alongside its features and updated build state, and glacier generation and
evolution emit their own `HydroEvent`s; both land on the Age's period.
`simulateHydrology` produces a `FlowResult` and no events, and
`mergeConvergingRivers` only rewrites the build state — their effect on terrain
is indirect, through the river features reconciliation later turns into events.

The terrain effect of an event is applied later, per column, through
`World.Hydrology.Event.applyHydroFeature` / `applyHydroEvolution`, dispatched
from `World.Geology.Event`. River carving itself is
`World.Hydrology.River` (`.River.Carving`, with `.River.Meander` and
`.River.Tributary`); glacier carving is `World.Hydrology.Glacier.Carving`.

This is carving mechanism **A** — see §8.

## 4. Stage 2 — compaction and stitched terrain

After the eon loop, still in `World.Geology.Timeline.buildTimeline`:

- **`World.Geology.Timeline.Compact.compactRiverEvents`** strips stale river
  `HydroEvent`s from every period and re-emits exactly one per currently-active
  river, carrying its FINAL state, into the most recent Age. Without this a
  river only ever carves the shape it had at birth. (Compaction is also where
  `World.Fluid.River.fixupSegmentContinuity` runs — see §10.)
- **`World.Geology.Timeline.Stitch`** builds the per-chunk timeline windows and
  stitches their interiors into an unambiguous global grid; the global coastal
  pass runs on that grid, and `stitchWorldTerrain` yields the settled
  `worldTerrain` the §5 identifiers run against.

## 5. Stage 3 — global identification

Still inside `buildTimeline`, the `World.Fluid.*` identifiers decide where
water finally is, and write their results onto `GeoTimeline`. They are ordered
by data dependency, not by textual order in the `let`.

**Prepared from earlier grids, not from the settled terrain.** These two are
inputs to the identifiers below and predate the stitch, so despite living in
the same stage they are not settled-terrain work:

| Produces | Module | Reads | Timeline field |
|---|---|---|---|
| Ice-level grid | `World.Fluid.IceLevel.computeIceLevelGrid` | the eon loop's final `ElevGrid` | `gtIceLevel` |
| `OceanMap` + `OceanDistMap` (chunk resolution) | `World.Fluid.Ocean.computeOceanMap` | the pre-lake timeline, sampled through `applyTimelineFast` | returned alongside the timeline |

**The identifiers proper, all reading the settled `worldTerrain`:**

| Produces | Module | Timeline field |
|---|---|---|
| Final lakes, incl. rift-lake bed carves | `World.Fluid.Lake.Identify.identifyWorldLakes`, `World.Fluid.Lake.Graben.grabenCarveIndex` | `gtWorldLakes` |
| Final rivers, incl. channel-bed fit | `World.Fluid.River.Identify.identifyWorldRivers` | `gtWorldRivers` |
| Continental-margin seabed relief and materials | `World.Fluid.Seabed.identifySeabed` | `gtSeabed` |
| Tile-resolution rendered-ocean bitmask | `World.Fluid.OceanMask.buildWorldOceanMask` over `Lake.Identify.Ocean.computeRenderedOcean` | `gtWorldOcean` |
| Lava pools (see §1) | `World.Magma.Pool.identifyLavaPools` | `gtWorldLavaPools` |

Lake identification is split across `World.Fluid.Lake.Identify` (the pipeline),
`.Identify.Ocean` (world-edge ocean BFS and coastal-basin detection),
`.Identify.Flood` (bucket-queue priority flood), `.Identify.Components`
(basin labelling and lake construction), and `.Identify.ChunkIndex`
(per-chunk index). River
identification splits the same way: `World.Fluid.River.Identify` plus
`.Identify.Flow`, `.Identify.Components`, `.Identify.BedDepth`,
`.Identify.Breakthrough`, `.Identify.ChunkIndex` and `.Identify.Common`.

Rivers and lakes each emit a per-chunk **carve delta** here
(`wrCarveDelta`, `wlCarveDelta`). That is carving mechanism **B** — see §8.

### 5.1 Spillway ownership is one-to-many

`World.Fluid.River.Identify.Flow.computeSpillways` gives each lake ONE
outlet tile: the lowest above-sea neighbour of any of its tiles that is not
its own (`-1` where the lowest such neighbour is at or below sea level — that
basin drains to the ocean, not to a river). Nothing constrains two *adjacent*
basins from picking the **same** tile, so the per-tile inverse of that table is
a **relation, not a function**.

That inverse is `SpillwayOwners` (`.Identify.Common`), a compressed-sparse-row
map from tile to the complete set of lakes spilling through it, always in
ascending `LakeId` order. The ordering is a property of how it is built, not of
a later sort: no consumer can observe an owner that depends on traversal order.
`Flow.resolveSpillways` is the whole stage — outlet selection, inversion,
descent, and the demotion below — and is the only thing `identifyWorldRivers`
calls for it.

**All contributing basins are excluded from the descent.** `computeDescentDirs`
gives a spillway tile a steepest-descent direction chosen from neighbours in
*none* of its contributing basins. Excluding only one of them would let a
second contributor's injected outflow step straight back into its own lake,
where `walkInject` absorbs it — leaving that lake with no usable outlet at all.

**The no-descent fallback: spillway `-1`, before injection.** If excluding every
contributor removes the shared tile's *last* descent candidate, keeping the tile
as anyone's outlet would still add each contributor's accumulated flow there,
because `computeFlowAccumulation` injects before `walkInject` ever observes the
missing direction. So `demoteBlockedSharedSpillways` treats **every**
contributor of such a tile as having spillway `-1`: none of their flow is added
at the tile, no injection walk starts from it, and it contributes no river
source. Two bounds keep this to the collision case:

- It fires only where the **contributor exclusion** removed the last candidate.
  A shared tile with no descent for an unrelated reason — it is itself a third
  lake's tile, or has no lower non-void neighbour at all — keeps its
  contributors, because nothing is being routed back into itself there.
- It fires only on tiles with **two or more** contributors. A unique-owner
  spillway with no valid descent still injects, and its walk simply terminates.

The demotion changes injection and source metadata only. The tile keeps the
`dirNone` the exclusion produced; there is no second, exclusion-free descent
pass that could re-admit a contributing basin for it.

### 5.2 `rivSourceLake` is a component-wide union

`.Identify.Components.buildRivers` collects, over the whole connected river
component, the union of contributors of every spillway tile it contains — both
basins of a shared outlet, and the separate basins of two non-shared outlets
that happen to feed one component. `River.rivSourceLake` is `Just lakeId` **only
when that union names exactly one lake**, and `Nothing` otherwise.

So `Nothing` means "does not have exactly one lake source", which is broader
than the "precipitation-fed" it used to mean: a river fed by two basins sharing
one spillway, or by two lakes' separate spillways, records `Nothing` rather than
an arbitrarily elected representative. The field stays
`Maybe LakeId` — the `River` wire shape and the `world-pages` component version
are unchanged, and historical scalar values decode exactly as written.

## 6. Stage 4 — per-chunk composition

`World.Generate.Chunk.generateChunk` composes one chunk from the tables stages
2–3 produced. The chain below is a data dependency, not merely the order the
bindings appear in:

1. Timeline application (which is where stage 1's `HydroEvent` carves land),
   then seabed, then **subtract the stage 3 carve deltas** —
   `max` of the river and lake delta at each tile — then a post-carve despike.
   `World.Generate.Chunk.Zoom` mirrors this for the zoom cache.
2. **`World.Generate.Chunk.Fluid.composeFluidMap`** — surface fluid placement.
   It reads the global tables (`gtWorldLakes`, `gtWorldRivers`, `gtWorldOcean`,
   `gtWorldLavaPools`), not the water table: ocean from the coarse chunk flood
   OR'd with the tile-resolution mask, lakes from each lake's surface, rivers
   from each river chunk entry's per-tile `rcePerTileSurfZ`, lava on top.
3. **`World.Fluid.Ice.computeChunkIce`** — the per-chunk ice overlay.
4. **`World.Hydrology.WaterTable.computeWaterTable`** — the subsurface
   baseline, then `World.Generate.Chunk.SoilGates.applyFluidWt` lifts it now
   that the fluid map is known. See §11.

## 7. Stage 5 — runtime fluid simulation

The only stage where fluid moves. `Sim.Thread` ticks loaded chunks and
`Sim.Fluid.Active.simulateActiveTick` runs the volume-conserving simulation
over `Sim.Fluid.Types.ActiveFluidCell`, per world
(`Sim.State.Types.SimWorldState`). Chunks settle, then deactivate at
equilibrium; results are emitted back to the world thread.

Nothing under `World.Fluid` or `World.Hydrology` participates in this stage
beyond supplying the initial `FluidCell` map that stage 4 composed.

## 8. The two carving mechanisms

Both exist deliberately; they operate on different inputs and do not
double-carve.

**A — geological-history carving (stage 1).** `HydroEvent`s from the per-Age
loop, applied through `World.Hydrology.River` / `World.Hydrology.Glacier` as
the timeline is applied to a column. This is what produces migrating rivers,
widening valleys and terraces — the evolution of the landscape. Dropping it
loses valley history.

**B — final channel-bed fit (stage 3, applied in stage 4).** The global
identifiers run on already-settled terrain and emit a bounded per-tile
`wrCarveDelta` / `wlCarveDelta`, subtracted during chunk generation. This
needs the finalized path, width and surface that only exist after stage 2, so
it cannot be folded into A; it is a top-up to channel depth on terrain A
already shaped.

## 9. Ocean and lake ownership

Ocean is split by resolution, seeding, and purpose. The two floods start from
completely different places — do not conflate them:

- **`World.Fluid.Ocean`** — `computeOceanMap`, the coarse **chunk-resolution**
  flood, plus per-chunk distance-from-ocean. It is **seeded from tectonic
  plates**, not from the world edge: each non-land plate contributes its centre
  chunk, spiralling outward up to 4 chunks if that chunk is above sea level (so
  an ocean plate with a volcanic island centre still gets a seed). The BFS then
  propagates through chunks whose *median* elevation over five sample points is
  at or below sea level, which is what stops ocean bleeding inland through a
  single low corner. Its types (`OceanMap`, `OceanDistMap`, `oceanDistAt`) live
  in **`World.Ocean.Types`**, separately, so they can be depended on without
  the computation.
- **`World.Fluid.Lake.Identify.Ocean`** — the **tile-resolution** ocean work,
  and the only **world-edge** flood: `computeWorldEdgeOcean` (the
  edge-connected ocean that seeds the priority flood and is excluded from
  basin labelling) and `computeRenderedOcean` (the wider "renders as ocean
  anywhere" flood, including enclosed inland seas with an oceanic core).
- **`World.Fluid.OceanMask`** — packs `computeRenderedOcean`'s grid into the
  per-chunk bitmask stored as `gtWorldOcean`, which is what lets chunk gen fix
  sea areas that used to stop dead at a chunk boundary.
- **`World.Generate.Chunk.Fluid`** — surface composition: ORs the coarse
  chunk test with that tile mask (§6.2).
- **`World.Fluid.Seabed`** (+ `.Seabed.Types`) — what the ocean floor is made
  of and shaped like, not where the ocean is.

Lakes are identified once, globally, by **`World.Fluid.Lake.Identify`** and its
submodules (§5), stored as `gtWorldLakes` (`World.Fluid.Lake.Types`), and
placed per chunk by `composeFluidMap`. `World.Fluid.Lake.Graben` supplies
inland rift-lake bed carves. There is no per-chunk lake identification.

## 10. River segment geometry vs. rendered water surface

`RiverSegment` (`World.Hydrology.Types`) describes **channel geometry only** —
endpoints, width, valley width, depth, flow rate, and reference terrain
elevations. It carries no water-surface field.

The rendered water surface does **not** come from segment geometry and does
**not** come from the water table. It is per-tile data on the global river
table: `World.Fluid.River.Identify` computes it, `.Identify.ChunkIndex` stores
it as `rcePerTileSurfZ` on each `RiverChunkEntry`, and
`World.Generate.Chunk.Fluid.composeFluidMap` reads it directly (§6.2).

Segment geometry still has to be self-consistent, which is what
`World.Fluid.River.fixupSegmentContinuity` enforces — adjacent segments share
endpoints and elevations are monotonically non-increasing downstream. Despite
its namespace it is a **stage 1/2 helper**, never called from chunk
generation. Its three callers are `World.Geology.Timeline.RiverTrace.Build`
(building a traced path's segments), `World.Geology.Timeline.River.Evolve`
(after a meander and after a deepen/widen), and
`World.Geology.Timeline.Compact` (after resampling a compacted river).

## 11. Subsurface water table

`World.Hydrology.WaterTable` owns the **subsurface** saturation horizon only —
what a player finds when they dig. The model is climate-only:
`wt[t] = terrain[t] − depthFromClimate(t)`, with no priority flood and no
per-tile spillway propagation, because surface water placement belongs to the
global tables (§5, §6.2).

`World.Generate.Chunk.SoilGates.applyFluidWt` then lifts that baseline once the
fluid map is known: an under-fluid bump (digging through a lake bed, river bed
or ocean floor exposes water) plus a fresh-water shore halo feeding the
wetland-soil gate. The result is stored as `lcWaterTableMap` on
`World.Chunk.Types.LoadedChunk`, and queried through `waterTableAtTile` /
`isSubsurfaceWet`.

## 12. Where does X live?

| Question | Answer |
|---|---|
| Where do rivers get *born* and *evolve*? | `World.Geology.Timeline.River.*` (stage 1) |
| Where is a river's *path* traced? | `World.Geology.Timeline.RiverTrace.*` |
| Where do rivers *carve valleys*? | `World.Hydrology.River.*` via `World.Hydrology.Event` (mechanism A) |
| Where are the *final* rivers decided? | `World.Fluid.River.Identify.*` (stage 3) |
| Where does a river's rendered surface come from? | `rcePerTileSurfZ`, §10 |
| Where are lakes decided? | `World.Fluid.Lake.Identify.*` (stage 3) |
| Where is "is this chunk ocean?" decided? | Coarse: `World.Fluid.Ocean`. Tile: `World.Fluid.Lake.Identify.Ocean` + `World.Fluid.OceanMask`. §9 |
| Where is surface fluid actually placed on a chunk? | `World.Generate.Chunk.Fluid.composeFluidMap` |
| Where is groundwater? | `World.Hydrology.WaterTable`, §11 |
| Where does water actually *flow* at runtime? | `Sim.Fluid.Active`, §7 |
| Where is flow accumulation for a geological Age? | `World.Hydrology.Simulation.Flow`, §3 |
| Where are glaciers? | `World.Hydrology.Glacier.*` |
| Where is sea ice? | `World.Fluid.IceLevel` (world grid) + `World.Fluid.Ice` (per chunk) |
| Where is lava? | `World.Magma.*`; `World.Fluid.Lava` is only a predicate. §1 |

## 13. Outside the pipeline

- **`World.River.Identity`, `World.River.Naming`** — river *identity* and
  *naming* (issues #1102/#1104), not hydrology. They read the identified
  rivers; they move no terrain and no water.
- **`World.Fluid.Internal`, `World.Fluid.Types`** — shared fluid types and
  helpers, no stage of their own.

## 14. Related documents

- `docs/code_health_findings.md` CH-80 (this document's origin), CH-81 (the
  `World.Fluids` / `World.Fluid.*` naming collision).
- `docs/history/river_rework.md` — an archived, superseded river redesign.
  Historical only.
