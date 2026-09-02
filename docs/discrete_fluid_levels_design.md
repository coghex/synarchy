# Discrete fluid levels design

Synarchy's active fluid solver already moves conserved integer quantities, but
the world boundary rounds every non-zero remainder up to a whole z-level and
the renderer hides adjacent height differences behind terrain-style slopes.
This design makes the quantity visible and durable: eight exact wet levels per
z, flat water surfaces, and stepped edges whose height matches the simulation.
Ocean uses the same convention even while generated seas remain calm planes,
so later wave simulation can vary ocean quantity without another state-format
or rendering migration.

Design state: `ready for issue processing`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [ ] EPIC. Make fluid quantity visible as eight deterministic levels
- [ ] DFL-1. Replace freshwater slope interpolation with whole-z flat steps
- [ ] DFL-2. Make eighth-z fluid state exact, conserved, and durable
- [ ] DFL-3. Create and approve the eight fluid-level face maps
- [ ] DFL-4. Render eighth-level fluid tops and partial step faces
- [ ] DFL-5. Give generated rivers deterministic eighth-level surface progression
- [ ] DFL-6. Add exact-level diagnostics and end-to-end compatibility gates

## Epic contract

- **Goal:** Make a tile's visible fluid height communicate the exact quantity
  the simulation owns, using eight deterministic wet levels per z instead of
  terrain-style water slopes or rounded whole-z surfaces.
- **Done when:** dry is level 0 and wet levels 1 through 8 fill one z; exact
  levels survive active/passive transitions, chunk writeback, eviction,
  save/load, and reactivation without gaining or losing fluid; old integer
  saves migrate to the same full-level heights they displayed before; fluid
  tops render flat at their exact height with matching partial side faces;
  every fluid type, including Ocean, uses the same exact state, persistence,
  query, and rendering convention without an integer-only ocean exception;
  generated oceans initialize as calm full-level planes but edited/runtime
  ocean cells can already retain and display partial levels;
  generated rivers can descend in eighth-level steps while lakes and oceans
  remain level; every required level mask has explicit owner approval; and
  deterministic, persistence, seam, conservation, render-geometry, and visual
  gates cover the result.
- **Users and operators:** players reading flow depth and shallow water from the
  world view; worldgen, simulation, rendering, persistence, and asset
  maintainers; Lua/tooling authors that inspect surface state.
- **Tracker relationship:** no existing closed epic owns this outcome. Closed
  #222 and #816 established freshwater slope interpolation, #1135 repaired its
  seam lookup, #1600 documented its flattening ambiguity, and #1685 repaired a
  related wet-neighbour rule; this arc deliberately supersedes that visual
  contract rather than reopening any completed issue. Open #1997 owns chunk
  residency and future sparse fluid authority, not fluid precision or
  presentation. `docs/fluid_reaction_design.md` owns unlike-fluid reactions.
- **Arc label:** `fluid` proposed; no matching label exists today.

## Current state and evidence

Verified on master `5dc077ffd1` and the tracker on 2026-08-31:

- The hydrology pipeline has five stages. `World.Fluid.*` identifies generated
  surface water; `World.Generate.Chunk.Fluid` composes it into chunks; only
  `Sim.Fluid.Active` moves runtime fluid. The ownership boundary is documented
  in `docs/hydrology_pipeline.md` §§2, 6, and 7.
- A resident column has at most one `FluidCell`, containing `fcType` and an
  integer `fcSurface` (`src/World/Fluid/Types.hs:29-36`). It records neither an
  exact fractional surface nor an independently durable volume.
- Ocean is already an ordinary `FluidType`. Chunk composition constructs it as
  `FluidCell Ocean seaLevel`, while active conversion and writeback operate on
  `FluidType` generically rather than through an ocean-specific state shape
  (`src/World/Generate/Chunk/Fluid.hs:194-209`,
  `src/Sim/Fluid/Types.hs:12-51`). A shared exact convention therefore extends
  the existing type boundary instead of introducing a second ocean model.
- An active cell carries `afcVolume :: Word16`, and
  `volumePerLevel = 7` (`src/Sim/Fluid/Types.hs:13-23`). The solver can create
  different sub-z quantities, but `volumeToSurface` immediately exposes them
  as `terrainZ + ceil(volume / 7)` (`:25-29`): volumes 1 through 7 all publish
  the same surface.
- `activeToFluidCell`, active-chunk writeback, and deactivation all use that
  rounded integer (`src/Sim/Fluid/Types.hs:44-51`,
  `src/Sim/Fluid/Active.hs:54-69,107-118`,
  `src/Sim/Thread.hs:352-390`). Reactivating reconstructs volume as whole
  integer depth times seven. A one-unit cell can therefore be published as one
  full z and later reconstructed as seven units; the sublevel is not durable.
- Gravity and seam pressure compare `volumeToSurface` integer results, although
  equal-terrain lateral equalisation compares volume directly
  (`src/Sim/Fluid/Active.hs:130-150,250-310,317-367`). Exact quantities exist,
  but not every transfer decision sees their exact surface.
- Saves enumerate each loaded `lcFluidMap` into `WeSetFluidSnapshot` edits that
  carry only fluid type and integer surface
  (`src/World/Thread/Command/Save/WriteWorld.hs:572-597`,
  `src/World/Edit/Types.hs:97-102`). The wire shape is `world-edits` v1's frozen
  `WorldEditDTO` (`src/World/Save/Component/Page.hs:354-400,1361-1384`).
- `world.getFluidAt`, `world.getSurfaceAt`, the area query, cursor text, and the
  dump expose integer surfaces. There are 41 production `fcSurface` uses across
  18 modules, so changing the meaning must go through named conversion helpers
  rather than scattered arithmetic.
- Generated lakes have one integer `lkSurface`; every lake tile shares it
  (`src/World/Fluid/Lake/Types.hs:36-54`). Generated rivers carry one integer
  `rcePerTileSurfZ` per river tile, described as quantised surface elevation
  (`src/World/Fluid/River/Types.hs:64-79`). The river identifier bounds whole-z
  differences using `waterfallQuantum` and copies a centre's plane across its
  widened cross-section (`src/World/Fluid/River/Identify.hs:188-205,231-241`).
- Freshwater rendering ignores the exact magnitude of a drop. `waterSlopeAt`
  sets direction bits whenever a wet neighbour's fluid surface or a dry
  neighbour's terrain surface is lower, then selects an ordinary terrain slope
  face map (`src/World/Render/WaterSlope.hs:30-92`,
  `src/World/Render/TileQuads.hs:287-329`). Seven different neighbour
  topologies collapse to the nonsensical all-corners mask and are flattened;
  #1600 and its tests document that representational limit.
- Water side faces deliberately begin only at gaps of two or more whole z,
  because a one-z gap is assigned to the slope quad
  (`src/World/Render/SideDecoQuads.hs:56-75`). Flat steps require replacing
  that ownership rule, not merely passing slope id zero.
- The world sprite is 96x64 with a 16-pixel vertical side
  (`src/World/Grid.hs:47-72`). Eight levels therefore land on exact two-pixel
  increments at native resolution. Eighths are also exactly representable as
  binary floating point at the render boundary.
- The fragment shader multiplies the material texture by a face map whose alpha
  defines the visible silhouette and whose RGB encodes face lighting
  (`src/Engine/Graphics/Vulkan/ShaderCode.hs:245-298`). Eight ordinary colour
  textures with one unchanged face map would not create eight geometric fill
  levels.
- `World.Slope.FaceMaps` already generates flat, ramp, and side masks
  procedurally at 96x64 (`src/World/Slope/FaceMaps.hs`). The shipped PNGs are
  loaded by Lua; the generator is currently an exported but otherwise unused
  implementation reference.
- Relevant open work must land before the solver precision slice: #2042 fixes a
  lateral `Word16` underflow that can manufacture volume, and #2044 makes
  runtime seam exchange use the world's cylindrical topology. The ready but
  unprocessed unlike-fluid reaction design changes the same four transfer
  sites and should consume the selected eight-unit foundation rather than
  independently freezing seven-unit fixtures.
- `docs/chunk_residency_streaming_design.md` / epic #1997 later move accepted
  fluid overrides into sparse authority (CRS-5/CRS-12). That arc must preserve
  exact eighth units, but its deferred storage overhaul is not a prerequisite
  for this player-visible feature: `world-edits` can carry the exact state in
  the interim.

## Desired experience

A tile with one eighth of water shows a shallow two-pixel-deep fill. Additional
quantity raises it in visibly regular two-pixel steps until level 8 reaches one
full z. A neighbouring lower tile is not joined by a soft terrain ramp: each
top stays flat and the exposed difference is a vertical water edge. The display
therefore answers “how much water is here?” rather than suggesting a continuous
surface the solver does not own.

Generated lakes and oceans remain calm planes. That calm ocean is an
initialization policy—`seaLevel * 8` on every generated ocean tile—not a type,
storage, activation, query, or renderer restriction. An edited/runtime Ocean
cell can already retain and display any of the eight levels. Generated rivers
use the same eighth-unit vocabulary to descend gradually downstream and hold
one exact plane across a widened cross-section. A real whole-z waterfall
remains a waterfall; the finer representation reduces unnecessary whole-block
drops without smearing a lip into a ramp.

Simulation remains deterministic integer arithmetic. Rendering converts the
exact integer surface to `Float` only while constructing vertices. Future
ocean-wave work may move exact units between ocean cells and may layer
non-authoritative interpolation between simulation ticks. This epic does not
choose that wave algorithm or cadence; it ensures the ocean cells, persistence,
transfer boundaries, queries, and geometry are ready to consume it. Visual
interpolation never changes stored quantity, collision answers, save bytes, or
level selection.

## Scope

### In scope

- One shared fixed-point vocabulary: 8 units per z, wet levels 1..8, 0 dry.
- The vocabulary and conversion APIs apply to every `FluidType`, including
  Ocean; generated ocean flatness is policy rather than representation.
- Exact active/passive conversions and pressure comparisons in surface units.
- Lossless writeback, deactivation, reactivation, edit replay, eviction handoff,
  and save/load through the current authority boundary.
- Backward-compatible migration of integer fluid snapshots to exact full
  levels.
- Flat stepped freshwater presentation, followed by exact eighth-height tops
  and partial side faces across in-chunk and loaded seam neighbours.
- Eight level-shaped face maps/masks, with a separate tracked asset slice and
  explicit owner signoff.
- Generated river surface progression in eighth units, including widened
  cross-section consistency, breakthrough mouths, waterfall bounds, carve/render
  agreement, deterministic output, baseline recapture, and component migration.
- Exact-level dump/cursor diagnostics and compatibility documentation.
- Coordination requirements for #1997's future sparse fluid component and the
  unlike-fluid reaction arc.

### Out of scope

- Arbitrary or authoritative floating-point fluid height.
- More than one fluid type or independently simulated vertical fluid cell in a
  single column.
- Implementing waves, surface normals, refraction, foam animation, or per-frame
  smoothing. Future wave simulation may transfer exact ocean units and add
  render-only interpolation without changing this model.
- New fluid types, steam, lava-water reaction rules, temperature, evaporation,
  rainfall, or weather-driven fluid input.
- Redesigning lake/ocean identification, river footprint selection, bed depth,
  erosion, water-table ownership, or runtime tick cadence except where exact
  surface arithmetic directly requires it.
- Making zoom-map pixels communicate eighth-level depth; their scale is too
  coarse, though their water classification must remain correct.
- Replacing resident snapshots with sparse storage or redesigning chunk
  residency; #1997 owns that work.
- Changing existing integer Lua query return arity silently. Any new exact
  query must coordinate with the checked Lua API arc (#1995).

## Design

### Exact surface vocabulary

The authoritative representation is a signed fixed-point absolute surface,
provisionally `newtype FluidSurfaceU = FluidSurfaceU Int`, measured in eighths
of z. Signed storage is required because world elevations can be negative. One
constant, provisionally `fluidUnitsPerZ = 8`, owns the scale.

The representation has named total helpers rather than ad-hoc division:

- `fullSurfaceU z = z * 8` converts an old/generated whole-z plane;
- `surfaceHeightF u = fromIntegral u / 8` is render-only;
- `surfaceCeilingZ u` supplies integer slice/collision consumers;
- `surfaceFloorZ u` supplies terrain/carve comparisons where the occupied
  integer cell matters;
- `topFillLevel u` maps a positive top-slab remainder to 1..8, treating an
  exact multiple as level 8 rather than level 0;
- `volumeAt terrainZ u = max 0 (u - fullSurfaceU terrainZ)` converts an
  absolute plane into same-footprint volume units.

`FluidCell` stores the exact absolute surface rather than a redundant pair of
integer surface plus sublevel. This applies uniformly to Ocean, Lake, River,
and Lava; no constructor or serialized branch may retain an integer-only ocean
surface. Keeping two authoritative fields would admit impossible combinations
and recreate the drift this arc removes.

`lcSurfaceMap` remains an integer map. It answers which z slice owns a rendered
or interactive column and uses the exact fluid plane's documented integer
ceiling under the existing River-versus-other terrain rule. Exact fluid state
lives only on `FluidCell`; an integer consumer must choose a helper whose name
states whether it wants ceiling, floor, or render height.

### Active simulation and conservation

`ActiveFluidCell.afcVolume` remains integer quantity. The scale changes from
seven to eight units per z. For ordinary cells, the exact absolute surface is
`terrainZ * 8 + volume`; unlike the current `volumeToSurface`, gravity and seam
pressure compare that exact value. Equal-terrain lateral equalisation continues
to compare volumes, now under the same eight-unit scale.

No active/passive boundary may round. An active cell writes a `FluidCell` whose
exact surface represents every remaining unit; deactivation clears active
scratch only after copying that value; activation reconstructs the same volume.
The rule is fluid-type agnostic, so a future ocean-wave solver can activate and
transfer Ocean cells without a representation conversion.
Pure round-trip laws cover all valid volumes, multiple-z depths, negative
terrain, zero/dry, and `Word16` bounds.

#2042 and #2044 are external prerequisites for the slice that claims exact
conservation. The unlike-fluid reaction arc should follow DFL-2; if its FR-1
lands first, DFL-2 must migrate its fixtures and retain its reaction arithmetic
unchanged rather than overwrite it.

### Persistence and migration

`world-edits` v1 is frozen. The exact-state slice introduces a v2 DTO and an
explicit v1 migration. Every historical `WeSetFluidSnapshot ... surfaceZ`
becomes exact `surfaceZ * 8`, preserving the old save's displayed whole-z plane
and treating it as level 8. No old save fabricates a fractional remainder.

Current saves write exact units, not a float and not an integer ceiling. The
tracked compatibility fixture corpus, component hash/size guards,
`save_compat_audit.py`, and the fresh-process migration probe cover the new
version. `docs/persistence_state_inventory.md` records `world-edits` v2 and the
precision promise.

When CRS-12 later replaces resident snapshots with a sparse versioned fluid
component, its migration input includes both v1 whole-z snapshots and v2 exact
snapshots. It must preserve exact units and `needsSettlement`; it may not derive
authority from `lcSurfaceMap` or another rounded cache.

### Flat step presentation

The first visual slice intentionally uses today's integer surfaces. River and
Lake tops select the flat face map, `waterSlopeAt` leaves the production path,
and the side-face generator owns every positive visible difference, including
the one-z gap it currently delegates to a slope. This is a reversible visual
proof of the selected language and retires the seven-topology mask-15
ambiguity before fractional state complicates it.

The exact renderer later computes top placement from `surfaceHeightF`. A top
stays flat regardless of neighbouring height. For each camera-visible edge, the
renderer compares exact owning and neighbouring fluid planes; a dry neighbour
uses its terrain surface. It emits the necessary full-z side segments plus one
partial segment so the side terminates at the exact top and bottom. Loaded
cross-chunk and cylindrical-seam results match in-chunk results; an unloaded
neighbour remains conservative and emits no invented edge.

Texture selection derives from `topFillLevel`, never from float rounding. The
same level mask shapes every fluid material; colour/tint remains fluid-type
specific. River, Lake, Lava, and edited/runtime Ocean opt into fractional
geometry. Generated Ocean begins at level 8 and therefore remains visually
flat, but it travels through the identical level-selection path.

### Level masks and assets

Geometry belongs in the face map because the shader uses face-map alpha as the
silhouette and face-map RGB for directional lighting. The selected art model is
one existing material texture per fluid appearance plus eight level-shaped
face maps, not eight redundant copies of the water colour texture.

Each mask is 96x64 and grows the visible vertical side in exact two-pixel
increments. Its flat top remains the same isometric footprint; left/right side
channels and alpha cover only the fill height owned by that level. A contact
sheet must show all eight levels over representative light/dark terrain and at
the supported camera facings. The owner signs off every mask before renderer
integration. The tracked PNGs are generated deterministically from one extended
`World.Slope.FaceMaps` definition, then reviewed as visual assets rather than
treated as unreviewed build products (D-10).

### Generated rivers

Lakes and oceans keep one exact full-z plane by default: their identity is a
level basin/sea, and introducing generated fractional tilt has no product
justification. For Ocean this is solely a generation policy. Each ocean
`FluidCell` still stores exact units and uses the same active, save, query, and
render paths, so later wave simulation can create partial neighbouring levels
without migrating a special-case ocean representation. Generated rivers gain
exact per-tile surface units. The downstream chain stays
monotone, widened wings inherit their centre's exact plane, breakthroughs meet
their sink plane exactly, and lateral adjacency remains bounded. Fractional
steps distribute whole-z descent across a reach instead of presenting every
change as a block cliff.

The slice must not accidentally change river footprint, width, lake identity,
or terrain carve merely because display precision increased. Any intentionally
changed carve is separately justified by a surface/bed consistency invariant.
The exact interpolation algorithm is an implementation proposal until the
slice investigates the chain topology; acceptance is observable: deterministic
monotone eighth-step surfaces, flat cross-sections, no uphill flow, and no gap
between a breakthrough and its sink.

Because `rcePerTileSurfZ` is serialized under the `world-pages` component and
the output itself changes, DFL-5 freezes the previous DTO, bumps that component,
migrates old integer surfaces by multiplying by eight, runs the full worldgen
output tier, regenerates tracked baselines, and follows the repository's
worldgen save-version policy.

### Queries and diagnostics

Existing Lua calls retain their integer return count and documented integer
surface meaning during this arc. Their value is the named integer ceiling of
the exact plane. An exact query, if desired, is added through #1995's checked
API as an additive function or structured field, not by silently appending a
third return value.

The dump and cursor diagnostics gain unambiguous exact fields such as
`fluidSurfaceUnits` and `fluidLevel`, while retaining `fluidSurf` as the
integer compatibility view. Diagnostics print both only where fluid exists;
dry remains absence, not level zero masquerading as a fluid cell.

## Decisions

### D-1. One z contains eight non-empty fluid levels; dry is separate level 0

Owner decision 2026-08-31. Level 8 is one full z, not another partial level.
Rejected: retaining seven units (it gives seven wet states plus dry, but does
not align with the requested eight wet levels or the 16-pixel side); arbitrary
resolution.

### D-2. Fixed-point integers are authoritative; Float exists only at render

Owner decision 2026-08-31. Eighths preserve exact conservation, comparisons,
deterministic saves, and cross-platform behavior. Eighth fractions are exactly
representable in binary float when vertex positions are finally constructed.
Rejected: storing `Float` on `FluidCell` or in the save format.

### D-3. Fluid tops are flat steps, not terrain slopes

Owner decision 2026-08-31. Neighbour differences are shown by vertical fluid
edges whose heights match state. Terrain slope rendering remains unchanged.
This deliberately supersedes the presentation established by closed #222 and
#816; those issues remain correctly closed records of their former contract.

### D-4. Integer surface maps remain compatibility views over exact fluid state

Owner-approved design consequence of D-2. `FluidCell` owns exact surface units;
`lcSurfaceMap` and existing integer queries choose a documented ceiling/floor
helper. They never become a second source of fluid truth.

### D-5. Historical integer fluid snapshots migrate as full eighth levels

An old saved surface z migrates to `z * 8`. This preserves its visible plane
exactly and invents no fractional history.

### D-6. Generated rivers eventually use the same eighth-level vocabulary

Owner decision 2026-08-31. Runtime-only fractional display would leave the
dominant generated river network in whole blocks, so the epic includes a later
worldgen slice. Lakes and oceans remain level full-z planes unless runtime
simulation changes their quantity.

### D-7. Level geometry uses eight face maps over existing material textures

Owner decision 2026-08-31. Shape belongs in the face-map alpha/lighting
channels. Eight separately coloured water textures are not required for the
geometry and may be considered later only for an observable art benefit.

### D-8. The rollout proves flat whole-z steps before changing state precision

Owner decision 2026-08-31. DFL-1 removes water slopes and makes one-z side
faces visible using today's data. Exact state and fractional geometry then land
behind a validated visual language rather than changing physics, persistence,
assets, and presentation in one PR.

### D-9. This is a new epic, not a reopened closed epic

Tracker review 2026-08-31 found no closed epic with this goal. The matching
closed issues are completed slope/seam repairs with narrow contracts; #768 and
#101 are completed persistence/world-scoping epics, not reusable fluid-feature
umbrellas. Reopening or retitling one would erase useful history and silently
broaden a completed contract.

### D-10. Generate the eight masks deterministically and review the outputs

Owner decision 2026-08-31, resolving Q-1. One checked geometric definition
produces eight tracked 96x64 PNGs in exact two-pixel increments. A contact
sheet still receives explicit owner approval; determinism does not replace art
review. Rejected: eight independently hand-authored masks whose shared geometry
could drift.

### D-11. Generated river terrain must provide real positive fluid depth

Owner decision 2026-08-31, resolving Q-2. DFL-5 repairs generated channel
terrain wherever a River plane would otherwise sit at or below a protrusion.
DFL-2 preserves existing passive cells until that repair instead of inflating a
zero-depth cell into a full shallow level. The implementation must retain fluid
identity during the interim activation path and prove exact round trips once
the generated invariant lands.

### D-12. Every fluid type uses fractional geometry; generated ocean flatness is policy

Owner decision 2026-08-31, resolving Q-3. River, Lake, Lava, and Ocean all use
the same exact state, persistence, activation, queries, masks, and renderer
conventions. Generated Ocean initializes at exact full-level `seaLevel * 8`,
so it remains flat today; edited/runtime Ocean may retain and render partial
levels. No ocean-only integer representation or renderer branch may block a
later wave simulator from transferring exact units between ocean cells.
Implementing wave dynamics and optional render interpolation remains a later
arc.

## Proposals

### P-1. Use one signed `FluidSurfaceU` newtype

This prevents raw integer z and eighth units from type-checking interchangeably
and avoids redundant `surface + sublevel` fields. DFL-2 confirms the practical
integer width and module home.

### P-2. Reuse level masks across fluid material types

Resolved by D-12. The mask describes geometry, so Lake/River/Ocean/Lava combine
it with their existing texture/tint.

### P-3. Generate the masks from one checked geometric definition

Resolved by D-10. `World.Slope.FaceMaps` already demonstrates deterministic
RGBA face-map generation. Extending it makes the two-pixel progression
auditable and can validate shipped PNGs byte-for-byte, while still producing
tracked assets for review.

## Open questions

### Q-1. Are the eight level masks procedural or hand-authored?

Resolved by D-10. The geometric masks are deterministically generated from one
definition, committed as eight PNGs, and presented as a contact sheet for
explicit owner approval.

### Q-2. How does activation handle a generated River cell whose visual plane is at or below protruding terrain?

Resolved by D-11. DFL-5 repairs generated channel terrain so every river cell
has real positive depth; DFL-2 preserves existing passive cells until that
repair rather than silently converting them into a full shallow cell.

### Q-3. Which fluid types receive fractional visual geometry?

Resolved by D-12. Every `FluidType` uses the exact vocabulary and fractional
geometry. Generated Ocean remains a full flat plane by policy, while an
edited/runtime Ocean cell may retain and display a partial level so future wave
simulation does not require a representation migration.

## Verification strategy

- Pure fixed-point laws cover conversion, ceiling/floor helpers, level
  selection, negative z, multi-z depth, maximum `Word16`, and every remainder
  0..7.
- Type-parametric fixtures prove Ocean, Lake, River, and Lava take the same
  exact conversion, save, and level-selection paths. A generated ocean plane is
  an exact multiple of eight; a partial edited/runtime Ocean cell survives
  active→passive→save→load and renders with the matching mask.
- Simulation properties cover volume conservation through gravity, lateral,
  waterfall, and ordinary/cylindrical seams; active→passive→active is identity
  for every valid non-zero volume; deactivation and save do not turn a partial
  cell into a full one.
- Existing #2042 conservation and #2044 seam cases remain green. Unlike-fluid
  reaction tests, once landed, prove type/reaction semantics survive the scale
  migration.
- `world-edits` v1 fixture bytes migrate to v2 full-level units, publish,
  resave, restart, and reload. Current v2 saves round-trip partial levels.
  Component hashes, manifest entries, persistence inventory, and the selective
  save-compat reproducibility gate follow repository policy.
- Pure renderer tests assert flat top face-map selection, no production
  `waterSlopeAt` use, one-z whole-step sides, each partial level's vertex y,
  full-plus-partial side decomposition, in-chunk/cross-chunk parity, U-seam
  parity, unloaded-neighbour conservatism, z-slice visibility, sort ordering,
  and all four camera facings.
- Asset validation checks 96x64 RGBA shape, exact two-pixel side progression,
  alpha bounds, left/right/top lighting channels, no opaque pixels outside the
  intended silhouette, and the level-8 mask's relationship to the existing
  flat full tile.
- A visual contact sheet and offscreen scene show levels 1..8 beside dry
  terrain, adjacent mixed levels, a whole-z stack, river/lake/lava/ocean
  variants, all four facings, and representative day/night lighting. Every
  asset receives explicit owner signoff.
- DFL-5 runs the worldgen full tier, regenerates tracked baselines, runs
  `world_check`, updates the component migration fixture, and proves river
  surfaces are deterministic, monotone downstream, exact across widened
  cross-sections, bounded across lateral adjacency, and continuous into sinks.
- Diagnostics prove `fluidSurf` remains the integer compatibility view while
  exact units/level expose the authoritative state.

## Delivery plan

### DFL-1. Replace freshwater slope interpolation with whole-z flat steps

- **Outcome:** River and Lake surfaces render as flat integer-height steps;
  every positive visible gap, including one z, has the appropriate vertical
  side instead of a terrain-style ramp.
- **Scope:** Remove `waterSlopeAt` from the production freshwater path; select
  the flat face map; transfer one-z ownership to side-face generation; preserve
  camera-facing, seam, unloaded-neighbour, ice-cover, slice, and sort behavior;
  replace slope-specific tests with flat-step geometry tests.
- **Phase:** 1 — visual proof
- **Depends on:** `none`
- **Ordering:** can land first
- **Relevant decisions:** D-3, D-8, D-9
- **Acceptance signals:** whole-z river/lake tops are flat; 1-z and multi-z
  edges render complete sides in-chunk and across a loaded U seam; enclosed
  equal-height water has no side; terrain slopes and ocean behavior are
  unchanged; focused headless render tests pass; user accepts an offscreen
  before/after scene.
- **Out of scope:** fractional state, new assets, solver changes, river
  generation.
- **Open questions:** None.

### DFL-2. Make eighth-z fluid state exact, conserved, and durable

- **Outcome:** The runtime and world boundary share one eight-unit fixed-point
  surface; every exact quantity survives writeback, deactivation, reactivation,
  edit replay, and save/load without rounding or volume creation.
- **Scope:** Introduce the exact type/helpers; set eight units per z; migrate
  `FluidCell`, active/passive conversion, gravity/seam comparisons, render and
  gameplay integer consumers; bump `world-edits` v1→v2 with frozen DTO and
  migration; update persistence inventory and compatibility fixtures; preserve
  existing Lua arity; forbid an integer-only Ocean path.
- **Phase:** 2 — state foundation
- **Depends on:** external #2042 and #2044
- **Ordering:** critical path
- **Relevant decisions:** D-1, D-2, D-4, D-5, D-11, D-12
- **Acceptance signals:** exhaustive remainder round trips; simulation
  conservation properties; a one-unit cell remains one unit after
  active→passive→save→restart→load→active; old snapshots preserve their visible
  z as level 8; a partial Ocean cell passes the same round trip; warning-clean
  build and relevant save gates pass.
- **Out of scope:** fractional render placement, new masks, generated-river
  interpolation, sparse fluid storage.
- **Open questions:** None.

### DFL-3. Create and approve the eight fluid-level face maps

- **Outcome:** Eight tracked 96x64 masks express level 1 through level 8 in
  exact two-pixel side increments and are approved for integration.
- **Scope:** Generate the assets from one checked definition in their own asset
  PR; add structural validation and a contact-sheet/preview route; document
  names and level mapping. No production binding until DFL-4.
- **Phase:** 2 — assets
- **Depends on:** `none`
- **Ordering:** independent; can proceed alongside DFL-2
- **Relevant decisions:** D-1, D-7, D-10, D-12
- **Acceptance signals:** all eight masks pass shape/channel validation; contact
  sheet covers required terrain, facings, and lighting; the owner explicitly
  approves every mask.
- **Out of scope:** material recolours, waves/foam, production renderer wiring.
- **Open questions:** None.

### DFL-4. Render eighth-level fluid tops and partial step faces

- **Outcome:** Exact fluid quantity is visible at the correct two-pixel level,
  with a flat top and side geometry terminating exactly at neighbouring fluid
  or terrain height.
- **Scope:** Load/register level masks; select by exact level; use float only for
  vertex placement; decompose visible differences into full and partial side
  segments; apply the same geometry path to every fluid type; update culling,
  z-slice checks, sort keys, ice cover, and seam lookups; focused geometry and
  offscreen visual QA.
- **Phase:** 3 — exact presentation
- **Depends on:** DFL-1, DFL-2, DFL-3
- **Ordering:** critical path
- **Relevant decisions:** D-1, D-2, D-3, D-4, D-7, D-10, D-12
- **Acceptance signals:** levels 1..8 appear at exact two-pixel increments;
  mixed neighbouring levels have neither gaps nor overlap; full-z stacks and
  partial caps compose correctly; generated Ocean remains flat at level 8 while
  a partial edited/runtime Ocean uses the matching mask; all facings and loaded
  seams match; the owner approves the integrated offscreen scene.
- **Out of scope:** generated-river surface redistribution, animated
  interpolation, zoom-map fractional depth.
- **Open questions:** None.

### DFL-5. Give generated rivers deterministic eighth-level surface progression

- **Outcome:** Generated rivers use exact eighth-unit downstream planes rather
  than only whole-z surfaces, while keeping level cross-sections, correct sinks,
  deterministic output, and deliberate bed/carve relationships.
- **Scope:** Exact river table/DTO; downstream interpolation and lateral clamp;
  width-wing, breakthrough, bed-depth, carve, chunk-composition, and zoom/detail
  compatibility; enforce D-11's generated terrain invariant; freeze/migrate the
  previous `world-pages` shape; full worldgen output tier and baselines.
- **Phase:** 4 — generated water
- **Depends on:** DFL-2, DFL-4
- **Ordering:** critical path for epic completion
- **Relevant decisions:** D-1, D-2, D-3, D-6, D-11
- **Acceptance signals:** deterministic eighth-step monotone rivers; no uphill
  surface, broken cross-section, sink discontinuity, new dry river tile, or
  unexplained carve movement; old river tables migrate by `z * 8`; full-tier
  tests, regenerated baselines, and world check pass.
- **Out of scope:** changing river footprint/width policy, lake/ocean surface
  algorithms, hydrological flow accumulation.
- **Open questions:** None.

### DFL-6. Add exact-level diagnostics and end-to-end compatibility gates

- **Outcome:** Maintainers and automated gates can observe exact level state and
  distinguish physics, persistence, and rendering regressions without reading
  pixels by hand.
- **Scope:** Add dump/cursor exact units and level while preserving integer
  compatibility fields; update hydrology/persistence/render documentation;
  add an end-to-end edit→flow→writeback→save→restart→render fixture or focused
  probe; record CRS-12 and fluid-reaction integration contracts.
- **Phase:** 5 — closure
- **Depends on:** DFL-2, DFL-4, DFL-5
- **Ordering:** final integration
- **Relevant decisions:** D-1 through D-12
- **Acceptance signals:** diagnostics agree with authoritative exact state;
  one end-to-end partial cell retains level and geometry across a fresh-process
  save/load; documentation names the exact/integer boundaries; relevant CI
  selectors include the new tests without making GPU/offscreen work a headless
  gate.
- **Out of scope:** a new unchecked Lua ABI, sparse-storage implementation,
  cosmetic animation.
- **Open questions:** None.

## Source notes

The owner began from a visual question: whether the existing fluid system
already registered different water levels within one tile, and whether eight
textures or exact float height should replace slopes. Repository inspection
showed transient volume units but integer world/render state. On 2026-08-31 the
owner selected eight visible wet levels, flat steps, fixed-point authority,
float only at rendering, one material texture plus level geometry masks, and a
whole-z visual proof before the persistence/physics migration. The owner then
approved deterministic mask generation, the generated-river terrain invariant,
and uniform fractional geometry for every fluid type, with generated Ocean
flatness defined as policy so a later wave simulator can use the exact state
without another migration.
