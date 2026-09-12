# Structure interaction and isometric occlusion design

Units should inhabit the terrain, ruins, buildings, and other units they see.
This design follows the owner's request to fix acolytes disappearing into ruin
floors and passing through walls, and the September 11 agreement to address
physical interaction and rendering together.

Design state: `ready for issue processing`

Readiness reviewed September 11, 2026 under the owner's instruction to check
the revisions and mark the document ready if they pass. The review verified
the prior five concerns, corrected the remaining activation gap, preserved
D-1 through D-4, and checked the nine-slice dependency ledger and tracker
overlap. Open choices have explicit child-level processing gates below.

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [ ] EPIC. Make world geometry govern unit movement and sprite occlusion
- [ ] SI-1. Define and derive world support surfaces and solid bounds
- [ ] SI-2. Ground units and ground objects on reachable support surfaces
- [ ] SI-6. Prove geometry-based occlusion on the ruin regression scene
- [ ] SI-3. Route and sweep unit motion around static obstacles
- [ ] SI-4. Integrate reachable interaction points and action completion
- [ ] SI-9. Migrate work and combat approaches to spatial reach
- [ ] SI-8. Keep spatial geometry coherent through edits and restoration
- [ ] SI-7. Integrate occlusion with scene assembly, picking, and performance gates
- [ ] SI-5. Add local avoidance and bounded conflict resolution between units

## Epic contract

- **Goal:** An acolyte can approach a ruin, enter through a traversable opening,
  stand on its floor, move around solid objects and other units, and remain
  correctly occluded by nearby surfaces at every supported camera facing.
- **Done when:** Actual movement, support height, action reach, rendered pixels,
  and picking agree in the regression scenes, including different elevations,
  construction/demolition, and save/load. The correction does not introduce
  floor holes, seams, cliff overdraw, tunnelling, or unbounded crowd work.
- **Users and operators:** Players commanding units; content authors defining
  structures/buildings; developers maintaining movement and rendering.
- **Arc label:** None proposed.

## Current state and evidence

Investigation revision: `13bd01bdeb3043e0087b93adc90d67352a253bf0`.

- `scripts/locations.lua:300` builds the shipped `ruin_small` from damaged
  structure art with deterministic missing perimeter segments. The floor
  builder in `scripts/structures.lua:417` stores its piece at terrain z + 1.
- `src/Structure/Render.hs` sorts a floor at `depthAnchor` 1 step (the
  tile-centre depth) plus `relativeZ * 0.001` plus a `0.0002` floor
  tie-break, drawn as one full-tile quad whose canvas includes the slab's
  side thickness. Screen-front walls are already split into per-strip quads
  at the front-edge depth (`frontWallStrips`, 2 steps); back walls and floors
  are not decomposed. Walls take a 4 px `floorLift` so they stand on the slab;
  units take no such lift.
- **Two candidate mechanisms for the swallow symptom, both untested.** The
  floor's anchor is one depth step ahead of its tile at every facing, but a
  unit feeds its continuous position through `applyFacingF`, so a unit at its
  tile centre sits +1, 0, −1, 0 steps ahead of the tile at south, west, north,
  east (`World.Grid`; the `depthAnchor` comment records the same numbers).
  At three facings the floor is therefore one or two whole steps ahead of a
  centred unit. At south the depths tie and a second, smaller term decides:
  the floor is stored at terrain z + 1 while the unit keeps `usGridZ` =
  terrain z, so the floor's z-term wins by 0.001 against the unit's
  `2 * unitSortNudge` = 0.0006. Both rejected experiments changed only the
  depth step. SI-6's first experiment measures both: the depth convention
  per facing, including how a unit orders against its own terrain tile, and
  the z-term.
- `src/Structure/Types.hs` already keys walls per tile edge
  (`SWallNE/NW/SE/SW`) and posts per tile vertex (`SPostN/E/S/W`), so the
  ruin's barrier set is a pure function of the existing overlay and needs no
  storage change. `SCeiling` exists (placed at terrain z + 2) but `ruin_small`
  places none, so roofs are not in the first acceptance scene.
- `src/Unit/Render.hs:284` sorts a unit by its continuous foot row plus small
  elevation/class terms, with a special far-side climb override.
- `src/Unit/Pathing/Cost.hs:283` reads terrain heights. Its step admission
  does not query structure wall edges or building occupancy, but it already
  decides per edge (slope bits, the no-corner-cutting diagonal rule, damaging
  drops under `FallProhibited`), which is the natural insertion point for wall
  barriers. Spawn and teleport height readers also use terrain/surface maps
  rather than structure support geometry. A checked building placement footprint is not a movement
  collider: `src/Building/Placement.hs` and `Building.Reservation` own the
  former, not the latter.
- `src/Engine/Scene/Types/Batch.hs` sorts/merges scalar quad keys. Its tie
  comparison uses rectangle coordinates and texture identity. The comment
  claiming tied quads have identical UVs is not generally true for animated
  atlas sprites; this is an additional source-level concern, not yet a
  reproduced cause of the ruin failure.
- `src/Structure/Types.hs` stores pieces under `(gx, gy, slot)`, without z
  in the key. Multiple floors of the same slot at one tile need a storage
  redesign; they are not already representable by the current overlay.
- `src/Unit/Sim/Types.hs` separates integer `usGridZ` from interpolated
  `usRealZ`, and stores local routes as XY waypoints. These fields belong to a
  positional serialized type. Fractional support height or support-aware
  waypoints cannot be introduced by casually changing those fields.
- `src/Unit/Pathing/AStar.hs` performs a bounded local search and can return
  a partial route to its closest reached position. A returned path does not
  necessarily prove that the requested destination is reachable.
- `src/Unit/Transfer.hs` separates deferred approach from required reach at
  execution. Its footprint-distance checks do not establish an unobstructed
  physical route or interaction through a wall.
- `Building.Reservation` (#2326) already admits a building's footprint
  atomically across threads; a building's solid bounds can be derived from the
  same reserved footprint rather than a second authored shape.
- Unit YAML already carries a physiological `height` (`data/units/*.yaml`).
  `Unit.Physics.metresPerZ` is 1.5 metres and governs fall physics; mantling
  instead uses `heightPerClimbZ = baselineUnitHeight` (1.8 metres) in
  `Unit.Thread.Movement.Climb`. Reuse authored height for body clearance,
  while specifying its conversion separately from gameplay climb reach.
- `Unit.LineOfSight` blocks sight on terrain z only. Structures are invisible
  to perception and ranged targeting; this arc does not change that.
- Picking is four independent screen projections: `World.Render.HitTest`,
  `Unit.HitTest`, `Building.HitTest`, and `World.Flora.HitTest`. Unit hit
  boxes already follow the continuous `uiRealZ`, so any foot lift must reach
  the hit box through the same value the renderer reads.
- `scripts/movement_arena.lua` and `tools/movement_probe.py` (engine
  contracts §Movement arenas) already run deterministic obstacle courses on a
  flat `world.initArena` world with the wander tick neutralised. The ruin
  fixture belongs there, not in a new harness.

Local diagnostic evidence lives under
`/tmp/synarchy-sprite-depth-evidence/README.md`, with a reproducible capture
script, metadata, and `restored-baseline/` PNGs at all four facings. In several
captures the floor covers most of the acolyte. The unit reports z = 0 and the
floor reports z = 1. A player-command sample entered through an intact wall
and remained at terrain height; it later entered `injured_collapse`, so that
sample does not prove a completed full-room route.

Two interventions were rejected and reverted: removing the floor's extra
depth row exposed the unit but let terrain cut holes in the floor; horizontal
floor strips exposed the unit but introduced seams from floor/side-face
interleaving. All 62 existing `World.Render.StructureRotation` examples passed
on the baseline. Those tests do not assert acolyte/floor occlusion. No
production fix was retained or published.

Temporary evidence is diagnostic context, not durable acceptance evidence.
SI-6/SI-7 must regenerate and retain the needed captures and manifests inside
their implementation PRs.

Tracker check on September 11 examined all 78 open issues in `coghex/synarchy`.
No matching interaction/occlusion epic was identified by title. Related work
is lifecycle-art epic #2078 and structure teardown #2491; construction and
teardown integration must recheck their current implementation during delivery.
This is not final issue-level deduplication.

Readiness recheck on September 11 again inspected all 78 open issue titles
and the epic inventory. No matching umbrella was identified. Integration
touchpoints also include #2490 (destructive fluid solidification), #2484
(ground-item repositioning), and #2496 (unit relations/combat). In particular,
the recovery policy here must not override an explicitly destructive world
event's existing entity-removal policy. Recheck these owners when processing
the affected child; this design creates no competing tracker artifacts.

## Desired experience and scope

An acolyte walks through an actual breach, with clearance for its body, rather
than through intact stonework. Its feet meet the supporting floor. Furniture
occupies physical space and actions target reachable positions beside it.
Units can pass or yield without walking through each other or shaking forever
in a doorway. A wall can hide a unit's lower body while its head remains visible
above the wall; units on another elevation can overlap on screen without
colliding physically.

Initial proposed scope is existing terrain, ruin floors/walls/posts, placed
buildings, relevant ground objects, and unit interactions across terrain
elevations. A full rigid-body engine, ragdolls, automatic climbing onto every
object, and automatic roof transparency are not proposed. Multi-storey building
authoring, additional traversal art, and pushing mechanics remain explicit
questions below rather than silently expanding the first release.

### Proposed first-release behavior

These are concrete defaults for discussion, not additional owner decisions.
The first milestone is the shipped ruin and existing building/object catalog,
with existing terrain elevation changes. It does not depend on authoring a
second floor above an already occupied structure slot.

| Encounter | Proposed physical behavior | Required visible behavior |
|---|---|---|
| Ruin floor | Feet rest on its calibrated top; entry follows a legal height transition. | Floor remains complete beneath the unit, with no body swallowed by the slab. |
| Intact wall or post | The unit's body stops before contact; planning seeks a usable opening. | Near stonework hides only the appropriate part of the unit. |
| Breach or doorway | Admit passage only when the full body fits, including corners and overhead clearance. | Crossing the opening changes occlusion continuously without flickering between layers. |
| Placed building or large object | An authored solid footprint blocks movement; actions use reachable approach positions. | Units pass behind and in front according to their actual positions. |
| Small loose item | Rest on a valid support surface without blocking walking by default. | Item height and picking agree with its supporting surface. |
| Another unit | Avoid, wait, yield, or report a blocked route under a bounded policy. | Stable ordering follows physical positions; animation frames do not decide ties. |
| Higher or lower terrain | Use eligible step, climb, or fall transitions; do not snap to a surface merely because it overlaps in XY. | Feet, body, shadows where applicable, and selection use the same elevation convention. |

A wall must stop horizontal motion; it must not lift a unit onto its top.
Support resolution supplies height only for an eligible supporting surface.
Likewise, collision with another unit does not automatically make that unit a
platform. Radius and height are gameplay dimensions, not just an anchor point.

## Design proposals

### One world-space description, distinct query roles

Derive a spatial view from each page's committed terrain, structures, buildings,
and units. Proposed owner: a small `World.Spatial` family with pure geometry
queries and explicit adapters from existing owners. Names are provisional.

Represent walkable support surfaces, solid volumes, and visual occluders
separately, sharing world coordinates, elevation conventions, and entity
identity. A collider need not equal a sprite's painted silhouette. Cloth,
shadows, and foliage can extend beyond the solid body. Camera rotation and
zoom must never change collision or support results.

Walls become edge barriers with thickness and a vertical interval; floors are
supporting slabs; buildings supply authored solid bounds and interaction
locations; units have a ground footprint (initial candidate: circle) and a
height interval. Small loose items are proposed nonblocking. No dimensions
should be inferred from transparent canvas size or an animation's current pose.
Existing `base_width` is an art anchor input and is not automatically a
calibrated gameplay collision radius.

Use chunk/spatial buckets to restrict queries to nearby geometry. Derive an
immutable view for a simulation batch and render publication, preserving page
ownership and committed edit boundaries. Start with measured necessary data;
classify any retained caches/support references in the persistence inventory
and use existing capabilities, following capability inventory §6.4.

The proposed query boundary is small and shared:

| Query responsibility | Inputs and result | Consumers |
|---|---|---|
| Resolve support | Page, position, previous support/height, body clearance, traversal policy → eligible support and foot height, or a typed failure. | Spawn, movement, falling, explicit repositioning, item placement, restoration. |
| Admit a connection | Source/destination support, body dimensions, terrain/hazard policy → legal transition and cost, or reason blocked. | Route planning and validation. |
| Sweep motion | Supported body and displacement → earliest solid contact and safe travel fraction. | Every continuous movement segment. |
| Find an interaction approach | Actor body, target identity, action-specific reach policy → usable approach candidates and completion predicate. | Player orders, AI work, transfers, applicable combat. |
| Describe occlusion | World primitives and stable identities → projected candidate overlaps and backend-specific ordering/depth inputs. | World scene assembly and picking. |

**Default: no new serialized state.** Support, barriers, and occluders are
derived views over committed content, rebuilt on load and on geometry
revision change. `usGridZ` keeps its integer meaning as the support level and
`usRealZ` keeps its continuous-position role; the fractional floor-top lift
is part of the derived support result read by physical clearance, movement,
rendering, and hit-testing, not merely an extra drawing offset. A slice may
add stored state only after a test shows the derived
form cannot express the behaviour, and then with the migrations listed below.

Preserve the existing coordinate convention where applicable: resting units
currently use terrain surface z, and the floor builder stores its slab one
cell above the chosen base. Other slots have their own offsets, and authored
base heights need not equal the terrain currently beneath them. The wall's
`floorLift` is 0.25 z in render units; that is a calibration candidate for
floor-top contact, not proof of a universal physical support height. SI-1
derives and tests each relevant slot's conversion against the pixels.

Names and concrete types are settled in SI-1. Query failures must distinguish
blocked geometry, absent support, and unavailable page/chunk data; unknown
geometry is not permission to pass through it. Camera culling or a visual
slice must not remove simulation colliders. Normalize coordinate aliases
through the existing tile-coordinate contract before indexing; local queries
near seams must agree with their canonical equivalents. Do not infer that
this alone supplies a new world-spanning route planner.

Stable identity must include page/session ownership and distinguish replacement
pieces at the same tile. Derived views carry a geometry revision. A planner
may read a snapshot, but movement and action commits revalidate affected
geometry when that revision changes. Scene publication must associate visible
pieces with geometry from the same committed content state. Publish edits
atomically at the owning boundary rather than independently updating colliders
and sprites and hoping their timing matches.

### Supporting height is a physical value

`supportAt` must find an eligible surface under the unit, accounting for its
current elevation, body clearance, and reachable step/climb/fall transitions.
It must not select the highest surface at `(x,y)` and teleport a unit onto a
roof. The resolved support and physical foot height feed simulation, rendering,
and selection through one projection contract.

The current floor's stored z is an artwork placement convention, not yet a
validated physical top height. Floor pixels include padding and thickness;
simply assigning `unit.z = piece.z` can move feet away from the visible top.
SI-1 must establish the conversion for existing content, and SI-2 must prove
contact against the pixels. This is a geometric contract, not an arbitrary
sprite-Y adjustment. Ground items need the same resting-surface semantics.

Explicitly define the relationship among terrain grid level, a support's top
height, continuous foot height, and the existing climb/fall state. Audit range
checks and other `usGridZ` consumers before assigning fractional physical
meaning to `usRealZ`. Rendering interpolation must not silently become the
only source of physical truth. Under the no-new-state default above, no
stored field or waypoint shape changes in the first release; if a slice proves
one necessary, its PR includes state classification, component wire
migrations, and old-save tests. A derived support cache is rebuilt on load;
it is not accidentally serialized as durable world state.

### Planning and actual motion agree

Pathfinding asks whether the unit's body can traverse each connection; walls
block edges rather than making their entire adjacent tiles impassable. Gates
include radius, clearance, support transition, and existing hazard policy.
Path state must identify a support/elevation where `(x,y)` alone is ambiguous.

The continuous mover sweeps the footprint along the complete proposed motion,
including every residual waypoint segment, to prevent passing through a thin
wall between ticks. It advances to a safe contact, slides or replans according
to policy, and preserves existing timing and position-hold contracts. It does
not implement collision only in A*, only in Lua orders, or only at the final
position of a tick.

Distinguish a route that reaches a valid goal, a partial route, a blocked goal,
and a search stopped by its budget. Following a partial route must not report
arrival or create a position hold at the wrong position. Replanning needs
bounded work and progress tracking, including the case where a reachable
breach lies away from the straight-line direction to the target. Preserve the
existing eligible-time stall budget: waiting and interruption cannot create
an immortal order, and a long journey that progresses must not expire solely
because it took a long time. Collision response must preserve the movement
residual-time contract across every consumed waypoint segment.

**Candidate overlap rule: motion may not increase penetration.** The sweep
rejects a displacement only where it deepens overlap with a solid, so a body
already inside one solid (an old save, a placement race, a piece that just
became solid) can move out of that solid and is never teleported. This is a
candidate escape mechanism, not a recovery guarantee: it does not make the
planner admit steps between two tiles inside a multi-tile collider, it does
not resolve a body inside several overlapping solids, and it says nothing
about a unit standing legally inside a room that has just been sealed. The
proposed companion is a transient per-solid exemption computed at restoration
or a committed geometry change that creates an overlap. It is not granted
after an ordinary movement step penetrates a collider. That unit ignores
exactly the recorded solid identities, and the planner admits edges within
them, until the body has fully left each one; exiting revokes that exemption
before another movement segment can re-enter. Replaced solids have new
identities, and save/load rebuilds exemptions under Q-5. Sealed rooms are an
enclosure question under Q-4, and the fallback when neither mechanism frees a
unit is settled under Q-5. Placement rejection remains a real safeguard, not
a courtesy.

First-release body: a circle footprint and the YAML-derived height interval,
with the required per-definition radius settled by D-4 and its numeric values
calibrated under Q-4. Passage through a breach is a measured clearance check: a
missing wall segment is nominally one tile wide, but the posts at its
vertices, the wall thickness, and any clearance margin narrow it, so no
radius bound guarantees passage and none is claimed. Overhead clearance
applies only under a ceiling and can stay a typed failure until roofs enter
scope (Q-4).

Dynamic unit avoidance is a later layer over this static legality check. It
needs deterministic priority/yield rules and bounded progress handling at
doors. Simple pairwise repulsion alone does not establish freedom from jams.
Vertical separation must exclude units on distinct floors from ground-plane
avoidance when their bodies do not overlap.

For proposed avoid/yield behavior, a stopped or position-held unit remains
occupied space. A yielding mover cannot silently clear another unit's player
hold. Symmetric doorway conflicts use stable priority plus bounded retry/yield
rules; when geometry provides no passing space, stopping with a blocked result
is an acceptable specified outcome. A guarantee that every crowd eventually
passes every doorway is not proposed. Bodies cannot be shrunk merely to make
an animation or congested passage fit; any posture-dependent clearance needs
an explicit transition check, including room to stand up again.

### Interaction and lifecycle must participate

Action destinations become reachable positions around an object rather than
points inside its collider. Completion checks for pickup, construction,
crafting, repair, transfers, and applicable combat actions must agree with
the reach policy; a short distance through a solid wall is insufficient.
Preserve the strict player-transfer versus lax AI-transfer policy distinction.

Keep two questions separate: can a legal approach be planned, and is the actor
currently allowed to complete the action? A remote player transfer can still
create an approach order; its final execution must recheck the target and
geometry. A building's nominal adjacent tile is insufficient if a wall blocks
the reach. Each action family owns its reach shape and allowed obstruction
rules; ranged combat must not inherit a melee adjacency rule. SI-4 introduces
the shared contract and migrates pickup/transfers; SI-9 migrates work and
combat approaches. The caller inventory records both the movement destination
and authoritative completion check. Terrain-only perception and ranged sight
remain outside this arc; their existing limitation must be explicit rather
than being presented as fixed by melee reach or physical wall collision.

Committed placement, construction transitions, demolition, support removal,
chunk load/eviction, page replacement, and save/load invalidate affected
geometry coherently. Preview ghosts do not become invisible physical walls.
The exact construction stage that begins blocking is Q-4. Prevent new geometry
from silently enclosing units, and settle old saves with units already inside
newly-solid geometry under an explicit compatibility policy (Q-5).

Proposed lifecycle defaults:

- Preview and unpaid ghosts are nonblocking. Completed pieces become solid
  at their committed placement boundary. An intermediate construction stage
  blocks only if its content contract explicitly defines visible geometry;
  the presence of a construction-frame texture alone does not define physics.
- Reject a new placement that intersects a unit or invalidates its immediate
  support/clearance. This local legality check does not promise that every
  placement preserves global access to every room.
- Removing a support invokes the existing fall/transition machinery when a
  valid lower surface exists; it does not leave a unit floating. Settle the
  missing-lower-support outcome under Q-4 before enabling removal behavior.
- Replacing a page or loading a session discards old spatial views, routes,
  and support references as required by their classifications. Old overlaps
  follow the ordered Q-5 policy, never an unbounded search or an arbitrary
  render-only lift.

SI-8 owns these lifecycle adapters. SI-2 still owns any migration required by
the state it introduces; the later lifecycle slice cannot defer that duty.

### Geometry informs rendering; collision does not replace occlusion

A legal unit behind a wall still overlaps it on screen. Supply semantic
world bounds/surfaces and stable identities to a scene-level occlusion pass,
instead of trying to repair every producer's scalar offsets independently.
The current static-run/dynamic-run merge assumes a total scalar order and
must be reconsidered at `Engine.Scene.Assembly` when dependencies cross runs.

SI-6 evaluates candidates in cost order and escalates only when a reference
capture fails:

1. **Convention and decomposition inside the existing scalar painter.** Key
   the floor's top face and a unit on it in a consistent projected coordinate
   frame at each facing, including the different tile-origin/center offsets
   recorded above. Actual front/behind relationships still rotate with the
   camera; a unit's depth is not held constant across facings. Make the
   floor's z-term agree with the unit convention,
   and split a floor into a top face sorted with its tile (just above
   terrain) and a side skirt sorted at the front-edge depth the way
   `frontWallStrips` already sorts walls. This is a surface split, not the rejected screen-band split, and it
   reuses `depthAnchor` and `justAbove`. Expected to fix the ruin scene; it
   cannot fix genuine cycles.
2. **Constraint ordering.** Use projected overlap as a broad phase, derive
   before/after constraints from world-space separation, and topologically
   order resolvable primitives.
3. **Geometric depth on the GPU** (below).

For every candidate, use stable entity identities to resolve genuine visual
ties; using the existing scalar painter does not waive this requirement.
Bounds alone cannot guarantee correct pixels for an arbitrary sprite or a
cycle. SI-6 must explicitly detect ambiguous/cyclic overlaps and prove a
bounded treatment such as correct surface subdivision/clipping or geometric
depth testing. Randomly breaking a cycle is not a correctness solution.
The previous horizontal floor-strip experiment demonstrates why top surfaces
and side faces cannot be treated as one plane.

A GPU depth implementation is an alternative to assess in SI-6, not an approved
backend choice. It requires actual per-fragment geometric depth and an alpha
policy; assigning one depth to a whole billboard is insufficient. No new art
is established as required for the initial analytic-geometry approach. If a
selected solution needs authored sprite depth maps, list those missing assets
and obtain the owner's supply/generation decision before that delivery work.

Primary technical reference for the dependency approach and its cycle limit:
[Drawing isometric boxes in the correct order](https://shaunlebron.github.io/IsometricBlocks/).
Its non-intersecting box assumptions are narrower than Synarchy's sprite world.

### Activation and delivery boundaries

The rendering proof runs early, after support calibration, so the feature does
not assume that adding collision will cure occlusion. There is no code-level
staging switch to remove later. Where a slice would otherwise break a
consumer that has not migrated, the gate is authored content metadata with a
conservative default, not a hook, and a default flips only in the PR that
delivers the safeguards that activation needs.

Collision never activates ahead of its lifecycle safeguards. SI-2/SI-3 use
explicit physical content declarations in the regression fixtures to exercise
the real support, planner, and mover consumers. Shipped content retains its
existing support and nonblocking behavior until SI-7 enables the new physical
declarations together. SI-4 and SI-9 migrate action callers without enabling
hard building or wall collision ahead of SI-8. This content-based rollout
does not add a second simulation implementation or a player-facing switch.

SI-8 supplies committed-edit invalidation, placement legality, support removal,
page replacement, and restoration under Q-5 before production activation.
Old saves can already contain overlapping units because today's movement
does not enforce these barriers; their legality cannot be inferred from spawn
locations in shipped definitions. SI-7 tests both old saves and saves created
with the new declarations active. Every introducing PR still owns any schema
migration it requires, including the no-new-state exceptions above.

Normal gameplay activation therefore requires support and static movement
(SI-2/SI-3), action approach/completion (SI-4/SI-9), lifecycle and overlap
handling (SI-8), and production rendering/picking (SI-7). Crowd policy (SI-5)
is not on that barrier: units overlapping each other is today's behaviour and
does not get worse when walls become solid; SI-7 completes the static-world
matrix and SI-5 adds the crowd scenarios afterwards. Each PR includes its own
relevant tests and required documentation; final integration evidence
supplements those gates.

## Decisions

### D-1. Address physical interaction and rendering together

The owner accepted a feature covering real interactions between units,
structures, and objects, including solid walls and supporting floors. The goal
is consistent world behavior and visible ordering, not just changing sort
constants. Algorithm details below remain proposals.

### D-2. Use the acolyte at a ruin as the first acceptance scenario

The owner identified this as a definite failure. Preserve this concrete case
through implementation rather than relying only on synthetic geometry tests.

### D-3. A load never fails on unit geometry

The owner chose the never-reject outcome for Q-5. A restored unit that neither
the per-solid exemption nor the bounded nudge frees stays embedded, keeps its
exemption, and is reported in the load log. No new pre-publication failure
path is introduced for unit positions; every save that opens today keeps
opening.

### D-4. Body radius is a required per-definition content value

The owner chose a required `body: { radius }` field in unit YAML with no
engine default. All five shipped definitions are authored deliberately in
SI-1, prototyped against the ruin's measured breach clearance, and the loader
rejects a definition without one.

## Open questions

### Q-1. Should structures affect movement? Resolved by D-1

Yes: physical interaction and rendering belong in the same feature arc.

### Q-2. How should units resolve congestion?

Proposed first policy: avoid and yield with bounded replanning; no pushing or
permission to overlap indefinitely. Whether friendly units may deliberately
squeeze past each other, and how enemies occupy space, still needs a decision
before SI-5 is specified.

### Q-3. Are stacked building floors part of the first release?

Propose terrain elevations plus existing single-floor structures first, while
making support identity explicit. Multiple structure floors at one tile require
new storage keys, edit replay, migrations, and navigation connections. Choose
scope before SI-1/SI-2 specs; do not assume the current overlay supports it.

### Q-4. Which content blocks, supports, or permits traversal?

Specify physical dimensions, step limits, roof/ceiling support, building
interaction sides, large-item classification, and when construction becomes
solid. Existing art does not answer all of these. Proposed default for
ceilings: an `SCeiling` piece is an occluder and an overhead-clearance bound
in the first release, never a support; standing on a roof waits for Q-3.
Prototype against the shipped ruin and acolyte; record numeric values only
after validating contact. The radius source is settled by D-4; the per-definition
numbers are still validated here.

### Q-5. How are pre-existing overlaps repaired? Load stance resolved by D-3

Old saves can legally contain units inside future colliders. Choose and test
a bounded recovery/load policy, including the no-nearby-valid-position case.
New placements should reject trapping overlaps by default. Any persisted
schema change requires component migrations, not only a global version bump.

Proposed policy, in order: a restored unit inside a solid keeps its saved
position and receives the transient per-solid exemption, so it may leave the
solids it starts inside and no others; a unit that cannot leave under that
rule (several overlapping solids, or no free surface adjacent to any of them)
takes a bounded same-page nudge to the nearest legal resting position that
does not cross an intact barrier; a unit with no such position remains
embedded and exempt, is reported in the load log, and never blocks the load.
Rejecting the load is ruled out by D-3. None of the three steps is a
guarantee that every unit recovers; the fixtures in SI-8 cover each outcome
explicitly. Still open here: the nudge's search bound and what the load log
entry carries.

### Q-6. Which occlusion backend passes the reference scenes?

Resolve in SI-6 with real pixel and timing evidence before SI-7. The shared
geometry model is the proposal; a complete sorting or depth-buffer backend is
not yet proven or approved by this draft.

### Q-7. What performance limits define acceptance?

Measure the proposed fixture sizes on supported hardware and agree simulation,
occlusion, frame-time, rebuild-spike, and memory limits before SI-7 is drafted.
SI-6 records its candidate measurements; SI-5 adds a crowd-work budget. These
are deliberate evidence gates, not permission to accept an unmeasured backend.

### Deliberately open choices and processing gates

The design can be ready for issue processing while these bounded questions
remain open. Processing the umbrella records them; processing an affected
child stops before presenting its issue for approval until its question is
resolved. Proposed defaults elsewhere in this document are not owner decisions.

| Question | Affected slice and stop/ask behavior |
|---|---|
| Q-2: crowd policy | Before SI-5, ask the owner to choose congestion behavior and record friendly/enemy and position-hold rules. |
| Q-3: stacked floors | Before SI-1, confirm existing single-floor structures first. If stacked floors are required, reset the design to exploring and revise storage, traversal, and migration slices before continuing. |
| Q-4: physical content | Before SI-1, agree supported content and candidate dimensions/conversions; calibration is that slice's measured outcome. Before SI-2/SI-3, define step/ceiling/clearance rules. Before SI-9/SI-8, settle action reach, construction stages, enclosure, and unsupported-unit outcomes. An unresolved behavior blocks that child. |
| Q-5: recovery details | Before SI-8, specify the bounded nudge search, allowed escape from initial overlaps, exemption lifecycle, and log fields, preserving D-3's never-reject stance. |
| Q-6: rendering backend | SI-6 is the bounded experiment that resolves this. Stop before SI-7 until its recorded backend verdict proves the reference matrix; new required artwork triggers the asset supply/signoff workflow. |
| Q-7: performance | Stop before SI-7/SI-5 until their numerical budgets and measurement protocol are agreed from baseline evidence. |

Ready means the arc and gated delivery plan are reviewable, not that all
implementation choices have already passed experiments. A changed scope or
failed backend proof that requires a different delivery architecture resets
the design to exploring. No child bypasses these gates because the umbrella
has already been filed.

## Verification strategy

### Reference scene and acceptance sequence

Retain a deterministic fixture based on the shipped `ruin_small`, built as a
`scripts/movement_arena.lua` course so `tools/movement_probe.py` drives it:
flat terrain, known floor placement, an intact wall, a reproducible breach,
and one acolyte under a player move order. The existing probe is headless and
proves movement only; an offscreen capture runner loads the same course for
pixel evidence. Extending the course does not by itself add GPU captures to
the headless probe. Its first visual capture is the minimal
reproduction: the acolyte teleported to the centre and to the back half of a
floor tile at all four facings, recording the floor, unit, and terrain sort
keys beside the pixels so the depth-convention and z-term mechanisms above
are measured separately. Record definition IDs, world coordinates, random
seed where used, camera facing/zoom, engine revision, and actual support/foot
heights. Explicitly wait for definitions to finish loading before constructing
the fixture. Use normal movement for the successful sequence; teleport-only
captures supplement it with precise overlap positions.

1. Approach an intact wall from outside. The body never crosses the solid;
   the planner uses the breach when a route exists within supported planning
   capabilities, or returns a specified partial/blocked outcome.
2. Enter through the breach, traverse the floor, turn around each post, and
   exit. Check foot contact and trajectory independently of camera rotation.
3. Repeat with a building and a loose item. The acolyte reaches a usable side
   and performs the action, while a wall-separated target cannot complete an
   action solely because its distance is small.
4. Add a second unit for crossing, following, and opposing doorway traffic;
   repeat with one unit holding position and with bodies vertically separated.
5. Add existing terrain height transitions beside the ruin. Verify stepping,
   climbing/falling, and occlusion at fractional positions on both sides.
6. Edit relevant geometry and perform a fresh-process save/load. Verify route
   invalidation, support changes, and each chosen overlap-recovery outcome.

Capture the sequence at all four facings and include empty-scene controls.
Temporary diagnostic files must be converted into a durable, reproducible
fixture and manifest, not treated as a permanent test dependency.

### Behavioral, visual, and performance gates

Use pure tests for support selection, swept crossing, body clearance, diagonal
corners, page/seam ownership, and spatial-index equivalence to a small brute
force oracle. Exercise the real planner and mover over different tick sizes,
fast movement, multiple waypoint continuations, narrow gaps, and changes during
travel. Existing movement hazard, residual-time, hold, and transfer contracts
remain applicable.

Select targeted Hspec groups and subsystem probes from
`docs/engine_contracts.md`, particularly Position hold, Commanded-order stall
budget, movement residual time, Player transfers, Construction, and Save/load
transaction. Update the responsible contracts and any affected persistence or
capability inventories in the same implementation PR. Existing 62 structure
rotation examples remain useful regression coverage but are not the visual
acceptance test for this feature.

Capture actual walking acolytes at every camera facing, near walls/posts,
buildings, ground objects, other units, and high/low terrain. Include empty
scene controls, fractional positions, multiple zoom levels, front/behind cases,
climbing/falling, and silhouettes extending beyond collision bodies. Test
picking against the final rendered order through all four hit-test
projections (`World.Render.HitTest`, `Unit.HitTest`, `Building.HitTest`,
`World.Flora.HitTest`), which do not share code. A headless pass is not
pixel proof.

Retain reproducible offscreen evidence, fresh-process save/load behavior,
construction/demolition checks, and scene/crowd timings in each responsible code
PR. Measure static rebuild cost, dynamic query count, frame time, and memory in
dense scenes; define acceptable budgets before the affected child is ready
under Q-7. Do not introduce an
all-pairs scene or crowd scan. Required docs, evidence, and owner verdicts are
completion criteria in the implementation PR, not a later docs-only landing.

Use identical seeded scenes and camera paths for baseline/candidate timing.
Include the one-acolyte ruin, a dense static scene, and proposed 64/256/512-unit
crowd scales; finalize those scales against supported gameplay before calling
them budgets. Separate simulation cost, occlusion CPU cost, GPU/frame cost,
static rebuild spikes, and peak memory. Record hardware, profile, scene size,
and the chosen worst-case/percentile limits. A faster average cannot excuse
unbounded doorway work or a repeated full-scene rebuild on every unit step.

## Delivery plan

All slices are proposed one-PR outcomes. Issue processing resolves the explicit
gates above; the ledger follows dependency-valid order and preserves stable IDs.
No tracker items exist. SI-8 separates lifecycle work from the original broad
SI-4 proposal; SI-6 moves earlier to resolve the rendering risk; SI-5 moves
off the activation barrier and its crowd scenarios out of SI-7's matrix.
SI-9 separates the work/combat caller migration from SI-4's shared query and
transfer boundary. Before SI-7, real-consumer acceptance uses the explicitly
physical fixture content; normal content activation occurs only in SI-7.

### SI-1. Define and derive world support surfaces and solid bounds

- **Outcome:** A tested spatial contract and adapters for existing content.
- **Scope:** Coordinate/elevation conventions, shapes, support identity,
  chunk-local queries, ownership/classification, authored metadata defaults.
- **Phase:** Foundation. **Depends on:** none. **Ordering:** critical path.
- **Relevant decisions:** D-1, D-2, D-4.
- **Acceptance signals:** The elevation convention written down and tested
  against terrain, floor, ceiling, and unit z; explicit floor-top alignment
  and independent geometry tests for shipped ruin pieces, acolyte footprint,
  and building bounds derived from the reserved footprint; all five unit
  YAMLs carry an authored `body.radius` and a definition without one fails
  to load.
- **Out of scope:** Switching live movement or rendering to new rules.
- **Open questions:** Q-3, Q-4; settle foundational geometry first.

### SI-2. Ground units and ground objects on reachable support surfaces

- **Outcome:** Correct support/foot height through spawn, motion, teleport,
  falling, and restoration, with matching render and hit-test placement.
- **Scope:** Support transitions, shared resting-height queries exposed to
  Lua beside `structure.floorZAt`, and state classification of the derived
  cache. No serialized change by default; any proven exception carries its
  migrations/reconciliation in this PR.
- **Phase:** Simulation. **Depends on:** SI-1. **Ordering:** critical path.
- **Relevant decisions:** D-1, D-2.
- **Acceptance signals:** Floor contact and no snapping to unreachable roofs;
  save/load and removal-of-support evidence.
- **Out of scope:** Crowd avoidance and general occlusion replacement.
- **Open questions:** Q-3, Q-4, Q-5; required policies precede delivery.

### SI-6. Prove geometry-based occlusion on the ruin regression scene

- **Outcome:** A bounded occlusion implementation/reference experiment resolves
  the chosen scenes and establishes the production backend contract.
- **Scope:** The candidate ladder in cost order (z-term convention and
  floor top/skirt decomposition first), cross-class overlap, support/elevation
  relations, stable identity, cycle/ambiguity handling, and surface
  subdivision or geometric depth evidence only if the cheaper candidate fails
  a retained capture.
- **Phase:** Rendering proof. **Depends on:** SI-2. **Ordering:** early risk gate.
- **Relevant decisions:** D-1, D-2.
- **Acceptance signals:** Acolyte/ruin captures at four facings, empty-scene
  controls, cliffs and object overlap; no floor holes/seams or hidden-body bug.
- **Out of scope:** Claiming production readiness from box bounds alone.
- **Open questions:** Q-6; publish the backend decision and measured limits.

### SI-3. Route and sweep unit motion around static obstacles

- **Outcome:** Planner and continuous mover agree on body-safe traversability.
- **Scope:** Wall edges and posts from the existing slots, building volumes
  enabled in physical fixtures, measured breach clearance, swept motion with the
  candidate overlap rule and per-solid exemption, hazard/step handling,
  support-aware route state, and revalidation when the geometry revision
  changes. SI-8 owns production lifecycle publication and restoration adapters.
- **Phase:** Simulation. **Depends on:** SI-2. **Ordering:** after the early
  rendering experiment in this proposed delivery sequence; no technical
  dependency on SI-6's backend implementation.
- **Relevant decisions:** D-1, D-2.
- **Acceptance signals:** Real acolyte enters through a breach, never tunnels
  through intact walls, and reaches valid destinations across tick sizes;
  revision changes reject stale route segments. Controlled initial-overlap
  fixtures prove exemptions admit exit but not re-entry or unrelated solids.
- **Out of scope:** Crowd arbitration and gameplay action migration.
- **Open questions:** Q-4; body dimensions and traversal rules must be settled.

### SI-4. Integrate reachable interaction points and action completion

- **Outcome:** Existing actions approach usable positions and complete only
  when their physical reach policy permits them.
- **Scope:** Shared approach queries, action-specific completion predicates,
  and migration of pickup/transfers at the Lua and authoritative Haskell
  boundaries. Record the work/combat caller inventory for SI-9. Preserve
  deferred player approach orders and the strict player versus lax AI transfer
  distinction.
- **Phase:** Gameplay integration. **Depends on:** SI-3. **Ordering:** critical path.
- **Relevant decisions:** D-1, D-2.
- **Acceptance signals:** Loot a ruin and transfer to a building without
  targeting solid interiors or completing through an intact wall;
  target changes during approach are revalidated at execution.
- **Out of scope:** Work/combat migration (SI-9), geometry publication/recovery
  (SI-8), and new action content.
- **Open questions:** Q-4; settle pickup/transfer reach semantics before this
  child is drafted.

### SI-9. Migrate work and combat approaches to spatial reach

- **Outcome:** Existing jobs and applicable combat approaches choose reachable
  positions and revalidate their action-specific completion conditions.
- **Scope:** Adapt construction, crafting, repair, and melee approach/execution
  to SI-4's shared query. Audit remaining work destinations and record whether
  each uses a safe shared helper or needs an explicit adapter. Preserve ranged
  sight/perception behavior and existing order/hold policies.
- **Phase:** Gameplay consumers. **Depends on:** SI-4. **Ordering:** before activation.
- **Relevant decisions:** D-1, D-2.
- **Acceptance signals:** A construction site whose nearest neighbor is
  blocked uses a reachable side; station work and repair complete from legal
  positions; wall-separated melee cannot complete by distance alone. Changes
  to a target or barrier during approach invalidate completion appropriately.
- **Out of scope:** New action content, ranged occlusion/perception redesign,
  crowd avoidance, and lifecycle publication.
- **Open questions:** Q-4; action reach rules and the caller inventory must be
  explicit in the child. If the audit discovers an independent subsystem
  redesign, revise this design's slices before filing that added scope.

### SI-8. Keep spatial geometry coherent through edits and restoration

- **Outcome:** Committed edits and session restoration preserve spatial
  legality and invalidate stale geometry, routes, and support references.
- **Scope:** Placement/construction/teardown publication, support removal,
  page/chunk replacement, the ordered Q-5 overlap policy on restoration, and
  reconciliation under the existing save/load transaction contract.
- **Phase:** Lifecycle integration. **Depends on:** SI-3, SI-9. **Ordering:** critical path.
- **Relevant decisions:** D-1, D-2, D-3.
- **Acceptance signals:** Place/remove pieces around a moving or standing unit,
  remove its support, and restore old overlapping saves with one fixture per
  Q-5 outcome: exempt-and-leave, nudged, and embedded-and-reported. No
  invisible blockers or stale-session refs.
- **Out of scope:** Deferring migrations required by earlier slices; new
  construction art or stacked-floor authoring.
- **Open questions:** Q-4, Q-5; choose construction stages, unsupported-unit
  behavior, and the nudge bound before this slice is ready.

### SI-7. Integrate occlusion with scene assembly, picking, and performance gates

- **Outcome:** Production movement and rendering satisfy the static-world
  arc: one unit against terrain, structures, buildings, and items. Crowd
  behaviour follows in SI-5.
- **Scope:** Static/dynamic scene integration, all relevant producers and
  pickers, invalidation, alpha behavior, dense-scene budgets, final evidence,
  and coherent normal-gameplay activation with every schema default flipped
  to its production value.
- **Phase:** Integration. **Depends on:** SI-9, SI-8, SI-6. **Ordering:** critical path.
- **Relevant decisions:** D-1, D-2.
- **Acceptance signals:** Acceptance steps 1–3, 5, and 6 of the reference
  sequence, save/load, owner visual verdict, and recorded performance limits
  pass in the same implementation PR; step 4 belongs to SI-5.
- **Out of scope:** Deferred stacked-floor authoring or unapproved art changes.
- **Open questions:** Q-6 must already be resolved; agree Q-7 performance budgets
  and refine producer migration size before marking this slice ready.

### SI-5. Add local avoidance and bounded conflict resolution between units

- **Outcome:** Units avoid occupied space and resolve narrow-passage conflicts.
- **Scope:** Nearby-unit queries, vertical separation, priority/yield policy,
  progress limits, deterministic symmetric cases.
- **Phase:** Crowds. **Depends on:** SI-7. **Ordering:** not on the static-world critical
  path; unit overlap is today's behaviour and activation does not wait for it.
- **Relevant decisions:** D-1.
- **Acceptance signals:** Acceptance step 4 of the reference sequence:
  crossing paths, following traffic, and opposing doorway traffic have
  bounded, specified outcomes without persistent overlap, captured with the
  same fixture and facings as SI-7.
- **Out of scope:** Rigid-body pushing, ragdolls, or new animations.
- **Open questions:** Q-2, Q-4, Q-7.
