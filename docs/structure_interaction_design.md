# Structure interaction and isometric occlusion design

Units should inhabit the terrain, ruins, buildings, and other units they see.
The original acolyte/ruin occlusion and wall-traversal failure is the first
regression case for a broader multi-level physical world: continuous elevation,
player-built and authored floors/stairs/roofs, structural falls, workstation
wrecks, and deconstruction must agree with rendering, picking, and persistence.

Design state: `ready for issue processing`

**Latest clarification — D-43:** furniture and workstations become destroyed
on landing and leave solid, deconstructable destruction-animation endpoint
wrecks. Structural pieces retain their existing appearance and remain intact
and usable after landing for now. This narrows D-42 and reaffirms D-19;
structural rubble remains future work. D-45 approves the audited shared motion,
shared wreck and separate lifecycle boundaries, adding SI-43 while retaining
all prior IDs. The current plan is 35 code slices plus eight art slices: 43 total.
D-46 confirms Cargo Holds, Solar Panels, Batteries and Portals: suspend services
throughout falling, become nonfunctional wrecks on landing and scatter stored
items in full. Art delivery and bounded implementation details remain gated.

D-44 settles the airborne workstation policy: pause jobs and services while
falling, then permanently discard jobs and functionality on destruction at landing.

The owner approved final design readiness on September 17 (D-47), explicitly
retaining the named child-approval gates. The final audit and 43-slice plan are
accepted for later processing by a different agent brand. No individual issue,
artwork, implementation, tracker mutation or publication is approved by this status.

The September 11 single-floor readiness assessment is superseded by the
September 16–17 decisions. D-1 through D-32 preserve the gameplay choices;
D-33 records approval of the audited delivery boundaries; D-36 completes the
stair subdivision, originally 34 code slices and eight art slices (42 entries).
D-45 adds one non-workstation lifecycle slice, bringing the current total to 43.
D-34 selects whole-tile stair openings. D-35 requires a separate damaged stair
set for authored ruins, generated with PixelLab and personally approved by the
owner. Q-10 records the approved inventory and bounded preview-staging gate.
The behavior/readiness audit rechecked tracker overlap. D-37 resolves its
workstation endpoint choice: the same destruction clips must end in visible
wreckage. D-38 allows construction to seal a room without intersecting its
occupants. D-39 temporarily pauses falling objects at contact with units below,
until those units clear; SI-20 replaces this response with impact/crushing.
D-40 fixes structural-group motion to straight down without tipping/rotation,
stopping the whole group at its first valid load-bearing contact. D-41 sets
deconstruction recovery to 100% of original construction materials for intact
targets and 50% for destroyed targets, as tunable initial balance values.
External art-contract coordination/delivery and the named child-level behavior
gates remain outstanding. D-47 supplies final design-readiness signoff, not
permission for this session to process or publish the document.

This foreground session is authorized to finish this design document only.
The owner requires a different agent brand to perform subsequent
`process-design-doc` work. Do not process the epic or children in this session.
The document remains local in `docs-wip` unless publication is requested.

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [ ] EPIC. Make world geometry govern unit movement and sprite occlusion
- [ ] SI-1. Define and derive world support surfaces and solid bounds
- [ ] SI-6. Prove geometry-based occlusion on the ruin regression scene
- [ ] SI-10. Preserve stacked structure identity through edits and saves
- [ ] SI-8. Publish committed geometry changes and invalidate spatial views
- [ ] SI-11. Define and author connected slopes, stairs, and floor openings
- [ ] SI-13. Preserve ground-item support identity through placement and saves
- [ ] SI-23. Preserve building support identity and vertical reservations
- [ ] SI-2. Ground units continuously on reachable support surfaces
- [ ] SI-3. Sweep supported unit motion against solid geometry
- [ ] SI-29. Plan bounded routes across distinct connected support surfaces
- [ ] SI-35. Integrate stair appearances with the structure art contract and tools
- [ ] SI-14. Author and approve the intact northeast stair appearance
- [ ] SI-36. Author and approve the intact northwest stair appearance
- [ ] SI-37. Author and approve the intact southeast stair appearance
- [ ] SI-38. Author and approve the intact southwest stair appearance
- [ ] SI-39. Author and approve the damaged northeast stair appearance
- [ ] SI-40. Author and approve the damaged northwest stair appearance
- [ ] SI-41. Author and approve the damaged southeast stair appearance
- [ ] SI-42. Author and approve the damaged southwest stair appearance
- [ ] SI-28. Prove stacked-floor and stair occlusion with manual z slicing
- [ ] SI-34. Integrate the proven backend with production scene assembly and picking
- [ ] SI-12. Carry selected support through player orders and persistent intent
- [ ] SI-4. Integrate reachable interaction points and action completion
- [ ] SI-9. Migrate work approaches and execution to spatial reach
- [ ] SI-30. Migrate melee approaches and execution to spatial reach
- [ ] SI-18. Derive structural connections and support after edits
- [ ] SI-19. Simulate and persist unsupported structural-group falls
- [ ] SI-15. Designate and preview construction on a selected support level
- [ ] SI-31. Complete construction with coherent support and occupied-site waiting
- [ ] SI-22. Deconstruct intact structures and buildings with durable worker progress
- [ ] SI-24. Simulate continuous unit falls and apply existing landing injuries
- [ ] SI-16. Simulate and persist loose-item falls between support surfaces
- [ ] SI-17. Simulate and persist shared placed-building fall motion
- [ ] SI-21. Retain building destruction endpoints as shared persistent wrecks
- [ ] SI-25. Destroy workstations on landing and scatter owned contents
- [ ] SI-43. Integrate non-workstation building falling and landing destruction
- [ ] SI-26. Carry loose items and placed objects on falling structural groups
- [ ] SI-27. Carry units on falling floors with full cliff-equivalent injury
- [ ] SI-32. Deconstruct destroyed targets at intact speed with half recovery
- [ ] SI-33. Restore spatial state and recover pre-existing unit overlaps
- [ ] SI-7. Activate the integrated physical world and pass the full acceptance matrix
- [ ] SI-5. Add local avoidance and bounded conflict resolution between units
- [ ] SI-20. Apply falling-object impact and crushing damage to units

## Epic contract

- **Goal:** Units traverse the ruin, slopes, repeatable stairs, stacked floors
  and reachable roofs with real support and clearance. Construction, support
  loss, falling occupants/sections, destroyed workstations and deconstruction
  produce the agreed physical outcomes at every camera facing and manual z slice.
- **Done when:** The reference sequence proves movement, action reach, rendered
  pixels and picking agree; floor/instance identity and continuous motion survive
  edits, eviction and fresh-process save/load. Construction waits at 99% when
  obstructed; structural groups land intact, while furniture becomes destroyed on landing;
  carried units take full normal fall damage; workstations lose function at
  landing, spill storage and leave solid final-frame
  wrecks. One worker deconstruction action uses equal intact/destroyed work and
  returns 100%/50% of original construction materials respectively at the initial
  balance values. No floor holes, seams, tunnelling, duplicate effects or
  unbounded crowd work are introduced.
- **Approved delivery milestones (D-33):** SI-7 activates the integrated non-crowd
  world, including falling, wrecks and deconstruction. SI-5 adds the agreed
  friendly wait/yield and hostile blocking rules. SI-20 delivers the approved
  later impact/crushing mechanic; it is not a prerequisite for initial falling.
  Passing the ruin scene alone, or activating SI-7 alone, does not finish every
  slice in this design. Evidence-driven sizing gates remain; D-36 settles the
  stair-art subdivision.
- **Users and operators:** Players commanding units; content authors defining
  structures/buildings; developers maintaining movement and rendering.
- **Arc label:** None proposed.

## Current state and evidence

Investigation revision: `13bd01bdeb3043e0087b93adc90d67352a253bf0`.
The storage and movement claims relevant to D-5/D-6 were rechecked on
September 16 at `064a255f06b59e2b1f8e881dcf4c747d52967a36`:

- `World.Edit.Apply` inserts `WeSetStructure` at `(gx,gy,slot)` and
  `WeClearStructure` removes that key without z. Replaying historical edits
  directly as new multi-level inserts would incorrectly resurrect overwritten
  floors. Migration must preserve historical replacement and clear semantics.
- `Structure.Types` uses that same key in the staging cache; changing only
  the committed chunk map would leave concurrent placements ambiguous.
- `Engine.Scripting.Lua.API.Structure.structurePlaceFn` currently reads z
  with `Lua.tointeger` and uses zero when conversion returns no value; queued
  placement and `StructurePieceData.spdGridZ` also use `Int`. D-10 therefore
  requires the full Lua-to-storage path to change. Invalid supplied z must not
  silently become zero.
- `Unit.Sim.Types` stores XY-only `usLocalPath`; `Unit.Pathing.AStar` keys its
  search by XY. `Unit.Thread.Movement.PathAdvance` writes
  `usRealZ = fromIntegral z` on ordinary movement. Its existing slope speed
  penalty and climb interpolation do not implement D-6.
- `World.Render.GroundItemQuads` derives item height from the terrain and
  bilinearly interpolates slope corner drops. This is useful existing visual
  behavior, but an XY-only item cannot identify which stacked floor owns it.
  `Item.Ground.GroundItem` stores instance identity and XY, with no vertical
  position or fall phase. D-14's loose-item falls require an explicit physical
  motion owner and persistence, not a new renderer-only height calculation.
- `Building.Types.BuildingInstance` stores an integer `biGridZ` and contains
  build progress, delivered material instances, storage, and spawn lifecycle
  state. `Building.Reservation` admits XY footprints. Falling placed objects
  require continuous physical placement and coherent occupancy updates, with
  explicit D-42/D-44 workstation teardown rather than accidental loss or replay of
  state through generic destroy-and-respawn.
- The structure slots and pack schema have no stair or ramp definition.
  A filename search of tracked `assets/` and `data/` found no named stair or
  ramp assets. `World.Edit.Types` mentions carved staircases under `WeSetCell`;
  that terrain-edit capability is not an authored stair connector or approved
  staircase art. Asset reuse needs visual inspection before being claimed.

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
  single-level ruin's barrier set can be derived from the existing overlay;
  stacked identity still needs SI-10. `SCeiling` exists (placed at terrain z + 2) but `ruin_small`
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

The September 11 discovery and readiness checks examined all 78 open issue
titles and the epic inventory in `coghex/synarchy`; no matching umbrella was
identified. This is historical evidence, not the new readiness check or final
per-child deduplication. Recheck lifecycle-art epic #2078, structure teardown
#2491, and integration touchpoints #2490 (destructive fluid solidification), #2484
(ground-item repositioning), and #2496 (unit relations/combat). In particular,
the recovery policy here must not override an explicitly destructive world
event's existing entity-removal policy. Recheck these owners when processing
the affected child; this design creates no competing tracker artifacts.

September 17 check for D-25: `src/Building/Thread/Command.hs` handles
`BuildingDestroy` by deleting the live instance, clearing selection/container
memory and retiring power nodes. Its comment explicitly retains craft bills
under the existing demolished-station tolerance. That handler alone therefore
does not satisfy the new falling-workstation requirement to lose all jobs.
SI-25 must integrate job/bill cleanup explicitly, not assume destruction already
does it. This is a targeted new fall transition, not authorization to change
every existing demolition policy.

September 17 check for D-27: `Building.Destruction.destructionFrameIndex`
returns no frame once a clip expires; `Building.Thread.Command` prunes the
effect. `Building.Types.DestructionEffect` and the `bmDestructions` row in
`docs/persistence_state_inventory.md` classify it as render-only,
noninteractive and session-transient. Keeping that effect alive alone does
not supply a durable wreck. SI-21 must introduce classified, migrated remnant
state without silently changing unrelated demolition behavior.

Workbench, Furnace, Machine Shop and Kitchen currently declare no destruction
role in their checked-in YAML. Furnace has four undeclared
`demolish/frame_*.png` preview assets, not a verified directional lifecycle.
Required destruction-art deliveries are already tracked under
[Workbench #2497](https://github.com/coghex/synarchy/issues/2497),
[Furnace #2503](https://github.com/coghex/synarchy/issues/2503),
[Machine Shop #2504](https://github.com/coghex/synarchy/issues/2504), and
[Kitchen #2507](https://github.com/coghex/synarchy/issues/2507), all checked open
on September 17. Their `docs/building_directional_assets_design.md` slices
explicitly include destruction art, but that does not establish suitable wreck
endpoints. The readiness audit below found #2497/#2504/#2507 explicitly require
empty or near-empty last frames. #2503 instead retains the Furnace's existing
south destruction sequence subject to owner confirmation. Reuse those delivery
owners, but reconcile their contracts with D-37 and verify delivered endpoints
before treating them as satisfied wreck prerequisites. The design choice is
settled; the external issues have not been amended here. Do not duplicate their
issues or generate assets here.

September 17 check for D-29: repository searches found the immediate
`BuildingDestroy` and `structure.clear` removal primitives, but no worker-paced
deconstruct job or deconstruction timing contract in this checkout. The comments
on `Building.Types.biMaterialsDelivered` preserve full item instances for
future recovery; they do not implement worker labor or define its rate.
Open-issue title searches for `deconstruct` and `deconstruction` found no match
(not final child-level deduplication). SI-22 must establish or integrate the
intact deconstruction work contract and apply the same one to destroyed targets;
do not claim that an existing destroy API already satisfies D-29.

September 17 check for D-32: `Unit.Thread.Movement.Fall.startFall` already
enters `TransitioningTo Falling` and clears the interrupted movement target;
`Unit.Anim.poseTag` selects the existing `falling` animation family.
`Unit.Thread.Movement.Timers` routes an ordinary fall landing to collapse and
stamps `usPendingFallDrop`; `Unit.Thread.Movement` applies `Unit.Fall.fallInjuries`
and knockdown through the existing wound/death pipeline. These are the reuse
points, not a request for another damage model. Current drop/endpoints are
integer-valued and timer-driven, so SI-24/SI-27 must adapt them for continuous
height and moving supports. Preserve existing integer-drop calibration and
the shared `Unit.Physics` conversion/gravity; do not round away fractional falls
or treat a still-descending carrier as a completed landing.

## Desired experience and scope

An acolyte walks through an actual breach, with clearance for its body, rather
than through intact stonework. Its feet meet the supporting floor. Furniture
occupies physical space and actions target reachable positions beside it.
Units can pass or yield without walking through each other or shaking forever
in a doorway. A wall can hide a unit's lower body while its head remains visible
above the wall; units on another elevation can overlap on screen without
colliding physically.

### Agreed scope at a glance

The decision record below is authoritative; this table is its compact index,
not a new set of proposals. The ruin remains the first regression scene, not
the whole acceptance target.

| Area | Agreed outcome | Decisions |
|---|---|---|
| World and authoring | Stacked supports at one XY; continuous physical z; player construction and authored locations. Engine/Lua accepts fractional anchors, while player tools place at whole z. | D-5, D-6, D-8, D-10 |
| Stairs and roofs | Straight one-tile/one-z flights repeat without landings; adjacent floors provide landings and whole-tile omissions provide openings. Reachable roofs are walkable. Intact and separate damaged stair art use PixelLab with personal owner signoff. | D-11–D-13, D-22, D-34, D-35 |
| View and movement | Manual camera z slicing; physical walls and full-body clearance; friendly wait/yield, hostile blocking without cooperative yielding. | D-1, D-9, D-20, D-21 |
| Construction | Unfinished pieces provide no collision/support. Body-intersecting completion waits at 99% with materials/progress retained; sealing a room without intersecting occupants is allowed. | D-23, D-24, D-38 |
| Structures | Terrain-connected overhangs hold; detached sections fall straight down without rotation, stopping intact as a whole when any piece reaches valid support. Until impact/crushing, unit contact pauses rather than lands the group. | D-16, D-17, D-19, D-39, D-40 |
| Occupants | Removal beneath occupants is allowed. Loose items ride falling support; carried units use falling animation and full normal cliff-fall damage. | D-14, D-15, D-31, D-32 |
| Furniture | Cargo Holds, Solar Panels, Batteries and Portals suspend services during falling, then become solid final-frame wrecks at landing and scatter stored items in full. Art coverage and precise scatter geometry remain gated. | D-15, D-42, D-43, D-46 |
| Workstations | Jobs and services pause during descent. Destruction and permanent job/function teardown occur at landing; stored items scatter in full there. Destruction playback retains a solid, nonfunctional last-frame wreck. | D-26–D-28, D-37, D-42, D-44 |
| Deconstruction | One action and the same work/time rules for intact and destroyed targets. Initial recovery is 100% of original construction materials for intact targets and 50% for destroyed targets; both rates may be tuned later. Stored cargo is separate. | D-29, D-30, D-41 |
| Compatibility | Existing saves do not fail on unit geometry; introducing slices own real migrations, not just a save-version bump. | D-3, D-5 |

### Deferred and excluded work

- **Later slice in this plan:** SI-20 impact/crushing damage to units struck by
  falling objects. Ordinary unit fall damage is required initially, and physical
  contact uses D-39's temporary pause/resume rule while damage is deferred.
- **Future work outside this plan:** structural rubble conversion and automatic
  camera cutaway (D-19/D-9). Persistent workstation final-frame wrecks are required
  now; they are not the deferred structural-rubble feature.
- **Not selected:** a full rigid-body/strength/stress model, ragdolls, automatic
  climbing onto every object, new ranged/perception occlusion, workstation repair/
  rebuilding, loose-item impact damage or general impact-triggered spilling.

### Proposals remain distinct from decisions

Body dimensions/calibration, the stair contact envelope, precise geometry
representation, other support/contact details and performance budgets remain
engineering proposals or explicit Q-N gates. Small loose items are proposed
nonblocking; this is not a blanket decision about every large ground object.
Art supply and the directional stair inventory are settled by D-13/D-35/D-36;
delivery and personal signoff remain required. D-39 settles the interim
falling-object response to a unit below, not every terrain/object contact.

A wall must stop horizontal motion; it must not lift a unit onto its top.
Support resolution supplies height only for an eligible supporting surface.
Likewise, collision with another unit does not automatically make that unit a
platform. Radius and height are gameplay dimensions, not just an anchor point.

## Design and integration contracts

Decided gameplay rules retain their D-N references. Proposed algorithms,
representations and tuning are marked as such; neither implementation guidance
nor an acceptance test silently resolves an open Q-N policy.

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

**Durable placement and intent; derived geometry.** D-5 supersedes the former
no-new-state default. Placed pieces, their elevation, connections, unit/item
support identity, and destinations need an unambiguous durable representation.
Geometry indices, projected occluders, and cached route searches remain derived
views rebuilt on load and geometry revision change. A whole live simulation
record is already serialized today; reclassifying a route as rebuildable must
explicitly preserve its durable destination and movement phase, with a component
migration rather than silently discarding current fields.

`usGridZ` may remain a derived integer address for voxel queries; it is not
authoritative physical height or floor identity. A continuous foot position and
support identity must reach collision, action reach, rendering, and picking.
Whether that evolves `usRealZ` or introduces a new type is settled with the
consumer inventory and migrations, not by changing its comment alone.

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
near seams must agree with their canonical equivalents. This does not supply a
world-spanning route planner, but the bounded planner must distinguish multiple
supports in one column and find the reference building's connecting staircase.

### Stacked storage, connections, and compatibility

Proposed representation: a page-qualified placed-piece identity, an exact
authored base elevation, and a slot/kind. The chunk index distinguishes both
elevation and slot; an incarnation distinguishes a replacement at the same
address. Derive one or more support-surface identities from the piece plus
local surface index. Continuous evaluated height is never a floating-point
hash key. D-10 requires fractional authored base heights in the engine and
direct Lua API. Player construction applies a whole-z placement policy above
that shared primitive; it must not impose quantization on authored content.

Choose and document a canonical finite numeric representation with sufficient
precision and range for world coordinates. Do not introduce a whole-z or
eighth-z restriction. Any representational precision is explicit; the accepted
placement value round-trips without later snapping. Identity is stable and
separate from numeric proximity: contact tolerances cannot merge distinct
placements. Queries/removal should prefer piece identity; a height-qualified
address uses the same canonical conversion as placement, not a separate
epsilon comparison at each consumer.

The general Lua placement API accepts representable fractional heights such as
2.5. Validate finite/range/geometry constraints before palette interning,
staging, or queuing; a malformed explicit value is a refusal, not omitted-z
defaulting. Every transport, edit, replay, geometry, render and codec boundary
preserves that height. Player previews snap their placement anchor to whole z,
and higher-level player construction admission enforces the same policy.
Both paths retain overlap, support, and clearance validation. Lua-authored
fractional content remains selectable and usable; player placement restrictions
do not round existing content or force units to whole-z physical positions.

SI-10 migrates the complete structure path: placement staging and commit tokens,
queued commands, ordered edits and replay, queries/removal, chunk eviction,
render enumeration, and save components. A new precise removal names one piece
or one elevation-qualified slot, leaving the other floor untouched. Legacy APIs
must preserve their old unambiguous behavior or refuse an ambiguous column;
they must not silently select the highest floor. `structure.floorZAt` alone
cannot represent all supports and needs a plural/qualified successor.

Historical replay retains historical semantics. An old set at z=1 followed by
an old set at z=4 in the same slot yields only the latter, and an old clear
removes that legacy slot. Freeze outgoing wire shapes and either normalize the
old log under its original replay semantics before emitting new records or
retain an explicit legacy replay adapter. Verify mixed legacy/new edit order,
replacement, removal, eviction, and fresh-process reload. Do not reinterpret
each old set as a new independent floor. Allocate stable identities
deterministically during conversion and do not reuse removed identities.

SI-11 defines directed connector endpoints, orientation, width, support
profiles, slab openings, and headroom. Under D-11, a stair piece is a straight
flight with no built-in landing. Matching upper/lower endpoints of successive
flights connect directly at the next horizontal position and elevation, making
a longer uninterrupted staircase. Each piece occupies one tile and rises one
z-level (D-12); the next piece is one tile forward and one z higher. The same
relative geometry applies to a fractional Lua-authored base height.
Any landing is an independent floor piece
in an adjacent space, with its own identity and placement/removal lifecycle.
Connections join actual surfaces; proximity in XY or z is not enough.
A staircase must reach an opening in the upper slab and permit the whole body
through it. D-34 uses absent whole floor/roof tiles for that opening, including
multiple cells when needed for headroom; partial-tile cutout pieces are excluded.
The underside of every remaining slab remains
a solid clearance boundary, and passing underneath is legal only where it fits.
Floor top and underside are separate roles of the same slab. D-22 applies the
same distinction to structural roof/ceiling pieces: the top is walkable support,
and the underside remains an overhead solid bound. SI-1/SI-11 must provide
physical roof surfaces rather than infer them from sprite pixels or require
a separate opt-in walkability flag. A roof still needs a legal route onto it;
collision with its edge or the wall below cannot lift a unit onto the top.

### Structural connections and overhangs

D-16 permits horizontal attachment to hold an overhanging wall, floor or stair
in place. The piece need not have solid ground directly beneath every occupied
space. A piece with neither supporting contact nor any structural connection
falls. This is a connection/support rule; no strength, weight, cantilever-length,
or bending model has been selected.

SI-18 derives a structural connection graph from placed piece identities and
their authored attachment geometry. Keep it distinct from the navigation graph:
a wall can transmit support while blocking movement. Evaluate world-space
contact at the actual authored heights, including fractional Lua placement;
neighboring XY keys alone do not establish an attachment. The content contract
specifies which wall ends, floor edges, post contacts, stair ends and vertical
contacts join, and which are merely nearby. A shared screen pixel, diagonal
corner, or matching bounding box is not automatically a structural joint.
Reuse ordinary floor pieces for landings; no hidden connector fills the gap.

D-17 completes the support rule: a connected component stays supported when
it has a path to an anchored terrain contact, directly or through another
supported structure. A section whose last anchored connection is removed falls
together. Two mutually connected pieces cannot hold each other indefinitely
in mid-air. Removing one bridge connection may split the graph: components
still reaching support remain fixed, and detached components fall independently.

Connection updates belong to the same committed geometry revision as placement,
demolition and terrain edits. Query/rebuild affected components with bounded
work; do not run an unbounded all-world scan each movement tick. Loaded chunk
boundaries and the cylindrical seam cannot break a connection that exists in
the world. An unavailable neighbor is unknown, not evidence of disconnection;
define the residency/continuation strategy before activation. Rebuild derived
connectivity from durable content on load, with stable ordering and identical
classification regardless of map traversal order.

SI-19 owns the transition from a committed static placement into structural
fall state and back into supported geometry on landing. Keep each piece's
identity, appearance, authored orientation and connections explicit, with one
authoritative physical position. The edit overlay and moving representation
must not both publish colliders or render copies for the same piece. Mid-fall
saves and chunk eviction cannot respawn the old fixed-height structure or lose
the moving one. D-17 requires coherent group motion that preserves relative
piece placement; do not schedule independent drops that tear an intact detached
section apart merely because of iteration order. D-40 specifies upright,
vertical-only translation: the earliest load-bearing contact of any member
stops the whole connected group, even when other members overhang empty space.
D-39's contact with a unit below is only a temporary pause, not that landing.
D-31 requires loose items
resting on a falling surface to move with it while that support remains valid;
do not apply a second independent gravity update or let them pass through it.
Preserve item/support association and physical placement across descent and
save/load. Removing the actual supporting piece releases the item into the
ordinary loose-item fall path. D-32 makes supported units descend with the floor
in the existing falling animation, taking full normal fall consequences on
landing. Moving support cannot suppress falling state or zero out the injury
drop. Remaining non-workstation-furniture carrier details, precise contact
geometry and reattachment mechanics remain Q-11 details before that
slice is specified. D-19 requires a landed section to remain
intact and usable at its new physical height: valid floor/stair surfaces still
support traversal and solid walls still block it. Rebuild geometry and access
from the landed position; intact does not guarantee a route to every surface.
Rubble conversion is a future extension, not part of this landing transition.
SI-26 owns the item/placed-object carrier integration and SI-27 the unit carrier
and injury integration; SI-19's initial group-motion fixtures are unoccupied.
No generation or physics implementation occurs in this design task.

All changed serialized owners require versioned component migrations and frozen
nested DTOs under `src/World/Save/CLAUDE.md`. The introducing owners are:

- SI-10: world edits and stacked placement; SI-13/SI-23: item/building placement.
- SI-2: supported unit position/phase; SI-29: route state; SI-24: free-fall and
  landing-injury accounting; SI-12: move/hold intent and affected Lua state.
- SI-15/SI-31: construction intent and completion/progress; SI-22/SI-32:
  shared deconstruction progress/recovery and destroyed-target extension.
- SI-16/SI-17: item/shared building motion; SI-21: shared durable wreck/playback;
  SI-25: workstation destruction and pending spill/salvage ownership;
  SI-43: non-workstation lifecycle adapters reusing the same boundaries.
- SI-19: moving structural groups relative to static edit replay; SI-26/SI-27:
  object/unit carrier associations and single-owner motion/injury continuity.

SI-18's connection index is derived and rebuilt. SI-8 establishes publication;
SI-33 reconciles restoration. Neither collects migrations deferred by these owners.
Verify the actual component ownership during each slice's inventory; these are
responsibilities, not an assumed exhaustive list.
Old saves derive support from their saved physical position and the historical
placement, never from the highest new surface in the column. D-3 still governs
unresolvable unit geometry. No global version bump substitutes for migration.

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

Explicitly define the relationship among voxel address, support identity,
continuous foot height, and the existing climb/fall state. Audit range checks
and other `usGridZ` consumers before assigning physical meaning to a field.
Rendering interpolation is a view of physical motion, never its sole owner.

Propose a surface evaluation `z = h(x,y)` over an explicitly bounded support
patch, piecewise linear for ramps and the proposed stair walk envelope. Sample
the actual position throughout movement, including within a tile and on every
residual segment. Shared edges agree at endpoints; discontinuous or disconnected
edges are a step/climb/fall decision, never blended into an invented ramp.
Terrain slope masks must be interpreted against the rendered terrain shape;
the current item bilinear helper is a compatibility input, not proof that all
slope combinations are planar. SI-1 measures and chooses one surface convention
shared by contact and pixels, retaining any intentional art approximation.

Proposed stair contact is a continuous inclined walk envelope over visible
treads. It joins directly to another stair flight or to a separately placed
adjacent floor, never to a built-in landing. This meets the requested linear
ascent without requiring per-foot inverse kinematics or a discrete hop on every tread. The
body still collides against the real slab, sides, and overhead geometry; the
envelope cannot authorize movement through a closed upper floor. Q-8 records
the remaining contact/authoring choices. Existing walking animation is a
candidate requiring visual acceptance, not assumed proof of foot contact.

Ordinary supported ascent uses walking, not the cliff-climbing transition for
every tread. Steep unsupported edges keep the existing climb/fall policy.
SI-2 preserves continuous supported position, including stopping halfway along
a slope/stair; SI-24 preserves interrupted unsupported fall state and landing
injury. Landing chooses the first eligible support along the trajectory, not
the column's highest terrain or floor. SI-13/SI-23 give loose objects/buildings
their own elevation-qualified placement; an upper-floor item cannot drop to
ground solely because the renderer sampled terrain.

### Planning and actual motion agree

SI-3 owns shared connection admission and sweeps; SI-29 owns bounded search
and route following using that same contract.

Pathfinding asks whether the unit's body can traverse each connection; walls
block edges rather than making their entire adjacent tiles impassable. Gates
include radius, clearance, support transition, and existing hazard policy.
Path state identifies a support/elevation as well as `(x,y)` in all node,
visited, cost, parent, goal, and waypoint records. Matching XY on a different
floor is neither arrival nor a reason to return an empty path. The heuristic
and partial-route score must make progress toward the actual support and its
connections, including when reaching the stairs initially increases XY distance.
Bound expansions across all levels and report exhausted searches distinctly.

The continuous mover sweeps the body's volume along the complete proposed motion,
including every residual waypoint segment, to prevent passing through a thin
wall between ticks. It advances to a safe contact, slides or replans according
to policy, and preserves existing timing and position-hold contracts. It does
not implement collision only in A*, only in Lua orders, or only at the final
position of a tick. Ramp elevation changes require a swept vertical interval
as well as a horizontal footprint; check headroom over the entire segment.
Q-8 also settles whether walking speed is measured along the sloped surface or
in XY. SI-3 preserves the residual-time accounting invariant using the chosen
distance metric; it cannot silently change speed at a waypoint or charge the
same elapsed time twice.

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
allowed outcome under D-38, not an overlap eligible for exemption. The fallback
when neither mechanism frees an actually overlapping unit is settled under
Q-5. Placement rejection remains a real safeguard, not
a courtesy.

Proposed first-release body: a circle footprint and the YAML-derived height interval,
with the required per-definition radius settled by D-4 and its numeric values
calibrated under Q-4. Passage through a breach is a measured clearance check: a
missing wall segment is nominally one tile wide, but the posts at its
vertices, the wall thickness, and any clearance margin narrow it, so no
radius bound guarantees passage and none is claimed. Overhead clearance
is required beneath every upper slab/ceiling and along all stair transitions;
it cannot be deferred now that stacked floors are required (Q-4).

Dynamic unit avoidance is a later layer over this static legality check. It
needs deterministic priority/yield rules and bounded progress handling at
doors. Simple pairwise repulsion alone does not establish freedom from jams.
Vertical separation must exclude units on distinct floors from ground-plane
avoidance when their bodies do not overlap.

D-20 selects wait/yield for friendly congestion, not squeezing through another
unit or shrinking collision bodies to admit a pass. A stopped or position-held unit remains
occupied space. A yielding mover cannot silently clear another unit's player
hold. Symmetric doorway conflicts use stable priority plus bounded retry/yield
rules; when geometry provides no passing space, stopping with a blocked result
is an acceptable specified outcome. A guarantee that every crowd eventually
passes every doorway is not proposed. Bodies cannot be shrunk merely to make
an animation or congested passage fit; any posture-dependent clearance needs
an explicit transition check, including room to stand up again.

D-21 keeps hostile units physically solid without cooperative yielding. A
legal route may go around them; otherwise existing combat/order rules govern
engagement or a blocked outcome. Avoidance cannot initiate an attack, override
a hold, or make an enemy step aside just to satisfy another unit's move order.

### Interaction and lifecycle must participate

Player commands carry the selected surface, not just its projected XY. SI-12
adds a support-qualified world pick and threads it through command admission,
the move target, queued orders, return-to-hold anchors, and persistence. A
click on the second floor cannot send a unit to the ground floor below it.
Decided view policy (D-9): use the camera's manually selected z level to expose
a floor and pick only the visible eligible surface; hidden geometry remains
physical. No automatic cutaway or selected-unit floor following belongs to this
arc. Terrain-only `world.pickPos`
and XY-only `unit.moveTo` callers need an explicit compatibility path and
inventory; ambiguous calls cannot default silently to the topmost support.

Targets identified by entity ID resolve their current support and reachable
approach points. An item, workstation, and held position above a unit are not
in reach merely because their XY matches. Placement occupancy and building
reservations also need a vertical interval so furniture on one storey neither
reserves all storeys nor intersects an upper slab unnoticed. The retained
identity follows item drop, pickup, spill, repositioning, building placement,
save/load, and chunk eviction. Player transfer policy remains separate from
lax AI transfer verbs; audit which physical restrictions each promises rather
than making them identical incidentally.

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
the shared contract and migrates pickup/transfers; SI-9 migrates work and SI-30
migrates melee approaches. The caller inventory records both the movement destination
and authoritative completion check. Terrain-only perception and ranged sight
remain outside this arc; their existing limitation must be explicit rather
than being presented as fixed by melee reach or physical wall collision.

### Committed lifecycle and destruction boundaries

SI-8 establishes committed publication for edits, residency and session
replacement. SI-31/SI-22/SI-32 integrate construction/deconstruction and each
motion owner handles support loss; SI-33 reconciles restoration. Publish physical
geometry and visible state from the same committed revision. Each introducing
slice owns its persistence/migrations; final integration cannot postpone them.
The detailed owner decisions are recorded under D-14–D-32; the following maps
their transition boundaries.

| Transition | Required publication | Owners |
|---|---|---|
| Blueprint → partial construction → occupied 99% wait | No collision or support; retain materials/progress. Unfinished stairs cannot provide work access or anchor an overhang. | SI-15/SI-31; D-23/D-24 |
| Clear, valid site → completed piece | Activate solid bounds, walkable surfaces and structural connections together; revalidate actual 3D occupancy, not only XY. | SI-31/SI-8 |
| Support removed | Start one fall for each newly unsupported occupant; removing an occupied floor is allowed. Existing unit order cancellation remains in force. | SI-24/SI-16/SI-17/SI-25 with SI-8; D-14/D-15 |
| Structural group detaches → lands | Transfer static geometry into one moving group, carry loose items/units, then restore intact supported geometry. Units animate as falling and receive full cliff-equivalent landing injury. | SI-18/SI-19/SI-26/SI-27; D-16/D-17/D-19/D-31/D-32 |
| Workstation starts falling | Pause jobs and services without marking the workstation destroyed. Preserve paused job state and identity/cargo/material ownership; no work advances or produces output during descent or a contact wait. | SI-25; D-42/D-44 |
| Workstation lands → destroyed | Commit permanent job/function teardown and one cargo spill on the actual landing tile/support, then start declared destruction playback. No work resumption after destruction. | SI-25/SI-21; D-26/D-27/D-42 |
| Destruction playback ends | Retain the correct-facing final frame and solid wreck collider; do not expire either or restore functionality. | SI-21; D-27/D-28 |
| Worker deconstruction completes | Remove the exact target/collider once. Use the same work/time and return 100% of original construction materials if intact, 50% if destroyed, separately from the full prior storage spill. | SI-22/SI-32 with SI-8; D-29/D-30/D-41 |

These transitions share the following integration invariants:

- **Legal placement:** Proposed rejection of new solid placements intersecting
  a unit or invalidating immediate support/clearance is distinct from the decided
  99% construction wait. It does not forbid passing through unfinished geometry
  or promise that every room remains globally accessible. D-38 permits sealing
  a room without intersecting its occupants; no escape-route guarantee applies.
- **Real support:** Recheck the first eligible landing along the trajectory,
  including stairs and fractional floors. Occupancy follows the physical volume,
  not the old floor or the camera slice. Unknown/unloaded geometry is not a known
  void. No-lower-support and missing-data handling remain separate Q-4 gates.
- **Single ownership:** Preserve item/ground IDs, quantities and nested contents;
  no destroy-and-respawn copies, duplicate gravity or duplicate colliders.
  Non-workstation furniture retains owned state through descent, then enters D-42's
  landing destruction. Workstation destruction
  must invalidate stale workers before they can produce outputs. Audit occupancy,
  power, work, selection, storage, spawn and render consumers across
  SI-17/SI-21/SI-25/SI-43; SI-17 itself owns motion, not service teardown.
- **Separate payloads:** A destroyed workstation's pending storage spill and
  recoverable construction materials survive descent/load under one owner each,
  but are not usable storage. Retain material provenance until deconstruction;
  never re-spill cargo, replay placement/spawn rewards or apply salvage loss twice.
- **Real restoration:** Discard/rebuild derived views on session replacement
  according to classification; preserve durable motion, injury accounting,
  destruction, wreck/playback and work progress. No resurrected jobs, restarted
  completed clip, missing/duplicate fall injury or duplicate salvage publication.
  Old unit overlaps use Q-5, not an arbitrary render lift or an unbounded search.
- **Two damage contracts:** Unit self-fall injury is required now, including
  carried units (D-32). Damage to a unit struck/crushed by another falling object
  is deferred to SI-20 (D-18). Earlier slices use D-39: stop a falling object
  or whole connected group at unit contact, resume when clear, and do not
  treat the pause as landing. Workstation destruction and carried-unit self-fall
  accounting continue to obey their separately decided lifecycle rules.

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

D-33 separates SI-6's early ruin proof, SI-28's extended stacked/stair proof,
SI-34's production scene/picker integration and SI-7's final activation.
Collision alone is not an occlusion fix. A failed proof reopens the affected
backend/sizing decision before its consumers proceed. There is no code-level
staging switch to remove later: explicit physical fixture content exercises
real owners, while shipped metadata retains conservative support/nonblocking
defaults until the complete SI-7 gate passes. No test-only parallel simulation
or player-facing partial-feature switch is introduced.

SI-8 establishes committed geometry publication early. Each later placement,
motion, graph and action owner joins it in its introducing PR, including
support-loss behavior, migration and classified restoration. SI-33 adds the
bounded Q-5 overlap-recovery integration. Old saves can already contain units
inside future solids; spawn legality is not evidence that they need no recovery.
SI-7 tests both old saves and newly active content with fresh-process reload.

Normal gameplay activation requires all of these code and art branches:

- Foundation, stacked identity and connectors: SI-1/SI-10/SI-11.
- Continuous support, sweeps, search and free fall: SI-2/SI-3/SI-29/SI-24.
- Item/building placement and falling: SI-13/SI-23/SI-16/SI-17; workstation
  teardown/spill and shared solid wrecks: SI-25/SI-21; non-workstation
  adapters: SI-43, including approved external art for every admitted definition.
- Structural support/motion and object/unit carriers: SI-18/SI-19/SI-26/SI-27.
- Selected-support intent and action reach: SI-12/SI-4/SI-9/SI-30.
- Construction intent/completion and intact/destroyed deconstruction:
  SI-15/SI-31/SI-22/SI-32.
- Lifecycle publication and restoration recovery: SI-8/SI-33.
- Stair art-consumer integration (SI-35), all eight approved appearances
  (SI-14/SI-36–SI-42), rendering proofs (SI-6/SI-28) and production
  scene/picking (SI-34).

SI-7's dependency closure includes every initial branch. Empty-group,
non-workstation and controlled-route fixtures make earlier slices independently
testable; they do not permit shipping missing carriers, live falling stations
or incomplete player construction. Every new dynamic producer retains its own
visual evidence; final integration repeats the full matrix and budgets.

Crowds (SI-5) and impact/crushing of struck units (SI-20) follow activation.
Neither delays the initial gate or substitutes for required self-fall injury.
Each PR includes its own tests, documentation and required owner evidence;
final integration supplements, rather than replaces, those obligations.

## Decisions

### D-1. Address physical interaction and rendering together

The owner accepted a feature covering real interactions between units,
structures, and objects, including solid walls and supporting floors. The goal
is consistent world behavior and visible ordering, not just changing sort
constants. Algorithm details in the design section remain proposals.

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

### D-5. Multiple walkable floors at the same `(x,y)` ship in this arc

On September 16 the owner explicitly required stacked level support now as
game-critical behavior. A unit must be able to travel to, occupy, and interact
on a second building floor while another surface exists directly below it.
The earlier single-floor-first proposal is rejected. Storage, edits, routing,
targeting, lifecycle, and save compatibility must all represent the distinction.
This decision permits necessary schema design; it does not waive migrations.

### D-6. Physical elevation varies continuously through slopes and stairs

The same owner instruction requires linear, non-discrete z movement. Supported
motion samples elevation along the traversed surface at the unit's actual
horizontal position. It must not snap between integer z levels or provide only
a cosmetic render interpolation while collision and action reach stay on an
integer floor. Stairs and slopes connect lower and upper supports in an ordinary
walk route. The exact stair contact model remains a proposal, not an additional
owner decision about tread geometry or animation.

### D-7. Finish the design here; a different brand processes it

This session edits only this document. The owner reserves epic and child
processing for a different agent brand. No tracker drafting, creation,
implementation, or publication is authorized by this design request.

### D-8. Both player construction and authored locations support stacked levels

The owner selected both on September 16. Players must be able to build upper
floors and stairs, and location authors must be able to supply them. Both use
the same placement identity, geometry, and connection rules. SI-15/SI-31 are required,
including elevated designation, worker access, completion, and precise teardown.

### D-9. Floor visibility uses only the camera's manual z level

The owner chose manual slicing only on September 16. The camera's z level
controls the view; automatic cutaway is explicitly far-future work. Selection
of a unit, its ascent to another storey, or a command to another floor does not
automatically move the slice or fade an upper floor. Picking, destination
feedback, box selection, and construction previews must agree with the visible
slice and name the actual support. Hidden floors remain physical. Continuous
elevation does not require a new automatic camera behavior.

### D-10. Fractional placement is general; whole-z placement is player policy

On September 16 the owner chose a general engine/storage design that permits
fractional placement heights, including through direct Lua calls. The normal
player construction tools restrict new placement anchors to whole z levels at
a higher layer. This restriction belongs to player construction policy, not
the shared placement primitive or save format. Both paths preserve the same
physical validation and accepted placement value. Units continue to move at
fractional heights regardless of which path authored the support.

### D-11. Straight stair flights repeat without built-in landings

The owner corrected the stair proposal on September 16: each stair piece is a
straight flight, repeatable diagonally along its horizontal run and vertical
rise to create a longer stair. Repetition must not insert a flat landing between
pieces. Landings are separate floors on adjacent spaces. Turns therefore use
separate floor placement and another oriented flight; the stair asset and
collision profile do not contain an attached platform. Rise/run, footprint and
alignment must make repeated endpoint heights and widths meet without gaps,
overlap, a vertical snap, or an artificial pause in walking.

### D-12. Each stair piece occupies one tile and rises one z-level

The owner confirmed one tile of horizontal run and one z-level of rise on
September 16. A repeated piece advances one tile in the flight's direction and
one z-level upward. The flight spans its full tile without a flat landing
portion. A chain of three pieces has three tiles of run and three z of rise.
Lua placement may start it at a fractional base under D-10; that does not change
the per-piece dimensions. Width/edge clearance, tread depiction, and slab
opening calibration must fit this decided footprint and rise rather than
silently changing them to accommodate an asset.

### D-13. Generate stair art with PixelLab and obtain the owner's personal signoff

The owner selected PixelLab generation on September 16 and explicitly requires
personal signoff. Use the existing dungeon art as the visual reference for the
repeatable staircase. Approval of this production method is not approval of
any generated image. The later art delivery includes provenance, a production
build, real in-engine preview, and the owner's recorded verdict before final
review and landing. Keep that evidence in the asset's PR. This session still
only finishes the design; it does not generate art or process tracker items.

### D-14. Occupied floors may be removed; units and loose items fall

On September 17 the owner chose to allow removal and falling. A unit or loose
item standing on a floor does not prevent demolition. When its support is
removed, it falls to the next valid supporting surface. This is physical
movement through the intervening space, with the same result at every camera
slice. Preserve the unit fall consequences and item identity; this does not
authorize adding item impact damage. Structural support/falling is separately
required by D-16.
This decision covers units and loose items; D-15 extends it to furniture and
workstations. D-16 settles structural overhangs and isolated-piece falling;
D-17 settles detached groups, and D-18 defers approved furniture/structure
impact damage to SI-20. Remaining contact questions are under Q-11/Q-4.

### D-15. Furniture and workstations also fall when they lose support

On September 17 the owner chose falling for placed furniture and workstations;
they need not be dismantled before their supporting floor can be removed.
The later correction D-42 governs destruction timing for both: they become
destroyed on landing, not at fall onset. The former promise that non-workstation
furniture lands intact and the intervening workstation onset-destruction rule
are superseded. Keep identity, original-material provenance and contents
accounted for through descent and the landing transition. D-43 selects solid
final-frame furniture wrecks; D-46 resolves full furniture cargo spill at landing. D-44
settles pausing workstation jobs/services during descent, then discarding them
at landing destruction.

D-45 assigns shared placed-object motion to SI-17, shared wrecks to SI-21,
workstation landing teardown/spill to SI-25, non-workstation adapters to SI-43
and carriers to SI-26. Art, contact and material-accounting gates remain before child approval.
Partial-footprint support remains open; D-18's later unit impact/crushing timing
is unchanged.

### D-16. Structural pieces fall when disconnected; side connections permit overhangs

On September 17 the owner required unsupported walls, floors and stairs to
fall, while explicitly allowing overhangs: a wall connected horizontally on
one side can remain in place; a piece with no connections on any side falls
when it has no supporting contact below. Do not require support directly below
every overhanging piece or attachments on both horizontal sides. D-17 settles
the case of several pieces connected only to one another. Structural
stress/strength simulation is not implied.

### D-17. A detached connected structural group falls together

The owner confirmed on September 17 that a connected group of walls, floors,
and stairs falls together when it loses its last connection to terrain or a
supported structure. Remaining mutual connections do not keep a detached group
suspended. Support is transitive through the connected structure; a one-side
overhang can remain attached through that path. Detached-group identity and
relative piece positions must survive physical motion and save/load. This
decides support and grouping. D-18 separately decides damage scope and timing;
remaining contact/reattachment details remain under Q-11; D-19 settles intact
landing rather than immediate rubble conversion.

### D-18. Impact/crushing damage is intended, but delivered in a later slice

On September 17 the owner approved impact/crushing damage to units hit by
falling furniture or structures, explicitly deferring it to a later slice.
SI-20 owns that follow-on; SI-17/SI-19 and initial production activation must
not depend on delivering the damage mechanic. Preserve existing injuries from
a unit's own fall. D-39 subsequently settles initial contact with units below:
pause at contact and resume when clear, without displacement or impact damage.
Q-4/Q-11 retain the other support/contact details. Q-12 gates SI-20's damage
rules. Loose-item impact damage, object breakage and contents spilling are not
added by this decision.

### D-19. Landed structural sections remain intact initially; rubble comes later

On September 17 the owner confirmed that a fallen structural section initially
remains intact and usable at its new height, with eventual conversion to rubble
reserved for future work. SI-19 preserves its pieces and their physical roles
through landing; do not destroy/respawn the section as rubble or leave a merely
decorative copy. Reachability, support, collision and picking use its actual
landed geometry. Exact contact/reattachment mechanics remain gated under Q-11.
Future rubble needs a separate design for conversion, gameplay, persistence
and any missing art; it is not silently included in SI-20's unit-damage slice
and does not block this arc. No new structural rubble assets are required for
initial delivery. D-27 separately requires workstation wrecks using their
destruction-animation endpoints, not structural rubble conversion.

D-43 reaffirms this structural rule after narrowing D-42 to workstations and
other furniture. The group keeps its existing appearance and intact recovery.

### D-20. Friendly units wait or yield instead of squeezing through one another

On September 17 the owner chose wait/yield when friendly units cannot pass
with physical clearance. They may move aside where there is room, wait, or
replan; friendship does not permit overlapping bodies or artificially reduced
collision radii. Preserve the existing player position-hold contract: yielding
cannot silently clear another unit's hold. Bounded conflict handling may return
a blocked outcome when no legal passing space exists; this is not a guarantee
of progress through every crowded doorway. SI-5 owns this behavior. D-21
settles enemy encounters; exact priority/retry rules remain gated under Q-2.

### D-21. Enemies block passage without cooperative yielding

On September 17 the owner approved solid occupancy for hostile units without
friendly-style cooperative yielding. Units must use a legal route around an
enemy, engage under existing combat rules, or stop with a blocked outcome.
This does not authorize walking through enemies, body shrinking, forced
displacement, or a new automatic-attack policy. Preserve existing orders,
position holds and combat authority. SI-5 applies this distinction only where
the units' physical bodies actually conflict; same XY on separate floors does
not by itself block a route.

### D-22. Roofs are walkable by default

On September 17 the owner confirmed that roofs are walkable. A reachable
structural roof/ceiling top is a support surface without requiring a separate
content opt-in. Its underside still blocks upward passage and enforces
headroom below. Apply the ordinary body-clearance and slope/traversal rules;
this does not authorize teleporting onto roofs, walking through a ceiling, or
automatically climbing every roof edge. Physical geometry must describe the
top independently of its artwork. Include roof traversal in the multi-level
reference scene; manual camera slicing, support identity, falling and save/load
must agree with the roof surface just as they do with an upper floor.

### D-23. Structural collision and support activate only on completion

On September 17 the owner chose completion as the activation point for walls,
floors and stairs. Planned or partially built pieces provide neither collision
nor support; beginning work or delivering materials does not change that.
Their roof/ceiling counterparts follow the same structural-piece lifecycle.
At committed completion, publish solid bounds, walkable support and structural
connections coherently, invalidating affected routes and geometry. Existing
completed authored pieces participate immediately when committed; this rule
does not introduce a construction requirement for authored locations.
Preserve unfinished state across save/load without phantom supports or walls.
D-24 settles a unit obstructing the final solid volume: wait at 99%, never
embed or displace it to force completion. D-38 separately permits enclosing
a unit whose body does not intersect the new solid.

### D-24. Occupied construction waits at 99% until the space clears

On September 17 the owner specified that construction obstructed by a unit
sits at 99% complete until the space is unoccupied, retaining materials and
earned progress. This is an unfinished physical state, not a completed object
merely displayed as 99%: D-23's no-collision/no-support rule still applies.
Check the final solid volume at the correct elevation, not just a shared XY
tile; units on a non-intersecting storey do not block completion. When clear,
revalidate occupancy and the other completion preconditions at the committed
transition. Keep completion and any associated effects exactly once, even
across save/restart or repeated blocked checks. The unit need not be moved by
the construction system, and its player hold is not implicitly cleared.
Bounded retry scheduling must preserve the site, materials and progress while
blocked; it must not silently cancel the build or repeatedly charge materials.

### D-25. Earlier workstation onset destruction — superseded by D-42

The earlier record put workstation destruction and irreversible loss of
jobs/functionality at fall onset. The owner clarified in D-42 that destruction
must instead happen when falling ends. Move permanent job/bill/claim teardown
and functional destruction to the actual landing boundary, alongside D-26's
cargo spill and D-27's destruction playback. Never resurrect the workstation
after that boundary.

Under D-44, jobs and services pause while airborne; permanent teardown waits
until destruction at landing. The pause must not secretly perform the
irreversible onset destruction that D-42 corrected. The rule applies to both independently falling
and carried workstations. D-43 confirms other furniture shares landing
destruction, while structural pieces retain D-19's intact landing behavior.

### D-26. Destroyed workstation contents scatter on the landing tile

On September 17 the owner chose to scatter stored items on the tile the
workstation falls onto. They become loose items on that landing support,
including an elevated or fractional-height floor, rather than being deleted,
left on the original storey, or spread arbitrarily across adjacent tiles.
Preserve each item's identity, quantity and nested contents. Scatter offsets
must be valid positions within the landing tile/support; precise placement
and multi-tile workstation landing-anchor rules are bounded SI-25 details.

Keep the contents under one authoritative owner through the fall and publish
the spill exactly once. Save/restart before or after landing cannot lose items,
clone them, re-scatter already published items or restore functional station
storage. If support changes again, loose items follow the ordinary SI-16 fall
rules. This does not refund already consumed crafting inputs, create unfinished
job outputs or itself decide construction-material salvage (settled separately
by D-30). D-42 places destruction of all
jobs and functionality at actual landing; D-44 pauses them during descent.

### D-27. Play workstation destruction once and retain the last frame

On September 17 the owner required the workstation to play its destruction
animation and remain on the final frame, rather than disappearing. On landing,
play its declared non-looping destruction clip once and retain its endpoint
as a nonfunctional wreck at the actual landing height. This follows D-26's
landing-tile scatter; D-42 places loss of jobs/functionality at that same
actual landing boundary. Neither playback nor the retained frame restores
storage, work, bills, power, or any other workstation service.

Reuse the definition's approved destruction art and the correct final frame
for each camera facing. Do not invent a replacement rubble sprite, reverse a
construction sequence, or silently disappear when a clip is missing. Missing
declarations/art are explicit external prerequisites listed in the evidence
section; coordinate with their existing owners. A clip whose endpoint cannot
depict the retained wreck needs owner-approved resolution, not a hidden fallback
or an assumption that filesystem presence proves signoff.

The remnant survives animation expiry, camera/page visibility changes,
chunk eviction and save/load. Save/restart during playback retains progress;
after playback it restores the pinned endpoint without replay or a second item
spill. SI-21 owns durable remnant identity, definition/facing references and
playback phase, with component migrations and inventory updates in its PR.
D-28 settles movement blocking and D-29 worker deconstruction; exact bounds,
support interactions and detailed salvage accounting remain under Q-4;
D-30 settles the recovery multiplier. This is
specific to fallen workstations here; D-43 extends endpoint-wreck behavior to
other furniture. It does not change ordinary demolition or D-19's intact
structural landings.

### D-28. Workstation wrecks block movement

On September 17 the owner chose solid blocking for the retained workstation
wreck, not walking over it. Route planning and swept movement must respect
its physical volume at the actual landing height. Units on another storey are
not blocked merely because they share its XY tile. Keep the collision after
destruction playback ends and across save/load, chunk eviction and camera
changes; a persistent picture with an expired collider does not satisfy this
decision. The wreck remains nonfunctional despite being physically solid.

SI-21 owns calibrated wreck bounds and collision publication with its durable
state. Do not infer dimensions from transparent sprite canvas or restore live
station services to obtain collision. The D-26 loose items remain separate
entities on the landing tile; their presence does not remove the wreck's
blocking volume. D-29 settles clearing through normal worker deconstruction;
D-41 settles recovery at 100%/50% of original materials for intact/destroyed
targets, with detailed accounting and
exact support interactions still gated under Q-4.

### D-29. Deconstruct destroyed structures and buildings at intact speed

On September 17 the owner required the normal deconstruct designation and
worker action to accept destroyed structures and destroyed buildings, using
the same speed as if the target were intact. Destruction does not make clearing
instant, faster or slower: for the same source definition, workers and work
conditions, use the same required work and rate modifiers as its intact
counterpart. Preserve source-definition identity in a remnant so it does not
lose its work contract when live building functionality is removed.

Use the same player-facing action for intact and destroyed targets. Workers
must reach a valid working position; a solid wreck is not a reason to stand
inside it. Until deconstruction completes the remnant remains present and
blocking. Committed completion removes the exact target and its collision,
invalidates affected routes/support, and cannot re-spill contents already
released under D-26 or recreate a new destruction remnant in a clearing loop.
Interrupted work and save/load retain the target and earned progress according
to the shared deconstruction contract; repeated completion cannot remove a
same-XY piece on another level or pay salvage twice.

SI-22 owns the shared intact workflow; SI-32 adds destroyed targets and proves
intact/destroyed timing parity without a separate work-rate policy.
The absolute work baseline remains an explicit Q-4 gate; D-41 fixes initial
recovery at 100% of original construction materials for intact targets and 50%
for destroyed targets, without changing work speed. This rule covers retained
destroyed structures as well as buildings, but does not require turning D-19's
initial intact structural landings into rubble or inventing new rubble art.

### D-30. Destroyed targets return 50% of normal deconstruction salvage

On September 17 the owner chose a reduced recovery amount of 50% for now.
The initial wording expressed this relative to normal intact recovery; D-41
subsequently fixes the actual baseline: 100% of original construction materials
for intact targets and 50% of those original materials for destroyed targets.
Apply the selected rate directly to that original-material basis, not an
additional reduction after a separate salvage calculation. Both values are
initial tunable balance choices. D-29's work requirement and speed remain
identical to the intact target.

The reduction concerns construction-material salvage, not stored cargo: D-26
still scatters all stored items with their quantities and nested contents
preserved. Do not apply the 50% loss twice, halve item condition as well, or
mint replacement copies of recoverable item instances. Retain the provenance
needed for salvage through functional destruction, falling, the persistent
wreck and save/load. Commit recovery once with deconstruction completion;
interruption, repeated callbacks or reload cannot reroll or duplicate a payout.

The intact recovery percentage is resolved by D-41. Before SI-22, specify
original-material provenance for player-built and authored/legacy content.
Before SI-32, specify integer
rounding for odd destroyed-target yields and deterministic selection among
distinct recoverable instances.
Those details remain Q-4 gates; no rounding rule is implied by the 50% choice.
Verify even, odd, zero and single-item cases and mixed material/condition
inventories. Initial fallen structural sections that remain intact under D-19
use intact recovery; the reduction applies to destroyed targets.

### D-31. Loose items fall with their supporting surface

On September 17 the owner confirmed that loose items on a surface fall with
it. While the surface continues supporting the item, its motion carries the
item down; it does not remain at the old height or fall through its carrier.
When the surface lands, the item remains supported on it at the new height.
Preserve item identity, quantity, nested contents and support-relative
placement through descent, landing, chunk eviction and save/load. One motion
authority composes carrier movement with any item state; do not integrate
gravity twice or create a second copy for rendering.

This differs from removing the supporting surface: if that piece is deleted
or no longer supports the item, SI-16 handles the resulting independent fall.
It also differs from D-26's spill of items stored inside a destroyed workstation;
items already resting loose on a falling floor are not scattered merely because
the floor lands. SI-26 owns moving-structure/item coupling using SI-16's item
motion boundary. D-32 separately settles units riding falling floors, including
their falling animation and full landing damage.

### D-32. Units on falling floors animate and take damage as in cliff falls

On September 17 the owner confirmed that units descend with a falling floor,
switch to the existing falling animation, and fall as if they had fallen off
a cliff, taking full damage on landing. Reuse the ordinary fall state,
interrupted-order handling, injury calculation and knockdown/death pipeline.
The floor confers no fall-damage protection. "Full" means the normal model for
the same unit and actual drop, including its existing thresholds and body/stat
effects; it is not a new flat damage amount or automatic death.

Track the unit's actual world-space fall from onset to the first real landing.
Do not use its unchanged height relative to the carrier as a zero-length fall,
count carrier displacement twice, apply a second gravity update, or fire a
landing merely because the unit still contacts its descending floor. The unit
must not fall through its valid carrier. If the supporting piece disappears
mid-fall, continue the same fall into independent motion without resetting
the drop or applying an intermediate landing injury.

Apply landing consequences exactly once. Save/restart during descent or across
the landing boundary preserves enough motion/injury state to prevent missed or
duplicate damage, premature standing or a reset to walking on the moving floor.
Equivalent cliff and carrier falls must produce equivalent injuries for equal
units and drops, including fractional-height support cases. This is required
in SI-24/SI-27 and is not deferred with SI-20's separate crushing/impact damage
to units struck by other falling objects.

### D-33. Adopt the audited slice boundaries and dependency ordering

On September 17 the owner approved applying the sizing/order audit. Retain
SI-1 through SI-22 for their narrowed outcomes and append SI-23 through SI-34
for the extracted outcomes. That audit produced 33 code slices plus the
provisional stair-art family (subsequently subdivided by D-36), with migrations/evidence kept in each introducing
PR. Prove the ruin early; publish geometry before its consumers; integrate
production picking before player intent; activate normal content only after
all initial-scope branches. Crowds and impact/crushing remain later.
This approves delivery decomposition, not unresolved gameplay proposals,
design readiness, publication, implementation or other-brand processing.

### D-34. Stair openings omit whole floor or roof tiles

On September 17 the owner selected whole-tile openings for now. Represent the
stairwell by leaving out complete floor/roof pieces, not by adding partial-tile
cutout shapes or textures. Omit as many adjacent tiles as the measured body
clearance requires; this decision does not promise that one missing tile gives
enough headroom for every flight or unit. Landings remain separate adjacent
floor pieces under D-11. A slab stays physically present until its own removal
commits; camera slicing is not a physical opening. Partial cutouts are future
scope, not missing art that this arc must silently generate.

### D-35. Include a separate PixelLab-generated damaged stair set now

On September 17 the owner required the separate damaged/weathered stair set
for authored ruins in this arc, explicitly choosing PixelLab.ai as its source.
Do not postpone it or substitute the intact stair appearance. Generate distinct
damaged direction-correct art, not merely the deterministic weathering transform
used for older Dungeon assets. D-13's personal preview/signoff requirement
applies to every new asset and frame before landing.

This is an authored visual variant, not a new rubble conversion, automatic
damage simulation, reduced structural strength or additional player build recipe.
It must remain consistent with the selected stair support/clearance geometry.
Keep the damaged identity through placement, camera rotation, ordinary teardown
and save/load; its destruction starts from its own static appearance. If a
damaged variant becomes buildable, it must also have its own construction clip
rather than inherit the intact one. Structural falls still preserve the section
and its existing appearance under D-19. This decision specifies future asset
delivery; the current session remains design-only under D-7.

### D-36. Adopt eight directional stair art deliveries and a separate code slice

On September 17 the owner approved the Q-10 subdivision. Retain SI-14 for
the intact northeast appearance; append SI-35 for stair-specific art-consumer
integration, SI-36–SI-38 for the other intact directions and SI-39–SI-42 for
the four damaged directions. Each directional appearance is one complete
asset delivery with its own issue, PR, PixelLab provenance and personal owner
signoff on every texture/frame. At this decision the plan had 34 code slices
and eight art slices, 42 total; D-45 later amends lifecycle boundaries and adds
SI-43 for 43 total.

SI-35 precedes art delivery. All eight art slices precede SI-28's extended
visual proof and therefore production integration and activation. No engine
feature is hidden in an art PR, and no incomplete family is registered as
playable production content. Q-10's bounded preview-staging contract must be
settled before SI-35 approval and before any art child is filed. This approves
the inventory and delivery boundaries, not generated art, final design
readiness, publication, issue processing or implementation under D-7.

### D-37. End workstation destruction clips in visible, retainable wreckage

On September 17 the owner approved the audit recommendation: the same declared
workstation destruction animation must end in visible wreckage suitable for
D-27/D-28's persistent, movement-blocking remnant. Falling workstations play
that clip on landing and retain its correct-facing last frame. Ordinary
demolition still expires its transient presentation after playback; a visible
endpoint does not make ordinary demolition leave a persistent collider.

Coordinate the existing Workbench #2497, Furnace #2503, Machine Shop #2504 and
Kitchen #2507 art deliveries with this requirement. Replace the empty/near-empty
endpoint requirement where it conflicts, and verify each facing's actual
endpoint against the intended wreck bounds. The Furnace's retained source
sequence needs its own suitability check and explicit approval for any change;
this decision does not certify that its current pixels are acceptable.
PixelLab supply and personal owner preview/signoff remain required through the
existing art workflow. No separate wreck sprite, additional animation role or
arbitrarily selected earlier frame replaces the declared last frame.

This resolves Q-10's endpoint design choice, not delivery or tracker state.
The later different-brand processor must coordinate the existing art contracts
through their authorized review workflow before SI-21 is approved, then verify
the art and signoff before landing SI-21. Do not create duplicate asset issues
or hide art changes inside its code PR. This session updates only this document;
no existing issue, artwork or implementation is changed, and the then-42-slice plan
is unchanged. Final design readiness remains a separate owner approval.

### D-38. Construction may seal a room without intersecting its occupants

On September 17 the owner approved completing construction that seals a unit
inside a room, provided the new solid does not physically intersect the unit.
D-24's 99% wait is a body-occupancy safeguard, not a guarantee that every
occupant retains an escape route. Do not reject designation or delay completion
solely because a valid final wall removes the room's last exit.

Ordinary placement, support, clearance and reachable worker-approach rules
still apply. A unit intersecting the final solid volume still blocks completion;
a unit elsewhere inside the enclosed room does not. Apply that distinction to
the worker as well as other occupants. Construction does not teleport units,
clear player holds or grant collision exemptions to escape a newly sealed room.
Q-5's old-overlap recovery remains for actual overlaps, not legal enclosure.
No global escape-route search is required for construction admission/completion.

SI-15/SI-31 must test sealing the final exit with a non-intersecting occupant,
contrasted with the same wall intersecting a body and waiting at 99%. Both
outcomes retain their correct geometry/progress across save/load. This resolves
Q-4's enclosure policy without changing the then-42-slice plan or authorizing code,
tracker processing or publication in this document-only session.

### D-39. Temporarily pause falling objects at unit contact until the space clears

On September 17 the owner approved the simple interim rule: a falling structure
or placed object stops at first contact with a unit underneath, without passing
through, injuring, teleporting or pushing that unit. Resume descent when the
obstruction clears. A connected falling section pauses as a whole, preserving
its relative piece positions. If the unit cannot leave or stays under a player
hold, the object may remain suspended indefinitely; no timeout forces a release.
This does not veto removal of occupied supports or make units structural anchors.

This is temporary falling-object contact handling pending SI-20's later
impact/crushing system, not deferral of movement collision, wall clearance or
contact detection. SI-17/SI-19 and their workstation/carrier consumers deliver
the interim response before activation. SI-20 must replace it with its agreed
contact/damage behavior, including restoration of already paused falls; do not
leave both responses active or preserve a permanent no-impact exception.

The pause is not a confirmed landing: it does not reattach the structure,
publish a workstation's landing spill or destruction playback, or restore
workstation functionality. Carried items and units stay associated with their
support; a carried unit on top is not itself the underneath obstruction.
Preserve D-32's accumulated world-space fall and required self-fall injury,
without an early or duplicate landing event merely because descent paused.
Actual support loss still hands occupants to their ordinary fall paths.

Use bounded contact/recheck work and preserve the paused position, pending
payloads, carrier associations and fall accounting across save/load, eviction
and visibility changes in each introducing owner. Revalidate live occupancy
before resuming; stale blocker identities must not freeze the object after the
unit leaves. Waiting does not bank a catch-up displacement that tunnels through
the next obstruction. Numeric motion/resumption details remain engineering
gates under Q-4/Q-11, not permission to reset injury history or overlap bodies.

This resolves the interim unit-contact policy in Q-4/Q-11. No loose-item
collision/damage model, earlier crushing damage, implementation or tracker
change is authorized; at this decision the document-only plan retained 42 slices.

### D-40. Structural groups fall vertically until their first supported contact

On September 17 the owner specified no tipping or rotating: a detached connected
section falls straight down until a piece hits the ground. Preserve every
piece's horizontal position, authored orientation and relative offset through
one common vertical displacement. Do not add tumbling, lateral sliding or
per-piece settling to make the group fit uneven terrain.

The first valid load-bearing contact of any member stops and lands the whole
group at that height. Interpret ground consistently with the earlier support
rules: terrain or an already supported floor, roof or structure may catch it;
do not pass through an upper support to reach terrain below. The other connected
pieces may remain overhanging without touching anything beneath them. No
centre-of-mass, balance, full-footprint or multiple-contact requirement is added
for the connected structural section. The section remains intact and usable
under D-19, with its newly supported geometry published coherently.

Find the earliest valid contact along the shared swept descent, not whichever
piece is visited first or where a large time step ends. Stop without sinking
other members into terrain or existing solids. A unit underneath instead
invokes D-39's temporary contact wait; it is not a load-bearing landing or a
terrain anchor. Unknown/unloaded geometry is neither a confirmed landing nor
permission to continue through a possible support.

Recompute support through the existing structural graph when landing commits
and when the catching support is later removed; landing is not permanent
anchoring. Precise attachment tolerances, in-flight edits/splitting and contacts
between separate moving groups remain Q-11 gates. This decision does not set
the support footprint for independent furniture. SI-19 owns the group rule;
SI-26/SI-27 preserve carriers through the common stop and actual landing.
At this decision the design remained document-only with 42 slices; no implementation, artwork,
tracker changes or final readiness approval is implied.

### D-41. One deconstruction action returns 100% intact and 50% destroyed materials

On September 17 the owner clarified the baseline: deconstructing an intact
structure or building returns 100% of its original construction materials;
deconstructing a destroyed target returns 50% of its original construction
materials. These are initial balance values, expressly subject to future
change, not immutable gameplay constants. Both apply through the same
Deconstruct action and D-29's equal work/time contract; no separate salvage
action or extra wreck penalty is introduced.

The original construction-material basis is shared by both rates. Do not
interpret 50% as half of a separately discounted intact payout, reduce material
condition as a second penalty, or apply it to stored cargo. D-26's full storage
spill remains separate. A structural group that lands intact under D-19/D-40
uses the intact rate, not the destroyed rate merely because it fell.

SI-22 owns intact recovery and the common provenance/accounting contract;
SI-32 applies the destroyed rate to that same basis. Record these as named,
tunable recovery values in the owning content/balance contract; the exact
configuration mechanism is an implementation detail, not a new player setting.
Preserve the necessary original-material provenance through destruction,
falling, wreck persistence and save/load. Define the authored/legacy-content
basis explicitly before SI-22 approval rather than silently using unrelated
cargo or a changed recipe as evidence of original material quantities.

This decision resolves Q-4's intact recovery percentage and D-30's previously
relative baseline. Integer rounding, per-material versus pooled selection and
selection among distinct item instances remain bounded SI-22/SI-32 approval
gates. The previously suggested round-down-per-material rule was not approved
by this clarification. Keep payouts exact-once and stable across reload;
no rounding or selection policy may be silently inferred from the percentages.
This decision retained 42 slices; D-45 later adds SI-43. The session stays document-only.

### D-42. Workstations and furniture become destroyed when falling ends

The owner corrected the earlier workstation rule: destruction happens when
falling ends, not when it starts. D-43 narrows the initial broad mention of
all structures/constructions to workstations and other furniture; D-19's intact
structural landing remains in force. D-25's irreversible fall-onset destruction
and D-15's original intact-furniture landing are superseded.

A D-39 temporary wait against a unit is not landing and must not trigger
destruction. At actual landing, permanently tear down workstation jobs/services,
scatter cargo once under D-26 and play destruction under D-27, retaining the
solid wreck. D-41's destroyed deconstruction rate applies after this transition.
Loose items and D-32's full unit fall injury retain their separate rules.

D-44 resolves airborne service/job behavior: pause during descent, then discard
jobs permanently at landing destruction. Preserve paused job state, identity,
cargo and original-material provenance through descent; do not silently perform
onset destruction.

### D-43. Furniture leaves landing wrecks; structural pieces remain unchanged for now

The owner confirmed furniture should use destruction playback and a retained
final-frame wreck, but walls, floors and stairs should stay the same for now.
Workstations and other furniture become destroyed at actual landing, play their
approved destruction animation once and retain its final frame as a solid,
nonfunctional, deconstructable remnant. Use the same deconstruction work as an
intact counterpart and D-41's initial 50% original-material recovery.

Structural groups instead retain their existing appearance and intact usable
geometry after D-40's upright, whole-group first-contact landing. D-19 remains
authoritative for structural pieces, including walls, floors, roofs, posts and
stairs. A previously authored damaged variant keeps that variant; landing does
not cause an additional destruction or rubble conversion. Intact structural
targets use D-41's 100% recovery. Loose items remain on their landed supporting
surface, and carried units still take full cliff-equivalent fall injury.

This removes the structural remnant/art/support expansion introduced by the
initial D-42 interpretation. It does not add unfinished-site falling/destruction
or change D-23's nonphysical construction stages. Structural rubble is future
work; the eight stair art scopes remain unchanged.

Before readiness, inventory non-workstation furniture destruction clips,
visible endpoints, facings and physical bounds under Q-10; select any missing
asset supply and obtain personal owner signoff during its own art delivery.
The explicit furniture choice does not prove those assets already exist.
D-45 applies the approved ownership split across SI-17/SI-21/SI-25/SI-43/
SI-26/SI-32, without absorbing furniture adapters into a workstation-only PR.
D-46 resolves the four non-workstation definitions and their cargo/service
policy; D-44 resolves airborne workstation services.
D-45 preserves the original 42 IDs and adds SI-43 after that review.

At D-43 this session remained document-only and exploring; D-47 later approves
readiness. No generation, implementation,
tracker changes or publication is authorized; later processing stays with a
different agent brand.

### D-44. Pause workstation jobs while falling; discard them on destruction

The owner approved pausing workstation jobs and services during descent, then
discarding jobs when the building becomes destroyed. Under D-42 that destruction
occurs at actual landing, not at fall onset or a D-39 temporary contact wait.

At fall admission, suspend job progress and functional services, including
production and power participation. Retain queued/active job state as paused
until destruction; no airborne crafting progress, completion output or service
effects are permitted. This applies equally to independent falls and stations
carried by a falling structural group. A contact wait does not resume work.

At landing, permanently discard jobs/bills/claims and functional participation
as part of the same destruction transition that publishes the cargo spill and
starts destruction playback. Do not complete paused jobs, refund already
consumed inputs or create unfinished outputs. The retained wreck is never a
working station. Existing D-26 cargo accounting remains separate.

Persist the distinction between paused-in-flight and destroyed-on-landing.
Save/load, eviction, carrier loss and contact waits must not resume work,
prematurely discard paused jobs, duplicate cargo/output or resurrect destroyed
jobs/services. Reject stale worker execution while falling as well as after
destruction; runtime worker/claim detachment must not erase the paused logical
job queue early. SI-25 owns this lifecycle and its component migration;
SI-26 reuses it for carried workstations.

This resolves the airborne job/service portion of Q-4/Q-11. D-44 itself adds
no slice; D-45 subsequently adopts the 43-entry plan and D-47 approves final
design readiness. Art and bounded implementation gates remain outstanding.
The session remains document-only.

### D-45. Adopt shared fall and wreck owners with a separate furniture lifecycle slice

The owner approved the post-D-44 slice-audit recommendation. SI-17 owns shared
placed-building fall motion only; SI-21 owns common persistent wreck playback,
geometry and identity; SI-25 owns workstation pause and landing teardown/spill.
Append SI-43 for non-workstation gameplay adapters reusing those boundaries.
Do not duplicate wreck schemas, cargo owners or destruction transactions.

Order the affected chain SI-17 → SI-21 → SI-25 → SI-43 → SI-26.
SI-26 depends on both lifecycle adapters and shared wrecks; SI-32 consumes their
common destroyed-target identity, and SI-7 stays downstream of the entire chain.
Keep SI-19/SI-27 unchanged in scope: structural pieces land intact. All existing
IDs survive; the plan now contains 35 code slices and eight art slices, 43 total.

Reuse existing building-art owners and reconcile their visible-endpoint contracts;
this does not approve artwork or authorize tracker edits. Approval is for the
delivery split, not by itself non-workstation content admission, cargo spill,
power/spawn policies, salvage provenance or final design readiness. D-46 later
settles the content and service/cargo choices. Q-4/Q-10
retain named stop/ask gates for SI-43 and its consumers. If adapter work exposes
substantial missing infrastructure, re-split before that child is approved.
Each code PR includes its migrations, tests, evidence and contract documentation.

At D-45 the document remained exploring; D-47 later approves readiness.
It stays local. Different-brand processing remains
required; this approval does not authorize generation, implementation or publication.

### D-46. Apply falling service suspension and landing wrecks to all four non-workstation buildings

The owner approved the same policy for Cargo Hold (`cargo_hold_S`), Solar Panel
(`solar_panel`), High-Voltage Battery (`high_voltage_battery`) and Acolyte Portal
(`acolyte_portal`). All four belong to SI-43's placed-building lifecycle; the
Portal is not exempt because its visual class is a gateway.

At fall onset suspend their services without destroying them early. Throughout
descent and D-39 contact waits, storage cannot accept deposits or withdrawals,
solar cannot supply power, batteries cannot charge/discharge or participate in
the network, and portals cannot spawn units. Preserve owned cargo and suspended
state until the landing transition. Apply this to independent falls and objects
carried by a falling structural group; carrier loss does not resume services.

On actual landing, permanently retire functionality, scatter any stored item
contents in full onto the actual landing tile/support using D-26's identity-
preserving exact-once rule, and admit one shared SI-21 wreck. Play declared
destruction once and retain the visible final frame as solid, nonfunctional
and deconstructable. Do not halve stored cargo: D-41's 50% rule applies only
to original construction-material recovery during later deconstruction.
Stored electrical charge and unspawned portal units are not stored item cargo;
this decision creates no charge-to-item salvage or catch-up spawns.

Save/load, eviction and temporary contact waits must preserve suspension before
landing and permanent nonfunctionality afterward. Test stale service requests,
power participation, portal timers and storage transfers directly, not only
through UI availability. No duplicated spill, re-enabled wreck, replayed spawn
or repeated destruction is allowed. SI-43 implements these adapters; SI-26
reuses them for carriers, and SI-32 clears their common wreck identities.

This resolves the content-admission and service/cargo policy portions of Q-4
and the post-D-44 audit. Precise contact/scatter anchors, salvage provenance
and rounding, art-contract coordination and actual owner-approved artwork
remain their existing named gates. The approved 43-slice graph is unchanged;
this is not final readiness, implementation or publication approval.

### D-47. Approve final design readiness with the named child gates retained

On September 17 the owner explicitly approved marking this document
`ready for issue processing` after the final readiness audit. The accepted plan
contains 43 dependency-ordered slices: 35 code and eight stair-art deliveries.
All prior decisions and stable IDs remain; no gameplay policy changes here.

The owner accepts retaining the audit's explicit child-level stop/ask gates:
geometry/contact/reach, fall/residency/recovery, deconstruction provenance and
rounding, rendering proofs and performance budgets, art staging/coordination
and personal signoff, and later crowd/impact details. This accepts the gates,
not their unresolved proposed defaults. An affected child must resolve its
questions before approval, and missing art or evidence still blocks delivery.
Material changes to scope or architecture return this design to exploring.

The document-only foreground task is complete with a local, uncommitted
`docs-wip` deliverable. No tracker artifact was created and nothing was published.
A different agent brand must perform later `process-design-doc` work: the epic
first, then exactly one child per invocation, with separate approval for every
tracker artifact. This session does not perform that processing.

## Open questions

### Q-1. Should structures affect movement? Resolved by D-1

Yes: physical interaction and rendering belong in the same feature arc.

### Q-2. How should units resolve congestion? Policies resolved by D-20/D-21

D-20 selects friendly wait/yield with physical clearance, not squeezing through
one another. Before SI-5, specify deterministic priority and bounded retry/
replanning behavior, preserving position holds and the existing eligible-time
stall budget. D-21 selects solid occupancy without cooperative yielding to
hostiles: go around, engage under existing combat rules, or report a blocked
route. Only the bounded implementation details remain open; do not reopen
these friendly/enemy behavior choices during processing.

### Q-3. Are stacked building floors part of the first release? Resolved by D-5

Yes. The owner rejected postponing multiple walkable floors at the same
`(x,y)`. Continuous elevation and a route up slopes or stairs onto another
floor are game-critical acceptance requirements. The earlier single-floor
proposal is superseded, including its assumption that existing storage and
XY-only routes can remain unchanged.

### Q-4. Which content blocks, supports, or permits traversal?

**SI-43 content/service policy: resolved by D-46.** Cargo Hold, Solar Panel,
Battery and Portal suspend services while falling, become nonfunctional wrecks
on landing and scatter stored items in full. Contact/scatter geometry and
original-material provenance remain bounded implementation gates, not permission
to reopen that policy.

The core lifecycle choices are recorded in D-4–D-6, D-14–D-19 and D-22–D-30,
with D-42 replacing workstation onset destruction, D-43 retaining intact structural landing,
and D-44 resolving airborne jobs/services as paused until landing destruction;
the lifecycle table above is the consolidated contract. Some edge cases still
need player-visible policy choices, not just numeric calibration. Remaining gates are:

- **Geometry and reach:** Calibrate physical dimensions/conversions, body
  radius values, step/headroom limits, roof bounds, interaction sides and
  large-item classification against the ruin and acolyte (SI-1/SI-2/SI-3/SI-9/SI-30).
  Existing pixels alone do not establish contact dimensions.
- **Construction:** Specify attachment geometry, reachable worker approaches,
  placement legality and bounded completion rechecks (SI-15/SI-31).
  Enclosure is resolved by D-38: sealing a non-intersecting occupant inside is
  allowed. Completed-only activation and the occupied 99% wait remain decided.
- **Falling contact:** D-39 resolves the interim response to units below:
  pause at contact and resume when clear. Set multi-tile support admission,
  descent clearance, numeric pause/resumption details and bounded landing/scatter anchors
  (SI-17/SI-25/SI-43/SI-19/SI-26/SI-27). Distinguish genuinely absent lower support from unavailable
  geometry for every fall owner and lifecycle reconciliation (SI-24/SI-16/SI-33).
  Neither is permission to veto occupied-floor removal.
- **Wrecks:** Calibrate solid bounds and support-loss behavior (SI-21); verify
  the approved declared clips and final frames through Q-10. Blocking,
  persistent final-frame appearance and deconstruction are already settled.
- **Deconstruction:** D-41 resolves initial recovery: 100% of original materials
  intact and 50% destroyed, both tunable later. Establish the shared work
  baseline, authored/legacy provenance, integer rounding and deterministic
  material selection (SI-22/SI-32). Preserve recoverable-material provenance
  through destruction (SI-25/SI-43/SI-21). Equal intact/destroyed work remains decided.

Q-11 owns structural connections and carrier mechanics; Q-12 owns later
impact/crushing damage. Additional required art must pass the supply/signoff
gate, not become an undocumented placeholder or scope reduction.

### Q-5. How are pre-existing overlaps repaired? Load stance resolved by D-3

Old saves can legally contain units inside future colliders. Choose and test
a bounded recovery/load policy, including the no-nearby-valid-position case.
New placements must obey D-23/D-24's actual-body intersection rules; D-38
permits enclosing a unit without intersecting it, so this is not an escape-route
requirement. Any persisted
schema change requires component migrations, not only a global version bump.

Proposed policy, in order: a restored unit inside a solid keeps its saved
position and receives the transient per-solid exemption, so it may leave the
solids it starts inside and no others; a unit that cannot leave under that
rule (several overlapping solids, or no free surface adjacent to any of them)
takes a bounded same-page nudge to the nearest legal resting position that
does not cross an intact barrier; a unit with no such position remains
embedded and exempt, is reported in the load log, and never blocks the load.
Rejecting the load is ruled out by D-3. None of the three steps is a
guarantee that every unit recovers; the fixtures in SI-33 cover each outcome
explicitly. Still open here: the nudge's search bound and what the load log
entry carries.

### Q-6. Which occlusion backend passes the reference scenes?

SI-6 proves the original ruin case; SI-28 must extend the verdict to the
stacked/stair/manual-slice matrix before SI-34 integrates production consumers.
SI-7 then verifies every dynamic producer and the full gameplay matrix. The
shared geometry model remains a proposal; no sorting or depth-buffer backend
is already proven or approved by this design.

### Q-7. What performance limits define acceptance?

Measure the proposed fixture sizes on supported hardware and agree simulation,
occlusion, frame-time, rebuild-spike, and memory limits before SI-7 is drafted.
SI-6/SI-28 record candidate measurements; agree SI-34's integration measurement
limits before approval, and SI-5's crowd-work budget separately. These
are deliberate evidence gates, not permission to accept an unmeasured backend.

### Q-8. What is the precise continuous walk and authoring convention?

D-6 settles continuous physical height, including stairs. D-10 settles authored
anchors: fractional in the general engine/Lua path, whole-z placement policy in
player tools. These do not select a numerical type or animation technique.
Proposed remaining defaults: a continuous stair walk envelope, walking speed
measured along the physical surface in common world units, and ordinary walk
animation subject to visual signoff. Before SI-1/SI-11, approve the contact
convention; before SI-2/SI-3, settle the distance metric and numeric tolerances
from measured geometry. Choosing a finite representation is engineering work
within D-10; a material quantization limit must return to the owner. Body
clearance uses the documented unit conversion; physics metres per z and climb
reach are not interchangeable.

### Q-9. How does the player expose and select the intended floor? Resolved by D-9

Use only the camera's manually selected z level. Automatic cutaway and floor
following are outside this arc. Include continuous slopes crossing a slice and
overlapping targets on different floors in the acceptance matrix. Technical
slice membership and clipping must be shared by rendering and picking; current
integer `uiGridZ` culling is not proof of correct continuous-height behavior.

### Q-10. Who authors the levels, and where does the missing stair art come from?

**Furniture coverage reopened by D-42/D-43:** non-workstation furniture now
needs approved destruction playback ending in a visible, solid retained wreck.
Inventory exact definitions, facings, endpoints and masks against existing
art ownership before deciding whether additional art slices are needed.
Stop for missing assets, supply decisions or personal owner signoff.
Structural landing-wreck art is not required: D-43 retains intact structural
landings and the existing stair scopes. No generation is authorized here.

The post-D-44 furniture lifecycle audit below completes the shipped-roster
inventory: #2498/#2509/#2511/#2501 own Cargo Hold, Solar Panel, Battery and
Portal destruction art. All four currently require empty/near-empty endpoints;
reconcile those contracts with visible wrecks for every admitted definition.
D-45 approves the shared wreck owner and separate SI-43 non-workstation adapter;
D-46 now confirms all four definitions and their service/cargo policy. Actual
art delivery, visible-endpoint coordination and personal signoff remain gated.

**Workstation endpoint choice: resolved by D-37.** #2497/#2504/#2507 were
verified to call for empty or near-empty destruction endpoints. The approved
resolution is to coordinate those existing art deliveries so each workstation's
same destruction clip ends in owner-approved visible wreckage, while ordinary
demolition still expires its transient effect. A declared clip alone does not
prove a usable wreck. Inspect the Furnace's retained endpoint separately rather
than asserting the same textual conflict in #2503. External contract updates,
delivered art and personal signoff remain prerequisites; this document does not
amend those issues. Separate wreck clips/sprites and alternative pinned frames
are not the selected solution.

Construction scope is resolved by D-8: both player-built upper floors/stairs
and authored locations are required now. SI-15/SI-31 are required slices. D-13
resolves art supply: PixelLab generation with the owner's personal signoff.
The September 17 inventory below identifies the required roles and D-36's
approved asset boundaries. D-34 settles whole-tile openings and D-35 requires
the separate damaged set. Inventory and subdivision are settled; the bounded
preview-staging contract remains a gate before SI-35 approval and before art
children are filed. Scope approval is not approval of artwork not yet generated.

No named staircase asset family or stair schema was found in the September 16
inventory. D-11 settles the shape: repeatable straight flights without attached
landings. D-12 settles one tile of run and one z of rise. SI-11 defines their
orientation mapping, width/clearance, connections to separate adjacent floors,
and openings; SI-14/SI-36–SI-42 need matching visible stair textures and required
facemaps/lifecycle frames, following D-13's chosen production workflow.
Inspect existing floor/ceiling assets for reuse. D-34 excludes partial cutouts;
any necessary new connector appearances must still be enumerated explicitly.
Each independently required directional asset has its own slice and later
issue/PR and owner signoff under D-36. No placeholder stair completes the
arc, and no art is generated as part of this design-only session.

The earlier built-in-landing proposal is rejected by D-11. Art and geometry
must tile along successive horizontal/vertical offsets to make a long straight
flight. Ordinary adjacent floor pieces supply landings and turning spaces;
do not invent a separate landing asset unless the existing floor art fails
the approved geometry. Calibrate width, headroom, and floor openings against
unit dimensions within D-12's decided run/rise before pinning asset canvases
or frame counts. If that geometry cannot admit a required unit, report the
specific clearance conflict to the owner rather than changing the dimensions.

#### Stair asset inventory — September 17

Verified at `064a255f06b59e2b1f8e881dcf4c747d52967a36` by inspecting the
pack/schema/catalogue and opening the actual floor, ceiling, wall and slab
facemap PNGs. This is source inspection, not an in-game alignment proof or
owner art signoff. No assets were generated or modified.

- `data/structure_packs/dungeon_1.yaml` declares floor, ceiling, post and four
  wall-edge appearances, with six damaged overrides; it declares no stair kind
  or lifecycle frame lists. A tracked-path search found no stair/ramp assets.
  `StructureSlot`, `AppearanceSlot` and the Lua piece-kind lists also lack stairs.
- Existing `floor.png`, `ceiling.png`, `floorface.png` and `ceilingface.png`
  are 96×64 full slabs/masks, not cutout pieces. They are candidates for ordinary
  adjacent landing floors and uncut upper floors/roofs. Floor and ceiling pixel
  anchors differ in `Structure.Render`; same canvas size does not prove matching
  support height, rim placement or underside clearance.
- `docs/structure_pack_schema.md` and the directional-art design require each
  buildable appearance's construction/static/destruction roles. Construction
  and destruction use the static facemap's lighting RGB with frame-owned alpha;
  per-frame facemaps are not required. Facemaps encode face lighting, not surface
  material artwork, collision or a GPU occlusion depth map.
- Live issue bodies/statuses checked September 17: [#2513](https://github.com/coghex/synarchy/issues/2513)
  is OPEN and owns the existing Dungeon lifecycle sets, explicitly excluding
  stairs/new kinds; [#2495](https://github.com/coghex/synarchy/issues/2495) is OPEN
  and owns the pack preview, also excluding new kinds; [#2491](https://github.com/coghex/synarchy/issues/2491)
  is OPEN and owns immediate structure teardown/transient destruction playback;
  [#2519](https://github.com/coghex/synarchy/issues/2519) is OPEN and owns the
  completeness audit, explicitly excluding new kinds. These are external work
  to reuse, not completed capabilities or new issues to duplicate. A narrow
  open-title search found no stair issue; final child deduplication remains later.

**Required variant scope:** a repeatable straight stair design matching the
Dungeon art, plus its separate damaged/weathered set (D-35), each with four
direction-correct appearances. Direction means the
projected rise direction after composing world orientation and camera facing;
four placements × four camera facings need a tested 16-case mapping, not an
automatic inventory of sixteen separately authored appearances. Names below
are logical screen directions, not an approved schema or file-path spelling.

| Missing directional appearance | Required static assets | Required lifecycle assets | Approved delivery slice |
|---|---|---|---|
| Stair rising screen northeast | One colour/alpha sprite and matching static facemap | One construction clip and one forward destruction clip | SI-14 — own issue/PR/signoff |
| Stair rising screen northwest | One colour/alpha sprite and matching static facemap | One construction clip and one forward destruction clip | SI-36 — own issue/PR/signoff |
| Stair rising screen southeast | One colour/alpha sprite and matching static facemap | One construction clip and one forward destruction clip | SI-37 — own issue/PR/signoff |
| Stair rising screen southwest | One colour/alpha sprite and matching static facemap | One construction clip and one forward destruction clip | SI-38 — own issue/PR/signoff |
| Damaged stair rising screen northeast | Separate PixelLab colour/alpha sprite; matching facemap binding with reuse only after verification | Its own forward destruction clip starting from the damaged static | SI-39 — own issue/PR/signoff |
| Damaged stair rising screen northwest | Separate PixelLab colour/alpha sprite; matching facemap binding with reuse only after verification | Its own forward destruction clip starting from the damaged static | SI-40 — own issue/PR/signoff |
| Damaged stair rising screen southeast | Separate PixelLab colour/alpha sprite; matching facemap binding with reuse only after verification | Its own forward destruction clip starting from the damaged static | SI-41 — own issue/PR/signoff |
| Damaged stair rising screen southwest | Separate PixelLab colour/alpha sprite; matching facemap binding with reuse only after verification | Its own forward destruction clip starting from the damaged static | SI-42 — own issue/PR/signoff |

D-36 approves **eight art deliveries: eight static sprites and twelve animation
clips** (four intact construction, four intact destruction and four damaged
destruction). The damaged set serves authored ruins; a new player-buildable
damaged variant is not implied. If one is selected later, add its four own
construction clips explicitly. All eight statics need correct facemap bindings:
four new intact masks, with damaged masks either separately authored or reused
only after silhouette/lighting verification. This is not a demand for eight new
mask files regardless of geometry, nor permission to clip damaged pixels with
an unsuitable intact mask. There are no per-animation-frame facemaps.

This does not fix the PNG/frame count: choose construction and destruction
lengths at authoring, with equal counts across each variant's four directions
for a lifecycle. Each appearance bundle keeps
its mask and pixel-identical lifecycle handoffs with its static source; every
texture/frame still needs the owner's approval. Do not substitute a mirrored
or rotated bitmap for an uninspected directional appearance.

Construction ends pixel-identical to that appearance's static sprite;
destruction starts pixel-identical and is authored forward, not construction
played backward, a fade or mechanical erase. Match the existing flat/unshaded
material and binary-alpha conventions; let the facemap supply face lighting.
PixelLab is the selected artwork source for both intact and damaged sprites
and their authored clips; the older deterministic Dungeon weathering tool is
not a substitute for D-35's chosen source. The matching technical masks
must encode the approved geometry exactly and be inspected alongside the art,
not be treated as arbitrary generated colour textures. Their concrete authoring
method is a delivery detail, not authorization to generate files now.

The enumerated stair clips support ordinary piece teardown. D-19/D-43 keep
fallen stairs intact, including their authored damaged appearance; no separate
stair landing-wreck or rubble art is required. Existing unit walking/falling
animations remain subject to the planned visual proof; this inventory does
not silently add new unit animation assets.

#### Reuse, opening and variant gates

| Surface or variant | Inventory result and required decision/evidence |
|---|---|
| Separate landings/turns | Reuse an ordinary adjacent floor piece, subject to measured endpoint/foot contact. No built-in landing, special landing sprite or hidden connector. |
| Uncut upper floors and walkable roofs | Reuse existing floor/ceiling appearances and their separately owned lifecycle deliveries. Verify top/underside and all camera facings; do not infer a cutout from a full slab sprite. |
| Stair opening | Resolved by D-34: omit whole floor/roof tiles, potentially several adjacent cells for headroom. No partial-cutout asset or new landing piece is required by this decision; geometry must still prove clearance. Camera slicing does not remove a physical slab. |
| Stair ends and repeated joins | The same flight must tile for three successive one-tile/one-z rises with separate floor connections and no inserted landing. Any proven need for extra cap/connector appearances expands the inventory before generation/signoff; do not copy the wall's four cap-mask variants by assumption. |
| Damaged/ruin stair appearance | Required now by D-35, using PixelLab: four distinct damaged statics, verified facemap bindings and four own forward destruction clips. Construction clips are additionally required if the variant becomes buildable. Never borrow the intact variant's lifecycle or silently substitute intact art in an authored ruin. |

The existing Dungeon floor/wall/post construction/destruction art remains owned
by #2513; the narrower issue explicitly gives non-buildable damaged appearances
destruction only. Reuse that approved delivery scope rather than broadening it
from older design prose. Existing workstation destruction art remains under
#2497/#2503/#2504/#2507 as already recorded; stair art does not replace it or
satisfy D-37's endpoint coordination and personal-signoff gate.

Do not fix the stair canvas to 96×64 merely because the current slabs use it.
SI-1/SI-11 must settle the one-tile/one-z envelope, thickness, headroom, pixel
anchor and clipping margins first. The visible underside must match the chosen
solid volume: a filled masonry wedge cannot visually promise an open passage,
and a thin flight cannot hide an unmodelled solid block. Any conflict with a
required unit's clearance returns to the owner without changing D-12 implicitly.

#### Art-consumer integration discovered by the inventory

SI-11 owns physical connectors, not the new stair appearance throughout the
catalogue, both pack readers, direction mapping, construction/destruction
playback, preview and completeness audit. The existing preview/audit issues
explicitly exclude this new kind. A generic folder of PNGs is not proof that
those consumers understand the stair appearance.

**Approved additional code slice SI-35 (D-36):** after SI-11, extend the existing
structure art contract and its consumers to the stair kind, including the
16 orientation/camera cases for each variant, lifecycle handoffs, preview
inspection and audit coverage. Reuse #2491/#2495/#2519 as external prerequisites
where applicable; do not reimplement their general facilities or assume them
landed. Its introducing PR owns compatibility and documentation updates, with
fixture evidence but no substitute production art. It must work independently
of SI-28/SI-34's new occlusion backend, avoiding an art/visual-proof cycle.

D-36 retains SI-14 for intact northeast; SI-36–SI-38 cover intact northwest,
southeast and southwest; SI-39–SI-42 cover the corresponding four damaged
appearances. With SI-35 this originally gave 34 code plus eight art slices;
D-45's SI-43 brings the current total to **35 code plus eight art slices (43)**.
Partial cutouts remain excluded by D-34.

Production pack registration must not point at missing or unsigned art while
the individual deliveries are staged. Before SI-35 approval and before filing
art children, specify the manifest/registration mechanism by which each
complete appearance is inspected in the real preview without pretending
an incomplete four-direction family is playable or bypassing completeness
guards. SI-35 owns that mechanism and its regression coverage. Each art PR
retains PixelLab provenance, a production build, the real preview verdict and
evidence for its own appearance and clips; it need not pretend the other seven
assets already exist. Before generation begins, agree the shared canvas/anchor,
material reference and per-lifecycle frame-count profile; keep equal counts
across a variant's directions. Production completeness remains mandatory.
Full repeated-flight, adjacent-floor,
manual-slice and in-world lighting acceptance remains required in SI-28/SI-34
and SI-7; preview inspection is not a replacement for that geometry proof.
Include intact, damaged and mixed-variant repeated flights in that evidence.
An authored ruined stair retains its selected damaged appearance after reload
and an intact group landing; no camera facing silently switches to intact art.

### Q-11. How do structural connections and falling groups interact?

D-43 reaffirms intact structural landing under D-19. Carrier handoff uses
that retained geometry, not newly invented structural wrecks.

Support and grouping are resolved by D-16/D-17: one horizontal connection can
support an overhang through a terrain-anchored structure, and a detached group
falls together. Before SI-18, specify actual attachment surfaces for each piece
kind and the bounded graph/residency protocol, including ordinary chunk seams,
the cylindrical seam, fractional placements, and supporting terrain edits.
These are implementations of the selected rule, not a new strength simulation.

D-19/D-43 retain intact usable structural pieces at landing; rubble remains
future work. D-31 settles loose-item carriers: an item rides its
supporting surface down and remains on it at landing, unless that support is
actually removed or lost. D-32 makes units ride the falling floor in their
falling animation and take full ordinary cliff-fall consequences on landing.
D-40 settles vertical-only group motion and landing the whole section at its
first valid load-bearing contact. Before SI-19, specify precise contact geometry,
reattachment mechanics, in-flight splitting, moving-group contacts and the
shared motion/persistence boundary. Before SI-26/SI-27, specify object/unit
carrier contact and handoff details, including non-workstation furniture.
No carried entity
receives duplicate gravity or passes through its valid support. D-25's former
onset-destruction rule is superseded: D-42 moves that transition to landing.
D-44 resolves airborne jobs/services as paused until destruction at landing;
the detailed landing carrier handoff remains an explicit gate.
The upright vertical translation is decided by D-40, retaining relative offsets
and piece orientation; tipping, rotation and lateral settling are excluded.
Keep unknown geometry distinct from an unsupported component, and retain
mid-fall state across save/load and eviction. D-18 defers impact/crushing damage
to SI-20; D-39 provides the initial pause/resume response to units below.
Remaining group/terrain/object contacts and exact cargo scatter anchors are still
gated; D-46 already settles the full spill policy. General fracture/rubble
simulation is excluded; settle the concrete
motion/persistence contract before delivery.

### Q-12. What are the later impact/crushing damage rules?

Scope and timing are resolved by D-18: falling furniture/structures should hurt
units they hit, but the mechanic belongs to SI-20 after initial falling works.
Before processing SI-20, settle damage scaling/thresholds, one-shot impact
versus sustained crushing, repeated-contact rules, and how injury/death and
contact resolution interact. Replace D-39's interim pause response coherently,
including saved objects already waiting at contact; do not run both policies
or apply catch-up/replayed damage. Reuse the existing injury/death authorities;
camera visibility must not affect damage, and save/load must not replay an
already-applied impact. Any new durable impact state needs classification and
component migration in that same PR. Do not expand this to object destruction
or loose-item damage without another scope decision.

### Deliberately open choices and processing gates

The design can be ready for issue processing while these bounded questions
remain open. Processing the umbrella records them; processing an affected
child stops before presenting its issue for approval until its question is
resolved. Proposed defaults elsewhere in this document are not owner decisions.

| Question | Affected slice and stop/ask behavior |
|---|---|
| Q-2: crowd policy | D-20/D-21 resolve friendly wait/yield and hostile solid blocking without cooperative yielding. Before SI-5, specify deterministic priority/retry behavior; preserve position holds, combat authority and eligible-time stall rules. |
| Q-3: stacked floors | Resolved by D-5. SI-10 owns stacked storage/migration; SI-11 owns explicit connections; all subsequent consumers retain support identity. |
| Q-4: physical content | Resolve the five scoped gates above before their named children are approved: geometry/reach, construction, falling contact, wrecks and deconstruction. Missing-support/data behavior must be settled before activation. Do not reopen the recorded lifecycle decisions. |
| Q-5: recovery details | Before SI-33, specify the bounded nudge search, allowed escape from initial overlaps, exemption lifecycle, and log fields, preserving D-3's never-reject stance. |
| Q-6: rendering backend | SI-6 proves the ruin; SI-28 proves the extended matrix before SI-34. Stop and revise the backend/sizing if either proof fails. SI-7 still requires full dynamic evidence. New required artwork triggers the supply/signoff gate. |
| Q-7: performance | Agree bounded measurements before SI-34; stop before SI-7/SI-5 until their numerical production/crowd budgets and protocol are agreed from baseline evidence. |
| Q-8: continuous contact and authoring | D-10 resolves fractional anchors and whole-z player policy. Resolve contact semantics before SI-1/SI-11 and speed/tolerance semantics before SI-2/SI-3. Never substitute discrete physical z. |
| Q-9: floor selection | Resolved by D-9. SI-28/SI-34/SI-12/SI-15 validate manual camera z-slicing, rendering/picking agreement and support-qualified intent; no automatic cutaway. |
| Q-10: construction and art | Stair inventory and eight-art/one-code subdivision are resolved by D-36; visible workstation destruction endpoints by D-37. Settle isolated-appearance preview/registration before SI-35 approval and filing art children; agree shared dimensions/counts before generation. PixelLab and personal owner signoff are settled. Reconcile #2497/#2503/#2504/#2507 with D-37 and, for admitted non-workstation definitions, #2498/#2509/#2511/#2501 with D-43. D-45 adopts SI-21 shared wrecks and SI-43 non-workstation adapters. Contracts must be reconciled before affected child approval and delivered art/signoff verified before its PR lands; do not duplicate art issues. |
| Q-11: structural support/motion | D-39 resolves unit-contact waiting; D-40 resolves vertical-only motion and the whole-group stop at first supported contact. Before SI-18, specify connections and bounded cross-chunk support tracking; before SI-19, precise contact/reattachment, moving-group contacts, splitting and persistence; before SI-26/SI-27, carrier/handoff mechanics including non-workstation furniture. Preserve D-16/D-17/D-19/D-31/D-32 and D-42/D-44/D-46's landing destruction and service policies; do not restore superseded D-25 onset destruction. |
| Q-12: later impact/crushing damage | D-18 approves the mechanic but defers it to SI-20. Resolve damage and repeated-contact rules before that slice; this does not block SI-17/SI-19 or initial activation. |

The current revision is ready under D-47, with the child gates retained.
Ready means the arc and gated delivery plan are reviewable, not that all
implementation choices have already passed experiments. A changed scope or
failed backend proof that requires a different delivery architecture resets
the design to exploring. No child bypasses these gates because the umbrella
has already been filed.

### Behavior and readiness audit — September 17

This is a design audit, not a new owner decision or implementation verdict.
D-1–D-36 established the original 42 delivery boundaries; D-45 subsequently
amends the affected lifecycle boundaries and adds SI-43. The intervening
owner approvals of the endpoint and enclosure recommendations are recorded in
D-37/D-38/D-39/D-40/D-41. D-42/D-43 change furniture/workstation landing
destruction, retaining intact structures and reopening furniture art/sizing gates. Ledger,
dependency and reference checks pass; the following distinctions prevent
bounded engineering work from concealing unchosen player-visible behavior.

**External coverage recheck.** Read all 62 open issue titles, then the relevant
epic/asset bodies and targeted stair, wreck and occlusion searches. No matching
open physical-structures/multi-level umbrella was found; this is an umbrella
overlap check, not final per-child deduplication. #2078 covers directional
lifecycle presentation, not stacked support, traversal or persistent wrecks.
#2491/#2495/#2513/#2519 and the four workstation art issues remain OPEN;
the epic's child checklist is not evidence that their implementations landed.
Reuse their owners and verify actual delivery at each consuming slice.

The important findings, in resolution order, are:

1. **Workstation endpoint conflict (Q-10; choice resolved by D-37).**
   #2497 Workbench, #2504 Machine Shop and #2507 Kitchen explicitly end
   destruction in an empty or near-empty frame. That cannot be assumed to
   depict D-27/D-28's persistent solid wreck. Corrected the earlier assertion
   that the existing issues necessarily supply suitable endpoints. #2503
   Furnace retains its existing south sequence subject to owner confirmation;
   source inspection of `furnace/demolish/frame_004.png` shows visible remains
   with flame/smoke, not proof of an approved static wreck at all four facings.
   The owner approved keeping the same declared destruction clip but requiring
   a visible, owner-approved wreck endpoint in each existing art delivery. Ordinary
   demolition can still remove its transient presentation after that clip:
   `Building.Destruction.destructionExpired` and `destructionFrameIndex` govern
   expiry independently of whether the final image is transparent. D-37 settles
   the design choice; later authorized tracker coordination remains required.
   no art issue has been edited and no duplicate asset slice has been added.

2. **Contact without crushing (resolved by D-39).**
   The owner approved pausing the falling object/whole connected section at
   first contact with a unit below and resuming when clear. A held or trapped
   unit may leave it suspended indefinitely. No push, teleport, penetration or
   impact injury is added; movement collision remains required now. Before
   SI-17/SI-19/SI-25/SI-26/SI-27 approval, specify bounded rechecks, persistence
   and pause/resume integration without treating contact as landing or erasing
   carried-unit fall injury. SI-20 replaces this temporary response later.

3. **Sealed rooms differ from intersecting bodies (resolved by D-38).**
   D-24 covers a unit occupying the new solid volume. The owner approved a
   final wall completing when it encloses a unit without intersecting its body.
   SI-15/SI-31 must not add an escape-route requirement to designation or
   completion. Test allowed enclosure versus body-intersecting 99% waiting,
   including a worker inside the room and restoration of both outcomes.

4. **Group motion/first landing resolved; detailed support gates remain.**
   D-40 selects vertical-only motion with no tipping/rotation and a whole-group
   stop when any member first reaches valid load-bearing support. Other pieces
   may overhang. Before SI-19, settle precise contact/reattachment geometry,
   moving-group contact and in-flight edits/splitting under Q-11. Before
   SI-17/SI-26, settle independent furniture support and carrier behavior.
   Do not reintroduce balance/stress or per-piece settling as an implementation
   detail. Return any materially larger physics scope to this design.

5. **Recovery rates resolved; integer accounting remains gated.**
   D-41 sets initial recovery to 100% of original construction materials for
   intact targets and 50% for destroyed targets, both tunable later through the
   same action. Q-4/SI-22/SI-32 still own the work baseline, authored/legacy
   provenance, odd/single-item rounding and per-material versus pooled selection.
   The proposed round-down rule has not been approved. Preserve equal work,
   full cargo spill, exact-once payout and save/load stability; do not silently
   retune recovery or add a second reduction in the destroyed-target adapter.

6. **Cross-arc timing/residency contracts require coordination, not a fork.**
   [#2478](https://github.com/coghex/synarchy/issues/2478) already records ordinary
   movement preparing chunks and forced unit movement through unknown terrain
   using best-effort prediction with interaction phased out until reconciliation;
   its implementation is assigned outside that timing epic.
   [#1997](https://github.com/coghex/synarchy/issues/1997) owns bounded residency,
   leases and durable gameplay independent of camera residency. Before approving
   SI-8/SI-18 and affected fall owners, identify the landed or prerequisite
   interfaces and reconcile Q-4/Q-11 with those contracts. Do not reopen the
   recorded unit policy or silently extrapolate it to structural groups,
   furniture or cargo. Unknown data must never count as absent terrain,
   a structural disconnection, a confirmed landing or a lost item. If reuse
   needs substantial missing infrastructure, name the prerequisite or revise
   the affected boundary; do not absorb either whole epic into this arc.

**Deliberately engineering/evidence-gated:** measured body dimensions and
clearance, numeric representation/tolerances, shared stair contact and speed
conventions, bounded recovery/search algorithms, preview staging, occlusion
backend selection and numerical performance budgets remain with their named
Q-2/Q-4–Q-8/Q-10/Q-11 children. A proposal is not approval: a choice changing
player behavior or failing the agreed geometry returns to the owner. Performance
selection should reuse #2478's recorded four-core/8 GB and 5–50-unit gameplay
target as context; this document's 64/256/512-unit scenes are proposed stress
tests, not a replacement shipping requirement or an agreed pass/fail budget.

**Historical verdict after D-46, before D-47 readiness signoff:** keep
`exploring` pending final approval. Workstations and furniture
become destroyed at landing; structural groups remain intact. The furniture art
inventory is complete and the 43-slice split is approved; endpoint coordination
and art signoff remain gated. D-46 resolves non-workstation content admission
and cargo/service policies; bounded geometry/material gates remain.
D-44 resolves the airborne workstation policy: pause, then discard at destruction.
Graph checks establish ordering and coverage, not artwork or final readiness. D-37 resolves the workstation endpoint choice, leaving the
external coordination/delivery gate; D-38 resolves enclosure and D-39 resolves
interim contact with units below. D-40 resolves group motion and first landing;
D-41 resolves the initial intact/destroyed recovery rates and original-material basis.
Walk through the remaining player-visible
gates above or explicitly accept their bounded stop/ask handling at final
readiness. Stair inventory is complete as a design scope, not delivered art.
No new slice, artwork, tracker mutation, publication or final readiness approval
follows from this audit or the endpoint decision.

### Furniture lifecycle slice audit after D-44 — September 17

**Status: delivery split approved and applied by D-45; content admission and
service/cargo policy resolved by D-46.** Actual art delivery and the remaining
bounded contact/material gates are not implied by those approvals.
Read current primary `02b7b183a8260841f3ba1fdf0ebc6ae51ac45b73`, the eight
`data/buildings/*.yaml` definitions, `Building.Destruction`, the live
`BuildingDestroy` handler, crafting/power contracts and the portal spawn
eligibility check. Read the current bodies of #2498/#2501/#2509/#2511; all four
are OPEN. This audit addresses D-42–D-44's changed lifecycle only, not a fresh
approval of the entire arc or permission to process it.

#### Verified content and art coverage

The four crafting stations are Workbench, Furnace, Machine Shop and Kitchen.
The other four shipped building definitions are not inert decorative furniture:

| Non-workstation definition | Current gameplay owner | Destruction art and existing delivery owner |
|---|---|---|
| `cargo_hold_S` | Building storage, capacity and remembered container contents | No declared destruction role. Undeclared `demolish/` files are not runtime coverage or approved wreck art. [#2498](https://github.com/coghex/synarchy/issues/2498) owns its four-facing destruction set. |
| `solar_panel` | Power source node | Static-only YAML; no destruction clip. [#2509](https://github.com/coghex/synarchy/issues/2509) owns its four-facing destruction set. |
| `high_voltage_battery` | Power storage node and persisted charge | Static-only YAML; no destruction clip. [#2511](https://github.com/coghex/synarchy/issues/2511) owns its four-facing destruction set. |
| `acolyte_portal` | Lua spawn sequencer, remaining spawn count and appearance/built loop | Appearance and built roles only; no destruction clip. [#2501](https://github.com/coghex/synarchy/issues/2501) owns its four-facing destruction set. |

Each of those four art issues currently requires an empty or near-empty terminal
frame. That is not proof of D-43's visible, solid persistent wreck. Before an
affected lifecycle child is approved, reconcile the existing owner's endpoint
contract with D-43; before its code PR lands, verify delivered declarations,
all-facing visible wreck endpoints and personal owner signoff. Ordinary
demolition may still expire its transient visual after the same clip. Keep the
existing PixelLab supply paths and art owners; no duplicate asset slice, new
generation or tracker edit is authorized here. Preserve approved static and
construction/appearance art. If an existing art issue lands before coordination,
recheck the result and obtain approval for any required follow-up delivery.

This is an inventory/contract audit, not visual approval of any endpoint.
For all four non-workstation definitions, suitable declared wreck coverage is
unproven. The eight stair scopes remain unchanged because D-43 keeps landed
structural pieces intact. The four previously audited workstation art owners
remain #2497/#2503/#2504/#2507; they do not supply the four sets above.

**Content admission: resolved by D-46.** All four non-workstation definitions
above use the placed-building lifecycle, including the portal despite its
`gateway` visual class. No separate chair/table furniture catalogue was
found in the shipped building roster; do not invent new content to fill this slice.

#### Concrete sizing and ordering findings

1. **SI-17's pre-audit scope combined two independently reviewable owners.** Continuous
   placed-object motion already includes support/contact, occupancy, render/pick
   publication, eviction and migration. Adding playback, persistent wreck
   identity/collision and each building's shutdown/storage rules duplicates
   SI-21 and overloads the motion slice. Return SI-17 to shared motion and its
   landing handoff, with controlled fixtures only until lifecycle integration.
   This is delivery staging, not permission for furniture to land intact in
   normal play.
2. **SI-21 now owns one shared wreck representation.** Its persistent
   playback/endpoint, collider, picking, source identity and support-loss contract
   are common to stations and other placed buildings. Move it ahead of the
   gameplay teardown adapters and test explicit wreck admission using approved
   real art. It must not depend on station job/cargo teardown merely to establish
   this shared owner, nor implement a second live building service layer.
3. **SI-25 remains a bounded but high-risk workstation adapter.** D-44 adds
   persisted paused logical work and stale-worker refusal before landing.
   The current `BuildingDestroy` handler intentionally leaves craft bills
   cancellable, removes the live instance immediately, forgets container
   knowledge and retires power nodes; its transient effect owns no inventory,
   collision or persistent wreck. Calling it unchanged cannot implement this
   arc. SI-25 must own explicit workstation teardown and an exact-once landing
   transaction handing cargo to SI-16 and the remnant to shared SI-21. Preserve
   ordinary demolition behavior. Do not also absorb non-station power/spawn
   consumer integration into this PR.
4. **SI-43 is the added non-workstation lifecycle-adapter slice.** Reuse SI-17 motion,
   SI-25's factored landing/payload transaction and SI-21's wreck owner; cover
   cargo storage, power nodes and portal spawning through bounded adapters.
   No new motion engine, wreck schema, art generation, power-network redesign
   or spawn-system redesign belongs here. Each adapter needs direct bypass,
   stale-reference, mid-fall reload and post-destruction tests, not just a
   rendered falling object. If the reuse boundary cannot support those three
   consumer families within one reviewable PR, split this adapter slice again
   before its issue is approved; do not hide new infrastructure inside it.
5. **SI-26 must depend on both complete gameplay adapters and shared wrecks.**
   It composes existing fall/landing owners for carriers, not invents a second
   destruction path. SI-32 consumes the same wreck identity for clearing and
   50% recovery; no new salvage action or furniture-specific work penalty.
   SI-19 and SI-27 need no new destruction scope: structures still land intact.

#### Approved replacement boundaries and graph

D-45 applies this table to the processing ledger and delivery sections, renames
the two shared owners and appends SI-43 without renumbering prior IDs.
Each introducing PR owns its tests, component migration/classification and
required contract documentation.

| Slice | Approved single-PR boundary | Required predecessors |
|---|---|---|
| SI-17 | Shared placed-building fall motion, contact waits and persisted landing handoff; no job teardown, cargo spill or wreck implementation | SI-23, SI-8, SI-34 |
| SI-21 | Shared persistent building wreck playback, final frame, geometry/picking, source identity and support-loss behavior; fixture admission, no live services | SI-17, SI-34; suitable approved art for the tested definitions |
| SI-25 | Workstation pause → landing destruction, permanent job teardown and exact-once storage spill, reusing shared wreck admission | SI-17, SI-16, SI-21 |
| SI-43 | Admit confirmed Cargo Hold/power/Portal definitions to falling and destruction; reconcile their storage/service owners through the shared boundaries | SI-17, SI-16, SI-21, SI-25; confirmed content policies and suitable approved art |
| SI-26 | Carry loose items and placed buildings; invoke their existing lifecycle exactly once at actual landing or carrier loss | SI-19, SI-16, SI-21, SI-25, SI-43 |
| SI-32 | Clear shared destroyed targets at intact work speed and 50% original-material recovery | SI-22, SI-21, SI-26, SI-27 |

Approved local order: SI-17 → SI-21 → SI-25 → SI-43 → SI-26.
SI-27 stays independently schedulable after SI-19/SI-24; SI-32 waits for the
required carrier/clearing prerequisites. SI-7 activates only the complete
pipeline and remains downstream of SI-43 through SI-26/SI-32. This removes
the old station-only SI-21 dependency that forced motion to implement furniture
wrecks first. The applied ledger and graph must retain exact mirroring,
dependency-valid order, no cycles and complete activation prerequisite coverage.

**Current count: 35 code slices + eight stair-art slices = 43 entries.**
No extra art entries are added because each affected building already has a
dedicated owner; endpoint corrections require authorized coordination, not
duplicate issues. This count does not declare the external art delivered.

**Adapter policy resolved by D-46:** all four definitions suspend services
during descent/contact waits, permanently lose functionality at landing, scatter
stored items in full and leave solid final-frame wrecks. No portal spawns or
battery service survive in a wreck. The remaining SI-22/SI-32 gate is explicit
original-material provenance for item-consuming solar/battery placement and
the zero-work portal; do not invent an item payout or convert electrical charge
into salvage. Contact and landing/scatter anchors remain bounded Q-4 details.

D-45 approves and applies the split, with ledger titles, dependencies, acceptance
scopes and ownership references reconciled. Q-4/Q-10's remaining geometry/material/art
stop/ask gates remain explicit. D-47 subsequently approves design readiness
while retaining those gates.

### Final design-readiness audit — September 17, after D-46

**Accepted by the owner under D-47: ready for later issue processing with the
explicit child gates retained.** No new gameplay
decision or slice is introduced by this audit. The current 43 entries remain
35 code deliveries and eight independently approved stair-art scopes.

**Freshness and overlap.** Rechecked primary
`02b7b183a8260841f3ba1fdf0ebc6ae51ac45b73`, all 61 current open issue titles,
targeted multi-level/physical-structure/occlusion/wreck searches and the relevant
epic bodies. No matching open physical-world umbrella was found. #2078 remains
the directional/lifecycle-art owner, not an implementation of spatial physics
or persistent wrecks; its checked child list is not evidence of landed work.
#1229 explicitly excludes multi-level dungeons and owns expedition encounters,
not this arc. The previously identified stair facilities, Dungeon art, eight
building-art issues, #1997 residency and #2478 timing remain external owners.
Their current open status and update timestamps were checked; final per-child
deduplication and delivered-interface checks still belong to later processing.

**New adjacent compatibility evidence.** Since the earlier geometry inventory,
#2505 introduced pending location-container shells and `world-pages` v12.
`docs/engine_contracts.md` §Pending container shells requires a bound pending
shell to remain the same top-level ground item on its source page until the
separate pickup/realization transition. It can contain authored default items
without its loot profile having been drawn. Falling/repositioning must not be
implemented by pickup-and-respawn, profile realization, inventory transfer or
new item IDs. SI-13/SI-16/SI-26 must preserve the ground/item identity and slot
association through physical motion, carrier loss and reload; extend existing
fixtures to cover a pending shell with authored contents. Coordinate with #2510
and #2522 if their pickup boundary has landed when those slices are specified.
This is compatibility coverage for existing item ownership, not another slice
or permission to redesign the portable-container arc. Freeze/migrate the actual
current component schema at implementation time, not a version from this audit.

**Readiness checks completed:**

- The goal and done condition are observable in the ruin, multi-floor/stair,
  construction, fall, wreck and deconstruction acceptance matrix. SI-7 activation
  is distinguished from completion of the later SI-5 crowd and SI-20 impact work.
- All 43 ledger entries mirror delivery headings, have bounded outcomes and
  acceptance scopes, and are dependency ordered. SI-7's transitive closure covers
  all initial slices; only SI-5/SI-20 remain intentionally downstream. No cycle,
  missing slice, undefined D/Q reference or duplicate stable ID remains.
- Each state-introducing PR owns classification, frozen legacy wire shapes,
  migration, behavioral save/load evidence and required documentation. Tests of
  runtime behavior and rendered pixels are distinct; no headless result claims
  art/occlusion signoff, and no static inventory claims codec compatibility.
- Scope remains continuous multi-level physics, both authored/player building,
  manual camera-z slicing, intact landed structural groups and destroyed landed
  furniture. Structural rubble, automatic cutaway and rigid-body rotation remain
  excluded. Landing destruction never means destruction at fall onset.
- Stair art has explicit PixelLab/owner-signoff deliveries. All eight building
  art owners are named; persistent visible endpoints require coordination and
  actual signoff. No new artwork was produced and no tracker contract was edited.

**Remaining gates to retain deliberately, not silently choose:**

| Gate family | Owner and mandatory stop point |
|---|---|
| Measured geometry, continuous contact, reach and speed | Q-4/Q-8: SI-1/SI-11 foundational conventions, SI-2/SI-3 motion calibration and SI-4/SI-9/SI-30 action policies. Settle before the relevant child is presented for approval; changed gameplay or inadequate clearance returns to the owner. |
| Falls, residency and restoration | Q-4/Q-5/Q-11: SI-8/SI-18 publication/graph protocol, SI-16/SI-17/SI-19/SI-24 contact and absent-versus-unavailable support, SI-21 wreck re-falls, SI-25/SI-43 scatter anchors, SI-26/SI-27 carriers and SI-33 recovery bounds. Stop before child approval; do not infer unknown terrain is empty or suppress allowed floor removal. Reuse #1997/#2478 interfaces where applicable. |
| Deconstruction accounting | Q-4: SI-22 shared work/material provenance for authored, legacy and item-placed content; SI-32 integer rounding and selection. Percentages and equal work are fixed by D-29/D-41, but round-down and special item payouts are not approved. Settle before affected child approval. |
| Rendering feasibility and measured budgets | Q-6/Q-7: SI-6 then SI-28 must prove the actual reference scenes before SI-34; agree bounded integration measurements there, full production budgets before SI-7 and crowd budgets before SI-5. A failed proof or substantially larger backend returns to design, not a nominal pass. |
| Art staging, coordination and actual approval | Q-10: SI-35 preview-staging contract before its approval and before art issues; shared canvas/anchor/count profile before generation. SI-21/SI-25/SI-43 require reconciled existing art contracts before child approval and delivered, personally signed-off art before PR final review/landing. No placeholders or duplicate art issues. |
| Later crowd and impact rules | Q-2/SI-5 owns deterministic priority/retry within D-20/D-21; Q-12/SI-20 owns impact scaling and replacing saved D-39 waits. Settle before those child approvals without adding them as prerequisites for SI-7. |

These gates permit an umbrella to describe a complete arc, but not a blocked
child to be approved on guessed defaults. The first foundation child itself
still requires Q-4/Q-8 geometry choices; ready does not mean every child can be
filed immediately. The future processor asks the owner, collects bounded
evidence, or records a named external prerequisite, and stops before approval
when a gate is unmet. Material scope/architecture changes reopen this design.

**Handoff approved by D-47:** the local document is complete and marked
`ready for issue processing`, retaining all gates above. A different agent brand
then uses `process-design-doc`, epic first and one child per invocation, with
separate approval for each tracker artifact. This foreground session neither
processes nor publishes the document. No code build/game launch is appropriate
for these documentation-only edits; validation here is document consistency,
dependency coverage and read-only source/tracker evidence, not implementation proof.

### Completed design-readiness checks

- [x] Retain D-36's eight independently scoped stair appearances under D-43;
  structural landing adds no new wreck art. Q-10 retains the bounded SI-35
  preview-staging gate. Scope approval is not artwork approval.
- [x] Inventory D-43's shipped non-workstation candidates and declared clips:
  the audit above identifies four existing art owners and their incompatible
  empty/near-empty endpoint requirements. This is not delivered-art signoff.
- [x] Approve and apply the post-D-44 split (D-45): shared motion and wreck
  owners plus SI-43's non-workstation lifecycle adapters, for 43 entries.
  Prior IDs remain stable; art/contact/material gates are not silently approved.
- [x] Resolve airborne workstation jobs/services: paused during descent and
  discarded at landing destruction (D-44), including carried falls and contact waits.
- [x] Confirm all four non-workstation definitions and their suspension,
  landing destruction and full stored-item spill policy (D-46).
- [x] Owner accepts the final readiness audit's explicit bounded child gates (D-47);
  all are mapped above, but their proposed defaults are not owner decisions.
- [x] Recheck tracker overlap and external prerequisites against current state
  (final September 17 pass: 61 open issues; not proof of delivery or child deduplication).
- [x] Resolve Q-10's workstation destruction-endpoint choice with the owner
  (D-37). Existing art owners supply visible last-frame wreckage; later authorized
  tracker coordination and personal art signoff remain SI-21 prerequisites.
- [x] Obtain the owner's explicit approval of the consolidated scope and
  audited delivery plan (D-47); mark `ready for issue processing`.

These are design-readiness checks, not implementation tasks or authorization
to process this document. D-7 reserves processing for a different agent brand.

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
   Friendly units wait/yield without overlap or body shrinking. A held unit
   retains its hold; an impossible pass produces a bounded blocked outcome
   rather than forcing a passage or retrying forever.
   Repeat with hostile units: no cooperative yielding or passage through an
   enemy. A legal detour, existing combat behavior or blocked outcome applies;
   avoidance itself neither creates an attack order nor overrides a hold.
5. Add existing terrain height transitions beside the ruin. Verify stepping,
   climbing/falling, and occlusion at fractional positions on both sides.
6. Edit relevant geometry and perform a fresh-process save/load. Verify route
   invalidation, support changes, and each chosen overlap-recovery outcome.
7. Build an authored two-storey reference scene with overlapping floor XY,
   independent lower/upper occupants and items, a slope, stairs, and an upper
   slab opening. Walk to the second floor and back using real player commands.
   Chain at least three stair flights directly along their rise/run with no
   inserted landing, verifying three tiles of run and three z of rise, including
   a chain starting at fractional z through Lua. Connect a separate adjacent floor as a landing/turn and
   verify its independent removal does not remove a stair piece.
   Pause at fractional progress on both connections and verify continuous
   physical height, body clearance, selection, and foot contact at all facings.
   Extend the route onto a reachable roof: its top supports walking without
   an opt-in flag, its underside blocks passage from below, and no unit climbs
   onto it merely by colliding with a wall. Verify manual slice/pick agreement
   and fresh-process save/load with an occupant on the roof.
8. Save halfway up a connection and on each floor, restart, restore, and resume
   the same destination without teleporting to another support. Place/remove
   one elevated piece without changing its same-slot neighbor below. Replay
   historical replace/clear sequences and prove they do not create extra floors.
   Include Lua-authored floors at fractional base heights, retained unchanged
   across staging, commit, eviction, and reload. Exercise the whole-z player
   placement policy separately; it cannot round existing Lua placements.
9. Remove an upper support or connector during travel. Verify route invalidation,
   bounded recovery/fall, and landing on the first eligible lower support. Show
   a low-headroom rejection, walking underneath a stair where clearance allows,
   a blocked opening, and a same-XY destination on the wrong floor that does not
   count as arrival. Create the stacked scene through the real construction
   workflow as well as the fixture builder (D-8).

   **Construction activation and occupancy:**

   At designation and partial progress, walls remain passable and floors/stairs
   provide no walkable or structural support. Save/restart each unfinished
   stage; only committed completion activates all physical roles together.
   An unfinished piece cannot supply a build approach or anchor an overhang.
   Keep a unit in a wall's final solid volume through the last work increment:
   the site waits at 99%, retains materials, and remains nonphysical. Repeat
   after save/restart, with a held unit, and with a unit on a different storey.
   Once the obstruction clears, completion commits once without double charges
   or effects; a non-intersecting unit on another storey never blocks the site.
   Complete the room's last wall with a non-intersecting unit inside: enclosure
   is allowed and no escape-route check delays completion. Repeat with the
   worker inside; save/restart preserves the sealed room without granting a
   collision exemption. Contrast with a body intersecting that same final wall,
   which still requires the persistent 99% wait (D-38/D-24).

   **Support removal and occupant falls:**

   Demolish an occupied floor with a unit and a loose item above another floor:
   removal succeeds and both fall onto that lower floor. Repeat above terrain,
   a fractional-height support, and a stair. Save/restart during each fall,
   remove a prospective landing during descent, and verify correct landing
   without duplicating the item, losing contents, or applying unit injuries twice.

   **Furniture and workstation destruction:**

   Repeat with placed furniture and a workstation containing stored items and
   queued/active work. Furniture and workstation become destroyed at actual landing,
   including when carried by a detached structural group. Verify jobs/services
   pause at fall onset, preserving queued/active job state without advancing
   work, producing output or providing services. Save/restart mid-fall and during
   a contact wait: work stays paused, not discarded or resumed. Actual landing
   discards jobs permanently; no stale worker emits output during descent or
   after destruction (D-44).
   Old occupancy is released coherently; stored items scatter within the actual
   landing tile/support, preserving instance identity, quantity and nested
   contents. Cover a landing on an upper floor with terrain below; save/restart
   immediately before and after publication without duplicate/lost spills.

   **Interim contact before impact/crushing:**

   Repeat the falling lifecycle for Cargo Hold, Solar Panel, Battery and Portal
   (D-46), independently and carried by a structural group. During descent and
   contact waits, direct and AI requests cannot transfer storage, generate or
   charge/discharge power, or spawn units. Save/restart retains suspension.
   Actual landing publishes the complete stored-item spill once, permanently
   retires services and leaves one solid shared wreck. No charge becomes item
   salvage and no pending portal spawn becomes catch-up output. Reload before
   and after landing cannot duplicate cargo, revive services or replay spawning.

   Drop furniture and a connected section toward a unit below, including held,
   hostile and trapped units: stop at contact without penetration, displacement
   or damage, then resume only when the space clears. Test multiple blockers
   and different floors; a projected sprite overlap alone must not stop descent.
   The entire group pauses coherently. Waiting is not terrain anchoring or
   landing: stations are not yet destroyed, cargo remains accounted for, and carrier motion
   and injury history survive the pause without an early spill, playback or
   injury event. Save/restart and evict/reload during the wait, then clear the
   blocker; resume without stale identities, catch-up tunnelling or duplicate
   effects. Carried units on top do not block their own support's descent.
   Their actual landing still receives full self-fall consequences (D-39/D-32).

   **Structural first-contact landing:**

   Drop an asymmetric connected section over uneven terrain and over a lower
   floor/roof. The first valid load-bearing contact of any piece stops the
   whole section, preserving XY, orientation and relative offsets. Other pieces
   remain overhanging; nothing tips, rotates, slides or settles independently.
   Vary piece iteration order and tick sizes, including a step that would cross
   multiple support heights; the earliest contact gives the same landing.
   Save/restart mid-fall and after a fractional-height landing, then remove the
   final catching support and verify that the unsupported section falls again.
   D-39's unit-contact pause must remain distinct from this landing (D-40).

   **Persistent wrecks:**

   On landing, both furniture and workstations play destruction once and retain the final
   frame at every camera facing. Save/restart during playback and after expiry,
   hide/show the page and evict/reload its chunk: the wreck persists without
   clip restart, double spill or restored functionality. The wreck blocks
   routing and swept movement before and after restoration, without blocking
   non-intersecting occupants on another storey.

   **Deconstruction and recovery:**

   Deconstruct intact and destroyed
   targets of the same definition using the same workers/conditions: required
   work and completion time match. Interrupt and reload partially completed
   clearing, then finish; the exact remnant/collider disappears once, without
   re-spilling stored items or producing another wreck. Verify the shared
   destroyed-structure target path without introducing structural rubble art.
   Intact targets return 100% and destroyed targets 50% of original construction
   materials at the D-41 baseline, under the agreed integer rounding/selection
   policy; stored cargo still spills in full. Exercise
   odd/even/single-item quantities and mid-clearing save/restart without a
   duplicate payout or altered per-item condition.
   Across these transitions, save/restart never resurrects station operation/jobs,
   repeats completion/spawn effects, or loses the physical fall.
10. Build a horizontal overhang attached at one side to a supported structure.
    It remains in place. Remove its last supported connection: the detached
    connected section falls together with relative piece positions preserved.
    A pair of pieces connected only to one another must fall rather than remain
    suspended. Split a supported group into an anchored and a detached part;
    only the detached component falls. Repeat across chunk/U seams, after
    eviction/restoration, and with fractional Lua heights. Save/restart during
    group descent; verify each piece exists exactly once with its original
    identity and there are no ghost colliders at its old fixed position.
    Place loose items on the falling section: they descend with their actual
    support and remain on it after landing, retaining identity, quantity and
    relative placement without gravity being applied twice. Repeat after
    mid-fall save/restart and eviction. Remove an item's supporting piece in
    flight and verify exactly one transition into independent falling, without
    duplication, disappearance or an unwanted workstation-style scatter.
    Put a unit on the same falling floor: it enters the existing falling
    animation, descends with the carrier and receives full ordinary fall
    injuries once on landing. Compare against a cliff fall with the same unit
    and world-space drop; include fractional heights and mid-fall carrier
    removal. Reload before, during and after landing without losing/doubling
    injuries or treating the moving support as safe standing ground.
    After landing, the section remains intact and usable at its new height:
    units can traverse reachable floor/stair surfaces, walls still block them,
    and save/restart preserves that geometry. No rubble conversion occurs.

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

Add multi-support node/goal tests, connector endpoints, fractional height
samples within and across tiles, conservation of admitted movement time under
the selected surface-distance convention, and vertical action/occupancy checks.
Save tests must exercise the real component codecs and old fixture corpus;
static inventory coverage alone cannot prove the migration. Generated-world
output changes, if required for authored locations, trigger the full worldgen
tier under `src/World/CLAUDE.md`; fixture-only geometry is not permission to
silently change ordinary generation or defer that gate.

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

## Applied slice sizing and ordering audit — September 17

D-33 records the owner's approval of the audit and its application. The old
22-entry plan combined eleven independently separable outcomes and an uncounted
art family. The first applied audit produced **33 code slices plus SI-14's
provisional art family**, 34 entries. D-36 then resolved the art boundary:
**35 code slices plus eight art slices**, 43 matching ledger/delivery entries
following D-45's lifecycle amendment.
No gameplay scope was added or deferred by either subdivision. Explicit
evidence-driven resizing gates remain outstanding.

Existing IDs retain a concrete part of their original outcome; SI-23 through
SI-34 name the extracted parts. This is delivery-plan approval, not design
readiness, issue creation, implementation or publication authorization.

### Evidence and sizing rule

Rechecked against primary checkout `064a255f06b59e2b1f8e881dcf4c747d52967a36`:

- `Item.Ground` owns XY-only ground entries; their wire representation is in
  `World.Save.Component.PageActivity`. Buildings separately own integer height,
  delivered material instances and stored contents in `Building.Types`, with
  entity snapshots and reservations. The former combined SI-13 crossed two independently testable
  placement/migration boundaries, not one common entity store.
- `Unit.Sim.Types.usLocalPath` and `Unit.Pathing.AStar` still use XY routes;
  `Unit.Thread.Movement.PathAdvance` both consumes residual movement time and
  stamps integer-derived height on arrival. Changing support-aware route search,
  supported locomotion and fall injury together would join distinct invariants.
- `Building.Thread.Command`'s ordinary destruction deletes the live instance
  but intentionally leaves craft bills. `Building.Destruction` owns a transient,
  expiring render effect. Thus workstation teardown/spill and persistent wrecks
  cannot be treated as incidental rendering adjustments to a motion PR.
- Work approaches have separate owners in `scripts/unit_ai_construct.lua`,
  `unit_ai_craft.lua`, `unit_ai_repair.lua` and the combat modules; authoritative
  execution checks also need migration. The first three main modules are already
  near their enforced 500-line limits. This is review-boundary evidence, not a
  request for unrelated size-driven refactoring.
- `Engine.Scene.Assembly` merges scalar-sorted static and dynamic runs;
  `Unit.HitTest` still culls by integer z while positioning by continuous z.
  Production scene/picker integration is real work, not merely a final switch.
  The early fixture proof cannot by itself satisfy later pointer-command tests.

Keep each changed owner's codec migration, classification, tests, contracts and
required visual evidence in its introducing PR. Do not manufacture small slices
by separating a state change from its persistence or an action from its safety
checks. A slice can use explicit physical fixture content before activation, but
must exercise real owners; it cannot introduce a second test-only simulation.

### Applied ID mapping

| Former combined slice | Retained outcome | Extracted outcome(s) |
|---|---|---|
| SI-13 — item/building placement | SI-13 ground items | SI-23 building placement/reservations |
| SI-2 — grounding and falls | SI-2 supported unit locomotion | SI-24 continuous unit free fall and landing injury |
| SI-17 — furniture/workstation falling | SI-17 shared placed-building motion | SI-21 shared wrecks; SI-25 workstation lifecycle; SI-43 non-workstation adapters (D-45) |
| SI-19 — structural falls/carriers | SI-19 unoccupied structural-group motion | SI-26 item/placed-object carriers; SI-27 unit carriers and injury |
| SI-6 — rendering proof | SI-6 early ruin proof | SI-28 extended stacked/stair/manual-slice proof |
| SI-3 — routes and sweeps | SI-3 connection admission and swept movement | SI-29 bounded support-aware route search |
| SI-9 — work/combat reach | SI-9 work approaches/execution | SI-30 melee approaches/execution |
| SI-15 — construction | SI-15 designation/preview/intent | SI-31 worker completion, attachment and 99% wait |
| SI-22 — deconstruction | SI-22 shared intact-target worker workflow/recovery | SI-32 destroyed-target adapters, equal work and half recovery |
| SI-8 — lifecycle/restoration | SI-8 early committed publication/invalidation | SI-33 old-overlap recovery and restoration integration |
| SI-7 — scene integration/activation | SI-7 final activation and full acceptance | SI-34 production scene/pickers before player intent |

SI-1, SI-10, SI-11, SI-18, SI-16, SI-21, SI-12, SI-4, SI-5 and SI-20 retain
their coherent boundaries, with dependencies/references adjusted to the new
owners. In particular, SI-10 keeps staging, replay and migration together;
SI-21 keeps durable wreck identity, playback and collision together for all
placed buildings; SI-25/SI-43 own their respective gameplay lifecycle adapters.
D-36 replaces SI-14's provisional family with eight complete directional
appearance deliveries (SI-14/SI-36–SI-42), plus SI-35's art-consumer integration.
Each art PR owns one static/mask/lifecycle bundle and its personal signoff;
SI-35 owns the code and the bounded preview-staging gate.

### Ordering corrections applied

- **Rendering risk first:** SI-6 now needs only SI-1 and existing approved
  ruin/acolyte art. SI-28 follows connected geometry, continuous supported unit
  placement, placement adapters and approved stairs, but not falling systems.
  Every later dynamic producer supplies real visual evidence in its own PR.
- **Publication before consumers:** SI-8 follows SI-10 and precedes connection,
  placement and motion consumers. Each owner implements its concrete invalidation
  and support-loss behavior; SI-7 no longer hides a late lifecycle implementation.
- **Pointer selection without a circular responsibility:** SI-34 produces real
  scene/picker integration before SI-12. SI-7 can consequently wait for complete
  player behavior without being its own implied prerequisite.
- **Independent work has independent dependencies:** SI-2 does not wait for
  ground-item placement; SI-16 does not wait for unit injury; SI-19 does not
  wait for workstation teardown; SI-22 does not wait for wrecks; SI-31 does not
  wait for melee migration. Shared geometry, publication, placement and query
  prerequisites remain explicit.
- **Activation unchanged:** SI-7's transitive dependencies include every initial
  code/art branch. Controlled routes, empty groups and non-workstation fixtures
  do not authorize incomplete gameplay. SI-5 and SI-20 follow SI-7; unit
  self-fall injury is delivered beforehand in SI-24/SI-27.

### Residual sizing gates

The backend verdict can change the size of SI-34. If it requires a new GPU
depth pipeline or substantial independent producer redesigns, revise those
boundaries after the proofs, before approving that child. An unlimited renderer
rewrite is not a one-PR outcome. Likewise, Q-11 must bound structural residency
before SI-18 approval, and the work/placement caller inventories must identify
complete adapters before their child approvals. Each resulting PR owns its
migrations, invariants, required docs and evidence; none of these gates permits
dropping a required caller or postponing a gameplay requirement.

## Delivery plan

D-33/D-36 established 34 code slices plus eight concrete stair art slices.
D-45 adds SI-43 and amends the lifecycle ownership/order: the current ledger
has 35 code and eight art slices, 43 entries, preserving every prior ID.
SI-14 is now the intact northeast asset, not a family placeholder. Existing
and new IDs each name one outcome, not nested epics.
The ledger follows dependency-valid order; independently ready branches need
not wait for unrelated work solely because it appears earlier in the list.

SI-6 is the early ruin proof, SI-28 extends it to stacked floors/stairs, and
SI-34 supplies production scene/picker integration before SI-12. SI-8 establishes
committed publication early; SI-33 integrates restoration recovery. SI-7 alone
activates normal content after all initial behavior and art prerequisites.
Every preceding code PR owns its state migrations, concrete consumer adapters,
targeted gates and required evidence. Before activation, explicit physical
fixture content exercises real owners without exposing incomplete gameplay.
SI-5 and SI-20 remain later deliveries. No tracker artifacts are recorded;
the September 17 overlap check is recorded above, and D-37's external contract
coordination and actual art deliveries remain explicit gates.

### SI-1. Define and derive world support surfaces and solid bounds

- **Outcome:** A tested spatial contract and adapters for existing content.
- **Scope:** Coordinate/elevation conventions, shapes, support identity,
  multiple surfaces per column, continuous height evaluation, body-volume
  clearance, chunk-local queries, ownership/classification, metadata defaults.
- **Phase:** Foundation. **Depends on:** none. **Ordering:** critical path.
- **Relevant decisions:** D-1, D-2, D-4, D-5, D-6, D-22.
- **Acceptance signals:** The elevation convention written down and tested
  against terrain, floor, ceiling, and unit z; explicit floor-top alignment
  and independent geometry tests for shipped ruin pieces, acolyte footprint,
  and building bounds derived from the reserved footprint; all five unit
  YAMLs carry an authored `body.radius` and a definition without one fails
  to load; two surfaces at identical XY remain distinct, and slope samples
  agree with the selected physical/pixel convention. Roof/ceiling geometry
  exposes a walkable top and solid underside with distinct query roles.
- **Out of scope:** Switching live movement or rendering to new rules.
- **Open questions:** Q-4, Q-8; settle foundational geometry first. D-5 resolves Q-3.

### SI-6. Prove geometry-based occlusion on the ruin regression scene

- **Outcome:** An early, bounded experiment establishes whether the shared
  geometry can fix the shipped ruin/acolyte regression before major motion work.
- **Scope:** Existing approved ruin/acolyte art, SI-1 geometry, four-facing
  depth/z-term measurements and the candidate ladder: convention/top-skirt
  decomposition, constraint ordering, then geometric depth if evidence requires
  it. Retain captures, timings, stable-tie and cycle/ambiguity treatment.
- **Phase:** Early rendering proof. **Depends on:** SI-1.
  **Ordering:** first risk gate, before the large migration chain.
- **Relevant decisions:** D-1, D-2, D-5, D-6, D-33.
- **Acceptance signals:** Reproducible original overlap and empty-scene controls,
  no floor holes/seams in the bounded reference, and an explicit backend verdict
  with limitations. A failed candidate is not success: revise the delivery
  architecture if no bounded solution passes.
- **Out of scope:** Stair art, new movement/falling systems, production
  scene/picker migration, or claiming the entire multi-level matrix is proven.
- **Open questions:** Q-6; SI-28 extends the proof and SI-34 integrates it.
  Required new depth-map artwork triggers Q-10 rather than a silent dependency.

### SI-10. Preserve stacked structure identity through edits and saves

- **Outcome:** Two same-slot pieces at different heights coexist and survive
  precise mutation, chunk reconstruction, and session restoration.
- **Scope:** Elevation-qualified store and placement identity, staging,
  command transport, ordered edits, replay, renderer enumeration, qualified
  queries/removal, frozen legacy DTOs, and component migrations. Keep these
  inseparable paths together so no stored placement has ambiguous semantics.
- **Phase:** Storage. **Depends on:** SI-1. **Ordering:** critical path.
- **Relevant decisions:** D-3, D-5, D-10.
- **Acceptance signals:** Place/remove either of two stacked floors without
  affecting the other; eviction and fresh-process save/load retain both;
  historical set-at-one-z/set-at-another-z/clear reproduces exactly the old
  single-slot result, with every retained codec baseline decoding correctly.
  Direct Lua fractional placement survives every boundary; invalid explicit z
  causes no mutation and never falls back to zero.
- **Out of scope:** Stair shape design, live movement, and construction UI.
- **Open questions:** None for authored anchors; D-10 settles the capability.
  Canonical numeric representation and validation are engineering choices with
  explicit precision/range and migration tests.

### SI-8. Publish committed geometry changes and invalidate spatial views

- **Outcome:** Spatial consumers can identify one committed geometry revision
  and invalidate stale support, route and graph views coherently.
- **Scope:** Actual placement/removal/terrain-edit publication, page/chunk
  replacement and session-incarnation boundaries; derived-view ownership and
  bounded notifications/queries. Establish the production boundary now; each
  later consumer joins it in its own introducing PR.
- **Phase:** Lifecycle foundation. **Depends on:** SI-10.
  **Ordering:** before live spatial consumers.
- **Relevant decisions:** D-1, D-3, D-5, D-6, D-14, D-33.
- **Acceptance signals:** Real committed edits, replacement, eviction and load
  invalidate the correct page/revision, never publish staged-only geometry or
  leave a stale-session view. Fixture consumers observe the committed result
  exactly once; derived state is rebuilt according to its classification.
- **Out of scope:** Implementing later motion owners, construction completion,
  old-unit overlap recovery (SI-33), or activating shipped collision.
- **Open questions:** Q-4's unavailable-data publication behavior. Every later
  slice still owns its concrete support-loss handling and persistence; SI-7
  verifies composition, not missing lifecycle implementations.

### SI-11. Define and author connected slopes, stairs, and floor openings

- **Outcome:** Content can describe and programmatically place a valid route
  between lower and upper supports, including the upper slab opening.
- **Scope:** Connector schema, orientation, footprint, direct repeated-flight
  joins and connections to separate floor landings, continuous
  contact profiles, headroom, whole-tile slab openings, validation, the agreed art inventory,
  and fixture/location builder entry points. Keep authored physical dimensions
  separate from sprite canvas size. Do not expose incomplete content in play.
- **Phase:** Content geometry. **Depends on:** SI-10, SI-8. **Ordering:** critical path.
- **Relevant decisions:** D-1, D-5, D-6, D-8, D-10, D-11, D-12, D-22, D-34.
- **Acceptance signals:** Lower/upper endpoints resolve to distinct supports;
  blocked openings, discontinuous endpoints, and insufficient headroom refuse
  clearly. A two-storey geometry fixture exists and the art manifest lists
  every appearance still missing. Synthetic geometry tests do not claim visual
  or owner acceptance.
  Repeating flights produces one continuous ascent; landings remain independent
  floor pieces, with no hidden flat segment inside a stair definition. Every
  piece has one tile of run and one z of rise at whole and fractional base heights.
  A legal connector reaches a roof support without disabling the roof's
  underside clearance or requiring an opt-in walkability override.
- **Out of scope:** Asset generation, stair art-consumer integration (SI-35),
  unit traversal, and construction controls.
- **Open questions:** Q-4, Q-8, Q-10; geometry and art ownership before approval.

### SI-13. Preserve ground-item support identity through placement and saves

- **Outcome:** Ground items remain on the intended support through placement,
  pickup/drop, explicit repositioning, edits and restoration.
- **Scope:** Durable item placement/support identity; inventory all ground
  spawn/drop/spill/transfer and compatibility paths; render/pick height
  publication; page activity migrations and typed-reference classification.
  Consume SI-8 revisions without confusing unavailable geometry with terrain.
- **Phase:** Item placement. **Depends on:** SI-11, SI-8.
  **Ordering:** before item falling and reach integration.
- **Relevant decisions:** D-1, D-3, D-5, D-6, D-10, D-33.
- **Acceptance signals:** Same-XY items on different supports remain distinct;
  instance identity, quantities and nested contents survive pickup/drop and
  fresh-process save/load. Qualified placement refuses ambiguity according to
  the agreed compatibility policy; render and pick positions agree.
  A pending location-container shell retains its top-level ground identity,
  source page/slot association and authored contents during repositioning and
  reload, without rolling its profile. Preserve the current pickup/realization
  boundary from #2505/#2510/#2522 rather than treating repositioning as pickup.
- **Out of scope:** Building reservations (SI-23), free fall (SI-16),
  carriers (SI-26), or changing strict-player versus lax-AI transfer policy.
- **Open questions:** Q-4; explicit placement/failure rules precede approval.

### SI-23. Preserve building support identity and vertical reservations

- **Outcome:** A placed building occupies the intended support/volume rather
  than reserving every floor in its XY footprint.
- **Scope:** Building placement/support identity, vertical reservation admission
  and commit, placement/ghost and Lua adapters, render/pick height, component
  migration and complete position-dependent consumer inventory. Preserve
  delivered materials, stored item instances and spawn lifecycle state.
- **Phase:** Building placement. **Depends on:** SI-11, SI-8.
  **Ordering:** independent of SI-13 after their shared prerequisites.
- **Relevant decisions:** D-1, D-3, D-5, D-6, D-10, D-15, D-33.
- **Acceptance signals:** Same-XY buildings on separate valid floors reserve
  and restore independently; conflicting physical volumes refuse atomically.
  Save/load, preview/commit and precise support invalidation keep the same
  location without replaying placement rewards or losing material provenance.
- **Out of scope:** Motion, workstation destruction, structural construction
  intent, or enabling incomplete upper-floor content in ordinary gameplay.
- **Open questions:** Q-4's footprint/support admission and clearance.

### SI-2. Ground units continuously on reachable support surfaces

- **Outcome:** Supported units occupy the correct surface continuously,
  including fractional progress on slopes, stairs and roofs.
- **Scope:** Continuous unit position and support identity, spawn/teleport
  adapters, support-qualified resting queries for Lua, integer-height consumer
  inventory, controlled connected-route locomotion, render/hit-test placement
  and movement-phase component migrations. Preserve existing cliff behavior
  while the extended unsupported-fall path is delivered in SI-24.
- **Phase:** Supported locomotion. **Depends on:** SI-11, SI-8.
  **Ordering:** before sweeps, route search and extended visual proof.
- **Relevant decisions:** D-1, D-2, D-3, D-5, D-6, D-22, D-33.
- **Acceptance signals:** Real movement over controlled legal connections has
  continuous foot height and body clearance; pause/restart halfway along a
  connector preserves the exact surface/position. Spawn, teleport, rendering
  and hit testing never silently choose another same-XY support.
- **Out of scope:** General route search, new unsupported-fall/landing injury
  semantics, moving-floor carriers, or production collision activation.
- **Open questions:** Q-4/Q-8 geometry, contact and speed rules; Q-5 recovery
  is delivered in SI-33, not inferred from a successful placement fixture.

### SI-3. Sweep supported unit motion against solid geometry

- **Outcome:** Every admitted movement segment is body-safe, including
  residual-time continuations on a controlled support-qualified route.
- **Scope:** Shared connection admission and swept-volume queries in the real
  mover; walls/posts, placed-building bounds, radius/headroom/step/hazard rules,
  diagonal clearance, per-solid escape exemptions and revision revalidation.
  Keep the shared predicate usable by SI-29's planner.
- **Phase:** Movement safety. **Depends on:** SI-2, SI-23, SI-8.
  **Ordering:** before multi-support search; no backend implementation dependency.
- **Relevant decisions:** D-1, D-2, D-3, D-5, D-6, D-33.
- **Acceptance signals:** Thin barriers cannot be tunnelled through across
  tick sizes; legal breach/stair traversal conserves admitted time under the
  selected metric. A controlled initial overlap may exit only its exempt solid,
  cannot re-enter it, and never gains an exemption by normal penetration.
  Stale geometry refuses/revalidates motion before publication.
- **Out of scope:** General route search (SI-29), load repair (SI-33),
  crowd arbitration or action migration.
- **Open questions:** Q-4/Q-8 clearance, contact response and tolerances;
  the exemption mechanism must support Q-5 without claiming recovery is proven.

### SI-29. Plan bounded routes across distinct connected support surfaces

- **Outcome:** Route search and following reach the selected floor through
  legal connectors, using exactly SI-3's movement admission rules.
- **Scope:** Support-qualified node/goal/visited/parent/cost and waypoint state,
  heuristic/partial-route progress, bounded multi-level search, replan and
  arrival semantics, real mover integration and any route-state migrations.
- **Phase:** Routing. **Depends on:** SI-3.
  **Ordering:** before player intent and action reach.
- **Relevant decisions:** D-1, D-5, D-6, D-33.
- **Acceptance signals:** A destination above the start requires stairs; a
  valid detour may initially increase XY distance. Real acolytes route through
  the breach. Complete, partial, blocked and budget-exhausted results remain
  distinct; wrong-floor XY matches never count as arrival. Edits invalidate
  stale paths, and timing/stall/hold contracts remain intact.
- **Out of scope:** A world-spanning planner, crowd policy or a duplicate
  planner-only collision model.
- **Open questions:** Q-4/Q-8 traversal and numeric rules; freeze these before
  specifying search acceptance and work bounds.

### SI-35. Integrate stair appearances with the structure art contract and tools

- **Outcome:** A stair appearance can be loaded, directionally resolved, played,
  inspected and validated through the existing structure art facilities.
- **Scope:** Stair-specific catalogue and pack-reader adapters, variant identity,
  orientation/camera mapping, construction/destruction resolution, static-mask
  lighting with frame-owned alpha, preview enumeration and completeness checks.
  Preserve existing content identifiers and any persisted variant references;
  include necessary compatibility, contracts and targeted tests in this PR.
  Implement Q-10's agreed isolated-appearance preview/registration mechanism
  without declaring missing production assets or weakening shipped-pack guards.
- **Phase:** Art integration. **Depends on:** SI-11.
  **Ordering:** before all eight stair art deliveries; reuse external
  #2491/#2495/#2519 facilities after their applicable prerequisites land.
- **Relevant decisions:** D-8, D-11, D-12, D-13, D-23, D-34, D-35, D-36.
- **Acceptance signals:** Tests cover the 16 world-orientation/camera mappings
  for each variant, exact lifecycle handoffs, missing/wrong-variant rejection,
  per-variant directional frame counts and correct mask/alpha behavior.
  The real preview can inspect one complete appearance and its clips while
  incomplete families remain unavailable as playable production content.
  Authored damaged appearance identity survives placement, rotation, teardown
  and save/load without borrowing intact clips. Required evidence and docs
  are complete before final review; fixtures do not claim owner art approval.
- **Out of scope:** Artwork generation, generic preview/audit reimplementation,
  new occlusion backend (SI-28/SI-34), unit movement or gameplay activation.
- **Open questions:** Q-10: freeze preview staging before this child's approval
  and before any art child is filed; settle the shared authoring profile before
  generation. Stop and revise this boundary if upstream facilities require
  independent new engine work rather than the bounded stair adapters.

### SI-14. Author and approve the intact northeast stair appearance

- **Outcome:** The intact screen-northeast stair appearance is complete
  and personally approved as one directional asset delivery.
- **Scope:** PixelLab static colour/alpha art, its matching static facemap,
  a construction clip ending pixel-identical to the static and a separately
  authored forward destruction clip starting pixel-identical to it.
  Follow Q-10's shared geometry, canvas/anchor, material and frame-count profile;
  preserve one-tile/one-z repeatable flights with no attached landing.
  Include provenance, asset declarations and required evidence in this art PR;
  use SI-35's staging mechanism until the production family is complete.
- **Phase:** Art. **Depends on:** SI-35.
  **Ordering:** required before SI-28; independent of the other intact directions once
  the common authoring profile is agreed.
- **Relevant decisions:** D-2, D-11, D-12, D-13, D-34, D-35, D-36.
- **Acceptance signals:** Production build, actual preview of this appearance
  and all its frames, owner verdict on every texture/frame, and matching
  validation evidence are retained before final review and landing. The asset
  obeys the shared profile and exact static/lifecycle handoffs, has correct
  binary alpha and lighting masks, and contains no unapproved mirrored fallback.
  Full mixed-flight/in-world/all-camera proof remains required in SI-28.
- **Out of scope:** Engine adapters, other directional assets, partial cutouts,
  attached landing art, rubble or falling-stair animation.
- **Open questions:** Q-10's technical authoring profile must be fixed before
  generation; personal approval of the actual artwork remains required.
  A clearance or silhouette conflict returns to the owner, not a geometry
  change hidden inside this asset.

### SI-36. Author and approve the intact northwest stair appearance

- **Outcome:** The intact screen-northwest stair appearance is complete
  and personally approved as one directional asset delivery.
- **Scope:** PixelLab static colour/alpha art, its matching static facemap,
  a construction clip ending pixel-identical to the static and a separately
  authored forward destruction clip starting pixel-identical to it.
  Follow Q-10's shared geometry, canvas/anchor, material and frame-count profile;
  preserve one-tile/one-z repeatable flights with no attached landing.
  Include provenance, asset declarations and required evidence in this art PR;
  use SI-35's staging mechanism until the production family is complete.
- **Phase:** Art. **Depends on:** SI-35.
  **Ordering:** required before SI-28; independent of the other intact directions once
  the common authoring profile is agreed.
- **Relevant decisions:** D-2, D-11, D-12, D-13, D-34, D-35, D-36.
- **Acceptance signals:** Production build, actual preview of this appearance
  and all its frames, owner verdict on every texture/frame, and matching
  validation evidence are retained before final review and landing. The asset
  obeys the shared profile and exact static/lifecycle handoffs, has correct
  binary alpha and lighting masks, and contains no unapproved mirrored fallback.
  Full mixed-flight/in-world/all-camera proof remains required in SI-28.
- **Out of scope:** Engine adapters, other directional assets, partial cutouts,
  attached landing art, rubble or falling-stair animation.
- **Open questions:** Q-10's technical authoring profile must be fixed before
  generation; personal approval of the actual artwork remains required.
  A clearance or silhouette conflict returns to the owner, not a geometry
  change hidden inside this asset.

### SI-37. Author and approve the intact southeast stair appearance

- **Outcome:** The intact screen-southeast stair appearance is complete
  and personally approved as one directional asset delivery.
- **Scope:** PixelLab static colour/alpha art, its matching static facemap,
  a construction clip ending pixel-identical to the static and a separately
  authored forward destruction clip starting pixel-identical to it.
  Follow Q-10's shared geometry, canvas/anchor, material and frame-count profile;
  preserve one-tile/one-z repeatable flights with no attached landing.
  Include provenance, asset declarations and required evidence in this art PR;
  use SI-35's staging mechanism until the production family is complete.
- **Phase:** Art. **Depends on:** SI-35.
  **Ordering:** required before SI-28; independent of the other intact directions once
  the common authoring profile is agreed.
- **Relevant decisions:** D-2, D-11, D-12, D-13, D-34, D-35, D-36.
- **Acceptance signals:** Production build, actual preview of this appearance
  and all its frames, owner verdict on every texture/frame, and matching
  validation evidence are retained before final review and landing. The asset
  obeys the shared profile and exact static/lifecycle handoffs, has correct
  binary alpha and lighting masks, and contains no unapproved mirrored fallback.
  Full mixed-flight/in-world/all-camera proof remains required in SI-28.
- **Out of scope:** Engine adapters, other directional assets, partial cutouts,
  attached landing art, rubble or falling-stair animation.
- **Open questions:** Q-10's technical authoring profile must be fixed before
  generation; personal approval of the actual artwork remains required.
  A clearance or silhouette conflict returns to the owner, not a geometry
  change hidden inside this asset.

### SI-38. Author and approve the intact southwest stair appearance

- **Outcome:** The intact screen-southwest stair appearance is complete
  and personally approved as one directional asset delivery.
- **Scope:** PixelLab static colour/alpha art, its matching static facemap,
  a construction clip ending pixel-identical to the static and a separately
  authored forward destruction clip starting pixel-identical to it.
  Follow Q-10's shared geometry, canvas/anchor, material and frame-count profile;
  preserve one-tile/one-z repeatable flights with no attached landing.
  Include provenance, asset declarations and required evidence in this art PR;
  use SI-35's staging mechanism until the production family is complete.
- **Phase:** Art. **Depends on:** SI-35.
  **Ordering:** required before SI-28; independent of the other intact directions once
  the common authoring profile is agreed.
- **Relevant decisions:** D-2, D-11, D-12, D-13, D-34, D-35, D-36.
- **Acceptance signals:** Production build, actual preview of this appearance
  and all its frames, owner verdict on every texture/frame, and matching
  validation evidence are retained before final review and landing. The asset
  obeys the shared profile and exact static/lifecycle handoffs, has correct
  binary alpha and lighting masks, and contains no unapproved mirrored fallback.
  Full mixed-flight/in-world/all-camera proof remains required in SI-28.
- **Out of scope:** Engine adapters, other directional assets, partial cutouts,
  attached landing art, rubble or falling-stair animation.
- **Open questions:** Q-10's technical authoring profile must be fixed before
  generation; personal approval of the actual artwork remains required.
  A clearance or silhouette conflict returns to the owner, not a geometry
  change hidden inside this asset.

### SI-39. Author and approve the damaged northeast stair appearance

- **Outcome:** The damaged screen-northeast stair appearance is complete
  and personally approved as one directional asset delivery.
- **Scope:** PixelLab static colour/alpha art, a verified static facemap binding
  (reuse only with silhouette/lighting evidence) and its own forward destruction
  clip starting pixel-identical to that damaged static. This is authored-ruin
  art, not an additional build recipe.
  Follow Q-10's shared geometry, canvas/anchor, material and frame-count profile;
  preserve one-tile/one-z repeatable flights with no attached landing.
  Include provenance, asset declarations and required evidence in this art PR;
  use SI-35's staging mechanism until the production family is complete.
- **Phase:** Art. **Depends on:** SI-35, SI-14.
  **Ordering:** required before SI-28; compare with the matching intact
  appearance for geometry and any proposed mask reuse.
- **Relevant decisions:** D-2, D-11, D-12, D-13, D-34, D-35, D-36.
- **Acceptance signals:** Production build, actual preview of this appearance
  and all its frames, owner verdict on every texture/frame, and matching
  validation evidence are retained before final review and landing. The asset
  obeys the shared profile and exact static/lifecycle handoffs, has correct
  binary alpha and lighting masks, and contains no unapproved mirrored fallback.
  Full mixed-flight/in-world/all-camera proof remains required in SI-28.
- **Out of scope:** Engine adapters, other directional assets, partial cutouts,
  attached landing art, rubble or falling-stair animation, and damaged
  construction clips unless separately made buildable.
- **Open questions:** Q-10's technical authoring profile must be fixed before
  generation; personal approval of the actual artwork remains required.
  A clearance or silhouette conflict returns to the owner, not a geometry
  change hidden inside this asset.

### SI-40. Author and approve the damaged northwest stair appearance

- **Outcome:** The damaged screen-northwest stair appearance is complete
  and personally approved as one directional asset delivery.
- **Scope:** PixelLab static colour/alpha art, a verified static facemap binding
  (reuse only with silhouette/lighting evidence) and its own forward destruction
  clip starting pixel-identical to that damaged static. This is authored-ruin
  art, not an additional build recipe.
  Follow Q-10's shared geometry, canvas/anchor, material and frame-count profile;
  preserve one-tile/one-z repeatable flights with no attached landing.
  Include provenance, asset declarations and required evidence in this art PR;
  use SI-35's staging mechanism until the production family is complete.
- **Phase:** Art. **Depends on:** SI-35, SI-36.
  **Ordering:** required before SI-28; compare with the matching intact
  appearance for geometry and any proposed mask reuse.
- **Relevant decisions:** D-2, D-11, D-12, D-13, D-34, D-35, D-36.
- **Acceptance signals:** Production build, actual preview of this appearance
  and all its frames, owner verdict on every texture/frame, and matching
  validation evidence are retained before final review and landing. The asset
  obeys the shared profile and exact static/lifecycle handoffs, has correct
  binary alpha and lighting masks, and contains no unapproved mirrored fallback.
  Full mixed-flight/in-world/all-camera proof remains required in SI-28.
- **Out of scope:** Engine adapters, other directional assets, partial cutouts,
  attached landing art, rubble or falling-stair animation, and damaged
  construction clips unless separately made buildable.
- **Open questions:** Q-10's technical authoring profile must be fixed before
  generation; personal approval of the actual artwork remains required.
  A clearance or silhouette conflict returns to the owner, not a geometry
  change hidden inside this asset.

### SI-41. Author and approve the damaged southeast stair appearance

- **Outcome:** The damaged screen-southeast stair appearance is complete
  and personally approved as one directional asset delivery.
- **Scope:** PixelLab static colour/alpha art, a verified static facemap binding
  (reuse only with silhouette/lighting evidence) and its own forward destruction
  clip starting pixel-identical to that damaged static. This is authored-ruin
  art, not an additional build recipe.
  Follow Q-10's shared geometry, canvas/anchor, material and frame-count profile;
  preserve one-tile/one-z repeatable flights with no attached landing.
  Include provenance, asset declarations and required evidence in this art PR;
  use SI-35's staging mechanism until the production family is complete.
- **Phase:** Art. **Depends on:** SI-35, SI-37.
  **Ordering:** required before SI-28; compare with the matching intact
  appearance for geometry and any proposed mask reuse.
- **Relevant decisions:** D-2, D-11, D-12, D-13, D-34, D-35, D-36.
- **Acceptance signals:** Production build, actual preview of this appearance
  and all its frames, owner verdict on every texture/frame, and matching
  validation evidence are retained before final review and landing. The asset
  obeys the shared profile and exact static/lifecycle handoffs, has correct
  binary alpha and lighting masks, and contains no unapproved mirrored fallback.
  Full mixed-flight/in-world/all-camera proof remains required in SI-28.
- **Out of scope:** Engine adapters, other directional assets, partial cutouts,
  attached landing art, rubble or falling-stair animation, and damaged
  construction clips unless separately made buildable.
- **Open questions:** Q-10's technical authoring profile must be fixed before
  generation; personal approval of the actual artwork remains required.
  A clearance or silhouette conflict returns to the owner, not a geometry
  change hidden inside this asset.

### SI-42. Author and approve the damaged southwest stair appearance

- **Outcome:** The damaged screen-southwest stair appearance is complete
  and personally approved as one directional asset delivery.
- **Scope:** PixelLab static colour/alpha art, a verified static facemap binding
  (reuse only with silhouette/lighting evidence) and its own forward destruction
  clip starting pixel-identical to that damaged static. This is authored-ruin
  art, not an additional build recipe.
  Follow Q-10's shared geometry, canvas/anchor, material and frame-count profile;
  preserve one-tile/one-z repeatable flights with no attached landing.
  Include provenance, asset declarations and required evidence in this art PR;
  use SI-35's staging mechanism until the production family is complete.
- **Phase:** Art. **Depends on:** SI-35, SI-38.
  **Ordering:** required before SI-28; compare with the matching intact
  appearance for geometry and any proposed mask reuse.
- **Relevant decisions:** D-2, D-11, D-12, D-13, D-34, D-35, D-36.
- **Acceptance signals:** Production build, actual preview of this appearance
  and all its frames, owner verdict on every texture/frame, and matching
  validation evidence are retained before final review and landing. The asset
  obeys the shared profile and exact static/lifecycle handoffs, has correct
  binary alpha and lighting masks, and contains no unapproved mirrored fallback.
  Full mixed-flight/in-world/all-camera proof remains required in SI-28.
- **Out of scope:** Engine adapters, other directional assets, partial cutouts,
  attached landing art, rubble or falling-stair animation, and damaged
  construction clips unless separately made buildable.
- **Open questions:** Q-10's technical authoring profile must be fixed before
  generation; personal approval of the actual artwork remains required.
  A clearance or silhouette conflict returns to the owner, not a geometry
  change hidden inside this asset.

### SI-28. Prove stacked-floor and stair occlusion with manual z slicing

- **Outcome:** SI-6's backend choice passes the extended multi-level visual
  matrix before production scene/picker integration.
- **Scope:** Real stacked placement and continuously supported unit fixtures,
  approved intact and damaged stair art, roof/slab openings, item/building overlap, all facings,
  fractional slope positions and camera-slice clipping/visibility conventions.
- **Phase:** Extended rendering proof. **Depends on:** SI-6, SI-2, SI-13, SI-23,
  SI-14, SI-36, SI-37, SI-38, SI-39, SI-40, SI-41, SI-42.
  **Ordering:** before SI-34 and real player targeting.
- **Relevant decisions:** D-1, D-2, D-5, D-6, D-9, D-11, D-12, D-13, D-22, D-33.
- **Acceptance signals:** Retained offscreen captures and measured costs show
  correct support/foot contact, front/behind ordering, ties and bounded
  cycle handling without holes/seams or hidden bodies across manual slices.
  Intact, damaged and mixed repeated flights retain their own directional
  appearances, joins and lifecycle handoffs without variant substitution.
  Controlled legal routes are sufficient; general route search is not a prerequisite.
- **Out of scope:** Waiting for free falls, workstation wrecks or carriers.
  Those producers supply their own dynamic evidence and join SI-7's full matrix.
- **Open questions:** Q-6/Q-7; a failed extended proof reopens the backend/sizing
  decision before SI-34, not after production activation.

### SI-34. Integrate the proven backend with production scene assembly and picking

- **Outcome:** Actual scene producers and pickers use the proven geometry and
  manual-slice conventions, enabling real selected-support player commands.
- **Scope:** Static/dynamic assembly, producer adapters, stable identity/ties,
  alpha policy and world/unit/building/flora plus ground-item picking. Preserve
  UI layer ordering. Each later motion/remnant owner extends these real adapters
  in its own PR rather than waiting for final activation.
- **Phase:** Production visual integration. **Depends on:** SI-28.
  **Ordering:** before SI-12; distinct from final content activation in SI-7.
- **Relevant decisions:** D-1, D-2, D-5, D-6, D-9, D-33.
- **Acceptance signals:** The extended fixture passes through actual assembly
  and all applicable pickers at four facings. Visible surfaces and pointer hits
  agree at fractional heights and manual slices. Baseline controls, alpha and
  supported frame-layer contracts remain correct; record measured overhead.
- **Out of scope:** Activating shipped collision or absorbing an unbounded GPU
  rewrite. A backend requiring independent producer/pipeline redesigns returns
  to design decomposition before this child is approved.
- **Open questions:** Q-6 must be resolved for this matrix; Q-7 sets bounded
  integration measurements. Additional art invokes Q-10.

### SI-12. Carry selected support through player orders and persistent intent

- **Outcome:** A player can choose either floor and command a unit there,
  preserving that exact destination through interruption, hold, and save/load.
- **Scope:** Support-qualified picking and destination feedback, manual camera
  z-slicing, command admission and move targets, XY-only API compatibility,
  return-to-hold anchors, affected Lua components, and migrations. Rendering
  visibility does not change collision or select a hidden alternative floor.
- **Phase:** Player interaction. **Depends on:** SI-29, SI-34.
  **Ordering:** critical path.
- **Relevant decisions:** D-1, D-5, D-6, D-9.
- **Acceptance signals:** Real pointer commands reach the chosen upper/lower
  surface, a hold returns to the same support, interrupted orders restore, and
  ambiguous legacy calls refuse or use a documented unambiguous adapter.
- **Out of scope:** Automatic cutaway, roof fading, floor following, and action callers.
- **Open questions:** None for the view policy; D-9 resolves Q-9. SI-1's geometry
  contract and SI-28/SI-34's rendered slice evidence determine the technical boundary.

### SI-4. Integrate reachable interaction points and action completion

- **Outcome:** Existing actions approach usable positions and complete only
  when their physical reach policy permits them.
- **Scope:** Shared approach queries, action-specific completion predicates,
  and migration of pickup/transfers at the Lua and authoritative Haskell
  boundaries. Record the work/combat caller inventory for SI-9/SI-30. Preserve
  deferred player approach orders and the strict player versus lax AI transfer
  distinction.
- **Phase:** Gameplay integration. **Depends on:** SI-12, SI-13, SI-23. **Ordering:** critical path.
- **Relevant decisions:** D-1, D-2.
- **Acceptance signals:** Loot a ruin and transfer to a building without
  targeting solid interiors or completing through an intact wall;
  target changes during approach are revalidated at execution.
- **Out of scope:** Work/combat migration (SI-9/SI-30), geometry publication/recovery
  (SI-8/SI-33), and new action content.
- **Open questions:** Q-4; settle pickup/transfer reach semantics before this
  child is drafted.

### SI-9. Migrate work approaches and execution to spatial reach

- **Outcome:** Existing work selects reachable approaches and validates
  physical reach again when the authoritative action executes.
- **Scope:** Construction, crafting, repair and the complete remaining work
  caller inventory; shared SI-4 helpers, page/target qualification and existing
  claim/hold/stall semantics. Include only cohesive helper extraction needed
  to remain within the existing module budgets.
- **Phase:** Work consumers. **Depends on:** SI-4.
  **Ordering:** before construction completion; independent of melee migration.
- **Relevant decisions:** D-1, D-2, D-33.
- **Acceptance signals:** A blocked nearest side leads to a reachable side;
  station work/repair cannot complete through a wall or on the wrong floor.
  Geometry/target changes during approach invalidate execution. The caller
  inventory records a tested adapter or a justified safe shared path for each.
- **Out of scope:** Melee (SI-30), new work content, or an independent subsystem
  redesign hidden in a consumer migration.
- **Open questions:** Q-4's work-specific reach policies and consumer inventory.

### SI-30. Migrate melee approaches and execution to spatial reach

- **Outcome:** Applicable melee actions approach a reachable physical position
  and cannot strike through intact barriers solely because XY distance is small.
- **Scope:** Combat approach and authoritative completion checks using SI-4;
  target motion/revision revalidation and existing attack/hold authority.
- **Phase:** Combat consumers. **Depends on:** SI-4.
  **Ordering:** independent of SI-9; required before SI-7.
- **Relevant decisions:** D-1, D-21, D-33.
- **Acceptance signals:** Same-floor legal attacks work, wall-separated or
  wrong-floor attacks fail physical reach, and moving targets are rechecked.
  Approach migration never creates an unsolicited attack or clears a hold.
- **Out of scope:** Ranged sight/perception redesign and unit crowd avoidance.
- **Open questions:** Q-4's melee reach policy; preserve existing combat rules
  outside the agreed physical reach boundary.

### SI-18. Derive structural connections and support after edits

- **Outcome:** Connected overhangs remain supported; isolated pieces and groups
  with no path to terrain support are classified for falling.
- **Scope:** Authored attachment geometry, placed-piece connection graph,
  terrain roots, affected-component updates under committed geometry revisions,
  bounded work and residency handling, reconstruction on load, and diagnostics.
  Keep the support graph distinct from movement routes and rendering adjacency.
- **Phase:** Structural support. **Depends on:** SI-10, SI-11, SI-8.
  **Ordering:** critical path.
- **Relevant decisions:** D-5, D-10, D-16, D-17.
- **Acceptance signals:** One-sided horizontal overhang holds; unconnected
  pieces and mutually connected but detached clusters are unsupported; removing
  a bridge splits anchored/detached components correctly. Results agree across
  ordinary/U seams, fractional placements, reload and iteration orders.
  Unavailable neighbor data never masquerades as confirmed disconnection.
- **Out of scope:** Falling integration (SI-19), strength/weight/stress simulation.
- **Open questions:** Q-11's precise attachment and graph/residency contract;
  measured work bounds are required before production activation.

### SI-19. Simulate and persist unsupported structural-group falls

- **Outcome:** A detached connected section moves vertically without rotation
  and lands intact as a whole at its first valid load-bearing contact, with
  its pieces usable at their new physical heights.
- **Scope:** Stable group/piece ownership, continuous common motion, relative
  offsets, contact/reattachment, in-flight split/removal and edit-replay
  reconciliation; component migrations, eviction and render/pick adapters.
  Fixture evidence for this slice uses unoccupied groups.
- **Phase:** Structural motion. **Depends on:** SI-18, SI-8, SI-34.
  **Ordering:** independent of workstation destruction; before carriers.
- **Relevant decisions:** D-5, D-6, D-10, D-16, D-17, D-19, D-33, D-39, D-40.
- **Acceptance signals:** Supported overhangs remain fixed; detached sections
  fall together, never publish duplicate static/moving copies, and preserve
  identity through reload and seams. Splitting/removing a piece in flight obeys
  the selected contact policy. A unit underneath pauses the whole group under
  D-39; clearing it resumes motion without false landing/reattachment. These
  fixtures need no occupants riding the group. Uneven-terrain and lower-floor/
  roof fixtures stop every member at the earliest valid contact of any piece;
  remaining connected overhangs stay put without tipping, rotating or settling
  independently. A fractional landing stays fractional, and removing its last
  support makes the section fall again. Results are independent of piece order
  and tick partitioning, including fresh-process mid-fall reload. Landed
  floors/stairs/walls retain physical roles and reachable geometry; no rubble
  conversion occurs.
- **Out of scope:** Occupied-group activation, carrier integration (SI-26/SI-27),
  structural strength, tumbling, new art or impact/crushing damage.
- **Open questions:** Q-11 precise contact/reattachment, moving-group contact and
  splitting; Q-4 absent-support
  outcomes. Own dynamic visual evidence; do not wait for SI-7 to adapt producers.

### SI-15. Designate and preview construction on a selected support level

- **Outcome:** Player construction intent identifies an exact support/height,
  while Lua-authored fractional placements remain unchanged.
- **Scope:** Support-qualified designations/previews, stair orientation,
  whole-z player admission above the fractional primitive, selection/precise
  removal identity, component and Lua migrations for durable intent.
- **Phase:** Construction intent. **Depends on:** SI-12, SI-11, SI-14, SI-36,
  SI-37, SI-38.
  **Ordering:** before worker completion and deconstruction.
- **Relevant decisions:** D-5, D-6, D-8, D-9, D-10, D-11, D-12, D-33, D-38.
- **Acceptance signals:** Choose either storey through real input; preview and
  saved designation agree on the piece/anchor. Reload retains unfinished intent;
  whole-z admission never rounds existing fractional Lua content. Removal
  targeting names only the selected piece, not every same-XY slot. Designating
  the last wall of a room is not rejected solely for enclosing its occupants.
- **Out of scope:** Worker completion/99% wait (SI-31), deconstruction work
  (SI-22/SI-32), or exposing incomplete player content before SI-7.
- **Open questions:** Q-4 placement/approach admission; Q-10's approved art
  scopes still require delivered assets and owner signoff.

### SI-31. Complete construction with coherent support and occupied-site waiting

- **Outcome:** Workers finish valid structural pieces once, or retain an
  obstructed site at 99% until its final solid volume clears.
- **Scope:** Reachable build approaches, material/progress accounting, bounded
  rechecks, structural attachment and commit publication through SI-8.
  Preserve all partial/99% states and completion ownership through migrations.
- **Phase:** Construction execution. **Depends on:** SI-15, SI-9, SI-18, SI-8.
  **Ordering:** before deconstruction and activation.
- **Relevant decisions:** D-8, D-10, D-14, D-16, D-23, D-24, D-33, D-38.
- **Acceptance signals:** Build the stacked scene with real workers. Unfinished
  walls remain passable and unfinished stairs/floors cannot provide work access,
  support or an overhang anchor. Completion activates every physical role from
  one commit. An obstructing held unit causes a persistent 99% wait without
  forced displacement; another-storey nonintersection does not block completion.
  Sealing a non-intersecting occupant or worker inside a room completes normally;
  no escape-route prerequisite, forced movement or collision exemption is added.
  Reload/rechecks never consume materials or fire completion effects twice.
- **Out of scope:** Deconstruction, new unrelated buildings or activating
  collision before all SI-7 prerequisites.
- **Open questions:** Q-4 attachment/approach legality and bounded completion
  rechecks. D-23/D-24 settle activation/occupancy; D-38 settles enclosure.

### SI-22. Deconstruct intact structures and buildings with durable worker progress

- **Outcome:** One normal worker action deconstructs exact intact targets
  with resumable work and 100% original construction-material recovery at the
  initial tunable baseline.
- **Scope:** Shared structure/building target adapter, designation/picking,
  reach and claims, work/rate/cancellation policy, durable progress, target
  disappearance/replacement handling and exact-once removal/recovery.
  Preserve source identity and original-material provenance for SI-32's extension;
  name the tunable recovery values and define authored/legacy-material accounting.
- **Phase:** Deconstruction foundation. **Depends on:** SI-31, SI-4, SI-13, SI-23.
  **Ordering:** before destroyed-target clearing; no wreck prerequisite.
- **Relevant decisions:** D-9, D-14, D-19, D-29, D-30, D-33, D-41.
- **Acceptance signals:** Real workers clear selected intact structures and
  buildings, leaving another-storey target unchanged. Interrupt/reload preserves
  progress; completed removal and recovery occur exactly once. Material instances
  retain the agreed identity/condition rules and return the full original
  construction-material quantities under D-41's intact baseline. Occupancy is not a demolition veto;
  the committed support-loss boundary is exercised and consumers join before SI-7.
- **Out of scope:** Destroyed-target adapters/reduced yield (SI-32), structural
  rubble, repair or a separate fast demolition path.
- **Open questions:** Q-4's work baseline and authored/legacy provenance;
  D-41 settles the recovery percentage. Recheck for a newly
  delivered workflow before introducing one; do not double-spill live cargo
  or silently establish a new general cargo-destruction policy.

### SI-24. Simulate continuous unit falls and apply existing landing injuries

- **Outcome:** Unsupported units descend to the first eligible lower surface,
  with existing falling animation and full normal landing consequences.
- **Scope:** Continuous unit fall state, contact/landing queries, pending
  injury/drop accounting, order/animation transitions and component migrations.
  Consume committed support loss and expose one boundary for SI-27's carriers.
- **Phase:** Unit free fall. **Depends on:** SI-2, SI-3, SI-8.
  **Ordering:** before unit carriers and activation, independent of item motion.
- **Relevant decisions:** D-6, D-14, D-32, D-33.
- **Acceptance signals:** Terrain, floor, stair and fractional landings use
  the first valid support; edits during descent are revalidated. Integer-drop
  calibration is retained and fractional drops are not snapped. Fresh-process
  reload before/during/after landing never loses or duplicates injuries, move
  cancellation or animation transitions.
- **Out of scope:** A new injury model, carried units (SI-27), or damage from
  being struck/crushed by another falling object (SI-20).
- **Open questions:** Q-4's contact, absent-support and unavailable-data outcomes;
  preserve the existing physical injury calibration.

### SI-16. Simulate and persist loose-item falls between support surfaces

- **Outcome:** A loose item that loses support descends physically and lands
  on the first valid support while keeping the same identity and contents.
- **Scope:** Continuous item height and fall phase, support-loss admission,
  descent/landing queries, one authoritative update owner on the existing
  simulation clock, render/pick publication, component migrations, and
  save-boundary classification. SI-13 owns resting placement identity; SI-16
  extends that state through flight, without a parallel renderer-owned fall.
  Consume SI-8's committed support changes in this PR, before activation.
  Expose one authoritative item-motion/support boundary for SI-26's D-31
  carrier integration; the later slice supplies moving structural supports.
- **Phase:** Item motion. **Depends on:** SI-13, SI-8, SI-34.
  **Ordering:** critical path before occupied-floor removal activates.
- **Relevant decisions:** D-5, D-6, D-10, D-14.
- **Acceptance signals:** Controlled support-loss fixtures land on a lower floor,
  stair or terrain, including fractional heights. Save/restart mid-fall preserves
  physical state; removal/replacement of a prospective landing is revalidated.
  Ground ID, instance ID, quantity and nested contents are unchanged and unique.
  Include a pending container shell: falling and landing do not realize its
  profile, change provenance or move it into a carrier inventory. Mid-fall
  reload must preserve the shell's existing ground ownership and slot binding.
  Unknown geometry is distinct from a known absence of lower support.
- **Out of scope:** Item breakage, impact-triggered spilling, rigid-body tumbling,
  furniture falling (SI-17), structural-piece collapse, or changing unit injury rules.
- **Open questions:** Q-4's no-lower-support and unavailable-data outcomes; the
  introducing PR names and classifies the authoritative motion owner.

### SI-17. Simulate and persist shared placed-building fall motion

- **Outcome:** One motion owner carries placed-building identity and owned
  payloads through continuous descent, contact waits and a precise landing handoff.
- **Scope:** Multi-tile support/contact, reservation/occupancy release and
  publication, render/pick adapters, support-loss and subsequent-fall handling,
  component migrations and state classification. Expose lifecycle admission
  and landing hooks for SI-25/SI-43; establish no second inventory or service
  owner. Use controlled fixtures until the complete lifecycle activates in SI-7.
- **Phase:** Placed-object motion. **Depends on:** SI-23, SI-8, SI-34.
  **Ordering:** before shared wrecks and both gameplay lifecycle adapters.
- **Relevant decisions:** D-5, D-6, D-10, D-15, D-33, D-39, D-42, D-43, D-45.
- **Acceptance signals:** Fixtures descend to the first valid support with one
  physical owner, no duplicate collider/gravity or placement/spawn replay.
  Identity, payload ownership, contact state and real position survive eviction
  and mid-fall restart. D-39 unit contact pauses without damage/displacement;
  bounded rechecks resume after clearance, never treating the wait as landing.
  Each completed fall emits one durable landing handoff across reload. Visual
  and pick evidence covers descent/contact/landing at all facings and heights.
- **Out of scope:** Production content admission, job/service teardown, cargo
  spill, persistent wreck playback/schema (SI-21), carriers, new art or rotation.
  Fixture staging does not authorize intact furniture landings in normal play.
- **Open questions:** Q-4 multi-tile support, numeric pause/resume and missing
  support/data behavior. Normal gameplay remains gated until SI-7.

### SI-21. Retain building destruction endpoints as shared persistent wrecks

- **Outcome:** A common wreck owner plays approved destruction once and retains
  its visible final frame as a persistent solid remnant for stations and furniture.
- **Scope:** Explicit fixture admission into landing-to-playback-to-endpoint
  state; stable remnant identity, actual support/height, facing-aware declared
  clips, rendering, calibrated bounds, route/sweep publication, picking and
  source-definition/material provenance for SI-32. Include classification and
  component migrations. Reuse SI-17's motion boundary for subsequent support
  loss; never retain a floating picture or revive services. Expose the shared
  admission contract consumed by SI-25/SI-43, without embedding their teardown.
- **Phase:** Persistent wreck lifecycle. **Depends on:** SI-17, SI-34.
  **Ordering:** before workstation and non-workstation lifecycle adapters.
- **External prerequisites:** Suitable approved real destruction art for every
  definition used by its acceptance fixtures, including representative station
  and non-station wrecks. Existing station owners are #2497/#2503/#2504/#2507;
  non-station candidates use #2498/#2509/#2511/#2501. Reconcile affected endpoint
  contracts before child approval; verify actual delivery and personal all-facing
  signoff before this PR's final review. Stop for missing art, never substitute
  placeholders or duplicate asset issues. SI-25/SI-43 separately require complete
  art coverage for every definition they admit.
- **Relevant decisions:** D-26, D-27, D-28, D-29, D-37, D-42, D-43, D-45.
- **Acceptance signals:** The same owner accepts station and non-station
  fixture identities with no duplicated schema or render path. Playback runs
  once, pins the correct-facing visible endpoint and retains solid actual-volume
  geometry. Pause, hidden pages, eviction and fresh-process reload preserve
  progress/completed wrecks without expiry, replay, duplicate items or services.
  Non-intersecting units on another floor remain unaffected. Removing support
  invokes the agreed fall behavior without replaying initial cargo/teardown.
  Prove source identity for SI-32 and offscreen appearance at every facing.
  Ordinary demolition still expires its transient effect and leaves no wreck.
- **Out of scope:** Job/service teardown, storage spill, production fall admission,
  worker-paced clearing (SI-32), new art, structural rubble or repair/rebuilding.
- **Open questions:** Q-4 calibrated wreck bounds/support-loss behavior and Q-10
  real-art prerequisites. D-28/D-43 already settle blocking and D-29 deconstruction.

### SI-25. Destroy workstations on landing and scatter owned contents

- **Outcome:** At actual landing, a workstation becomes destroyed, loses every
  job/function and scatters stored contents once onto its landing tile/support.
- **Scope:** Extend SI-17's physical owner with workstation fall admission,
  D-44's persisted airborne job/service pause, and atomic landing destruction,
  bill/job/claim/service teardown and stale-worker refusal.
  Publish the remnant through SI-21's shared admission boundary. Factor the
  landing/payload transaction for reuse by SI-43, without implementing its
  power/spawn/storage-specific adapters. Persist pending cargo,
  material-recovery provenance and landing-spill state;
  integrate scatter publication with SI-13/SI-16 without destroy/respawn copies.
- **Phase:** Workstation destruction. **Depends on:** SI-17, SI-16, SI-21.
  **Ordering:** after shared wrecks; before SI-43 and placed-object carriers.
- **Relevant decisions:** D-15, D-26, D-30, D-33, D-39, D-42, D-44, D-45.
- **Acceptance signals:** Fall onset and a D-39 contact wait do not mark the
  station destroyed. Jobs remain paused with no work progress/output or active
  services, including across save/load and contact waits. Actual landing
  discards jobs and commits destruction once; stale workers cannot execute
  during the pause or after destruction.
  Full cargo quantities, identities and
  nested contents scatter at the true landing support exactly once across
  save boundaries. Recoverable materials remain distinct from cargo. Reload
  never resurrects jobs/services or replays completion/spawn effects. D-39's
  contact wait keeps pending cargo intact; only actual landing publishes it.
- **External prerequisites:** Approved real destruction art for all four admitted
  stations from #2497/#2503/#2504/#2507, reconciled with D-37 before child
  approval and delivered with personal signoff before PR final review.
- **Out of scope:** Non-workstation service adapters (SI-43), ordinary demolition
  policy changes, implementation of persistent animation
  endpoint (SI-21), or impact/crushing damage.
- **Open questions:** Q-4's landing/scatter anchor and bounded offsets,
  especially for multi-tile stations; material provenance must support SI-32.

### SI-43. Integrate non-workstation building falling and landing destruction

- **Outcome:** Cargo Holds, Solar Panels, Batteries and Portals use the shared fall and
  wreck owners with correct storage/service suspension and destruction cleanup.
- **Scope:** Bounded adapters for the confirmed Cargo Hold, power-node and Portal
  content roster. Reuse SI-17 motion, SI-25's factored landing/payload transaction,
  SI-16 loose-item publication and SI-21 wreck admission. Wire real eligibility,
  execution, knowledge/selection and destruction consumers; preserve source
  material provenance. Own any introduced classified state, component migrations,
  tests and required contracts in this PR. Do not implement a second fall engine,
  inventory owner, wreck schema or destruction transaction.
- **Phase:** Non-workstation lifecycle. **Depends on:** SI-17, SI-16, SI-21, SI-25.
  **Ordering:** after the shared lifecycle boundaries; before carriers/activation.
- **External prerequisites:** For each admitted definition, reconcile visible
  destruction endpoints in #2498 (Cargo Hold), #2509 (Solar), #2511 (Battery)
  and #2501 (Portal) before child approval; require delivered declared clips and
  personal all-facing signoff before PR final review. Reuse the existing art
  owners and chosen supply paths, without generating art in this code slice.
- **Relevant decisions:** D-15, D-29, D-30, D-39, D-41, D-42, D-43, D-45, D-46.
- **Acceptance signals:** All four definitions suspend services at fall onset
  and through contact waits: no storage transfers, solar generation, battery
  charge/discharge or portal spawning. Actual landing permanently retires
  functionality, scatters stored items in full once and admits one wreck. Test
  direct execution bypasses and stale references, not only UI eligibility.
  Mid-fall/wait/landing reload and eviction preserve identities, owned cargo and
  service state without duplicate spill, power participation or portal spawns.
  Each landing admits exactly one solid shared wreck with original-material
  provenance and no live service. Offscreen evidence uses the approved art;
  final clearing remains SI-32. Existing ordinary demolition stays unchanged.
- **Out of scope:** New motion or wreck infrastructure, art generation,
  redesigning power networks or portal spawning, new furniture content,
  carrier integration, worker deconstruction, charge-to-material salvage.
- **Open questions:** D-46 resolves content mapping and service/cargo policy.
  Q-4 retains contact and landing/scatter anchors; Q-10 owns real art/signoff.
  SI-22/SI-32 settle original-material provenance for
  item-placed nodes and the zero-work portal. If three consumer families do not
  fit as bounded adapters to the shared transaction, split again before approval
  rather than absorb missing infrastructure.

### SI-26. Carry loose items and placed objects on falling structural groups

- **Outcome:** Items and placed objects descend with their actual supporting
  pieces, retaining one owner through carrier loss and landing.
- **Scope:** Item/building support association and relative placement, shared
  motion updates, loss-to-independent-fall transitions and migrations.
  A carried workstation invokes SI-25 destruction at actual landing, with its
  landing payload handled once through that same lifecycle. D-43 retains the
  structural carrier intact; furniture invokes SI-43 and becomes a shared SI-21
  wreck on that landed surface. Do not duplicate either adapter's teardown.
- **Phase:** Object carriers. **Depends on:** SI-19, SI-16, SI-21, SI-25, SI-43.
  **Ordering:** before occupied-group activation and destroyed-target clearing.
- **Relevant decisions:** D-15, D-17, D-26, D-31, D-33, D-39, D-40, D-42, D-43, D-44, D-45, D-46.
- **Acceptance signals:** Loose items ride down without scattering, double
  gravity or lost identity/contents and remain on the intact landed surface.
  Furniture becomes a solid final-frame wreck on landing (D-43). Carried
  stations pause jobs/services during descent and discard them at landing
  destruction under D-44. Piece removal, eviction and mid-fall reload transfer association
  once and never duplicate cargo, objects or colliders. D-39's whole-group
  pause retains object/support associations and never causes a premature spill.
  A pending container shell remains a top-level ground item while physically
  riding a support; carrier association is not cargo insertion or profile
  realization. Carrier removal and fresh-process reload preserve its provenance.
- **Out of scope:** Unit carriers, loose-item impact damage and new animations.
- **Open questions:** Q-11's carrier/contact mechanics; Q-4's support admission
  and landing anchors. D-31's loose-item behavior and D-46's building lifecycle
  policy are settled; do not reopen them as carrier implementation choices.

### SI-27. Carry units on falling floors with full cliff-equivalent injury

- **Outcome:** Carried units use the existing falling animation and receive
  full ordinary self-fall injury on landing, not protection from the carrier.
- **Scope:** Unit/carrier association, relative placement, real world-space
  drop accounting and SI-24's landing/injury boundary; loss-to-free-fall
  transition, render/pick publication and component migrations.
- **Phase:** Unit carriers. **Depends on:** SI-19, SI-24.
  **Ordering:** independent of object carriers after shared prerequisites.
- **Relevant decisions:** D-14, D-17, D-19, D-32, D-33, D-39, D-40.
- **Acceptance signals:** Compare carried and cliff falls for the same unit
  and world-space drop, including fractional heights and mid-flight support
  removal. Animation and full landing consequences agree; save/load or a
  carrier handoff never resets, duplicates or bypasses accumulated drop/injury.
  A carrier paused by another unit below retains its occupants' fall accounting
  without early landing injury; carried occupants do not obstruct their own
  carrier. Resume and land with full consequences after the D-39 wait clears.
- **Out of scope:** New injury rules, ragdolls, structural rubble or SI-20's
  separate damage to struck units.
- **Open questions:** Q-4/Q-11 contact and landing timing; D-32 already fixes
  the animation and full-damage policy.

### SI-32. Deconstruct destroyed targets at intact speed with half recovery

- **Outcome:** SI-22's normal worker action clears destroyed structures and
  building wrecks at intact-equivalent speed, returning 50% of original
  construction materials at the initial tunable baseline.
- **Scope:** Exact destroyed-target adapters, work/progress parity, agreed
  integer rounding/material selection, source provenance and exact-once
  removal/recovery. Include support-qualified wreck and structural targets;
  compose with moving-group edits, carrier loss and D-43's furniture wrecks;
  structural groups themselves remain intact. Do not invent rubble art.
- **Phase:** Destroyed-target clearing. **Depends on:** SI-22, SI-21, SI-26, SI-27.
  **Ordering:** before final activation.
- **Relevant decisions:** D-9, D-14, D-26, D-28, D-29, D-30, D-33, D-41, D-42.
- **Acceptance signals:** Matched intact/destroyed targets and workers have
  equal work/rates/time. Wrecks block until committed clearing, then vanish once.
  Reload retains progress and material choice; odd/even/single-item yields
  obey D-41's 50%-of-original-materials baseline and the separately agreed
  rounding/selection policy without changing condition or reducing the prior
  full cargo spill. No respill, recursive wreck, ghost collider or duplicate payout.
- **Out of scope:** Generating structural rubble, repair/rebuilding, new art
  or destruction-specific work-rate modifiers.
- **Open questions:** Q-4 integer recovery/selection; use SI-22's shared original-
  material basis and D-41's tunable rates. D-43 requires furniture wreck adapters
  and art coverage before approval; fallen structural groups remain intact and
  use SI-22's intact target path.

### SI-33. Restore spatial state and recover pre-existing unit overlaps

- **Outcome:** Whole-session restoration reconciles spatial owners and applies
  D-3's bounded unit overlap recovery without rejecting a save for geometry.
- **Scope:** Q-5 exempt-and-leave, bounded same-page nudge and embedded/reported
  outcomes; exemption reconstruction/revocation, page/session replacement and
  classified derived-view rebuild. Exercise introducing owners' real codecs,
  never postpone their migration into this slice.
- **Phase:** Restoration integration. **Depends on:** SI-8, SI-29, SI-12, SI-24.
  **Ordering:** before activation; independent of later wreck schema ownership.
- **Relevant decisions:** D-3, D-5, D-6, D-14, D-32, D-33.
- **Acceptance signals:** Fresh-process fixtures cover every Q-5 outcome and
  no-nearby-valid-position case, exact floor/intent restoration, no stale-session
  references or barrier-crossing nudge. Geometry overlap never rejects load.
  Transaction failure behavior and derived/durable classification remain correct.
- **Out of scope:** Replacing the save/load transaction, inventing missing
  per-owner codecs, or guaranteeing every embedded unit can be freed.
- **Open questions:** Q-5 search bound, exemption lifecycle and log fields;
  Q-4 distinguishes missing support from unavailable data.

### SI-7. Activate the integrated physical world and pass the full acceptance matrix

- **Outcome:** Normal content enables the complete initial non-crowd physical
  world with matching movement, actions, rendering, picking and persistence.
- **Scope:** Final content-default activation and cross-owner integration,
  complete reference matrix, dense-scene/rebuild/memory budgets and retained
  owner visual verdict. Fix composition defects here, not absent foundational
  migration, motion, construction or rendering implementations.
- **Phase:** Final activation. **Depends on:** SI-34, SI-30, SI-32, SI-33.
  **Ordering:** after every initial-scope branch; their transitive closure is
  required, including approved stair and all eight buildings' external destruction art.
- **Relevant decisions:** D-1, D-2, D-5, D-6, D-8, D-33.
- **Acceptance signals:** Reference steps 1–3 and 5–10 pass with real player
  commands/workers, edits, mid-fall saves, old saves, carrier injuries, station
  teardown/spill, blocking wrecks and deconstruction. All four facings/manual
  slices, actual pickers and numerical performance limits pass in this PR with
  reproducible evidence and owner signoff. Step 4 follows in SI-5.
- **Out of scope:** Crowd behavior (SI-5), impact/crushing of struck units
  (SI-20), missing art, or treating a fixture-only pass as activation evidence.
- **Open questions:** Q-6 verdicts and Q-7 budgets must already be settled;
  all earlier child gates and external asset approvals must be satisfied.

### SI-5. Add local avoidance and bounded conflict resolution between units

- **Outcome:** Units avoid occupied space and resolve narrow-passage conflicts.
- **Scope:** Nearby-unit queries, vertical separation, priority/yield policy,
  progress limits, deterministic symmetric cases including staircase traffic.
- **Phase:** Crowds. **Depends on:** SI-7. **Ordering:** not on the initial integration critical
  path; unit overlap is today's behaviour and activation does not wait for it.
- **Relevant decisions:** D-1, D-20, D-21.
- **Acceptance signals:** Acceptance step 4 of the reference sequence:
  crossing paths, following traffic, and opposing doorway traffic have
  bounded, specified outcomes without using overlap or body shrinking to
  squeeze past. Friendly units wait/yield, and held units retain their holds;
  enemies block without cooperative yielding or avoidance-created attacks.
  Impossible passes terminate under the chosen blocked/stall policy. Capture
  with the same fixture and facings as SI-7.
- **Out of scope:** Rigid-body pushing, ragdolls, or new animations.
- **Open questions:** Q-2's priority/retry details, Q-4, Q-7; friendly/enemy
  encounter policy is settled by D-20/D-21.

### SI-20. Apply falling-object impact and crushing damage to units

- **Outcome:** Units struck by falling furniture or structural groups receive
  impact/crushing consequences under the agreed injury policy.
- **Scope:** Contact-to-injury integration for SI-17/SI-19 motion and the
  SI-25/SI-43 lifecycle adapters, bounded
  victim queries, repeated-contact accounting, injury/death lifecycle, and
  persistence classification/migrations for any new durable state. Required
  contracts and behavioral evidence stay in this implementation PR.
- **Phase:** Later damage follow-on, deferred until initial falling is active.
  **Depends on:** SI-17, SI-19, SI-7. **Ordering:** not on the initial
  falling or stacked-floor activation critical path; no earlier slice depends
  on this mechanic.
- **Relevant decisions:** D-15, D-17, D-18, D-39.
- **Acceptance signals:** Furniture and detached structural groups damage a
  struck unit according to the chosen policy, not a merely screen-overlapping
  unit on another elevation. Tick partitioning, camera slicing and mid-contact
  save/restart do not duplicate impacts; sustained contact follows the agreed
  crushing rule. Replace D-39's temporary pause response and reconcile saved
  contact-wait states without duplicate damage or permanent suspended exceptions.
  Existing unit self-fall injuries remain separately correct.
- **Out of scope:** Loose-item impact damage, structural fracture, furniture
  destruction, contents spilling, rigid-body rotation, or new art.
- **Open questions:** Q-12; resolve before this slice is presented for approval,
  without reopening D-18's approval of the mechanic or delaying earlier motion.
