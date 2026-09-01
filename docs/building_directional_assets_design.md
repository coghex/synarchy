# Directional building and structure lifecycle assets design

Synarchy's building art currently assumes one south-facing image for every
camera orientation. The rejected Kitchen construction sequence exposed that as
a system-level omission: before more buildings are authored, the game needs a
clear contract for which directional and lifecycle assets every building owns,
which scene elements remain separate structure art, and how those assets are
validated and reviewed.

The same destruction rule covers structure pieces: every building and every
structure requires a dedicated forward-authored destruction animation. The
structure renderer's already-settled camera-facing identity remains its own
contract; this arc adds the missing destruction lifecycle and art.

Design state: `exploring`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [ ] EPIC. Give buildings directional lifecycle assets and every constructible dedicated destruction art
- [ ] BDA-1. Introduce camera-facing building asset declarations and distinct lifecycle roles
- [ ] BDA-2. Render and hit-test the correct building view at every camera facing
- [ ] BDA-3. Add a non-looping building destruction presentation lifecycle
- [ ] BDA-4. Make the building preview inspect every direction and lifecycle role
- [ ] BDA-5. Author the directional Workbench asset set
- [ ] BDA-6. Author the directional Cargo Hold asset set
- [ ] BDA-7. Author the directional Acolyte Portal asset set
- [ ] BDA-8. Author the directional Furnace asset set
- [ ] BDA-9. Author the directional Machine Shop asset set
- [ ] BDA-10. Redesign and author the directional Kitchen asset set
- [ ] BDA-11. Author the directional Solar Panel asset set
- [ ] BDA-12. Author the directional High-Voltage Battery asset set
- [ ] BDA-13. Enforce the complete building asset contract before play

## Epic contract

- **Goal:** Every gameplay building presents the correct authored view as the
  camera rotates and has an explicit, reviewable asset inventory for each
  lifecycle it supports, without baking room walls or floors into an indoor
  fixture; every building and structure piece also has dedicated destruction
  art.
- **Done when:** All eight shipped building definitions use the settled
  directional schema; construction, appearance, built, and destruction roles
  resolve consistently at all four camera facings; destruction has settled
  gameplay timing for buildings and structures; the preview exposes the
  complete matrix; and automated guards reject incomplete or inconsistent
  building or structure destruction art.
- **Users and operators:** Players rotating the world camera, artists adding
  buildings, reviewers approving generated art, and maintainers extending the
  building catalog.
- **Arc label:** `art` and `construction` are existing labels; no new label is
  proposed yet.

## Current state and evidence

Verified against `b4631ca6` on 2026-08-31:

- The camera has exactly four 90-degree facings: `FaceSouth`, `FaceWest`,
  `FaceNorth`, and `FaceEast` (`Engine.Graphics.Camera`).
- `renderBuildingQuads` already receives the current `CameraFacing`, but
  `Building.Render.pickBuildingFrame` always looks up `DirS`. Camera facing
  changes the building's world/sort coordinates, not its texture.
- `BuildingAnimation` uses `Map Direction ...`, but its type comment says the
  map is intentionally a singleton. The YAML loader ignores each textual
  direction name and repeatedly inserts every frame list under `DirS`; a file
  declaring more than one direction would silently retain only one list.
- Static building art is one `bdTexture`, loaded from one required YAML
  `sprite` path. Ghosts and pre-material construction silhouettes also use that
  one texture.
- `BuildingInstance` and its save snapshot contain no building orientation.
  Adding player-rotatable placement would therefore require new instance,
  placement, persistence, and compatibility behavior. Merely selecting a
  camera-facing view does not.
- `BuildingActivity` has only `Appearing` and `Built`. Positive-`build_work`
  construction and zero-work timed appearance are overloaded onto
  `Appearing`; open issue #1853 already proposes separating those meanings, but
  its current one-direction completeness guard would freeze the obsolete
  `frames.default` convention.
- `BuildingDestroy` immediately deletes the instance and performs knowledge and
  power cleanup. There is no destroying activity, retained render effect, or
  destruction timing. The checked-in `cargo_hold_S/demolish` and
  `furnace/demolish` folders are preview-only assets that no gameplay YAML or
  renderer consumes.
- The building preview treats each animation directory as one list entry. It
  has no building direction strip or lifecycle matrix, and deliberately exposes
  undeclared numbered directories such as `demolish`.
- The eight gameplay definitions and their currently declared roles are:

  | Building | Work | Declared gameplay art |
  |---|---:|---|
  | Acolyte Portal | 0 | 16-frame appearance, 8-frame built loop, static fallback |
  | Cargo Hold | 240 | 4-frame construction, static built; 4 undeclared demolition frames |
  | Furnace | 180 | 8-frame construction, static built; 4 undeclared demolition frames |
  | High-Voltage Battery | 0 | static built only |
  | Kitchen | 100 | static built only |
  | Machine Shop | 200 | static built only on master; PR #2023 adds four south-facing construction frames |
  | Solar Panel | 0 | static built only |
  | Workbench | 120 | 4-frame construction, static built |

- Master therefore contains 48 gameplay-selected building images when each
  default and declared animation frame is counted, plus eight undeclared
  demolition frames. A strict four-view conversion would require 192 gameplay
  views, or 224 including those destruction sequences, before adding missing
  Kitchen/Machine Shop/destruction art. This is an epic, not one reviewable art
  PR.
- Kitchen's current `default.png` depicts a complete stone-walled room corner
  and floor around its hearth and counter. The project owner rejected carrying
  that enclosure into the construction sequence: the Kitchen is an indoor
  fixture, while walls, floors, and other room structure are separate assets.
  Workbench and Machine Shop already read as isolated fixtures; Cargo Hold,
  Furnace, Battery, and Solar Panel read as self-contained installations.
- Existing tracker overlap is real but not a duplicate of this arc. Epic #1837
  and children #1845/#1848/#1849/#1850/#1853 cover designation ghosts and
  south-facing construction progress. Closed #1712 explicitly left building
  rendering out of its camera-facing structure-wall fix. No searched issue
  defines directional building art, player orientation, or a destruction
  lifecycle contract.

## Desired experience

- Rotating the camera changes every building to the matching authored view
  without changing its world identity, progress stage, animation phase, anchor,
  hit target, or footprint.
- A building under construction visibly remains at the same progress stage
  when the camera turns. A timed appearance or built loop likewise keeps one
  clock while its view changes.
- Removing a building gives the player a readable non-looping destruction
  presentation without leaving a usable, collidable, powered, or selectable
  building behind after the gameplay removal point.
- `--preview buildings/<name>` makes omissions obvious by allowing the reviewer
  to inspect every lifecycle role at all four camera facings. Every generated
  texture remains behind explicit owner signoff.
- Indoor fixtures contain only the fixture, its directly owned base/contact
  shadow, and its working parts. Enclosure walls, room floors, structural posts,
  and ceiling elements remain owned by structure packs.
- Adding a new building starts from one declared asset manifest, not from
  discovering after implementation that another direction or lifecycle is
  missing.

## Scope

### In scope

- A building-specific four-facing art schema and runtime representation.
- Direction-correct static, animated, ghost, render, sizing, and hit-test paths.
- Distinct construction, timed appearance, built, and destruction roles.
- A settled destruction presentation and removal boundary.
- Dedicated forward-authored destruction animations for every building and
  every structure piece, including the Portal.
- Direction/lifecycle-aware building preview and diagnostic dump state.
- A visual-ownership classification that prevents indoor fixtures from owning
  room enclosure art.
- One isolated art slice per shipped building, each with PixelLab or
  owner-supplied source, gameplay-scale evidence, and explicit signoff.
- A repository guard for path containment, direction coverage, frame ordering,
  image decoding, dimensions, anchor continuity, role completeness, and
  lifecycle handoffs.
- Coordination or supersession of #1845, #1848, #1849, and #1853 once this
  design is ready and its replacement tracker artifacts exist.

### Out of scope

- Player-selected building rotation. This arc covers only the four views caused
  by rotating the game camera; it adds no placement control, persisted building
  orientation, orientation-dependent footprint, or save migration.
- Structure wall/floor/post directional identity and remapping; structure packs
  already own that facing contract. This arc consumes the resolved view for
  destruction playback but does not redesign it.
- Changing building footprints, recipes, materials, work rates, storage,
  operations, power behavior, or placement validity.
- A generic mechanically recolored, cropped, erased, mirrored, or placeholder
  frame used to satisfy the matrix.
- Compiling building animations into unit atlases unless a measured resource
  problem justifies a later storage slice.

## Design

### Proposed directional model

Use the four `CameraFacing` values as the building art domain rather than the
eight movement directions in `Unit.Direction`. A building that has no persisted
orientation resolves its `south`, `west`, `north`, or `east` authored view
directly from the camera. This makes the data model say what the art means and
prevents unit-style horizontal mirroring from quietly reversing asymmetrical
doors, controls, lettering, tools, pipes, or damage.

The static built sprite becomes a four-path set. Every animated lifecycle
becomes four ordered frame lists. The legacy singular `sprite` and
`frames.default` forms may remain readable during migration, but the final guard
accepts neither for a shipped gameplay building.

All four directions of one animation should have the same non-zero frame count
and stage semantics. Construction frame `i` must mean the same progress band in
every view; appearance, built-loop, and destruction frames should share one
clock and retain phase across a camera turn. Every view in one role keeps the
same decoded canvas, sprite anchor convention, and footprint registration.

### Proposed lifecycle matrix

| Role | Required when | Selection model | Handoff invariant |
|---|---|---|---|
| Built | Every building | static view or declared loop | canonical visible result at all four facings |
| Construction | `build_work > 0` | progress-indexed, non-looping | final frame equals the built view for each facing |
| Appearance | a zero-work timed materialisation is declared | time-indexed, non-looping | final appearance is continuous with the initial built view |
| Built animation | a living/active built loop is declared | time-indexed, looping | camera rotation preserves phase |
| Destruction | every destructible gameplay building | non-looping | begins continuously from built presentation and ends with no building quad |

Construction and appearance are separate roles even if they reuse the current
animation machinery. A role the definition does not support is absent; it is
never synthesized from another role at runtime. The guard, rather than a
renderer fallback, explains a missing required role before play.

### Visual ownership classes

- **Indoor fixture:** apparatus, furniture, directly owned plinth, contact
  shadow, and moving/working components only. Never owns room walls, floor
  tiles, posts, or ceiling. Kitchen is confirmed in this class; Workbench and
  Machine Shop appear to fit it.
- **Freestanding installation:** owns its housing and purpose-built base or
  platform, but still does not own a surrounding room. Cargo Hold, Furnace,
  Solar Panel, and High-Voltage Battery appear to fit it.
- **Phenomenon or gateway:** owns the portal/effect and any indispensable local
  frame, with appearance and built-loop roles instead of construction. Acolyte
  Portal appears to fit it.

Every building definition declares one of these classes through a required
`visual_class` YAML field. The production decoder and final asset guard reject
a missing or unknown class, and the preview exposes it during review. The field
makes ownership an explicit choice for every new building, but visual signoff
still judges whether the pixels obey it: image analysis cannot reliably prove
that a wall belongs to a room rather than to a machine.

### Destruction boundary

Preserve the current immediate gameplay teardown, but spawn a transient,
render-only destruction effect carrying the resolved def, page, anchor, z,
facing-independent world identity, and start time. Selection, collision,
storage access, knowledge, jobs, power, and save authority disappear at the
existing destroy command boundary; only the non-interactive visual remains
until its last frame. The effect owns no inventory, power, jobs, collision,
selection, or other gameplay state. It is session-transient, is not serialized,
and does not change the building save format.

### Preview and review

The building viewer should expose lifecycle entries and a four-cell facing row
using the same order as the camera. Selecting a lifecycle plays one shared clock
while selecting a facing changes only the view. Static built art exposes no
playback. The dump reports the selected lifecycle, facing, resolved path/frame,
frame inventory for all directions, and missing/unavailable cells.

Each art slice works in an isolated worktree, generates or imports only the
named building's asset set, builds the focused path, and pauses in
`--preview buildings/<name>` for owner signoff. Gameplay captures at fixed
camera/zoom then verify anchoring, handoffs, and scene composition. No art slice
is complete merely because the files decode or the guard passes.

## Decisions

### D-1. Pause Kitchen art until the systemic contract is settled

The rejected Kitchen construction work is abandoned and issue #1848 is
unclaimed. No replacement Kitchen frames should be generated until the
directional, lifecycle, and scene-ownership contract is ready.

### D-2. Kitchen is an indoor fixture; structure art owns its room

Kitchen artwork must not include the stone room corner, structural walls, room
floor, posts, or ceiling. Those are independently placed structure assets. The
Kitchen owns its hearth/cooking apparatus, counter or directly attached work
surface, and local contact treatment only.

### D-3. Contract work precedes further building proliferation

The required asset inventory for a building type must be explicit and enforced
before the project continues adding buildings ad hoc. Missing facings and
lifecycle art are tracked work, not an acceptable fallback.

### D-4. Generated building art requires interactive signoff

PixelLab candidates are reviewed in the building preview and at gameplay scale
before their art slice is accepted. A solver must pause for owner approval; it
must not infer approval from a successful build or automated asset audit.

### D-5. Direction means game-camera facing, not player-controlled building rotation

Each building authors exactly the views needed when the existing game camera
rotates through south, west, north, and east. A placed building has no
player-selected orientation and gains no orientation field, rotation control,
placement semantics, or persisted state in this arc. If player-controlled
rotation is wanted later, it requires a separate design that composes building
orientation with camera facing and addresses footprints and save compatibility.

### D-6. Demolition makes the building inoperable immediately

The existing destroy command remains the functional removal boundary. At that
moment the building stops participating in selection, collision and occupancy,
storage, jobs, knowledge, power, or any other gameplay system. A separate
non-interactive render effect then plays the direction-correct destruction
animation from the former page, anchor, and z position. That visual is transient
and unsaved; reaching its final frame removes the effect, not a still-operable
building.

### D-7. Every building declares a mandatory visual ownership class

Each building YAML definition must declare `visual_class` as one recognized
value. The initial catalog is `indoor_fixture` for Kitchen, Workbench, and
Machine Shop; `freestanding_installation` for Cargo Hold, Furnace, Solar Panel,
and High-Voltage Battery; and `gateway` for Acolyte Portal. The schema and guard
enforce presence and vocabulary, the preview displays the class, and owner
signoff enforces its pixel-level composition. Furnace's current enclosed casing
is part of the freestanding machine; it is not a room wall.

### D-8. Every frame has four separately authored camera views

Every static sprite and every animation frame owns distinct south, west, north,
and east files, including art that currently appears symmetric. The completed
schema permits no facing aliases, horizontal mirroring, shared paths, or
`default` substitution. All four cells are inspected independently during
preview signoff. This uniform inventory prevents a later asymmetrical detail
from silently invalidating a reuse assumption.

### D-9. Every building and structure has dedicated forward destruction art

Destruction is its own authored animation for every gameplay building and every
structure piece. The Portal is not exempt. A destruction sequence may not be a
construction sequence played backward, mechanically reversed, or substituted
from another lifecycle. Its four camera-facing views depict credible teardown,
breakage, collapse, discharge, extinguishing, or dispersal appropriate to the
subject, and each asset set receives explicit preview and gameplay signoff.

## Proposals

### P-1. Require four authored camera views with no mirroring exemption

Store south, west, north, and east for every frame. Even apparently symmetric
art receives four reviewed cells so later asymmetrical edits cannot silently
invalidate a symmetry declaration. Adopted by D-8 with no exemption.

### P-2. Keep destruction visual-only after immediate gameplay teardown

The current destroy command remains the authority for gameplay removal, while a
separate transient effect finishes the animation. This avoids making dead
buildings usable during playback and avoids a save-version migration. Adopted
by D-6.

### P-3. Replace the existing south-only guard rather than layering on it

Issue #1853's useful construction/appearance role split should be absorbed into
BDA-1, but its `frames.default` completeness rule should not land unchanged.
BDA-13 becomes the final direction-and-lifecycle completeness gate after every
art slice lands.

### P-4. Retain already approved south-facing art as one view

The Workbench construction sequence and PR #2023's approved Machine Shop
sequence can remain the south-facing source for their later directional sets.
Their current YAML wiring is transitional, not the finished contract.

## Open questions

### Q-1. Are these four camera-facing views, player-rotatable building orientation, or both?

Resolved by D-5. The arc authors one view per game-camera facing and changes no
building orientation or save data. Player-chosen placement orientation is
explicitly out of scope and remains a possible separate design.

### Q-2. When does a destroyed building stop existing for gameplay?

Resolved by D-6. Gameplay removal is immediate. A non-interactive, unsaved
render effect may remain until the destruction animation finishes, but the
building is already inoperable and absent from every gameplay authority.

### Q-3. How strict should the visual-ownership declaration be?

Resolved by D-7. Every definition carries a required `visual_class` YAML field.
The approved initial catalog classifies Kitchen, Workbench, and Machine Shop as
indoor fixtures; Cargo Hold, Furnace, Solar Panel, and Battery as freestanding
installations; and the Portal as a gateway. Furnace's housing belongs to the
machine, while room enclosure remains structure-owned.

### Q-4. May a visually symmetric building reuse one authored view for multiple camera facings?

Resolved by D-8. Every frame has four separately authored and reviewed files.
Facing aliases, mirroring, shared paths, and `default` substitution are not part
of the completed contract.

### Q-5. Is destruction dedicated art or construction played backward?

Resolved by D-9. Every building and every structure piece requires separately
authored, forward-playing destruction art. The Portal is not exempt, and
construction playback in reverse is forbidden.

## Verification strategy

- Pure schema tests reject unknown/missing facing keys, duplicate or escaping
  paths, empty lists, mismatched direction counts, invalid role mappings, and
  inconsistent dimensions/anchors.
- An asymmetric four-view fixture proves the renderer, ghost path, texture-size
  lookup, hit-test, and camera rotation all resolve the same view. A symmetric
  fixture cannot satisfy this gate accidentally.
- Construction tests pin the same progress stage through all four camera
  rotations; appearance/built/destruction tests pin one shared clock and
  expected phase behavior.
- Destruction tests prove gameplay registries are cleaned at the chosen
  boundary, the transient is non-interactive, and the last frame removes the
  visual. Save tests prove the settled persistence behavior.
- Preview headless coverage asserts lifecycle ordering, facing availability,
  path/frame dump data, selection preservation, resize behavior, and playback
  phase. Gameplay-scale evidence supplies four fixed-camera captures for each
  static role and representative frames/handoffs for each animation.
- The final audit follows the existing self-test plus bare-audit CI shape and
  runs only after all eight building asset slices land. It complements rather
  than replaces explicit visual signoff.

## Delivery plan

### BDA-1. Introduce camera-facing building asset declarations and distinct lifecycle roles

- **Outcome:** The data/types/loader represent four camera views and distinct
  construction, appearance, built, and destruction roles without silently
  collapsing direction keys.
- **Scope:** YAML schema including `visual_class`, building visual types,
  legacy-read compatibility, construction/appearance role split, focused
  decoder tests, and documentation.
- **Phase:** Foundation
- **Depends on:** `none`
- **Ordering:** `critical path`, `can land first`
- **Relevant decisions:** D-1, D-3, D-5, D-7, D-8
- **Acceptance signals:** An asymmetric fixture decodes all four static and
  animated inventories; malformed/mixed schemas fail precisely; existing saves
  still re-resolve definitions.
- **Out of scope:** Runtime camera selection, shipped art, preview UI,
  destruction timing, and final no-legacy enforcement.
- **Open questions:** Q-5

### BDA-2. Render and hit-test the correct building view at every camera facing

- **Outcome:** Placed, ghosted, designated, and animated buildings draw and
  hit-test from the same camera-facing visual selection.
- **Scope:** Renderer, ghost/designation handoff, hit-test sizing, animation
  phase/progress selection, and asymmetric all-facing tests.
- **Phase:** Foundation
- **Depends on:** BDA-1
- **Ordering:** `critical path`
- **Relevant decisions:** D-3, D-5, D-8
- **Acceptance signals:** Camera rotation swaps only the view; progress, clock,
  geometry, anchor, and hit target remain continuous at all four facings.
- **Out of scope:** Player-oriented placement and shipped asset backfill.
- **Open questions:** `None`

### BDA-3. Add a non-looping building destruction presentation lifecycle

- **Outcome:** Gameplay removal produces one direction-correct destruction
  presentation and then no visual residue.
- **Scope:** Destroy command handoff, transient render-only state, render
  behavior, cleanup ordering, tests, and compatibility documentation.
- **Phase:** Foundation
- **Depends on:** BDA-1, BDA-2
- **Ordering:** `critical path`
- **Relevant decisions:** D-3, D-6, D-8
- **Acceptance signals:** Removal semantics stay authoritative; effects cannot
  be selected or used; a camera turn during playback resolves the same phase in
  the matching view; cleanup is complete after the last frame.
- **Out of scope:** Authoring destruction frames.
- **Open questions:** Q-5

### BDA-4. Make the building preview inspect every direction and lifecycle role

- **Outcome:** `--preview buildings/<name>` exposes the complete review matrix
  and diagnostics needed for art signoff.
- **Scope:** Discovery/model/UI/dump changes, four-facing selection, lifecycle
  ordering, shared playback clock, missing-cell diagnostics, and focused tests.
- **Phase:** Foundation
- **Depends on:** BDA-1, BDA-3
- **Ordering:** `critical path`
- **Relevant decisions:** D-4, D-7, D-8
- **Acceptance signals:** Every declared role and facing is selectable; camera
  order and gameplay paths agree; selection/phase survive resize; missing views
  are visible rather than substituted.
- **Out of scope:** Generating or approving art.
- **Open questions:** Q-5

### BDA-5. Author the directional Workbench asset set

- **Outcome:** The indoor Workbench has owner-approved built, construction, and
  destruction art at every required facing.
- **Scope:** One building's complete matrix, YAML migration, preview/gameplay
  evidence, and focused asset tests.
- **Phase:** Art pilots
- **Depends on:** BDA-4
- **Ordering:** `critical path`; indoor-fixture and construction pilot
- **Relevant decisions:** D-3, D-4, D-7, D-8
- **Acceptance signals:** Four-view construction remains stage-aligned and
  completes without an anchor or pixel handoff; no room enclosure is present.
- **Out of scope:** Other buildings or engine behavior.
- **Open questions:** Q-5

### BDA-6. Author the directional Cargo Hold asset set

- **Outcome:** The freestanding Cargo Hold has owner-approved built,
  construction, and destruction art at every required facing.
- **Scope:** One building's complete matrix, including migration of the existing
  undeclared demolition sequence.
- **Phase:** Art pilots
- **Depends on:** BDA-4
- **Ordering:** `critical path`; construction/destruction pilot
- **Relevant decisions:** D-3, D-4, D-7, D-8
- **Acceptance signals:** Tile-bottom anchoring and the built/construction/
  destruction handoffs remain continuous at all facings.
- **Out of scope:** Other buildings.
- **Open questions:** Q-5

### BDA-7. Author the directional Acolyte Portal asset set

- **Outcome:** The Portal has owner-approved appearance, built-loop,
  destruction, and fallback-free directional art.
- **Scope:** One building's complete matrix and its time-driven transitions.
- **Phase:** Art pilots
- **Depends on:** BDA-4
- **Ordering:** `critical path`; appearance/built-loop pilot
- **Relevant decisions:** D-3, D-4, D-7, D-8
- **Acceptance signals:** Four views retain animation phase and transition
  continuously through appearance, built loop, and destruction.
- **Out of scope:** Portal spawning/gameplay behavior.
- **Open questions:** Q-5

### BDA-8. Author the directional Furnace asset set

- **Outcome:** Furnace has owner-approved built, construction, and destruction
  art at every required facing.
- **Scope:** One building's complete matrix and migration of its undeclared
  demolition sequence.
- **Phase:** Art backfill
- **Depends on:** BDA-4
- **Ordering:** `independent` after the pilots
- **Relevant decisions:** D-3, D-4, D-7, D-8
- **Acceptance signals:** Housing, anchor, progress stages, and handoffs remain
  stable at four facings.
- **Out of scope:** Furnace crafting/gameplay.
- **Open questions:** Q-5

### BDA-9. Author the directional Machine Shop asset set

- **Outcome:** The indoor Machine Shop has a complete owner-approved matrix,
  retaining approved south-facing construction art where compatible.
- **Scope:** Built, construction, and destruction views, YAML migration, and
  focused evidence.
- **Phase:** Art backfill
- **Depends on:** BDA-4
- **Ordering:** `independent` after the pilots
- **Relevant decisions:** D-3, D-4, D-7, D-8
- **Acceptance signals:** All views depict the same machinery and progress
  stages without a room enclosure; handoffs and anchor are continuous.
- **Out of scope:** Machine Shop gameplay and power behavior.
- **Open questions:** Q-5

### BDA-10. Redesign and author the directional Kitchen asset set

- **Outcome:** Kitchen becomes an indoor fixture with a complete, approved
  directional construction/built/destruction matrix and no baked room shell.
- **Scope:** Replacement built art, construction stages, destruction stages,
  four camera views, YAML migration, and preview/gameplay signoff.
- **Phase:** Art backfill
- **Depends on:** BDA-4, BDA-5
- **Ordering:** `critical path`; follows the indoor-fixture pilot
- **Relevant decisions:** D-1, D-2, D-3, D-4, D-7, D-8
- **Acceptance signals:** Every frame contains only Kitchen-owned equipment and
  reads correctly inside independently rendered structure walls/floor at all
  camera facings.
- **Out of scope:** Kitchen recipes, operations, materials, and room generation.
- **Open questions:** Q-5

### BDA-11. Author the directional Solar Panel asset set

- **Outcome:** Solar Panel has owner-approved built and destruction views at
  every required facing.
- **Scope:** One instant-placed freestanding installation's complete matrix.
- **Phase:** Art backfill
- **Depends on:** BDA-4
- **Ordering:** `independent` after the pilots
- **Relevant decisions:** D-3, D-4, D-7, D-8
- **Acceptance signals:** The panel's asymmetry, base, anchor, and destruction
  read correctly across four facings.
- **Out of scope:** Power generation and placement inventory behavior.
- **Open questions:** Q-5

### BDA-12. Author the directional High-Voltage Battery asset set

- **Outcome:** Battery has owner-approved built and destruction views at every
  required facing.
- **Scope:** One instant-placed freestanding installation's complete matrix.
- **Phase:** Art backfill
- **Depends on:** BDA-4
- **Ordering:** `independent` after the pilots
- **Relevant decisions:** D-3, D-4, D-7, D-8
- **Acceptance signals:** Terminals, housing, base, anchor, and destruction read
  consistently across four facings.
- **Out of scope:** Storage ratings and power-network behavior.
- **Open questions:** Q-5

### BDA-13. Enforce the complete building asset contract before play

- **Outcome:** CI rejects any shipped building whose direction/lifecycle matrix
  is incomplete, inconsistent, undecodable, untracked, or still uses the legacy
  singular/default form.
- **Scope:** Self-tested audit, CI/local parity wiring, final removal of legacy
  runtime fallback, catalog/ownership documentation, and migration sweep.
- **Phase:** Enforcement
- **Depends on:** BDA-5, BDA-6, BDA-7, BDA-8, BDA-9, BDA-10, BDA-11, BDA-12
- **Ordering:** `critical path`, lands last
- **Relevant decisions:** D-3, D-7, D-8
- **Acceptance signals:** All eight production definitions pass; fixtures prove
  each missing role/direction/path/frame/count/dimension/anchor failure; runtime
  has no silent south/default substitution.
- **Out of scope:** Visual-quality judgment, which remains owner signoff.
- **Open questions:** Q-5
