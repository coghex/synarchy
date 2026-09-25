# Testing arena scenarios and gameplay tags design

Provide reusable authored worlds for the testing arena. The motivating case is
the expedition integration gate (#2640), which needs an empty ruin, an occupied
ruin, a colony site, water, and controlled travel routes in one world.

Design state: `ready for issue processing`

Issue processing is complete: epic #2698 and all seventeen child issues are
filed and linked below. Decisions D-1 through D-59 are recorded; Q-1 through
Q-16 are resolved. Implementation remains separate work under the child issues.

Owner: `coghex/synarchy`; publication target: `master`.
The owner explicitly requested standalone batch publication of this completed
planning document on 2026-09-24. Each implementation child still delivers its
code, required documentation, and evidence together in the same PR.
The owner approved one umbrella epic and the seventeen delivery slices below.

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [x] EPIC. Author, capture, and load testing scenarios with reusable gameplay tags — [#2698]
- [x] SCN-01. Define the versioned scenario schema and validation contract — [#2699]
- [x] SCN-02. Implement the sparse tag registry and native query evaluator — [#2700]
- [x] SCN-03. Persist tags for units, buildings, and locations — [#2701]
- [x] SCN-04. Preserve physical-item tags through transfers and saves — [#2703]
- [x] SCN-05. Persist tile and flora tags across residency changes — [#2705]
- [x] SCN-06. Expose the complete gameplay tag API to Lua — [#2706]
- [x] SCN-07. Unify arena generation and reconstruction — [#2707]
- [x] SCN-08. Enforce and persist optional finite arena bounds — [#2708]
- [x] SCN-09. Materialize and capture scenario item trees — [#2709]
- [x] SCN-10. Initialize and capture deterministic scenario units — [#2710]
- [x] SCN-11. Materialize and capture terrain, fluids, and flora — [#2711]
- [x] SCN-12. Materialize and capture buildings and real locations — [#2712]
- [x] SCN-13. Load scenarios through one paused session transaction — [#2713]
- [x] SCN-14. Capture scenarios through a coherent snapshot and safe file write — [#2714]
- [x] SCN-15. Expose scenario services through Lua and direct CLI boot — [#2715]
- [x] SCN-16. Add the arena scenario browser and Escape-menu workflow — [#2716]
- [x] SCN-17. Ship reusable scenarios and Python regression coverage — [#2717]

The umbrella and all seventeen children were individually approved and filed.
Checked entries record tracker creation, not implementation completion. Each
implementation PR includes its required documentation, migrations, and behavioral
evidence; no later docs-only completion is assumed.

## Epic contract

- **Goal:** Author, capture, and load reproducible testing scenarios through editable
  data files and the testing arena.
- **Done when:** The agreed scenario content can be authored and captured; omitted
  values use deterministic defaults; capture writes current values as explicit
  overrides; loads replace the previous testing session and become ready fully
  initialized and paused; optional dimensions enforce finite map bounds; the
  resulting baselines are verified across fresh-process loads; a general tag API
  supports flexible object labeling/querying in gameplay scripts and scenarios;
  ordinary saves restore arena bounds, tags, and scenario identity without the
  source YAML, through the existing save system under D-52.
- **Users and operators:** The project owner authoring/manual-testing scenarios and
  automated probes loading repeatable baselines.
- **Arc label:** None proposed.

The product contract, delivery boundaries, order, and readiness are approved
under D-53. Each tracker artifact still requires its own complete draft/signoff.

## Initial readiness checkpoint — 2026-09-24

At the initial checkpoint, the owner approved readiness and requested drafting
the epic. Fifty-three
decisions are recorded; Q-1 through Q-10 are resolved at the product-policy
level. The seventeen approved slices have a matching processing ledger and
acyclic dependency order, with required documentation/evidence inside each PR.
Tracker overlap had been checked; the umbrella epic was then the next entry.
Exact field spellings and codec details are implementation responsibilities;
a newly discovered material behavior choice requires owner input before the
affected implementation proceeds. Readiness is not approval to file an unseen
epic or batch-create its children.

At that checkpoint, no implementation issue, epic, or PR had been created.
The processing ledger above records the subsequent completed tracker handoff.

## Current state and evidence

Verified against `master` at `2cf71b78e43870c7aad837be85ff2434af54102f`
on 2026-09-24 through source inspection; no new runtime experiments yet.

- `src/World/Generate/Arena.hs:generateArenaChunks` creates the initial 5-by-5
  chunk loam-over-granite patch. Its recorded seed reconstructs that initial
  patch on load. This is the startup footprint, not the arena's total extent.
- `scripts/pause_menu.lua:buildMenuItems` already includes Settings, Exit to Menu,
  and Exit to Desktop. `onExitToMenu` runs session teardown and destroys all
  worlds; `onExitToDesktop` calls `engine.quit`. Preserve these established
  actions when adding the arena-specific scenario controls.
- `scripts/ui_manager.lua:onKeyDown` handles F7 by calling `world.openArena()`;
  the `onOpenArena` broadcast reaches `scripts/ui_manager_menu.lua:onOpenArena`
  and routes into the arena. This is the intended developer entry point (D-26).
  `scripts/main_menu.lua:buildMenuItems` has no arena button; that is intentional,
  despite the stale access comment in `scripts/test_arena.lua`.
- `app/Main.hs` currently uses `--arena` to select the arena boot profile in
  graphical, headless, and offscreen modes; it has no scenario-path option.
  `app/CLAUDE.md` owns mode compatibility, pre-boot flag validation, resource-root
  handling, and console listener policy. New scenario CLI handling must honor these.
- `tools/probe_protocol.py` provides static check descriptors, structured result
  events, per-run artifacts, and setup-abort reporting. WARN diagnostics alone
  do not fail a check: D-15 needs an explicit scenario-setup assertion. The existing
  `tools/run_probes.py` machinery supports batch scheduling and isolated resources;
  reuse it when integrating scenario-driven Python tests instead of a second runner.
- `src/World/Thread/ChunkLoading.hs:updateChunkLoading` can generate more arena
  terrain around the camera through `generateFlatChunk`. The arena uses a large
  `wgpWorldSize = 100000` sentinel and non-wrapping chunk identity. The owner
  describes this as the current infinite map; the implementation provides an
  expandable arena with finite numeric/loader limits. Earlier investigation
  incorrectly treated the initial 80-by-80 tile patch as a total arena limit.
- The startup builder and camera builder differ: startup has layered loam over
  granite and seeded grass variants, while `generateFlatChunk` has a single
  loam layer and one grass variant. Reproducible baseline reconstruction needs
  explicit handling of initial generation, later loading, and reload paths.
- Explicit queued chunk requests currently reach `drainInitQueues`, which calls
  `generateLoadedChunk` without the camera loader's arena branch. Preset loading
  and automated scenario demand must use a consistent arena generation path.
- `scripts/test_arena.lua:createArenaWorld` accepts no scenario selection;
  it calls `world.initArena` and installs textures.
- `scripts/movement_arena.lua` already authors named terrain courses with
  tile, fluid, and slope edits. This proves reusable scripted scenario construction
  exists for terrain; it is not a general arena preset import/export facility.
- `src/World/Thread/Command/Init.hs:handleWorldInitArenaCommand` starts from
  default generation parameters, whose location instance table is empty.
- The normal world initialization path constructs location overlays and stable
  instances together using `buildLocationInstancesWithSeed`. The internal
  `allocateLocationInstance` seam in `src/Location/Instance.hs` can allocate
  checked location geometry, but is not a complete arena placement API.
- `src/Engine/Scripting/Lua/API/Register/World.hs` exposes location queries,
  content/encounter updates, and lifecycle updates, but no API to author a new
  placed location. Independent unit and item spawns would not establish the
  location identity and discovery/clearance relationships #2640 must exercise.
- The arena startup profile already loads location and loot definitions
  (`scripts/startup_loader.lua:queueArenaProfile`).
- `src/Unit/Thread/Command/Spawn.hs` rolls eager stats, body composition, skills,
  knowledge, and names through the shared stat RNG, and constructs starting
  inventory/equipment from definitions. `data/units/acolyte.yaml` declares
  nonzero stat, body, and skill ranges. Ordinary fresh spawning alone therefore
  does not establish identical initial individuals across preset reloads.
- `src/Unit/Stats.hs:rollStat` is already pure: it accepts and returns a random
  generator. The spawning caller chooses the shared RNG; the distribution itself
  can be reused with a deterministic private generator. Initial body composition,
  carrying capacity, and blood initialization depend on the rolled properties,
  so setting a few stats after a normal spawn is insufficient.
- `src/Location/Instance.hs:encounterFromDef` already derives encounter counts
  from persisted world seed and stable instance identity, independent of chunk
  loading order. This is an existing example of repeatable initial variation,
  not proof that all actor and loot creation currently follows that contract.
- `src/Item/Types.hs:ItemInstance` represents nested contents recursively and
  stores fill, quality, wear/condition, weight, sharpness, temperature, and bulk
  alongside physical identity. Inventory export therefore needs an explicit
  item-property policy in addition to preserving item definitions and ownership.
- Unit health is structured, not a single HP field: `src/Unit/Types/Wound.hs`
  records body part, wound kind/severity, clotting, healing, dressing, infection,
  and necrosis. The Lua unit API already exposes stat/skill edits, injuries, and
  health queries. Existing individual debug setters do not establish an atomic,
  validated, reproducible scenario-initialization contract.
- Arena save/load is supported: `src/World/Save/CLAUDE.md` and
  `docs/headless_console.md` describe base reconstruction plus edit replay;
  `tools/multiworld_save_probe.py --arena` is the standing behavioral gate.
  This does not establish a reusable preset contract or arbitrary arena bounds.
- `tools/expedition_loop/setup.py` currently searches generated terrain for a
  suitable colony and a placed ruin. #2640's approved specification requires
  extending that generated-world scenario. Moving its gate to authored worlds
  requires an explicit later specification revision and review.
- Initial open-tracker inventory and targeted arena/predefined-world searches
  found no matching predefined arena-world feature. No matching local design
  document was found in the primary or docs worktree.
- A 2026-09-24 refresh of the full open-issue inventory found no existing
  scenario/general-object-tag umbrella. Read #2496 and #2551: faction tags
  carry provenance, owner-scoped mutation, and relationship policy, unlike
  D-29's arbitrary object labels. Do not merge these APIs or let general tag
  mutation change faction policy. Read #2547/#2549/#2552: flora uses existing
  occurrence IDs for wild/row plants and canonical tile ownership for crop
  plots, with condition persistence under #2526. Reuse those authorities;
  do not introduce a competing flora condition system in this arc.
  These are integration neighbors, not proof their unmerged behavior exists.
  Recheck their current heads and coverage when processing affected slices.

## Desired experience

The owner can manually author a predefined world in a file and load it into the
testing arena. The owner can also edit an arena and capture it as a reusable
preset. The expedition scenario is the motivating consumer.
Captures contain scenario data only. Loading establishes a reproducible starting
baseline by using deterministic defaults for omitted fields and applying explicit
file overrides. Saving through the arena writes the current scenario values as
overrides, including current unit stats and health conditions (D-12). Orders and
player progress remain outside the previously agreed scenario scope.
Inventory and equipped gear form part of the scenario, including contents of
containers. Unit reinitialization must respect the authored loadout.
The scenario becomes ready with initialization complete and simulation paused.
The owner or automated test explicitly starts simulation from that baseline.
Only one testing scenario is active at a time; loading replaces the previous testing
session rather than adding a second scenario beside it.

## Scope and ownership

All D-13 content families ship in v1. The following table maps the accepted
scenario vocabulary to current authorities; it is not permission to serialize
every field of a runtime record indiscriminately.

| Scenario content | Runtime authorities | Required representation and boundary |
|---|---|---|
| Format, identities, camera, bounds | New scenario codec; `World.State.Types`, `World.Save.Component.PageCore` | Independent YAML version, stable scenario references, camera-only starting coordinate, optional exact rectangle; no dependence on source YAML after ordinary save |
| Terrain and fluids | `World.Generate.Arena`, `World.Edit.Types`, `World.Chunk.Types`, runtime `Sim.Fluid` | Definition identifiers, tile/elevation/shape data and supported fluid quantities; tile-level clipping, shared reconstruction, closed finite edges |
| Flora and crops | `World.Flora.Types`, `World.Flora.Identity`, `World.Flora.CropPlot`, existing persisted overlays | Species names, placement, age/health and supported current condition values; use existing occurrence/plot identity, preserve on eviction, remove tags on actual deletion |
| Units | `Unit.Types.Instance`, `Unit.Stats`, unit spawn and simulation owners, wound/body types | Definition, placement, supported initial attributes and health fields, inventory/equipment; deterministic omissions, explicit current capture values, consistent dependent body/resource values |
| Physical items | `Item.Types.ItemInstance`, unit equipment/inventory, building storage, ground-item owners | Definition, fill, quality, condition, sharpness, weight, temperature, bulk and recursively owned contents; physical identity and tags follow transfers; no duplicate default loadout |
| Buildings and structures | `Building.Types`, structure/world-edit owners, existing construction and power authorities | Definition, placement/footprint, supported physical state and inventory; reject whole footprints outside bounds; reconstruct dependent runtime state |
| Locations | `Location.Instance`, `Location.Placement`, `Location.Overlay` | Definition, placement/bounds and supported authored content/encounters with real instance references; reject whole invalid locations and owned contents; no copied player discovery or expedition progress |
| General tags | New typed sparse registry under existing state owners | Nonempty assignments only, category forward/reverse indexes, current memberships captured, ordinary-save persistence, Haskell set algebra and Lua list/count/exists |

SCN-01 documents each supported field's type, unit, default/override behavior,
reference kind, and derived-state dependencies. Domain adapters implement that
contract. Texture handles, cache fields, threads, stale queued commands, and
transient renderer state are not authorable scenario values. Orders and player
progress remain excluded under D-3; current stats and physical conditions remain
included under D-12. If a concrete field straddles those meanings, stop and
present that field and its consequence to the owner before deciding its scope.

Out of scope: deterministic gameplay replay, multiworld product support,
world-level tags, faction-policy redesign, a comprehensive property editor,
new art, automatic tag inheritance/relocation, and changing #2640's existing
specification. Existing editing tools plus YAML authoring serve the first version.
Scenario name shortcuts and a browser do not imply a second asset catalog.

### Cross-cutting delivery constraints

- Add persistent state through existing owners/capabilities. Every state-adding
  PR includes ordinary save capture/restore, component migration, historical
  fixtures, and persistence inventory changes required for its state. Do not
  expose a mutable general-tag API before its lifetime and persistence work.
- Scenario export uses the coherent capture boundary and its own supported
  schema; ordinary session saves keep their existing broader contract.
- Shared construction code must not change ordinary spawning's random stream
  or reseed Lua. Arena-local deterministic initialization stays isolated.
- World generation changes follow `src/World/CLAUDE.md`'s output-change gates;
  any changed reconstruction semantics must preserve/migrate existing arena
  saves deliberately. A version bump alone does not migrate positional bytes.
- Each slice supplies targeted tests/probes and docs for its own behavior.
  The final integration slice supplements that evidence. UI evidence requires
  offscreen rendering; headless success alone does not prove the menus work.
- D-52 permits existing-save integration only within the bounded extension.
  If a major redesign proves necessary, retain evidence and ask the owner for
  revised scope before proceeding; never silently drop persistent state.

## Decisions

### D-1. Prefer a manually authored arena for the expedition scenario

On 2026-09-24 the owner requested a manually created test world in the testing
arena, and directed a pivot to implementing loadable predefined worlds if that
was not easily possible. Investigation found a location-placement and preset
loading gap, so the foreground focus has moved to this prerequisite. #2640
remains open and unchanged; it was never assigned or implemented in this session.

### D-2. Support both authoring routes

The owner explicitly selected: “Support both file authoring and capturing an
edited arena as a preset.” Neither route is optional in the intended feature.

### D-3. Capture the scenario only; initialize units when loading

The owner explicitly chose scenario-only capture on 2026-09-24. Orders and player
progress are outside the preset's scope. The initial exclusion of captured unit
conditions was expanded first by D-8 (explicit file overrides), then superseded
by D-12 (capture all current scenario values as overrides). Unit conditions and
current stats are now captured. The original intent of a correctly initialized,
testable starting scenario remains; initialization uses D-5 fallback values only
where the file omits explicit overrides.

### D-4. Presets establish reproducible, testable baselines

The owner's stated purpose is “reproducable and testable baselines.” Initial
construction must support that purpose. This decision does not specify replay
determinism for gameplay after loading, exact identity across content revisions,
or a particular random-number implementation; those details remain open.

### D-5. Repeat the same per-unit variation on each load

The owner explicitly accepted repeatable variation on 2026-09-24. Units retain
the individual differences described by their definitions, while the same placed
unit receives the same initial rolls every time the same preset loads with
unchanged definitions. Uniform baseline values for all individuals were not
selected. This is the fallback policy for omitted values. Explicitly authored
or captured current values override those defaults under D-8 and D-12.

### D-6. Preserve all inventory as part of the scenario

The owner answered “yes, preserve any inventory” to preserving equipped gear and
carried items. Scenario capture includes inventory contents and equipment, including
items nested in containers, rather than replacing authored loadouts with unit
definition defaults. The same inventory-preservation scope applies to scenario
storage. The treatment of item properties is D-7, and unit-condition capture is
now included by D-12. This decision does not define runtime item-ID reuse or
authorize copying gameplay provenance/progress flags.

### D-7. Preserve and allow editing item physical properties

The owner approved retaining item state, including fill, quality, wear/condition,
sharpness, and temperature, and explicitly requested data-file overrides.
Half-full canteens and weapons close to breaking are required example scenarios.
The item-property schema must support those meaningful starting values.

### D-8. Allow explicitly authored unit conditions and initial-value overrides

The owner expanded the scope to author health conditions and override starting
values in the data file for broad, quickly repeatable test coverage. Unspecified
unit values retain definition-driven, repeatable initialization (D-5); explicit
values take precedence. D-12 subsequently extends this to programmatic capture:
current runtime scenario values become explicit scenario inputs when saved.
Orders and player progress remain excluded.

Health overrides must represent actual engine health state and initialize its
dependent values consistently. The complete field vocabulary, legal combinations,
units/percentages, and diagnostics require further schema design; this decision
does not promise arbitrary writes to internal engine memory.

### D-9. Default to the expandable arena; optional dimensions bound the map

The owner accepted rectangular dimensions with boundaries units cannot cross,
and specified: “we can default to the current infinite map, but when specified
the map should shrink to the specified size.” Omitted dimensions therefore keep
the current expandable arena behavior. Specified width and height define the
actual finite map extent; they are not just camera or startup-loading limits.
An 80-by-80 finite default was not selected.

D-19 settles the coordinate anchor; D-14/D-21 settle out-of-bounds content.
Partial chunks must honor those exact dimensions. This decision does not claim
literally unlimited coordinates or unlimited resident memory.

### D-10. Finish loading fully initialized and paused

The owner approved this default on 2026-09-24. A scenario is ready only after its
terrain, entities, deterministic initialization, and explicit overrides have
completed. Simulation remains paused so the owner or automated test can inspect
the baseline before hunger, bleeding, movement, combat, or other gameplay changes
it. Starting simulation requires an explicit action from the owner or test.
Completing a queued request alone is insufficient evidence of scenario readiness.

### D-11. Replace the existing testing session; support one scenario at a time

The owner chose: “we should only support one scenario at a time for now, so replace
any existing testing sessions would be the correct behavior.” A successful load
establishes the new single testing scenario and retires the previous testing session.
Old test entities, orders, and session-wide test progress must not influence the
new baseline. D-10 still requires the new scenario to be initialized and paused.

This decision concerns testing sessions. It does not authorize silently replacing
an ordinary gameplay session. D-26 specifies entry through the existing F7
developer route; fit scenario loading into that route's lifecycle.

### D-12. Capture all current scenario values as explicit overrides

The owner clarified: “if we recapture, it should take the current values and
save them all,” and “we have default deterministic fallback values, and when the
user saves a testing setup through the program itself, it will save everything
as these overrides.” This supersedes D-3's original restriction on capturing
unit conditions and rejects the proposal to retain original health/stat overrides
after the live values have changed.

- Hand-authored files may omit values; deterministic defaults fill those gaps.
- Explicit file values take precedence over defaults on load.
- Saving through the arena serializes current values for all supported scenario
  fields as explicit overrides, including unit stats, injuries/health, terrain,
  placement, inventory/equipment, and item physical properties.
- Recapturing replaces old overrides with the currently observed values. If an
  injury healed during the test, the new preset contains that healed state.
- Explicit empty lists, zero values, and other valid empty values remain
  authoritative overrides. For example, an empty wound list must remove the
  former authored injury rather than preserving it or triggering a fallback.

The first-version content scope is D-13; its detailed field vocabulary still needs
to be mapped to the owning runtime types. This decision captures the test setup
within that vocabulary, with orders and player progress still outside the earlier
agreed scope. The scenario is fully initialized and paused on load (D-10).

### D-13. Include the full proposed scenario content in the first version

The owner approved the listed first-version scope on 2026-09-24: terrain,
fluids, flora, structures and buildings, real locations, units, ground items,
and all inventories. Each included family needs file authoring, capture, and
loading coverage consistent with D-12; existing editing tools are used where
available, with detailed overrides editable in the file. A comprehensive new
visual property editor is outside this first-version proposal.

The implementation plan must account for every included family rather than
quietly dropping a difficult family to reduce scope. Exact fields and references
need repository-grounded mapping before delivery slices are finalized.

### D-14. Skip rejected input with warnings and load the usable scenario

The owner corrected the proposed all-or-nothing validation policy: reject unknown
definitions or overrides and objects outside map bounds, with corresponding
warnings, and “we should still load what we can.” Rejection applies to the
offending entry or override, not automatically to the entire scenario load.

- Skip objects whose required definition cannot be resolved.
- Skip unsupported overrides, warn about the offending entity/field, and keep
  the rest of the object where it can be initialized consistently. Omitted
  effective fields use the deterministic fallback rules.
- Skip out-of-bounds objects, including footprints crossing the boundary.
- Continue loading usable content. A load with skipped inputs must expose its
  warnings and initialize the retained scenario completely before reporting ready
  and paused under D-10.

Warnings identify what was rejected and why. The earlier proposal to reject the
complete scenario and preserve the old test for any such input was not accepted.
File/syntax failures are settled by D-16; owned contents of skipped objects are
settled by D-17. D-47 settles other required and optional dependencies.
Automated test grading is settled by D-15.

### D-15. Unexpected loader warnings fail automated test setup

The owner approved failing automated tests whose scenario setup produces unexpected
loader warnings: malformed content must not yield a passing test of a different
scenario. For example, skipping an unknown soldier definition fails the combat
test's setup even though the usable scenario still loads paused under D-14.

Tests specifically exercising malformed-content handling may declare the warnings
they expect and assert the resulting partial scenario. An expected warning is not
itself a passing result: the test must verify the intended handling, and unexpected
warnings still fail setup. Ordinary tests require valid scenario inputs. This policy
does not change interactive partial loading.

### D-16. File and syntax failures preserve the current screen and scenario

The owner approved treating a missing, unreadable, corrupted, or syntactically
invalid file as a genuine unsuccessful load, presented to the player as a popup
warning. Keep the current screen and existing arena available; do not construct
or replace a scenario from input that cannot be read and parsed successfully.
Reading and syntax validation must precede scenario construction or replacement.

Distinguish core syntax from malformed scenario data: a successfully parsed document
may contain missing fields, incorrect fields, or invalid entries. Handle those
through D-14's warnings and usable-content loading, using the agreed deterministic
fallbacks where applicable. A strict whole-document schema decoder must not turn
every malformed object into an unrecoverable syntax failure. The supported schema
must identify required fields and which entries cannot be constructed without them.

The warning's presentation does not imply success: expose the unsuccessful load
to automated callers, while preserving the previous scenario for interactive users.

### D-17. Rejected containers do not spawn their contents

The owner confirmed that a rejected container's contents must not exist on load.
Skip its entire contents recursively, including nested containers and their
contents. This covers the rejected chest/unit inventory case presented for
signoff. Do not spill or relocate those items into the scenario. Warn about the
rejected owner and the resulting omission; independent valid objects still load.

This settles containment dependencies. Other references between scenario objects
still need mapping to their actual owners and validation rules.

### D-18. Finite map edges block fluid flow

The owner approved closed fluid boundaries for maps with specified dimensions.
Fluid cannot flow across the finite rectangle's outer edge or drain off-map.
Together with D-9's unit limits, this keeps bounded testing scenarios contained.
Apply the boundary consistently to fluid simulation and chunk availability;
the choice does not itself require authored wall objects or new artwork.

### D-19. Center finite worlds on zero; starting coordinates position the camera

The owner corrected the proposed movable map origin. Finite worlds have exact
width and height in tiles and are centered on (0, 0). The starting coordinate
positions the camera only; it does not translate the world bounds or its objects.
Use conventional Cartesian directions when presenting these coordinates (positive
Y upward), rather than graphics-coordinate conventions. The owner's example is
a 20-by-10 world with its upper-left corner around (-10, 5).

The owner subsequently approved this exact discrete-tile convention:

- Upper-left tile: (-floor(width/2), floor(height/2)).
- Include exactly width tiles increasing X and height tiles decreasing Y.
- 20-by-10: X=-10..9, Y=-4..5; upper-left tile (-10, 5).
- 21-by-11: X=-10..10, Y=-5..5; upper-left tile (-10, 5).

Odd tile counts are symmetric around zero; even tile counts use the one-tile
asymmetry shown above. These are tile coordinates, not physical outer edges.
The initial suggestion about rounding odd sizes is superseded by these approved
examples. The earlier optional minimum-corner origin proposal is withdrawn.

### D-20. Default the camera to zero and capture its current position

The owner approved (0, 0) as the starting camera position when the file omits it.
Saving an edited arena captures the current camera position as the explicit
starting position, so reloading opens on the part of the scenario the author framed.
This controls the view only, as specified in D-19; it does not shift the map or
its objects. This decision covers position; it does not silently extend the
capture contract to other view settings such as zoom.

### D-21. Clip terrain and fluid by tile; reject crossing locations and buildings

The owner approved retaining the in-bounds portion of terrain and fluid features.
Reject only their out-of-bounds tiles, with warnings; crossing the boundary does
not reject the whole terrain/fluid feature.

Locations and buildings are indivisible for bounds validation: if any part of
their footprint is outside the map, do not spawn the object and issue a warning.
A rejected location must not leave partial terrain stamps or spawned contents;
validate its complete footprint before applying its contributions. This extends
D-14's whole-object rule explicitly to locations. Keep D-17's recursive exclusion
of contents when their owning container is rejected.

### D-22. Version scenario files and migrate older data while preserving overrides

The owner requested versioned scenario files with migration functions, analogous
in purpose to game-save migrations. Explicit authored/captured values retain
precedence across definition changes: stored strength 12 must still load as 12
even when the definition's default strength changes.

If a current unit definition introduces a stat absent from an older scenario, use
that definition's deterministic fallback for the new stat under D-5. Do not reset
existing explicit stats. This is expected defaulting for an omitted field, not
malformed-content rejection. The guarantee remains deterministic initialization
with unchanged current definitions, not frozen gameplay across game versions.

Migrations must translate supported older scenario representations before current
validation/initialization. A version number alone is insufficient. Supported-version
policy and migration failure handling are settled by D-44.
D-24 settles source-file preservation; D-27 settles file format/version granularity.
This decision concerns scenario presets; ordinary game saves retain their own
existing persistence contract.

### D-23. Unsupported newer scenario formats leave the current arena intact

The owner approved refusing to load a scenario whose newer format version cannot
be understood by the running game. Present a warning, report an unsuccessful
load, and preserve the current scenario and screen. Do not attempt partial scenario
construction by interpreting unfamiliar schema semantics as the current format.
This extends D-16's preservation behavior to unsupported newer scenario formats;
it does not change partial loading of malformed entries within a supported format.

### D-24. Migrate in memory; overwrite files only through explicit user save

The owner approved performing supported migrations in memory during loading.
Loading, migration, and fallback generation leave the source file unchanged.
An existing file may be replaced only when the user explicitly saves over that
file. Saving to a different file preserves the original. Explicit save/capture
writes the current format and current scenario values under D-12.

### D-25. Provide the complete arena Escape menu

The owner approved the arena Escape-menu loading/capture workflow and explicitly
required these actions:

- Load Scenario: select a scenario file.
- Save Scenario As…: choose a destination and capture the current arena; confirm
  before overwriting an existing destination, as approved with D-24.
- Settings.
- Exit to Menu.
- Exit to Desktop.

Reuse the established settings and exit actions. Arena scenario controls belong to
the testing arena; ordinary game-save actions retain their own semantics.

### D-26. Keep F7 as the developer-only arena entry point

The owner rejected the proposed main-menu Testing Arena button. The testing
arena is a developer tool, not a player-facing game mode. Its interactive entry
point is the existing F7 key. Preserve that route and provide D-25's scenario
controls inside the arena. Do not introduce a main-menu arena button or require
Exit to Menu before entering the arena; both parts of that proposal are withdrawn.

### D-27. Use YAML with an independent scenario-format version

The owner approved YAML as the scenario-file format. Both handwritten presets and
programmatic captures use it. Include one explicit scenario-format version at the
top level, independent of ordinary game-save versions, and use D-22's explicit
migrations to interpret supported older representations. Keep the format
human-readable for direct authoring of overrides.

The owner subsequently questioned coupling this version to ordinary save
versions, then reaffirmed independence in D-46 after source inspection.

### D-28. Support scenario CLI boot and Lua load/save for Python-driven tests

The owner approved automated access and explicitly required:

- A command-line scenario option that starts Synarchy directly in the testing arena
  with the specified YAML scenario, usable by Python scripts running many tests.
- Lua API functions to load and save scenario files in an existing engine session,
  callable from scripts, the socket console, and shell-driven console commands.
- The interactive menu, CLI, and Lua calls use the same scenario semantics and
  loading/capture implementation, including structured completion and warnings.

F7 remains the interactive developer entry; automation does not need to synthesize
keypresses. Capture through Lua remains subject to D-24's explicit overwrite
intent. Session replacement still obeys D-11 and supported failure policies.

The owner has no preference between paused completion followed by explicit resume
and immediately running after initialization. Retain D-10's paused completion as
the implementation choice: Python can inspect the baseline, grade setup warnings,
then resume. Never allow simulation to alter a half-initialized scenario.

The owner requested guidance on test capture rather than choosing a new recording
system. Proposed workflow: YAML stores starting state; Python supplies actions
and assertions and records results through the existing probe protocol. Automatic
recording/replay of interactive actions is not implied by scenario capture.

### D-29. Separate scenario identity from flexible gameplay tags

The owner approved automatic stable internal scenario IDs separately from optional
script-facing tags, and expanded scope to a full tag-system API useful to both
gameplay scripting and testing scenarios. A scenario ID identifies an object for
capture, references, and repeatable defaults; it is not a required script label.

Objects may have multiple tags and each tag may belong to multiple objects.
Tags impose no uniqueness or required-role rules. Scripts may query tagged objects
and apply their own behavior or test assertions; a tag alone does not set stats,
create an injury, or issue an order. The tag system must be usable outside the
testing arena. Existing faction tags already have relationship semantics and
are not interchangeable with this general labeling facility.

The expanded feature's object-coverage, mutation/query, and lifecycle contracts
are settled by D-32 through D-42; D-31 settles persistence. Do not quietly reduce
the requested full API to a scenario-loader-only lookup.

### D-30. Call the authored testing setups scenarios

The owner selected **scenario** as the canonical product and API terminology,
replacing the earlier scene terminology. Use Load Scenario and Save Scenario As…
in D-25's menu, scenario-format version in YAML, and scenario names in CLI/API
documentation. Earlier decisions have been normalized to this vocabulary without
changing their behavior. Keep the existing design-document path as a durable
reference during this conversation.

### D-31. Tags are always persistent

The owner explicitly required tags to be persistent and saved/loaded under all
circumstances. Preserve current tag assignments through scenario capture/load
and every ordinary save/load path, including autosaves and tags created or
modified by scripts during gameplay. Do not introduce transient-only tags or
persist tags only when running in the arena.

Tag state is part of the coherent captured state of its owning objects. Every
supported serialization/restoration route for those objects must preserve it.
The implementation must classify this state, update the corresponding save
components and migration/default rules, and prove behavioral round trips.

### D-32. Tag objects and tiles; exclude whole-world tagging

The owner approved tags on units, buildings, locations, plants, items on the
ground/carried/equipped/inside containers, and individual map tiles. Whole worlds
are explicitly outside scope. The owner does not want this work to introduce
multiworld support; keep the feature's product scope to the active testing/game
context. Existing internal identity scoping still needs correct handling, but
does not authorize world-tag APIs or a new multiworld product feature.

### D-33. Avoid tag storage on every untagged object

The owner requires tag memory to scale with actual tag usage where possible.
Do not add an empty tag pointer/list/optional tag field to every object or tile.
The implementation must avoid per-object tag overhead for the overwhelmingly
untagged population. A small shared registry overhead is distinct from a field
on every object. D-34 approves sparse storage; D-35 settles category/index
organization.

### D-34. Use a typed sparse tag registry

The owner approved the sparse registry approach: store tag assignments separately
from object records, create an entry only when an object has tags, and remove the
entry when its last tag is removed. Use typed target identities and an abstract
nonempty tag collection to keep empty entries out of the registry. Untagged
objects do not gain a tag field. Persist authoritative assignments under D-31.

The owner then proposed grouping categories and sorting each by its index to
improve search and updates as usage grows. D-35 approves the resulting design.

### D-35. Separate ordered category maps and maintain both lookup directions

The owner approved ordered indexes per category with both object-to-tags and
tag-to-objects lookup from the first version. Keep units, buildings, locations,
plants, physical items, and tiles in their respective typed categories. Key
forward maps by the category's existing identity, using a canonical coordinate
ordering for tiles. Key reverse maps by tag, with ordered target sets.

Use balanced ordered maps for efficient lookup, insertion, and deletion without
re-sorting whole lists. `Data.Map.Strict` documents O(log n) operations within
one map (key-comparison cost also matters):
https://downloads.haskell.org/ghc/9.12.1/docs/libraries/containers-0.7-8aed/Data-Map-Strict.html.

Both lookup directions must remain consistent on updates; remove empty entries.
Persist authoritative tag assignments and rebuild the derived reverse index
on load. The extra memory is for actual tag relationships, with no tag field on
untagged objects. Category partitioning improves access, but does not bound
legitimate tag growth or eliminate the cost of enumerating matching objects.

### D-36. Remove tile tags when the tile is removed

The owner rejected preserving tags at a coordinate after its tile disappears.
Tags belong to the existing tile, not an independent persistent spatial marker.
Removing the tile removes its assignments from both indexes; creating a new
tile at that coordinate does not inherit the deleted tile's tags. The earlier
coordinate-marker proposal is withdrawn.

The example had been `meeting_point` (a gathering place), not `melting_point`.
The owner correctly requires melting points and other material properties to
remain in material definitions rather than becoming per-tile tag data. General
tags remain optional scripting labels under D-29, with no implicit physics.

This concerns actual tile removal. Unloading a chunk from memory does not remove
its logical tiles or their persistent tags. D-37 settles in-place material
changes while the tile still exists.

### D-37. Preserve tags on surviving tiles; scripts own tag meaning and recovery

The owner reaffirmed deletion cleanup and approved retaining tags when a tile
changes material but still exists. General tag infrastructure maintains
assignments and accurate queries; gameplay scripts interpret labels and decide
how to respond to a missing target. No automatic relocation of a `meeting_point`
belongs in the tag system.

A script can remember its preferred position, check whether its tagged destination
still exists, choose a new suitable tile, and assign the tag there. That is script
behavior, including any pathing/passability checks and any persistence required
for the script's remembered position. The tag system must not infer these rules
from a tag's spelling. Internal chunk unloading must not masquerade as gameplay
deletion in membership queries.

### D-38. Tags follow object identity and are not inherited by new objects

The owner approved retaining tags on an existing object through moves, equipping,
and storage, and removing its assignments when that object is gone. Tags do not
automatically transfer to replacement objects, crafted outputs, or other new
objects. Newly created objects receive tags only through explicit authored data
or script assignment. In-place changes on surviving tiles remain D-37's case.

Deletion removes the destroyed object's memberships from both indexes; other
objects sharing those tag names retain their assignments. Reuse of a numeric
identity must not revive stale memberships. UI grouping of existing physical
items is not object creation or destruction and must preserve each item's tags.

### D-39. Evaluate all/any/none tag queries in Haskell

The owner approved the native Haskell query path after reviewing its concrete
set-algebra implementation. Expose `all`, `any`, and `none` filter groups to Lua
and evaluate them against D-35's reverse indexes. `all` requires every listed
tag, `any` requires at least one listed tag, and `none` excludes any listed tag;
nonempty groups combine conjunctively. Convert the final result to Lua, rather
than requiring scripts to fetch and combine intermediate lists.

This is membership algebra only: scripts retain gameplay interpretation under
D-37. Use existing `Data.Set` operations; no expression parser or persistent
query cache is needed. Empty results are normal. D-40 settles the starting set
and extends the interface to composable set expressions.

### D-40. Include untagged objects and evaluate complete set expressions natively

The owner requires negative queries to include untagged objects. The universe
is all existing objects in the requested category; for tiles it is specifically
all tiles currently loaded in memory. Thus all units minus defenders includes
both units tagged only `reserve` and units with no tags. A tile exclusion query
may return every loaded tile without the excluded tag.

Allow scripts to request multiple composed set operations in one command,
including union, intersection, difference, and complement relative to the query
universe. Haskell evaluates the complete expression and passes only the final
result to Lua. Broad results are allowed on request; do not require a positive
tag filter, explicit candidate list, or rectangular bound, and do not silently
truncate results. Scripts are responsible for choosing useful filters.

The earlier positive-filter/explicit-candidate restriction is withdrawn. Obtain
the universe from existing object managers or loaded tile storage when needed;
untagged objects still require no registry entries or per-object tag field.
Native evaluation can avoid unnecessary intermediate materialization while
preserving the requested set semantics, but broad queries can still require
substantial temporary storage and Lua result allocation.

Loaded-tile query scope does not change D-31/D-36/D-37 persistence or lifetime:
chunk eviction preserves assignments for surviving tiles. Absence from a
loaded-scope result alone does not prove destruction; reloading brings surviving
tiles back into that query universe with their tags intact.

### D-41. Support list, count, and exists query results

The owner approved all three result modes for the same native set-expression
query: `list` returns every matching object, `count` returns the number of
matches, and `exists` returns whether any object matches. Count and exists
return only a number or boolean to Lua; they must not marshal an object list
as an intermediate API result. Empty matches produce an empty list, zero, and
false respectively. Full list results remain available under D-40.

The owner favors a broad useful API surface. This approves the three enumerated
query result modes; further behavior proposals remain subject to their own
explicit scope decisions.

### D-42. Complete the core tag operations and category selection

The owner approved adding/removing tags, replacing or clearing an object's
complete tag set, listing its tags, and checking membership. Queries may select
one category, several categories, or all supported categories together. Mixed
results identify each target's kind and identity, so unit 12 and building 12
are distinct. Tile results identify their coordinates and remain restricted
to loaded tiles under D-40. The selected categories define the universe for
complement; all three D-41 result modes support this query scope.

This settles the core product scope of Q-10. Exact API names, typed result
encoding, and documentation/examples belong to the implementation contract;
any newly discovered material behavior choice must be surfaced explicitly.

### D-43. Prefer preserving the arena on construction failure; allow flat fallback

The owner approved the proposed staged-loading failure policy but explicitly
made preservation of the previous arena optional for runtime construction
failures. Prefer preparing the replacement before switching and retaining the
previous paused arena when existing transaction machinery makes this practical.
If retaining it would require disproportionate additional machinery, recovery
to a fresh default flat testing arena is acceptable. Do not make a bespoke
rollback mechanism or previous-arena preservation a v1 completion requirement.

An unexpected construction failure still produces a warning and an unsuccessful
scenario-load result, even if recovery to the flat arena succeeds. Keep recovery
paused and controls available for another load or exit. Automated setup must not
mistake a fallback arena for the requested scenario. After a completed session
swap, script/UI reconciliation failure remains unsuccessful with the new arena
paused and no promise of restoring the old session, as in the approved proposal.

This concession concerns runtime construction failures. D-16 and D-23 still
require unreadable/syntactically invalid files and unsupported newer formats
to leave the current screen/session intact without constructing a replacement.
Recoverable per-object content problems still follow D-14.

### D-44. Retain compatibility from the first released scenario format

The owner approved retaining migrations for every released scenario format
starting with v1. Maintain migration code and historical fixtures as formats
evolve. Older scenario syntax remains interpretable; removed content definitions
still follow the warning/partial-load policy in D-14, explicit values remain
authoritative, and new omitted stats receive D-22's deterministic defaults.

An unrecoverable format migration failure warns, reports an unsuccessful load,
and leaves the current arena untouched before construction begins. Migration
and loading never rewrite the source file under D-24. D-23 continues to govern
unsupported newer formats. This scenario compatibility commitment is separate
from the ordinary game's existing save-format baseline and migrations.

### D-45. Treat ordinary arena-session saving as conditional on modest effort

The owner would like ordinary game saves of scenario-based arena sessions if
the existing implementation can support them relatively easily. Do not make a
major save/load redesign a requirement of this scenario feature. Investigate
reuse before finalizing the delivery scope and revise affected design sections
if the chosen integration changes their contracts. D-31's gameplay-wide tag
persistence remains approved; this conditional preference does not remove it.

The owner also asked whether scenario YAML versions should track ordinary save
versions. This is a question for evidence and a renewed decision, not approval
to replace D-27's independent version policy.

### D-46. Reaffirm independent scenario YAML versioning

After reviewing the existing per-component ordinary save versions and arena
restore support, the owner approved retaining an independent scenario YAML
version. D-27 stands. Ordinary save components and scenario YAML need migrations
when their own representations change; they need not change together. Share
underlying construction and validation where appropriate without coupling file
version numbers. D-45's modest-effort condition on additional ordinary arena
save integration remains in effect.

### D-47. Reject unresolved required dependencies and retain unrelated content

The owner approved skipping entries whose required targets are missing or
rejected, with actionable warnings. Propagate rejection through further required
dependencies so the loaded subset has no dangling required relationships.
Keep unrelated valid objects. If an invalid reference is optional and a valid
absent/default state exists, discard only that field under D-14.

For example, rejecting a generator building also excludes its associated
power-system record. D-17's recursive containment exclusion remains in force.
This is dependency handling for supported scenario content, not an expansion
into new authored power-network features. Unexpected warnings still fail
automated setup under D-15.

### D-48. Reject overlapping save/load requests with an immediate busy result

The owner approved extending the existing ordinary save/load exclusion policy
to scenario loading and capture. While a conflicting operation is active,
reject the new request immediately with an explicit busy result. Do not queue
it, cancel the current operation, or replace that operation's completion status.
Scripts may wait for completion and retry. Interactive controls may disable
conflicting actions while busy. Capture cannot observe a half-loaded scenario.

### D-49. Support scenario names, launch-relative CLI paths, and absolute paths

The owner requires command-line relative paths to use the directory from which
Synarchy was launched. In-game relative/name-based access is limited to the
scenario folder, superseding the proposed engine-resource-root-relative Lua
lookup. Explicit absolute paths work through every entry point, including
in-game access. A single-word scenario name must also be accepted.

The owner requested checking the scenario folder first, then trying an absolute
path, and warning that the scenario does not exist if neither resolves. D-50
clarifies command-line extension requirements and in-game name shorthand;
do not invent an absolute path from a bare name.

### D-50. Require extensions for CLI and absolute paths; expand in-game names

The owner requires the full file extension on command-line inputs and explicit
absolute paths. Relative command-line filenames use the launch directory under
D-49, including a filename such as `ambush.yaml` without a directory prefix.
An extensionless CLI `ambush` is not an implicit scenario-name lookup.

In-game names accept either `ambush` or `ambush.yaml`, resolving under the
scenario folder; append `.yaml` automatically when the extension is absent.
Explicit absolute paths remain supported everywhere and require the extension.
Missing scenarios warn without replacing the current arena under D-16. This
supersedes the proposal to accept extensionless bare names through the CLI.

### D-51. Capture pauses the arena and requires explicit resume

The owner approved matching ordinary saves: an accepted scenario capture pauses
the simulation, obtains a coherent snapshot, writes the scenario file, and
leaves the arena paused. The player or controlling script explicitly resumes.
All captured values must belong to the same snapshot, including tags and nested
inventory; an ordinary pause flag alone is insufficient if state-owning threads
or Lua callbacks can still mutate them. Reuse the save barrier as appropriate.
D-48 still rejects conflicting requests before admitting a new capture.

### D-52. Include bounded ordinary arena-save integration in v1

The owner approved extending the existing ordinary save path in v1 to preserve
the new arena bounds, general tags, and scenario identity. Loading an ordinary
save restores its session without reading the source YAML. Keep the existing
save semantics and independently versioned component format; do not create a
second session-save system or add new ordinary-save menu controls in this arc.

If implementation requires a major save/load redesign, return to the owner with
the evidence and a concrete scope revision before expanding the work. This
makes D-45's modest-effort condition an explicit implementation stop, not a
license to ship ordinary saves that silently lose the new state. The owner
also authorized preparing the delivery breakdown for review.

### D-53. Approve the epic structure, delivery plan, and readiness

The owner approved the presented one-epic, seventeen-PR breakdown and said
"lets draft it" in response to the explicit readiness checkpoint. Mark the
design ready for issue processing and draft the EPIC entry first. The approved
scope, dependency order, same-PR documentation/evidence, and bounded save/load
integration remain binding. This authorizes drafting; the complete epic body
and each later child still receive separate signoff before tracker creation.

### D-54. Address existing loaded targets; identify individual tiles by x, y, z

During SCN-05 preparation the owner confirmed that tile tags identify individual
cells at canonical `(x, y, z)`, scoped to their owning page. The owner then
approved restricting runtime tag targets to existing loaded objects generally.
For terrain, a target must be an actually generated, resident, non-air cell.
Air and positions whose terrain has not been generated cannot receive tags;
tagging must not generate or load terrain to make a target eligible. A cell
below the surface is eligible only when it is already loaded and contains a
tile. No surface-only or player-discovery requirement is added.

Loaded query universes include eligible untagged objects as well as tagged
ones. Persistent assignments retained for evicted targets do not make those
targets loaded or eligible for runtime operations. D-31/D-36 still require
eviction and ordinary save/load to preserve these assignments; restoring
durable state does not itself constitute a new script assignment. Once the
same surviving target becomes loaded again, its tags are available again.
Actual deletion removes the target's memberships, and replacement at the same
coordinate does not inherit them. An in-place material change on a surviving
non-air cell retains its tags under D-37.

Source evidence at `e3c781c77bf3534e5ab1204f6e5ab073f53c5d37`:
`World.Generate.Chunk.Columns.buildChunkColumns` constructs a limited vertical
range from `exposeFrom` to the surface; it does not instantiate arbitrary
underground depths. `ColumnTiles` explicitly stores material versus air inside
that range. `generateFlatChunk` initially has one terrain cell per column.
Thus a loaded chunk does not imply every underground coordinate contains a
known tile. This clarification applies to SCN-05's live target resolution,
SCN-06's API, and SCN-11's terrain/tag construction, without changing their
approved boundaries or dependency order.

### D-55. Use a uniform bounded foundation for the default arena

The owner approved four loam layers over twelve granite layers throughout the
default arena, also allowing indefinite granite as an acceptable alternative.
Choose the bounded sixteen-layer foundation, which already fits the current
column representation and requires no new unbounded-depth generation model.
Startup, expansion, explicit chunk demand, eviction regeneration, and ordinary
save restoration must agree on the chosen arena base at the same coordinate.
Keep the surface at the current sea-level elevation and retain D-54's rule that
only actually generated resident non-air cells can receive runtime tile tags.

This extends the initial arena's current material profile across the expandable
arena; it does not impose finite horizontal bounds or change ordinary generated
worlds. Q-13 separately determines how legacy arena saves adopt or preserve
their previous base; approval of the new default does not silently settle that
compatibility choice.

### D-56. Upgrade legacy arena bases to the corrected foundation

The owner approved loading existing arena saves on D-55's uniform sixteen-layer
foundation, then replaying their persisted edits. This intentionally adds
subsurface material where the former expansion/regeneration path produced only
one loam layer; excavation there may behave differently. Record this as an
approved arena reconstruction correction, not exact preservation of the former
base. There is no separate legacy deep-center/shallow-exterior profile.

Preserve the saved seed, edit history, and other ordinary session state through
the existing load transaction. Loading alone does not rewrite the source save;
explicit saving and autosaving retain their normal publication rules. Retained
save formats must remain readable; any actual wire changes still require their
own component migrations. The missing historical record of which generation
path last produced a chunk is not recoverable and must not be invented.

This resolves Q-13 and the remaining SCN-07 drafting blocker. The approved
delivery boundary and dependency order remain unchanged, and issue processing
resumes with SCN-07's complete draft still subject to its own filing approval.

### D-57. Restore exact scenario contents without gameplay insertion checks

The owner selected "Restore exact contents" for Q-14. Scenario construction
preserves authored/captured inventory trees even when a normal gameplay insert
would exceed capacity, such as 12 kg of contents in a 10 kg container. Supported
existing trees with absent storage-capacity data also remain representable;
do not discard contents merely because a new gameplay insertion would fail.

This is scenario initialization, not permission to relax gameplay transfers.
Ordinary moves retain their current weight/bulk limits, absence handling,
identity checks, and refusal behavior. Unknown definitions, invalid individual
overrides, cyclic/shared ownership, and duplicate physical identities still
follow the existing scenario validation contract. Exact contents do not gain
extra default kits or spill into another owner.

This resolves Q-14 and the SCN-09 drafting blocker. The complete child issue
still requires separate approval before filing; delivery boundaries and
dependency order are unchanged.

### D-58. Exit nonzero after fatal headless/offscreen scenario CLI startup failure

The owner selected "Exit with nonzero code" for Q-15 on 2026-09-24.
When launched with the scenario CLI flag in headless or offscreen mode, a
fatal scenario-load result (including missing/unreadable input, invalid YAML,
unsupported format/migration failure, construction failure, or incomplete
post-publication reconciliation) reports diagnostics and terminates the process
with a nonzero exit code. Successful fallback-arena recovery does not convert
the requested scenario failure into startup success. Shut down through the
existing worker/resource lifecycle rather than abandoning the engine.

This is startup process handling around the shared service's result, not a
change to that service's validation or recovery semantics. Graphical sessions
retain recovery controls, and Lua load requests made in an already running
session retain paused recovery. Valid partial loads remain available with
their structured warnings; D-15 still makes unexpected warnings fail automated
setup. Malformed/unsupported CLI flags retain pre-boot rejection under app rules.

This resolves Q-15. The approved SCN-15 boundary and dependency order remain
unchanged; its complete issue draft still requires separate filing approval.

### D-59. Disable Exit to Menu while a conflicting operation is busy

The owner selected "Disable Exit to Menu while busy" for Q-16 on 2026-09-24.
In the arena scenario workflow, disable Exit to Menu while an operation owns
the shared save/load/capture gate, explain the busy state, and re-enable the
action after terminal success or failure. Recheck at activation so a request
admitted after the UI was drawn cannot bypass the restriction. Do not queue
an exit or cancel the operation. Exit to Desktop remains available through
normal engine shutdown and the services' existing shutdown semantics.

This resolves Q-16 without changing the SCN-16 boundary or dependency order.
The complete issue draft still requires separate filing approval.

## Initialization and capture contract

The selected model is **deterministic defaults → explicit overrides → initialized
paused scenario**. A hand-written minimal scenario and a complete program-exported scenario
use the same precedence rules. Saving exports the current scenario values rather
than merely retaining original inputs or writing only differences from defaults.
Runtime-derived values must be reconstructed consistently from the effective
inputs; the schema must distinguish these from independently authorable fields.

Example: a file omits strength but declares an injured unit carrying a half-full
canteen. Loading rolls strength reproducibly and applies the authored injury and
fill. After the unit heals and drinks, saving writes its current strength, empty
wound list, and reduced fill. The next load recreates those saved starting values.

## Proposals

- Use a shared representation and loader for file-authored and captured worlds,
  accessible to the interactive arena and automated probes. D-27 settles YAML;
  D-28 settles CLI and Lua automation. Scenario-only capture is D-3.
- Implement D-5 using a fixed scenario seed and stable internal scenario IDs. Derive
  private random inputs for named property domains using a specified stable
  encoding/mixer, and reuse the existing rolling functions. Avoid dependence on
  runtime unit IDs, spawn order, hash-map iteration, or the ambient gameplay RNG.
  Build dependent body/resource values through the normal initialization rules.
  Export should preserve existing scenario IDs; identity assignment for newly
  placed entities is an implementation detail still to design.
- Reuse production location construction, content spawning, encounter, and
  clearance behavior for authored locations. Investigate how to expose the
  existing checked allocation seam while keeping overlay and instance state
  consistent.
- Materialize a baseline by validating authored data, combining deterministic
  definition defaults with explicit overrides, and initializing dependent values
  before ordinary gameplay can change the scenario. Use an explicit supported schema
  with useful field/entity warnings and partial loading as selected in D-14.
- For D-9, use one bounds contract across generation, placement, movement,
  terrain edits, and fluid simulation. Audit all chunk demand/restore paths;
  limiting only the initial patch or the camera would not bound the world.
- For D-19, map the approved authored convention to engine tile coordinates; avoid
  confusing the two horizontal map axes with engine elevation.
- Under D-22/D-44, resolve scenario references against current content
  definitions. D-22 settles
  preserved explicit values and deterministic defaults for new omitted stats.
  Apply supported schema migrations before deciding an old field is unsupported;
  unresolved definitions/overrides then follow D-14/D-15. D-27/D-44/D-46 settle
  the independent compatibility policy; do not use the ordinary game save's
  transitional currentSaveVersion as its schema version.
- For D-28, use a scenario-path CLI argument (proposed spelling `--arena-scenario PATH`)
  implying the arena boot profile, and Lua load/save/status functions with
  explicit completion results. Resolve file paths according to a documented
  resource-root/caller-directory contract. Distinguish console availability from
  scenario readiness; Python must wait for the scenario result before its assertions.
  Exact flag/API names and schema fields are implementation details to document.
- D-29 separates automatic stable scenario identity from optional script-facing
  tags. Use internal scenario IDs for references and deterministic defaults;
  changing tags must not change an object's identity or reroll its defaults.
- D-42 approves the core mutation/inspection operations and querying one,
  several, or all supported categories with typed identities in mixed results.
  Document exact API names and result encodings alongside implementation.
- For D-43, first assess reuse of the existing save/load staging boundary for
  scenario construction. The phase distinction is documented in
  engine_contracts.md, Save/load transaction, and src/World/Save/CLAUDE.md.
  Preservation is preferred, with fresh default flat-arena recovery allowed
  for construction failure; record which path the implementation provides.
- D-44 approves migrations for all released scenario formats from v1, retained
  fixtures, and preservation of the current session on format migration failure.
- Under D-52, ordinary game saves of arena sessions preserve the
  current session under the existing persistence contract, including new finite
  bounds, tags, and identity state needed for later scenario capture. Restoring
  such a save must not reload the source scenario YAML or reroll its baseline;
  renaming/deleting/editing that YAML after the save cannot change the saved
  session's contents. Scenario export remains D-3/D-12's baseline capture with
  orders and player progress excluded. Existing game-save transience rules
  continue to apply; this integration adds no new arena save-menu control. Owner
  approval is recorded in D-52, retaining D-45's bounded-effort condition.
- D-52 integration plan: include ordinary arena-session
  save/load compatibility in v1 by extending the existing state capture,
  versioned component codecs, snapshot adapters, and arena staging path. Scope
  the additions to finite bounds, mandatory tag persistence, and scenario
  identity metadata needed for later capture. Migrate legacy arenas to their
  existing expandable behavior and missing general tags to an empty registry.
  Preserve the current ordinary-save semantics and avoid a new save format or
  new arena game-save UI. If investigation during implementation requires a
  major save/load redesign, stop and bring a concrete revision to the owner
  under D-45; do not silently expand scope or ship lossy ordinary saves.
  D-52 approves this bounded v1 integration.
- Source-level feasibility: `World.Save.Snapshot.PageSnapshot` already collects
  generation parameters, terrain edits, units, buildings, inventories, flora,
  simulation, and power state. `World.Save.Component.PageCore` and the other
  component codecs own versioned encoding/migrations; `World.Save.Types`,
  `World.Save.Snapshot.Adapter`, and `World.Load.Stage` carry restore state.
  These provide extension points, not a measured cost estimate or proof of
  round-trip behavior. Finite-bounds restoration and post-load recapture need
  behavioral coverage alongside existing arena save/load probes.
- Q-4 source inspection confirms existing arena restore support in
  `src/World/Load/Stage.hs:813`: it reconstructs the flat base from saved
  generation parameters and replays saved edits. The existing
  `tools/multiworld_save_probe.py --arena` covers this path, but was not run in
  this design session. New finite bounds and scenario identity are not thereby
  proven supported and require explicit persistence integration.
- Ordinary on-disk saves use independently versioned components, for example
  `world-pages` v12 and `unit-sim` v3 in the inspected code. The comment at
  `src/World/Save/Types.hs:144` explicitly says `currentSaveVersion` is internal
  bookkeeping and does not govern disk compatibility. There is no single
  gameplay save schema version to mirror. Recommend retaining D-27: independently
  version scenario YAML and each affected ordinary save component, while reusing
  shared state construction/validation where appropriate. An order-state format
  change can require a save migration without changing scenario YAML; an
  authored YAML representation change can require a scenario migration without
  changing runtime save data. Coupling numbers would not remove either codec's
  migration obligations. D-46 records renewed owner approval of independence.
- For D-39, the implementation path is concrete: D-35's reverse indexes provide ordered target sets;
  `Data.Set.intersection`, `union`/`unions`, and `difference` implement all-of,
  any-of, and exclusion without an expression parser. The repository already
  uses these operations, for example `Unit.Faction.Profile` and
  `Engine.Asset.YamlFactions`. Standard-library reference:
  https://downloads.haskell.org/ghc/9.12.1/docs/libraries/containers-0.7-8aed/Data-Set.html.
  Keep the three named filter groups as a convenient shorthand, evaluated
  coherently against the Haskell indexes, converting only the final result to Lua.
  For positive all-of queries, start with the smallest candidate set and stop on
  an empty intersection; subtract the union of excluded-tag sets. Combined
  queries need temporary result storage but no new persistent cache. Script
  semantics stay with scripts; tag membership algebra can live in Haskell.
- For D-40, recommend structured Lua tables for nested union, intersection,
  difference, and complement expressions, with tag sets and the category's
  universe as leaves. This supports complete algebra without a custom string
  expression language. Evaluate one request against coherent membership and
  object state. Exact API spelling is an implementation detail.
- D-41 approves `list`, `count`, and `exists` result modes for the same query.
  Where possible, avoid building a complete native result for `exists`; this is
  an implementation optimization and must preserve the full query semantics.
- D-34 approves a sparse registry conceptually shaped as
  `Map TagTarget NonEmptyTagSet`, keyed by typed existing object identities or
  canonical tile coordinates. Only tagged targets have entries; removing the
  last tag removes the entry. Do not add a tag field or allocate a tag-specific
  identity for every untagged object. Hide the nonempty set's constructors behind
  checked operations so empty records cannot enter the registry.
  Haskell sum types distinguish units, buildings, locations, flora, physical
  items, and tiles; ordinary boxed `Maybe`/list fields alone do not remove the
  per-record field. GHC's representation documentation describes boxed fields
  as references: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/primitives.html.
  A shared registry has fixed overhead plus entries/tags for actual assignments;
  do not promise zero total memory or an unmeasured exact byte count. D-35's
  indexes and any string interning must be included in retained-memory accounting.
  Follow capability inventory section 6.4(a): place owned state
  under its existing manager/world owner rather than defaulting to EngineEnv.
- For D-38, existing evidence: `Item.Types.iiInstanceId` persists through equip/store/
  withdraw/transfer/drop. `scripts/ui/item_list.lua` groups physical instances
  for display while retaining their IDs; UI grouping is not object creation.
- For D-11, propose preparing/validating the new scenario before replacing the
  previous testing session, preparing the usable subset when D-14 warnings occur.
  Distinguish complete usable-subset initialization from an incomplete load.
  D-16 settles file/syntax failures; D-43 settles runtime construction failure
  recovery. Preserve D-26's existing F7 entry behavior.
- For D-47, map required references to skipped objects beyond the containment
  behavior settled by D-17 and apply the approved dependency rejection policy.
  A concrete existing dependency is a power node's building reference:
  `src/World/Save/Integrity.hs` enumerates `pnBuilding` as a `RefBuilding`
  edge. If a generator building is rejected, its associated power-node data
  must not become a dangling live record.
- For D-48, use one shared admission boundary for scenario load/capture and
  ordinary save/load, applying the approved immediate busy-result policy.
  `Engine.Save.Barrier.beginSave` already refuses an active save
  transaction; `Engine.Scripting.Lua.API.Save` rejects save-during-load and
  load-during-save/load requests. The ordinary contract requires mutual exclusion.
  Exact integration and request/status encoding need implementation design.
- D-49/D-50 settle file lookup: relative CLI paths use the launch directory
  and require an extension; in-game names use the scenario folder and append
  `.yaml` if absent. Absolute paths resolve directly everywhere and require
  the full extension. Report the resolved path in status/diagnostics.
  Source evidence:
  `app/Main.hs` already captures `callerDirectory` before root resolution, and
  `app/CLAUDE.md` documents runtime root-relative I/O plus the audio preview's
  caller-relative explicit file path. D-49 replaces the earlier proposed
  resource-root-relative in-game file lookup with scenario-folder-relative access.
- For D-12, obtain a coherent capture across state owners before encoding the
  current scenario values. Existing save-barrier ownership is relevant evidence;
  serializing a succession of unrelated live reads would not ensure one baseline.
- D-51 approves pausing for coherent scenario capture and leaving the arena
  paused until explicit resume, matching `docs/persistence_contract.md:59`.
  D-10 already settles paused scenario loads.

## Open questions

### Q-1. Does capture produce a starting scenario or a session snapshot?

Resolved by D-3 and D-4, refined by D-8 and D-12: a reproducible scenario baseline
with all current supported scenario values captured as overrides. This includes
unit conditions; orders and player progress remain excluded.

### Q-2. What content and editing controls must the first version support?

Resolved by D-13, with inventory and override behavior in D-6 through D-8 and
D-12. The full listed content is in scope, using existing editing tools and file
overrides. Map the exact supported fields and references to runtime owners next;
surface any newly discovered behavior choices rather than inferring them.

### Q-3. What are the world bounds and topology?

The main choice is resolved by D-9: expandable by default, finite rectangular
bounds when dimensions are specified. The initial 5-by-5 chunks (80 by 80 tiles,
using `src/World/Chunk/Types.hs:chunkSize = 16`) are not the total current map.
D-14 settles out-of-bounds objects: skip and warn while loading usable content.
D-18 settles fluid edges: no flow or drainage across the finite boundary.
D-19 settles the centered world, exact odd/even tile indexing, and camera-only
starting coordinate. D-20 settles the camera default and capture behavior.
D-21 settles terrain/fluid clipping versus whole-location/building rejection.
The product choices in this question are resolved. Exact dimensions must be
honored even at partial chunks; chunk storage and auditing all affected systems
remain implementation responsibilities. Surface any newly discovered behavior
choice with concrete evidence rather than reopening these settled decisions.

### Q-4. What must remain reproducible after files or content change?

D-22 settles versioned presets with migration functions, explicit-value
preservation across changed defaults, and deterministic fallback for newly added
stats absent from old scenarios. D-23 resolves unsupported newer formats: warn,
do not load, and preserve the current arena. D-27 settles YAML with one independent
top-level scenario-format version. D-44 settles retained support for every
released scenario format from v1 and pre-construction migration failure.
Stable scenario references are distinct from tags under D-29; exact encoding
belongs to SCN-01. D-52 settles normal save/load ownership and v1 scope, subject
to D-45's explicit stop if major redesign is needed. Q-4's product choices are
resolved. D-46 reaffirms D-27's independent version policy;
source inspection establishes per-component ordinary versions and existing
arena restore support. The existing save
format and normal game-save guarantees remain
authorities for any persisted state changes. Repository evidence: persistence
contract section 5 uses explicit per-component migrations for ordinary saves;
currentSaveVersion is only a transitional in-memory bridge, not the preset format.
D-24 settles source-file changes: loading/migration never rewrites a file;
only an explicit save over the old file replaces it.

### Q-5. How does loading interact with an existing session?

The initial-run policy is resolved by D-10: report ready only after initialization
and overrides complete, leave simulation paused, and explicitly start it.
D-11 resolves coexistence: one testing scenario, replacing the previous testing
session. D-14 resolves recoverable bad input: skip it, warn, and load the usable
remainder. D-16 resolves unreadable files and core syntax failures: preserve the
current screen/scenario, show a popup warning, and report an unsuccessful load without
constructing a replacement. D-17 settles rejected containers: recursively omit
their contents. D-25 settles the interactive arena Escape menu; D-26 settles F7
as the developer entry point; D-28 settles direct CLI boot and Lua load/save
automation. D-43 settles runtime construction failure: prefer preserving the
previous arena, allow fresh default flat-arena recovery, and report failure
either way. D-47 settles required dependency rejection and optional reference
fallback. D-48 settles immediate busy rejection for overlapping operations.
D-49/D-50 settle paths and extension handling: CLI relative paths use the launch
directory with a full extension, in-game names use the scenario folder with
optional extension shorthand, and absolute paths work everywhere with a full
extension. D-51 settles capture pausing and explicit resume. The product-policy
choices in Q-5 are resolved. Exact API/status encoding and storage integration
remain implementation responsibilities; surface any newly discovered material
behavior choice before treating it as an agreed contract.

### Q-6. How should initial unit variation be handled?

Resolved by D-5: repeatable individual variation from the unit definitions,
with correct initialization of dependent values. D-12 clarifies this supplies
fallbacks only; current captured values are explicit overrides.

### Q-7. Which item properties does inventory capture preserve?

Resolved by D-7: retain the physical item properties and permit explicit file
overrides. D-8 additionally permits file-authored unit health conditions.

### Q-8. What does recapture do with changed starting values?

Resolved by D-12: write all current scenario values as explicit overrides, replacing
old values. The proposal to retain original health/stat overrides after play was
rejected. Newly placed units also export their current values. Deterministic
defaults apply only to omitted fields in files, not in place of captured values.

### Q-9. How should automated tests grade loads that issue warnings?

Resolved by D-15: unexpected loader warnings fail automated test setup. Tests of
malformed-content handling may explicitly expect specific warnings and must
assert correct handling. D-14 still permits loading the usable scenario.

### Q-10. What is the complete gameplay tag API and persistence contract?

D-29 settles flexible many-to-many assignment and gameplay-wide usefulness,
separate from automatic scenario identity. D-31 settles persistence through every
scenario and ordinary save/load path. D-32 settles units/buildings/locations/
plants/items/tiles, excluding whole worlds and new multiworld support. D-33
requires no per-object storage for absent tags, and D-34 approves a typed sparse
registry. D-35 settles ordered category maps and both lookup directions. Define
query scope/combinations, runtime mutation, and destruction/transfer behavior.
D-36 settles actual tile removal: delete its tag assignments. D-37 retains tags
on surviving tiles through material changes and assigns semantic recovery to
scripts. D-38 settles existing-object retention and new-object non-inheritance.
D-39 settles native all/any/none evaluation. D-40 settles inclusion of untagged
objects, the loaded-tile universe, and complete composed set expressions in one
native request. D-41 settles list/count/exists result modes. D-42 settles the
mutation/inspection operations and single/multiple/all-category query scope
with typed identities. The core product choices in Q-10 are resolved; document
the exact implementation contract and surface newly discovered material choices.

Repository evidence: faction tags in `Unit.Faction.Profile` and
`docs/engine_contracts.md` already affect alliances and combat relations.
The searched Lua registration/API surfaces expose no general object
add/remove/list/query tag API. Verify tracker overlap for this expanded scope
before declaring readiness. Any new runtime state must be classified under the
persistence and capability inventories, with corresponding migrations/gates.

### Q-11. Can underground or empty positions carry tile tags?

Resolved by D-54: individual `(x, y, z)` cells qualify only when they exist,
are loaded, and are non-air. Unknown underground positions and air are not
tag targets. The owner approved the existing-loaded-target rule generally;
eviction still preserves already assigned tags under D-31/D-36.

### Q-12. Which material profile should all default arena paths share?

Resolved by D-55: four loam layers over twelve granite layers everywhere in
the default arena. The owner also allowed indefinite granite; the bounded
option was selected within that authorization to fit the existing model.

### Q-13. How should legacy arena saves reconstruct their base?

Resolved by D-56: reconstruct legacy arenas using the corrected uniform
sixteen-layer base and replay saved edits. The owner accepts the added
subsurface material and possible excavation differences. Loading does not
rewrite the source save. A separate legacy terrain profile is not required.

### Q-14. Does scenario construction enforce gameplay insertion capacity?

Resolved by D-57: restore exact authored/captured contents while normal gameplay
transfers retain their restrictions. Source inspection at master
`4634fbaf1eb87d90ba83df214c25b1fa22c861d4` found two distinct boundaries:
`Item.Materialize.materializeNode` constructs initial recursive contents,
whereas `Item.Ownership` validates later moves against snapshotted storage
weight/bulk limits and refuses inserts when required capacity/bulk data is
absent. Existing kits and legacy instances can hold contents despite such
absence; an existing tree is not proof that a new insertion would pass.

The owner selected exact restoration over enforcing insertion capacity and
omitting contents on scenario load. Invalid definitions/fields and malformed
ownership still follow the existing validation contract. D-57 clarifies the
SCN-09 boundary without changing ordinary inventory rules.

### Q-15. Should fatal CLI scenario startup failure exit headless/offscreen?

Resolved by D-58 on 2026-09-24: exit nonzero after fatal headless/offscreen
scenario CLI startup failure, preserving graphical and existing-session
recovery and valid partial-load warning behavior. D-28 requires direct scenario
boot and an observable result; D-16/D-43 preserve interactive recovery. They do
not explicitly choose the process lifecycle after a fatal scenario-load result
when headless/offscreen was launched with the scenario CLI flag.

At master `1ccbedd9ccc03484e3fb33511b14ddd536d0ff19`, `app/CLAUDE.md`
requires malformed/unsupported CLI flags to exit before engine startup, while
`docs/headless_console.md` distinguishes console listener READY from load
completion. Neither establishes the new scenario startup failure policy.

The owner chose nonzero exit over remaining paused with socket access after
fatal startup failure. This affects SCN-15 startup handling and its automation
evidence without changing shared load-service outcomes, Lua requests in an
already open session, or malformed-flag rejection.

### Q-16. How should Exit to Menu behave during a scenario operation?

Resolved by D-59 on 2026-09-24: disable Exit to Menu while busy, re-enable
after terminal success or failure, and keep Exit to Desktop available.

D-25 requires the established settings and exit actions, while D-48 governs
competing save/load/capture requests. Neither explicitly settles Exit to Menu
while a scenario load or capture is still running.

Current evidence at master `1ccbedd9ccc03484e3fb33511b14ddd536d0ff19`:
`scripts/pause_menu.lua:onExitToMenu` invokes the session teardown registry,
queues `world.destroyAll()`, clears the active-world and pause state, and
switches to the main menu. `scripts/lib/session_teardown.lua` documents the
asynchronous drain window after destruction is queued. There is no scenario
operation gate in that action. Exit to Desktop calls `engine.quit()`; the
approved services retain asynchronous shutdown behavior.

Approved choice: disable Exit to Menu while a conflicting operation owns the shared
save/load/capture gate; show why it is unavailable and re-enable it after
terminal success or failure. Recheck at activation so a concurrent request
cannot bypass the restriction. Do not queue a deferred exit or cancel the
operation. Keep Exit to Desktop available through normal engine shutdown.

Alternative: accept Exit to Menu by cancelling/draining the operation safely
before tearing down the session. This requires an additional cancellation
contract rather than only presenting the existing service state.

The owner selected the first option. D-59 records the decision; no tracker
mutation is authorized until the complete SCN-16 draft receives approval.

## Verification strategy

For D-3 through D-5, compare initial units across fresh-process loads of one
preset with unchanged content: their default rolled stats/body/skills must match
where omitted; explicit authored or captured values must take precedence
repeatably (D-8/D-12). Orders and player progress must not carry over.
Verify the derived body and resource values as well as the rolled inputs.
For D-6, verify exact inventory membership, recursive container ownership, and
equipment slots survive scenario export/import, with no extra default spawn kit.
For D-7 and D-8, include a half-full canteen, a near-breaking weapon, and authored
health conditions; verify their initial state before gameplay advances and their
use by the real relevant gameplay systems. Verify unsupported definitions and
overrides are rejected individually with specific warnings while usable content
loads, following D-14; unexpected warnings fail automated setup under D-15.
For D-9, verify expansion when dimensions are absent and actual finite limits
when present, including movement and chunk requests at edges. Compare fresh,
evicted/regenerated, and restored terrain for the same authored coordinates.
For D-10, verify readiness includes all asynchronous entity initialization and
overrides, the completed baseline remains stable while paused, and gameplay
begins only after explicit resume. Review Lua callbacks that run while paused
rather than assuming the pause flag alone proves these guarantees.
For D-11, load two scenarios consecutively and verify only the second is active,
with no units, orders, or session-wide test progress leaking from the first.
For D-12, change unit stats/health and inventory state after loading, capture,
and load in a fresh process. Compare the resulting baseline to the captured
values, including a healed formerly-authored wound represented as an explicit
empty list. Verify export includes current values even when equal to defaults,
and removing an override deliberately restores the deterministic fallback.
For D-14, load a scenario containing usable content alongside an unknown definition,
unsupported override, and out-of-bounds object. Verify the usable subset loads
fully initialized/paused, each rejection is warned about, and the load result
exposes those warnings to both interactive users and test callers.
For D-15, verify an ordinary test fails setup when a requested soldier is skipped;
verify a malformed-content test can expect that warning and check the partial
scenario, while any additional unexpected warning still fails its setup.
For D-16, attempt missing/unreadable files and syntactically broken documents;
verify a warning is presented, no new scenario is constructed, the current screen
and scenario remain available, and callers receive an unsuccessful load result.
Separately use syntactically valid documents with missing/wrong fields and verify
entry-level handling preserves usable content under D-14.
For D-17, reject a container with valid nested contents and verify none of its
descendants are instantiated, including as ground items; independent valid
objects still load. Cover rejected units with carried/equipped containers too.
For D-18, place fluid against each finite-map edge and verify no flow crosses
the boundary and no fluid is lost through off-map drainage. Include edges that
fall inside a chunk once the exact rectangle specification is settled.
For D-19, verify exact tile counts and centered placement for even and odd sizes
using the approved indexing convention. Changing only the starting camera
coordinate must not change world bounds or object positions.
For D-20, omit the camera coordinate and verify the view starts at (0, 0).
Move the camera, capture, and reload; verify the saved starting position is
restored while object positions and map bounds are unchanged.
For D-21, load terrain and fluid features spanning a map edge and verify only
outside tiles are rejected, with warnings. Load partially out-of-bounds buildings
and locations and verify neither spawns, including any location-owned terrain
stamps or contents, while independent valid features load normally.
For D-22, retain representative scenario-version fixtures and exercise the real
migration/initialization path. Change a definition's default strength while an
older scenario stores strength 12; verify 12 survives. Add a stat absent from that
fixture and verify repeatable fallback from the current definition without
resetting explicit values or treating legitimate omission as malformed input.
For D-23, attempt a scenario declaring an unsupported newer format; verify the
warning and unsuccessful result, no replacement construction, and preservation
of the current scenario/screen. Supported-format partial loading must still work.
For D-24, load/migrate a historical fixture and compare its bytes before/after;
it must remain unchanged. Saving to another destination preserves the source,
and replacing the original requires explicit overwrite intent.
For D-25, verify the arena Escape menu exposes all five approved actions. Exercise
scenario selection, capture/destination selection, overwrite confirmation, Settings
and return, Exit to Menu cleanup, and Exit to Desktop. Obtain rendered UI evidence
through the permitted offscreen workflow; headless checks alone do not prove
the menu's visibility, layout, or hit targets.
For D-26, verify F7 reaches the developer arena and the approved scenario controls;
the main menu must not gain an arena entry. Preserve the existing key-capture
exception while settings are rebinding a key. Audit the F7 lifecycle when
integrating scenario replacement rather than substituting a new player-facing flow.
For D-27, exercise both handwritten YAML and program-exported YAML through the
same parser, version dispatch, and validation path. Verify the explicit format
version survives capture and is independent of ordinary game-save versions.
For D-28, exercise CLI direct arena-scenario boot in headless mode and Lua scenario
load/save through a running process's socket. Verify UI/API/CLI share validation,
initialization, capture, and warnings semantics; compare initialized baselines.
Demonstrate multiple Python-driven scenario cases with independent outcomes, explicit
setup failure on unexpected loader warnings, bounded waits, and retained failure
evidence using the existing probe infrastructure. Keep visual UI verification
offscreen and separate from GPU-free behavioral assertions.
For D-29, verify multiple tags per object and multiple objects per tag, mutation
and queries through gameplay Lua, and unchanged stats/identity when tags change.
Assert that general tags do not implicitly change faction relations. The tag
object coverage and lifetime checks below implement the resolved Q-10 contract.
For D-31, round-trip script-added/removed tags through scenario capture/load,
ordinary manual save/load, and autosave/load, including fresh-process reloads.
Verify current tag assignments are captured coherently, restored onto the correct
objects, and usable by gameplay Lua after each path. Cover older-format migration
once the compatibility contract is settled; inventory declarations alone do not
prove tag restoration.
For D-32, cover all approved target kinds, including equipped/nested items and
individual tiles. Whole-world tagging is outside scope.
For D-33, inspect object layouts and measure retained tag storage against the
number of tagged targets/assignments, including a large untagged population.
Removing all tags must leave no empty target entries. Count any reverse indexes
and intern tables in the evidence; do not infer memory behavior from types alone.
For D-34, prove adding the first tag creates one target entry and removing the
last removes it. Reject empty collections at the registry boundary, preserve
typed identity across persistence, and verify tag mutations do not alter object
stats or deterministic scenario identity.
For D-35, compare object-to-tags and tag-to-objects results after add/remove,
last-tag removal, lifecycle changes, scenario loading, and ordinary save loading.
Rebuilding the reverse index must reproduce the same relationships and ordering.
Measure tagged-population scaling by category and include both indexes' memory.
For D-36, tag a tile, remove it, and verify both indexes lose the assignment;
recreate a tile at that coordinate and verify no old tags reappear, including
after scenario/ordinary save-load. Separately unload/reload a chunk and verify
its logically surviving tiles retain their tags.
For D-37, change a tagged tile's material in place and verify its tags remain.
Delete a tagged destination and verify no engine-driven relocation occurs;
exercise a script that detects the missing destination and explicitly reassigns
its tag. Cover chunk eviction so it cannot trigger a false deletion response.
For D-38, move/equip/store/drop tagged physical items and verify memberships
follow the same instances. Destroy an object and verify only its assignments
are removed from both indexes; objects sharing those tags remain queryable.
Consume tagged inputs to create a new output and verify no automatic inheritance,
then demonstrate explicit script assignment when a caller wants that behavior.
For D-39, exercise all/any/none separately and together against overlapping tag
sets; compare results to simple reference set algebra, including missing tags,
duplicate requested tags, exclusions, and empty matches. Verify the Lua API
returns the same result as the Haskell evaluator and does not mutate membership.
For D-40, include untagged objects in complement and exclusion results; compare
nested union/intersection/difference/complement expressions with a reference
evaluator. Cover an entirely untagged universe, absent tag names, empty loaded
tile sets, and broad results without truncation. Verify tile queries include
only currently loaded tiles while chunk eviction/reload preserves assignments.
Verify composed queries marshal only their final result to Lua and do not add
registry entries for untagged objects.
For D-41, compare list cardinality with count and list nonemptiness with exists
against the same unchanged state, including nested expressions, untagged
objects, and empty results. Verify count/exists return scalar Lua values and
do not marshal complete matching-object lists across the API boundary.
For D-42, cover add/remove/replace/clear/list/membership across all supported
target kinds. Query one, several, and all categories with overlapping numeric
identities and confirm typed results keep objects distinct. Check complements
against only the requested categories and apply list/count/exists consistently.
For D-43, exercise the chosen runtime failure recovery path and verify the
arena remains paused and usable, the warning identifies the failed request,
and load status remains unsuccessful even if a default flat arena is recovered.
If staging preserves the old arena, verify it remains intact on pre-swap
failure. Exercise post-swap reconciliation failure without claiming readiness
or rollback. Retain separate D-16/D-23 checks that never replace the session.
For D-44, retain historical YAML fixtures and exercise migration through the
real scenario loader for every released version. Confirm explicit values
survive, new omitted fields receive documented defaults, and source bytes
remain unchanged. Inject unrecoverable migration failure and verify the old
arena survives, no replacement construction occurs, and status is unsuccessful.
For D-47, exercise a chain of required references to a rejected object and
verify the whole dependent chain is omitted with diagnostics, while unrelated
objects survive. Cover optional references with valid absence/default states,
containment, forward references to valid objects, and valid mutually referring
entries; rejection must follow unresolved targets, not mere declaration order
or a cycle whose targets all exist.
For D-48, issue overlapping requests across scenario load/capture and ordinary
save/load. Verify immediate busy rejection, no queued later execution or
cancellation, unchanged active-request status, and successful retry after the
first request completes. Capture must not publish partially initialized data.
For D-49/D-50, use distinct launch and resource directories with same-named
files to verify CLI and in-game resolution. Cover in-game names with/without
`.yaml`, explicit absolute paths, missing files, and extensionless CLI/absolute
path rejection. Failed lookup must preserve the current arena. Verify relative
in-game access stays within the scenario folder and explicit absolute access
still works.
For D-51, capture from running and already-paused sessions and verify coherent
values across state owners, successful YAML publication, and a paused session
after completion. Confirm no implicit resume and that an explicit resume works.
Exercise failure after admission without inadvertently resuming simulation.
For D-52, ordinary-save a modified bounded scenario with tags and nested items,
remove or change the source YAML, then restore the save in a fresh process.
Verify the saved session, bounds, tags, and later scenario recapture are correct.
Exercise legacy arena saves with expandable bounds and empty general tags.
These checks supplement, rather than replace, the existing arena save probes.

## Delivery plan

The following seventeen slices are approved under D-53. Each owns its
required docs, migrations, and targeted evidence. The list is a topological
order; independent foundations may proceed separately when checkout conflicts
allow. It is not a promise that every slice has equal cost.

### SCN-01. Define the versioned scenario schema and validation contract

- **Outcome:** A documented, typed YAML v1 representation with decode/migration/validation fixtures.
- **Scope:** Map all approved content fields to owners, units, defaults, explicit-empty semantics, references and derived-state dependencies. Define stable scenario identities and structured per-entry diagnostics. Decode syntax separately from recoverable content validation; retain v1 migration fixtures.
- **Phase:** 1 — foundations
- **Depends on:** `none`
- **Ordering:** `can land first`
- **Relevant decisions:** D-3–D-8, D-12–D-17, D-22–D-24, D-27, D-29, D-44, D-46, D-47.
- **Acceptance signals:** Minimal and fully explicit examples decode; malformed syntax is fatal; unknown fields/definitions and unresolved required references have the approved warning/rejection behavior. Reference order does not change identity resolution. Authoring documentation and complete field table accompany the codec.
- **Out of scope:** Runtime world replacement, Lua/UI entry points, and new gameplay fields outside the approved schema.
- **Open questions:** None at the agreed product-policy level. If concrete field ownership or integration reveals a new material choice, stop and ask before implementing it; D-52's redesign stop remains binding.

### SCN-02. Implement the sparse tag registry and native query evaluator

- **Outcome:** A pure typed registry and composed set evaluator with list/count/exists results.
- **Scope:** Use category-separated forward/reverse ordered maps and nonempty assignments. Accept the category universe from object owners, including untagged objects; evaluate nested set operations without Lua intermediate lists. Define mixed typed target results and loaded-tile universe inputs.
- **Phase:** 1 — foundations
- **Depends on:** `none`
- **Ordering:** `independent`
- **Relevant decisions:** D-29, D-32–D-35, D-39–D-42.
- **Acceptance signals:** Reference algebra agrees across all operations/modes and mixed categories; mutations keep both indexes coherent; last-tag removal removes empty records; untagged objects require no per-object field. Retained storage evidence includes both indexes.
- **Out of scope:** Runtime lifecycle hooks, save codecs, faction policy, and Lua exposure.
- **Open questions:** None at the agreed product-policy level. If concrete field ownership or integration reveals a new material choice, stop and ask before implementing it; D-52's redesign stop remains binding.

### SCN-03. Persist tags for units, buildings, and locations

- **Outcome:** Live unit/building/location tag state survives save/load and is removed on actual destruction.
- **Scope:** Attach sparse state to existing owners, wire identity/lifecycle cleanup and whole-session replacement, classify persistence and add versioned component migrations and fixtures. Restore assignments and derive reverse indexes. Untagged legacy content starts with no assignments.
- **Phase:** 2 — persistent tag owners
- **Depends on:** `SCN-02`
- **Ordering:** `critical path`
- **Relevant decisions:** D-31–D-38, D-52.
- **Acceptance signals:** Creation/mutation/destruction, reused IDs, ordinary save/load, session replacement, and sibling objects sharing tags behave correctly. Required inventory/capability and migration gates pass.
- **Out of scope:** Physical-item and terrain/flora tag hooks, public Lua mutation, faction membership changes.
- **Open questions:** None at the agreed product-policy level. If concrete field ownership or integration reveals a new material choice, stop and ask before implementing it; D-52's redesign stop remains binding.

### SCN-04. Preserve physical-item tags through transfers and saves

- **Outcome:** One physical item's tags survive every ownership transfer and disappear when it is destroyed.
- **Scope:** Integrate ground, equipped, carried, accessory, building-held, delivered-material and recursively nested item owners using existing item identity. Include ordinary save capture/restore and cleanup; do not attach tags to display-group rows or copy them onto newly crafted outputs.
- **Phase:** 2 — persistent tag owners
- **Depends on:** `SCN-03`
- **Ordering:** `critical path`
- **Relevant decisions:** D-6, D-31–D-38, D-52.
- **Acceptance signals:** Real equip/store/withdraw/transfer/drop and nested-container round trips retain memberships; consumption/destruction/rejected-container paths leave no orphan tags; same-definition items stay distinct.
- **Out of scope:** New transfer rules, automatic tag inheritance, scenario item materialization.
- **Open questions:** None at the agreed product-policy level. If concrete field ownership or integration reveals a new material choice, stop and ask before implementing it; D-52's redesign stop remains binding.

### SCN-05. Persist tile and flora tags across residency changes

- **Outcome:** Tile/plant tags survive eviction and saves while deletion removes the correct memberships.
- **Scope:** Use canonical tile keys, existing plant occurrence IDs, and the crop-plot owner where applicable. Distinguish residency from destruction and material change from replacement. Preserve sparse persistent assignments without allocating a field on every tile. Use the current flora identity/condition authorities; coordinate with #2526.
- **Phase:** 2 — persistent tag owners
- **Depends on:** `SCN-03`
- **Ordering:** `critical path`
- **Relevant decisions:** D-31–D-38, D-40, D-52, D-54.
- **Acceptance signals:** Eviction/reload and save/load preserve assignments; material changes retain tags; actual removal and replacement do not inherit; co-tenant plants remain distinct; loaded-tile query universes include untagged resident non-air cells at exact x/y/z coordinates; air and ungenerated or unloaded targets cannot receive new tags or trigger generation/loading.
- **Out of scope:** A competing flora identity/condition system, whole-world tags, automatic meeting-point relocation.
- **Open questions:** None at the agreed product-policy level. If concrete field ownership or integration reveals a new material choice, stop and ask before implementing it; D-52's redesign stop remains binding.

### SCN-06. Expose the complete gameplay tag API to Lua

- **Outcome:** Scripts can mutate/inspect tags and issue complete native set queries.
- **Scope:** Register add/remove/replace/clear/list/membership operations, category selection, nested query expressions, and list/count/exists results. Document typed results and the loaded-tile scope. Follow the current Lua registration and argument-validation owners; distinguish this API from #2551's faction API.
- **Phase:** 3 — scriptable tags
- **Depends on:** `SCN-03`, `SCN-04`, `SCN-05`
- **Ordering:** `critical path`
- **Relevant decisions:** D-29, D-39–D-42, D-54.
- **Acceptance signals:** Real Lua calls exercise every target kind, invalid arguments without accidental mutation, all query modes and mixed identities; count/exists marshal scalars only. Examples show meeting points and defender/ranged/reserve selection.
- **Out of scope:** Scenario load/save verbs and any implicit faction/diplomacy effect.
- **Open questions:** None at the agreed product-policy level. If concrete field ownership or integration reveals a new material choice, stop and ask before implementing it; D-52's redesign stop remains binding.

### SCN-07. Unify arena generation and reconstruction

- **Outcome:** Startup, camera loading, explicit chunk demand, eviction regeneration, and save restoration agree on arena terrain.
- **Scope:** Create one arena generation authority across the currently divergent paths, preserving existing arena identification and recorded-seed semantics. Define compatibility handling for any reconstruction change and include it with the code. Preserve ordinary generated-world behavior.
- **Phase:** 4 — world foundation
- **Depends on:** `none`
- **Ordering:** `independent`
- **Relevant decisions:** D-4, D-9, D-22, D-52, D-55, D-56.
- **Acceptance signals:** The same arena coordinate yields consistent base terrain through every path; legacy arena saves restore deliberately; affected worldgen-output gates, baselines and migrations are included where required by the world contract.
- **Out of scope:** Finite-bound enforcement, authored content patches, scenario API/UI.
- **Open questions:** None at the agreed product-policy level; Q-13 is resolved by D-56. New material integration choices require owner input, and D-52's redesign stop remains binding.

### SCN-08. Enforce and persist optional finite arena bounds

- **Outcome:** An arena can use exact centered nonwrapping dimensions that survive ordinary saves.
- **Scope:** Apply one bounds authority to chunk admission, tile lookup/editing, actor movement/pathing, placement and runtime fluid edges. Handle partial chunks. Preserve expandable arenas when bounds are absent and migrate old saves accordingly.
- **Phase:** 4 — world foundation
- **Depends on:** `SCN-01`, `SCN-07`
- **Ordering:** `critical path`
- **Relevant decisions:** D-9, D-14, D-18–D-21, D-52.
- **Acceptance signals:** Odd/even dimensions match D-19 exactly; units and fluids cannot escape; terrain/fluid patches clip per tile and buildings/locations are validated as whole footprints; save/load retains the same bounds.
- **Out of scope:** Changing generated-world topology or using camera coordinates as world origin.
- **Open questions:** None at the agreed product-policy level. If concrete field ownership or integration reveals a new material choice, stop and ask before implementing it; D-52's redesign stop remains binding.

### SCN-09. Materialize and capture scenario item trees

- **Outcome:** Typed scenario item trees round-trip all supported physical properties and current nested contents.
- **Scope:** Implement item definition resolution, explicit/default property values, stable reference mapping and inventory ownership adapters. Use current item IDs internally and preserve tags through the mapping. Prepare reusable item adapters for unit, building and ground ownership.
- **Phase:** 5 — content adapters
- **Depends on:** `SCN-01`, `SCN-04`
- **Ordering:** `critical path`
- **Relevant decisions:** D-6, D-7, D-12, D-14, D-17, D-29, D-38, D-57.
- **Acceptance signals:** Half-full canteens, near-broken weapons, explicit zero/empty values, temperature/bulk and nested contents round-trip; rejected owners exclude their contents; captured items do not receive extra definition kits.
- **Out of scope:** Unit body initialization, world replacement, new inventory rules.
- **Open questions:** Q-14 is resolved by D-57. New material integration choices require owner input; D-52's redesign stop remains binding.

### SCN-10. Initialize and capture deterministic scenario units

- **Outcome:** Units receive reproducible omitted values and consistent explicit stats, health, and loadouts.
- **Scope:** Use private scenario-keyed randomness and existing domain initialization. Apply the schema to supported stats/body/skills/health and item ownership, constructing dependent values coherently. Capture current supported values as explicit overrides; exclude orders and player progress.
- **Phase:** 5 — content adapters
- **Depends on:** `SCN-01`, `SCN-03`, `SCN-09`
- **Ordering:** `critical path`
- **Relevant decisions:** D-4–D-8, D-10, D-12, D-22, D-29.
- **Acceptance signals:** Fresh constructions with unchanged definitions agree; explicit strength survives default changes; newly omitted fields get defaults; injuries/healing and empty loadouts round-trip; normal gameplay RNG and ordinary spawn behavior are unchanged.
- **Out of scope:** Deterministic gameplay replay, engine-memory dumps, new combat/health mechanics.
- **Open questions:** None at the agreed product-policy level. If concrete field ownership or integration reveals a new material choice, stop and ask before implementing it; D-52's redesign stop remains binding.

### SCN-11. Materialize and capture terrain, fluids, and flora

- **Outcome:** World-content adapters construct and recapture the supported physical baseline.
- **Scope:** Apply authored terrain/fluid patches with tile-level bounds clipping; create flora/crop occurrences through existing owners with supported current physical values. Export durable authored changes and live supported values coherently, retaining IDs/tags and respecting the existing regeneration model.
- **Phase:** 5 — content adapters
- **Depends on:** `SCN-01`, `SCN-05`, `SCN-08`
- **Ordering:** `critical path`
- **Relevant decisions:** D-12–D-14, D-19–D-21, D-31, D-36, D-37, D-54.
- **Acceptance signals:** Out-of-bounds patch tiles warn while interior tiles load; authored/captured world values and plant memberships survive reconstruction and scenario round trips; no player work designations are smuggled into the baseline.
- **Out of scope:** New flora mortality/condition mechanics, new hydrology generation, location placement.
- **Open questions:** None at the agreed product-policy level. If concrete field ownership or integration reveals a new material choice, stop and ask before implementing it; D-52's redesign stop remains binding.

### SCN-12. Materialize and capture buildings and real locations

- **Outcome:** Authored structures and locations have real runtime identity, contents and valid dependent references.
- **Scope:** Reuse checked placement/overlay and construction owners, composing item/unit/world adapters. Validate complete footprints before mutation; reconstruct required power/content relationships; capture physical content without player discovery/expedition history or duplicate implicit spawns.
- **Phase:** 5 — content adapters
- **Depends on:** `SCN-01`, `SCN-03`, `SCN-08`, `SCN-09`, `SCN-10`, `SCN-11`
- **Ordering:** `critical path`
- **Relevant decisions:** D-12–D-14, D-17, D-21, D-29, D-47.
- **Acceptance signals:** An empty ruin and occupied ruin establish real location instances; valid contents and references load once; invalid footprints leave no stamps or contents; required dependency rejection preserves unrelated content.
- **Out of scope:** Changing expedition gameplay or #2640's specification, new building/location art.
- **Open questions:** None at the agreed product-policy level. If concrete field ownership or integration reveals a new material choice, stop and ask before implementing it; D-52's redesign stop remains binding.

### SCN-13. Load scenarios through one paused session transaction

- **Outcome:** A shared engine service loads the usable scenario subset and reports an honest terminal result.
- **Scope:** Compose migration, validation, private construction, identity/tag mapping and session publication. Reuse save/load exclusion and staging where practical. Implement preflight preservation, preferred old-arena preservation or permitted flat fallback on construction failure, and post-publication reconciliation failure handling. Persist scenario identity needed for ordinary-save restoration and later capture.
- **Phase:** 6 — complete services
- **Depends on:** `SCN-01`, `SCN-06`, `SCN-08`, `SCN-09`, `SCN-10`, `SCN-11`, `SCN-12`
- **Ordering:** `critical path`
- **Relevant decisions:** D-10, D-11, D-14–D-17, D-23, D-28, D-43, D-44, D-47, D-48, D-52.
- **Acceptance signals:** Ready means all supported content/overrides initialized and paused; repeated loads replace old testing state; warning/fatal/busy outcomes are distinguishable; ordinary-save restore needs no source YAML and recapture works; injected failures never report false readiness.
- **Out of scope:** Public CLI/menu entry, a major persistence redesign without renewed owner approval.
- **Open questions:** None at the agreed product-policy level. If concrete field ownership or integration reveals a new material choice, stop and ask before implementing it; D-52's redesign stop remains binding.

### SCN-14. Capture scenarios through a coherent snapshot and safe file write

- **Outcome:** The engine exports current scenario state to YAML and leaves the arena paused.
- **Scope:** Use a coordinated capture boundary, all content adapters and current tag memberships. Emit explicit values including empty/zero overrides and current format version. Preserve source files on load/migration; require explicit overwrite intent and publish a complete output file with storage failure diagnostics.
- **Phase:** 6 — complete services
- **Depends on:** `SCN-13`
- **Ordering:** `critical path`
- **Relevant decisions:** D-2, D-12, D-24, D-31, D-48, D-51.
- **Acceptance signals:** An edited loaded scenario recaptures its healed wounds and changed inventory rather than original overrides; a fresh engine loads equivalent supported values; failure cannot leave a partial destination or masquerade as success; capture ends paused.
- **Out of scope:** Capturing player progress/orders or adding a session-save format.
- **Open questions:** None at the agreed product-policy level. If concrete field ownership or integration reveals a new material choice, stop and ask before implementing it; D-52's redesign stop remains binding.

### SCN-15. Expose scenario services through Lua and direct CLI boot

- **Outcome:** Python/scripts can load, capture, and poll scenarios in a running engine or start directly in one.
- **Scope:** Register shared-service Lua load/save/status functions; add the scenario CLI path flag for supported boot modes. Implement launch-directory CLI paths, in-game scenario-folder names, absolute-path and extension rules. Report resolved paths, admission and completion separately; distinguish console READY from scenario readiness.
- **Phase:** 7 — entry points
- **Depends on:** `SCN-13`, `SCN-14`
- **Ordering:** `critical path`
- **Relevant decisions:** D-15, D-16, D-28, D-48–D-50.
- **Startup failure clarification:** D-58 resolves Q-15; fatal scenario startup in headless/offscreen exits nonzero after diagnostics and orderly teardown.
- **Acceptance signals:** Script, socket and CLI use identical baseline/warning semantics; busy retries work; separate launch/resource directories resolve correctly; direct boot reaches the paused requested baseline or an explicit failure; malformed flags fail according to app rules.
- **Out of scope:** A second Python runner, GUI file browser, input recording/replay.
- **Open questions:** Q-15 is resolved by D-58. New material integration choices require owner input; D-52's redesign stop remains binding.

### SCN-16. Add the arena scenario browser and Escape-menu workflow

- **Outcome:** F7 arena users can load scenarios, capture as a file, and use all agreed menu actions.
- **Scope:** Add Load Scenario and Save Scenario As with overwrite confirmation and warnings. Keep Settings, Exit to Menu and Exit to Desktop through established actions; retain F7 as the developer entry. Use the shared services and reflect busy/completion state without adding a player-facing arena entry.
- **Phase:** 7 — entry points
- **Depends on:** `SCN-15`
- **Ordering:** `critical path`
- **Relevant decisions:** D-24–D-26, D-43, D-48–D-51, D-59.
- **Acceptance signals:** Offscreen evidence covers navigation, file selection/names, overwrite refusal/acceptance, warnings, settings/exits and resize/input behavior. Loading/capture ends paused and another operation remains usable after failure.
- **Out of scope:** New art, a main-menu arena button, comprehensive property editing, ordinary game-save menu additions.
- **Open questions:** Q-16 resolved by D-59. New material integration choices require owner input; D-52's redesign stop remains binding.

### SCN-17. Ship reusable scenarios and Python regression coverage

- **Outcome:** Documented sample scenarios and existing-runner Python tests prove the complete workflow.
- **Scope:** Add representative YAML fixtures and Python consumers of the shared status/query APIs. Cover fresh-process baselines, recapture, warnings, expected malformed-content tests, native tag algebra, and ordinary save/load without source YAML. Register with existing probe tooling and preserve per-run evidence.
- **Phase:** 8 — integration
- **Depends on:** `SCN-06`, `SCN-15`, `SCN-16`
- **Ordering:** `critical path`
- **Relevant decisions:** D-4–D-15, D-22–D-24, D-28, D-31, D-40–D-52.
- **Acceptance signals:** Unexpected loader warnings fail setup; explicitly expected warnings are asserted; successful fallback is never graded as the requested scenario; list/count/exists agree; bounded edges and legacy migrations have behavioral evidence. Authoring/API docs and fixture provenance are complete.
- **Out of scope:** Silently porting #2640, a new test framework, claiming deterministic gameplay replay.
- **Open questions:** None at the agreed product-policy level. If concrete field ownership or integration reveals a new material choice, stop and ask before implementing it; D-52's redesign stop remains binding.

## Source notes

The owner explicitly requested a continuing foreground walkthrough: when the
work is unfinished, prompt for the next concrete blocker rather than ending with
a passive status or asking the owner to work out the next step.

Foreground session: `20260924T141938Z-testing-predefined-arena-worlds-fcdf57`.
This replaces the completed investigation session
`20260924T141315Z-issue-2640-d97b3a`; completion there records the pivot decision,
not completion of GitHub issue #2640.
