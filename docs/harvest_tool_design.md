# Harvest designation tool design

This design defines a player-directed harvest workflow for plants: a dedicated
tool, durable and visible harvest intent, reliable single-target and drag-box
selection across steep terrain, and artwork that belongs beside Synarchy's
canonical Dig and default-tool visuals.

Design state: `ready for issue processing`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [ ] EPIC. Add player-directed plant harvesting with cliff-correct selection
- [ ] HARV-1. Define and expose harvest-designation state and commands
- [ ] HARV-2. Make plant selection correct for clicks and drag boxes across cliffs
- [ ] HARV-3. Execute player harvest designations through unit work
- [ ] HARV-4. Add canonical harvest-tool and designated-plant artwork
- [ ] HARV-5. Integrate and verify the complete player harvest workflow

## Epic contract

- **Goal:** Let a player intentionally select one or many harvestable plants,
  see exactly which plants are designated, and have units complete the intended
  harvest without terrain-height ambiguity.
- **Done when:** The Harvest tool is discoverable; click and drag behavior is
  settled and implemented; drag selection remains correct around large cliffs;
  selected plants have an unambiguous canonical-style designation marker;
  eligible units complete all ordinary harvesting through player-designated
  work; autonomous harvesting occurs only as a starvation-survival behavior;
  and the interaction, rendering, persistence, cancellation, and terrain-edge
  contracts have suitable automated or manual verification.
- **Users and operators:** Players directing colony work; maintainers of world
  picking, designation state, unit AI, HUD tools, and tool artwork.
- **Arc label:** None proposed

## Current state and evidence

- Harvesting exists as a Lua-facing backend (`world.harvestFlora`) that can
  harvest wild flora or planted crops, spawn ground-item yields, and start a
  wild plant's regrowth timer or clear a one-shot planted crop.
- Units currently reach that backend autonomously through two distinct actions:
  hunger-driven `forage` and routine, farming-skill-scaled `auto_harvest`.
  There is no Harvest toolbar entry, `harvest_tool.lua`, harvest-designation
  map, save component, command family, or designation render pass today.
- Mine, Chop, and Till are not click-drag tools: each is a two-click tile
  rectangle. The first click stores a picked terrain tile, the second picks
  another terrain tile, and the world thread enumerates the tile-coordinate
  rectangle between them. Chop already filters that rectangle across all
  surface heights, but it still answers a world-tile rectangle rather than the
  set of plant sprites enclosed by a screen-space gesture.
- `world.pickTile` is one authoritative, elevation-aware single-pixel terrain
  unprojection shared by live hover and Lua clicks. It correctly finds the
  topmost solid tile but does not hit-test flora sprites.
- A real click-versus-drag controller already exists for unit selection:
  `scripts/unit_drag_select.lua` uses a four-window-pixel threshold, draws a
  framebuffer-scaled outline, and asks `unit.hitTestInRect` for rendered unit
  centers inside the screen rectangle. It deliberately disables box-selection
  effects whenever a Mine/Chop/Till/Plant/Build tool claims the press. Harvest
  can reuse or extract its gesture lifecycle, but cannot silently arm the unit
  selection effect.
- Flora rendering already has the geometry needed for a correct plant oracle:
  tile coordinates, sub-tile offsets, actual surface z, camera facing, wrap
  offset, texture size, and a ground-contact anchor. There is no matching flora
  click/rectangle hit-test API yet.
- Existing world designations are canonical tile-keyed maps which capture z for
  marker rendering, persist per world page, and have frozen save DTOs. A new
  persisted harvest layer therefore implies the normal save-format/component/
  compatibility work rather than a Lua-only table.
- The existing coordinated foraging probe can stop before exercising harvest
  behavior when its generated fixture contains no harvestable raspberry or
  clover. That is a test-fixture limitation, not evidence that harvesting is
  absent.
- Every toolbar tool has a 32×32 default and selected texture. The user identifies
  the current Default and Mine/Dig icons as canonical. Chop and Build use the
  same small pixel-art tool language, while Till and Plant are visibly flatter
  glyphs. Existing standing designation textures are 96×64 solid-color
  isometric diamonds rather than plant-specific icons.
- Closed farming epic #331 and child #336 delivered automatic harvest while
  explicitly leaving player designation open. Closed tree-felling issue #97
  delivered the separate Chop designation. A readiness recheck on 2026-08-25
  found no open harvest, designation, toolbar-icon, tool-icon, cliff-drag, or
  flora arc that would duplicate this work; the open flora matches were
  species/probe work rather than player harvesting.

## Desired experience

- The player chooses a dedicated Harvest tool.
- A click designates the topmost eligible plant sprite under the pointer; it
  never harvests immediately or bypasses unit work.
- A click-drag rectangle selects all eligible plants the player visually
  intended by their rendered ground-contact anchors, even when their world
  tiles span sharply different heights.
- Each selected plant gains a clear designation marker that reads at gameplay
  scale and matches the Dig/default-tool art language.
- Units perform the work through a visible, interruptible work lifecycle; the
  harvested yields and plant regrowth behavior continue to use the existing
  harvest backend.
- Non-starving units harvest plants only in response to player designations.
  A starving unit may still autonomously find and harvest food as a survival
  behavior.
- Any otherwise-capable player worker may complete a harvest designation;
  farming skill changes work performance and Farmer-role weighting makes
  skilled farmers prefer it without reserving the job exclusively for them.
- Harvest remains active across repeated add and erase gestures. Escape or
  choosing another toolbar tool exits it; a right-click miss is an erase no-op.
- Ineligible, already-designated, already-harvested, out-of-season, hidden, or
  unloaded candidates behave consistently and give sufficient feedback.

## Scope

### In scope

- A player-facing Harvest tool for currently harvestable non-wood wild flora
  and ripe planted crops, as settled by D-9.
- Single-click and drag-box target selection semantics.
- Correct selection around large cliffs and other material height differences.
- Harvest designation state, rendering, cancellation, lifecycle, and execution.
- A new Harvest tool icon and a distinct designated-plant marker.
- Focused verification of click, drag, terrain-height, rendering, and work
  completion behavior.

### Out of scope

- Changing the underlying yield tables, nutrition values, regrowth durations,
  or seasonal growth rules unless repository evidence shows the designation
  workflow cannot preserve them.
- A wholesale redesign of farming, foraging, chopping, or general unit-job
  arbitration.
- Restyling unrelated tool icons. D-11 preserves those mismatches as a separate
  art-cleanup effort to capture and file with the user.
- Filing tracker issues or changing implementation during design.

## Design

### Interaction model

The tool has two input forms: a single click and a drag rectangle. Both target
actual plant instances rather than merely terrain cells, and cliff geometry
cannot make the selected set diverge from what was visibly enclosed. A click
creates one designation and a drag creates multiple designations; neither
gesture directly harvests a plant.

This is press/move/release input, not the two-click anchor convention used by
Mine, Chop, and Till. The existing unit-selection controller proves the engine
already routes and classifies that gesture, including fast drags which can
complete between periodic Lua ticks.

### Selection and terrain ownership

The design should have one authoritative projection from the screen gesture to
eligible plant instances. A drag rectangle must not be implemented as only the
two endpoint terrain picks when a cliff can cause those endpoints to land on
different height surfaces. The repository points toward a flora-specific
screen-space query beside `unit.hitTestInRect`, sharing the actual
`FloraQuads` projection math so z, facing, wrap, sub-tile offsets, and texture
dimensions cannot drift from rendering. The result must define edge inclusion,
unloaded terrain, world wrapping, and click-versus-drag threshold behavior.
Terrain-occlusion filtering is preferred when it can reuse a small,
maintainable visibility check, but D-13 deliberately keeps it from forcing a
second renderer or depth-identification pipeline. The selector returns a
deduplicated, deterministic identity order even when wrap copies or internal
map iteration expose the same candidates in different orders.

### Designation lifecycle and execution

The intended design is a real unit-work designation, not a second harvesting
backend: completion should converge on `world.harvestFlora` so yields, crop
clearing, wild-plant regrowth, textures, and persistence retain one authority.
The designation needs explicit ownership, cancellation, race handling when a
plant becomes ineligible, and a decision on persistence. The existing routine
`auto_harvest` behavior must be removed, restricted, or repurposed so a
non-starving unit never harvests an undesignated plant. The separate
hunger-driven forage behavior remains: a starving unit may autonomously obtain
food as a survival exception. The exact action/module boundary is
implementation design; the player-control contract is not. Any otherwise-
capable player worker can claim ordinary designated work, with farming skill
and Farmer-role signals affecting performance and preference rather than hard
eligibility.

Persisted state follows the repository's save-compatibility contract. Existing
saves which predate harvest designations load with an empty designation layer;
new saves round-trip pending intent, and the implementation updates the save
version, DTO/component inventory, fixtures, and compatibility checks through
their established mechanisms rather than mutating fixture data by hand.

### Artwork

Two assets are known to be missing: a Harvest tool icon for the tool palette
and a designated-plant marker for the world. They are tracked work, not
implementation details, and require user signoff on the final textures. The
Dig and default-tool icons are the canonical references. This epic produces
only those two harvest assets. The broader canonicalization of mismatched
existing tool icons is a separate art-cleanup effort which the user wants help
capturing and filing after this design workflow. D-14 assigns the two in-scope
final assets to a repo-native pixel-art pass with user signoff.

The toolbar asset needs both normal and selected 32×32 variants. The world
marker should be designed as a plant annotation, not assumed to be another
solid-color 96×64 tile diamond merely because that is how older designation
layers render. D-14 assigns both to a repo-native pixel-art pass with preview
and final user signoff.

## Proposals

### P-1. Mirror unit selection's click/box oracle at the flora render boundary

For a click, return the topmost eligible rendered flora sprite whose hit area
contains the pointer. For a drag, include an eligible rendered plant when its
ground-contact anchor lies inside the screen-space rectangle. This mirrors the
existing unit-selection distinction—sprite hit area for a click, stable center
point for a box—without letting a tall canopy select from far outside the box.
Both paths should share flora render geometry and return canonical target
identity plus the visual anchor needed by the marker. D-8 adopts this core
model; D-13 settles the fully-occluded-anchor policy as preferred but
non-blocking terrain filtering.

### P-2. Keep Harvest separate from Chop

The Harvest tool should accept currently harvestable non-wood wild flora and
ripe planted crops. Wood-tagged trees remain under the existing Chop tool;
decorative, juvenile, off-season, dead, regrowing, or unloaded plants are
ineligible. This matches the current untagged harvest boundary and avoids two
player tools claiming the same trees. Adopted by D-9.

### P-3. Persist pending intent but do not create dormant seasonal jobs

Accept only a currently harvestable target, persist that pending designation
with its page, and clear it on completion, cancellation, plant removal, or a
race which makes the target ineligible before work executes. Do not leave an
out-of-season or regrowing plant designated for days awaiting a future window;
the player can designate it when it becomes ready again. Adopted by D-10.

### P-4. Extract the gesture lifecycle instead of copying unit drag selection

Share press/release coordinates, the four-pixel threshold, fast-drag
classification, focus-loss cancellation, framebuffer-scaled rectangle drawing,
and action-outcome recording. Keep the commit effect pluggable: unit mode calls
`unit.hitTestInRect`, while Harvest mode calls the new flora selector and
designation command. This avoids a second drag state machine without arming
unit selection under a claimed tool.

### P-5. Keep unrelated icon cleanup in a separate art inventory

The epic should own the two new Harvest assets and their user signoff. Existing
mismatched toolbar icons should be captured as a separate bounded art audit or
findings report, then replaced in independent issues so harvest delivery is not
held hostage by a palette-wide redraw. Adopted by D-11; the exact external
filing workflow is deliberately deferred until this design is settled.

## Decisions

### D-1. Harvesting reuses the existing harvest backend

Player-directed work will converge on the same `world.harvestFlora` behavior
used by autonomous harvesting rather than introducing another yield, crop, or
regrowth implementation. This preserves one authority for materialized yields
and plant lifecycle behavior.

### D-2. The tool supports both single-target and multi-target gestures

The Harvest tool will support a click for one visible plant and click-drag for
multiple plants. D-6 settles both as designation gestures rather than direct
harvesting.

### D-3. Cliff-correct selection is an explicit epic requirement

The selected set must follow the plants visibly targeted by the pointer or drag
rectangle even around large cliffs. A solution that only works on flat terrain
does not complete the arc.

### D-4. Harvest designation needs its own visible marker

Designated plants will have a new, unambiguous world-space indication rather
than relying only on a tool cursor or generic selection highlight.

### D-5. New harvest artwork follows the Dig/default-tool visual language

The canonical references for new harvest-tool artwork are the current Dig and
default-tool icons. Existing AI-generated icons are not accepted as the style
authority merely because they are already present.

### D-6. Every player harvest gesture creates designations

A click creates one harvest designation and a click-drag creates multiple
harvest designations. The player cannot instantly harvest a plant through the
tool; every ordinary harvest requires a unit to claim and perform the work.

### D-7. Autonomous plant harvesting is limited to starvation survival

A non-starving unit harvests a plant only when the player has designated it.
The existing routine `auto_harvest` behavior must no longer consume arbitrary
ripe plants without player intent. A starving unit may still autonomously seek
and harvest edible plants through the emergency forage behavior.

### D-8. Clicks use sprite hits and drags use rendered ground anchors

A click chooses the topmost eligible rendered plant sprite under the pointer.
A drag includes eligible plants whose rendered ground-contact anchors fall
inside the screen-space rectangle. Both queries share the flora renderer's
projection inputs so cliffs, sub-tile offsets, facing, wrapping, and texture
geometry do not drift from what the player sees. Whether a fully occluded
anchor participates follows D-13's preferred-but-non-blocking terrain rule.

### D-9. Harvest covers ready non-wood plants

The Harvest tool accepts currently harvestable non-wood wild flora and ripe
planted crops. Wood-tagged trees remain exclusively under Chop. Decorative,
juvenile, off-season, dead, regrowing, and unloaded plants are ineligible and
cannot acquire a harvest designation. The starvation-survival exception in
D-7 remains narrower: autonomous units seek only edible plants.

### D-10. Pending intent persists, but invalid intent does not become dormant

A valid pending harvest designation persists with its world page through
save/load. It clears on completion, player cancellation, plant removal, or any
race which makes the target ineligible before work executes. An ineligible,
out-of-season, or regrowing plant cannot be pre-designated to wait for a future
harvest window.

### D-11. Broader tool-icon cleanup is a separate follow-up

This epic owns only the new Harvest tool icon and designated-plant marker,
including explicit user signoff on both. Existing mismatched or AI-generated
tool icons will be inventoried and filed through a separate art-cleanup effort
with the user's participation, so that work neither disappears nor blocks the
harvest gameplay arc.

### D-12. Left gestures add and right gestures erase

While Harvest is active, a left click adds one designation and a left drag adds
every eligible selected plant. A right click removes one existing designation
and a right drag removes every designated plant selected by the same
cliff-correct oracle. Both directions are idempotent: adding an existing
designation or erasing an absent one is a harmless no-op.

### D-13. Terrain-occlusion filtering is preferred, not a completion gate

Correct projection at each plant's real surface height remains mandatory. The
selector should exclude a plant whose rendered ground anchor is hidden behind
a nearer cliff when that can reuse a simple, maintainable terrain-visibility
check. If doing so requires a costly new depth/identity pipeline or duplicated
render logic, otherwise-eligible hidden anchors may be selected without
blocking the epic. Plant-on-plant pixel occlusion is not evaluated.

### D-14. Final Harvest assets are repo-native pixel art with user signoff

The art slice will construct the Harvest toolbar icon and designated-plant
marker from the canonical Dig/default references, preview them at production
and gameplay scale, and obtain explicit user approval. Raw AI-generated output
will not be accepted as the final asset or visual authority.

### D-15. Any capable worker may harvest, while Farmers prefer it

Any otherwise-capable player worker may claim and complete an ordinary harvest
designation. Farming skill continues to affect work performance and derived
Farmer-role weighting makes skilled farmers prefer the job, but the role is not
a hard eligibility gate. Starvation survival may preempt ordinary designated
work.

### D-16. Harvest remains active until explicitly exited

The Harvest tool stays active across repeated left-add and right-erase
gestures. Escape or choosing another toolbar tool exits Harvest. A right-click
which finds no existing designation is an erase no-op rather than an exit, so
the right gesture retains one consistent meaning.

## Open questions

### Q-1. Does a single click designate one plant or harvest it immediately?

Resolved by D-6.

The phrase “tool to designate plants” points toward a one-plant designation,
while “clicking ... should simply harvest the thing under the click” can mean an
instant player action. Immediate harvesting would bypass unit travel/work and
make click behavior materially different from drag behavior. A one-plant
designation keeps both gestures in the same job lifecycle. Resolve this before
the command and AI ownership are fixed.

### Q-2. Which plant classes can the Harvest tool designate?

Resolved by D-9.

Known candidates are currently harvestable wild forage, ripe planted crops,
and wood-tagged trees. Trees already have a Chop designation and probably stay
outside Harvest, but the exact boundary between forage, crop harvest, and chop
must be deliberate. This affects eligibility feedback, icons, and AI reuse.

### Q-3. Is broader tool-icon canonicalization inside this epic?

Resolved by D-11.

The minimum arc needs two new signed-off assets: the Harvest tool icon and the
designated-plant marker. Options are: keep the epic to those assets and capture
other mismatches separately; include a bounded named list of existing icons;
or make the whole tool palette a visual-system slice. The last option can
dominate an otherwise coherent gameplay epic.

### Q-4. Should a fully occluded plant count in a drag?

Resolved by D-13.

D-8 settles rendered ground-contact-anchor inclusion for the box and a topmost
sprite hit for a click. The remaining choice is whether a plant whose anchor
projects inside the rectangle but is fully occluded by a nearer cliff or sprite
should count. Exact occlusion requires more than geometry; accepting all
otherwise-rendered anchors is simpler and matches unit box selection, while
frontmost-only selection more literally means “what I can see.”

### Q-5. Do harvest designations persist through save/load and regrowth?

Resolved by D-10.

The likely contract is that pending work persists with its world page but is
removed when completed, cancelled, or made permanently invalid. A plant that
is temporarily out of season or regrowing raises a separate choice: refuse the
designation up front, keep a dormant designation, or clear it. This affects
save compatibility and player feedback.

### Q-6. Does player designation replace routine auto-harvest or coexist with it?

Resolved by D-7.

The current `auto_harvest` action automatically finds any nearby ripe edible
flora. Options are: require a player designation for routine farm harvesting
while retaining emergency hunger-driven forage; keep undesignated auto-harvest
and give designations higher priority; or restrict one path to crops and the
other to wild forage. Once that control model is settled, the design can fix
which capable units claim the work, how farm role/skill weights it, and how an
explicit job competes without becoming uninterruptible.

### Q-7. What player gesture cancels pending harvest designations?

Resolved by D-12.

Existing rectangle tools expose cancellation commands but do not provide a
complete multi-designation erase gesture. Options include right-clicking one
marker, right-dragging an erase rectangle while Harvest is active, clicking an
already-designated plant to toggle it off, or a separate cancel mode/modifier.
Cancellation must be discoverable and use the same cliff-correct target oracle.

### Q-8. Who produces the final Harvest artwork?

Resolved by D-14.

Both missing assets need an explicit production owner: the user can supply the
final pixels, or the delivery slice can construct them from the canonical Dig
and default-tool references and present gameplay-scale previews for user
signoff. Because mismatched generative artwork prompted this cleanup, raw
AI-generated output should not become the final style authority; a repo-native
pixel-art pass with iterative user approval is the recommended delivery path.

### Q-9. Which units claim ordinary harvest designations?

Resolved by D-15.

The current farming behavior already has a farming skill and derived Farmer
role. The recommended contract is that any otherwise-capable player worker may
perform designated harvest work, farming skill continues to scale the work,
and Farmer-role weighting makes skilled farmers prefer it without making the
job inaccessible to everyone else. Starvation survival may still outrank this
ordinary work. Restricting harvest designations to Farmers would make a player
order wait even when another capable unit is available.

### Q-10. How does the player leave the persistent Harvest tool?

Resolved by D-16.

D-12 consumes right-click for designation erasure, so it cannot also be an
unambiguous tool-exit gesture. The recommended model keeps Harvest active
across repeated add and erase gestures; Escape or choosing another toolbar tool
exits it, while a right-click miss is simply an erase no-op.

## Verification strategy

- Pure or headless coverage for designation add/remove/idempotence, eligibility,
  page ownership, old-save defaulting, new-save round trips, races, and
  convergence on the existing harvest backend.
- A focused real-engine scenario proving designation-to-unit-work-to-yield and
  regrowth behavior without relying on a best-effort natural forage fixture.
- Offscreen screenshot/input coverage for the Harvest tool, single-click target,
  drag-box target set, cancellation, and designation marker.
- A cliff-focused oracle that compares the actual selected plant identities
  with the plants whose rendered anchors fall within the gesture, including
  substantial height discontinuities rather than only flat-world coordinates.
  If the optional terrain-occlusion filter is included, cover its cliff-hidden
  exclusion separately; its absence does not weaken the height-projection gate.
- Visual QA and explicit user signoff for every new or replaced texture.

## Delivery plan

### HARV-1. Define and expose harvest-designation state and commands

- **Outcome:** One authoritative per-world plant-harvest designation model and
  command/query surface exists without changing unit AI or HUD behavior.
- **Scope:** Identity, eligibility boundary, add/remove/idempotence, page
  ownership, lifecycle, persistence decision, and backend-facing types.
- **Phase:** 1 — state foundation
- **Depends on:** `none`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-4, D-6, D-7, D-9, D-10, D-12
- **Acceptance signals:** Deterministic state/query tests cover valid,
  duplicate, stale, cross-page, cancellation, old-save defaulting, new-save
  round trips, and chosen persistence behavior.
- **Out of scope:** Mouse gestures, artwork, rendering, and unit execution.
- **Open questions:** `None`

### HARV-2. Make plant selection correct for clicks and drag boxes across cliffs

- **Outcome:** One gesture-to-plant selection boundary produces the intended
  identities for single clicks and drag rectangles across steep terrain.
- **Scope:** Projection/picking ownership, click threshold, rectangle edge
  policy, eligibility query integration, cliffs, occlusion decision, and any
  justified shared world-tool repair.
- **Phase:** 2 — interaction geometry
- **Depends on:** HARV-1
- **Ordering:** `critical path`
- **Relevant decisions:** D-2, D-3, D-6, D-8, D-9, D-12, D-13
- **Acceptance signals:** Identity-based tests or probes distinguish correct
  plant selection on flat terrain and around large cliffs and prove stable,
  deduplicated results across repeated queries.
- **Out of scope:** Unit execution and final textures.
- **Open questions:** `None`

### HARV-3. Execute player harvest designations through unit work

- **Outcome:** Eligible units claim, travel to, perform, complete, and clean up
  explicit harvest designations through the existing harvest backend.
- **Scope:** AI/job ownership, work duration, arbitration, race handling,
  cancellation, yield collection, completion, and skill/role policy.
- **Phase:** 3 — work execution
- **Depends on:** HARV-1
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-6, D-7, D-9, D-10, D-15
- **Acceptance signals:** A deterministic scenario observes a designation,
  claimed work, harvest completion, materialized yields, and cleared intent;
  ineligibility and competing-unit races terminate cleanly.
- **Out of scope:** Final HUD integration and unrelated AI redesign.
- **Open questions:** `None`

### HARV-4. Add canonical harvest-tool and designated-plant artwork

- **Outcome:** Signed-off bitmap assets exist for the Harvest tool and its
  in-world designation marker in the Dig/default-tool visual language.
- **Scope:** Reference audit, asset source decision, generation or supplied art,
  sizing, transparency, gameplay-scale previews, and final user signoff.
- **Phase:** 2 — art production
- **Depends on:** `none`
- **Ordering:** `independent`
- **Relevant decisions:** D-4, D-5, D-11, D-14
- **Acceptance signals:** Both assets render cleanly at their production sizes
  and receive explicit user approval.
- **Out of scope:** Code integration and the separate icon cleanup excluded by
  D-11.
- **Open questions:** `None`

### HARV-5. Integrate and verify the complete player harvest workflow

- **Outcome:** The HUD tool, gesture handling, marker rendering, cancellation,
  work execution, and player feedback operate as one coherent workflow.
- **Scope:** Tool registration, cursor/tool state, click/drag dispatch,
  designation rendering, final assets, feedback, focused integration tests,
  manual/offscreen verification, and relevant documentation.
- **Phase:** 4 — integration
- **Depends on:** HARV-2, HARV-3, HARV-4
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2, D-3, D-4, D-5, D-6, D-7, D-8,
  D-9, D-10, D-11, D-12, D-13, D-14, D-15, D-16
- **Acceptance signals:** A player can select one or many intended plants,
  including around a large cliff, see the exact designation set, watch units
  complete it, receive yields, cancel pending work, and save/load according to
  the settled contract.
- **Out of scope:** Unrelated tool-icon cleanup and harvest balance changes.
- **Open questions:** `None`

## Source notes

- The user wants a new Harvest tool, a new icon for designated plants, correct
  drag-box selection around large cliffs, click and drag support, and artwork
  aligned to the current Dig and default tools.
- The user expects adjacent icon/art inconsistencies and other small concerns
  may need work outside the epic rather than being silently absorbed into it.
- The user confirmed that every player tool action is a designation rather than
  an instant harvest; non-starving units harvest only designated plants;
  starvation retains autonomous harvesting as a survival mechanic; and the
  topmost-sprite click / rendered-ground-anchor drag model is acceptable.
- The user confirmed that Harvest covers ripe crops and currently harvestable
  non-wood wild plants, that valid pending designations persist through save/load
  but clear on invalidation, and that unrelated icon canonicalization belongs
  in a separate follow-up which they want help filing.
- The user confirmed symmetric left-add/right-erase click and drag gestures,
  preferred terrain-occlusion filtering when it is inexpensive but acceptance
  of cliff-hidden selections otherwise, and repo-native final pixel art with
  explicit signoff rather than raw AI-generated assets.
- The user confirmed that any capable worker may perform designated harvest
  work while farming skill and Farmer role influence performance and
  preference, and that Harvest stays active until Escape or another tool exits
  it.
