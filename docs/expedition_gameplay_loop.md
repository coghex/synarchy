# Expedition gameplay loop design

This document is the durable design authority for completing Synarchy's first
expedition arc. Most of the playable loop has shipped; the remaining design
work adds a real confrontation, a capability-changing reward, and integrated
verification of those two verbs.

Design state: `ready for issue processing`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [ ] EPIC. Complete the expedition gameplay loop
- [x] EXP-1. Add hostile location occupants and the first combat encounter — [#916]
- [ ] EXP-4. Add information-revealed locations and staged map icons
- [x] EXP-2. Add a transformational expedition reward — [#917]
- [ ] EXP-5. Extend the first-session tutorial through confrontation and advancement
- [ ] EXP-3. Extend the integrated expedition gate to cover confrontation and advancement

## Epic contract

- **Goal:** Extend the shipped prepare → travel → discover → extract → return
  loop so a normal expedition includes an understandable confrontation and a
  guaranteed reward that visibly expands colony capability.
- **Done when:** One authored ruin encounter can arise through ordinary play
  and clear only when every assigned hostile is dead or incapacitated; its
  guaranteed location-intelligence reward can be returned to built colony cargo
  and explicitly analyzed to reveal nearby instances of a separate hidden-by-
  default location definition as anonymous map hints that become type-specific
  on approach; and the tutorial plus end-to-end gate prove the completed loop
  across a fresh save/load process without regressing survival and retrieval.
- **Users and operators:** Players running the first expedition; content
  authors adding locations, occupants, and rewards; maintainers operating the
  location, combat, tutorial, and persistence gates.
- **Arc label:** `expedition`

## Current state and evidence

- The location foundation and embark-to-discovery slice shipped under closed
  epic #159 and issues #777–#782. `Location.Instance` now owns stable placed
  identities, bounds, content-spawn state, and the persisted lifecycle added by
  closed issue #911.
- Per-unit experiential location knowledge shipped in closed issue #915 and is
  persisted through `lua.unit_ai`; the global discovery lifecycle remains a
  separate cartographic layer.
- Typed faction properties and relations shipped in closed issue #912, removing
  that original prerequisite for hostile occupants.
- `LocationLifecycle` already contains the append-only progression `unknown →
  hinted → discovered → active → cleared → depleted`, and proximity discovery
  already promotes both `unknown` and `hinted` to `discovered`. `hinted` has no
  producer today. The zoom-map renderer currently draws every mapped instance
  and selects the same definition-specific `undiscovered` texture for both
  `unknown` and `hinted`, then the `discovered` texture for every later state.
- Survival calibration, direct retrieval and return, ruin loot, the
  first-session tutorial foundation, and the integrated man-versus-nature gate
  shipped in closed issues #919–#923. `tools/expedition_loop_probe.py` is the
  current end-to-end authority.
- The current gate deliberately substitutes **survive the journey** for
  confrontation and **bank ordinary colony stock** for advancement. Open issue
  #916 owns the hostile-encounter slice and remains blocked on unit art; open
  issue #917 owns the transformational reward and remains blocked on choosing
  something meaningful to reveal or gate.
- No open tracker epic exactly owns the remaining pair plus their integration.
  Closed epic #918 covers survival calibration only and explicitly defers
  #916/#917; closed epic #159 covers the location foundation.

## Scope

### In scope

- One authored hostile occupant type and one reliable ruin encounter.
- Hostility detection, bounded pursuit/disengagement, explicit attack/retreat,
  outcome feedback, loot, and a persisted location outcome.
- One guaranteed reward, distinct from random ruin loot and the existing
  communications `radio`, that reveals eligible hidden locations within a
  finite radius of the cleared source location.
- At least one separate location definition authored as hidden by default, so
  the new information loop can be observed without changing existing mapped
  ruins or the starting-site-selection experience.
- A backwards-compatible location-visibility policy and staged zoom-map icon
  behavior: invisible while unknown, generic while hinted, type-specific while
  discovered/active, and completed when cleared/depleted.
- A separate tutorial extension built only after the encounter and reward own
  durable predicates.
- Extending the existing end-to-end expedition gate and player-facing guidance
  to prove the completed confront and invest verbs across save/load.

### Out of scope

- Procedural or multi-level dungeons, encounter respawning, formations,
  diplomacy, faction reputation, quests, and settlement simulation.
- A generalized caravan or world-scale logistics interface.
- Full fog of war or hiding every existing surface ruin; current always-mapped
  definitions retain their existing behavior by default.
- Large loot-tier systems, complex shelter requirements, or balancing every
  possible thirty-minute expedition.

## Desired experience

Synarchy's first complete gameplay slice should be one expedition, not a
general-purpose expansion of the location framework.

The central loop is:

> Discover → prepare → travel → confront risk → recover something → return →
> improve the colony → reach farther next time.

This joins the game's major strengths and intended influences:

- Dwarf Fortress-style settlement simulation gives expeditions meaning.
- RTS-style unit control makes expeditions direct and legible.
- Procedural locations give the world destinations and stories.
- Survival creates preparation decisions.
- Loot and trade feed progress back into the settlement.

The first slice should prove that this loop is enjoyable before locations,
survival, combat, trade, factions, or procedural generation are expanded in
isolation.

## Intended first 30 minutes

### Minutes 0–5: arrival and orientation

The player chooses a starting area, places the acolyte portal, and receives
five acolytes plus the technomule. The game should immediately communicate:

- these are the player's people;
- they need water, food, rest, and protection;
- the mule or a designated area is the initial supply point;
- there is something nearby worth investigating.

The first location does not have to be completely unknown. The player can
begin with a lead, such as a visible ruin or a weak radio signal, instead of
waiting for a full rumor or exploration system.

### Minutes 5–10: stabilize

The player establishes a minimal foothold:

- identify fresh water;
- establish storage or use the technomule as the supply point;
- secure a small food reserve;
- optionally construct one simple camp or shelter structure;
- learn selection, movement, designations, inventory, and time controls.

Early survival pressure should motivate preparation without punishing a player
who is still learning the interface.

### Minutes 10–15: prepare an expedition

The player selects two or three acolytes and chooses what they carry:

- canteens;
- rations;
- one or more weapons;
- first-aid supplies;
- enough spare capacity for salvage.

The important early choice is not an elaborate character build. It is who the
colony can spare and how much limited equipment the player is willing to risk.

### Minutes 15–25: travel and encounter

The party travels to a nearby ruin and faces one complication:

- a hostile animal or occupant;
- dangerous terrain that forces a detour;
- or a survivor whose presence creates a decision.

The first slice should prefer one reliable, understandable encounter. It is a
contextual test of movement, combat, injury, equipment, AI reactions, retreat,
and treatment—not yet a procedural dungeon.

### Minutes 25–30: return and advancement

The expedition recovers something consequential and brings it home. The first
ruin should contain a guaranteed progression reward, for example:

- a radio component that reveals more distant locations;
- a regulator required for the first powered workshop;
- a seed cache that enables reliable agriculture;
- a machine component that unlocks an improved recipe;
- or a map identifying a settlement.

Random loot can surround this reward, but should not replace it. The recovered
item must visibly increase what the colony can do.

**Status:** this is still the intent, and it is still unbuilt. #921 removed the
fixed `kind: item` entries from `data/locations/ruin_small.yaml`, so a ruin
currently guarantees no specific item at all — its contents are weighted
loot-table draws, seed-stable per instance but not authored. The guaranteed
reward returns with #917, which also supplies the "visibly increase what the
colony can do" half; until then the shipped slice ends at *recovered, banked
and usable*, which is what the step-9 gate asserts.

The first-session milestone might be:

> Settlement established — recovered location intelligence has identified new
> signals near the cleared ruin.

The sandbox continues after this milestone, but the player has completed a
recognizable arc.

## Existing foundation

The engine already supports much of the required substrate:

- deterministic location placement in suitable world chunks;
- lazy materialization as chunks load;
- persistent, idempotent geometry and content spawning;
- location content containing items, units, buildings, nested structures, and
  loot-table rolls;
- damaged structure variants;
- ground-item pickup, inventory, equipment, storage, survival, combat, injury,
  construction, crafting, and saving.

The missing work is primarily the connective tissue that turns a location from
scenery into a decision: discovery, expedition intent, risk, extraction,
return, and a reward that changes colony capability.

## Embark map and location knowledge

Locations visible during starting-site selection do not count as fully
discovered. Synarchy distinguishes four kinds of knowledge:

1. **Physical visibility** — whether the structure can be seen in the world.
2. **Cartographic knowledge** — whether the zoom map marks that something is
   present.
3. **Semantic knowledge** — whether the player knows what the location is.
4. **Experiential knowledge** — whether player-controlled units have visited
   it and learned its actual condition, occupants, and rewards.

Surface ruins are cartographically visible from the beginning. This makes
starting-site selection a strategic decision rather than a blind choice: the
player can settle near opportunities without being told exactly what they
contain. Fog of war, if added later, remains useful for tactical visibility
and genuinely hidden locations; it is not required to make surface locations
interesting.

The first slice does **not** recommend or choose a starting location for the
player. It preserves free starting-site selection and provides two kinds of
feedback instead:

- the acolyte portal cannot be placed where its footprint intersects a
  location's bounding box;
- placing it farther than the remote-start threshold from every mapped
  location presents a warning and asks for confirmation, but remains allowed.

Remote placement is a warning, not an invalid placement condition. The exact
distance threshold should be tuned through play rather than embedded as a
permanent design constant prematurely.

### Zoom-map icons

Each mapped location initially uses an **undiscovered** (visually hidden or
obscured) icon texture. A paired **discovered** texture replaces it once a
player-controlled unit approaches the location. The icon is a dynamic map
annotation above the zoom-map terrain, not a pixel baked into the terrain
atlas, so it can retain a readable screen size, receive hover/click input, and
change without rebuilding the map texture.

The first icon pair is for ruins. Additional types can provide their own pairs
later. A future **cleared** texture may indicate that enemies have been
eliminated and/or the location objective has been attained, but cleared-state
rules and art are outside the first map-discovery slice.

### Spatial bounds and discovery

Every placed location needs a first-class tile bounding box relative to its
anchor. The current ruin builder's implicit footprint is insufficient because
portal validation, discovery, map interaction, encounter state, and future
location types must agree on the same extent.

Portal rejection uses exact footprint-versus-location-bound intersection.
Discovery should occur slightly before a unit physically enters the structure:
entering the exact bounding box can put the unit through a breached wall or
into hostile range before the player receives feedback. For the first slice,
use a small configurable approach margin around the bounding box and trigger
when a player-controlled unit enters that expanded discovery area. The margin
belongs to the location definition so large towns and small ruins can later
use different approach distances.

Discovery changes the icon, emits clear player feedback, and persists across
save/load. It does not imply that the location is cleared or that all of its
contents are known.

### Portal placement feedback

The portal's construction ghost follows the general building-placement rule:

- soft white and translucent when placement is valid;
- red and translucent when placement is invalid, including uneven terrain,
  invalid surface, occupied footprint, or intersection with location bounds.

The current renderer already intends to provide white/red valid-state tinting;
the first implementation issue should verify that behavior end to end and fix
it if it is not visible rather than introducing a second ghost-color path.

## Implementation status

The embark-to-discovery slice of this loop — steps 1-3 below, scoped to a
single reliable ruin type with no combat encounter or reward yet — is
implemented, under the parent locations epic #159:

- location spatial bounds — #777
- portal placement exclusion + white/red ghost feedback — #778
- remote-start confirmation (`Establish Here` / `Choose Another Site`) — #779
- persistent unit-driven discovery + player event — #780
- paired discovered/undiscovered zoom-map icons — #781
- end-to-end embark-to-discovery integration probe — #782

The terminology above matches what shipped: "remote-start threshold" is the
`building.remoteCheck` distance gate, "discovery" is the expanded-bounds
approach margin firing a `location_discovery` player event, and the
undiscovered/discovered icon pair is what `World.Render.Zoom.Icons` renders
from each location's persisted lifecycle state.

**Step 2 (instance identity and lifecycle) — #911.** Each placed location is
now a first-class persisted record (`Location.Instance.LocationInstance`),
keyed per world page by a stable `LocationInstanceId` allocated at placement
time from the deterministic overlay. It carries its definition id, anchor,
resolved absolute bounds, discovery margin, display name, one-time
content-spawn flag, and its lifecycle
(`unknown → hinted → discovered → active → cleared → depleted`).
`world.listPlacedLocations()` reports all of it and
`world.getLocationInstance(id)` looks one up by id.

Two deliberate boundaries on what #911 landed:

- **The states past `discovered` are defined, persisted, and reachable
  programmatically (`world.setLocationLifecycle`), but nothing in the game
  drives an instance into them yet.** The encounter (step 4), reward
  (step 5), and retrieval (step 6) work is what they exist to serve.
- **`hinted` is currently unreachable, and that is correct.** Every location
  is cartographically visible from world generation and stays that way for
  now — a deliberate development-phase simplification: the player can always
  see something is there, and it stays unexplored until a player-controlled
  unit discovers it by proximity. `hinted` is for a future class of locations
  that are *not* visible by default and must be revealed by information
  rather than proximity. That class is planned but unbuilt; the state is
  documented here so it is not later mistaken for dead weight.

Display names are a placeholder derived from each definition's `label`.
Wiring them to the language/naming system (#708) is deliberately separate
work.

**Step 8 (the first-session objective) — tutorial epic #956.** The objective
panel shipped as a reusable, data-authored *tutorial* system rather than a
bespoke checklist. `data/tutorials/first_session.yaml` owns the stable ids,
labels, tooltips, objective kinds, display order and tree relationships
(#957); `scripts/tutorial_progress.lua` owns what the player has done with
that tree and persists it as the optional `lua.tutorial_progress` save
component (#958); `scripts/tutorial_eval.lua` binds each authored evaluator
key to a predicate over durable gameplay state (#959); and
`scripts/tutorial_hud.lua` draws the transparent, scrollable right-side list
behind its HUD toggle (#960). #922 gated the four together.

Two completion semantics, and the difference is the point: a **full**
objective latches permanently once its predicate is true — later destruction,
consumption or transfer never unticks it — while a **subobjective** is a live
component requirement that checks and unchecks as the game state changes and
is recomputed from scratch every session. Only the durable latches are saved.

The delivered branch is deliberately narrow: one global `first_session` tree
per save, covering preparation only.

```text
Place portal
  -> Secure water source
    -> Prepare an expedition
         - Prepare water
         - Prepare food
```

The panel measures the arc, it does not gate it. Nothing in it blocks
placement, discovery or travel, and evaluation ignores whether a row is
currently visible.

**Steps 6, 7 and 9 — retrieval, survival tuning, and the full slice gate.**

- **Step 6 (retrieval and return) — #920.** Direct RTS retrieval only: order
  pickup, see whether the item fits, identify its carrier, walk home, deposit.
  No caravan interface was added. Gate: `tools/expedition_retrieval_probe.py`.
- **Step 7 (survival tuning) — #925 and #919, epic #918.** Recorded in
  `docs/expedition_survival_calibration.md` as an observation log; the
  project-owner decision was to land the calibration with **zero** balance
  changes and file the mechanic-level findings as follow-ups instead. The
  `expedition` and `first-aid` scenarios in `tools/gameplay_scenarios.py` are
  diagnostic utilities, deliberately outside CI and the probe registry, and
  their exit status never carries a balance verdict.
- **Step 9 (the full slice gate) — #923.** `tools/expedition_loop_probe.py`
  runs the whole loop as one session, described under "9. Gate the full slice"
  below.

**Steps 4 and 5 remain future work, and are deliberate deferrals rather than
oversights.** #916 (one hostile occupant and the first combat encounter) is
blocked on unit art; #917 (a transformational reward) has nothing to reveal or
gate yet, because the location-intelligence system a recovered radio core would
feed does not exist. Together they would add the **confront** verb and turn
**invest** from "the loot is banked and usable" into "the loot changed what the
colony can do". Everything else in the first-30-minutes sequence is
implemented.

## Design

### Encounter ownership and completion

Each authored encounter owns a fixed, durable membership set established when
the location spawns its hostile occupants. A hostile leaving the location's
bounds remains a member; merely fleeing or being driven away cannot clear the
site. The encounter promotes to `active` when ordinary hostility begins and to
`cleared` only when every assigned hostile is dead or incapacitated. The
promotion is one-way and exactly once, so returning later cannot recreate or
re-clear the encounter.

Encounter membership and its terminal result must survive save/load. Adding
that durable state follows the component-version and frozen-DTO migration rules
in `docs/persistence_contract.md`; it must not overload the independent
contents-spawned or geometry-stamped flags.

### Information-revealed locations and map icons

Location definitions gain an explicit map-visibility policy. The compatibility
default preserves today's always-mapped behavior; only definitions authored as
information-revealed begin absent from the player-facing map.

For an information-revealed location, lifecycle and map presentation mean:

| Lifecycle | Player-facing map state |
|---|---|
| `unknown` | No icon; the player has no cartographic knowledge of the instance. |
| `hinted` | A shared generic **Unknown location** icon; definition, name, and contents remain concealed. |
| `discovered` / `active` | The correct definition-specific **uncompleted** icon. |
| `cleared` / `depleted` | The definition-specific completed icon when authored, otherwise the uncompleted icon as a compatibility fallback. |

The existing proximity-discovery path already accepts `hinted → discovered`;
that remains the one transition that replaces the anonymous hint with the
correct type-specific icon. Existing always-mapped definitions continue using
their current definition-specific icon before discovery, so old worlds and the
starting-site-selection experience do not silently lose every known ruin.

The icon schema may add an optional completed texture, but absence must remain
valid. A hidden location's generic hinted icon is shared rather than authored
per definition, because per-definition art at that state would leak the very
identity the hint is intended to withhold.

### Radius reveal transaction

Activating location intelligence promotes eligible `unknown` instances to
`hinted` on the same world page when their anchors fall within an authored,
finite radius of the cleared source location. Distance is shortest cylindrical-
world tile distance, so the U seam cannot hide a nearby target. Candidate
enumeration is ordered by stable location-instance ID; already hinted or later
states are idempotent no-ops.

The whole reveal is a world-thread-owned lifecycle transaction. It records only
the durable promotions and emits one player-facing summary, rather than a
separate popup for every location. Save/load preserves the hinted instances
through the existing location lifecycle wire field.

### Reward activation

The guaranteed reward is location intelligence distinct from the existing
unit-communications `radio`. It needs a durable association with its source
`(world page, location instance)` so the reveal radius remains centered on the
cleared site after the item is moved, saved, and loaded.

The intelligence does not activate when the encounter clears or merely when the
item enters a unit inventory. After the recovered item is deposited into a
built colony cargo store, its inventory entry offers an explicit **Analyze
intelligence** action. Successful analysis records a durable, one-shot result,
performs the radius reveal, and leaves the item stored as an analyzed artifact;
repeating the action cannot reveal or notify twice. This keeps the return-and-
invest verb visible without inventing a second project system or a synthetic
"home radius" around the starting portal.

### Tutorial ownership

The tutorial extension is a separate delivery slice, not part of the behavior
or integration-gate PRs. It extends the existing `first_session` tree with
objectives backed by the durable encounter, reward-source, return/deposit, and
reveal state. The final gate depends on that slice so it can verify the complete
player-facing arc without making the gate itself own UI or objective behavior.

## Decisions

### D-1. Prove one expedition before generalizing locations

The arc optimizes for one coherent, understandable expedition rather than a
general expansion of locations, survival, combat, trade, or procedural
generation in isolation. Every included system must strengthen prepare,
travel, discover, confront, extract, return, or invest.

### D-2. Keep four kinds of location knowledge distinct

Physical visibility, cartographic knowledge, semantic knowledge, and per-unit
experiential knowledge are separate. Surface ruins may be visible on the map
before the player or any unit knows what is inside them.

### D-3. Preserve free starting-site choice

Portal overlap with a location is invalid, but a remote start remains legal
after a warning. The game provides strategic information and feedback without
choosing a starting site for the player.

### D-4. Treat the shipped survival loop as a deliberate intermediate slice

Until EXP-1 and EXP-2 land, survival supplies are the expedition's risk and
ordinary recovered stock is its payoff. The existing integrated gate is valid
for that shipped slice; it is not evidence that confrontation or advancement
already exists.

### D-5. Build one reliable encounter, not a general aggression system

The first confrontation uses one authored hostile unit type, bounded pursuit
and disengagement, the existing player attack/retreat surfaces, and a durable
location outcome. Diplomacy, formations, reputation, and procedural encounter
generation remain outside this arc.

### D-6. Make the progression reward guaranteed and capability-changing

Random loot may surround the reward but cannot replace it. The reward is a
distinct item—not the existing `radio` used for unit communication—and must
visibly enable or reveal something the colony could not do before.

### D-7. Keep retrieval in the direct RTS interaction model

Pickup, carrier visibility, ordered return, and deposit use existing unit
commands. A caravan or generalized logistics surface is added only if direct
retrieval is later proven inadequate.

### D-8. Author tutorial objectives only over durable predicates

The tutorial system may gain investigate, recover, return, or advancement rows
only after the runtime can answer them from durable state. UI rows must not be
used to invent progression state that gameplay and persistence do not own.

### D-9. Clear an encounter only when every assigned hostile is dead or incapacitated

Being driven away, leaving the location bounds, or temporarily disengaging does
not complete the encounter. The fixed membership set makes completion
observable and prevents a retreat from silently converting into a cleared site.

### D-10. Use location intelligence to reveal nearby hidden locations in stages

The reward promotes eligible hidden locations within a seam-aware radius of the
cleared source from `unknown` to `hinted`. Hinted locations use one generic
unknown icon; proximity changes them to the correct definition-specific
uncompleted icon. Cleared locations may use an authored completed icon. Existing
always-mapped definitions retain their current visibility by default.

### D-11. Deliver tutorial expansion as its own slice

The first-session tutorial extension follows the durable encounter and reward
behavior and lands before the final integrated gate. Keeping it separate avoids
mixing UI/objective work into either gameplay ownership or test infrastructure.

### D-12. Activate recovered intelligence through an explicit cargo action

Depositing the source-associated item into built colony cargo exposes one
**Analyze intelligence** action. Analysis, not encounter clear or deposit alone,
performs the one-shot radius reveal. The analyzed item remains in storage as an
artifact, while a durable activation record makes retries and save/load
idempotent.

### D-13. Author a separate hidden-by-default location definition

The first reveal population comes from at least one distinct location
definition whose map-visibility policy begins hidden. Existing mapped ruin
definitions retain their current behavior, including their role in choosing a
starting site. The new definition may reuse the common placement and content
substrate, but it has its own stable definition identity and the icon states
required by D-10.

## Open questions

### Q-1. Which hostile unit art and definition unblock the first encounter?

Issue #916 requires a purpose-built hostile humanoid content package with the
states and directional animations needed by ordinary combat. Reusing a passive
animal would technically exercise combat but violates D-5's intended encounter.
This question blocks EXP-1 until the owner-provided art and unit definition are
available.

### Q-2. What exact outcome completes the first encounter?

Resolved by D-9: every hostile assigned to the encounter must be dead or
incapacitated. Driving the occupants away does not clear the location.

### Q-3. Which colony capability does the guaranteed reward change?

Resolved by D-10: recovered location intelligence reveals eligible locations
within a radius of the cleared source. They first appear anonymously and become
type-specific only when a player-owned unit approaches.

### Q-4. Should the first-session tutorial expand when the two deferred verbs land?

Resolved by D-11: add a separate tutorial delivery slice after the durable
encounter/reward behavior and before the final integrated gate.

### Q-5. When does recovered location intelligence activate?

Resolved by D-12: after the item is deposited into built colony cargo, the
player explicitly selects **Analyze intelligence**. Deposit alone does not
activate it, and the analyzed artifact remains in storage.

### Q-6. What supplies the first information-revealed location population?

Resolved by D-13: a separate authored location definition begins hidden by
default. Existing mapped ruins and their starting-site behavior remain
unchanged.

## Verification strategy

- Preserve the focused location-instance, discovery, map-icon, faction, Lua
  persistence, and save-compatibility hspec coverage that owns the existing
  substrate.
- Extend the relevant combat and location-content behavior probes so the
  encounter begins through ordinary hostility, refuses to clear while any
  assigned hostile remains capable, resolves once after all are dead or
  incapacitated, and remains resolved after save/load.
- Add pure lifecycle/icon coverage for always-mapped versus information-
  revealed definitions, including invisible `unknown`, anonymous `hinted`,
  type-specific uncompleted, optional completed-icon fallback, and proximity
  promotion without identity leakage.
- Verify radius selection at the boundary and across the cylindrical seam,
  deterministic instance-ID ordering, one-way/idempotent promotion, and a
  save/load round trip with hinted targets.
- Verify that encounter clear, unit pickup, and cargo deposit do not activate
  the intelligence; the explicit cargo action does so once, retains the analyzed
  artifact, and remains idempotent across a save/load round trip.
- Extend the tutorial's focused evaluator, persistence, and HUD coverage for
  the new durable objective predicates before adding them to the integrated
  scenario.
- Extend `tools/expedition_loop_probe.py` rather than creating a second whole-
  arc scenario. It must place the encounter between travel and extraction and
  replace the current deposit-only payoff with an observable radius reveal.
- Continue reporting stages independently so a failure distinguishes encounter,
  reward, return, save, and fresh-process load behavior.
- Use offscreen/manual verification only for player-facing feedback that cannot
  be established headlessly. Keep exact issue-level commands for later
  `process-design-doc` runs.

## Delivery plan

### EXP-1. Add hostile location occupants and the first combat encounter

- **Outcome:** A normal expedition to the authored ruin can trigger, resolve,
  and persist one understandable hostile encounter.
- **Scope:** The hostile unit content package; location spawning; hostility
  detection; bounded pursuit and disengagement; attack/retreat behavior;
  durable encounter membership; outcome feedback; loot; and promotion of the
  owning location's lifecycle.
- **Phase:** Confront
- **Depends on:** `none` (typed faction relations and location instances are
  already shipped); Q-1 is an external content precondition.
- **Ordering:** `independent`
- **Relevant decisions:** D-1, D-2, D-5, D-9
- **Acceptance signals:** The occupant spawns exactly once; ordinary hostility
  starts the encounter; retreat and re-entry are coherent; the location does
  not clear while any assigned hostile remains capable; all assigned hostiles
  dead or incapacitated advances it exactly once; and membership plus outcome
  survive save/load.
- **Out of scope:** Diplomacy, formations, reputation, respawning encounters,
  and procedural dungeons.
- **Open questions:** Q-1

### EXP-4. Add information-revealed locations and staged map icons

- **Outcome:** Authored hidden locations move from absent, to anonymous map
  hints, to type-specific uncompleted icons, while existing mapped definitions
  retain their current behavior.
- **Scope:** A backwards-compatible visibility policy; a shared unknown-location
  icon; optional completed icons; lifecycle-aware rendering; player-facing
  identity concealment at `hinted`; and focused map/discovery/save coverage.
- **Phase:** Location-intelligence substrate
- **Depends on:** `none`
- **Ordering:** `can land first`
- **Relevant decisions:** D-2, D-10, D-13
- **Acceptance signals:** An information-revealed `unknown` instance draws
  nothing; `hinted` draws only the generic icon; proximity selects the correct
  uncompleted icon; cleared/depleted selects the completed icon or documented
  fallback; at least one separate hidden-by-default definition supplies an
  eligible instance; an always-mapped legacy definition renders as before; and
  every state survives save/load without changing stored enum order.
- **Out of scope:** The reward that creates hints, full fog of war, hiding all
  existing surface ruins, or changing physical location visibility.
- **Open questions:** None

### EXP-2. Add a transformational expedition reward

- **Outcome:** The cleared ruin guarantees recoverable location intelligence
  whose investment reveals eligible hidden locations around that source.
- **Scope:** The guaranteed item; its durable association with the source page
  and location instance; one-shot activation; seam-aware radius selection;
  deterministic `unknown → hinted` promotion; player feedback; coexistence with
  random loot; and save/load behavior.
- **Phase:** Invest
- **Depends on:** `EXP-1`, `EXP-4`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-6, D-7, D-8, D-9, D-10, D-12, D-13
- **Acceptance signals:** The item is guaranteed but not duplicated; remains
  distinct from random loot and the communications radio; retains its cleared-
  location source through movement and save/load; does not activate on clear,
  pickup, or deposit; exposes an explicit action only from built cargo; remains
  as an analyzed artifact afterward; reveals exactly the eligible in-radius
  instances once, including across the U seam; and leaves out-of-radius and
  already-known instances unchanged.
- **Out of scope:** A general technology tree, large loot tiers, quests, and
  replacing random salvage with fixed rewards.
- **Open questions:** None

### EXP-5. Extend the first-session tutorial through confrontation and advancement

- **Outcome:** The existing tutorial tree presents and durably tracks the
  confrontation, recovery, return, and location-intelligence payoff.
- **Scope:** New data-authored rows and evaluator keys over encounter clear,
  reward recovery, return/investment, and successful radius reveal; persistence
  of full-objective latches; live recomputation of subobjectives; HUD behavior;
  and focused tests.
- **Phase:** Player guidance
- **Depends on:** `EXP-1`, `EXP-2`
- **Ordering:** `critical path`
- **Relevant decisions:** D-8, D-9, D-10, D-11, D-12
- **Acceptance signals:** Every row is answerable from durable gameplay state;
  full objectives latch, subobjectives recompute, save/load preserves only the
  intended latches, and no objective itself mutates encounter or reward state.
- **Out of scope:** A quest framework, objective rewards unrelated to location
  intelligence, or a second onboarding system.
- **Open questions:** None

### EXP-3. Extend the integrated expedition gate to cover confrontation and advancement

- **Outcome:** The existing end-to-end scenario proves the completed prepare →
  travel → discover → confront → extract → return → invest loop across two
  engine processes.
- **Scope:** Extend `tools/expedition_loop_probe.py`, its stage diagnostics,
  probe registration/documentation, fresh-process identity checks, and the new
  tutorial objectives.
- **Phase:** Integration gate
- **Depends on:** `EXP-1`, `EXP-4`, `EXP-2`, `EXP-5`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-4, D-6, D-7, D-8, D-9, D-10, D-11,
  D-12, D-13
- **Acceptance signals:** The scenario exercises the real encounter; proves it
  remains uncleared until every hostile is dead or incapacitated; returns and
  deposits and explicitly analyzes the real intelligence reward; observes
  anonymous in-radius hints from the separate hidden definition; verifies the
  analyzed artifact plus encounter, reward, hint, and tutorial state in a fresh
  process; and retains the existing prepared-versus-unprepared survival
  comparison.
- **Out of scope:** A second location type, repeated-run balance thresholds,
  and combat balancing beyond the authored encounter.
- **Open questions:** None

## Historical delivery rationale

The following numbered sequence is retained as source context for the shipped
arc. Steps 1–3 and 6–9 have landed; current issue processing follows the
five-slice delivery plan above.

### 1. Make one ruin reliable

Fix the current location bugs and make the existing location probes green.
The ruin must:

- appear in a sensible place;
- stamp correctly;
- be reachable and enterable;
- spawn contents exactly once;
- tolerate player edits without respawning;
- preserve its state across save/load.

Do not add another location until this contract is dependable.

### 2. Add location identity and gameplay state

Give each placed location a stable instance identity and lifecycle, such as:

```text
unknown → hinted → discovered → active → cleared → depleted
```

An instance should retain:

- stable ID and definition ID;
- position;
- discovery state;
- encounter state;
- loot or progression state;
- display name and optional description.

This becomes the basis for maps, missions, trading, ownership, respawning, and
history later.

### 3. Add minimal discovery and navigation

The first slice does not require full fog of war. Surface ruins appear as
undiscovered icons during starting-site selection. A player-controlled unit
entering the ruin's expanded discovery bounds changes the icon to its
discovered texture and emits feedback. Location state and icon state survive
save/load.

### 4. Finish one combat encounter

Implement only what the first expedition needs:

- one hostile unit type;
- reliable hostility detection;
- attack, limited pursuit, and disengagement;
- explicit player attack and retreat/move commands;
- death or incapacitation and loot;
- clear feedback about who is attacking and why.

Diplomacy and sophisticated formations are later systems.

### 5. Create one transformational reward

Add a guaranteed item, such as a salvaged radio core, with one initial purpose:
complete a colony project that reveals or enables the next destinations.

```text
Colony preparation
        ↓
Expedition capability
        ↓
Recovered radio core
        ↓
Location intelligence
        ↓
More ambitious expedition
```

### 6. Make retrieval and return clear

Prove that the player can order pickup, see whether an item fits, identify its
carrier, return home, and deposit or consume it in the progression project.
Do not add a generalized caravan interface unless direct RTS retrieval proves
inadequate.

### 7. Tune survival around the journey

The first expedition should ask only:

- is there enough water?;
- is there enough food?;
- can the party survive one injury?

Advanced shelter, morale, disease, spoilage, and seasonal supply chains should
not block the first slice.

### 8. Present the first-session objective

**Delivered** as the data-authored tutorial foundation described under
"Implementation status" above (epic #956, gated by #922). The shipped
`first_session` tree teaches and measures the preparation half of the
sequence:

```text
Place portal
  -> Secure water source
    -> Prepare an expedition
         - Prepare water
         - Prepare food
```

The investigate / recover / return rows this step originally sketched are
deliberately absent from that first branch, because each would need durable
state the model cannot yet answer from. A recovered item carries no record of
where it came from — `ItemInstance` has a stable identity but no source
location, and `GroundItem` stores only the item and its coordinates — so
"recover ITS key resource" has nothing to bind to. "Return it to the colony"
has no canonical runtime destination either; `tools/expedition_retrieval_probe.py`
chooses its own home coordinates and cargo building. An objective whose
predicate cannot be answered from durable state is a row that never ticks, so
those rows wait on the encounter (step 4), reward (step 5) and retrieval
(step 6) work that would define them.

They are future *branches of the same tree*, not a second onboarding system:
adding one is authoring YAML plus binding an evaluator key, not new
architecture. Shelter and beds, completed/future-objective filters, a
graphical progression tree, and objective rewards are deferred on the same
terms.

This is onboarding scaffolding, not a quest or mission framework.

### 9. Gate the full slice

**Delivered** as `tools/expedition_loop_probe.py` (#923) — one fixed-seed
scenario, run unattended across two engine boots, proving the loop this arc
actually ships:

```text
spawn colony from a real portal roster
→ secure water, provision the party off the technomule
→ travel and discover the location by proximity
→ SURVIVE the journey (the encounter is deferred — #916)
→ extract the ruin's own loot-table output (no guaranteed reward — #917)
→ return and deposit into colony storage
→ save / reload in a fresh process
→ location, per-unit knowledge, objective and inventory state remain correct
```

Two substitutions from the sketch above, both forced by the deferrals and
neither hidden: "defeat or survive its encounter" is **survive the journey**,
and "collect progression item → complete colony project" is **extract real loot
→ bank it as usable colony stock**. What replaces the encounter as the arc's
risk is survival, so the scenario runs a second, **unprepared control party**
over the same route under the same orders from the same starting deficits,
differing only in what it carried out of the colony. The control must end
measurably worse off in named physiological metrics — otherwise the gate would
be proving that walking works, not that preparation matters.

The probe reports eight independent stages (`setup`, `prepare`, `travel`,
`extract`, `return`, `save`, `load`, `control`) so a failure names which part
of the loop broke, and prints a fingerprint of its selected ruin, loot and
sites so two consecutive runs can be compared for identity. It is manual-only
by classification in `tools/ci_probes.py`: a real worldSize-64 generation plus
two travellers walking ~30 tiles each way is too slow for a blocking per-PR
gate, and it leans on AI arbitration timing.

When #916 and #917 land, this scenario is where their verbs join the loop:
the encounter belongs between travel and extract, and the progression project
turns the existing deposit assertion into a capability change.

## Deferred systems

The first slice should explicitly defer:

- multi-level procedural dungeons;
- town simulation;
- regional economies and dynamic trading prices;
- diplomacy and faction reputation;
- procedural missions;
- full fog of war;
- squad formations;
- world-scale caravans;
- location ownership and conquest;
- replenishing encounters;
- large loot tier systems;
- complex shelter requirements;
- interior decoration systems.

After the ruin slice, expand in this order:

1. abandoned ruin — exploration and salvage;
2. occupied outpost — combat and retreat;
3. multi-room site — endurance and extraction;
4. friendly camp — barter and relationships;
5. village — repeated trade and regional specialization;
6. deep dungeon — long preparation, attrition, and major rewards.

## Scope rule

Every proposed system in this arc must strengthen at least one of these verbs:

> Prepare, travel, discover, confront, extract, return, invest.

If a feature does not improve one of those verbs, or the colony decisions that
support them, it does not belong in the first 30-minute slice.
