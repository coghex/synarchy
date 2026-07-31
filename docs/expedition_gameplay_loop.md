# Expedition Gameplay Loop

## Purpose

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

> Settlement established — the recovered radio is operational. Two distant
> signals have been located.

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

## Implementation order

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
