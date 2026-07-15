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

A temporary objective panel can teach and measure the intended sequence:

```text
Establish a foothold
✓ Deploy the acolyte portal
✓ Locate fresh water
✓ Prepare an expedition
□ Investigate the nearby location
□ Recover its key resource
□ Return the resource to the colony
```

This is onboarding scaffolding, not necessarily the final mission system.

### 9. Gate the full slice

Add an end-to-end scenario proving:

```text
spawn colony
→ equip party
→ reach and discover location
→ defeat or survive its encounter
→ collect progression item
→ return
→ complete colony project
→ save/reload
→ location and milestone states remain correct
```

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
