# Expedition gameplay loop design

This document is the durable design authority for completing Synarchy's first
expedition arc. Most of the playable loop has shipped; the remaining design
work adds a real confrontation, a location worth clearing, and integrated
verification of those two verbs.

Design state: `ready for issue processing`

> **2026-08-11 — material design change, re-signed-off the same day.** The
> location-visibility and reward design below was rewritten against new
> decisions D-14 to D-22. Reveal is now by unit line of sight, every location is
> marked from world generation by a shared question-mark icon, a cleared
> location's icon is tinted, clearing is gated on guaranteed significant loot,
> and the "recovered radio core reveals distant locations" reward is
> **retired**. D-6 is partially superseded; D-10, D-12 and D-13 are fully
> superseded, retained with their rationale. EXP-4 and EXP-2 changed shape.
> Q-1 through Q-12 are resolved.
>
> **Q-12 is RESOLVED (2026-08-15, in #1230).** The project owner made the
> shared question-mark texture and it ships at
> `assets/textures/icons/location/location_unknown.png`; they also authorized
> retiring `ruin_hidden.png` and repurposing `ruin_discovered.png` as
> `ruin.png`. Nothing here remains an art blocker. The rule that produced that
> outcome still stands for the next texture: do not placeholder, do not reuse
> another sprite, and do not assume a generation method — stop and ask.
>
> **Processing state (2026-08-11).** D-21's corrections are APPLIED: #917 and
> epic #1229 were both rewritten to this design. EXP-4 is filed as #1230. EXP-5
> is `[deferred]` until #916 and #917 close by merged PRs, which leaves EXP-3
> dependency-blocked behind it — so the arc currently has no selectable entry.
>
> **Processing state (2026-09-02).** EXP-1 (#916) and EXP-2 (#917) both closed
> by merged PRs, discharging EXP-5's deferral. EXP-5 is filed as #2301. EXP-3 is
> the one remaining unprocessed entry, dependency-blocked behind #2301.

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [x] EPIC. Complete the expedition gameplay loop — [#1229]
- [x] EXP-1. Add hostile location occupants and the first combat encounter — [#916]
- [x] EXP-4. Mark every location unknown and reveal its type by unit sight — [#1230]
- [x] EXP-2. Gate location clearing on guaranteed significant loot — [#917]
- [x] EXP-5. Extend the first-session tutorial through confrontation and advancement — [#2301]
- [ ] EXP-3. Extend the integrated expedition gate to cover confrontation and advancement

## Epic contract

- **Goal:** Extend the shipped prepare → travel → discover → extract → return
  loop so a normal expedition includes an understandable confrontation and a
  location the player can genuinely finish — its type learned by looking at it,
  its hostiles put down, and its guaranteed significant contents carried home.
- **Done when:** One authored ruin encounter can arise through ordinary play;
  every location is marked on the zoom map by a shared question-mark symbol from
  world generation, resolves to its type icon only once a player-owned unit
  actually sees a tile the location occupies, and tints to a cleared state once
  every assigned `ruin_small` hostile is exactly dead **and** every guaranteed
  significant item has been taken from it; and the tutorial plus end-to-end gate
  prove the completed loop across a fresh save/load process without regressing
  survival and retrieval.
- **Users and operators:** Players running the first expedition; content
  authors adding locations, occupants, and rewards; maintainers operating the
  location, combat, tutorial, and persistence gates.
- **Arc label:** `expedition`

## Current state and evidence

- The location foundation and embark-to-discovery slice shipped under closed
  epic #159 and issues #777–#782. `Location.Instance` now owns stable placed
  identities, bounds, content-spawn state, and the persisted lifecycle added by
  closed issue #911.
- **D-14, D-15, D-16 and D-22 landed in #1230.** The bullets below describe the
  pre-#1230 code and are kept as the design's starting point, not as a
  description of the tree: reveal is now sight-based, the icon model is
  shared-unknown / type / dark-type, `unitVisibleTiles` applies the night
  factor to its radius, and `discovery_margin` no longer exists anywhere.
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
- **Discovery is proximity, not sight.** `Location.Discovery.haloContactsWhere`
  tests `boundsContainsPoint worldSize (expandBounds (liDiscoveryMargin inst)
  (liBounds inst))` against each player-owned unit's tile — a location-owned
  halo that ignores facing, terrain and time of day. Both the player-wide
  `findDiscoveries` and #915's per-unit `findAwareness` are built on that ONE
  enumeration, deliberately, so the two layers cannot drift.
- **Per-unit sight already exists and is unused by locations.**
  `Unit.LineOfSight.unitVisibleTiles` returns the tiles a unit can actually
  see: a `perception × awareRangeTiles` (6.0) radius, intersected with a 120°
  cone on the unit's facing, intersected with terrain-Z line of sight, with a
  `nightPerceptionFactor`. Its own docstring names driving fog of war as its
  intended future use.
- **RGB tinting of world quads is established practice, not a new exception.**
  `World/Render/TileQuads.hs` tints underwater terrain (`Vec4 0.7 0.8 1.0`),
  lake/river surfaces, and lava. Zoom-map icons are the outlier: `emitIconQuad`
  pins `color = Vec4 1.0 1.0 1.0 alpha`, using only the alpha channel for the
  zoom fade.
- **Only one location definition exists.** `data/locations/` contains exactly
  `ruin_small.yaml`, and `assets/textures/icons/location/` exactly
  `ruin_hidden.png` and `ruin_discovered.png`. `Location.Overlay` already places
  every definition with `ldMaxCount > 0` at its own `ldMinSpacing`, so more
  definitions are content work, not engine work.
- Survival calibration, direct retrieval and return, ruin loot, the
  first-session tutorial foundation, and the integrated man-versus-nature gate
  shipped in closed issues #919–#923. `tools/expedition_loop_probe.py` is the
  current end-to-end authority.
- The current integrated gate deliberately selects #916's zero-occupant outcome
  so its survival comparison is not confounded by combat, while the shipped
  runtime now owns the occupied branch's durable nomad encounters. It still
  substitutes **bank ordinary colony stock** for advancement.
- **#917 carries EXP-2, rewritten 2026-08-11.** It was originally filed around a
  recovered radio core that reveals distant locations; D-17 retires that idea
  outright. Per D-21 the issue kept its number and was rewritten in place to the
  guaranteed-significant-loot design, opening with a dated banner recording the
  rejected premise. Title is now *Gate location clearing on guaranteed
  significant loot*. It remains `blocked`, now on #916 and #1230 rather than on
  "nothing to reveal".
- **Epic #1229 tracks this arc**, corrected the same day and the same way: its
  goal, `Done when`, background, dependency structure and checklist titles were
  all rewritten off the retired design.
- **#1230 carries EXP-4** (*Mark every location unknown on the zoom map and
  reveal its type by unit sight*), filed 2026-08-11 with the question-mark
  texture recorded as an explicit stop-and-ask art blocker. That blocker was
  reached and cleared as designed: the solver built everything the texture did
  not gate and stopped, and the owner supplied `location_unknown.png`
  (2026-08-15).
- No other open tracker epic owns this arc. Closed epic #918 covers survival
  calibration only and explicitly defers #916/#917; closed epic #159 covers the
  location foundation.

## Scope

### In scope

- One authored hostile occupant type and one reliable ruin encounter.
- Hostility detection, bounded pursuit/disengagement, explicit attack/retreat,
  outcome feedback, loot, and a persisted location outcome.
- A two-class split of location contents: incidental items (materials, ordinary
  loot) that have no bearing on clearing, and **guaranteed significant items**
  — story-progression items and valuable unique equipment — that must be taken
  from the location before it can be cleared.
- Uniform zoom-map icon behavior for every location, surface and underground
  alike: a shared question-mark symbol from world generation, the definition's
  type icon once a player-owned unit sees a tile the location occupies, and that
  same type icon tinted dark once the location is cleared.
- Reveal driven by real per-unit line of sight rather than a proximity halo.
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

> Settlement established — the ruin has been swept and what it was holding is
> ours.

*(Superseded 2026-08-11: the original wording read "recovered location
intelligence has identified new signals near the cleared ruin", from the
retired reward — see D-17.)*

The sandbox continues after this milestone, but the player has completed a
recognizable arc.

## Existing foundation

The engine already supports much of the required substrate:

- deterministic location placement in suitable world chunks;
- lazy materialization as chunks load;
- persistent, idempotent geometry and content spawning;
- location content containing items, units, buildings, and loot-table rolls
  (the closed four-kind vocabulary of #1708 — the nested-structure kind was
  removed there, having translated the outer definition's bounds around a
  shifted anchor);
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

> **Superseded 2026-08-11 by D-14, D-15 and D-16.** The paragraphs above
> describe what #781 shipped and remain accurate as history. The design going
> forward is three states, not a pair: one shared unknown symbol for every
> definition until its type is seen, then the definition's type icon, then that
> same icon tinted dark when cleared. The trigger is line of sight, not
> approach, and the cleared state is a tint rather than a third authored
> texture. See "Map icons: unknown, typed, cleared" under Design.

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

> **Superseded 2026-08-15 by D-15 and D-22, landed in #1230.** The approach
> margin above describes what #780 shipped and remains accurate as history.
> The `discovery_margin` field is gone — from the location YAML, the runtime
> definition, the live instance, both Lua tables, and the `world-pages` wire
> (v7). `bounds` is the only location footprint, and the trigger is the
> intersection of a player-owned unit's night-aware visible-tile set with
> those bounds. The "slightly before a unit physically enters" intent it was
> introduced for is now served better by sight itself, which reaches further
> than any halo in the open and not at all through a hill.

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
resolved absolute bounds, display name, one-time
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
  see that something is there, though not WHAT (D-14's shared unknown
  marker), and it stays unexplored until a player-owned unit SEES it
  (D-15, landed in #1230).

  > **Superseded 2026-08-15 by #1230.** This bullet used to end by
  > reserving `hinted` for a future class of locations revealed by
  > information rather than proximity. That class is retired. `hinted`
  > survives only because `LocationLifecycle` is a positionally
  > serialized append-only enum, so removing a constructor would corrupt
  > saves; behaviourally it is an ordinary unknown state, drawing the
  > shared marker and promoting to `discovered` on sight exactly as
  > `unknown` does. The state is still documented here so it is not
  > later mistaken for dead weight.

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
  `docs/history/expedition_survival_calibration_2026-07.md` as an observation
  log, since processed into `docs/expedition_survival_calibration.md` as the
  SURV-N findings ledger; the project-owner decision was to land the
  calibration with **zero** balance changes and file the mechanic-level
  findings as follow-ups instead. The
  `expedition` and `first-aid` scenarios in `tools/gameplay_scenarios.py` are
  diagnostic utilities, deliberately outside CI and the probe registry, and
  their exit status never carries a balance verdict.
- **Step 9 (the full slice gate) — #923.** `tools/expedition_loop_probe.py`
  runs the whole loop as one session, described under "9. Gate the full slice"
  below.

**Step 4 is implemented by #916.** `ruin_small` now owns a uniform persistent
0–3 nomad encounter, death-only clearance, and the first autonomous hostile
combat loop. #917 keeps step 5's slot but its
premise changed on 2026-08-11: it is no longer "a reward that changes what the
colony can do" but "guaranteed significant contents that must be taken before a
location counts as cleared" (D-17, D-18). Its issue body was rewritten to match
on the same day (D-21). Together they add the **confront** verb and give
**invest** an objective rather than a stockpile entry. Everything
else in the first-30-minutes sequence is implemented.

## Design

### Encounter ownership and completion

Each authored encounter owns a fixed, durable membership set established when
the location spawns its hostile occupants. A hostile leaving the location's
bounds remains a member; merely fleeing or being driven away cannot clear the
site. The encounter promotes to `active` when ordinary hostility begins and to
`cleared` only when its authored clearance policy is satisfied. `ruin_small`
uses `death_only`, so collapsed, crawling, missing, or disengaged nomads still
block it. The promotion is one-way and exactly once, so returning later cannot
recreate or re-clear the encounter.

Encounter membership and its terminal result must survive save/load. Adding
that durable state follows the component-version and frozen-DTO migration rules
in `docs/persistence_contract.md`; it must not overload the independent
contents-spawned or geometry-stamped flags.

### Map icons: unknown, typed, cleared

Every placed location is marked on the zoom map from world generation onward.
There is no per-definition visibility policy and no class of locations that
begins absent from the map. What changes as the player learns about a location
is *which* icon it draws, in three states:

| State | Player-facing map icon |
|---|---|
| Type not yet seen | One **shared question-mark symbol**, identical for every definition, so the marker says "something is here" without saying what. |
| Type known | The definition's own type icon, drawn normally. |
| Cleared | The definition's own type icon, **tinted dark**, so a swept location reads at a glance as holding nothing further of value. |

The unknown symbol is shared rather than authored per definition for the same
reason the superseded design gave: per-definition art at that state would leak
the identity the marker is meant to withhold. It is the arc's one new required
texture.

The cleared state is a **tint of the type icon**, not a second authored
texture. `emitIconQuad` already carries a per-quad `Vec4` colour and currently
pins RGB to white; darkening is a constant there. This deliberately avoids
doubling the art bill for every location type the game will eventually carry
(D-16).

### Reveal by line of sight

A location's type becomes known when a **player-owned unit can actually see a
tile the location occupies** — not when a unit wanders inside a proximity halo.
The predicate is the intersection of `Unit.LineOfSight.unitVisibleTiles` with
the location's own defined tiles: perception radius, 120° facing cone, terrain
line of sight, and the existing night factor all apply. A ruin behind a hill
stays unknown; a unit walking past facing away does not reveal it.

This replaces the `discovery_margin` halo as the trigger for **both** layers.
`findDiscoveries` and #915's `findAwareness` keep deriving from ONE shared
enumeration — precisely so the map layer and per-unit `knownLocations` cannot
drift — which now enumerates sight contacts instead of halo contacts (D-15,
resolving Q-7). Nothing consumes `discovery_margin` afterwards, and D-22
(resolving Q-11) removes it outright.

Everything else about discovery is unchanged: the promotion is one-way, fires
exactly one player-facing event, is page-scoped, and persists.

### Clearing a location

A location is **cleared** when both halves hold:

1. its authored encounter-completion policy is satisfied (`ruin_small` uses
   death-only, so every assigned nomad must be exactly dead; D-9); and
2. every **guaranteed significant item** it spawned has been taken — picked up
   at least once by any unit of any faction, latched per item and never
   un-latched (D-20).

Location contents therefore fall into two classes (D-17). *Incidental* contents
— materials, ordinary loot-table draws — are what a location is worth to
scavenge and have no bearing on clearing. *Guaranteed significant* contents are
authored to always appear: story-progression items and valuable unique
equipment. They are the reason to go, and recovering them is half of what
completes the site.

This is what replaces the retired radio-core reward (D-17). The "invest" verb
stops meaning "an artifact grants a map-wide power" and starts meaning "the
thing worth having is out there, and getting it home is the objective."

A location with no guaranteed significant contents authored clears on the
hostile condition alone; a location with no hostiles clears on the loot
condition alone.

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

### D-6. Make the progression reward guaranteed ~~and capability-changing~~

**Partially superseded 2026-08-11 by D-17.** The surviving half: random loot may
surround a guaranteed item but cannot replace it, and that item is distinct from
the existing `radio` used for unit communication. The retired half: the reward
does **not** have to "visibly enable or reveal something the colony could not do
before". A guaranteed significant item earns its place by being worth the trip
and by gating the location's cleared state — not by granting a new capability.

### D-7. Keep retrieval in the direct RTS interaction model

Pickup, carrier visibility, ordered return, and deposit use existing unit
commands. A caravan or generalized logistics surface is added only if direct
retrieval is later proven inadequate.

### D-8. Author tutorial objectives only over durable predicates

The tutorial system may gain investigate, recover, return, or advancement rows
only after the runtime can answer them from durable state. UI rows must not be
used to invent progression state that gameplay and persistence do not own.

### D-9. Make encounter completion an explicit per-location policy

Being driven away, leaving the location bounds, or temporarily disengaging does
not complete the encounter. The fixed membership set makes completion
observable and prevents a retreat from silently converting into a cleared site.
For the first `ruin_small` encounter, #916 deliberately chooses death-only:
collapsed and crawling nomads are recoverable and remain uncleared. Future
encounters may author a different terminal policy without changing that rule.

### D-10. ~~Use location intelligence to reveal nearby hidden locations in stages~~

**Superseded 2026-08-11 by D-14, D-15 and D-16.** The staged reveal was built on
a reward that no longer exists and on a hidden-by-default location class the
project owner does not want. Recorded because the *shared anonymous icon* idea
survived into D-14 — what was rejected is the radius reveal that produced it and
the per-definition visibility policy underneath it.

### D-11. Deliver tutorial expansion as its own slice

The first-session tutorial extension follows the durable encounter and reward
behavior and lands before the final integrated gate. Keeping it separate avoids
mixing UI/objective work into either gameplay ownership or test infrastructure.

### D-12. ~~Activate recovered intelligence through an explicit cargo action~~

**Superseded 2026-08-11 by D-17.** There is no location intelligence to
activate, so there is no Analyze action, no durable item-to-source association,
and no one-shot activation record. Recorded because the underlying instinct —
that recovering an item should *do* something observable rather than silently
enter a stockpile — survives in D-18, where taking the guaranteed significant
items is half of what clears the location.

### D-13. ~~Author a separate hidden-by-default location definition~~

**Superseded 2026-08-11 by D-14.** With every location marked from the start,
there is no hidden-by-default class for a definition to belong to, so no
definition needs authoring to serve a visibility mechanic. New location
definitions remain wanted (D-19) — as *content*, sequenced by what the world
should contain, not by what the map machinery needs to demonstrate itself.

### D-14. Mark every location from the start with one shared question-mark icon

Every placed location draws a zoom-map icon from world generation onward, and
until its type is known that icon is a single shared **question-mark** symbol,
identical across every definition. There is no per-definition visibility policy
and no unmarked location class — **surface and underground alike**, every
location is on the map as a question mark from the beginning and is resolved by
going and looking.

*Consequence:* the player can always see that something is out there, which
keeps starting-site selection strategic (D-3), while what it *is* stays
genuinely unknown until someone goes and looks. This makes the map a list of
leads rather than a catalogue. It also means the arc needs exactly one new
shared texture rather than a per-definition art matrix.

*Consequence:* **`LifecycleHinted` now has no planned producer, ever.** It was
reserved for a class of locations revealed by information rather than
proximity; D-14 and D-21 remove that class from the design entirely. The state
nonetheless stays in the enum: `LocationLifecycle` derives `Serialize` and is
guarded by `tools/enum_append_only_audit.py`, so deleting a constructor is a
wire-format break the audit refuses to record. Leave it in place and leave the
`Location.Instance` comment explaining it, corrected to say the reserved use was
dropped rather than that it is pending.

*Out of scope but recorded:* how an underground location at depth is drawn on
what is currently a surface zoom map is a real problem, and it belongs to D-19's
long-term work, not to this arc.

### D-15. Reveal a location's type by real per-unit line of sight

A location's type becomes known when a player-owned unit can see a tile the
location occupies, using the full `Unit.LineOfSight.unitVisibleTiles`
predicate — perception radius, 120° facing cone, terrain line of sight, and the
night factor. Seeing any single occupied tile is enough.

*Consequence:* terrain and facing become meaningful for exploration — a ruin
behind a hill stays unknown, and scouting means pointing units at things.
Rejected alternative: reusing the existing `discovery_margin` proximity halo,
which ignores facing, terrain and time of day and would reveal a location the
unit demonstrably cannot see. The cost is that discovery stops being a cheap
point-in-box test.

*Consequence (resolves Q-7):* **both** layers move to sight. The player-wide
cartographic layer (`findDiscoveries`) and #915's per-unit experiential layer
(`findAwareness`, backing `aiState[uid].knownLocations`) keep deriving from ONE
shared enumeration, which now enumerates sight contacts instead of halo
contacts. The unsplittable-enumeration contract in `CLAUDE.md` survives intact,
and a unit can no longer "know" a location it never saw. #915's own gates and
`location_content_probe.py`'s per-unit-knowledge checks must be re-verified
against the new trigger, and the two layers keep their existing differences —
awareness reports every qualifying unit and ignores lifecycle, discovery reports
the first qualifying unit and only while the instance can still promote.

*Consequence:* `ldDiscoveryMargin` / `liDiscoveryMargin` lose their only
consumer. What happens to the field is Q-11.

### D-16. Draw a cleared location as its type icon, tinted dark

Clearing does not swap in a separate authored texture. It tints the
definition's own type icon darker, via the per-quad colour `emitIconQuad`
already carries.

*Consequence:* this is an explicit, enumerated exception to the project's
no-tinting rule, alongside the existing underwater, fluid-surface and lava
tints in `World/Render/TileQuads.hs`. The rule's purpose is to stop agents
tinting things that should have their colour baked in; map-state annotation is
not that. The alternative — a per-definition "cleared" texture — was rejected
because it doubles the art bill for every location type the game will ever
carry (D-19 names a dozen).

### D-17. Retire the location-intelligence reward; gate clearing on guaranteed loot

The "recovered radio core reveals distant locations" reward is **rejected
outright** as a bad idea, not deferred. In its place, a location's contents
split into two classes:

- **Incidental** — materials and ordinary loot-table draws. What the site is
  worth to scavenge; no bearing on clearing.
- **Guaranteed significant** — authored to always appear: story-progression
  items and valuable unique equipment. The reason to go.

*Consequence:* #917 keeps its slot in the arc, and its entire body — obsolete
the moment this decision landed — was rewritten in place the same day (D-21,
Q-9). The "invest" verb changes meaning: the payoff is the significant item
itself and the site it completes, not a map-wide power the item confers.

### D-18. Clear a location on hostiles down AND significant items taken

A location is cleared when its authored hostile-completion policy is satisfied
(D-9) **and** every guaranteed significant item it spawned has been taken from
it. A location authored with only one of the two conditions clears on that one;
`ruin_small` currently has only its death-only hostile condition.

*Consequence:* clearing becomes a genuine objective rather than a combat
outcome, and a player who wins the fight but leaves the prize has not finished.
It also gives the `cleared` lifecycle state its first real producer.

### D-19. Locations own everything outside world-generator scope

Long-term, "location" covers every authored or assembled place the world
generator does not itself produce, both surface and underground:

- **Surface** — towns, villages, ruins, camps, fortresses, castles, stray
  containers, and eventually enemy-faction holdings.
- **Underground** — small loot rooms, and the arc's real destination: large
  sprawling dungeons with enemies and loot, which are intended to be where the
  main gameplay happens.

The intended underground approach is a bounding volume per feature so placements
cannot collide, then random assembly of dungeon sections into a dungeon. The
engine already handles arbitrary depth well.

*Consequence:* this is direction, not scope. None of the underground work is in
this arc. It is recorded because it decides that `ruin_small`'s single 5×5 room
is the trivial case of a much larger system, so nothing in this arc may assume
a location is small, single-level, or surface.

### D-20. "Taken" means picked up by any unit, of any faction, once

A guaranteed significant item counts as taken the first time **any** unit picks
it up — the player's units, a hostile occupant, wildlife, anything. Faction is
irrelevant.

The flag is **per item and latched**: each guaranteed significant item records
that it has been taken the first time it leaves the ground into any inventory,
and nothing un-records it. Clearing then tests that every one of them is
latched, which sidesteps the simultaneity problem — the player does not have to
be holding all of them at the same instant.

*Consequence:* dropping an item back inside the location does not un-clear the
site, and neither does losing it afterwards. This is consistent with the
existing lifecycle machinery: `promoteLifecycle` refuses backward and same-state
transitions, so `cleared` is already one-way and fires exactly once.

*Consequence:* a hostile that loots the prize and is then killed still leaves
the location cleared, and the item recoverable from its corpse or the ground.
That was judged acceptable — the site really has been emptied of what mattered.
It also means clearing is evaluable entirely from location-local state, with no
dependency on colony storage, so a lost or destroyed item can never leave a site
permanently unclearable.

### D-21. Correct #917 in place and correct epic #1229 (resolves Q-9)

#917 keeps its number and its slot as EXP-2. Its body is rewritten to the
guaranteed-significant-loot design, opening with a dated note recording that its
original premise — a recovered radio core revealing distant locations — was
rejected on 2026-08-11 and why. Keeping the number preserves the ledger link and
the epic checklist entry; keeping the note preserves the rejected premise on the
record, which closing and refiling would scatter across two issues.

Epic #1229's `## Goal`, `## Done when`, and the EXP-4/EXP-2 checklist titles are
corrected the same way, since they were written against the retired design.

Both edits are tracker mutations and therefore belong to `/process-design-doc`,
under its normal per-artifact approval — not to this design pass. **They were
applied on 2026-08-11.**

### D-22. Remove `discovery_margin` everywhere (resolves Q-11)

With both layers on sight (D-15), nothing consumes the field. It is removed from
`LocationDef`/`ldDiscoveryMargin`, from the location YAML schema, from
`LocationInstance`/`liDiscoveryMargin`, and from the CURRENT `world-pages`
component DTO, with a component version bump and a migration that drops the
value when decoding the previous shape.

The frozen historical DTOs keep their field and keep decoding, per
`docs/persistence_contract.md` — freezing them is what makes the removal safe.
Chosen over leaving a dead field on the record and in the authoring schema,
which would invite a content author to set a number that does nothing.

*Consequence:* EXP-4 carries a `world-pages` schema change and therefore the
persistence-inventory audit and save-compatibility gates, not just render and
discovery tests.

## Open questions

### Q-1. Which hostile unit art and definition unblock the first encounter?

Issue #916 requires a purpose-built hostile humanoid content package with the
states and directional animations needed by ordinary combat. Reusing a passive
animal would technically exercise combat but violates D-5's intended encounter.
This question blocks EXP-1 until the owner-provided art and unit definition are
available.

### Q-2. What exact outcome completes the first encounter?

Resolved by D-9: the completion rule is authored per encounter. `ruin_small`
uses death-only, so every assigned nomad must be exactly dead; collapsed,
crawling, missing, or driven-away occupants do not clear it.

### Q-3. Which colony capability does the guaranteed reward change?

~~Resolved by D-10~~ — **reopened and re-resolved by D-17 (2026-08-11).** The
answer is now *none*: the premise that a guaranteed reward must change a colony
capability is itself rejected. A guaranteed significant item is worth having on
its own terms and gates the location's cleared state.

### Q-4. Should the first-session tutorial expand when the two deferred verbs land?

Resolved by D-11: add a separate tutorial delivery slice after the durable
encounter/reward behavior and before the final integrated gate.

### Q-5. When does recovered location intelligence activate?

**Moot as of 2026-08-11.** D-17 retired location intelligence; nothing
activates. ~~Resolved by D-12.~~

### Q-6. What supplies the first information-revealed location population?

**Moot as of 2026-08-11.** D-14 marks every location from the start, so there is
no information-revealed population to supply. ~~Resolved by D-13.~~

### Q-7. Does per-unit location knowledge also move to line of sight?

**Resolved by D-15 (2026-08-11): both layers move to sight.** The shared
`Location.Discovery` enumeration keeps deriving both `findDiscoveries` and
#915's `findAwareness`, but now enumerates sight contacts rather than halo
contacts. The unsplittable-enumeration contract survives, and a unit can no
longer know a location it never saw.

Rejected: moving only the map layer, which would have split the one enumeration
`CLAUDE.md` documents as unsplittable and left units "knowing" ruins they never
laid eyes on.

### Q-8. What does "taken" mean for a guaranteed significant item?

**Resolved by D-20 (2026-08-11): picked up by any unit, of any faction, once,
latched per item.** Not "removed from bounds" and not "deposited in colony
storage" — a hostile looting the prize counts, and the flag never clears.

Rejected: the colony-storage reading, which would have made a location's cleared
state depend on distant colony state and could leave a site permanently
unclearable if the item were lost or destroyed.

### Q-9. How do #917 and epic #1229 get corrected?

**Resolved by D-21 (2026-08-11), at the project owner's delegation:** rewrite
#917 in place, keeping its number and its dated record of the rejected premise,
and correct #1229's goal, `Done when`, and affected checklist titles.

**Applied 2026-08-11.** Both edits landed through `/process-design-doc` under
its per-artifact approval. #917 is now *Gate location clearing on guaranteed
significant loot*; #1229 is now *[epic] Complete the expedition gameplay loop:
confrontation and a location worth clearing*.

### Q-10. Which locations does the lore/NPC stretch goal reveal, and does `hinted` survive?

**Resolved by D-14 (2026-08-11): neither.** There is no information-revealed
location class at any depth — surface and underground alike, every location is a
question mark on the map from world generation and is resolved by going and
looking. A lore or NPC reveal has nothing left to reveal, so it is dropped as a
location mechanic rather than deferred.

Consequently `LifecycleHinted` has **no planned producer, ever**. It stays in the
enum anyway: `LocationLifecycle` derives `Serialize` and
`tools/enum_append_only_audit.py` refuses to record a constructor removal as
anything but a wire-format break. The `Location.Instance` comment reserving it
for a future class must be corrected to say the reserved use was dropped.

### Q-11. What happens to `discovery_margin` once nothing consumes it?

D-15 moves both layers to sight, leaving `ldDiscoveryMargin` (the YAML/def
field) and `liDiscoveryMargin` (the persisted per-instance field) with no
consumer. The frozen `world-pages` v1/v2 DTOs must keep decoding it regardless —
they are frozen. The live record and current DTO are the choice:

1. **Keep both, unused.** Zero migration, zero risk, one dead field on the
   record and in the YAML schema.
2. **Drop it from `LocationDef`/YAML, keep the persisted field.** Stops content
   authors setting a value that does nothing; no component version bump.
3. **Remove it everywhere,** with a `world-pages` component version bump and a
   migration from the current DTO.

**Resolved by D-22 (2026-08-11): option 3 — remove it everywhere,** with a
`world-pages` component version bump and a migration. The frozen historical DTOs
keep the field and keep decoding.

### Q-12. Who makes the shared question-mark location icon?

D-14 requires exactly one new texture: a shared question-mark map symbol drawn
for every location whose type is not yet known, at the same 32 logical pixels
`locationIconTargetPixels` uses. At the time of writing it did not exist —
`assets/textures/icons/location/` held only `ruin_hidden.png` and
`ruin_discovered.png`. The sibling icon families use a `*_unknown.png`
convention (`skill_unknown.png`, `injury_unknown.png`,
`infection_unknown.png`), so `location_unknown.png` is the name that fits.

**Resolved 2026-08-11: the project owner will make this icon themselves.**

**Landed 2026-08-15 in #1230.** The owner supplied
`assets/textures/icons/location/location_unknown.png` (32x32 RGBA), and
authorized retiring `ruin_hidden.png` and repurposing
`ruin_discovered.png` as `ruin.png` — the one bitmap now serving
`discovered`, `active`, and (darkened) `cleared`/`depleted`.

It is nonetheless recorded as an explicit **art blocker in #1230** rather than
resolved ahead of time. The standing workflow rule is that a solver
reaching an art blocker STOPS and asks, and that stopping is the default: unless
the owner has already stated a method for that specific asset, the agent must
not assume either "the owner will supply it" or "generate it via PixelLab". A
placeholder or a reused ruin sprite is never an acceptable substitute.

No other art is required by this arc. D-16 deliberately makes the cleared state
a tint rather than a per-definition texture, and `ruin_small` already ships the
type icon its reveal resolves to.

## Verification strategy

- Preserve the focused location-instance, discovery, map-icon, faction, Lua
  persistence, and save-compatibility hspec coverage that owns the existing
  substrate.
- Extend the relevant combat and location-content behavior probes so the
  encounter begins through ordinary hostility, refuses to clear while any
  assigned hostile remains capable, resolves once after all are dead or
  incapacitated, and remains resolved after save/load.
- Add pure icon-selection coverage for the three map states — shared unknown
  symbol, definition type icon, tinted cleared — extending
  `Test.Headless.Location.MapIcons` (`describe "Location map icons"`) rather
  than adding a parallel spec.
- Add focused sight-reveal coverage: an occupied tile inside the cone and within
  the perception radius reveals; the same tile behind blocking terrain, outside
  the cone, or beyond the radius does not; one-way and page-scoped promotion and
  the exactly-one-event contract still hold. Extend
  `Test.Headless.Location.Discovery` and `Test.Headless.World.LocationDiscovery`.
- Cover D-22's removal of `discovery_margin` as a persistence change, not just a
  deletion: the `world-pages` component version bump, a migration that drops the
  value when decoding the previous shape, the frozen historical DTOs still
  decoding, and the persistence-inventory / save-compatibility audits.
- Re-verify the per-unit awareness layer against the same trigger, since D-15
  moves both layers together: `--match "unit location knowledge"` and
  `location_content_probe.py`'s per-unit-knowledge phases must still pass, with
  awareness keeping its existing differences from discovery (every qualifying
  unit, lifecycle ignored).
- Verify the compound clear predicate at both partial states (hostiles down with
  loot remaining; loot taken with a hostile still capable), the single-condition
  authoring cases, exactly-once promotion, and a save/load round trip carrying
  per-item taken state.
- Extend the tutorial's focused evaluator, persistence, and HUD coverage for
  the new durable objective predicates before adding them to the integrated
  scenario.
- Extend `tools/expedition_loop_probe.py` rather than creating a second whole-
  arc scenario. It must place the encounter between travel and extraction, and
  replace the current deposit-only payoff with an observable cleared location
  whose guaranteed significant item came home.
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
  not clear while any assigned hostile remains alive; all assigned hostiles
  exactly dead advances it exactly once; and membership plus outcome
  survive save/load.
- **Out of scope:** Diplomacy, formations, reputation, respawning encounters,
  and procedural dungeons.
- **Open questions:** Q-1

### EXP-4. Mark every location unknown and reveal its type by unit sight

- **Outcome:** Every location draws a shared unknown symbol from world
  generation, resolves to its own type icon once a player-owned unit actually
  sees a tile it occupies, and draws that type icon tinted dark once cleared.
- **Scope:** The shared question-mark icon; sight-based reveal replacing the
  proximity halo in the ONE shared enumeration, moving both the cartographic and
  the #915 per-unit awareness layers together; the cleared tint; lifecycle-aware
  icon selection; the corrected `LifecycleHinted` comment; and focused
  map/discovery/awareness/save coverage.
- **Phase:** Location visibility
- **Depends on:** `none`
- **Ordering:** `can land first`
- **Relevant decisions:** D-2, D-14, D-15, D-16, D-22
- **Acceptance signals:** An unseen instance of any definition draws the shared
  question-mark symbol, never its type icon; a unit that can see one occupied tile
  reveals the type, and a unit whose line of sight to every occupied tile is
  blocked by terrain or falls outside its facing cone does not; the reveal
  remains one-way, fires exactly one player event, is page-scoped, and survives
  save/load; a cleared instance draws the type icon tinted; and no stored enum
  order changes.
- **Out of scope:** The clear condition itself (EXP-2 owns it — this slice only
  renders the state), full fog of war, changing physical location visibility,
  new location definitions, and the lore/NPC reveal.
- **Open questions:** none. Q-12 was a deliberate **art blocker carried into
  the issue** — the shared question-mark texture did not exist, and the solver
  had to stop and ask the owner for it rather than placeholder, reuse the ruin
  sprite, or assume a generation method. It resolved that way on 2026-08-15:
  everything the texture did not gate was built and tested first, then the
  owner supplied `location_unknown.png` and authorized retiring
  `ruin_hidden.png` and repurposing `ruin_discovered.png` as `ruin.png`.

### EXP-2. Gate location clearing on guaranteed significant loot

- **Outcome:** A location authors guaranteed significant contents alongside its
  incidental loot, and only clears once its hostiles are down and every
  guaranteed significant item has been taken from it.
- **Scope:** The two-class content split in the location schema; guaranteed
  (not rolled) spawning that coexists with loot-table draws; durable per-item
  tracking of whether each has been taken; the compound clear predicate; the
  `cleared` lifecycle promotion and its player feedback; and save/load behavior.
- **Phase:** Invest
- **Depends on:** `EXP-1`, `EXP-4`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-6, D-7, D-9, D-17, D-18, D-20
- **Acceptance signals:** A guaranteed significant item appears exactly once per
  instance and is never duplicated by a reload or a re-entered chunk; it is
  distinguishable from incidental loot in both data and runtime state; a
  location with hostiles down but a significant item still in place does NOT
  clear; taking the last one clears it exactly once; a location authored with
  only one of the two conditions clears on that one; and the cleared state plus
  the per-item taken state survive save/load.
- **Out of scope:** The map rendering of the cleared state (EXP-4 owns it), a
  technology tree, loot tiers, quests, replacing incidental salvage with fixed
  rewards, and any map-reveal effect of a recovered item.
- **Open questions:** None. Q-8 is settled by D-20 and Q-9 by D-21; #917's body
  must already have been corrected per D-21 before this slice is worked.

### EXP-5. Extend the first-session tutorial through confrontation and advancement

> **Filed as #2301 (2026-09-02).** The 2026-08-11 deferral is discharged:
> #916 closed by merged PR #1900 and #917 by merged PR #2125, so encounter
> membership and outcome, guaranteed significant contents and their per-item
> taken flag, and a real `cleared` producer all exist. Two premises the issue
> had to settle first: a `composite` may not declare `children`, so the tree
> had no legal attachment point below `first_session_prepare_expedition`, and
> the location query verbs are page-scoped where `tutorial_eval.lua` requires
> global-per-save scope. The owner chose to relax the schema and to enumerate
> through `aiState[uid].knownLocations`.

- **Outcome:** The existing tutorial tree presents and durably tracks the
  confrontation, recovery, return, and cleared-location payoff.
- **Scope:** New data-authored rows and evaluator keys over encounter clear,
  significant-item recovery, return, and the location reaching `cleared`;
  persistence
  of full-objective latches; live recomputation of subobjectives; HUD behavior;
  and focused tests.
- **Phase:** Player guidance
- **Depends on:** `EXP-1`, `EXP-2`
- **Ordering:** `critical path`
- **Relevant decisions:** D-8, D-9, D-11, D-15, D-17, D-18
- **Acceptance signals:** Every row is answerable from durable gameplay state;
  full objectives latch, subobjectives recompute, save/load preserves only the
  intended latches, and no objective itself mutates encounter or loot state.
- **Out of scope:** A quest framework, objective rewards, or a second onboarding
  system.
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
- **Relevant decisions:** D-1, D-4, D-6, D-7, D-8, D-9, D-11, D-14, D-15,
  D-16, D-17, D-18
- **Acceptance signals:** The scenario exercises the real encounter; observes
  the location as a shared unknown symbol before it is seen and as its type icon
  after; proves the location remains uncleared both while any hostile is capable
  and while any guaranteed significant item remains in place; recovers and
  returns the real significant item; observes the cleared state; verifies
  encounter, per-item taken, cleared and tutorial state in a fresh process; and
  retains the existing prepared-versus-unprepared survival comparison.
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

**Superseded 2026-08-11 by D-17** — the radio core and the location-
intelligence chain below were rejected outright. Retained as the historical
sketch. What replaced it: guaranteed significant contents whose recovery is half
of what clears a location.

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
→ travel and discover the location by sight
→ SURVIVE the journey through a zero-occupant ruin (the #916 encounter roll
  is real, but this control path deliberately excludes combat)
→ extract the ruin's own loot-table output (no guaranteed reward — #917)
→ return and deposit into colony storage
→ save / reload in a fresh process
→ location, per-unit knowledge, objective and inventory state remain correct
```

The scenario selects a `ruin_small` whose persistent #916 encounter roll is
zero. That keeps the survival control isolated from combat timing while still
proving the shipped zero-occupant semantics: discovery reaches `cleared` from
the start and remains so across reload without emitting a fake clearance. The
remaining substitution is still explicit: "collect progression item → complete
colony project" is **extract real loot → bank it as usable colony stock**. The
scenario runs a second, **unprepared control party** over the same route under
the same orders from the same starting deficits, differing only in what it
carried out of the colony. The control must end measurably worse off in named
physiological metrics — otherwise the gate would be proving that walking works,
not that preparation matters.

The probe reports eight independent stages (`setup`, `prepare`, `travel`,
`extract`, `return`, `save`, `load`, `control`) so a failure names which part
of the loop broke, and prints a fingerprint of its selected ruin, loot and
sites so two consecutive runs can be compared for identity. It is manual-only
by classification in `tools/ci_probes.py`: a real worldSize-64 generation plus
two travellers walking ~30 tiles each way is too slow for a blocking per-PR
gate, and it leans on AI arbitration timing.

The hostile branch of #916 belongs between travel and extract, but remains out
of this survival-control probe so combat cannot confound its food comparison.
When #917 lands, its guaranteed significant item can strengthen the existing
deposit assertion. (As of 2026-08-11 this replaces the earlier wording, "the
progression project turns the existing deposit assertion into a capability
change" — see D-17.)

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
