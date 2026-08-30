# Nomad primitive encounter design

This document designs Synarchy's first generic human enemy and the smallest
repeatable authored location threat: one to three primitive nomads living in
the wilderness and sometimes occupying a small ruin. It exists because this is
larger than one implementation issue: the arc needs an approved character
identity, a substantial directional animation package, a spawnable unit
definition, species-specific ambient behavior, autonomous hostility, optional
location encounter authoring, durable encounter ownership, persistence,
feedback, and focused verification.

Design state: `exploring`

Implementation authority is now the approved #916 specification. This document
retains the earlier alternative exploration, but where it differs from that
handoff the issue wins: every `ruin_small` rolls 0–3 occupants, the authored
policy is death-only, ordinary player Move is the withdrawal interaction, and
nomads start unarmed with no inventory drops.

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [ ] EPIC. Add nomad primitives and optional hostile ruin encounters
- [ ] NPE-1. Establish the nomad primitive character and base rotations
- [ ] NPE-2. Produce and validate the nomad primitive animation package
- [ ] NPE-3. Register the nomad primitive as a gameplay unit
- [ ] NPE-4. Add durable authored encounter ownership to locations
- [ ] NPE-5. Author optional one-to-three-nomad small-ruin encounters
- [ ] NPE-6. Complete autonomous combat, withdrawal, and encounter feedback
- [ ] NPE-7. Integrate encounter verification and documentation

## Epic contract

- **Goal:** Add one reusable wilderness-human enemy whose small groups can
  produce the game's smallest understandable location encounter without making
  every small ruin hostile.
- **Done when:** A normal world can contain both unoccupied small ruins and
  small ruins occupied by one to three `nomad_primitive` units, selected by a
  reusable per-location encounter-set function with a 25% chance of each count
  from zero through three for `ruin_small`; the occupants
  have complete, approved visuals and coherent ambient/combat behavior; the
  encounter spawns exactly once, begins through ordinary hostility, has bounded
  pursuit and explicit player withdrawal, reports its important transitions,
  and preserves membership and outcome across a fresh-process save/load.
- **Users and operators:** Players undertaking early expeditions; the project
  owner designing and approving unit art; content authors adding later nomad
  encounters; maintainers operating unit-asset, location, combat, and
  persistence gates.
- **Arc label:** `expedition`

## Current state and evidence

- `docs/expedition_gameplay_loop.md` already makes one hostile occupant and the
  first combat encounter EXP-1, tracked by #916. Its durable design calls for a
  complete hostile unit content package, optional encounter behavior, bounded
  pursuit/disengagement, durable location membership, feedback, and save/load.
  It does not design the hostile human itself.
- #916 currently has `reviewed:changes`. Its canonical review correctly found
  that the body is a deferred sketch rather than a hand-off-quality contract;
  it also requires the exact unit and location definitions, art-readiness
  contract, encounter provenance, withdrawal behavior, defeat rule, loot
  ownership, and focused verification to be settled.
- Typed faction relations (#912) and persisted location lifecycle state (#911)
  have shipped. `scripts/locations.lua` already defaults authored unit contents
  to faction `hostile`, but it discards the spawned UID and therefore cannot
  establish durable encounter membership or an encounter-relative leash.
- `data/locations/ruin_small.yaml` is currently an abandoned 5x5 ruin with two
  deterministic `ruin_common` loot-table rolls and no units. Up to six
  instances may be placed in one world.
- Location unit contents currently support a fixed integer `count`, an optional
  fixed position, and a faction. The YAML/runtime contract has no count range,
  encounter probability, or alternative-content choice, so “one to three” and
  “some ruins remain empty” require an explicit authoring design rather than a
  data-only edit.
- Runtime units are auto-discovered from `data/units/*.yaml`, but behavior is
  separately registered in Lua. A new species normally needs a gameplay unit
  YAML, species AI/config wiring, resource/need policy, and a combat animation
  suffix entry when its animation names vary by weapon class.
- `scripts/unit_ai_combat.lua` has only the `incoming_hit` threat source;
  `hostile_in_sight` remains a commented future source. Ordinary faction
  hostility therefore does not begin a fight today.
- `scripts/unit_ai_combat_attack.lua` already owns pursuit and attack animation
  selection. `scripts/unit_ai_combat.lua` already owns autonomous retreat from
  a futile fight, but a player move order does not reliably withdraw a unit
  because active combat has higher utility.
- The current faction model is not a tag system. `Unit.Faction` gives every
  unit exactly one closed `Faction` constructor (`player`, `wildlife`,
  `hostile`, `neutral`, or `debug`), saves one lowercase `factionTag`, and
  hard-codes ownership, commandability, and the symmetric relation table in
  Haskell. Adding only a `FactionNomad` constructor would not express
  `{player, nomad}` or keep player ownership orthogonal to cultural hostility.
- The asset contract is strict. Gameplay frames live under
  `assets/textures/units/<name>/animations/<activity>/<direction>/frame_NNN.png`,
  every frame must be declared in `data/units/<name>.yaml`, filenames are
  contiguous from `frame_000.png`, one animation has one pixel size, and the
  strict atlas validator decodes every PNG. Symmetric animations store five
  directions and mirror westward views; asymmetric weapon-hand animations
  store all eight.
- The existing `tiller` and `unknown_unit` trees are asset-only, non-spawnable,
  and incomplete for this role. Neither is the hostile unit requested here.
- The project owner will draw the nomad art manually. No agent image generation
  or PixelLab work belongs to this arc. The first art handoff is a small set of
  engine-shaped base files, previewed before the owner draws the full animation
  matrix.

## Desired experience

The nomad primitive is the baseline human danger in the wilderness rather than
a boss, faction leader, or bespoke story character. A player may encounter one
alone or a small group of up to three. At a ruin they should look as though
they live there: some patrol, some idle or lounge, and they react coherently
when a hostile unit becomes visible.

An unoccupied ruin remains a valid ordinary result. A player should not be able
to infer from “small ruin” alone that combat is guaranteed. When an occupied
ruin does occur, its occupants belong to that encounter even if they patrol
outside its footprint, pursue the player, disengage, or are moved by combat.

The first slice is intentionally small. It establishes a reusable unit and a
one-to-three-member ruin encounter; it does not attempt nomad society,
settlements, diplomacy, procedural equipment cultures, or a general encounter
  director.

Nomad identity and player control are independent dimensions. An ordinary
encounter nomad carries the `nomad` tag. A future player-controlled nomad
carries both `player` and `nomad`; the `player` tag grants ownership/control but
does not change relations. Player acolytes carry `player` and `acolyte`.
`acolyte` and `nomad` are mutually hostile regardless of which side, if either,
is player-controlled.

## Scope

### In scope

- One exact gameplay unit identifier, `nomad_primitive`, representing a human
  wilderness nomad suitable for reuse alone or in small groups.
- An explicitly approved visual identity and a validated directional texture
  and animation package.
- A spawnable unit definition: body, stats, skills, needs, equipment/inventory,
  portrait and directional fallbacks, state-to-animation mappings, and atlas
  declarations.
- Composable unit identity tags sufficient to represent `{player, acolyte}`,
  `{nomad}`, and future `{player, nomad}` units while keeping ownership/control
  separate from acolyte-versus-nomad hostility.
- Nomad-specific ambient behavior for guarding a home area, patrolling, and
  visually legible idle or lounging periods.
- One-to-three-member small-ruin encounters, while retaining unoccupied small
  ruins as an ordinary world result.
- Typed-faction hostility, same-page visible-target acquisition, bounded
  encounter-relative pursuit, disengagement, and an explicit player withdrawal
  interaction.
- Durable association between every encounter member and its location
  instance, exactly-once spawning, one-way outcome, and save/load.
- Edge-triggered player feedback for initial aggression, disengagement, and
  encounter resolution.
- Focused asset, unit, location, combat, persistence, and integrated encounter
  verification.

### Out of scope

- A full nomad faction simulation, diplomacy, reputation, recruitment, trade,
  settlements, families, schedules, or population growth.
- More nomad equipment tiers or visual cultures than the first unit needs.
- Formations, squad tactics, ranged-combat expansion, respawning encounters,
  procedural dungeons, or a general-purpose encounter director.
- Making every `ruin_small` instance hostile.
- Reusing passive-animal or asset-only placeholder art for the hostile human.
- The guaranteed-significant-loot half of location clearing, which remains the
  separate EXP-2 arc tracked by #917.

## Design

### Tracker shape

Existing #916 will be rewritten as a nested epic tracker under the expedition
epic rather than closed and replaced. This keeps the original dependency links
and design history while acknowledging that the unit art, gameplay definition,
faction substrate, encounter substrate, combat behavior, and verification are
separately reviewable deliveries. No tracker mutation occurs during this
design workflow.

### Unit identity and faction

`nomad_primitive` names the unit's culture/technology role. It carries the
`nomad` identity tag rather than the generic current `hostile` faction. A
player-controlled nomad later carries `{player, nomad}`; a player-controlled
acolyte carries `{player, acolyte}`. The `player` tag governs ownership and
commandability only and contributes nothing to relations. The relation policy
makes `acolyte` and `nomad` mutually hostile.

This cannot be implemented faithfully by adding one enum constructor. The
current single-faction field, save wire value, Lua API, discovery/commandability
properties, medic/swarm alliance checks, and attack permission all require a
designed transition to a set of tags plus policy queries. A narrow
`FactionNomad` patch would bake the wrong model into more saves and call sites.

**Proposal P-1:** Treat composable faction identity as a separately designed
prerequisite arc or prerequisite delivery, not as a hotfix hidden inside the
nomad runtime PR. It may later have its own tracker because it changes every
unit's identity contract and save/API shape. This document records the nomad
requirements that prerequisite must satisfy but does not yet choose its final
slice boundary.

### Provisional art and animation inventory

The project owner draws every visual. Agents may inventory paths, validate
files, wire YAML, and preview existing art, but must not generate or edit the
nomad images. The final matrix still depends on weapon/loadout and on how
visually complete the first generic human must be.

**First manual drawing step — exact engine files**

The smallest useful first handoff is a neutral idle/T-pose at the same human
scale as the acolyte. Draw these five transparent 48x48 RGBA PNGs:

```text
assets/textures/units/nomad_primitive/animations/idle/south/frame_000.png
assets/textures/units/nomad_primitive/animations/idle/south-east/frame_000.png
assets/textures/units/nomad_primitive/animations/idle/east/frame_000.png
assets/textures/units/nomad_primitive/animations/idle/north-east/frame_000.png
assets/textures/units/nomad_primitive/animations/idle/north/frame_000.png
```

The engine mirrors those into south-west, west, and north-west. If the base
silhouette has handed equipment, one-sided clothing, or another feature that
must not mirror, also draw the following three 48x48 files and the idle
declaration will use `flip: false`:

```text
assets/textures/units/nomad_primitive/animations/idle/south-west/frame_000.png
assets/textures/units/nomad_primitive/animations/idle/west/frame_000.png
assets/textures/units/nomad_primitive/animations/idle/north-west/frame_000.png
```

The 32x32 portrait is part of the same identity checkpoint but can be drawn
after the directional silhouette is approved:

```text
assets/textures/units/nomad_primitive/portrait.png
```

No other animation files are needed for this first step. The next manual-art
step will begin with `walk` and `run`, then stop for an in-game preview before
combat, injury, lounge, and lifecycle files are drawn.

**Base identity and static assets**

- Five or eight directional idle/T-pose drawings, according to the approved
  symmetry rule; these establish silhouette, clothing, hair, skin, carried
  gear, handedness, and scale.
- `portrait.png` at 32x32.
- Five stored directional idle frames for symmetric presentation, with the
  three westward directions mirrored at runtime; all eight directions for any
  asymmetric weapon-in-hand state.

**Baseline locomotion and world state**

- `idle`, `walk`, and `run`.
- `injured_idle`, `injured_walk`, and `injured_run` if the unit uses the shipped
  wounded-visual contract rather than falling back to healthy motion.
- `falling`, `landing`, and the pose transitions needed when pursuit crosses
  ordinary world elevation.
- Climb and pull-up coverage if these nomads are allowed to pursue across
  climbable terrain; otherwise the AI/pathing contract must prevent that route
  rather than silently T-posing.

**Combat**

- `combat_idle`, `combat_hit_react`, `attack_quick`, and `attack_heavy` for
  every supported weapon class.
- Corresponding `injured_` combat variants if wounded combat remains visually
  distinct.
- `hit_react` for non-combat state-driven impacts where the combat override is
  not active.

**Incapacitation and death**

- A transition into `collapsed`, a held collapsed pose, recovery transitions
  for a recoverable human, and `death`.
- `injured_death` and any injured-collapse distinction required by the chosen
  state map.
- Crawling states and transitions only if incapacitated or badly injured
  nomads are intended to move; otherwise the behavior contract must keep a
  collapsed unit stationary.

**Living at the location**

- At least one visually distinct lounging/at-ease loop if “lounging around the
  location” is meant literally rather than as ordinary idle.
- Eating, drinking, sleeping, pickup, and posture-transition animations only
  for needs and activities the first nomad AI actually performs. They should
  not be copied from the acolyte matrix merely because they exist there.

The document must eventually turn this inventory into an exact table naming
each animation, its source state, symmetry/handedness rule, state mapping,
looping behavior, intended frame count, and owning delivery slice before art
drawing begins.

### Provisional unit data and script inventory

**New data/assets expected**

- `data/units/nomad_primitive.yaml` as a runtime `units:` definition.
- `assets/textures/units/nomad_primitive/portrait.png`.
- `assets/textures/units/nomad_primitive/animations/...` for the approved
  matrix.
- New equipment/item YAML and textures only if the chosen primitive loadout
  cannot be expressed with shipped equipment.

**New Lua ownership proposed**

- `scripts/nomad_primitive_ai.lua` for location-relative ambient behavior,
  patrol/lounge state, and species config/action registration.
- A generic location-encounter module if durable membership, leash checks,
  clearance, and feedback would otherwise bloat `scripts/locations.lua` or a
  species-specific AI file. Encounter lifecycle must not be owned by the nomad
  species module because later hostile occupants should reuse it.

**Existing Lua surfaces likely to change**

- `scripts/unit_ai.lua` or its boot wiring to load/register the new species.
- `scripts/unit_ai_combat.lua` for a same-page, visibility-aware
  `hostile_in_sight` threat source and clean disengagement.
- `scripts/unit_ai_combat_attack.lua` for the nomad's weapon-class animation
  naming and encounter-relative pursuit/leash handoff.
- `scripts/locations.lua` for deterministic optional group spawning and for
  retaining each spawned UID long enough to register durable provenance.
- `scripts/init_context_menu.lua` and/or command plumbing only after the player
  withdrawal interaction is chosen.
- `scripts/combat_log.lua` or the established player-event surface for
  non-spamming aggression, disengagement, and resolution feedback.
- Lua persistence registration only for state that belongs in Lua; durable
  encounter identity and outcome should remain location-owned.

**Existing Haskell surfaces likely to change**

- `Engine.Asset.YamlLocations` and `Location.Types` if optional contents or a
  deterministic count range become authorable location data.
- `Location.Instance` and its Lua API for durable member/provenance and outcome
  queries, without overloading geometry/content-spawn flags.
- The current save DTO/component version and migration for any new persisted
  location fields, following `docs/persistence_contract.md` and the append-only
  lifecycle rule.
- Unit APIs only where existing visibility, faction, pose, inventory, and
  movement queries cannot express the approved behavior.

### Generic location encounter selection and determinism

Every location invokes one generic encounter-selection function with the
location's definition and placed-instance information. The function chooses
one authored unit set from the candidates for that location and passes the
selected set to generic unit spawning/membership registration. A location with
no encounter candidates or an empty selected set spawns no enemies. The
selector is not nomad-specific and later locations may offer mixed or different
unit sets without adding a new spawn implementation.

For `ruin_small`, the first candidate table is exactly four equiprobable sets:

| Weight | Selected set |
|---:|---|
| 25% | no encounter units |
| 25% | one `nomad_primitive` with tag `nomad` |
| 25% | two `nomad_primitive` units with tag `nomad` |
| 25% | three `nomad_primitive` units with tag `nomad` |

The selection is random in play but must derive from stable world seed and
location-instance identity rather than global `math.random` call order. The
same placed ruin therefore selects the same set before and after load and in a
fresh process, while a test can choose known identities that exercise all four
outcomes. The selected set is either persisted directly or reproducible from
durable inputs; that implementation boundary remains to be designed with the
save contract.

### Encounter ownership and behavior

Each occupied ruin establishes a fixed membership set when its contents spawn.
The association survives leaving the bounds, pursuit, disengagement, collapse,
death, save, and load. It supplies the home anchor/leash and prevents clearance
from being inferred from whichever units happen to stand inside the footprint.

Patrolling and lounging are encounter-relative ambient behaviors. A nomad may
move outside the 5x5 room, but the approved patrol radius and valid terrain
must keep it plausibly attached to the ruin. On hostility, only a `RelHostile`
unit on the same world page and satisfying the approved visibility rule can be
acquired. Pursuit ends at the encounter-relative leash or under the approved
withdrawal/disengagement conditions, clears combat state, and returns a living
nomad toward its home behavior.

The approved #916 handoff makes the first ruin policy death-only: collapsed,
crawling, missing, or disengaged nomads remain assigned and prevent clearance.
The separate #917 design may later add a guaranteed-significant-item half
before the owning location itself becomes `cleared`.

### Documentation impact

- This document is the unit/encounter design authority while exploring and
  later becomes the source for epic and child tracker processing.
- `docs/expedition_gameplay_loop.md` will need a concise amendment replacing
  its unresolved generic art blocker with this arc's selected unit and linking
  #916's eventual nested-epic role. It should retain the parent arc's settled
  defeat and clearing decisions.
- `docs/asset_generation.md` should change only if this work establishes a new
  reusable pipeline rule. The unit-specific art matrix belongs here,
  not in the global pipeline guide.
- `docs/persistence_contract.md` is a contract to follow, not automatically a
  document to edit. A new persisted root/owner must update the authoritative
  persistence inventory; an ordinary versioned field addition follows the
  existing migration procedure.
- `tools/README.md` must describe any new probe or materially extended
  location/combat probe.
- `CLAUDE.md` changes only for a durable cross-cutting contract future work
  must know, not for a one-unit implementation narrative.
- The first publication of this new document requires a separate PR that also
  enrolls its path in `docs/agent-workflow-contract.md` as required by the
  workflow contract. This design session leaves it local and unpublished.

## Decisions

### D-1. The first generic enemy is `nomad_primitive`

The first generic enemy is a human wilderness nomad rather than a reused
animal or placeholder. The stable gameplay identifier is
`nomad_primitive`. This unit establishes the smallest reusable human threat;
larger nomad social structures and more advanced variants come later.

### D-2. Nomad primitives appear alone or in groups of at most three

The first location encounter contains one to three primitive nomads. They may
patrol or lounge around their location rather than remaining fixed to one tile.
The small group is the baseline encounter, not a formation or squad system.

### D-3. Small ruins are not uniformly hostile

Some small ruins remain unoccupied and retain their existing salvage role.
Players must not be able to infer that every `ruin_small` guarantees combat.
The generic selector and equal four-way outcome are specified by D-5 and D-6.

### D-4. Existing #916 becomes the nested epic tracker

#916 will be rewritten in place as this arc's epic tracker under the existing
expedition epic. Its current changes-requested specification remains untouched
until this design is ready and later tracker processing presents the full epic
rewrite for separate approval.

### D-5. Every location uses a generic encounter-set selector

Each placed location passes its definition and instance information to one
generic function that chooses an authored unit set and hands it to generic
spawning and membership registration. Location definitions supply candidate
sets; the selector and spawn path do not hard-code nomads or ruins.

### D-6. A small ruin has four equiprobable nomad outcomes

`ruin_small` selects zero, one, two, or three `nomad_primitive` occupants with
25% probability for each outcome. It remains one location definition rather
than gaining a separate occupied variant.

### D-7. Unit identity is a composable tag set

Player acolytes carry `{player, acolyte}`, ordinary nomads carry `{nomad}`, and
future player-controlled nomads carry `{player, nomad}`. `player` supplies
ownership/control but has no relation effect. `acolyte` and `nomad` are
mutually hostile, including when either side also carries `player`.

## Open questions

### Q-1. Should existing #916 become this arc's epic tracker?

**Resolved by D-4.** #916 will be rewritten in place as a nested epic under the
existing expedition epic. The edit waits for this design to become ready and
for the later tracker-processing approval checkpoint.

### Q-2. How is an occupied small ruin authored and selected?

**Resolved by D-5 and D-6.** Every location uses the generic encounter-set
selector, and `ruin_small` supplies four equally weighted sets holding zero
through three nomads. A separate occupied definition is rejected.

### Q-3. Is `nomad_primitive` intrinsically hostile?

**Resolved by D-7.** `nomad_primitive` carries the `nomad` tag. Hostility comes
from the symmetric acolyte-versus-nomad tag relation, not from a generic
`hostile` identity. A future player nomad retains `nomad` while also carrying
`player` for ownership/control.

### Q-4. What is the first nomad's approved visual identity and loadout?

The art matrix cannot be finalized until silhouette, clothing, skin/hair
variation, handedness, weapon class, armor, carried gear, and portrait
direction are chosen. New weapon/equipment art expands the arc; reusing shipped
equipment constrains the look and combat animation suffixes.

### Q-5. How complete is the first human animation matrix?

Choose between full acolyte-like human lifecycle coverage and an encounter-
focused set whose AI/pathing explicitly prevents unsupported activities.
Patrol, lounge, needs, climb/fall, injury, collapse/recovery, and death each
need either real visual coverage or an explicit behavioral exclusion.

### Q-6. What ambient life does “patrolling or lounging” require?

Decide patrol radius and cadence, whether some members remain near the room,
which lounge postures are visible, whether nomads eat/drink/sleep or scavenge,
and how they return home after disengagement. These choices drive animation,
AI state, persistence, and test scope.

### Q-7. What inventory and loot survive a defeated nomad?

The engine does not automatically turn a dead unit's inventory into ground
loot. The arc must decide whether nomads carry recoverable equipment/items,
whether death or incapacitation drops anything, and whether occupant loot is
separate from the ruin's incidental and guaranteed-significant contents.

### Q-8. What is the explicit player withdrawal interaction?

An ordinary move order currently loses to active combat utility. The design
must choose whether a player move command explicitly cancels/preempts combat,
whether a dedicated Retreat command is added, or whether another interaction
expresses withdrawal. Autonomous fear-based retreat is not a substitute for a
player command.

### Q-9. What visibility and leash rules define aggression?

The parent arc requires same-page hostile acquisition, a selected visibility
rule, bounded pursuit, and clean disengagement. This design must name the exact
visibility source, home-relative patrol and pursuit radii, reacquisition rules,
and the state/feedback emitted when contact breaks.

### Q-10. Is composable faction identity a child of #916 or a separate prerequisite arc?

The desired tag model changes every unit's identity type, save wire shape,
ownership/commandability policy, Lua faction API, relation resolution, and
alliance/attack call sites. It is too broad and compatibility-sensitive for a
hotfix. The remaining choice is whether this design owns it as one or more #916
children, or whether a separate faction-tag design/tracker lands first and #916
depends on it. The latter keeps generic faction architecture out of an
encounter epic; the former keeps all work needed by the first nomad encounter
under one umbrella.

## Verification strategy

- Validate and preview the owner's five/eight manual idle drawings, then the
  `walk`/`run` files, before the owner draws the full matrix. Agents do not
  generate or edit the images.
- Run `python3 tools/test_pack_atlas.py` and
  `python3 tools/pack_atlas.py --validate-only --strict` for every asset/YAML
  delivery. Preview coverage must prove every declared animation and direction
  resolves without fallback or accidental mirroring of asymmetric gear.
- Add focused YAML-decoder tests for the chosen optional-encounter and group-
  size authoring contract, including malformed ranges/probabilities and stable
  defaults.
- Extend location-content coverage for deterministic empty versus occupied
  outcomes, group sizes one through three, exactly-once spawning, valid faction
  assignment, durable UID membership, and no dependency on load/call order.
- Add headless Lua coverage for patrol/lounge state, faction filtering,
  same-page acquisition, visibility rejection, leash, disengagement, return
  home, explicit player withdrawal, and non-spamming transition feedback.
- Exercise death and recoverable incapacitation separately; the encounter must
  not resolve while any assigned member remains capable and must not infer
  membership from current bounds.
- Add a fresh-process save/load round trip with an encounter active and another
  resolved, proving membership, unit/AI state, spawn idempotency, and one-way
  outcome.
- Extend the existing expedition integrated gate only after the focused unit,
  encounter, combat, and persistence gates are stable. Keep stage reporting
  separate so failures identify art/registration, spawn, aggression,
  withdrawal, resolution, save, or load.
- Include an offscreen/manual path for player-visible combat and disengagement
  feedback that a headless event assertion cannot establish visually.

## Delivery plan

The slices below are provisional while Q-4 through Q-10 remain open. They are
kept dependency-valid and intentionally smaller than the current #916 body.

### NPE-1. Establish the nomad primitive character and base rotations

- **Outcome:** The character's visual identity, scale, silhouette, loadout and
  eight directional base rotations are approved.
- **Scope:** Character brief, owner-drawn five/eight directional 48x48 idle
  frames, symmetry/handedness choice, 32x32 portrait direction, preview, and an
  asset-only declaration if base files are committed before the gameplay
  matrix.
- **Phase:** Art identity
- **Depends on:** `none`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1
- **Acceptance signals:** The owner approves the directional silhouette;
  stored files have the intended dimensions, transparency and naming and pass
  the strict asset gate without agent-generated imagery.
- **Out of scope:** State-animation drawing, gameplay registration, encounter
  logic.
- **Open questions:** Q-4

### NPE-2. Produce and validate the nomad primitive animation package

- **Outcome:** Every approved locomotion, combat, injury, incapacity, death and
  ambient animation exists in every required direction and previews correctly.
- **Scope:** Owner-drawn approved matrix, YAML animation declarations,
  portrait, preview and strict atlas validation. Agents may inventory and
  validate files but do not create or edit the art.
- **Phase:** Art matrix
- **Depends on:** NPE-1
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2
- **Acceptance signals:** Exact matrix complete; no missing/corrupt frames;
  symmetry rules correct; baseline and combat/injury previews show no fallback.
- **Out of scope:** Runtime stats/behavior and location spawning.
- **Open questions:** Q-4, Q-5, Q-6, Q-7

### NPE-3. Register the nomad primitive as a gameplay unit

- **Outcome:** `nomad_primitive` is spawnable, renderable, physiologically
  coherent, and capable of its approved non-encounter ambient behavior.
- **Scope:** Runtime unit YAML, body/stats/skills, equipment/inventory, state
  mappings, needs/resources, species config, ambient AI registration, and
  combat animation naming.
- **Phase:** Unit runtime
- **Depends on:** NPE-2
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2
- **Acceptance signals:** Direct spawn/list/render succeeds; ambient actions and
  every reachable pose/activity resolve an approved animation; unit state and
  AI survive save/load.
- **Out of scope:** Location membership, optional occupancy, autonomous
  hostility.
- **Open questions:** Q-4, Q-5, Q-6, Q-7, Q-10

### NPE-4. Add durable authored encounter ownership to locations

- **Outcome:** A location can own a fixed, persisted member set and expose the
  home/provenance and outcome needed by any authored encounter.
- **Scope:** Generic encounter identity and membership, spawned-UID capture,
  location-relative home/leash data, lifecycle/outcome queries, versioned
  persistence and migration.
- **Phase:** Encounter substrate
- **Depends on:** `none`
- **Ordering:** `can land first`
- **Relevant decisions:** D-3
- **Acceptance signals:** Membership is fixed at spawn, independent of current
  bounds, exactly-once, page-safe and preserved through fresh-process load.
- **Out of scope:** Nomad art/AI, aggression policy, guaranteed loot.
- **Open questions:** Q-9

### NPE-5. Author optional one-to-three-nomad small-ruin encounters

- **Outcome:** Ordinary worlds and direct fixtures can produce both empty small
  ruins and occupied small ruins with a stable group size from one to three.
- **Scope:** Generic candidate-set authoring and selection, stable 25%-each
  zero/one/two/three distribution for `ruin_small`, nomad tag assignment,
  placement constraints, nomad membership
  registration and initial patrol/lounge home state.
- **Phase:** Encounter content
- **Depends on:** NPE-3, NPE-4
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2, D-3
- **Acceptance signals:** Empty and occupied cases both reachable; group sizes
  one/three bounded correctly; fixed seed/instance reproduces the selection;
  contents never respawn.
- **Out of scope:** General encounter director, other locations/species.
- **Open questions:** Q-6, Q-7, Q-10

### NPE-6. Complete autonomous combat, withdrawal, and encounter feedback

- **Outcome:** Occupied-ruin nomads notice legitimate hostiles, fight within a
  bounded home-relative pursuit contract, disengage coherently, and expose an
  explicit player withdrawal interaction and understandable feedback.
- **Scope:** Hostile-in-sight acquisition, faction/page/visibility filtering,
  target pursuit/leash/reacquisition, return-home behavior, player withdrawal,
  aggression/disengagement/resolution events, and hostile-half encounter
  completion.
- **Phase:** Confrontation
- **Depends on:** NPE-5
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2, D-3
- **Acceptance signals:** Neutral/allied/off-page/invisible units do not
  auto-acquire; legitimate hostiles do; pursuit ends at the leash; withdrawal
  clears combat; messages edge-trigger; all assigned hostiles down resolves
  once and survives load.
- **Out of scope:** Diplomacy, formations, ranged-combat expansion, #917's
  guaranteed-significant-loot predicate.
- **Open questions:** Q-7, Q-8, Q-9

### NPE-7. Integrate encounter verification and documentation

- **Outcome:** The encounter is covered by focused and integrated gates and the
  durable repository documentation points to the final ownership boundaries.
- **Scope:** Probe/test consolidation, fresh-process integrated encounter stage,
  `tools/README.md`, parent expedition design amendment, and only those global
  contract docs actually changed by implementation.
- **Phase:** Integration
- **Depends on:** NPE-6
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2, D-3
- **Acceptance signals:** Focused gates and the extended expedition scenario
  pass with independently reported stages; docs match live owners and contracts.
- **Out of scope:** Tutorial expansion and #917's reward implementation.
- **Open questions:** None after preceding slices settle their contracts.

## Source notes

The project owner described `nomad_primitive` as humans living in the
wilderness, alone or in small groups. They are the first generic enemy and the
smallest generic location threat. A small ruin may have one to three primitive
nomads patrolling or lounging around it, while some ruins must still have no
threat. The owner requested a design document and epic tracker shape before any
new issues are created or the existing #916 specification is edited.

The owner subsequently selected #916 as the nested epic tracker; rejected a
separate occupied-ruin definition in favor of a generic per-location function
that selects authored unit sets; specified equal 25% `ruin_small` outcomes for
zero through three nomads; and required a composable tag model in which
`{player, acolyte}` and `{nomad}` are mutually hostile by their cultural tags,
while future `{player, nomad}` units remain player-owned without becoming
acolyte-allied. The owner will draw all nomad visuals and asked agents only for
the exact file inventory and technical integration work.
