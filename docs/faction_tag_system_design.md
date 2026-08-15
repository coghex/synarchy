# Faction tag system design

This document designs a composable unit-identity and relation system to replace
Synarchy's current one-enum-per-unit faction model. The immediate need is to
represent player acolytes, non-player nomads, and future player-controlled
nomads without making ownership synonymous with culture. The design also keeps
the data model capable of distinguishing different players later, without
attempting to build multiplayer networking in this arc.

Design state: `ready for issue processing`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [ ] EPIC. Replace single factions with composable unit identity and relations
- [ ] FTS-1. Add the pure faction identity and relation policy model
- [ ] FTS-2. Add validated faction-tag definitions and legacy mappings
- [ ] FTS-3. Migrate runtime units, spawning, and save data to faction profiles
- [ ] FTS-4. Port ownership, commandability, and discovery consumers
- [ ] FTS-5. Port alliance, hostility, combat, and AI consumers
- [ ] FTS-6. Expose mutable faction profiles and directed order hostility
- [ ] FTS-6A. Propagate directed hostility through orders and team knowledge
- [ ] FTS-7. Retire the legacy enum and close compatibility documentation

## Epic contract

- **Goal:** Let one unit carry independent control/ownership and cultural or
  organizational identities, so player ownership does not erase faction
  relationships and faction identity does not decide who can issue commands.
- **Done when:** Player-controlled acolytes, ordinary nomads, and future
  player-controlled nomads are representable without duplicate unit
  definitions; same-controller and shared-team units are friendly by default
  but explicit higher-precedence conflict tags can divide either group; human-,
  AI-, and uncontrolled units are distinguishable; units may carry any number
  of opaque string tags minted by data or runtime systems; diplomacy changes
  directed tag-to-tag relationships rather than controller identity; a shared
  team makes naturally hostile affiliations friendly until a more specific
  live conflict applies; different teams sharing `acolyte` remain friendly
  until an explicit diplomatic change makes them hostile; unrelated tags
  default neutral; acolyte and nomad identities are mutually hostile when no
  higher-precedence live relation applies; live causes reduce by
  `hostile > neutral > ally`; relation mutations name their exact tags and
  ordinary systems can remove only the tag memberships they own; and an
  initiated fight can be canceled cleanly only before reverse hostility is
  established; the target
  team is triggered only when communication installs its reverse team-wide
  relation; escalated hostility is cleared only by diplomacy scripting while
  already active attack goals finish only against their assigned targets; all
  tag provenance, relation, incident, communication, and grandfathered-combat
  state persists; explicit debug combat remains isolated from ordinary
  diplomacy; Lua uses narrow authoritative operations rather than replacing
  profiles; the five legacy faction values have an exact compatibility map;
  faction definitions and base relations are data-driven; every current
  ownership, commandability, alliance, combat, discovery, transfer, spawn, Lua,
  and persistence consumer reads the new authoritative policy; and old saves
  migrate without silently changing existing unit behavior.
- **Users and operators:** Players controlling units; future multiplayer and
  diplomacy designers; content authors defining unit identities and relations;
  maintainers of unit, combat, AI, location, Lua, and save contracts.
- **Arc label:** `units`

## Current state and evidence

- `Unit.Faction` defines one closed `Faction` enum with five constructors:
  `player`, `wildlife`, `hostile`, `neutral`, and `debug`. Every live
  `UnitInstance` stores exactly one `uiFactionId :: Faction`.
- The current type intentionally separates three single-faction properties
  (`isPlayerOwned`, `isPlayerCommandable`, and
  `hasUnrestrictedCombat`) from one symmetric two-faction relation table. That
  separation is conceptually sound and should survive; what no longer fits is
  deriving every answer from one enum value.
- Only `player` is player-owned. `player` and `debug` are commandable and allied;
  `debug` has unrestricted combat. `neutral` is neutral with every distinct
  faction. Every other distinct pair is hostile, and a faction is allied with
  itself.
- Faction assignment is spawn-time-only. Unit definitions carry no faction
  identity. Portal/building paths pass `player`, debug paths pass `debug`,
  location contents default to `hostile`, and an omitted `unit.spawn` faction
  defaults to `wildlife` for world-generated animals.
- `unit.spawn` accepts one faction string. `unit.getFaction` returns one string.
  The pure Lua `faction` table accepts one or two strings for ownership,
  commandability, alliance, relation, and attack-permission queries.
- The current model has production consumers across discovery, building
  knowledge, transfer eligibility, context-menu attack permission, medic AI,
  group combat effectiveness, location spawning, save/load, and unit spawn.
  A tag conversion cannot safely update combat alone.
- Saves carry the lowercase faction string, not the enum constructor. The
  current `units` component is schema version 1; its frozen
  `UnitInstanceDTO.uidFactionId` and live `UnitInstanceSnapshot.uisFactionId`
  are both `Text`. A profile containing multiple tags and an owner identity is
  a real wire-shape change requiring a v2 DTO, a frozen v1 decoder, and an
  explicit v1-to-v2 migration.
- Unrecognized current faction strings warn and degrade to inert `neutral`.
  This fail-safe property prevents corrupt or hand-edited data from creating
  new hostility and is worth preserving in the new model.
- The repository has no multiplayer player/controller identity model today.
  Existing “player” state is local/singular; apparent search hits for owners or
  peers belong to building ownership, saved Lua references, and autosave intent,
  not network players.
- The typed model and its focused Haskell/Lua gates shipped under #912. This arc
  supersedes its closed vocabulary while preserving its central rule: call
  named policy queries instead of comparing raw identity values at call sites.
- A readiness tracker search on 2026-08-15 found no existing composable-faction
  epic. Closed #912 is the typed scalar-policy foundation this arc replaces;
  open #916 and expedition epic #1229 concern hostile location encounters and
  consume faction behavior rather than owning this profile/diplomacy migration.
- The nomad primitive encounter design depends on this arc. It needs ordinary
  nomads, player acolytes, and future player nomads to share unit definitions
  while receiving different ownership and relation outcomes.
- The current combat AI already has unit-local incident awareness:
  `uiLastAttackerUid`/`uiLastAttackerAt` record a damaging attacker, and the
  `incoming_hit` threat source may retaliate for ten seconds. That is not a
  team-wide diplomatic state. Those fields are currently runtime-only and are
  reset during load, while the Lua `unit_ai` component persists its per-unit
  state exactly; D-20 requires deliberate persisted ownership for new incident
  and notification state rather than inheriting this split accidentally.
- The current `unitAi.commandAttack` only installs one unit's attack goal, and
  the context-menu Attack action is disabled unless `faction.canAttack` says
  the target is already attackable. There is no attack-order transaction that
  first establishes hostility, nor a combat hold/cancel path that removes it.
- The existing radio behavior in `scripts/unit_ai_notify.lua` communicates
  water-source knowledge only. It broadcasts to every radio-bearing acolyte,
  selected by unit definition rather than relationship tags; it does not carry
  combat incidents or model partial team awareness. Acolytes without radios
  already use walk-and-notify propagation, which is useful precedent but not a
  generic faction communication system.

## Desired experience

Ownership answers “who can control this unit?” Identity answers “what groups
does this unit belong to?” Relations answer “how should these two units regard
one another?” Those questions are related but not interchangeable.

The intended examples are:

| Unit | Semantic identity | Expected control/relation behavior |
|---|---|---|
| Local player's acolyte | local player controller + player team + acolyte | Player-owned and commandable; naturally hostile to nomads unless controller, team, or diplomacy supplies a higher-precedence relation. |
| Ordinary wilderness nomad | nomad, no controller or team | Not player-owned or commandable; allied with ordinary nomads; hostile to acolytes unless a higher-precedence team or diplomacy relation applies. |
| Local player's future nomad | local player controller + player team + nomad | Player-owned and commandable; allied with that player's units through controller and team even when natural affiliation would conflict. |
| Another player's acolyte | other player controller + that player's team + acolyte | Controlled by that other player; conflict with another acolyte team begins through tag diplomacy, not controller inequality alone. |
| Autonomous NPC acolyte | acolyte, no controller | Allied with other uncontrolled acolytes; not automatically allied with NPCs carrying a different faction tag. |
| Red-tribe acolyte | red-tribe team + acolyte | Friendly with red-tribe nomads because shared team outranks natural acolyte/nomad hostility. |
| AI-controlled acolyte team | AI controller + team tag + acolyte | Commanded by its AI authority; diplomacy still relates its team tag to other tags. |
| Naturally occurring acolyte | acolyte, no controller or team | Friendly with other acolytes through their shared natural affiliation; fights another acolyte only after a team/diplomacy context explicitly makes them hostile. |
| Player-controlled internal fight | local controller + shared player team + either `fight_team_A` or `fight_team_B` | The live directed relation between the temporary fight tags outranks shared controller/team defaults; removing the temporary cause can reunify them. |
| Team A attacks unaware team B | A tags are hostile toward B tags; B has no reverse hostility yet | Team A begins the ordered attack. A damaged B unit knows its attacker locally; team B becomes hostile only after that incident is successfully communicated. |

“Same controller” means the same specific command-authority identity, not merely
that both units are controlled. It supplies a friendly default, not an
unbreakable prohibition: temporary conflict tags may deliberately make subsets
of one controller's roster hostile. A future player 1, player 2, and AI
controller must remain distinguishable. This arc makes that distinction
representable and queryable; network sessions, authentication, synchronization,
lobbies, and multiplayer UI remain outside scope.

## Scope

### In scope

- A composable faction profile for every unit, replacing the single closed
  enum value.
- An optional stable controller identity capable of distinguishing human-player
  and AI command authorities, including local player and future player 1/player
  2 identities. Absence means that the unit is naturally occurring or otherwise
  has no strategic controller; there is no reserved `none` controller.
- Cultural/organizational tags including at least `acolyte`, `nomad`, and
  `wildlife`, plus compatibility representations for current neutral/debug and
  legacy hostile behavior.
- Independent policy queries for ownership, commandability, unrestricted debug
  combat, relation, alliance, and attack permission.
- Same-controller precedence: two units controlled by the same specific human
  or AI authority are friendly by default, but explicit live conflict tags may
  intentionally split one controller's units into opposing temporary teams.
- A relation model capable of making acolyte and nomad mutually hostile,
  making a shared team override that natural hostility, leaving different
  acolyte teams friendly until explicitly provoked, and accepting a future live
  tag-to-tag diplomacy overlay for multiplayer and single-player AI factions.
- Data-driven faction-tag definitions and base relations in validated YAML.
- Arbitrarily many opaque string tags per unit, including dynamically minted
  tags that do not need a predeclared catalogue row.
- Directed live tag relations, ownership-scoped mutable tag membership,
  source-scoped temporary hostility, and attack/hold/cancel integration.
- Script-owned diplomacy resolution that may clear an escalated relation while
  allowing already active combat engagements to resolve without recruiting new
  combatants.
- A boundary between unit-local threat awareness and team-wide diplomatic
  knowledge, with damage, witnessing, radio, and in-person communication able
  to promote an incident only when information actually propagates.
- Unit-definition defaults, spawn-time controller/identity assignment, runtime
  membership provenance, and explicit authored-identity reclassification.
- Runtime, save DTO/component migration, narrow Lua API, content/spawn ingress,
  and all current policy consumers.
- Exact persistence of controller/tag profiles and membership provenance, live
  relation causes, local incident knowledge, communication work in progress,
  and current-goal combat authorization after diplomacy clears.
- Strict validation, inert handling of invalid data, deterministic policy, and
  focused compatibility coverage.

### Out of scope

- Multiplayer transport, replication, authentication, matchmaking, lobbies,
  player creation, network authority, rollback, or synchronization.
- A diplomacy UI, treaties, reputation, negotiation, or broad political
  simulation. This arc does include the minimal directed runtime relation state
  and mutation boundary needed by attack, hold/cancel, damage awareness, and
  team communication.
- Nomad art, nomad AI, location encounter selection, or combat encounter
  behavior; those remain in the nomad encounter design and depend on this arc.
- Replacing the unit-definition system or making arbitrary gameplay tags serve
  as faction tags without basic string validation. Dynamic relationship tags
  may be minted without a static catalogue row, but unrelated gameplay tags
  do not enter faction policy accidentally.
- Inferring ownership or relation from unit definition names at runtime.

## Design

### Separate identity from control

The system needs two independent kinds of information even if both are exposed
through one faction-profile API:

- **Controller identity:** which specific human or AI authority, if any, owns
  strategic command of the unit. Equality supplies the same-controller safety
  rule. This value is dynamic, optional, and cannot be a fixed enum if future
  player, AI, and multiplayer identities are to be distinguishable. `Nothing`
  means no strategic controller; it does not prevent ordinary unit AI from
  running. A fabricated `none` identity would incorrectly make every
  naturally occurring unit share a controller.
- **Relationship tags:** opaque, non-empty validated string IDs used by the
  relation system. Natural affiliations such as `acolyte`, organizations such
  as `red_tribe`, session teams, and temporary conflict groups such as
  `fight_team_A` all use the same tag type. A profile may hold any number of
  them. Static YAML may document and relate known tags, while runtime systems
  may mint arbitrary new IDs without editing the catalogue first.

Per D-5, represent this as a structured `FactionProfile` rather than a bare set
of unrelated strings:

```text
FactionProfile
  controller: optional ControllerId
  tags:       mutable validated set of opaque FactionTag strings
  capabilities: validated set of exceptional policy flags, if needed
```

This preserves the tag-system goal while making the difference between
“controlled by player 1” and “belongs to the nomad culture” explicit. A
namespaced all-tags alternative (`player:1`, `nomad`) was rejected because the
engine would still have to parse the controller namespace as structured data
and prevent that string convention from leaking into call sites.

### Directed relation precedence

The controller and tag profiles are both passed into one relation query, but
they have different jobs. Controller equality supplies a friendly default;
controllers are not keys in the diplomacy table. Diplomacy relates arbitrary
tags, so the same machinery covers human players, AI-controlled teams,
naturally occurring groups, temporary internal conflicts, and multiplayer
setups. Relations are directional: `relation(A, B)` need not equal
`relation(B, A)`. The approved policy order is:

1. An explicit debug/unrestricted-combat request may bypass normal attack
   permission for developer tooling without mutating diplomacy or alerting a
   team unless the debug request explicitly asks for those effects (D-24).
2. If the live overlay defines any applicable directed relation from one of the
   subject's tags to one of the target's tags, reduce every matching active
   cause by `hostile > neutral > ally` (D-23). This tier intentionally outranks
   controller equality and shared tags, allowing
   `fight_team_A -> fight_team_B = hostile` within one player's roster. A live
   neutral cause suppresses a live ally cause when no hostile cause exists;
   insertion order never affects the answer.
3. If both profiles have the same non-null controller, relation is `ally`.
4. If the profiles share any relationship tag, relation is `ally`.
5. Otherwise, evaluate their explicit directed base tag-pair relation from
   YAML. A symmetric YAML shorthand may expand to both directions.
6. Otherwise, the relation is `neutral`. Malformed tags and invalid data also degrade
   inertly and can never create control, alliance, or hostility by accident.

Consequently, a red-tribe acolyte and red-tribe nomad are allies through their
shared tag; two different acolyte teams are allies through `acolyte` until a
live directed relation overrides it; and two subsets of one player's shared
team can fight through temporary conflict tags. Controller inequality never
creates hostility on its own.

### Tag definitions and relations

Per D-7, put known tag documentation and base relations in validated YAML
rather than adding Haskell constructors for each new culture. Base relations
are directional, with an explicit symmetric shorthand for common mutual
hostility. A possible authoring shape is:

```yaml
faction_tags:
  - id: acolyte
  - id: nomad
  - id: wildlife
  - id: red_tribe

relations:
  - { pair: [acolyte, nomad], relation: hostile }
```

The exact filename and schema remain open. The loader must reject duplicate or
malformed static tag declarations, contradictory pair declarations, invalid
relation values, and ambiguous mixtures of directed and symmetric declarations.
Same-tag alliance and the neutral default must be explicit engine contracts,
never accidental map fallbacks. The engine does not need to classify a tag as
`team` or `affiliation`; the precedence follows relation source—live override,
shared membership, then YAML base relation—rather than tag kind.

YAML owns everything that can be static: known tag descriptions, base pair
relations, and unit-definition defaults. Controller identifiers, arbitrary
runtime-minted tag strings, tag membership changes, and current directed
diplomacy entries are runtime/save identity and state. The subsystem that
creates multiplayer users, spawned AI teams, or temporary fights chooses those
tag names; faction policy only validates a safe string representation and
evaluates the tags it receives.

### Unit-definition and spawn ownership

Culture is normally inherent to a unit definition while control is assigned by
the spawn source. Requiring every portal, location, wildlife, and debug caller
to repeat both facts is error-prone.

**Approved direction (D-22):** Unit YAML supplies authored default tags, while
a spawn source supplies the optional controller and any additional tags its
scenario needs. Runtime systems may subsequently add temporary tags through
owner-scoped mutations. Each mutable membership records its provenance/owner;
an ordinary system may remove only a membership it added. Removing or replacing
a YAML-authored default requires an explicit conversion/reclassification path,
so generic cleanup cannot silently erase authored identity:

```yaml
units:
  - name: acolyte
    faction_tags: [acolyte]
```

- Portal/player roster spawn adds the local controller.
- A nomad location spawn uses the nomad definition's `[nomad]` default and no
  controller.
- A future player-nomad spawn uses the same nomad definition and adds that
  player's controller and current team tags.
- A red-tribe encounter adds `red_tribe` to both acolytes and nomads, making
  them allies without changing either unit definition's natural affiliation.
- A multiplayer player can spawn an AI-controlled acolyte team by assigning an
  AI controller and that spawned team's relationship tag; the human player's
  own controller identity need not masquerade as its diplomacy identity.
- Wildlife definitions carry `[wildlife]`; an omitted controller remains
  non-player-owned.
- Debug tooling applies an explicit debug capability/override rather than
  pretending debug is a culture.

The faction layer does not generate multiplayer numbering or temporary fight
names. It receives those IDs from the owning system and preserves both the
membership and its ownership/provenance through runtime and save contracts.
The exact API names remain an implementation detail, but the boundary should
support narrow operations equivalent to “add an owner-scoped runtime tag,”
“remove this owner's runtime tag,” and “explicitly reclassify authored tags,”
not unrestricted replacement of the whole tag set.

### Runtime API

Policy consumers should continue asking named questions rather than inspecting
sets directly:

```text
isPlayerOwned(localController, profile)
isPlayerCommandable(localController, profile)
hasUnrestrictedCombat(profile)
relationFromTo(policyContext, subjectProfile, targetProfile)
isFriendlyToward(policyContext, subjectProfile, targetProfile)
canAttackNow(policyContext, subjectProfile, targetProfile, localAwareness)
canInitiateAttackOrder(orderingController, subjectProfile, targetProfile)
addRuntimeTag(owner, unit, tag)
removeRuntimeTag(owner, unit, tag)
reclassifyAuthoredTags(unit, conversion, replacementTags)
resolveDiplomaticHostility(tagA, tagB, resolutionCause)
```

The tag-mutation names above are illustrative rather than a frozen API, but
their ownership boundary is part of the design contract.

The local-controller argument matters: in multiplayer, “player-owned” is
observer-relative. A unit controlled by player 2 is player-owned from player
2's client/session perspective but not commandable by player 1. Until
multiplayer exists, the engine can provide one stable local controller ID while
preserving this API shape.

Lua receives narrow unit-based policy queries and explicit controller/tag
getters plus owner-scoped tag and relation-cause mutations (D-25), not a
comma-delimited pseudo-tag string or a whole-profile replacement operation. A
structured profile may be returned as a read-only snapshot for inspection.
`unit.getFaction` may need a compatibility window while the narrow profile,
tag, and controller queries land. The pure `faction` table can continue owning
policy, but its arguments and fixtures must move from scalar strings to the
approved profile representation; Lua scripts never reimplement precedence.

`canAttackNow` and `canInitiateAttackOrder` are deliberately different. The
current context menu asks whether the relation is already hostile and therefore
cannot start a player-directed conflict. The replacement order transaction may
authorize a commandable unit to initiate a legal conflict, install the outgoing
directed relation first, and only then enqueue the attack. Ordinary autonomous
target selection continues to require current hostility or local incident
awareness; it must not invent a diplomatic change merely because a target is
visible.

### Mutable tags and cause-scoped live relations

Per D-11 and D-12, a unit may hold any number of relationship tags, and runtime
systems may mint and add arbitrary valid string IDs. Per D-22, each membership
has an owner/provenance: a runtime system removes only its own memberships, and
authored defaults change only through explicit reclassification. The faction
layer does not generate multiplayer numbering or temporary fight names. It
receives those IDs from the owning system and preserves the IDs and provenance
through its runtime and save contracts.

Live relations are directional and need provenance. An attack order from team
A to team B receives explicit source and target tags and adds an outgoing
hostile cause for exactly that `A -> B` pair; it does not automatically add
`B -> A`. Any matching active hostile cause wins until removed. The design must
not represent this as one naked mutable boolean, because canceling one order
must not erase hostility independently created by another active order, a
communicated attack, or a later diplomacy decision.

Hold/Cancel is conditional. Before B has established reverse team hostility,
canceling A's order removes A's order-owned cause. Once a communicated incident
has installed `B -> A: hostile`, the conflict is escalated: Hold/Cancel removes
neither direction. Ending an escalated conflict requires a separate resolution
script call owned by the diplomacy system rather than pretending B instantly
knows A changed its mind.

The diplomacy script clears the explicitly named hostile causes in both
directions. That clear prevents new autonomous engagements and new units from
joining. A unit already executing an attack goal retains a scoped authorization
to finish only that goal against its assigned target. It may not switch targets
or restart the attack after the target dies or escapes, the attacker retreats,
or the goal otherwise completes (D-21).

The temporary-tag example is therefore valid even under one controller:

```text
attacker tags = {player_team, acolyte, fight_team_A}
target tags   = {player_team, acolyte, fight_team_B}
live relation = fight_team_A -> fight_team_B: hostile (cause: attack order)
```

The live directed rule outranks the controller and shared `player_team`/
`acolyte` defaults. If B never establishes reverse hostility, canceling the
order may remove the order-owned relation and temporary fight tags, returning
the units to the remaining shared-tag/controller result. Once reverse hostility
exists, those conflict identities remain until the separate resolution path
settles the escalated fight.

### Local awareness and team knowledge

Faction hostility and knowledge of an incident are separate. A target team does
not become globally hostile merely because one of its units was attacked where
no ally could know about it.

**Approved direction (D-13, D-18):** Preserve three stages:

1. **Local awareness:** A unit that takes damage knows its attacker immediately
   and may defend itself through a local threat override even while its team's
   directed tag relation remains friendly or neutral. A witness may similarly
   learn an incident when a later perception system proves it saw the attack or
   death.
2. **Communication in progress:** The informed unit carries an incident and
   attempts to notify the relevant team. A radio can broadcast; without one,
   in-person propagation may reuse the existing walk-notify pattern. Killing a
   victim before it reports prevents that victim from informing anyone; another
   unit must witness and communicate the event.
3. **Team knowledge:** Only successful communication installs the reverse
   directed team relation, such as `fight_team_B -> fight_team_A: hostile`, for
   the explicit informed scope. This is the moment at which uninvolved team
   members may autonomously recognize the attackers as hostile, B counts as
   triggered, and A's ordinary Hold/Cancel can no longer unwind either
   direction. A victim's short-term local retaliation before communication does
   not cross that threshold.

The faction arc owns the directed relation and mutation API. FTS-6A integrates
the minimal damage/witness/communication path needed to prove that awareness is
not omniscient. The order and report caller passes explicit source and target
tags; faction policy never guesses among the units' memberships. It should
generalize the radio recipient scope from “every acolyte” to the explicit
relationship tag carried by the report without turning all future knowledge
types into faction concerns. Water-source knowledge and
the broader reusable intelligence system remain separate consumers.

### Persistence and migration

The `units` component must advance from v1 to v2. Its frozen v1 DTO retains one
`uidFactionId :: Text`; v2 stores the approved profile shape. Migration must be
total for all five old tags and must not rely on current mutable relation data
to decide what an old save meant.

The unit snapshot owns controller identity and mutable tag membership together
with membership ownership/provenance. Directed live relations and their causes
are shared state and must not be duplicated into every member unit; they need
one authoritative persisted relation component or manager. Per D-20,
unit-local incident knowledge and communication-in-progress state also receive
explicit persisted owners. The current split—last-attacker
fields reset on load while Lua AI state persists exactly—must change where
necessary; it is evidence, not permission to lose the new state.

**Approved legacy meanings (D-26):**

| v1 tag | v2 migration intent |
|---|---|
| `player` | Local controller plus the loaded unit definition's default affiliation tags. |
| `wildlife` | No controller plus the definition's default tags, falling back to `wildlife` for legacy definitions without a declaration. |
| `hostile` | No controller plus `legacy_hostile`; data-authored compatibility relations reproduce the old hostile behavior. |
| `neutral` | No controller and an inert neutral profile with no ordinary faction identity. |
| `debug` | No ordinary controller or diplomacy identity; explicit debug capabilities preserve local commandability and unrestricted combat without making the unit player-owned. |

The migration needs a definition-aware adapter if v1 `player` acolytes and
technomules are to acquire different default tags from their definitions. If
component decoding cannot consult definitions directly, the translation can
retain a legacy marker until the load stage resolves the live profile; it must
not guess from unit names inside the frozen DTO decoder.

Unknown v1 strings should continue producing one warning per distinct value and
degrade to an inert profile because v1 had a closed vocabulary. In v2, an
unregistered but syntactically valid tag is legitimate; malformed tag strings
from corrupt data must be rejected or dropped without granting control or
creating hostility. Compatibility coverage must include decode-old/save-new and
fresh-process load, not merely pure codec round trips.

### Compatibility behavior to preserve

- Current player units remain locally owned and commandable after migration.
- Debug tools retain their deliberate ability to stage otherwise forbidden
  fights without making debug units discover locations as player-owned units,
  changing diplomacy, or alerting teams unless the debug request explicitly
  asks for those side effects.
- Wildlife remains mutually allied until a separate predation design changes
  it.
- Current neutral behavior remains inert.
- Existing hostile fixtures and location probes do not silently become allied
  or player-owned during the compatibility window.
- Location discovery remains ownership-based, not alliance-based.
- Transfer eligibility remains commandability-based, not alliance-based.
- Medic and group-effectiveness logic use alliance, not controller equality or
  “not the enemy” shortcuts.
- Context-menu attack permission and autonomous threat detection share the
  authoritative relation/permission policy.

### Documentation impact

- `CLAUDE.md` must replace the current single-faction contract with the stable
  profile/query/precedence rules once implemented.
- `docs/persistence_state_inventory.md` must record the changed persisted unit
  identity shape and component owner; the `units` v2 migration follows
  `docs/persistence_contract.md`.
- `docs/engineenv_capability_inventory.md` changes only if a live tag registry
  or tag-diplomacy store adds or changes `EngineEnv` state. The preferred
  design should first consider an existing manager rather than assuming a new
  field; the directed relation store is mutable and cannot be treated as only
  immutable loaded policy.
- Global YAML/authoring documentation must name the validated tag vocabulary,
  defaults, relation table, and spawn overlay rules.
- `tools/README.md` must describe any new behavior probe or materially expanded
  persistence/combat probe.
- `docs/nomad_primitive_encounter_design.md` will replace its open faction
  prerequisite with a link to this arc after the profile and precedence
  decisions are approved.
- Per the repository's manual documentation workflow, publication uses
  `tools/docs_land.sh` to land only approved docs directly on `origin/master`.

## Decisions

### D-1. Unit faction identity is composable

A unit may carry player ownership/control and a cultural identity at the same
time. Player acolytes, ordinary nomads, and player-controlled nomads must not
require duplicate unit definitions merely to express those combinations.

### D-2. Same-controller authority supplies a friendly default

Two units controlled by the same specific human or AI authority are friendly
unless a higher-precedence explicit live tag relation divides them. Shared
control normally overrides YAML base hostility, but it is not an absolute
friendly-fire prohibition: temporary `fight_team_A`/`fight_team_B` tags may
allow one controller's units to fight when the player deliberately orders it.
This revises the earlier absolute same-controller rule after the owner supplied
the internal-conflict requirement.

### D-3. Different players remain distinguishable

“Player” is not one universal alliance shared by all human participants.
Future player 1 and player 2 controller identities remain distinct, but
controller inequality does not itself create hostility. Their units conflict
when their relationship/team tags have an explicit hostile diplomacy entry.
This arc makes both values available to policy without implementing multiplayer
networking.

### D-4. Acolyte and nomad are mutually hostile affiliations

When no higher-precedence live relation, same-controller default, or shared tag
applies, an acolyte and a nomad regard each other as hostile in both directions
because their YAML base relation uses the symmetric shorthand. Ordinary nomads
therefore threaten player acolytes, while a player-controlled or same-team
nomad remains safe unless a more specific live conflict divides them.

### D-5. Controller identity is a separate optional field

`FactionProfile` stores `controller: Maybe ControllerId` separately from its
validated faction tags. A unit with no controller is naturally occurring or
otherwise lacks a strategic command authority. The system must not create a
reserved `none` controller, because equal controller IDs trigger the
same-controller alliance rule and would therefore make all uncontrolled NPCs
allies.

Controller identity is dynamic runtime/save data. Faction tags may come from
authored defaults or runtime systems. The rejected alternative encoded
controller IDs as namespaced faction tags, which would mix command authority
with relationship identity and still require special parsing for every
ownership query.

### D-6. Diplomacy relates tags, not controllers

Relation evaluation receives both profiles, but the live diplomacy table is
keyed by relationship tags rather than controller IDs. Controllers determine
command authority and same-controller default friendliness. Tags determine
inter-group relations, allowing one policy to cover player teams, AI teams,
naturally occurring factions, temporary conflicts, and multiplayer
arrangements.

Different controllers are not automatically hostile. A player-controlled team
and an AI-controlled team fight only when their relevant tags have a hostile
relation. This replaces the earlier controller-to-controller diplomacy
proposal, which could not naturally express uncontrolled factions or a player
spawning a separately AI-controlled team.

### D-7. Faction vocabulary and base policy are data-driven

Everything static that can reasonably be authored in data belongs in YAML:
known tag descriptions, base pair relations, and unit-definition defaults. New
factions such as `nomad` must not require a new Haskell constructor. The static
catalogue is not a closed vocabulary: runtime systems may mint arbitrary valid
tag strings. Runtime controller IDs, tag membership, relation causes, and
current diplomacy state are saved/session state rather than static YAML.

### D-8. Unrelated tags default neutral and hostility is explicit

If two valid profiles share no tag and neither the live overlay nor YAML
declares an applicable directed relation, they are neutral. Hostility never
arises from an omitted pair. An unregistered but syntactically valid runtime tag
is legitimate and inert until shared or related; malformed data remains inert.
Content is expected to declare meaningful base hostilities explicitly in YAML;
in particular, `acolyte` versus `nomad` is a symmetric hostile declaration.

### D-9. Shared tags override base relations but not live conflict

Units sharing any relationship tag are allied before YAML base relations are
consulted: a `red_tribe` acolyte and `red_tribe` nomad are friendly. An explicit
live directed relation has still higher precedence, so `fight_team_A ->
fight_team_B` can make two units hostile despite their shared controller,
`player_team`, and `acolyte` identities. Removing that live conflict restores
the shared-tag result.

Broad descriptive properties such as `human` are not automatically faction
memberships. If they are needed later, they belong in a separate gameplay-tag
namespace unless deliberately declared as relationship-bearing affiliations.

### D-10. Controllers may represent human or AI command authorities

`ControllerId` is an opaque command-authority identity, not a synonym for human
player. A unit may be controlled by the local player, another multiplayer
player, or an in-game AI authority; naturally occurring units may have no
controller. This supports a human player spawning a separately AI-controlled
acolyte team while keeping that team's diplomacy entirely tag-driven.

### D-11. Units may carry any number of relationship tags

There is no one-team limit. A unit may simultaneously carry authored faction
identity, permanent organization membership, session teams, coalitions, and
temporary conflict tags. Multiple membership is the point of the system, not an
exception to validate away. Relation policy must therefore reduce all
applicable directed tag-pair rules deterministically rather than selecting one
primary team.

### D-12. Runtime systems may mint arbitrary tag strings

Faction policy accepts any syntactically valid opaque string tag. Static YAML
defines known defaults and base relations, but it is not an exhaustive registry
that multiplayer, AI-team, or scenario systems must edit. Those owning systems
choose dynamic names and numbering; the faction layer stores and evaluates the
IDs it is given. Malformed strings are rejected or degraded inertly, while an
unknown but valid string is allowed.

### D-13. Live hostility is directional and knowledge-gated

An attack order from team A toward team B creates hostility from A to B only.
Team B does not become globally hostile toward A until information reaches it.
A damaged B unit has local knowledge of its attacker; a killed B unit cannot
report, so a surviving witness must perceive the incident and communicate it.
Radio or in-person propagation can eventually establish the reverse B-to-A
team relation. Relations and notification must not assume symmetry or
omniscient faction knowledge.

### D-14. Attack intent owns a reversible hostility cause

Issuing an attack order must establish the attacker's outgoing hostility before
the attack begins. Before the target team establishes reverse hostility, a hold
or cancel order may remove the hostility cause created by that order, returning
relation evaluation to any remaining live causes, shared tags/controllers, or
YAML base relation. Once reverse team hostility exists, D-17 prevents ordinary
Hold/Cancel from clearing either direction. Cause-scoped mutation remains
necessary so unrelated causes are never erased.

### D-15. Any active hostile cause wins

Every applicable live directed relation retains its causes. If any matching
cause is hostile, the effective subject-to-target relation is hostile until
that particular cause is removed. A shared controller, shared tag, live ally,
or live neutral entry cannot mask an active hostile cause. A ceasefire or other
resolution removes/suspends hostile causes rather than layering a contradictory
friendly value over them.

### D-16. Relation mutations name their tags explicitly

Attack orders, reports, and diplomacy mutations must pass the exact source and
target tag IDs whose directed relation they affect. Faction policy never guesses
a primary tag from a profile with many memberships. A temporary internal fight
therefore explicitly names `fight_team_A -> fight_team_B`; a reverse report
explicitly names `fight_team_B -> fight_team_A`.

### D-17. Reverse hostility locks an initiated fight

If A cancels before B establishes reverse team hostility, A's order-owned
hostility is removed. If B has already become hostile toward A, ordinary
Hold/Cancel clears neither A-to-B nor B-to-A hostility. At that point the fight
has escalated into a mutual conflict and needs a distinct ceasefire or
resolution mechanism. This prevents A from becoming friendly while an informed
B continues attacking and prevents cancellation from silently rewriting B's
knowledge.

### D-18. Team-wide reverse relation is the escalation threshold

Team B counts as triggered only when successful communication installs the
reverse team-wide `B -> A` hostile relation. Before that, an individual victim
still defends itself through the existing short-term last-attacker behavior,
but its local reaction does not lock the broader conflict. This preserves the
current rule that units protect themselves against a recent aggressor while
keeping faction knowledge non-omniscient.

### D-19. Diplomacy scripts clear hostility without interrupting active combat

An escalated conflict ends only when the diplomacy system explicitly scripts
the removal of the hostile causes for the named tag pair; Hold/Cancel and time
alone do not end it. Clearing diplomacy prevents new engagements, but units
already fighting are allowed to resolve their existing combats before the
peace applies to them. The implementation therefore needs a scoped active-
engagement authorization rather than rechecking the cleared team relation and
aborting combat immediately. D-21 defines exactly when that authorization
expires.

### D-20. All tag and diplomacy state persists

Save/load must preserve controller/tag profiles, mutable and temporary tag
membership with its ownership/provenance, every live directed relation and
cause, the escalation state, unit-local incident knowledge, and radio/in-person
communication already in
progress, including any grandfathered authorization for a combat that was
already active when diplomacy cleared. Loading cannot erase or invent
hostility, reset who knows about an incident, prematurely inform a team, cancel
a pending report, restart a cleared war, or terminate an active conflict merely
because some of the state is short-lived.

### D-21. Cleared diplomacy grandfathers only the current attack goal

When diplomacy becomes peaceful during combat, each already executing attack
goal may finish against its currently assigned target. Its grandfathered
authorization ends when that target dies or escapes, the attacker retreats, or
the goal otherwise completes. It cannot select a replacement target, restart
the completed attack, or authorize another unit to join. This is narrower than
grandfathering an encounter and less disruptive than aborting mid-action.

### D-22. Tag mutation is ownership-scoped

Every mutable tag membership records where it came from. An ordinary runtime
system may add tags under its own owner identity and remove only memberships it
owns; it cannot remove a YAML-authored default or another system's membership.
Changing authored identity requires an explicit conversion/reclassification
operation whose purpose is visible at the call site. This supports temporary
multi-tag composition without letting cleanup, orders, or scenario scripts
silently rewrite a unit definition's natural affiliation.

### D-23. Live relation causes reduce by hostile, then neutral, then ally

For all applicable live causes, the deterministic severity order is
`hostile > neutral > ally`. Any hostile cause wins. If none is hostile, a
neutral cause wins over ally; ally applies only when every applicable live
cause is ally. This conservative reduction prevents uncertainty or a ceasefire
constraint from accidentally granting alliance-only behavior such as medical
support.

### D-24. Explicit debug combat bypass is isolated from diplomacy

An explicit developer/debug combat action may bypass controller, alliance, and
ordinary attack-order restrictions, including between units sharing one
controller. That bypass authorizes the requested staged combat only. It does
not add or remove diplomatic relation causes, create team-wide hostility, or
alert incident-communication systems unless the debug request explicitly asks
to exercise those effects. Debug commandability and unrestricted combat remain
capabilities, not player ownership or a normal faction identity.

### D-25. Lua receives narrow faction operations, never profile replacement

Lua may query a unit's controller and tags, obtain a structured read-only
profile snapshot, ask the authoritative relation/permission questions, add or
remove owner-scoped runtime tags, and add or remove explicitly scoped relation
causes. It may not replace an entire faction profile or reproduce precedence in
script. Controller changes and authored-tag reclassification use dedicated
authoritative operations rather than a generic profile setter.

### D-26. Legacy faction migration has one exact compatibility mapping

The v1 values migrate as follows: `player` becomes the local controller plus
the loaded definition's default tags; `wildlife` becomes no controller plus
definition defaults, falling back to `wildlife`; `hostile` becomes no
controller plus `legacy_hostile` with compatibility relations preserving old
hostility; `neutral` becomes no controller plus an inert neutral profile; and
`debug` becomes no normal controller or diplomacy identity plus explicit local
commandability and unrestricted-combat capabilities. Unknown v1 values still
warn and degrade inertly. Migration does not infer a new culture from a unit
name or current diplomacy.

## Open questions

### Q-1. Is controller identity a separate field or a namespaced tag?

Resolved by D-5. Controller identity is a separate optional field. Absence
means autonomous NPC; it is not represented by a sentinel controller or a
namespaced faction tag.

### Q-2. What outranks what for two different players?

Resolved by D-2, D-3, D-6, and D-9. Different controllers do not supply a
relation. A live directed tag relation wins first; same-controller and shared
tags then supply friendly defaults before YAML base relations. Thus
different-player acolytes remain friendly until their tags become hostile, and
same-player units can still be split by a more specific live conflict.

### Q-3. Are tag definitions and affiliation relations data-driven?

Resolved by D-7. Static tag definitions, same-tag behavior, base relations, and
unit defaults are validated YAML. Runtime identities and mutable diplomacy
state remain runtime/save data.

### Q-4. Which identity comes from the unit definition versus the spawn source?

Resolved by D-22. Unit YAML supplies authored defaults; spawn and runtime
systems may add scenario/team memberships under explicit owners. Ordinary
systems may remove only memberships they own. Replacing YAML-authored identity
requires an explicit conversion/reclassification path.

### Q-5. What is the default relation between unrelated valid tags?

Resolved by D-8. Unrelated valid tags default neutral. Content is expected to
declare meaningful hostility explicitly in YAML; missing or invalid data never
creates hostility.

### Q-6. Does debug unrestricted combat bypass the same-controller protection?

Resolved by D-24. An explicit debug action may bypass same-controller and all
other normal relation restrictions, but the bypass does not mutate diplomacy
or alert teams unless the request explicitly asks for those side effects.

### Q-7. Are faction profiles immutable after spawn?

Resolved by D-11, D-14, and D-22. Tag membership is mutable after spawn so
temporary teams can be added and removed. Controller changes remain less common
but the v2 profile representation must not make them impossible. Mutation goes
through authoritative owner-scoped APIs/events rather than direct set edits at
call sites.

### Q-8. What replaces the scalar Lua API?

Resolved by D-25. Lua receives narrow unit-based policy calls, explicit
tag/controller getters, owner-scoped tag and relation-cause mutations, and at
most a structured read-only profile snapshot. It cannot replace a whole profile
or implement relation precedence itself.

### Q-9. How are the five legacy faction values mapped exactly?

Resolved by D-26. `player`, `wildlife`, `hostile`, `neutral`, and `debug` use
the exact migration table in the persistence section; unknown v1 values warn
and degrade inertly.

### Q-10. What identities can the future diplomacy overlay relate?

Resolved by D-6 and D-12. Diplomacy relates syntactically valid relationship
tag strings and never needs a controller-to-controller or controller-to-faction
table. Both profiles, including their controllers and tags, are passed to
relation policy so the same-controller default and tag diplomacy can be
evaluated together.

### Q-11. How do multiple faction tags combine?

Resolved by D-9 and D-11 for membership. A unit may have any number of tags; an
applicable live directed relation overrides same-controller/shared-tag defaults,
and shared membership otherwise overrides YAML base hostility. D-15 and D-23
resolve several simultaneous live matches by the deterministic
`hostile > neutral > ally` order.
Broad traits such as `human` remain outside the faction namespace unless they
are deliberately intended to affect relationships.

### Q-12. May one unit belong to more than one team?

Resolved by D-11. Units may carry any number of relationship tags, including
multiple permanent or temporary teams. The system must resolve ambiguity rather
than forbidding the composition.

### Q-13. May sessions create team tags dynamically?

Resolved by D-12. Runtime systems may create arbitrary syntactically valid tag
strings. Multiplayer owns user/team numbering, AI/scenario systems own their
names, and faction policy neither generates nor requires pre-registration of
those IDs.

### Q-14. What exactly changes when an attack order starts hostility?

Resolved by D-13 and D-14. An attack order creates a reversible outgoing
hostility cause from the attacking tags toward the target tags. It does not
create reverse hostility. Damage or witnessing creates local knowledge; only
successful communication promotes that incident into team-wide reverse
hostility. Hold/Cancel removes the attack-order-owned outgoing cause only while
reverse team hostility has not yet been established; D-17 governs escalation.

### Q-15. How are multiple simultaneous live relations reduced?

Resolved by D-15 and D-23. Every directed entry retains its causes, and all
applicable causes reduce by `hostile > neutral > ally`. Ceasefire does not hide
hostility under a contradictory ally value; it removes or suspends the hostile
causes.

### Q-16. Which tags does an order or report use as its diplomatic scope?

Resolved by D-16. The order/report/diplomacy caller always supplies the exact
source and target tags. Faction policy does not infer a primary team or choose
among memberships.

### Q-17. Which temporary state survives save/load?

Resolved by D-20, D-21, and D-22. All tag and diplomacy state persists,
including mutable membership ownership/provenance, relation causes, unit-local
incident knowledge, and communication in progress, plus the exact current-goal
authorization grandfathered after diplomatic hostility clears.

### Q-18. Does canceling the attack also calm the informed defender?

Resolved by D-17. If B has not established reverse hostility, A's Hold/Cancel
clears A's order-owned cause. If B is already hostile, Hold/Cancel clears
neither direction; a separate conflict-resolution mechanism is required.

### Q-19. What exact event counts as B becoming hostile?

Resolved by D-18. B is triggered only when communication installs its reverse
team-wide tag relation. A damaged victim defends itself immediately through the
existing short-term local retaliation behavior without tripping the team-wide
escalation latch.

### Q-20. What ends an escalated mutual conflict?

Resolved by D-19. Only an explicit diplomacy-system script clears the hostile
causes for the named relationship. Hold/Cancel and timers do not. If the script
runs during combat, existing engagements resolve while no new combat begins
from the cleared diplomatic hostility.

### Q-21. How do live neutral and ally causes combine without hostility?

Resolved by D-23. Live causes reduce by `hostile > neutral > ally`; if no
hostile cause exists, neutral wins over ally.

### Q-22. When has an existing combat “resolved” after diplomacy clears?

Resolved by D-21. Grandfathered authorization lasts only through the already
executing attack goal against its assigned target and ends when that target
dies or escapes, the attacker retreats, or the goal otherwise completes. It
does not permit target switching, restarting, or recruiting another combatant.

## Verification strategy

- Add pure exhaustive policy coverage over controller equality/difference,
  human/AI/no-controller cases, arbitrarily many overlapping tags,
  same-controller/shared-tag defaults, higher-precedence live directed rules,
  the `hostile > neutral > ally` live-cause reduction, neutral fallback, debug
  capability, malformed versus unknown-valid tags, and empty profiles.
  Directionality is asserted explicitly: A-to-B and B-to-A are independent
  unless YAML symmetric shorthand creates both.
- Freeze a v1 `units` DTO fixture and prove its v2 migration for every legacy
  faction value against D-26's exact map, multiple unit definitions, unknown
  legacy tags, and warning deduplication. Re-encoding must emit only v2.
- Run fresh-process save/load coverage with player acolyte, wildlife, neutral,
  debug, legacy-hostile, NPC nomad, and player-controlled nomad profiles.
- Port the focused Haskell `Unit.Faction` and Lua faction-model suites rather
  than leaving parallel scalar and profile authorities.
- Prove Lua can inspect controller/tags, call authoritative policy, and mutate
  only owner-scoped memberships/causes, while no whole-profile replacement API
  or script-side precedence implementation remains available.
- Preserve discovery gates proving player ownership is not alliance; transfer
  gates proving commandability is not alliance; medic and group-effectiveness
  gates proving alliance is not “not the enemy”; and context-menu/combat gates
  proving attack permission uses the shared policy.
- Add spawn/YAML validation for duplicate memberships, malformed tag strings,
  arbitrary unknown-valid strings, invalid controller encodings, conflicting
  directed/symmetric declarations, omitted defaults, and inert failure
  behavior.
- Add tag-ownership coverage proving a runtime owner can remove its own
  membership but cannot remove a YAML-authored default or another owner's tag;
  only the explicit conversion/reclassification path can change authored
  identity. Ownership/provenance must round-trip through save/load.
- Add a compatibility probe that spawns the same unit definition as NPC and as
  local-player-controlled, demonstrating identity reuse while ownership and
  relation outcomes differ.
- Add relation matrices covering red-tribe acolyte/nomad alliance, distinct-team
  acolyte alliance before diplomacy, A-to-B hostility without B-to-A hostility,
  same-controller temporary fight tags overriding shared memberships, and
  natural acolyte/nomad hostility without a higher-precedence live/shared rule.
- Add debug staging coverage proving explicit debug combat can bypass a shared
  controller and ordinary alliance while leaving diplomacy, team hostility,
  incident alerts, player ownership, and discovery unchanged unless those side
  effects are explicitly requested.
- Add order lifecycle coverage proving Attack installs its outgoing cause before
  combat starts; Hold/Cancel removes it while B remains untriggered; reverse
  B-to-A hostility locks both directions against ordinary cancellation; and
  another simultaneous cause prevents premature peace.
- Add awareness/communication coverage proving a damaged unit may retaliate
  locally, an unwitnessed kill does not inform the team, a surviving witness
  may report, non-radio propagation is not instantaneous, and a completed radio
  report installs the reverse directed team relation exactly once. Local
  retaliation before that report does not trip the team-wide escalation latch.
- Add diplomacy-resolution coverage proving only the scripted resolution clears
  an escalated tag pair; clearing it blocks new engagements; already active
  combats receive authorization only for each currently assigned attack goal;
  target death/escape, attacker retreat, or goal completion ends it; and the
  unit cannot retarget, restart, or recruit another combatant under the cleared
  relation.
- Add save/load coverage for a pre-alert attack, local retaliation, radio/walk
  report in progress, escalated two-way hostility, diplomacy cleared during an
  active combat, and every temporary tag/relation cause involved. Each restored
  run must make the same next decision as the uninterrupted run.
- Run the persistence inventory and save-compatibility audits whenever the
  units component, inventory documentation, or frozen/current DTOs change.

## Delivery plan

Q-1 through Q-22 are resolved. The slices preserve the current scalar model
until a tested compatibility adapter exists, then migrate consumers before
removing it.

### FTS-1. Add the pure faction identity and relation policy model

- **Outcome:** A standalone typed profile and policy can express controller
  identity, arbitrarily many relationship tags, directed live overrides,
  same-controller/shared-tag defaults, neutral fallback, and current
  compatibility behavior without changing live units yet.
- **Scope:** Profile/tag/controller types, pure property/relation queries,
  precedence, inert invalid profile, exhaustive unit tests.
- **Phase:** Policy foundation
- **Depends on:** `none`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2, D-3, D-4, D-5, D-6, D-8, D-9, D-10,
  D-11, D-12, D-13, D-14, D-15, D-16, D-17, D-18, D-21, D-23,
  D-24, D-26
- **Acceptance signals:** Every approved example and legacy behavior has one
  deterministic answer; no runtime field or save bytes change.
- **Out of scope:** YAML, live unit migration, Lua, nomad encounter behavior.
- **Open questions:** None

### FTS-2. Add validated faction-tag definitions and legacy mappings

- **Outcome:** Known tags, base relations, unit defaults, and compatibility
  identities load through one validated authority without closing the runtime
  string namespace.
- **Scope:** Catalogue/schema if data-driven, unit-YAML tag defaults, duplicate
  and relation validation, legacy mapping definitions, loader tests.
- **Phase:** Authoring
- **Depends on:** FTS-1
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-4, D-7, D-8, D-9, D-11, D-12, D-13,
  D-22, D-26
- **Acceptance signals:** `acolyte`, `nomad`, and current compatibility tags
  resolve; malformed strings fail/degrade while unknown-valid runtime strings
  remain usable exactly as approved; the five v1 identities have D-26's exact
  deterministic migration meanings.
- **Out of scope:** Live unit/save migration and policy consumer changes.
- **Open questions:** None

### FTS-3. Migrate runtime units, spawning, and save data to faction profiles

- **Outcome:** Every live and persisted unit carries the approved mutable
  controller/tag profile; existing saves and spawn sources preserve behavior
  through migration.
- **Scope:** `UnitInstance`, unit commands, spawn defaults/arguments, snapshot,
  frozen v1/current v2 DTOs, component bump/migration, load warnings, fresh-
  process compatibility tests.
- **Phase:** Runtime and wire
- **Depends on:** FTS-2
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2, D-3, D-4, D-5, D-6, D-7, D-8,
  D-9, D-10, D-11, D-12, D-13, D-14, D-15, D-16, D-17, D-18,
  D-19, D-20, D-21, D-22, D-23, D-24, D-26
- **Acceptance signals:** All five old values and unknown values migrate;
  current portal/wildlife/location/debug spawns keep their old behavior; new
  profiles and membership ownership/provenance round-trip through units v2.
- **Out of scope:** Changing higher-level consumers to exploit nomad identity.
- **Open questions:** None

### FTS-4. Port ownership, commandability, and discovery consumers

- **Outcome:** Every non-relation policy consumer reads controller-aware
  profile queries and remains distinct from alliance.
- **Scope:** Location discovery, building knowledge, transfer eligibility,
  player commands, observer/local-controller context, focused regression tests.
- **Phase:** Control policy
- **Depends on:** FTS-3
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2, D-3, D-5, D-10, D-11, D-24, D-26
- **Acceptance signals:** Same-controller units are commandable only by their
  controller; allied NPCs do not discover or become commandable; other-player
  units are not locally owned.
- **Out of scope:** Combat/AI relation consumers and networking.
- **Open questions:** None

### FTS-5. Port alliance, hostility, combat, and AI consumers

- **Outcome:** Medic, group combat, attack permission, context menu, and threat
  logic use one controller/tag-aware relation authority.
- **Scope:** Alliance and relation call sites, debug override, context menu,
  medic/group AI, autonomous hostility policy hooks, focused Lua/Haskell tests.
- **Phase:** Relation policy
- **Depends on:** FTS-4
- **Ordering:** `critical path`
- **Relevant decisions:** D-2, D-3, D-4, D-6, D-7, D-8, D-9, D-10,
  D-11, D-12, D-13, D-15, D-16, D-17, D-18, D-19, D-21, D-23,
  D-24, D-26
- **Acceptance signals:** Same-controller/shared-tag pairs are friendly by
  default but a directed live conflict can override them; acolyte/nomad pairs
  without a higher-precedence live/shared rule are mutually hostile; A-to-B and
  B-to-A may differ; local retaliation does not imply team hostility; cleared
  diplomacy starts no new combat and permits only each already executing goal
  to finish against its assigned target; debug staging bypasses permission
  without silently changing diplomacy, alerts, ownership, or discovery.
- **Out of scope:** Nomad-specific threat range/AI and multiplayer transport.
- **Open questions:** None

### FTS-6. Expose mutable faction profiles and directed order hostility

- **Outcome:** Lua/content/order callers can query arbitrary tags, mutate
  runtime-owned membership, and initiate/cancel cause-scoped directed hostility
  through narrow authoritative operations without replacing profiles or
  duplicating precedence logic.
- **Scope:** Narrow unit-based faction Lua API and read-only profile snapshot,
  spawn/content ingress,
  tag mutation, persisted directed-relation cause API, Attack transaction,
  Hold/Cancel cleanup, diplomacy resolution scripting, compatibility window for
  scalar callers, fixtures, authoring docs.
- **Phase:** Integration API
- **Depends on:** FTS-5
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2, D-3, D-4, D-5, D-6, D-7, D-8,
  D-9, D-10, D-11, D-12, D-13, D-14, D-15, D-16, D-17, D-18,
  D-19, D-20, D-21, D-22, D-23, D-24, D-25, D-26
- **Acceptance signals:** Real scripts consume only authoritative queries; an
  attack order installs A-to-B hostility before combat; pre-escalation
  Hold/Cancel removes its cause; post-escalation Hold/Cancel removes neither
  direction; a diplomacy script may clear the escalated relation without
  spawning new combat or interrupting existing combat incorrectly; arbitrary
  temporary tags work through owner-scoped mutation; ordinary callers cannot
  remove authored or foreign-owned memberships or replace a whole profile;
  invalid inputs are inert and reported once.
- **Out of scope:** General diplomacy UI and incident communication, which is
  FTS-6A.
- **Open questions:** None

### FTS-6A. Propagate directed hostility through orders and team knowledge

- **Outcome:** A target does not gain omniscient team hostility: damage or a
  witnessed death creates local knowledge, and only successful radio/in-person
  reporting promotes that incident to a reverse directed team relation.
- **Scope:** Local incident representation, direct-damage self-defense,
  witnessed-attack/death event boundary, explicit report scope, radio and
  walk-notify integration, idempotent reverse-relation cause, persistence as
  approved.
- **Phase:** Knowledge propagation
- **Depends on:** FTS-6
- **Ordering:** `critical path`
- **Relevant decisions:** D-6, D-9, D-11, D-12, D-13, D-14, D-15, D-16,
  D-17, D-18, D-19, D-20, D-21, D-23, D-24
- **Acceptance signals:** Team A can attack team B while B remains unaware; a
  damaged survivor may retaliate locally; an unwitnessed kill does not alert B;
  a witness or survivor can communicate; only completed communication makes
  uninvolved B members treat A as hostile; all knowledge and propagation state
  survives save/load exactly.
- **Out of scope:** General-purpose intelligence sharing, diplomacy UI,
  negotiation, reputation, and multiplayer networking.
- **Open questions:** None

### FTS-7. Retire the legacy enum and close compatibility documentation

- **Outcome:** No production path depends on the old five-value enum or scalar
  faction API, and durable docs/audits describe the live profile system.
- **Scope:** Remove adapters after consumers migrate; source/audit searches;
  persistence inventory, `CLAUDE.md`, API/authoring docs, probe registry notes;
  link the nomad encounter design to this prerequisite.
- **Phase:** Closure
- **Depends on:** FTS-6A
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2, D-3, D-4, D-5, D-6, D-7, D-8,
  D-9, D-10, D-11, D-12, D-13, D-14, D-15, D-16, D-17, D-18,
  D-19, D-20, D-21, D-22, D-23, D-24, D-25, D-26
- **Acceptance signals:** Legacy production references are zero except frozen
  v1 migration code; focused/integrated gates and relevant audits pass; docs
  match current ownership and precedence.
- **Out of scope:** Deleting frozen v1 DTOs or implementing multiplayer.
- **Open questions:** None after preceding slices settle their contracts.

## Source notes

The project owner wants factions to be a tag system rather than one exclusive
enum. Player acolytes combine player control with `acolyte`; nomads carry
`nomad`; future player-controlled nomads combine player control with `nomad`.
Acolyte and nomad identities are automatically hostile, but two units
controlled by the same player are not hostile because shared control overrides
other hostility. Different players remain distinguishable and may conflict in
a future multiplayer environment. The owner requested this as a separate
design arc and paused nomad-asset design work to settle the faction foundation
first.

The owner confirmed that controller identity should be optional: NPCs have no
controller rather than sharing a sentinel `none` controller. Different human
players and future AI-controlled factions participate in a diplomacy system
that can turn hostilities on or off. Without a controller or diplomacy
override, NPCs are friendly only when they share the same faction; otherwise
their data-authored base relation applies. The owner also established a broad
authoring rule for this arc: everything reasonably definable in YAML should be
data-driven rather than compiled into the faction vocabulary.

The owner then clarified that diplomacy belongs entirely to the tag side of
the model, while controller identity is still passed alongside the tags for
commandability and same-controller safety. Tags have at least two relationship
roles: natural affiliations such as `acolyte`/`nomad`, and teams such as
`red_tribe`. Shared team wins over natural hostility, so a red-tribe acolyte and
nomad are friendly. Shared natural affiliation keeps different acolyte teams
friendly until an explicit team-tag diplomacy change makes them hostile. An
attack order against another friendly acolyte team is expected to establish
that diplomatic hostility rather than act as a one-off permission bypass.

Controllers must cover human players, in-game AI authorities, and future
multiplayer identities; naturally occurring units can remain uncontrolled. A
multiplayer player may also spawn a separately AI-controlled acolyte team, so
neither the human controller nor the `acolyte` affiliation can stand in for the
team's diplomatic identity. The owner asked for the most generic system that
covers these cases without building the full diplomacy or multiplayer feature.

The owner rejected a one-team limit. Units may carry arbitrarily many tags;
temporary `fight_team_A` and `fight_team_B` tags can divide units that still
share a player controller, player team, and `acolyte` tag. Runtime systems own
the tag names and any dynamic numbering. Faction policy accepts arbitrary valid
string IDs rather than requiring every session-created tag to appear in YAML.

The owner also rejected symmetric instant hostility. A player order from team A
to team B makes A hostile toward B and begins the attack, but B remains unaware.
A damaged survivor can know and report the attack; if the victim dies, another B
unit must witness the event and communicate it. Radio equipment exists so this
knowledge can propagate rather than appearing globally. Only when the relevant
team learns should its reverse hostility become team-wide. The owner initially
specified that Hold/Cancel removes the initiating order's outgoing hostility,
then narrowed that behavior: it may do so only before reverse hostility exists.
These requirements revise the earlier absolute same-controller and symmetric
diplomacy proposals.

The owner approved hostile-cause dominance: if several live tag relations
apply, any active hostile cause wins until removed. The owner also required
every order, report, or diplomacy mutation to name its source and target tags
explicitly; the faction system must never guess which memberships represent
the intended teams.

Cancel semantics are escalation-sensitive. If A cancels before B establishes
reverse hostility, A's outgoing order hostility clears. If A has already
triggered B-to-A hostility, ordinary cancellation clears neither direction.
The mutual conflict then requires a separate resolution mechanism rather than
allowing A's controller to erase B's informed reaction.

The owner set the escalation threshold at the team-wide reverse relation, not
the first local injury or retaliation. An individual victim continues using the
existing short-term last-attacker defense before its team knows. The owner also
assigned conflict resolution exclusively to diplomacy scripting: clearing the
hostile tag relation blocks future engagements, but combats already underway
resolve before those units stand down.

All state belonging to tags or diplomacy must persist. That includes temporary
memberships, directed causes, escalation, individual incident knowledge, and
radio/in-person communication in progress; save/load may not reset the
information boundary.

The owner then approved the proposed boundaries from Q-4, Q-21, and Q-22.
Runtime tag mutation is ownership-scoped: ordinary systems remove only
memberships they added, while changing YAML-authored defaults requires an
explicit conversion/reclassification path. Applicable live causes reduce by
`hostile > neutral > ally`, so neutral suppresses ally when no hostile cause
exists. After diplomacy clears during combat, an already executing attack goal
may finish only against its assigned target; target death or escape, attacker
retreat, or goal completion ends that authorization, with no retargeting,
restart, or new combatant.

The owner approved the last three architecture choices. Explicit debug combat
may bypass ordinary restrictions, including same-controller protection, but it
does not mutate diplomacy or alert teams unless the debug request deliberately
tests those effects. Lua receives narrow authoritative queries and owner-scoped
mutations and cannot replace a whole profile. Legacy saves use the exact D-26
map: definition-aware local-player and wildlife profiles, a `legacy_hostile`
compatibility identity, inert neutral, and capability-based debug without
player ownership or ordinary diplomacy identity.
