# Scenario file format (v1)

Scenarios are reusable testing-arena setups (epic #2698). An author writes one as
YAML, and later the arena captures one from a running session (SCN-14). This
document is the authoring guide and contract for format version 1, defined by #2699
(SCN-01). The decoder and validator live in `src/Scenario/`. The design rationale
is in [designs/predefined_arena_worlds_design.md](designs/predefined_arena_worlds_design.md),
in SCN-01 and the decisions it cites.

The scope of this slice is the representation and its validation. Nothing here
constructs, replaces or captures a live world. Runtime construction, finite-map
enforcement and capture belong to the later SCN slices. Those slices consume
`Scenario.Schema.loadScenarioFile` and the typed values in `Scenario.Types`. No
runtime behaviour is claimed from the schema tests.

The schema is pure data. It adds no `EngineEnv` field, no manager or thread state,
and no save-wire type. It therefore needs no persistence-inventory or capability
change and no `currentSaveVersion` bump. The scenario format version is
independent of ordinary save component versions (D-27, D-46).

## Loading contract

`loadScenarioFile catalog path` returns exactly one of two results:

- `ScenarioFailed f` means an **unsuccessful load**. Nothing may be constructed
  from it, and it is never an empty scenario (D-16, D-23, D-44). It covers:
  - an unreadable or missing file (`ScenarioUnreadable`);
  - invalid YAML (`ScenarioSyntaxError`);
  - a document that is not a mapping (`ScenarioNotAMapping`);
  - a missing `version` (`ScenarioVersionMissing`) or a non-integer one
    (`ScenarioVersionMalformed`);
  - an unsupported version (`ScenarioVersionUnsupported`): newer than this build,
    below 1, or with a gap in the migration chain;
  - a failed migration step (`ScenarioMigrationFailed`).
- `ScenarioLoaded scenario diagnostics` is the **usable content**, after every
  recoverable rejection, together with every diagnostic. The diagnostics are sorted
  by document path.

Loading only reads the source. Validation, migration and fallback preparation never
write the file (D-24).

The `ScenarioCatalog` argument describes the current definitions: unit stats and
skills, body parts, equipment slots, item fluid capacity and container kind,
building footprints, build work, materials and power capacity, location footprints
and the item definition each significant slot requires, flora species, materials, structure packs, infections
and knowledge. It is plain data. The runtime adapters build it from the loaded
registries, and the tests build fixtures. Validation never needs an engine.

### Diagnostics

Each `ScenarioDiagnostic` carries three things:

- **`sdPath`**: the document path of the affected entry or field, such as
  `units[1].stats.agility` or `buildings[0].storage[2]`.
- **`sdReason`**: the cause, for example `UnknownField`, `InvalidValue expected`,
  `MissingRequired`, `UnknownDefinition name`, `DuplicateId id`, `DerivedValue`,
  `EmptyTag`, `MissingReference id`, `WrongReferenceKind id`, `RejectedReference id`,
  `OwnerRejected path`, `AmbiguousBinding id`, `DefinitionMismatch definition`, `OutsideBounds`,
  `FootprintOutsideBounds` or `ClippedTiles n`.
- **`sdEffect`**: what the rejection cost.

| Effect | Meaning |
|---|---|
| `EntryRejected` | The whole entry and everything it owns is gone. An entry is any list element, including a wound, scar, modifier or tile. |
| `FieldRejected` | One field or override was dropped and the entry was kept. The field takes its omission default. |
| `TilesClipped` | The out-of-bounds tiles of a terrain or fluid patch were dropped. |
| `CascadeRejected` | Dropped because an owner, or a required reference target, was rejected. |

Automated callers compare the complete diagnostic list against the diagnostics they
expect. An unexpected one fails setup (D-15).

### Rejection rules (D-14, D-17, D-47)

- **Required data.** An entry whose required field is missing or malformed, or
  whose definition is unknown, is rejected. Removed definitions follow the same
  rule.
- **Optional fields.** An unknown key or an invalid optional value drops only that
  field.
- **Owned contents.** Rejecting an owner rejects everything it owns, recursively:
  a unit's inventory, equipment and accessories; a building's storage and delivered
  materials; an item's contents. Nothing spills into the retained scenario.
- **Required references.** A unit's `encounter` must name a surviving location.
  - If the target was never declared, the unit is rejected with `MissingReference`.
  - If the target was declared but rejected, the unit is rejected as a
    `CascadeRejected` `RejectedReference`.
  - If the target is a different kind of entry, the unit is rejected with
    `WrongReferenceKind`.

  These rules apply transitively, to a fixpoint.
- **Optional references.** A location's `significant_items` binding to a missing,
  rejected or wrong-kind item, to an item of a definition other than the one that
  slot requires (`DefinitionMismatch`), or to an item that another binding also claims
  (`AmbiguousBinding`, which drops every claimant), drops only that binding. The slot
  stays unbound, which is the ordinary not-yet-spawned state.
- **Unrelated entries** always survive.

## Coordinates and bounds (D-9, D-19, D-21)

Positions are horizontal tile coordinates in conventional Cartesian orientation,
with positive Y up. Omitting `map` keeps the existing expandable arena. A finite
`map: {width, height}` is an exact rectangle of tiles centred on zero:

```text
minX = -floor(width / 2)      maxX = minX + width - 1
maxY =  floor(height / 2)     minY = maxY - height + 1
```

The bounds are inclusive and contain exactly `width × height` tiles:

| Size | X | Y |
|---|---|---|
| 20×10 | −10…9 | −4…5 |
| 21×11 | −10…10 | −5…5 |
| 20×11 | −10…9 | −5…5 |
| 21×10 | −10…10 | −4…5 |
| 1×1 | 0 | 0 |

- **Dimension domain.** Width and height are integers in `[1, 100000]`
  (`Scenario.Bounds.maxScenarioDimension`). The bounds are computed with checked
  `Integer` arithmetic. A map outside the domain, or one missing either dimension,
  is a field rejection and leaves the expandable arena.
- **Independence from worldgen.** This rectangle does not follow the generated
  world's own convention: `World.Chunk.Types` uses a half-open `[−half, half)` on
  u. The runtime slices convert between them. Changing the scenario formula is a
  format decision, not an implementation detail.
- **Continuous positions.** A unit or ground item at continuous `x, y` occupies the
  tile `(floor(x + 0.5), floor(y + 0.5))`, because tile centres sit on integers.
  The tile is computed as an unbounded integer, so a huge finite coordinate such
  as `1e30` lies outside every finite map instead of overflowing into it.
- **Camera.** `camera: {x, y}` defaults to `(0, 0)` and never translates bounds
  or objects.
- **Patches.** Terrain and fluid patches keep their in-bounds tiles and report one
  `TilesClipped` diagnostic with the dropped count. A patch with no tiles inside the
  map is rejected.
- **Objects.** Units, ground items, flora and structure pieces outside the map are
  rejected.
- **Footprints.** A building or location whose footprint (anchor plus the catalog's
  inclusive offsets) crosses the edge is rejected whole, with everything it owns.

## Identity, references and tags (D-29)

- **Explicit id.** `id:` is an explicit scenario identity made of letters, digits,
  `_`, `-`, `:` and `.`. Quote an id that YAML would read as another type: an
  unquoted `off`, `yes` or `1` is a boolean or a number, not an id. Capture writes one for every entry. A malformed `id` is a
  field rejection, and the entry falls back to its automatic id.
- **Automatic id.** An entry with no `id` gets its document path as an automatic
  identity, such as `units[2]` or `units[2].inventory[0]`. An explicit id cannot
  contain `[`, so the two spaces never collide.
- **Separate from runtime ids.** Scenario identities are not runtime allocation ids:
  item instance ids, unit ids, building ids, location instance ids and flora
  instance ids are allocated at construction and are never authorable.
- **Default-generation seed.** The identity is the input that seeds deterministic
  defaults for omitted values (D-5). Tags never contribute to it.
- **One namespace.** All entries share one id namespace. An id held by more than one
  surviving entry rejects every holder, whatever the order (`DuplicateId`).
- **Resolution.** References may only name explicit ids. They resolve against the
  whole document, so a forward reference behaves exactly like a backward one.
- **Tags.** `tags:` is an optional list of gameplay tags, and any non-empty string
  is a tag (`Gameplay.Tags.Types.mkGameplayTag`). An empty string drops only that
  tag. There are no case, spelling, uniqueness or role rules. Tags do not feed
  identity, and this slice does not use the live tag registry.

## Omitted versus explicit values (D-5, D-8, D-12)

- An **omitted** optional field is eligible for the deterministic,
  definition-driven fallback.
- An **explicit** value, including `0`, `""`, `[]` or `{}`, is authoritative.
  Two examples:
  - `wounds: []` is a healed unit.
  - `inventory: []` is an empty unit that does not receive the definition's
    starting kit.
- Capture writes every supported current value explicitly.
- `stats` and `skills` hold explicit overrides keyed by name. A name that is not
  listed stays fallback-eligible. An explicit `strength: 12` stays 12 whatever the
  definition's default becomes, because the scenario never stores definition
  defaults. A stat that the current definition added after the file was written
  is simply omitted. It takes its deterministic default with no diagnostic
  (`Scenario.Validate.fallbackStatNames`).

## Field table

The tables below use these column headings:

- **Opt** is the field's requirement level: **req** for required, **opt** for
  optional.
- **Omitted** is the omission default.
- **Explicit empty** is the meaning of an explicitly empty value.
- **Runtime owner / construction** is the owning runtime field or construction rule.

Every entry also accepts `id` (opt, see above) and `tags` (opt list of non-empty
strings, default `[]`).

### Top level

| YAML | Type | Opt | Omitted | Runtime owner / construction |
|---|---|---|---|---|
| `version` | integer | req | unsuccessful load | Format dispatch; must be 1 or migratable to it. |
| `map.width`, `map.height` | integer, `[1, 100000]` tiles | opt (both or neither) | Expandable arena | Finite page bounds (SCN-08). |
| `camera.x`, `camera.y` | number, tiles | opt | `0` | Initial camera position only (SCN-13). |
| `terrain`, `fluids`, `flora`, `structures`, `buildings`, `locations`, `units`, `ground_items` | list | opt | `[]` | One family each; a non-list is a field rejection. |

### `terrain[]` — column patches

| YAML | Type | Opt | Omitted | Runtime owner / construction |
|---|---|---|---|---|
| `rect` | `{x0, y0, x1, y1}` integers, inclusive | req (exactly one of `rect` / `tiles`) | — | Corners may be given in any order. |
| `tiles` | list of `[x, y]` integer pairs | req (exactly one of `rect` / `tiles`) | — | A malformed tile drops that tile, duplicates collapse, and an empty result rejects the patch. |
| `material` | material definition name | req | — | Column material (`World.Edit.Types` `WeSetCell`/`WeAddTile`); unknown → entry rejected. |
| `surface_z` | integer z | req | — | Top solid cell of every column in the patch. |
| `slope` | integer `[0, 15]` | opt | `0` (flat) | Ramp bitmask, bits N/E/S/W (`WeSetSlope`). |

Dependent values to reconstruct: column surface maps, vegetation placement and fluid
neighbour state (SCN-11). These are derived, not authorable.

### `fluids[]` — fluid patches

| YAML | Type | Opt | Omitted | Runtime owner / construction |
|---|---|---|---|---|
| `rect` / `tiles` | as terrain | req | — | Clipped per tile. |
| `fluid` | `ocean` \| `lake` \| `river` \| `lava` | req | — | `World.Fluid.Types.FluidType`. |
| `surface_z` | number on the exact fluid plane (a multiple of `1/fluidUnitsPerZ`, currently 1/8) | req | — | Absolute fluid surface, stored in fluid units (`World.Fluid.Exact`, `WeSetFluidSnapshot`). It is read from the document's exact decimal, never through a float. A value off the plane, or one too large for `Int`, rejects the patch. |

Derived: the per-cell fluid depth below the surface and active-simulation state.

### `flora[]`

| YAML | Type | Opt | Omitted | Runtime owner / construction |
|---|---|---|---|---|
| `species` | flora species name | req | — | `FloraSpecies` by name (`World.Flora.Reference`). |
| `x`, `y` | integer tile | req | — | Tile position. |
| `z` | integer | opt | Column surface | `fiZ`. |
| `age` | number ≥ 0, game days | opt | Deterministic default | `fiAge`. |
| `health` | number `[0, 1]` | opt | Full health | `fiHealth`. |

Derived: sub-tile offsets, visual variant, base width, phase and stage textures,
and the flora instance id (allocated at construction).

### `structures[]` — structure pieces

| YAML | Type | Opt | Omitted | Runtime owner / construction |
|---|---|---|---|---|
| `pack` | structure pack name | req | — | `data/structure_packs/<pack>.yaml`. |
| `piece` | `floor`, `ceiling`, `wall_ne`, `wall_nw`, `wall_se`, `wall_sw`, `post_n`, `post_e`, `post_s`, `post_w`, `wire` | req | — | `Structure.Types.StructureSlot`; the pack must supply that piece kind. |
| `x`, `y` | integer tile | req | — | Tile position. |
| `z` | integer | opt | Column surface | Piece z (`WeSetStructure`). |

Derived: texture and facemap palette ids, and wire connection variants.

### `buildings[]`

| YAML | Type | Opt | Omitted | Explicit empty | Runtime owner / construction |
|---|---|---|---|---|---|
| `definition` | building definition name | req | — | — | `biDefName`. |
| `x`, `y` | integer tile, footprint anchor (min corner) | req | — | — | `biAnchorX/Y`; footprint from the definition. |
| `z` | integer | opt | Terrain z at the anchor | — | `biGridZ`. |
| `storage` | list of items | opt | Nothing stored | `[]` = nothing stored | `biStorage`. Exact contents, with no gameplay capacity check (D-57). |
| `build_progress` | number `[0, build work]`, worker-seconds | opt | Complete (`= build work`) | — | `biBuildProgress`. |
| `materials_delivered` | list of items, each a material of the definition | opt | Nothing delivered | `[]` | `biMaterialsDelivered`, grouped by definition at construction. A non-material item is rejected. |
| `power_charge` | number `[0, capacity]`, watt-hours | opt | Placement default (empty) | — | The storage node's `pnStoredWh`. Only valid for a power-storage definition. |

Derived: the footprint size, texture, spawn time, the building id, and power-network
membership and relationships.

### `locations[]` — real placed locations

| YAML | Type | Opt | Omitted | Runtime owner / construction |
|---|---|---|---|---|
| `definition` | location definition id | req | — | `liDefId`. |
| `x`, `y` | integer tile anchor | req | — | `liAnchor`; footprint (`liBounds`) from the definition. |
| `significant_items` | map `slot → item id` | opt | All slots unbound | Binds `liSignificant` slot `n` (1-based, at most the definition's count) to that item's physical identity. This is an optional reference. Slot keys are plain decimals (`"1"`, never `"01"`), so two keys cannot name one slot. The target must be an item of the definition that slot requires (`lsiItemDefName`); a mismatch drops the binding with `DefinitionMismatch`. |

Derived or excluded: display name, gloss and etymology are derived. Encounter roll
and roster are derived from definition and identity; units join by `encounter`.
Container slots are derived from the definition. The location instance id is
allocated. Lifecycle and discovery, encounter progress, the taken latch and the
clearance notice are player and expedition progress, and are excluded by D-3.

### `units[]`

| YAML | Type | Opt | Omitted | Explicit empty | Runtime owner / construction |
|---|---|---|---|---|---|
| `definition` | unit definition name | req | — | — | `uiDefName`. |
| `x`, `y` | number, tiles | req | — | — | `uiGridX/Y`. |
| `z` | integer | opt | Column surface | — | `uiGridZ`. |
| `name` | string | opt | Deterministic draw from the name pool | `""` = unnamed | `uiName`. |
| `facing` | compass direction (`south`, `north-east`, `sw`, …) | opt | `south` | — | `uiFacing` (`Unit.Direction.parseDirectionName`). |
| `encounter` | location id | opt | Not an encounter occupant | — | **Required reference** when present: adds the unit to that location's encounter roster (`leOccupants`). |
| `stats` | map `stat → number` | opt | Deterministic roll | `{}` = no overrides | `uiStats`. Unknown name → field rejected; a definition-derived stat → `DerivedValue`. |
| `skills` | map `skill → number` | opt | Deterministic roll | `{}` = no overrides | `uiSkills`. |
| `knowledge` | map `knowledge → number ≥ 0` | opt | Deterministic roll | `{}` = knows nothing | `uiKnowledge`. When authored, it is the complete known set. |
| `modifiers` | map `stat or skill → list of {source, delta, percent, remaining}` | opt | None | `{}` | `uiModifiers` (`StatModifier`). `source` is required (non-empty). `delta` and `percent` default to 0. `remaining` is game seconds after scenario start (> 0); omitted means permanent. |
| `wounds` | list of wound records | opt | Definition default (healthy) | `[]` = healed | `uiWounds`. |
| `scars` | list of scar records | opt | None | `[]` | `uiScars`. |
| `blood` | number ≥ 0, litres | opt | `body_mass × 0.075` | — | `uiBlood`. |
| `immune_response` | number `[0, 1]` | opt | `0` | — | `uiImmuneResponse`. |
| `immunities` | map `infection id → [0, 1]` | opt | None | `{}` | `uiImmunities`. |
| `inventory` | list of items | opt | Definition starting inventory | `[]` = carries nothing; no kit | `uiInventory`. |
| `equipment` | map `slot → item` | opt | Definition starting equipment | `{}` = nothing equipped | `uiEquipment`. An unknown slot drops that item and its contents. |
| `accessories` | list of items | opt | Definition starting accessories | `[]` | `uiAccessories`. |

**Wound record** (`Unit.Types.Wound.Wound`). These fields are required, and a bad
value drops the wound:

- `part`: a body part of the definition.
- `kind`: one of `slash`, `stab`, `blunt`, `fracture`, `concussion`, `internal`,
  `severed`, `arterial` or `frostbite`.
- `severity`: in `[0, 1.6]`, or `[0, 0.4]` for `blunt` (`Unit.Injury`).

These fields are optional, and a bad value falls back to its default:

- `age`: seconds ≥ 0 before scenario start, default 0. It sets `woundAt = start − age`.
- `bandage`: `[0, 1]`, default 1.
- `clot`: `[0, 1]`, default 0.
- `heal`: `[−0.5, 1]`, default 0.
- `dressing`: `""`, `bandage` or `tourniquet`; default `""`.
- `infection`: `[0, 1]`, default 0.
- `clean`: bool, default false.
- `infection_type`: an infection id or `""`; default `""`.
- `necrosis`: `[0, 1]`, default 0.

**Scar record** (`Scar`). `part`, `kind` and `severity` are required, as for wounds;
the scar severity range is `[0, 1.6]`. `age` (seconds ≥ 0) sets `scarAt = start − age`.

The following are derived or excluded:

- **Derived from definition and body:** texture and direction sprites, base width,
  `strength` (body-block units: from `strength_base` and lean mass), `strength_body`,
  `max_hydration`, `max_hunger`, `max_calories`, `carrying_capacity`, the maximum
  blood, pose, activity and animation.
- **Excluded by owner decision:** the faction profile (`uiFaction`); units use their
  definition's default tags.
- **Runtime-only:** last attacker, freeze/force-loop, climb destination, trail
  state and the unit id.
- **Excluded by D-3:** orders.

Which stats are authorable and which are derived is the catalog's `StatRule` for the
current definition.

### Items — in inventories, equipment, accessories, storage, delivered materials and contents

| YAML | Type | Opt | Omitted | Explicit empty | Runtime owner / construction |
|---|---|---|---|---|---|
| `definition` | item definition name | req | — | — | `iiDefName`. |
| `fill` | number litres, `[0, capacity]`; must be `0` for a non-fluid item | opt | Definition default | — | `iiCurrentFill`. |
| `quality` | `[0, 100]` | opt | Deterministic roll | — | `iiQuality`. |
| `condition` | `[0, 100]` | opt | `100` | — | `iiCondition`. |
| `sharpness` | `[0, 100]` % of the definition's base | opt | `100` | — | `iiSharpness`. |
| `weight` | kg ≥ 0, the instance's own empty weight | opt | Definition weight or roll | — | `iiWeight`. |
| `bulk` | litres ≥ 0, or `null` | opt | Snapshot of the current definition's bulk | `null` = no recorded bulk | `iiBulk`; `null` is the runtime's honest absence (`Nothing`) on legacy instances, and fails closed in gameplay. |
| `storage_capacity` | `{weight: kg ≥ 0, bulk: litres ≥ 0}`, or `null` | opt | Snapshot of the current definition's storage | `null` = no internal capacity | `iiStorage` (`ItemStorage`); `null` is explicit absence (non-storage items and legacy instances). |
| `temperature` | `ambient` or °C ≥ −273.15 | opt | Ambient | `ambient` | `iiTemp` (`Nothing` = ambient). |
| `contents` | list of items | opt | Definition default contents | `[]` = empty | `iiContents`. Any item may state `contents: []`. Only item containers (kits, toolboxes) may list items; a non-empty list on any other item is a field rejection that drops the listed items. Exact contents, with no capacity check (D-57). |

The item instance id (`iiInstanceId`) is allocated at construction and is never
authorable. Carried and contained totals (`itemTotalWeight`) are derived.

### `ground_items[]`

These are items lying in the world. A ground item takes every item field above
plus `x` and `y` (numbers, tiles, required), the ground position (`giX`/`giY`).
Its tile must be inside the map.

## Authoring examples

A minimal scenario on the expandable arena:

```yaml
version: 1
```

A bounded duel: an injured scout carrying a half-full canteen, and a near-broken
knife.

```yaml
version: 1
map: {width: 20, height: 10}
camera: {x: 0, y: 0}
terrain:
  - {id: floor, rect: {x0: -10, y0: -4, x1: 9, y1: 5}, material: granite, surface_z: 3}
locations:
  - {id: ruin-1, definition: ruin, x: -5, y: 2, significant_items: {"1": relic}}
units:
  - id: scout
    definition: acolyte
    x: 0.5
    y: -1
    facing: north-east
    stats: {strength_base: 1.2}
    modifiers:
      strength: [{source: poison-A, delta: -0.2, remaining: 30}]
    wounds:
      - {part: left_leg, kind: slash, severity: 0.5, age: 60, bandage: 0.05}
    inventory:
      - {id: canteen-1, definition: canteen, fill: 0.5}
    equipment:
      main_hand: {id: knife-1, definition: knife, condition: 5, sharpness: 15}
  - {id: nomad-1, definition: nomad, x: -5, y: 3, encounter: ruin-1}
ground_items:
  - {id: relic, definition: gold_idol, x: -5, y: 2.5}
```

## Compatibility policy (D-22, D-27, D-44, D-46)

- **Version field.** Every file declares one top-level integer `version`. It is
  mandatory: a missing or non-integer version is an unsuccessful load and never
  defaults to 1.
- **Version numbering.** The format version is independent of ordinary save
  component versions. Neither bumps because the other did.
- **Migration chain.** `Scenario.Schema.scenarioFormat` names the current version
  and holds one migration step per released older version (`k → k + 1`). A file is
  supported when the chain from its version to the current one is complete.
  - Each step rewrites the parsed document in memory before current validation runs.
  - Every released version keeps its step forever.
  - A newer version, a version below 1, or a gap in the chain is unsupported.
  - A step that fails is an unsuccessful load. In every unsupported or failed case,
    nothing is constructed.
- **What bumps the version.** Any change to the v1 vocabulary or its meaning ships as
  a new version with a migration, and the v1 fixtures stay as evidence. This includes
  adding a field: an older build reports a new field as unknown.
- **Definition changes.** Removed definitions follow the ordinary rejection rules.
  Explicit values are never reset. Stats added to a definition later take their
  deterministic defaults.
- **Source files.** Loading never rewrites the source. Only an explicit save writes
  the current format (D-24).

## Owner decisions recorded for v1

The SCN-01 design required straddling fields to be brought back to the owner. The
owner decided on 2026-10-02, relayed through the owner's assistant.

- **13:51:04Z.** Include skills, knowledge overrides, an optional name, an optional
  facing (default south), building construction progress and delivered materials
  (omitted means complete with nothing delivered), timed stat modifiers, and
  power-node charge. Exclude per-unit faction-profile overrides and the
  building-spawn roster countdown (`biSpawnRemaining`). Location lifecycle and
  discovery remain excluded.
- **13:57:53Z.** Game-time stamps are relative and the file has no scenario clock.
  Wound and scar `age` are seconds before scenario start. A timed modifier's
  `remaining` is seconds after scenario start.

## Fixture expectations and tests

The `Scenario.Schema` Hspec group (`test-headless/Test/Headless/Scenario/Schema.hs`)
needs no engine, world or GPU. It writes inline YAML fixtures to scratch files and
loads them through the real `loadScenarioFile`. Its assertions are the complete
validated structures and the exact diagnostic lists. They cover:

- the five pinned bounds rectangles and the dimension domain;
- minimal and fully explicit v1 fixtures, every content family and nested ownership;
- omitted versus explicit zero and empty values, and automatic ids;
- recoverable invalid entries and overrides, with recursive owned-content exclusion;
- required-reference cascades, duplicate ids and optional-binding drops;
- declaration-order independence;
- explicit stats across changed definitions, and removed definitions;
- tile clipping versus whole-object and whole-footprint rejection;
- syntax, version and migration failures, including a migration through a
  synthetic v2 format;
- byte-for-byte source preservation.

The v1 fixture texts are frozen. Never edit them when a later version lands: they
must keep decoding to the same content through the migration chain.

```sh
cabal build all synarchy-test-headless
cabal test synarchy-test-headless --test-options='--match "Scenario.Schema"'
```
