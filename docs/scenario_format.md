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
independent of ordinary save component versions (D-27, D-46). The one piece of
scenario identity that must survive an ordinary save, the mapping from a
constructed item to the reference it came from, is owned by item construction
(SCN-09, [below](#item-construction-and-capture-scn-09-2709)), not by the schema.

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
  `FootprintOutsideBounds`, `ClippedTiles n` or `CyclicContents cycle`
  (reported by item construction: a definition's default contents re-enter a
  definition already being expanded, for example `a → b → a`).
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
  field. This includes an unknown key inside a nested mapping (`map`, `camera`,
  `rect`, `storage_capacity`): only that key is dropped, and the valid members
  beside it are kept. A missing or invalid required member of a nested mapping
  costs what its parent costs: a `rect` corner rejects the patch, while a `map`
  dimension or a `storage_capacity` member drops that optional field.
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
  `_`, `-`, `:` and `.`, at most 512 characters (the longest reference an ordinary
  save carries). Quote an id that YAML would read as another type: an
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
| `stats` | map `stat → number` | opt | Deterministic roll | `{}` = no overrides | `uiStats` (stored values). Unknown name → field rejected; a definition-derived stat → `DerivedValue`. Which keys are authorable is "Field ownership" under SCN-10 below. |
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
| `contents` | list of items | opt | Definition default contents | `[]` = empty | `iiContents`. Any item may state `contents: []`. Only an item that can hold items may list items: one whose definition does (kits, toolboxes) or one that states its own `storage_capacity` (ordinary inserts key on the instance's storage, so a captured instance with storage lists what it holds). A non-empty list on any other item is a field rejection that drops the listed items. Exact contents, with no capacity check (D-57). |

The item instance id (`iiInstanceId`) is allocated at construction and is never
authorable. Carried and contained totals (`itemTotalWeight`) are derived.

### `ground_items[]`

These are items lying in the world. A ground item takes every item field above
plus `x` and `y` (numbers, tiles, required), the ground position (`giX`/`giY`).
Its tile must be inside the map.

## Item construction and capture (SCN-09, #2709)

`Scenario.Items.*` turns the validated item entries above into real item trees and
back. It is the item half of every owner; units (SCN-10), buildings and locations
(SCN-12) and the session transaction (SCN-13) call it for the items they own.
Nothing here installs an owner, so a rejected tree leaves no live reference.

| Module | Does |
|---|---|
| `Scenario.Items.Build` | `buildItemForest`: checks each entry against the live definition, then mints the survivors through `Item.Materialize` (the one mint boundary). Returns the trees, the reference mapping, the tag assignments and the diagnostics. |
| `Scenario.Items.Owners` | The shapes each owner holds: `buildUnitItems` (inventory, equipment by sorted slot, accessories), `buildBuildingItems` (storage, delivered materials grouped by definition), `buildGroundItems` and `installGroundItems` (ground rows). Each collection stays `Authored`: omitted means the owner's definition decides, `[]` means it holds nothing. |
| `Scenario.Items.Capture` | `captureUnitItems`, `captureBuildingItems`, `captureGroundItems`: read coherent owner snapshots into entries with every supported value stated. |
| `Scenario.Items.Encode` | `itemEntryValue`: an entry as the key/value pairs the decoder reads, so `decode (encode e) ≡ e`. File writing is SCN-14's. |
| `Scenario.Items.Refs` | The durable item → reference mapping. |

**Explicit values are authoritative; only omissions default.** A stated `0`, an empty
`contents`, `bulk: null`, `storage_capacity: null` and `temperature: ambient` are
applied verbatim: no clamp, no salvage wear (`item.spawnGround`'s wear is its own
route), no re-roll. Resolve the derived total with `itemTotalWeight`; the `weight`
field is the item's own empty weight only. Explicit ambient and an omitted
temperature are the same state, `iiTemp = Nothing`, so the schema needs no separate
flag. Omitted `bulk` and `storage_capacity` snapshot the CURRENT definition.

**Invalid optional values are never repaired into a success.** The builder checks
every stated value against the live definition before minting: a fill outside
`[0, capacity]`, or non-zero on a non-container, a quality, condition or sharpness
outside `[0, 100]`, a negative or non-finite weight, bulk or capacity, and a
temperature below absolute zero each yield a `FieldRejected` `InvalidValue`
diagnostic at `<entry path>.<field>`, and the field takes its omission value.
(`Item.Materialize` still clamps a fill for the ordinary mint sites; the clamp is
not reachable from scenario construction because the check runs first.)

**Omitted values are reproducible (D-5).** Each node rolls its omitted quality and
empty weight from a private generator seeded by FNV-1a over
`"synarchy.scenario.item/v1" 0x00 reference 0x00 domain`, where the reference is the
node's `scenarioIdText` and the domain is `quality`, `weight` or the derived label
of a default-expanded child (`contents.<entry>.<copy>/<domain>`). Tags, the owner, the
allocated instance ids, sibling order and the gameplay RNG never enter, and stating
one value does not move another. Children a definition's default contents expand
draw from their parent's stream, so a kit with omitted contents reproduces its
bandages exactly.

**Exact contents (D-57).** Contents are installed as stated, even when they exceed
the container's capacities or the container states no capacity or bulk at all.
Nothing spills, truncates, relocates or rejects for those reasons. This is
initialization, not a relaxed transfer: `Item.Ownership` still refuses the next
ordinary insertion into such a container (`OverTargetWeight`, `TargetNotStorage`,
`CandidateNoBulk`, …) and the structural writer guards still hold. Omitted contents
expand the definition's defaults; `contents: []` is exactly empty; a stated list
replaces the defaults. An item that cannot hold items (its definition has neither
`storage` nor default contents, and the entry states no `storage_capacity` of its own)
cannot own contents: the list is a `FieldRejected` and the listed items are
`CascadeRejected` rather than promoted to a sibling or the ground.

**Identities stay separate.** A scenario reference (`id:`) is the file's stable
name; the instance id is allocated at construction; a ground-row id is a page-local
slot. `ibRefs` maps the first two, only for nodes an entry named (default-expanded
children carry none). A duplicate reference rejects every holder and what they own
(`DuplicateId`). Tags ride the mapping to the same instance, and a rejected entry
leaves no tag and no reference because nothing is minted for it.

**Recoverable failures are data.** Unknown definitions are `EntryRejected`
`UnknownDefinition`; every descendant of a rejected node is `CascadeRejected`
`OwnerRejected <path>`. Default expansion reports an unknown entry as
`UnknownDefinition` and a cycle as `CyclicContents` at the dropped entry's path
(`units[0].inventory[0].contents[1]`); the node holding the back edge and its other
children are kept. The ordinary mint sites keep their old behaviour: a silent drop
for an unknown entry and a log warning for a cycle.

**Attachment is all or nothing.** The orchestration installs the owner first and then
calls `Gameplay.Tags.Live.commitItemTreeAssignments` with `itemTagTargets` and
`itemRefTargets`. Every target must be live; a reference another item holds, or a
second reference for one item, refuses the whole commit, and so does a reference the
save could not carry (empty, longer than 512 characters, or outside the id vocabulary).
The reference check is repeated inside the atomic step against the manager being
committed, so an attachment that lands between the check and the write makes the
loser install nothing, tags included.

**Durable identity (D-52).** `World.State.Types.wmItemScenarioRefs` is session state
with the item tags' lifetime: it follows an item across every move, is forgotten with
the item (`forgetItemTags`, one atomic step with the tags), and is persisted by
`core-session` v3 (`csItemRefs`, ascending by instance id). A v1 or v2 save decodes
with none. Load staging drops a reference whose item the saved or the restored
session does not hold, with a `dangling-reference` warning at
`core-session.item-refs[item=N]`. Loading an ordinary save never re-runs scenario
defaulting.

**Capture writes the item as it is.** Every entry states its fill, quality,
condition, sharpness, own empty weight, bulk (or `null`), storage capacity (or
`null`), temperature and current contents, so an emptied kit is `contents: []` and a
partly used one lists what is left. Nothing from the original scenario overrides is
reused, definition defaults are not consulted, and player observations, orders and
progress are not item state. Capture mints nothing, draws no gameplay RNG and
modifies no inventory. An item built from reference `R` is captured as `R`; an
automatic path id (`units[2].inventory[0]`) is captured in its explicit form
(`units.2.inventory.0`). An unmapped item gets `<owner>.<collection>.<index>`
(`scout.inventory.2`, `ground.0`), made unique with `-2`, `-3`, … against every
reference already taken; the names depend on position only, never on instance ids.
An item's existing reference is preserved verbatim at any admitted length; only a
generated name (or the numeric suffix that resolves a clash) is shortened with a stable
hash, and only when it would pass 512 characters.
**One naming rule, four tiers.** Scenario ids share one namespace, so a capture names
items by priority, and a lower tier never displaces a higher one whatever order things
are captured in: (1) the ids of the document's OTHER entries (units, buildings,
locations, …); (2) an item's authored explicit reference, verbatim; (3) an item's
automatic reference in its bracketless form; (4) a generated position-derived name.
An item that cannot have its tier's spelling is written as that spelling plus `-2`,
`-3`, …, the first nothing has taken; characters outside the id vocabulary in a
generated name become `_`. `newCaptureState ctx documentIds trees` is the only way to
start a whole capture: it takes the other entries' ids and EVERY tree of every owner
together and names every referenced item up front, clean spellings first, then the
displaced ones, each group ordered by raw reference. The names of tiers 2 and 3 are
therefore a function of the set of references and ids alone: not of the owner, the list
position, the ground row or the order owners are captured in. Only tier 4 depends on
where an unreferenced item sits. Ground rows are ordered by a structural key compared
with derived `Ord` (physical fields as float bit patterns, the reference as held with
its kind, the tag set, the children), never by a rendered string or by row id, so two
rows tie only when they export identically.

## Unit construction and capture (SCN-10, #2710)

`Scenario.Units.*` turns the validated unit entries above into live units and back.
It composes the item adapters (#2709) for the unit's loadout. It installs one unit at
a time, and the session transaction (SCN-13) calls it for the scenario's units.

| Module | Does |
|---|---|
| `Scenario.Units.Catalog` | The field table. `unitCatalogEntryOf` builds the decoder's `UnitCatalogEntry` from the live definition, the physiology registry and the equipment classes. The construction adapter and capture read the same rules (`unitStatRules`). |
| `Scenario.Units.Defaults` | The deterministic fallback for every omitted value. |
| `Scenario.Units.Build` | `buildUnit`: one entry becomes a complete `UnitInstance`, its `UnitSimState`, its item trees, and the reference and tag assignments to commit. It installs nothing. |
| `Scenario.Units.Install` | `installBuiltUnit` / `installUnitBuilds`: the fenced commit, and the per-entry `UnitCompletion` result. |
| `Scenario.Units.Capture` | `captureUnits`: reads a coherent snapshot of live units into entries, with every supported value stated. |
| `Scenario.Units.Encode` | `unitEntryValue`: an entry as the key/value pairs the decoder reads, so `decode (encode e) ≡ e`. |

### Field ownership

Which `stats` keys a unit may state depends on its CURRENT definition
(`Scenario.Units.Catalog.unitStatRules`). The rows are listed by definition kind.

**A body-bearing definition** is an eager one: spawn seeds its body from `height`,
`bulk` and `bodyfat`.

| Stat | Rule | Meaning |
|---|---|---|
| Every stat template except `strength` | authorable | The unit's stored value. |
| `strength_base` | authorable | The un-scaled strength potential. |
| `strength` | **derived** | `strength_base × (lean_mass / 8.8·height²)^0.7` (`Unit.Thread.Command.Body`). |
| `height`, `bulk`, `bodyfat` | authorable | The seed inputs of a fresh body. `bulk` and `bodyfat` are consumed by the seed and never stored. |
| `frame_mass`, `body_mass`, `lean_mass`, `fat_mass` | authorable | The CURRENT body composition. |
| `strength_body`, `max_hydration`, `max_hunger`, `max_calories`, `carrying_capacity` | **derived** | From the body. |

**A lazy definition** has no body. Every stat template, `strength` included, is a
plain authorable stat.

**Physiology keys** are owned keys of a system that resolves Simulated for the
definition's own defaults, with no override and no applied engagement.

- **Authorable current values:** `hydration`, `hunger`, `calories`, `stamina`,
  `exhaustion`, `sleep_pressure`, `core_temp`, `salt`, `salt_store`, `heart_rate`
  and `blood_oxygen`. The stored base is stated.
- **Not stated:** the `mind` system's keys, which are mental episode state and
  timers. They take their system's own seed.
- **Derived:** each Simulated system's derived keys (`salt_conc`, `circulation`,
  `consciousness`, …).
- **Not scenario values:** meters, the combat-engagement marker, and the keys of a
  Nominal or Unsupported system.
- **Blood** is the separate `blood` field. It may be stated only while the
  definition's blood resolves Simulated.

**Related fields are checked as a group, never by the order they are applied in.**

- **Body masses.** State the four masses together, with
  `lean_mass + fat_mass ≤ body_mass`, or state none of them. A partial or
  incoherent set rejects each stated mass (`FieldRejected InvalidValue`), and the
  unit takes the seed path.
- **Seed inputs beside masses.** With a valid set of masses, a stated `bulk` or
  `bodyfat` is rejected: those fields only seed a fresh body.
- **Pool limits.** A pool stated above its maximum (the healthy value of a pool
  sized by an input, for example `hydration ≤ max_hydration`) is rejected and then
  seeded like an omission.

### Initialization order

`buildUnit` runs one fixed sequence. Each step reads only finished values:

1. **Definition and z.** The definition, then the column `z`.
2. **Stats.** Every omitted template is drawn from the unit's private generator,
   and the explicit values are laid over the draws.
3. **Body, once, through the body authority.**
   - With explicit masses, the spawn seed and its viability margins are bypassed,
     and `recomputeBodyDerivedStats` derives the rest. `strength_base` is present,
     so nothing is promoted twice.
   - Otherwise the spawn path runs (`seedBodyComposition`).
4. **Identity values.** Skills, knowledge, name and facing.
5. **Health.**
   - Wounds, with `woundAt = start − age`, and scars.
   - The immune response and immunities.
   - An infection type is checked against the live `InfectionManager`.
   - Then the death check (below).
6. **Items.** The explicit collections go through `Scenario.Items.Owners`. An
   omitted collection mints the definition's kit with the unit's private rolls.
   Nothing is shed and nothing is topped up (D-57).
7. **Modifiers.** They are built once, as spawn builds them
   (`spawnModifierMap`): the definition's innate modifiers with every WORN
   accessory's buffs at its current condition. Then the authored ones are added
   (`smExpiry = start + remaining`).
8. **Physiology.** The omitted pools go through
   `Unit.Physiology.Transition.reconcileStoredKeys`, the one seeding authority that
   spawn and load share. Its missing-only rule leaves every stated pool exactly as
   stated.
9. **Blood and pose.** Blood falls back to `canonicalBlood`, else the body-mass
   seed. A unit whose Simulated blood is below the wound tick's unconscious
   threshold is built `Collapsed`, without ticking.

**Pools are seeded natively.** The trusted issue review called resource pools
Lua-owned. Since the physiology epic (#2831, #2834, #2837), every pool belongs to a
registry system. Spawn and load seed it natively, and the Lua tick raises on a
missing Simulated pool rather than refilling it. The adapter therefore calls that
native authority, under the review's own "one shared authority it calls" clause
(manager ruling, 2026-10-18). Nothing Lua-owned is outstanding at completion.

**Death conditions.** A unit whose stated health already meets an existing death
condition is rejected whole, with what it owns (`EntryRejected InvalidValue`). It is
never built as a corpse. The conditions are:

- a wound of severity 1 or more on a vital part;
- necrosis of 0.85 or more on a vital part (gangrene);
- `blood ≤ 0` while blood is Simulated.

### Defaults (D-5)

Every omitted value comes from the definition, through a private generator per value.

- **Seed.** FNV-1a over `"synarchy.scenario.unit/v1" 0x00 reference 0x00 domain`.
  The reference is the unit's `scenarioIdText`.
- **Domains:** `stat/<name>`, `body/<name>`, `skill/<name>`, `knowledge/<name>`,
  `name`, and `<collection>/<slot>/<roll>` for the definition's kit.
- **What never enters:** the gameplay stat RNG, Lua's `math.random`, an allocated
  unit or item id, the tags, the unit's place in the file, and the build order.
- **Consequences.** The same placed unit gets the same values in every process, and
  stating one value never moves another.
- **Lazy definitions** resolve every template at construction. A later read draws
  nothing from the gameplay RNG.
- **A stat added to a definition later** takes its fallback, and stated values are
  kept.

### Loadout and modifiers

- **Omitted versus explicit.** An omitted `inventory`, `equipment` or
  `accessories` is the definition's kit. An explicit one replaces exactly that
  collection, including `[]` and `{}`.
- **No second kit, no shedding.** Explicit contents never gain a second kit and
  are never shed to fit capacity. The overloaded unit stays over capacity, and the
  strict player-transfer policy still refuses its next insertion
  (`receiver_full`).
- **Equipment kinds.** An equipment item whose kind the slot does not accept is
  `FieldRejected` with its contents, before anything is minted.
- **Owner-supplied sources are rejected when authored.** An authored modifier whose
  source the definition or a worn accessory already supplies is rejected
  (`FieldRejected`), and so is a second modifier from one source on one stat. Such
  an effect is rebuilt from its owner and is never stored twice.
- **Slot equipment adds no modifiers.** Its effects are read from the equipped
  instance where they apply.

### Completion and lifecycle

`installBuiltUnit` is one commit behind the page-incarnation fence, the spawn
handler's fence applied to the same two writes.

- **The commit.** It allocates the unit id. Then, inside the page-lifecycle lock
  and after re-checking the page's incarnation, it installs four things: the
  instance, its tags (`umTags`), its scenario reference (`umScenarioRefs`) and its
  simulation state.
- **Item identities.** The item trees' references and tags then attach through
  `Gameplay.Tags.Live.commitItemTreeAssignments`. If that refuses, the unit is
  retired again in one step.
- **The result.** `UnitInstalled uid ref` is returned only after all of that.
  `UnitRejected diagnostics` and `UnitNotInstalled ref (PageNotAdmitting |
  ReferenceHeld r | ItemsRefused ts)` leave nothing behind: no id is consumed, and
  no manager-only unit, simulation-only unit, reference, tag or item remains.
- **Faction.** The profile is `resolveSpawnFaction` over the definition's default
  tags with no spawn tag (the owner's decision), so a scenario acolyte has no
  controller.
- **Encounters.** An `encounter` is returned on the build for the location adapter
  (SCN-12), which owns the roster.

After completion, the paused hooks leave the baseline unchanged:

- `unit.recomputeBody` and `unit_resources.onSaveLoaded`;
- inspection, such as `unit.getStat` on a lazy unit.

The first real resource tick makes ordinary changes and never refills a stated pool.

### Capture

Capture is a pure read of a coherent snapshot. It advances nothing, heals and refills
nothing, rerolls no live stat, and draws no gameplay RNG.

**What it writes.** Every value the field table supports, explicitly:

- the stored stats;
- every skill, and the complete known knowledge;
- the wounds (`[]` for a healed unit), with `age = now − woundAt`, and the scars;
- blood, while it is Simulated, and the immune state;
- the current loadout through `Scenario.Items.Capture`. An emptied inventory is
  `[]`.

**Gaps and owned values.** A supported stat a lazy unit never resolved is written as
its deterministic fallback for the captured identity, and the live unit is not
touched. A modifier whose source the definition or a worn accessory supplies is
not written, and neither is an expired one.

**Excluded:**

- orders, AI jobs, paths and transitions;
- the pose, which is re-derived from health;
- render handles;
- the faction profile and physiology overrides;
- meters and mental episode state;
- encounter rosters, which belong to the location adapter;
- dead units.

**Names** follow the item capture's tier rule. Unit names are tier 1 for items, so a
unit's reference is never displaced by an item's.

- A unit built from `R` is captured as `R`, and an automatic path id `units[1]` as
  `units.1`.
- The referenced units are named before any item. Explicit references come before
  automatic ones, then they are ordered by raw reference, and a displaced name
  takes `-2`, `-3`, …
- An unreferenced unit is named `units.<k>` by its place in a structural order:
  `UnitSig`, a derived `Ord` over raw values with floats as bit patterns and item
  signatures. It is never a rendered string and never a unit id, and it is reserved
  after the items' references (`Scenario.Items.Capture.reserveDocumentId`).

### Durable identity (D-52)

`Unit.Types.Manager.umScenarioRefs` is the unit half of the scenario-id ↔ runtime-id
mapping.

- **Shape.** It is sparse and injective, and it is never a tag.
- **Lifetime.** It shares `umTags`'s lifetime: every removal path clears it in the
  same atomic modify.
- **Persistence.** The `units` component v6 persists it per page (`puScenarioRefs`,
  ascending by unit id). A v1–v5 payload carries none.
- **Validation.**
  - A malformed list fails `units` validation: an unadmissible text (the item
    references' vocabulary and 512-character bound), a unit listed twice, a
    reference claimed twice, or a list out of order.
  - A reference held by units on two pages is a `duplicate-scenario-reference`
    integrity error that refuses the load.
  - One naming no restored unit is a `dangling-reference` warning, and the restore
    drops it.
- **Fixture.** `z13-unit-scenario-refs` is the v6 fixture.

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

The `Scenario.Units` group (`test-headless/Test/Headless/Scenario/Units.hs`) covers unit
construction, installation and capture against the real engine. It uses:

- the shipped acolyte, plus a lazy, bodyless definition registered beside it;
- the real decoder, body, physiology, item and modifier authorities;
- the engine's own manager refs and page-lifecycle lock;
- the production Lua hooks.

Its fresh-process case re-runs the test binary twice as child processes. One is
plain. The other lists the units in another order, edits every tag, and advances the
gameplay stat RNG and both allocators first. The case compares the children's
canonical captures, and both children's outputs are kept at
`$TMPDIR/synarchy-scenario-units-fresh/{plain,perturbed}.json`:

```sh
cabal test synarchy-test-headless --test-options='--match "Scenario.Units"'
# one child alone (writes the canonical JSON to OUT):
SYNARCHY_SCENARIO_UNITS_CHILD='OUT|plain' \
  "$(cabal list-bin synarchy-test-headless)" --match "Scenario.Units/fresh process/child entry"
```

The `Scenario.Items` group (`test-headless/Test/Headless/Scenario/Items.hs`) covers
item construction and capture against the real materializer, owners, live attachment
boundary, decoder and ownership policy; `Gameplay.Tags.Items` proves the references
follow every move and die with every destruction the tags do.

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
