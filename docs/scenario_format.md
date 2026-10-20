# Scenario file format (v1, v2, v3)

Scenarios are reusable testing-arena setups (epic #2698). An author writes one as
YAML, and later the arena captures one from a running session (SCN-14). This
document is the authoring guide and contract for format version 1, defined by #2699
(SCN-01), and for version 2, which #2711 (SCN-11) added to give flora's physical
state a home (see [Flora, format v2](#flora-format-v2)), and for version 3, which #2712 (SCN-12) added so that structure pieces can name a pack variant and a location can bind its container shells (see [Placement construction and capture](#placement-construction-and-capture-scn-12-2712)). The decoder and validator live in `src/Scenario/`. The design rationale
is in [designs/predefined_arena_worlds_design.md](designs/predefined_arena_worlds_design.md),
in SCN-01 and the decisions it cites.

The scope of the schema slice is the representation and its validation. Nothing in
the schema constructs, replaces or captures a live world. Runtime construction,
finite-map enforcement and capture belong to the later SCN slices (construction:
SCN-09 to SCN-12; the session transaction that composes them:
[SCN-13](#loading-a-scenario-session-scn-13-2713); writing the arena back to a
file: [SCN-14](#capturing-a-scenario-session-scn-14-2714)). Those slices consume
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
  stays unbound in the decoded value; what construction does with an unbound slot is
  in [Locations](#locations).
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
| `camera.x`, `camera.y` | number, tiles | opt | `0` | Initial camera position only (SCN-13: the replacement session's camera and its page's). |
| `terrain`, `fluids`, `flora`, `structures`, `buildings`, `locations`, `units`, `ground_items` | list | opt | `[]` | One family each; a non-list is a field rejection. |

### `terrain[]` — column patches

| YAML | Type | Opt | Omitted | Runtime owner / construction |
|---|---|---|---|---|
| `rect` | `{x0, y0, x1, y1}` integers, inclusive | req (exactly one of `rect` / `tiles`) | — | Corners may be given in any order. |
| `tiles` | list of `[x, y]` integer pairs | req (exactly one of `rect` / `tiles`) | — | A malformed tile drops that tile, duplicates collapse, and an empty result rejects the patch. |
| `material` | material definition name | req | — | Column material (`World.Edit.Types` `WeSetCell`/`WeAddTile`); unknown → entry rejected. |
| `surface_z` | integer z | req | — | Top solid cell of every column in the patch. |
| `slope` | integer `[0, 15]` | opt | `0` (flat) | Ramp bitmask, bits N/E/S/W (`WeSetSlope`). Always recorded as an override, flat included: the engine derives border slopes from terrain and neighbour residency, and an omitted slope means flat, not "derived". |

Dependent values to reconstruct: column surface maps, vegetation placement and fluid
neighbour state (SCN-11). These are derived, not authorable. Construction and capture
rules: [World content construction and capture](#world-content-construction-and-capture-scn-11-2711).

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
| `species` | flora species name | req | — | `FloraSpecies` by name (`World.Flora.Reference`). Its category decides the representation (see below). |
| `x`, `y` | integer tile | req | — | Tile position. |
| `z` | integer | opt | Column surface (the tile's final terrain top) | `fiZ`. A plant stands on the column's top solid cell (the render and pick resolvers place it there whatever `fiZ` says), so an authored value other than that top is a field rejection and the plant takes the top; a crop plot has no z at all. |
| `age` | number ≥ 0, **game days** | opt | Deterministic default | The plant's TOTAL growth at the scenario's initialized clock, before the lifecycle wraps it into a generation and a dead window (not the stored baseline; see [the time basis](#flora-format-v2)). |
| `health` | number `[0, 1]` | opt | Full health (`1`) | `fiHealth` / `cpHealth`; scales the growth rate. |
| `offset_u`, `offset_v` | number `[-0.5, 0.5]`, tiles (**v2**) | opt | Deterministic hash of the scenario id | `fiOffU`, `fiOffV`. Not applicable to a crop plot. |
| `variant` | integer `[0, 3]` (**v2**) | opt | Deterministic hash of the scenario id | `fiVariant`. Not applicable to a crop plot. |
| `regrowth` | number ≥ 0, **game seconds** (**v2**) | opt | Harvestable now | Remaining regrowth, `World.Flora.Harvest.FloraHarvests`. Only a harvestable occurrence accepts it; `0` is an explicit "harvestable now". Not applicable to a crop plot (its harvest deletes the plot). |

Derived: base width (the species' footprint), phase and stage textures, and the flora
instance id (allocated at construction from the page's existing planted allocator).

<a id="flora-format-v2"></a>
#### Flora, format v2

Format version 2 (#2711, the owner's choice "Format v2 (Recommended)" on 2026-10-19)
adds four optional `flora[]` fields — `offset_u`, `offset_v`, `variant`, `regrowth` —
because the physical plant state a capture must write and a materialization must
restore (D-12) includes values v1 called derived. The change is additive. The v1 → v2
migration step is the identity, the v1 fixture texts stay frozen, and the decoder keeps
v1's vocabulary closed by the document's **declared source version**: a version-1 file
that names a v2 field still reports it as `UnknownField`.

- **Representation follows the species.** A groundcover crop species
  (`worldGen.category: groundcover_crop`) becomes a tile-keyed crop plot
  (`World.Flora.CropPlot`); every other species becomes an occurrence — a wild plant
  or a planted row crop — with its own `FloraInstanceId`. There is no selector field.
  One crop plot per tile: a second one is rejected as an entry. Several occurrences may
  share a tile.
- **Identity.** An occurrence takes its id from the page's EXISTING planted allocator
  (`wpsPlantedFloraCursor`) in the authored order of the accepted entries, and the
  advanced cursor is committed with it. No scenario reference for a plant or a terrain
  patch is persisted: nothing references them, capture names them structurally, and
  ordinary saves carry the plants themselves.
- **Time basis — units are part of the contract.** `age` is **game days**, and it is
  the plant's TOTAL growth *at the initialized clock*: `fiAge + day · growthRate(health)`
  (`World.Flora.Growth`), the value the lifecycle starts from, before it wraps it into a
  generation and a dead window. Exporting the wrapped display age instead would lose the
  lifecycle (a dead plant would come back living), whereas the total, run through the
  same hash-derived lifespan, reproduces the generation, the dead window and the phase
  exactly. Construction stores the baseline `fiAge = age − day · rate` (searched to the
  last bit so the derived total is the authored one whenever the float grid at that
  clock can say it — always at day 0 — and its nearest neighbour otherwise; a derived
  value is on the grid and round-trips exactly), and capture exports the derived total,
  never `fiAge` or the planted day. A crop plot stores a planted **day** and derives
  `age = elapsedDays · rate`, so its age lands on the nearest whole elapsed day.
  `regrowth` is **game seconds remaining**, counted down by the existing regrowth tick;
  it needs no clock conversion. The scenario file has no clock of its own (see the v1
  owner decisions).
- **Defaults.** An omitted `age`, `offset_u`, `offset_v` or `variant` is what the world
  generator's own producer (`World.Flora.Placement.mkInstance`) makes from a private
  hash of the scenario id — never gameplay randomness, never an allocation number.
  An omitted `health` is full health; an omitted `regrowth` is "harvestable now".
  Authored values, zero included, are authoritative.
- **Rejections.** An unknown species, an out-of-bounds plant (never relocated), or a
  second crop plot on a tile rejects the entry. An out-of-domain value, a field that
  does not apply to a crop plot, and `regrowth` on a species with no harvest block
  reject only that field.
- **Placed directly.** A plant is a physical baseline, so it is placed through the
  planting edit primitives themselves (`WePlaceFloraWithId` + `WeSetFloraState`) — no
  planting designation, no tilled-soil requirement and no climate or soil suitability
  check, which belong to the farming and worldgen paths. Its default z is the column's
  top solid cell, where the render and pick resolvers put it.
- **Chop state is not scenario state.** Construction leaves `wpsChopDesignations` empty
  and every constructed instance's `fiChopDesignated` clear; capture ignores both.

### `structures[]` — structure pieces

| YAML | Type | Opt | Omitted | Runtime owner / construction |
|---|---|---|---|---|
| `pack` | structure pack name | req | — | `data/structure_packs/<pack>.yaml`. |
| `piece` | `floor`, `ceiling`, `wall_ne`, `wall_nw`, `wall_se`, `wall_sw`, `post_n`, `post_e`, `post_s`, `post_w`, `wire` | req | — | `Structure.Types.StructureSlot`; the pack must supply that piece kind. |
| `x`, `y` | integer tile | req | — | Tile position. |
| `z` | integer | opt | Derived level (see [Structure pieces](#structure-pieces)) | Piece z (`WeSetStructure`). |
| `variant` | pack variant name (format v3) | opt | The pack's default art | The pack's named art variant (`damaged`, `weathered`, `broken`, `ruined`, …) the piece is drawn with. The name is validated against the registered pack at decode time. Wall cap and wire connection variants are derived, never authored. |

Derived: texture and facemap palette ids, and wire connection variants. `tags` are
accepted and dropped with a `FieldRejected` diagnostic: a structure piece has no
gameplay-tag owner.

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
| `significant_items` | map `slot → item id` | opt | Constructed: every slot gets the definition's own guaranteed item (see [Locations](#locations)); `{}` = none owed | Binds `liSignificant` slot `n` (1-based, at most the definition's count) to that item's physical identity. This is an optional reference. Slot keys are plain decimals (`"1"`, never `"01"`), so two keys cannot name one slot. The target must be an item of the definition that slot requires (`lsiItemDefName`); a mismatch drops the binding with `DefinitionMismatch`. |

| `containers` | map `slot → {item, realized, profile}` (format v3) | opt | Constructed: every slot gets its pending shell (see [Locations](#locations)); `{}` = no shell bound | Binds container slot `n` (1-based, numbered over the definition's `container` entries alone) to a physical shell. `item` is an optional item reference to an item of the definition that slot mints (a mismatch drops the binding with `DefinitionMismatch`, an unrejected target that did not survive with `RejectedReference`). `realized` is optional and defaults to `false`: a PENDING slot keeps its loot profile (`profile`, else the definition's) and nothing is drawn, a REALIZED slot is latched with its profile discarded, its contents being exactly the stated tree. `profile` is allowed only on a pending slot. Capture never opens a shell and never rolls loot. |
| `incidental_content` | boolean (format v3) | opt | Constructed: the definition's fixed non-significant items and loot-table rolls (see [Locations](#locations)) | `false` = none owed: whatever lies in the location is the document's own `ground_items[]`. `true` is the same as omitted. |

Derived or excluded: display name, gloss and etymology are derived. The encounter's
roster is exactly the units that join it by `encounter` (see
[Locations](#locations)); the instance id is allocated. Lifecycle and discovery,
encounter progress, the taken latch and the clearance notice are player and
expedition progress, and are excluded by D-3.

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
  `lean_mass + fat_mass ≤ body_mass` and `fat_mass` at least its floor
  (`0.02 × frame_mass`, the floor every starvation pass clamps a body up to), or
  state none of them. A partial or incoherent set rejects each stated mass
  (`FieldRejected InvalidValue`), and the unit takes the seed path. A capture can
  never hold fat below the floor, so an authored body below it is rejected, never
  clamped (manager ruling, 2026-10-18). A lean mass at or below its own floor is a
  death condition instead, and the unit is rejected whole.
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
9. **Blood and blood-loss pose.** Blood falls back to `canonicalBlood`, else the
   body-mass seed. The blood-loss collapse is the wound tick's own verdict over
   zero elapsed time (`Combat.Wounds.Tick.tickOneUnitWith`, its unit discarded).
   Nothing drains, heals or rolls.
10. **Paused readiness: a pure evaluation of the authored state.** This step
    runs at install, before completion (`Scenario.Units.Install.uirReady`;
    production `Scenario.Units.Ready`, on the Lua thread).
    `unit_resources.settleScenarioUnit` runs the ONE per-unit reconciliation
    the resource tick itself runs, `unitResources.reconcileUnit`, once at zero
    elapsed time. It covers stance, cardio, the system ticks, injuries and
    their lethality, failure meters, every pool's death, kill-on-zero and
    collapse thresholds, starvation's floors, the calorie strength discount,
    and revival.

    - **Every mutation goes through one seam.** `scripts/unit_effects.lua`
      carries all of them: stat writes, body recomputes, the stamina and
      physio commits, the mode verbs, frostbite, stance recovery, the
      equipment drop, the combat engagement, the four pose verbs, alerts and
      events. In production each one is the existing verb, so the tick is
      unchanged.
    - **Nothing is performed during the pass.** For this unit and this call,
      the seam records instead of acting. Stat writes are recorded. The two
      commits answer from read-only native previews, `unit.previewStamina` and
      `unit.previewPhysio`, which run the commits' own pure steps without
      publishing. Frostbite, the drop, stance recovery, the engagement and the
      mode verbs are refused. Alerts and events are not emitted.
    - **What happens afterwards.** Only two things are applied:
      - the recorded writes to stats the unit does not author: the strength
        discount and the failure meters;
      - the verdict: a death rejects the unit, retired through the destroy
        step; a pose is established with the `UnitCollapse` / `UnitCrawl` /
        `UnitRevive` handlers.
    - **Authored values are never changed by readiness.** Frostbite in a cold
      arena and a disabled hand dropping its weapon happen at the first real
      tick, as ordinary gameplay.
    - **Static check.** A static check in the `Scenario.Units` group fails if a
      readiness module calls a mutating engine verb other than through the
      seam, calls an unclassified verb, or captures a namespace or verb in a
      local. The physiology access audit judges a call through the seam as
      the direct verb it forwards to.
    - **What it skips.** The mind trio (`brain`, `thoughts`, `mental`): over
      zero time it changes nothing, and `thoughts` would draw the gameplay
      random stream when a thought is due.
    - **Hook failure.** A failing hook retires the unit (`ReadinessFailed`).
    - **Ordering.** The step is idempotent and must run after any paused hook
      that resets strength: `unit.recomputeBody`, like every ordinary load,
      re-derives `strength` from the body.

**Pools are seeded natively.** The trusted issue review called resource pools
Lua-owned. Since the physiology epic (#2831, #2834, #2837), every pool belongs to a
registry system. Spawn and load seed it natively, and the Lua tick raises on a
missing Simulated pool rather than refilling it. The adapter therefore calls that
native authority, under the review's own "one shared authority it calls" clause
(manager ruling, 2026-10-18). Nothing Lua-owned is outstanding at completion.

**Death conditions.** A unit whose stated health already meets an existing death
condition is rejected whole, with what it owns (`EntryRejected InvalidValue`). It is
never built as a corpse. Each condition is judged by the authority that would apply
it in gameplay, never by a restated threshold:

- **Inflicted vital severity.** An inflicted severity of 1 or more on a vital
  part, by the combat rule `Combat.Resolution.Common.isAlreadyDead`.
- **Gangrene.** The wound tick's own `DiedNow` verdict over zero elapsed time.
- **Gameplay lethality.** The rule the injury pass kills by,
  `injuries.lethalCause`, over the gameplay-visible wound view
  (`severity × (1 − heal)`). The paused-readiness step checks it. For example,
  a heart stab of severity 0.8 healing backwards (`heal: -0.5`) reads 1.2 there
  and is rejected.
- **Blood.** `blood ≤ 0` while blood is Simulated.

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
  `Gameplay.Tags.Live.commitItemTreeAssignments`.
- **Retirement.** If the attachment or the readiness step refuses, the unit is
  retired by the unit-destroy command's own step
  (`Unit.Thread.Command.Lifecycle.destroyedUnit`). Every item it held is
  released by the epoch-guarded item-destruction cleanup (`withDestroyedItems`),
  so no item tag or reference outlives it, and a retry of the same entry
  installs.
- **The result.** `UnitInstalled uid ref` is returned only after all of that.
  `UnitRejected diagnostics` and `UnitNotInstalled ref (PageNotAdmitting |
  ReferenceHeld r | ItemsRefused ts | ReadinessFailed e)` leave nothing behind.
  A gameplay-lethal answer from readiness is reported as `UnitRejected`, like the
  native death checks. Either way, nothing is left behind: no id is consumed, and
  no manager-only unit, simulation-only unit, reference, tag or item remains.
- **Faction.** The profile is `resolveSpawnFaction` over the definition's default
  tags with no spawn tag (the owner's decision), so a scenario acolyte has no
  controller.
- **Encounters.** An `encounter` is returned on the build for the location adapter
  (SCN-12), which owns the roster.

The Hspec group checks this over 160 generated specs. They mix exhausted, dehydrated
and starved pools, low endurance, bodies at and below the muscle and fat floors, and
concussion, fracture, bleeding, worsening vital and disabled-hand wounds, with and
without a tagged, referenced dagger in the disabled hand, and with and without a
cold arena. The checks:

- **Accepted entries.** Every authored value is bit-identical after readiness; only
  the pose may differ.
- **Rejected entries.** They leave no unit, item, tag, reference or ground item.
- **Verdicts.** Death, pose and strength discount match the ordinary path: an
  ordinarily spawned unit in the same state, after the wound tick and
  `reconcileUnit` at zero time, with the queued commands applied. A queued kill
  counts as the death. The cold specs are left out of this comparison, because
  the ordinary pass applies the environment's zero-time frostbite and readiness,
  by design, never does.

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
  adding a field: an older build reports a new field as unknown. Version 2 (#2711)
  is the first such step: `flora[]` gained `offset_u`, `offset_v`, `variant` and
  `regrowth`, the v1 → v2 migration is the identity, and a version-1 document keeps
  its closed vocabulary (the decoder gates the new fields on the declared source
  version). Version 3 (#2712) is the second: `structures[]` gained `variant`,
  `locations[]` gained `containers`, the v2 → v3 migration is the identity, and a
  version-1 or version-2 document keeps its vocabulary.
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

## Owner decisions recorded for v2

The #2711 design found that the v1 schema could not express the physical plant state
its adapters must restore (sub-tile offsets, visual variant, harvest regrowth), and
that adding it is a format decision. The owner decided on 2026-10-19, directly in the
manager pane: **"Format v2 (Recommended)"**, approving the proposal as written —
an additive format v2 with an identity v1 → v2 migration and frozen v1 fixtures;
optional `offset_u`, `offset_v`, `variant` and `regrowth` on `flora[]`; omitted offsets
and variant a deterministic hash of the scenario id; a unit and clock-basis table;
representation by species category with no selector; one appended durable edit
constructor (`WeSetFloraState`, `world-edits` v6); base width stays derived. The same
decision confirmed that every authored occurrence uses the existing planted allocator
and that no flora or terrain scenario references are persisted.

## Owner decisions recorded for v3

The #2712 design raised four decisions the issue left open. The owner decided on
2026-10-19, relayed by the project manager, approving each recommendation as
written. Each is recorded with the owner's own words.

1. **"v3 as proposed (Recommended)".** An additive format v3 with an identity
   v2 → v3 migration and frozen v1 and v2 fixtures. `locations[].containers` maps a
   slot to `{item, realized (default false), profile (pending only)}`, under the
   same optional-reference rules as `significant_items` (a binding that cannot hold
   drops only itself). Capture never opens or rolls a shell.
2. **"Add structures[].variant (Recommended)".** An optional pack variant name in the
   same v3; omitted means the pack's default art.
3. **"No extra units (Recommended)".** Every scenario building is stored with
   `biSpawnRemaining = 0`; scenario units come only from `units[]`. Requirement 7
   of the issue wins over the trusted spec amendment's roster expansion.
4. **"Confirm (Recommended)".** Encounter occupants are exactly the units that join
   by `encounter`; zero joined units is an empty ruin; a unit that joins a
   definition with no encounter entry is rejected with a diagnostic
   (`NoEncounter`).

## World content construction and capture (SCN-11, #2711)

`Scenario.World.*` constructs and recaptures the terrain, fluid and flora a scenario
states. It is pure up to one commit.

| Module | Role |
|---|---|
| `Scenario.World.Terrain` | `buildTerrain`, `buildFluids`: patches to durable edits. |
| `Scenario.World.Flora` | `buildFlora`: occurrences, crop plots, regrowth timers, planted ids. |
| `Scenario.World.Build` | `buildWorldContent`: the three families, their per-chunk edit log, tag memberships and the scenario-id → runtime-target mapping (`WorldRefs`). |
| `Scenario.World.Install` | `requestWorldInstall` / the world-thread commit (`WorldInstallScenarioContent`). |
| `Scenario.World.Capture` | `captureWorldContent`: a coherent snapshot back to `terrain[]`, `fluids[]`, `flora[]`. |

### Terrain

- A patch gives every admitted tile a column whose **top solid cell** is `surface_z`
  in the named material with the authored slope. Relative to the arena's unedited
  column (sixteen cells, loam over granite, top at sea level) that is a raised pad of
  the material, a column dug down to the surface with the top cell set, or the top
  cell replaced. The cells the base column already has stay. A column that already has
  the stated shape produces no shape edit and keeps the base's grass (its slope
  override is still recorded); any column a shape edit changes carries no surface
  vegetation (vegetation is derived).
- **Overlap.** Patches apply in authored order and a later one replaces the column
  state of every tile it covers. Edits are built from each tile's FINAL state, so an
  earlier patch's relative raise never stacks under a later one.
- **Limits.** `surface_z` must lie in `[sea level − 15, sea level + 256]` (a column
  cannot grow downward below its sixteenth cell); a family may ask for at most
  1,048,576 tiles; and the terrain may change at most 4,194,304 cells in all (a tile
  costs one plus the z it raises or digs, so a tall raise over a wide region is bounded
  too). A patch beyond any limit is rejected whole with a diagnostic. `air` is not a
  terrain material: excavation is a lower `surface_z`.
- **Bounds.** Regions are clipped tile by tile against the page's own bounds
  (`World.Arena.Bounds.clipTileRegion`, partial chunks included): the admitted tiles
  are kept with one `TilesClipped` diagnostic; a patch with none is rejected.
- **Tags.** A terrain patch's tags label the surviving top cell `(x, y, surface_z)` of
  each tile, recorded after the cell is known to exist; a tag whose cell a later
  patch lowered is reported, never invented.

### Fluid

A tile is wet when the patch's exact plane is above the tile's **final** terrain top
(`exactSurface > 8 · top`), and records an exact `WeSetFluidSnapshot` — never rounded
to a z or a full cell. A plane more than 65,535 exact units above any admitted tile's
terrain (the simulation's `Word16` volume range, beyond which it would clamp and lose
fluid) rejects the patch whole, with overflow-safe arithmetic. A tile the terrain changed that no patch wets is cleared
EXPLICITLY (`WeClearFluidSnapshot`): the replay of a dug column at or below the water
table would otherwise reveal groundwater, and an authored absence is authoritative.
A fluid patch has no tag owner, so its `tags` are rejected as a field. Authored fluid
reaches the simulation through the page's own `SimChunkEdited` re-seed
(`syncEditToSim`); nothing advances the simulation, and the finite edge stays closed.

### Commit and replacement

`installWorldContent` runs on the world thread, after checking the page incarnation
and that the page is an ARENA page (a generated world's chunks are not rebuilt from the
arena base, so a materialization would destroy them: the answer is
`WorldInstallNotArena` and nothing changes).
A materialization REPLACES the page's terrain, fluid, plant-occurrence and crop-plot
state — in the edit log every entry except structure pieces (`WeSetStructure` /
`WeClearStructure`), which keep their relative order — so a second materialization or a
recapture accumulates nothing. Every plant the replaced content placed is forgotten
(regrowth timer, tags, chop designation), resident or not. It assumes a quiescent page:
ground items, units and spoil already standing there are not repositioned. EVERY resident chunk is rebuilt (not only those the log names: the simulation's
writebacks put fluid into chunks with no entry of their own) exactly as an admission
rebuilds them (arena base, replayed log, rectangle re-asserted) and swapped in through
`replaceChunkForgettingFlora`; a non-resident chunk gets only the log and is rebuilt by
its own admission. A tagged cell in a chunk that is not resident is judged against the
replacement geometry at once: a cell the replacement no longer has loses its tags (a
cell created there later inherits nothing), one that survives keeps them. Slope
overrides (`WeSetSlope`, flat included) are restored after the admission pass that
recomputes border slopes from terrain (`applySlopeEditsTd`): the EFFECTIVE value of each
named cell is read off a replay of the whole log over the loader's own base — so a later
cell write, or a delete and refill, that cleared a slope is respected — and a ramp or an
explicit flat on a chunk-edge tile survives the chunk's admission and a neighbour's.

### Capture

Capture reads one coherent snapshot (the page's live edit log, resident chunks, crop
plots, regrowth timers, tag assignments) with no engine handle; the caller takes it after
the save barrier has reconciled the simulation with the world, so it never mixes a newer
simulation grid with stale chunks. It writes every
supported current value explicitly: a column's top (surface z, material, AUTHORED slope), each
wet tile's kind and exact eighth-z plane, and each plant's species, tile, z, derived
total age (see the time basis), health, offsets, variant and regrowth (an explicit `0` for a harvestable
plant whose timer ran out). A column equal to the base is not a patch unless it carries
tags; a wet cell whose plane does not rise above its terrain top is dry; an occurrence
outside the planted id namespace is generated flora the world regenerates, not an
authored one. A non-resident chunk is read by regenerating the base and replaying its
durable edits — non-residency is never deletion. **Slopes: authored only.** A live chunk's slope also carries DERIVED bits (the
border-strip recompute after an admission, which depends on which neighbours are
resident, and mid-dig masks), which are not scenario state; capture reads the slope
the durable log records for the column's top cell (`WeSetSlope`), else flat. Tags on a cell below the surface, a
column whose top material has no registered name, and a column with no solid cell are
outside the vocabulary and come back as diagnostics (`terrain.tags`, `terrain.material`,
`terrain.columns`) rather than being written wrongly. A tile is captured when its shape differs from the base, when it carries tags, OR when the
durable log holds a slope override for its top cell — an explicit flat on a base-shaped tile
included, since without it admission would derive a ramp there. Entries are written under
generated explicit ids that depend only on the content (family, values, tile, and an
ordinal among same-species, same-tile siblings, ranked by the whole exported entry — species, tile, z, offsets, variant, health, age, regrowth and tags — as raw values under one derived `Ord`, so no input order can change the result), made unique against the ids of
the document's other entries.

### Persistence

`world-edits` v6 appends one edit constructor, `WeSetFloraState` (tile, plant id, z,
offsets, variant, health, stored age baseline), written after the planting edit
(`WePlaceFloraRef` on disk) it follows; `migrateWorldEditsV5` crosses every older
payload verbatim. Crop plots and regrowth timers already persist in `world-activity`; the planted-id
cursor and the tile, occurrence and plot tags in `world-edits`. The `z16-scenario-world` fixture pins the
whole set. See [persistence_state_inventory.md](persistence_state_inventory.md).

## Placement construction and capture (SCN-12, #2712)

`Scenario.Placement.*` constructs and recaptures the structure pieces, buildings and
real placed locations a scenario states, through the runtime's own owners. Each adapter
is pure up to one commit.

| Module | Role |
|---|---|
| `Scenario.Placement.Catalog` | The decoder's catalog of building definitions, location definitions (with their container slots, loot profiles and encounter), pack variants. |
| `Scenario.Placement.Structure` | `buildStructures` / `captureStructures`: pieces to `WeSetStructure` edits and back. |
| `Scenario.Placement.Building` | `buildBuildings`, `installBuildingBuilds`, `captureBuildings`. |
| `Scenario.Placement.Location` | `validateLocations`, `resolveLocationInstalls`, `applyLocationInstalls`, `captureLocations`. |
| `Scenario.Placement.Install` | `requestPlacementInstall` / the world-thread commit (`WorldInstallScenarioPlacement`). |
| `Scenario.Placement.Encode` | The YAML values of the three entry families. |

The orchestration order is fixed by the dependencies: buildings are judged against the
real terrain; locations are validated, and the units that would join a rejected
location are excluded before they are built; units and ground items are installed;
the locations are resolved against what actually installed, and the structures and
locations are committed on the world thread in one request. Composing the product
load is SCN-13's; the `Scenario.Placement` tests drive this order end to end.

### Structure pieces

- **Levels.** An omitted `z` is derived from the column's terrain surface: floor, wall
  and wire at `surface + 1`, ceiling at `surface + 2`, and a post at the level of the
  floor on its tile, else `surface + 1`. Every captured piece states its level.
- **Finite bounds.** A piece whose tile is outside the finite map is rejected
  (`EntryRejected`). The same tile and slot stated twice keeps the later entry and
  reports the replaced one.
- **Commit.** The materialization establishes the page's structure baseline: it
  replaces every structure edit on the page, interns the art paths in the palette only
  once the page admits the request, rebuilds each resident chunk's overlay through
  the ordinary replay, clears the staged write-ahead cache and revalidates
  construction designations. A removed ruin's geometry stays absent after chunk
  eviction and readmission because the durable edit log is the authority.
- **Capture.** A placed piece stores palette ids and a level, so its art identity is
  the (sprite, facemap) pair they resolve to. Capture inverts that exactly:
  `Structure.ArtCatalog.placedArtPairs` enumerates every pair the registered packs can
  place, and a piece resolves to the pack and variant that places its pair for its
  slot's kind. Art that renders identically under several claimants (a variant that
  inherits the default's sprite and facemap) resolves to the default when it is one of
  them, otherwise to the least pack and variant, so the document is one fixed
  spelling of the same rendering. A sprite no registered pack places for that slot is
  reported and the piece skipped. Variant facemaps are registered by
  `structure.registerPackArt` (`variantFaces`), since a variant's facemap is not
  otherwise in the art catalog.

### Buildings

- **Footprint.** The whole footprint is judged by `Building.Placement.canPlaceAt`
  against the real terrain, with the finite-map rectangle enforced including partial
  chunks, and against the buildings already accepted from the same document (a
  rejected building leaves no trace in the occupancy later entries are judged
  against). A starting building (`bdIsStarting`) is rejected only inside a placed
  location's bounds.
- **Level.** A building stands on the ground: an explicit `z` that differs from the
  terrain surface at the anchor is dropped with `FieldRejected`.
- **State.** `build_progress`, `materials_delivered`, `storage` (nested, explicitly
  empty and overloaded trees alike) and `power_charge` are restored exactly. Every
  scenario building is stored with `biSpawnRemaining = 0`, never the `-1` "seed the
  roster" sentinel, so the building-spawn sequencer neither seeds nor spawns a unit;
  scenario units come only from `units[]` (owner decision 3).
- **Rejection.** A rejected building leaves no instance, no reservation, no power
  node, no tag and no reference. Its owned items are rejected with it
  (`OwnerRejected` / `CascadeRejected`), and an optional reference to one drops with
  `RejectedReference`.
- **Commit.** Under the page-lifecycle lock and the page's incarnation fence: the
  footprint is reserved and committed, the instance, tags and scenario reference are
  written in one modification of the building manager, the power node is minted with
  its charge, and the item trees take their references and tags. A failure retires
  the building again.
- **Capture.** A building built from a reference is captured as it; any other is named
  `buildings.<definition>.<x>.<y>`. `power_charge` is read from the node.

### Locations

- **A location is an instance.** Each accepted entry adds one
  `Location.Instance.LocationInstance` at the authored anchor tile. Two instances may
  share a chunk and a footprint may span several. Scenario locations never enter the
  world-generation overlay, so the stamper's every-load dispatch cannot re-stamp or
  re-spawn them; the geometry and contents are the scenario's own `structures[]` and
  `units[]`, `buildings[]` and `ground_items[]`.
- **Finite bounds.** The whole footprint (`liBounds` around the anchor) must be inside
  the finite map. A rejected location leaves no instance and no id, and every unit
  whose `encounter` names it is rejected with it.
- **Encounter.** The roster is exactly the units installed with `encounter` naming the
  location: its rolled count is the number of joined units, its roster is complete,
  and nothing is rolled or spawned. Zero joined units is an empty ruin, cleared as an
  ordinary roll of zero is. A unit that joins a definition with no encounter entry is
  rejected at decode (`NoEncounter`, owner decision 4).
- **Completion state is rebuilt.** A fresh instance is `LifecycleUnknown` with its
  clearance notice seated, computed LAST against the final instance (a location already
  satisfied when initialization completes — nothing owed and no living occupant, or every
  owed item already taken — starts with the notice spent, so discovering it announces
  nothing). The anchor chunk's geometry marker (`wgpLocationStamped`) is set by the
  commit, and a scenario location never enters the overlay, so neither the stamper nor
  `spawnContents` can add a copy. A paused initialization advances nothing and realizes
  no pending loot.
- **Omitted versus stated content.** `significant_items`, `containers` and
  `incidental_content` are tri-state (manager ruling, 2026-10-19: omitted keys expand ALL
  definition-provided non-unit content). When a key is OMITTED, the definition-provided
  content it governs is constructed once: each significant slot gets a default item of the
  slot's definition (`significant_items`); each container slot its pending shell, with
  the definition's profile and nothing drawn (`containers`); every fixed non-significant
  item entry is placed and every `loot_table` entry rolled (`incidental_content`). Rolls
  use PRIVATE scenario randomness — the very draw an ordinary placement makes
  (`rollLootTableFor`) over a context folded from the location's scenario reference, the
  entry's 1-based position and the roll number, never gameplay RNG. Every constructed item
  is a ground item at a deterministic private tile of the footprint (the entry's fixed
  offset from the anchor when it has one, else a stable hash of the location's reference
  and the slot or roll) under an automatic reference `ground_items[n]` — the first index no
  surviving ground item (nested trees included) already holds, so a rejected earlier
  sibling's gap is never reused by two items. Definition-provided buildings are never constructed (owner
  decision 3) and units follow the encounter rule (owner decision 4). A rejected location
  constructs nothing. When a key is PRESENT, even as `{}` or `false`, it is exactly what is
  stated: an unbound significant slot is dropped (nothing would ever fill it, and the
  location could never clear), after which the content-spawn flag (`liContentsSpawned`) is
  set because every remaining slot is bound, the only state a saved table may hold with
  the flag set; an unbound shell slot stays unbound. Capture writes all three keys, so a
  recapture never expands again and gains no second copy.
- **Bindings.** `significant_items` and `containers` are optional references resolved
  against what installed; a missing, rejected or wrong-definition target drops only
  that binding (`RejectedReference` / `DefinitionMismatch`); the physical item stays
  where it is. Ownership is judged as the save's integrity rules judge it: a significant
  item that lies on the ground as its own root is bound untaken, one held anywhere else
  (a pack, a building's storage, inside another item) is bound as taken, and a PENDING
  container shell that is not such a ground item is refused (`InvalidValue`) because
  play can never reach it; a realized shell may be anywhere. Every write goes through
  the instance owner's own functions, so every rule a decoded table must satisfy binds
  this path too.
- **Capture.** Every referenced unit, building and location is named together,
  document-wide (`nameDocumentRefs`: an automatic reference such as `buildings[0]` is
  written in its bracketless form `buildings.0`, a clash takes the first free `-2`, `-3`,
  … and another entry's id outranks an item's reference), and every item reference is
  reserved up front, so identities are stable across captures and never illegal ids. The
  table is read as it is: tags, slot bindings, and each container slot
  as pending (with its profile) or realized. Occupants write `encounter` on their unit
  entry. Capture order is items, buildings, locations, units, structures.

### Persistence

Scenario identity survives an ordinary save in two existing components, each with a
frozen previous-version type and a migration: `buildings` v4 carries
`bmScenarioRefs` (`pbScenarioRefs`) and `world-pages` v16 carries the location
references (`pcLocationScenarioRefs`, rebuilt into `lisScenarioRefs`). Integrity
refuses a reference two buildings or two locations hold, and a building and a
location reference that collide, and warns about a building reference that names
no building. The
`Scenario.Placement` group saves a materialized page through the real envelope,
decodes and stages it, and a capture of the restored state equals the original
without the source document. See
[persistence_state_inventory.md](persistence_state_inventory.md).

## Loading a scenario session (SCN-13, #2713)

`Scenario.Load.Service` loads a decoded scenario as one paused session
transaction. The exclusion, status, failure and recovery contract is in
[engine_contracts.md](engine_contracts.md#scenario-load-the-paused-session-transaction-2713);
this section is what the FORMAT means to it.

- **What a source is.** A path, or a document held in memory under a name. The
  loader reads it and never writes it: not on success, not on failure, not
  while migrating (D-24).
- **`map` and `camera`.** `map` decides the replacement arena's bounds (D-9): a
  finite rectangle, or the expandable arena when omitted. `camera` is the
  initial camera position of the replacement session and of its page (D-21); it
  translates neither the bounds nor any object.
- **What replaces what.** A scenario replaces the TESTING session whole (D-11):
  every page, unit, building, item, tag, order, reference, allocator and clock of
  the previous one is gone, and the new session starts at the session epoch with
  fresh allocators. Runtime ids may therefore be reused; nothing queued against
  the old ids reaches the new ones. A live session that holds an ordinary
  gameplay page is never replaced.
- **Order of construction.** Terrain, fluids and flora; then buildings (judged
  against that terrain); then locations, with the units that would join a
  rejected one excluded first; then units and ground items (the omitted default
  content of the accepted locations among them); then structures and locations
  in one commit. This is the order the SCN-09..12 sections document, composed.
- **Warnings.** Every rejection (`ScenarioDiagnostic`) found by the decoder or by
  any adapter is kept in the request's result in the order found, with its path,
  reason and effect. A document with rejections loads its retained subset and is
  ready WITH those warnings; an automated caller compares the list with the one
  it expects and fails setup on any other (D-15).
- **Omitted `z` of a unit** is the column surface of the terrain the scenario
  itself installed, read after the terrain and before the units.
- **Paths.** A diagnostic names an entry by the path it had in the DOCUMENT
  (`buildings[2]`), not by its index among the entries that survived decoding:
  `Scenario.Schema.loadScenarioFilePaths` returns each family's source paths
  beside the scenario (`Scenario.Types.SourcePaths`, not part of a scenario's
  identity), and the adapters report at them. The same holds for NESTED lists
  (a building's `storage`, an item's `contents`, a unit's `inventory`,
  `wounds`, one stat's `modifiers`): the decoder records which source
  positions of each survived (`spSurvivors`), the adapters number the
  survivors, and `Scenario.Types.translateNested` rewrites each adapter
  diagnostic back to the positions the author wrote (an item behind a
  rejected first sibling is reported at `storage[1]`, not `storage[0]`). A rejection at the decoder
  (`buildings[0].definition`) followed by an adapter's rejection of the
  overlapping third building reads `buildings[2]`, never `buildings[1]`. The
  default content a location constructs is not in the document and is labelled
  `ground_items(default)[j]`.
- **Carried items bind too.** A location's `significant_items` / `containers`
  reference may name an item an installed unit carries (inventory, equipment,
  accessories and what each holds), as it may a ground item or a building's
  contents: a carried significant item binds as taken and a carried REALIZED
  container is a legitimate shell. A pending shell must still be an outer
  ground item (the save's own provenance rule).
- **Saving the loaded session.** An ordinary save of the session carries the
  bounds, the tags, every scenario reference and every physical value (see each
  family's Persistence section); loading that save needs no scenario file, and
  reapplies no default and rerolls nothing.

## Capturing a scenario session (SCN-14, #2714)

The capture service (`Scenario.Capture.Service`; contract in
[engine_contracts.md](engine_contracts.md#scenario-capture-the-coherent-snapshot-and-the-safe-file-write-2714))
writes the live testing arena as a scenario document in the CURRENT format, from
one coherent snapshot, and leaves the arena paused. It composes the per-family
capture adapters described under each family above; this section is what the
whole document looks like and how it relates to the one you wrote.

**Every supported current value is written, explicitly (D-12).** A handwritten
file may omit values and let deterministic defaults fill the gaps; a captured one
states them all, so loading it never rolls anything and a later capture never
depends on a default that changed. Zero, empty and explicit-absence values are
authoritative: a healed wound is `wounds: []`, an emptied container is
`contents: []`, a dry cell is no fluid patch, a removed structure is no piece. No
old override is merged in and no current value is replaced by a definition
default. A recapture therefore REPLACES what you wrote.

**Shape of the document.** `version` (the current one), `map` (only when the
arena is finite), `camera` (where it stands: the live camera when the arena owns
it, the page's own otherwise), then the eight families in the decoder's order —
`terrain`, `fluids`, `flora`, `structures`, `buildings`, `locations`, `units`,
`ground_items` — each always present, `[]` when empty. An entry's own keys come in
the order a person writes them (`id`, `tags`, the definition, the position, then
the physical values); every other key is alphabetical, and lists keep the order
the capture produced, which is structural (never an allocation number or a
hash-map order) and keeps nested item and content order. Two captures of the same
arena are byte-identical. Because a YAML 1.1 reader would take a bare `y` for a
boolean, the encoder writes the key as `'y'`; both spellings read the same.

**Identities.** Every entry is written with an explicit `id`. An object loaded
from a scenario keeps the id it was loaded under (the ordinary save carries it,
so an arena restored from an ordinary save captures to the same ids). An object
made by hand has none and is named structurally: terrain, fluids and flora as
`terrain.<z>.<material>.s<slope>.<x>.<y>`, `fluid.<kind>.<eighths>.<x>.<y>`,
`flora.<species>.<x>.<y>`; structures `structures.<pack>.<piece>.<x>.<y>`;
unreferenced units `units.<k>` by a structural order; a unit's items
`<unit>.inventory.<k>`, `<unit>.equipment.<slot>`, `<unit>.accessories.<k>`. Ids
share one namespace across every family, items included, and are made unique
against each other.

**What a scenario does not carry.** Orders and AI jobs, work designations,
discovery, expedition history and other player progress are not part of the
format and are neither written nor disturbed: capture never clears an order
merely because it is not exported. Pending container shells are written as the
descriptors they are (`containers: {1: {item: box, realized: false}}`) without
being opened; realized contents are written as they are, never rerolled.

**What stops a capture.** A document is never written if it could not be read
back as the arena: a unit or building of a definition no longer registered, a
structure no registered pack places, a planted plant or crop plot of a species the
catalog cannot name, a column with no solid cell, or a tag on a cell below the
surface each fail the capture with one diagnostic per finding
(`terrain.tags`, `units[…]`, …) and nothing is written. The document is also
decoded again through the shared boundary and compared with what was captured; a
document the schema refuses, or one that decodes into something else, is never
published. A living unit whose captured resources are already fatal is written
(whether a unit survives readiness is decided by Lua at load) and the loader
rejects it with its own diagnostic.

**Overwriting.** A capture without overwrite intent may create a new file and
refuses an existing one, at publication as well as beforehand: a file that
appears while the capture runs is not replaced. With intent it replaces the
destination in one step and keeps no backup. Saving under another name
preserves the original; loading, migration and capture preparation never
rewrite the source.

### Example: handwritten, then captured

Handwritten (every unlisted value is a deterministic default):

```yaml
version: 3
map: {width: 40, height: 40}
units:
  - id: medic
    definition: acolyte
    x: 1
    y: 1
    stats: {hydration: 4.0}
    wounds: [{part: torso, kind: slash, severity: 0.2}]
    inventory: [{id: kit, definition: field_toolbox, contents: [{id: spare, definition: steel_bar}]}]
```

Captured from that arena after `hydration` was changed to 3 through the engine
(excerpt of a real capture; the whole file is retained in
[pr-proofs/issue-2714-scenario-capture-evidence.md](pr-proofs/issue-2714-scenario-capture-evidence.md)):

```yaml
version: 3
map:
  width: 40
  height: 40
camera:
  x: 3
  'y': 4
…
units:
- id: medic
  tags:
  - squad
  definition: acolyte
  x: 1
  'y': 1
  z: 6
  blood: 3.2
  inventory:
  - id: kit
    definition: field_toolbox
    contents:
    - id: spare
      definition: steel_bar
      bulk: 0.75
      condition: 100
      …
  skills:
    balance: 34.878227            # every skill: rolled once at load, stated now
    …
  stats:
    calories: 600
    hydration: 3                  # the CURRENT value, not the 4.0 that was written
    …
  wounds:
  - {age: 0, bandage: 1, clean: false, clot: 0, dressing: '', heal: 0,
     infection: 0, infection_type: '', kind: slash, necrosis: 0, part: torso, severity: 0.2}
```

### Example: edited, then recaptured

After the arena is played with — the wound heals, the unit's hydration drops to
3 and its calories to 150, the canteen is half emptied, the dagger nearly breaks
and one toolbox is emptied while another is overloaded — a recapture states the
arena as it IS, not as it was written:

```yaml
units:
- id: medic
  stats: {calories: 150, hydration: 3, …}
  wounds: []                       # healed: an empty list, never the old wound
  equipment:
    right_hand: {id: blade, definition: steel_dagger, condition: 3, …}
  inventory:
  - {id: flask, definition: canteen_steel_2l, fill: 1, …}   # half of 2 L
  - {id: kit, definition: field_toolbox, contents: [], …}   # emptied: stays empty on reload
  - id: pack
    definition: field_toolbox
    contents: […]                  # 13 items, past the definition's capacity, as it is
```

Loading the recaptured file and capturing again writes the same bytes: nothing
accumulates across cycles, and no entity or membership is added or lost.

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

The `Scenario.Placement` group (`test-headless/Test/Headless/Scenario/Placement.hs`)
needs no GPU. It boots a headless engine with a running world thread, loads the
shipped items, units, buildings, loot tables and profiles, the `ruin_small` location
and the `dungeon_1` pack (through the real `scripts/structures.lua`), creates real
finite arena pages, and drives materialize, capture, YAML encode and decode, and an
ordinary save and stage. Its property examples draw seeded layouts and compare them
against an oracle written from the map rectangle alone. Mutation checks of the
adapters are recorded with the PR.

```sh
cabal test synarchy-test-headless --test-options='--match "Scenario.Placement"'
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

The `Scenario.Capture` group (`test-headless/Test/Headless/Scenario/Capture.hs`,
#2714) drives the PRODUCTION capture service, the production Lua message pump,
the real save barrier, the real world thread, the real projection and the real
file writer against the shipped definitions. Only the owner threads a headless
harness never starts are stand-ins (`Test.Headless.Scenario.Load.Harness`), and
the examples act as those owners the way `Test.Headless.Save.Barrier` does:
withholding a stand-in's acknowledgement to put a mutation that crosses owners on
either side of the snapshot boundary, never sleeping. It covers the document
encoder as a property over 500 generated scenarios; capture of a loaded, an
edited and a manually edited arena with explicit current values; pending
containers and orders left alone; the busy matrix and the unpause refusal held at
each stage; stale and cancelled work; deferred failures inside the capture window;
an injected failure at every storage step (assert the destination, the temporary
files, the publication flag, the terminal result and the paused state); and the
route back through an ordinary save and through the captured file. Faults are
injected through the writer's operation seam, never through permissions or timing.
`tools/scenario_capture_probe.py` adds the live multi-thread and fresh-process
evidence against real engines:

```sh
cabal test synarchy-test-headless --test-options='--match "Scenario.Capture"'
python3 tools/scenario_capture_probe.py --port 9231     # after the probe-slot grant
```
