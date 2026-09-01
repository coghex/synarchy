# Building asset schema and lifecycle roles (BDA-1, #2080)

The as-built reference for how a building declares its art and its
lifecycle, what the temporary migration forms are, and where this
slice's responsibility stops. The design record for the whole arc is
[`building_directional_assets_design.md`](building_directional_assets_design.md);
this file describes what the tree actually does today.

Owners: `Building.Schema` (the vocabulary), `Engine.Asset.YamlBuildings`
(decoding + rejection), `Engine.Scripting.Lua.API.Buildings.Yaml` (the
loader conversion), `Building.Types` / `Building.Render` (the runtime).
Gate: hspec `--match "building asset schema and lifecycle roles"`, plus
`--match "Workbench construction animation"` /
`"Machine Shop construction animation"` for the shipped-asset contract.

## 1. Four camera facings, never eight unit directions

A building is drawn from one of the camera's four 90-degree facings
(`Engine.Graphics.Camera.CameraFacing`), so a building asset is declared
once per facing. `Building.Schema.FacingSet` holds exactly one value per
facing, in the canonical order **south, west, north, east** — the same
order the YAML spells and the derived `Foldable` walks.

The set is TOTAL: there is no "missing direction" case downstream,
because the decoder refuses an incomplete declaration instead. Nothing
mirrors one view into another; a building's doors, controls, lettering
and pipes are asymmetric, which is exactly why `Unit.Direction`'s eight
values and its mirror flag are wrong here and are not used.

## 2. The canonical YAML

```yaml
sprites:
  south: "…"
  west:  "…"
  north: "…"
  east:  "…"

animations:
  raise:
    fps: 4
    loop: false
    frames:
      south: ["…", "…"]
      west:  ["…", "…"]
      north: ["…", "…"]
      east:  ["…", "…"]
```

Both key sets are CLOSED. Every rejection below names the building, and
where it applies the animation, the direction, and the stage:

| Rejected | Because |
|---|---|
| a missing direction in `sprites` or `frames` | all four are required |
| an unknown direction key | the key set is exactly south/west/north/east |
| `sprites` beside legacy `sprite` | two forms, resolved by neither precedence nor merge |
| a directional `frames` key beside `frames.default` | same rule, independently |
| an animation with no `frames` at all | an animation with no frames is a typo, not a default |
| an empty canonical frame list | every direction needs at least one frame |
| unequal canonical frame counts | frame *i* must mean the same stage in every view |
| one path claimed by two facings **at one stage** | each direction needs its own art |
| a missing or unrecognized `visual_class` | see §4 |
| an unknown `state_animations` key | see §3 |
| legacy `appearing` beside the role it resolves to | see §3 |

The stage-distinctness rule is per stage, deliberately: one path
recurring at a LATER stage of the same clip is an ordinary repeated
frame, not a collapsed declaration.

## 3. Lifecycle roles

`state_animations` maps a closed role vocabulary
(`Building.Schema.BuildingRole`) to animation names:

| Key | Meaning |
|---|---|
| `construction` | worker-driven build, indexed by `build_progress / build_work` |
| `appearance` | timed materialisation, indexed by elapsed game time |
| `built` | the finished loop |
| `destruction` | declarable, and deliberately not played yet — BDA-3 owns playback |

The derived activity (`Building.Types.currentActivity`) splits with it:
a positive-`build_work` instance short of its target is `Constructing`,
a zero-work instance inside its declared appearance is `Appearing`, and
both eventually reach `Built`. `building.getActivity` reports
`"constructing"` / `"appearing"` / `"built"` accordingly
(`buildingActivityLabel` is the single mapping).

**Code that only asks whether a building is OPERABLE keeps testing
against `"built"`** — constructing and appearing are both not-built, and
that is why the operability call sites did not change. Only
construction-PROGRESS presentation and worker/delivery targeting moved
to `"constructing"`.

A `Built` building that declares no `built` animation still pins the
last frame of the role its own `build_work` selected — `construction`
for a positive-work definition, `appearance` for a zero-work one — so
the completed sprite does not snap back to the static art.
`Building.Schema.legacyRoleFor` is that one discriminator, shared by the
pin, the appearance-duration lookup, and the legacy-key resolution
below, so they cannot disagree.

## 4. Visual class

`visual_class` is MANDATORY and records which art family owns a
building's textures. It affects no placement, gameplay, or rendering
behaviour; it exists so the art slices know what they are producing.

| Value | Shipped members |
|---|---|
| `indoor_fixture` | Kitchen, Workbench, Machine Shop |
| `freestanding_installation` | Cargo Hold, Furnace, Solar Panel, High-Voltage Battery |
| `gateway` | Acolyte Portal |

Mandatory means a missing or unrecognized value refuses the whole YAML
file through `loadYamlList`, which is why every non-shipped building
declaration in the tree — the probe fixtures under `tools/`, and the
inline decoder fixtures in `test-headless/` — carries one too.

## 5. The two migration axes, which are independent

Migration has two axes, and a definition may be legacy on one and
canonical on the other. Rejection applies WITHIN an axis, never across:

1. **Paths.** Legacy singular `sprite` and legacy `frames.default`
   remain readable. Each is refused only beside its own canonical form.
2. **Lifecycle.** Legacy `appearing` remains readable, resolving through
   `legacyRoleFor` — construction for a positive-`build_work`
   definition, appearance for a zero-work one. It is refused only beside
   the ONE canonical role it resolves to; `built` beside it stays legal.

All eight shipped definitions are exactly that mixed state after this
slice: canonical lifecycle roles and `visual_class` now, legacy art
paths until the art slices land. A decoder that refused any legacy form
once any canonical form appeared would refuse every shipped building.

`Building.Schema.AssetSource` records which form produced a facing set,
at BOTH the YAML level and on the runtime definition, so a
legacy-sourced declaration stays observable after the loader conversion.
That marker is what BDA-13's whole-tree audit later rejects from shipped
definitions; without it the compatibility branch would be
indistinguishable from a real four-facing declaration.

## 6. The loader conversion

`engine.loadBuildingYaml` keeps every facing separate:

- A CANONICAL declaration loads each view's own path on its own handle,
  registered as `building_<name>_<facing>` (and
  `building_<name>_<anim>_<facing>_<i>` per frame). The facing is in the
  registry key precisely so two views cannot claim one key and overwrite
  each other.
- The LEGACY branch loads its single path ONCE and exposes that one
  handle through all four views — so an unmigrated definition costs
  exactly the uploads it always did, and there is only one asset for
  nothing to overwrite.

The build menu's pinned icon (`building_<name>_ui`, #2075's dual-use
pair) is the SOUTH view, and `BuildingInstance.biTexture` is the south
view copied at placement and re-resolved from `biDefName` at load.

## 7. What this slice deliberately does NOT do

- **Camera selection.** Rendering still reads the south view
  (`bdSouthTexture`, `facingAsset FaceSouth`). BDA-2 owns making placed,
  ghosted, designated and hit-test geometry follow the active camera.
- **Destruction.** `destruction` decodes; nothing plays it. BDA-3.
- **Preview direction/lifecycle controls.** The preview keeps decoding
  both forms and enumerating every facing's frame paths for its
  content-based directory association, and gains no UI. BDA-4.
- **Art.** No texture is authored, generated, mirrored or approved here.
- **The final audit.** Removing the legacy forms, and checking file
  decoding, dimensions and anchor consistency across the whole tree, is
  BDA-13.
- **Persistence.** No orientation is stored, `BuildingInstanceSnapshot`
  is unchanged, and no save version moved. An in-flight build reloads
  under the construction role at the same progress.
