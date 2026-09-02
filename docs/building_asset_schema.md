# Building asset schema and lifecycle roles (BDA-1, #2080)

The as-built reference for how a building declares its art and its
lifecycle, what the temporary migration forms are, and where this
slice's responsibility stops. The design record for the whole arc is
[`building_directional_assets_design.md`](building_directional_assets_design.md);
this file describes what the tree actually does today.

Owners: `Building.Schema` (the vocabulary), `Engine.Asset.YamlBuildings`
(decoding + rejection), `Engine.Scripting.Lua.API.Buildings.Yaml` (the
loader conversion), `Building.Types` (the runtime record),
`Building.Visual` (the facing-aware selection / geometry boundary, §8),
`Building.Render` / `Building.HitTest` (its two consumers).
Gate: hspec `--match "building asset schema and lifecycle roles"`, plus
`--match "Workbench construction animation"` /
`"Machine Shop construction animation"` for the shipped-asset contract,
and `--match "Building camera-facing visuals"` for §8.

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
| one path claimed by two facings in `sprites` | a canonical block never aliases one view into another |
| one path claimed by two facings **at one stage** of an animation | same rule, per stage |
| a missing or unrecognized `visual_class` | see §4 |
| an unknown `state_animations` key | see §3 |
| legacy `appearing` beside the role it resolves to | see §3 |

The distinctness rules are CANONICAL-only — a legacy declaration
reaching all four views is the compatibility branch's whole job — and
the animation one is per stage, deliberately: one path recurring at a
LATER stage of the same clip is an ordinary repeated frame, not a
collapsed declaration.

## 3. Lifecycle roles

`state_animations` maps a closed role vocabulary
(`Building.Schema.BuildingRole`) to animation names:

| Key | Meaning |
|---|---|
| `construction` | worker-driven build, indexed by `build_progress / build_work` |
| `appearance` | timed materialisation, indexed by elapsed game time |
| `built` | the finished loop |
| `destruction` | played once, forward, by the transient presentation a demolition captures (BDA-3, #2091) — never by a live instance; must declare `loop: false` and a finite positive `fps`, or the demolition is silent and the declaration is reported |

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
view copied at placement and re-resolved from `biDefName` at load. Neither
is what the world draws: the rendered and hit-tested view is the active
camera's (§8), and `biTexture` is read only when the definition is missing
from the manager.

## 7. What this slice deliberately does NOT do

- **Camera selection** landed as BDA-2 (#2088, §8): placed, ghosted and
  hit-test geometry follow the active camera through `Building.Visual`.
  The committed building DESIGNATION still draws its generic marker;
  #1845 consumes the same boundary when it replaces that.
- **Destruction.** `destruction` is played by `Building.Destruction` (BDA-3, #2091): the `BuildingDestroy` drain captures a render-only effect from the declared clip in the same transition that removes the instance, plays it once from the game clock, and prunes it at `frameCount / fps`. No fallback: a definition without the role is removed with no visual, and a looping, zero-, negative- or non-finite-fps declaration is reported with building/animation context and plays nothing.
- **Preview direction/lifecycle controls.** The preview keeps decoding
  both forms — its static hint reads canonical `sprites.south` and falls
  back to legacy `sprite`, so an art slice's migration cannot silently
  drop a building out of its default-selection ladder — and enumerates
  every facing's frame paths for its content-based directory
  association. It gains no UI. BDA-4.
- **Art.** No texture is authored, generated, mirrored or approved here.
- **The final audit.** Removing the legacy forms, and checking file
  decoding, dimensions and anchor consistency across the whole tree, is
  BDA-13.
- **Persistence.** No orientation is stored, `BuildingInstanceSnapshot`
  is unchanged, and no save version moved. An in-flight build reloads
  under the construction role at the same progress.

## 8. Camera selection and the render / hit-test ownership rule (BDA-2, #2088)

`Building.Visual` is the ONE facing-aware boundary, and it is pure:
every function takes the camera facing, the instance, the definition,
the game clock / progress and the texture-size table as explicit
arguments, because the scanned render entry points emit nothing without
a texture system (the headless state) and the agreement below has to be
assertable without a GPU.

- **Selection.** The camera facing maps DIRECTLY onto the declared
  view — `FaceSouth` → south, `FaceWest` → west, `FaceNorth` → north,
  `FaceEast` → east (`facingAsset`). It is never composed with a stored
  building orientation: `BuildingInstance` carries none and none was
  added, so no save field moved. `pickBuildingFrame facing now inst def`
  picks the lifecycle frame from the SELECTED direction's own frame
  list; `placedBuildingVisual` wraps it with the two other cases — a
  placed pre-delivery ghost shows the facing's STATIC view (flagged, and
  drawn at 0.6 alpha), and an instance whose definition is missing from
  the manager shows its stamped `biTexture`, facing-blind, on both
  sides. `previewBuildingTexture` is the placement preview's static
  view, and the one #1845's designation ghost consumes.
- **Lifecycle is facing-independent.** `currentActivity` takes no
  facing: construction indexes on `biBuildProgress / bdBuildWork`, the
  timed roles on elapsed game time × fps (looped or clamped), and a
  Built building without a `built` clip pins its lifecycle role's last
  frame. The SAME rule is applied to the selected direction's REAL
  count, so a direction with fewer frames (reachable until BDA-13
  enforces counts) derives its index from its own length at the same
  progress fraction, and an EMPTY direction falls back to that facing's
  static sprite, never south's. A camera turn cannot flip Constructing /
  Appearing / Built, and restarts, advances or rewinds nothing.
- **Geometry.** `buildingQuadRect` sizes the quad from the SELECTED
  handle's pixel size (the base tile size for a handle the table does
  not know), centres it on the footprint's anchor tile and puts its
  bottom edge on that tile's iso bottom plus `spriteAnchorOffset`
  (`tile_bottom` drops it by `tileSideHeight`; `diamond_bottom` and a
  missing definition do not) — at every facing. Rotation changes the
  selected canvas and the projection; footprint, grid position, grid z
  and sort ownership are untouched. The placed sort key is
  texture-independent; the ghost preview's key keeps its canvas-height
  term, so a facing with a taller canvas legitimately sorts the GHOST
  differently.
- **Ownership.** `Building.Render.buildingToQuad` and
  `Building.HitTest.hitTestBuildingAt` both read `placedBuildingQuad`;
  the placement preview reads `previewBuildingTexture` +
  `buildingQuadRect`. Neither consumer decides an asset or a rect on its
  own, so a click inside the visible quad targets that building and a
  pixel lying only inside another view's bounds — the south sprite after
  a rotation, or the un-dropped rect of a `tile_bottom` building — does
  not, for a clickable pre-delivery ghost as much as a built one.
  Everything else in the hit test is policy, not geometry, and is
  unchanged: the active world only, the z-slice / view-depth band, the
  degenerate-window guard, highest grid z wins, equal-z ties go to the
  closer quad centre, and it stays quad-based rather than per-pixel.
- **Outside the facing domain.** The build menu's `iconTex` is
  `bdIconTexture`, the pinned south view; `bdSouthTexture` and
  `biTexture` stay the camera-blind south handle for it and for the
  def-missing fallback. A legacy declaration renders FaceSouth's output
  at every facing by construction (one value in all four views).

Gate: hspec `--match "Building camera-facing visuals"` — asymmetric
fixtures (four distinct static handles and frame lists, a different
canvas per facing) through the pure quad builders AND the real
`hitTestBuildingAt` on a headless engine.
