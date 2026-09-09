# Structure pack schema: static art and construction frames (#2488)

The as-built reference for what a `data/structure_packs/<pack>.yaml`
declares, how the two Lua loaders hand it to the engine, and what the
engine will and will not accept. The sibling document for buildings is
[`building_asset_schema.md`](building_asset_schema.md); the design record
for the arc is
[`building_directional_assets_design.md`](building_directional_assets_design.md).

Owners: `scripts/structures.lua` and `scripts/wire.lua` (reading the
YAML and loading its textures), `scripts/structure_frames.lua` (the one
declaration-to-payload rule both share),
`Engine.Scripting.Lua.API.StructureArt`
(the `structure.registerPackArt` payload and the image measurement),
`Structure.ArtCatalog` (the vocabulary, the registration rules and
resolution), `World.Render.StructureGhost` (the one consumer that draws
construction frames), `Structure.Render` (the shared geometry).

Gates: hspec `--match "structure construction frames"`,
`--match "Structure.ArtCatalog"`, `--match "structure ghost"`; probes
`construction_probe.py`, `wire_probe.py`, `structure_rotation_probe.py`,
and the offscreen pixel gate `structure_construction_probe.py`.

## 1. The two pack shapes

There are two schemas, because there are two loaders.

**Piece packs** (`scripts/structures.lua`, e.g. `dungeon_1.yaml`) declare
`build:` costs per kind, a `pieces:` block for `floor` / `ceiling` /
`post`, a `walls:` block for the four authored edges, and an optional
`variants:` block whose entries override any subset of either.

**The wire pack** (`scripts/wire.lua`) declares one `build:` entry, one
shared `facemap:`, and a `connections:` map of sixteen autotile shapes.

Both loaders resolve their pack through a module field
(`structures.packDir` + `structures.pack`, `wire.packPath`) rather than a
hard-coded constant, so a spec can point the real loader at a fixture
pack. Nothing in the game changes those fields.

## 2. Appearances, which are coarser than art slots

An **appearance** is one authored static sprite. It is the granularity a
construction sequence is declared at, and it is deliberately coarser than
the `ArtKey` the static art catalogue is keyed by:

| Kind | Art slots | Appearances |
|---|---|---|
| floor / ceiling / post | 1 each | 1 each |
| wall | 16 (4 edges × 4 cap facemaps) | 4 (one per edge) |
| wire | 16 (one per connection shape) | 16 |

A wall's four cap facemaps all draw the SAME sprite, so a pack declares
one sequence per edge and not one per cap. `Structure.ArtCatalog`'s
`AppearanceKey` pairs that slot with an optional VARIANT name; `Nothing`
is the pack's own default art.

## 3. Declaring construction frames

Any appearance may add an ordered `construction:` list of image paths.

```yaml
pieces:
  floor:
    texture: assets/textures/buildings/dungeon_1/floor.png
    facemap: assets/textures/facemap/floorface.png
    construction:
      - assets/textures/buildings/dungeon_1/build/floor_0.png
      - assets/textures/buildings/dungeon_1/build/floor_1.png

walls:
  ne:
    texture: assets/textures/buildings/dungeon_1/wall_ne.png
    facemaps: { "00": …, "01": …, "10": …, "11": … }
    construction:
      - assets/textures/buildings/dungeon_1/build/wall_ne_0.png
      - assets/textures/buildings/dungeon_1/build/wall_ne_1.png

variants:
  damaged:
    pieces:
      floor:
        texture: assets/textures/buildings/dungeon_1/damaged/floor.png
        construction:
          - assets/textures/buildings/dungeon_1/damaged/build/floor_0.png
```

The wire pack's `connections:` entries accept either form:

```yaml
connections:
  isolated: assets/textures/structures/wire/isolated.png   # legacy scalar
  cross:                                                    # declaring form
    texture: assets/textures/structures/wire/cross.png
    construction:
      - assets/textures/structures/wire/build/cross_0.png
```

The scalar form is not deprecated. A connection with no sequence has
nothing to say beyond its texture, and every shipped connection is in
exactly that state.

**A declaration is keyed to exactly one appearance.** A variant's
override never inherits or substitutes the default's frames, and an
appearance with no declaration resolves no sequence — never another
appearance's. `scripts/structure_frames.lua`'s `declaredBy` is written as
an explicit branch for that reason: the `variant and over.construction or
base.construction` idiom silently falls back to the default whenever the
override declares none, which is the inheritance this rule forbids.

**The loaders preserve a declaration's SHAPE and normalise nothing.**
`scripts/structure_frames.lua` is the one place either pack schema turns
a `construction:` value into a registration payload, and it rebuilds a
list BY KEY rather than with `ipairs`: `engine.loadYaml` decodes a YAML
null to a Lua `nil`, so `[a, null, c]` arrives as a table with a hole,
and `ipairs` would stop at it and hand the engine a dense one-frame list
indistinguishable from an authored one — silently dropping every later
stage. The gap is copied through so the engine's own density check sees
it, a value that is not a list at all goes over untouched so the engine
refuses it as one, and an entry that is present but not a path becomes an
entry the engine refuses by index. Every judgement stays the engine's.

## 4. What the engine refuses

`structure.registerPackArt` is all-or-nothing per pack, and construction
frames are under the same rule as the static art: any of these refuses
the WHOLE pack, logs one warning naming the pack and the appearance, and
leaves the catalogue exactly as it was.

| Refusal | Why |
|---|---|
| an empty `construction:` list | an authored empty list is a typo, not an absent declaration |
| the same image twice in one list | a sequence that repeats a stage was not authored that way |
| a path that escapes the resource root (`..`, `.`, a leading `/` or `~`, a backslash, a colon) | a pack may only name art this game ships |
| a frame handle that is not a loaded handle | the renderer cannot draw it |
| a sequence whose static sprite is not the one the pack declares for that appearance (default art only) | the handoff would land on a different sprite |
| a LAST frame whose pixel dimensions differ from that static sprite's | the handoff would jump size — see §6 |
| an image that cannot be measured | an unmeasurable image is a fault, not a check to skip |
| a wall family whose declared directions run to different lengths | one progress value would select different stages at different facings — see §5 |
| the same appearance declared twice | registration order would decide what the pack means |
| frames for a kind the pack does not declare | the appearance does not exist |
| a conflicting repeat (frames included) | the STORED declaration is kept |

An identical repeat is an idempotent no-op and stays silent.

A terminal texture-load failure on a declared FRAME makes the whole pack
resolve nothing, exactly as a failed static sprite does, and is reported
once per (pack, path) naming the appearance and the frame's position.

**An escaping path is refused before anything opens it.** The rule is
`Structure.ArtCatalog.escapingPath` and there is one copy of it. The
loaders ask it through `structure.isSafeArtPath` and send the declaration
WITHOUT a handle rather than calling `engine.loadTexture`, so such a path
is never queued; `structure.registerPackArt` preflights the same
predicate before it measures anything, so it is never read either. The
missing handle is that guard's own consequence, so the escape check runs
first and the reported fault names the escape rather than the handle.

**Dimensions are measured from the files, not from
`rvTextureSizeRef`.** That cache is filled by a completed GPU upload,
which has not happened when a pack registers on the first Lua tick and
never happens at all in a headless session — a check written against it
would pass having compared nothing.
`Engine.Scripting.Lua.API.StructureArt.measureSequences` reads the two
images a sequence's check needs (its static sprite and its last frame)
and nothing else, so a pack declaring no sequences reads no files.

## 5. Rotation, and why a wall family's directions must agree

A wall's authored edge never moves, but the screen edge it occupies does
(`Structure.Facing.screenWallEdge`, #1712). The sprite drawn is the
family's art for the SCREEN edge, so the construction sequence drawn is
that edge's too — otherwise a turning camera would show one direction's
build stages on another direction's wall.

The frame INDEX must not change with the camera, which is what makes the
equal-length rule a registration requirement rather than a convention. A
direction the pack never declared simply stays absent (§7).

The cap facemap travels with the rotation exactly as it does for a placed
piece: `Structure.Render.structurePieceQuadsResolved` asks
`Structure.WallCatalog.rotatedWallArt` about the STATIC pair — a
construction frame is not registered art and could not identify a family
— and then swaps only the texture.

**…and the frame follows that answer, not the facing.**
`rotatedWallArt` returns nothing for art no registered family carries and
for a path two families contest, and the renderer then draws the piece
exactly as authored. `World.Render.StructureGhost.drawnWallEdge` asks the
same function with the same arguments and picks the screen edge only when
it resolves, so the frame and the cap mask always name ONE appearance. A
screen-edge frame over an authored-edge mask is the exact pairing the
shared-rotation discipline exists to prevent.

## 6. The handoff

Progress is `cdProgress` (0.0 → 1.0), advanced by
`construction.addJobProgress` and saved through `cdiProgress`. The frame
index is the same convention buildings use
(`Building.Visual.pickBuildingFrame`): `floor (progress * n)`, clamped,
so 0.0 selects the first frame and 1.0 the last.

Presentation is continuous by three separate rules:

1. the last frame is validated to occupy the static sprite's exact canvas,
   so the swap does not change the quad's size;
2. the site keeps drawing until its piece is COMMITTED to the per-chunk
   overlay the structure pass renders — `World.Construct.Art.structureCommittedAt`
   ignores the staging cache on purpose, because a staged-but-uncommitted
   piece is on screen nowhere and blanking the tile for the width of that
   hand-off is the intermediate empty frame this rule forbids;
3. the two ghost passes are disjoint: `structureDesignationGhosts` takes
   the UNPAID designations at D-19's 60 %, `structureConstructionGhosts`
   the PAID ones at full opacity, so nothing is drawn twice.

## 7. No declaration is a supported state

A paid designation whose appearance declares no construction frames keeps
today's behaviour: the site draws NOTHING until the piece appears. No
blueprint, no fade, no other appearance's frames, no static sprite scaled
by progress.

Every shipped pack is in that state today; authoring production frames
for `dungeon_1` and `wire` is BDA-15/BDA-16, and enforcing that every
shipped appearance declares them is BDA-13.

The gap is reported once per (pack, appearance) at REGISTRATION, at info
level, by
`Structure.ArtCatalog.undeclaredConstructionAppearances` /
`missingConstructionMessage`. Registration time is deliberate: it is
naturally once per appearance, it needs no per-frame or per-candidate
dedup state, and an idempotent repeat says nothing.

## 8. Lighting: the lifecycle alpha flag

A construction frame reuses the finished piece's facemap for LIGHTING —
one authored mask per appearance, not one per frame. But a half-built
wall's pixels do not share the finished wall's silhouette, and the
bindless fragment shader's normal rule is `color.a * faceAlpha`, which
would clip every frame pixel outside it.

`Engine.Graphics.Vulkan.Types.Vertex.renderFlagLifecycleAlpha` (bit 1 of
`renderFlags`) makes the frame texture's own alpha authoritative. Only
the alpha term changes: the RGB path, including the existing
fall-through to top light where the facemap's RGB sums to ~0, is
untouched, and an unflagged quad keeps `color.a * faceAlpha` exactly.

No per-frame facemap may be declared or required.

## 9. What this does not touch

The corner-slope stamping (`applyConstructSlopeToChunk` writing
`constructCorners` progress into `ctSlopes`, which `Unit.Pathing.Cost`
reads) is PATHING state, not decoration. The designation-tools design's
D-18 fences it: neither removed nor extended. Authored frames draw in
addition to it.

Saved designations carry no new field. A designation loaded at progress
`p` renders the same frame it did before saving, derived from
`cdProgress` and the current pack declaration alone.
