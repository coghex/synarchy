# src/Unit/Atlas/ — compiled unit animation atlases

Loaded when you work under `src/Unit/Atlas/`. The same rules cover
`assets/textures/units/`, `data/units/`, `tools/pack_atlas.py`,
`tools/unit_texture_budget.json`, and the unit-YAML / preview /
registration decoders. Design authority:
[`docs/texture_infrastructure.md`](../../../docs/texture_infrastructure.md)
(TEX-2…TEX-7, decisions D-1…D-12, measured results). Fresh-checkout
workflow and D-7's restart-to-reload rule:
[`docs/asset_generation.md`](../../../docs/asset_generation.md).

Source PNG frames stay the editable artwork (D-1); unit YAML stays the
only hand-edited semantic authority (D-11); everything under a unit's
`atlas/` directory is DERIVED and nobody hand-edits it (`atlas/` is a
SIBLING of `animations/`).

**One command covers all four concerns:**
`python3 tools/pack_atlas.py --validate-only --strict` runs the art
inventory, the compiler's freshness comparison, AND the two budgets. Add
`--compile [--unit <name>]` to regenerate, or `--compile --check` to
report staleness without writing. Deps are pinned in
`tools/requirements-assets.txt` (PyYAML + Pillow), spelled again in
`.github/ci/Dockerfile` (`test_pack_atlas.py` fails if the two drift).
Pillow is load-bearing for VALIDATION too: an absent decoder is one
loud error naming the install command, never a silent skip.

## Inventory (#1257, #1311)

Discovery is **filesystem-first**: it walks every PNG under
`assets/textures/units/<unit>/animations/<animation>/<direction>/` and
checks the declarations against it. **Every committed animation PNG is
owned by exactly one animation-frame declaration; there is no directory
or glob exemption mechanism.** Non-animation unit textures (`sprite`,
`directional_sprites`, `portrait`) are existence-checked only, and
"duplicate" means duplicate ANIMATION-FRAME claims only — reusing an
animation frame as a sprite or portrait is legal.

Two declaration forms live under `data/units/`, and the top-level key is
the entire runtime distinction: `units:` (a gameplay unit — registers,
loads textures, lists, spawns; `name` + `sprite` mandatory) and
`asset_units:` (asset-only: exactly `name` + `animations`, as a
WHITELIST enforced by BOTH decoders; nothing registers, loads, lists, or
spawns them). A file holding NEITHER key is refused rather than decoded
as zero units, and so is a key present with an explicit `null` (aeson's
`.:?` reads that as absent). Three
decoders share the shape: `UnitYamlFile`, `Engine.Preview.Unit`'s
`UnitAnimMetaFile`, and `pack_atlas.py`. Animation/direction keys are
strings, never coerced. The structural invariants and the three
independent CONTENT checks: `docs/engine_contracts.md` §Unit animation
art — the three checks each have a fixture the other two accept, so do
not fold them into one.

**Deleting art needs the owner's explicit confirmation** (#1257 R4):
present an exact path-level classification first.

## Compiler (#1258, TEX-2)

Output is **one atlas per ANIMATION** (D-2),
`assets/textures/units/<unit>/atlas/<animation>.png`, beside a generated
`atlas/index.json`. Rows are the AUTHORED directions in `Unit.Direction`
order (five for `flip: true`, eight for `flip: false`); columns are the
max authored frame count, with the index recording each direction's
TRUE count — no padding slot is addressable. Cells are exact integer
copies of the source frames' decoded RGBA8 samples; a size mismatch is a
compile error, never an implicit rescale (D-6). Since #2076 each cell
sits one texel inside a `(cell+2) x (cell+2)` SLOT whose gutter copies
that cell's own edge texels outward, so a linear tap can never reach a
neighbouring frame; the index records `cell_padding` at
`schema_version` 2 and carries two PER-ANIMATION `sha256` digests
(D-12). `--validate-only` is index-aware: a unit with NO index is valid
to the TOOL but not to the ENGINE; an existing index is REGENERATED and
compared, so a tampered index cannot certify a tampered atlas. Exact
invariants: `docs/engine_contracts.md` §Unit atlas compiler.

**Every shipped unit's atlases ARE committed**, so a fresh checkout runs
with no packer step.

## Budgets (#1262, TEX-7)

`tools/unit_texture_budget.json` is the SINGLE machine-readable source
for two independent budgets; a missing or malformed one is a hard
error, never a skipped check. **Both read the COMPILED TREE ONLY** —
the stored `atlas/index.json` documents and the entries beside them —
so neither says anything about runtime behaviour (#2217).

- **Generated atlas entries — a hard ERROR.** Exactly one entry besides
  `index.json` in a unit's `atlas/` directory per COMPILED ANIMATION,
  the bound derived from each unit's own generated index. Non-animation
  textures are excluded BY CONSTRUCTION, not by an exemption list. This
  catches a per-frame regression in the ASSET TREE; a loader regression
  that leaves `atlas/` alone is invisible to it.
- **Projected decoded memory — a WARNING, so `--strict` is what
  blocks.** Decoded RGBA8 footprint PROJECTED from each index's declared
  `atlas_width × atlas_height × 4` over the WHOLE tracked roster
  (`startup_loader.lua` feeds every `data/units/*.yaml` to the loader at
  boot). Two distinct quantities: that index-projected total, and the
  same total × `roster_growth_factor`, which is the one compared against
  a 384 MiB threshold **confirmed by the project owner on 2026-08-16** —
  raising it is the owner's call. Neither is measured on a GPU. A breach
  IS D-10's precondition for resuming deferred TEX-5 (KTX2). A
  single-unit `--unit` run does NOT evaluate this one.

**The runtime registration bound is a different gate.** The Hspec group
`Unit.Atlas.Load — the real unit registration boundary`
(`test-headless/Test/Headless/Unit/Atlas/Loader.hs`, in the
always-blocking headless suite) is what holds the loader to one queued
atlas upload request and one distinct logical texture handle per
animation, each published into the definition's animation storage, and
to no per-frame ordinary requests. It inspects the queued
`LuaToEngineMsg` values the loader produces, so it owns the request and
handle boundary — not completed Vulkan uploads or bindless-table
publication, which happen downstream. A green
`pack_atlas.py --validate-only --strict` is never evidence about any of
that.

## Runtime (#1259/#1260/#1261, TEX-3/TEX-6)

**Every shipped unit uses the compiled path, and there is no other way
for a unit animation to load.** The per-frame loader is GONE from the
tree.

- **Storage is a named SUM with one constructor.**
  `Unit.Types.Def.Animation`'s `aStorage` is
  `Unit.Atlas.Types.AnimStorage`, exactly `StorageAtlas`. Read
  frames through the storage-neutral accessors — `storageFrameCount` /
  `storageFrameCounts` / `storageMaxFrameCount` / `storageSampleAt` —
  never by matching the constructor. Buildings are not on this type:
  they are never compiled (D-8) and keep
  `Building.Types.BuildingAnimation`.
- **The index is the whole answer, and failure is failure.**
  `Unit.Atlas.Load.loadUnitAtlasIndex` reads, parses, decodes and
  verifies EVERY declared atlas before `loadUnitYaml` allocates one
  handle; `Unit.Atlas.Index.planUnitAtlasStorage` adds the
  YAML-staleness half. A missing, incomplete, stale, unsupported or
  malformed index refuses the whole unit definition — no partial
  registration, nothing to fall back to. Validation passes, digests, and
  `pythonFloatRepr` pinning: `docs/engine_contracts.md` §Unit animation
  atlas runtime.
- **`pickFrame` returns a `FrameSample`, and its arithmetic is FROZEN**
  (D-3): the stable handle (#286 — never a slot), UV endpoints, pixel
  dimensions when known, and the mirror flag. The per-direction frame
  COUNT is the index's REAL count, so padding is unreachable (D-5).
  Non-rendering consumers of a clip's LENGTH (`Unit.Thread.Command.Pose`
  transitions, `unit.getAnimDuration`) go through
  `storageMaxFrameCount`.
- **Cell dimensions size everything.** `frameDimensions` is the one
  funnel: an atlas sample answers from its cell, a whole-image sample
  (the direct default/directional sprite a T-pose falls back to) falls
  through to `rvTextureSizeRef`. Nothing may measure an atlas handle's whole-image entry where
  it means a frame — hit testing sizes from the SAME `pickFrame` sample
  the renderer draws (`Unit.HitTest.unitHitRect`).
- **Mirroring reflects across the frame's own sub-rect**, never the
  whole image — with atlases, `1-u` lands in a different cell
  (`UI.Render.renderSpriteBatch`: `u' = su0 + su1 - u` over `ussUV`; a
  whole-image sprite is the unchanged `1-u`). Anything DISPLAYING a unit's live frame must use
  `unit.getFrameSample`, not `unit.getFrameTexture`, and publish with
  `UI.setSpriteFrame`, which lands texture, sub-rect and mirror in ONE
  manager transition — separate setters race the render thread.
- Atlas slots follow the global nearest/linear sampler with one mip
  level (D-6); cell UVs sit on the logical cell's exact edges with no
  half-texel inset.

## Gates

`python3 tools/test_pack_atlas.py` +
`pack_atlas.py --validate-only --strict` (~2 s; unconditional in `make ci` and post-merge CI,
path-selective on PRs via `ci_expensive_gates.py --gate unit-assets`);
hspec `--match "Asset.UnitInventory"`, `--match "Unit.Atlas"`,
`--match "pickFrame"` (the whole logical-choice matrix checked against
an independently written `expectedChoice` table, so an edit to either
side fails), `--match "the real unit registration boundary"` (drives
`registerUnitDefs` against a live headless engine: one atlas upload and
one published `Animation` per animation, no per-frame textures, a
rejected index queueing nothing).
Roster-wide headless evidence:
`tools/combat_anim_probe.py --roster-only`, which reads the texture-NAME registry
(`engine.getTextureHandle`) rather than `engine.getLoadedTexturePaths()`
— the latter is EMPTY headless.
