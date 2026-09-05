# AI Asset Generation Pipeline (PixelLab MCP)

How to generate game textures with the PixelLab MCP server (`https://api.pixellab.ai/mcp/docs`).
Written for agents; every recipe here was validated end-to-end (sugar_maple flora set, 2026-06).

## Service basics

- **Account**: Tier 2 subscription (~5000 generations/cycle). Check with `get_balance`. Generations
  are effectively free at our usage level — iterate freely, but probe before batching.
- **Job model**: all `create_*` calls return immediately with an ID; poll `get_*`. Real ETA is
  **5–8 minutes per object** regardless of the "30-90s" the tool claims. Fire batches, sleep ~300s
  in the background, then collect.
- **Concurrency**: max **10 background jobs** at once (Tier 2). Fire ≤9 per batch and retry the rest.
- **Objects auto-delete after 8 hours** — download promptly. Characters persist.

## Raw v2 API (needed for anything beyond basic generation)

The MCP tools hide parameters that the raw API supports. Get the schema from
`https://api.pixellab.ai/v2/openapi.json`. The bearer token appears in any path-mode MCP response.

```
POST https://api.pixellab.ai/v2/map-objects
{ "description": ..., "image_size": {"width": W, "height": H},
  "view": "side", "outline": "selective outline", "shading": "detailed shading",
  "detail": "high detail", "seed": 42,
  "background_image": {"type":"base64","base64":"...","format":"png"},
  "inpainting": {"type":"mask","mask_image":{"type":"base64","base64":"...","format":"png"}} }
```

**Critical**: build the JSON body with a script (`json.dump` + `base64.b64encode`) and POST with
`curl -d @file`. Inlining two base64 images into a shell string corrupts them
("cannot identify image file" 500s at processing time, 8 minutes later).

Parameter findings (tested):
- `seed` — exists on the raw API (MCP `create_map_object` hides it). Canopy/composition lineage
  comes from the seed.
- `init_image` + `init_image_strength` (1–999, default 300) — **locks appearance, not just
  structure**. Even at strength 80 a green tree stays green; the prompt cannot recolor. Use only
  for near-clones, never for seasonal/state recolors.
- `color_image` (forced palette) — **broken server-side** as of 2026-06; returns
  "cannot identify image file" even with verified-clean base64.
- `inpainting` mask convention: **white = generate, black = frozen**. The freeze is *soft* —
  expect re-tinting and stray additions in the frozen zone; always post-process (below).

## Asset taxonomy

| Category | Path | Canvas | Tool |
|---|---|---|---|
| World materials | `assets/textures/world/<name>/<name>.png` + zoommap | 96×64 iso block | `create_tiles_pro` style mode (untested) |
| Vegetation | `assets/textures/vegetation/<name>_1..4.png` | 96×64 overlay | `create_tiles_pro` style mode (untested) |
| Flora | `assets/textures/flora/<species>/<stage>.png` | 128×128 tree / 48×48 sprout | `create_map_object` (validated) |
| Buildings | `assets/textures/buildings/<name>/default.png` | 96×96 | `create_map_object` high top-down |
| Units | `assets/textures/units/<name>/animations/...` | 48×48 humanoid / 92×92 large quadruped | `create_character` + states + animations |

`assets/textures/world/facemap/` is UV/slope masks (green top, blue/red sides), **not** material art.
Engine handles arbitrary sprite sizes; trimming margins is only a memory optimization.

## Flora pipeline (validated)

A deciduous species needs 10 stages (authoritative list = `data/flora/*.yaml`, not existing folders):
`matured`, `matured_{budding,flowering,senescing,dormant}`, `dead`, `sprout`,
`sprout_{budding,dormant,senescing}`.

Hand-made sets share ONE skeleton per family (matured-family / sprout-family); only foliage
changes per season. Replicate that with a **skeleton-freeze mask**:

1. **Bases**: generate `matured` (128×128) and `sprout` (48×48) via `create_map_object`,
   `view:"side"`. Prompt must include *"no dirt mound, no soil, exposed roots, transparent under
   the plant"* or you get a dirt trapezoid to alpha out. For sprouts, do NOT name the species —
   "maple seedling" at 48px draws one giant maple leaf on a stick. Prompt shape instead:
   *"small tree sapling with many tiny light-green new leaves scattered on thin small twigs"*.
2. **Boundary scan**: find the last foliage row (PIL: lowest row with green pixels).
   sugar_maple: trunk-only from y≥86/128; sprout stem from y≥30/48.
3. **Mask**: white above the boundary (regenerate canopy), black below (freeze trunk band,
   including the surrounding transparency — that's what keeps ground/dirt out).
4. **All stages through the masked pipeline with one fixed seed** — including regenerating the
   bases themselves at the end, because the original seedless bases won't match the seeded
   variants' canopy lineage (learned the hard way).
5. **Band-restore post-pass** (mandatory; the server freeze is soft): copy every pixel below the
   boundary from the base into each variant. Result: byte-identical skeletons, ground artifacts
   deleted.
   - **Exception — color-shifted stages (dead/burnt/diseased)**: the model re-tints the whole
     trunk to match (grey for dead); a raw band copy creates a visible color seam at the mask
     line. Rebuild instead: model's tinted color where base alpha>0, transparent where base
     alpha=0, base color where the model left holes. Silhouette stays identical, tint stays
     consistent.
6. **Audit**: pixel-diff each variant's band vs its base → expect 0 (silhouette-only check for
   color-shifted stages).
7. **Data**: append the species to the right `data/flora/*.yaml` (copy white_oak's structure:
   phases / annualCycle / cycleOverrides / worldGen). Validate with a quick
   `cabal run -v0 exe:synarchy -- --dump=terrain --seed 42 --worldSize 64 --region 0,0,0,0`.
   Note: dump mode logs "Flora catalog snapshot: 0 species" even on a healthy tree — that's
   normal; real placement check needs the GUI or headless TCP.

## Unit pipeline (PixelLab characters)

Engine layout: `assets/textures/units/<name>/animations/<activity>/<direction>/frame_NNN.png`
plus `portrait.png` (32×32). Unit yaml (`data/units/<name>.yaml`) points `sprite:` at an idle frame.

**Every generated animation frame must be declared before it lands** (#1257).
`python3 tools/pack_atlas.py --validate-only --strict` is a blocking gate that
walks the filesystem first, so a newly generated folder with no declaration
fails CI — there is no exemption mechanism to reach for. Declare a gameplay
unit's animations under `units:` as usual; declare a tree that ships as art but
is not a spawnable unit under the asset-only `asset_units:` key (`name` +
`animations` only). Frames must be contiguous from `frame_000.png`, declared in
ascending order, and match the `flip` rule below exactly: `flip: true` ⇒ exactly
the five stored directions, `flip: false` ⇒ all eight.
See **Unit animation art** in `CLAUDE.md` and `docs/engine_contracts.md`
§Unit animation art for the full contract.

The gate validates frame CONTENTS as well as paths and structure (#1311): it
decodes every declared frame, so a truncated or corrupt PNG, a non-image, and
a valid image of another format renamed `.png` all fail. One pixel size per
animation is enforced there too — differing frame COUNTS per direction remain
fine. Any legitimate PNG colour type passes; the rule is "decodes as a PNG",
not "is already RGBA8".

- **Directions**: store 5 (`east`, `north`, `north-east`, `south`, `south-east`); the engine
  mirrors W/SW/NW at runtime. **Exception**: asymmetric animations (e.g. `*_RH_dagger`) store all
  8 directions — a mirrored right hand becomes a left hand.
- **Frame counts** vary per activity (4–9 in existing units). Activity names are engine-meaningful
  (see acolyte/bear_brown trees for the full vocabulary: idle/walk/run, drink/eat, attack_quick/
  attack_heavy, hit_react, death, collapse, climb*, crawling*, pose transitions, plus `injured_*`
  mirrors of most of these).

PixelLab model: a **character** (8 rotations) → **states** via `create_character_state`
(pose/equipment variants — "collapsed", "equipped with dagger" — grouped by group_id, same
identity) → **animations** via `animate_character` per state (template mode = 1 gen/direction
with fixed skeletons; v3 mode = custom `action_description`, 1 gen/direction; pro = 20–40×, needs
cost confirmation). Existing acolyte (48×48, group of ~13 states) and bear_brown (92×92, ~11
states) live in the PixelLab account — `list_characters` / `get_character` show rotation and
per-frame URLs (backblaze), downloadable with plain curl.

Recipe for a new unit (validated with bear_brown, 2026-06):
1. `create_character` (humanoid, or quadruped with template bear/cat/dog/horse/lion — **v3 mode
   rejects quadrupeds**, use standard). `size` is the character; the canvas comes out ~40% larger
   (64px character → 92×92 canvas, like the bear). Dog-template animations: bark, fast-walk, idle,
   running-4/6/8-frames, sneaking, walk-4/6/8-frames.
2. **MANDATORY STOP: after the base 8-direction character generates, present the rotations to
   the user and wait for explicit sign-off before generating anything else** (no states, no
   animations, no batches). This is a standing user requirement, not a suggestion.
3. Probe: animate `idle`/`walk`/`run` (template mode), slot in, check in game before committing
   to the full matrix.
4. **Queue discipline**: each `animate_character` spawns 8 direction-jobs against the 10-job cap,
   and a batch drains in ~10–12 min — animations queue strictly one at a time. A bear-scale
   matrix (~30 anims) is a multi-hour babysat loop.
5. **Download**: `GET /v2/characters/{id}/zip` (bearer auth) — blocks with a JSON
   "still being generated" body until every animation is done, then returns a zip already in
   engine layout (`<Name>/animations/<anim-id>/<direction>/frame_NNN.png` + `rotations/` +
   `metadata.json` mapping ids→names/frame-lists). Copy the 5 stored directions per activity
   (all 8 for asymmetric), generate the yaml `animations:` block with a script, validate every
   referenced path exists.
6. `data/units/<name>.yaml` is auto-discovered (`startup_loader.lua` `addYamlDir("data/units")`).
   Rendering/spawning needs nothing else; *behavior* needs Lua wiring (a `<species>_ai.lua`
   config, `unit_resources.lua` species block, `unit_ai_combat_attack.lua` COMBAT_ANIM_SUFFIX entry) — engine/
   Lua territory, check with the user first.

## Unit animation atlases: the compiled runtime path

Everything above makes *source artwork*. This section is about the other
half — turning artwork the repository already tracks into the artifacts the
engine actually samples. **The two are entirely separate jobs**, and most
sessions need only this one: compiling costs seconds, touches no external
service, and invents nothing. If you are not adding or repainting art, you
are here.

`assets/textures/units/<unit>/animations/**/frame_NNN.png` stay the
hand-edited source of truth (D-1) and `data/units/<unit>.yaml` stays the only
hand-edited semantic authority (D-11). `tools/pack_atlas.py --compile` turns
that pair into one atlas per animation plus a generated index:

```
assets/textures/units/<unit>/atlas/<animation>.png   one image per animation
assets/textures/units/<unit>/atlas/index.json        the generated index
```

Both are **tracked in git**, so a fresh checkout runs with no packer step
(D-12). Since #1261 the engine has no other way to load a unit animation: a
unit that declares animations and ships no compiled artifacts is refused
outright, with no fall back to the frames beside them.

### Fresh checkout: validate and preview every unit

```bash
python3 -m pip install --user -r tools/requirements-assets.txt  # PyYAML + Pillow
python3 tools/pack_atlas.py --validate-only --strict            # ~2 s, whole corpus
python3 tools/pack_atlas.py --compile --check                   # nothing out of date
python3 tools/test_pack_atlas.py                                # the checker's own self-test
```

On a PEP 668 "externally managed" Python (recent Homebrew and most
distributions) that first line refuses; use a virtualenv, your package
manager's `python3-yaml` / `python3-pillow`, or `--break-system-packages`
if you know what your environment is.

Both Python dependencies are load-bearing for *validation*, not just for
compilation: every declared frame is decoded, and every recorded digest is
over decoded RGBA8. An absent decoder is one loud error naming the install
command, never a silent skip. Validation does not need the exact pinned
toolchain — it decodes rather than encodes — so any reasonably recent Pillow
verifies a committed atlas. Compilation should use the pins.

**Take the unit roster from the inventory, never from a list in a
document.** The tool walks the filesystem first, so its own output is the
authoritative roster:

```bash
python3 tools/pack_atlas.py --validate-only --strict
# OK — 7 unit declaration(s) (0 asset-only), 116 animation(s), 4620 frame(s); …
# BUDGET — 116 generated atlas entr(ies) for 116 indexed animation(s) across 7 …
ls -d assets/textures/units/*/            # the same seven trees
```

Then eyeball each one through the real viewer. `--preview` opens a real
window (there is no offscreen variant), so run it yourself rather than from
an agent session:

```bash
cabal run exe:synarchy -- --preview units/acolyte
```

The viewer is on the production asset path (D-9): it reads the same index,
through the same loader, with the same frozen cell arithmetic gameplay uses.
A malformed index or a stale atlas fails there exactly as it fails in game —
which is the entire reason it is an acceptance surface. Its debug-console
dump (`require("scripts.preview_manager").dump()`) reports the playing
frame's atlas path, cell and UVs if you need to check a specific cell.

### Regenerating after an art change

```bash
python3 tools/pack_atlas.py --compile --unit acolyte   # one unit
python3 tools/pack_atlas.py --compile                  # everything
python3 tools/pack_atlas.py --compile --check          # report staleness, write nothing
```

Compilation refuses outright on an inventory that does not validate, so fix
declaration errors first. A run is deterministic and **local**: it compares
each artifact against what it would generate and writes only on a real
difference. Repainting one frame rewrites that animation's atlas and its
unit index, and nothing else — measured at 2 of 123 tracked artifacts
(`docs/texture_infrastructure.md`, *Measured results*). An mtime-only touch
changes nothing; the digests are over content.

**Restart to see it (D-7).** There is no hot reload. A recompiled atlas takes
effect the next time the game or the preview boots — if a change seems not to
have landed, restart before debugging it.

### When something is stale or broken

`--validate-only` regenerates each index from the live sources and compares,
so it reports the real cause rather than a checksum mismatch. The usual ones:

| Report | What happened | Fix |
|---|---|---|
| `does not match a fresh compile` / `atlas content does not match its sources` | art or YAML changed, artifacts did not | `--compile --unit <name>` |
| `is not canonically serialized` | the index was hand-edited or reformatted | `--compile --unit <name>` |
| `obsolete compiler-owned output` | an animation was renamed or deleted | `--compile --unit <name>` (it sweeps) |
| `generated atlas directory has no index.json` | a partial copy or a bad merge | `--compile --unit <name>` |
| `unclassified frame on disk` | a new frame nobody declared | declare it in `data/units/<unit>.yaml` |
| `budget: expected N generated atlas entr(ies)` | something put per-frame files in `atlas/` where one per animation belongs | see below |
| `unit-texture memory budget exceeded` | the roster outgrew the recorded budget | see below |

**Never hand-edit `index.json`, and never "bless" a digest by copying the
reported value into the file.** The comparison is against a fresh
regeneration, not against the numbers the file carries about itself, so an
edited index cannot certify anything — it just moves the failure. Regenerate.

The two budgets live in `tools/unit_texture_budget.json`, which is the single
source the strict gate reads:

- **Generated-atlas-entry budget** — a hard error. One entry besides
  `index.json` in a unit's `atlas/` directory per compiled animation (D-2),
  derived from the index's own animation count. A breach means generated
  per-frame files are creeping back into the compiled tree; the diagnostic
  names the unit, the expected and actual counts, and the offending entries.
- **Projected-memory budget** — a warning, so `--strict` (CI and `make ci`)
  fails on it. The decoded RGBA8 total is projected from each index's
  declared `atlas_width × atlas_height × 4`, and it is that total scaled by
  `roster_growth_factor` that the threshold compares — neither number is
  measured on a GPU. Crossing it is D-10's precondition for resuming deferred
  TEX-5 (KTX2 atlas loading). It is a decision to escalate to the project
  owner, not a number to quietly raise: the recorded threshold carries the
  owner's confirmation. It is also **not** D-12's separate guardrail on
  tracked artifact bytes on disk.

**Both budgets read the compiled tree and nothing else** — the stored
`atlas/index.json` documents and the entries beside them. `pack_atlas.py`
has no view of the loader, the texture request queue, the bindless table or
a running engine, so a green strict run validates generated artifacts and a
projection of their decoded size, and is not evidence about runtime
uploads, handles or bindless slots. That bound belongs to the Hspec group
`Unit.Atlas.Load — the real unit registration boundary`
(`test-headless/Test/Headless/Unit/Atlas/Loader.hs`), which runs in the
always-blocking headless suite and asserts one queued atlas upload request
and one distinct logical texture handle per animation, published into the
definition's animation storage, with no per-frame ordinary requests:

```bash
cabal test synarchy-test-headless \
  --test-options='--match "the real unit registration boundary"'
```

### Adding a new unit's art

The generation recipe above still applies; only the tail changes. After the
frames land and `data/units/<unit>.yaml` declares them:

```bash
python3 tools/pack_atlas.py --validate-only --strict --unit <unit>   # declarations vs art
python3 tools/pack_atlas.py --compile --unit <unit>                  # artifacts
cabal run exe:synarchy -- --preview units/<unit>                     # eyeball it
git add assets/textures/units/<unit>                                 # sources AND atlas/
```

Commit the generated `atlas/` directory with the source frames. Leaving it
out ships a unit the engine will refuse to register.

## Gotchas index

- ETA ~5-8 min/object; tool says 30-90s. Plan batches accordingly.
- ≤10 concurrent jobs; objects expire in 8h.
- Soft freeze ⇒ band-restore always; tinted rebuild for color-shifted stages.
- Seed = composition lineage; bases must be re-generated through the same seeded pipeline.
- `init_image` ≠ style transfer (locks color); `color_image` broken; build JSON via file.
- "No dirt/soil" prompt clause for anything grounded; shape-not-species prompts on tiny canvases.
- macOS `base64` has no `-w`; use `base64 < f | tr -d '\n'` or python.
- Animation direction-jobs can **silently vanish** (one of 200 did) — audit per activity×direction
  after a matrix run and re-queue gaps.
- The raw API **partially queues** animations: it takes as many direction-jobs as slots allow and
  returns which `directions` it accepted — a queue-feeder loop must track per-direction state
  (see the runner pattern: poll, post missing dirs, subtract accepted, repeat).
- Backblaze frame URLs in the character JSON return 401/403 — always download via
  `GET /v2/characters/{id}/zip` instead.
- v3 sometimes returns frame_count+1 frames; take the first N.
- A 25-animation matrix (200 direction-jobs) takes ~2 h with a continuous queue-feeder.
