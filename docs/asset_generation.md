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
   config, `unit_resources.lua` species block, `unit_ai.lua` COMBAT_ANIM_SUFFIX entry) — engine/
   Lua territory, check with the user first.

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
