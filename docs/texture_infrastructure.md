# Texture Infrastructure Plan

**Status:** Pre-implementation, written 2026-05-24.

## Goals

Lay foundations for a unit-animation pipeline that scales to:
- Multiple factions × multiple unit types × multiple weapon classes
- 8 directions × 4–16 frames per animation
- AI-generated frame sequences as the authoring input
- Editing-friendly PNG sources alongside compressed shipping atlases
- Clear errors on missing/extra assets

Set the pattern **before the second unit lands** so the first unit migration validates the design and every subsequent unit drops into a stable groove.

## Non-goals

- Per-frame anchor points / compositional equipment rendering (deferred indefinitely).
- Streaming / async asset loading (not needed at current scope).
- Mip-mapping (pixel art uses NEAREST sampling; mips would soften edges).
- LOD systems (worry about it when the world is bigger).

## Background: how GPU texture compression works

A confusion to clear up first: GPU-compressed formats (BC7, BC1, ASTC) **stay compressed in VRAM**. The GPU's texture sampler hardware decompresses on-the-fly during shader sampling. Compression saves VRAM, disk, AND load time, with no shader code changes.

This is fundamentally different from PNG/JPEG, which must be fully decoded to raw RGBA on the CPU before upload to VRAM.

**Format choice for this project:** **BC7** in a **KTX2** container.

- BC7 is high-quality, handles sharp pixel-art edges well, supports alpha. ~4× compression vs raw RGBA.
- KTX2 is the modern Khronos container — Vulkan-native, well-tooled, supports BasisU transcoding if multi-platform becomes a concern later.
- macOS/MoltenVK supports BC7 through Metal translation.

Tooling: `toktx` from [Khronos KTX-Software](https://github.com/KhronosGroup/KTX-Software). Install via `brew install ktx`. Command pattern:

```bash
toktx --t2 --encode uastc --uastc_quality 4 \
      --uastc_rdo_l 1.0 --zcmp 18 \
      atlas_out.ktx2 atlas_in.png
```

(UASTC + KTX2 zlib supercompression — universal, transcodes to BC7 at load. Easier than committing to BC7 directly and dealing with platform variance.)

## Directory layout

```
assets/
  units/
    acolyte/
      animations.yaml        # manifest — single source of truth
      sources/               # editable PNG frames (gitignored .ktx2 alongside is fine)
        idle/
          n_0.png n_1.png n_2.png n_3.png
          ne_0.png ne_1.png ...
          ...
        walk/
          ...
        combat/
          unarmed/
            attack/
              ...
            hit_react/
              ...
          1h_melee/
            attack/
              ...
      atlases/               # build output (gitignored — generated)
        idle.png
        idle.ktx2
        walk.png
        walk.ktx2
        combat_unarmed_attack.png
        combat_unarmed_attack.ktx2
        combat_unarmed_hit_react.ktx2
        ...
    brown_bear/
      animations.yaml
      sources/
        ...
      atlases/
        ...
```

**Rationale:**

- `sources/` holds individual frame PNGs — what the artist (or AI tool) produces. Editable, hot-reloadable during dev.
- `atlases/` holds the packed-and-compressed output — what the engine actually loads in release. Both PNG (debug) and KTX2 (ship) atlases live here.
- One subdir per animation, one folder deep for combat / class. Easy to navigate, easy to script.
- `animations.yaml` at the unit root is the manifest — the engine reads this; never does filesystem discovery in production. Discovery is for the validation tool only.

**File naming:** `<dir>_<frame>.png` where `<dir>` is one of `n`, `ne`, `e`, `se`, `s`, `sw`, `w`, `nw` and `<frame>` is 0-indexed. If `flip` is enabled in the manifest, only `n`, `ne`, `e`, `se`, `s` need to exist; `nw`/`w`/`sw` are mirrored at runtime.

## YAML manifest schema

```yaml
# assets/units/acolyte/animations.yaml
unit: acolyte

# Defaults applied to every animation unless overridden.
defaults:
  directions: 8        # 8 | 5 (with flip)
  fps: 12              # 12 fps = ~83ms per frame
  loop: true

animations:
  idle:
    frames: 4
    fps: 6             # slower than default
    flip: true         # 5 source dirs, 3 mirrored
  walk:
    frames: 8
    flip: true
  run:
    frames: 8
    flip: true
  pickup:
    frames: 4
    loop: false
  idle_fight:
    frames: 8
    flip: true
  hit_react:
    frames: 4
    flip: true
    loop: false
  death:
    frames: 8
    flip: true
    loop: false        # holds last frame

# Combat animations are grouped by weapon class. Each class adds the same
# set of attack-side animations on top of the base set above.
combat:
  unarmed:
    attack:
      frames: 6
      flip: false      # holds visible "right hand" punching — don't mirror
      loop: false
    # Future: attack_heavy, etc.
  1h_melee:
    attack:
      frames: 6
      flip: false      # weapon stays in right hand
      loop: false
```

**Key decisions baked into the schema:**

1. **`flip: true|false` per animation.** This is the user's concern — a character holding a weapon in their right hand cannot be naively mirrored. The manifest declares which animations are bilaterally symmetric (idle, walk, hit_react, death — usually most non-combat) vs. asymmetric (anims that involve a held prop). The atlas-packer respects this: `flip: true` packs 5 directions; `flip: false` packs 8.

2. **`loop: true|false`** controls whether the animation cycles or holds its last frame. Critical for `death`, `pickup`, `attack` (one-shot) vs. `idle`, `walk` (continuous).

3. **`fps` per animation.** Overridable so `idle` can breathe slowly while `attack` snaps.

4. **`combat:` groups class-specific anims** without mixing them into the base set. Adding a new weapon class = new top-level key under `combat:`.

## Atlas-packer tool

A Python script at `tools/pack_atlas.py` that takes a unit directory and produces all atlases listed in `animations.yaml`.

**Behaviour:**

1. Reads `animations.yaml`.
2. For each animation, looks at `sources/<animation_path>/`.
3. Validates that exactly the expected frame files exist (5 dirs × N frames if `flip: true`, 8 dirs × N frames otherwise). Loud error if mismatch.
4. Packs into a single atlas image: rows = directions, columns = frames. Atlas is `(48 × frames) × (48 × dirs)` pixels (e.g., 384×240 for 8 frames × 5 dirs).
5. Writes `atlases/<animation_name>.png` (debug) and `atlases/<animation_name>.ktx2` (ship) — both, always. KTX2 via `toktx` shellout.
6. Writes a `.json` sidecar next to each atlas with: `{frames, dirs, fps, loop, flip}` — so the runtime doesn't need to re-parse YAML to know packing geometry. (Optional refinement: bake this into KTX2 metadata via `--genmipmap` flags if `toktx` supports custom KV pairs.)

**Invocation:**

```bash
# Pack one unit
python3 tools/pack_atlas.py assets/units/acolyte

# Pack everything
python3 tools/pack_atlas.py --all

# Validate only (no output) — for CI
python3 tools/pack_atlas.py --validate-only --all
```

**Why Python:** ergonomic image manipulation via PIL, simple shell-out to `toktx`, no engine recompile needed when iterating on the tool. Existing project tooling at `tools/` follows this pattern.

## Validation tool

Same script with `--validate-only`. Checks:

1. Every `animations.yaml` declared animation has the matching source files (exact count, correct names).
2. No orphan source files in `sources/` that aren't declared in the YAML.
3. All frames within one animation are the same dimensions (catches AI-tool size drift).
4. Atlas KTX2 files are present and newer than their source frames (skip-rebuild logic).

Add to CI as a separate step from `cabal build`. Fast (filesystem-only, no image decode required for the count check).

## Runtime loader integration

**Phase 1 — minimum changes to existing engine.**

1. New module `Engine.Graphics.AnimationManifest` that reads `animations.yaml` and produces an in-memory map: `(UnitType, AnimationName, WeaponClass?) → AnimationMeta`.
2. `AnimationMeta` carries `{ atlasHandle, frames, dirs, fps, loop, flip }`.
3. Existing texture-loading path (presumably already loads PNGs into bindless slots) gains a code path for KTX2:
   - Prefer `.ktx2` if present.
   - Fall back to `.png` if `.ktx2` is missing (dev convenience).
4. Existing sprite-draw path picks the row in the atlas by direction: for `flip: true` animations, if the requested direction is one of `nw`/`w`/`sw`, look up the mirrored direction and flip UVs at draw time.

**UV flip implementation:** add a `flipX :: Bool` parameter (or a sign bit on the existing tex-coord) to the per-quad data. Single shader change — flip U coordinate in the vertex shader if the flag is set. Free at runtime.

## Migration order

This is the work plan. Each step is independently shippable; don't bundle.

1. **Add the `flipX` UV flip path to the sprite renderer.** No assets touched yet — just the engine capability. Verify with a manual flip on an existing sprite. (~30 min if the renderer is straightforward.)

2. **Pick KTX2 format + install `toktx`.** Document the install in the project README. Pin the tool version.

3. **Write `tools/pack_atlas.py` (validate-only mode first).** Walks the directory tree, reports what would be packed. Run against an existing unit's source frames as a dry-run; iterate on the script until it reports cleanly.

4. **Extend `pack_atlas.py` with actual packing (PNG output only, no KTX2 yet).** Manually inspect the packed atlas PNGs to verify dir/frame layout.

5. **Add KTX2 output to `pack_atlas.py`.** Shell out to `toktx`. Verify a KTX2 atlas loads in the engine.

6. **Migrate the acolyte's existing animations to the new layout.** Rename source files into `assets/units/acolyte/sources/<anim>/<dir>_<frame>.png`. Author `animations.yaml`. Run packer. Wire the engine's animation lookup to read from the manifest.

7. **Decide PNG-vs-KTX2 dev workflow.** Recommended: KTX2 is the default load path; PNG atlas falls back if KTX2 missing. Devs can `rm assets/units/.../atlases/*.ktx2` to force PNG hot-reload during iteration.

8. **Add validation to CI.** A `cabal test` extension or a separate `make validate-assets` target. Block PRs that miss assets.

9. **Author the brown bear's `animations.yaml` + sources.** First test of the system with a NEW unit. Should be drop-in if steps 1-7 went well.

10. **Author the unarmed combat animations for both units.** This is where the combat work formally begins; the infrastructure is now ready to receive it.

## What this plan does NOT cover

- Combat sim itself (separate plan).
- Particle/effect overlays for weapon-class differentiation (separate work; comes after class anims exist).
- Equipment-as-overlay rendering (deferred indefinitely per discussion).
- Per-frame anchor points (deferred indefinitely).
- Material-set / faction-palette skinning (deferred; current plan uses baked colors per faction unit).

## Open questions

- **Hot reload:** is texture hot-reload during dev a requirement? If yes, the PNG-fallback path needs filesystem-watching; if no, restart-to-reload is fine. Default to "no" for simplicity unless it slows iteration meaningfully.
- **CI duration:** `toktx` on a single 384×384 atlas is fast (<1s). On 100+ atlases it's still seconds, not minutes. But validate-only mode is essentially free and should run on every PR.
- **Per-faction palette swaps:** if the same acolyte sprite needs to render in different faction colors, that's a shader uniform per draw rather than a per-faction asset duplication. Worth deciding before authoring the second faction.
