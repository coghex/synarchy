# Eighth-level fluid rendering — integration review (#2529)

Owner verdict: **approved, 2026-09-24**. After the preliminary Lake
progression checkpoint, the owner reviewed the integrated comparisons and said:
“those look right, the ice scenes are identical before and after, which i think
is correct”. This records visual acceptance of the integration; canonical PR
review remains a separate delivery gate.

The unchanged ice scenes are expected: terrain is at z=30, the authored partial
water surface is at z=30⅜, and ice remains at z=31. The water is above the terrain
bed and beneath the ice, which hides its top.

Open [index.html](index.html) locally for the 72 paired views. Select a scene,
facing and day/night setting. Original PNG links preserve the full 1280×720
capture; no pixels were rescaled, recolored or edited in this archive.

## What to review

- Lake, River, Lava and partial Ocean: eight separated layers beside dry
  terrain and an adjacent staircase. Both use exact surfaces −31…−24 eighths
  above terrain −4. The adjacent row crosses the loaded x=16 chunk boundary.
- Tall stacks: full-z strips capped by a partial top, different left/right
  neighbour heights, multi-z drops, and 3×3 equal-height pools with an enclosed
  center. Look for cracks, excess strips and seam-dependent changes.
- Ice: ordinary generated seed-42 terrain contains dry drape and basin ice.
  Both remain at z=31. Partial Lake and Ocean were added at exact surface
  243/8 = 30⅜ under existing ice, which stays at z=31 with its original mode.
  Covered tops remain hidden. The basin-interior views are uniform ice;
  the drape-boundary views provide visible terrain/ice edges.

The driver polls ice height/mode on both builds. Headless production-path
checks independently cover fluid-ceiling-derived ice bases, dry drape,
basin fill and its 20-z cap. Ice generation and rendering code are unchanged.

## Provenance and fixture

Base: `d2a782088b4477bef2ba6ec8c965c2dc11bced25`.
Rendering implementation: `7321a5a627a66d7eba8738a2fe7c8bf1ec07e87d`.
The subsequent `e13fb8d1f` commit only improves the capture driver: explicitly
bind each restored page's textures and allow a named scene subset.

All runs use the same normal session save, SHA-256
`02c2b839392aeda79e0793c102420187b83519125afbec37fb0bc5ff20316abb`.
It is retained in `fixture.tar.gz` with `recipe.json`. The debug-only
`debug.setFluidSurface` authoring hook records `WeSetFluidSnapshot`; the
normal save codec and load replay carry those exact values to the base.
There is no exact-unit public query/setter change or save-format change.

`fixture-summary.json` is produced by the real compiled save decoder. All
112 authored arena cells match the recipe's coordinate/type/exact-height
FNV-1a digest `626316aa8f9799ca`. The generated ice page's remaining snapshot
cells are natural generation. Every run's manifest records binary/driver
hashes, source revision, resource root, invocation, camera and lighting.
Camera/scene records agree for all 72 pairs. `sun_angle` in these manifests
is the requested clock phase: day=3:00 (0.125), night=18:00 (0.75); the normal
renderer still applies its location-dependent sunlight calculation.

The first baseline run captured the arena correctly but left the second,
inactive saved world's textures unbound. Its 32 checkerboard ice frames
are excluded. `before-arena/archive-selection.json` identifies the 40 retained
frames; its untouched original manifest records the full invocation. The
separate `before-ice` run uses explicit per-page rebinding and supplies all
32 valid ice views. The gallery never displays the rejected captures.

## Validation

Production build and headless suite build passed with no GHC warnings.
Targeted groups, all nonempty and passing:

| Group | Examples |
|---|---:|
| World.Render.FluidLevels | 17 |
| World.Render.SideFace | 24 |
| World.Render.PickSeam | 19 |
| World.Slope.slopeBit | 21 |
| World.Slope.FaceMaps | 15 |

Texture-path, Lua-registration, persistence-inventory, capability-inventory,
module-budget and Unicode-operator audits passed. The approved eight PNGs
match their production Haskell generator and regenerated PNG bytes, using
Pillow 11.3.0 as required by that gate. No mask bytes, zoom-map classification,
terrain slope behavior or tint constants changed. Build/test output is under
`validation/`; screenshots are human evidence, not a pixel-equality gate.

## Reproduce

Extract the fixture into an empty directory, preserving `recipe.json` beside
`fixture/`. Use the current driver in this PR with independently built base
and implementation executables and the matching resource checkout for each.
`--engine` and `--codec` accept retained immutable executable copies. Without
these arguments the driver builds through the shared Cabal lock. Every run
uses an isolated resource root, port 9429 or another non-8008 port, and
`--offscreen`; it stops only its own engine. `--scenes` is optional.

Exact original invocations follow. The temporary paths identify the original
runs; substitute your extracted fixture and built executable paths when
reproducing. A fresh default invocation of the current driver captures the
complete matrix with per-page texture binding.

### Implementation

```sh
python3 tools/fluid_levels_render_capture.py --port 9429 --size 1280x720 --engine /tmp/foreground-2529-build/synarchy --codec /tmp/foreground-2529-build/synarchy-save-codec --out /tmp/foreground-2529-complete-draft2
```

### Base arena

```sh
python3 tools/fluid_levels_render_capture.py --port 9431 --size 1280x720 --engine /tmp/foreground-2529-base/synarchy --codec /tmp/foreground-2529-build/synarchy-save-codec --source-root /Users/vincentcoghlan/work/synarchy --fixture /tmp/foreground-2529-complete-draft2/fixture --out /tmp/foreground-2529-complete-base
```

### Base ice

```sh
python3 tools/fluid_levels_render_capture.py --port 9431 --size 1280x720 --engine /tmp/foreground-2529-base/synarchy --codec /tmp/foreground-2529-build/synarchy-save-codec --source-root /Users/vincentcoghlan/work/synarchy --fixture /tmp/foreground-2529-complete-draft2/fixture --scenes dry-drape-ice dry-basin-ice covered-partial-lake covered-partial-ocean --out /tmp/foreground-2529-base-ice
```
