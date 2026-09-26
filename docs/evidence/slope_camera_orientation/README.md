# Slope camera orientation correction

Implementation and rendered evidence, 2026-09-26. The owner accepted the actual
correction and authorized a PR: “that looks right, make this a pr”.
Open `index.html`; B is selected initially.
This work is separate from the experimental water backend and PR #2736.

## Change

Previously, camera rotation moved terrain tiles but selected the same slope
face map and UVs. Lighting changed with the camera; the lowered silhouette
remained screen-facing. The fixed renderer derives the visual mask from the
world slope flags and camera facing at texture selection. Terrain, vegetation
and crop overlays, and spoil overlays use the same orientation convention.
World state and existing art are unchanged. A world E mask selects E/N/W/S
through South/West/North/East facings; combined masks rotate together. Invalid
mask IDs retain the existing flat fallback.

This does not establish a physical bed for the water solver, revise authored
mask shapes or shading, or add exact slope-pixel picking. The current picker
uses occupied tile columns; its policy is unchanged and its seam tests pass.

## Captures

`before/` and `after/` contain sixteen unedited 1280×900 engine captures each:
an isolated E slope with a world-east marker, the two-row descending hill,
all sixteen terrain masks, and all sixteen vegetation masks. Each fixture's
flags are authored once, before capture. The driver checks all 66 sampled
tiles' terrain, slope, vegetation and dry state after every rotation and again
at the end. It does not rewrite slope flags between views.

All four FaceSouth (view 0) images are pixel-identical before and after. The
other views change; `pixel-comparison.json` records their changed bounds.
Before/after fixture readbacks are identical. Individual image comparisons
establish this run's regression control, not a cross-machine pixel baseline.

The before executable is the existing local arena build at base
`04f5b75a749bf1e5eedfc95b77d9759520ea3b58` with its opt-in water experiments
inactive. Its terrain/vegetation/spoil mask selectors are the unchanged
production selectors. The after executable is the isolated fix worktree at
base `5a44b615b`, with the changes described here. Each manifest records the
startup executable hash. `source-hashes.json` identifies the fixed source and
capture driver; the files accompany this evidence in the same delivery.

Runtime resource roots are ignored, so the evidence does not publish local
configuration or absolute asset symlinks. Both owned offscreen engines stopped.

## Validation

The engine and headless tests built successfully. A name clash with Hspec's
`context` in the new test helper was fixed before the final test build; the
initial and final build logs are retained under `validation/`.

| Hspec group | Examples | Failures |
|---|---:|---:|
| World.Render.SlopeFacing | 6 | 0 |
| World.Render.FluidLevels | 17 | 0 |
| World.Render.PickSeam | 19 | 0 |
| World.Slope.FaceMaps | 15 | 0 |
| World.Spoil | 6 | 0 |
| World.Render.SideFace | 24 | 0 |

The new tests exercise all sixteen masks at all four facings through texture
selection, real terrain quads and the shared vegetation/crop quad path, plus
all invalid mask IDs, four-turn identity and missing vegetation. The spoil
tests exercise its existing quantity rules; spoil's rendering call site now
passes facing to the same terrain-mask selector. There is no new rendered
spoil-pile fixture in this capture set.

Unicode and texture-path audits passed, as did `git diff --check`. Viewer
JavaScript syntax, scene/view/A/B controls and all 32 image paths passed a
minimal DOM check. Engine screenshots were visually inspected; browser layout
was not automatically screenshot-tested. `validation.json` summarizes results.

Reproduce each capture with:

```sh
python3 tools/slope_orientation_capture.py --engine /absolute/path/to/engine --out /absolute/new/directory
```

Omitting `--engine` prepares the current engine through the normal exclusive
build-lock helper. The driver uses offscreen Vulkan, port 9550 by default,
and an isolated resource root. Targeted tests use:

```sh
cabal build synarchy-test-headless
cabal test synarchy-test-headless --test-options='--match "World.Render.SlopeFacing"'
```

Repeat the final command for the other five groups above. No whole-world
generation, save migration, new art, fluid simulation or publication is part of
this correction. Keep the evidence and contract with the code PR.
