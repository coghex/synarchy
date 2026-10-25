# Wire art: pre-change save fixture and evidence (#2516)

## Fixture

`pre-2516.tgz` is a save written by a binary built at master 047832b6b,
BEFORE any change in this PR (old `wire.yaml`, the shared `floorface.png`
facemap on every Wire piece). Unpack it and point the verify phase at the
directory:

    tar xzf docs/pr-proofs/wire-save-fixture/pre-2516.tgz -C /tmp
    python3 tools/wire_art_save_probe.py verify --fixture /tmp/pre-2516 --port 9517

`world.synworld` sha256 `6913ab6797f6ded44f4afc8d069c288489776c8b40c846e4a0658a2932fa254c`.

Generated with (the probe's own `make` phase, from the pre-change binary):

    SYNARCHY_PROBE_ENGINE_EXE=<synarchy built at 047832b6b> \
      python3 tools/wire_art_save_probe.py make --out <dir> --port 9516

Seed 42, worldSize 64, 4 plates, page `wire_art_fixture`. Contents: a solar
panel (tile 36,-8) and battery (40,-8) joined by a five-tile Wire run
(36..40,-7) in chunk (2,-1), which is genuinely EVICTED before the save
(the camera is driven through fresh ground until `world.getChunkInfo`
reports it unloaded); a paid, half-built Wire site at (44,-7) with its
material receipt; and a two-tile resident run at (168,-8),(169,-8). The
saved pieces store `assets/textures/facemap/floorface.png`.

## verify phase (post-change binary, fresh process)

61 checks passed, covering: the Wire chunk comes up NOT loaded; resident
and evicted-then-reloaded Wire keep their shapes, STILL STORE the legacy
facemap and are DRAWN (`structure.drawnFacemap`, the render pass's own
resolver) with their shape's dedicated map; network membership, battery
charge and the site's paid state, progress and receipt are unchanged. It
then RESAVES, restarts in a fresh process and repeats every check.

## Offscreen lighting regression (`tools/wire_lighting_probe.py`)

    python3 tools/wire_lighting_probe.py --port 9518

```
  PASS  unit/building defs populated
  PASS  a real arena page is active
  PASS  the arena renders and picks
  static: 972 subject pixels common to all sun angles
    hour   mean    top    side   side/top
    7       78.3   91.5   65.4    0.71
    9       71.3   83.2   59.5    0.72
    12      42.8   46.3   39.3    0.85
    15      21.9   23.7   20.2    0.85
    17      14.1   15.3   13.0    0.85
  PASS  static: the subject is on screen at every sun angle
  PASS  static: brightness changes with the sun
  PASS  static: the side/top ratio changes with the sun — per-face shading from the facemap, not one global factor
  construction (50 %): 754 subject pixels common to all sun angles
    hour   mean    top    side   side/top
    7       78.0   90.0   65.4    0.73
    9       70.9   81.8   59.5    0.73
    12      42.7   45.9   39.3    0.86
    15      21.9   23.5   20.2    0.86
    17      14.1   15.2   13.0    0.85
  PASS  construction (50 %): the subject is on screen at every sun angle
  PASS  construction (50 %): brightness changes with the sun
  PASS  construction (50 %): the side/top ratio changes with the sun — per-face shading from the facemap, not one global factor
  destruction (mid-clip): 754 subject pixels common to all sun angles
    hour   mean    top    side   side/top
    7       78.4   90.8   65.4    0.72
    9       71.4   82.5   59.5    0.72
    12      42.8   46.1   39.3    0.85
    15      21.9   23.6   20.2    0.86
    17      14.2   15.3   13.0    0.85
  PASS  destruction (mid-clip): the subject is on screen at every sun angle
  PASS  destruction (mid-clip): brightness changes with the sun
  PASS  the colour image is byte-for-byte what it was at the start
captures in /Users/vincentcoghlan/.local/state/synarchy-2516/lighting
```
