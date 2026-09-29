# Dungeon floor/post pilot

The owner approved these fourteen source-art states on 2026-09-29 in the
requested HTML report: “these all look good, i sign off, continue with the pr
process”. [Open the approved report](index.html) or the
[contact sheet](contact_sheet.png). `manifest.json` binds the approval to exact
PNG and decoded RGBA SHA-256 values at their production paths.

The selected set is four deterministic floor construction stages, four
PixelLab floor damage states, two PixelLab post construction stages, and four
PixelLab post damage states. Construction's final frame and damage's intact
state reuse the existing sprite, with identical decoded pixels. The other ten
colour sprites are copied without editing from the owner's selected outputs.

## Integration contract

`dungeon_1.yaml` declares construction only for the default floor and post.
Progress chooses a completed structural state; bricks do not fly into place.
Three named static variants, `weathered`, `broken`, and `ruined`, expose the
selected damage states independently for ruin/dungeon authoring. Each has its
own floor/post texture and facemap. The existing `damaged` variant and every
existing asset path remain intact.

For example, in a loaded world, through the existing Lua placement API:

```lua
local S = package.loaded['scripts.structures']
assert(S.floor(gx, gy, worldId, baseZ, 'ruined'))
assert(S.post(gx, gy, 's', worldId, baseZ, 'ruined'))
```

`worldId` and `baseZ` may be nil. A post needs a floor. Placement persists the
texture and facemap paths in the existing structure palette. The variants are
visual states with ordinary floor/post behavior; they do not imply structural
health, reduced collision, or an automatic random distribution.

This is a bounded, standalone pilot contributing to #2513, not completion of
its original twenty-sequence contract. That issue remains open for ceiling,
wall, and remaining lifecycle art. No `destruction:` clip is declared: the
current engine removes a demolished piece and plays a transient effect, then
discards it. The owner requested recognizable, independently placeable ruins;
feeding the approved damage art into that disappearing effect would not meet
that direction. Changing demolition to retain a damaged structure requires a
separate gameplay contract and implementation.

## Provenance and masks

`generation.json` retains the selected PixelLab prompts, seeds and job IDs.
The original deterministic floor construction detects connected brick faces
between the source mortar, orders whole bricks from back to front, retains
their source coordinates, and adds seated vertical edges. It includes 25%,
50%, and 75% of those brick groups before the exact complete sprite.

The first generated final floor-damage state changed the brick pattern and
was rejected. Job `f4092457-81a5-4b3f-955f-d91b5f5f09ac` (seed 251401) repairs
the approved preceding state through `floor_damage_mask.png`: 481 changed
pixels inside three patches totaling 495 pixels, zero changes outside them.
No cleanup or recoloring follows the selected PixelLab outputs.

Static rendering multiplies sprite alpha by facemap alpha. Reusing the intact
mask would clip damaged-post rubble. The six derived masks instead match each
approved sprite's alpha exactly, retain the original face-lighting channels
where defined, and use top lighting for newly exposed pixels, matching the
lifecycle shader's fallback. Transparent mask pixels are black. This changes
no colour-art pixels and requires no renderer change.

```sh
python3 tools/dungeon_lifecycle_art.py
# Rebuild only the derived masks, then run the same audit:
python3 tools/dungeon_lifecycle_art.py --write-facemaps
```

The audit checks all fourteen hashes, dimensions, binary alpha, distinct
states, exact final construction handoffs, YAML declarations, all six mask
silhouettes, and the final floor edit boundary. Pillow and PyYAML are required.
Prior rejected experiments remain local in the pilot worktree; they are not
dependencies of this archive or the shipped art.

## Validation

The production build, targeted Hspec results, and native preview/capture
evidence are recorded in `validation.md`. The source-art approval above was
given in the owner's requested browser report; engine checks are reported
separately rather than described as an additional owner verdict.

Reproduce the real arena captures (Vulkan, no window):

```sh
python3 tools/dungeon_lifecycle_capture.py --out /tmp/dungeon-lifecycle-capture
```

The first row contains intact/weathered/broken/ruined floors; the second row
adds matching south-corner posts. `world/placed.json` records all twelve
placements and their resolved palette paths. The four PNGs show camera
rotation and actual static rendering, including the masks' rubble coverage.

Foreground session: `20260929T130823Z-art-dungeon-structure-lifecycle-74b3d2`.
