# Wire at gameplay scale (#2516, checkpoint 3 — flat Wire only)

Offscreen captures (`--offscreen`, no window) of the flat Wire art at the
camera's default gameplay zoom (0.5), flat arena, clock frozen. Every file
has a `_x4` sibling: a nearest-neighbour 4x crop around the cable, because
the cable is 2 px wide at gameplay scale.

Regenerate (needs a GPU, manual only):

    python3 tools/wire_gameplay_capture.py --out docs/pr-proofs/wire-gameplay --port 9518

`captures.json` lists each file and what it shows.

## What is here

| Requirement 8 item | Files |
|---|---|
| Two-tile run at all four camera facings, three sun positions (world hours 7, 12, 17; hour 7 is brightest, 17 darkest) | `facing_{south,west,north,east}_hour{07,12,17}` |
| Construction completing a neighbour's shape change | `neighbour_0_lone_tile` (isolated) → `neighbour_1_neighbour_half_built` (next tile paid, 50 %, its construction frame drawn) → `neighbour_2_completed` (first tile re-capped to `end_e`, new tile `end_w`) |
| Destruction through `structure.clear` | `teardown_0_before_clear` (three-tile run) then `teardown_1..5_clip`: the middle tile at 0.00 / 0.15 / 0.25 / 0.35 / 0.45 game s of its 0.5 s clip (frames 1-5: the static art, cut + sparks, fragments, debris, soot specks) |

The destruction clip runs on `engine.gameTime()`, which pause freezes and
`world.setTimeScale` does not touch; each capture runs the clock for a
measured real-time step inside one console call, so the frame it holds is
the one named in `captures.json`.

## Correction (round 1 of the review, owner-approved)

The first set's `neighbour_1_neighbour_half_built` drew the designation
GHOST, not a construction frame: the capture's payer unit carried no wiring,
so the site was never paid, and an unpaid site draws the ghost. The capture
now gives its payer wiring, asserts that `construction.payMaterials`
succeeded and that the site reads back paid at progress 0.5, and so draws the
real paid construction frame (frame 3 of 4). The completed-neighbour frame
no longer shows the payer unit. The owner approved the corrected frame
("Approve corrected frame"). Every other capture is pixel-identical to the
first set outside the arena's wandering bear.

## Findings to read before signing off

1. **Wire art does not rotate with the camera.** At the default facing
   (south) the two tiles of a run join into one continuous cable. At west,
   north and east the same two tiles draw as two cable segments that do NOT
   meet: each shape's sprite is a fixed picture keyed to the WORLD
   connection (`wireShapeFor` knows nothing of the facing), while the tile
   grid itself rotates. This is how Wire has always rendered (the old
   artwork drew its spokes to the same fixed screen edge midpoints); this
   issue changes the art, not the selection rule, and a camera-aware shape
   rule is outside its stated scope ("new lighting models or unrelated
   renderer refactoring"). It is worth its own issue.
2. The dark diamond under the half-built and completed neighbour tile is
   existing behaviour of the capture's driver, not of the art:
   `construction.addJobProgress` stamps the D-18 corner slope into the
   terrain, and cancelling the designation does not undo it. A real build
   job does not hit this path the same way; the teardown scene is on a fresh
   row for that reason.
3. The completed neighbour is placed by `scripts/wire.place`, which is the
   exact call the construct AI makes on completion.
