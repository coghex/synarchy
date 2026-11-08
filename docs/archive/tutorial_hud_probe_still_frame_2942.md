# tutorial_hud_probe: why the frame was never still (#2942)

Verdict: the **probe** was at fault. It captured a live, running world; the
engine's render is deterministic once the simulation is paused. No HUD or
renderer change was needed.

## Evidence

Focused offscreen run (1280x720, current master `19fe2bd86`), real menu flow
into a generated world, popups 0 active / 0 queued, then three consecutive
`debug.captureScreenshot` captures of an unchanged state, 0.5 s apart, full-frame
RGB difference:

| State | Capture pair | Differing bounding box | Changed pixels | Max luma delta | Pixels > 8 luma |
|---|---|---|---|---|---|
| live (not paused) | 0 to 1 | (0, 0, 1280, 720) | 205,217 (22.3%) | 1 | 0 |
| live (not paused) | 1 to 2 | (0, 0, 1280, 720) | 189,161 (20.5%) | 1 | 0 |
| live (not paused) | 2 to 3 | (0, 0, 1280, 720) | 206,755 (22.4%) | 1 | 0 |
| `engine.setPaused(true)` | 0 to 1, 1 to 2, 2 to 3 | none | 0 | 0 | 0 |
| paused, popups dismissed | 0 to 1, 1 to 2, 2 to 3 | none | 0 | 0 | 0 |

So the live world repaints about a fifth of the frame, scattered across the
whole framebuffer, by one luma level, every capture. Every strict
"changed pixel" oracle in the probe sees that: the transparency check's
changed fraction reads 1.000 over the list rect, `_differing_columns` reports
hundreds of drifting columns between two captures of an unchanged state, and
the sticky-branch "frame held still" check fails. This is the same
whole-frame flicker #2654 found in
`construction_blueprint_footprint_probe.py` ("the simulation is not paused
between captures").

Candidates from the issue, as measured: the simulation running (calendar,
time-of-day lighting, idle animation) is the cause; pausing removes the
difference completely. A camera still easing is not needed to explain it
(the paused frames are identical after the same 3 s settle). Notification
popups were not present in this run (0 active, 0 queued), so they did not
cause it here; the fix still clears them because a popup appearing between
captures would also move pixels (#1765).

## Fix

`hold_world_still` in `tools/tutorial_hud_probe.py`, called at the end of
`reach_hud`: `engine.setPaused(true)`, confirm `engine.isPaused()`, dismiss
popups, wait one second, then a new unthresholded check that two captures of
the held frame are identical over the whole framebuffer (the regression gate;
it fails with the differing region when the world moves, as the live rows
above would). Pausing stops unit movement and the page clock only. UI ticks,
tutorial evaluation and the HUD's presentation acknowledgement keep running,
so the checks that drive the tutorial model are unchanged.

No threshold, no pixel-difference criterion and no retry structure was
touched.

## Result

Focused `python3 tools/tutorial_hud_probe.py` runs on the fix, all
"ALL CHECKS PASSED" with no retries: 216 checks in 180.9 s (fix only), 217 in
147.3 s (with the gate), and 218 in 150.4 s on the final head, against 269 to
300 s for the red shakedown runs.

## Trap found in review

The first version of the identity gate compared RGBA frames with
`ImageChops.difference(...).getbbox()`. Pillow's `getbbox()` looks at the alpha
channel only for RGBA images, and the engine forces every screenshot's alpha to
255, so the difference is all-zero alpha and the gate reported "identical" for
any colour change. The gate now compares RGB, and a negative control inside the
probe proves a one-level RGB difference on opaque frames is detected.
