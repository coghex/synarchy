# #2812 — reload-stage screenshots, image-read verdict

Real `--offscreen` run of `python3 tools/location_embark_probe.py --keep-artifacts`
on the implementer's Mac, branch `issue-2812-reload-state-gate` at the commit
the PR was opened from: **all checks passed**, including both explicit state
checks (target remains discovered, control remains undiscovered).

The images are the 96x96-pixel centre of each full 1280x720 frame, enlarged
4x with nearest-neighbour. Each was read as an image; the probe grades none of them.

| File | Should show | Read as |
|---|---|---|
| `issue-2812-icon_unknown_target.png` | target ruin before the portal: `?` unknown marker | `?` marker on a dark, night-lit background. Matches. |
| `issue-2812-icon_reloaded_target.png` | target ruin after save/quit/restart/load: ruin type icon, not `?` | Tan broken-wall ruin icon, no `?`. Matches: discovered state visibly survived. |
| `issue-2812-icon_unknown_control.png` | control ruin before the portal: `?` unknown marker | `?` marker on a bright grey background. Matches. |
| `issue-2812-icon_reloaded_control.png` | control ruin after reload: the same `?` marker | `?` marker, identical shape and colours to `icon_unknown_control.png`, same background brightness. Matches: undiscovered state survived. |

Scene lighting varies a lot between ruins in this run (the target frames are dark,
the control frames bright grey), so a whole-frame comparison across ruins would grade
lighting, not the icon. In this particular run the same-ruin pairs (before the portal
vs after reload) happened to have matching lighting, so it did not reproduce the
brightness shift seen in the #2786 Tier A run; the state checks, not these frames,
are the gate either way.
