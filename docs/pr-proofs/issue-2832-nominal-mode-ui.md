# #2832 — physiology modes in the unit UI: real offscreen evidence

S6 of epic #2827. The unit-info Status panel shows a unit's resolved physiology
modes: **Simulated** rows are unchanged, a supported **Nominal** row reads its
healthy value (a pool reads `Nominal`) in the neutral colour with the tooltip
"Not simulated for this unit: always healthy.", and an **Unsupported** (bodyless)
unit's physiology rows are hidden. A headless dump is not the evidence here;
these are real `--offscreen` frames (GPU on, window off) of the shipped panel.

## Provenance

| | |
|---|---|
| Code under test | branch `issue-2832-nominal-mode-ui` at **`ed182e83b0bc6ed40527771584c973035ba80fec`** (the implementation commit; later review-round commits are recorded in the pull request, and the captures are re-taken if a round changes the panel) |
| Build | `cabal-3.16.1.0 build all`, production profile, GHC 9.12.2, macOS (Apple Silicon) |
| Resource root | the task worktree `~/worktrees/coghex/synarchy/issue-2832-nominal-mode-ui` (`--resource-root`); no `config/*.local.yaml` |
| Port | **9018** (never 8008); one engine at a time, each stopped by its own `engine.quit()` |
| Viewports | 1920x1080 (full survey), 1280x720 and the formal minimum 800x600; UI scale 1.0 |

Setup and capture, exactly as run (the driver is
[`issue-2832-capture.py.txt`](issue-2832-capture.py.txt); copy it to a `.py`):

```sh
cd ~/worktrees/coghex/synarchy/issue-2832-nominal-mode-ui
cabal-3.16.1.0 build all
python3 issue-2832-capture.py --out ev1080 --size 1920x1080 --mode full
python3 issue-2832-capture.py --out ev720  --size 1280x720  --mode owned
python3 issue-2832-capture.py --out ev800  --size 800x600   --mode owned --quick
# each run boots:  $(cabal-3.16.1.0 list-bin exe:synarchy) --offscreen --port 9018 \
#                  --size WxH --arena --resource-root "$PWD"
# waits for READY, the unit defs and the arena view, then:
#   engine.setPaused(true); world.setTimeScale(0); world.setSunAngle(0.5); engine.hideDebug()
#   unit.spawn x4 (acolyte, bear_brown, nomad_primitive, tiller) at tiles around pickTile(centre)
#   unit.injure(tiller, 'torso', 'slash', 0.5, 1.0)        -- the bodyless unit's injury
# per unit: unit.hitTestAt locates the sprite; input.click SELECTS it through the shipped UI;
#   input.scroll over the stats rect SCROLLS the panel (the panel shows 1-3 rows at these sizes);
#   input.moveMouse hovers a Nominal row (the engine's own 400 ms dwell raises the tooltip);
#   debug.captureScreenshot per scroll window; engine.quit() at the end.
```

Two console writes are the only state changes, and both are production verbs:
`physiology.setMode` (the facade's transition verb, used for the transition
frames) and `unit.injure` (the bodyless unit's wound). Nothing writes a pool,
a mode override or a stat to make a frame look healthy.

Unit modes as the engine resolved them (`unit.getPhysioMode`, recorded in every
sheet title):

| unit | thermo | salt | hydration | nutrition |
|---|---|---|---|---|
| acolyte (`rich`) | simulated | simulated | simulated | simulated |
| bear_brown (`reduced`, mixed) | simulated | simulated | nominal | nominal |
| nomad_primitive (`exempt`) | nominal | nominal | nominal | nominal |
| tiller (bodyless) | nil | nil | nil | nil |

## What each image shows (each read as an image)

The sheets are the panel's top region cropped from the full frames at each
scroll window (left to right = scrolling down the Status list); the
`captures-*.json` files hold, per frame, the selected uid, the scroll offset and
the text of every visible row, as read back from the shipped labels.

### 1920x1080 — full survey

| File | Read as |
|---|---|
| [`issue-2832-1920x1080-acolyte-simulated.png`](issue-2832-1920x1080-acolyte-simulated.png) | All Simulated: `4.3 / 4.3 L`, `37.0°C`, `100%`, and the pool pairs `571.9 / 571.9`, `1143.9 / 1143.9`, `34.3 / 34.3`; today's formats, no `Nominal` anywhere. |
| [`issue-2832-1920x1080-bear-mixed.png`](issue-2832-1920x1080-bear-mixed.png) | Mixed: Body Temp `37.0°C` (green, thermo Simulated) and Salt `100%` (salt Simulated) show their normal readouts; Hunger, Calories and Hydration read **`Nominal`** in white. |
| [`issue-2832-1920x1080-nomad-nominal.png`](issue-2832-1920x1080-nomad-nominal.png) | All Nominal: Body Temp `37.0°C` and Salt `100%` are the healthy readings from their own systems; Hunger, Calories, Hydration read `Nominal`. No pool shows `?`. |
| [`issue-2832-1920x1080-tiller-unsupported.png`](issue-2832-1920x1080-tiller-unsupported.png) | Bodyless: **no** Body Temp, Salt, Hunger, Calories or Hydration row (Pain goes straight to Heart Rate). The unowned rows remain: Blood, Pain, Heart Rate, Blood O₂, Consciousness, Circulation, Stamina and Sleep (the last two read `?`: unrolled unowned stats on this unit, unchanged behaviour), and the **injury rows survive**: `Bleeding` and `Deep lacerati…`. |
| [`issue-2832-1920x1080-transition.png`](issue-2832-1920x1080-transition.png) | One selected acolyte, never reselected: Simulated (`921.3 / 921.3`, `1842.6 / 1842.6`, `55.3 / 55.3`) → after `setMode` hydration and nutrition Nominal, the rows read `Nominal`, same tab, same scroll window, same scrollbar → the Nominal tooltip raised on hover → back to Simulated, reseeded healthy. |
| [`issue-2832-1920x1080-nominal-tooltip-frame.png`](issue-2832-1920x1080-nominal-tooltip-frame.png) | The full frame of the hover: the tooltip reads "Not simulated for this unit: always healthy." under its row title. |

### 1280x720 and 800x600 (the formal minimum)

At these sizes the shipped panel shows one row per scroll window, so each sheet
holds the windows for Body Temp, Salt, Hunger, Calories and Hydration.

| File | Read as |
|---|---|
| [`issue-2832-1280x720-acolyte-simulated.png`](issue-2832-1280x720-acolyte-simulated.png) | `37.0°C`, `100%`, `773.2 / 773.2`, `1546.4 / 1546.4`, `46.4 / 46.4`. |
| [`issue-2832-1280x720-bear-mixed.png`](issue-2832-1280x720-bear-mixed.png) | `37.0°C`, `100%` (Simulated), then `Nominal` x3. |
| [`issue-2832-1280x720-nomad-nominal.png`](issue-2832-1280x720-nomad-nominal.png) | `37.0°C`, `100%` (healthy readings), then `Nominal` x3. |
| [`issue-2832-1280x720-tiller-unsupported.png`](issue-2832-1280x720-tiller-unsupported.png) | The whole bodyless list, one row per window: Blood, Pain, Heart Rate, Blood O₂, Consciousness, Circulation, Stamina, Sleep, `Bleeding`, `Deep lacerati…`; no physiology row. |
| [`issue-2832-1280x720-transition.png`](issue-2832-1280x720-transition.png) | Hydration `46.4 / 46.4` → `Nominal` → tooltip → `46.4 / 46.4`, selected throughout. |
| [`issue-2832-1280x720-nominal-tooltip-frame.png`](issue-2832-1280x720-nominal-tooltip-frame.png) | Full-frame hover tooltip. |
| [`issue-2832-800x600-bear-mixed.png`](issue-2832-800x600-bear-mixed.png) | At the minimum viewport the panel is reachable and the same rows read `37.0°C`, `100%`, `Nominal`, `Nominal`, `Nominal`. |
| [`issue-2832-800x600-nominal-tooltip-frame.png`](issue-2832-800x600-nominal-tooltip-frame.png) | The Hydration tooltip fits inside the 800x600 frame: "Hydration — Not simulated for this unit: always healthy." |

## Checks

* **Label**: Nominal pools read exactly `Nominal` (no `?`, no numeric pair).
* **Colour**: Nominal text is white (`CONTENT_VAL_COLOR`); the Simulated Body Temp in the mixed unit keeps its in-range green, so the two are visibly different.
* **Tooltip**: exact string, on the value row; the icon's description tooltip is unchanged.
* **Hidden rows**: all five physiology rows are absent for the bodyless unit.
* **Surviving rows**: injury rows (`Bleeding`, `Deep lacerati…`) and the unowned vitals remain.
* **Transition**: the selected unit's rows and tooltip change without reselecting; tab and scroll window are preserved.
* **Unchanged, not part of this change**: `Sleep` (and, on the bodyless tiller, `Stamina`) reads `?` on a freshly spawned unit (unowned stats not yet rolled) and the tiller's Blood reads `0.0 / 5.2 L` red; both are unowned rows passing through exactly as before.

Not covered by pixels: the legacy debug text (`unit_info_panel.lua`) is never
rendered in the shipped UI (`unit_info_v2` suppresses it), so its
`Hydration: Nominal` lines are verified headlessly in
`UI.PhysiologyModes` through the shipped push path.
