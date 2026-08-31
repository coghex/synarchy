# Rendered Till/Plant toolbar workflow test design

Test design state: `ready for implementation`

Source proposal: `20260830T211843Z-proposal-probe-farming-toolbar-workflow-52a219`

Source test ID: `probe:farming-toolbar-workflow`

Source ref and commit: `origin/master` at `30a764fcbb9b1d9dfb4dfc39c0b10939f5badbea`

Designed against: `origin/master` at `92b9866ed5b278f12a82afa18112837fd894dc23`

Accepted at: `2026-08-31T16:37:39Z`

Implementation authorization: `not granted`

Recommended tier: `manual-only`

## Purpose and coverage gap

Prove the real rendered Till and Plant controls from player click through
selected bitmap, world tool mode, tile interaction, and committed designation.
Current headless coverage stubs or directly invokes these boundaries, while the
registered `till`, `plant`, and `farm_ai` probes call domain APIs without proving
the Vulkan HUD's texture binding and click route. CI compiles the graphical
suite but does not run it. The source-art inspection and the prior naive farming
playtest likewise do not cover this end-to-end path.

## Binding decisions

- Approved for planning as an optional manual-only probe registered in the tools probe inventory. It must not be CI-eligible, included in CI, or added to any required gate; it is intended only for occasional selection by the coordinated $test skill. Approval does not authorize implementation.
- Keep one offscreen, GPU-backed, scenario-heavy probe under the exact registry
  key `farming_toolbar_workflow`.
- The graded path must use `ui.dumpWidgets()` and `input.click`; it must not call
  `world.setToolMode`, `till.designate`, or `plant.designate` directly.
- Direct world mutation is allowed only to create the deterministic plantable
  tile. Naive icon discoverability, AI completion, crop growth/harvest,
  persistence, seam aliases, and aesthetic approval remain excluded.

## Current repository evidence

At the design commit, `scripts/hud.lua` loads the exact normal/selected Till
and Plant PNGs, places `tool_till` and `tool_plant` in the production toggle,
and maps a selection change to world tool modes. `scripts/till_tool.lua` uses a
two-click rectangle: the first click sets an anchor and the second calls the
designation boundary and clears it. `scripts/plant_tool.lua` requires the
selected Plant mode, live-picks the clicked tile, and opens
`scripts/plant_panel.lua` only for a plantable tile.

The accepted proposal described a crop-row click as the commit. Current
`scripts/plant_panel.lua` instead makes the row a preview/selection step and
requires a second rendered `Plant here` button (`plant_panel_plant_btn`) to call
`plant.designate` and close the panel. This design follows that verified
production interaction without changing the accepted end-to-end coverage.

`tools/offscreen_probe.py` already supplies private screenshot storage,
`ui.dumpWidgets()` lookup, injected clicks, world/HUD startup, and broad image
difference helpers. `tools/till_probe.py` and `tools/plant_probe.py` supply the
authoritative designation queries. Neither `farming_toolbar_workflow` nor an
equivalent rendered interaction is registered in `tools/run_probes.py` or
classified in `tools/ci_probes.py` at `92b9866e`.

## Scenario and boundaries

Boot one invocation-owned 1280x720 offscreen Vulkan session on a leased port
and a private resource root. Create a pinned world and enter the zoomed-in
gameplay HUD. Locate `tool_till` and `tool_plant` by widget name and require
distinct, non-empty, in-frame interactive bounds. Verify the production source
registry names all four shipped normal/selected texture paths and retain a
default toolbar crop.

Discover a visible dry tillable tile at runtime with authoritative world
queries and correlate it with screen coordinates through `world.pickTile`.
Click the rendered Till widget. Require accepted input, `world.getToolMode() ==
"till"`, and a meaningful relative image change from the normal to selected
toolbar crop. Click the selected tile twice: first to establish the Till anchor,
second to commit. Require the anchor to clear and exactly one designation at the
chosen tile, with a rendered preview/designation delta.

Stage a separate visible tile as already tilled through a setup-only mutation.
Click the rendered Plant widget, require `world.getToolMode() == "plant"` and
the Plant selected image, then click the staged tile. Require the real plant
panel for those exact coordinates. Locate and click one enabled crop row by
widget dump, require its suitability detail/selection, then locate and click
the rendered `Plant here` control. Require the panel to close and exactly one
plant designation with the selected row's species and category, plus a retained
frame showing the designation marker.

Finally click the rendered Default tool and require both selected states and
the tool mode to clear without creating any additional designation. Shut down
the engine and remove its private root unless evidence retention was requested.

## Oracle

Command success means the driver and engine terminate cleanly; it is distinct
from setup and behavior success.

Setup succeeds only if the real HUD is visible, both toolbar controls have
distinct interactive bounds, all four exact shipped paths are registered, a
screen-correlated dry tillable tile exists, and a separate plantable tile can be
staged. Failure to establish one of those prerequisites is a setup failure, not
a farming regression.

Behavior passes only when:

- each toolbar click is accepted and reaches its matching `world.getToolMode()`;
- the active control's relative crop changes to its selected runtime image
  without relying on a platform-specific full-frame hash;
- the two Till world clicks establish then clear the anchor and create exactly
  one designation at the canonically selected tile;
- the Plant world click opens the panel for the exact staged tile, the row
  click selects a real enabled crop, and the `Plant here` click closes the panel
  and creates exactly one matching species/category designation;
- retained gameplay frames show meaningful Till preview/designation and Plant
  marker deltas; and
- switching to Default clears selection without duplicate designations,
  rejected/misrouted actions, engine errors, crashes, or leaked resources.

Wrong texture binding, wrong mode, wrong page/tile/species, duplicate or absent
designation, or missing state-backed visual delta is a behavioral failure. If
screen-to-tile correlation or runtime image evidence is ambiguous, report the
run as inconclusive rather than guessing from pixels.

Retain widget dumps and click outcomes; normal and selected toolbar crops; Till
anchor, preview, and committed frames; the Plant panel before and after row
selection; the final marker frame; authoritative tile/designation snapshots;
and the engine log.

## Apparatus and integration

Implement `tools/farming_toolbar_workflow_probe.py`. Reuse the shared offscreen
engine lifecycle and isolated root, `offscreen_probe.py`'s widget/click and
screenshot helpers, and the till/plant probes' eligibility and designation
queries. Add only narrow helpers for screen-to-tile discovery, named widget
selection, crop-row discovery, and relative crop/delta measurement; unit-test
those helpers without an engine where practical.

Register the exact key `farming_toolbar_workflow` in `tools/run_probes.py`.
Classify it in `tools/ci_probes.py` with both `needs-gpu` and `scenario-heavy`
manual-only reasons. It owns its port, private root, screenshot directory,
state dumps, logs, and teardown, including cleanup after exceptions or setup
failure.

## Reliability and cost

Expected warm runtime is 2–5 minutes: one Vulkan boot and one small pinned
world/HUD setup, with no AI-completion wait or restart. A GPU is required, and
the scenario includes real world/UI preparation, so it is unsuitable for the
current CI runner and for every required gate.

Poll readiness, widget appearance, world mutations, and committed designations;
do not sleep for correctness. Discover suitable tiles at runtime instead of
depending on one permanently valid coordinate. Use semantic widget names and
relative crops with generous non-zero change thresholds, never exact golden
hashes. The main risks are GPU/framebuffer variance, camera projection drift,
widget-name changes, and a pinned world's available tile mix. Preserve setup
failures separately so those risks cannot masquerade as gameplay defects.

## Implementation plan

1. Add the offscreen workflow probe with deterministic tile setup, named
   toolbar/panel interaction, authoritative state oracles, diagnostic image
   deltas, retained artifacts, and cleanup on every exit path.
2. Register `farming_toolbar_workflow` in `tools/run_probes.py` and add its
   `needs-gpu` plus `scenario-heavy` manual-only classification in
   `tools/ci_probes.py`, with no CI path mapping or required-gate invocation.
3. Add focused engine-free tests for new coordinate/crop/image helpers, run the
   registry self-test, then run this exact probe once on a GPU-capable host and
   visually inspect the retained crops alongside its state-backed verdicts.

## Validation and handoff

Focused validation is `python3 tools/ci_probes.py --self-test`, any new helper
self-tests, and one explicit
`python3 tools/run_probes.py --only farming_toolbar_workflow --exact --jobs 1`
run on a GPU-capable machine. The coordinated `$test` skill should discover the
probe through the normal `run_probes.py` inventory and select it only
occasionally for the UI/farming-toolbar area. The key must stay absent from
`CI_ELIGIBLE`, CI path selection, GitHub CI, `make ci`, pre-push checks, and all
other required gates.

## Open questions

None.
