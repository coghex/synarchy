# Player-facing save, relaunch, and Continue round trip test design

Test design state: `ready for implementation`

Source proposal: `20260830T185426Z-proposal-probe-player-save-continue-ui-f4b4d6`

Source test ID: `probe:player-save-continue-ui`

Source ref and commit: `origin/master` at `58480c364375df151baeb0a10c0a8fc579b24438`

Designed against: `origin/master` at `92b9866ed5b278f12a82afa18112837fd894dc23`

Accepted at: `2026-08-30T18:59:08Z`

Implementation authorization: `not granted`

Recommended tier: `manual-only`

## Purpose and coverage gap

Prove the ordinary player path from the rendered pause-menu Save control,
through a clean process exit and relaunch, to the rendered main-menu Continue
control and a restored playable world. Current persistence probes exercise
`engine.saveWorld` and `engine.loadSave` directly, while the existing offscreen
UI apparatus reaches world generation and selected load flows without driving
this complete two-process player journey. Headless tests cover the underlying
save codecs and menu behavior separately, so they cannot detect an integration
break between injected input, asynchronous storage, save discovery, the real
loading-screen handoff, and post-load world/UI rebinding.

## Binding decisions

- Accepted by user for future planning: the end-to-end UI-driven save, relaunch, and Continue path covers a high-impact persistence integration boundary. Acceptance does not authorize implementation.
- Keep this one optional, manual-only behavior probe. It must not join
  `CI_ELIGIBLE`, GitHub CI, `make ci`, or any other required gate.
- The graded save and load transitions must use rendered controls and injected
  player input. Direct persistence APIs are allowed only for setup inspection,
  polling, and the oracle, never to substitute for either click.
- Scope is one deterministic world, one uniquely identifiable durable
  sentinel, one manual `quicksave`, two engine boots, and one Continue choice.
  Corruption, autosave rotation, multiple-save selection, combat/AI progress,
  and visual-aesthetic approval remain excluded.

## Current repository evidence

At the design commit, `scripts/pause_menu.lua` renders a `Save` item whose
callback pauses through `scripts.pause`, calls `world_view.saveGame()`, and
therefore queues the canonical asynchronous `engine.saveWorld` transaction for
the `quicksave` slot. `scripts/main_menu.lua` builds `Continue` only when
`engine.listSaves()` returns a slot, chooses the canonical newest listing, and
routes the click through `loadAndShowSave()`. That path waits for the real load
transaction in `scripts/loading_screen.lua`, then binds `worldManager` and
`worldView` to the published active page.

`tools/offscreen_probe.py` already supplies the useful UI boundary:
`ui.dumpWidgets()` label lookup, widget-center `input.click`, screenshot capture,
world-generation polling, and a real Continue click after generation. Existing
persistence probes provide request-aware save/load polling and fresh-process
patterns. `tools/run_probes.py` has no `player_save_continue_ui` key, and
`tools/ci_probes.py` has no classification for it; the accepted gap therefore
still exists at `92b9866e`.

## Scenario and boundaries

Create an invocation-owned resource root and screenshot directory, then run two
offscreen Vulkan engines sequentially on one leased port.

On boot A, wait for the production main menu, create a pinned world through the
existing UI flow, and enter the gameplay HUD. Stage a sentinel through setup
APIs only: record the displayed world identity and one durable unit/item
instance with exact definition, instance identity, owner, position, active
page, paused state, and time state. Open the pause menu through injected input,
locate `Save` through `ui.dumpWidgets()`, capture it, click it, and wait for the
specific save request to reach a terminal successful outcome. Require exactly
one completed `quicksave` listing with the expected world identity. Open the
pause menu again, click `Exit to Desktop`, wait for orderly process termination,
and retain the private root.

On boot B against that same root, wait for the main menu, require an enabled
`Continue` widget, capture it, and click it. Observe a real loading-screen
transition and poll the load request until publication and a usable
`world_view`. Capture the restored HUD, then compare the restored world and
sentinel with the boot-A snapshot. Quit cleanly and remove every process and
private artifact root unless artifact retention was requested.

The sentinel is fixture setup; staging it does not count as the player-facing
behavior under test. The probe does not grade save-browser ordering, autosave,
arbitrary legacy saves, gameplay simulation after restoration, or pixel-perfect
menu appearance.

## Oracle

Command success means both probe-controlled engine processes and the Python
driver finish without timeout, crash, or leaked resources. It does not by
itself establish behavior success.

Setup succeeds only when boot A reaches a real gameplay HUD, the sentinel is
durably representable and can be read back exactly, and the private root begins
without an existing save that could supply Continue. A missing widget, failed
world generation, unsuitable sentinel, or absent explicit request status is a
setup failure rather than a persistence verdict.

Behavior passes only when:

- the injected Save click is accepted and creates one successful `quicksave`
  with the displayed world identity;
- boot A exits cleanly through the rendered menu;
- boot B discovers that save and renders an enabled Continue control;
- the injected Continue click produces an accepted load, a real loading-screen
  transition, and a published playable world;
- active page/world identity, paused/time state, and every recorded sentinel
  identity, ownership, definition, and position field equal the saved snapshot;
- no duplicate session state, terminal save/load error, engine error, crash,
  surviving process, or leaked root occurs.

A rejected or misrouted player action, wrong slot/identity, terminal transaction
failure, or mismatched restored state is a behavioral failure. If the retained
evidence cannot tie a timeout to a request or cannot prove which rendered
control received the click, report the run as inconclusive rather than treating
the absence of state as a product failure.

Retain screenshots of Save, relaunched Continue, and restored gameplay; both
engine logs; the widget records used for each click; request/status timelines;
the save listing/metadata; and the before/after sentinel snapshots.

## Apparatus and integration

Implement `tools/player_save_continue_ui_probe.py`. Reuse `probelib` or the
current shared engine lifecycle, the isolated-resource-root convention,
`offscreen_probe.py`'s widget/click/screenshot helpers, and request-aware
persistence polling already used by the save probes. Keep reusable helper
changes narrow; do not make this scenario a new persistence framework.

Register the exact key `player_save_continue_ui` in `tools/run_probes.py` with
a two-engine UI/persistence description. Add a `MANUAL_ONLY_REASONS` entry in
`tools/ci_probes.py` carrying both `needs-gpu` and `scenario-heavy` reasons. The
probe owns its port, root, screenshots, logs, and teardown; it must tolerate a
failed boot or mid-scenario exception without stranding the first engine or
deleting evidence before it is retained.

## Reliability and cost

Expected warm runtime is 3–6 minutes: one generated world, two Vulkan boots,
one disk save, and one real load. It requires a GPU-capable host, so it cannot
run on the current CI worker. Its two-process world lifecycle is also too slow
and scenario-heavy for a required per-change gate.

Use a pinned world-generation tuple and stable sentinel setup, an
invocation-owned clean root, semantic widget labels/bounds, and explicit status
polling. Do not use fixed sleeps for save/load completion or full-frame golden
hashes. Screenshots are diagnostic evidence, not the sole oracle. The main
maintenance risks are widget-name churn, GPU startup variance, and changes to
save-list metadata; failures must identify which boundary drifted.

## Implementation plan

1. Add the two-boot probe with private-root lifecycle, shared widget/click and
   request polling, deterministic sentinel setup, retained evidence, and strict
   cleanup on every exit path.
2. Register `player_save_continue_ui` in `tools/run_probes.py` and classify it
   manual-only for `needs-gpu` plus `scenario-heavy` in `tools/ci_probes.py`,
   without adding any path selector or required-gate invocation.
3. Add engine-free tests for any new parsing, state comparison, or image-delta
   helper, run the registry self-test, then execute this exact probe once on a
   GPU-capable host and inspect all retained boundaries.

## Validation and handoff

Focused validation is `python3 tools/ci_probes.py --self-test`, any new
engine-free helper tests, and one explicit
`python3 tools/run_probes.py --only player_save_continue_ui --exact --jobs 1`
run on a GPU-capable machine. The coordinated `$test` skill should discover the
probe from the normal `run_probes.py` registry and select it occasionally by
its persistence/player-UI area and manual-only classifier. Registration must
not make it CI-eligible or place it in GitHub CI, `make ci`, a pre-push gate, or
any required aggregate.

## Open questions

None.
