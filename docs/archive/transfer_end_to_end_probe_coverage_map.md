# Coverage map: the retired end-to-end transfer probe (`unified_transfer`, #1255) → focused coverage

Issue [#2935](https://github.com/coghex/synarchy/issues/2935), PRB-4 of `docs/probe_value_findings.md`. Written 2026-11-07 for the pull request that retires `tools/unified_transfer_probe.py` and its eight stage modules. The owner decided on 2026-11-07 to replace the probe with focused coverage; this map is the evidence that each of its checks was either already gated by a test that fails when the behaviour breaks, moved to a named new home, or deliberately dropped for a stated reason.

## How the 310 reconciles

The issue reports 310 checks (setup 22, knowledge 21, modeB 120, modeA 72, batch 18, widget 14, save 20, load 23). Reading the source and a complete passing run (quruntul tier-A run `A5`, 2026-10-08: `setup 22, knowledge 21, modeB 120, modeA 99, batch 18, widget 14, save 20, load 23`, all pass) gives **337**. Only `modeA` differs: the "72" is the count printed by the failing run A (`1 of 72 checks`), where the stage stopped before its third session. Derived from source, `modeA` is 6 seeding checks + 1 walk-away + 10 menu-route (two menu-routed sessions × 5) + 2 direct-create + 3 `await_session_open` + 3 stack-shape + 1 unit-to-unit roles + 66 legs (6 × 11) + 7 teardown (3 `closing the level` + 4 `unit N is released`) = 99. Conditional failure paths (a `localization failed` line, a `kept resource root` print) add nothing to a passing run's count, and the loop-expanded labels (`modeB`/`modeA` legs, the six `widget` views, the unit-to-unit release lines) are listed below once with their multiplicity. The two teardown boundary-log checks per engine are inside `save` (2) and `load` (2). This map classifies all 337.

The `FINGERPRINT` line is not a check. **Disposition: dropped.** Its only consumer was a human diffing two consecutive runs; no gate, tool or test ever compared two fingerprints, every identity it summarised is asserted directly (the order's id, endpoints and instance ids by `transfer_order_probe.py` phase 9, each leg's exact instance by the gesture stages), and a diff of measurements adds nothing to a pass/fail result.

## Classes

- **covered**: a named test or probe check fails if the behaviour breaks. A group that merely touches the area is not cited. Where a probe check is cited, the probe's own label is quoted; where an Hspec example is cited, its `it` title is quoted (all under `test-headless/Test/Headless/`).
- **unique**: no other gate states it. Each carries its new location. Several unique rows are kept under the same printed label, so a reviewer can match them one to one.
- **dropped**: not worth keeping. Each carries its reason: fixture plumbing for a removed stage, a precondition of a check that is itself covered, bookkeeping about the removed probe's own ledger, or an assertion about a component the transfer system does not own.

Abbreviations: `CM` = `tools/transfer_context_menu_probe.py`; `TO` = `tools/transfer_order_probe.py`; `IL` = `tools/item_list_widget_probe*.py`. Hspec: `Unit transfer contract` = `Unit/Transfer.hs`; `Unit transfer Lua API (orders, #1247)` = `Unit/TransferOrderApi.hs`; `Unit transfer Lua API` = `Unit/TransferApi.hs`; `Container knowledge` = `Building/Knowledge.hs`; `Transfer context menu` = `UI/TransferContextMenu.hs`; `Transfer context menu (Mode A escort session, #1250)` = `UI/TransferSession.hs` and `UI/TransferSession/*.hs`; `durable transfer orders survive the LIVE save/load path` = `World/TransferOrders.hs`.

## What the replacement is

The retired probe was the only gate that drove REAL right-clicks on REAL rendered item rows to move one exact item instance between every ordered pair of the three endpoint classes (acolyte, technomule, built storage) in both player modes. Every Hspec suite that states the same behaviour does so on a stub VM that calls the menu callbacks directly and never renders a row, and `item_list_widget_probe` does it for one pair. So:

1. **Gesture stages** in `tools/transfer_context_menu_probe.py` (already offscreen; library code in `tools/transfer_gesture_stages.py` and `tools/transfer_gesture_lib.py`, reported per stage `setup`, `modeB`, `modeA`):
   - `modeB`: all six directed legs. The gesture fires from out of reach and must queue exactly one durable order naming both endpoint identities and the exact instance, move nothing at click time, and the contract must refuse the identical request for range alone. The order is then cancelled (its walk and arrival commit are `TO` phase 1 and `Unit transfer Lua API (orders, #1247)`'s), and the instance must still be on the source.
   - `modeA`: three real escort sessions (acolyte/storage, technomule/storage, acolyte/technomule) and the same six legs, each committing on the spot from the clicked pane, with the structured answer flipping from `accepted` to `instance_missing`; plus the real session-open reveal and the camera-band precondition.
   - The probe boots on an invocation-owned resource root (symlinked content, copied default config without `*.local.yaml`, its own `saves/`), as the retired probe did, so a local notification override cannot decide a gesture's outcome and the engine's runtime writes stay out of the checkout.
   - A new small probe was not needed: both recipients already exist and already boot what is needed. The probe is `needs-gpu` already (`tools/ci_probes.py`), so its classification is unchanged.
2. **Fresh-process re-check** in `tools/transfer_order_probe.py` phase 9: the order carries two instances; the fresh process must reproduce its exact durable identity (order id, both endpoint identities, both instance ids in entry order), the carrier must still hold exactly those instances, the order must complete exactly once, each save is awaited by its own request id to its terminal phase (`SaveCaptureComplete`) before either boundary is certified, and each engine's log is read only once its process is gone, failing on any `integrity diagnostic` and on an unreadable log (the old `log_lines` returned `[]` on an `OSError`, which would have passed vacuously).
3. **Diagnostics preserved.** The stage-attributed failure records of #2802 and the boundary-log oracle of #1487 now live in `tools/transfer_probe_checks.py`, used by both probes; `tools/probe_runner_tests/diagnostics.py` drives the real recorder and the real gesture-stage catch-all.

### Not carried over, and why (the Mode A / Mode B behaviours that are only reachable by a real walk)

- The real walk of a Mode B carrier to its arrival commit, per endpoint pair: pair-agnostic, and gated with a real walking carrier by `TO` phase 1 plus `Unit transfer Lua API (orders, #1247)` `the arrival commit`.
- The out-of-reach start and real walk of a Mode A escort: `Transfer context menu (Mode A escort session, #1250)` `a distant carrier WALKS toward the endpoint's footprint…` and `TO` own it. The new `modeA` spawns each escort beside its endpoint (and unpauses only for the few ticks the real AI needs to open the pair), because a real walk across generated terrain is also what failed the old probe intermittently (falls are knockdowns, and a knocked-down escort ends a session by rule).
- The Mode A session NOT surviving a load: a fresh process holds no session by construction, so the old check was only meaningful if a save component for the session could exist; `the save-load reset hook ends the session and releases the unit, and the session still contributes NO save component (D-3)` asserts it cannot, and `the successful-load reset releases BOTH units…` asserts the release on the live reset hook. Per the review of #2935, the retained form would have to establish a live session before saving and use production modules after the restart; neither is possible in a headless restart probe, and the Hspec pair states the rule without the vacuous half.

### Checks the replacement adds (not among the 337)

- Gesture `setup`: 1 (the 12 leg item defs register).
- Gesture `modeB`: 14 per leg × 6 = 84 (the 12 `unique` rows below, plus `the contract refuses … for RANGE alone` and `cancelling the order retires it and still moves nothing`).
- Gesture `modeA`: 97 (1 camera band + 3 spawn-beside + 6 seeds + 6 direct-create + 3 await + 3 stack + 1 roles + 1 session-open reveal + 66 legs + 7 teardown).
- `TO` phase 9: `the in-flight order names both exact instances`, `the order survives with its EXACT identity`, `the carrier still holds exactly the instances it names`, `it completed EXACTLY ONCE across the process boundary`, `the save completes and is durable`, `…and completes durably` (the post-completion save), and the four boundary-log checks (`engine B (save)`, `engine C (load, then a second save)`: process gone, no diagnostic); the four inline `log_lines` assertions they replace are gone. Every phase is now a stage, so a failure names its phase, and an operational failure (socket error, malformed response, boot `SystemExit`) is recorded under it with the stage summary still printed.

## Validation of the replacement

One focused offscreen/engine run of each changed probe, re-run after the first review round on the final probe code (each waited for the `cabal-build` and `gpu-desktop` locks; no whole suite, no sweep):

- `python3 tools/run_probes.py --only transfer_context_menu --exact`: PASS, 367.8 s (`setup` 1, `modeB` 84, `modeA` 97 checks, all pass). Two earlier runs on earlier revisions of the new stages are kept honest here: the first passed every real-gesture check and failed only the new `cancelling the order retires it` check, because it used the raw `unit.cancelTransferOrder` verb, which marks entries cancelled without pruning, instead of the player's own `unit_ai.cancelTransferOrder`; the second failed only the session-open reveal on the SECOND hold session, because the world is paused between sessions and the game clock had not advanced since the previous commit's reveal, so the check is now made once, before any session has touched the hold.
- `python3 tools/run_probes.py --only transfer_order --exact`: PASS, 503.1 s (phase 9 included: exact identity, exactly-once, request-awaited saves, both boundary logs).
- Self-tests: `tools/test_run_probes.py`, `tools/ci_probes.py --self-test`, `.quruntul/checks.py`, `tools/test_probe_census.py`, `tools/test_readme_registry_count.py`, `tools/test_probe_evidence.py`, `tools/test_persistence_contract_sweep.py`, `tools/test_transfer_context_menu_probe.py`, `tools/ci_parity_audit.py`.


## Per-stage tallies

| Stage | Checks (passing run) | Covered | Unique (moved) | Dropped |
|---|---:|---:|---:|---:|
| `setup` | 22 | 14 | 2 | 6 |
| `knowledge` | 21 | 14 | 1 | 6 |
| `modeB` | 120 | 48 | 72 | 0 |
| `modeA` | 99 | 11 | 88 | 0 |
| `batch` | 18 | 16 | 0 | 2 |
| `widget` | 14 | 12 | 0 | 2 |
| `save` | 20 | 7 | 3 | 10 |
| `load` | 23 | 14 | 5 | 4 |
| **Total** | **337** | **136** | **171** | **30** |

## Stage `setup` (22 checks)

| Check (label as printed; `<…>` expands per leg, session or view) | × | Class | Where it is covered, where it moved, or why it is dropped |
|---|---:|---|---|
| the loading screen reaches the main menu | 1 | covered | `transfer_context_menu_probe.py` `loading screen -> main menu` |
| the create-world screen is reached | 1 | covered | `transfer_context_menu_probe.py` `create-world screen reached` |
| the create-world screen is pinned to seed 0x0000002A at world size 64 | 1 | dropped | only served the `FINGERPRINT` diff; the gesture stages search for their sites and need no particular seed |
| worldgen completes (phase 3) | 1 | covered | `transfer_context_menu_probe.py` `worldgen completes (phase 3)` |
| the post-generation Continue button appears | 1 | covered | `transfer_context_menu_probe.py` `post-generation Continue button appears` |
| the in-game HUD is reached | 1 | covered | `transfer_context_menu_probe.py` `in-game HUD reached` |
| the generated world really carries the pinned seed 42 | 1 | dropped | same reason as the pinned-seed check above |
| the engine reports a usable window and framebuffer extent | 1 | covered | `transfer_context_menu_probe.py` `engine reports a usable window and framebuffer extent` |
| the probe's 15 throwaway item defs registered | 1 | unique | `transfer_gesture_lib.load_item_defs`, run by the `setup` stage of the gesture stages (now 12 defs: the batch, stale and save defs left with their stages) |
| the probe's four throwaway building defs registered | 1 | covered | `transfer_context_menu_probe.py` `probe building def loaded` (the one 200 kg storage def the gesture stages reuse) |
| seven separated dry sites are found for the fixtures | 1 | covered | `transfer_context_menu_probe.py` `found five separated dry sites for the fixtures` (five, because the knowledge, batch and save fixtures are gone and Mode A escorts are spawned beside their endpoint) |
| storage hold: reaches Built activity | 1 | covered | `transfer_context_menu_probe.py` `storage building reaches Built activity` |
| small hold (room for exactly eight): reaches Built activity | 1 | dropped | fixture of the `batch` stage, which is covered without it (see `batch`) |
| knowledge hold: reaches Built activity | 1 | dropped | fixture of the `knowledge` stage, which is covered without it (see `knowledge`) |
| never-inspected hold: stays UNBUILT (worker-built, zero progress...) | 1 | covered | `item_list_widget_probe` `knowledge_scenario`: `the engine reports the worker-built fixture as genuinely never-inspected` builds the same worker-built zero-progress fixture |
| two dry tiles beside the knowledge hold... | 1 | dropped | fixture of the `knowledge` stage |
| the camera is settled in the zoomed-in tile band before any session exists... | 1 | unique | `modeA: the camera is settled in the zoomed-in tile band before any session exists` (`transfer_gesture_stages.stage_mode_a`) |
| the acolyte / technomule / storage endpoint is eligible to the transfer contract | 3 | covered | `Unit transfer contract` `accepts an ordinary player acolyte as a destination`, `moves an exact instance unit → building storage`, `… building storage → unit`; `Unit transfer Lua API` `endpoint info`; `transfer_context_menu_probe.py` `building reports transfer-eligible via the endpoint query` |
| and the wildlife unit is NOT — it is not player-commandable... | 1 | covered | `Unit transfer contract` `refuses a non-player-commandable unit SOURCE as source-ineligible` / `DESTINATION as receiver-ineligible`; `transfer_context_menu_probe.py` `wildlife menu: 'Transfer' is ABSENT (not player-commandable)` |
| the knowledge hold is stocked and remembered | 1 | dropped | stimulus for the `knowledge` stage; the assertion is `Container knowledge` `an inspected container holding something is KNOWN-CONTENTS` |

## Stage `knowledge` (21 checks)

| Check (label as printed; `<…>` expands per leg, session or view) | × | Class | Where it is covered, where it moved, or why it is dropped |
|---|---:|---|---|
| a container nothing has ever interacted with reads as genuinely never-inspected | 1 | covered | `Container knowledge` `a container nobody has interacted with is NEVER-INSPECTED`; `item_list_widget_probe` `the engine reports the worker-built fixture as genuinely never-inspected` |
| its capacity is still LIVE and positive even with no record at all | 1 | covered | `item_list_widget_probe` `its capacity is still live and positive even with no record`; `Container knowledge` `capacity is never remembered` |
| knowledge: the container window opens on building #N | 1 | dropped | plumbing; `item_list_widget_probe` `the container window opens on a never-inspected container` |
| its header reads the stored weight as UNKNOWN while still showing the live capacity | 1 | covered | `item_list_widget_probe` `its header reads the stored weight as UNKNOWN…`; `ResponsiveGameplay/Container.hs` asserts `Storage: unknown / 400.00 kg` |
| a never-inspected container renders no rows — it is not an empty one | 1 | covered | `item_list_widget_probe` `a never-inspected container renders NO rows` and `it renders an explicit never-inspected line, not an empty list` |
| opening the window and interacting with it reveals NOTHING | 1 | covered | `item_list_widget_probe` `opening (and interacting with) the window reveals NOTHING…`; `Container knowledge` `selecting the container changes nothing` |
| the never-inspected fixture is retired once it has been observed... | 1 | dropped | fixture hygiene for the removed stage |
| the stocked container is remembered with both instances and an observation time | 1 | covered | `Container knowledge` `an inspected container holding something is KNOWN-CONTENTS`, `every one of ItemInstance's eleven fields is copied AS OF the…`, `the reveal time is the game-time clock at observation` |
| a player unit is standing right beside it (footprint Chebyshev 1) | 1 | dropped | precondition of the proximity check below |
| and stayed in its immediate vicinity for the whole interval | 1 | dropped | precondition of the proximity check below |
| PROXIMITY ALONE never reveals: eight seconds of a player unit standing beside it... | 1 | covered | `Container knowledge` `moving a unit onto the container's tile changes nothing` and the `what does NOT reveal contents` group |
| a non-player unit withdraws one instance | 1 | dropped | stimulus; the rule is `Container knowledge` `a NON-commandable unit's otherwise-successful storage interaction…` and `Unit transfer Lua API` `a non-commandable unit's transfer reveals nothing` |
| the player's picture is now GENUINELY STALE... | 1 | covered | `Container knowledge` `contents changing underneath a real record leaves it STALE` |
| the escort is alive and unhurt before the session that measures the reveal... | 1 | dropped | precondition of the reveal check below |
| knowledge Mode A reveal: a Mode A session is created | 1 | covered | `Transfer context menu` `session creation (scripts/transfer_session.lua)`; kept as the `create_session` check of every `modeA` session |
| knowledge Mode A reveal: the session names the exact endpoint identities | 1 | covered | `transfer_context_menu_probe.py` `session source == the selected acolyte, named` / `session destination == …, named`; kept in `modeA` |
| knowledge Mode A reveal: the REAL unit AI walks the escort and opens the pair on arrival | 1 | covered | `Transfer context menu (Mode A escort session, #1250)` lifecycle `a distant carrier WALKS toward the endpoint's footprint and does not open anything; standing adjacent it stops, reveals, opens`; kept in `modeA` as `await_session_open` |
| a Mode A session OPENING on the container is what refreshes it — a fresh observation time | 1 | unique | `modeA: a Mode A session OPENING on the container is what refreshes it` (the stub-VM lifecycle test counts the reveal call but cannot show the real session reaching the real record) |
| and the refreshed record now agrees with live storage, the withdrawn instance gone | 1 | covered | `Container knowledge` `a reveal REPLACES the whole record -- no merge`; `Unit transfer Lua API` `a successful WITHDRAWAL reveals it too (#1087 covers both directions)` |
| knowledge Mode A reveal: closing the level ends the session (one coupled teardown) | 1 | covered | `Transfer context menu (Mode A escort session, #1250)` lifecycle `closing the window closes BOTH panes, ends the session and…`; kept in `modeA` as `close_session` |
| knowledge Mode A reveal: unit N is released by the session ending | 1 | covered | `Transfer context menu (Mode A escort session, #1250)` lifecycle `closing the window closes BOTH panes, ends the session and…`; kept in `modeA` |

## Stage `modeB` (120 checks)

| Check (label as printed; `<…>` expands per leg, session or view) | × | Class | Where it is covered, where it moved, or why it is dropped |
|---|---:|---|---|
| modeB building route: camera preconditions settled (z-tracking off, slice == the building's gridZ, tile zoom band) | 1 | covered | `transfer_context_menu_probe.py` `camera preconditions settled for the hit test…` |
| modeB building route: located the building's own screen pixel through the production hit test | 1 | covered | `transfer_context_menu_probe.py` `located the storage building's own screen pixel` |
| modeB building route: the camera centres on the building | 1 | covered | `transfer_context_menu_probe.py` `the camera centres on the building…` |
| modeB building route: the right-click menu offers 'Contents' | 1 | covered | `transfer_context_menu_probe.py` `building menu: 'Contents' still appears` |
| modeB building route: the activated row opened the window on the exact endpoint identity | 1 | covered | `transfer_context_menu_probe.py` `the container window opened on THIS building endpoint, at the base level` |
| modeB <leg>: the source holds exactly one <def>, so the row is single-membered and its gesture names an EXACT instance | 6 | unique | `modeB` leg (`transfer_gesture_stages.mode_b_leg`), same label |
| modeB <leg>: the destination does not already hold instance N | 6 | unique | `modeB` leg, same label |
| modeB <leg>: the container window opens on <kind> #N | 6 | unique | `modeB` leg, same label |
| modeB <leg>: the two endpoints are OUT of the contract's reach when the gesture fires | 6 | unique | `modeB` leg, same label (rendered gesture from out of reach) |
| modeB <leg>: the <def> row is located on the rendered unit-info inventory / container window | 6 | unique | `modeB` leg, same label (a rendered row; the stub-VM `TransferGestures.hs` fixtures never render one) |
| modeB <leg>: the rendered row stands for exactly instance N | 6 | unique | `modeB` leg, same label |
| modeB <leg>: the rendered row menu offers 'Store 1' / 'Retrieve 1' | 6 | unique | `modeB` leg, same label; also `Transfer context menu (Mode B Store/Retrieve gestures, #1249)` (`TransferGestures.hs`) `a merged row offers Store 1 and Store all; a single-instance row…` |
| modeB <leg>: a single-instance row shows the singular entry alone | 6 | unique | `modeB` leg, same label |
| modeB <leg>: the 'Store 1' / 'Retrieve 1' entry is clickable | 6 | unique | `modeB` leg, same label (the real click on the real rendered entry) |
| modeB <leg>: firing it queues exactly one durable transfer order | 6 | unique | `modeB` leg, same label; the stub-VM equivalent is `Transfer context menu (Mode B Store/Retrieve gestures, #1249)` (`TransferGestures.hs`) `Store 1 queues a durable order…` |
| modeB <leg>: the queued order runs from <src> to <dst> naming exactly instance N | 6 | unique | `modeB` leg, same label (all six directed pairs) |
| modeB <leg>: nothing moved at click time — the source still owns instance N | 6 | unique | `modeB` leg, same label |
| modeB <leg>: the carrier walks the order and commits on arrival | 6 | covered | `transfer_order_probe.py` phase 1 (a real carrier walks to the hold's far end and commits once, footprint-measured); `Unit transfer Lua API (orders, #1247)` `the arrival commit`; the commit is endpoint-class agnostic: `Unit transfer contract` `all four endpoint pairs` |
| modeB <leg>: and left the source — the instance is in exactly one endpoint, never both | 6 | covered | `Unit transfer contract` `moves exactly one instance exactly once`, `leaves both endpoints unchanged on a refusal`; `Unit transfer Lua API (orders, #1247)` `prunes a COMPLETED order, and the item really did move` |
| modeB <leg>: the completed order was pruned on the tick that ended it | 6 | covered | `Unit transfer Lua API (orders, #1247)` `prunes a COMPLETED order, and the item really did move`; `transfer_order_probe.py` phase 1 (the store is left holding no terminal order) |
| modeB <leg>: the completion reached the player exactly once | 6 | covered | `Transfer context menu` `outcome reporting is exactly-once (#1253)`; `transfer_order_probe.py` phase 1 (`exactly ONE attributed unit_event`, summing the log's coalesced `count`) |
| modeB <leg>: the order's own entry ran the lifecycle and never reported a failure state | 6 | covered | `Unit transfer Lua API (orders, #1247)` `advances queued → in_transit → ready_to_commit → completed…`; `Unit transfer contract` `walks queued → in transit → ready to commit → completed` |
| modeB <leg>: before the gesture the contract refuses this exact request for RANGE alone (`out_of_range`) | 6 | covered | `Unit transfer contract` `accepts a distant pair the required-reach policy refuses`; `Unit transfer Lua API (orders, #1247)` `queues an order the immediate contract refuses on range`; also kept in `modeB` because it brackets the real gesture |
| modeB <leg>: and afterwards it refuses the identical request on IDENTITY (`instance_missing`) | 6 | covered | `Unit transfer contract` `refuses a missing or stale instance id`, `fails as stale when the source instance is gone by commit`; the immediate-commit form stays in `modeA` |
| a COMPLETED movement refreshes the container's record (D-2's other reveal trigger) | 1 | covered | `Unit transfer Lua API` `a successful deposit reveals the container's contents (#1087)` / `a successful WITHDRAWAL reveals it too` |

## Stage `modeA` (99 checks)

| Check (label as printed; `<…>` expands per leg, session or view) | × | Class | Where it is covered, where it moved, or why it is dropped |
|---|---:|---|---|
| modeA <session>: <src>:N holds exactly one <def> for the <leg> leg | 6 | unique | `modeA` session setup (`transfer_gesture_stages.stage_mode_a`), same label |
| modeA acolyte<->storage: the escort starts OUT of the contract's reach, so the session has a real walk to make | 1 | covered | `Transfer context menu (Mode A escort session, #1250)` lifecycle `a distant carrier WALKS toward the endpoint's footprint…`; the new `modeA` spawns escorts beside their endpoint, because a real walk across generated terrain is exactly what that group and `transfer_order_probe.py` own |
| modeA <session>: camera preconditions settled (z-tracking off, slice == the entity's gridZ, tile zoom band) | 2 | covered | `transfer_context_menu_probe.py` `camera preconditions settled for the hit test…` (building and unit forms); the two menu-routed sessions only |
| modeA <session>: located the building's / unit's own screen pixel through the production hit test | 2 | covered | `transfer_context_menu_probe.py` `located the storage building's own screen pixel`, `technomule: located the unit's own screen pixel` |
| modeA <session>: the camera centres on the building / unit | 2 | covered | `transfer_context_menu_probe.py` `the camera centres on the building…`, `technomule: the camera centres on the unit…` |
| modeA <session>: the right-click menu offers 'Transfer' | 2 | covered | `transfer_context_menu_probe.py` `building menu: 'Transfer' is visible`, `unit menu: 'Transfer' is visible on the technomule` |
| modeA <session>: activating the rendered 'Transfer' row created the session on the exact endpoint identity | 2 | covered | `transfer_context_menu_probe.py` `activating 'Transfer' created a session` + `session destination == the storage building, named` / `… the technomule, named` |
| modeA technomule<->storage: a Mode A session is created / the session names the exact endpoint identities | 2 | unique | `modeA` session (`create_session`, same labels, created directly as the menu route is the context-menu scenarios') |
| modeA <session>: the REAL unit AI walks the escort and opens the pair on arrival | 3 | unique | `modeA` session (`await_session_open`, same label); the walk itself is `Transfer context menu (Mode A escort session, #1250)` lifecycle's |
| modeA <session>: the pair is ONE stack level of the escort kind owning two panes | 3 | unique | `modeA` session, same label; also `item_list_widget_probe` `the pair is ONE stack level of the escort kind` |
| modeA acolyte<->technomule: unit-to-unit holds BOTH ends — the escort as 'source', the destination as 'target' | 1 | unique | `modeA` session, same label; also `Transfer context menu (Mode A escort session, #1250)` `openPair` asserts both `roleOf` values |
| modeA <leg>: the <def> row is located on the rendered <pane> pane | 6 | unique | `modeA` leg (`mode_a_leg`), same label (all six directed pairs on rendered panes) |
| modeA <leg>: the rendered row stands for exactly instance N | 6 | unique | `modeA` leg, same label |
| modeA <leg>: the <pane> pane's row menu offers 'Store 1' / 'Retrieve 1' | 6 | unique | `modeA` leg, same label; also `Transfer context menu (Mode A escort session, #1250)` `the source pane offers Store and the destination pane Retrieve…` |
| modeA <leg>: and offers no 'Retrieve' / 'Store' — direction comes from WHICH pane was clicked | 6 | unique | `modeA` leg, same label; also `item_list_widget_probe` `a source-pane row offers no Retrieve` |
| modeA <leg>: the 'Store 1' / 'Retrieve 1' entry is clickable | 6 | unique | `modeA` leg, same label |
| modeA <leg>: the gesture commits ON THE SPOT — instance N is at <dst> immediately | 6 | unique | `modeA` leg, same label |
| modeA <leg>: and left <src> — the instance is in exactly one endpoint | 6 | unique | `modeA` leg, same label |
| modeA <leg>: the commit reached the player exactly once | 6 | unique | `modeA` leg, same label; also `Transfer context menu` `outcome reporting is exactly-once (#1253)` |
| modeA <leg>: the session stays open and repeatable after a commit | 6 | unique | `modeA` leg, same label; also `item_list_widget_probe` `the session stays open and repeatable after a commit` |
| modeA <leg>: the contract ACCEPTS the exact request before the gesture (`completion` 'all') | 6 | unique | `modeA` leg (`assert_structured_move`, same label) |
| modeA <leg>: and afterwards it refuses the identical request on IDENTITY (`instance_missing`) | 6 | unique | `modeA` leg (`assert_structured_move`, same label) |
| modeA <session>: closing the level ends the session (one coupled teardown) | 3 | unique | `modeA` session (`close_session`, same label); also `Transfer context menu (Mode A escort session, #1250)` lifecycle `closing the window closes BOTH panes, ends the session and…` |
| modeA <session>: unit N is released by the session ending | 4 | unique | `modeA` session (`close_session`, same label); also `Transfer context menu (Mode A escort session, #1250)` lifecycle `closing the window releases BOTH units of a unit-to-unit session` |

## Stage `batch` (18 checks)

| Check (label as printed; `<…>` expands per leg, session or view) | × | Class | Where it is covered, where it moved, or why it is dropped |
|---|---:|---|---|
| batch: the acolyte carries exactly twelve probe_ut_batch instances and nothing else of that def | 1 | dropped | stimulus for the partial batch |
| batch: the small hold starts empty of them | 1 | dropped | stimulus for the partial batch |
| batch: a Mode A session is created | 1 | covered | kept for every `modeA` session (`create_session`) |
| batch: the session names the exact endpoint identities | 1 | covered | kept for every `modeA` session |
| batch: the REAL unit AI walks the escort and opens the pair on arrival | 1 | covered | kept for every `modeA` session (`await_session_open`) |
| batch: the merged twelve-instance row is located on the source pane | 1 | covered | `item_list_widget_probe` `the merged rations row is located on the source pane` (escort scenario); `Transfer context menu (Mode A escort session, #1250)` `Store all moves EVERY exact instance the merged row stands for…` |
| batch: the merged row stands for all twelve exact instances | 1 | covered | `Transfer context menu (Mode A escort session, #1250)` `Store all moves EVERY exact instance the merged row stands for, immediately…`; `Transfer context menu (Mode B Store/Retrieve gestures, #1249)` (`TransferGestures.hs`) `Store all queues EVERY instance id of the merged row, in order` |
| batch: a merged row offers Store all beside Store 1 | 1 | covered | `item_list_widget_probe` `a source-pane row offers Store 1 and Store all`; `Transfer context menu (Mode A escort session, #1250)` `the source pane offers Store and the destination pane Retrieve, each 1-and-all on a merged row…` |
| batch: the 'Store all' entry is clickable | 1 | covered | `item_list_widget_probe` `the Store all entry is clickable` |
| batch: eight of the twelve fit and were stored | 1 | covered | `Unit transfer contract` `keeps the FIRST eight of twelve when only eight fit`, `queues the first eight of twelve at a distance (D-1)`; `Transfer context menu (Mode A escort session, #1250)` `a partial batch commits what fits and reports the remainder by count and by the contract's own reason (D-1)`; `transfer_order_probe.py` phase 3 (twelve into eight, real engine) |
| batch: the four that did not fit are still on the acolyte | 1 | covered | same tests as the line above |
| batch: every one of the twelve is in EXACTLY ONE endpoint | 1 | covered | `Unit transfer contract` `leaves both endpoints unchanged on a refusal`, `does not let one failed item stop later unique items`; `Transfer context menu (Mode A escort session, #1250)` `a partial batch commits what fits…` (no instance on both sides or neither) |
| batch: the remainder is REPORTED exactly once, naming the four and the 'receiver_full' reason | 1 | covered | `Transfer context menu (Mode A escort session, #1250)` `a partial batch commits what fits and reports the remainder by count and by the contract's own reason (D-1)` (`couldn't Store 1 x`, `receiver_full`); `transfer_order_probe.py` phase 3 (`warned … exactly once`) |
| batch: the contract's own structured answer already splits twelve into eight and four... | 1 | covered | `Unit transfer contract` `reports one outcome per requested item, in request order`, `gives every completion category a stable identifier`, `keeps the FIRST eight of twelve when only eight fit` |
| batch: afterwards the contract refuses the eight that moved on IDENTITY | 1 | covered | `Unit transfer contract` `refuses a missing or stale instance id` |
| batch: and still names the four that stayed | 1 | covered | `Unit transfer contract` `does not let one failed item stop later unique items` |
| batch: closing the level ends the session (one coupled teardown) | 1 | covered | kept for every `modeA` session (`close_session`) |
| batch: unit N is released by the session ending | 1 | covered | kept for every `modeA` session |

## Stage `widget` (14 checks)

| Check (label as printed; `<…>` expands per leg, session or view) | × | Class | Where it is covered, where it moved, or why it is dropped |
|---|---:|---|---|
| every container view this scenario can produce was actually opened and recorded | 1 | dropped | bookkeeping about the probe's own ledger |
| the <view> is rendered by the ONE item-list widget (its list id names a live instance of it) | 6 | covered | `item_list_widget_probe` reads every view's rows ONLY through the widget's own `dump()` (`item_rows` filters `type == "item_list"` and scopes by instance id): `cargo_scenario` (building window), `unit_endpoint_scenario` (unit window), `knowledge_scenario` (never-inspected window), the escort scenario (both panes), and the inventory scenario (unit-info section). A host not rendered by the widget finds no rows and fails them. The widget's own grouping and representative rules are `Item list widget` `ItemList/Model.hs` |
| every row the <view> rendered came back through that same widget's dump (N rows) | 6 | covered | same probes; each of those scenarios asserts its row counts from that dump |
| each view was its own INSTANCE of that widget rather than one shared list re-pointed at a second endpoint | 1 | dropped | asserted only over the removed probe's own ledger; the instance-scoped reads each `item_list_widget_probe` scenario depends on (`item_rows` takes one list id because several hosts are on screen at once) are what would break |

## Stage `save` (20 checks)

| Check (label as printed; `<…>` expands per leg, session or view) | × | Class | Where it is covered, where it moved, or why it is dropped |
|---|---:|---|---|
| save: the carrier starts its order well away from the destination | 1 | dropped | fixture precondition; `transfer_order_probe.py` phase 9 places its carrier twelve tiles from the hold |
| save: the carrier holds exactly one probe_ut_save_item | 1 | dropped | fixture precondition (phase 9 gives its carrier two `probe_ingot`s and now checks both ids) |
| save: the container window opens on building #N | 1 | dropped | plumbing |
| save: the carrier is far from the destination, so the order has a real trip left | 1 | dropped | fixture precondition (phase 9 `the order is IN FLIGHT when the save is taken`) |
| save: the probe_ut_save_item row is located | 1 | covered | the real gesture is the `modeB` leg (`row is located on the rendered unit-info inventory`) |
| save: the 'Store 1' entry is clickable | 1 | covered | the real gesture is the `modeB` leg |
| save: the gesture queued one durable order naming instance N | 1 | covered | the real gesture is the `modeB` leg; the order's persistence is phase 9 below |
| save: the acolyte carries exactly one live order | 1 | dropped | fixture precondition (phase 9 reads the order by id) |
| save: a dry tile beside the technomule for its escort partner | 1 | dropped | belongs to the Mode A transience scenario (see `load`) |
| save: a Mode A session is created | 1 | dropped | belongs to the Mode A transience scenario (see `load`) |
| save: the session names the exact endpoint identities | 1 | dropped | belongs to the Mode A transience scenario |
| save: the REAL unit AI walks the escort and opens the pair on arrival | 1 | dropped | belongs to the Mode A transience scenario |
| save: the Mode B order is still IN FLIGHT when the save is taken — non-terminal, nothing committed | 1 | covered | `transfer_order_probe.py` phase 9 `the order is IN FLIGHT when the save is taken`, and the new `the in-flight order names both exact instances` |
| save: none of its entries has reached a terminal state | 1 | covered | `transfer_order_probe.py` phase 9 (`in_transit` entry states); `durable transfer orders survive the LIVE save/load path` `a save taken after a terminal outcome carries only LIVE orders…` |
| save: its instance is still on the carrier | 1 | unique | `transfer_order_probe.py` phase 9 `and the carrier still holds exactly the instances it names` (new) |
| save: engine.saveWorld accepted the request | 1 | covered | `transfer_order_probe.py` phase 9 `engine.saveWorld accepted` |
| save: the save completes and is durable | 1 | covered | `transfer_order_probe.py` phase 9 `the save file appeared`; `durable transfer orders survive the LIVE save/load path` `a populated wsTransferOrdersRef is captured by the real…` |
| save: captured order #N in flight and a Mode A session holding [..] | 1 | dropped | an unconditional `ok(True)` bookkeeping line |
| engine A: the engine process is gone, so everything it wrote is in its log before the log is read | 1 | unique | `transfer_order_probe.py` phase 9 `engine B (save): the engine process is gone…` (`transfer_probe_checks.check_boundary_log`) |
| engine A: no unexpected persistence integrity diagnostic at this boundary | 1 | unique | `transfer_order_probe.py` phase 9 `engine B (save): no unexpected persistence integrity diagnostic…`, with the log-unreadable failure the old `log_lines` lacked |

## Stage `load` (23 checks)

| Check (label as printed; `<…>` expands per leg, session or view) | × | Class | Where it is covered, where it moved, or why it is dropped |
|---|---:|---|---|
| load: the fresh process reaches the main menu | 1 | dropped | `transfer_order_probe.py` phase 9 boots a fresh engine and proceeds only through `engine.loadSave accepted` |
| the probe's 15 throwaway item defs registered | 1 | covered | `transfer_order_probe.py` phase 9 `bootstrap_defs` before the load (the load fails to publish without its content) |
| the probe's four throwaway building defs registered | 1 | covered | same |
| load: the save loads and publishes in the fresh process | 1 | covered | `transfer_order_probe.py` phase 9 `the load published` |
| load: the loaded page is the one that was saved | 1 | covered | `transfer_order_probe.py` phase 9 finds the order by id in the loaded store; `durable transfer orders survive the LIVE save/load path` |
| load: the Mode B order survives with its EXACT identity — same order id, same endpoint identities, same instance ids | 1 | unique | `transfer_order_probe.py` phase 9 `the order survives with its EXACT identity…` (new; two instances, so entry order is part of the identity) |
| load: and is still non-terminal, with work left to do | 1 | covered | `transfer_order_probe.py` phase 9 `still non-terminal, so there is a trip left to resume` |
| load: the carrier still holds the exact instance the order names | 1 | unique | `transfer_order_probe.py` phase 9 `and the carrier still holds exactly the instances it names` (new) |
| load: the session module is live in the fresh process... | 1 | dropped | guard for the transience check below, which is covered without it |
| load: the Mode A session did NOT survive — a session is transient by design | 1 | covered | `Transfer context menu (Mode A escort session, #1250)` lifecycle `the save-load reset hook ends the session and releases the unit, and the session still contributes NO save component (D-3)` (asserts `registry['transfer_session']` is `nil`, so a load cannot restore one) and `the successful-load reset releases BOTH units…`; a fresh process holds no session by construction |
| load: and its panes are gone with it | 1 | covered | same two tests (`depth` 0 after the reset) |
| load: formerly held unit N came back (two units) | 2 | dropped | asserts the unit save component, not the transfer system (`persistence contract`, `save components`) |
| load: formerly held unit N is not held by anything (two units) | 2 | covered | `Transfer context menu (Mode A escort session, #1250)` lifecycle `the successful-load reset releases BOTH units and leaves a restored durable Mode B order on a reused uid alone` |
| load: formerly held unit N takes an ordinary player order again (two units) | 2 | covered | `Transfer context menu (Mode A escort session, #1250)` lifecycle `the hold is a real in-progress LOCK…` and the release in `the save-load reset hook ends the session and releases the unit…` (the hold's tick returns `-inf` after the reset) |
| load: the resumed carrier walks the surviving order and commits it | 1 | covered | `transfer_order_probe.py` phase 9 `the carrier resumes, the order completes and is pruned` |
| load: and the instance left the carrier | 1 | covered | `transfer_order_probe.py` phase 9 `both items are in the hold` |
| load: the completed order was pruned | 1 | covered | `transfer_order_probe.py` phase 9 `and the carrier is left with no order at all` |
| load: it completed EXACTLY ONCE across the process boundary | 1 | unique | `transfer_order_probe.py` phase 9 `it completed EXACTLY ONCE across the process boundary` (new) |
| engine B: the engine process is gone, so everything it wrote is in its log before the log is read | 1 | unique | `transfer_order_probe.py` phase 9 `engine C (load, then a second save): the engine process is gone…` |
| engine B: no unexpected persistence integrity diagnostic at this boundary | 1 | unique | `transfer_order_probe.py` phase 9 `engine C (load, then a second save): no unexpected persistence integrity diagnostic…` |
