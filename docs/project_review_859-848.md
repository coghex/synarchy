# Project Review Findings: PRs #859–#848

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #859, #858, #856, #857, #855, #854, #852, #853, #851, #850, #849, and #848 — plus the two direct first-parent commits (`aa85eb6b` and `4f4675134`) in the same window, for later one-at-a-time disposition.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

PR #858's active-world remote-warning guard, #855's per-page location-map icons, #854's save envelope, #852's immutable save snapshot, #853's discovery persistence, #851's until-stock bills, and #850's longitude-local solar phasing retain their intended contracts in the current tree and passed the focused checks run during this review. No separate current concern was confirmed for those PRs or for the two direct commits. The GPU-backed embark probe was not executed because the source-level audit below establishes that it writes fixed save slots and does not clean them; running it in the primary resource root would itself risk overwriting user state.

## Status

- [x] PRR-1. The embark probe overwrites fixed save slots and never cleans its artifacts — [#1569]
- [x] PRR-2. The embark probe restarts before its asynchronous saves are durable — [#1746]
- [x] PRR-3. The clipping regression fixtures inherit the user's local UI scale — [#1747]
- [x] PRR-4. Changing a page's modal exclusivity does not invalidate a pending activation — [#1748]
- [x] PRR-5. F3 click correlation sees controls below modal and pointer-blocking surfaces — [#1750]
- [x] PRR-6. A post-step screenshot failure discards already-drained oracle evidence — [#1752]

## 1. Embark-probe artifact ownership

### [#1569] PRR-1. The embark probe overwrites fixed save slots and never cleans its artifacts

> **Captured note:** Give every embark-probe invocation unique, probe-owned save and log paths and remove all of its saves, screenshots, and logs from a top-level `finally`. The current probe uses ordinary fixed save-slot names under the active resource root and explicitly keeps its screenshot directory, despite issue #782 requiring cleanup after success or failure.

**Verification:** Verified structurally. The probe owns four fixed `/tmp` log names, two fixed durable save-slot names, and one newly allocated screenshot directory. Engine shutdown is attempted per session, but no path removes any of those artifacts; the report explicitly announces that screenshots are kept. Because saves are ordinary resource-root slots, an existing user slot with either fixed name is republished rather than isolated as test data.

**Evidence:**

- Issue #782 / PR #859 explicitly require the probe to clean up temporary saves, screenshots, logs, and ports after both success and failure.
- `tools/location_embark_probe.py:91-105` declares four process-global `/tmp/location_embark_*_engine.log` paths plus the fixed save names `location_embark_base` and `location_embark_local`.
- `tools/location_embark_probe.py:437-458` publishes the generated fixture to `SAVE_BASE`; `:823-827` publishes the exercised session to `SAVE_LOCAL`. Neither name includes a PID, random suffix, temporary root, or invocation identity.
- The normal save path publishes under the process resource root's `saves/<slot>/` directory. Reusing a slot name is an intentional save update with generation rotation, not a test-only namespace, so a user's same-named slot is mutated.
- `tools/location_embark_probe.py:892-947` allocates `shots` with `tempfile.mkdtemp`, then has several early returns and three session `finally` blocks that call only `quit_engine`. There is no outer artifact-cleanup block.
- `tools/location_embark_probe.py:950-959` prints `screenshots kept in ...` on both pass and failure. Repository searches found no `remove`, `unlink`, `rmtree`, or save-deletion call associated with the two slots or four log paths.
- Other save probes already demonstrate the expected ownership shape: they use unique names and/or final cleanup rather than leaving fixed player-visible save slots behind.
- `python3 tools/ci_probes.py --self-test` passed during this review, but that registry check proves only registration/classification; it does not execute or audit cleanup behavior.
- Tracker and findings-report searches found no existing issue or report entry owning this cleanup miss. Closed #923 consumes the embark probe as a prerequisite but does not repair its artifact ownership.

**Handoff context:**

- **Current behavior:** Running `python3 tools/location_embark_probe.py` can replace or rotate real saves named `location_embark_base` or `location_embark_local`, overwrites shared `/tmp` logs, and leaves both save slots plus a screenshot tree behind. An early failure leaks the same classes of artifacts.
- **Expected behavior:** Each run owns collision-free artifact identities and removes every artifact it created from an outer `finally`, while still attempting orderly engine shutdown. Cleanup must not delete a pre-existing user save that the probe did not create.
- **Scope and constraints:** Surfaced in PR #859 / issue #782. Preserve the three-session scenario and useful failure diagnostics; if screenshots/logs must be retained on failure for diagnosis, copy or report them under an explicit opt-in rather than contradicting the default cleanup contract.
- **Remaining uncertainty:** The exact save-directory generation left behind depends on the current save-storage implementation, but the fixed-slot mutation and total absence of probe cleanup are direct. The probe was deliberately not launched during this review because doing so would perform the unsafe writes being reported.

## 2. Embark-probe save completion

### [#1746] PRR-2. The embark probe restarts before its asynchronous saves are durable

> **Captured note:** Capture the request id from each `engine.saveWorld` call and wait for that exact request to reach `SaveCaptureComplete` before quitting or loading it in the next session. A fixed sleep after queue acceptance is not a durability boundary, and returning a literal string after the Lua call does not even verify that the request was accepted.

**Verification:** Verified structurally. Both saves are followed only by short sleeps and then engine shutdown. The persistence save's check always receives the literal string `true`, regardless of the Boolean returned by `engine.saveWorld`. The current save API documents queue acceptance as synchronous and encoding/disk publication as asynchronous, while the shared probe library already contains the request-specific completion primitive this probe omits.

**Evidence:**

- Issue #782 / PR #859 require a real `save -> quit -> restart -> load` persistence proof. That proof needs the save which session (b) produced, not a stale same-named generation or a request interrupted during shutdown.
- `tools/location_embark_probe.py:453-458` calls `engine.saveWorld(..., SAVE_BASE)`, sleeps for one second, returns, and immediately reaches `quit_engine` in `finally`.
- `tools/location_embark_probe.py:823-828` calls `engine.saveWorld(..., SAVE_LOCAL)`, appends `return 'true'`, checks only whether that literal response contains `true`, sleeps for 0.5 seconds, and returns to the caller. The caller's `finally` quits the engine before session (c) loads the slot.
- `src/Engine/Scripting/Lua/API/Save.hs:245-255` states that `saveWorld` returns true once the command is queued; disk-write failures are asynchronous.
- `tools/probelib.py:291-325` provides `wait_save_complete`. Its contract explains that the save barrier may release before encode/disk I/O is durable and that only the matching request's `SaveCaptureComplete` or `SaveFailed` status is authoritative.
- Fixed slot names amplify the false-positive risk: if an earlier run left `SAVE_BASE` or `SAVE_LOCAL`, the next session can load that stale generation when the new request failed, had not published, or was interrupted.
- `quit_engine` is an orderly-shutdown attempt, not a save-completion primitive; it can terminate the process after its own timeout. No call in this probe captures `engine.getSaveStatus()` before shutdown.
- The probe was not executed because PRR-1 establishes unsafe fixed-slot writes. The structural async contract is independently covered by the current save API and shared probe helper.
- Tracker and findings-report searches found no existing owner for this embark-probe-specific durability race.

**Handoff context:**

- **Current behavior:** The probe may terminate the fixture or exercised session while its save is still encoding/writing. It can then fail nondeterministically, or worse, pass session (c) against a stale save left by an earlier run; the session-(b) “save” assertion cannot detect an immediate `false` return.
- **Expected behavior:** The probe validates queue acceptance, captures the new request id, polls the request-specific status to terminal completion, fails with the save status/log context on `SaveFailed` or timeout, and only then shuts down and starts the loading session.
- **Scope and constraints:** Surfaced in PR #859 / issue #782. Reuse `probelib.wait_save_complete` and the existing request-id capture pattern rather than inventing another timing sleep. Pair the repair with PRR-1's unique slots so completion cannot be confused with an older same-named request.
- **Remaining uncertainty:** No timing failure was induced live because launching the probe would make the unsafe persistent writes from PRR-1. The ordering bug does not depend on machine speed: the code never observes a durability signal at all.

## 3. Clipping-test configuration isolation

### [#1747] PRR-3. The clipping regression fixtures inherit the user's local UI scale

> **Captured note:** Pin `uiscale = 1` in the #747 Lua widget fixtures, or derive every assertion from the scale they intentionally select. These headless tests hard-code 1× geometry while the production widgets default to `engine.getUIScale()`, so a normal gitignored video preference makes the committed suite fail.

**Verification:** Verified directly and traced to local configuration ingress. With the current checkout clean and `config/video.local.yaml` set to the supported value `ui_scale: 1.5`, the focused `UI.Clipping` group fails two examples in isolation. The observed 90-pixel list viewport and slider value 65 are exactly the 1.5× results; the fixtures omit `uiscale` while asserting 60-pixel/1× geometry.

**Evidence:**

- Issue #747 / PR #857 require deterministic headless clipping and widget-parent coverage. PR #857 added both failing examples and their Lua fixtures.
- `test-headless/Test/Headless/UI/Clipping.hs:718-743` builds a list with `itemHeight = 20`, `maxVisible = 3`, and no `uiscale`, then `:565-569` requires the clip height to be exactly 60.
- `scripts/ui/list.lua:134-142` resolves `uiscale = params.uiscale or scale.get()` and scales `itemHeight`. With the ordinary local setting 1.5, the production viewport correctly becomes `3 * floor(20 * 1.5) = 90`, contradicting the fixture's hard-coded 60.
- `test-headless/Test/Headless/UI/Clipping.hs:776-808` similarly creates the parented slider without `uiscale`. `:652-671` hard-codes the 1× track start 68, width 184, and expects a click at 252 to produce 100.
- `scripts/ui/slider.lua:116-139` scales width and cap width through `scale.get()` when the parameter is absent. At 1.5×, the click coordinate selected by the test is only about 65% through the real track; the isolated run returned `65`.
- `scripts/ui/scale.lua:4-7` delegates to `engine.getUIScale()`. The review environment's gitignored `config/video.local.yaml` contains `ui_scale: 1.5`, while `config/video_default.yaml` contains 1.0. Both are supported runtime values.
- Isolated focused results during this review:
  - `--match "exposes the list"`: expected clip `100,100,200,60`, got `100,100,200,90`.
  - `--match "slider drag-to-value mapping stays correct once parented"`: expected `100`, got `65`.
- The rest of `UI.Clipping` passed, and `UI.PopupPlacement` passed all 25 focused examples. This narrows the problem to fixture hermeticity rather than the clipping or placement implementation.
- Tracker and findings-report searches found no issue owning the local-UI-scale contamination of these tests.

**Handoff context:**

- **Current behavior:** A developer who changes the supported UI scale through the application can make the headless suite fail until they manually restore/delete their local video config. The failure presents as a clipping/slider regression even though the widgets are applying the configured scale correctly.
- **Expected behavior:** Headless regression geometry is independent of gitignored user preferences. Tests which intend 1× pass `uiscale = 1`; tests intended to cover multiple scales select each scale explicitly and compute/assert the corresponding geometry.
- **Scope and constraints:** Surfaced in PR #857 / issue #747. Fix both fixtures and audit the remainder of that file for the same implicit-scale/hard-coded-coordinate shape. Do not change production scaling to satisfy the tests and do not overwrite the user's local config from a test.
- **Remaining uncertainty:** CI normally lacks `video.local.yaml`, so this does not explain CI failures on a clean runner. It is a deterministic local-suite failure under an officially supported persisted setting, not a product rendering bug.

## 4. Pending-activation invalidation

### [#1748] PRR-4. Changing a page's modal exclusivity does not invalidate a pending activation

> **Captured note:** Treat a real change to `upInputExclusive` as a page-level route mutation for release activation. If a visible page becomes an exclusive modal boundary and then pass-through again during one press, the final route looks restored and the current epoch checks allow the old press to activate even though the route was interrupted in between.

**Verification:** Partially verified structurally. The public mutation definitely changes `routePointer`'s page scope and definitely leaves both pending-activation epochs untouched. The same toggle reverted before release therefore cannot be distinguished from no mutation. No current production Lua call that deliberately toggles an already-visible page during an active press was found; existing callers configure a newly created page before showing it, so current gameplay reachability remains a latent API/contract question for the processor.

**Evidence:**

- Issue #745 / PR #856 require hiding, replacing with a modal, changing menus, or otherwise interrupting the route to cancel safely. Returning inside restores only a positional excursion; route-affecting state restored before release is supposed to remain invalidated.
- `src/UI/ControlActivation.hs:131-167` snapshots `upmPageEpoch` plus the pressed element/ancestor `ueRouteEpoch` chain and cancels only when one of those values changed before the final `routePointer` check.
- `src/UI/Manager/Page.hs:67-89` bumps `upmPageEpoch` for real show/hide transitions, which is why a modal that appears and disappears is remembered.
- `src/UI/Manager/Page.hs:118-126` changes `upInputExclusive` without checking for a no-op and without calling `bumpPageEpoch`.
- `src/UI/InputOwnership.hs:125-180` makes `upInputExclusive` the authority for the topmost modal boundary and the set of pages eligible for routing. A real exclusivity change is therefore route-affecting at page scope even when visibility does not change.
- `src/UI/ControlActivation.hs:162-174` re-runs only the final route after its epoch checks. A `false -> true -> false` exclusivity transition leaves the final routing and both stored epochs identical to press time, so the original control activates.
- `test-headless/Test/Headless/UI/ControlActivation.hs:347-388` covers page hide/show and a separate modal show/hide, and the full focused group passed 30 examples. Repository searches found no activation example which changes `setPageInputExclusive` during a press.
- The Lua binding at `src/Engine/Scripting/Lua/API/UI/Page.hs:82-95` exposes this as an unrestricted mutation, not a construction-only setter. Current production callers in `scripts/popup.lua` and `scripts/input_check_fixture.lua` set pass-through immediately after page creation; no current mid-gesture caller was found.
- Tracker and findings-report searches found no existing issue owning the exclusivity/activation epoch gap.

**Handoff context:**

- **Current behavior:** The public UI state can interrupt a pending click by inserting a modal boundary and then restore the old route without canceling the click. This contradicts the durable-history logic used for visibility/clickability/detach transitions, but shipped callers do not presently appear to exercise the timing window.
- **Expected behavior:** A real `upInputExclusive` transition bumps the relevant page-level invalidation epoch; a no-op assignment does not. A transition on any visible page that changes input scope cancels pending discrete activations, even if reverted before release.
- **Scope and constraints:** Surfaced in PR #856 / issue #745, building on #742's setter. Preserve the deliberate rule that unrelated element churn does not cancel clicks, and preserve current no-op behavior. Add a focused pure or wire test for `false -> true -> false` during a press.
- **Remaining uncertainty:** The processor should decide whether `setPageInputExclusive` is contractually construction-only despite being public and undocumented as such. If that restriction is intended, codify/enforce it; otherwise the epoch omission is a direct release-activation gap.

## 5. F3 routing parity

### [#1750] PRR-5. F3 click correlation sees controls below modal and pointer-blocking surfaces

> **Captured note:** Export enough routing-scope and blocker information for the F3 oracle to answer “which control could this click actually reach,” not merely “which reported control paints highest.” A lower HUD button under empty exclusive-modal space or a callback-less pointer blocker is currently correlated even though the real router consumes the gesture before that button.

**Verification:** Verified structurally. PR #849 repaired passive-control classification and paint-order ties, but the dump/critic join still ranks only control records. It neither filters pages below the live modal boundary nor includes non-control pointer blockers as occluders. The actual pointer router applies both policies before selecting a callback target.

**Evidence:**

- Issue #783 / PR #849 require overlapping click correlation to select the same topmost control as UI input for the coordinates and current page state, with inactive-page records ineligible.
- `src/UI/InputOwnership.hs:125-180` computes the topmost visible input-exclusive page and removes every lower page from the input search. Empty modal space therefore blocks a visible lower HUD control even though no modal element covers the point.
- `src/UI/InputOwnership.hs:183-211` searches with `elementBlocksPointer`, so a callback-less element with explicit pointer blocking returns `RouteBlocked` and prevents a lower control from receiving the gesture.
- `scripts/ui/registry.lua:37-66` marks labels and panels as `control=false`; `:82-115` stamps widget records with paint key/order but no page-scope or modal-boundary eligibility.
- `scripts/ui/registry.lua:117-140` adds raw elements only when `el.interactive` (has a click callback). It omits raw callback-less pointer-blocking elements even though `UI.getElementInfo` already exposes `pointerBlocking` at `src/Engine/Scripting/Lua/API/UI/Property.hs:179-182,207-220,264-267`.
- `tools/playtest/critic.py:192-271` discards every `control=false` record and selects the highest `(paintKey, paintOrder)` among the remaining control bounds. It has no blocker/occlusion pass and no modal-scope field to consult.
- `test-headless/Test/Headless/UI/ClickCorrelation.hs:160-177` proves a modal control outranks an overlapping HUD control, but it gives the modal page a button at the click point. It does not cover the real empty-modal-space case. `test-headless/Test/Headless/UI/InputOwnership.hs:83-90` separately proves that exact empty-space route is blocked, demonstrating the policy the F3 test omits.
- `python3 tools/playtest/critic.py --selftest` passed, and the real-Lua `ui.dumpWidgets control/paintKey` group passed four examples. Their green status is consistent with the gap: neither suite plants an exclusive modal with no control at the click point or a higher callback-less blocker.
- Tracker searches found closed #645/#783 but no follow-up owning modal-scope or blocker parity, and no findings report already records it.

**Handoff context:**

- **Current behavior:** A dead click behind an empty modal boundary or callback-less blocker can be attributed to a visible lower control. The critic may classify the result as discoverability/disabled-control friction and suppress `phantom-affordance-join`, even though the control was not input-eligible for that click.
- **Expected behavior:** F3 correlation reproduces the complete pointer target policy: live modal scope, effective visibility/clipping, pointer-blocking occlusion, and then the control target's paint ordering. Passive context remains available without itself becoming a control.
- **Scope and constraints:** Surfaced in PR #849 / issue #783. Reuse engine-owned policy facts rather than re-deriving layer semantics in Python. Preserve shown-but-disabled controls as correlatable when they are the actual top eligible surface, and keep all oracle data critic-only.
- **Remaining uncertainty:** The exact trace schema change is open: the dump could mark `inputInScope` and retain blockers, or expose a point-query oracle. The current disagreement between the two selection policies is direct.

## 6. Post-step evidence crash recovery

### [#1752] PRR-6. A post-step screenshot failure discards already-drained oracle evidence

> **Captured note:** Merge post-step event/outcome evidence into the turn record immediately after draining it, before attempting the screenshot. If screenshot capture then fails, preserve those destructive-read results and record the missing post frame separately instead of falling back to the pre-step-only oracle.

**Verification:** Verified with a deterministic offline reproduction. A fake engine emitted one event and one accepted outcome only on the post-step drain, then raised `EngineCrash` on the post-step screenshot. The runner performed both oracle reads, but the recorded turn contained empty events/outcomes and `post_screenshot = null` because `oracle` was not reassigned until after the failing screenshot.

**Evidence:**

- Issue #775 / PR #848 require each action's own post-step outcomes, events, and visible result to remain associated with that action while preserving crash/interruption recording.
- `tools/playtest/engine.py:260-307` documents `oracle_events()` as a cursor/destructive read. F4 outcomes are drained from the engine and event-log cursor state advances, so a consumed post-step slice cannot be recovered by the next call.
- `tools/playtest/run.py:284-288` assigns `oracle` to a pre-step-only merge before advancing the simulation.
- `tools/playtest/run.py:327-334` destructively reads `post_events`, then calls `eng.screenshot(post_frame)`, and only after screenshot/hash success reassigns `oracle` with the post events.
- `tools/playtest/run.py:335-350` records `oracle` from `finally`. If the screenshot raises, that variable still contains only the earlier pre-step merge even though the post evidence has already been drained.
- Replay has the identical ordering at `tools/playtest/run.py:458-466`, so replay traces lose their own drained post-step evidence on the same failure.
- `tools/playtest/engine.py:242-248` raises `EngineCrash` when screenshot capture fails, making engine loss/crash a real path through this window.
- Review-time deterministic reproduction: the fake engine's `oracle_events` was called twice; the second returned `event_log_new=[{"text":"post-only"}]` and one accepted outcome; the second screenshot raised. The recorded turn reported `event_reads: 2`, but `recorded_events: []`, `recorded_outcomes: []`, and `post_screenshot: None`.
- `python3 tools/playtest/run.py --selftest` passed its existing suite. It covers post-injection, unpause, pacing, repause, pre-oracle, and normal final-post evidence cases, but contains no failure injected after the post drain and before `_merge_oracle`.
- Tracker and findings-report searches found closed #775 but no follow-up owning this crash window.

**Handoff context:**

- **Current behavior:** A screenshot failure after a completed simulation step erases the action's already-observed post-step event and outcome evidence from the persisted trace. The critic then sees only pre-step evidence for the crash turn, and the destructive F4 data is gone.
- **Expected behavior:** As soon as post events are drained, the turn's durable in-memory oracle includes them. Screenshot success may add `post_screenshot` and `visual_change`; screenshot failure records their absence/error without discarding the event/outcome slice. Session and replay paths share the same contract.
- **Scope and constraints:** Surfaced in PR #848 / issue #775. Preserve the original `EngineCrash`, acknowledged input prefixes, `step_phase`, replay semantics, and player/oracle separation. Add the deterministic failure injection described above to the runner self-test.
- **Remaining uncertainty:** If screenshot failure should also alter the stop-reason taxonomy or add an explicit oracle error field, that is a secondary schema decision. Retaining the already-drained evidence is required regardless.
