# Project Review Findings: PRs #2441–#2425

Reviewed twelve merged PRs in newest-first merge order: #2441, #2439, #2438,
#2437, #2436, #2435, #2434, #2433, #2432, #2431, #2430, and #2425. Review
covered their linked issues, descriptions, commit messages, merged patches,
and current code and consumers. Each GitHub diff matched its actual
first-parent merge diff. The interval `6732510d8^..b615db242` contains exactly
those twelve PR merges and no direct commits. No concern was excluded.
Current failure-path verification was completed on 2026-09-07 at `e89e61bb1`;
the only change since the freshly built `3e158738b` executable is unrelated
capability-inventory documentation.

Focused validation passed: 114 responsive-menu examples, 12 wound-API
examples, six notification config-load/save examples, eight fixture-logging
examples, 237 capability-audit test groups (538 assertions), nine module-budget
tests (26 assertions), 18 CI-timing groups (130 assertions), the CI-parity
self-test, the etymology resource/window self-test, and 85 persistence-sweep
self-test assertions. The deflake suite's 265 cases and 2,207 assertions had
already passed on the same production code during the preceding batch.
Independent structural comparisons found no lost definitions in the deflake
contract extraction and no lost test statements in the responsive-menu split.
The chop fixture was checked against its registration, placement, harvest and
teardown consumers; no live chop or GPU etymology run was performed here.
No additional already-tracked or fixed-later defect needs a handoff.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]` reviewed and deliberately never to be filed · `[deferred]` blocked on a concrete precondition

## Status

- [ ] PRR-1. The etymology probe grades page activation before the queued switch completes

## 1. Probe page-switch synchronization

### PRR-1. The etymology probe grades page activation before the queued switch completes

> **Captured note:** Synchronize phase 7 of the etymology probe with actual
> activation of its custom-named page before grading that page. PR #2434
> correctly repairs HUD resource handling for #1983, but its validation
> discloses a separate, surviving page-switch race left without a tracker
> handoff. This race predates the PR; the resource repair did not introduce it.

**Verification:** Complete current-code trace, supplemented by an offline
execution of the real `phase7_unavailable` with the transport replaced by a
delayed-worker reading. Returning `main_world` on the first active-page query
produced one failed check, exactly one active-page read, and no panel opening.
That execution demonstrates the probe's premature verdict, not the live
worker's timing. Independently, PR #2434 reports this exact failure in one of
three live runs, followed by two passing runs. A fresh live GPU reproduction
was not attempted during this review.

**Evidence:**

- `tools/etymology_probe.py:977` — initializes the custom page and waits for
  initialization before requesting its visibility. This wait cannot fence the
  subsequent show request.
- `tools/etymology_probe.py:980` — requests `world.show('custom')`.
- `src/Engine/Scripting/Lua/API/World/Lifecycle.hs:640` —
  `enqueueSelectionChange` updates pending/projection state and writes a world
  command; it does not wait for the world worker to apply visibility.
- `src/Engine/Scripting/Lua/API/World/Lifecycle.hs:726` — `worldShowFn`
  enqueues `WorldShow` and returns, without an application acknowledgement.
- `src/World/Thread/Command/UI.hs:39` — the worker's atomic update is what
  eventually puts a newly visible page at the head of `wmVisible`.
- `src/Engine/Scripting/Lua/API/World/Clock.hs:313` —
  `worldGetActiveWorldIdFn` reads applied `wmVisible`, not the queued request.
- `tools/probelib.py:58` — `send` waits for console output/idle, not an
  acknowledgement from the world-command consumer. Its incidental pacing is
  not a completion barrier.
- `tools/etymology_probe.py:985` — samples the active page once; lines 986–989
  immediately count a failure and return when that reading is still the old
  page, skipping the custom-name/unavailable checks.
- `tools/etymology_probe.py:1599` — the next lifecycle phase runs regardless;
  the failed phase-7 count later makes the whole probe exit nonzero.
- [PR #2434](https://github.com/coghex/synarchy/pull/2434), “Pre-existing flake
  observed, not touched” — records the same `active='main_world'` result and
  identifies the absent wait explicitly.

**Handoff context:**

- **Current behavior:** Correct asynchronous page switching can produce a
  failing etymology run and prevent its required unavailable-state assertions
  from executing. The result depends on whether the worker processes the
  command before the single sample.
- **Expected behavior:** Establish the intended active page through a bounded,
  observable completion condition before querying its etymology. A switch
  that never completes must still fail clearly; delayed successful activation
  must not be reported as a product failure. Use exact page identity rather
  than substring acceptance when judging that condition.
- **Scope and constraints:** Keep this a probe-side synchronization repair.
  Preserve the real custom-name initialization, production page-selection
  operation, all unavailable-state assertions, resource-warning grading and
  manual-only GPU classification. Do not add a fixed sleep or change the
  production `world.show` semantics to make the probe pass. Check the phase's
  exit/cleanup page state too: its trailing `world.show(PAGE)` does not by
  itself reorder an already-visible page under the current handler contract.
- **Verification target:** A deterministic offline case where the old page is
  observed before the requested page must continue to the existing panel
  assertions; immediate success must also work. Permanent non-activation and
  malformed readings must terminate with clear failure evidence. Then run
  the real offscreen probe and retain evidence that the custom-name checks
  actually ran, with no resource warnings after either HUD rebuild.
- **Deduplication:** Open-tracker inventory, all-state etymology title searches,
  and searches for `etymology active world`, `etymology race`, and
  `"world.show" "etymology"` found no matching issue. Closed #1983 owns the
  repaired resource handles; #1604/#1608 own entry-point/overflow fixture
  requirements; #1362 concerns a different Hspec worker-death fixture;
  #1265 concerns recurrence page scope. Existing project-review reports do
  not capture this probe's activation race.
- **Remaining uncertainty:** Live frequency on current HEAD is unmeasured.
  The historical one-in-three result is the PR author's observation, not a
  frequency estimate from this review. No production page-switch defect is
  asserted.
