# Project Review Findings: PRs #668–#655

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #668, #667, #666, #663, #662, #661, #660, #659, #658, #657, #656, and #655 — for later one-at-a-time disposition. The first-parent window contains no direct non-PR commits.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

The UI-manager, Buildings API, top-level Lua router, Lua API registration, UI Manager, input-state, and world-edit splits retain their intended public surfaces in the current tree, and both Haskell/Lua module-budget audits pass. The current preview CLI, UTF-8 text, and config-state probes also pass. PR #661's changed fresh-clone defaults were explicitly approved by the repository owner during that PR's review and are not re-filed here. Two concerns remain: PR #659's dawn-centered bear sleep urge conflicts with the shared dawn wake boundary, and PR #666's once-clean Cabal metadata now fails `cabal check` after the later checked-in `-Werror` policy.

## Status

- [x] PRR-1. The nocturnal bear can wake at the same dawn that makes it seek sleep — [#1945]
- [x] PRR-2. The Cabal package has regressed to a failing `cabal check` — [no-issue]

## 1. Species-specific sleep phase and wake phase

### [#1945] PRR-1. The nocturnal bear can wake at the same dawn that makes it seek sleep

> **Captured note:** Make automatic circadian wake timing compatible with the per-species sleep phase. A dawn-centered nocturnal species must not be driven to bed by dawn and then immediately woken by the same dawn crossing.

**Verification:** Verified against the current engine and Lua code. Brown bears have `circadian_center = 0.25`, documented as a dawn peak at which they bed down after being active overnight. The shared sleep action independently treats the first crossing of the fixed `DAWN_ANGLE = 0.25` as a wake condition for every species. In a controlled arena run at dawn with a bear at 40% sleep pressure, directly exercising the real `sleepExecute` state boundary with `sleepLastSunAngle = 0.249` changed `sleepPhase` from `sleeping` to `waking`; the otherwise identical call with `sleepLastSunAngle = 0.251` left it `sleeping`. Thus sleep duration can jump from almost none to as much as the next pressure/full-day boundary depending on which side of dawn the sleeping pose completes. The shipped #613 probe passes because it places the bear at exactly dawn, waits for it to fall asleep after that crossing, and wakes it only through the explicit public wake API.

**Evidence:**

- PR #659 / issue #613 made brown bear the proof case for a species whose sleep drive peaks in a different time-of-day window from the default diurnal curve. The issue requires its AI to demonstrably seek sleep in that different window.
- `scripts/unit_resource_config.lua:237-251` describes the bear as active overnight and bedding down as the sun comes up, then sets `circadian_center = 0.25` and `circadian_width = 0.125`.
- `scripts/unit_ai_sleep.lua:18-22` defines wake as pressure near full or the first dawn crossing since falling asleep, whichever comes first. That rule predates the species-specific curve and is still described as universal v1 behavior.
- `scripts/unit_ai_sleep.lua:40-54` hard-codes `DAWN_ANGLE = 0.25`; `dawnHasArrived` returns true whenever the stored prior angle is below 0.25 and the current angle is at or above 0.25, without consulting the unit definition or its circadian center.
- `scripts/unit_ai_sleep.lua:110-120` calls that fixed dawn predicate for every sleeping unit after the pressure-full check. A sleep-deprived bear is therefore not protected by its low pressure once it crosses dawn.
- Live controlled reproduction through the real headless engine: at sun angle 0.25, a bear with 40% sleep pressure and an AI state forced to `sleeping` returned `waking` from `sleepExecute` when its previous angle was 0.249; resetting only the previous angle to 0.251 returned `sleeping`.
- `tools/circadian_species_probe.py:16-30` says its end-to-end phase proves selection and the sleeping pose, then wakes through the public API. Lines 235-273 start the clock at exactly dawn before the bear begins seeking sleep and call `wakeUnit` after it sleeps; they never cross dawn while the bear is already asleep.
- The current `circadian_species_probe.py` still passes its raw phase, utility crossover, selection, pose, and manual-wake checks. That confirms the regression is in the interaction between the new phase and the older automatic wake condition, not in the per-species urge itself.
- Full open-tracker title inventory, targeted GitHub search, and findings-report search found no existing issue or report for a nocturnal bear waking on its sleep-urge boundary.

**Handoff context:**

- **Current behavior:** A bear that reaches the sleeping phase just before dawn wakes on the crossing even if its sleep pressure is far from recovered. A bear that reaches the same phase just after dawn does not see another dawn crossing until the next day and instead normally wakes on pressure recovery. The discontinuity is tied to pose-completion timing rather than species physiology.
- **Expected behavior:** Automatic circadian wake timing is derived from or otherwise compatible with the species' configured phase. The diurnal default can retain its intended dawn wake, pressure recovery and `wakeUnit` remain valid wake paths, and a dawn-centered nocturnal animal gets a meaningful sleep interval after its dawn urge.
- **Scope and constraints:** Surfaced from PR #659 / issue #613 interacting with the shared #612 sleep action. Preserve the per-species raised-cosine urge, longitude-aware local sun angle, sleep-pressure recovery threshold, explicit wake API, pose chain, and existing acolyte behavior. Extend the focused species probe with a sleeping-before-boundary case and a sleeping-after-boundary case; an end-to-end natural timing check would additionally guard the pose-transition window.
- **Remaining uncertainty:** Issue #613 scoped itself to per-species sleep-drive curves, while #612 explicitly called dawn wake a v1 rule. The original PR also acknowledged that the wake rule remained universal. The processor should decide whether that was an intentional temporary limitation deserving deferral or an integration bug, but the current proof species makes the two accepted rules behaviorally contradictory at exactly the configured peak.

## 2. Cabal metadata validation

### [no-issue] PRR-2. The Cabal package has regressed to a failing `cabal check`

> **Disposition:** No issue — the exit-1 is a signed-off consequence of #1057's checked-in `-Werror`, documented at `synarchy.cabal:99-104` as "accepted deliberately, not an oversight", alongside the same treatment for the `-O2` (`:145`) and upper-bounds (`:160-164`) warnings. No live gate runs `cabal check`: it is absent from `ci.yml`, `tools/ci-local.sh`, the Makefile, and every tracked doc, so nothing in the repository implies the checker passes. #635's clean-check acceptance is a closed issue's record of its merge state, not a standing contract, and the two available repairs would either stop ordinary local builds being warning-fatal — the gap #1057 closed — or relocate `-Werror` to `cabal.project` purely so a command no gate runs returns 0.

> **Captured note:** Reconcile the checked-in fatal-warning policy with the clean-package contract established by #635. Either restore a zero-exit `cabal check` while keeping ordinary builds warning-fatal, or explicitly supersede the old acceptance contract so the repository does not continue to imply that the Cabal checker passes.

**Verification:** Verified in the current primary checkout. `cabal check` exits 1. Cabal reports the checked-in `ghc-options: -Werror` as the `[werror]` portability error and ends with `Error: Hackage would reject this package.` It also emits non-fatal warnings for `-O2` and missing upper bounds on dependencies other than `base`. The source-distribution half of PR #666 remains healthy: `cabal sdist --list-only` succeeds with 6,711 entries, and comparison against tracked `assets/`, `data/`, `scripts/`, `config/`, and `cbits/` files found only the three deliberately excluded legacy config paths. The current failure is therefore specifically a checker-policy regression, not the stale-glob or missing-resource failure #666 originally repaired.

**Evidence:**

- PR #666 / issue #635 was filed because Cabal metadata errors made the package description untrustworthy. Its requirements say all `cabal check` errors must be resolved, and its acceptance list begins with a successful `cabal check`.
- PR #666's final review recorded `cabal check` exiting 0 with two justified warnings and verified a real source tarball. The merge therefore met the linked issue before later policy changes.
- Current live command: `cabal check` exits 1 and reports `[werror] 'ghc-options: -Werror' makes the package easy to break with future GHC versions`, followed by the Hackage-rejection error.
- `synarchy.cabal:73-100` adds `-Werror` to the shared package warning stanza. Lines 94-99 explicitly acknowledge that this makes `cabal check` complain and accept the result because the project is pinned and not published to Hackage.
- Closed issue #1057 later required ordinary local builds and CI to share one checked-in fatal-warning policy. Its acceptance checks builds and the presence of `-Werror`, but does not mention or explicitly supersede #635's `cabal check` gate.
- `synarchy.cabal:13-15` likewise states that the package is developed from the repository and is not published to Hackage. That lowers the distribution severity but does not change the fact that the repository-local acceptance command now fails.
- `cabal sdist --list-only` currently succeeds. Its 6,711-file manifest contains the runtime data, PNG/TTF assets, Lua scripts, tracked templates, headers, and documentation #666 promised. The absent tracked `config/keybinds.yaml`, `config/notifications.yaml`, and `config/video.yaml` are legacy migration fixtures reintroduced after #666; the Cabal comment intentionally enumerates templates to exclude runtime/local config, so this report does not treat those omissions as the checker failure.
- The open tracker contains #1280 for contradictory Cabal module-inventory paths, but that issue does not cover `cabal check`, `-Werror`, or distribution-policy exit status. Full tracker and findings-report searches found no current item reconciling #635 with #1057.

**Handoff context:**

- **Current behavior:** A normal repository build is warning-fatal as intended by #1057, but the package fails the exact Cabal metadata validation command #635 made green. A contributor re-running #635's acceptance sees a hard failure even though source-distribution creation still succeeds.
- **Expected behavior:** The repository has one explicit, internally consistent contract. Preferably `cabal check` returns 0 without weakening warning-fatal builds; if Cabal cannot express both under the selected policy, the superseding decision and replacement validation should be recorded where #635's clean-check claim is discoverable.
- **Scope and constraints:** Surfaced from PR #666 / issue #635 after closed issue #1057. Preserve `-Werror` for repository-owned library, executable, and test builds; do not impose it on dependencies; keep the shared warning categories, pinned toolchain, current optimization profiles, and complete sdist manifest. This is not a request to publish to Hackage or broadly bound every dependency.
- **Remaining uncertainty:** The Cabal file deliberately accepts this checker failure, and the package explicitly disclaims Hackage publication, so the processor may conclude that #1057 intentionally superseded #635 and disposition this without a new issue. What is not uncertain is the observable regression: `cabal check` changed from exit 0 at #666 to exit 1, while #635's acceptance contract remains the only durable statement attached to that cleanup.
