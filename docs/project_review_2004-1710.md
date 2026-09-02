# Project Review Findings: PRs #2004–#1710

This report records the senior review of the next twelve uncovered merged pull
requests in merge-date order — #2004, #1753, #1751, #1749, #1742, #1741,
#1727, #1728, #1726, #1725, #1700, and #1710. The review read each pull
request, its linked specification where one existed, merged diff and commits,
then traced the surviving behavior at current HEAD. The first-parent landing
interval also contains 32 direct documentation commits, audited here:
`87ae3951`, `4960d4d9`, `0dd0cdc8`, `99d73d07`, `83fddc35`, `91444631`,
`19af28ea`, `1f591b9d`, `dc470999`, `ae680144`, `3cf352c2`, `80821ee7`,
`2174eacc`, `0f167e87`, `b8598932`, `abe25c1c`, `74c7c975`, `fa8248d6`,
`6afb9bba`, `506f150a`, `f647359e`, `05ff148e`, `f9cb3fcd`, `1c1c9a61`,
`95d7cee7`, `1f8fb793`, `10af3154`, `4fa04422`, `91ff70e2`, `1a92447f`,
`339c7a41`, and `0a4cc084`. Their current descendants retain their intended
design, findings-disposition, probe-census, bug-record, and project-review
roles; none introduces production behavior.

The sweep produced one new current finding from PR #1728. Two other current
concerns encountered in the interval are already preserved and are not
duplicated here: the page-local craft-bill identity race remains unprocessed as
HPA-48 in `docs/holistic_project_audit_findings.md`, and the location-completion
marker that can outlive #1749's declined structure placement is already
`docs/project_review_2003-1754.md` PRR-1.

Focused Hspec checks passed 148 examples across deferred drag selection, the
quad camera snapshot, declined structure staging, player-event progress,
saguaro content, bindless failure publication, texture-handle zero, cross-page
unit cargo, AI page pairing, and craft-claimant reconciliation. The sleep probe
passed its complete real-engine wake-boundary scenario. The playtest harness
self-test, findings-report audit, and both module-budget guards passed. The
lunge probe passed its success, timeout, and unlifted cases, but its replaced-
target cleanup check failed once and passed on a diagnostic rerun, producing
the timing defect below. The five saguaro source images also passed dimensions,
RGBA/alpha, variant-difference, and contact-sheet inspection. No full headless
suite, graphical session, worldgen tier, world check, baseline capture, full
probe sweep, or `make ci` was run.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [x] PRR-1. The lunge probe can grade a fresh replacement-target launch as stale bookkeeping — [#2168]

## 1. Lunge acceptance stability

### [#2168] PRR-1. The lunge probe can grade a fresh replacement-target launch as stale bookkeeping

> **Captured note:** Stabilize PR #1728's replaced-target lunge scenario so it
> proves that the original launch cleared all seven bookkeeping fields without
> allowing a legitimate follow-on launch at the replacement target to
> repopulate those same fields between the probe's two console reads.

**Verification:** Reproduced with the current real-engine probe. One ordinary
`python3 tools/lunge_probe.py` run exited 1 after the replaced-target case
reported zero strikes carrying the cancelled launch's identity but all seven
lunge fields still set. The other three scenarios passed. A diagnostic rerun
passed all ten checks, establishing timing dependence. The current writer trace
shows why the failure is not evidence that production omitted cleanup:
`lungePhase` can only be set to `"air"` by a launch or cleared to `nil` by
`M.clear`, which clears the complete seven-field list. The probe first polls
until one console read sees `phase != "air"`, then makes a separate console
request for the current field set while the subject's AI remains active against
the decoy. A new launch can occur between those requests and atomically
repopulate exactly the seven fields the failed run printed.

**Evidence:**

- `tools/lunge_probe.py:496-509` — the replaced-target case changes
  `attackTargetUid` to the live decoy but neither suspends the subject nor
  refuses a subsequent jump before grading cleanup.
- `tools/lunge_probe.py:581-595` — `_grade_cancellation` first polls the current
  `phase` until it is no longer `"air"`, then performs a distinct
  `leftover_fields` request and treats any currently populated lunge as residue
  from the cancelled launch. It does not compare the later fields with the
  captured launch identity.
- `scripts/unit_ai_combat_lunge.lua:75-88` — `LUNGE_FIELDS` is the complete
  seven-field set and `M.clear` nils every member together.
- `scripts/unit_ai_combat_lunge.lua:174-188` — phase 2 cancels and clears when
  the caller's target no longer matches the stored launch target, then returns
  control to ordinary attack execution.
- `scripts/unit_ai_combat_lunge.lua:206-254` — a later eligible execute tick can
  launch at the replacement target and writes all seven fields together,
  including `lungePhase = "air"`.
- The captured failing run reported `replaced_nostrike` as passing and
  `replaced_clear` as failing with
  `lungePhase,lungeSawAir,lungeStartAt,lungeTarget,lungeMode,lungeReach,lungeImpactSpeed`;
  the immediate diagnostic rerun passed all ten lunge checks. This exact
  pass/fail alternation is consistent with the inter-read relaunch window and
  cannot distinguish it from omitted cleanup.

**Handoff context:**

- **Current behavior:** A correct target-replacement cancellation can make the
  manual lunge gate fail when the same actor begins another valid lunge before
  the probe's leftover-field read. The failure incorrectly attributes the new
  launch's state to the cancelled launch.
- **Expected behavior:** The scenario observes the original launch's terminal
  cleanup under a fixture that cannot relaunch before grading, or otherwise
  binds its assertion to the captured launch identity. It still proves zero
  momentum strikes against both the original and replacement targets.
- **Scope and constraints:** Keep production lunge behavior, the seven-field
  all-or-nothing cleanup, target replacement, real leap/landing, bounded launch
  attempts, record-only attack wrapper, and the other three scenarios
  unchanged. Refusing only future jumps after the first launch is already a
  pattern in the probe's unlifted case and must not cancel the in-flight leap.
- **Verification target:** Make the replaced-target case deterministic, run it
  repeatedly through the real engine, and prove that an intentionally omitted
  `M.clear` still fails while a legitimate follow-on attack cannot create a
  false cleanup failure. Retain `python3 tools/lunge_probe.py --self-test` and
  the probe-registry self-check.
- **Deduplication:** All-state tracker searches for lunge-probe relaunch,
  replacement cleanup, false failures, and bookkeeping fields found only
  closed #1713, the production contract PR #1728 implemented, and open #1909,
  which exclusively owns keeping this probe's fixtures on loaded arena terrain.
  No existing findings-report entry covers the inter-read relaunch race.
- **Remaining uncertainty:** The first failing output did not print the
  repopulated `lungeTarget`; it printed only the complete field-name set. The
  writer inventory makes a fresh launch the only path that can repopulate all
  seven after the poll has observed a non-air phase, but a corrected probe
  should retain the old and new target identities explicitly so future
  failures diagnose this boundary directly.
