# Project Review Findings: PRs #609–#535

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #609, #608, #605, #602, #601, #600, #598, #597, #595, #594, #536, and #535 — for later one-at-a-time disposition. The same first-parent window also contains direct commits `b09c1518` (`CI updates`) and `eaee85c3` (`quick comment`).

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

The current furnace/machine-shop content, recipe-dependent power accounting, infection isolation, medic coordination, disarm behavior, Craft module split, and added texture assets all remain functional in focused verification. The `machine_shop`, `power_workshop`, `infection`, `disarm`, and `medic_coord` probes passed together; the focused `Craft.RecipeYaml`, #590 power-accounting, and infection hspec groups passed; both probe-policy and module-budget self-checks passed. #601's apparently lost disarm promotion was not retained as a finding because later repository policy explicitly records the probe as manual-only after Linux timing instability. Direct commit `eaee85c3` only preserves historical triage and clarifies HsLua stack indexing. Three current concerns remain: parallel probe assignments can collide when a probe reserves more than one port, the unreviewed `b09c1518` policy rewrite erased previously accepted feature-gate coverage without per-probe revalidation, and the probe-runner README still embeds the drifting count #594 sought to eliminate.

## Status

- [x] PRR-1. Parallel probe assignments can overlap a neighboring probe's secondary port — [#1571]
- [ ] PRR-2. A direct CI rewrite removed accepted feature-gate coverage without per-probe evidence
- [ ] PRR-3. The probe-runner README still carries the drifting count #594 meant to remove

## 1. Parallel probe port allocation

### [#1571] PRR-1. Parallel probe assignments can overlap a neighboring probe's secondary port

> **Captured note:** Make `run_probes.py --jobs` allocate non-overlapping port reservations, not merely distinct base integers. A registered probe that legitimately binds `--port + 1` must not collide with the next probe's assigned base, and an explicit aggregate `--port` must either participate in the parallel allocation or be rejected instead of being silently ignored.

**Verification:** Verified in the current runner by source tracing and a reproducible two-probe execution. Parallel mode assigns consecutive bases, `9400 + index`, while `debug_console_boot_probe.py` reserves both its base and base-plus-one. Selecting `debug_console_boot` immediately before `transactional_load` therefore assigned bases 9400 and 9401. The latter engine successfully listened on 9401; the former's successful-bind check then tried the same port and failed with `Address already in use`. The aggregate run reported `debug_console_boot` failed and `transactional_load` passed. `offscreen_probe.py` independently has the same two-concurrent-engine, base-plus-one contract, so this is not a one-script anomaly. The parallel branch also ignores the parsed `--port` value entirely even though both the CLI help and README describe it as an override for every probe.

**Evidence:**

- PR #536 / issue #531 added the `--jobs` runner and promised one independent engine on a unique port per concurrent probe. Its final review fixed retry accounting, but the design represented a probe's port requirement as one integer rather than a reserved span.
- `tools/run_probes.py:44-49` documents `PARALLEL_PORT_BASE + index` as the unique per-probe override. `:432-437` implements exactly that consecutive assignment, with no probe metadata for additional listeners.
- `tools/debug_console_boot_probe.py:412-433` explicitly defines `--port` as a base and binds both `args.port` and `args.port + 1`: one occupied-port test at the base and one successful engine boot on the adjacent port.
- `tools/offscreen_probe.py:723-735` keeps its first offscreen engine alive on the assigned base while booting a second engine on `base + 1`, which can likewise overlap the following parallel assignment.
- `python3 tools/run_probes.py --only debug_console_boot,transactional_load --exact --jobs 2 --retries 0 --timeout 240 --tail 60` reproduced the collision. `/tmp/transactional_load_probe_9401.log` recorded `READY port=9401`; `/tmp/debug_console_boot_probe_9401.err` recorded that its listener on 9401 could not start because the address was already in use; the aggregate exited 1.
- `tools/run_probes.py:369-371` says `--port` will “override every probe's --port,” and `tools/README.md:398-399` says the override is uniform. Sequential mode passes `args.port` at `run_probes.py:413-416`, but parallel mode unconditionally substitutes `PARALLEL_PORT_BASE + idx` at `:432-435`, so `--port` is silently ignored whenever `--jobs` exceeds one.
- Closed issue #723 standardized `--port` support across registered probes and describes the runner as handing every probe a unique base. It did not add port-span metadata or account for probes that derive a second simultaneous listener from that base. Targeted all-state tracker searches and findings-report searches found no existing item for adjacent port-range overlap or the parallel override mismatch.

**Handoff context:**

- **Current behavior:** Distinct parallel tasks get distinct base numbers, but one task may legally consume the next task's base. The resulting failure is presented as a probe regression even though both probes can pass alone. A user-provided aggregate `--port` affects sequential execution but has no effect in parallel execution.
- **Expected behavior:** Every concurrently running probe owns all ports it may bind for that invocation, with no overlap between allocations. The aggregate CLI's `--port` contract is consistent across sequential and parallel modes, or the unsupported combination fails immediately with a clear explanation.
- **Scope and constraints:** Surfaced from PR #536 / issue #531 and still present after #723. Preserve the GUI-port 8008 guard, per-probe historical defaults for bare direct invocation, parallel isolation, and the existing “parallel first, retries solo” accounting. Do not assume every probe needs a single port; `debug_console_boot` and `offscreen` are current two-port cases, and future probes should not require another hard-coded allocator exception.
- **Remaining uncertainty:** The allocation interface is a design choice: explicit per-probe span metadata, a conservative stride, or dynamically reserved ports could all satisfy the behavior. The exact semantics of `--port` with multiple jobs also need a deliberate choice, but silent disregard is inconsistent with the current CLI and documentation.

## 2. Behavior-probe gate coverage

### PRR-2. A direct CI rewrite removed accepted feature-gate coverage without per-probe evidence

> **Captured note:** Re-evidence the behavior probes demoted by direct commit `b09c1518` against the repository's current promotion criteria, and make the resulting coverage decisions explicit. In particular, a power-only change should not be routed to a green behavior-probe gate that executes zero power behavior unless maintainers have deliberately accepted and recorded that gap; previously reviewed cooking and infection coverage should not remain removed solely because the rewrite labeled it narrow or scenario-heavy without supporting measurements.

**Verification:** Partially verified. The historical and current coverage changes are exact: the reviewed #535 gate selected `cooking` for recipe changes; #600 promoted the self-contained `infection` probe; and the original #530 mapping selected `power_workshop` for power changes. Less than an hour after #600 merged, first-parent direct commit `b09c1518` changed the eligibility rule from fast and deterministic to broad, cheap smoke only, removed all three probes, and added self-test expectations that power and infection paths select zero probes. Current policy has since regained catalogue smoke for infection and several generic recipe probes, but `src/Power/*` remains explicitly mapped to an empty set. What is not yet proven is the right final disposition: one current local run of `infection` and `power_workshop` passed, but that is not sufficient evidence under the later repeated-run, contention, timing, breadth, and cost criteria established by #722.

**Evidence:**

- Issue #530's goal was a path-selective blocking gate that made green CI mean affected features still worked. Its accepted implementation mapped `src/Power/*` to `power_workshop` and originally treated docs/assets as the only zero-probe fast path.
- PR #535's first review identified a concrete recipe-only coverage hole and required `data/recipes/*` to select both `craft` and `cooking`. Commit `7f7cd03b` implemented that correction before approval.
- PR #600 / issue #593 made `infection_probe.py` self-contained, documented ten clean runs, and promoted it to the blocking set. Its merge commit is `e98131c0`; direct commit `b09c1518` followed at 10:01 on the same morning and removed `infection`, `cooking`, and `power_workshop` without a linked issue, PR review, run table, or probe-specific failure evidence.
- The `b09c1518` diff added the `scenario-heavy` and `targeted` categories, reduced the eligible set from twelve probes to four, changed recipes from `{craft, cooking}` to `{craft}`, changed power from `{power_workshop}` to an empty set, changed infection/combat to an empty set, and rewrote self-tests to require those reduced selections. Its commit subject is only `CI updates`.
- Current `tools/ci_probes.py:119-176` classifies `infection` and `power_workshop` as scenario-heavy and `cooking` as targeted. `:328-349` routes recipe changes to four generic/content probes, infection changes to `content_registry`, and `src/Power/*`, `scripts/power*.lua`, `scripts/wire.lua`, and `data/structure_packs/*` to `set()`.
- Current `python3 tools/ci_probes.py --changed src/Power/Network.hs` prints `no CI-eligible probes for changed paths`; the self-test passes because that empty result is an asserted policy. This matters to the reviewed range because #608 / issue #590 changed live job-dependent power accounting, and `power_workshop_probe.py` is the focused real-engine exercise of powered workshop behavior.
- In current local verification, `power_workshop` passed in 84.1 seconds and `infection` passed in 54.6 seconds while sharing a five-probe `--jobs 3` batch. The focused #590 hspec group also passed all 14 examples. These results show the current implementations work and make re-evaluation plausible, but they are only one contention run rather than the evidence needed for CI promotion.
- Later issue #722 established objective promotion evidence for then-unclassified probes: repeated solo and parallel runs, maximum runtime, no GPU/worldgen, direct rather than AI-timing assertions, and sufficient breadth. It explicitly excluded reclassification of probes that already had a reason, so it mechanically accepted the buckets created by `b09c1518` without re-testing these demotions. Its review also acknowledged the hard-coded power-empty self-test as current state rather than proving it desirable.
- All-state tracker searches found #530, #590, #592, #593, and #722 as the relevant closed history but no open issue asking whether the direct rewrite's demotions or empty power mapping still meet the current evidence standard. Findings-report searches found no existing concern for this coverage-policy gap.

**Handoff context:**

- **Current behavior:** A PR touching only the power network, power Lua, wire Lua, or structure packs runs no behavior probe. Infection catalogue changes exercise registry load/query but not infection progression; recipe changes run generic craft/content/repair probes but not the cooking integration probe that #535 explicitly added. Full CI and focused hspec coverage still run, so this is specifically a loss of behavior-probe gating, not an assertion that these paths have no automated tests.
- **Expected behavior:** Probe eligibility and feature mappings reflect measured current evidence and an explicit coverage/cost decision. If a subsystem intentionally has no eligible behavior smoke, that accepted gap and its compensating coverage are traceable rather than inherited from a broad direct rewrite that bypassed the normal reviewed workflow.
- **Scope and constraints:** Surfaced from PRs #535 and #600, direct commit `b09c1518`, and made more material by #608's power behavior. Apply #722-quality evidence before promoting anything; local greenness alone is especially inadequate for AI/timing-sensitive probes. Preserve the path-selective gate's bounded cost and do not conflate broad smoke coverage with targeted manual probes. The processor may split power, infection, and cooking if their evidence leads to different dispositions.
- **Remaining uncertainty:** The current classifications may be the right budget tradeoff. `power_workshop` is 84 seconds and includes AI/day-night behavior; `infection` deliberately waits through timed progression; `cooking` may be adequately represented by today's broader recipe selection. The problem recorded here is the unreviewed, unmeasured transition and its surviving empty power route; the eventual issue may be evidence gathering/documentation rather than mandatory promotion.

## 3. Probe-runner inventory documentation

### PRR-3. The probe-runner README still carries the drifting count #594 meant to remove

> **Captured note:** Remove the remaining prose assertion about the current number of registered probes, or derive it from the registry. The README already names `run_probes.py --list` as authoritative; it should not immediately undercut that source of truth with another hand-maintained estimate that has fallen almost thirty probes behind.

**Verification:** Verified against the current registry and file history. `tools/README.md` says the registered inventory is “currently in the mid-50s” while `python3 tools/run_probes.py --list` emits 84 rows. Issue #539 / PR #594 was specifically intended to repair the README's stale probe count and avoid a fragile exact number; the merged wording kept a qualitative “currently” count. Issue #721 later updated that phrase from “low 30s” to “mid-50s” when the registry reached 55, demonstrating that the compromise still requires manual synchronization. The inventory has now grown to 84 and drifted again.

**Evidence:**

- Issue #539 required the README's hard-coded probe count to be removed or replaced unless it was generated or easy to maintain. PR #594 added `--list` as the authoritative listing but retained a qualitative current-count claim.
- `tools/README.md:373-379` currently says `--list` is authoritative, that the inventory has grown over time, and that the document does not track the exact number, then immediately estimates it as “currently in the mid-50s.”
- `python3 tools/run_probes.py --list | wc -l` returns 84 in the current tree. The estimate is therefore no longer a useful order-of-magnitude description of the registry.
- `git blame` attributes the surrounding authority/no-exact-count wording to the #594 implementation and the current “mid-50s” phrase to commit `bea21434` for issue #721, which registered twelve orphan probes and synchronized the docs when the actual total was 55. The second manual edit confirms the wording recreated the maintenance burden #594 set out to remove.
- The current README probe table is otherwise complete for the registered scripts, and `ci_probes.py --status` provides live eligibility counts, so no static inventory count is needed for discoverability.
- Targeted all-state tracker searches for the mid-50s wording, runner count, and probe-count drift found closed issues #539 and #721 but no open follow-up. Findings-report searches found no existing concern for this current stale estimate.

**Handoff context:**

- **Current behavior:** Readers are correctly directed to the live registry and then given a conflicting stale estimate in the next clause. Every substantial probe-registration wave requires another prose edit if the estimate is to remain meaningful.
- **Expected behavior:** The README relies on the executable registry for the current total, or obtains any displayed total mechanically, so adding probes cannot make the documentation false.
- **Scope and constraints:** Surfaced from PR #594 / issue #539, with later recurrence in #721. Keep `run_probes.py --list` and `ci_probes.py --status` as the sources of truth. Do not replace “mid-50s” with another hand-maintained number such as 84; that merely resets the same drift clock.
- **Remaining uncertainty:** None about the mismatch. The processor may decide it is too small for a standalone issue and close it as direct documentation maintenance, but the current statement is demonstrably stale and repeats the exact class of problem #594 addressed.
