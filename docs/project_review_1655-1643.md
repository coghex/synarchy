# Project Review Findings: PRs #1655–#1643

These entries record focused evidence from the senior review of the next twelve merged PRs in merge-time order — #1655, #1654, #1653, #1651, #1650, #1649, #1648, #1647, #1646, #1645, #1644, and #1643 — for later one-at-a-time disposition. There were no direct first-parent commits in the same landing interval.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]` reviewed and deliberately never to be filed · `[deferred]` blocked on a concrete precondition

PRs #1655, #1654, #1653, #1651, #1650, #1649, #1646, and #1644 retain their intended contracts in the current tree, and their focused tests, self-tests, or audits passed during this review. PR #1645's own review identified the remaining partial-stamp materialization behavior, but that concern is already pending as PRR-6 in `docs/project_review_432-412.md`, so it is not duplicated here. The closely related foraging-probe setup-boundary defect is already recorded as PRR-1 in `docs/project_review_1684-1656.md`; that entry does not own the three additional probes below. No batch defect was found to have been repaired by a later merge, and tracker and pending-report searches found no existing owner for the two scopes recorded here.

## Status

- [x] PRR-1. Three isolated-root probes stage their trees outside the cleanup boundary — [no-issue]
- [x] PRR-2. Four isolated-root probes can inherit undeletable config modes, and item-instance hides the failure — [#1912]

## 1. Isolated-root setup cleanup

### [no-issue] PRR-1. Three isolated-root probes stage their trees outside the cleanup boundary

> **Disposition:** No issue — fixed after this report was written. Issue #1791 (commit `c8a2a526`, 2026-08-28) moved staging inside the cleanup guard in all three probes plus `foraging_probe.py`, and added `tools/test_probe_root_cleanup.py`, which injects a `copytree` failure after the root and symlinks exist and asserts the invocation base is gone — the exact coverage this finding asked for. Wired into both probe-runner self-test blocks, CI and `tools/ci-local.sh`; passing.

> **Captured note:** Extend the failure-path guard in the farm-AI, flora-growth, and item-temperature probes around isolated-root construction itself. Each PR deliberately moved `boot` inside the cleanup-owning `try`, but `make_isolated_root` still mutates the invocation-owned tree before control reaches that guard.

**Verification:** Verified structurally and by isolated fault injection in all three probes. Replacing each module's `shutil.copytree` with a synthetic `OSError` made root construction fail after `<base>/root` and its three content-family symlinks had been created. In every case the invocation base and partial root survived until the review harness removed them externally.

**Evidence:**

- `tools/farm_ai_probe.py:293-308`, `tools/flora_growth_probe.py:355-370`, and `tools/item_temp_probe.py:287-302` — `mkdtemp` and `make_isolated_root(base)` execute before the `try`; only engine boot and later phases are protected by the unconditional cleanup.
- `tools/farm_ai_probe.py:86-96`, `tools/flora_growth_probe.py:63-73`, and `tools/item_temp_probe.py:75-85` — construction is incremental: each helper creates `root`, adds three symlinks, copies `config`, and then creates `saves`. A source, permission, disk, or interruption failure can therefore occur after cleanup-worthy state exists.
- `tools/farm_ai_probe.py:689-705`, `tools/flora_growth_probe.py:502-518`, and `tools/item_temp_probe.py:467-483` — `remove_run_root(base)` is unconditional only after execution enters the corresponding `try`; a staging exception bypasses it entirely.
- PRs #1647 and #1648 explicitly explain why a pre-`READY` boot failure belongs inside the guard so it cannot strand the root, but leave the earlier construction path outside the same boundary. Issue #1616 and #1613 requirement 6 require failing runs to leave no run-created save artifact, while both PR descriptions make removal of the complete temporary root part of their isolation contract.
- PR #1649 provides the same batch's corrected ownership shape for the embark probe: its guard starts immediately after `mkdtemp` and covers isolated-root construction as well as every later phase.
- All-state tracker searches for these probes' setup/root-cleanup failure found only the closed implementation issues #1616 and #1613. Pending-report searches found the analogous foraging-probe entry in `docs/project_review_1684-1656.md`, but that entry is scoped to `tools/foraging_probe.py` and does not own these three files.

**Handoff context:**

- **Current behavior:** A root-staging exception terminates the affected probe non-zero but leaves its uniquely created temporary base and whichever symlinks, copied configuration, or save directory were already placed there.
- **Expected behavior:** Once `mkdtemp` succeeds, every later exit path attempts deletion of exactly that invocation-owned base. Orderly engine shutdown remains conditional on a process actually having launched, and cleanup failure remains a reported nonzero result.
- **Scope and constraints:** Surfaced in PR #1647 / issue #1616 and PR #1648 / issue #1613. Preserve per-invocation save names, config copies without local overrides, symlink-safe deletion, no deletion of pre-existing paths, and the busy-port rule that avoids sending `engine.quit()` to another process.
- **Verification target:** Add setup-failure coverage for each affected helper, or one shared contract test if the helpers are consolidated, that raises after root creation and asserts the invocation base is gone. Retain the existing boot-failure, successful-run, scenario-failure, and cleanup-failure behaviors.
- **Deduplication:** Open/closed tracker and project/findings-report searches found no owner beyond the closed source issues. The pending foraging PRR is the same defect class in a fourth file, not an owner for this scope; disposition may combine them if that entry is still unprocessed.
- **Remaining uncertainty:** The review injected failure at `copytree` rather than forcing actual disk exhaustion or an external interruption. The cleanup bypass and surviving partial directory do not depend on which incremental staging operation raises.

## 2. Copied-config cleanup permissions

### [#1912] PRR-2. Four isolated-root probes can inherit undeletable config modes, and item-instance hides the failure

> **Captured note:** Make every invocation-owned config copy removable independently of the source checkout's permission bits, and make item-instance treat surviving residue as a failing check. These four helpers copy modes verbatim; three report deletion failure but leave the artifact, while item-instance passes `ignore_errors=True` and can return its already-computed green summary after cleanup silently did nothing.

**Verification:** Verified structurally and with a controlled filesystem fixture. A private root containing a `0555` config directory and `0444` file survived `shutil.rmtree(..., ignore_errors=True)` with both entries intact. This is the shape produced when `copytree` copies a read-only source tree. The three explicit cleanup helpers would return false for the same `OSError`; item-instance suppresses it and never checks whether the base remains.

**Evidence:**

- `tools/farm_ai_probe.py:92-95`, `tools/flora_growth_probe.py:69-72`, `tools/item_temp_probe.py:81-84`, and `tools/item_instance_probe.py:81-84` — each uses `shutil.copytree` without normalizing the permissions of the invocation-owned destination.
- `tools/location_embark_probe.py:154-181` documents the concrete contract already discovered in the next-newer sibling implementation: `copytree` preserves source mode bits, so a read-only checkout, cache, mount, or archive can produce a private config tree whose entries cannot be unlinked. Its `_make_owner_writable` normalizes only the copied destination.
- `tools/farm_ai_probe.py:100-120`, `tools/flora_growth_probe.py:77-98`, and `tools/item_temp_probe.py:89-112` correctly turn an `rmtree` exception or survivor into a false cleanup result, and their callers return nonzero. They nevertheless leave the invocation-owned tree behind, contrary to #1616/#1613 requirement 6 and the PRs' no-residue contract.
- `tools/item_instance_probe.py:521-531` computes and returns `summarize()` from inside the `try`, then calls `shutil.rmtree(tmpdir, ignore_errors=True)` in `finally`. A deletion error is swallowed, residue is not checked, and the previously computed zero exit status is returned unchanged.
- Issue #1617 requirement 6 and PR #1643 both require passing and failing paths to remove the artifact the run created. PR #1643's verification covered an unwritable `saves/` directory, but not copied read-only `config/` modes or a deletion failure; its root builder otherwise has the same copied-config shape as the three adjacent probes.
- All-state tracker searches for read-only copied config, cleanup residue, and item-instance `rmtree` behavior found no open or closed issue beyond the four closed implementation issues. Pending project/findings-report searches found no entry owning this permission and false-green scope.

**Handoff context:**

- **Current behavior:** When the source config tree lacks owner-write permission, all four probes can finish with an invocation-owned root they cannot delete. Farm-AI, flora-growth, and item-temperature report the cleanup failure and exit nonzero; item-instance can report all scenario checks passed and exit zero while silently retaining its root and save.
- **Expected behavior:** Each probe can delete every path it creates regardless of source-tree mode bits, without changing the source or following its content-family symlinks. Any survivor is both reported and reflected in a nonzero result.
- **Scope and constraints:** Surfaced in PRs #1643, #1647, and #1648 and issues #1617, #1616, and #1613. Preserve copied config defaults with `*.local.yaml` excluded, invocation-only ownership, symlink-safe removal, engine shutdown before deletion, unique slot behavior, and the rule that no pre-existing developer path is removed or chmodded.
- **Verification target:** Add a pure cleanup case that stages config directories/files without owner-write bits, releases the root, and asserts it is absent. Add an item-instance case that forces cleanup failure or a survivor and asserts a named nonzero result. Retain checks that the repository's real content and configuration modes remain unchanged.
- **Deduplication:** Open/closed tracker and project/findings-report searches found no current owner. PR #1649 fixed this condition only for `location_embark_probe.py`; its helper neither covers nor is shared by the four affected probes.
- **Remaining uncertainty:** The current primary checkout's config tree is writable, so ordinary local runs there do not trigger this branch. The controlled fixture represents the read-only checkout/cache/mount environments that the current code and PR #1649's own contract explicitly recognize.
