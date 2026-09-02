# Project Review Findings: PRs #1987–#1893

This report records the senior review of the next twelve uncovered merged pull requests in merge order — #1987, #1906, #1904, #1903, #1902, #1901, #1899, #1898, #1897, #1895, #1894, and #1893 — plus direct first-parent commits `99d73d07`, `0dd0cdc8`, and `4960d4d9` in the same landing interval. The review read each pull request, its linked specification where one existed, merged diff and commits, then traced the surviving behavior at current HEAD. The direct census reconciliation exposed one post-merge protocol-status drift from already-covered PR #1979; it is retained below because it is current, reproducible, and still blocks the authoritative census validator. The twelve selected pull requests and the other two direct documentation commits produced no separate current concern, and no concern was explicitly excluded from this batch.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [x] PRR-1. Reconcile the text-encoding probe's census protocol status — [no-issue]

## 1. Probe-census registry agreement

### [no-issue] PRR-1. Reconcile the text-encoding probe's census protocol status

> **Disposition:** No issue — already fixed. Commit `b1a97d46f` (2026-08-31, after this report's capture) landed the `text_encoding` census row at `probe-result/v1`; on 2026-09-02 `python3 tools/probe_census.py --validate` exits 0 against the branch-resolved docs-wip manifest (95 probes) and `tools/test_probe_census.py` passes (973 assertions). Master's own committed copy still lags the docs-wip working copy on seven unrelated rows, which is the ordinary accumulate-then-land state, not this drift.

> **Captured note:** Complete the post-merge census reconciliation promised by PR #1979: the authoritative docs-worktree row for `text_encoding` still says `legacy`, while the live de-flake registry declares `probe-result/v1`, so the census validator refuses the manifest.

**Verification:** Verified against the branch-resolved `docs-wip` manifest and the current in-repository registry. `python3 tools/probe_census.py --validate` exits 1 with `probe 'text_encoding': manifest protocol status 'legacy' disagrees with the in-repo registry ('probe-result/v1')`. PR #1979 deliberately left the census untouched for post-merge reconciliation and named this exact mismatch as expected until that follow-up; the follow-up has not reached the authoritative docs worktree.

**Evidence:**

- `docs/probe_census.json:1470-1472` in the `docs-wip` worktree — the authoritative `text_encoding` row records `"protocol": "legacy"`.
- `tools/probe_flake.py:75-80` — the live protocol registry includes `"text_encoding": probe_protocol.PROTOCOL_VERSION`, which currently resolves to `probe-result/v1`.
- `tools/probe_census.py:938-950` — the manifest implied by current code derives each row's protocol from `probe_flake.protocol_status`, so the stored row is not merely descriptive prose; it disagrees with the inventory authority used by validation.
- `tools/probe_census.py:978-1035` — `validate_manifest` compares the stored inventory fields to that live manifest and emits the reproduced controlled refusal on protocol drift.
- PR #1979's merged description says `docs/probe_census.json` was deliberately untouched and should be reconciled only after the migration merged; its validation section records the same unpublished docs-worktree mismatch. Direct commit `0dd0cdc8` had reconciled the census earlier that day, before #1979 changed this probe's protocol status.
- The project-review corpus contains no existing current entry for this mismatch. The up-front open-issue inventory and all-state searches for `text_encoding` census/protocol drift found no tracker owner; the generic closed census issues #1428, #1492, #1493, and #1660 define the mechanism but do not track this post-merge row update.

**Handoff context:**

- **Current behavior:** The de-flake lab's authoritative census cannot pass its inventory validator because one manual-only probe's stored protocol status lags the live registry. Read-only census views that validate the manifest fail before producing a usable result.
- **Expected behavior:** The `text_encoding` census row agrees with the live `probe-result/v1` declaration while preserving every existing census policy, cohort, sample, attempt, claim, outcome, and deferral field.
- **Scope and constraints:** This is the post-merge documentation-state follow-up explicitly deferred by PR #1979, not a defect in the probe's migrated result protocol. Reconcile through the census tool's docs-worktree mutation path; do not hand-edit away retained census history or move the manifest into runtime authority.
- **Verification target:** `python3 tools/probe_census.py --validate` exits 0 against the branch-resolved `docs-wip` manifest, and the focused census self-tests continue to pass.
- **Deduplication:** No matching tracker issue or project-review/findings-report entry was found. Closed issues #1428, #1492, #1493, and #1660 are mechanism/history context rather than owners of this concrete drift.
- **Remaining uncertainty:** None.
