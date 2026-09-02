# Epic Review Findings: Epic #641 — Naive-player UX playtesting harness

This report records the current-HEAD review of epic #641 at
`dc9721904ed9abc7aae91c9fe60155c6d0e58a4b`. The epic declares nine children:
C1/manual #642, F1/screenshot #643, F2/input injection #644, F3/widget oracle
#645, F4/action outcomes #646, H1/runner #647, H2/critic #648, C2/personas
#649, and P1/offscreen rendering #650. All nine are closed and their
implementation pull requests merged. The completed arc remains coherent: the
player sees pixels and uses injected input, the harness retains a replayable
lockstep trace and separate oracle evidence, the critic produces grounded
findings, personas are reproducible, and offscreen rendering supports
unattended runs. Later work repaired the historical action-evidence,
click-correlation, trace-gap, persona-validation, and critic-verdict defects
found after the epic closed. The action-outcome stream's process-lifetime bound
is already pending as HPA-31 and is not duplicated here. Two new current
mistakes survive: the closed epic leaves every child unchecked, and the
critic's advertised separate output directory produces a report whose
screenshot links are all broken.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [x] ER-1. Epic #641 leaves all nine completed children unchecked — [#641]
- [x] ER-2. A separate critic output directory breaks every report screenshot link — [#2220]

## 1. Epic completion steering

### [#641] ER-1. Epic #641 leaves all nine completed children unchecked

> **Captured note:** Epic #641 is closed and its closure comment confirms that
> all nine scoped children #642 through #650 landed, but every child remains
> unchecked in the live epic body.

**Verification:** The contradiction is confined to the live tracker body. All
nine declared children are closed, their implementation pull requests merged,
and the closure comment explicitly says all nine completed. The current
implementation and focused gates agree with the completed arc. The unchecked
roadmap is stale steering state, not evidence of missing implementation.

**Evidence:**

- [Epic #641's live body](https://github.com/coghex/synarchy/issues/641) — the
  `Sub-issues` section still presents #642 through #650 as nine unchecked
  entries.
- [The closure comment](https://github.com/coghex/synarchy/issues/641#issuecomment-4950078573)
  — states that all nine scoped children are closed, including phase-2
  offscreen rendering, and that the manual, harness, personas, and critic are
  present.
- `tools/playtest/trace.py:1` and `tools/playtest/critic.py:1295` — retain the
  landed H1 trace and H2 critic boundaries.
- Current focused checks passed: the runner, critic, persona, and
  action-outcome coverage self-tests; 151 focused Hspec examples across action
  outcomes, input injection, screenshot capture, CLI validation, and click
  correlation; and the real-engine `action_outcome_probe.py`.

**Handoff context:**

- **Current behavior:** A reader opening the closed epic sees all nine children
  presented as unfinished despite every live child state and the closure record
  saying the arc is complete.
- **Expected behavior:** Mark #642 through #650 complete in the epic's
  `Sub-issues` checklist.
- **Scope and constraints:** Tracker-body-only correction to epic #641. Preserve
  the child set, dependency graph, phase-2 distinction, and historical scope.
- **Verification target:** The closed epic shows exactly the same nine declared
  children, all checked.
- **Deduplication:** All-state tracker searches for #641, its title, the child
  checklist, and the closure wording found no corrective owner beyond the epic
  itself. The docs-worktree report corpus has no pending owner for this
  tracker-body correction; similar findings for other completed epics concern
  different tracker bodies.
- **Remaining uncertainty:** None. This is a mechanical checklist correction.

## 2. Critic report artifact integrity

### [#2220] ER-2. A separate critic output directory breaks every report screenshot link

> **Captured note:** H2's `--out DIR` option writes `report.md` outside the
> session trace while rendering the trace-relative `frames/...` screenshot
> paths unchanged, so the report's evidence images no longer resolve.

**Verification:** Reproduced offline at current HEAD with the shipped canned
trace and `FakeCritic`. Writing the report to a sibling `separate-output`
directory produced twelve Markdown screenshot references and zero resolvable
files relative to the report: `broken refs: 12 of 12`. With no explicit output
directory the report lives in the trace root and the same references work. The
critic self-test passes because its report assertions use the default output
location and never exercise this advertised CLI branch.

**Evidence:**

- `tools/playtest/trace.py:85-87` and `tools/playtest/trace.py:125-127` — define
  both pre-step and post-step screenshot paths as relative to the trace
  directory.
- `tools/playtest/critic.py:1385-1396` — copies those trace-relative paths
  directly into every finding's screenshot references.
- `tools/playtest/critic.py:1239-1258` — renders each stored reference verbatim
  as a Markdown image link.
- `tools/playtest/critic.py:1401-1409` — relocates `findings.json` and
  `report.md` to `out_dir` without rebasing or copying the referenced images.
- `tools/playtest/critic.py:2454-2455` — advertises `--out` as the report output
  directory, so this is a supported path rather than an internal-only
  parameter.

**Handoff context:**

- **Current behavior:** `critic.py TRACE --out OTHER_DIR` completes
  successfully, but every `frames/...` link in `OTHER_DIR/report.md` points
  under `OTHER_DIR` even though the images remain under `TRACE`.
- **Expected behavior:** Every screenshot link in a generated Markdown report
  resolves from that report's location for both the default and an explicit
  output directory.
- **Scope and constraints:** Preserve the trace's relative-path schema, critic
  evidence/frame-ownership rules, `findings.json` and `report.md` contents, and
  default in-trace output. Choose one canonical, portable way to rebase or
  stage report images when `out_dir` differs. Coordinate with the structural
  split tracked by #2069, but do not make that refactor a prerequisite for this
  behavioral correction.
- **Verification target:** Extend the offline critic self-test to build the
  canned trace, call `run_critic` with a distinct output directory, and prove
  every rendered Markdown screenshot target exists when resolved from the
  report directory; retain all existing self-test checks.
- **Deduplication:** All-state tracker searches for `critic`, `--out`, output
  directories, broken screenshots, screenshot references, and `report.md`
  found no issue owning this failure. Open #2069 preserves the CLI and current
  screenshot-reference behavior during a module split; it does not specify a
  repair. The docs-worktree report corpus has no matching pending finding.
- **Remaining uncertainty:** The correction may rebase links or copy/link
  evidence into the output directory; the durable requirement is that the
  written report remains portable and its evidence resolves.
