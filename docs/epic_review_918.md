# Epic Review Findings: Epic #918 — Expedition survival mechanics

This report records the current-HEAD review of epic #918 at
`faf652ac3a846ffa7f8efb5fd9a5b49e5133f30f`. The epic has no native GitHub
sub-issues and its body declares exactly two children, #925 followed by #919.
Both are closed as completed and both implementation PRs are merged. The manual
`expedition` and `first-aid` scenarios remain documented, outside CI and the
probe registry, and preserve their diagnostic-only exit-status contract. The
mechanic and scenario defects found by the original calibration were later
dispositioned through SURV-1 through SURV-10; #998 and the filed follow-ups
#1212, #1213, and #1216–#1221 are now closed, while the two explicit product
decisions remain `[no-issue]`. One new current mistake survives: the current
file identified as the calibration observation log no longer contains the run
record that child #919 required it to preserve.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [x] ER-1. The survival calibration document no longer contains its required run record — [#2185]

## 1. Calibration evidence retention

### [#2185] ER-1. The survival calibration document no longer contains its required run record

> **Captured note:** Child #919 required
> `docs/expedition_survival_calibration.md` to retain the exact scenario commands
> and revision, provisioning choices, expedition checkpoints, first-aid
> observations, and the rationale for every changed or deliberately unchanged
> tunable. A later direct documentation rewrite converted that path into a
> process-report findings ledger and removed the source calibration narrative.

**Verification:** PR #937 originally landed the complete record at this path.
Immediately before commit `9eda1412b`, the document still had `Run record`,
`Scenario 1 — expedition`, `Provisioning choices`, `Checkpoint observations`,
`Scenario 2 — first-aid`, `The treatment`, and `Tunables changed` sections,
including the exact two commands and the tested revision. Commit `9eda1412b`
then rebuilt the file as numbered SURV findings. The current document says it
“extracts the remaining actionable mechanics and scenario-quality concerns”
from a source calibration, but neither contains nor links that source record.
The current expedition design nevertheless continues to cite this same path as
the step-7 “observation log.” The evidence remains recoverable through Git
archaeology, but no current documentation artifact fulfills #919's durable
record contract.

**Evidence:**

- [Issue #919, requirement 2](https://github.com/coghex/synarchy/issues/919) —
  requires this exact document to record the commands/revision, provisioning,
  body-state and return checkpoints, fall/treatment and kit observations, and
  tunable rationale.
- [PR #937](https://github.com/coghex/synarchy/pull/937) — originally landed the
  observation-first calibration and explicitly reported zero tunable changes
  after the project-owner decision.
- `9eda1412b^:docs/expedition_survival_calibration.md:23` — the pre-rewrite file
  contains the run-record table; line 30 names both scenario commands, and the
  later scenario/checkpoint/treatment/tunable sections preserve the evidence
  #919 requested.
- `docs/expedition_survival_calibration.md:3` — the current file describes
  itself as an extraction of remaining concerns rather than the calibration
  record and provides no current link to that removed source narrative.
- `docs/expedition_gameplay_loop.md:486` — current steering still calls this
  path the step-7 observation log.

**Handoff context:**

- **Current behavior:** The processed SURV dispositions are durable, but a
  reader cannot inspect the observations that justified the no-tuning decision
  without discovering and reconstructing an unnamed historical Git revision.
- **Expected behavior:** Restore the original #919 run evidence as a current,
  linked historical artifact—preferably a dated file under `docs/history/`—and
  link it from both the current SURV findings report and the expedition design.
  Preserve at least the commands, tested revision, provisioning, checkpoints,
  first-aid result, kit state, and tunable/no-change rationale. Archiving the
  complete pre-`9eda1412b` document would also retain #998's later calibration
  addendum.
- **Scope and constraints:** Documentation-only recovery. Keep the current
  findings report's checklist, markers, and dispositions unchanged; do not
  rerun the scenarios, reinterpret their balance outcome, or change gameplay
  constants as part of restoring the record.
- **Verification target:** A current linked artifact contains every #919
  requirement-2 field and identifies its source revision; both
  `docs/expedition_survival_calibration.md` and
  `docs/expedition_gameplay_loop.md` point to it without misdescribing the
  processed findings ledger as the raw observation log.
- **Deduplication:** All-state tracker searches for the calibration path, source
  run record, removed observations, and calibration history found only the
  closed source/follow-up issues. The docs-worktree report corpus has no pending
  owner for restoring the evidence. The current expedition design is a
  contradictory reference, not an owner for the missing artifact.
- **Remaining uncertainty:** Only the archival shape and filename. The original
  record and its removal are exact, reproducible Git objects.
