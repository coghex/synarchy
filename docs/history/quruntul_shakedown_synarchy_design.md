<!--
Archival provenance (2026-10-08)
Source repository: coghex/quruntul
Source path: docs/designs/shakedown_synarchy_design.md
Last-change commit: aea8e068bc83049cd65ccbffea33f5788deb1a6a
Source Git blob: ce8bccc4df7f9a6a14ec9e15bed1ca856144040b
Source bytes SHA-256: a14613977eb688d15fe2f17af36ddf68e54c87533a736abcb82552face60b490
Reason: owner/manager decision laz gdvt2c4i. Quruntul remains generic;
consumer-specific onboarding records belong in the consumer, Synarchy.
Everything after this comment and its separating blank line is the source
file's unchanged bytes. This is a historical record, not an active mandate.
-->

# Shakedown and Synarchy onboarding design

Quruntul measures Hetoimasia today. Synarchy has an adapter but an almost
empty ledger, and its `$test` history still lives in the legacy `codex-test`
registry. This arc does two things. It gives quruntul a cheap way to prove an
adapter launches every suite correctly before an expensive flake batch relies
on it. It then brings Synarchy onto quruntul: shaken down, carrying its
valuable `$test` history, and seeded.

> **Superseded in part (owner decision 2026-10-08).** Quruntul stays generic
> and independently adoptable: it carries no consumer's onboarding plan,
> selection, budget or special case, and depends on no consumer's progress.
> What remains in force is the generic mechanics that QS-1 (#6) and QS-2 (#8)
> delivered, as [the design](../design.md) now states them; the decisions
> behind them stay here as their rationale. Everything specific to Synarchy is
> history and is no longer a quruntul plan: the epic contract, the Synarchy
> evidence and counts, the onboarding steps, D-1's arc, D-2's choice of store,
> D-6, D-9's target list, D-10, Q-6 and the moved delivery plan. Synarchy owns that work in its
> own records. Any engine change it needs, such as a per-suite trial count,
> comes to quruntul as a separate generic request. Nothing below is rewritten;
> it is kept as the record of 2026-09-30 to 2026-10-02.

Design state: `closed — historical record; do not process` (was `ready for
issue processing` until 2026-10-08)

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [x] EPIC. Shake down adapters and bring Synarchy onto quruntul — [#5]
- [x] QS-1. Add a one-trial shakedown lane that proves an adapter launches every suite — [#6]
- [x] QS-2. Import a repository's legacy `$test` history into the ledger through an adapter hook — [#8]

## Epic contract

- **Goal:** every Synarchy suite is launched correctly by its adapter, proven
  cheaply, and Synarchy's ledger holds its imported `$test` history and a
  complete first flake measurement.
- **Done when:**
  - `quruntul shakedown` exists and passes for Synarchy (and Hetoimasia);
  - the `codex-test` registry's history is in Synarchy's ledger;
  - `$flake` reports `no-candidate` for Synarchy on this platform.
- **Users and operators:** the owner and the Codex/Claude agents that run
  `$flake`, `$test`, `$deflake` and `$assess-tests` in Synarchy and
  Hetoimasia.
- **Arc label:** None proposed (quruntul has no arc labels yet).

## Current state and evidence

Verified on 2026-09-30 against quruntul `3d6b479` and Synarchy `origin/master`.

**Quruntul engine.**
- It has flake, test, deflake, assess and proposal lanes, one SQLite ledger
  per clone, and immutable run evidence (`docs/design.md` Principles, Lanes).
- A flake batch runs K trials (the adapter's `flake_trials`, default 10) of a
  suite's selected tests. It changes status only at the upstream head
  (`docs/design.md` Batches).
- The adapter contract has the `trial_env`, `outcomes` and `seed` hooks.
  `seed` pre-fills the first status of tests the ledger has just met, and
  only to `stable` or `flaky` (`docs/design.md` Adapter).
- The test lane records freshness per suite in `suites.last_test_run` and
  `suites.last_test_identity` (`quruntul/state.py`). `$test` selects a probe
  that has never been tested, whose identity has changed, or that is older
  than `refresh_days` (`quruntul/select.py` `test_order`).
- `$assess-tests` still drains the legacy `codex-test` registry through a
  separate path (`docs/design.md` Legacy repositories).

**Lessons from seeding Hetoimasia (2026-09-29).** Three harness defects were
each found only after a full 10-trial batch:
- the Hspec parser misread child-process output (quruntul#2);
- the adapter launched `shader-tests` without a file `run.sh` had deleted
  (hetoimasia#351);
- a consistent failure was labelled flaky (quruntul#3).

Each is the adapter's launch drifting from how CI runs the suite.

**Synarchy.**
- The adapter (synarchy#2760, merged) declares 102 suites. They are:
  - `synarchy-test-headless`, CI, Hspec, sliced into 400-test batches
    (`HSPEC_SLICE`);
  - `synarchy-test-graphical`, a desktop Hspec probe;
  - every registered probe, as a `command` or `exit` suite.
- The ledger has enumerated 4 of them: 10,528 tests, of which 10,526 are the
  headless suite's examples. 10 are `stable`, seeded from the probe census.
- The census (`docs/probe_census.json`, 100 probes) already feeds the
  adapter's `seed` hook and its deferrals.
- The legacy `codex-test` registry (`.git/codex-test/registry.json`, schema
  `codex-test-coordinator/v1`) holds:
  - 359 `$test` runs from 2026-08-12 to 2026-09-22, over 127 targets;
  - execution: 302 passed, 54 failed, 3 cancelled;
  - interpretation: 257 clean, 83 with observations, 13 blocked,
    6 inconclusive;
  - 364 snapshots, 5 assessments and 4 proposals;
  - reports and artifacts beside the registry.
- 309 of those runs, covering 85 targets, name a ledger suite exactly
  (`probe:<name>`). The 42 unmatched targets are 19 `playtest:`, 14 `probe:`
  (renamed or retired), 3 `gameplay:`, 3 `visual:`, and one each of
  `diagnostic:`, `manual:` and `graphics:`.

**Tracker.** coghex/quruntul has no issues. No Synarchy issue covers quruntul
onboarding or the registry.

## Desired experience

- **Adapter authors** run `quruntul shakedown` after changing an adapter or
  onboarding a repository. Within one trial per suite they learn whether every
  suite builds, lists its tests and reports every one of them, and which
  suites fail, before paying for 10-trial batches.
- **In Synarchy**, `$test` knows which probes were already observed and when,
  and the legacy observations and proposals are reachable from quruntul. Its
  first `$flake` sweep then spends time only on genuinely unmeasured tests.

## Scope

### In scope

- A shakedown lane in the quruntul engine, with its CLI and skill guidance.
- A generic, adapter-driven import of legacy `$test` history into the ledger.
- Synarchy's implementation of that import for the `codex-test` registry.
- Shaking down Synarchy's suites and repairing its adapter.
- Sizing the headless suite's slices, then seeding Synarchy's ledger.

### Out of scope

- The `codex-profile` lab (1.2 GB) and `$profile`/`$performance` engine lanes.
  The owner named only the `codex-test` registry as the legacy data to port
  (2026-09-30).
- Per-example Hspec flake history for Synarchy: none was found to exist.
- Synarchy's playtest harness.
- The probe census, which the `seed` hook already imports.
- Engine 0.3.0, the README fixes and the stale `docs-wip` README edit, which
  land as a standalone PR (D-11).

## Design

### Shakedown lane (proposal)

`quruntul shakedown [--target SUITE]` runs one trial of each suite that
applies on this platform (or of the one target) at the upstream head.
- It uses the same prepare, enumerate and launch path as `$flake`, so it tests
  exactly the launch that flake batches will use.
- It records a run in a new lane, `shakedown`, and writes a
  `quruntul-result/v1` report.
- It never changes a test's status, and writes nothing about tests or suites
  to the ledger (D-12). Its enumeration is compared with what the ledger
  knows, and differences are reported, not recorded: new tests, and ledger
  tests no longer listed. The next `$flake` enumeration records them.
- It runs only at the upstream head, never at a candidate `--ref` (D-13).
- Per suite, it reports one of: build failed, enumeration failed, examples
  unreported (`missing`), examples failed, or clean.
- Each non-clean suite becomes one observation for `$assess-tests`.

A shakedown is advisory (D-3): flake selection never reads its result. The
`$flake` skill and the onboarding guidance recommend running one after an
adapter or harness change. It covers every suite that applies on this
platform, desktop suites included under the standing approval, one
window-opening suite at a time. A suite is clean only when it built, every
listed test reported, and none failed (D-7).

### Legacy `$test` history import (proposal)

The engine gains an import step and a new optional adapter hook, named
provisionally `legacy_history(ctx)`. The hook returns records in an
engine-defined shape: runs, their observations, assessments and proposals,
each keyed by a target id. The engine alone owns the ledger schema, so it
validates and writes them; the adapter only reads its repository's legacy
store, keeping the engine free of `codex-test` specifics.
- Imported runs are marked as imported, with provenance: the legacy run id,
  the report path, and the revision. They are never re-executed, and a second
  import writes no duplicates.
- A run whose target names a current suite attaches to that suite. Every
  other run is archived, attached to no suite and counting toward no
  freshness (D-9).
- Imported runs set each matched suite's `last_test_run` and
  `last_test_identity`, the latter to the suite's identity at import time
  (D-4).
- Only closed history is imported (D-5): runs, observations with their final
  dispositions, approved assessments, and decided proposals. None enters an
  open queue.
- The owner starts it with a dedicated command, and a repeated run changes
  nothing (D-14).
- Evidence follows a finite manifest the adapter declares, with no
  engine-chosen size cap. Each file is either:
  - `copy`: copied with its SHA-256, and carried by export;
  - `reference`: recorded by path, and disclosed as excluded from export in
    the import report and per-record provenance, both of which are exported
    (D-18).

  A promised copy that cannot be copied exactly refuses the import (D-15).
- Records are identified by source store, record kind and source record id.
  The import is all-or-nothing. Identical, already-imported records are
  no-ops, even after later ledger activity, and a changed one is refused.
  Each of these refuses the import (D-16, D-17):
  - duplicate identities in the supplied input;
  - freshness that would move backwards or stay equal;
  - a proposal target the ledger already holds, under its existing limit
    of one proposal per target;
  - any copy failure.
- The import refuses a legacy store that still has open items, naming them.
  Open means an unassessed observation, or a proposal that is not rejected,
  designed or implemented (D-8). A partial drain therefore cannot be imported
  by mistake.

### Synarchy onboarding

1. Shake down all suites (QS-3), repairing the adapter until every suite is
   clean or has a recorded disposition.
2. Measure one headless trial to size `batch_tests` (QS-4).
3. Seed (QS-5), once the owner has chosen the headless suite's trial count
   from QS-4's measurement (D-10).

## Decisions

### D-1. Shake down, then onboard Synarchy

The owner chose this arc on 2026-09-30, over onboarding Synarchy directly,
engine lanes for profiling and performance, or housekeeping first.
Consequence: the shakedown lane (QS-1) precedes every Synarchy measurement
slice.

### D-2. The legacy data to port is the `codex-test` registry

The owner named the `codex-test` registry (2026-09-30) as the valuable
Synarchy history. Not selected: the probe census, which is already seeded;
`codex-profile`; and a per-example Hspec history, which was not found.
Consequence: the import carries `$test` history, meaning runs, observations,
assessments and proposals, not flake verdicts.

### D-3. A shakedown is advisory; it never gates `$flake`

The owner chose this on 2026-09-30 (resolves Q-2). A shakedown writes its
report and observations, and flake selection ignores its result.
Rejected: skipping a suite in flake selection until its latest shakedown at
its current identity is clean. That would have prevented the Hetoimasia
pattern mechanically, but it demands a shakedown after every adapter or input
change.
Consequence: running the shakedown before expensive batches is a practice the
skills recommend, not something the engine enforces. QS-5's seeding runs
after QS-3's clean shakedown by delivery order.

### D-4. Imported legacy runs count toward `$test` freshness, at the suite's current identity

The owner chose this on 2026-09-30 (resolves Q-3). The import sets each
matched suite's `last_test_run` to its newest imported run, and
`last_test_identity` to the suite's identity at import time. That trusts
that the suite has not changed since the legacy run.
Rejected:
- leaving the identity empty, so every imported probe reads as `changed`;
- keeping the history for reference only.

Consequences:
- Synarchy's `refresh_days` is 7, and the newest legacy run is from
  2026-09-22. So at import every imported probe is already `stale` rather
  than `never-tested`.
- `$test` therefore observes the 15 probe suites the registry never saw
  first, then the imported probes, oldest observation first.
- A probe whose code changed after its legacy run is not flagged `changed`;
  it waits for its refresh window.

### D-5. Drain the legacy open items first, then import closed history

The owner chose this on 2026-09-30 (resolves Q-4). Before the import, the
legacy registry's open observations are assessed through the legacy
`$assess-tests` path (`skills/assess-tests/references/synarchy.md`), and its
proposals are dispositioned. The import then carries only closed history:
- runs;
- observations with their final dispositions;
- approved assessments;
- decided proposals.

Rejected: importing open items into quruntul's queues and retiring the legacy
drain at the same time.

Consequences:
- The legacy drain stays in `$assess-tests` until the import is done.
- The import refuses a registry that still has open items (D-8).
- `$assess-tests` loses its legacy Synarchy route once the import has run.

### D-6. Synarchy-side slices are tracked in coghex/synarchy

The owner chose this on 2026-09-30 (resolves Q-7). QS-3 to QS-6 are filed in
coghex/synarchy, so each Synarchy pull request closes its own issue. The
quruntul epic cross-references them. QS-1 and QS-2 are filed in
coghex/quruntul.
Rejected: filing all slices under the quruntul epic, where Synarchy PRs could
only reference issues in another repository.

### D-7. A shakedown runs every applicable suite; clean means zero failures

The owner chose this on 2026-09-30 (resolves Q-1). A shakedown runs every
suite that applies on this platform, desktop suites included, under the
standing desktop approval and one window-opening suite at a time. A suite is
clean only when all three hold:
- it built;
- every test it listed reported a result;
- none failed.

A failing test makes its suite non-clean, and its observation names the
harness as the first suspect.
Rejected:
- skipping desktop suites unless targeted;
- counting built-and-reported as clean, with failures merely listed.

### D-8. "Drained" means assessed and decided, checked as QS-6's precondition

The owner chose this on 2026-09-30 (resolves Q-9). The legacy store is
drained when both hold:
- every observation has been assessed;
- every proposal is rejected, designed or implemented.

The 3 accepted proposals must be carried through the legacy proposal lane or
closed first. The import refuses a store that fails this bar and names the
open items. The drain is not its own slice: QS-6 verifies the bar before
importing.
Rejected:
- requiring only observations to be assessed;
- a dedicated drain slice.

### D-9. Every unmatched registry target is archived

The owner chose this on 2026-09-30 (resolves Q-5). The 42 targets that name
no current suite are imported as archived history, attached to no suite. They
are:
- 19 `playtest:`;
- 14 renamed or retired `probe:` targets;
- 9 one-offs: `gameplay:`, `visual:`, `diagnostic:`, `manual:` and
  `graphics:`.

Nothing is dropped, and no alias map is kept. Archived runs count toward no
suite's freshness.
Rejected:
- mapping renamed probes to current suites through an adapter alias table;
- leaving playtest runs in the legacy store.

### D-10. The headless suite's trial count is decided after QS-4 measures it

The owner chose this on 2026-09-30. Q-6 stays deliberately open until QS-4
has measured one trial of `synarchy-test-headless`. QS-4 reports the measured
duration and the projected seeding time at 10 trials, then stops and asks the
owner to choose:
- 10 trials, the rule for every other suite;
- or a per-suite reduction, which would need an engine change filed
  separately.

Rejected:
- fixing 10 trials now;
- adding a per-suite trial count to the engine before any measurement.

### D-11. Housekeeping lands outside this epic

The owner chose this on 2026-09-30 (resolves Q-8). The following land as one
standalone quruntul PR, not as a slice here:
- the engine 0.3.0 bump (the `failing` status and the parser fix);
- the README's stale adapter references;
- discarding the stale `docs-wip` README edit.

### D-12. A shakedown writes nothing about tests or suites to the ledger

The owner chose this on 2026-09-30, while QS-1 was processed. In the flake
lane, enumeration at the upstream head (`State.enumerated`) adds new tests,
retires vanished ones, re-queues `pending` and `failing` tests when a suite's
identity changes, and records the suite's identity. A shakedown does none of
that. It reads the ledger, compares its own enumeration against it, and
reports the differences: new tests, and ledger tests no longer listed. The
next `$flake` enumeration records them. The shakedown records only its own
run, report and observations.
Rejected:
- adding new tests as `new` while retiring and re-queueing nothing;
- recording enumeration exactly as the flake lane does, which would retire
  and re-queue tests and so change status.

### D-13. A shakedown runs only at the upstream head

The owner chose this on 2026-09-30, while QS-1 was processed. It takes no
`--ref`.
Rejected: accepting a candidate `--ref`, harmless since a shakedown changes
no status, so that an adapter fix could be shaken down before its pull
request merges.
Consequence: QS-3's adapter fixes are verified by a shakedown after they
merge. Each fix's own pull request relies on its adapter checks and on
focused runs.

### D-14. An import runs only when the owner runs its command

The owner chose this on 2026-09-30, while QS-2 was processed. A dedicated
quruntul command runs the import once and reports what it imported, archived
and refused. Running it again changes nothing. No lane imports on its own.
Rejected: importing automatically whenever the adapter's hook has unimported
history, for example at the start of `$test` or `$flake`.

### D-15. Named evidence is copied or referenced exactly as the adapter declares

The owner chose copying on 2026-09-30, while QS-2 was processed, and refined
it the same day.

- **A finite manifest.** For each imported run and assessment, the hook
  returns an explicit list of files. Each entry gives:
  - its source path;
  - its role: report, log, assessment document, or other evidence such as an
    image or a small artifact;
  - its declared size;
  - whether it is `copy` or `reference`.

  The engine never discovers files by walking directories and applies no byte
  threshold of its own. Copy work is bounded by the manifest's declared sizes
  and their total.
- **Copies.** Reports, and whichever run logs and assessment documents exist,
  are declared `copy`, and the adapter may declare any other file `copy` too.
  The engine copies every `copy` entry into the matching quruntul run or
  assessment evidence and records its source path, destination and SHA-256 in
  provenance. Copied evidence survives `quruntul export`. The whole import is
  refused, naming the file and the reason, when a promised copy is:
  - missing or unreadable;
  - not a regular file;
  - a different size from its declaration;
  - changed while being read.

  A promised copy is never truncated, omitted or downgraded to a reference.
- **References.** A `reference` entry is recorded with its source path and
  provenance only. The import's report and the run's provenance mark it as
  excluded from portable export, and no run with a reference is described as
  self-contained.
- **Preservation.** Source files, the legacy store, the existing ledger and
  all existing evidence are never modified or deleted. An import that refuses
  or fails, including partway through staging or publication, leaves no new
  visible ledger rows and no imported evidence files. Recovering from an
  interrupted import never deletes pre-existing evidence or resets the
  ledger.
- **Implementation details:** streaming, regular-file and path validation, a
  space check before copying, and detecting a source that changes during the
  import.

Rejected:
- recording the legacy report path as provenance only;
- an engine-chosen size threshold that could turn a named file into a
  reference or leave it out.

### D-16. An import is all-or-nothing, idempotent per source record, and refuses conflicts

The owner chose refusal on 2026-09-30, while QS-2 was processed, and refined
it the same day.

- **Source identity.** Every imported record carries a namespaced identity,
  and the ledger keeps it. The identity is the stable source store, the
  record kind, and the source record id: for example Synarchy's `codex-test`
  registry, `run`, and a legacy run id. A bare legacy id is never an
  identity. Imported records are runs, observations, assessments, proposals
  and evidence files.
- **Idempotence.** Only one thing is exempt: a record whose identity was
  already imported with identical content and evidence, meaning the same
  copied SHA-256s. It is a no-op. A record whose content or evidence changed
  since it was imported is refused. Repeating an identical source is a no-op
  even after later legitimate ledger activity. Only genuinely new records are
  checked for freshness conflicts and applied to freshness. This is
  collision-proof idempotence, not a model of archived and current
  records.
- **Freshness.** For each matched suite, only its genuinely new runs count.
  `last_test_run` becomes the newest one's completion time, and
  `last_test_identity` the suite's identity at import time (D-4). Several runs
  of one suite contribute only their newest.
- **Conflicts.** These are checked among genuinely new records, and each one
  refuses the whole import by name:
  - two records in the supplied input with the same identity, even when a
    record with that identity was imported before;
  - a matched suite whose ledger `last_test_run` is later than, or equal to,
    its newest new run's completion time, compared as UTC instants. A later
    time would move freshness backwards, and an equal one cannot justify
    changing `last_test_identity` (D-17);
  - an imported proposal whose target already has a quruntul proposal
    imported from another source record or created in quruntul. The ledger
    keeps its existing limit of one proposal per target.
- **Atomicity.** Any conflict, any D-15 validation or copy failure, and any
  open item under D-8 refuse the whole import, and nothing is written.
- **Proposals.** There is no archived-versus-current split. An imported
  decided proposal occupies its target as any proposal does, so a later
  `quruntul propose` for that target returns it.

Rejected:
- letting the ledger win, which skips and lists conflicting history;
- letting the import overwrite the ledger.

### D-17. An equal freshness time is a conflict

The owner accepted this on 2026-09-30 (resolves Q-10). When a matched suite's
ledger `last_test_run` equals the completion time of its newest genuinely
new imported run, as UTC instants, the import is refused as a conflict. An
equal time cannot tell the engine whether it is the same observation, so it
cannot justify replacing `last_test_identity`. Identical, previously
imported records stay no-ops even after later ledger activity (D-16).
Rejected: applying an equal time, which keeps the time but replaces the
identity.

### D-18. References are disclosed in the import report and per-record provenance

The owner accepted this on 2026-09-30 (resolves Q-11). Evidence that is only
referenced, not copied, is disclosed as excluded from portable export in two
places: the import's report and each record's provenance. Both are part of
every export, since the report is in the run's evidence and the provenance
is in the ledger. `quruntul export` gains no additional manifest. Tests
verify that the disclosure is present in an exported report and ledger, and
that nothing claims referenced bytes were copied.
Rejected: an additional export manifest of excluded references.

## Open questions

### Q-1. Which suites does a shakedown run, and what counts as clean?

Resolved by D-7 (every applicable suite, desktop included; clean means
built, all reported, none failed).

### Q-2. Does a failed shakedown gate `$flake`?

Resolved by D-3 (advisory only).

### Q-3. Do imported legacy runs count toward `$test` freshness?

Resolved by D-4 (they count, at the suite's current identity).

### Q-4. What happens to the legacy open observations, assessments and proposals?

Resolved by D-5 (drain them through the legacy path first, then import only
closed history).

### Q-5. What becomes of the 42 unmatched registry targets?

Resolved by D-9 (all archived, attached to no suite).

### Q-6. What seeding cost is acceptable for `synarchy-test-headless`?

Deliberately open (D-10). At 10 trials and 400 tests per slice, its 10,526
examples are about 27 batches, and one trial's duration is unmeasured. QS-4
measures it, reports the projected seeding time, and stops for the owner's
choice between 10 trials and a per-suite reduction. QS-5 does not start
before that choice is recorded.

### Q-7. Where are the Synarchy-side slices tracked?

Resolved by D-6 (in coghex/synarchy, cross-referenced from the quruntul
epic).

### Q-8. Is the housekeeping outside this epic?

Resolved by D-11 (a standalone quruntul PR).

### Q-9. What does "drained" mean, and is the drain tracked?

Resolved by D-8 (assessed and decided, checked by QS-6 as a precondition;
the import refuses a store that fails it).

### Q-10. Does an equal freshness time conflict?

Resolved by D-17 (it is a conflict and refuses the import).

### Q-11. Where are excluded references disclosed?

Resolved by D-18 (in the import report and per-record provenance, both
exported; no additional export manifest).

## Verification strategy

- **Engine:** fixture-driven lab tests in `tests/test_lab.py`, in the style of
  the existing flake-lane tests, covering:
  - a shakedown that catches a build failure, an unreported example and a
    failing example, each as one observation, with no status change;
  - an import that is idempotent, keeps provenance, and attaches matched and
    archives unmatched targets.
- **Hetoimasia regression:** a shakedown of Hetoimasia at the current head
  reports every suite clean, because its ledger is fully seeded and stable.
- **Synarchy:** the shakedown report after QS-3; the ledger's imported run and
  observation counts, which should match the registry's 359 runs and 83
  observation-bearing runs; and `$flake` reporting `no-candidate` after QS-5.

## Delivery plan

> **Moved (2026-10-02).** QS-3, QS-4, QS-5 and QS-6 are Synarchy-side slices
> (D-6). They are processed from coghex/synarchy's
> `docs/designs/quruntul_onboarding_design.md`, not from this ledger.

### QS-1. Add a one-trial shakedown lane that proves an adapter launches every suite

- **Outcome:** `quruntul shakedown` runs one trial per suite through the
  flake lane's launch path and reports build, enumeration,
  unreported-example and failure problems as observations, without changing
  any status.
- **Scope:** the engine lane, the CLI command, the report, `docs/design.md`,
  and skill guidance for when to run it.
- **Phase:** 1
- **Depends on:** none
- **Ordering:** critical path; can land first
- **Relevant decisions:** D-1, D-3, D-6 (filed in coghex/quruntul), D-7, D-12, D-13
- **Acceptance signals:** the lab tests above; a clean shakedown of
  Hetoimasia.
- **Out of scope:** gating flake selection (D-3).
- **Open questions:** None

### QS-2. Import a repository's legacy `$test` history into the ledger through an adapter hook

- **Outcome:** an adapter can supply legacy `$test` history, and the engine
  imports it idempotently with provenance, attaching matched targets to
  suites and archiving the rest.
- **Scope:** the hook contract, the import step, its ledger representation,
  and `docs/design.md`.
- **Phase:** 1
- **Depends on:** none
- **Ordering:** independent (parallel with QS-1)
- **Relevant decisions:** D-2, D-4, D-5, D-6 (filed in coghex/quruntul), D-8, D-9, D-14, D-15, D-16, D-17, D-18
- **Acceptance signals:** fixture tests for:
  - identities namespaced by source store, record kind and source record id;
  - idempotence, including after later ledger activity;
  - refusal of a changed record under an imported identity;
  - refusal of duplicate identities in the input, even when previously
    imported;
  - provenance;
  - matched and archived targets;
  - freshness from the newest new run at the current identity;
  - refusal of later or equal ledger freshness (D-17);
  - only closed history imported;
  - `copy` evidence with SHA-256 carried by export;
  - `reference` evidence disclosed in the exported report and ledger, with
    nothing claiming its bytes were copied (D-18);
  - refusal for a missing, changed or wrongly sized promised copy;
  - refusal for a store with open items or a proposal collision;
  - no rows or files left after a refusal or an interrupted import.
- **Out of scope:** reading any specific legacy format; profiles; importing
  open observations or proposals (D-5).
- **Open questions:** None
