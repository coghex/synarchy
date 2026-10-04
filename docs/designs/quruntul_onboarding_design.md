# Quruntul onboarding design

This document is how Synarchy tracks its side of quruntul onboarding: the four
Synarchy slices of the quruntul epic
[coghex/quruntul#5](https://github.com/coghex/quruntul/issues/5). It exists
because Synarchy's work is tracked by Synarchy issues (quruntul decision
D-6), and a design document's processing ledger can only record issues in its
own repository. Those issues close only once their post-merge evidence is
verified, and pull requests reference them without closing them (D-3). It is a processing mechanism for scope already
designed and approved in quruntul. It reverses nothing and adds no scope.

The generic engine work stays in quruntul: the shakedown lane and the
legacy-history import. Any per-suite trial-count engine feature likewise stays
quruntul work, filed separately, and only after measurement and an explicit
owner choice (quruntul D-10). Synarchy's part is its adapter
(`.quruntul/`), running the lab against Synarchy, and assessing what the lab
finds.

Design state: `ready for issue processing`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [x] EPIC. Onboard Synarchy onto quruntul (local tracking epic linking coghex/quruntul#5; D-2) — [#2777]
- [x] QS-6. Read Synarchy's `codex-test` registry through the adapter's legacy-history hook — [#2785]
- [x] QS-3. Shake down every Synarchy suite and repair its adapter — [#2786]
- [x] QS-4. Size `synarchy-test-headless`'s flake slices from a measured trial — [#2787]
- [x] QS-5. Seed Synarchy's flake ledger — [#2788]

## Identifiers and authorities

- **Slice IDs** `QS-3` to `QS-6` are the stable IDs from the quruntul design.
  They are not renumbered here, and QS-1 and QS-2 are quruntul slices that do
  not appear in this ledger.
- **Decisions and questions** of this document are `D-N` and `Q-N`. The
  quruntul design's are always written qualified, as "quruntul D-N", meaning
  `docs/designs/shakedown_synarchy_design.md` in coghex/quruntul.
- **External prerequisites** are named by canonical identity: an issue or
  pull request as `coghex/<repo>#N`, a commit by full repository and SHA. A
  bare `#N` in this document means a coghex/synarchy issue. The local
  tracking epic is #2777; no child issue exists yet, and none is implied.
- **Authority for the engine contracts.** This document summarises the
  quruntul contracts only for orientation. When they disagree, the
  authorities win, and a solver re-reads them, never this summary:
  - For the legacy-history import: the body of
    [coghex/quruntul#8](https://github.com/coghex/quruntul/issues/8), its
    canonical review amendments (the `issue-review:v2` comment of
    2026-09-30, verdict APPROVE), and `docs/design.md` in coghex/quruntul at
    `0269675` (the commit that merged coghex/quruntul#8 through
    coghex/quruntul#9) or later.
  - For the shakedown lane: coghex/quruntul#6 as merged by
    coghex/quruntul#7 at `b2e2a52a35b7de8a591b718d720c4fe941b1c7f6`, and
    quruntul's `docs/design.md` at that commit or later.
- **Evidence figures** below are dated snapshots, supporting context only.
  They are not acceptance truth: a solver re-reads the current registry,
  ledger, adapter and upstream revision before relying on any count. The
  `docs-wip` worktree is not upstream: operational evidence pins the real
  upstream revision it ran at, and the census provenance it used.

## Epic contract

- **Goal:** Synarchy's side of coghex/quruntul#5 is delivered:
  - its adapter launches every applicable suite cleanly under a shakedown;
  - its quruntul ledger holds the legacy `$test` history's closed records;
  - it has a first flake measurement whose coverage is reconciled against
    the suites the adapter declares, with every observation the onboarding
    raised assessed.
- **Done when:**
  - QS-3, QS-4, QS-5 and QS-6 each reach their completion signal under the
    delivery lifecycle below. Each is recorded on its own coghex/synarchy
    issue, owned by the local tracking epic (D-2), and closed by the owner
    once verified (D-3);
  - coghex/quruntul#5 references those issues through the handoff sequence
    in Q-4.
- **Users and operators:** the owner, and the Codex and Claude agents that
  run `$flake`, `$test`, `$deflake` and `$assess-tests` in Synarchy.
- **Arc label:** None proposed.

## Current state and evidence

Snapshots taken on 2026-09-30 (the ledger at 14:52Z). Re-read everything
before relying on it.

- **Upstream.** `origin/master` was at `9a274311de75`, and this document's
  `docs-wip` worktree at `313657b59b9d`.
- **Adapter.** `.quruntul/adapter.py` (coghex/synarchy#2760, merged) declares
  102 suites:
  - `synarchy-test-headless`: CI, Hspec, `batch_tests = 400` (`HSPEC_SLICE`);
  - `synarchy-test-graphical`: a probe and a desktop suite; CI only compiles
    it, and it needs a display;
  - every registered probe the census does not defer, as a `command` or
    `exit` suite. `audio_manual` is left out (`DIRECT_ONLY`), because it needs
    a mode flag on a direct invocation.

  It has `flake_trials = 10`, `refresh_days = 7` and a `seed` hook fed by
  `docs/probe_census.json`. Its checks are `python3 .quruntul/checks.py`,
  which cover suites, results and seeding, not an import hook, which does not
  exist yet.
- **Ledger** (`<git-common-dir>/quruntul/ledger.sqlite3`):
  - 102 suite rows, of which 4 had tests enumerated;
  - 10,538 tests: 10,528 `new` and 10 `stable`, the latter seeded from the
    census;
  - 9 flake runs (8 complete, 1 blocked), and 1 open observation;
  - no `$test`-lane runs.
- **Legacy registry** (`<git-common-dir>/codex-test/`, schema
  `codex-test-coordinator/v1`, `updated_at` 2026-09-30T09:06:29Z):
  - 359 completed coordinator runs over 127 targets. Each has a report, a
    log and a completion time; execution status is `passed`, `failed` or
    `cancelled`.
  - 309 runs (85 targets) name a current suite. 42 targets name none.
  - Its assessment registry lists 15 assessments, all `completed`, with
    source-approval fields.
  - 4 proposals: 1 `rejected`, and 3 `accepted`. The proposal-design registry
    has a `completed` design naming each accepted proposal's id.
  - 633 artifact files (145 MB).
  - The number of observations not yet assessed was not computed.

  A recent `updated_at` shows neither current use nor that the store is
  drained.
- **Engine.**
  - The shakedown lane exists: coghex/quruntul#6, merged by
    coghex/quruntul#7.
  - The legacy-history import is merged (coghex/quruntul#8, merged by
    coghex/quruntul#9 on 2026-10-01 at `0269675`). Its merged hook shape and
    import-report schema are facts for QS-6 to read from the merged contract.
  - quruntul is at engine version 0.4.0 (re-read 2026-10-02).
- **Related Synarchy work.** coghex/synarchy#2743 measures real Linux CI
  runs to partition CI lanes. It is not QS-4, which sizes local quruntul
  batches; its figures come from another platform and runner.
- **Umbrella overlap check** (2026-09-30, read-only, coghex/synarchy, run at
  readiness). This was not an exhaustive search.
  - **Scope:** open issues labelled `epic`, of which 13 were listed, plus
    searches across all states for "quruntul", "shakedown", "flake lab
    onboarding", "flake ledger", "codex-test registry" and "legacy test
    history", each capped at 6 results.
  - **No results:** "quruntul", "shakedown" and "flake lab onboarding".
  - **Unrelated matches:** "flake ledger" matched closed probe-runner and
    atlas work (#1571, #1436, #1570, #2168, #2130, #1256). The other two
    searches matched open scenario, save, fluid, structure and flora work
    (#2700, #2649, #2548, #2717, #2721, #2735, #2722, #2519, #2707, #2547,
    #2557).
  - **Closest open epic:** #2742, which brings pull-request CI under twenty
    minutes. #2743 above belongs to that CI lane-partition effort, as the
    first independent review of this document found.

  None tracks quruntul onboarding, so the local tracking epic (D-2) duplicates
  no Synarchy epic. Its umbrella is the explicitly linked coghex/quruntul#5.
  Final per-child deduplication remains each processing run's job.

## Desired experience

The owner asks for each slice in turn:
- QS-6 brings the legacy `$test` history into the ledger once, after the old
  registry is drained;
- QS-3 shows every suite launching cleanly, with each failure explained and
  owned;
- QS-4 presents the measured cost of seeding the headless suite and waits for
  the owner's choice;
- QS-5 seeds the ledger, reconciles what was and wasn't measured, and ends
  with every onboarding observation assessed.

## Scope

### In scope

- Synarchy's adapter changes under `.quruntul/` needed for these slices, with
  their checks.
- Running quruntul's shakedown, import and flake lanes against Synarchy, when
  the owner asks.
- Assessing the observations those runs raise.

### Out of scope

- Any generic engine change: the shakedown lane, the import, a per-suite
  trial count, or anything else in `quruntul/`. Engine defects found here are
  reported to coghex/quruntul.
- Candidate-revision shakedowns or imports: both run at the upstream head
  only (quruntul D-13; coghex/quruntul#8 review).
- Product fixes and flaky-test fixes. They go through `$assess-tests` issues
  and `$deflake`, each separately.
- New Synarchy features, the `codex-profile` lab, and `$playtest`.
- Editing quruntul's skills. If the legacy `$assess-tests` route needs a
  follow-up once the import is complete (see QS-6), that is coghex/quruntul
  work.

## Design

This is orientation only; the authorities above govern.

### Delivery lifecycle

QS-6 changes code, as does QS-3 when it finds adapter defects to repair.
Both then need a real operation that can only run once that code is
upstream. The shakedown runs only at the upstream head
(quruntul D-13), and the import resolves adapter declarations from one pinned
upstream revision (coghex/quruntul#8 review). QS-4 and QS-5 may change no
code at all. Each slice therefore has two stages:

- **Pre-merge**, in the slice's pull request:
  - the adapter change;
  - `python3 .quruntul/checks.py`, plus the adapter's own fixtures, such as
    temporary-history fixtures for QS-6;
  - focused evidence where the lanes support it;
  - the documentation the change requires.

  Required measurement notes, mappings, verdicts and owner choices go in that
  same pull request whenever it exists.
- **Upstream-only**, after the merge: the real operation at the upstream head
  (the shakedown, the import, or the flake seed), with its evidence recorded
  on the slice's issue.

Each issue stays open until its upstream-only evidence is verified; its pull
request references it without closing it (D-3). No slice's pull request
claims an operation it cannot yet have run.

### Durable evidence

A slice's durable record is a short summary: the upstream revision, the
platform, the commands, the quruntul run and observation ids, and the
outcome. It cites the local lab evidence, and never commits the SQLite
ledger or copies a run directory into the repository.

### QS-6: importing the legacy registry

- The adapter implements coghex/quruntul#8's hook to read the `codex-test`
  registry. It supplies records with stable, namespaced identities: source
  store, record kind and record id, independent of machine paths.
- **Evidence.**
  - Every run's report, and every run log and assessment document that
    exists, is declared `copy`.
  - A log or document that never existed is declared absent explicitly. An
    existing file is never declared absent.
  - A promised file that is missing refuses the whole import.
  - Artifacts are declared `copy` or `reference` file by file.
  - Unmatched targets are archived, with no alias remapping and no
    freshness.
- **Adapter-facing consequences of coghex/quruntul#8.** These are already in
  its approved contract; they are listed so the adapter's fixtures cover
  them, not as new engine work.
  - A `completed` legacy assessment is imported as approved only with actual
    source approval. An assessment or proposal Markdown file is not, by
    itself, an approved assessment.
  - A legacy `accepted` proposal is normalized to a decided status only when
    a `completed` design names its proposal id. The raw proposal still reads
    `accepted`.
  - The adapter documents how every legacy status maps to terminal and
    approved.
  - Identity and content are stable across repeated calls and machine paths.
    Engine-generated timestamps, local destinations and later suite matching
    are not source-content changes, and identical records keep their
    original attachment and identity snapshot.
  - Identical records already imported, proposals included, are no-ops on a
    repeated import. A proposal-target conflict arises only between distinct
    proposal identities sharing a target: a genuinely new proposal against
    the ledger, or two within the supplied batch. It refuses the import, as
    do missing mandatory evidence declarations and changed source content or
    manifests under an imported identity. None of these is ever skipped.
  - The import prepares, enumerates, seeds and executes nothing, and
    recovers no unrelated native run. The durable ledger commit is its
    success boundary: leftovers from before the commit stay invisible to
    readers and export. A refused or pre-commit interrupted import leaves no
    durable schema migration behind. An interruption after the durable
    commit preserves the complete import and its committed migration.
- **Portability.** A run with a `reference` is not self-contained. Copied
  assessment Markdown may still link to absolute external paths, and copying
  a file doesn't copy what it links to. The import discloses those external
  dependencies; nothing copies them recursively.
- **When it runs.** The import runs only through quruntul's command, only
  after coghex/quruntul#8 has merged and the hook is upstream, and only once
  the registry meets the drain bar (quruntul D-8). Just before importing, the
  solver checks for source changes and active coordinator claims. The
  registry is never modified.
- **After it completes.** quruntul D-5 says `$assess-tests` keeps its legacy
  Synarchy route until the import is done. QS-6 records that the import is
  complete, and then determines whether the skill's routing needs any
  change. Its legacy path already depends on there being unassessed legacy
  observations. Any change needed goes to coghex/quruntul, not this pull
  request.

### QS-3: shaking down every suite

A shakedown of every applicable Synarchy suite at the upstream head. It is
advisory, and it leaves test and suite state unchanged while recording its
own run, report and observations (quruntul D-3, D-12, D-13). It includes
desktop suites under the carried standing lab approval, one window-opening
suite at a time (quruntul D-7; see Q-2).

- **Clean** means the suite built, every listed test reported, and none
  failed (quruntul D-7). Hspec `pending` and command `unproven` results in a
  clean suite are recorded as reported, not as passed.
- **Reconciliation.** Every suite the adapter declares at the pinned revision
  is reconciled against the report, keeping every problem the engine
  records:
  - build failed;
  - enumeration failed;
  - failed tests;
  - unreported results;
  - incomplete execution;
  - never run: `busy` or `not-run`.

  A `busy` or `not-run` suite is incomplete work, not coverage. Declared
  exclusions stay disclosed exclusions: the engine's `skipped` results, with
  their reasons (platform, or a ledger deferral made with `quruntul defer`),
  a census deferral, or `DIRECT_ONLY`. A later attempt may measure unreached suites, but it
  never rewrites finished evidence or replays a retained trial.
- **Dispositions** for each non-clean suite:
  - an adapter defect, repaired in the QS-3 pull request with evidence;
  - an engine defect, reported to coghex/quruntul;
  - a product failure, assessed and filed separately;
  - a verified environment blocker.

  An engine or environment defect that prevents trustworthy measurement
  carries a checkable follow-up or resume condition. Filing it doesn't show
  that the suite launched correctly.

### QS-4: sizing the headless suite

- **Order.** QS-4 keeps the approved order: first one measured headless
  trial, then the projection and the owner's choice, and only then a
  complete validating batch at the chosen count. It is not an open-ended
  profiling campaign.
- **Measure, don't extrapolate.** Per-process start-up and world-generation
  fixtures can dominate a small selection, so one whole-suite trial is not a
  linear per-example cost.
- **What it records:**
  - the upstream revision, toolchain and platform;
  - the commands and quruntul run ids;
  - build and enumeration time, kept apart from trial time;
  - selected example counts;
  - the slice estimate and its uncertainty.
- **The batch model.** `batch_seconds` bounds the trial loop, which starts
  another trial only when a full `trial_seconds` still fits; preparation and
  enumeration come earlier.
- **The projection** of seeding at 10 trials includes the slice count and the
  start-up cost every trial repeats.
- **Stop point.** QS-4 then stops for the owner's choice between 10 trials and
  a per-suite reduction (quruntul D-10). It never lowers the global
  `flake_trials`, and never substitutes the existing per-command trial
  override for that decision.
- **Validation** uses a complete batch at the chosen count, with every
  planned trial and selected result accounted for. A short elapsed time from
  a batch that stopped early doesn't show the batch fits.
- **Existing figures,** such as coghex/synarchy#2743's CI measurements, are
  cited with their platform and runner differences, never treated as local
  measurements. Its lane-partition work is not repeated.

### QS-5: seeding the ledger

QS-5 runs `$flake` to `no-candidate` and `$assess-tests` on what it raised,
only after QS-3, QS-4 and the owner's choice. No fixes are bundled.

`no-candidate` alone doesn't certify completion. Flake selection skips
deferred, platform-inapplicable, suite-claimed and desktop-claimed suites
before considering them, and still reports `no-candidate`. So QS-5 adds a
coverage reconciliation:
- the pinned revision, adapter identity and platform;
- the declared suites and their applicable tests;
- measurements, or census evidence inherited through the `seed` hook;
- intentional exclusions, with their reasons and resume conditions;
- any work remaining `new`.

A transient `busy` or claim is never completion. `pending`, `failing` and
`flaky` stay distinct from `stable`. An approved assessment doesn't mean a
product fix landed.

## Decisions

### D-1. Synarchy's quruntul slices are processed from this document

The owner chose this on 2026-09-30, while processing quruntul QS-6: the
recommended option, "track the Synarchy slices in Synarchy's own design". It
implements quruntul D-6 and neither reverses nor extends it: QS-3 to QS-6
are coghex/synarchy issues, and coghex/quruntul#5 references them. Rejected:
- filing them in coghex/quruntul, which reverses D-6;
- hand-filing them in Synarchy outside the transaction helpers, which gives
  up recording that survives a crash.

On 2026-10-02 the owner had the four slices removed from the quruntul
design's ledger and delivery plan, leaving a pointer to this document, rather
than recording them there as `[no-issue]` (Q-4).

### D-2. A small Synarchy tracking epic owns the four children

The owner chose this on 2026-09-30 ("a tracking epic is fine"; resolves Q-1).
This document's EPIC entry becomes a small coghex/synarchy tracking epic,
filed through the unchanged `/process-design-doc` workflow. It owns exactly
the four local children, QS-6, QS-3, QS-4 and QS-5, and links
coghex/quruntul#5 as the umbrella for the whole arc. It claims no second
product arc and adds no scope; its checklist lists only the four children.
Rejected: marking EPIC `[no-issue]`, which would need a bounded exception to
the installed processing workflow.

### D-3. Issues stay open until their post-merge evidence is verified

The owner chose this on 2026-09-30 ("issues can stay open until post merge is
verified"; resolves Q-3). This is the bounded post-merge completion gate.
- **The pull request** carries everything required before merge: the code,
  the adapter checks and fixtures, the documentation the change requires,
  and any measurement notes, mappings, verdicts or owner choices recorded
  so far. It references its issue without a closing keyword, so merging
  doesn't close the issue.
- **After the merge,** the solver runs the slice's upstream-only operation at
  the upstream head and records its durable summary on the issue, naming
  the evidence.
- **The owner** closes the issue once that evidence is verified.
- **A slice that changes no code,** such as QS-5, or QS-3 or QS-4 when no
  adapter change is needed, uses the same gate with no pull request: its
  evidence is recorded on the issue, and the owner closes it.

Rejected:
- splitting each post-merge operation into its own operational issue;
- landing the evidence as a standalone docs pull request, which would be an
  exception to Synarchy's docs landing lane.

Execution note: a workflow that adds a closing reference by default must use
a non-closing reference for these issues instead.

### Carried from the quruntul design

These bind the slices here as decided there; the full text is in that
document:

| Decision | What it settles |
|---|---|
| D-2 | The legacy data to port is the `codex-test` registry. |
| D-3 | A shakedown is advisory and never gates `$flake`. |
| D-4 | Imported runs count toward `$test` freshness, at the suite's current identity. |
| D-5 | Drain the legacy open items first, then import only closed history; `$assess-tests` keeps its legacy route until the import is done. |
| D-6 | Synarchy's slices are tracked in coghex/synarchy. |
| D-7 | A shakedown runs every applicable suite, desktop suites included under the standing lab approval, one window-opening suite at a time; clean means built, every listed test reported, none failed. |
| D-8 | "Drained" means every observation assessed and every proposal rejected, designed or implemented. |
| D-9 | Every unmatched registry target is archived. |
| D-10 | The headless suite's trial count is chosen by the owner after QS-4 measures. |
| D-12 | A shakedown leaves test and suite state unchanged, and records only its own run, report and observations. |
| D-13 | A shakedown runs only at the upstream head; adapter fixes are confirmed by a shakedown after they merge. |
| D-14 to D-18 | The import's explicit command, named copy-or-reference evidence, all-or-nothing idempotence and conflicts, equal-time conflict, and disclosure of references. |

For the import, D-14 to D-18 are superseded wherever coghex/quruntul#8's body
or its review amendments say more.

## Open questions

### Q-1. Which tracking mechanism does this document use?

Resolved by D-2: a small Synarchy tracking epic that owns the four children
and links coghex/quruntul#5. The installed `/process-design-doc` workflow
selects children only after a local `[#N]` EPIC exists, and its transaction
helper cannot adopt coghex/quruntul#5.

### Q-2. Which approval covers desktop suites?

Resolved as carried: quruntul D-7's standing lab desktop approval covers
Synarchy's applicable desktop suites, `synarchy-test-graphical` among them,
in shakedowns and flake batches. Its scope:
- the consent the adapter supplies;
- the lab's `desktop` claim;
- advance notice that windows will appear;
- one window-opening suite at a time.

It covers the adapter's direct launch of the Hspec executable. It is not
general permission for an ordinary game launch, which opens a window:
Synarchy's launch rules forbid that apart from sprite preview. The game's
`--dump`, `--headless` and `--offscreen` modes stay permitted under those
same rules, and the probes these slices run use them. Running any of it
still needs the owner's request for that slice.

If a suite turns out to launch something outside this scope, the slice stops
and asks about that specific command.

### Q-3. How does a slice complete when its outcome is operational?

Resolved by D-3: each issue stays open until its upstream-only evidence is
verified. Its pull request references the issue without closing it and
carries all pre-merge evidence, and the owner closes the issue.

### Q-4. What is the cross-repository handoff sequence?

Each repository's transactions are local: creating an issue in Synarchy
doesn't update coghex/quruntul#5 or the quruntul design. The sequence, as it
stands on 2026-10-02:
1. The owner approved this document as ready.
2. The local tracking epic (D-2), coghex/synarchy#2777, is filed and confirmed.
   Each Synarchy child is filed and confirmed here, one approved artifact at a
   time.
3. The quruntul design's four ledger lines for these slices were deleted by the
   owner's choice on 2026-10-02, with a pointer to this document. That replaces
   the earlier proposal to mark them `[no-issue]: tracked in
   coghex/synarchy#N`. The quruntul design's own record of the move is its
   pointer note.
4. As a separate approved step in coghex/quruntul, coghex/quruntul#5's
   checklist names each Synarchy child's full URL once it exists.

A partially completed cross-reference is reconciled before either cursor
advances. Neither tracker's labels nor either durable cursor is ever
hand-edited as a shortcut. Filing here never changes coghex/quruntul#5 by
itself.

## Verification strategy

- **Adapter changes:** `python3 .quruntul/checks.py`, the adapter's own
  fixtures, and the tool self-tests in the Testing tiers table of `AGENTS.md`
  that the change touches.
- **Lab runs:** each slice's durable summary, citing quruntul run and
  observation ids at a pinned upstream revision:
  - QS-3: the shakedown reconciliation;
  - QS-4: the measurement and projection;
  - QS-5: the coverage reconciliation and approved assessments.
- **The import:** its receipt, reconciled by record kind against the eligible
  source history at import time rather than against the snapshot above.

## Delivery plan

### QS-6. Read Synarchy's `codex-test` registry through the adapter's legacy-history hook

> Tracked by #2785.

- **Outcome:** Synarchy's ledger holds the legacy registry's closed history,
  imported once through quruntul's command with the adapter's hook.
- **Scope:**
  - the adapter's hook and its fixtures and checks;
  - documenting the legacy-status mapping;
  - the engine-version requirement;
  - after the merge, verifying the drain bar, running the import, and
    recording its durable summary.
- **Phase:** 2 (after quruntul's engine work)
- **Depends on:**
  - coghex/quruntul#8 merged; its merged contract is the authority;
  - for the upstream-only stage, this slice's hook upstream, and the legacy
    registry drained to quruntul D-8's bar, re-verified against the legacy
    coordinators just before importing. That means every legacy observation
    assessed through the legacy `$assess-tests` path, and every proposal
    rejected, designed or implemented.

  No local slice.
- **Ordering:** not on the critical path; independent of QS-3, QS-4 and QS-5
  (flake selection reads no `$test` history).
- **Relevant decisions:** D-1, D-3; quruntul D-2, D-4, D-5, D-8, D-9, D-14 to D-18,
  as superseded by coghex/quruntul#8.
- **Acceptance signals:**
  - **Pre-merge:**
    - `python3 .quruntul/checks.py` passes;
    - hook fixtures cover: normalized terminal statuses; source approval;
      the proposal-to-design join; stable identities; manifest completeness,
      including explicit absence; unmatched targets archived; and refusal of
      open or inconsistent records.
  - **Upstream-only**, the completion signal:
    - a successful import whose imported and archived records reconcile, kind
      by kind, with the eligible source history;
    - a second import that changes nothing;
    - freshness for matched suites as quruntul D-4 and D-17 specify;
    - the drain verification recorded;
    - portability limits disclosed: references, and external links in copied
      files;
    - imported history outside every open queue.
  - A refused import is a blocker with its reason, not a partial success.
- **Out of scope:** draining the registry, which precedes the import; any
  engine change; editing quruntul's skills; `codex-profile`.
- **Open questions:** None. Stop and report if coghex/quruntul#8's merged
  contract cannot represent something the registry holds.

### QS-3. Shake down every Synarchy suite and repair its adapter

> Tracked by #2786.

- **Outcome:** a shakedown of every applicable Synarchy suite at the upstream
  head is clean, or each non-clean suite has a recorded disposition, with
  every declared suite reconciled.
- **Scope:**
  - running quruntul's shakedown at the owner's request. It is a full-suite
    run, which Synarchy's "no full suites by default" rule otherwise
    forbids.
  - evidence-backed adapter repairs;
  - dispositions as in the design above;
  - confirming repairs by a shakedown after they merge (quruntul D-13).
- **Phase:** 2
- **Depends on:** coghex/quruntul#6, merged by coghex/quruntul#7 at
  `b2e2a52a35b7de8a591b718d720c4fe941b1c7f6`, which is already satisfied. No
  local slice.
- **Ordering:** critical path; can land first (it needs nothing from QS-6).
- **Relevant decisions:** D-1, D-3; quruntul D-3, D-6, D-7, D-12, D-13.
- **Acceptance signals:**
  - **Pre-merge:** for any repair, `python3 .quruntul/checks.py` and focused
    evidence.
  - **Upstream-only:** a shakedown reconciliation of every declared suite,
    either clean or dispositioned, that keeps each problem kind separate and
    lists `busy` and `not-run` suites as incomplete. `pending` and `unproven`
    are recorded as not passed, and exclusions are disclosed.
- **Out of scope:** product and flaky-test fixes; engine defects, which are
  reported to coghex/quruntul; gating `$flake`; candidate-revision
  shakedowns.
- **Open questions:** None.

### QS-4. Size `synarchy-test-headless`'s flake slices from a measured trial

> Tracked by #2787.

- **Outcome:**
  - `batch_tests` for `synarchy-test-headless` is set from one measured
    trial;
  - the projected seeding time at 10 trials is reported;
  - the owner's trial-count choice is recorded;
  - a complete batch at the chosen count validates the setting.
- **Scope:** the measurement as the design above describes, the adapter's
  slice size, the recorded choice, and the validating batch.
- **Phase:** 2
- **Depends on:** QS-3
- **Ordering:** critical path
- **Relevant decisions:** D-3; quruntul D-10
- **Acceptance signals:**
  - the recorded measurement and projection, separating build and
    enumeration time from trial time and stating the slice estimate with its
    uncertainty;
  - the owner's choice, recorded;
  - a complete batch at that count, with every planned trial and selected
    result accounted for, finishing within `batch_seconds`.
- **Stop point:** after the projection, stop and ask the owner to choose
  between 10 trials and a per-suite reduction. Choose neither. A reduction is
  a separate coghex/quruntul engine issue, and QS-5 waits until it is
  identified, merged, and usable by the Synarchy adapter.
- **Out of scope:**
  - splitting the Hspec executable;
  - any engine change;
  - lowering the global `flake_trials`;
  - repeating coghex/synarchy#2743's CI lane-partition work.
- **Open questions:** quruntul Q-6 (the trial count), deliberately open until
  the stop point above.

### QS-5. Seed Synarchy's flake ledger

> Tracked by #2788.

- **Outcome:** `$flake` reports `no-candidate` for Synarchy on this platform.
  The coverage reconciliation shows every declared suite either measured,
  seeded from the census, or intentionally excluded with a reason and resume
  condition. Every observation from the onboarding runs has an approved
  assessment.
- **Scope:** running `$flake` to completion at the owner's request, the
  coverage reconciliation, and `$assess-tests` on the observations of the
  onboarding runs.
- **Phase:** 3
- **Depends on:** QS-3, QS-4, and QS-4's recorded owner choice (with any
  engine issue it requires merged in coghex/quruntul).
- **Ordering:** critical path; last
- **Relevant decisions:** D-3; quruntul D-3, D-7, D-10
- **Acceptance signals:**
  - `no-candidate` together with the coverage reconciliation. A `busy` or
    claimed suite never counts as complete.
  - `pending`, `failing` and `flaky` reported apart from `stable`.
  - Approved assessments for every observation raised by the onboarding
    runs. That means the QS-3 shakedown runs, any QS-4 measurement or
    validation runs that emitted observations, and the QS-5 flake runs,
    listed by run id. It is a finite set, not a growing global queue, and no
    product or flaky-test fix is required before an assessment closes.
- **Out of scope:** product fixes, and flaky-test fixes (`$deflake`).
- **Open questions:** None.

## Delivery constraints

These come from Synarchy's `AGENTS.md`; re-read it before working.

- **Worktrees and docs.**
  - Implement in an isolated worktree, with one issue per pull request.
  - Keep the primary checkout clean.
  - Documentation and evidence summaries that accompany code go in that pull
    request. Standalone documentation stays in `docs-wip` until the owner
    asks for it to land.
- **Build lock.** Wait for the `cabal-build` lock in a foreground 60-second
  wake loop for up to 30 minutes. Never bypass the lock or stop another
  owner's process.
- **Launch rules.**
  - Never launch the game normally, meaning an ordinary windowed launch.
    Sprite preview is the only exception; `--dump`, `--headless` and
    `--offscreen` remain allowed.
  - Desktop suites run only under the carried lab approval (Q-2).
  - Use non-8008 ports, and never `pkill -f synarchy`.
- **Full suites.** A shakedown or a flake seed of Synarchy is a full-suite
  run: only on the owner's explicit request for that slice.
- **Concurrency.** Respect quruntul's claims, resource holds, desktop
  consent, the legacy coordinators' claims, and other agents' active work.
  Stop only processes you started.
- **Evidence.**
  - Record a short durable summary citing quruntul run and observation ids
    at a pinned upstream revision.
  - Never commit the SQLite ledger or copy its run directories into the
    repository.
  - Never modify the legacy registry.
