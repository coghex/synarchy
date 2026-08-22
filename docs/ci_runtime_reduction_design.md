# CI runtime reduction design

This design reduces pull-request feedback time without quietly dropping
regression coverage. It is motivated by the 2026-08-15 run for PR #1328, whose
headless Hspec step remained active for more than an hour even though recent
successful runs normally completed the same step in about four minutes. A hang
is something to restart, reproduce, and measure—not justification for adding a
timer to the test suite.

Design state: `ready for issue processing`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [ ] EPIC. Restore fast, bounded CI feedback without weakening regression coverage
- [ ] CIR-1. Publish durable CI timing and selection diagnostics
- [ ] CIR-2. Decode save-compat fixture descriptors once per self-test run
- [ ] CIR-3. Reproduce and localize headless-suite hangs without timers
- [ ] CIR-7. Isolate parallel probes behind one prebuilt executable
- [ ] CIR-4. Rotate the project cache every eight build-relevant master changes
- [ ] CIR-8. Prove and adopt only safe Hspec parallel regions
- [ ] CIR-5. Run post-build gate families on independent critical paths
- [ ] CIR-6. Reassess full-suite and behavior-probe selection from measured coverage

## Epic contract

- **Goal:** Make ordinary pull-request CI give fast, predictable feedback and
  make abnormal runs easier to reproduce and diagnose.
- **Done when:** CI publishes enough timing and selection data to explain its
  critical path; repeated work identified in the save-compat audit is removed;
  a rerun of a hung headless suite provides enough evidence to compare and
  localize the behavior; build-cache age is bounded without unbounded churn;
  parallel probes never mutate one shared Cabal build tree; independent gate
  families no longer serialize behind one another unnecessarily after the
  build; and safe Hspec regions use available concurrency without corrupting
  shared fixtures. Any reduction in which tests run on a PR is
  backed by an explicit path-to-coverage contract and retains a full post-merge
  backstop.
- **Users and operators:** Contributors waiting for PR checks, reviewers deciding
  whether a result is trustworthy, and maintainers diagnosing flakes and hangs.
- **Arc label:** `tooling` proposed

## Current state and evidence

### Verified current state

- `.github/workflows/ci.yml` has one `build-test` job after the image resolver.
  Compilation, the complete headless Hspec suite, every static audit, optional
  unit-asset/worldgen gates, and path-selected behavior probes execute serially
  in that one job. Its job timeout is 90 minutes.
- The full headless suite is deliberately blocking on every pull request. The
  graphical test build, unit-asset gate, world check, and behavior probes are
  already path-selective on pull requests; master runs the complete post-merge
  backstop described in the repository testing contract.
- Recent successful samples show materially different critical paths:
  - [master run 31901107228](https://github.com/coghex/synarchy/actions/runs/31901107228)
    completed in 11m47s. Inside `build-test`, the headless suite took 4m01s,
    save compatibility took 2m20s, world check took 1m32s, and no behavior
    probes ran.
  - [PR run 31898986158](https://github.com/coghex/synarchy/actions/runs/31898986158)
    completed in 24m42s. The changed modules took 2m19s to build, test suites
    took 3m23s to build, Hspec took 4m02s, save compatibility took 2m26s, and
    the selected behavior probes took 9m58s.
  - [PR run 31900657765](https://github.com/coghex/synarchy/actions/runs/31900657765)
    entered `Headless test suite` at 18:22:45 UTC on 2026-08-15 and was still
    in that step more than an hour later. At the user's direction, that attempt
    was cancelled and the same workflow restarted on commit `93958043` so the
    behavior can be measured again before changing the suite.
- The current Hspec layout deliberately shares generated worlds under one
  `aroundAll withHeadlessEngine` group. World generation fell from about 16
  generations / 185 seconds to about six generations / under a minute, so
  naive suite sharding could regress runtime by duplicating those worlds.
- `tools/test_save_compat_audit.py` invokes `sca.audit(...)` 24 times. Each
  synthetic manifest containing a complete-session fixture reaches
  `verify_fixture_descriptors`, whose `dump_fixture_descriptors` starts
  `cabal repl test:synarchy-test-headless`. This repeated real-codec startup
  explains why the CI step documented as a cheap static audit consistently
  costs about 2m20s. The production audit still needs one real fixture decode;
  synthetic unit cases do not need to pay for a fresh Cabal REPL each time.
- The pre-CIR-4 `dist-newstyle` cache key was stable for a dependency plan.
  Because GitHub cache entries are immutable, the first snapshot for a plan was
  frozen and later runs rebuilt every project change since that snapshot. This
  bounded cache count but let incremental compilation drift upward throughout a
  long-lived plan window.
- Behavior probes already have per-probe 900-second timeouts and process-group
  termination in `tools/run_probes.py`. The aggregate runs up to two probes in
  parallel and retries a failure once in isolation. There are currently eleven
  CI-eligible probes; core or unclassified changes select all eleven.
- Every ordinary probe boot still reaches `tools.probelib.boot`, which launches
  `cabal run -v0 exe:synarchy` against the checkout's one `dist-newstyle`.
  Parallel Python processes and distinct debug ports do not isolate Cabal's
  inplace package database. `persistence_contract` additionally launches
  `cabal repl test:synarchy-test-headless` through
  `tools/persistence_snapshot.py`.
- This is a measured failure, not only a source-level risk. In
  [PR run 32491150012](https://github.com/coghex/synarchy/actions/runs/32491150012),
  `cargo_capacity` and `persistence_contract` failed in the parallel batch and
  then passed alone. Their solo retries consumed 92.2 s and 188.2 s: 280.4 s
  (4m40s) added to a probe step whose wall time reached 10m32s. The verified
  shared-build-directory race is also recorded as unprocessed PRR-1 in
  `docs/project_review_534-518.md`.
- A cloned `dist-newstyle` is not a sound isolation primitive. The current
  Cabal plan records absolute `bin-file` paths under the originating checkout,
  and the inplace package database is the exact mutable surface racing today.
  A local warmed build tree is about 1.3 GB, while a source/resource worktree is
  much smaller. Copying that build tree per probe would therefore be both
  path-sensitive and needlessly expensive.
- The current master-scoped project cache was created at
  2026-08-19T23:18:26Z. By the 2026-08-21 measurement, master had advanced 81
  first-parent commits and 106 Haskell paths differed from the snapshot. On
  [fresh-cache master run 32312982306](https://github.com/coghex/synarchy/actions/runs/32312982306),
  the library/executable and test-suite builds took 11 s + 75 s = 1m26s. On
  [master run 32491022029](https://github.com/coghex/synarchy/actions/runs/32491022029),
  the same two stages took 176 s + 280 s = 7m36s. Hspec itself took 232 s in
  both runs, making the roughly six-minute build increase good evidence of
  cache-age drift rather than a generally slower runner.
- The CI image has no scheduled refresh. Its immutable tag is the content hash
  of `.github/ci/Dockerfile` plus `.github/workflows/ci-image.yml`; a tag is
  published only once. The Dockerfile nevertheless resolves moving external
  inputs (`ubuntu:22.04`, `apt-get`, and the ghcup download), and the image
  workflow explicitly notes that identical instructions are not guaranteed to
  rebuild to identical bytes. Image refresh remains content-change-driven: an
  intentional recipe or base refresh must mint a new immutable identity, never
  overwrite the current content tag.
- The checked-in Hspec version is 2.11.17. Its runner supports `--jobs=N`, but
  Hspec only schedules specs explicitly marked with its `parallel` combinator;
  `test-headless/Spec.hs` marks none today. The large shared-world
  `aroundAll withHeadlessEngine` block deliberately relies on sequential
  execution and memoizes generated worlds. Many other groups also own mutable
  engine state, fixed temporary paths, process-wide configuration, or
  repo-relative resources, so adding `parallel` at the root would be unsafe.
- The readiness tracker scan found no existing CI-runtime epic. Open issue
  #1358 directly overlaps CIR-1's cache-outcome reporting and should be reused
  or deduplicated when that slice is processed rather than silently drafting a
  second issue. Open issue #1427 measures concurrency and RTS effects for the
  separate non-CI probe de-flake lab; its evidence can inform CIR-7's worker cap,
  but it does not isolate CI probes from shared Cabal mutation. Issues #1364 and
  #1475 add CI coverage rather than reduce its runtime. No open tracker umbrella
  duplicates this design's combined cache, build-handoff, Hspec, audit, and
  probe-isolation arc.

## Desired experience

1. An ordinary PR should surface its first meaningful failure quickly rather
   than making every gate wait behind unrelated work.
2. A run should state which expensive gates and probes were selected and show
   their elapsed times in a durable summary that remains available after
   cancellation or failure where GitHub permits.
3. When Hspec hangs, restart the run on the same commit and compare the two
   attempts before proposing a CI policy change. More granular, flushed test
   progress may make that comparison useful, but must not impose a timer.
4. A cache hit should have an observable age. Cache reuse must not silently
   turn into ever-growing recompilation work merely because the dependency plan
   has not changed for weeks.
5. Master retains a complete regression backstop. Any PR-only selection rule
   must fail closed when a path is unclassified and must have a self-test.
6. Parallel work should start from one verified build. Each worker gets an
   isolated execution/resource tree and immutable binaries rather than a copy
   of mutable Cabal state.
7. Project-cache freshness should be automatic and observable without spending
   Actions minutes on a scheduled warmer. Cache deletion remains a deliberate,
   dry-run-first maintainer action after the replacement is proven usable.

## Scope

### In scope

- GitHub Actions workflow structure, summaries, and artifact handoff.
- Ephemeral worktree/resource-root isolation for concurrent consumers of one
  build.
- Git-history-derived project-cache epochs and manual bounded retention.
- Cabal build-product cache freshness and bounded retention strategy.
- Headless Hspec diagnostics, grouping, reruns, and local reproduction.
- Static-audit startup cost, beginning with save compatibility.
- Existing path selectors for expensive gates and behavior probes.
- Measurement of PR wall-clock latency, runner minutes, cache age/hit state,
  selected gates, and retry frequency.

### Out of scope

- Removing a regression gate solely because it is slow.
- Making manual-only, GPU, flaky, or scenario-heavy probes blocking.
- Replacing GitHub Actions or adopting self-hosted runners in the first pass.
- Changing game behavior, worldgen output, save formats, or persisted data.
- Duplicating shared world generation across shards without measurements showing
  a net critical-path win.
- Copying or hard-linking a mutable `dist-newstyle` into probe worktrees as a
  substitute for an explicit executable handoff.
- Treating `make ci` as an exact wall-clock mirror of a parallel cloud workflow;
  it remains the local coverage mirror and may execute the same gates serially.

## Design

### Measurement before selection changes

The workflow should emit one machine-readable and human-readable summary per
run containing event type, selected expensive gates, selected behavior probes,
cache exact-hit/fallback/miss and snapshot epoch, build duration, each gate
family's duration, retries, and total critical-path duration. The
initial implementation may use step timestamps and `$GITHUB_STEP_SUMMARY`; it
does not need an external metrics service.

The first baseline should cover enough completed PR and master runs to report
median and tail behavior separately. Cancelled superseded runs must be counted
as consumed feedback/runner time but not mixed into successful critical-path
percentiles.

### Remove repeated real-codec startup

The save-compat audit should retain one real decode of all tracked complete
session fixtures in the production audit. Its Python self-tests should inject a
deterministic descriptor result or explicitly opt out for cases testing
unrelated validation. Dedicated tests must still exercise descriptor success,
decode failure, missing decoded output, and manifest/real-descriptor mismatch.
The optimization must not replace the real Haskell decoder with a Python wire
format reimplementation.

### Reproduce and localize Hspec hangs

Do not add a suite-level or per-example timer in response to the observed hang.
Restart the same workflow on the same commit, compare the rerun with the hung
attempt, and reproduce locally with successively narrower Hspec matches if the
rerun hangs again. Arrange output so the last entered example or describe
context can be recovered during a future hang without changing how long tests
are allowed to run.

A later partition may separate the shared-world block from independent fast or
engine-backed specs, but partitioning must preserve the single-generation cache
for each shared `(seed, size, plateCount)` world and prove that concurrent test
processes do not contend for ports, config state, or repo-relative runtime
files.

### Isolate parallel probes after one build

Build the production executable once, resolve its exact path, and make that
immutable binary the input to every parallel probe. `probelib.boot` needs an
explicit runner-supplied executable override; ordinary one-probe developer use
may retain `cabal run`, but aggregate/CI mode must never start another Cabal
build or registration process merely to boot an engine that was already built.

Each concurrently active probe gets an ephemeral source/resource worktree at
the tested commit, its own debug port, logs, save/config output, and temporary
artifact directory. The built executable remains outside those trees and is
invoked with `--resource-root <worker-tree>`. Prepare all Git worktrees before
fan-out so Git metadata operations themselves do not race. Dispose of a worker
tree after its probe, or prove it clean before reuse; do not let one probe's
runtime writes become another probe's fixture.

This intentionally does **not** clone `dist-newstyle`. Cabal build products are
path-sensitive and mutable, and a warmed local tree is roughly 1.3 GB. Copying
it per probe would preserve the wrong abstraction at much higher I/O cost. The
handoff unit is a tested executable (and, where needed, a dedicated helper
executable), not Cabal's internal build database.

`persistence_contract` is the current exception because its structural save
comparison launches GHCi. Initially it must run without any concurrent Cabal
consumer. The stronger follow-up is a small prebuilt codec-helper executable
or equivalent binary interface so this probe also becomes a pure artifact
consumer. Retrying a Cabal race alone is not accepted isolation: an
infrastructure failure must not be converted into five minutes of hidden
latency and a green result.

### Keep build caches fresh without recreating cache explosion

Replace the indefinitely frozen per-plan project snapshot with an immutable
eight-change epoch. `tools/ci_cache_epoch.py` derives that epoch from
first-parent master history after a checked-in anchor: it counts only changes
to compiled-product inputs, so docs, Lua, assets, data and other runtime-only
edits do not spend the freshness budget. The anchor starts epoch zero; each
eighth relevant change advances the epoch. Pull requests derive the epoch from their
base SHA and are restore-only consumers. A successful master push is the sole
writer, which makes the new snapshot available in default-branch scope without
a scheduled warmer or mutable API counter.

On an epoch miss, restore the newest compatible older epoch before compiling,
then save the refreshed tree only after all blocking work succeeds. Compatibility
includes the exact immutable image reference, so an image-only change cannot
reuse old project objects. Retain the legacy per-plan key as the final bootstrap
fallback only for the image known to have written it. Concurrent first writers
remain benign because cache keys are immutable. A pre-anchor or unavailable PR
base warns and selects epoch 0 rather than failing the PR, and the growing
first-parent range is classified in one Git process rather than one per commit.

Retention is intentionally manual. `tools/ci_cache_cleanup.py` lists exact
cache IDs and proposed reasons in a dry run by default, keeps the newest three
snapshots per compatible image/toolchain, and never selects dependency caches or PR refs
under its default master scope. Legacy selection is a separate opt-in and is
refused until a v3 master cache exists; deletion requires another explicit
`--delete`. GitHub's normal expiry still handles unused branch-scoped entries.
The CI image remains independently content-addressed and refreshes only when
its recipe changes.

### Prove safe Hspec parallel regions

Hspec can do this directly: version 2.11.17 provides the `parallel` spec
modifier and the test binary already exposes `--jobs=N`; the suite is linked
with `-threaded` and has an RTS `-N` default. `--jobs` alone changes nothing,
however, because the current test tree marks no spec parallelizable.

Parallelism must be opt-in at audited group boundaries. The canonical
shared-world `aroundAll withHeadlessEngine` group remains sequential: its
examples share one `EngineEnv`, deliberately reuse memoized generated worlds,
and include readers plus mutation-sensitive fixtures whose ordering is part of
the current performance/correctness contract. Many other groups also own
mutable engine state, fixed temporary paths, process-wide configuration, or
repo-relative resources, so adding `parallel` at the root would be unsafe.

The first candidates are pure, CPU-bound specs with no engine,
process-global environment, fixed temporary path, current-directory mutation,
or repo-relative output. Independently owned engine groups are candidates only
after their ports, resource roots, temporary paths, and RTS capability budgets
are isolated. Measure `--jobs=1`, `2`, and a cap no higher than the runner's
useful CPU capacity. A global `parallel` wrapper is out of scope.

If audited in-process parallelism gives little benefit because world generation
still dominates, retain it only where measured and prefer process/job-level
fan-out: one serial shared-world Hspec lane plus separate isolated lanes for
genuinely independent groups. Process sharding must run the already-built test
executable from separate worktrees and must not regenerate the same shared
worlds in multiple lanes.

### Shorten the critical path after one build

The target workflow shape is one compilation producer followed by independent
consumers for:

- headless Hspec;
- static audits and selector self-tests;
- path-selected behavior probes; and
- worldgen/unit-asset/graphical gates when selected.

The producer should upload only the runnable binaries and minimal metadata each
consumer needs, not the whole `dist-newstyle` tree. GitHub Actions artifacts,
not caches, are the handoff mechanism for outputs produced by one job and
consumed by other jobs in the same workflow. Each consumer gets GitHub's clean
checkout in the same immutable CI image, downloads the artifact, and uses that
checkout as its resource tree. The final stable `build-test` check depends on
all consumer jobs and consolidates their verdicts so branch protection does
not mistake a partial fan-out for success.

Before adopting this shape, a spike must verify that both Haskell executables
run in the identical immutable CI container after artifact download, that the
test executable accepts Hspec options directly, and that `world_check.py` and
probe launching accept an explicit binary path instead of requiring
`cabal run`. CIR-7 supplies the probe half of this interface. If binary handoff
costs or dynamic-library coupling erase the latency win, keep one job and
parallelize only proven non-contending work in isolated worker trees.

Separate jobs increase total runner minutes even while reducing wall time. That
trade is accepted for independent, measured gate families, with explicit
concurrency caps and timing/cost reporting. The required-check surface remains
one stable aggregate rather than exposing every internal lane as a permanent
branch-protection contract.

### Revisit PR selection only with coverage evidence

After the structural and repeated-work wins land, measure what remains. Changes
to the policy that the full Hspec suite runs on every PR are a separate,
explicit decision. A candidate model is a small always-run smoke/contract set,
path-selected integration groups, and the full suite on master, but it is not
adopted by this design yet. Any selector must be fail-closed, self-tested, and
map tests to source/config/data ownership without relying on test filename alone.

## Decisions

### D-1. Preserve coverage while removing repeated work and serialization

The first optimization passes target duplicated setup, unbounded hangs, stale
cache snapshots, and unnecessary critical-path serialization. Existing tests
are not deleted or demoted merely to meet a runtime number.

### D-2. Retain the complete post-merge master backstop

Path selection may reduce safe pull-request work, but every gate that is
selective on PRs continues to run after merge so selector omissions cannot make
master permanently green without the covered check ever running.

### D-3. Keep the shared-world generation contract

Hspec restructuring must not regenerate identical expensive worlds per shard.
The existing memoized shared-world block is a performance invariant unless a
measured replacement is faster in total and on the critical path.

### D-4. Restart and measure Hspec hangs instead of adding timers

The observed Hspec hang does not authorize a suite-level or per-example timer.
Restart the same commit first, then use comparative run evidence and narrower
local reproduction to decide whether the fault is a test, engine lifecycle, or
runner-specific problem.

### D-5. Build once, then isolate execution trees rather than Cabal trees

Parallel probes and post-build consumers receive immutable executables from one
verified build. Each concurrently active consumer runs in its own checkout or
ephemeral Git worktree/resource root. No consumer receives a cloned,
hard-linked, or concurrently mutable `dist-newstyle`; any remaining GHCi user
runs exclusively until it is replaced by a prebuilt helper.

### D-6. Rotate after eight build-relevant master changes

The project-cache epoch is derived reproducibly from first-parent master
history, with eight compiled-input changes per epoch. Pull requests use their
base's epoch and never publish; successful master CI is the only writer. A
pre-anchor or unavailable base degrades visibly to epoch 0. The exact resolved
image is a separate compatibility component of every v3 key and restore prefix.
This bounds compile drift without a scheduled workflow. Retention is a separate
manual, dry-run-first operation, and legacy caches cannot be selected until a
replacement has been seeded.

### D-7. Prefer bounded parallel critical paths over minimum runner minutes

After one build, independent Hspec, audit, probe, and selected expensive-gate
families should run concurrently. Additional GitHub-hosted runner minutes are
an accepted trade for shorter feedback, provided concurrency is capped,
coverage is unchanged, and the workflow reports both latency and total runner
cost.

### D-8. Hspec parallelism is explicit and fixture-aware

Use Hspec's `parallel`/`--jobs` only on audited groups. The shared-world block
and any group with unisolated mutable engine, filesystem, environment, or
process state remain sequential. `parallel` is never applied to the whole
suite merely because the framework supports it.

## Open questions

### Q-1. What latency and runner-minute budgets define success?

Proposed starting service levels are PR median at or below 10 minutes, PR 95th
percentile at or below 15 minutes when no selected scenario inherently exceeds
that budget, and master at or below 15 minutes. These are optimization targets,
not test timers. The user may prefer a different balance between wall-clock
latency and parallel-runner consumption. CIR-1 may publish these as provisional
measurement bands, but it must not turn them into failure gates without a later
maintainer decision.

### Q-2. May the full Hspec suite become path-selective on pull requests?

Keeping it unconditional is the conservative coverage choice and still permits
meaningful wins from CIR-2 through CIR-5. Making integration groups selective
could reduce ordinary PR latency further, but requires durable ownership
mapping and accepts that an omitted cross-area regression may be found only by
the master backstop. This question affects CIR-6 only. CIR-6 must stop for an
explicit maintainer decision before changing PR selection; retaining the
unconditional suite is the safe default.

### Q-3. Is increased GitHub-hosted runner usage acceptable to reduce wall time?

Resolved by D-7. The user prefers parallel execution wherever independence can
be proved; bounded extra runner usage is acceptable in exchange for shorter
feedback.

### Q-4. What is the right cache refresh epoch?

Resolved by D-6 for the first deployment: eight build-relevant first-parent
master changes per immutable epoch. This deliberately spends no scheduled
runner minutes. CIR-1 must measure whether the build-time budget is exceeded
before the eighth change; changing the count later does not alter the
master-writer or immutable-epoch design.

### Q-5. How should the headless suite expose its last active example?

Candidates are a small custom formatter/runner hook that flushes example starts,
a wrapper that preserves line-buffered progress, or partition-level markers.
The choice must help compare a manually restarted run and must not serialize
tests that are intentionally parallelizable later. CIR-3 may choose the
smallest reliable diagnostic that preserves the existing output and scheduling
contracts; it must stop for a maintainer decision if useful diagnostics require
changing either contract.

### Q-6. Which Hspec groups are both safe and worth parallelizing?

The framework capability is verified, but the repository boundary is not. CIR-8
must inventory fixture and process ownership, measure candidate groups at
`--jobs=1/2/...`, and leave any uncertain group sequential. If no safe group
materially shortens the critical path without duplicating shared worldgen,
CIR-8 records that result rather than forcing a parallel implementation.

### Q-7. Are the built executables self-contained enough for artifact handoff?

The producer and consumers use the same immutable container image, which makes
handoff plausible, but the Linux binary's dynamic-library requirements and the
direct Hspec runner's runtime environment have not yet been exercised as a
downloaded artifact. CIR-5 stops after the spike and reports the blocker if the
minimal binary bundle cannot run without copying Cabal's build database.

## Verification strategy

- Capture baseline and post-change timings from both PR and master workflows,
  separating successful, failed, cancelled, and timed-out runs.
- Run the save-compat audit and its self-test, proving the production path still
  invokes the real decoder while the self-test covers descriptor failure modes
  without 24 Cabal REPL startups.
- Compare the restarted PR #1328 run with its cancelled predecessor. If it
  hangs again, reproduce with successively narrower Hspec matches before
  changing CI.
- Run the full CI-eligible probe selection with at least two workers from one
  prebuilt executable, proving no worker invokes `cabal run`, no
  `package.conf.inplace`/shared-build mutation occurs, isolated resource trees
  remain independent, and the prior solo-retry tax disappears.
- Validate cache keys and fallback ordering with a pure self-test or dry-run
  script, then inspect exact-hit/fallback evidence on successive workflow runs.
  Prove that change seven retains its epoch, change eight advances it, and
  docs/runtime-only changes do not count; verify that PRs cannot save and the
  replacement is written in master scope before legacy cleanup is allowed.
- For artifact handoff, execute downloaded binaries in the same container and
  prove Hspec, one representative behavior probe, and world check can locate
  all repo-relative resources.
- For Hspec, establish a `--jobs=1` baseline, mark only audited candidate
  groups, then compare `--jobs=2` and the runner-appropriate cap across repeated
  runs. Example counts and coverage stay identical; the shared-world generation
  count must not increase, and fixture/state failures or output races reject
  the candidate boundary.
- Compare wall-clock critical path and total runner minutes before and after
  each slice. A wall-time improvement that causes unbounded cost or materially
  higher flake/retry rates does not pass.
- Keep workflow YAML, local `make ci` coverage, and repository testing docs in
  sync. The local gate need not reproduce cloud parallel scheduling.

## Delivery plan

### CIR-1. Publish durable CI timing and selection diagnostics

- **Outcome:** Every CI run explains cache state, selected gates/probes, stage
  durations, retries, and critical-path duration in its summary.
- **Scope:** Workflow summary plumbing and a bounded historical baseline of PR
  and master runs; no gate-policy changes.
- **Phase:** 1 — measurement
- **Depends on:** `none`
- **Ordering:** `can land first`
- **Relevant decisions:** D-1, D-4
- **Acceptance signals:** A successful run and a controlled failure both retain
  useful summaries; cancelled runs are identified separately in analysis.
- **Out of scope:** External observability services and alerting.
- **Open questions:** Q-1

### CIR-2. Decode save-compat fixture descriptors once per self-test run

- **Outcome:** Save-compat self-tests no longer launch a Cabal REPL for each of
  24 audit calls, while the production audit still decodes real fixtures once.
- **Scope:** Dependency injection/caching at the descriptor verification seam
  and focused failure-mode tests.
- **Phase:** 1 — remove repeated work
- **Depends on:** `none`
- **Ordering:** `can land first`
- **Relevant decisions:** D-1
- **Acceptance signals:** Existing correctness tests pass; descriptor mismatch
  and decoder-failure cases remain covered; measured CI time drops materially
  from the current ~2m20s stage.
- **Out of scope:** Reimplementing the Haskell envelope decoder in Python.
- **Open questions:** `None`

### CIR-3. Reproduce and localize headless-suite hangs without timers

- **Outcome:** Repeated hangs can be compared and localized to the last active
  test context without imposing a suite-level or per-example timer.
- **Scope:** Same-commit rerun comparison, flushed progress/context, and a
  narrowing procedure for local Hspec reproduction.
- **Phase:** 1 — diagnose pathological runs
- **Depends on:** `CIR-1`
- **Ordering:** `critical path`
- **Relevant decisions:** D-4
- **Acceptance signals:** The restarted run is compared with the cancelled
  attempt; a repeated hang can be narrowed to a describe/example or lifecycle
  boundary; ordinary Hspec runtime and verdict are unchanged.
- **Out of scope:** Fixing the product/test deadlock that triggered any one
  specific hung PR run.
- **Open questions:** Q-5

### CIR-7. Isolate parallel probes behind one prebuilt executable

- **Outcome:** The full CI-eligible probe selection can use multiple workers
  without any worker mutating a shared Cabal build tree or paying retry tax for
  an infrastructure race.
- **Scope:** An explicit executable override for probe boot, ephemeral worker
  worktree/resource-root lifecycle, unique per-worker ports and outputs, and an
  exclusive boundary for the remaining persistence-contract GHCi consumer.
- **Phase:** 1 — remove infrastructure contention
- **Depends on:** `none`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-5
- **Acceptance signals:** A two-worker full selection boots one prebuilt
  executable without invoking `cabal run`, produces no shared
  `package.conf.inplace` errors, and needs no race-induced solo retries;
  single-probe invocation still works; worker trees are disposed cleanly.
- **Out of scope:** Copying or hard-linking `dist-newstyle`, increasing the
  worker cap above two before measuring resource contention, restructuring the
  overall workflow job graph, or changing probe assertions.
- **Open questions:** `None`

### CIR-4. Rotate the project cache every eight build-relevant master changes

- **Outcome:** Successful master CI automatically seeds a fresh immutable
  project-cache epoch after each group of eight compiled-input changes, while
  pull requests consume master caches without publishing their own.
- **Scope:** A deterministic Git-history epoch, master-only cache writes,
  compatible older-epoch and legacy restore order, run-summary diagnostics,
  a dry-run-first exact-ID cleanup command, and focused self-tests.
- **Phase:** 2 — shorten compilation
- **Depends on:** `CIR-1`
- **Ordering:** `not on the critical path`
- **Relevant decisions:** D-1, D-6
- **Acceptance signals:** The seventh relevant change remains on its current
  epoch and the eighth advances; docs/runtime-only changes do not advance it;
  PRs derive from the base and cannot save; master saves only after success;
  same-epoch exact hits and compatible prior-epoch fallback are observable;
  cleanup previews exact IDs and protects dependency, PR and un-replaced
  legacy caches.
- **Out of scope:** Scheduled warmers, automatic deletion, mutable image tags,
  and self-hosted persistent build directories.
- **Open questions:** `None`

### CIR-8. Prove and adopt only safe Hspec parallel regions

- **Outcome:** Audited independent Hspec groups run concurrently when that
  materially reduces the critical path, or the repository records a measured
  no-win result instead of forcing unsafe parallelism.
- **Scope:** Fixture/process ownership inventory, candidate `parallel`
  annotations, repeated `--jobs=1`, `2`, and runner-cap measurements, and a
  bounded spike of isolated process lanes if in-process concurrency cannot
  reach the worldgen-dominated path.
- **Phase:** 2 — shorten test execution
- **Depends on:** `CIR-1`, `CIR-3`
- **Ordering:** `can proceed independently of cache work`
- **Relevant decisions:** D-1, D-3, D-8
- **Acceptance signals:** Example counts and verdicts remain identical across
  repeated runs; shared-world generation count does not increase; no mutable
  fixture/state races appear; retained parallel boundaries show a material
  wall-time improvement.
- **Out of scope:** A root-level `parallel`, duplicating canonical world
  generation across shards, or making Hspec path-selective.
- **Open questions:** Q-6

### CIR-5. Run post-build gate families on independent critical paths

- **Outcome:** Hspec, static audits, behavior probes, and selected expensive
  gates do not add their full serial durations after compilation.
- **Scope:** A compilation-producer artifact, clean-checkout consumer jobs,
  explicit binary-path support where needed, workflow job graph, one stable
  aggregate required check, and measured runner-minute tradeoff.
- **Phase:** 3 — structural parallelism
- **Depends on:** `CIR-1`, `CIR-2`, `CIR-3`, `CIR-7`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2, D-3, D-4, D-5, D-7
- **Acceptance signals:** Identical gate coverage and verdicts, no duplicate
  shared world generation, lower PR critical path, bounded artifact overhead,
  bounded runner-minute growth, and no material retry/flakiness regression.
- **Out of scope:** Parallel execution of tests that share mutable engine state
  without isolation, or uploading the complete `dist-newstyle` tree.
- **Open questions:** Q-7

### CIR-6. Reassess full-suite and behavior-probe selection from measured coverage

- **Outcome:** Either retain the unconditional PR Hspec policy with evidence
  that structural wins are sufficient, or adopt a fail-closed, self-tested
  path-to-test-group selector with a full master backstop.
- **Scope:** Coverage/ownership map, test grouping, selector self-tests, and
  documented stop/ask behavior for unclassified paths.
- **Phase:** 4 — policy optimization
- **Depends on:** `CIR-1`, `CIR-5`, `CIR-8`
- **Ordering:** `not on the critical path`
- **Relevant decisions:** D-1, D-2, D-3, D-8
- **Acceptance signals:** A measured decision; if selection changes, every path
  has an explicit result, unclassified paths fail closed, and master continues
  to run the complete suite.
- **Out of scope:** Demoting tests based only on their current duration.
- **Open questions:** Q-2
