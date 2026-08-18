# CI runtime reduction design

This design reduces pull-request feedback time without quietly dropping
regression coverage. It is motivated by the 2026-08-15 run for PR #1328, whose
headless Hspec step remained active for more than an hour even though recent
successful runs normally completed the same step in about four minutes. A hang
is something to restart, reproduce, and measure—not justification for adding a
timer to the test suite.

Design state: `exploring`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [ ] EPIC. Restore fast, bounded CI feedback without weakening regression coverage
- [ ] CIR-1. Publish durable CI timing and selection diagnostics
- [ ] CIR-2. Decode save-compat fixture descriptors once per self-test run
- [ ] CIR-3. Reproduce and localize headless-suite hangs without timers
- [ ] CIR-4. Refresh the frozen build-product cache on a bounded cadence
- [ ] CIR-5. Run post-build gate families on independent critical paths
- [ ] CIR-6. Reassess full-suite and behavior-probe selection from measured coverage

## Epic contract

- **Goal:** Make ordinary pull-request CI give fast, predictable feedback and
  make abnormal runs easier to reproduce and diagnose.
- **Done when:** CI publishes enough timing and selection data to explain its
  critical path; repeated work identified in the save-compat audit is removed;
  a rerun of a hung headless suite provides enough evidence to compare and
  localize the behavior; build-cache age is bounded without unbounded churn; and
  independent gate families no longer serialize behind one another after the
  build unnecessarily. Any reduction in which tests run on a PR is backed by an
  explicit path-to-coverage contract and retains a full post-merge backstop.
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
- The `dist-newstyle` cache key is intentionally stable for a dependency plan.
  Because GitHub cache entries are immutable, the first snapshot for a plan is
  frozen and later runs rebuild every project change since that snapshot. This
  bounds cache count but lets incremental compilation drift upward throughout a
  long-lived plan window.
- Behavior probes already have per-probe 900-second timeouts and process-group
  termination in `tools/run_probes.py`. The aggregate runs up to two probes in
  parallel and retries a failure once in isolation. There are currently eleven
  CI-eligible probes; core or unclassified changes select all eleven.
- Tracker searches for `CI runtime`, `CI pipeline`, `CI slow`, and `headless
  hang` found no existing CI-runtime epic. Issue #1262 concerns closing the unit
  atlas pipeline, and issue #1323 concerns descendant cleanup after ordinary
  behavior-probe failure; both are adjacent but not duplicates of this arc.

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

## Scope

### In scope

- GitHub Actions workflow structure, summaries, and artifact handoff.
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

### Keep build caches fresh without recreating cache explosion

Replace the indefinitely frozen per-plan project snapshot with a bounded epoch,
such as plan plus calendar week, while retaining a restore prefix that can reuse
the newest older compatible snapshot on an epoch miss. The exact epoch is a
proposal pending measurement. The key must remain compiler-, Cabal-, OS-, and
plan-sensitive, and concurrent first writers must remain benign. Cache age and
whether the restore was exact or fallback must appear in the run summary.

### Shorten the critical path after one build

The target workflow shape is one compilation producer followed by independent
consumers for:

- headless Hspec;
- static audits and selector self-tests;
- path-selected behavior probes; and
- worldgen/unit-asset/graphical gates when selected.

The producer should upload only the runnable binaries and minimal metadata each
consumer needs, not the whole `dist-newstyle` tree. Before adopting this shape,
a spike must verify that the Haskell executables run in an identical immutable
CI container after artifact download, and that `world_check.py` and probe
launching can accept an explicit binary path instead of requiring `cabal run`.
If binary handoff costs or dynamic-library coupling erase the latency win, keep
one job and parallelize only proven non-contending static work.

Separate jobs increase total runner minutes and make the required-check surface
more complex even while reducing wall time. The workflow should therefore split
only gate families with a demonstrated critical-path benefit and retain clear,
stable check names for branch protection.

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

## Open questions

### Q-1. What latency and runner-minute budgets define success?

Proposed starting service levels are PR median at or below 10 minutes, PR 95th
percentile at or below 15 minutes when no selected scenario inherently exceeds
that budget, and master at or below 15 minutes. These are optimization targets,
not test timers. The user may prefer a different balance between wall-clock
latency and parallel-runner consumption.

### Q-2. May the full Hspec suite become path-selective on pull requests?

Keeping it unconditional is the conservative coverage choice and still permits
meaningful wins from CIR-2 through CIR-5. Making integration groups selective
could reduce ordinary PR latency further, but requires durable ownership
mapping and accepts that an omitted cross-area regression may be found only by
the master backstop. This question affects CIR-6 only.

### Q-3. Is increased GitHub-hosted runner usage acceptable to reduce wall time?

Running Hspec, probes, and audits on separate runners after one build can turn
their sum into their maximum, but consumes more runner minutes and requires
artifact handoff. This question determines whether CIR-5 targets separate jobs
or conservative within-job concurrency.

### Q-4. What is the right cache refresh epoch?

A weekly epoch is a concrete proposal, not yet a decision. Timing data should
compare compile drift, cache size, restore time, and the number of simultaneous
dependency plans before choosing weekly, daily, or an explicit rolling policy.

### Q-5. How should the headless suite expose its last active example?

Candidates are a small custom formatter/runner hook that flushes example starts,
a wrapper that preserves line-buffered progress, or partition-level markers.
The choice must help compare a manually restarted run and must not serialize
tests that are intentionally parallelizable later.

## Verification strategy

- Capture baseline and post-change timings from both PR and master workflows,
  separating successful, failed, cancelled, and timed-out runs.
- Run the save-compat audit and its self-test, proving the production path still
  invokes the real decoder while the self-test covers descriptor failure modes
  without 24 Cabal REPL startups.
- Compare the restarted PR #1328 run with its cancelled predecessor. If it
  hangs again, reproduce with successively narrower Hspec matches before
  changing CI.
- Validate cache keys and fallback ordering with a pure self-test or dry-run
  script, then inspect exact-hit/fallback evidence on successive workflow runs.
- For artifact handoff, execute downloaded binaries in the same container and
  prove Hspec, one representative behavior probe, and world check can locate
  all repo-relative resources.
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

### CIR-4. Refresh the frozen build-product cache on a bounded cadence

- **Outcome:** Compatible project artifacts receive a fresh bounded snapshot
  periodically, limiting cumulative recompilation without per-commit cache
  explosion.
- **Scope:** Cache-key epoch, restore order, cache diagnostics, comments/docs,
  and concurrency-safe self-tests where practical.
- **Phase:** 2 — shorten compilation
- **Depends on:** `CIR-1`
- **Ordering:** `not on the critical path`
- **Relevant decisions:** D-1
- **Acceptance signals:** Same-epoch exact hits, new-epoch compatible fallback,
  one bounded snapshot per plan/epoch, and lower compile drift over the chosen
  window.
- **Out of scope:** Self-hosted persistent build directories.
- **Open questions:** Q-4

### CIR-5. Run post-build gate families on independent critical paths

- **Outcome:** Hspec, static audits, behavior probes, and selected expensive
  gates do not add their full serial durations after compilation.
- **Scope:** Binary artifact spike, explicit binary-path support where needed,
  workflow job graph, stable required-check names, and measured runner-minute
  tradeoff.
- **Phase:** 3 — structural parallelism
- **Depends on:** `CIR-1`, `CIR-2`, `CIR-3`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2, D-3, D-4
- **Acceptance signals:** Identical gate coverage and verdicts, no duplicate
  shared world generation, lower PR critical path, bounded artifact overhead,
  and no material retry/flakiness regression.
- **Out of scope:** Parallel execution of tests that share mutable engine state
  without isolation.
- **Open questions:** Q-3

### CIR-6. Reassess full-suite and behavior-probe selection from measured coverage

- **Outcome:** Either retain the unconditional PR Hspec policy with evidence
  that structural wins are sufficient, or adopt a fail-closed, self-tested
  path-to-test-group selector with a full master backstop.
- **Scope:** Coverage/ownership map, test grouping, selector self-tests, and
  documented stop/ask behavior for unclassified paths.
- **Phase:** 4 — policy optimization
- **Depends on:** `CIR-1`, `CIR-5`
- **Ordering:** `not on the critical path`
- **Relevant decisions:** D-1, D-2, D-3
- **Acceptance signals:** A measured decision; if selection changes, every path
  has an explicit result, unclassified paths fail closed, and master continues
  to run the complete suite.
- **Out of scope:** Demoting tests based only on their current duration.
- **Open questions:** Q-2
