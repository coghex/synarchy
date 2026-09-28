# CI runtime reduction design

This design reduces pull-request feedback time without quietly dropping
regression coverage. It was first motivated by the 2026-08-15 run for PR #1328,
whose headless Hspec step remained active for more than an hour even though
recent successful runs normally completed the same step in about four minutes.
A hang is something to restart, reproduce, and measure—not justification for
adding a timer to the test suite.

The problem has since changed shape. By 2026-09-28 the save-compat REPL, the
engine-free audits, the probe build race and cache staleness had all been
addressed (CIR-1, CIR-2, CIR-4, CIR-7), yet a successful pull-request run
takes a median of 31 minutes. The headless Hspec run alone is a median of 17
minutes, strictly sequential, and grows with the suite (about 5,000 examples on
2026-09-05, 10,500 on 2026-09-28). The Haskell build in front of it is a median
of 10 minutes. The maintainer's stated goal is pull-request CI under twenty
minutes without losing significant coverage. The remaining slices are
re-planned around those two costs.

Design state: `ready for issue processing`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [x] EPIC. Bring pull-request CI under twenty minutes without weakening regression coverage — [#2742]
- [x] CIR-1. Publish durable CI timing and selection diagnostics — [#2277]
- [x] CIR-2. Decode save-compat fixture descriptors once per self-test run — [#2273]
- [x] CIR-7. Isolate parallel probes behind one prebuilt executable — [#1570]
- [x] CIR-4. Rotate the project cache every eight build-relevant master changes — [no-issue]: delivered directly as 7b3757ded without a tracker issue
- [x] CIR-14. Measure headless Hspec time per top-level group on CI — [#2743]
- [x] CIR-15. Partition the headless suite into lanes that run every example exactly once — [#2744]
- [x] CIR-16. Run the headless lanes as parallel CI jobs — [#2745]
- [x] CIR-17. Attribute pull-request build time to epoch drift versus the change under test — [#2746]
- [x] CIR-3. Reproduce and localize headless-suite hangs without timers — [#2747]
- [x] CIR-18. Report headless lane durations against an advisory budget — [#2748]
- [ ] CIR-8. Prove and adopt only safe in-process Hspec parallel regions — [deferred]: until #2745 has merged and, over at least five master runs, tools/ci_timing_report.py shows the longest lane's median is a lane that does not hold the shared-world block
- [ ] CIR-5. Hand prebuilt executables to the remaining post-build gates — [deferred]: until #2745 has merged; then check whether save compatibility or world_check still runs after a headless lane in the same job
- [ ] CIR-6. Reassess full-suite and behavior-probe selection from measured coverage — [deferred]: until #2745 has merged and #2746's build-side recommendation has been landed or declined; then measure the median successful PR run over at least ten runs with tools/ci_timing_report.py
- [ ] CIR-9. Make required CI checks merge-group aware
- [ ] CIR-10. Make repository identity transfer-ready
- [ ] CIR-11. Transfer the repository and rebind existing automation
- [ ] CIR-12. Pilot the native merge queue manually
- [ ] CIR-13. Convert the drainer into a queue-admission controller

## Epic contract

- **Goal:** Make ordinary pull-request CI give fast, predictable feedback —
  under twenty minutes, per the maintainer's 2026-09-28 target — and make
  abnormal runs easier to reproduce and diagnose.
- **Done when:** the median run wall time of successful pull-request runs is
  under twenty minutes (D-15), measured with `tools/ci_timing_report.py` over a
  stated sample of runs; the headless Hspec
  suite runs as parallel lanes that together execute every example exactly
  once, without generating any shared world twice; the build in front of those
  lanes has a measured, explained cost and any build-side change it justifies
  has landed; a rerun of a hung headless suite provides enough evidence to
  compare and localize the behavior; and several approved PRs can advance
  through hosted merge groups without serial contributor-managed branch
  updates. Already delivered: timing and cache diagnostics (CIR-1), the
  save-compat REPL removal (CIR-2), bounded cache age (CIR-4), and prebuilt
  probe executables (CIR-7). Any reduction in which tests run on a PR is backed
  by an explicit path-to-coverage contract and retains a full post-merge
  backstop.
- **Users and operators:** Contributors waiting for PR checks, reviewers deciding
  whether a result is trustworthy, and maintainers diagnosing flakes and hangs.
- **Arc label:** `tooling` proposed

## Current state and evidence

### Re-measurement, 2026-09-28

Taken with `tools/ci_timing_report.py --last 30 --event pull_request` and
`--last 20 --event push --branch master` (the CIR-1 tool), plus the raw job
logs of runs 36424074239, 36343120829, 36320504148 and 36051757184.

- **Critical path.** Seventeen successful PR runs: median wall time 31m00s,
  95th percentile 36m32s. Nineteen successful master pushes: median 31m03s,
  p95 36m58s. `test-and-audits` is the slowest job in every one of them.
  `static-audits` (about 5–6 min) and `behavior-probes` (about 9–12 min) finish
  well before it and are not on the critical path.
- **Where `test-and-audits` spends it (PR medians).** Container start 49 s,
  dependency plan 21 s, `Build (library + executable)` 182 s (p95 245 s), audio
  build boundary 31 s, `Build test suites` 425 s (p95 524 s), `Headless test
  suite` **1035 s (17m15s, p95 19m34s)**, save compatibility 30 s, and
  `world_check --quick` 94 s when the worldgen gate fires. The two build steps
  together are a median of about ten minutes; the Hspec run is about seventeen.
- **Hspec is sequential and growing.** No spec is marked `parallel`, so the
  4-vCPU runner executes 10,521 examples one at a time (`Finished in 1123.37
  seconds` on run 36424074239). The same step took 854 s on 2026-09-24 with
  10,440 examples; 37 more examples cost 265 s more. About 111 s of that is the
  new `Test.Headless.WorldGen.ExactRiverWorld` (landed 2026-09-26 in
  04f5b75a7), which rebuilds the full geology timeline for four worlds, three
  of them (`(42,32,4)`, `(4567,32,4)`, `(13579,32,4)`) used by no other spec.
  The rest is general worldgen slowdown: the map-pyramid w64 golden went from
  41.9 s to 52.9 s, basic generation from 30.5 s to 38.5 s.
- **The twenty slowest examples are about 530 s of the 1123 s,** and about
  450 s of that is inside the shared-world `aroundAll withHeadlessEngine` block
  (`test-headless/Spec.hs` lines 460–575): map-pyramid goldens and
  composition (≈137 s), zoom artifact (53 s), `ExactRiverWorld` (111 s), zoom
  parity (40 s), basic and determinism generation (51 s), flatness (20 s),
  chunk/fast parity (19 s), and the full-tier-only w128 volcano exposure case
  (59 s). The rest of the suite (≈600 s) is spread across ten thousand cheaper
  examples. The per-group split is not yet measured; the CI log carries only the
  top twenty (`--print-slow-items=20`).
- **Shared worlds by key.** `sharedWorld env 42 64 3` has at least ten
  consumers (WorldGen, Geology, ZoomArtifact, Exposure, InlandSources,
  ActionOutcome, Climate, SelectTileZ, Realization, ExactRiverWorld,
  MapPyramid). `(42,128,3)` is generated on every run by the map-pyramid
  worldSize-128 golden and reused by the full-tier Exposure case.
  Single-consumer keys include `(1840733254,64,10)` (ZoomParity), `(7,64,3)`
  (BorderProbe), `(42,32,3)` (InlandSources) and the three private
  `ExactRiverWorld` keys. About twenty test modules also send their own
  `WorldInit` for private worlds, both inside the block (Exposure's w8 rim
  world, for example) and outside it.
- **Two costs were ruled out.** Log volume: since #1916 both CI invocations pass
  `--format=failed-examples`, so a passing run prints a constant handful of
  lines. Per-example engine boots: the 361 examples under `around
  withHeadlessEngine` are cheap — `UI.InteractiveBounds` and
  `UI.FocusNavigation` (111 examples) finish in 0.7 s locally.
- **Build time is driven by drift and cascade, not by cache misses.** Exact
  project-cache hits still cost 10–12.6 min of build on runs 36320504148,
  36317483841 and 36288123692 (epoch 52, position 3/8), while other exact hits
  cost 1.5–5 min. The epoch snapshot is written once, by the epoch's first
  successful master push; every later PR and master run in that epoch replays
  every build-relevant change since, plus its own diff, and the `-O2`
  interface cascade decides how much of the 900-module library and
  480-module test suite that recompiles. #2276
  (`docs/test_suite_compile_cost_2276.md`) measured the test suite at 904.5
  CPU-s under `-O2`, 484.7 under `-O0` (−46 %) and −16.6 % under `-O1`; `-O0`
  was kept out of that issue's scope by owner specification.
- **Hangs recur.** Master run 36054401429 (merge of #2704, 2026-09-24) sat in
  `Headless test suite` for 82 minutes until the 90-minute job timeout cancelled
  it. Its last output was a run of `READY port=…` lines from the debug-socket
  and debug-console specs at 20:47:42, sixteen minutes into the step, then
  nothing until cancellation at 21:52:57. With `--format=failed-examples`
  nothing else identifies the active example. It is not isolated: every
  failed or cancelled CI run from 2026-08-16 to 2026-09-28 (852 runs) was
  scanned, and ten headless steps ran 78–87 minutes into the 90-minute job
  timeout — master pushes 32401924621 (08-20), 32432146709 (08-21),
  33079354477 (08-27), 33274229355 (08-29), 33319248031 (08-30), 33426496347
  (08-31), 33559257760 (09-01), 33690302067 (09-02) and 36054401429 (09-24),
  and PR run 32627523057 (08-23). A further run, 32510304445 (08-21), was
  cancelled forty minutes into the step. Nine of the ten are master pushes,
  which always run the full tier, but the PR hang ran without it, so the full
  tier is not required. Each costs ninety minutes of a runner and a missing
  master verdict.
- **Runner economics.** GitHub documents standard GitHub-hosted runner usage as
  free for public repositories, a limit of 20 concurrent jobs on the Free
  plan, and 4 CPUs, 16 GB of RAM and 14 GB of SSD for a public repository's
  `ubuntu-latest` runner. A PR run today holds three long-running jobs at once
  (`test-and-audits`, `static-audits`, `behavior-probes`); peak concurrency
  across overlapping PR and master runs has not been measured.

### Verified current state

The bullets below are the pre-2026-09 evidence that shaped CIR-1 through CIR-7.
They stay as the record; where they conflict with the re-measurement above,
the re-measurement is current.

- `.github/workflows/ci.yml` now runs `test-and-audits`, `static-audits` and
  the PR-only `behavior-probes` worker in parallel after image resolution,
  then reports one stable `build-test` aggregate. Compilation, the complete
  headless Hspec suite, the two save-compatibility steps and selected
  worldgen work remain serial inside `test-and-audits`; every engine-free
  audit, the probe-runner self-tests and the unit-asset gate moved to
  `static-audits` in #2272, which needs only `resolve-image`. That removed
  roughly 4.5-5.5 minutes from the main worker's critical path, but its build
  plus headless suite is still that path.
- `master` protection is strict: the required `build-test`, `review-approved`,
  and `behavior-probes` checks must pass on a head that is up to date with its
  base. The current drainer has one active lane, so a branch update, pending
  check, or rerun for the candidate holding that lane prevents every later
  candidate from advancing. The latest forty merged PRs include hours with
  four merges, making repeated twenty-minute revalidation a throughput limit
  rather than an occasional inconvenience.
- The drainer is deliberately default-branch-only. It refuses to start from a
  checkout not on the repository default branch, refuses a PR whose
  `baseRefName` is not that branch, and holds one repository-wide run lock for
  its lifetime. Running one independent drainer per lieutenant branch is
  therefore not supported by configuration; it would require a multi-base lane
  design in Kanban or a different integration controller.
- CI's push trigger names only `master`, and the project cache is written only
  by a successful `refs/heads/master` push. PR workflows already accept any
  base branch, but an integration branch would not receive its own full
  post-merge run or seed branch-scoped project caches until both policies were
  generalized. GitHub's [cache access
  rules](https://docs.github.com/en/actions/reference/workflows-and-actions/dependency-caching#restrictions-for-accessing-a-cache)
  permit a PR workflow to restore caches from its base branch and the default
  branch, so a trusted integration-branch push can be a useful cache writer
  without sharing sibling-branch products.
- GitHub's native merge queue is the closest built-in match for this problem:
  it creates temporary merge groups against the current base and can run
  several group builds concurrently without requiring every PR author to
  update their branch. It requires the workflow to handle `merge_group`.
  However, GitHub's [merge-queue availability and workflow
  contract](https://docs.github.com/en/repositories/configuring-branches-and-merges-in-your-repository/configuring-pull-request-merges/managing-a-merge-queue)
  currently offers the feature for organization-owned public repositories (and
  qualifying organization-owned private repositories), while
  `coghex/synarchy` is public and user-owned. Using it therefore first requires
  transferring the repository to an organization.
- GitHub's [repository transfer
  contract](https://docs.github.com/en/repositories/creating-and-managing-repositories/transferring-a-repository)
  carries issues, pull requests, releases, settings, webhooks, secrets, deploy
  keys, Git history, and fork relationships. Old Web and Git URLs redirect, but
  creating a new repository or fork at the retired `coghex/synarchy` location
  would permanently delete that redirect. The
  current repository has one Actions secret (`NTFY_URL`), one environment
  (`copilot`), no repository variables, no webhooks, no deploy keys, and no
  Pages site. Its only collaborator and the only observed issue assignee are
  `coghex`, so making `coghex` an owner/member of the target organization before
  transfer preserves the relevant access and assignments.
- GHCR is the material transfer exception. GitHub's [package-transfer
  contract](https://docs.github.com/en/packages/learn-github-packages/about-permissions-for-github-packages#about-repository-transfers)
  says the container registry uses user/organization-scoped granular
  permissions, so
  `ghcr.io/coghex/synarchy-ci` remains owned by the personal account, loses its
  repository link, and no longer grants the transferred repository's Actions
  workflow access. The active CI-image references are hard-coded in
  `.github/workflows/ci-image.yml`; two compatibility checks in `ci.yml` and
  `tools/ci_cache_report.py` intentionally name one historical image and must
  not be mechanically rewritten. The safe destination is a new
  `ghcr.io/synarchy-game/synarchy-ci` package written by the transferred
  repository's `GITHUB_TOKEN`, while the old public package remains
  temporarily available as rollback evidence.
- Seven tracked implementation/configuration files contain live or tested
  `coghex/synarchy` identity assumptions: the two CI workflows,
  `synarchy.cabal`, the probe-census schema ID, the cache reporter's historical
  image constant, and two Python self-tests. Historical measurement/report
  links can keep the old slug because the redirect preserves their target;
  canonical package metadata, generated links, tests, and active image
  publication must use the new identity.
- The installed PR drainer is keyed by canonical repository identity in both
  its discovery record and launchd label:
  `coghex/synarchy` / `com.coghex.drain-prs.coghex.synarchy`. Its controller
  resolves identity from `origin`, so updating the remote before uninstalling
  the old entry would make that entry undiscoverable through normal control.
  It must be stopped and uninstalled while the old remote is still canonical,
  then installed under the new identity after transfer. The shared issue-review
  backend is not repository-keyed, and no Synarchy issue-approval background
  service is installed.
- Every current local worktree shares the primary checkout's `.git/config`, so
  one `origin` update changes remote resolution for all of them. Existing
  directory names under `worktrees/coghex/synarchy/` are local labels and do not
  need a disruptive move. Open pull requests currently use same-repository
  heads and transfer with the repository; no contributor branch recreation is
  required.
- The user selected a new dedicated organization rather than either existing
  organization membership and approved `synarchy-game` as its slug. A
  2026-08-30 namespace check found no public account at that name. A missing
  public account is not a reservation guarantee; availability must be rechecked
  during organization creation, and an unavailable name stops for a new user
  decision rather than silently selecting a substitute.
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
  `verify_fixture_descriptors`, whose `dump_fixture_descriptors` started
  `cabal repl test:synarchy-test-headless`. That repeated real-codec startup
  is what made the CI step documented as a cheap static audit consistently
  cost about 2m20s — the measurement above stands as the record of it.
  CIR-2 was implemented in #2273 by taking the stronger form: every GHCi
  program in that family is now the compiled `exe:synarchy-save-codec`,
  which `cabal build all` produces and `dump_fixture_descriptors` execs.
  The production audit still performs one real fixture decode; it no longer
  pays an interpreter startup for it, and no descriptor caching was needed.
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
  `tools/persistence_snapshot.py`. That last clause describes the tree as
  surveyed; issue #2274 replaced that repl with `compare` in the compiled
  `exe:synarchy-save-codec`, which the runner's preflight resolves and hands
  down, so `persistence_contract` no longer holds `cabal-build` exclusively.
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
   than making every gate wait behind unrelated work, and a green PR run should
   complete in under twenty minutes. The headless suite keeps running in full on
   every PR; it just stops running one example at a time on one runner.
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
8. Several approved PRs may enter one hosted merge train without rewriting
   their source branches after every preceding merge. GitHub tests each exact
   cumulative candidate, ejects a failing change, and advances `master` without
   making contributors operate intermediate integration branches.

## Scope

### In scope

- GitHub Actions workflow structure, summaries, and artifact handoff.
- Ephemeral worktree/resource-root isolation for concurrent consumers of one
  build.
- Git-history-derived project-cache epochs and manual bounded retention.
- Cabal build-product cache freshness and bounded retention strategy.
- Headless Hspec diagnostics, grouping, reruns, and local reproduction.
- Partitioning the headless suite into lanes that run as parallel CI jobs,
  and the build strategy that supplies each lane with the test executable.
- Measuring and reducing the per-PR Haskell build (cache-epoch drift and the
  test suite's compile cost).
- Static-audit startup cost, beginning with save compatibility.
- Existing path selectors for expensive gates and behavior probes.
- Measurement of PR wall-clock latency, runner minutes, cache age/hit state,
  selected gates, and retry frequency.
- Native merge-queue admission, merge-group CI/cache behavior, repository
  transfer prerequisites, failure recovery, and the current drainer's role.

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

The 2026-09-24 master hang (run 36054401429) shows the diagnostic gap is now
wider than when this section was written. #1916 moved both CI invocations to
`--format=failed-examples` so a passing run's log is a handful of lines; the
cost is that a hung run's log names no active example at all, and that hang
could be placed only because the debug-console specs happen to print `READY
port=` lines. Whatever CIR-3 adds must keep #1916's quiet passing log — for
example, context markers at lane or top-level-group granularity, or a
formatter that records the entered example somewhere other than the log —
rather than restoring one line per passing example. Lanes (CIR-15, CIR-16)
also narrow a hang to one lane's groups and let every other lane report, but
they do not replace this diagnosis.

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

`persistence_contract` was the exception at design time because its structural
save comparison launched GHCi, and initially it ran without any concurrent
Cabal consumer. The stronger follow-up this paragraph names — a small prebuilt
codec-helper executable so the probe becomes a pure artifact consumer — is
what issues #2273 and #2274 delivered: `exe:synarchy-save-codec`, whose
`compare` operation the probe execs, resolved once by the runner's preflight
beside the engine. The probe is a build-state reader now, declares nothing
exclusively, and runs beside the rest of a `--jobs 2` sweep. Retrying a Cabal
race alone was never accepted isolation: an
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

### Headless Hspec lanes (proposal)

The headless suite is the largest single cost and it runs its examples one at
a time. Rather than make examples concurrent inside one process, split
the suite into a small number of **lanes**, each a separate process on its own
runner, that together execute every example exactly once.

- **The shared-world block stays whole in one lane** (D-3). Its examples share
  one `EngineEnv` and memoized worlds; putting a `(seed, size, plates)` key's
  consumers in two lanes would generate that world twice. If CIR-14 shows the
  block alone is too long for the target, it may be split only along world-key
  affinity: every consumer of a given key stays in the same lane, so no world is
  generated twice. `(42,64,3)` alone has at least ten consumer modules, so that
  key's lane is the floor on how short the suite can get this way.
- **Everything else is balanced across the remaining lanes** by measured
  duration from CIR-14, not by example count.
- **Coverage is partitioned by construction.** The lane a new top-level group
  lands in must be determined without anyone remembering to list it — for
  example, lane A selects a named wrapper `describe` and lane B `--skip`s the
  same name, so a new group outside the wrapper can only land in B. A check
  compares the per-lane example lists against the unpartitioned suite (Hspec
  `--dry-run`) and fails on any example that is missing or runs twice.
- **Running without a lane selector is unchanged.** A developer's `cabal test`
  and `make ci` still run the whole suite in one process; lanes are a CI
  scheduling concern and `tools/ci_parity_audit.py` keeps treating the lanes'
  union as the one headless gate.
- **Each lane is its own job under the stable `build-test` aggregate,** so
  branch protection keeps one required check (as CIR-5 already proposed for
  gate families).

D-14 settles how each lane job obtains the test executable: it builds it
from the shared cache. The comparison that decided it: artifact handoff (one
build, then consumers) costs a job boundary on the critical path: a job with
`needs:` starts only after its producer finishes, then pays container start
(≈49 s median), checkout and an artifact download of the test executable
(330 MB on a local macOS build; the Linux size is unmeasured) before its first
example. Building in every lane from the same
restored project cache — what the `behavior-probes` job already does for
`exe:synarchy` — starts every lane at time zero, at the price of duplicated
compile time on free runners and more concurrent jobs.

Rough arithmetic at today's medians, before CIR-14 replaces the estimates: a
shared-world lane of about 9–10 minutes (its top-twenty items alone are about
6.5 minutes, 7.5 with the full-tier volcano case), a build of about 10 minutes, and 1.5 minutes of job
start. Per-lane build: about 21 minutes. Handoff: about 23–24 minutes. So lanes
alone do not reach the twenty-minute target at today's median build; the
build side (CIR-17 and the choices it informs) has to move as well.

### Build cost inside a cache epoch (proposal)

CIR-4's epoch bounds how stale the project cache can get, but not how much a
PR recompiles: runs with exact project-cache hits range from 1.5 to 12.6
minutes of build, and three exact hits at the same epoch position (52, 3/8)
each took over ten. Before choosing a remedy, attribute that time:
how much is replaying the epoch's earlier master changes (fixable by writing a
snapshot more often, a D-6 change) and how much is the change under test
itself (fixable only by making recompilation cheaper, such as a lower
optimisation level for the test suite, which #2276 measured and owner scope
excluded). CIR-17 produces that attribution; the remedy it points to is a
decision (Q-14), not something CIR-17 adopts on its own.

### Prove safe Hspec parallel regions

This is now the follow-up to lanes rather than the first move. In-process
`parallel` cannot touch the shared-world block, and on a 4-vCPU runner world
generation already uses every capability (`-with-rtsopts=-N`), so its
remaining value is inside the non-worldgen lanes once they exist.

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

Since this section was written, #2272 moved the static audits into their own
job and the probe lane already runs beside the main worker, so the gate
families it lists are mostly independent already; the one serial giant left is
the headless suite, which the lanes above address. What remains for this
section is the post-build tail of `test-and-audits` — save compatibility
(≈30 s) and `world_check --quick` (≈94 s when selected). D-14 chose per-lane
builds, so most of the handoff machinery below is unnecessary for the lanes and
CIR-5 shrinks to running that tail beside them. The text below is retained as
the plan should a later decision reintroduce handoff.

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

### Native merge queue (proposal, not yet adopted)

The queue keeps one contributor-facing destination: `master`. A pull request
still receives its ordinary review and PR CI. Once its approval label and
initial required checks are satisfied, the drainer or a maintainer adds it to
the queue instead of updating its source branch and merging it immediately.
GitHub then owns the changing-base problem.

Suppose PR A and PR B are both ready while `master` is at M. GitHub creates one
temporary merge group for `M + A`, and a later cumulative group for `M + A +
B`. With build concurrency of at least two, both exact candidate trees can be
checked concurrently. If both pass, A and B can advance through the queue
without B's source branch being rewritten after A lands. If A's group fails,
A is removed; GitHub regenerates B's candidate as `M + B` and checks that new
tree before allowing B to merge. This preserves combined-tree coverage while
moving invalidation and recovery out of the contributor workflow.

This improves **merge throughput**, not the duration of one CI execution. With
the current strict branch and single-lane drainer, two already-green PRs can
pay roughly one additional twenty-minute update-and-revalidate turn each in
series. A queue with two concurrent group builds can validate the two
cumulative candidates during one approximately twenty-minute wave, subject to
runner availability. Each PR still normally pays its original PR CI and a
merge-group CI, so queue adoption increases runner work unless later evidence
justifies safely avoiding duplication.

Synarchy must make these changes before enabling the queue:

1. Add the `merge_group`/`checks_requested` event to the main workflow and run
   every queue-required executable check on the merge-group SHA. The current
   PR-only behavior-probe condition would otherwise omit a required check.
2. Resolve the `review-approved` contract. That required workflow currently
   handles only `pull_request`; a merge group will not receive the check unless
   the workflow gains a safe group-aware form or the label check becomes an
   admission condition while a different executable aggregate protects groups.
3. Treat merge groups as cache consumers, not writers. They can restore a
   compatible default-branch cache; successful `master` pushes remain the
   writer and retain the complete post-merge backstop from D-2.
4. Change the drainer from a one-at-a-time update-and-merge controller into an
   admission controller: verify freshness of approval and ordinary PR checks,
   enqueue the PR, then release its lane while GitHub constructs and validates
   the group. Queue incidents and ejections still need visible ownership.
5. Transfer the public user-owned repository to a GitHub organization, because
   native merge queues are not available to this repository's current owner
   shape. Repoint and verify the repository-scoped drainer, local remotes,
   Actions permissions, and the GHCR CI-image/package relationship before the
   new identity becomes the production path.

Initial activation should be deliberately small: build concurrency two, a
small merge limit, only non-failing pull requests allowed to merge, and the
existing complete `master` run retained. Measure approved-to-merge time, group
rebuild count, ejections, queue occupancy, runner minutes, cache outcomes, and
post-merge failures before widening it. Exact configuration values remain a
maintainer choice at activation time.

### Repository transfer migration (proposal)

The transfer and the merge-queue activation are separate changes. The
repository should first operate normally under its organization identity with
the existing merge policy. Only after that state passes CI and automation
verification should the queue be required for `master`.

#### 1. Create and prepare the dedicated organization

- Recheck and create the dedicated GitHub Free organization `synarchy-game`,
  producing the canonical repository URL
  `github.com/synarchy-game/synarchy` and active image namespace
  `ghcr.io/synarchy-game/synarchy-ci`. Existing organizations are not
  candidates. GitHub exposes merge queues for any organization-owned public
  repository, so no paid plan is required for this public repository. If the
  slug cannot be created, stop for a replacement decision.
- Make `coghex` an organization owner before transfer. Confirm the organization
  permits repository creation/transfer, GitHub-hosted Actions, the pinned
  `actions/*` and `docker/*` actions used here, and organization package
  publication. The destination must not already contain `synarchy` or a fork in
  the same network.
- Keep the repository name `synarchy` during transfer. Renaming at the same time
  adds no queue benefit and expands every identity migration and redirect.

#### 2. Land transfer-readiness before changing ownership

- Make the active CI-image namespace owner-derived rather than hard-coded to
  `coghex`, with a normalized lowercase registry owner. Keep the explicitly
  documented old `LEGACY_IMAGE_REF` unchanged: it describes which historical
  v2 cache objects are compatible, not where new images are published.
- Update canonical Cabal homepage/source/bug URLs, the probe-census schema ID,
  and self-test fixtures that assert the live repository identity. Historical
  run and issue citations may keep the redirected old URL.
- Add a deliberate post-transfer CI entry point, such as `workflow_dispatch`,
  so the organization image and cache can be seeded without inventing an empty
  commit. Prove before transfer that the owner-derived path still resolves and
  publishes correctly as `coghex`.
- Complete CIR-9's merge-group event work, but do not require the queue yet.
  This lets the transferred repository prove ordinary PR/push CI before the
  branch-protection behavior changes.

#### 3. Freeze integration and transfer

- Pick a short maintenance window, stop admitting merges, and wait for active
  Actions runs to finish. Snapshot repository identity, required checks,
  rulesets/branch protection, Actions permissions, secret and environment
  names, open PRs, and the current master SHA for post-transfer comparison.
- Confirm the drainer is idle and has no unresolved obligation. Stop and
  uninstall its `coghex/synarchy` service while `origin` still resolves that
  identity. Its historical runtime records remain on disk by design.
- Transfer the repository from its Settings/Danger Zone without renaming it.
  Do not create a replacement `coghex/synarchy`; preserving the old-location
  redirect is part of compatibility.

#### 4. Rebind and verify before enabling the queue

- Change the shared `origin` URL to
  `git@github.com:synarchy-game/synarchy.git` and verify fetch and push using
  that canonical URL. Existing worktree paths remain in place.
- Compare the transferred repository with the snapshot: owner and visibility,
  master SHA, open PRs/issues/releases, collaborator role, ruleset and strict
  required checks, Actions policy/default token permissions, `NTFY_URL`, the
  `copilot` environment, and workflow history/access. Reauthorize any OAuth or
  connector whose organization policy requires approval.
- Trigger the explicit CI entry point. The new organization image namespace is
  initially empty, so the resolver must build, validate, and publish the first
  `ghcr.io/synarchy-game/synarchy-ci:<content-tag>` image, then consume it in
  both heavy workers. Verify the new package is linked to the transferred
  repository and grants its Actions workflows read/write access. Treat project
  cache reuse as untrusted until observed; the namespace change deliberately
  prevents the old image-specific project objects from being mistaken for new
  ones.
- Reinstall and start the drainer against the new `origin`; its discovery key,
  reported repository, launchd label, and runtime namespace must respectively
  resolve as `synarchy-game/synarchy`, `synarchy-game/synarchy`,
  `com.coghex.drain-prs.synarchy-game.synarchy`, and
  `synarchy-game.synarchy`. The old personal GHCR package and old drainer
  runtime records remain untouched until the new path has operated
  successfully.
- Run one ordinary disposable PR through the unchanged merge policy. Only then
  begin CIR-12's manual queue pilot; CIR-13 automates admission after the hosted
  queue behavior is proven.

The expected disruption is a temporary merge freeze and one cold organization
image publication, not loss of issue/PR history or a need to recreate branches.
Normal old Git URLs redirect during the migration, but automation stays paused
until it has been verified against the new canonical identity.

### Lieutenant integration branches (rejected alternative)

The Linux-kernel-style alternative would route ordinary pull requests to
long-lived subsystem branches, then gate separate promotions from those
branches into `master`. Ephemeral batch branches are a lighter variant, but
they have the same semantic question: is the contribution complete at the
intermediate merge or only after final promotion?

The user rejected this direction for the present use case. If only `master`
counts as complete, lieutenant branches add routing, ownership, promotion CI,
branch drift, and failed-convergence recovery while moving rather than removing
the final wall. If an intermediate branch counts as complete, they materially
change the project's completion and release model. A production version would
also require multi-base Kanban lanes, generalized branch protection and cache
writing, and exact combined-commit gates. The native queue retains the desired
single-`master` model and delegates those transient integration branches to
GitHub, so permanent or manually operated lieutenant branches will not be
piloted unless this decision is explicitly revisited.

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

Amended by D-14 for the headless lanes, which each build from the shared
immutable cache instead of consuming one handed-off executable.

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

### D-9. Keep one integration branch and pursue hosted merge groups

Do not introduce permanent lieutenant branches or a manual ephemeral-branch
pilot for this use case. Continue designing around one protected `master` and
GitHub's native merge queue, subject to an explicitly approved organization
transfer and a safe merge-group approval contract. This targets the actual
single-lane throughput wall without redefining an intermediate branch as
contributor completion.

### D-10. Accept an organization transfer in principle

The repository may move from the personal `coghex` account to an organization
to unlock GitHub's native merge queue. This approves continued design and
preparatory work, not an immediate transfer: the transfer-readiness change,
merge-group approval contract, and explicit maintenance-window approval remain
gates before ownership changes.

### D-11. Create a dedicated organization for Synarchy

Do not place Synarchy in either existing organization membership. Create a new
GitHub Free organization dedicated to the project, with `coghex` as owner. This
keeps repository, Actions, package, and membership policy under project control
and prevents unrelated organization governance from becoming a CI dependency.
The organization identity is fixed by D-12.

### D-12. Use `synarchy-game` as the organization slug

Create the dedicated organization as `synarchy-game`. After transfer, the
canonical repository is `github.com/synarchy-game/synarchy` and the active CI
package is `ghcr.io/synarchy-game/synarchy-ci`. If GitHub refuses the slug when
creation is attempted, stop and ask for a replacement rather than modifying or
suffixing it without approval.

### D-13. Lanes first, with the 2026-09-28 slice plan

Approved 2026-09-28. The headless suite is split into CI lanes before any
in-process parallelism: CIR-14 measures, CIR-15 partitions, CIR-16 runs the
lanes as jobs; CIR-17 investigates the build; CIR-18 adds an advisory lane
budget; CIR-8 and CIR-5 follow the lanes in their re-scoped forms. The lane
slices do not wait for CIR-3: hang diagnosis proceeds independently, and lanes
confine a hang to one lane without changing how it behaves. Resolves Q-15.

### D-14. Each headless lane builds its own test executable

Approved 2026-09-28. Every lane job restores the same dependency and project
caches and builds `synarchy-test-headless` itself, so all lanes start at time
zero; there is no artifact handoff for the lanes. This amends D-5 for the
headless lanes only: each lane's build tree is its own, restored from an
immutable cache, and is never shared with or copied to another concurrent
consumer. The successful master push remains the only project-cache writer.
The duplicated compile time is accepted under D-7 (free runner minutes for this
public repository); the concurrent-job count is reported by CIR-16. Resolves
Q-13.

### D-15. The twenty-minute target is the median PR run

Approved 2026-09-28. The arc's latency target is a median run wall time under
twenty minutes for successful pull-request runs, as reported by
`tools/ci_timing_report.py` over a stated sample. Slow-build outliers above
twenty minutes do not by themselves fail the target. It is an optimization
target, not a test timer or a failing gate. Resolves Q-1.

## Open questions

### Q-1. What latency and runner-minute budgets define success?

Resolved by D-15: median run wall time of successful PR runs under twenty
minutes. The history below is kept.

Partially answered 2026-09-28: the maintainer wants pull-request CI under
twenty minutes without losing significant coverage (today: median 31m00s, p95
36m32s over seventeen successful PR runs). Still open: which statistic the
twenty minutes applies to — median, 95th percentile, or every non-hung run —
and whether it is measured as `test-and-audits` job time or run wall time. The
difference matters: the build alone ranges from 1.5 to 12.6 minutes today, so a
median target is reachable with lanes plus modest build work, while a p95
target needs the build tail fixed too.

The earlier proposal was PR median at or below 10 minutes, PR 95th
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
tests that are intentionally parallelizable later. Since #1916 it must also
keep a passing run's log to a handful of lines: restoring one line per passing
example is ruled out by that issue, which is why the 2026-09-24 hang left no
record of its active example. CIR-3 may choose the
smallest reliable diagnostic that preserves the existing output and scheduling
contracts; it must stop for a maintainer decision if useful diagnostics require
changing either contract.

### Q-6. Which Hspec groups are both safe and worth parallelizing?

Re-scoped 2026-09-28: CIR-8 now follows the lanes (CIR-16) and asks the
question only inside the non-shared-world lanes, where it can still shorten a
lane that turns out to be the longest. The framework capability is verified,
but the repository boundary is not. CIR-8 must inventory fixture and process
ownership, measure candidate groups at
`--jobs=1/2/...`, and leave any uncertain group sequential. If no safe group
materially shortens the critical path without duplicating shared worldgen,
CIR-8 records that result rather than forcing a parallel implementation.

### Q-7. Are the built executables self-contained enough for artifact handoff?

The producer and consumers use the same immutable container image, which makes
handoff plausible, but the Linux binary's dynamic-library requirements and the
direct Hspec runner's runtime environment have not yet been exercised as a
downloaded artifact. D-14 chose per-lane builds for the lanes, so this
question only needs answering if a later decision reintroduces handoff for
the lanes or for CIR-5's post-build tail. `tools/world_audit.py` (behind `world_check.py`) launches the
engine as `cabal run exe:synarchy`, so a clean-checkout consumer of it also
needs an explicit executable override like the probes' `SYNARCHY_PROBE_ENGINE_EXE`. CIR-5 stops after the spike and reports the blocker if the
minimal binary bundle cannot run without copying Cabal's build database.

### Q-8. What event counts as a completed contribution under a lieutenant model?

Resolved by D-9: completion remains integration into `master`.

### Q-9. How should lieutenant branches converge and be trusted?

Resolved by D-9: hosted merge groups replace project-operated convergence
branches, and GitHub gates the exact cumulative candidate before `master`.

### Q-10. Is transferring the repository to an organization acceptable?

Resolved by D-10. The user accepts the transfer in principle; CIR-11 retains an
explicit stop before the actual ownership change.

### Q-11. How should approval apply to a merge group?

The current `review-approved` required check proves a label on one pull-request
event, while a `merge_group` event represents a cumulative temporary ref. The
design must either re-verify that every included PR still carries fresh
approval, or make approval a strictly enforced queue-admission condition and
require a merge-group-specific executable aggregate. CIR-9 must establish from
the event/API data that the chosen contract fails closed before changing branch
protection.

### Q-12. Which organization should own Synarchy?

Resolved by D-11 and D-12: ownership will be the new dedicated GitHub Free
organization `synarchy-game`. Availability is rechecked at creation; refusal
stops for a new decision.

### Q-13. How does each headless lane get the test executable?

Resolved by D-14: per-lane builds. The comparison below is kept.

Two options, both keeping one cache writer (the successful master push):

- **Per-lane build.** Every lane job restores the same dependency and project
  caches and runs `cabal build synarchy-test-headless` itself. All lanes start
  at time zero; no artifact, no Q-7 spike, and the pattern already exists in
  `behavior-probes`. The costs are duplicated compile time (free runner minutes
  for this public repository) and more concurrent jobs against the Free plan's
  twenty. It departs from D-5's "build once" wording, so choosing it amends D-5
  for lanes: each lane's build tree is its own, restored from an immutable
  cache, never shared or copied between concurrent consumers.
- **Artifact handoff.** One producer builds and uploads the executable; lane
  jobs `needs:` it. It matches D-5 as written and spends the least compute, but
  adds a job boundary — roughly two to three minutes of upload, container start
  and download — to the critical path, and needs Q-7 answered first.

The earlier arithmetic favours per-lane builds by those two to three minutes.
CIR-16 stops for this decision before changing the workflow.

### Q-14. Which build-side lever, if any, should follow CIR-17?

Known candidates: (a) write the project-cache snapshot more often than every
eight build-relevant master changes, which revises D-6 and trades cache storage
(ten gigabytes per repository) and eviction churn for less replayed drift;
(b) compile the test suite (not the library) at `-O0` or `-O1` in CI, which
#2276 measured at −46 % or −16.6 % test-suite compile CPU but which that issue's
owner specification excluded, and whose effect on Hspec run time is unmeasured
(the heavy worldgen code lives in the `-O2` library); (c) neither, if CIR-17
shows the PR's own diff dominates and lanes plus the other slices already meet
D-15's target. No build-side change is designed until CIR-17 reports.

### Q-15. Do the lane slices wait for CIR-3?

Resolved by D-13: they do not. The reasoning below is kept.

The old CIR-5 and CIR-8 depended on CIR-3. This revision proposes that lanes
(CIR-14 through CIR-16) do not: lanes do not change how a hang behaves, they
confine it to one lane and let the others report, and holding the largest
latency win behind an unscheduled diagnosis would stall the arc. CIR-3 keeps
its own place in the plan and gains the 2026-09-24 evidence. If the maintainer
prefers the original ordering, CIR-15 gains a dependency on CIR-3.

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
- For lanes, prove on every run that the lanes' example lists are disjoint and
  that their union equals the unpartitioned suite's `--dry-run` list, that the
  summed example count matches, and that no shared-world key is initialised in
  more than one lane. Compare the lanes' slowest wall time with the old
  single-process step over repeated PR and master runs, and report lane skew so
  a lane that outgrows the others is visible.
- For the build, record per run the epoch position, cache outcome and build
  seconds, so CIR-17's attribution can be recomputed rather than trusted.
- For Hspec, establish a `--jobs=1` baseline, mark only audited candidate
  groups, then compare `--jobs=2` and the runner-appropriate cap across repeated
  runs. Example counts and coverage stay identical; the shared-world generation
  count must not increase, and fixture/state failures or output races reject
  the candidate boundary.
- Compare wall-clock critical path and total runner minutes before and after
  each slice. A wall-time improvement that causes unbounded cost or materially
  higher flake/retry rates does not pass.
- Before queue activation, use disposable PRs to prove that two cumulative
  merge groups run concurrently, a failing first change is ejected, the second
  group's replacement is revalidated, every required check reports on the
  group SHA, and the complete post-merge `master` backstop still runs.
- Keep workflow YAML, local `make ci` coverage, and repository testing docs in
  sync. The local gate need not reproduce cloud parallel scheduling.

## Delivery plan

### CIR-1. Publish durable CI timing and selection diagnostics

> Delivered: the cache half as #1358 (`tools/ci_cache_report.py`), the timing
> half as #2277 (`tools/ci_timing_report.py`, run on demand rather than as a
> per-run workflow summary, plus `--print-slow-items=20` in both CI Hspec
> invocations). A per-run summary was not built; the lane slices report their
> own durations instead.

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

> Delivered as #2273 in its stronger form: the compiled
> `exe:synarchy-save-codec` replaced every `cabal repl` in that family.

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

### CIR-7. Isolate parallel probes behind one prebuilt executable

> Delivered as #1570 (commit 0ffc10df8: `tools/probe_engine.py`, the
> runner-resolved `SYNARCHY_PROBE_ENGINE_EXE`), with follow-ups #2274
> (persistence_contract through the prebuilt codec, no exclusive hold, no test
> suite in the probe job) and #2275 (longest-first dispatch). The per-worker
> ephemeral resource worktrees in this slice's scope were not built;
> `tools/run_probes.py` creates none.

- **Outcome:** The full CI-eligible probe selection can use multiple workers
  without any worker mutating a shared Cabal build tree or paying retry tax for
  an infrastructure race.
- **Scope:** An explicit executable override for probe boot, ephemeral worker
  worktree/resource-root lifecycle, unique per-worker ports and outputs, and an
  exclusive boundary for the remaining persistence-contract GHCi consumer.
  That last item was the interim answer. Issue #2274 removed the consumer
  instead: the structural comparison became `compare` in the compiled
  `exe:synarchy-save-codec` (#2273), the preflight resolves that binary
  beside the engine and hands both down, `persistence_contract` dropped its
  exclusive `cabal-build` declaration, and the `behavior-probes` job stopped
  building `synarchy-test-headless`.
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

> No separate issue: delivered directly as commit 7b3757ded (2026-08-22,
> `tools/ci_cache_epoch.py`, `tools/ci_cache_cleanup.py`). CI logs report it as
> `CI_CACHE_EPOCH epoch=… position=…/8`.

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

### CIR-14. Measure headless Hspec time per top-level group on CI

- **Outcome:** A recorded, reproducible table of how the headless suite's CI
  run time divides across its top-level groups, the shared-world block's
  members, and each shared-world key's first generation, together with a
  proposed lane count and partition and the predicted longest lane.
- **Scope:** A measurement taken on the real CI runner (for example a
  measurement-only run with `--print-slow-items` set high enough to list every
  item, or an equivalent per-group timing hook), repeated on at least three
  runs to expose variance, aggregated by top-level group and by shared-world
  key, and written up under `docs/` in the manner of #2276's report. The
  proposed partition keeps every consumer of a shared-world key in one lane.
- **Phase:** 2 — measure the headless suite
- **Depends on:** `none`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-3, D-13, D-15
- **Acceptance signals:** The per-group totals account for the run's `Finished
  in` time to within a few percent; each shared-world key's generation cost is
  identified; the proposed partition states its predicted longest lane and the
  evidence behind it; the aggregation is repeatable by the next agent from the
  recorded commands.
- **Out of scope:** Permanently raising CI log volume (see #1916), changing
  `Spec.hs`, workflow jobs, or which tests run.
- **Open questions:** `None` (the partition is sized against D-15's target).

### CIR-15. Partition the headless suite into lanes that run every example exactly once

- **Outcome:** The headless test executable can run one named lane at a time.
  The lanes are pairwise disjoint and their union is the whole suite, each
  shared-world key's consumers sit in exactly one lane, and running with no
  lane selected is unchanged for developers and `make ci`.
- **Scope:** Lane selection in `test-headless/Spec.hs` following CIR-14's
  partition; a partition-by-construction rule so that a new top-level group
  cannot be silently omitted from every lane; a self-test comparing per-lane
  `--dry-run` example lists against the unpartitioned list; testing-document
  updates in the same PR.
- **Phase:** 2 — prepare lanes
- **Depends on:** `CIR-14`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-3, D-8, D-13
- **Acceptance signals:** The per-lane lists are disjoint and their union
  equals the unpartitioned list; per-lane example counts sum to the whole; each
  lane passes when run alone; no shared-world key is initialised in more than
  one lane; `cabal test synarchy-test-headless` with no selector reports the
  same example count as before the change.
- **Out of scope:** Workflow changes (CIR-16), in-process `parallel` (CIR-8),
  and changing, removing or demoting any example.
- **Open questions:** `None`

### CIR-16. Run the headless lanes as parallel CI jobs

- **Outcome:** Pull-request and master CI run each headless lane as its own job
  under the stable `build-test` aggregate, and the critical path shrinks by the
  measured difference between the old single-process step and the longest lane.
- **Scope:** One workflow job per lane, each restoring the shared caches and
  building the test executable itself (D-14);
  the worldgen full-tier variable reaching whichever lane holds full-tier
  examples; one project-cache writer; `build-test` requiring every lane;
  CIR-15's coverage check running in CI; `tools/ci_parity_audit.py` and
  `tools/ci-local.sh` treating the lanes' union as the one headless gate (local
  `make ci` may still run the suite in one process); `tools/ci_timing_report.py`
  recognising lane jobs; and a before/after measurement.
- **Phase:** 3 — structural parallelism
- **Depends on:** `CIR-15`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2, D-3, D-5, D-7, D-13, D-14, D-15
- **Acceptance signals:** Every run proves lane coverage parity and a summed
  example count equal to the unpartitioned suite on the same commit; no shared
  world is generated twice; over at least ten PR runs, the longest lane's
  median is reported against the pre-change `Headless test suite` median of
  1035 s; runner-minute growth and peak concurrent jobs are reported; branch
  protection still requires only `build-test`.
- **Out of scope:** Changing which examples run on a PR, in-process `parallel`,
  and moving save compatibility or `world_check` (CIR-5).
- **Open questions:** `None` (Q-13 resolved by D-14; Q-7 does not apply to
  per-lane builds).

### CIR-17. Attribute pull-request build time to epoch drift versus the change under test

- **Outcome:** A measured account of why builds with exact project-cache hits
  range from 1.5 to 12.6 minutes: for a sample of recent PR and master runs,
  how many library and test-suite modules each recompiled, how much of that
  replays the epoch's earlier master changes versus the run's own diff, and the
  projected build time under a per-master-push snapshot and under a lower
  test-suite optimisation level.
- **Scope:** An investigation in the manner of #2276. CI builds run with
  `-v0`, so module counts come from reproducing sampled builds in the CI image
  from the epoch snapshot's commit forward. The report recommends one Q-14
  option (or none) with its projected effect on median and 95th-percentile
  build time.
- **Phase:** 2 — measure the build
- **Depends on:** `none`
- **Ordering:** `independent`
- **Relevant decisions:** D-1, D-6, D-13, D-15
- **Acceptance signals:** Each sampled run's recompiled-module count and build
  seconds are split between drift and diff; the recommendation's projection
  is stated; the reproduction commands are recorded.
- **Out of scope:** Changing the epoch length, optimisation flags, or the
  workflow; those follow a Q-14 decision as their own slice.
- **Open questions:** Q-14

### CIR-3. Reproduce and localize headless-suite hangs without timers

> Still open, with new evidence: the headless step hung on ten runs between
> 2026-08-20 and 2026-09-24 (nine master pushes, one PR; listed under the
> re-measurement), each cancelled by the 90-minute job timeout. The
> 2026-09-24 hang left only incidental `READY port=` lines to place it (see
> Q-5).

- **Outcome:** Repeated hangs can be compared and localized to the last active
  test context without imposing a suite-level or per-example timer.
- **Scope:** Same-commit rerun comparison, flushed progress/context that keeps
  #1916's quiet passing log, and a narrowing procedure for local Hspec
  reproduction. Once lanes exist, per-lane context is enough to start.
- **Phase:** 2 — diagnose pathological runs
- **Depends on:** `CIR-1`
- **Ordering:** `independent`
- **Relevant decisions:** D-4
- **Acceptance signals:** A repeated hang can be narrowed to a describe/example
  or lifecycle boundary from the CI log alone; a passing run's log stays a
  constant handful of lines per lane; ordinary Hspec runtime and verdict are
  unchanged.
- **Out of scope:** Fixing the product/test deadlock that triggered any one
  specific hung run.
- **Open questions:** Q-5 (Q-15 resolved by D-13: the lane slices do not wait
  for this one).

### CIR-18. Report headless lane durations against an advisory budget

- **Outcome:** Every CI run's summary shows each lane's duration, example count
  and slowest items against an advisory per-lane budget, and flags a lane that
  exceeds it, without failing the run. Growth like the 265 s added between
  2026-09-24 and 2026-09-27 becomes visible on the PR that causes it.
- **Scope:** `$GITHUB_STEP_SUMMARY` output from the lane jobs or the
  aggregate, budget values in one checked-in place, and a documented response
  to an over-budget lane (rebalance, add a lane, or make the expensive example
  cheaper).
- **Phase:** 3 — keep it fast
- **Depends on:** `CIR-16`
- **Ordering:** `not on the critical path`
- **Relevant decisions:** D-1
- **Acceptance signals:** A run with a lane forced over budget shows the flag
  and still passes; lane durations and top items are readable without opening
  raw logs.
- **Out of scope:** Failing CI on duration (D-15 makes the target an
  optimization target, not a gate; changing that needs a new decision) and
  automatic rebalancing.
- **Open questions:** `None`

### CIR-8. Prove and adopt only safe in-process Hspec parallel regions

> Deferred 2026-09-28: CIR-8 only shortens the critical path when a lane
> without the shared-world block is the longest. Before lanes exist the
> shared-world block alone is at least 6.5 minutes while the rest of the suite
> (about 10 minutes over ~10,000 cheap examples) can be balanced across lanes
> by #2744, so the likely result today is a recorded no-win. Revisit once
> #2745's lane timings show a non-shared-world lane is the longest; if they
> show the shared-world lane stays longest, this slice becomes `[no-issue]`.

- **Outcome:** Inside the lanes that do not hold the shared-world block,
  audited independent Hspec groups run concurrently when that materially
  shortens the longest lane, or the repository records a measured no-win
  result instead of forcing unsafe parallelism.
- **Scope:** Fixture/process ownership inventory for the candidate lanes,
  candidate `parallel` annotations, and repeated `--jobs=1`, `2`, and
  runner-cap measurements.
- **Phase:** 4 — tune lanes
- **Depends on:** `CIR-16`
- **Ordering:** `not on the critical path`
- **Relevant decisions:** D-1, D-3, D-8
- **Acceptance signals:** Example counts and verdicts remain identical across
  repeated runs; shared-world generation count does not increase; no mutable
  fixture/state races appear; retained parallel boundaries show a material
  wall-time improvement on the longest lane.
- **Out of scope:** A root-level `parallel`, `parallel` inside the shared-world
  block, duplicating canonical world generation, or making Hspec
  path-selective.
- **Open questions:** Q-6

### CIR-5. Hand prebuilt executables to the remaining post-build gates

> Re-scoped 2026-09-28. #2272 already split the static audits off, the probe
> job already runs beside the main worker, and CIR-16 handles the headless
> suite. What is left is the serial tail after the build.

> Deferred 2026-09-28: #2745 runs every lane in its own job and removes the
> single-process headless step, so save compatibility and `world_check
> --quick` should end up running beside the lanes rather than after one.
> Revisit when #2745 merges: if either still follows a headless lane in the
> same job on the critical path, file this slice; otherwise it becomes
> `[no-issue]` (delivered by #2745).

- **Outcome:** Save compatibility and `world_check --quick` no longer run
  serially after the build in the job that also carries a headless lane, so
  neither extends the critical path once lanes exist.
- **Scope:** Running that tail beside the lanes — as its own job or inside the
  lane with the most slack — building as the lanes do (D-14);
  an explicit executable override for `tools/world_audit.py`'s `cabal run
  exe:synarchy` launch and for the save-codec lookup where a consumer has no
  build tree; the `build-test` aggregate; and a measured critical-path and
  runner-minute comparison.
- **Phase:** 4 — tune lanes
- **Depends on:** `CIR-16`
- **Ordering:** `not on the critical path`
- **Relevant decisions:** D-1, D-2, D-5, D-7
- **Acceptance signals:** Identical gate coverage and verdicts; the tail no
  longer appears after the build on the critical path; bounded artifact or
  duplicate-build overhead; no material retry/flakiness regression.
- **Out of scope:** Parallel execution of tests that share mutable engine state
  without isolation, uploading the complete `dist-newstyle` tree, or changing
  what `world_check` or save compatibility verify.
- **Open questions:** Q-7 only if a later decision reintroduces handoff

### CIR-6. Reassess full-suite and behavior-probe selection from measured coverage

> Deferred 2026-09-28: this slice decides whether structural wins are enough
> before anything about PR test selection changes (Q-2). That evidence exists
> only after the lanes (#2745) and the build-side change #2746 recommends. If
> the measured median successful PR run is then under twenty minutes (D-15),
> the unconditional suite stays and this slice becomes `[no-issue]`; otherwise
> draft it with that measurement.

- **Outcome:** Either retain the unconditional PR Hspec policy with evidence
  that structural wins are sufficient, or adopt a fail-closed, self-tested
  path-to-test-group selector with a full master backstop.
- **Scope:** Coverage/ownership map, test grouping, selector self-tests, and
  documented stop/ask behavior for unclassified paths.
- **Phase:** 4 — policy optimization
- **Depends on:** `CIR-1`, `CIR-16`, `CIR-17`
- **Ordering:** `not on the critical path`
- **Relevant decisions:** D-1, D-2, D-3, D-8
- **Acceptance signals:** A measured decision; if selection changes, every path
  has an explicit result, unclassified paths fail closed, and master continues
  to run the complete suite.
- **Out of scope:** Demoting tests based only on their current duration.
- **Open questions:** Q-2

### CIR-9. Make required CI checks merge-group aware

- **Outcome:** A temporary merge-group ref receives a complete, fail-closed set
  of required checks against its own SHA without writing project caches or
  weakening the existing PR approval requirement.
- **Scope:** `merge_group` workflow triggers, explicit event classification,
  behavior-probe and aggregate-check semantics, approval admission/revalidation,
  cache restore-only behavior, queue diagnostics, and focused event-fixture
  tests. Include a dry-run design for changing the drainer from merge execution
  to queue admission.
- **Phase:** 3 — prepare hosted integration
- **Depends on:** `CIR-1`
- **Ordering:** `must land before queue activation; can proceed before transfer`
- **Relevant decisions:** D-1, D-2, D-7, D-9
- **Acceptance signals:** Synthetic pull-request, push, and merge-group payloads
  select the intended gates; every branch-protection check reports on the group
  SHA; approval fails closed; merge groups cannot save caches; ordinary PR and
  complete `master` behavior remain unchanged.
- **Out of scope:** Enabling the queue, transferring the repository, reducing
  coverage, or assuming that enqueue success means the group will merge.
- **Open questions:** Q-11

### CIR-10. Make repository identity transfer-ready

- **Outcome:** The current personal-account repository continues to pass CI
  while every live publication and canonical-metadata path is ready to resolve
  through `synarchy-game` after transfer.
- **Scope:** Owner-derived lowercase GHCR namespace, explicit post-transfer CI
  dispatch, canonical Cabal/schema URLs, identity-sensitive self-test fixtures,
  and transfer preflight/snapshot documentation. Preserve historical URLs and
  the old compatibility image constant deliberately.
- **Phase:** 3 — prepare ownership migration
- **Depends on:** `none`
- **Ordering:** `can land before CIR-9; must land before CIR-11`
- **Relevant decisions:** D-1, D-2, D-9, D-10, D-11, D-12
- **Acceptance signals:** CI still publishes and consumes the current personal
  package through the owner-derived path; tests exercise an owner change; the
  manual dispatch runs the ordinary complete gate; an audit distinguishes live
  identity references from historical redirected evidence.
- **Out of scope:** Transferring the repository, rewriting historical links,
  deleting the personal GHCR package, or enabling the merge queue.
- **Open questions:** `None`

### CIR-11. Transfer the repository and rebind existing automation

- **Outcome:** Synarchy operates normally at `synarchy-game/synarchy` with its
  repository history and protections intact, a newly owned CI package seeded,
  and the existing drainer installed under the new canonical identity.
- **Scope:** Explicit maintenance approval; merge freeze and state snapshot;
  old drainer stop/uninstall; repository transfer without rename; shared remote
  update; repository/Actions/secret/environment/protection comparison; first
  organization image publication; connector authorization; new drainer
  install/start; and one ordinary-merge smoke PR.
- **Phase:** 4 — migrate ownership
- **Depends on:** `CIR-9`, `CIR-10`
- **Ordering:** `blocked on Q-11; stop again immediately before transfer`
- **Relevant decisions:** D-1, D-2, D-9, D-10, D-11, D-12
- **Acceptance signals:** Master SHA and open tracker/PR state are preserved;
  old URLs redirect; fetch/push uses the new remote; required checks and secrets
  match the snapshot; CI publishes and consumes the organization package; the
  new identity-keyed drainer reports healthy; an ordinary PR merges under the
  pre-queue policy.
- **Out of scope:** Recreating `coghex/synarchy`, deleting old package/runtime
  evidence, enabling the queue during the transfer, or renaming the project.
- **Open questions:** Q-11

### CIR-12. Pilot the native merge queue manually

- **Outcome:** Two or more approved PRs are validated as cumulative merge
  groups concurrently and reach one protected `master` without contributor
  branch rewrites after each preceding merge.
- **Scope:** Temporary manual admission with the drainer stopped, branch queue
  settings, build concurrency two, a small merge limit, disposable green and
  controlled-failure PRs, metrics, incident ownership, and rollback to the
  verified pre-queue branch protection.
- **Phase:** 4 — prove hosted integration
- **Depends on:** `CIR-1`, `CIR-9`, `CIR-11`
- **Ordering:** `begin only after the transferred ordinary-merge smoke test`
- **Relevant decisions:** D-1, D-2, D-7, D-9, D-10, D-11, D-12
- **Acceptance signals:** Two cumulative group builds run concurrently; a red
  leading PR is ejected and a trailing candidate is regenerated and rechecked;
  green groups merge in queue order; `master` runs the complete backstop and
  remains the only project-cache writer; disabling the queue restores the
  snapshotted policy.
- **Out of scope:** Unattended drainer admission, removing initial PR CI,
  increasing concurrency beyond the pilot, or changing coverage policy.
- **Open questions:** Q-11

### CIR-13. Convert the drainer into a queue-admission controller

- **Outcome:** Approved PRs enter the proven GitHub queue without one
  repository-wide controller lane remaining occupied through merge-group CI.
- **Scope:** A Kanban-side repository-agnostic enqueue operation, fresh approval
  and initial-check validation, prompt lane release after admission, queue
  status/ejection incidents, safe restart/reconciliation, and retirement of
  direct update-and-merge behavior when the base requires a queue.
- **Phase:** 5 — automate queue operation
- **Depends on:** `CIR-12`
- **Ordering:** `cross-repository follow-up after the manual pilot`
- **Relevant decisions:** D-1, D-7, D-9, D-10, D-11, D-12
- **Acceptance signals:** Several ready PRs can be admitted without waiting for
  an earlier group's verdict; restart does not double-admit or lose ownership;
  ejection is visible and recoverable; non-queue repositories retain their
  existing behavior; Synarchy's primary checkout still satisfies its clean-tree
  and post-merge obligations.
- **Out of scope:** Reimplementing GitHub's group construction, operating
  lieutenant branches, or increasing queue concurrency automatically.
- **Open questions:** `None`
