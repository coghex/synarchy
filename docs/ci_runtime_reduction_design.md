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
- [ ] CIR-7. Isolate parallel probes behind one prebuilt executable
- [ ] CIR-4. Rotate the project cache every eight build-relevant master changes
- [ ] CIR-8. Prove and adopt only safe Hspec parallel regions
- [ ] CIR-5. Run post-build gate families on independent critical paths
- [ ] CIR-6. Reassess full-suite and behavior-probe selection from measured coverage
- [ ] CIR-9. Make required CI checks merge-group aware
- [ ] CIR-10. Make repository identity transfer-ready
- [ ] CIR-11. Transfer the repository and rebind existing automation
- [ ] CIR-12. Pilot the native merge queue manually
- [ ] CIR-13. Convert the drainer into a queue-admission controller

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
  shared fixtures; and several approved PRs can advance through hosted merge
  groups without serial contributor-managed branch updates. Any reduction in
  which tests run on a PR is
  backed by an explicit path-to-coverage contract and retains a full post-merge
  backstop.
- **Users and operators:** Contributors waiting for PR checks, reviewers deciding
  whether a result is trustworthy, and maintainers diagnosing flakes and hangs.
- **Arc label:** `tooling` proposed

## Current state and evidence

### Verified current state

- `.github/workflows/ci.yml` now runs `test-and-audits` and the PR-only
  `behavior-probes` worker in parallel after image resolution, then reports one
  stable `build-test` aggregate. Compilation, the complete headless Hspec suite,
  every static audit, and selected unit-asset/worldgen work remain serial inside
  `test-and-audits`; the existing fan-out therefore removes probe serialization
  without removing the main worker's critical path.
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
- Before queue activation, use disposable PRs to prove that two cumulative
  merge groups run concurrently, a failing first change is ejected, the second
  group's replacement is revalidated, every required check reports on the
  group SHA, and the complete post-merge `master` backstop still runs.
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
