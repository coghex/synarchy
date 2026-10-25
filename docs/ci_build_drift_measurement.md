# How much of a pull request's build replays earlier master changes (#2746)

Measured 2026-10-24 against `master@b86f9fc30`. This is an investigation
report: it records measurements, a validated attribution, a projection and a
recommendation. It changes no behaviour: `.github/`, `tools/ci_cache_epoch.py`,
`synarchy.cabal` and `cabal.project*` are untouched. Part of epic #2742
(CIR-17); builds on [`test_suite_compile_cost_2276.md`](archive/test_suite_compile_cost_2276.md).

**Headline.** The long exact-hit builds are not epoch drift in the sense of
"many master changes piling up". They are *interface cascades*: a change to one
of a handful of hub type modules (`World.Fluid.River.Types`,
`World.ZoomMap.ColorPalette`, `Unit.Command.Types`, …) recompiles 268–532
library-and-executable modules and 312–401 test-suite modules, and a second such
change adds almost nothing because the first already recompiled the same
modules. A run therefore costs either its ~105 s fixed floor, or a full cascade
of 420–780 s; how many master changes sit between the snapshot and the base
matters only through *whether one of them was a hub change*. Writing a
snapshot on every build-relevant master change (`EPOCH_SIZE = 1`) turns the
median sampled run from **9m19s** to **1m53s** of build, but leaves
the 95th percentile at **11m40s**, because a pull request whose *own* diff touches a hub
pays the cascade whatever the snapshot. Compiling the test suite at `-O1` saves 12 % of its
sequential compile CPU at no measurable run-time cost; `-O0` costs 23 % *more*
compile CPU, slows a lane and breaks an example (§6). The recommendation is in §7.

## 0. Corrections to the issue's premises

| Premise | Measured |
|---|---|
| The three exact hits sat at epoch 52, position 3/8 | Only 36320504148 did. 36317483841 was at 2/8 and 36288123692 at **1/8**, and 36288123692's base *is* the snapshot commit: it had **no drift at all**. |
| The snapshot is written by the epoch's first successful master push, at the epoch boundary | Epoch 52's boundary push (`1ccbedd9c`, relevant count 416, run 36054401429) **was cancelled** (attempt 1, 1h30m, `Save project build cache` skipped). The key was first written by the *next* successful push, 417 (`835f9b6a3`, run 36285855248, 2026-09-27 02:02Z), three days later; in between, pull requests at epoch 52 fell back to older snapshots by prefix (the 417 push's own restore matched an epoch-51 key). The cache is seeded by the first eligible successful job that finds the key absent, not by the boundary commit. |
| One snapshot per epoch | One per (image, toolchain, dependency plan, epoch). The plan hash changed at 420 (`8b8481c8f`, PR #2740), so epoch 52 holds two key families (`20edd55a…` for 416–419, `5520c6d3…` from 420). Run 36343120829 is PR #2740 itself: its primary key carries the *new* plan, it missed it, and it prefix-restored the old family's snapshot (417). |
| "Over 17 successful PR runs to 2026-09-28 the median is 182 s / 425 s" | Those two numbers are run 36288123692's own steps. Over the 59 successful pull-request runs from 2026-09-26 the medians are 123 s and 368 s (median of the sums 493 s, 95th percentile 762 s). |
| Lane jobs (#2745) are a future assumption | #2745 merged on 2026-10-02. 144 successful pull-request runs since carry measured lane timings, used in §5 and §7 instead of assumptions. Their median wall time is **28m37s** (95th percentile 35m45s). |
| Image/cache keys | #2872 (Linux CI on Ubuntu 26.04) closed after the sampled runs; it changes the image and cache keys. The historical runs were **not** re-sampled on the new image. |

## 1. Method

1. **Collect** (`tools/ci_build_drift.py collect`): for every successful CI run in a
   window, the `test-and-audits` job's steps, its log's `CI_CACHE_EPOCH`,
   `CI_CACHE_REPORT`, merge and cache-size lines, and every job's duration.
   Reruns are recorded with their attempt; the data are the attempt that
   succeeded. A push whose first attempt was cancelled and later re-run
   (36054401429, the case above) is recorded as attempt 2 with the re-run's
   completion time (2026-10-12), which correctly keeps it from counting as a
   snapshot writer for any sampled run. Writers are ordered by when they
   published, not by when their run was created.
2. **Provenance**: the restored snapshot of a run is the last master push that
   *published* the matched key (its log shows `Cache saved with key`) before the run's restore. Epoch positions, relevant
   counts and tested merges come from the logs; `master_chain` recomputes the
   relevant count of every first-parent commit with `ci_cache_epoch.py`'s own
   build-input globs and agrees with every logged `relevant_count`.
3. **Replay** (`replay`): one worktree (`issue-2746-replay`, macOS arm64, 16
   cores shared with other agents, GHC 9.12.2, cabal-install 3.16.1.0,
   production `-O2`), whose `dist-newstyle` is swapped between `cp -Rpc`
   clones of saved states. Each step restores a state, checks a commit out,
   runs CI's two build commands (`cabal build all`, `cabal build
   synarchy-test-headless`; the graphical suite was skipped in every sampled
   run) at `-v1` and records every `Compiling` line by Cabal component. The
   tested tree is the merge of the run's head into its base, rebuilt with
   `git merge-tree --write-tree` because GitHub's merge commits are not
   fetchable. The first snapshot was a cold build (917 library modules, 13
   executable modules, 514 suite modules, 5m40s); every later state is an
   incremental build.
4. **Attribution**: three builds per run with a base ahead of its snapshot,
   `D` (snapshot → base, "master drift"), `O` (base → tested tree, "own diff") and
   `U` (snapshot → tested tree in one build: what CI did), as sets of
   `(component, module)`. Because a module can compile in both `D` and `O` (or
   in neither of two sequential steps and still in `U`), the counts of the
   sequential builds are never added. Convention, stated once: **a module in
   `U` is drift if `D` recompiled it, otherwise own if `O` did, otherwise
   residual.** The attribution model checked against the unsplit replay is the
   union `D ∪ O` versus `U`, per run and per step group (§2).
5. **Seconds** come from the runs' logs only, as observed step totals. They are
   split by an *estimate*: an ordinary least-squares line of observed seconds on the unsplit module
   count, per step, over the 17 runs. The intercept is the cost that does not scale with
   recompiled modules (cabal planning, `Setup`, linking, restore interplay) and
   the slope the cost of one more recompiled module; both are fitted, not
   observed.
6. **Policies** are projected against the writers' *real* outcomes. Each policy's published
   snapshots are derived first: its key is (image, toolchain, dependency plan, group of N
   relevant changes), published by the earliest-finished successful master job that writes it;
   a job that finishes later publishes nothing. The restore ladder then runs, for the key the run itself requested (a pull
   request can change the dependency plan; its base only fixes the epoch group), over those
   snapshots that finished before the run started: the exact key, else the newest-created key
   of the same plan, else the newest-created key of another plan of the same image and
   toolchain (`restore_snapshot`); another image or toolchain is never restored. For today's
   N = 8 this reproduces the restored snapshot in all 17 runs (the planner refuses to
   continue otherwise), including the cancelled boundary push. Every projected module count is a
   replayed build (`unsplit`, `own`, or a snapshot-to-tested build); only the
   seconds attached to the counts are modelled.

## 2. Results

### Sampled runs

| Run | PR | Hit | Epoch pos. | Snapshot | Base | Attempt | lib+exe s | suites s |
|---|---:|---|---:|---:|---:|---:|---:|---:|
| 36288123692 | #2736 | exact | 1/8 | 417 (`835f9b6a3`) | 417 (`835f9b6a3`) | 1 | 182 | 425 |
| 36317483841 | #2738 | exact | 2/8 | 417 (`835f9b6a3`) | 418 (`1e19e1e86`) | 1 | 229 | 505 |
| 36320504148 | #2739 | exact | 3/8 | 417 (`835f9b6a3`) | 419 (`e7bed43d5`) | 1 | 238 | 519 |
| 36343120829 | #2740 | prefix | 3/8 | 417 (`835f9b6a3`) | 419 (`5de514f96`) | 1 | 224 | 516 |
| 36424074239 | #2741 | exact | 4/8 | 420 (`8b8481c8f`) | 420 (`8b8481c8f`) | 2 | 62 | 91 |
| 36443962564 | #2749 | exact | 5/8 | 420 (`8b8481c8f`) | 421 (`c2270c401`) | 1 | 47 | 54 |
| 36595400637 | #2759 | exact | 7/8 | 420 (`8b8481c8f`) | 423 (`4a3205c5d`) | 1 | 71 | 100 |
| 36610894929 | #2760 | exact | 0/8 | 424 (`3b1477a10`) | 424 (`3b1477a10`) | 1 | 51 | 57 |
| 36664523163 | #2763 | exact | 0/8 | 424 (`3b1477a10`) | 424 (`591507651`) | 1 | 111 | 356 |
| 36672797648 | #2762 | exact | 1/8 | 424 (`3b1477a10`) | 425 (`9a274311d`) | 1 | 111 | 313 |
| 36681200606 | #2762 | exact | 1/8 | 424 (`3b1477a10`) | 425 (`9a274311d`) | 1 | 153 | 454 |
| 36744783577 | #2762 | exact | 2/8 | 424 (`3b1477a10`) | 426 (`d974d0b4a`) | 1 | 120 | 365 |
| 36749407052 | #2765 | exact | 3/8 | 424 (`3b1477a10`) | 427 (`8e317ca88`) | 1 | 144 | 434 |
| 36822282351 | #2767 | exact | 4/8 | 424 (`3b1477a10`) | 428 (`a0a4c8752`) | 1 | 132 | 375 |
| 36861571111 | #2768 | exact | 5/8 | 424 (`3b1477a10`) | 429 (`b1097e507`) | 1 | 232 | 530 |
| 36871884401 | #2775 | exact | 6/8 | 424 (`3b1477a10`) | 430 (`dc766cdb2`) | 2 | 237 | 535 |
| 36912885361 | #2776 | exact | 7/8 | 424 (`3b1477a10`) | 431 (`65bd147e9`) | 1 | 252 | 527 |

### Provenance of each sampled run

| Run | Attempt | Tested head | PR base | Merge tree | Image | Toolchain | Plan | Snapshot (writer run, commit) | Restore |
|---|---:|---|---|---|---|---|---|---|---|
| 36288123692 | 1 | `106131bd56e8` | `835f9b6a3a28` | `303cd401ab37` | `ci-f90dae38568ffe9f` | ghc 9.12.2, cabal 3.16.1.0 | `20edd55a` e52 | 36285855248, `835f9b6a3` | exact |
| 36317483841 | 1 | `c93f17b67d7a` | `1e19e1e86eec` | `07a76f1da337` | `ci-f90dae38568ffe9f` | ghc 9.12.2, cabal 3.16.1.0 | `20edd55a` e52 | 36285855248, `835f9b6a3` | exact |
| 36320504148 | 1 | `5d7f4f576adf` | `e7bed43d5488` | `06e5632dd2ef` | `ci-f90dae38568ffe9f` | ghc 9.12.2, cabal 3.16.1.0 | `20edd55a` e52 | 36285855248, `835f9b6a3` | exact |
| 36343120829 | 1 | `26b95c68476f` | `5de514f96dbe` | `9047b3174f1b` | `ci-f90dae38568ffe9f` | ghc 9.12.2, cabal 3.16.1.0 | `20edd55a` e52 | 36285855248, `835f9b6a3` | prefix (wanted epoch 52, plan 5520c6d3) |
| 36424074239 | 2 | `42fe9dd4338d` | `8b8481c8f305` | `ae49c26a9594` | `ci-f90dae38568ffe9f` | ghc 9.12.2, cabal 3.16.1.0 | `5520c6d3` e52 | 36347729198, `8b8481c8f` | exact |
| 36443962564 | 1 | `f5eec86707c3` | `c2270c401b59` | `3d67431d3e04` | `ci-f90dae38568ffe9f` | ghc 9.12.2, cabal 3.16.1.0 | `5520c6d3` e52 | 36347729198, `8b8481c8f` | exact |
| 36595400637 | 1 | `e718d59ee9a4` | `4a3205c5d7a1` | `9ec450923e87` | `ci-f90dae38568ffe9f` | ghc 9.12.2, cabal 3.16.1.0 | `5520c6d3` e52 | 36347729198, `8b8481c8f` | exact |
| 36610894929 | 1 | `928c6be3ff6d` | `3b1477a10a96` | `5a2cc6f1d6f2` | `ci-f90dae38568ffe9f` | ghc 9.12.2, cabal 3.16.1.0 | `5520c6d3` e53 | 36598707561, `3b1477a10` | exact |
| 36664523163 | 1 | `7969a4ce97d8` | `59150765113c` | `2c07dde87df4` | `ci-f90dae38568ffe9f` | ghc 9.12.2, cabal 3.16.1.0 | `5520c6d3` e53 | 36598707561, `3b1477a10` | exact |
| 36672797648 | 1 | `68810d8d3729` | `9a274311de75` | `09a3ab1d5ec0` | `ci-f90dae38568ffe9f` | ghc 9.12.2, cabal 3.16.1.0 | `5520c6d3` e53 | 36598707561, `3b1477a10` | exact |
| 36681200606 | 1 | `f67ee7598358` | `9a274311de75` | `62b4abe11ea5` | `ci-f90dae38568ffe9f` | ghc 9.12.2, cabal 3.16.1.0 | `5520c6d3` e53 | 36598707561, `3b1477a10` | exact |
| 36744783577 | 1 | `ea31b0d0e475` | `d974d0b4ab77` | `be20b983b887` | `ci-f90dae38568ffe9f` | ghc 9.12.2, cabal 3.16.1.0 | `5520c6d3` e53 | 36598707561, `3b1477a10` | exact |
| 36749407052 | 1 | `85312ebe2b3c` | `8e317ca88b79` | `d314f6ecb72f` | `ci-f90dae38568ffe9f` | ghc 9.12.2, cabal 3.16.1.0 | `5520c6d3` e53 | 36598707561, `3b1477a10` | exact |
| 36822282351 | 1 | `521e411c3a2f` | `a0a4c8752ea1` | `941ee9728928` | `ci-f90dae38568ffe9f` | ghc 9.12.2, cabal 3.16.1.0 | `5520c6d3` e53 | 36598707561, `3b1477a10` | exact |
| 36861571111 | 1 | `29f60e92b1ed` | `b1097e5071a4` | `7ec9203162f5` | `ci-f90dae38568ffe9f` | ghc 9.12.2, cabal 3.16.1.0 | `5520c6d3` e53 | 36598707561, `3b1477a10` | exact |
| 36871884401 | 2 | `0bf81aa143c5` | `dc766cdb2440` | `9f40b994259e` | `ci-f90dae38568ffe9f` | ghc 9.12.2, cabal 3.16.1.0 | `5520c6d3` e53 | 36598707561, `3b1477a10` | exact |
| 36912885361 | 1 | `158dbe01a086` | `65bd147e9f3f` | `b7679eb2f237` | `ci-f90dae38568ffe9f` | ghc 9.12.2, cabal 3.16.1.0 | `5520c6d3` e53 | 36598707561, `3b1477a10` | exact |


Taken as a union, the drift replay and the own replay equal the unsplit replay
exactly: `D ∪ O` = `U` in all 34 run-and-group comparisons (0.0 % against the 5 %
tolerance, which the report enforces: it exits non-zero past it), with **no cancellations and no residuals**; the sequential counts
themselves overlap and are never added. The two
attribution conventions diverge only where a drift and an own cascade overlap
(`36317483841`, `36343120829`, `36861571111`, `36871884401`), and the table
shows the whole range: drift-first, own-first and an even split.

### Recompiled modules: Library and executables

| Run | D drift replay | O own replay | overlap | D∪O | U unsplit | cancelled | residual | (D∪O−U)/U |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| 36288123692 | 0 | 448 | 0 | 448 | 448 | 0 | 0 | 0.0% |
| 36317483841 | 448 | 324 | 321 | 451 | 451 | 0 | 0 | 0.0% |
| 36320504148 | 451 | 0 | 0 | 451 | 451 | 0 | 0 | 0.0% |
| 36343120829 | 451 | 288 | 265 | 474 | 474 | 0 | 0 | 0.0% |
| 36424074239 | 0 | 6 | 0 | 6 | 6 | 0 | 0 | 0.0% |
| 36443962564 | 6 | 0 | 0 | 6 | 6 | 0 | 0 | 0.0% |
| 36595400637 | 8 | 0 | 0 | 8 | 8 | 0 | 0 | 0.0% |
| 36610894929 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0.0% |
| 36664523163 | 0 | 268 | 0 | 268 | 268 | 0 | 0 | 0.0% |
| 36672797648 | 268 | 0 | 0 | 268 | 268 | 0 | 0 | 0.0% |
| 36681200606 | 268 | 0 | 0 | 268 | 268 | 0 | 0 | 0.0% |
| 36744783577 | 268 | 1 | 1 | 268 | 268 | 0 | 0 | 0.0% |
| 36749407052 | 268 | 0 | 0 | 268 | 268 | 0 | 0 | 0.0% |
| 36822282351 | 271 | 0 | 0 | 271 | 271 | 0 | 0 | 0.0% |
| 36861571111 | 271 | 498 | 266 | 503 | 503 | 0 | 0 | 0.0% |
| 36871884401 | 503 | 442 | 413 | 532 | 532 | 0 | 0 | 0.0% |
| 36912885361 | 532 | 0 | 0 | 532 | 532 | 0 | 0 | 0.0% |

### Recompiled modules: Headless test suite

| Run | D drift replay | O own replay | overlap | D∪O | U unsplit | cancelled | residual | (D∪O−U)/U |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| 36288123692 | 0 | 356 | 0 | 356 | 356 | 0 | 0 | 0.0% |
| 36317483841 | 356 | 313 | 313 | 356 | 356 | 0 | 0 | 0.0% |
| 36320504148 | 356 | 0 | 0 | 356 | 356 | 0 | 0 | 0.0% |
| 36343120829 | 356 | 320 | 306 | 370 | 370 | 0 | 0 | 0.0% |
| 36424074239 | 0 | 5 | 0 | 5 | 5 | 0 | 0 | 0.0% |
| 36443962564 | 5 | 0 | 0 | 5 | 5 | 0 | 0 | 0.0% |
| 36595400637 | 16 | 4 | 1 | 19 | 19 | 0 | 0 | 0.0% |
| 36610894929 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0.0% |
| 36664523163 | 0 | 312 | 0 | 312 | 312 | 0 | 0 | 0.0% |
| 36672797648 | 312 | 2 | 2 | 312 | 312 | 0 | 0 | 0.0% |
| 36681200606 | 312 | 2 | 2 | 312 | 312 | 0 | 0 | 0.0% |
| 36744783577 | 314 | 2 | 2 | 314 | 314 | 0 | 0 | 0.0% |
| 36749407052 | 314 | 0 | 0 | 314 | 314 | 0 | 0 | 0.0% |
| 36822282351 | 316 | 0 | 0 | 316 | 316 | 0 | 0 | 0.0% |
| 36861571111 | 315 | 386 | 315 | 386 | 386 | 0 | 0 | 0.0% |
| 36871884401 | 386 | 378 | 363 | 401 | 401 | 0 | 0 | 0.0% |
| 36912885361 | 401 | 0 | 0 | 401 | 401 | 0 | 0 | 0.0% |


### Reproducibility of the replay

Thirteen builds fall in five groups of identical inputs (four planned pairs and
one build repeated three more times as a deliberate replicate). Twelve reproduced
their group's module sets exactly. One did not: `alt2:36288123692:416`
recompiled 187 suite modules where four other builds of the same inputs
recompiled 357. The 187 is unexplained; no table here uses it (under the
real-writer rule every policy snapshot of that run is the snapshot itself).
The calibration's RMSE (18 s and 42 s) is the other bound on noise.

### Calibration

### Calibration (seconds = intercept + slope x recompiled modules)

| Step | intercept s | slope s/module | R^2 | RMSE s | n |
|---|---:|---:|---:|---:|---:|
| Build (library + executable) | 46.2 | 0.361 | 0.932 | 18.3 | 17 |
| Build test suites | 59.1 | 1.136 | 0.940 | 42.2 | 17 |


The zero-recompile run (36610894929) cost 51 s + 57 s = 108 s observed against
105 s fitted, an independent check on the intercepts. The sample is slightly
heavier than the window it was drawn from (observed median 578 s versus 493 s over all 59
runs since 2026-09-26; 95th percentiles 773 s versus 762 s), because repeated runs of
one pull request were not all sampled.

### Attribution of the unsplit build (modules, library+executables / suites)

| Run | drift-first drift | drift-first own | own-first drift | split drift | residual |
|---|---:|---:|---:|---:|---:|
| 36288123692 | 0 / 0 | 448 / 356 | 0 / 0 | 0 / 0 | 0 / 0 |
| 36317483841 | 448 / 356 | 3 / 0 | 127 / 43 | 287.5 / 199.5 | 0 / 0 |
| 36320504148 | 451 / 356 | 0 / 0 | 451 / 356 | 451 / 356 | 0 / 0 |
| 36343120829 | 451 / 356 | 23 / 14 | 186 / 50 | 318.5 / 203 | 0 / 0 |
| 36424074239 | 0 / 0 | 6 / 5 | 0 / 0 | 0 / 0 | 0 / 0 |
| 36443962564 | 6 / 5 | 0 / 0 | 6 / 5 | 6 / 5 | 0 / 0 |
| 36595400637 | 8 / 16 | 0 / 3 | 8 / 15 | 8 / 15.5 | 0 / 0 |
| 36610894929 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| 36664523163 | 0 / 0 | 268 / 312 | 0 / 0 | 0 / 0 | 0 / 0 |
| 36672797648 | 268 / 312 | 0 / 0 | 268 / 310 | 268 / 311 | 0 / 0 |
| 36681200606 | 268 / 312 | 0 / 0 | 268 / 310 | 268 / 311 | 0 / 0 |
| 36744783577 | 268 / 314 | 0 / 0 | 267 / 312 | 267.5 / 313 | 0 / 0 |
| 36749407052 | 268 / 314 | 0 / 0 | 268 / 314 | 268 / 314 | 0 / 0 |
| 36822282351 | 271 / 316 | 0 / 0 | 271 / 316 | 271 / 316 | 0 / 0 |
| 36861571111 | 271 / 315 | 232 / 71 | 5 / 0 | 138 / 157.5 | 0 / 0 |
| 36871884401 | 503 / 386 | 29 / 15 | 90 / 23 | 296.5 / 204.5 | 0 / 0 |
| 36912885361 | 532 / 401 | 0 / 0 | 532 / 401 | 532 / 401 | 0 / 0 |


## 3. Why the three exact hits cost over ten minutes (requirement 2)

| Run (PR) | Position | What the replay shows |
|---|---:|---|
| 36288123692 (#2736) | 1/8 | Base = snapshot (`835f9b6a3`): no drift. The PR's own 30 changed source files include four hub types (`World.Fluid.River.Types`, `World.Ocean.Types`, `World.Generate.Types`, `World.Save.Types`; static reverse-import closure 463–520 library, 368–376 suite modules). Replayed: **448 library+exe / 356 suite** modules. Estimate 671 s (105 s fixed + 566 s) against 607 s observed. |
| 36317483841 (#2738) | 2/8 | Base 418 = #2736 merged, after the snapshot. Drift: #2736's cascade, 448 / 356. The PR's own change (`World.ZoomMap.ColorPalette`, closure 430 / 343) recompiles 324 / 313 on its own, almost all of it already in the drift (overlap 321 / 313): union 451 / 356. |
| 36320504148 (#2739) | 3/8 | Base 419. Drift = #2736 (448 / 356) then #2738 (324 / 313 on top, replayed one change at a time); directly snapshot → base is 451 / 356. The PR's own diff (Markdown and tool files only) recompiles **0** modules: of the 757 s observed (12m37s), about 105 s is the fixed floor and the rest replayed master. |

So the master changes that drove all three are **#2736** (merged as relevant
count 418, `World.Fluid.River.Types` and three more hub types) and **#2738**
(419, `World.ZoomMap.ColorPalette`), both before the base and after the
snapshot. Other measured contributors: the snapshot was 417, not the epoch's
first commit 416, because the 416 writer was cancelled (§0), which does not
change these three runs' drift (it starts after 417); no cache
incompatibility (exact key, same image, plan and toolchain); the exact hit
itself says nothing about freshness. A snapshot at 418 would have left only
#2738's cascade (est. 578 s) for run 36320504148, and one at 419, 0 modules
(est. 105 s); the 419 snapshot finished writing at 13:20Z, 26 minutes after the
run started, so "every push" would have restored 418 for it.

### Which master changes are hubs

Static fan-out (`ci_build_drift.py fan-out`) of each build-relevant master change
in epochs 52–53: the changed module with the largest reverse-import closure.
It is an upper bound (ABI-unchanged edits stop propagating; the replay gives
the real size): against the replays it over-counts library cascades by 18–33 %
(529 versus 448 modules, 431 versus 324) and suite cascades by 6–10 %, and
mis-sizes small ones, so it ranks and names the drivers and does not size them.

| Count | PR | Changed src/app/test files | Top changed module (library / suite modules importing it) |
|---:|---|---:|---|
| 417 | #2737 (`835f9b6a3`) | 5 | `World.Render.Textures` (18 / 227) |
| 418 | #2736 (`1e19e1e86`) | 30 | `World.Fluid.River.Types` (520 / 376) |
| 419 | #2738 (`e7bed43d5`) | 6 | `World.ZoomMap.ColorPalette` (430 / 343) |
| 420 | #2740 (`8b8481c8f`) | 9 | `Engine.Core.Types` (322 / 325) |
| 421 | #2741 (`c2270c401`) | 5 | `Engine.Scripting.Lua.API.WorldQuery.Fluid` (4 / 175) |
| 422 | #2757 (`5d2177f54`) | 7 | `Structure.ArtCatalog` (430 / 348) |
| 423 | #2758 (`4a3205c5d`) | 2 | `Test.Headless.Unit.MentalWanderFallback` (0 / 1) |
| 424 | #2759 (`3b1477a10`) | 3 | `Test.Headless.Item.PortableKnowledge.Lifecycle` (0 / 2) |
| 425 | #2763 (`9a274311d`) | 12 | `Unit.Command.Types` (312 / 319) |
| 426 | #2764 (`d974d0b4a`) | 3 | `Test.Headless.Unit.SurvivalExemption` (0 / 2) |
| 427 | #2762 (`8e317ca88`) | 1 | `Test.Headless.World.Chop.Authority` (0 / 1) |
| 428 | #2766 (`a0a4c8752`) | 6 | `Gameplay.Tags.Types` (2 / 3) |
| 429 | #2767 (`b1097e507`) | 0 | (no source change) |
| 430 | #2768 (`dc766cdb2`) | 86 | `Unit.Faction` (541 / 398) |
| 431 | #2775 (`65bd147e9`) | 6 | `World.Flora.Types` (601 / 398) |

The replays confirm these cascades: 418 (448 / 356), 419 (324 / 313 on top of
418), 425 (268 / 312 from snapshot 424), and the steps 429 → 430 → 431 grow it
from 271 / 315 to 503 / 386 to 532 / 401. The static ranking also flags 422
(`Structure.ArtCatalog`, 430 / 348) and 420, but replaying 420 → 423 recompiled
only 8 and 16 modules, so 422 does not realise its closure: the changed
interface of a hub is what cascades, and an edit that leaves its ABI alone does
not. Since one hub cascade recompiles most of what the next would, the size
saturates at 270–530 library modules and 310–400 suite modules, and the
seconds follow.

## 4. Seconds by cause (requirement 1)

Observed seconds are step totals; the split below is an estimate from the
calibration in §2, allocating the fitted per-module seconds to the attributed
module counts and leaving the intercept as the fixed floor. Linking and other
non-compilation time are in the intercept; it is not allocated to either cause.

### Estimated seconds by cause (drift-first convention, calibration above)

| Run | observed | fixed | drift | own | residual |
|---|---:|---:|---:|---:|---:|
| 36288123692 | 607 | 105 | 0 | 566 | 0 |
| 36317483841 | 734 | 105 | 566 | 1 | 0 |
| 36320504148 | 757 | 105 | 567 | 0 | 0 |
| 36343120829 | 740 | 105 | 567 | 24 | 0 |
| 36424074239 | 153 | 105 | 0 | 8 | 0 |
| 36443962564 | 101 | 105 | 8 | 0 | 0 |
| 36595400637 | 171 | 105 | 21 | 3 | 0 |
| 36610894929 | 108 | 105 | 0 | 0 | 0 |
| 36664523163 | 467 | 105 | 0 | 451 | 0 |
| 36672797648 | 424 | 105 | 451 | 0 | 0 |
| 36681200606 | 607 | 105 | 451 | 0 | 0 |
| 36744783577 | 485 | 105 | 453 | 0 | 0 |
| 36749407052 | 578 | 105 | 453 | 0 | 0 |
| 36822282351 | 507 | 105 | 457 | 0 | 0 |
| 36861571111 | 762 | 105 | 456 | 164 | 0 |
| 36871884401 | 772 | 105 | 620 | 27 | 0 |
| 36912885361 | 779 | 105 | 647 | 0 | 0 |


Across the 17 runs, replayed master changes account for **82 %** of the variable
(module-scaling) build seconds under the drift-first convention, **54 %** when
own claims the overlap and **68 %** when the overlap is split evenly (6 963
variable seconds in total, against 17 × 105 s of fixed floor). The share is a
majority under every convention; the convention moves it by 28 points, which is
the honest width of "how much replays master".

## 5. Policy projection (requirement 3)

### Projected build seconds per run (library+exe step plus suites step)

| Policy | runs | median | 90% CI | p95 | 90% CI |
|---|---:|---:|---|---:|---|
| today (size 8) | 17 | 9m19s | 9m16s-11m12s | 12m33s | 11m28s-12m33s |
| size 4 | 17 | 9m19s | 9m16s-11m12s | 12m31s | 11m28s-12m31s |
| size 2 | 17 | 9m16s | 1m53s-9m38s | 11m40s | 10m21s-12m03s |
| size 1 | 17 | 1m53s | 1m48s-9m33s | 11m40s | 9m56s-12m03s |
| every successful push | 17 | 1m53s | 1m48s-9m33s | 11m40s | 9m56s-12m03s |
| ideal every push (lower bound) | 17 | 1m48s | 1m45s-9m16s | 11m40s | 9m50s-12m03s |
| suites -O1 | 17 | 8m35s | 8m33s-10m23s | 11m37s | 10m38s-11m37s |
| suites -O0 | 17 | 10m41s | 10m38s-12m45s | 14m18s | 13m04s-14m18s |
| size 1 + suites -O1 | 17 | 1m52s | 1m48s-8m48s | 10m48s | 9m12s-11m10s |
| size 1 + suites -O0 | 17 | 1m54s | 1m48s-10m56s | 13m19s | 11m20s-13m44s |

### Projected whole-run wall time (lane jobs of #2745)

| Policy | lane run | median | p95 | critical job |
|---|---|---:|---:|---|
| today (size 8) | p25 | 22m44s | 25m38s | lane rest |
| today (size 8) | p50 | 24m53s | 27m48s | lane rest |
| today (size 8) | p75 | 26m54s | 29m49s | lane rest |
| size 4 | p25 | 22m43s | 25m37s | lane rest |
| size 4 | p50 | 24m53s | 27m46s | lane rest |
| size 4 | p75 | 26m54s | 29m47s | lane rest |
| size 2 | p25 | 22m40s | 24m51s | lane rest |
| size 2 | p50 | 24m50s | 27m01s | lane rest |
| size 2 | p75 | 26m51s | 29m02s | lane rest |
| size 1 | p25 | 15m53s | 24m49s | lane rest |
| size 1 | p50 | 18m03s | 26m58s | lane rest |
| size 1 | p75 | 20m04s | 28m59s | lane rest |
| every successful push | p25 | 15m53s | 24m49s | lane rest |
| every successful push | p50 | 18m03s | 26m58s | lane rest |
| every successful push | p75 | 20m04s | 28m59s | lane rest |
| ideal every push (lower bound) | p25 | 15m44s | 24m49s | lane rest |
| ideal every push (lower bound) | p50 | 17m54s | 26m58s | lane rest |
| ideal every push (lower bound) | p75 | 19m55s | 28m59s | lane rest |
| suites -O1 | p25 | 22m03s | 24m47s | lane rest |
| suites -O1 | p50 | 24m13s | 26m57s | lane rest |
| suites -O1 | p75 | 26m14s | 28m58s | lane rest |
| suites -O0 | p25 | 24m11s | 27m27s | lane rest |
| suites -O0 | p50 | 26m23s | 29m38s | lane rest |
| suites -O0 | p75 | 28m26s | 31m41s | lane rest |
| size 1 + suites -O1 | p25 | 15m53s | 24m01s | lane rest |
| size 1 + suites -O1 | p50 | 18m03s | 26m10s | lane rest |
| size 1 + suites -O1 | p75 | 20m04s | 28m11s | lane rest |
| size 1 + suites -O0 | p25 | 16m05s | 26m32s | lane rest |
| size 1 + suites -O0 | p50 | 18m17s | 28m44s | lane rest |
| size 1 + suites -O0 | p75 | 20m20s | 30m47s | lane rest |


Percentiles are linear-interpolation ("type 7") over the 17 per-run estimates;
the intervals are the central 90 % of 4000 bootstrap resamples of the runs.
With a bimodal sample of 17 the median interval is wide and the 95th percentile
is the second-largest run; treat both as indications.

* **Writers and availability.** The projection uses the writers' real outcomes
  (one master push in 33 in the window had a cancelled first attempt). A snapshot counts as
  published only when the writer's log says `Cache saved with key`, not when its save step is green: of
  45 master pushes in the sample window 9 published one, and two more had a green save step without it
  (36008105084 logged `Unable to reserve cache with key …, another job may be creating this cache`;
  37031633313 logged nothing). A key is immutable, so only the first writer to finish publishes it. 17 of 69
  pull requests in the sample window started 9–42 minutes before the snapshot of
  their own base had finished writing (the writer was the push's whole
  `test-and-audits` job); since #2745 the writer is the separate `project-cache` job after
  `build-test`, whose run wall time (push run median 31m25s, 95th percentile
  52m18s) sets the lag. Those runs restore the previous snapshot. `size 1` and `every successful push`
  include it (the latter takes the newest snapshot any successful master job at or
  before the base had finished writing, including pushes with no build-relevant change,
  which write a snapshot of an unchanged tree); on this sample they pick the same
  snapshot for all 17 runs and differ only in how many snapshots they write.
  `ideal every push` assumes the base's snapshot is always ready and is shown only as
  a lower bound.
* **Fallback and cold cache.** A group with no finished writer falls back to
  the newest earlier group; there is no cold-cache case in the sample (a cold
  restore would cost the full cascade of 930 library+exe and 514 suite modules, about
  1025 s by the fitted model, against 5m40s locally on 16 cores).
* **Whole-run model.** Wall time is the resolve-image jobs plus the longest remaining
  job (jobs run in parallel) plus 5 s, per sampled run, not a sum of builds or a
  difference of medians. A lane job is its fixed cost (about 102 s for each lane)
  plus 0.92 × the run's estimated library+suite build (a lane builds no executable;
  0.92 is the median ratio of lane build to the audit job's two build steps in the
  same runs) plus the lane's Hspec run (`rest` 716 / 846 / 967 s and `world`
  547 / 632 / 649 s at the 25th / 50th / 75th percentile of the 144 runs). The audit
  job is 298 s plus the build; the probe job is its measured time with its
  prerequisite build scaled by the library estimate; static audits keep each
  run's own time. The `rest` lane is the critical job in every projection.
* **Writer cost.** A snapshot is only written when the key is absent. Over the
  22 days since #2745, 74 of 196 successful master pushes ran the writer job to success and 11 ran its save step
  to success (median 877 s rebuild plus 67 s save; publication is not log-verified in this timing-only window). `size 1` writes about 2.4–3.0 snapshots a day
  (the relevant merges per day), versus 0.3–0.4 now.

### Cache storage and eviction against the 10 GB quota

A snapshot is 388–437 MB compressed in the sample window and 450–488 MB now;
each dependency cache is 120 MB. Today the usage API counts 13 entries and 3.96 GB of 10 GB: six snapshots
(`dist-v3-…`, created 2026-10-17 to 10-23, across three plan hashes) and six
dependency caches among them. GitHub evicts an entry unused for seven
days and then the least recently used when the quota is exceeded;
`tools/ci_cache_cleanup.py`'s default is a read-only proposal, so nothing
here assumes it runs.

| Policy | Snapshots written per day | Snapshots kept at steady state | Quota share (0.47 GB each) | Retention if quota-bound |
|---|---:|---:|---:|---|
| today (size 8) | 0.3–0.4 (plus plan/image changes) | 2–3 per week | 1–1.4 GB, 10–14 % | over 7 days |
| size 4 | 0.6–0.75 | 4–5 | 2–2.5 GB, 20–25 % | over 7 days |
| size 2 | 1.2–1.5 | 8–10 | 4–5 GB, 40–50 % | over 7 days |
| size 1 | 2.4–3.0 | 17–21, capped by quota | up to ~9.3 GB, 93 % | about 6–8 days |
| every successful push | 3.3–3.5 | 19 (quota cap) | ~9.3 GB | about 5.4–5.8 days |

Capacity is (10 − about 0.7 GB of dependency caches) / 0.47 GB ≈ 19 snapshots.
At `size 1` the quota is nearly filled by project snapshots alone and a second
live key family (an image or dependency-plan change doubles the keys for a while)
evicts the oldest first: the *needed* snapshot is almost always the newest,
which is also the most recently used, so eviction risk to the snapshot a run
needs is low unless its base is older than about six days (the oldest base in the
sample window was 44 hours old). `every successful push` writes 17 % more
snapshots than `size 1` for byte-identical compiled content (a push with no
build-relevant change compiles to its predecessor's tree) and is dominated by it.

## 6. Test-suite optimisation level (requirements 3 and 4)

Matched measurement on `master@b86f9fc30` in a separate worktree (`issue-2746-opt`):
the library and executables built once at `-O2` and never touched; the headless
suite (638 modules: 624 under `test-headless/`, 7 under `app/`, 7 under
`tools/river_runtime`) recompiled from nothing at each level with only its
own `-O` changed, then both CI lanes run one at a time
(`--lane world`: 764 examples; `--lane rest`: 11 744 examples; the same set at
every level). Verified per variant: the new `-O` is on the suite's GHC command
lines and `-O2` stays on the library's, the library did not recompile (one
generated module does), an immediate rebuild is a no-op, and the example counts
match. Hardware: the same 16-core Apple Silicon host throughout; the CPU was
76–92 % idle at the start of each of the eight final lane runs and 81–91 % at
their end, with no other build visible (four `ghc-9.12.2` processes of other sessions
were resident throughout, constant in number, while the CPU stayed idle), so none was rerun. Wall times of the rest lane are noisy on
identical builds (the two `-O2` runs: 805 s and 667 s; world: 194 s and 184 s),
so a difference under about 15 % in a lane's wall time is not a finding.

**Suite compile, `cabal build` at `-j16` on this host** (what the host did):

| Variant | wall s | CPU s (user+sys) |
|---|---:|---:|
| `-O2` | 192 | 1764 |
| `-O2` (repeat) | 203 | 1832 |
| `-O1` | 169 | 1503 |
| `-O0` | 175 | 1788 |

**Suite compile, sequential** (`ghc --make` on the captured suite command without
`-j`, a fresh output directory, 638 modules; what a CPU-bound 4-vCPU runner
pays, since parallel CPU on this host includes spin and GC overhead):

| Variant | CPU s | versus `-O2` |
|---|---:|---:|
| `-O2` | 1224 | |
| `-O1` | 1075 | **−12.1 %** |
| `-O0` | 1505 | **+23.0 %** |

**Headless suite run** (the same examples; `world` / `rest` lane):

| Variant | world wall s | rest wall s | world CPU s | rest CPU s | failed examples |
|---|---:|---:|---:|---:|---:|
| `-O2` | 194 | 805 | 828 | 616 | 0 |
| `-O2` (repeat) | 184 | 667 | 820 | 578 | 0 |
| `-O1` | 189 | 680 | 812 | 589 | 0 |
| `-O0` | 229 | 748 | 862 | 661 | 1 |

* `-O1` reproduces #2276's direction (−16.6 % there; −12.1 % sequential and
  −16.4 % at `-j16` here) at no measurable run-time cost: lane wall times are
  inside the noise above and lane CPU is 0.99 × at both lanes. Both lanes pass
  every example.
* `-O0` does **not** reproduce #2276's −46.4 % at today's suite: its sequential compile
  CPU is 23 % *higher* than `-O2`, its `-j16` CPU equal, its wall 11 % lower. On the
  heaviest single module (`Test.Headless.UI.TransferSession.Lifecycle`)
  `-O0` is 5.5× cheaper (14.0 s at `-O2`, 10.7 s at `-O1`, 2.6 s at `-O0`), so some other modules
  must be much dearer unoptimised; which was not diagnosed. Its `world` lane is 21 %
  slower in wall time (229 s versus 184–194 s; CPU +4.6 %), its `rest` lane 10.7 %
  more CPU, and **one example fails**: `Unit.Fall … gives motion and injury ONE source of truth
  for gravity` expects `6.5400004` and gets `6.54` (a `Float` constant folded at compile time
  at `-O2` and computed at run time at `-O0`). `-O0` changes numerics and is not a drop-in.
* In the projection (§5) the suite step's fitted per-module seconds are scaled by the
  sequential ratio (0.879 for `-O1`, 1.230 for `-O0`); each lane's run time is scaled by its measured
  wall ratio, floored at 1 (a gain inside the noise is never credited).

## 7. Recommendation (requirement 5)

**Build-side decision (Q-14): write the snapshot on every build-relevant master
change, `EPOCH_SIZE = 1`, and leave the optimisation flags alone.** The change
is the maintainer's and is filed separately.

| | today (`EPOCH_SIZE = 8`) | `EPOCH_SIZE = 1` | `EPOCH_SIZE = 1` + suite `-O1` |
|---|---:|---:|---:|
| build (library+exe + suites), median | 9m19s | **1m53s** | 1m52s |
| build, 95th percentile | 12m33s | **11m40s** | 10m48s |
| whole run, median (lane run at its median) | 24m53s | **18m03s** | 18m03s |
| whole run, 95th percentile | 27m48s | **26m58s** | 26m10s |

Whole-run figures use the lane jobs of #2745 as merged, with per-lane fixed
cost and Hspec run time from the 144 pull-request runs since (§5 tables give the
25th and 75th percentile lane runs: median run 15m53s and 20m04s under
`EPOCH_SIZE = 1`). Percentiles are over 17 per-run estimates; the 90 % interval
of the median build under `EPOCH_SIZE = 1` is 1m48s–9m33s because the sample
is bimodal, so the *direction* is secure (every run whose base was ahead of its
snapshot gets cheaper) and the median's size is not. The observed whole-run
median of the same regime is 28m37s and the model puts today's at 24m53s: the
model is about 13 % optimistic about today's runs (the sample comes from an earlier, lighter regime), so the figures are for comparison between options, not for forecasting
a single run.

* **What it fixes.** Of the 17 runs, 13 had a base ahead of their snapshot and
  all 13 recompiled replayed master changes; a snapshot of the base removes that part
  entirely, and 9 of the 13 then recompile at most 6 modules. It also makes a
  cancelled writer at an epoch boundary (§0) a one-change problem instead of a
  three-day one.
* **What it does not fix.** 6 of the 17 runs recompile a hub cascade on their *own*
  diff (268–498 library+exe modules): their cost is unchanged, which is why
  the 95th percentile barely moves. Cascades come from a few widely imported
  type modules (`World.Fluid.River.Types`, `World.ZoomMap.ColorPalette`,
  `Unit.Command.Types`, `Unit.Faction`, `World.Flora.Types`, …). #2276 ranked
  stabilising `Engine.Core.State` "not worth doing" on a cascade measured through
  that one module; the cascades measured here run through *different* hub
  modules, so that ranking was never tested against them. It is a design question,
  out of scope here (the issue excludes it), recorded for the maintainer.
  Nor does it touch the lane run itself: the `rest` lane's median 845 s plus
  about 100 s of job cost is a 15m47s floor, so even a zero-second build
  leaves the median run about 16–17 minutes, three minutes under the target
  and only when the lane runs at its median or better (at its 75th percentile the
  median run is 20m04s).
* **Suite `-O1`**: −12 % suite compile CPU, no measured run-time cost, moves the
  95th percentile build by about 50 seconds and the median not at all once the
  snapshot is fresh. A reasonable follow-up, not needed to reach the target.
  **Suite `-O0` is rejected**: more compile CPU, a slower `world` lane, and one failing
  example whose expectation is optimisation-dependent.
* **`every successful push` picks the same snapshots as `EPOCH_SIZE = 1` on this sample** (it just writes
  17 % more of them for an unchanged tree), so `EPOCH_SIZE = 1` is the choice; `ideal every push` in §5 is a
  lower bound that ignores writer availability.
* **`EPOCH_SIZE` 2 and 4 are not worth it**: the median stays 9m16s and
  9m19s, because the median run's base is rarely a group start; they save
  storage and little else.
* **Cost.** About 2.4–3.0 snapshots a day of 0.45–0.49 GB (§5): the quota is
  nearly all snapshots (about 6–8 days of retention before the quota evicts
  the oldest). **`tools/ci_cache_cleanup.py`'s default `--keep 3` would
  reduce that to about a day**, so the default must be raised or the tool left
  unused. The writer job runs its rebuild for about 3 pushes a day instead of
  about 0.4. A pull request that starts before its base's snapshot is written
  (a quarter of them) restores the previous change's snapshot, which is already
  in the projection; shortening the writer lag (it now waits for `build-test`,
  push run median 31m25s) would help further and is a separate decision about
  caching an unverified tree.

## 8. Repeating this

**Regenerating the tables needs no CI access.** The measurement inputs are
preserved in [`evidence/ci-build-drift-2746/`](evidence/ci-build-drift-2746/):

| File | Contents |
|---|---|
| `runs.json` | the 118 successful CI runs of 2026-09-24 to 10-02 as collected (steps, jobs, cache keys and report lines, merge parents, attempt, writer evidence) |
| `lanes.json` | the 341 runs of 2026-10-03 to 10-24, timings only, trimmed to the steps the model reads |
| `plan.json` | the 17 sampled runs: tested head, PR base, **merge-tree identity of the tested tree**, full primary and restored cache keys, snapshot commit and writer run, and each policy's snapshot |
| `replay.json` | every replayed build: commit and recompiled modules per Cabal component |
| `replay_replicates.json` | the three replicate builds of §2 |
| `opt_measurements.json`, `opt.json` | the raw §6 timings, loads and sequential compiles, and the ratios derived from them (compile CPU ratio = sequential CPU of the level / of `-O2`; run ratio = lane wall time / the mean of the two `-O2` runs) |

```bash
E=docs/evidence/ci-build-drift-2746
python3 tools/ci_build_drift_report.py --runs $E/runs.json --plan $E/plan.json \
  --replay $E/replay.json --lanes $E/lanes.json --opt $E/opt.json
```

To repeat the whole measurement (needs `gh` and a machine that builds the project):

```bash
WT=~/worktrees/coghex/synarchy/issue-2746-ci-build-drift     # the checkout holding the tools
STATE=~/.local/state/ci-drift-2746; mkdir -p $STATE/logs
# 1. runs: sample window and the post-#2745 window (timings only)
python3 tools/ci_build_drift.py collect --since 2026-09-24 --until 2026-10-02 --cache-dir $STATE/logs --out $STATE/runs.json
python3 tools/ci_build_drift.py collect --jobs-only --since 2026-10-03 --until 2026-10-24 --cache-dir $STATE/logs --out $STATE/runs_lanes.json
# 2. plan the replay of the sampled runs (fetch any PR head that is not local first:
#    git fetch origin <sha>)
python3 tools/ci_build_drift.py plan --runs $STATE/runs.json --worktree $WT --out $STATE/plan.json \
  --chain-ids 36320504148 36343120829 --ids 36288123692 36317483841 36320504148 36343120829 36424074239 36443962564 36595400637 36610894929 36664523163 36672797648 36681200606 36744783577 36749407052 36822282351 36861571111 36871884401 36912885361
# 3. a separate replay worktree, cold-built once at the first snapshot, saved as state c417
git worktree add --detach ~/worktrees/coghex/synarchy/issue-2746-replay 835f9b6a3
(cd ~/worktrees/coghex/synarchy/issue-2746-replay && cabal-3.16.1.0 build all && cabal-3.16.1.0 build synarchy-test-headless)
cp -Rpc ~/worktrees/coghex/synarchy/issue-2746-replay/dist-newstyle $STATE/states/c417
python3 tools/ci_build_drift.py replay --plan $STATE/plan.json --worktree ~/worktrees/coghex/synarchy/issue-2746-replay \
  --state-dir $STATE/states --out $STATE/replay.json
#    interrupted, or states dropped? re-plan only what is left (a state a pending step
#    needs is rebuilt from the nearest live one, keeping its recorded measurement):
python3 tools/ci_build_drift.py plan ... --resume $STATE/replay.json --state-dir $STATE/states --out $STATE/plan.json
# 4. tables (as above, with $STATE paths), then acceptance
python3 tools/ci_timing_report.py --run 36320504148   # likewise 36317483841, 36288123692
git diff origin/master --stat -- .github/ tools/ci_cache_epoch.py synarchy.cabal 'cabal.project*'   # prints nothing
```

The suite-only optimisation measurement (§6) used a second worktree
(`issue-2746-opt`) whose `synarchy.cabal` carries, for the run only, a copy
of `common build-policy` named `build-policy-test` with `-O2` replaced, imported
by `test-suite synarchy-test-headless` alone. Two simpler routes were tried
and rejected: a `ghc-options: -O1` line in the stanza (cabal puts it *before* the
imported `-O2`, so `-O2` still wins; the compile CPU was unchanged) and a
command-line `--ghc-options=-O1` (it also recompiles the library, which then
stops being the matched baseline). Each variant moves the test suite's build
directory aside to force a full recompile, checks that the library did not
recompile, that the new flag is on the suite's GHC command lines and that an
immediate rebuild is a no-op, then runs both lanes
(`cabal test synarchy-test-headless --test-show-details=direct
--test-options='--lane world|rest --print-slow-items=20 --format=failed-examples'`)
one at a time under the granted probe slot, recording the machine load.
The driver and the other two measurements are in the appendix.

## 9. Limits

* The replay is macOS arm64 (dynamic-too, `DARWIN` defined); CI is Linux with a
  static GHC. Module identity does not depend on either, but a module set could
  in principle differ; the fitted seconds reproduce CI's step totals with
  R² 0.93–0.94 and the zero-recompile run to within 3 s, which bounds that.
* One replay per build (plus the replicates above): the sampled counts carry the
  one unexplained deviation reported in §2. The fit's residuals reach about 85 s
  on the large cascades (it over-estimates run 36288123692 by 64 s and under-estimates
  36320504148 by 85 s), so single-run seconds are estimates, not observations.
* 17 runs; the median of a bimodal sample is wide (intervals above). The sample
  is 17 % heavier than its window at the median.
* Lane timings are medians over the 144 pull-request runs since #2745, taken as
  fixed per lane; a lane's build is scaled from the audit job's library+suite
  build (0.92 ×, the lane builds no executable). Runs on the Ubuntu 26.04 image
  were not re-sampled.

## Appendix: the optimisation-level measurement

`opt_run.sh O1=-O1 O0=-O0 O2b=none` (after a first `O2=none` run, then the repeat
`O2b`), run in `issue-2746-opt` at `master@b86f9fc30`. It restores the library at
`-O2`, and per variant rewrites `synarchy.cabal` (local only, reset with
`git checkout`), moves the suite's build directory aside, compiles, checks, and runs both lanes.

```zsh
#!/bin/zsh
# #2746 requirement 4: test-suite-only -O2 / -O1 / -O0 against a matched baseline.
# Same revision, same library artifacts, same lanes; the cabal file edit is local to this worktree.
set -u
WT=$HOME/worktrees/coghex/synarchy/issue-2746-opt
OUT=$HOME/.local/state/ci-drift-2746/opt
CABAL=cabal-3.16.1.0
cd $WT
load() { echo "$1 $(date -u +%FT%TZ) load: $(uptime | sed 's/.*load averages*: //') | other-ghc: $(pgrep -f 'ghc-9.12.2' | wc -l | tr -d ' ') | $(top -l 2 -n 0 -s 1 | grep 'CPU usage' | tail -1)" >> $OUT/load.log; }
git checkout -q synarchy.cabal
if [ ! -f $OUT/lib.done ]; then
  load "lib-start"
  /usr/bin/time -l $CABAL build all -v1 > $OUT/lib.log 2> $OUT/lib.time && touch $OUT/lib.done
  load "lib-end"
fi
tsdir() { find dist-newstyle/build -maxdepth 6 -type d -name synarchy-test-headless -path '*synarchy-0.1.0.0/build/*' | head -1; }
for variant in "${@}"; do
  name=${variant%%=*}; flag=${variant#*=}
  # The test suite gets its own copy of the build-policy stanza with only -O2 replaced, so the
  # library and executables keep their flags. (A stanza's own ghc-options come BEFORE the imported
  # -O2 on the GHC command line, and a command-line --ghc-options reaches the library too: both
  # were tried and rejected, see the document.)
  git checkout -q synarchy.cabal
  python3 - "$flag" "$name" <<'PY'
import re,sys,time
flag,name=sys.argv[1:3]
s=open('synarchy.cabal').read()
start=s.index("common build-policy\n")
end=s.index("\nlibrary\n",start)
block=s[start:end]
new=block.replace("common build-policy\n","common build-policy-test\n",1)
if flag!="none":
    assert "ghc-options: -O2\n" in new
    new=new.replace("ghc-options: -O2\n","ghc-options: "+flag+"\n",1)
new+=f"\n-- variant {name} {time.time()}\n"
s=s[:end]+"\n"+new+s[end:]
anchor="test-suite synarchy-test-headless\n    import: warnings, lang, build-policy\n"
assert anchor in s
s=s.replace(anchor,"test-suite synarchy-test-headless\n    import: warnings, lang, build-policy-test\n",1)
open('synarchy.cabal','w').write(s)
PY
  GHCOPT=""
  TS=$(tsdir)
  # force a full test-suite recompile (a missing directory already is one)
  [ -n "$TS" ] && [ -d "$TS" ] && mv "$TS" "$TS.old-$name-$(date +%s)"
  load "$name-compile-start"
  /usr/bin/time -l $CABAL build synarchy-test-headless $GHCOPT -v2 > $OUT/$name.compile.log 2> $OUT/$name.compile.time
  load "$name-compile-end"
  grep -o ' -O[0-9]' $OUT/$name.compile.log | sort | uniq -c > $OUT/$name.flags
  grep -cE '^\[ *[0-9]+ of [0-9]+\] Compiling' $OUT/$name.compile.log > $OUT/$name.compiled
  # the library must not recompile: nothing "Compiling" before the test-suite banner
  nlib=$(awk '/^Building test suite/{exit} /^\[ *[0-9]+ of [0-9]+\] Compiling/{n++} END{print n+0}' $OUT/$name.compile.log)
  ntest=$(( $(cat $OUT/$name.compiled) - nlib ))
  echo "$name: library modules compiled $nlib, test-suite modules compiled $ntest" >> $OUT/load.log
  if [ "$nlib" -gt 3 ] || [ "$ntest" -lt 400 ]; then echo "$name: bad compile (library $nlib, tests $ntest); aborting" >> $OUT/load.log; exit 1; fi
  if [ "$flag" != "none" ] && [ "$(grep -c -- "$flag" $OUT/$name.compile.log)" -lt 1 ]; then echo "$name: flag $flag not on any command line; aborting" >> $OUT/load.log; exit 1; fi
  /usr/bin/time -l $CABAL build synarchy-test-headless $GHCOPT -v1 > $OUT/$name.rebuild-check.log 2> $OUT/$name.rebuild-check.time   # must be a no-op
  for lane in world rest; do
    load "$name-$lane-start"
    /usr/bin/time -l $CABAL test synarchy-test-headless $GHCOPT -v0 --test-show-details=direct \
      --test-options="--lane $lane --print-slow-items=20 --format=failed-examples" \
      > $OUT/$name.$lane.log 2> $OUT/$name.$lane.time
    echo "exit=$?" >> $OUT/$name.$lane.time
    load "$name-$lane-end"
  done
done
echo finished > $OUT/finished
```

The sequential compile used the suite's own captured command (`dist-newstyle/build/.../build/ghc-capture/units/*-synarchy-test-headless/inv.*/rsp.0`,
written by `Setup.hs`) with `-j`, `-j1`, `-O` and `-O1` removed, a fresh output directory per level
and the level appended last:

```zsh
#!/bin/zsh
# whole-suite sequential compile (ghc --make without -j) of the captured test-suite command, per -O level
cd ~/worktrees/coghex/synarchy/issue-2746-opt
S=$HOME/.local/state/ci-drift-2746/ab
RSP=dist-newstyle/build/aarch64-osx/ghc-9.12.2/synarchy-0.1.0.0/build/ghc-capture/units/synarchy-0.1.0.0-inplace-synarchy-test-headless/inv.UYOAct/rsp.0
OUT=$HOME/.local/state/ci-drift-2746/opt-seq
for lvl in "$@"; do
  rm -rf $S/out-$lvl; mkdir -p $S/out-$lvl
  python3 - "$RSP" "$S" "$lvl" <<'PY'
import sys
rsp,S,lvl=sys.argv[1:4]
t=[l for l in open(rsp).read().split('\n') if l]
out=[];skip=0
for l in t:
    if skip: skip-=1; continue
    if l in('-j','-j1','-O','-O1'): continue
    if l in('-outputdir','-odir','-hidir','-stubdir'): out+=[l,f'{S}/out-{lvl}']; skip=1; continue
    if l=='-hiedir': out+=[l,f'{S}/out-{lvl}/hie']; skip=1; continue
    out.append(l)
out.append('-'+lvl)
open(f'{S}/seq-{lvl}.rsp','w').write('\n'.join(out)+'\n')
PY
  echo "$lvl start $(date -u +%FT%TZ) $(uptime | sed 's/.*load averages*: //')" >> $OUT/seq-ghc.log
  /usr/bin/time -l ~/.ghcup/bin/ghc @$S/seq-$lvl.rsp > $S/seq-$lvl.log 2> $S/seq-$lvl.time
  echo "$lvl end $(date -u +%FT%TZ) $(grep -E ' real ' $S/seq-$lvl.time) compiled=$(grep -cE '^\[ *[0-9]+ of [0-9]+\] Compiling' $S/seq-$lvl.log)" >> $OUT/seq-ghc.log
done
echo done >> $OUT/seq-ghc.log
```

The single-module check ran the same flags with every module target replaced by
`test-headless/Test/Headless/UI/TransferSession/Lifecycle.hs`, `-c -fforce-recomp` and the level appended,
into a copy of the suite's object directory: 14.0 s, 10.7 s and 2.6 s of user CPU at `-O2`, `-O1` and `-O0`.

