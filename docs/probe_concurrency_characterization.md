# Probe measurement under concurrency and RTS overrides (#1427)

What this is: one measured characterization of the #1425 flakiness
harness (`tools/probe_flake.py`) running under concurrency, on one host,
at one commit. It does **not** propose a replacement for #1425's shipped
`+RTS -N4 -RTS` default — the evidence below supports leaving that
default exactly as it is.

Every number here comes from the retained
`probe-flake-result/v1` documents under
[`docs/measurements/probe_concurrency_1427/`](measurements/probe_concurrency_1427/),
which also holds the exact launcher scripts, the launcher logs, the
aggregation script, and the durable event/stdout/engine-log evidence for
every non-pass attempt. `summary.json` there is this report's
machine-readable form; nothing in the tables below was transcribed by
hand.

## 1. What was executed

**Tested code.** Engine and tools at
`caaed17e40c3f2e274b34f68bb863a63a95ab51d` (`master`, the merge of
PR #1529). The tested tree was **clean** — no tracked or untracked source
modification — and every cohort re-recorded both facts itself
(`cohort.txt`'s `commit=` and `tree_clean=`), because
`Measurement.commit_sha` records only `git rev-parse HEAD` and cannot
reveal a dirty tree. The report commit is necessarily later than the
tested commit; they are different things and only the SHA above names
the code that ran.

**Executable artifact.** One `dist-newstyle` in the issue worktree
`issue-1427-probe-concurrency-characterization`, built **warm before the
first cohort** with `cabal build exe:synarchy` (2 min 14 s, prod
profile — no `-f dev`) and not rebuilt again. This matters: the primary
matrix runs through the shipped #1425 launch path, which is
`probelib.boot`'s `cabal run -v0 exe:synarchy`
(`tools/probelib.py:217`), so a cold build directory would have put
Cabal's inplace package registration inside the measurement. See §6 on
why nothing was substituted for that launch path.

**Workloads.** The three `probe_flake.PROTOCOL_PROBES` entries, which are
also #1427's three readiness prerequisites:

| probe | class | issue | solo elapsed at `-N4` |
|---|---|---|---|
| `role` | worldgen | #265 | ~146 s |
| `thermo_altitude` | worldgen (size 128) | #308 (migrated by #1474) | ~81 s |
| `position_hold` | long AI / real-time, arena | #1216 (registered by #1471) | ~148 s |

**Concurrency.** As #1427's authoritative amendment defines it: the
achieved peak number of live `probe_flake.py` **harness invocations**,
recorded by each result document's `peak_concurrency` from the
machine-wide live registry (`tools/probe_flake.py:497-503`).
`run_probes.py --jobs` was not used and is not an equivalent
measurement.

**Attempt counts.** Every cell of every matrix requests exactly **8
total attempts**, held constant by splitting them across the
invocations rather than by holding `--runs` constant — `measure()` runs
`--runs` sequentially *within* one invocation
(`tools/probe_flake.py:854-878`), so a constant `--runs` would have
multiplied the denominator with concurrency. 21 cells x 8 attempts =
**168 attempts**, and every one of them is in the retained data.

| requested concurrency | invocations | `--runs` each | total attempts |
|---|---|---|---|
| 1 | 1 | 8 | 8 |
| 2 | 2 | 4 | 8 |
| 4 | 4 | 2 | 8 |
| 8 | 8 | 1 | 8 |

**Matrices.**

- **Primary (`-N4`, the shipped default):** all three probes x
  concurrency {1, 2, 4, 8}. 12 cells.
- **Drift / test-retest control:** `n4-c1-role` repeated identically as
  the last primary cohort, 1 h 42 m after the first, to separate machine
  drift and per-cell sampling noise from the concurrency effect.
- **RTS subset A (capability sweep under contention):** all three probes
  x `--rts-caps` {1, 8} at concurrency 4. Directly comparable to the
  primary `n4-c4-*` cells — same probe, same concurrency, same 8
  attempts.
- **RTS subset B (no-contention reference):** `thermo_altitude` x
  `--rts-caps` {1, 8} at concurrency 1, completing one probe's full
  3 x 2 capability x concurrency block at equal denominators.

## 2. Host characteristics and controls

| control | value |
|---|---|
| Host | Apple M3 Max, 16 physical / 16 logical cores, 64 GiB |
| OS / arch | macOS 26.6 (25G5065a), arm64 |
| Toolchain | GHC 9.12.2, cabal-install 3.16.1.0, Python 3.14.6 |
| Build profile | production (`-O2 -optc-O3`); `cabal.project` sets `ghc-options: -j`, `semaphore: False` |
| Build/cache state | **warm** — built once before cohort 1, never rebuilt |
| Executable `-N` | `synarchy.cabal`'s baked `-N -A128M`, **overridden per run** by the harness's explicit `+RTS -N<n> -RTS` |
| Timeout budget | `probe_flake.DEFAULT_TIMEOUT` = 900 s per attempt (unmodified) |
| Port range | 8009-8999, atomically leased; port 8008 never used (verified across all 168 attempts) |
| Ports actually taken | 8009-8016 |
| Artifact root | `~/probe-flake-1427-artifacts`, outside every worktree |
| Config overlays | the worktree's `config/*.local.yaml` were **self-materialized by the first engine boot** from the tracked `*_default.yaml` / `data/notification_categories.yaml` — `keybinds.local.yaml` and `video.local.yaml` are byte-identical to their tracked defaults, and none was copied from the developer's primary checkout. A fresh clone reproduces this state. |
| Background load | none scheduled; no other probe, build, or test run. Enforced, not assumed: the launcher **refuses to start** a cohort while any `synarchy-probe-flake-live-*.json` registration exists, so no unrelated `probe_flake.py` invocation could contaminate the registry. It never fired — all 21 cohorts started. |
| Isolation | one cohort at a time, strictly sequential; each invocation writes a distinct `--result` path, distinct stdout/stderr capture, and its own per-run artifact directory. No save-slot, resource-root or port collision was observed. |

The cohort schedule is in §7. `-N` was applied to **every** engine start
in every cohort: each of the three probes has exactly one
`probelib.boot` call site and each passes `rep.engine_args()`
(`tools/role_probe.py:177-179`, `tools/thermo_altitude_probe.py:120-121`,
`tools/position_hold_probe.py:205-206`), and `thermo_altitude`'s additional
`--dump` subprocess splices the same block
(`tools/thermo_altitude_probe.py:126-134`). The requested value is recorded
per invocation as `rts_capabilities`, and the block itself is
constructed by the prerequisite contract
(`tools/probe_protocol.py:458-468`); no engine API was added to read the
count back.

## 3. Classification rule

The raw terminal outcome in each result document (`PASS`, `FAIL`,
`TIMEOUT`, and the separate `error_run` harness record) is **never
rewritten**. The categories below are a report-level analysis layered on
top of it, applied mechanically by
[`aggregate.py`](measurements/probe_concurrency_1427/aggregate.py):

| category | rule | evidence |
|---|---|---|
| pass | outcome `PASS` | — |
| behavioral failure | outcome `FAIL` with >= 1 check `FAIL` | the failing check ids |
| setup failure | outcome `FAIL` with 0 `FAIL` checks and >= 1 `MISSING` check | `Reporter.abort()` emits a `WARN` diagnostic and leaves the checks it prevented `MISSING` (`tools/probe_protocol.py:509-519`); the retained `events.jsonl` carries it |
| timeout | outcome `TIMEOUT` | listed separately as a **censored** observation, never folded into an elapsed summary |
| harness / infrastructure error | the measurement's `error_run` | protocol stream untrustworthy; no rate is calculable for that invocation |

Setup failure is an analytical classification, not a fifth protocol
outcome. A non-pass attempt is additionally scanned for known
infrastructure signatures (the `dist-newstyle` inplace-package race, an
engine that never printed `READY`, port-bind failure, OOM/kill) across
its retained stdout, event stream and engine log; **no attempt in this
run matched any of them**.

Retries do not exist in this harness, so no first-attempt failure was
erased: every one of the 168 requested attempts appears as its own raw
record.

Denominators are stated per category and are always the cell's 8
**requested** attempts. No cell was curtailed: `completed_attempts`
equals `requested_attempts` in all 21, and every invocation exited 0.

## 4. Results

### 4.1 Primary matrix, per cell (`-N4`)

| cell | probe | `-N` | requested C | achieved peak (min–max) | mean parallelism | attempts | PASS | behav. fail | setup fail | timeout | harness err | failure rate | elapsed min/med/p90/max (s) | cohort wall (s) |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| `n4-c1-role` | `role` | 4 | 1 | 1–1 | 1.0 | 8 | 2 | 6 | 0 | 0 | 0 | 0.750 | 127.1 / 146.4 / 166.5 / 174.7 | 1188 |
| `n4-c1-thermo_altitude` | `thermo_altitude` | 4 | 1 | 1–1 | 1.0 | 8 | 8 | 0 | 0 | 0 | 0 | 0.000 | 80.0 / 80.5 / 81.2 / 82.2 | 645 |
| `n4-c1-position_hold` | `position_hold` | 4 | 1 | 1–1 | 1.0 | 8 | 7 | 1 | 0 | 0 | 0 | 0.125 | 137.5 / 147.9 / 174.7 / 182.8 | 1227 |
| `n4-c2-role` | `role` | 4 | 2 | 2–2 | 1.95 | 8 | 1 | 7 | 0 | 0 | 0 | 0.875 | 128.6 / 143.3 / 166.4 / 170.8 | 606 |
| `n4-c2-thermo_altitude` | `thermo_altitude` | 4 | 2 | 2–2 | 1.99 | 8 | 8 | 0 | 0 | 0 | 0 | 0.000 | 87.3 / 88.2 / 91.7 / 93.1 | 358 |
| `n4-c2-position_hold` | `position_hold` | 4 | 2 | 2–2 | 1.92 | 8 | 7 | 1 | 0 | 0 | 0 | 0.125 | 140.9 / 147.4 / 174.7 / 183.2 | 647 |
| `n4-c4-role` | `role` | 4 | 4 | 4–4 | 3.82 | 8 | 4 | 4 | 0 | 0 | 0 | 0.500 | 126.0 / 147.3 / 157.7 / 167.2 | 305 |
| `n4-c4-thermo_altitude` | `thermo_altitude` | 4 | 4 | 4–4 | 3.96 | 8 | 8 | 0 | 0 | 0 | 0 | 0.000 | 103.3 / 107.8 / 109.5 / 110.4 | 217 |
| `n4-c4-position_hold` | `position_hold` | 4 | 4 | 4–4 | 3.69 | 8 | 7 | 1 | 0 | 0 | 0 | 0.125 | 133.9 / 156.6 / 166.8 / 173.9 | 338 |
| `n4-c8-role` | `role` | 4 | 8 | 8–8 | 6.97 | 8 | 4 | 4 | 0 | 0 | 0 | 0.500 | 150.5 / 167.1 / 185.6 / 194.5 | 195 |
| `n4-c8-thermo_altitude` | `thermo_altitude` | 4 | 8 | 7–8 | 7.76 | 8 | 8 | 0 | 0 | 0 | 0 | 0.000 | 193.1 / 203.6 / 206.1 / 206.4 | 207 |
| `n4-c8-position_hold` | `position_hold` | 4 | 8 | 3–8 | 7.22 | 8 | 8 | 0 | 0 | 0 | 0 | 0.000 | 136.8 / 157.3 / 171.6 / 174.0 | 174 |
| `n4-c1-role-retest` | `role` | 4 | 1 | 1–1 | 1.0 | 8 | 3 | 5 | 0 | 0 | 0 | 0.625 | 127.1 / 141.8 / 159.4 / 175.5 | 1155 |

Achieved concurrency: **every cell reached its intended peak.** Two
invocations recorded a lower figure than the cohort's — one `7` in
`n4-c8-thermo_altitude` and one `3` in `n4-c8-position_hold`. That is a
sampling artifact of the registry, not a shortfall: `LiveRegistry`
samples on entry, before each run and after each run, so a `--runs 1`
invocation has only three sample instants and can miss the cohort's
middle entirely. The `mean parallelism` column is an independent check
that does not depend on those instants — total probe run time divided by
the cohort's own wall clock — and it tracks the requested level closely
(6.97-7.76 at C=8), confirming the cohorts really did run concurrently.

### 4.2 Elapsed vs concurrency (`-N4`)

Median elapsed, seconds:

| probe | C=1 | C=2 | C=4 | C=8 | C=8 ÷ C=1 |
|---|---|---|---|---|---|
| `role` | 146.4 | 143.3 | 147.3 | 167.1 | **1.14×** |
| `thermo_altitude` | 80.5 | 88.2 | 107.8 | 203.6 | **2.53×** |
| `position_hold` | 147.9 | 147.4 | 156.6 | 157.3 | **1.06×** |

p90 elapsed, seconds:

| probe | C=1 | C=2 | C=4 | C=8 | C=8 ÷ C=1 |
|---|---|---|---|---|---|
| `role` | 166.5 | 166.4 | 157.7 | 185.6 | **1.11×** |
| `thermo_altitude` | 81.2 | 91.7 | 109.5 | 206.1 | **2.54×** |
| `position_hold` | 174.7 | 174.7 | 166.8 | 171.6 | **0.98×** |

**Elapsed moves with concurrency, and how much depends entirely on what
the probe is waiting for.** `thermo_altitude` is CPU-bound worldgen and
inflates 2.5x by concurrency 8. `position_hold` is an arena probe whose
duration is dominated by real-time waits for AI cadences, and it barely
moves (1.06x). `role` sits between them (1.14x). A single "elapsed
sensitivity" figure for probes as a class would be meaningless.

There were **zero timeouts** in all 168 attempts, so no censored
observations exist and no timeout budget was folded into any elapsed
figure above. The worst single attempt anywhere was 233.1 s
(`n1-c4-thermo_altitude`), 26% of the 900 s budget.

### 4.3 Failure rates vs concurrency (`-N4`)

| probe | C=1 | C=2 | C=4 | C=8 |
|---|---|---|---|---|
| `role` | 0.750 (6/8) | 0.875 (7/8) | 0.500 (4/8) | 0.500 (4/8) |
| `thermo_altitude` | 0.000 (0/8) | 0.000 (0/8) | 0.000 (0/8) | 0.000 (0/8) |
| `position_hold` | 0.125 (1/8) | 0.125 (1/8) | 0.125 (1/8) | 0.000 (0/8) |

**No monotonic concurrency-related degradation was observed across the
twelve eight-attempt primary cells, and both probes that failed at all
were already failing at concurrency 1.** That is the observational
result this table supports:

- `thermo_altitude` is 0/8 at every level — 32 of 32 passes across the
  whole `-N4` column.
- `position_hold` sits at 1/8 for C=1, 2 and 4 and 0/8 at C=8 — no rise
  under load.
- `role` is already heavily flaky **at concurrency 1**: 6/8 solo. Its
  C=4 and C=8 cells (4/8) are *lower* than its C=1 cell (6/8), so the
  column does not ascend with concurrency.

**That is not evidence that failure rates are invariant to concurrency.**
At n = 8 these cells cannot be distinguished from one another. The
Wilson 95% intervals for `role` are 0.409–0.929 (6/8), 0.529–0.978 (7/8)
and 0.215–0.785 (4/8, both cells); every one of them overlaps every
other, and each is wider than the whole range of rates it is being asked
to separate. Overlap at this denominator is what a design with no power
to detect a rate effect produces whether or not an effect exists, so a
concurrency effect on failure rate is **unresolved here, not excluded**,
and the four `role` cells are not in agreement — they are
uninformative about each other. Establishing invariance would require a
stated practical equivalence margin and a randomized, repeated-cohort
design at far larger denominators. Neither was measured, and no such
result is claimed.

The drift control is a single same-cell retest. `n4-c1-role` was
repeated identically 1 h 42 m later:

| cell | started | failure rate | elapsed median |
|---|---|---|---|
| `n4-c1-role` | 01:59:44Z | 0.750 (6/8) | 146.4 s |
| `n4-c1-role-retest` | 03:41:32Z | 0.625 (5/8) | 141.8 s |

Two endpoint samples 1 h 42 m apart differ by 3% in elapsed median and
by one attempt in rate. That is one observed difference, not an estimate
of spread: a single pair has no dispersion to measure, so it supplies
neither a general "+/- 1 attempt" noise bound nor any account of the
7/8 -> 4/8 difference between the C=2 and C=4 cells. What it does show is
that the first and last `n4-c1-role` samples of the session did not
differ greatly in elapsed median — a much weaker statement than a
bounded noise level, and it is offered only as that. It does not bound
session drift, and §7 no longer treats it as bounding the monotonic
scheduling confound.

The checks that failed are a small, recurring set. `role`'s failures are
drawn from three checks (`steer_miner`, `steer_woodcutter`,
`demote_laborer`) and `position_hold`'s single failure per cell is
`work_resumes` at C=1 and C=2 and `control_works` at C=4 — all
AI-timing checks. Which of them appears in a given cell is not stable:
`demote_laborer` is absent from `n4-c1-role` and present only in its
retest, and `control_works` appears only in the C=4 cell. At one to five
occurrences per check per cell, this composition carries even less
resolution than the cell rates above, and no conclusion about which
checks concurrency affects should be drawn from it:

| cell | failing checks |
|---|---|
| `n4-c1-role` | `steer_miner` x4, `steer_woodcutter` x2 |
| `n4-c1-role-retest` | `steer_miner` x3, `demote_laborer` x2, `steer_woodcutter` x1 |
| `n4-c2-role` | `demote_laborer` x5, `steer_miner` x4 |
| `n4-c4-role` | `steer_miner` x4, `demote_laborer` x1, `steer_woodcutter` x1 |
| `n4-c8-role` | `steer_miner` x3, `demote_laborer` x1 |
| `n4-c1-position_hold` | `work_resumes` x1 |
| `n4-c2-position_hold` | `work_resumes` x1 |
| `n4-c4-position_hold` | `control_works` x1 |

### 4.4 Throughput

| probe | metric | C=1 | C=2 | C=4 | C=8 |
|---|---|---|---|---|---|
| `role` | cohort wall (s) | 1188 | 606 | 305 | 195 |
| `role` | attempts/min | 0.40 | 0.79 | 1.57 | 2.46 |
| `thermo_altitude` | cohort wall (s) | 645 | 358 | 217 | 207 |
| `thermo_altitude` | attempts/min | 0.74 | 1.34 | 2.21 | 2.32 |
| `position_hold` | cohort wall (s) | 1227 | 647 | 338 | 174 |
| `position_hold` | attempts/min | 0.39 | 0.74 | 1.42 | 2.76 |

Throughput rises nearly linearly to concurrency 4 for both long probes
(`role` 0.40 -> 1.57 attempts/min, `position_hold` 0.39 -> 1.42). From 4
to 8 the two AI-bound probes still gain (+57% and +94%) but
`thermo_altitude` gains only **+5%** (2.21 -> 2.32) while its elapsed
median almost doubles — that cohort is CPU-saturated, and the extra
invocations buy queueing rather than work.

### 4.5 Capability sensitivity

All three probes at concurrency 4, 8 attempts each; the `-N4` column is
the primary matrix's own `n4-c4-*` cell.

| probe | metric | `-N1` | `-N4` | `-N8` |
|---|---|---|---|---|
| `role` | elapsed median (s) | 184.0 | 147.3 | 158.2 |
| `role` | elapsed p90 (s) | 194.0 | 157.7 | 172.0 |
| `role` | failure rate | 0.875 | 0.500 | 0.625 |
| `role` | setup failures | 0 | 0 | 0 |
| `role` | cohort wall (s) | 381 | 305 | 344 |
| `thermo_altitude` | elapsed median (s) | 223.2 | 107.8 | 141.8 |
| `thermo_altitude` | elapsed p90 (s) | 232.5 | 109.5 | 160.2 |
| `thermo_altitude` | failure rate | 0.000 | 0.000 | 0.000 |
| `thermo_altitude` | setup failures | 0 | 0 | 0 |
| `thermo_altitude` | cohort wall (s) | 448 | 217 | 288 |
| `position_hold` | elapsed median (s) | 149.4 | 156.6 | 153.0 |
| `position_hold` | elapsed p90 (s) | 152.4 | 166.8 | 173.8 |
| `position_hold` | failure rate | 0.125 | 0.125 | 0.000 |
| `position_hold` | setup failures | 1 | 0 | 0 |
| `position_hold` | cohort wall (s) | 302 | 338 | 331 |

`thermo_altitude` only — the one probe measured at every capability x
concurrency combination, all at 8 attempts.

| metric | `-N1` C=1 | `-N4` C=1 | `-N8` C=1 | `-N1` C=4 | `-N4` C=4 | `-N8` C=4 |
|---|---|---|---|---|---|---|
| elapsed median (s) | 182.2 | 80.5 | 68.8 | 223.2 | 107.8 | 141.8 |
| elapsed max (s) | 214.7 | 82.2 | 93.7 | 233.1 | 110.4 | 160.2 |
| failure rate | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 |
| cohort wall (s) | 1511 | 645 | 588 | 448 | 217 | 288 |

**Capability count matters, and the best setting is not the largest
one.** In `thermo_altitude`'s complete block:

- With no contention (C=1), more capabilities are strictly better:
  182.2 s at `-N1`, 80.5 s at `-N4`, 68.8 s at `-N8`.
- Under contention (C=4), that reverses past `-N4`: 223.2 s at `-N1`,
  **107.8 s at `-N4`**, 141.8 s at `-N8`. Four invocations x 8
  capabilities is 32 runnable capabilities on 16 cores, and the
  oversubscription costs 32% against `-N4`.

The same shape holds for the other two probes at C=4: `-N4` is the
fastest median for `role` (147.3 s vs 184.0 / 158.2) and every setting is
within noise for `position_hold`, whose duration is real-time-bound.

**The only setup failure of the entire run happened at `-N1`.**
`n1-c4-position_hold` attempt inv4/run-002 aborted after 86.0 s with all
12 checks `MISSING` and one `WARN` diagnostic, *"the commanded acolyte
never arrived and never held"*
([retained evidence](measurements/probe_concurrency_1427/artifacts/position_hold-20260822T041444Z-46835-18b3565d__run-002.txt):
event stream, empty stdout, full engine log). The engine log shows a
healthy boot, arena init and unit spawn, then shutdown at the probe's own
deadline. This is a single-capability engine missing a real-time AI
deadline while sharing the host with three others — neither a behavioral
regression in the position-hold feature nor a harness error, which is
exactly why the three are classified apart.

## 5. Resource and budget failures

None. Across 168 attempts: 0 timeouts, 0 harness/infrastructure errors,
0 port exhaustions, 0 invocations exiting non-zero, 0 matches against
any known infrastructure signature, and no observed memory pressure
(peak plausible demand at C=8 x `-N4` is 8 engines x `-A128M` nursery
plus heaps, far inside 64 GiB). Every invocation exited 0 and every cell
completed all 8 requested attempts.

## 6. The shared `dist-newstyle` question

`docs/project_review_534-518.md` §PRR-1 records a real concurrency race:
parallel probes launching `cabal run` against one `dist-newstyle` can
die in Cabal's inplace package registration before an engine exists.
Per #1427's amendment the primary matrix was run through that shipped
launch path anyway, with nothing substituted and no per-process build
directories.

**It did not fire once**, at any concurrency up to 8 — 168 attempts, no
`package.conf.inplace` collision, no `removeDirectoryRecursive` failure,
no engine that failed to reach `READY`.

That does not contradict PRR-1, and this run cannot say why the two
differ, because PRR-1's report does not record the build state its
reproduction ran under. What it does establish is that concurrent Cabal
*invocation* against one `dist-newstyle` is not by itself sufficient to
trigger the race: with the build directory complete and unmodified
throughout, eight simultaneous `cabal run`s were harmless here. So the
warm build is treated as a **load-bearing precondition** of every
recommendation below rather than an optimization, and a cold or
concurrently-written build directory is explicitly outside what this run
measured.

Had the race fired it would have been recorded as a harness/setup or
resource result feeding the recommendation, not isolated away — the
classifier in §3 scans every non-pass attempt for exactly those
signatures.

## 7. Cohort schedule

One cohort at a time, in this order. Cohorts 1-13 are the primary matrix
(concurrency-major ascending), 14-21 the RTS subsets.

| # | cell | started (UTC) | finished (UTC) | wall (s) |
|---|---|---|---|---|
| 1 | `n4-c1-role` | 01:59:44Z | 02:19:32Z | 1188 |
| 2 | `n4-c1-thermo_altitude` | 02:19:32Z | 02:30:17Z | 645 |
| 3 | `n4-c1-position_hold` | 02:30:18Z | 02:50:45Z | 1227 |
| 4 | `n4-c2-role` | 02:50:45Z | 03:00:51Z | 606 |
| 5 | `n4-c2-thermo_altitude` | 03:00:51Z | 03:06:49Z | 358 |
| 6 | `n4-c2-position_hold` | 03:06:49Z | 03:17:36Z | 647 |
| 7 | `n4-c4-role` | 03:17:36Z | 03:22:41Z | 305 |
| 8 | `n4-c4-thermo_altitude` | 03:22:41Z | 03:26:18Z | 217 |
| 9 | `n4-c4-position_hold` | 03:26:18Z | 03:31:56Z | 338 |
| 10 | `n4-c8-role` | 03:31:56Z | 03:35:11Z | 195 |
| 11 | `n4-c8-thermo_altitude` | 03:35:11Z | 03:38:38Z | 207 |
| 12 | `n4-c8-position_hold` | 03:38:38Z | 03:41:32Z | 174 |
| 13 | `n4-c1-role-retest` | 03:41:32Z | 04:00:47Z | 1155 |
| 14 | `n1-c4-role` | 04:00:54Z | 04:07:15Z | 381 |
| 15 | `n1-c4-thermo_altitude` | 04:07:15Z | 04:14:43Z | 448 |
| 16 | `n1-c4-position_hold` | 04:14:44Z | 04:19:46Z | 302 |
| 17 | `n8-c4-role` | 04:19:47Z | 04:25:31Z | 344 |
| 18 | `n8-c4-thermo_altitude` | 04:25:32Z | 04:30:20Z | 288 |
| 19 | `n8-c4-position_hold` | 04:30:20Z | 04:35:51Z | 331 |
| 20 | `n1-c1-thermo_altitude` | 04:35:51Z | 05:01:02Z | 1511 |
| 21 | `n8-c1-thermo_altitude` | 05:01:02Z | 05:10:50Z | 588 |

Total measurement wall time: 3 h 11 m, plus the 2 m 14 s warm build.

Concurrency ascends monotonically within the primary matrix, so any
monotone machine drift over the session aliases onto the concurrency
effect and **this design cannot separate the two**. The drift control in
§4.3 does not bound that confound: repeating the *first* cell as cohort
13, at the end of the session, is a single pair of samples, and one pair
estimates no spread. Its 3% elapsed-median difference is small relative
to the elapsed effects in §4.2, which is a reason to think those
particular effects are not drift artifacts; it establishes nothing
comparable for the failure-rate column, where the putative signal and the
cell-level uncertainty are the same size (§4.3). Interleaving or
randomizing the concurrency order across cohorts would remove the
confound by design; this run did not, and the confound therefore stands
unresolved.

## 8. Reproducing this

Each cohort is one command. `cohort.sh`, `primary.sh`, `rts.sh` and
`aggregate.py` are retained verbatim beside the data.

```bash
# One cohort: <probe> <rts-caps> <concurrency> <total-attempts> <outdir>
bash docs/measurements/probe_concurrency_1427/cohort.sh \
     role 4 8 8 /tmp/probe-1427/n4-c8-role

# which expands to, from the repository root:
for i in $(seq 1 8); do
  python3 tools/probe_flake.py --probe role --runs 1 --rts-caps 4 \
    --artifact-root /tmp/probe-1427/probe-flake-artifacts \
    --result /tmp/probe-1427/n4-c8-role/inv${i}.json \
    > /tmp/probe-1427/n4-c8-role/inv${i}.stdout \
    2> /tmp/probe-1427/n4-c8-role/inv${i}.stderr &
done; wait

# The full primary matrix (13 cohorts) and the RTS subsets (8 cohorts).
# The output root is REQUIRED and has no default:
bash docs/measurements/probe_concurrency_1427/primary.sh /tmp/probe-1427
bash docs/measurements/probe_concurrency_1427/rts.sh     /tmp/probe-1427

# Re-derive summary.json from the retained result documents:
python3 docs/measurements/probe_concurrency_1427/aggregate.py \
        docs/measurements/probe_concurrency_1427/cohorts
```

`cohort.sh` refuses to start while any `probe_flake.py` invocation is
registered machine-wide, and records the commit, the tree-clean flag and
the cohort's own wall clock. All three scripts also refuse an output
directory inside any git working tree — judged before anything is
created — so a rerun cannot land on top of the checked-in dataset this
report cites, and retained probe artifacts go to
`<output-root>/probe-flake-artifacts` rather than a fixed home path.
Build the executable once (`cabal build exe:synarchy`) before the first
cohort — §6.

The commands above write per-invocation `.stdout`/`.stderr` captures and
a directory per retained attempt, which is what produced this data. The
checked-in copy packages those down to fit the review payload limit: one
`.txt` bundle per retained attempt with deterministic engine-log boot
noise elided and counted, and only the launcher's stderr progress kept
as `invocation_launches.txt`. Every invocation's `inv<i>.json` result
document is verbatim and still its own file, and the data
[README](measurements/probe_concurrency_1427/README.md) states exactly
what was left out and why.

## 9. Recommendation

**Cap concurrent `probe_flake.py` harness invocations at 4 on this host,
at the shipped `-N4` default, after a warm build.**

The decision rule, and what each part of the evidence contributes:

1. **Stability neither supports nor rules out a higher cap.** No
   monotonic rate degradation was observed up to concurrency 8, and
   concurrency 8 produced zero timeouts, zero setup failures and zero
   harness errors (§5) — but at n = 8 per cell the failure-rate column
   cannot resolve a concurrency effect either way (§4.3), so it carries
   no weight here. **The cap rests entirely on the elapsed-tail,
   throughput and oversubscription evidence in items 2–4**, which is
   independent of the rate column and unaffected by its limits.
2. **Timeout headroom does constrain it.** The binding number is
   `thermo_altitude`'s elapsed tail: p90 81.2 s solo -> 206.1 s at C=8, a
   2.5x inflation on the most CPU-bound workload. That is still only 23%
   of the 900 s budget for *this* probe, but #1426's census targets far
   slower probes — `farm_ai` alone is documented around 11.5 minutes solo
   (`tools/run_probes.py:460-461`, `tools/ci_probes.py:403-405`). A
   comparable 2.5x would exceed the budget outright. That is an
   **untested extrapolation**, not a result:
   `farm_ai` is legacy, this harness rejects legacy probes by name, and
   nothing here measured it.
3. **Throughput stops paying at 4.** C=1 -> C=4 buys 3.9x on `role` and
   3.6x on `position_hold`. C=4 -> C=8 buys +5% on the CPU-bound
   `thermo_altitude` while nearly doubling its elapsed (§4.4).
4. **Capability count and concurrency interact, and 4 x 4 is the
   inflection.** `-N8` beats `-N4` with no contention and loses to it by
   32% at C=4 (§4.5). Four invocations at `-N4` is 16 runnable
   capabilities against 16 cores; the settings that oversubscribe (`-N8`
   at C=4) or starve (`-N1` anywhere) are both measurably worse, and
   `-N1` under contention produced the run's only setup failure.
5. **Resource headroom is not the limit** on this host (§5).

**4 is a recommended cap among the tested levels for agents running this
probe harness on this host, not an established maximum.** Concurrency 8
produced no timeout, setup failure or harness error for these three
workloads and is simply the highest level tested; nothing here
establishes where the true ceiling is. The result is specific to a
16-core Apple M3 Max with a warm build, and to these three probes — it
does not transfer to other hardware, to CI's Linux runners, or to
unrelated repository workloads such as `cabal build`, the hspec suite or
`make ci`.

**#1425's `-N4` default needs no change, so no follow-up decision issue
is filed.** Every cell that could have indicted it exonerated it. At
concurrency 4, `-N4` had the fastest median for `role` (147.3 s) and
`thermo_altitude` (107.8 s), and for `position_hold` all three settings
(149.4 / 156.6 / 153.0 s) fall well inside that probe's own solo
run-to-run spread of 137.5-182.8 s. `-N4` produced no setup failure at
any concurrency; the run's only setup failure was at `-N1`.

## 10. Limits of this characterization

- One host, one OS, one architecture. Nothing was measured on Linux or
  on CI hardware.
- Three probes. They were chosen as #1427's readiness prerequisites and
  span worldgen-heavy and real-time-AI classes, but they are not a
  sample of the ~85 registered probes, and the slowest probes in the
  registry are legacy and unmeasurable by this harness.
- n = 8 per cell. One attempt (0.125) is a cell's *granularity*, not its
  resolution. The Wilson 95% interval at n = 8 is 0.324 wide at the
  extremes (0/8, 8/8) and 0.570 wide mid-range (4/8) — roughly 2.6 to
  4.6 times that granularity. Cell-level rate uncertainty is therefore
  broad enough that the entire `role` column is mutually
  indistinguishable and no rate comparison in §4.3 is resolved. These
  widths describe that uncertainty; they are not a minimum detectable
  effect, which would additionally depend on the comparison chosen, the
  equivalence margin and the study design. `thermo_altitude`'s elapsed
  shift is a separate quantity and is far outside n = 8 noise.
- Concurrency ascends monotonically within the primary matrix, so
  session drift is confounded with the concurrency effect. The §4.3
  drift control does not bound that confound (§7); only an interleaved
  or randomized cohort order would remove it.
- The `-N1`/`-N8` sweep covers concurrency 1 and 4 only, and its C=1 half
  covers `thermo_altitude` only.
- `peak_concurrency` is sampled at three instants in a `--runs 1`
  invocation, so a single invocation's figure can understate the
  cohort's; §4.1's mean-parallelism column exists because of that.
