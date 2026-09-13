# Why the headless test suite is expensive to compile (#2276)

Measured 2026-09-13 against `master@494b8d9f8`. This is an investigation
report: it records measurements, a validated cascade model, and a ranked
remedy table. It implements no remedy.

**Headline.** The suite's own source is not the problem, and neither is
its import surface. Compiling all 484 modules costs 582.7 CPU-seconds, of
which 74.7% is the Core-to-Core simplifier working on code the modules
themselves contain. On CI the step is bimodal: 55% of runs pay 86 s and
38% pay ~398 s, and the split is explained by the project build cache's
lineage, not by the pull request's own diff. Every structural remedy the
issue proposed measures at or below zero.

## 0. Corrections to the dossier

The issue's premises were measured at 2026-09-02 and have moved.

| Dossier claim | Measured at `494b8d9f8` |
|---|---|
| 348 test modules | **484** (483 `other-modules` + `Spec.hs`; 478 under `test-headless/`, 6 under `app/`) |
| 807 library modules | **897** exposed modules |
| Suite build paid on BOTH lanes | **One lane.** #2274 landed; `behavior-probes` builds only `exe:synarchy` and `exe:synarchy-save-codec` (`.github/workflows/ci.yml:1619-1634`). `test-and-audits` is the only job that compiles it (`ci.yml:532-540`). |
| Build test suites 4.6 min (271 s) | p50 **94 s** over 60 successful runs; see §4 for the full distribution |
| Headless suite 5.2 min | p50 **838 s** — the suite *run* is now 9x the suite *build* at the median |
| 109 modules import `Test.Headless.Harness` | **90** import the facade exactly; 180 import something in the `Test.Headless.Harness*` namespace; 397 of 478 import `Engine.*`/`World.*` directly |
| `UI/TransferSession.hs` 2,157 lines | 47-line facade since `ac201eafc`; the current >1,900-line files are `UI/ResponsiveMenus.hs` (2,149) and `World/Save/Storage.hs` (1,965) |

The reviewer's instruction to use the replay's observed recompilation
count rather than the constant 211 is followed throughout §2.

## 1. Where the compile time goes

### Method

Two full rebuilds of `synarchy-test-headless` in an isolated worktree,
both against a library already built with identical flags, both verified
as genuine full rebuilds by counting `Compiling` lines (484 of 484).
`pgrep -fl ghc` was empty before each; no other build ran on the machine.

**Machine:** Apple Silicon, 16 cores (`hw.ncpu` = `hw.physicalcpu` = 16),
64 GB. GHC 9.12.2, cabal-install 3.16.1.0, production profile
(`-O2 -optc-O3`), `--builddir=dist-2276`.

**The `-j` trap.** GHC's `-ddump-timings` measures each phase with
`getCPUTime`, which is *process-wide*. Under `-j16` every phase's `time=`
therefore counts all sixteen worker threads' CPU for its interval: the
phase totals summed to 11,254 s against a process that consumed 930.8
CPU-s, a 12x over-count. Per-module and per-phase attribution below is
taken from a **sequential** (`-j1`) rebuild, where the deltas are
single-threaded and attributable. Wall-clock and parallel behaviour come
from the `-j16` rebuild. Phase CPU totals are not additive wall-clock
values and are not presented as such.

### Whole-suite totals

| Build | Wall | Process CPU | Modules |
|---|---:|---:|---:|
| `-j16` (cabal's own flags) | **119.5 s** | 930.8 s | 484 |
| `-j1` (same flags, `-j1`) | **599.1 s** | 582.7 s | 484 |

Speedup 5.0x on 16 cores, at 1.6x the CPU — the parallel build spends
348 extra CPU-seconds on contention and GC. Link is separate and is
measured in §4.

### Phase split (sequential run)

| Phase | CPU s (vanilla) | % | CPU s (`-dynamic-too` second pass) | total |
|---|---:|---:|---:|---:|
| Parsing (Parser + ConsistencyCheck) | 1.7 | 0.5% | 0.0 | 1.7 |
| Renaming / typechecking | 5.9 | 1.6% | 0.0 | 5.9 |
| Desugaring | 3.8 | 1.1% | 0.0 | 3.8 |
| Simplification (Core-to-Core) | 267.7 | 74.7% | 0.0 | 267.7 |
| Code generation (Tidy/Prep/Stg/CodeGen) | 78.6 | 21.9% | 75.5 | 154.1 |
| Assembly (systool:as) | 0.5 | 0.1% | 0.4 | 0.9 |
| Interface writing (WriteIface) | 0.2 | 0.1% | 0.2 | 0.4 |
| **Attributed subtotal** | **358.3** | **100%** | **76.1** | **434.4** |
| Dependency chasing (whole session) | 0.3 | — | — | 0.3 |
| Unattributed (interface loading, GC, I/O) | — | — | — | 148.0 |
| **Measured process CPU** | | | | **582.7** |

`-ddump-timings` has no interface-loading phase; loading happens inside
`Renamer/typechecker` and outside any `withTiming` region, which is why
§3 measures it separately rather than reading it off this table.
`WriteIface` is reported outside renaming/typechecking, and linking is
reported outside per-module compilation (§4).

`-dynamic-too` is forced here: GHC on macOS/aarch64 is dynamically
linked, so Template Haskell requires dynamic objects. Removing
`-dynamic-too` from the command line changes nothing — GHC re-enables it
and still emits 483 `.dyn_o` files. The Linux CI image installs a
statically-linked GHC via ghcup, so **the 76.1 s second code-generation
pass is a macOS-only cost and is excluded from every CI projection below.**

### The twenty slowest modules

| # | CPU s | lines | imports | module |
|---:|---:|---:|---:|---|
| 1 | 14.20 | 891 | 7 | `Test.Headless.UI.TransferSession.Lifecycle` |
| 2 | 7.12 | 1376 | 21 | `Test.Headless.UI.TransferContextMenu` |
| 3 | 6.47 | 376 | 7 | `Test.Headless.UI.TransferSession.Failure` |
| 4 | 5.52 | 2149 | 14 | `Test.Headless.UI.ResponsiveMenus` |
| 5 | 5.20 | 1797 | 7 | `Test.Headless.UI.ResponsiveGameplay.Surfaces` |
| 6 | 5.07 | 1290 | 56 | `Test.Headless.Asset.TextureFallback` |
| 7 | 4.97 | 1215 | 29 | `Test.Headless.World.MapPyramid` |
| 8 | 4.95 | 1212 | 408 | `Spec` |
| 9 | 4.47 | 1635 | 43 | `Test.Headless.World.Solidification` |
| 10 | 4.34 | 1965 | 44 | `Test.Headless.World.Save.Storage` |
| 11 | 4.04 | 1427 | 47 | `Test.Headless.World.Save.Integrity` |
| 12 | 3.90 | 845 | 28 | `Test.Headless.Structure.ConstructionPacks` |
| 13 | 3.79 | 1427 | 54 | `Test.Headless.Item.Condition` |
| 14 | 3.68 | 1025 | 36 | `Test.Headless.Structure.ConstructionFrames` |
| 15 | 3.52 | 1314 | 48 | `Test.Headless.World.DesignationSeam` |
| 16 | 3.46 | 958 | 45 | `Test.Headless.Building.DestructionPresentation` |
| 17 | 3.45 | 1263 | 52 | `Test.Headless.World.GeneratedLibrary` |
| 18 | 3.38 | 989 | 39 | `Test.Headless.Structure.ArtCatalog` |
| 19 | 3.38 | 941 | 20 | `Test.Headless.Location.Instance` |
| 20 | 3.27 | 791 | 21 | `Test.Headless.UI.TransferGestures` |

The top 20 are 98.2 s of 434.4 s of attributed phase time — **22.6%**.
The median module costs 0.505 s, the mean 0.898 s. Line count is a weak
predictor: Pearson r(lines, CPU) = 0.728 over 484 modules, and the most
expensive module in the suite is 891 lines while the longest is 2,149.

## 2. The cascade

### Replay

The dossier's run 33666483367 cannot be re-applied to today's tree (its
`src/` diff no longer applies in either direction). Instead the real
event was re-run: a worktree at `0cdba2113` (the first parent of PR
#2586's merge, `83535fb52`), fully built, then advanced to `83535fb52`
and rebuilt. PR #2586 changed **11 files under `src/`** — the same shape
as the dossier's 11-file change — plus 7 files under `test-headless/`.
Its changed modules (`Structure.ArtCatalog`, `World.Command.Types`,
`World.Construct.Plan`, `Engine.Scripting.Lua.API.StructureArt` and
seven more) reach the engine-state and Lua-thread interfaces, giving the
same broad fan-out the original had.

Comparability: the original recompiled 211 of 348 test modules
(60.6%); this replay recompiled **270 of 447 (60.4%)**.

| | Library | Test suite |
|---|---:|---:|
| Modules in the component at `0cdba2113` | 855 | 447 |
| Recompiled | **271** (31.7%) | **270** (60.4%) |
| Wall at `-j16` | 42.5 s | 93.1 s (incl. link) |
| Interfaces whose **ABI hash** changed | **239** | — |

32 of the 271 recompiled library modules produced an unchanged ABI, so
propagation stopped at them.

### Changed interfaces, not changed files

Recompilation was determined from ABI fingerprints, not timestamps.
Every `.hi` in the library was read with `ghc --show-iface` before and
after and compared on `ABI hash`. That is exactly the hash a consumer
records: `Test.Headless.World.Save.Storage`'s interface lists
`import -/ World.Save.Storage 8519e7e5b0fc8e14e7baf56c6f729724`, which
is `World/Save/Storage.hi`'s own `ABI hash`.

**The granularity is the finding.** The library is a *separate package*
from the test component, so a test module's usage entry for a library
module is a single whole-module ABI hash with no per-name refinement.
(Home-package usages — one test module importing another — do get
per-name hashes: `Test.Headless.Harness.Isolation` appears with
`exports: <hash>` and `withExclusiveTempDirectory <hash>` beneath it.)
Changing any exported declaration of a library module therefore
recompiles every test module that records that module, whether or not
the changed declaration is the one it uses.

### Model, and its validation

From the 484 test interfaces, a test module recompiles iff its usage
list intersects the ABI-changed set, then transitively through
home-package usages. Against the replay:

| | Modules |
|---:|---|
| Direct usage of a changed library interface | 255 |
| + transitive through home-package usages | **268** |
| **Observed recompiled** | **270** |

99.3% accurate; the two extra are files the PR edited itself. The
model is used for the counterfactuals below.

### Attribution (many-to-many; counts do not sum)

| Test modules recording it | Changed interface |
|---:|---|
| 213 | `Engine.Core.State` |
| 101 | `Engine.Scripting.Lua.Thread` |
| 98 | `Engine.Scripting.Lua.API` |
| 88 | `Engine.Scripting.Lua.Thread.Console` |
| 48 | `Engine.Core.Init` |
| 27 | `Engine.Graphics.Vulkan.Types.Vertex` |

`Engine.Core.State` is recorded by 213 of the 270, but is the *sole*
changed usage for only **39**. The remaining 174 record at least one
other changed interface, so removing the hub does not remove them.
Counterfactual over the model, holding one interface ABI-stable:

| Held stable | Library upper bound | Test upper bound |
|---|---:|---:|
| (nothing) | 472 | 311 |
| `Engine.Core.State` | 424 | **282** (−9.3%) |
| `Engine.Scripting.Lua.Thread` | 471 | 311 (−0%) |
| `Engine.Core.Init` | 471 | 308 (−1%) |
| all four hubs above | 421 | 282 (−9.3%) |

The cascade is broad and multi-path, not funnelled through one hub.

### What a narrower harness would avoid: zero

Of the 255 test modules the cascade recompiled, **0** lack a direct
`import Engine.*` / `import World.*` in their own source. There is no
module in the suite whose only route to a library interface is the
`Test.Headless.Harness` facade. The facade's nine library imports
(`Engine.Core.Init`, `Engine.Core.Queue`, `Engine.Core.State`,
`Engine.Core.Thread`, `Engine.Graphics.Camera`, `UPrelude`,
`World.Chunk.Queue`, `World.Thread`, `World.Types`) are all reachable
another way for every module that records them.

Separately: `UPrelude` is a recorded usage of **480 of 484** test
modules. Any ABI change to it recompiles the entire suite.

### What a split component would avoid: zero

192 of 447 test modules were not recompiled by this cascade — 188 of
them still record library usages, just none that changed this time.
Moving them into a separately linked component saves nothing they do not
already get for free, and adds a second link (§4). Only 4 test modules
record no library usage at all, and all four are short facades
(`Lua/SaveModules.hs` 66 lines, `Unit/Atlas.hs` 33, `World/Save/Compat.hs`
59, `World/Save/Components.hs` 31).

## 3. Interface loading, measured separately

Two independent measurements, both saying the cost is what the modules
*contain*.

**Import-only twins.** For each probe, a module with an identical import
list and an empty body, compiled in its own GHC session with the suite's
own flags (best of 3):

| Probe | Imports | Wall | CPU |
|---|---:|---:|---:|
| `Z0` — no imports (session floor) | 0 | 2.19 s | 2.17 s |
| `Z3` — `UI/TransferSession/Lifecycle.hs`'s imports | 7 | 2.18 s | 2.16 s |
| `Z4` — `UI/ResponsiveMenus.hs`'s imports | 14 | 2.11 s | 2.10 s |
| `Z2` — `Test/Headless/Harness.hs`'s imports | 19 | 2.23 s | 2.21 s |
| `Z6` — `World/Save/Storage.hs`'s imports | 44 | 2.14 s | 2.13 s |
| `Z1` — `import Test.Headless.Harness` | 1 | 2.23 s | 2.21 s |
| `Z5` — `Spec.hs`'s imports (the whole surface) | 408 | 2.44 s | 2.42 s |

Building the import environment for the largest import surface in the
suite costs **0.25 s** over a zero-import session.

**Residual per module.** Compiling one real module alone in a fresh
session, against the 2.17 s floor and its own phase total:

| Module | Session CPU | Phase total | Floor | Residual |
|---|---:|---:|---:|---:|
| `UI.TransferSession.Lifecycle` | 16.28 s | 12.55 s | 2.17 s | 1.56 s |
| `UI.ResponsiveMenus` | 10.21 s | 6.60 s | 2.17 s | 1.44 s |
| `World.Save.Storage` | 7.73 s | 4.68 s | 2.17 s | 0.88 s |

Across the whole sequential build the residual is 148.0 s over 484
modules — 0.31 s each, and `--make` loads each external interface once
per session into the EPS, so it is not paid per consumer.

**Answer:** the cost is what these modules contain. Interface loading is
under 2% of it.

## 4. CI: what this costs per pull request

60 successful runs (2026-09-06 → 2026-09-13), `test-and-audits` job.

| Step | min | p50 | p90 | max |
|---|---:|---:|---:|---:|
| Build (library + executable) | 21 s | 60 s | 194 s | 353 s |
| **Build test suites** | **41 s** | **94 s** | **443 s** | **490 s** |
| Headless test suite | 489 s | 838 s | 873 s | 914 s |

Job wall times over the 16 most recent pull-request runs (p50):
`test-and-audits` **1150 s**, `behavior-probes` 548 s, `static-audits`
359 s. `test-and-audits` is the critical path, and
the suite *build* is 8% of it at the median while the suite *run* is 73%.

The step is bimodal, not noisy:

| Bucket (by library build time) | n | share | Build test suites p50 |
|---|---:|---:|---:|
| warm, small library change (<100 s) | 33 | 55% | **86 s** |
| large library rebuild (100–250 s) | 23 | 38% | **398 s** |
| cold / full library rebuild (>250 s) | 4 | 7% | **488 s** |

Expected cost per run: **232 s**. Measured floor 41 s — a run that
recompiled essentially nothing still paid 41 s of cabal overhead plus
the link of a 484-module executable. Locally, one edited leaf module
costs 9.5–10.8 s wall and 20.1 s CPU on 16 cores, of which the module
itself is ~0.5 s: **the link is ~19.5 CPU-s / ~9 s wall here, and 41 s
on a 4-vCPU runner.**

**The expensive bucket is cache lineage, not the diff.** Reading the
cache-restore outcome out of the job logs for the 11 most and 11 least
expensive runs:

| Project-cache restore | n | Build test suites p50 |
|---|---:|---:|
| exact key hit | 18 | **78 s** |
| prefix fallback | 4 | **488 s** |

and 4 of the 17 expensive runs with a resolvable diff **changed no
`src/` file at all**. The mechanism is in `ci.yml:698-708`: the project
cache is saved only on a master push whose primary key *missed*, so one
snapshot is written per (plan hash, image, epoch) lineage and then
frozen while `tools/ci_cache_epoch.py`'s `EPOCH_SIZE = 8` merges of
drift accumulate against it; when the lineage rotates, the restore falls
back through `dist-v3-…-cabal-3.16.1.0-` to an older tree. Runs at
epoch positions 0–2 had a median of 89 s against 327 s at positions 5–7
(n=35), though the per-position correlation is weak (r = 0.17), so
lineage rotation rather than position alone is what the data supports.

Confirmed not to be the mechanism: adding a module to `other-modules`
does not force a full rebuild (3 modules recompiled, 12.6 s) and does
not change `plan.json`, which carries no module lists.

## 5. Remedies, ranked

Savings are 4-vCPU critical-path wall seconds per pull-request run,
weighted across the three buckets above (55% / 38% / 7%) using the
measured compile-only portions (45 s / 357 s / 446 s). The issue's 30 s
threshold is applied to that number. Runner cost is noted where it
differs.

| Rank | Remedy | Saving / PR run | Verdict | Cost |
|---:|---|---:|---|---|
| 1 | **Refresh the project build cache more often** — write a snapshot on every successful master push, or lower `EPOCH_SIZE` | measured differential **488 s → 78 s** between prefix-fallback and exact-hit restores; bucket model puts the reachable ceiling at **146 s** | **worth doing** | more cache storage (~360 MB/snapshot against a 10 GB quota); needs its own measurement to pick between the two mechanisms |
| 2 | Test suite at `-O1` instead of `-O2` | **32.0 s** (measured: 904.5 → 754.0 CPU-s on a forced full rebuild) | **out of scope** (issue §Out of scope) — but see the note below | changes the suite's own codegen, not the product's |
| 3 | Make `Engine.Core.State` ABI-stable against command/queue churn | **15.5 s** (model: 9.3% of the cascade) | **not worth doing** | large refactor of `EngineEnv`'s interface for one tenth of one bucket |
| 4 | Narrow `Test.Headless.Harness`'s import surface | **0 s** (measured: 0 of 255 cascade recompiles avoided) | **not worth doing** | none — there is nothing to gain |
| 5 | Split the largest test modules | **~0 s** | **not worth doing** | top 20 are 22.6% of CPU but the 4-vCPU schedule is CPU-bound with 270–484 modules queued, not tail-bound |
| 6 | Move engine-free specs into a separately linked component | **−41 s** (adds one link; avoids 0 recompiles) | **harmful** | a second link on every run |

**On rank 2.** The dossier records `-O0` vs `-O2` as measured with no
effect (106 s vs 111 s). That does not reconcile with the phase split,
and a controlled re-measurement disagrees. Four forced full rebuilds of
all 484 modules, fresh output directory each time, `Compiling` lines
counted:

| Variant | Wall (`-j16`) | Process CPU | Modules |
|---|---:|---:|---:|
| `-O2` (baseline) | 115.0 s | 904.5 s | 484 |
| `-dynamic-too` dropped from the command line | 113.7 s | 906.7 s | 484 |
| `-O1` | 104.5 s | 754.0 s (−16.6%) | 484 |
| `-O0` | 56.6 s | 484.7 s (−46.4%) | 484 |

`-O0` halves the suite's compile cost, worth **88.8 s** per PR run by
the model above. Changing optimisation flags is out of scope for this
issue and is not proposed here; what is reported is that the "measured,
no effect" premise is not reproducible, and that 46% of the suite's
compile cost is optimisation of test code that is never hot.

**Interactions.** Rank 1 is a tuning of CIR-4, whose epoch scheme
already exists and works as designed; it is not a duplicate of it.
CIR-5 (compilation-producer artifact handoff) and CIR-8 (Hspec parallel
regions) remain reserved and untouched — neither is affected by anything
above, and the far larger prize they aim at is visible in §4: the suite
*run* is 838 s against the build's 94 s at the median. No open issue
covers any remedy in this table; #2276 is the only tracker item on the
subject.

## 6. Reproducing this

```bash
WT=~/worktrees/coghex/synarchy/issue-2276-test-suite-compile-cost
git worktree add "$WT" -b issue-2276-measure origin/master && cd "$WT"
pgrep -fl ghc                       # must be empty

# Library first, same flags, so the timed build is the suite alone.
cabal build lib:synarchy --builddir=dist-2276 \
  --ghc-options="-ddump-timings -ddump-to-file"

# (1) Parallel: wall clock and parallel efficiency.
/usr/bin/time -l cabal build synarchy-test-headless --builddir=dist-2276 \
  --ghc-options="-ddump-timings -ddump-to-file"
grep -c Compiling <log>             # must read 484

# (2) Sequential: the only attributable per-module timings.
#     Capture cabal's own GHC command, then replay it at -j1.
BD=dist-2276/build/aarch64-osx/ghc-9.12.2/synarchy-0.1.0.0
./$BD/setup/setup --working-dir="$PWD" build --verbose=3 \
  --builddir="$BD" synarchy-test-headless > setup_v3.log 2>&1
# the response file's contents are echoed between "contents: <<<" and ">>>"
sed 's/^-j$/-j1/' ts_compile.rsp > ts_compile_j1.rsp
find "$BD/build/synarchy-test-headless/synarchy-test-headless-tmp" -type f \
  \( -name '*.o' -o -name '*.hi' -o -name '*.dyn_o' -o -name '*.dyn_hi' \
     -o -name '*.dump-timings' \) -delete
/usr/bin/time -l ghc "@ts_compile_j1.rsp"
# phases: each <Module>.thr.dump-timings / .thr_dyn.dump-timings holds
#   "<phase> [<Module>]: alloc=<n> time=<ms>"

# (3) Interface loading: an import-only twin per probe, own GHC session.
#     Same response file, module list replaced, plus
#     -Wwarn -Wno-missing-home-modules.

# (4) ABI fingerprints and the usage graph.
ghc --show-iface <module>.hi | awk '/ABI hash:/{print $3; exit}'
ghc --show-iface <module>.hi | grep '^import '   # the recompilation deps

# (5) Cascade replay of a real pull request.
git worktree add --detach ~/worktrees/coghex/synarchy/issue-2276-replay \
  83535fb525d0a00312ff6a19efad8fb6202cb643^1
cabal build lib:synarchy synarchy-test-headless --builddir=dist-r
#   snapshot every library .hi's ABI hash, then:
git checkout --detach 83535fb525d0a00312ff6a19efad8fb6202cb643
cabal build lib:synarchy --builddir=dist-r      # 271 of 855
cabal build synarchy-test-headless --builddir=dist-r   # 270 of 447

# (6) Optimisation and dynamic-too variants: same response file with
#     -O0 / -O1 appended or -dynamic-too removed, each into a FRESH
#     output directory, each verified at 484 Compiling lines.

# (7) CI distribution.
gh run list -R coghex/synarchy --workflow ci.yml --limit 100 \
  --json databaseId,event,conclusion,headBranch
gh api repos/coghex/synarchy/actions/runs/<id>/jobs   # step start/end times
gh api repos/coghex/synarchy/actions/jobs/<id>/logs   # CI_CACHE_EPOCH, restore keys
```

Both measuring worktrees were removed after the run and the primary
checkout was left clean.
