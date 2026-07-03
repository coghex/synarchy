# Worldgen setup/timeline phase profile (2026-07)

**Date:** 2026-07-03
**Issue:** #448
**Build:** `cabal build exe:synarchy --enable-profiling -f profile --builddir=dist-prof`
(prod `-O2` + `-fprof-late`, per `synarchy.cabal`'s `profile` flag)
**Seeds/sizes tested:** seed 42 / w128, seed 1337 / w128, seed 42 / w64 (all plates=3)

## Gotcha: profiling + parallel sparks segfaults — pin `-N1`

The first attempt (`+RTS -p -RTS`, default `-N` from the binary's baked-in
`-with-rtsopts=-N -A128M`) crashed with `SIGSEGV` inside the RTS every time,
seconds into `buildTimeline`. The macOS crash report shows the fault on a
`ghc_worker` thread inside `pushCostCentre` → `schedule` — a GHC RTS
cost-centre-stack race under concurrent sparks, not an engine bug. This
codebase parallelizes worldgen with `Control.Parallel.Strategies`
(`parListChunk 64 rdeepseq`, e.g. `World.Geology.Timeline.buildTimelineStageCache`),
and GHC 9.12.2's profiled RTS doesn't reliably handle sparks pushing/popping
cost-centre stacks across capabilities.

**Workaround:** override the baked-in RTS options at the command line —
`+RTS -N1 -p -RTS` (the exe is built with `-rtsopts`, so this is honored).
Single-capability profiling runs are the only ones used below. Anyone
profiling worldgen in the future needs this; the `dev`/`profile` build docs
should point here.

**Side-effect of `-N1`:** the dump command's own watchdog
(`waitForInit`, `app/Main.hs:353`, `max 300 (worldSize * 4)` seconds) is sized
for normal prod throughput and fires before a `-N1`+profiled run — which is
both parallelism-starved (loses the ~6.9× speedup `parListChunk` normally
gives) and instrumented (`-fprof-late` cost-centre bookkeeping) — finishes.
That's fine for profiling purposes: the world thread keeps running after the
main thread's timeout exception, and finishes the full setup phase (timeline
→ ocean map → climate refine, confirmed by the "Tectonic Plates" /
"Flora catalog snapshot" log lines all appearing) shortly after, and the RTS
still writes a complete final `.prof` report on exit. The dump never reaches
its JSON output in this configuration, but that's not what this profile run
needs.

## Results

| Cost centre | Location | seed42 w128 | seed1337 w128 | seed42 w64 |
|---|---|---|---|---|
| `applyTimelineChunk.applyOnePeriod` (per-tile loop) | `World/Generate/Timeline.hs:357-432` | 16.8% | 22.3% | 12.9% |
| `...erosionMod` branch of the same loop | `World/Generate/Timeline.hs:405-428` | 11.6% | 16.9% | 8.9% |
| **combined loop total** | | **28.4%** | **39.2%** | **21.8%** |
| `wrappedValueNoise2D` | `World/Plate.hs:423-455` | 9.1% | 4.1% | 7.9% |
| `hashCoord` | `World/Plate.hs:823-832` | 6.1% | 2.8% | 5.4% |
| `applyRiverCarve` | `World/Hydrology/River/Carving.hs:32-33` | 7.0% | 3.8% | 8.9% |
| `$wcarveFromSegment` | `World/Hydrology/River/Carving.hs:90` | 1.4% | 1.7% | 7.5% |
| `$wapplyErosionScalar` | `World/Geology/Erosion.hs:118` | 4.7% (9.4% alloc) | 5.9% (14.9% alloc) | 3.6% (9.2% alloc) |
| `identifyWorldRivers` | `World/Fluid/River/Identify.hs:185-218` | 1.9% (29.2% alloc) | — (<1%) | 1.6% (16.1% alloc) |

Total sampled time / allocation per run: seed42 w128 = 449.92s / 583 GB;
seed1337 w128 = 448.71s / 479 GB; seed42 w64 = 141.17s / 149 GB. These
absolute numbers are **not** representative of prod wall-clock — `-N1` gives
up the ~6.9× `parListChunk` speedup and `-fprof-late` cost-centre bookkeeping
adds its own large constant overhead (prod w128 setup is ~9s per the
2026-07-01 perf survey vs. ~450s here). Only the **relative %** ranking is
meaningful, and it holds stable across two seeds and two world sizes: the
per-tile timeline-replay loop dominates (22–39%), followed by the
`World.Plate` terrain-noise stack (`wrappedValueNoise2D`/`hashCoord`,
8–18% combined) and river carving (5–16%).

## Why the top cost centre is what it is

`applyTimelineChunk` isn't called once — `World.Geology.Timeline.buildTimelineStageCache`
(`src/World/Geology/Timeline.hs:308-328`) calls it once **per chunk, for
every chunk in the world** (`worldSize²` chunks — 16,384 at w128), replaying
the *entire* geological timeline's periods onto each chunk independently so
that stitching the results gives a window-position-independent, deterministic
global terrain (the parallelism-and-determinism tradeoff is called out
explicitly in the surrounding comments). It's already parallelized via the
same `parListChunk 64 rdeepseq` that segfaults under profiling, and it's
already been profiled and partially optimized before — a comment at
`Timeline.hs:145` notes the exploded river-carve-event cache was added
because recomputing it per-tile "was 35% of CPU in profiling." This is a
known, previously-worked hot path, not a fresh discovery.

## Decision

Not "irreducible, stop here" — but not a safe blind optimization either.
The per-chunk replay is architecturally deliberate (parallel + deterministic
by construction), so the accessible remaining angle isn't "do less work" but
"stop redoing the *same* work": adjacent chunks each carry a `chunkBorder`
margin and independently recompute `World.Plate` base-terrain noise
(`wrappedValueNoise2D`/`hashCoord`) for their overlapping border tiles.
Whether that overlap recomputation is large enough to matter, and whether
it can be memoized without changing worldgen output (byte-identical
requirement — see `CLAUDE.md` Testing Tiers), needs its own investigation;
guessing at a fix here risked a worldgen-output change without the tier-3
rebaseline discipline that requires. Filed as a scoped follow-up: #500.

## Reproducing

```bash
cabal build exe:synarchy --enable-profiling -f profile --builddir=dist-prof
cabal run exe:synarchy --enable-profiling -f profile --builddir=dist-prof -- \
    --dump --seed 42 --worldSize 128 --plates 3 --region -1,-1,1,1 \
    +RTS -N1 -p -RTS
# synarchy.prof written to the cwd; -po<name> sets the output filename
# for parallel runs.
```
