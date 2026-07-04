# Worldgen setup/timeline phase profile (2026-07)

**Date:** 2026-07-03
**Issue:** #448
**Build:** `cabal build exe:synarchy --enable-profiling -f profile --builddir=dist-prof`
(prod `-O2` + `-fprof-late`, per `synarchy.cabal`'s `profile` flag)
**Seeds/sizes tested:** seed 42 / w128, seed 1337 / w128, seed 42 / w64 (all plates=3)

## Gotcha 1: profiling + parallel sparks segfaults — pin `-N1`

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
Single-capability profiling runs are the only ones used below.

## Gotcha 2: don't use `--dump` for a profiled run — its watchdog isn't caller-controlled

`--dump` mode's `waitForInit` (`app/Main.hs:353`) has a hardcoded budget —
`max 300 (worldSize * 4)` seconds — sized for normal prod throughput. A
`-N1`+`-fprof-late` run is both parallelism-starved (loses the ~6.9×
`parListChunk` speedup) and instrumented, so it routinely blows through that
budget. On timeout, `waitForInit` returns `False`, `runDump` throws
`TimeoutError`, and the `Left` branch of `runDump` (`app/Main.hs:470`) calls
`shutdownThread` on every thread in sequence — a fixed 10 s cooperative wait
(`Engine.Core.Thread.shutdownThread`, `src/Engine/Core/Thread.hs:26`) before
`killThread`. **This does not reliably give a profiled `buildTimeline` call
enough time to finish** — an earlier draft of this doc claimed the world
thread "keeps running and finishes shortly after," inferred from log lines
that turned out to be logger-buffered output landing in file order, not
evidence the computation actually outran the kill. A future run with a
slower seed or a bigger world could just as easily get killed mid-`force`,
silently producing a truncated profile that looks complete.

**Fix:** don't use `--dump` for a profiled capture at all. Use `--headless`
instead — its main loop (`Engine.Loop.Headless.headlessLoop`) has no
watchdog of its own; `world.init` + `world.waitForInit(<seconds>)` are
driven over the debug-console TCP with a timeout **the caller controls**,
and nothing tears down threads if that timeout elapses (the `nc` client
just disconnects — the engine process and the world thread underneath it
keep running untouched). Only call `engine.quit()` once `waitForInit`
actually reports done, so the shutdown that follows is the normal `Right _`
path (idle threads, cooperative stop, no `killThread`):

```bash
cabal build exe:synarchy --enable-profiling -f profile --builddir=dist-prof
cabal run exe:synarchy --enable-profiling -f profile --builddir=dist-prof -- \
    --headless --port 9448 +RTS -N1 -p -RTS > /tmp/engine.log 2>&1 &
until grep -q READY /tmp/engine.log; do sleep 1; done

echo 'world.init("test", 42, 128, 3)' | nc -w 2 localhost 9448
# Generous, caller-controlled — re-issue with a fresh budget if it elapses
# before waitForInit itself returns; the engine keeps working regardless.
echo 'return world.waitForInit(900)' | nc -w 900 localhost 9448
echo 'engine.quit()' | nc -w 3 localhost 9448
# synarchy.prof written to the cwd on the clean shutdown that follows.
```

Confirmed clean for all three runs below: each log ends with `World
initialized: ... : test` (the world thread reaching `LoadDone` on its own)
followed by `Headless engine shutting down...` from the *normal* shutdown
path, not the exception/kill path — i.e. no watchdog raced the computation.
`-po<name>` sets a distinct output filename for concurrent runs.

Aside: the w64 run's table below shows a small `$wc_pollLoop
GHC.Internal.Event.Poll` entry (~4%) that the `--dump`-based captures didn't
have — `headlessLoop`'s own 16.6 ms message-poll tick, interleaved on the
same `-N1` capability. It doesn't change the ranking; noted for anyone
diffing against the superseded `--dump` numbers.

## Results

| Cost centre | Location | seed42 w128 | seed1337 w128 | seed42 w64 |
|---|---|---|---|---|
| `applyTimelineChunk.applyOnePeriod` (per-tile loop) | `World/Generate/Timeline.hs:357-432` | 15.2% | 20.6% | 12.1% |
| `...erosionMod` branch of the same loop | `World/Generate/Timeline.hs:405-428` | 10.6% | 15.7% | 8.4% |
| **combined loop total** | | **25.8%** | **36.3%** | **20.5%** |
| `wrappedValueNoise2D` | `World/Plate.hs:423-455` | 8.9% | 4.3% | 7.4% |
| `hashCoord` | `World/Plate.hs:823-832` | 6.1% | 2.9% | 4.9% |
| `applyRiverCarve` | `World/Hydrology/River/Carving.hs:32-33` | 6.6% | 4.0% | 8.3% |
| `$wcarveFromSegment` | `World/Hydrology/River/Carving.hs:90` | 1.3% | 1.9% | 7.0% |
| `$wapplyErosionScalar` | `World/Geology/Erosion.hs:118` | 4.2% (8.9% alloc) | 5.3% (11.9% alloc) | 3.3% (9.0% alloc) |
| `identifyWorldRivers` | `World/Fluid/River/Identify.hs:185-218` | 1.9% (27.5% alloc) | — (<1%) | 1.5% (15.9% alloc) |

Total sampled time / allocation per run: seed42 w128 = 454.0s / 620 GB;
seed1337 w128 = 774.0s / 1,034 GB; seed42 w64 = 151.2s / 151 GB. (Seed 1337
simply rolled a geologically busier timeline — more/longer ages — hence the
much larger total; that's expected seed-to-seed variance, not a methodology
artifact.) These absolute numbers are **not** representative of prod
wall-clock — `-N1` gives up the ~6.9× `parListChunk` speedup and
`-fprof-late` cost-centre bookkeeping adds its own large constant overhead
(prod w128 setup is ~9s per the 2026-07-01 perf survey vs. several minutes
here). Only the **relative %** ranking is meaningful, and it holds stable
across two seeds and two world sizes: the per-tile timeline-replay loop
dominates (20–36%), followed by the `World.Plate` terrain-noise stack
(`wrappedValueNoise2D`/`hashCoord`, 7–15% combined) and river carving
(6–15%). These numbers also closely reproduce an earlier, methodologically
unsound `--dump`-based capture (28.4% / 39.2% / 21.8% for the same three
runs) — reassuring in substance, but that capture's numbers weren't trusted
as the basis for this report; the headless-based numbers above are.

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
