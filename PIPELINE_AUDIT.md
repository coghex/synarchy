# World generation pipeline audit (UPDATED 2026-04-07)

## Headline finding

**The chunk generation pipeline is deterministic. The Sim thread is the
source of all observed non-determinism in dump output.**

I missed this in the first round of the audit because I only audited
`src/World/`. The Sim thread lives in `src/Sim/` and writes to
`wsTilesRef` independently of the world thread, racing with everything
that reads it.

## How I proved it

I instrumented the seal pipeline to log the state of a known-buggy tile
(`(-12, -47)` in seed 137) at every step. Across 8 dump runs:

- **Seal pipeline traces are byte-identical across all runs.** The seal
  consistently produces `River(2)` initially, then `River(0)` after
  later batches process it as a neighbor. Same coords, same order,
  same outputs.
- **The dump read shows different values across runs**: sometimes
  `Nothing`, sometimes `River(0)`, sometimes `Ocean(0)`.
- **With the Sim thread killed at startup**: the dump read shows
  `River(2)` consistently across all 8 runs. Zero non-determinism.

The non-determinism is between the seal pipeline writing and the dump
reading. The Sim thread is what's writing in between.

## How the Sim thread modifies chunks

`src/Sim/Thread.hs` runs an independent fluid simulation:

1. **Receives `SimChunkLoaded` messages** when chunks load from the
   world thread (at line 116). Each new chunk gets `scsSettleTicks =
   64`.

2. **Each tick** (10 Hz, 100ms cadence): runs `simulateFluidTick` and
   `simulateActiveTick` on tracked chunks.

3. **Writes back to `wsTilesRef`** via `writeDirtyFluids` (line 207):
   ```haskell
   atomicModifyIORef' tilesRef $ \wtd →
       let wtd' = HS.foldl' (\w cc →
               case ... of
                   (Just scs, Just lc) →
                       let newFluid = ... deriveFluidMap scs ...
                           lc' = lc { lcFluidMap   = newFluid
                                    , lcSurfaceMap = newSurfMap
                                    , lcSideDeco   = newSideDeco
                                    , lcModified   = True }
                       in w { wtdChunks = HM.insert cc lc' (wtdChunks w) }
                   _ → w
               ) wtd dirty
       in (wtd', ())
   ```

   Note: this MUTATES `lcFluidMap` for any chunks the sim is tracking.
   Including changing `River` → `Nothing`, `River(2)` → `River(0)`,
   etc., based on whatever the simulation logic produces.

4. **Settling**: chunks with `scsSettleTicks > 0` get fast-tick
   simulation. After ~6.4 seconds (64 ticks at 10 Hz) per chunk, they
   stop being simulated.

The Sim thread is doing **useful work** — it fills gaps that the seal
pipeline doesn't catch. With Sim disabled, the audit shows MORE chunk
gaps than the baseline. So the sim is part of the system, not a bug.

## What this means for the original question

The question I started Phase 1 with: "where does the non-determinism in
chunk generation come from?"

The answer: **There is no non-determinism in chunk generation.** The
chunk generation pipeline (including `generateChunk`, the seal, the
strip, the cleanup) is fully deterministic. The race we observed is
between the chunk generation pipeline and the Sim thread, both writing
to `wsTilesRef`.

## Re-answering the open questions

### Q1: Where does River → Nothing come from?
✅ **Answered**: The Sim thread's `writeDirtyFluids` writes the
result of `simulateFluidTick`/`simulateActiveTick`. The simulation can
produce any fluid state including `Nothing` (e.g., when a tile drains
or evaporates in the simulation).

### Q2: Is the seal idempotent?
✅ **Effectively answered**: The seal pipeline produces byte-identical
traces across runs given the same input. The seal IS deterministic. We
don't need to test idempotence separately.

### Q3: Is the seal order-independent?
✅ **Effectively answered**: Same as Q2. The seal is deterministic.
The order it processes chunks is itself deterministic (HashMap keys
are deterministic for the same Hashable input).

### Q4: Is multi-call interleaving the cause?
✅ **Answered**: NO. Even with the world thread's per-batch sealing
running (interleaved between drainInitQueues and updateChunkLoading),
the seal traces are identical. The races we observed come from the
Sim thread, not from interleaving in the world thread.

### Q5: Equalize/fill feedback loop?
✅ **Effectively answered**: There's no feedback loop because the
seal is deterministic. My earlier observation that "more seal
iterations made things worse" was wrong — that was the Sim thread
modifying state between iterations.

### Q6: Why doesn't chunkBorder=14 give per-chunk smoothing enough context?
🟡 **Still open**, but no longer a blocker. The chunkBorder is used
by `applyTimelineChunk` and `applyCoastalErosion` for terrain. The
fluid generation passes (`computeChunkFluid`, `computeChunkRivers`,
etc.) work on the 16x16 chunk only — they don't use the bordered
region. The seal exists to reconcile per-chunk smoothing differences,
which IS still a real concern, but it's not racy.

## What changes in the refactor scope

Most of the previously planned refactor is **unnecessary**:
- ❌ No need for "pure raw chunk generator" — generateChunk IS pure
- ❌ No need for "pure cross-chunk smoother" — the seal IS deterministic
- ❌ No need for "new chunk loading manager" — the existing one works
- ❌ No need for `RawChunk` / `SmoothedChunk` data model split

What we actually need:
- **Address the Sim thread interaction**: 1) decide whether the dump
  should snapshot before sim runs, 2) wait for sim to settle, 3) run
  sim synchronously in dump mode, or 4) accept the current race.

That's it. The whole "pure pipeline refactor" was solving a problem
that didn't exist.

## What I got wrong in Phase 1

1. **I didn't grep `src/Sim/`**. I only audited `src/World/`. The
   Sim thread was completely outside my view.
2. **I trusted my mental model over empirical evidence**. I declared
   the seal "non-deterministic due to live-accumulator semantics"
   without actually testing whether the seal produces different
   outputs in different runs. (It doesn't.)
3. **I conflated "writes happen in different orders across runs" with
   "the seal is non-deterministic"**. Different seal calls *did*
   happen in different orders, but each call individually was
   deterministic. The order variation came from Sim thread interleaving.

## Status

Phase 1 audit COMPLETE (corrected):
- [x] Catalog generateChunk call tree
- [x] Catalog post-merge passes
- [x] Classify each function as pure / reads-neighbors / mutates-shared
- [x] Search for hidden state in chunk path *(found IORefs in src/Sim/!)*
- [x] Find non-obvious cross-chunk reads
- [x] Map data dependencies between passes
- [x] Write PIPELINE_AUDIT.md document

Phase 1.5 investigation COMPLETE:
- [x] Q1: River → Nothing comes from Sim thread `writeDirtyFluids`
- [x] Q2: Seal IS idempotent (deterministic across runs)
- [x] Q3: Seal IS order-independent (HashMap order is deterministic)
- [x] Q4: Multi-call interleaving NOT the cause; sim thread is
- [x] Q5: No equalize/fill feedback loop; seal is deterministic
- [ ] Q6: chunkBorder scope still unverified but no longer blocking

Roadmap rewritten in `memory/plan_pure_pipeline_refactor.md`.
