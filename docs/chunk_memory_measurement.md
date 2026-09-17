# Detailed chunk memory measurement (#2625)

The target is **4 GiB of whole-process resident memory**, across all pages and
subsystems, during gameplay, generation and save loading. This experiment starts
on the development M3 Max / 64 GiB Mac. It does not validate the four-core / 8 GB
laptop. This is instrumentation and evidence, with no budget enforcement.

## Console contract

`world.getChunkMemory()` returns a fresh table. `world.resetChunkMemoryWindow()`
starts a new observation window without changing the game. Both are registered
through the ordinary world API; neither forces GC. There is no background memory
walk when the diagnostic is unused.

The schema-1 response contains:

- `model="logical-unshared-v1"`, `wordBytes`, and actual RTS `capabilities`.
- `windowStartSeconds`, `sampleTimeSeconds` (monotonic process clock), `samples`,
  `highWaterKind="sampled"`, `resident`, `logicalEstimatedBytes`, `peakResident`
  and `peakLogicalEstimatedBytes` across all currently registered tile pages.
- `pages`: page id, incarnation, sample timestamp, resident payload count,
  requested/in-flight/owner-resident counts, `ownerKeysAgree`, the logical-byte
  groups `columns`, `derivedMaps`, `overlays`, `containers`, `total`, and minimum
  and maximum individual chunk estimates.
- `pageHighWater`: scalar count/byte maxima for current page incarnations only.
  Replaced/dropped page rows disappear on the next query. Process maxima retain
  the explicitly named observation window, including earlier sessions, until
  reset; they are historical observations, not a claim about the current page.
- `simulation`: `available`, sample timestamp and age, total logical estimate,
  and per-page incarnation/count/estimate. `currentIncarnation` identifies a
  match with the current world snapshot. `notInTileCache` is the set difference
  against that incarnation's tile keys; it is absent for an unmatched/retired
  incarnation. Unmatched simulation pages remain visible separately because
  their process memory has not disappeared.

The first query requests a simulation snapshot and returns immediately with
`available=false`. Subsequent queries consume a completed snapshot and schedule
the next one. There is at most one outstanding request per Lua runtime, even if
the sim is parked or the window is repeatedly reset. Load publication cancels
discarded requests; a later query resumes. Only scalar estimates and keys leave
the sim owner; diagnostic history does not retain terrain/fluid buffers.

**Consistency:** each immutable tile map is read once. Its owner-state ref is a
separate read; `ownerKeysAgree=false` exposes a publication mismatch. Simulation
samples have their own time and epoch. These are independent owner samples, not
a transaction: agreement does not prove simultaneous publication, and set
differences can include intervening admissions/evictions. Do not treat their
sum as a globally atomic census. The driver waits for acknowledged chunk work,
checks every requested canonical chunk, and records the sampling boundaries.

## Byte model and limitations

This is a **logical unshared estimate**, not a physical retained-heap census,
and is neither a lower nor an upper bound on RSS. Every visible buffer/record is
costed independently. Shared empty vectors, identical columns, fluid/terrain
buffers shared between world and sim, boxed cells shared by vector replication,
and shared old/new snapshots can make the model overcount physical allocations.
Sliced buffers can retain capacity beyond their visible length and make it
undercount. No pointer-identity deduplication or capacity introspection is claimed.

The word size is obtained from the running Haskell `Int`. Model v1 uses:

- Seven words per vector for wrapper/array-header allowance, plus word-aligned
  visible payload. Boxed vectors include one word per slot. Static `Nothing`
  has no per-slot allocation; each present cell adds two words for `Just` and
  three words for a passive fluid/ice record or four for active fluid.
- Five words per column record plus its three byte vectors; interior air is
  stored and charged. Surface, terrain and water-table maps use word elements;
  side decorations use bytes.
- Twenty words per flora instance including list cell, scalar fields and
  identity allowance; two words for the flora wrapper. Structures add four
  words per value; magma includes its wrapper and sparse entries.
- Twelve words per map entry for amortized trie/leaf/key allowance; sixteen
  words per loaded-chunk record/coordinate. Simulation uses ten record words
  per chunk plus its map entry and fluid/terrain/active-fluid/decoration arrays.

These object/trie allowances are estimates, not ABI size assertions. Heap
alignment details, allocator slack, stacks, GHC nurseries, generation timelines,
edit logs, reactions/queues, registries, native allocations, render caches and
GPU resources are outside this model. Consequently **never subtract these
estimates from RSS and call the residual measured non-chunk memory**, or add
world and sim estimates as if shared buffers were proven independent.

## Reproduction

Build production code with `cabal build all`. Record `cabal list-bin exe:synarchy`
and the exact commit; the driver hashes the binary. Run the tool at a committed
revision through the coordinated profiling lab. The script itself does not
build, choose a revision, or overwrite an existing result directory.

```sh
python3 tools/test_chunk_memory_measure.py
python3 tools/chunk_memory_measure.py --binary /absolute/path/to/synarchy \
  --output /absolute/new/run-directory --size 64 --rts production
```

Required matrix: worldSize 64 and 256, each under `--rts production` and
`--rts small-nursery`, each repeated in three fresh processes. Add an offscreen
worldSize-64 run under each RTS configuration with `--mode offscreen`. Offscreen
is manual-only and requires a GPU; no CI probe entry is added. All runs use seed
42 and three plates, a serpentine traversal with at least 1,000 unique verified
canonical chunks, then a real save/load transaction. Offscreen also loads the
ordinary content catalog, establishes a five-unit scenario on a requested
10-by-10 chunk footprint, and observes 30 seconds of unpaused simulation.

Production defaults remain the binary's baked `-N -A128M`; the control adds only
`+RTS -A8M -RTS`, retaining the same capability count. Neither is substituted for
the other. The control tests whether nursery capacity masks chunk allocations;
it is not an approved shipping change or a minimum-machine simulation.

The live sampler invokes `ps -o rss` at a nominal 250 ms interval and records
bytes after converting the returned KiB. The independent `ru_maxrss` conversion
helper explicitly distinguishes macOS bytes from Linux KiB. Samples retain
timestamps, so actual gaps remain visible. Sampling can miss transient peaks.
RSS is the OS process counter, not a complete attribution of unified/GPU memory;
offscreen runs retain `vmmap -summary` separately. Shared/driver resources must
be assessed without blindly adding overlapping figures. An unavailable graphics
attribution cannot support a whole-game pass.

The driver uses a private resource root and save slot, launches only its own
headless/offscreen process on a non-8008 port, and retains one JSON file plus a
compressed engine log per run. JSON includes raw console replies, query elapsed
times (instrumentation cost), all RSS samples, phase boundaries, completion
checks, metadata and failures. A failed run stops and is preserved; it is not a
passing matrix cell. No forced collection or implicit retry is performed.

## Validation and results

The `chunk residency accounting` Hspec group covers field costs, lifecycle
transitions, aliases, edits at unchanged count, mismatch visibility, simulation
retention, registered console queries, window reset, incarnation isolation and
load-discard cancellation. The Python tests reject missing phases, incomplete
terrain, malformed console results and counter-unit errors.

Measurement results and owner disposition will be recorded here before final PR
review. The 8 GB target-machine validation remains outstanding.
