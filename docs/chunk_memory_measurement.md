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
  incarnation. Unmatched simulation pages remain visible separately as evidence
  of what that owner retained at its sample time; a delayed reply does not prove
  those allocations remain live when the Lua response is read.

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

### Deterministic examples

The deterministic 64-bit fixtures in `Test.Headless.World.ChunkMemory` give
these model values (bytes). Each has 256 columns; the deep-air case replaces
materials with air without removing stored cells. The rich case adds 256 fluid
and ice cells, one flora instance, one edited structure and one magma-cap entry.

| Fixture | Columns | Derived maps | Overlays | Containers | Total |
|---|---:|---:|---:|---:|---:|
| Depth 8 | 61,496 | 8,728 | 2,120 | 224 | 72,568 |
| Depth 80, solid or interior air | 116,792 | 8,728 | 2,120 | 224 | 127,864 |
| Depth 8, rich overlays | 61,496 | 18,968 | 12,800 | 224 | 93,488 |

These deliberately replicated fixtures share Haskell values, so their logical
totals must not be described as measured physical allocation sizes. In the rich
fixture, fluid adds 10,240 bytes; ice adds 10,240, flora 160, the structure 128,
and magma 152 under model v1. The equal-count depth change adds 55,296 bytes.

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

The complete serial workload matrix, after selecting the committed binary, is:

```sh
MEMORY_BINARY=/absolute/path/to/synarchy
MEMORY_OUT=/absolute/new/matrix-directory
for size in 64 256; do
  for rts in production small-nursery; do
    for rep in 1 2 3; do
      python3 tools/chunk_memory_measure.py --binary "$MEMORY_BINARY" \
        --output "$MEMORY_OUT/w$size-$rts-$rep" --size "$size" --rts "$rts" || exit
    done
  done
done
for rts in production small-nursery; do
  python3 tools/chunk_memory_measure.py --binary "$MEMORY_BINARY" \
    --output "$MEMORY_OUT/w64-$rts-1-offscreen" \
    --size 64 --rts "$rts" --mode offscreen || exit
done
```

In the coordinated lab, wrap each invocation in `profile_coordinator.py exec
--run-id <claimed-run> --phase measure --timeout 1800 -- <command>`; only the
last planned invocation uses `--final`. A failure ends that coordinated run
for reporting. Any repaired driver needs a new committed follow-up run.

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

Apple's [Metal memory analysis guide](https://developer.apple.com/documentation/xcode/analyzing-the-memory-usage-of-your-metal-app)
distinguishes allocations, resident size, and dirty plus compressed/swapped
footprint. Its [VM guide](https://developer.apple.com/library/archive/documentation/Performance/Conceptual/ManagingMemory/Articles/VMPages.html)
also distinguishes virtual reservations from physical pages and explains shared
mappings. Accordingly the report keeps `vmmap` footprint, region residency and
RSS separate: adding those overlapping totals would double-count memory.

The driver uses a private resource root and save slot, launches only its own
headless/offscreen process on a non-8008 port, and retains one JSON file plus a
compressed engine log per run. JSON includes raw console replies, query elapsed
times (instrumentation cost), all RSS samples, phase boundaries, completion
checks, metadata and failures. A failed run stops and is preserved; it is not a
passing matrix cell. No forced collection or implicit retry is performed.

## WorldSize-1024 projection

The approved WML product target is worldSize 1024. A bounded detailed working
set does not automatically grow with total world area: 200 chunks with the
fixture shapes above cost the same logical amount in a 64, 256 or 1024 world.
World size can still change actual terrain, required simultaneous footprints,
generation state and map storage; those effects are not held constant by this
illustration.

| Illustrative resident set | Depth-8 fixture | Depth-80 fixture |
|---|---:|---:|
| 200 detailed chunks | 13.841 MiB | 24.388 MiB |
| 1,000 detailed chunks | 69.206 MiB | 121.941 MiB |
| All 524,288 chunks of a size-1024 world | 35.434 GiB | 62.434 GiB |

These are logical model projections, excluding simulation and every other
subsystem, with neither an uncertainty bound nor a physical allocation claim.
The last row illustrates why visiting a world must not imply keeping all its
detail resident; it is not the workload demanded by ordinary streaming.
Measured size-64/256 RSS cannot be multiplied into a safe size-1024 budget.
Generation and the current map representation have separate limits and gates
under #2017; this experiment does not run or validate size 1024. The map
format's addressing plans through 8192 are not whole-engine support evidence.

## Validation and results

The `chunk residency accounting` Hspec group covers field costs, lifecycle
transitions, aliases, edits at unchanged count, mismatch visibility, simulation
retention, registered console queries, window reset, incarnation isolation and
load-discard cancellation. The Python tests reject missing phases, incomplete
terrain, malformed console results and counter-unit errors.

### Development-host results, 2026-09-17

Host: Apple M3 Max, Mac15,9, 16 physical/logical CPUs, 64 GiB RAM; macOS 26.6
build 25G5065a. GHC 9.12.2 and Cabal 3.16.1.0 built production code with
`dev=false`, `profile=false`, `-O2 -optc-O3`; every diagnostic sample reported
16 RTS capabilities. `GHCRTS`, `ENGINE_DEBUG`, `SYNARCHY_RESOURCE_ROOT`,
`DYLD_INSERT_LIBRARIES` and `MallocStackLogging` were unset. No forced GC,
eventlog, cost-centre profiling or sanitizer was used. Power/thermal conditions
and other host activity were not isolated. A focused test rebuild overlapped
size-64/small-nursery repetition 2; that run is retained without exclusion.

The twelve headless cells measured revision
`7650862c6b9f48102e5ae17e036615e678cec9f3`. The repaired offscreen driver is at
`b279123fb684795d1e8d6a48a5ea439e2fd6a057`. Both use the identical engine binary,
SHA256 `9bf5a5905dc1121125eb387b8020ba2b34d0c310055f0c6f3e0e69fd1e15d491`.
Later report-only changes do not alter that measured binary.

Each entry below is a **sampled phase peak in GiB**, not an exact maximum or a
phase's incremental allocation. Memory from earlier phases can remain resident
in later phases. No complete cell or outlier was removed. Every size-64 run
verified 1,088 distinct canonical chunks; size 256 verified 1,089.

| Headless size | RTS nursery | Repetition | Generation | Traversal | Loading |
|---|---|---:|---:|---:|---:|
| 64 | Production 128 MiB | 1 | 2.5215 | 2.5238 | 2.5251 |
| 64 | Production 128 MiB | 2 | 2.5487 | 2.5513 | 2.5526 |
| 64 | Production 128 MiB | 3 | 2.5186 | 2.5212 | 2.5224 |
| 64 | Control 8 MiB | 1 | 0.5438 | 0.5461 | 0.5473 |
| 64 | Control 8 MiB | 2 | 0.5742 | 0.5763 | 0.3569 |
| 64 | Control 8 MiB | 3 | 0.5508 | 0.5529 | 0.5541 |
| 256 | Production 128 MiB | 1 | 6.9994 | 7.0080 | 7.0123 |
| 256 | Production 128 MiB | 2 | 6.6599 | 6.6702 | 6.7082 |
| 256 | Production 128 MiB | 3 | 7.4509 | 7.4514 | 7.4161 |
| 256 | Control 8 MiB | 1 | 5.1207 | 5.1223 | 4.5815 |
| 256 | Control 8 MiB | 2 | 5.3474 | 5.1373 | 5.2530 |
| 256 | Control 8 MiB | 3 | 5.3256 | 5.3275 | 5.3226 |

Size-256 production median peaks are 6.9994 GiB for generation, 7.0080 GiB
for traversal and 7.0123 GiB for loading. All six size-256 processes exceed
4 GiB in all three phases, including the nursery control. All size-64 headless
observations are below 4 GiB; this is not a whole-game or minimum-machine pass.
The control establishes sensitivity to RTS nursery configuration, without
measuring exact nursery residency or authorizing a default change.

### Offscreen gameplay and graphics accounting

Both size-64 offscreen runs completed normal startup, generation, the same
1,088-chunk traversal, the five-player-unit scenario, save/load and clean exit.
The normal catalog loaded 202 flora definitions and eight unit definitions;
world generation captured 16 worldgen flora entries. Five player acolytes
(IDs 8–12) were added to seven naturally present units. All twelve were present
after the 30-second unpaused interval. This is a small scenario without a busy
50-unit colony, production chains or real audio-device output.

| RTS nursery | Generation GiB | Traversal GiB | Gameplay GiB | Loading GiB |
|---|---:|---:|---:|---:|
| Production 128 MiB | 2.8353 | 3.0215 | 3.0221 | 3.0310 |
| Control 8 MiB | 0.9757 | 0.9879 | 0.9884 | 0.9895 |

The base requested 100 chunks. Both gameplay captures actually held **235 tile
chunks**, with a 17.273 MiB logical world estimate and 1.978 MiB maximum sampled
simulation estimate. This also demonstrates that today's 200-chunk cache target
is not an enforced total ceiling. At the final gameplay sample, world columns
were 14,602,568 bytes, maps 2,527,200, overlays 929,448 and containers 52,640.
Requested/in-flight counters were zero and owner keys agreed at that sample.

Each `vmmap -summary` succeeded after gameplay. Values below preserve its
rounded display units; they are not additional amounts to add to RSS.

| `vmmap` observation | Production | Control |
|---|---:|---:|
| Physical footprint | 3.4G | 1.4G |
| Lifetime physical-footprint peak | 3.5G | 1.5G |
| `VM_ALLOCATE` resident | 2.9G | 901.0M |
| `IOAccelerator (graphics)` resident | 17.4M | 17.4M |
| `owned unmapped (graphics)` resident | 421.8M | 421.8M |
| Other `owned unmapped` resident | 3840K | 4096K |
| Shared-library read-only resident | 438.9M | 438.9M |
| All reported regions, resident total | 3.9G | 1.9G |

The owned-unmapped rows expose game-owned allocations outside ordinary mapped
regions. They are retained separately because a simple RSS value does not
provide their attribution. Their exact overlap with the RSS/footprint counters
has not been established; no subtraction or addition is used to manufacture a
combined peak. The large `VM_ALLOCATE` region is not automatically all GHC live
objects, and the graphics rows are not a complete renderer allocation census.
The reported 1.0T reserved address space has zero residency and is not memory
consumption. Shared library pages are not privately charged a second time.

**Phase verdicts:** size-64 generation, gameplay and loading were below 4 GiB in
sampled RSS, including normal offscreen content. Complete whole-game compliance
remains unproven: the graphics/footprint snapshot is at one point, external
phase-specific peaks and counter overlap are unresolved, and these are single
offscreen observations per RTS setting. Size-256 generation and loading
definitively fail the RSS envelope in all repetitions; rendering can provide
no exemption from that failure. A size-256 gameplay measurement was not run.

### Instrumentation and retained evidence

The 14 completed processes retain **5,942 RSS samples**. Maximum observed
sampling gaps were 0.256–0.270 seconds across runs. Per-process median memory
query round trips were 37.342–40.432 ms; the largest individual round trip was
82.810 ms. These include transport, its 25-ms idle boundary, JSON conversion
and owner sampling, not just the pure cost function. No instrument-off control
was run, so the net CPU/RSS distortion is unknown. `vmmap` inspection adds work
at the end of gameplay. Phase tagging at sampling boundaries is approximate;
neither the phase labels nor polling capture exact instantaneous peaks.

[Evidence inventory and reproduction](evidence/chunk_memory_2625/README.md)
links the checksum-verified archive, per-process tables and recomputation script.
Raw commands, replies, RSS timestamps, owner observations, metadata, engine logs
and `vmmap` output are retained. The successful headless and offscreen runs have
separate exact source revisions and an identical binary hash.

Three failed attempts remain visible and do not count as completed matrix cells:

- Initial diagnostic `85583c`: console banner/prompt framing caused a false
  chunk-queue failure. Its result, timeline, log and driver remain in the archive.
  Corrected diagnostic `d4df58` completed three held-region processes at base
  revision `064a255f06b5`; that earlier workload is not interchangeable with
  the traversal matrix.
- Instrumented attempt `fe3390` at `a68ab4ca9`: a multi-return generation query
  was incorrectly parsed as one JSON object. Partial samples and log remain.
- Offscreen cell in `969a01` at `7650862c6`: valid Lua unit ID `8.0` failed an
  overly strict Python integer check. Its completed traversal and partial
  gameplay-labelled samples remain; they are not five-unit gameplay evidence.
  Follow-up `696c70` at `b279123fb` repaired numeric-ID validation and completed
  both offscreen cells. It did not replace any successful headless sample.

### Detailed and simulation estimates

Every headless process sampled a peak of 200 resident tile chunks while visiting
over 1,000. The current cache evicts along the traversal; this is distinct from
the earlier bulk-region diagnostic which held 1,088 at once. Across all six
size-64 cells the maximum logical world estimate was 14.612 MiB and simulation
estimate 1.807 MiB; at size 256 they were 13.977 MiB and 1.399 MiB respectively.
These separately sampled, potentially shared values must not be added into a
physical retained-memory total.

Representative final-traversal world field groups (bytes), production run 1:

| Size | Resident | Columns | Derived maps | Overlays | Containers | Total | Individual range |
|---|---:|---:|---:|---:|---:|---:|---:|
| 64 | 200 | 12,457,120 | 2,037,400 | 631,184 | 44,800 | 15,170,504 | 66,488–94,688 |
| 256 | 200 | 12,321,640 | 1,782,960 | 424,000 | 44,800 | 14,573,400 | 72,568–82,808 |

Columns dominate this logical model. The depth/overlay fixtures show why a
count alone does not describe memory: an unchanged count can change estimated
bytes substantially. Neither the sampled individual range nor the depth-80
fixture is a worst-case bound for arbitrary terrain or edits.

Some current-incarnation simulation replies contained up to 43 (size 64) or 44
(size 256) keys absent from the independently read tile map. Traversal and reply
latency can explain set differences; these are observations, not a persistent
leak finding. Settled post-load headless observations show the new incarnation
with 25 tile and simulation chunks and no unmatched current-incarnation keys.
Old simulation replies visible during cutover remain explicitly unmatched.

### Owner-accepted disposition, 2026-09-17

The owner accepted the following measured outcome and authorized continuing
through review and merge. The 4-GiB whole-process target remains unchanged.

- **Streaming trim target:** retain today's 200 solely as the existing locality
  policy while Arc A proceeds. This measurement does not validate it as a
  memory-safe global limit or change the policy.
- **Hard residency ceiling:** unresolved. No safe new chunk count or byte
  allowance can be justified against a process already exceeding 4 GiB during
  generation with only 25 detailed chunks sampled at completion. Physical
  retained-heap/native/graphics attribution and representative simultaneous
  page/reservation footprints are missing; logical estimates cannot be
  subtracted from RSS to invent non-chunk headroom.
- **Count versus bytes:** count remains useful operationally; variable depth
  and overlays require a calibrated byte constraint for memory safety. Model
  v1 is diagnostic and is not itself a safe enforcement charge.
- **Chunk-storage/hibernation gate:** keep deferred. Detailed residency has not
  been established as the cause of the envelope failure. First separate
  generation/map/runtime/native costs from retained detail with a physical
  census and bounded simultaneous workloads. A whole-process failure alone
  does not open Arc B or revive D-20's background-simulation hibernation.
- **Minimum machine:** four-core/8-GB laptop validation remains outstanding,
  as do a representative busy 50-unit colony and measured size-1024 behavior.

All measurement cells and the required owner disposition are complete. Required
documentation and evidence stay in this code PR; later enforcement and minimum-
machine validation remain separate work.
