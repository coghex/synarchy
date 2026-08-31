# Chunk residency and world streaming design

This document designs the foundation that lets Synarchy keep a large, finite,
procedural world without keeping every detailed chunk in memory. It separates
three concerns that the current runtime partially mixes together: the generated
world foundation, the bounded set of chunks materialized for immediate use, and
the sparse gameplay state that must survive chunk eviction and save/load.

The design deliberately does not treat the in-memory cache as save data. A save
restores the same world and the same gameplay consequences, but it may choose a
different useful set of resident chunks after loading.

Design state: `deferred — blocked on the world-map level-of-detail design`

**Deferred 2026-08-31.** Issue processing must not resume on this document
until the world-map level-of-detail arc is designed and its representation
decided. `/process-design-doc` will refuse this document while the state line
above reads anything other than `ready for issue processing`, which is
intentional.

The reason is that the zoom map, not the detailed chunk cache, is the binding
constraint on world size — and it is a dependency of this document rather than
a later slice of it:

- The zoom atlas stores a fixed 32x32 pixels per chunk
  (`World.ZoomMap.Types.zoomTileSize`), and `buildZoomCache` enumerates
  `worldSize` squared chunks, so the atlas side is `worldSize * 32` in one
  contiguous RGBA8 buffer. That is 1024 MiB at worldSize 512 — exactly at the
  common 16384-pixel GPU dimension limit — and 4096 MiB at worldSize 1024,
  which exceeds it outright.
- Nothing queries Vulkan's `maxImageDimension2D` and nothing validates
  `zadWidth`/`zadHeight` before upload, so the failure mode above the limit is a
  driver error during image creation, not a degraded map.
- By contrast, 289 resident detailed chunks did not move a 184 MiB peak set by
  world generation itself (see §Measurements). The cache this document is about
  is an order of magnitude cheaper than the map.
- Arc B specified that the generated-world foundation stores the zoom output.
  Freezing that bundle format before the map's representation is decided would
  freeze it around a representation that is about to be replaced.

Revision 2's content below is retained as drafted. Two known defects were
identified after it was written and are deliberately **not** repaired here,
because the deferral makes them moot until the arc is rescoped: the Arc B gate
is circular (it checks a measurement against a ceiling chosen after that
measurement is read), and CRS-14 is gated on a memory threshold when its real
trigger is a texture-dimension limit that is already reachable.

Linked tracker items stay open. #2001 (CRS-1, canonical chunk identity) is
unaffected by any of this: one physical chunk equalling one key holds under
every candidate map and storage design.

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [x] EPIC. Bound detailed-world memory while preserving durable gameplay state — [#1997]
- [x] CRS-1. Centralize chunk demand and canonical chunk identity — [#2001]
- [ ] CRS-2. Measure resident chunk cost and residency high-water
- [ ] CRS-3. Express chunk demand as streaming requests and batch reservations
- [ ] CRS-4. Enforce the residency budget against unreserved chunks only
- [ ] CRS-5. Make active fluid simulation eviction- and save-safe
- [ ] CRS-6. Reserve chunks used by unit work and authoritative world edits
- [ ] CRS-7. Make location stamping transactional across its footprint
- [ ] CRS-8. Add residency diagnostics and the long-travel plateau gate
- [ ] CRS-9. Define the versioned generated-world bundle and base-chunk record
- [ ] CRS-10. Write base chunks and the reusable zoom artifact during world generation
- [ ] CRS-11. Materialize resident chunks from the bundle plus durable deltas
- [ ] CRS-12. Replace resident-chunk fluid snapshots with sparse durable fluid state
- [ ] CRS-13. Move chunk reads and fallback generation off the world thread
- [ ] CRS-14. Keep the complete zoom map available at large world sizes
- [ ] CRS-15. Add bundle corruption, latency, and pressure gates

CRS-1 through CRS-8 are **Arc A**; CRS-9 through CRS-15 are **Arc B**, which
does not begin until CRS-2's measurement opens its gate. See §Two arcs.

## Revision history

**Revision 2 (2026-08-31)** restructured the delivery plan after processing
stalled on the original CRS-2. Four changes, each signed off:

1. **Two arcs, the second gated.** Correctness work that stands alone was
   separated from capacity work that rests on an unmeasured premise.
2. **Expression precedes enforcement.** The original plan enforced a residency
   ceiling one slice *before* introducing the leases that let a consumer say
   "hold this set." That ordering is unsound; the two slices are swapped and
   leases are generalized into reservations.
3. **The budget is per-page**, not process-global.
4. **Disk-backed hibernation is withdrawn** from the delivery plan and made
   conditional.

Revision 1's slice IDs CRS-2…CRS-13 were unprocessed and have been reassigned.
CRS-1 keeps its identity and its issue. Revision 1's full text is in git
history.

## Two arcs

The original document treated one problem. There are two, and they carry very
different amounts of evidence.

**Arc A — residency correctness.** Eviction currently decides things it must
not decide. It can drop a chunk whose fluid simulation is still ticking, whose
location stamp has not committed, or whose tiles a batch consumer is still
waiting to read. One physical chunk has two names near the seam. Requests carry
no identity, so nothing can be deduplicated, held, or accounted for. **These are
defects at today's world sizes**, and two of them have already shipped as bugs
(#1674, #1719). Arc A needs no new storage format, no bundle, and no
asynchronous loading.

**Arc B — capacity and streaming.** An immutable generated-world bundle, base
chunk records, materialization from bundle plus deltas, asynchronous reads, and
a zoom map that survives large worlds. This is the larger body of work and it
rests on one premise: that resident detailed chunks are the memory problem.
**That premise is not yet measured.** See §Measurements.

Arc A's invariant is the one worth stating on its own, because every slice in it
is an instance of it:

> Residency is a performance choice. It is never world truth, and it never
> silently changes an answer.

## Measurements

### What has been measured

`--dump` at `--seed 42 --worldSize 64`, comparing a 9-chunk region against a
289-chunk region, run with a small allocation area so major GCs actually sample
the loaded state (158 and 160 samples respectively, against 6 with the baked-in
`-A128M`):

| dump region | chunks resident | peak residency |
|---|---|---|
| `-1,-1,1,1` | 9, plus the camera's 5x5 box | 184.3 MiB |
| `-8,-8,8,8` | 289 | 184.3 MiB |

The difference is 18 KiB across 280 additional resident chunks.

**What this supports:** at worldSize 64 the entire 289-chunk resident set fits
underneath a peak that world *generation* establishes. The resident cache is not
the high-water mark; the timeline, plate and erosion passes are.

**What this does not support:** a per-chunk resident cost. `maximum residency` is
a maximum over the whole run, so if generation peaks at 184 MiB and the chunks
later occupy anything less, the maximum never moves. The figure is an upper
bound on the resident set, not a measurement of it.

**Separately measured, and decision-relevant:** that same bare `--dump` holds
**289 chunks simultaneously resident against a documented 200-chunk ceiling**,
and emits all 289 tiles' worth of output. The ceiling is not enforced on that
path at all. This is the correctness defect, and unlike the memory premise it is
observed rather than assumed.

### What CRS-2 must measure

The per-chunk figure needs a counter at the right moment, not `+RTS -s`. CRS-2
exists to produce it, and its results decide whether Arc B is justified,
deferred, or reshaped:

- resident bytes attributable to a single materialized `LoadedChunk`, by field
  group, at representative terrain complexities;
- residency high-water across a long traversal at the world sizes actually
  targeted, not just at 64;
- how much of that is `Chunk`'s per-column vectors versus the derived maps,
  since that ratio decides what `BaseChunkV1` should even contain;
- whether the count ceiling or a byte ceiling is the binding constraint.

Until those numbers exist, statements about how much memory the cache uses are
assumptions, and this document marks them as such rather than building on them.

## Current state and evidence

- `World.Tile.Types.WorldTileData` owns a strict `HashMap ChunkCoord LoadedChunk`
  and an integer `wtdMaxChunks`. The default ceiling is 200 chunks (100 for
  arenas). It counts entries, not their memory cost.
- That ceiling is per page, not global, and it is not enforced on every path.
  `evictDistantChunksWithReport` preserves the camera keep-radius and evicts
  other chunks furthest-first. It has no concept of active simulations,
  in-flight edits, unit work, location footprints, reservations, recency, or
  dirty writeback.
- The camera-driven path in `World.Thread.ChunkLoading.updateChunkLoading`
  inserts a generated batch and then invokes that eviction function — but only
  `when (not $ null batch)`. Once the camera's own neighbourhood is loaded it
  has no batch, so nothing evicts for the rest of a fill. The explicit/init
  path in `drainInitQueues` inserts chunks and never evicts at all.
- `world.loadChunksInRegion` and the dump's `--region` fill can therefore
  request a region far larger than the ceiling, and today they succeed by
  accident rather than by policy.
- A batch consumer that loses a chunk fails **silently**. `dumpTilesJSON`'s
  per-chunk step is `case lookupChunk coord td of Nothing -> []`: a missing
  chunk contributes no tiles and no error. The dump reads `wsTilesRef` once,
  after `waitForChunks` returns, so it needs its whole region resident
  simultaneously and cannot stream.
- Chunk coordinates have cylindrical aliases. #1723 gave the init queue one
  canonical seam identity (`World.Chunk.Queue.chunkQueueCanon`, which guards an
  arena's sentinel `wgpWorldSize` and a non-positive world size), and every
  init-queue producer now measures and dedups through it. The camera path is not
  on it: it builds its own `wrapChunkCoordU (wgpWorldSize params)`, the
  unguarded function. CRS-1 (#2001) closes that.
- Pending work is `wsInitQueueRef :: IORef [ChunkCoord]`, annotated "for progress
  tracking". There is no first-class in-flight state; "is this already being
  worked on?" is reconstructed per call in a documented snapshot order that is
  the residue of #43.
- `World.Generate.Chunk.generateLoadedChunk` is pure and deterministic. It
  builds a substantial `LoadedChunk`: columns, surface and terrain maps, fluid,
  ice, flora, side decoration, water table, magma, and structures. These fields
  mix immutable generated data, cheaply derived data, and mutable overlays.
- Chunk generation currently happens in batches on the world thread. `parMap`
  parallelizes the pure calculation, but the world thread still initiates and
  waits for the batch before continuing its tick.
- The persistence inventory classifies `wsTilesRef` as rebuilt from generation
  parameters plus ordered world edits. The runtime cache itself is intentionally
  absent from the save.
- At save time, `appendFluidSnapshot` writes all 256 fluid cells for every
  currently resident chunk into the edit log. This makes the saved fluid
  representation depend on residency and records unchanged ocean chunks merely
  because the camera loaded them.
- `Sim.Thread` creates a `SimChunkState` when a detailed chunk loads and deletes
  it on `SimChunkUnloaded`. Eviction does not ask the simulation whether a chunk
  is active and does not await a final writeback before deleting it. The #1596
  generation fence prevents a stale pre-edit result overwriting a newer edit,
  but it does not make eviction a coordinated handoff.
- Flora already demonstrates the intended reconstruction boundary.
  `World.Flora.Growth` derives ordinary growth from deterministic placement plus
  the persisted absolute world day; the sparse harvest/regrowth map separately
  preserves player-visible exceptions.
- Location placement has a durable generated overlay and per-location stamped
  flags, but materialization is asynchronous, and the Lua builder queues
  authoritative edits across a footprint with no single commit acknowledgement.
- #1674 and #1719 were the two narrow repairs to that race. **Both are now
  closed** (2026-08-26 and 2026-08-27). CRS-7 must preserve their acceptance
  cases rather than reintroduce their failure modes; neither defined a
  reservation for a whole footprint or a general chunk transaction.
- Closed #1207 is the governing precedent: persistent wire topology was moved
  out of loaded-chunk-only state because camera-driven eviction must not suspend
  gameplay systems.
- Save loading rebuilds the whole zoom cache and, for the active page, all
  per-chunk pixel data, a whole atlas, and a preview.
- `World.ZoomMap.ChunkTexture` packs all 32-by-32 RGBA chunk tiles into one
  image, whose pixel payload grows linearly with physical chunk count and
  eventually hits both RAM and GPU-dimension limits.
- Despite the name "chunk" in its tile-building code, the zoom map is already a
  separate generated representation: `World.ZoomMap.Cache.BuildPixels` derives
  it from `WorldGenParams`, not from resident chunks. This separation is correct
  and must become explicit in storage.
- Gameplay creates exactly one world page (`scripts/world_manager.lua` holds the
  only gameplay `world.init`). Additional pages come from arenas, probes and
  tests. A process-global budget is therefore not yet solving a real
  configuration.
- Tracker searches on 2026-08-25 and again on 2026-08-30 found no existing
  chunk-residency, chunk-streaming, or generated-world-bundle epic beyond
  #1997 itself.

## Desired experience

### Generating a world

World generation may remain a deliberate, potentially long operation. When it
finishes, it publishes one complete generated-world foundation atomically. A
crash or cancellation before completion leaves no bundle that can be mistaken
for a usable world. *(Arc B.)*

The expensive result is reusable. Starting a game, loading a save, opening the
zoom map, or revisiting a distant region should not repeat work that was already
finished during world generation. *(Arc B.)*

### Travelling during play

The camera and active gameplay request nearby detailed chunks. Recently useful
chunks may remain warm, but the amount of resident detail reaches a plateau.
Travel across ten thousand chunks must not leave ten thousand chunks in RAM.

A visible or imminently needed chunk is materialized, and the player sees the
same altered terrain, structures, fluid, harvest state, and location completion
that existed before eviction.

An ordinary eviction does not write a chunk file. Mutable consequences already
belong to the durable gameplay layer. Eviction is dropping reconstructible RAM
after proving nobody still holds a reservation.

### Batch and tooling consumers

Some consumers do not stream. A dump, a scripted region load, a save warmup, a
location's declared footprint, and a fluid simulation's neighbour footprint each
need a specific set of chunks materialized *together* until the operation
finishes. For these, residency is not a cache at all — it is a materialization
request with a completion condition.

Such a consumer declares its set up front and is either admitted or refused. It
is never admitted and then silently trimmed, because a trimmed batch produces a
wrong answer rather than a slow one.

### Simulation away from the camera

Being off camera is not itself a reason to forget gameplay. A fluid chunk that
is actively ticking stays resident, along with the neighbouring footprint its
simulation requires, until a final state is accepted by the world owner. A
moving or working unit reserves the terrain it is actively using. Persistent
systems such as power remain authoritative independently of loaded chunks.

Conversely, a static middle-of-ocean chunk has no reason to remain resident.
Ordinary flora is reconstructed and advanced from the world clock. Sparse
exceptions such as harvested plants remain durable without pinning the chunk.

### Saving and loading

Saving does not serialize the incidental LRU order, resident set, pending
prefetches, or worker jobs. It persists gameplay state, and — once Arc B lands —
a reference to the immutable generated foundation. Active fluid is represented
by its accepted cell state plus enough activity metadata to resume settling.

The exact post-load cache contents are allowed to differ from the pre-save
contents; the gameplay answers are not.

### Zoom map

The zoom map is always available as a complete view of the finite world and is
unrelated to detailed chunk residency. Opening it never requests, reserves, or
waits for gameplay chunks. *(Arc B makes its storage independent; the behavioural
rule holds from Arc A onward.)*

## Scope

### In scope

- A large but finite procedural world.
- A per-page residency budget with a documented intended total.
- One canonical request, deduplication, load, publish, reserve, and eviction
  lifecycle for detailed chunks.
- Two request classes: streaming demand and batch reservation.
- Count-based bounding first and measured-byte bounding after measurement
  justifies it.
- Reservations for camera visibility, simulation, unit work, location
  materialization, authoritative edits, and explicit callers.
- Coordinated fluid activation, final writeback, save/load restoration, and
  reservation release.
- Metrics, diagnostics, and a deterministic long-travel plateau gate.
- *(Arc B)* An immutable generated-world bundle with a manifest, version and
  generator identity, chunk index, checksums, atomic publication, and reusable
  zoom data; a shared world-library bundle referenced by every descendant save;
  composition of base data, sparse deltas, and time-derived state; a sparse
  per-chunk fluid representation independent of residency; asynchronous
  read/decompress/fallback-generation workers with world-thread publication;
  immediate entry into the game scene with progressive chunk pop-in; an
  always-renderable zoom map.

### Out of scope

- An infinite world or generation beyond the finite world's declared boundary.
- Keeping every detailed chunk or simulating every tile continuously.
- Saving and restoring the exact RAM cache, its recency order, or pending
  background work.
- Making every cosmetic or deterministic derived value into persistent save data.
- A general rewrite of unit, building, item, or power persistence. This arc
  changes only their dependency on chunk residency where required.
- Cloud storage, cross-device bundle transfer, multiplayer synchronization, or a
  shared remote chunk service.
- Changing terrain, hydrology, flora, or location generation output merely to
  make storage easier.
- Automatic crash recovery or a write-ahead journal for progress since the last
  manual or autosave.
- **Disk-backed hibernation of background simulation.** Withdrawn in revision 2;
  see D-20 for the condition that would revive it.

## Design

### Three separate layers

The word "cache" should name only the third row below. Keeping the layers
separate prevents cache eviction from becoming either a save operation or a
gameplay deletion.

| Layer | Meaning | Lifetime | Examples |
|---|---|---|---|
| Generated-world foundation | Immutable result of expensive finite-world generation | From successful world creation until the last referencing save/world is deleted | Base chunk records, generation identity, location overlay, complete zoom artifact |
| Durable gameplay state | Authoritative differences and clocks | Save/session lifetime, independent of chunk residency | Terrain/structure edits, sparse fluid state, flora harvests, stamped locations, units and buildings |
| Resident detail cache | Materialized chunks ready for immediate queries/render/simulation | Bounded runtime working set | Base chunk + current deltas + derived seams/decoration |

Arc A establishes that the third layer may be dropped safely. Arc B introduces
the first layer as a stored artifact. Until then, the first layer is recomputed
by `generateLoadedChunk`, which is already pure and deterministic — the
correctness properties Arc A needs do not depend on where base data comes from.

A chunk load is conceptually:

```text
base chunk data (stored in Arc B, regenerated before it)
    + latest durable per-chunk deltas
    + state derived from current world time
    + resident-neighbor derived repair
    = published LoadedChunk
```

### Two request classes

This is the load-bearing distinction of the redraft, and the thing whose absence
made the original CRS-2 unsound.

**Streaming demand** says *"keep these near me if you can."* The camera's
neighbourhood and any prefetch are streaming demand. It is open-ended in time,
bounded in count, and freely evictable: the budget trims it, and trimming it
costs performance, never correctness.

**A batch reservation** says *"I need exactly this set materialized together
until I release it."* It names its complete set up front, carries an owner and a
reason, and is reference-counted so overlapping reservations compose. Its
holders are:

- a dump or scripted region load, until its read completes;
- save warmup, until the session is published;
- an active fluid simulation, including the cardinal neighbours it reads;
- a unit's current movement, path validation, interaction, or construction
  footprint;
- a location stamp's complete declared footprint;
- an accepted authoritative terrain/fluid/structure edit, until commit or
  rejection is acknowledged.

The two classes differ in what the budget is allowed to do to them, and that is
the whole point. **Streaming demand is trimmed. A reservation is refused or
granted, never trimmed.**

Reservations are released by a bracketed or token-scoped lifecycle so a leak is
attributable. Long-lived reservation ages and reason counts are observable.
Camera movement rotates a small neighbourhood; prefetch is never a reservation,
so a prefetched chunk stays immediately evictable until a real consumer claims
it.

### Central residency lifecycle

Per-page state remains with `WorldState`. This is not a new unrestricted
`EngineEnv` dumping ground, and per D-15 it does not become one.

Every request first becomes a canonical `ChunkKey` containing its page and
u-wrapped physical coordinate. One key has at most one resident value and one
in-flight load. Camera loading, `world.loadChunksInRegion`, save-load warmup,
cursor/debug generation, units, simulation, and location stamping use the same
admission path. *(CRS-1, landed as #2001.)*

The manager tracks these conceptual states:

```text
absent -> requested -> loading -> resident/unreserved -> eviction -> absent
                         |               ^        |
                         +--- publish ---+        +--- acquire reservation
                                       resident/reserved
```

Dirty or activity state is orthogonal: it belongs to a durable gameplay owner or
an acknowledged in-flight transaction, never only to the cache entry.

Publication remains world-thread-owned. A worker may read, verify, decompress,
or regenerate immutable base data, but it returns a candidate tagged with the
canonical key, world generation identity, and request epoch. The world thread
rejects candidates for deleted or replaced pages and obsolete epochs, applies
the latest mutable deltas, repairs resident-neighbour-derived data, publishes
once, and notifies dependent threads.

### Budget, admission, and eviction

The budget is **per page** (D-15) and is **two numbers** (D-11): a streaming trim
target, and a hard residency ceiling covering everything resident. Both start as
counts; a byte budget follows only if CRS-2's measurement shows count is the
wrong constraint (D-12).

Admission answers a request against them:

- **Streaming demand** is always admitted. It may immediately put the page over
  its trim target, and eviction trims it back.
- **A batch reservation** is admitted only if the page's total resident set
  would stay beneath the hard ceiling. **If it would not, the request is refused
  at request time, with an error naming the requested size, the current
  reserved total, and the ceiling.** It is never partially admitted.

**Precedence when pressure rises** is fixed, and resolves what would otherwise be
a contradiction between "streaming demand is always admitted" and "the ceiling is
hard":

1. Trim unreserved chunks toward the streaming target, least recently useful
   first.
2. If reservations alone still approach the hard ceiling, keep trimming the
   unreserved set — toward zero if necessary. Streaming is the first thing
   sacrificed, because losing it costs performance and nothing else.
3. Only when the unreserved set is exhausted and reservations alone reach the
   hard ceiling are further reservations refused. Existing reservations are never
   revoked to make room for new ones.

Eviction chooses only entries with zero reservations, no publish in progress,
and no unacknowledged authoritative transaction. Among eligible entries it uses
least recent useful access as the primary signal, with distance and prefetch
class as tie-breakers. "Last checked" must mean least recently used, not the most
recent chunk the player touched.

Eviction is not the normal persistence trigger. Any mutable state that would be
lost by dropping the cache has already been committed to its authoritative
runtime layer.

A refusal is a real outcome that callers must handle, and making it one is
migration work rather than a flag: `world.loadChunksInRegion` returns a count
today, and every existing caller — scripts and probes included — must handle the
refused case. The dump reports a refusal and exits non-zero rather than emitting
a truncated world. A gameplay consumer that cannot obtain its footprint defers
its operation rather than reading unloaded terrain as flat ground.

### Active fluid ownership

A chunk whose fluid is ticking holds a reservation covering itself and the
cardinal neighbours its solver reads. Eviction of that footprint is not a
decision the cache makes: the simulation deactivates, its final state is
written back, the world thread acknowledges, and only then is the reservation
released and the chunk eligible.

The existing #1596 generation fence extends through final writeback and
reservation release, so a late result cannot overwrite a newer edit and cannot
resurrect a released chunk.

Accepted fluid override and activity state belong to a durable gameplay owner,
independent of residency. Arc B's CRS-12 completes the save wiring; Arc A's
CRS-5 establishes the ownership.

### Units, edits, and other gameplay consumers

Movement, path validation, interactions, construction, and other active unit
work take explicit small reservation footprints. Edit residency is held from
validation through authoritative commit or rejection, with command identities
and acknowledgements sufficient to reconcile staged read-your-writes values.

Systems with compact off-cache authority — power topology being the precedent
from #1207 — stay authoritative without pinning their full footprint.

An operation that cannot obtain its footprint receives an explicit admission
failure. It never reads an unloaded chunk and treats the miss as flat ground.

### Transactional location materialization

A definition's canonical chunk footprint is resolved and reserved before
materialization. Authoritative placement acknowledgements aggregate under one
stamp identity, and geometry is marked complete only after every required commit
lands. Failures reconcile and retain idempotent retry; content spawning stays
independent.

This subsumes the failure shapes #1674 and #1719 repaired narrowly, and their
acceptance cases must remain green.

### Generated-world bundle *(Arc B)*

An immutable, checksummed, versioned bundle stores the expensive base data and
the zoom-map output, published atomically so a crash leaves nothing mistakable
for a usable world. Saves descended from one generated world share it by
identity, with reference-aware cleanup. Missing or incompatible base data has a
defined, compatibility-checked regeneration path.

Its manifest, index, checksum, `BaseChunkV1` and zoom-page DTO contracts freeze
in CRS-9, and the two open questions gating that freeze are Q-1 and Q-2 below.

### Asynchronous reads and presentation *(Arc B)*

Bounded prioritized worker queues handle read, checksum, decompress and pure
fallback generation. Duplicate demand joins; results carry page, bundle and
request epochs. Mutable delta application, derived repair, publication and
notification stay on the world thread under a per-tick drain budget. Save
loading enters the game scene after index validation, renders missing detail as
black, and publishes only fully composed chunks, centre and visible first.

### Always-available zoom map *(Arc B)*

The zoom map loads a complete, separately generated artifact and never enters
the detailed residency manager. Where one texture is unsafe, its internal
storage becomes tiles or multiple resolutions; absent fine detail falls back to
the complete base layer rather than a hole.

### Metrics and observability

Resident count and estimated bytes per page, request and in-flight counts,
reservation counts by reason and their ages, admission refusals with their
requested and available sizes, eviction rate and selection reasons, and pending
queue depth. Human-readable residency diagnostics are reachable from the debug
console.

## Decisions

### D-1. The world remains large and finite

Generation beyond the declared boundary is not a goal. Bounding residency is.

### D-2. Expensive creation is acceptable; ordinary play should be fast

World creation may take a long time once. Starting, loading and travelling
should not repeat it.

### D-3. The exact resident cache is transient

The resident set, its recency order and its pending work are never save data. A
load may choose a different useful set.

### D-4. Generated foundation and mutable gameplay state are separate

Digging a tile does not rewrite generated base data, and the gameplay layer does
not duplicate deterministic base data.

### D-5. Cache eviction must never decide whether gameplay survives

This is Arc A's invariant. Closed #1207 is its precedent.

### D-6. Ticking fluid chunks are not directly evictable

Eviction follows deactivation and an acknowledged final writeback, never
precedes them.

### D-7. Active fluid scratch is rebuilt, not cache-saved

A save flushes accepted state and records that settlement remains, rather than
forcing quiescence or preserving runtime scratch.

### D-8. Ordinary flora is reconstructed; sparse exceptions persist

Deterministic placement plus the persisted world date advances flora across
eviction. Harvest and regrowth remain sparse durable state.

### D-9. All chunk loading passes through one canonical manager

Camera demand, init queues, explicit regions, units, simulation, location
stamping and debug tools may not maintain independent insertion or eviction
rules. *(Delivered by CRS-1, #2001.)*

### D-10. No eviction or pressure path may exist that cannot see reservations

Stated as an invariant rather than a schedule, because it must still constrain
code long after the slices that introduce it have merged: **every path that can
remove a chunk from residency consults the reservation table first.** A second
eviction site added in a year is a violation of this decision, not merely a
deviation from a delivery order.

The ordering consequence is that expression precedes enforcement — CRS-3
introduces streaming demand and batch reservations and changes no eviction
behaviour, and CRS-4 then enforces against a table it can read.

The reason is narrower than revision 1 assumed, and worth stating precisely
because the obvious version of it is wrong. Refusing an over-large *request* does
not require reservations to exist: a ceiling-first implementation could compare a
region's size against the budget and reject it. What a ceiling-first
implementation cannot do is protect a set that was **already admitted** from
pressure generated **later by someone else's demand**. A dump admitted at 150
chunks still loses tiles when the camera moves and trims the cache underneath it.
Reservations are what make an admitted set durable for the life of the operation,
and no amount of admission-time checking substitutes for that.

### D-11. The budget is two numbers, and a reservation is refused only against the hard one

`wtdMaxChunks` has exactly one reader in the tree today — the eviction function
at `src/World/Tile/Types.hs:51`. It has never been an admission limit or a
memory safety limit; it is a working-set trim target. Treating it as a ceiling
is what made revision 1's enforcement slice unimplementable: a bare `--dump`'s
289-chunk default region exceeds 200, and a dump that loses a chunk emits a
silently short world, because `dumpTilesJSON` skips a missing chunk with no
error.

Every gameplay reservation is small — a fluid footprint is 5 chunks, a
`ruin_small` stamp is 1 to 4, the camera box and save warmup are 25 each. The
only large consumers are `--dump` and `world.loadChunksInRegion`, both tooling
paths, both unbounded by construction. One number cannot serve both populations.

So the one number becomes two, with different jobs:

- **Streaming trim target** (per page). What unreserved chunks are trimmed to.
  This is today's 200. It is a locality and performance number. Reservations do
  not count against it and can never be refused by it.
- **Hard residency ceiling** (per page). Total resident, reserved plus
  unreserved. Its only job is preventing process death. It is substantially
  larger, and its value is derived from CRS-2's measurement rather than guessed.

A batch reservation is admitted if it fits beneath the hard ceiling, and
**refused at request time with its numbers named if it does not** — never
partially admitted, and never admitted and then trimmed, because a batch
consumer that loses a chunk produces a wrong answer rather than a slow one.

Under this split a 289-chunk dump is ordinary: it holds 289 reservations well
beneath the hard ceiling, while the streaming set is trimmed to its own target
independently. A 50,000-chunk region request is refused — which is the
protection that does not exist today at all, and the real risk a ceiling should
address.

**One ceiling applies everywhere.** A tooling page gets no special allowance:
the number is chosen so that a legitimate dump region fits comfortably, and a
genuinely enormous one is refused by the same rule that protects a live session.
A second policy for batch pages would be a second thing to get wrong, and would
diverge from what gameplay actually exercises.

### D-12. Residency is bounded by count first, then measured bytes

Count enforcement closes today's bypasses and establishes ownership. A byte
budget follows only if CRS-2's measurement shows count is the wrong constraint —
not automatically.

### D-13. Work in progress uses scoped reservations

Camera visibility, active simulation, unit work, location footprints, accepted
edits and explicit tool requests declare temporary residency explicitly.
Reservations are reference-counted by reason, not a sticky boolean, and a
reservation is releasable runtime ownership, never persistence.

### D-14. The world thread remains the sole publisher of live chunks

Workers may produce immutable candidates. The world thread applies the latest
mutable state and publishes or rejects, preserving existing single-owner
invariants.

### D-15. The residency budget is per page, with a documented total

Gameplay creates exactly one world page. A process-global budget would require a
new `Engine.Core.Capability` record and the `docs/engineenv_capability_inventory.md`
§6.4 procedure with maintainer approval, to solve a configuration that does not
yet occur. Each `WorldState` owns its budget; the intended total and the
one-page assumption are written down here so the constraint is explicit even
though nothing enforces it across pages.

The trigger for revisiting: the first gameplay feature that keeps two pages
resident simultaneously promotes the budget to global, as its own issue.

### D-16. Eviction does not normally write a chunk file

The base is regenerable (Arc A) or already stored (Arc B), and mutable
consequences have an authoritative layer. Eviction drops an unreserved
reconstruction, avoiding disk latency on ordinary camera movement.

### D-17. Measurement precedes the capacity arc

Arc B's justification is that resident detailed chunks are the memory problem.
That is currently an assumption; the only measurement taken shows 289 resident
chunks sitting underneath a peak set by world generation itself. CRS-2 produces
the real numbers, and Arc B does not begin until they show resident bytes at the
targeted world sizes exceeding an acceptable ceiling.

If they do not, Arc A still stands on its own: it fixes observed correctness
defects that have nothing to do with how much memory a chunk costs.

### D-18. Zoom output is a generated artifact independent of detailed residency

The zoom map is already derived from `WorldGenParams` rather than from resident
chunks. Arc B makes that explicit in storage; the behavioural rule that opening
the map never requests gameplay chunks holds throughout.

### D-19. Regeneration is a compatibility-checked fallback

Missing or incompatible base data regenerates only under a matching generator
and content identity. Regeneration under a different identity is never a silent
substitute.

### D-20. Disk-backed hibernation is withdrawn from the delivery plan

The original revision specified a checkpoint format for accepted fluid cells, a
hibernation tier, and a deterministic fair-resumption queue, on the critical
path between the fluid slice and the final gates.

It is withdrawn because it is a large, novel, on-disk mechanism built for a
pressure condition that has never been observed, and its necessity depends on
the same unmeasured premise as Arc B. The pressure *policy* it belonged to —
target, protected foreground reserve, hard ceiling, and admission backpressure —
survives in CRS-4, which needs it anyway.

**Revival condition:** CRS-8's long-travel gate shows that reservations alone,
after Arc A's correctness work, hold a page above its hard ceiling in ordinary
play. At that point hibernation returns as its own issue, with the measurement
attached.

### D-21. Saves from one generated world share one world-library bundle *(Arc B)*

The foundation is stored once and referenced by every descendant save. Local
deletion is reference-aware. A future portable export may copy it; ordinary
saves do not.

### D-22. Save loading enters gameplay immediately and allows chunk pop-in *(Arc B)*

No separate loading transition. Missing detail renders black; centre and visible
chunks arrive at highest priority and appear only when completely composed.
Operations requiring a missing chunk wait for their reservation.

### D-23. Crash recovery beyond the last save is out of scope

Manual and autosave state is durable; progress after the last completed save may
be lost in a crash. Automatic crash recovery is separate complexity outside this
arc.

### D-24. The zoom map always renders and never depends on detailed chunks

Its internal large-world representation may be tiled or multi-resolution; a
complete global layer is always available and never shows cache holes.

## Open questions

### Q-1. Which fields belong in `BaseChunkV1`? *(Arc B, gates CRS-10)*

Status: deliberately open pending CRS-9 profiling, and informed by CRS-2.

Candidate fields are columns, base terrain and surface, fluid, ice, flora
placement, water table, and magma. Neighbour-dependent slopes, side decoration
and render caches may be cheaper and safer to derive after publication. CRS-2's
per-field-group measurement is a direct input: the ratio of `Chunk`'s per-column
vectors to the derived maps largely determines the answer. CRS-9 may select a
clearly dominant measured payload; if none meets the load target without
materially changing fidelity, bundle size or compatibility risk, it stops before
freezing and asks for a maintainer decision.

### Q-2. What shard size and compression should the bundle use? *(Arc B, gates CRS-10)*

Status: deliberately open pending CRS-9 prototype measurements.

The choice balances random-read latency, compression ratio, checksum repair,
file-count overhead and atomic publication. The contract requires indexed,
versioned, independently verifiable records; it does not choose a codec. Same
stop-and-ask rule as Q-1.

### Q-3. What are the streaming target and the hard residency ceiling, numerically?

Status: open, and answered by CRS-2 rather than by this document.

D-11 fixes the shape — two numbers, different jobs — but not their values. The
streaming target is today's 200 and may stay there; it is a locality choice, and
CRS-2 should confirm it is not accidentally the wrong order of magnitude. The
hard residency ceiling is the one that matters: it must be large enough that a
legitimate `--dump` region and every gameplay reservation fit comfortably, and
small enough to prevent process death. CRS-2 reports the measured per-chunk cost
and proposes both; the maintainer sets them.

### Q-4. Should a refused reservation block or fail? *(Gates CRS-4)*

Status: open, to be resolved inside CRS-4 with the callers in hand.

D-11 settles that a reservation is refused rather than trimmed. It does not
settle whether a gameplay caller that cannot fit should receive an immediate
failure, or wait until headroom appears. A dump should fail; a unit's movement
footprint probably should wait. CRS-4 must enumerate the callers and choose per
caller, stopping for a decision if any caller has no safe answer.

### Q-5. Must save capture active fluid mid-settle or force it to quiesce?

Status: resolved by D-7. The chosen behaviour flushes accepted state, records
that settlement remains, and resumes after load. Forcing equilibrium was
rejected because it could make saves unexpectedly slow and would advance the
simulation as a side effect.

## Verification strategy

Verification is layered so each slice proves its contract without waiting for a
final stress probe.

- Pure manager tests cover canonical aliases, one in-flight request per key,
  reservation reference counts, eligible eviction, LRU ordering, per-page
  accounting, count pressure, stale epochs, and bounded pending queues.
- Admission tests prove a reservation exceeding the hard ceiling is refused with
  its numbers reported, that nothing is partially admitted, and that streaming
  demand is trimmed rather than refused.
- Pressure-precedence tests prove the unreserved set is trimmed toward zero
  before any reservation is refused, and that an existing reservation is never
  revoked to admit a new one.
- Regression coverage proves a bare `--dump` still emits its complete 289-chunk
  default region under enforcement, and that a region exceeding the hard ceiling
  is refused rather than silently truncated. This is the specific defect that
  stalled revision 1.
- Integration coverage drives both camera loading and `world.loadChunksInRegion`
  beyond the budget and proves resident count returns to target after
  reservations release.
- A fluid test activates a chunk near the eviction frontier, moves the camera far
  enough to create pressure, proves the active footprint stays resident, awaits
  acknowledged final writeback, then proves eviction and exact reload.
- Fluid save/load coverage captures a still-active chunk, reloads it without
  restoring the old cache, and proves accepted cell state plus settlement
  resumption. Loading static ocean must not create a save delta.
- Unit and action coverage moves or constructs across a chunk boundary under
  pressure and proves the action either holds its footprint or receives an
  explicit admission failure — never a silent unloaded read.
- Location coverage stamps a multi-chunk footprint while the camera moves, proves
  every command is acknowledged before the marker, and proves a rejected attempt
  retries idempotently. #1674's and #1719's acceptance cases remain green.
- A deterministic long-travel headless probe crosses many multiples of the
  resident budget, mixes camera demand with explicit regions and active work,
  revisits altered chunks, and asserts a plateau in count and estimated bytes
  plus bounded queues and preserved gameplay state.
- *(Arc B)* Bundle codec tests freeze `BaseChunkV1`, round-trip every field,
  reject bad versions and checksums, ignore partial publication and validate
  index uniqueness under canonical wrapping. Rehydration tests compare
  `base + deltas + derived repair` against `generateLoadedChunk + replayEdits`.
  Save compatibility tests migrate a pre-bundle save. Async tests delay and
  reorder worker results and prove stale candidates never publish. Zoom tests
  prove the complete map renders with zero detailed chunks resident.
- Worldgen-output changes, if any slice causes them, follow the repository's full
  worldgen tier and save-version rules. Storage-only changes should prove output
  equivalence rather than recapture baselines casually.

## Delivery plan

Each slice is intended to fit one pull request. Issue processing may split a
slice if repository evidence proves it cannot be reviewed safely as one PR, but
must preserve the dependency order and update both this plan and the ledger.

## Arc A — residency correctness

### CRS-1. Centralize chunk demand and canonical chunk identity

> Processed: linked to #2001.

- Introduce the residency owner in `WorldState` behind a narrow boundary.
- Define canonical `ChunkKey`, request identity, in-flight deduplication, and
  page/generation epoch handling.
- Route camera, init queue, explicit-region, save warmup, cursor/debug and other
  current generation entry points through one request API without changing
  eviction behavior yet.
- Add pure tests proving aliases cannot create duplicate resident or in-flight
  entries.
- **Depends on:** none.

### CRS-2. Measure resident chunk cost and residency high-water

- Add per-page resident count and estimated-byte accounting to the CRS-1 owner,
  and expose it to the debug console.
- Measure a materialized `LoadedChunk`'s resident cost by field group at
  representative terrain complexities, using a counter at a defined moment rather
  than `+RTS -s` maxima, which sample only at major GCs and report world
  generation's peak instead of the cache's.
- Measure residency high-water across a traversal at the world sizes actually
  targeted, not only at 64.
- Report whether count or bytes is the binding constraint.
- **Propose both of D-11's numbers** — the streaming trim target and the hard
  residency ceiling — with the measured evidence for each, for the maintainer to
  set under Q-3. CRS-4 cannot be implemented until they exist.
- Write the results into this document, and record whether Arc B's gate opens.
- **Depends on:** CRS-1. **Independent** of every other Arc A slice and **can
  land first** among them.

### CRS-3. Express chunk demand as streaming requests and batch reservations

- Introduce the two request classes of D-10 with owner and reason diagnostics
  and a bracketed release.
- Migrate camera minimum-residency to streaming demand; migrate explicit region
  loads, save warmup, the dump fill, simulation footprints, unit work, location
  stamps and accepted edits to reservations.
- Reference-count reservations by reason; make ages and counts observable.
- **Change no eviction behavior.** This slice is additive: nothing is dropped
  differently, and the existing ceiling behaves exactly as it does today.
- **Depends on:** CRS-1.

### CRS-4. Enforce the residency budget against unreserved chunks only

- Move insertion and eviction eligibility into the manager.
- Split `wtdMaxChunks` into D-11's two numbers: a streaming trim target and a
  hard residency ceiling, both per page, using the values CRS-2 measured and the
  maintainer set.
- Apply the trim target to unreserved chunks on camera and explicit/init paths
  alike, now that a held set is distinguishable from a droppable one.
- Replace furthest-only selection with tested recency plus distance and priority
  tie-breaking, over unreserved entries only.
- Implement D-11 admission: a reservation is refused at request time against the
  hard ceiling, with its numbers, and never partially admitted. Implement the
  three-step pressure precedence, so streaming is sacrificed before any
  reservation is refused and no existing reservation is ever revoked.
- Resolve Q-4 per caller, and migrate every existing caller of
  `world.loadChunksInRegion` to handle a refusal rather than a count.
- Bound pending requests so a huge region call cannot consume unbounded memory
  before admission even runs.
- Prove a bare `--dump` still emits its complete 289-chunk default region, and
  that a region exceeding the hard ceiling is refused rather than truncated.
- **Depends on:** CRS-3 for the reservation mechanism, and CRS-2 for both
  budget numbers.

### CRS-5. Make active fluid simulation eviction- and save-safe

- Acquire simulation footprint reservations before activation.
- Reverse the unload relationship: eviction occurs only after simulation
  deactivation and acknowledged final writeback.
- Extend the #1596 per-chunk generation fence through final writeback and
  reservation release.
- Add accepted fluid override and activity ownership independent of residency;
  save wiring completes in CRS-12.
- **Depends on:** CRS-4.

### CRS-6. Reserve chunks used by unit work and authoritative world edits

- Give movement, path validation, interactions, construction and other active
  unit work explicit small reservation footprints.
- Hold edit residency from validation through authoritative commit or rejection.
- Add command identities and acknowledgements needed to reconcile staged
  read-your-writes values safely.
- Preserve compact off-cache authority for systems such as power instead of
  pinning their full topology.
- **Depends on:** CRS-4. Independent of CRS-5 and may proceed in parallel.

### CRS-7. Make location stamping transactional across its footprint

- Resolve a definition's canonical chunk footprint and reserve it before
  materialization.
- Aggregate authoritative placement acknowledgements under one stamp identity.
- Mark geometry complete only after every required commit; reconcile failures and
  retain idempotent retry.
- Preserve independent content spawning.
- Keep #1674's and #1719's acceptance cases green; both are closed, and this
  slice must not reintroduce either failure mode.
- **Depends on:** CRS-6.

### CRS-8. Add residency diagnostics and the long-travel plateau gate

- Complete the metrics inventory and human-readable residency diagnostics.
- Add the deterministic mixed-workload long-travel probe and the save/revisit
  oracle, asserting a plateau in count and estimated bytes.
- Update the persistence inventory and relevant engine contracts to state the
  final ownership boundaries.
- Record whether D-20's hibernation revival condition is met.
- **Depends on:** CRS-4, CRS-5, CRS-6, CRS-7.

## Arc B — capacity and streaming *(gated)*

**Gate:** CRS-2's measurement shows resident bytes at the targeted world sizes
exceeding the ceiling set for Q-3. Until that is recorded in §Measurements, every
slice below is correctly dispositioned `[deferred]` on this precondition.

### CRS-9. Define the versioned generated-world bundle and base-chunk record

- Resolve Q-1 and Q-2 with focused size, cold-read and generation-cost
  prototypes; stop for a maintainer decision before freezing either format if
  the documented measurement rules do not yield a clear choice.
- Freeze manifest, index, checksum, `BaseChunkV1` and zoom-page DTO contracts.
- Define world, generator and content identity and compatibility rules.
- Implement atomic incomplete/complete publication and corruption detection in
  isolation from runtime loading.
- **Depends on:** the Arc B gate. Logically independent of Arc A.

### CRS-10. Write base chunks and the reusable zoom artifact during world generation

- Stream every physical canonical base chunk into indexed bundle shards during
  finite-world creation without retaining all detailed chunks at once.
- Store the complete reusable zoom base plus any internal fine-detail data.
- Verify the complete inventory and atomically publish the manifest.
- Measure generation time, peak memory, bundle size and compression.
- Register the published bundle in the shared world library under D-21's identity.
- **Depends on:** CRS-9.

### CRS-11. Materialize resident chunks from the bundle plus durable deltas

- Read and validate `BaseChunkV1`, convert to the live chunk, apply ordered
  gameplay state and time-derived flora, then perform resident-neighbour repair
  before publication.
- Add bundle identity to the save component graph and migrate pre-bundle saves
  through the compatible-generation path.
- Reuse one registered bundle across descendant saves with explicit
  last-reference cleanup.
- Define missing, corrupt and mismatched bundle diagnostics without partial world
  publication.
- Keep pure generation as a compatibility-checked fallback.
- **Depends on:** CRS-9, CRS-10, and CRS-1's publish boundary.

### CRS-12. Replace resident-chunk fluid snapshots with sparse durable fluid state

- Introduce a versioned fluid-state component keyed by canonical chunk, carrying
  accepted override data and `needsSettlement`.
- Stop `appendFluidSnapshot` enumerating every resident chunk.
- Migrate existing ordered fluid snapshot edits exactly, preserving the latest
  per-chunk result whether or not that chunk was loaded at migration time.
- Make save barriers serialize accepted fluid authority rather than cache
  membership.
- **Depends on:** CRS-5, CRS-11.

### CRS-13. Move chunk reads and fallback generation off the world thread

- Add bounded prioritized worker queues for read, checksum, decompress and pure
  fallback generation.
- Join duplicate demand and tag results with page, bundle and request epochs.
- Keep mutable delta application, derived repair, publication and notifications
  on the world thread under a per-tick drain budget.
- Add camera-motion prefetch and cancellation behavior.
- Enter the game scene after index validation, render missing detail as black,
  and publish only fully composed chunks centre and visible first, per D-22.
- **Depends on:** CRS-4, CRS-11.

### CRS-14. Keep the complete zoom map available at large world sizes

- Load a complete separately generated zoom artifact from the bundle; opening it
  never enters the detailed residency manager.
- Preserve an always-available complete base representation, including when no
  detailed chunks are resident.
- Replace one unsafe whole-world atlas with internal texture-dimension-safe tiles
  or multiple resolutions; absent fine detail falls back to the complete base.
- Retain live icon and discovery overlays and preserve current pixels and
  interaction coordinates at existing sizes.
- **Depends on:** CRS-10. **Independent** of CRS-11 through CRS-13 and **not on
  the critical path**.

### CRS-15. Add bundle corruption, latency, and pressure gates

- Add bundle corruption and partial-publication recovery probes.
- Add the performance probe recording bundle size, worldgen write overhead, cold
  and warm chunk latency, save-load time, zoom-page upload time and resident high
  water.
- Establish documented latency and high-water baselines from measurements, then
  gate regressions at stable thresholds.
- **Depends on:** CRS-9, CRS-10, CRS-11, CRS-12, CRS-13, CRS-14, and CRS-8.

## Source notes

- Primary cache and eviction: `src/World/Tile/Types.hs`.
- Camera and init/explicit loading: `src/World/Thread/ChunkLoading.hs` and
  `src/Engine/Scripting/Lua/API/WorldQuery/Chunk.hs`.
- Canonical queue identity: `src/World/Chunk/Queue.hs` (#1723).
- Dump region fill and emit: `app/App/Dump.hs`, `app/App/Cli.hs`.
- Chunk shape and deterministic generation: `src/World/Chunk/Types.hs` and
  `src/World/Generate/Chunk.hs`.
- Fluid activity and writeback: `src/Sim/State/Types.hs`,
  `src/Sim/Fluid/Active.hs`, and `src/Sim/Thread.hs`.
- Current fluid save snapshot: `src/World/Thread/Command/Save/WriteWorld.hs`.
- Persistence classifications: `docs/persistence_state_inventory.md`.
- Capability and `EngineEnv` ownership rules:
  `docs/engineenv_capability_inventory.md`.
- Flora reconstruction precedent: `src/World/Flora/Growth.hs` and
  `src/World/Flora/Harvest.hs`.
- Location materialization: `scripts/location_stamper.lua`,
  `scripts/locations.lua`, and `src/World/Thread/ChunkLoading.hs`.
