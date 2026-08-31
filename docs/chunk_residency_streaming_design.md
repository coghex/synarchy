# Chunk residency and world streaming design

This document designs the foundation that lets Synarchy keep a large, finite,
procedural world without keeping every detailed chunk in memory. It separates
three concerns that the current runtime partially mixes together: the generated
world foundation, the bounded set of chunks materialized for immediate use, and
the sparse gameplay state that must survive chunk eviction and save/load.

The design deliberately does not treat the in-memory cache as save data. A save
restores the same world and the same gameplay consequences, but it may choose a
different useful set of resident chunks after loading.

Design state: `ready for issue processing`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [x] EPIC. Bound detailed-world memory while preserving durable gameplay state — [#1997]
- [x] CRS-1. Centralize chunk demand and canonical chunk identity — [#2001]
- [ ] CRS-2. Enforce bounded residency across every chunk-loading path
- [ ] CRS-3. Add scoped chunk leases and an explicit pressure policy
- [ ] CRS-4. Make active fluid simulation eviction- and save-safe
- [ ] CRS-4A. Add disk-backed hibernation and fair fluid resumption
- [ ] CRS-5. Lease chunks used by unit work and authoritative world edits
- [ ] CRS-6. Make location stamping transactional across its footprint
- [ ] CRS-7. Define the versioned generated-world bundle and base-chunk record
- [ ] CRS-8. Write base chunks and the reusable zoom artifact during world generation
- [ ] CRS-9. Materialize resident chunks from the bundle plus durable deltas
- [ ] CRS-10. Replace resident-chunk fluid snapshots with sparse durable fluid state
- [ ] CRS-11. Move chunk reads and fallback generation off the world thread
- [ ] CRS-12. Keep the complete zoom map available at large world sizes
- [ ] CRS-13. Add long-travel, latency, pressure, and corruption gates

## Epic contract

- **Goal:** Keep detailed-world RAM use bounded independently of travel distance,
  while preserving every gameplay-affecting consequence and making ordinary
  play and save loading fast after the finite world has been generated once.
- **Done when:** Every chunk request passes through one canonical residency
  manager; the resident set is measured and bounded; a chunk in foreground use
  cannot be evicted; background fluid can cross an acknowledged checkpoint into
  disk-backed hibernation and later resume; gameplay edits, fluid changes, flora
  harvests, locations, structures, units, and other authoritative state do not
  depend on cache residency; an immutable, checksummed, versioned
  generated-world bundle stores the expensive base data and zoom-map output;
  saves descended from one generated world share that foundation by identity
  and carry only mutable gameplay state; loading a chunk composes base data with
  current deltas and time-derived state; missing compatible base data has a
  defined regeneration path; disk work and fallback generation do not block the
  world thread; the independently generated zoom map always renders complete
  world coverage without requiring detailed chunks or one unbounded atlas; and
  deterministic long-travel tests prove that memory plateaus and revisited
  chunks preserve gameplay state.
- **Users and operators:** Players travelling through large worlds; developers
  increasing world size or chunk complexity; maintainers of world generation,
  simulation, locations, save/load, units, and rendering.
- **Arc label:** proposed `world-streaming`

## Current state and evidence

- `World.Tile.Types.WorldTileData` owns a strict `HashMap ChunkCoord LoadedChunk`
  and an integer `wtdMaxChunks`. The default ceiling is 200 chunks. It counts
  entries, not their memory cost.
- That is a nominal per-page count cap, not a reliable memory limit. Ordinary
  worlds use 200 and arenas use 100, while the camera requests a 5-by-5
  neighborhood (`chunkLoadRadius = 2`). There is no global or byte ceiling, and
  several pages may each hold their own allowance.
- `evictDistantChunksWithReport` preserves the camera keep-radius and evicts
  other chunks furthest-first. It has no concept of active simulations,
  in-flight edits, unit work, location footprints, leases, recency, or dirty
  writeback. Its comment assumes the edit log is sufficient for every edited
  chunk.
- The camera-driven path in `World.Thread.ChunkLoading.updateChunkLoading`
  inserts a generated batch and then invokes that eviction function. The
  explicit/init-queue path in `drainInitQueues` inserts chunks but does not run
  eviction. `world.loadChunksInRegion` can therefore request a region much
  larger than the nominal cache ceiling.
- Chunk coordinates have cylindrical aliases. `drainInitQueues` now
  canonicalizes immediately before generation, but request producers and cache
  accounting do not yet share one canonical admission point. A residency
  manager must make one physical chunk equal one cache key before deduplication,
  leasing, accounting, or queueing.
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
  currently resident chunk into the edit log. It replaces older fluid snapshots
  for those resident chunks while preserving snapshots for chunks that happen
  to be unloaded. This makes the saved fluid representation depend on residency
  and records unchanged ocean chunks merely because the camera loaded them.
- `Sim.Thread` creates a `SimChunkState` when a detailed chunk loads and deletes
  it on `SimChunkUnloaded`. Active chunks track volume state and equilibrium
  ticks; `Sim.Fluid.Active` deactivates them after 200 equilibrium ticks.
  Eviction does not ask the simulation whether a chunk is active and does not
  await a final writeback before deleting it.
- The existing fluid writeback generation fence from #1596 prevents a stale
  pre-edit result from overwriting a newer edit, but it does not make eviction a
  coordinated simulation handoff.
- Flora already demonstrates the intended reconstruction boundary.
  `World.Flora.Growth` derives ordinary growth and generational changes from
  deterministic placement plus the persisted absolute world day. The sparse
  harvest/regrowth map separately preserves player-visible exceptions.
- Location placement has a durable generated overlay and per-location stamped
  flags, but materialization is asynchronous. The loader dispatches a Lua stamp
  whenever the host chunk loads; the Lua builder queues authoritative world
  edits and currently marks the location stamped without a world-thread commit
  acknowledgement.
- Open #1674 covers a staged structure phantom when eviction lands between the
  Lua pre-check and world-thread commit. Open #1719 covers the narrower case in
  which location placement reports failure but the location is still marked
  complete. Both remain valid repairs; neither defines a lease for the whole
  footprint or a general chunk transaction.
- Closed #1207 is an important precedent: persistent wire topology was moved
  out of loaded-chunk-only state because camera-driven eviction must not suspend
  gameplay systems. Residency is a performance choice, not world truth.
- Save loading rebuilds the whole zoom cache and, for the active page, all
  per-chunk pixel data, a whole atlas, and a preview. The loaded-save path lacks
  the fresh-generation bordered cache and explicitly pays terrain recomputation.
- `World.ZoomMap.ChunkTexture` packs all 32-by-32 RGBA chunk tiles into one
  image. The pixel payload alone grows linearly with physical chunk count, and
  the single atlas eventually hits both RAM and GPU-dimension limits as finite
  world size increases.
- Despite the name “chunk” in its tile-building code, the zoom map is already a
  separate generated representation: `World.ZoomMap.Cache.BuildPixels` derives
  it from `WorldGenParams`, not from which detailed `LoadedChunk` values happen
  to be resident. This separation is correct and must become explicit in the
  new storage design.
- The first tracker search and the readiness recheck on 2026-08-25 found no
  existing chunk-residency, chunk-streaming, or generated-world-bundle epic.
  The four currently open epics own unrelated probe determinism, item-container,
  and expedition arcs. The open location bugs above overlap two failure paths;
  existing save/load, worldgen, and power issues are consumers or precedents,
  not owners of this architectural arc.

## Desired experience

### Generating a world

World generation may remain a deliberate, potentially long operation. When it
finishes, it publishes one complete generated-world foundation atomically. A
crash or cancellation before completion leaves no bundle that can be mistaken
for a usable world.

The expensive result is reusable. Starting a game, loading a save, opening the
zoom map, or revisiting a distant region should not repeat work that was already
finished during world generation.

### Travelling during play

The camera and active gameplay request nearby detailed chunks. Recently useful
chunks may remain warm, but the amount of resident detail reaches a plateau.
Travel across ten thousand chunks must not leave ten thousand chunks in RAM.

A visible or imminently needed chunk is loaded from the generated foundation.
The engine then applies mutable gameplay state and current time-derived state
before publishing it. The player sees the same altered terrain, structures,
fluid, harvest state, and location completion that existed before eviction.

An ordinary eviction does not write a complete chunk file. Base data is already
in the generated-world bundle, and mutable consequences already belong to the
durable gameplay layer. Eviction is therefore mostly dropping reconstructible
RAM after proving nobody still holds a lease.

### Simulation away from the camera

Being off camera is not itself a reason to forget gameplay. A fluid chunk that
is actively ticking stays resident, along with the neighboring footprint its
simulation requires, until a final state is accepted by the world owner. Under
exceptional memory pressure, eligible background fluid may instead checkpoint
that accepted state, enter a disk-backed hibernation queue, release its chunks,
and resume later. Nothing is discarded merely because it is off camera. A
moving or working unit leases the terrain it is actively using. Persistent
systems such as power remain authoritative independently of loaded chunks.

Conversely, a static middle-of-ocean chunk has no reason to remain resident.
Ordinary flora can be reconstructed and advanced from the world clock. Sparse
exceptions such as harvested plants remain durable without pinning the chunk.

### Saving and loading

Saving does not serialize the incidental LRU order, resident set, pending
prefetches, or worker jobs. It persists gameplay state and a reference to the
immutable generated foundation. Active fluid is represented by its accepted
cell state plus enough activity metadata to resume settling, not by preserving
the whole runtime cache.

After save metadata, bundle identity, and the gameplay-state index validate,
loading enters the game scene without a separate loading transition. Missing
detail renders as black background. The center and visible chunks are requested
at highest priority and appear only when each is completely composed; chunks
may pop in independently. Gameplay and simulation operations that require a
missing chunk wait for its lease instead of reading partial data. The exact
post-load cache contents are allowed to differ from the pre-save contents; the
gameplay answers are not.

### Zoom map

The zoom map is always available as a complete view of the finite world and is
unrelated to detailed chunk residency. Opening it never requests, leases, or
waits for gameplay chunks. World generation writes its reusable map artifact,
and save loading restores that artifact instead of rebuilding it from detailed
terrain.

At world sizes too large for one GPU texture, the renderer may divide the
artifact internally or use multiple resolutions. That is invisible storage:
the whole world still has an immediately renderable base layer, and finer data
may replace it without black areas or dependency on the detailed cache.

## Scope

### In scope

- A large but finite procedural world.
- A process-wide residency budget with per-world attribution.
- A disk-backed hibernation tier for checkpointable background simulation when
  leases alone would otherwise exceed the hard memory ceiling.
- A single canonical request, deduplication, load, publish, lease, and eviction
  lifecycle for detailed chunks.
- Count-based bounding first and measured-byte bounding after the ownership
  model is stable.
- Scoped leases for camera visibility, simulation, unit work, location
  materialization, authoritative edits, and explicit callers.
- Coordinated fluid activation, final writeback, save/load restoration, and
  lease release.
- An immutable generated-world bundle with a manifest, version and generator
  identity, chunk index, checksums, atomic publication, and reusable zoom data.
- A shared world-library bundle referenced by every save descended from that
  generated world, with reference-aware deletion.
- Composition of generated base data, sparse mutable deltas, and time-derived
  state into a resident chunk.
- A sparse per-chunk fluid representation independent of current residency.
- Asynchronous read/decompress/fallback-generation workers with world-thread
  publication and stale-result rejection.
- Priority, prefetch, metrics, diagnostics, and deterministic stress coverage.
- Immediate entry into the game scene with black missing-detail regions and
  fully composed chunks appearing progressively, rather than a loading screen.
- An always-renderable zoom map independent of detailed chunk residency. Its
  internal storage may be tiled or multi-resolution to remain GPU-safe, but a
  complete global layer is always available and never shows cache holes.
- Compatibility and failure behavior for missing, corrupt, or incompatible
  generated foundations.

### Out of scope

- An infinite world or generation beyond the finite world's declared boundary.
- Keeping every detailed chunk or simulating every tile continuously.
- Saving and restoring the exact RAM cache, its recency order, or pending
  background work.
- Making every cosmetic or deterministic derived value into persistent save
  data.
- A general rewrite of unit, building, item, or power persistence. This arc
  changes only their dependency on chunk residency where required.
- Cloud storage, cross-device bundle transfer, multiplayer synchronization, or
  a shared remote chunk service.
- Changing terrain, hydrology, flora, or location generation output merely to
  make storage easier.
- Treating regeneration under a different generator/content identity as a
  silent substitute for the original generated foundation.
- Automatic crash recovery or a write-ahead journal for progress since the last
  manual/autosave.

## Design

### Three separate layers

The word “cache” should name only the third row below. Keeping the layers
separate prevents cache eviction from becoming either a save operation or a
gameplay deletion.

| Layer | Meaning | Lifetime | Examples |
|---|---|---|---|
| Generated-world foundation | Immutable result of expensive finite-world generation | From successful world creation until the last referencing save/world is deleted | Base chunk records, generation identity, location overlay, complete zoom artifact |
| Durable gameplay state | Authoritative differences and clocks | Save/session lifetime, independent of chunk residency | Terrain/structure edits, sparse fluid state, flora harvests, stamped locations, units and buildings |
| Resident detail cache | Materialized chunks ready for immediate queries/render/simulation | Bounded runtime working set | Base chunk + current deltas + derived seams/decoration |

The disk-backed hibernation store is a physical backing mechanism for durable
gameplay state, not a fourth source of truth. It holds accepted checkpoints and
pending-work markers when their full detailed chunks must leave RAM.

A chunk load is conceptually:

```text
indexed BaseChunk
    + latest durable per-chunk deltas
    + state derived from current world time
    + resident-neighbor derived repair
    = published LoadedChunk
```

The generated foundation is not rewritten when a player digs a tile. The
resident cache is not copied into the save. The gameplay layer does not need to
duplicate deterministic base data.

### Central residency lifecycle

One manager per process owns the global budget and attributes use to each
`WorldPageId`. Per-page state remains with `WorldState`; this manager is not a
new unrestricted `EngineEnv` dumping ground. The exact capability boundary must
follow `docs/engineenv_capability_inventory.md` if implementation needs a new
capability.

Every request first becomes a canonical `ChunkKey` containing its page and
u-wrapped physical coordinate. One key has at most one resident value and one
in-flight load. Camera loading, `world.loadChunksInRegion`, save-load warmup,
cursor/debug generation, units, simulation, and location stamping use the same
admission path.

The manager tracks these conceptual states:

```text
absent -> requested -> loading -> resident/unleased -> eviction -> absent
                         |              ^       |
                         +-- publish ---+       +-- acquire lease
                                      resident/leased
```

Dirty or activity state is orthogonal: it belongs to a durable gameplay owner
or an acknowledged in-flight transaction, never only to the cache entry.

Publication remains world-thread-owned. A worker may read, verify, decompress,
or regenerate immutable base data, but it returns a candidate tagged with the
canonical key, world generation identity, and request epoch. The world thread
rejects candidates for deleted/replaced pages or obsolete epochs, applies the
latest mutable deltas, repairs resident-neighbor-derived data, publishes once,
and notifies dependent threads.

### Bounded residency and eviction

The first enforcement milestone uses a count budget so all ingress paths become
correct before byte estimation adds complexity. The second records an estimated
resident size for each materialized chunk and enforces a byte target, because a
future `LoadedChunk` may be much larger than today's.

Eviction chooses only entries with zero leases, no publish in progress, and no
unacknowledged authoritative transaction. Among eligible entries it uses least
recent useful access as the primary signal, with distance and prefetch class as
tie-breakers. “Last checked” must mean least recently used, not the most recent
chunk the player touched.

The budget is global so several loaded world pages cannot each consume the full
allowance invisibly. Per-page minimums or weights may prevent a background page
from starving the active page, but their sum remains accountable to one total.

Eviction is not the normal persistence trigger. Any mutable state that would be
lost by dropping the cache has already been committed to its authoritative
runtime layer. The save operation later serializes that layer. Optional crash
recovery journaling is a separate question from cache correctness.

Per D-17, the manager has both a target and a hard ceiling. It first drops
unleased reconstructions. If checkpointable background simulation still keeps
the process above target, that work may enter disk-backed hibernation and release
its leases. Foreground camera capacity has a protected reserve. If
non-hibernatable foreground leases nevertheless reach the hard ceiling, new
work waits rather than allowing an out-of-memory failure.

### Scoped leases

A lease is a temporary declaration that a system needs one chunk or an explicit
footprint of chunks to remain resident and stable enough for its operation. It
does not make the chunk persistent. Each lease carries an owner/reason and is
released by a bracketed or token-scoped lifecycle so diagnostics can identify a
leak.

Initial lease reasons are:

- camera visible/near-future neighborhood;
- active fluid simulation, including required cardinal neighbors;
- a unit's current movement, path validation, interaction, or construction
  footprint;
- a location stamp's complete declared footprint;
- an accepted authoritative terrain/fluid/structure edit until commit or
  rejection acknowledgement;
- an explicit tool or script request until its promised operation completes.

Leases are reference-counted by reason, not represented as one sticky boolean.
Eviction requests against a leased chunk become deferred pressure, not a forced
unload. Long-lived lease ages and reason counts are observable.

Camera movement rotates a small neighborhood lease. Prefetch is not a lease: a
prefetched chunk remains immediately evictable until a real consumer claims it.

### Active fluid ownership

Fluid is the clearest gameplay-affecting case and defines the handoff pattern
other simulations can reuse.

Before activation, the simulation acquires leases for the chunk and every
neighbor required by seam transfer. Activation fails or waits if that footprint
cannot be admitted safely. While active, camera distance cannot evict any member
of the footprint.

Each simulation result carries the existing edit generation/revision fence. A
chunk becomes releasable only after:

1. it has remained at equilibrium for the required duration;
2. the simulation emits its final world writeback;
3. the world thread accepts that writeback into the resident chunk and the
   authoritative sparse fluid layer;
4. the world thread acknowledges the accepted revision; and
5. the simulation marks the chunk inactive and releases its leases.

An ordinary unload request during this sequence waits. `SimChunkUnloaded` is a
result of successful cache eviction, not the command that prematurely destroys
active state. Hard pressure may ask the simulation to checkpoint instead: the
same accepted-writeback handshake records current cells and a continuation
marker, changes the activity state from ticking to hibernated, and only then
releases the leases. A hibernated chunk is no longer actively calculating; Q-9
settles how its delayed game-time progress should be presented.

Saving does not enumerate whatever chunks happen to be resident. The save
barrier obtains an accepted fluid revision for every dirty/active fluid chunk
and serializes a per-chunk override plus a `needsSettlement` marker where
settling should resume after load. The transient solver scratch vectors remain
rebuildable and are not saved.

The durable representation is sparse by chunk: chunks identical to their base
fluid do not appear. An altered chunk may initially store a complete 256-cell
fluid snapshot for simplicity; cell-level compression/diffing is an encoding
choice, not an ownership change. Static ocean therefore costs bundle space but
not unbounded save growth merely because the player viewed it.

### Units, edits, and other gameplay consumers

A moving or working unit needs exact terrain only for the chunks participating
in its current action. That action acquires a small lease footprint and releases
it when the action ends or moves on. An idle unit's persistent identity,
inventory, task state, and position do not require its entire chunk to stay
materialized.

An accepted world-edit command holds a lease or equivalent residency ticket
from validation through authoritative commit/rejection. This closes the general
time-of-check/time-of-use gap in which Lua sees a chunk, eviction removes it,
and the world thread later declines the command. Read-your-writes staging still
needs a command identity so success or rejection clears only the matching
staged value.

Systems whose truth spans unloaded terrain—power topology is the existing
example—continue to own compact authoritative data outside the cache. The new
manager must not solve such systems by pinning every chunk they touch.

### Transactional location materialization

A location stamp calculates its declared chunk footprint before it starts and
requests a lease for the entire footprint. The host chunk alone is insufficient
for future definitions that cross seams or span several chunks.

The stamp has one transaction identity. Placement commands report authoritative
world-thread success or rejection to that transaction. The location is marked
stamped only after every required geometry command has committed. On failure,
staged read-your-writes entries are reconciled, the durable completion marker is
not written, and the existing load-triggered retry remains available and
idempotent. Content spawning retains its independent one-time marker.

Open #1674 and #1719 should either land as prerequisite correctness repairs or
be explicitly subsumed by this slice during issue processing. They must not be
silently duplicated or closed merely because this design exists.

### Generated-world bundle

Successful world generation publishes an immutable indexed bundle rather than
one file per cache eviction. Its logical contents are:

- a manifest with bundle schema version, world identity, world dimensions,
  generator/content fingerprint, base-chunk schema version, zoom schema
  version, shard inventory, sizes, and checksums;
- an index from canonical physical `ChunkCoord` to a chunk record and checksum;
- base-chunk records sufficient to avoid the expensive generation work chosen
  under Q-4;
- reusable zoom-map entries and pixel pages;
- any immutable world-wide inputs that are not already embedded safely in the
  save's generation parameters.

The physical layout should use indexed shards or packs, not thousands of
individually opened tiny files and not one monolith that must be rewritten for
one corrupt region. The exact shard size and compression codec are measured
choices under Q-5.

Generation writes to a temporary bundle location, verifies the manifest and
record inventory, then atomically publishes completion. The manifest (or its
complete marker) is written last. Partial bundles are ignored or recoverable;
they are never presented as valid worlds.

A save records the bundle identity and required fingerprint. If the matching
bundle is missing, fallback regeneration is allowed only when the current
generator/content identity is declared compatible. A mismatch must produce a
clear migration/recovery decision rather than silently rebuilding different
base terrain under old gameplay deltas.

Per D-18, one world-library bundle is shared by all saves descended from that
generated world. Saves carry its identity and required fingerprint rather than
copying the base terrain. Save deletion decrements its known references, and a
bundle becomes eligible for confirmed cleanup only after no saves reference it.
A future self-contained export may copy the bundle alongside a save without
changing ordinary local ownership.

### Base chunk versus materialized chunk

`LoadedChunk` should not automatically become the disk schema. Its fields have
different ownership:

- generated candidates include columns, base terrain/surface, base fluid and
  ice, flora placement, water table, and magma;
- mutable overlays include player terrain/fluid/structure changes and other
  authoritative gameplay consequences;
- cheap resident-derived values include side decoration, neighbor-sensitive
  slope repair, and render caches;
- location geometry currently materializes lazily from the generated location
  overlay and durable completion state.

`BaseChunkV1` is a frozen storage DTO with an explicit conversion into the live
type. It is not a `Serialize LoadedChunk` shortcut. Which generated candidates
belong in V1 is decided by measured generation cost and size under Q-4; the
contract is that rehydration produces the same gameplay-visible chunk as
generation plus replay.

### Save/load composition and compatibility

The save and bundle evolve independently:

- bundle schema changes describe immutable generated artifacts;
- save component schema changes describe mutable gameplay state and the bundle
  reference;
- runtime chunk types may change without redefining either wire format.

On load, the engine validates the save component graph, resolves the bundle and
gameplay-state index, then publishes the world shell without a separate loading
transition. Missing detailed regions render black. Center and visible chunk
requests run first, and each chunk publishes only after its base, deltas,
time-derived state, and resident-neighbor repair are complete. Older saves
without a bundle reference migrate by creating or locating a compatible bundle
from their persisted generation parameters before they are rewritten in the
new format; that one-time migration may still require an explicit conversion
operation rather than pretending the old save has a ready bundle.

The existing ordered world edits may remain the initial general delta format,
but fluid snapshots move to a residency-independent component. Later compaction
may fold long per-chunk edit histories into snapshots without changing the
layering contract.

### Asynchronous reads, fallback generation, and prefetch

Workers receive immutable job inputs and perform file read, checksum validation,
decompression, DTO decoding, or pure fallback generation. They do not mutate
`WorldState`, send simulation activation directly, stamp locations, or publish
render data.

The world thread drains ready results within a time budget and performs the
sole authoritative publish. A result is discarded if its page, bundle identity,
or request epoch is stale. Duplicate demand joins the existing in-flight job.

Initial priority classes are:

1. chunks in the current visible camera view, with the center first;
2. chunks required by an already active simulation or unit action;
3. explicit gameplay/script operations;
4. predicted camera prefetch;
5. diagnostics and bulk warming.

Prefetch follows camera motion into a small bounded margin and is canceled or
left evictable when direction changes. Worker queues themselves have limits, so
a huge explicit region request cannot turn pending work into a second unbounded
cache.

Per D-19, missing detail is a supported render state rather than a reason to
enter a loading transition. Numeric latency thresholds are established from the
prototype measurements in CRS-13, but progressive appearance is part of the
behavioral contract now.

### Always-available zoom map

The zoom map is not part of detailed chunk residency. Its current builder uses
per-coordinate tiles internally, but it derives a separate whole-world product
from generation parameters; opening the map must never cause gameplay chunks to
load or evict.

Persisting that worldgen output removes save-load recomputation. At larger world
sizes, one whole-world GPU atlas still becomes unsafe, so the stored artifact
may use fixed internal tiles and multiple resolutions. A compact complete-world
base level remains available at all times. Higher-resolution tiles may refine
the image, but missing high detail falls back to the complete base rather than
black, and none of these tiles share the detailed residency manager.

Dynamic icons and discovery state remain overlays read from live gameplay state
and do not force rebaking immutable map terrain. The internal storage strategy
is an implementation detail; the player-facing contract is one complete map
that always renders.

### Metrics and observability

The manager exposes at least:

- resident chunk count and estimated bytes, globally and per page;
- leased count/bytes by reason, oldest lease age, and deferred evictions;
- pending and in-flight jobs by priority;
- memory hits, bundle hits, fallback regenerations, checksum failures, and
  evictions;
- request-to-publish latency distribution and worst observed latency;
- durable delta count/bytes by category, especially fluid;
- zoom base/fine-detail CPU and GPU residency plus upload latency;
- bundle read, decompression, and fallback-generation timings.

Debug output must make “why can this chunk not evict?” answerable without a
profiler. Production counters should remain cheap and bounded themselves.

## Decisions

### D-1. The world remains large and finite

This arc does not introduce an infinite generator. Finite world generation may
visit every physical chunk or equivalent global data when correctness requires
it.

### D-2. Expensive creation is acceptable; ordinary play should be fast

World creation may take significant time. That cost buys a reusable generated
foundation so gameplay, revisits, and save loading do not repeat the full
generation pipeline.

### D-3. The exact resident cache is transient

A save does not preserve which chunks happened to be loaded, their recency
order, prefetches, or pending jobs. Loading reconstructs the useful working set
around current gameplay needs.

### D-4. Generated foundation and mutable gameplay state are separate

Immutable base chunks and zoom output belong to a versioned generated-world
bundle. Gameplay-affecting differences belong to the save/runtime durable
layer. A resident chunk is their materialized composition.

### D-5. Cache eviction must never decide whether gameplay survives

Any state whose loss would change gameplay is either independently durable,
derived from independently durable inputs, or protected by a lease until its
authoritative handoff completes.

### D-6. Ticking fluid chunks are not directly evictable

A chunk with currently ticking fluid and its required simulation neighbors
remain leased until the world thread acknowledges either the final settled state
or D-17's transition into hibernation. The cache never simply drops active
solver state. Static base ocean has no such lease.

### D-7. Active fluid scratch is rebuilt, not cache-saved

Save/load preserves accepted fluid state and whether settling must resume. It
does not serialize solver scratch arrays or the surrounding resident cache.

### D-8. Ordinary flora is reconstructed; sparse exceptions persist

Deterministic placement plus the persisted world date advances ordinary flora
across eviction. Harvest/regrowth and any future gameplay-authored exceptions
remain sparse durable state.

### D-9. All chunk loading passes through one canonical manager

Camera demand, init queues, explicit regions, units, simulation, location
stamping, and debug tools may not maintain independent insertion/eviction rules.

### D-10. Residency is bounded by count first, then measured bytes

Count enforcement closes today's bypasses and establishes ownership. Estimated
byte accounting then protects against chunks becoming more expensive while the
game develops.

### D-11. Work in progress uses scoped leases

Camera visibility, active simulation, unit work, location footprints, and
accepted edits declare temporary residency explicitly. A lease is releasable
runtime ownership, not persistence.

### D-12. The world thread remains the sole publisher of live chunks

Workers may produce immutable candidates. The world thread applies the latest
mutable state and publishes or rejects a candidate, preserving existing
single-owner invariants.

### D-13. Eviction does not normally write a full chunk file

The immutable base was written during world generation; mutable consequences
already have an authoritative layer. Eviction drops an unleased reconstruction,
avoiding disk writes and latency on ordinary camera movement. D-17's explicit
background-simulation checkpoint is the pressure exception, not the ordinary
eviction path.

### D-14. Zoom output is a generated artifact independent of detailed residency

The worldgen result includes reusable zoom data. Save loading restores that
separate artifact rather than regenerating it or loading detailed gameplay
chunks. Internal tiling or multiple resolutions may replace one unlimited GPU
atlas, but the map itself always has complete renderable coverage.

### D-15. Regeneration is a compatibility-checked fallback

Missing or corrupt base records may be regenerated when the stored generator
and content identity is compatible. The engine never silently applies old
gameplay deltas to a different regenerated foundation.

### D-16. The seven implementation stages are the epic's backbone

The arc must deliver: genuine bounded residency; leases before storage changes;
fluid ownership repair; eviction-safe location work; an immutable generated
foundation; asynchronous reads/generation with sole world-thread publication;
and metrics plus long-travel verification. The finer delivery slices below
preserve that dependency order.

### D-17. Hard pressure uses disk-backed hibernation where work is checkpointable

The current 200-chunk value is not a sufficient memory guarantee: it is a
per-page count, not bytes, and some loading paths bypass its eviction. The new
manager uses a global measured target and hard ceiling. It evicts unleased
reconstructible chunks first, then checkpoints eligible background simulation
to a disk-backed session store and releases those leases. Camera/foreground work
keeps a protected minimum; if the remaining indispensable footprint reaches the
hard ceiling, new work waits rather than risking process exhaustion.

Disk hibernation preserves the accepted gameplay state and the fact that work
remains. It does not make computation continue on disk. Only systems with an
explicit checkpoint/resume format may use this path; arbitrary in-flight unit
or edit transactions are not silently frozen.

The pressure decision uses configured budgets and deterministic size accounting,
not fluctuating operating-system “free memory.” Emergency diagnostics may warn
about actual process memory, but nondeterministic host pressure does not choose
which gameplay simulation advances next.

### D-18. Saves from one generated world share one world-library bundle

The immutable generated foundation is stored once and referenced by every save
descended from that world. Local deletion is reference-aware and offers cleanup
only after the last referencing save is gone. A future portable export may
embed or copy the bundle, but ordinary saves do not duplicate it.

### D-19. Save loading enters gameplay immediately and allows chunk pop-in

There is no separate loading transition after the save and bundle indexes have
validated. The game scene may initially show black wherever detailed chunks are
not ready. Fully composed chunks appear independently, center and visible first;
the renderer never shows an unpatched base chunk and then corrects it. Gameplay
requiring absent detail waits on residency rather than observing partial data.

### D-20. Hibernated fluid pauses exactly and resumes through a deterministic queue

Hibernation records the last accepted fluid cells, revision, pending-settlement
state, and deterministic queue position. It performs no elapsed-time
fast-forward and makes no random approximation when the chunk resumes. The next
calculation is the next logical fluid tick after the checkpoint.

The background scheduler uses a fixed logical work allowance and a stable order
derived from persisted queue tickets with canonical coordinates as a tie-break.
It does not use wall-clock timing, worker completion order, or current host free
memory. Every queued fluid chunk eventually receives work even if the camera
never returns. The rejected alternative—guessing what fluid did during elapsed
world time—would be substantially more complex and would weaken determinism.

### D-21. Crash recovery beyond the last save is out of scope

Manual saves and autosaves are the durability boundary. A disk hibernation file
is session backing so RAM can be released; it is not a write-ahead journal and
does not promise recovery after a process or machine crash. A normal save
includes every accepted active or hibernated fluid state needed to resume from
that save.

### D-22. The zoom map always renders and never depends on detailed chunks

The zoom map is one complete, separately generated world representation.
Opening or moving around it never requests, leases, or evicts detailed gameplay
chunks. For worlds too large for one GPU atlas, internal tiling or a
multi-resolution representation is allowed only behind an always-available
complete base layer: the map has no black residency holes. This scaling work
remains a separate late slice inside the arc because it shares the generated
world bundle, but it is independent of the detailed-cache critical path.

## Open questions

### Q-1. What happens when required leases exceed the memory budget?

Status: resolved by D-17 and D-20.

The design cannot promise both an absolute memory cap and unlimited simultaneous
camera, fluid, unit, and authoring leases. The chosen policy combines ordinary
eviction, disk-backed hibernation for explicitly checkpointable background
work, a protected foreground reserve, and final backpressure if indispensable
leases themselves reach the hard ceiling.

### Q-2. Who owns a generated-world bundle when several saves reference it?

Status: resolved by D-18.

One world-library bundle is shared. Ordinary saves reference it; last-reference
cleanup is explicit/reference-aware. Self-contained export remains a possible
later copying operation rather than the normal layout.

### Q-3. What exact latency is “near instant,” and may detail appear progressively?

Status: resolved by D-19 for behavior; numeric regression thresholds are set
from CRS-13 measurements.

The game enters its scene without a loading transition. Black background is
acceptable where detail is absent, and complete chunks may pop in. The center
and current view receive top priority; operations requiring missing detail wait.

### Q-4. Which fields belong in `BaseChunkV1`?

Status: deliberately open pending CRS-7 profiling.

At minimum it must skip enough expensive work to meet Q-3. Candidate fields are
columns, base terrain/surface, fluid, ice, flora placement, water table, and
magma. Neighbor-dependent slopes, side decoration, and render caches may be
cheaper and safer to derive after publication. Size and generation-time probes
should decide rather than serializing `LoadedChunk` wholesale. CRS-7 may select
a clearly dominant measured payload; if no candidate meets the load target or
the remaining tradeoff changes user-visible fidelity, bundle size materially,
or compatibility risk, it stops before freezing `BaseChunkV1` and asks for a
maintainer decision. CRS-8 cannot begin until this is resolved.

### Q-5. What shard size and compression should the bundle use?

Status: deliberately open pending CRS-7 prototype measurements.

The choice must balance random-read latency, compression ratio, checksum repair,
file-count overhead, and atomic publication. The contract requires indexed,
versioned, independently verifiable records; it does not yet choose a codec.
CRS-7 may choose the clearly dominant measured layout. If no option satisfies
the cold-read target and practical bundle size without a material operational
tradeoff, it stops before freezing the manifest and asks for a maintainer
decision. CRS-8 cannot begin until this is resolved.

### Q-6. Is explicit saving sufficient durability, or is crash recovery wanted?

Status: resolved by D-21.

Manual/autosave state is durable; progress after the last completed save may be
lost in a crash. The session hibernation store is not a write-ahead journal.
Automatic crash recovery was rejected as separate complexity outside this arc.

### Q-7. Must save capture active fluid mid-settle or force it to quiesce?

Status: resolved by D-7 and D-17.

The chosen behavior flushes an accepted current state, records
`needsSettlement`, and resumes after load. Forcing all fluid to equilibrium
was rejected because it could make saves unexpectedly slow and would advance
the simulation as a side effect.

### Q-8. Should paged zoom rendering ship inside this epic or as a dependent epic?

Status: resolved by D-22.

The phrase “paged zoom” did not mean tying the map to detailed chunks or letting
it develop holes. The zoom map remains a complete independent artifact that
always renders. Its internal large-world texture representation is kept as a
late independent slice because it shares bundle output, not cache residency.

### Q-9. How does game time treat hibernated fluid?

Status: resolved by D-20.

Disk preserves the current fluid cells and `needsSettlement`, but it does not
run the solver. Hibernated fluid pauses exactly and resumes through a fair,
deterministically ordered background queue. Elapsed-time fast-forward and random
approximation were rejected.

## Verification strategy

Verification is layered so each slice proves its contract without waiting for
the final stress probe.

- Pure manager tests cover canonical aliases, one in-flight request per key,
  lease reference counts, eligible eviction, LRU ordering, per-page attribution,
  count/byte pressure, stale epochs, and bounded pending queues.
- Integration coverage drives both camera loading and
  `world.loadChunksInRegion` beyond the budget and proves resident count/bytes
  return to target after leases release.
- A fluid test activates a chunk near the eviction frontier, moves the camera
  far enough to create pressure, proves the active footprint remains resident,
  awaits final writeback acknowledgement, then proves eviction and exact reload.
- A hard-pressure fluid test checkpoints a background chunk, proves its full
  detailed footprint leaves RAM while its accepted state and pending-work marker
  remain on disk, then proves D-20's exact pause and deterministic fair
  resumption.
- Fluid save/load coverage captures a still-active chunk, reloads it without
  restoring the old cache, and proves accepted cell state plus settlement
  resumption. Loading static ocean must not create a save delta.
- Unit/action coverage moves or constructs across a chunk boundary under
  pressure and proves the action either holds its footprint or receives an
  explicit admission failure—never a silent unloaded read.
- Location coverage stamps a multi-chunk footprint while the camera moves,
  proves every command is acknowledged before the marker, and proves a rejected
  attempt retries idempotently. Existing #1674/#1719 acceptance cases remain
  green.
- Bundle codec tests freeze `BaseChunkV1`, round-trip every field, reject bad
  versions/checksums, ignore partial publication, and validate index uniqueness
  under canonical wrapping.
- Rehydration tests compare `bundle base + deltas + derived repair` against
  `generateLoadedChunk + replayEdits` for representative chunks, seams, fluids,
  flora, magma, and locations without changing worldgen baselines.
- Save compatibility tests migrate a pre-bundle save, resolve or build its
  foundation, round-trip the new reference and fluid component, and reject an
  incompatible generator identity without partially publishing a world.
- Async tests delay and reorder worker results, delete or replace a page, and
  prove stale candidates never publish or notify the simulation.
- Load presentation coverage enters the game scene with no resident detail,
  renders missing regions as black, and proves only fully composed chunks pop in
  center-first; gameplay queries never observe a half-materialized chunk.
- Zoom tests prove the complete map renders with zero detailed chunks resident,
  opening and panning it creates no detailed requests, its base layer has no
  holes, and any internal high-resolution pages preserve seams, UV addressing,
  bounded GPU residency, and current pixel output at existing world sizes.
- A deterministic long-travel headless probe crosses many multiples of the
  resident budget, mixes camera demand with explicit regions and active work,
  revisits altered chunks, and asserts a plateau in count/estimated bytes plus
  bounded queues and preserved gameplay state.
- A performance probe records bundle size, worldgen write overhead, cold and
  warm chunk latency, save-load time, zoom-page upload time, and resident high
  water. Numeric gates are set only after baseline measurements on supported
  environments.
- Worldgen-output changes, if any slice causes them, follow the repository's
  full worldgen tier and save-version rules. Storage-only changes should prove
  output equivalence rather than recapture baselines casually.

## Delivery plan

Each slice is intended to fit one pull request. Issue processing may split a
slice if repository evidence proves it cannot be reviewed safely as one PR, but
must preserve the dependency order and update both this plan and the ledger.

### CRS-1. Centralize chunk demand and canonical chunk identity

- Introduce the residency owner in `WorldState`/a narrow manager boundary.
- Define canonical `ChunkKey`, request identity, in-flight deduplication, and
  page/generation epoch handling.
- Route camera, init queue, explicit-region, save warmup, cursor/debug, and other
  current generation entry points through one request API without changing
  eviction behavior yet.
- Add pure tests proving aliases cannot create duplicate resident or in-flight
  entries.
- **Depends on:** none.

### CRS-2. Enforce bounded residency across every chunk-loading path

- Move insertion and eviction eligibility into the manager.
- Apply the count ceiling to camera and explicit/init requests alike.
- Replace furthest-only selection with tested recency plus distance/priority
  tie-breaking for unleased entries.
- Bound pending requests so a huge region call cannot consume unbounded memory.
- Add initial resident/request counters.
- **Depends on:** CRS-1.

### CRS-3. Add scoped chunk leases and an explicit pressure policy

- Implement reference-counted leases with owner/reason diagnostics and bracketed
  release.
- Migrate camera minimum-residency and explicit operations to leases; leave
  prefetch unleased.
- Enforce D-17's target, protected foreground reserve, hard ceiling, and
  backpressure behavior; checkpointable disk spill lands in CRS-4A.
- Add per-entry estimated size and graduate the manager from count-only to byte
  accounting while retaining a count safety guard.
- **Depends on:** CRS-2.

### CRS-4. Make active fluid simulation eviction- and save-safe

- Acquire simulation footprint leases before activation.
- Reverse the unload relationship: eviction occurs only after simulation
  deactivation/final acknowledged writeback.
- Extend the existing per-chunk generation fence through final writeback and
  lease release.
- Add accepted fluid override/activity ownership independent of residency; save
  wiring completes in CRS-10.
- **Depends on:** CRS-3.

### CRS-4A. Add disk-backed hibernation and fair fluid resumption

- Define the checksummed session spill record for accepted fluid cells,
  revision, and `needsSettlement`; it is runtime backing, not automatically a
  committed user save or a promise of crash recovery.
- Under hard pressure, checkpoint eligible off-camera fluid, await world-thread
  acceptance, move it from ticking to hibernated, and release its detailed
  chunk footprint.
- Resume hibernated chunks through a bounded fair queue even if the camera does
  not revisit them, preserving and using D-20's deterministic queue ticket.
- Protect the foreground reserve and fall back to admission backpressure when no
  safe hibernation candidate exists.
- **Depends on:** CRS-4.

### CRS-5. Lease chunks used by unit work and authoritative world edits

- Give movement, path validation, interactions, construction, and other active
  unit work explicit small lease footprints.
- Hold edit residency from validation through authoritative commit or rejection.
- Add command identities/acknowledgements needed to reconcile staged
  read-your-writes values safely.
- Preserve compact off-cache authority for systems such as power instead of
  pinning their full topology.
- **Depends on:** CRS-3.

### CRS-6. Make location stamping transactional across its footprint

- Resolve a definition's canonical chunk footprint and lease it before
  materialization.
- Aggregate authoritative placement acknowledgements under one stamp identity.
- Mark geometry complete only after every required commit; reconcile failures
  and retain idempotent retry.
- Preserve independent content spawning.
- Reuse, prerequisite, or explicitly subsume open #1674 and #1719 after a
  freshness check during issue processing.
- **Depends on:** CRS-5.

### CRS-7. Define the versioned generated-world bundle and base-chunk record

- Resolve Q-4/Q-5 with focused size, cold-read, and generation-cost prototypes;
  stop for a maintainer decision before freezing either format if the documented
  measurement rules do not yield a clear choice.
- Freeze manifest, index, checksum, `BaseChunkV1`, and zoom-page DTO contracts.
- Define world/generator/content identity and compatibility rules.
- Implement atomic incomplete/complete publication and corruption detection in
  isolation from runtime loading.
- **Depends on:** none logically; scheduled here so residency semantics settle
  before storage is integrated.
- **Open questions:** Q-4 and Q-5, both resolved or explicitly stopped within
  this slice before any wire format freezes.

### CRS-8. Write base chunks and the reusable zoom artifact during world generation

- Stream every physical canonical base chunk into indexed bundle shards during
  finite-world creation without retaining all detailed chunks at once.
- Store the complete reusable zoom base plus any internal fine-detail data rather
  than discarding and recomputing them on save load.
- Verify the complete inventory and atomically publish the manifest.
- Measure generation time, peak memory, bundle size, and compression.
- Register the atomically published bundle in the shared world library under
  D-18's stable identity.
- **Depends on:** CRS-7.

### CRS-9. Materialize resident chunks from the bundle plus durable deltas

- Read and validate `BaseChunkV1`, convert to the live chunk, apply the latest
  ordered gameplay state and time-derived flora, then perform resident-neighbor
  repair before publication.
- Add bundle identity to the save component graph and migrate pre-bundle saves
  through the compatible-generation path.
- Reuse one registered bundle across descendant saves and make last-reference
  cleanup explicit rather than duplicating or deleting it accidentally.
- Define missing/corrupt/mismatched bundle diagnostics without partial world
  publication.
- Keep pure generation as a compatibility-checked fallback.
- **Depends on:** CRS-7, CRS-8, and the residency publish boundary from CRS-1.

### CRS-10. Replace resident-chunk fluid snapshots with sparse durable fluid state

- Introduce a versioned fluid-state component keyed by canonical chunk,
  carrying accepted override data, `needsSettlement`, and deterministic
  hibernation queue tickets where required.
- Stop `appendFluidSnapshot` from enumerating every resident chunk.
- Migrate existing ordered fluid snapshot edits exactly, preserving the latest
  per-chunk result whether that chunk was loaded at migration time or not.
- Make save barriers serialize accepted fluid authority rather than cache
  membership.
- **Depends on:** CRS-4 and CRS-9.

### CRS-11. Move chunk reads and fallback generation off the world thread

- Add bounded prioritized worker queues for read/checksum/decompress and pure
  fallback generation.
- Join duplicate demand and tag results with page/bundle/request epochs.
- Keep mutable delta application, derived repair, publication, and notifications
  on the world thread under a per-tick drain budget.
- Add camera-motion prefetch and cancellation/evictability behavior.
- Enter the game scene after index validation, render missing detail as black,
  and publish only fully composed chunks center/visible-first per D-19.
- **Depends on:** CRS-2 and CRS-9.

### CRS-12. Keep the complete zoom map available at large world sizes

- Load a complete separately generated zoom artifact from the world bundle;
  opening it never enters the detailed residency manager.
- Preserve an always-available complete base representation, including when no
  detailed chunks are resident.
- Replace one unsafe whole-world atlas, when needed, with internal
  texture-dimension-safe tiles or multiple resolutions; absent fine detail
  falls back to the complete base instead of a black hole.
- Retain live icon/discovery overlays and preserve current pixels and interaction
  coordinates at existing sizes.
- **Depends on:** CRS-8. It is independent of CRS-9 through CRS-11 and not on
  the detailed-cache critical path.

### CRS-13. Add long-travel, latency, pressure, and corruption gates

- Complete the metrics inventory and human-readable residency diagnostics.
- Add the deterministic mixed-workload long-travel probe and save/revisit
  oracle.
- Add bundle corruption/partial-publication recovery and performance probes.
- Establish documented latency and high-water baselines from measurements, then
  gate regressions at stable thresholds.
- Update the persistence inventory and relevant engine contracts to state the
  final ownership boundaries.
- **Depends on:** CRS-2, CRS-3, CRS-4, CRS-4A, CRS-5, CRS-6, CRS-7, CRS-8,
  CRS-9, CRS-10, CRS-11, and CRS-12.

## Source notes

- Primary cache and eviction: `src/World/Tile/Types.hs`.
- Camera and init/explicit loading: `src/World/Thread/ChunkLoading.hs` and
  `src/Engine/Scripting/Lua/API/WorldQuery/Chunk.hs`.
- Chunk shape and deterministic generation: `src/World/Chunk/Types.hs` and
  `src/World/Generate/Chunk.hs`.
- Fluid activity and writeback: `src/Sim/State/Types.hs`,
  `src/Sim/Fluid/Active.hs`, and `src/Sim/Thread.hs`.
- Current fluid save snapshot: `src/World/Thread/Command/Save/WriteWorld.hs`.
- Persistence classifications: `docs/persistence_state_inventory.md`.
- Flora reconstruction precedent: `src/World/Flora/Growth.hs` and
  `src/World/Flora/Harvest.hs`.
- Location materialization: `scripts/location_stamper.lua`,
  `scripts/locations.lua`, and `src/World/Thread/ChunkLoading.hs`.
- Zoom regeneration and atlas: `src/World/Load/Stage.hs`,
  `src/World/ZoomMap/Cache/BuildPixels.hs`, and
  `src/World/ZoomMap/ChunkTexture.hs`.
- Tracker overlap first checked and readiness-rechecked 2026-08-25: no competing
  epic; open #1674 and #1719 plus closed #1207 and #1596 are the relevant
  overlaps and precedents.
