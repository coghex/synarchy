# Texture asset telemetry design

This document designs a focused developer-observability arc for Synarchy's
texture system. It exists so Lua tooling can answer how much bindless capacity,
stable-handle space, asset-pool state, and estimated image memory a live session
is using, rather than inferring pressure from repository file counts or private
Haskell structures.

`docs/asset_system_findings.md` remains the defect report and
`docs/asset_pool_scaling_design.md` remains the broader architectural context.
The open correctness issues identified there stay independent: this arc exposes
and verifies their resource behavior, but does not re-file or absorb their
fixes.

Design state: `ready for issue processing`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [ ] EPIC. Expose texture capacity and residency telemetry through Lua
- [ ] ATEL-1. Establish the engine-side texture telemetry snapshot
- [ ] ATEL-2. Expose a versioned texture-statistics summary through Lua
- [ ] ATEL-3. Attribute resident slots and estimated bytes to texture origins
- [ ] ATEL-4. Expose bounded resident-texture detail to Lua
- [ ] ATEL-5. Add a real-GPU telemetry probe and representative baselines
- [ ] ATEL-6. Add capacity-pressure warnings after a budget is approved

## Epic contract

- **Goal:** Lua diagnostics can retrieve a stable, read-only snapshot of the
  live texture system's bindless descriptors, stable handles, managed assets,
  aliases, failures, provenance, and estimated memory.
- **Done when:** `engine.getTextureStats()` returns a versioned table in every
  boot profile; its live and high-water counts agree with tested allocator and
  pool transitions; bindless absence is explicit rather than an error; family
  and byte totals declare their accounting coverage; resource detail is
  bounded and paginated; the in-game shell and TCP console can inspect the
  result; and a
  real-GPU probe demonstrates expected deltas across upload, reuse, replacement,
  and release. Measured, maintainer-approved pressure thresholds emit bounded
  warnings without changing allocation or gameplay behavior.
- **Users and operators:** the maintainer growing the asset pool, developers
  diagnosing leaks or churn, Lua debug tooling, probes, and future in-game
  diagnostics.
- **Arc label:** None proposed.

## Current state and evidence

There is no engine-wide texture telemetry API today. The pieces exist, but a
caller must know several internal maps and their different meanings:

- `Engine.Graphics.Vulkan.Texture.Types.BindlessTextureSystem` contains the
  accepted descriptor capacity, slot allocator, handle-to-slot map, image-view
  map, and pinned-sampler map.
- `Engine.Graphics.Vulkan.Texture.Slot.TextureSlotAllocator` contains the next
  never-used slot and free-slot set. These are enough to derive current and
  high-water slot use without a new counter.
- `Engine.Asset.Types.AssetPool` contains the next monotonic texture handle,
  canonical file-texture resources, path cache, and loading/ready/failed handle
  states.
- `rvTextureSizeRef` contains handle-indexed dimensions, including aliases, so
  summing it directly would double-count shared images.
- `handleSlotTableSize` fixes the shader-visible handle namespace at 65,536
  entries, with handle 0 reserved.

The existing `blood.gpuStats()` is a narrow precedent. It reads
`textureSystemRef` and `textureSizeRef` from the Lua thread and returns a Lua
table containing engine-wide bindless and size-map counts plus a blood-owned
count. It exists for one lifecycle probe and neither explains capacity nor
distinguishes slots, mappings, aliases, managed resources, or memory.

`engine.getLoadedTexturePaths()` already reads the shared `AssetPool` from the
Lua thread. `docs/engineenv_capability_inventory.md` explicitly classifies the
Lua thread as a reader of `assetPoolRef`, `textureSystemRef`, and
`textureSizeRef`, all available through `RenderViewCapability`. The live summary
therefore needs no new `EngineEnv` field or capability merely to observe current
state.

The `engine` Lua table is installed by
`Engine.Scripting.Lua.API.Register.Engine`. The in-game shell copies that whole
table into its sandbox, and the TCP debug console sees the ordinary globals, so
an `engine` query is automatically reachable from both existing diagnostic
surfaces.

Some desired values are not reconstructible after the fact:

- registration attempts and failures by reason;
- cache hits versus actual uploads;
- release counts;
- structured origin for generated preview, zoom, blood, and face-map textures;
  and
- exact or estimated bytes for slots not represented in `AssetPool`.

Those require explicit telemetry updates or structured registration metadata.
They must not be guessed from log messages or ambiguous source strings.

A tracker search on 2026-08-26 found no open or closed asset-telemetry,
texture-statistics, descriptor-statistics, or texture-diagnostics epic. Closed
#1585 and its blood lifecycle probe are the closest read-only-query precedent.
Closed #1689, #1690, #1691, and #1699 delivered the correctness outcomes whose
resource behavior this telemetry observes. The readiness recheck on the same
date again found no competing epic; the only open epic returned by the broad
diagnostics searches was unrelated determinism-test epic #1374.

## Desired experience

From a Lua script, the in-game shell, or the TCP debug console:

```lua
local stats = engine.getTextureStats()

print(stats.descriptors.used, stats.descriptors.usable)
print(stats.handles.issued, stats.handles.remaining)
print(stats.assets.canonical, stats.handles.aliases)
print(stats.memory.estimatedResidentBytes)
```

The proposed version-1 result is one table with a fixed top-level shape:

```lua
{
  schemaVersion = 1,
  gpuAvailable = true,

  descriptors = {
    shaderCapacity = 16384,
    allocatedCapacity = 16384,
    reserved = 1,
    usable = 16383,
    used = 438,
    free = 15945,
    highWater = 512,
    pinnedSlots = 121
  },

  handles = {
    tableEntries = 65536,
    allocatable = 65535,
    issued = 1842,
    remaining = 63693,
    liveMappings = 693,
    distinctMappedSlots = 421,
    aliases = 272
  },

  assets = {
    canonical = 421,
    loading = 0,
    readyHandles = 689,
    failedHandles = 4,
    cachedPaths = 421
  },

  activity = {
    registrations = 447,
    registrationFailures = 0,
    cacheHits = 1395,
    uploads = 447,
    releases = 26
  },

  memory = {
    estimatedResidentBytes = 155189248,
    accountedSlots = 410,
    unaccountedSlots = 11
  },

  families = {
    { name = "units", slots = 132, handles = 190,
      estimatedBytes = 79691776 },
    { name = "ui", slots = 181, handles = 301,
      estimatedBytes = 25165824 }
  }
}
```

The numbers above are illustrative, not captured baselines. Lua field names are
lower camel case to match existing engine tables. Counts are pushed as Lua
integers. `families` is a deterministically ordered array rather than a table
whose dynamic keys and iteration order become part of the API.

In a boot profile without a bindless system, the function still returns the
same table shape with `gpuAvailable = false`, zero live descriptor counts, and
the CPU-side handle/asset values that exist. Absence of a GPU is not an error
and does not make the whole result `nil`.

## Scope

### In scope

- A typed engine-side texture telemetry snapshot.
- Current, free, usable, and high-water bindless slot counts.
- Stable-handle capacity, issued count, remaining count, live mappings,
  distinct mapped slots, and alias count.
- Asset-pool loading, ready, failed, canonical-resource, and cached-path counts.
- Since-boot activity counters for registrations, failures, cache hits, uploads,
  and releases.
- Structured texture origin sufficient for family aggregation.
- Estimated resident bytes with explicit accounted/unaccounted coverage.
- A versioned, read-only Lua summary query available through `engine`.
- A bounded, paginated detailed query for individual resident resources.
- Headless behavior, deterministic ordering, pure coverage, and a real-GPU
  behavior probe.
- Threshold warnings selected only after representative baselines and an
  approved supported-device budget.

### Out of scope

- Fixing descriptor, handle, publication, or shutdown correctness already owned
  by #1689, #1690, #1691, and #1699.
- Creating an umbrella issue that re-parents or duplicates those repairs.
- Remote analytics, network uploads, player tracking, or persistent usage
  history.
- Saving telemetry into game saves or player configuration.
- Letting gameplay behavior depend on device-specific resource counts.
- A new resource viewer UI, graph dashboard, or permanent HUD widget.
- Atlas construction, streaming, eviction, compression, or asset unloading.
- Pretending an RGBA payload estimate is exact Vulkan heap consumption.
- Font descriptor telemetry in version 1. Font atlases use their own descriptor
  pool and must be named separately if later included.

## Design

### Meaning of each count

The API distinguishes resource layers that currently get conflated:

| Field | Meaning |
|---|---|
| `descriptors.used` | Allocated non-reserved bindless slots, not handle mappings |
| `handles.liveMappings` | Stable handles currently resolving through the bindless map |
| `handles.distinctMappedSlots` | Unique slot indices referenced by those mappings |
| `handles.aliases` | `liveMappings - distinctMappedSlots`; extra handles sharing slots |
| `assets.canonical` | File-texture resource records in `apTextureAtlases` |
| `assets.readyHandles` | Ready pool-side handles, including aliases |
| `memory.estimatedResidentBytes` | Accounted decoded/upload payload, deduplicated by resident resource/slot |

The relationships are diagnostic. If slots remain flat while issued handles and
aliases rise, acquisition churn is the problem. If slots rise while handles are
stable, residency or atlasing is the likely concern. If bytes rise while slots
stay modest, image dimensions/encoding/residency deserve attention.

### Derivable live and high-water slot state

For the current allocator:

```text
usable slots       = tsaMaxSlots - reserved slots
currently used     = tsaNextSlot - reserved slots - size(tsaFreeSlots)
slot high-water    = tsaNextSlot - reserved slots
```

`tsaNextSlot` only advances when no reusable free slot exists, so its distance
from the reserved prefix records the greatest number of simultaneously minted
slots. Reusing a freed slot does not inflate the high-water mark.

The implementation must keep these formulas in one pure telemetry module with
boundary tests. Lua must not recompute them, and probes must not infer them from
map sizes.

### Stable-handle accounting

`apNextTextureHandle` is monotonic and handle 0 is reserved. The issued and
remaining values are therefore derivable without another counter. They measure
process-lifetime namespace consumption, not current residency. Release may
reduce live states and mappings but does not restore `remaining` under the
current architecture.

Alias count is derived from actual bindless mappings grouped by slot index,
rather than from `readyHandles - canonical`. The latter would misclassify
generated/transient resources and pool-only states.

### Snapshot ownership and threading

The main-render thread remains the only writer of the bindless system. Existing
Lua-side handle allocation and the main-render registration/release boundaries
publish typed telemetry events into one process-lifetime telemetry state owned
inside an existing asset/texture container, not a new top-level `EngineEnv`
field. Multi-writer updates use one atomic modification boundary.

Every update publishes one complete immutable `TextureStatsSnapshot` with a
monotonic `epoch`. The snapshot is an observational mirror of completed texture
events: it may be one event behind while a mutation is in flight, but its own
descriptor, handle, asset, activity, memory, and family subrecords all describe
the same published epoch. Lua reads exactly that one value through
`RenderViewCapability`; it never assembles one result from adjacent reads of
`assetPoolRef`, `textureSystemRef`, and `textureSizeRef`.

The query never queues GPU work, waits for device idle, mutates a counter, or
blocks until a future frame. Values can naturally change immediately after
return, but one returned table never mixes epochs. Registration, cache reuse,
upload, failure, and release behavior stays authoritative; telemetry
publication is observational and cannot change the resource outcome it records.

### Structured origin

File textures already carry a path; their first family can be derived from the
normalized `assets/textures/<family>/...` prefix. Generated resources need a
closed origin such as world preview, zoom atlas, blood decal, default face map,
or other explicitly labelled producer. Registration already carries a free-form
source string for diagnostics, but telemetry must not parse prose.

Each live slot receives one origin and byte estimate at successful
registration. Aliases reference the existing slot and do not add bytes.
Unregistration removes the live origin record after counter deltas are captured.
If a producer cannot yet report dimensions/format, its slot remains visible in
`unaccountedSlots`; it is never silently omitted from both sides of the memory
total.

### Memory estimates

Version 1 reports decoded/upload payload estimates, normally
`width * height * bytesPerPixel` plus explicitly known mip levels. It does not
claim to include driver allocation granularity, tiling overhead, staging
buffers, descriptor-pool storage, or heap fragmentation.

Every memory table includes accounted and unaccounted slot counts. A later
Vulkan memory-budget integration may add exact device heap budget/usage as a
separate field without redefining the payload estimate.

### Lua API compatibility

The summary is additive and versioned:

- `schemaVersion = 1` identifies the field contract.
- Existing version-1 fields retain meaning and type.
- New optional fields may be added without incrementing the version.
- Removing, renaming, changing type, changing units, or changing semantic scope
  requires a version bump.
- Counts use integers and bytes use integer byte units; human formatting stays
  in Lua tooling.
- No table relies on unspecified Lua map iteration order.

The function is observational and side-effect free. It has no reset verb:
high-water and activity counters describe the whole process since texture-system
initialization. Probes take before/after snapshots and compare deltas, following
the repaired blood lifecycle precedent rather than mutating the oracle.

### Detailed resident inventory

The summary remains bounded regardless of asset-pool size. The separate
`engine.listResidentTextures(options)` returns a bounded deterministic page
with fields such as origin, normalized path or generated label, canonical
handle, slot, alias count, dimensions, estimated bytes, pinned status, and
state.

The options would provide a required `limit` capped by the engine, an opaque or
stable numeric continuation, and optional family/origin filters. A full
unbounded resource array is not embedded in `getTextureStats()`.

### Existing issue relationship

- #1689 must settle what `allocatedCapacity` means for every accepted device
  before a capacity baseline becomes authoritative.
- #1690 and #1699 must settle the failure outcomes whose counters and probe
  transitions telemetry names.
- #1691 supplies the orderly-release event that shutdown telemetry can verify.
- #1705 validates authored direct paths but is not a runtime telemetry
  dependency.

The telemetry slices may be designed and partially implemented alongside these
issues. The real-GPU acceptance baseline must use their final behavior rather
than enshrining current false-success or missing-shutdown transitions.

## Decisions

### D-1. Texture capacity and residency telemetry is available through Lua

The maintainer must be able to retrieve the live data from the Lua API, not only
from Haskell logs, a debugger, or an external Vulkan tool. This makes the same
snapshot available to scripts, the in-game shell, the TCP console, and probes.

### D-2. Existing correctness issues remain independent

The telemetry arc does not duplicate or absorb the already-filed descriptor,
handle, false-publication, shutdown, or path-validation issues. It consumes
their eventual contracts and makes them measurable.

### D-3. Lua receives a separate bounded resident-resource query

The summary remains small and stable, while
`engine.listResidentTextures(options)` exposes individual resident resources
through deterministic bounded pages. It never embeds an unbounded resource
array in `engine.getTextureStats()` and never provides mutation or unload
control.

### D-4. Every Lua result comes from one coherent published epoch

The engine publishes one complete immutable snapshot after texture-system
events. A Lua query reads that snapshot once rather than joining several live
maps that can change between reads. This gives probes and future budget checks
an internally consistent result without blocking on the render thread.

### D-5. The arc includes measured capacity-pressure warnings

Warnings are part of the completed telemetry capability, but their thresholds
are selected only after representative real-device baselines and the final
supported-device capacity contract are known. They are bounded and rate-limited
and never trigger eviction, reject content, or affect gameplay.

### D-6. The stable summary API is `engine.getTextureStats()`

The `engine` namespace already owns FPS, video configuration, texture loading,
texture size, handle lookup, and loaded-path diagnostics, and it is available in
the in-game shell sandbox. The versioned summary uses
`engine.getTextureStats()` rather than adding a new global or hiding the query
under the test-oriented `debug` table.

### D-7. Texture telemetry is diagnostic state, never simulation state

Scripts may display, log, probe, and warn from telemetry. Simulation decisions,
save data, and deterministic game behavior do not branch on device-, timing-,
or session-dependent resource values.

### D-8. Version 1 measures the bindless texture system

Font atlases use a separate descriptor pool, so folding them into one generic
descriptor count would make the result ambiguous. Version 1 names bindless
capacity and residency precisely; later font-descriptor or Vulkan-heap sections
are additive and separately scoped.

## Proposals

### P-1. Use `engine.getTextureStats()` for the stable summary

Status: accepted by D-6.

The `engine` namespace already owns FPS, video configuration, texture loading,
texture size, handle lookup, and loaded-path diagnostics. It is also copied into
the in-game shell sandbox. A read-only texture-system summary fits this
namespace better than the test-oriented `debug` table.

### P-2. Treat the API as diagnostics, not gameplay state

Status: accepted by D-7.

The values are device-, timing-, and session-dependent. Scripts may display,
log, test, or alert on them, but simulation behavior and save data do not branch
on them.

### P-3. Keep version 1 bindless-focused

Status: accepted by D-8.

Font atlases use a different descriptor pool and would make generic
"descriptor" totals ambiguous. Version 1 names the bindless texture system
precisely; a later font or Vulkan heap section is additive and separately
scoped.

### P-4. Establish baselines before warnings

Status: accepted by D-5.

The first real-GPU probe records representative values and checks accounting
invariants. Warning thresholds are selected only after the supported-device
capacity and normal high-water behavior are known.

## Open questions

### Q-1. Does Lua need individual resident-resource records?

Status: resolved by D-3.

Yes. Leak diagnosis and future resource inspection require paths, slots,
dimensions, and alias counts per resource. They are exposed through a separate
bounded/paginated `engine.listResidentTextures(options)` call, never an
unbounded array inside the summary.

### Q-2. Must one query represent a single coherent publication epoch?

Status: resolved by D-4.

Yes. Adjacent atomic reads are individually safe but can produce a mixed result
when registration lands between them. One explicitly published immutable epoch
provides stronger probe and budget semantics without making the query block on
the render thread.

### Q-3. Is the epic observational only, or should it warn on pressure?

Status: resolved by D-5.

It warns on pressure after measurements establish the supported budget. A
premature percentage would encode the development GPU rather than the supported
minimum, so ATEL-6 remains last and consumes ATEL-5's baseline evidence.

## Verification strategy

- Pure tests pin empty, partial, full, freed, and reused slot allocator
  snapshots; the slot high-water formula; handle capacity boundaries; alias
  grouping by slot; deterministic family ordering; and accounted versus
  unaccounted byte totals.
- Bare-Lua headless tests pin the exact version-1 table shape, field types,
  integer units, `gpuAvailable = false`, and absence of stack leaks.
- Synthetic successful and failed transitions pin since-boot counters without a
  Vulkan device where possible.
- A manual/offscreen probe invokes `engine.getTextureStats()` through the TCP
  console before and after controlled ordinary upload, same-path reuse, pinned
  upload, transient replacement, and release. It asserts deltas and accounting
  invariants rather than hard-coding one machine's absolute counts.
- Representative captures record normal boot, world load, unit-heavy view,
  broad UI use, preview churn, save/load replacement, and orderly shutdown.
  These inform budgets but are not cross-machine golden numbers.
- Existing focused gates remain authoritative for the behavior being observed:
  #1690/#1699 headless publication boundaries, #1281 release coverage, and
  relevant offscreen probes.

## Delivery plan

### ATEL-1. Establish the engine-side texture telemetry snapshot

- **Outcome:** one typed, pure-queryable snapshot reports live descriptor,
  handle, alias, asset-state, and since-boot activity totals with documented
  formulas and headless behavior.
- **Scope:** telemetry types/module; allocator and pool aggregation; atomic
  activity counters at existing mutation boundaries; no new top-level
  `EngineEnv` field; focused pure tests.
- **Phase:** 1 — accounting foundation.
- **Depends on:** `none`.
- **Ordering:** `critical path`.
- **Relevant decisions:** D-1, D-2, D-4, D-7, D-8.
- **Acceptance signals:** allocator transitions produce exact used/free/high-
  water values; aliases are grouped by slot; handle capacity is correct at its
  boundary; absent bindless state produces a total snapshot.
- **Out of scope:** Lua marshalling, origin attribution, thresholds, or changing
  any asset outcome.
- **Open questions:** None.

### ATEL-2. Expose a versioned texture-statistics summary through Lua

- **Outcome:** `engine.getTextureStats()` returns the stable version-1 summary
  to ordinary scripts, the in-game shell, and the TCP console.
- **Scope:** `Engine.Scripting.Lua.API.Graphics` or a cohesive telemetry module,
  `Register.Engine`, table marshalling, schema documentation, and headless Lua
  tests.
- **Phase:** 2 — public diagnostic API.
- **Depends on:** ATEL-1.
- **Ordering:** `critical path`.
- **Relevant decisions:** D-1, D-2, D-4, D-6, D-7, D-8.
- **Acceptance signals:** the exact table shape and types are pinned; the call is
  read-only; headless returns `gpuAvailable = false`; shell JSON serialization
  succeeds.
- **Out of scope:** per-resource rows, UI presentation, or gameplay use.
- **Open questions:** None.

### ATEL-3. Attribute resident slots and estimated bytes to texture origins

- **Outcome:** the Lua summary includes deterministic family/origin aggregates
  and honest estimated-byte coverage across file and generated bindless
  textures.
- **Scope:** structured registration origin, live slot metadata, deduplicated
  byte estimates, family aggregation, accounted/unaccounted counts, and all
  current registration producers.
- **Phase:** 3 — attribution.
- **Depends on:** ATEL-1, ATEL-2.
- **Ordering:** `critical path` for capacity planning.
- **Relevant decisions:** D-1, D-2, D-4, D-8.
- **Acceptance signals:** aliases add handles but no bytes or slots; generated
  resources have explicit origins; aggregate slots equal accounted plus
  unaccounted; family order is deterministic.
- **Out of scope:** Vulkan heap-budget accuracy, font descriptors, or changing
  producer lifetimes.
- **Open questions:** None.

### ATEL-4. Expose bounded resident-texture detail to Lua

- **Outcome:** Lua can inspect individual live resources without embedding an
  unbounded inventory in the summary.
- **Scope:** a deterministic bounded/paginated query, origin/family filters,
  per-resource identifiers and dimensions, and query validation.
- **Phase:** 4 — diagnostic detail.
- **Depends on:** ATEL-3.
- **Ordering:** `not on the critical path`.
- **Relevant decisions:** D-1, D-3, D-4, D-6, D-7, D-8.
- **Acceptance signals:** every page respects the hard result cap; continuation
  cannot skip or duplicate unchanged records; bad options return a documented
  terminal result; the query mutates nothing.
- **Out of scope:** resource mutation, unload controls, thumbnails, or a UI.
- **Open questions:** None.

### ATEL-5. Add a real-GPU telemetry probe and representative baselines

- **Outcome:** a controlled offscreen run proves the Lua numbers follow upload,
  cache reuse, aliasing, replacement, failure, and release, and records scenario
  baselines for capacity planning.
- **Scope:** one focused probe, status registration in `ci_probes.py`, probe
  self-test updates where required, and a concise operator recipe for collecting
  representative snapshots.
- **Phase:** 5 — integration evidence.
- **Depends on:** ATEL-2, ATEL-3; final expected failure/release transitions use
  the landed behavior of closed #1690, #1691, and #1699.
- **Ordering:** `critical path` before warnings.
- **Relevant decisions:** D-1, D-2, D-4, D-6, D-7, D-8.
- **Acceptance signals:** same-path reuse does not add a slot or bytes; a real
  upload does; replacement and release remove the right live accounting;
  snapshot invariants hold throughout; the probe reports actionable deltas on
  failure.
- **Out of scope:** machine-independent absolute golden counts or a full probe
  sweep.
- **Open questions:** None.

### ATEL-6. Add capacity-pressure warnings after a budget is approved

- **Outcome:** long sessions emit bounded, rate-limited warnings before a
  supported resource limit becomes operationally dangerous.
- **Scope:** approved descriptor and handle thresholds, high-water crossing
  detection, rate limiting, one summary payload per crossing, and tests.
- **Phase:** 6 — policy.
- **Depends on:** ATEL-5 and the landed capacity contract from closed #1689.
- **Ordering:** `not on the critical path` until a budget is approved.
- **Relevant decisions:** D-1, D-2, D-5, D-6, D-7, D-8.
- **Acceptance signals:** warnings fire once per threshold epoch, name current/
  limit/high-water values, remain quiet below threshold, and do not affect
  allocation outcomes.
- **Out of scope:** automatic eviction, content rejection, limit changes, or
  remote reporting.
- **Open questions:** None.
