# Asset-pool scaling design

This document records the architectural direction for growing Synarchy's
texture corpus without silently exhausting bindless descriptors, stable texture
handles, GPU memory, or lifecycle bookkeeping. It builds on
`docs/asset_system_findings.md`: that report owns the verified defects and
content findings, while this document owns the intended capacity model,
component boundaries, decisions, open questions, and dependency-ordered
delivery plan.

Design state: `exploring`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [ ] EPIC. Make the texture asset pool safe to grow beyond the current corpus
- [x] APS-1. Make bindless slot-registration failure a truthful terminal outcome — [#1690]
- [x] APS-2. Refuse stable texture handles the shader cannot represent — [#1699]
- [x] APS-3. Make accepted-device descriptor capacity match the shader contract — [#1689]
- [x] APS-4. Drain ordinary texture resources during orderly shutdown — [#1691]
- [x] APS-5. Add descriptor, handle, residency, alias, and memory observability — [no-issue]
- [ ] APS-6. Replace path-only texture identity with a policy-aware asset key
- [ ] APS-7. Separate canonical texture identity from asynchronous load requests
- [ ] APS-8. Add explicit lifetime classes and owner-scoped release
- [x] APS-9. Remove preview's abandoned-request handle growth — [no-issue]
- [x] APS-10. Make all direct texture references a blocking validated inventory — [#1705]
- [ ] APS-11. Generate a capacity-aware asset catalog and budget report
- [ ] APS-12. Pilot another atlas family only if measurements justify it
- [ ] APS-13. Add bounded lazy residency or eviction only if measured pressure remains

## Epic contract

- **Goal:** Authors can substantially increase the texture corpus while the
  engine has explicit, observable, and truthful limits for bindless slots,
  stable handles, GPU memory, and asset lifetime.
- **Done when:** a supported graphical device cannot be accepted with an
  invalid descriptor layout; exhausted slots or handles never produce a loaded
  callback; all resident ordinary textures have a deliberate shutdown path;
  repeated acquisitions do not consume unbounded stable-handle ids; texture
  identity includes GPU sampling policy; debug output reports current and
  high-water resource use; the authored catalog is validated; and any further
  atlasing or residency policy is selected from measured pressure rather than
  file count alone.
- **Primary concern:** descriptor headroom before a large asset expansion.
- **Tracker overlap:** closed #1689, #1690, #1691, #1699, and #1705 delivered
  five foundation slices and are linked in the processing ledger. No matching
  umbrella epic was found in the fresh tracker search on 2026-08-26. The
  focused telemetry work is owned by `docs/asset_telemetry_design.md`; later
  issue processing must not duplicate either that arc or the closed issues.
- **Arc label:** None proposed.

## The short version

Synarchy already uses atlases, but selectively:

1. Unit animation source frames are compiled ahead of runtime into one PNG
   sheet per animation. Those sheets use one bindless slot each.
2. Font glyphs are packed into runtime-generated font atlases. They use the
   font renderer's dedicated descriptors, not bindless texture slots.
3. The world zoom map is assembled at runtime into one generated texture atlas
   and uses one bindless slot for the current generation.

There is no generic atlas builder for ordinary UI, item, vegetation, flora,
building, icon, or world textures. Each distinct resident ordinary image and
sampling policy normally consumes one bindless slot. The confusing part is
that `Engine.Asset.Types.TextureAtlas` is also the record used for a plain
single-image texture; for those records `amSubTextures` is empty. The type name
does not mean that the file was packed with other images.

The existing bindless design should be hardened, not replaced. The important
architectural change before a large expansion is to stop treating every load
request as a new texture identity. A canonical policy-aware asset identity,
separate request/subscriber identity, and explicit ownership boundary will
protect both the 16,384-slot descriptor space and the separate 65,536-entry
stable-handle table.

## Current system

### Loading path

```text
YAML / Lua / engine subsystem
          |
          | path + requested sampling policy
          v
new monotonic TextureHandle + AssetLoading
          |
          v
Lua-to-engine texture request queue
          |
          v
PNG decode -> Vulkan image/view -> bindless slot registration
          |
          +--> AssetPool resource + path cache + dimensions
          |
          +--> handle-to-slot shader table
          |
          +--> LuaAssetLoaded callback
```

`TextureHandle` is the stable number embedded in render data. The bindless slot
is recyclable and lives behind the handle-to-slot table. That indirection is a
good foundation: a resource can move or disappear without rewriting every
cached vertex that refers to it.

The current acquisition path nevertheless allocates a fresh monotonic handle
before checking the path cache. A cache hit shares the existing image and
bindless slot but creates another handle alias, another handle-state entry,
another size-map entry, and another reference-count increment. Slots and
handles therefore have different pressure models.

### Capacity model

| Resource | Current fixed bound | What consumes it | Reclaimed today |
|---|---:|---|---|
| Bindless image/sampler slots | 16,384 shader entries, with actual device handling subject to #1689 | One unique resident image plus sampling policy; transient preview, zoom, and decal images also register | Individual release can return a slot; ordinary production ownership rarely reaches it |
| Stable texture-handle table | 65,536 entries including handle 0 | Every handle allocation, including aliases and abandoned preview requests | Never recycled during the process |
| GPU image memory | Device-dependent | Decoded RGBA images, atlas padding, generated zoom data, and other GPU resources | Ordinary file textures currently survive until process exit because shutdown does not call the existing drain |
| Asset-pool maps | Memory-dependent | Canonical resources plus every live handle alias and size/state entry | Only through currently unwired release paths |

The descriptor count does **not** equal the number of PNGs in the repository.
It is closer to the number of unique images and sampler policies resident in a
particular session. Repeated same-policy loads of one path normally share one
slot, although they still spend handle ids. Conversely, the same path loaded
once with the ordinary global sampler and once pinned to nearest cannot safely
share a slot and may occupy two.

### Atlas inventory

#### Unit animation atlases

`tools/pack_atlas.py --compile` deterministically produces one atlas PNG per
animation and one `atlas/index.json` per unit. It does this as an authoring/build
step; the game does not pack these source frames during startup.

Each authored direction is a row and each frame is a column. The generated
index records cell size, row, real frame count, UV geometry, mirroring, timing,
and content digests. At runtime `Unit.Atlas` loads one texture handle for the
whole animation and renders a frame by changing its UV rectangle. The atlas
follows the player-selected nearest/linear sampler; each logical cell's
one-texel extrusion ring prevents linear filtering from reaching adjacent
cells.

The tracked corpus currently contains:

- 4,620 unit animation source-frame PNGs;
- 116 compiled per-animation atlas PNGs; and
- seven generated unit atlas indexes.

Thus this family needs 116 bindless descriptors for its animation sheets rather
than as many as 4,620 if every source frame were independently resident: about a
40-to-1 reduction for that source set. Default and directional T-pose sprites
remain ordinary whole-image textures. Atlasing reduces descriptor count and
draw-state variety; it does not automatically reduce decoded memory, because
the sheets are still RGBA images and can include transparent padding.

#### Font atlases

`Engine.Graphics.Font.Atlas` and `Engine.Graphics.Font.SDF` rasterize many
glyphs into a grid texture at runtime. Text drawing changes glyph UVs within
that image. These atlases substantially reduce font-image and descriptor count,
but `Engine.Graphics.Font.Upload` owns a dedicated font descriptor pool, so
they are not consumers of the bindless texture array discussed above.

#### World zoom atlas

`World.ZoomMap.ChunkTexture` assembles every chunk's small RGBA tile into one
row-major image at world initialization or load. That current generation uses
one bindless slot and is replaced as a transient resource. Its risk is the
opposite of the unit family: as world size grows, the single image eventually
hits memory and maximum-image-dimension limits. The separate
`docs/chunk_residency_streaming_design.md` already owns the future internally
tiled or multi-resolution zoom representation. This arc must not duplicate or
couple that work to detailed chunk residency.

#### Ordinary textures

UI, items, vegetation, flora, buildings, icons, structures, and most world
textures are not automatically packed. A loaded ordinary PNG becomes a single
Vulkan image, a `TextureAtlas` record with no subtextures, and one bindless
slot. The current tracked PNG counts outside the unit tree include 221 world,
183 UI, 92 item, 90 flora, 77 vegetation, 67 icon, 66 building, 54 facemap, 16
structure, and three utility files. File count is useful authoring context, but
only residency telemetry can show how many are simultaneously consuming slots.

## Verified risks

The underlying correctness defects are recorded with full evidence in
`docs/asset_system_findings.md`. The scaling-relevant set is:

- Slot exhaustion previously could publish `AssetReady`, cache the path, and
  send `LuaAssetLoaded` even though the handle had no descriptor mapping.
  Closed #1690 delivered the truthful failure transition.
- A monotonically allocated handle beyond the shader's 65,536-entry table was
  not representable while allocation could continue. Closed #1699 delivered
  the refusal boundary on top of #1690's failure model.
- An accepted device could receive fewer descriptors than the fixed shader
  array declared. Closed #1689 delivered the device capability/layout
  contract.
- Ordinary texture cleanup closures existed without an orderly-shutdown pool
  drain. Closed #1691 delivered that wiring.
- Repeated preview races reuse the image and slot but leak handle aliases and
  refcounts. ASSET-3 deliberately has no dedicated issue; its regression case
  belongs in APS-7/APS-8 rather than a tactical preview issue.
- The all-reference texture checker was red on comment prose and absent from
  CI. Closed #1705 repaired and wired it as a blocking gate.

These are reasons to harden capacity behavior before adding thousands of new
assets. They are not reasons to abandon bindless rendering.

## Design principles

### D-1. Preserve stable handle to recyclable slot indirection

The renderer continues to store stable `TextureHandle` values in long-lived
render data and resolve them through the shader table. Direct slot ids do not
escape as resource identity, and slot recycling never requires rewriting all
cached geometry.

### D-2. Capacity failure is a normal terminal asset outcome

Every request ends in an observable success or failure. A failed registration
publishes no ready resource, poisons no cache, destroys prepared GPU objects,
and does not replace a still-live transient generation. Lua waiters must be able
to finish without treating failure as success or hanging forever.

### D-3. Correctness and telemetry precede content-driven optimization

The false-success boundaries, supported-device contract, and shutdown drain
land before a large asset expansion. Capacity telemetry then establishes the
actual boot, gameplay, preview, and long-session high-water marks. Atlas or
streaming work is selected from those measurements.

### D-4. Texture identity includes GPU interpretation policy

A path alone is not a complete cache key. At minimum, the key includes a
normalized resource-root-relative path and sampler class (`global` or
`pinned-nearest`). The type leaves room for color space, mip policy, storage
format, or other upload decisions if those become real distinctions. Callers
do not reproduce ad hoc path-plus-flag comparisons.

### D-5. Authored source and generated runtime artifact remain separate

Editable source frames remain authoritative. Generated atlases and indexes are
deterministic, reproducible derivatives with validation and content digests.
The engine consumes generated artifacts; it does not rewrite source art or pack
large content families during normal startup.

### D-6. Atlasing is selective, not universal

An atlas family is appropriate when many small textures share sampling, color,
lifetime, and update policy and all consumers can carry a UV sub-rectangle.
Per-animation unit sheets satisfy that contract. A universal mega-atlas does
not: it couples unrelated lifetimes, increases rebuild blast radius and
padding, complicates filtering/gutters, and eventually reaches image dimension
limits.

### D-7. Texture handles are not unload authority

Lua and gameplay code do not receive a raw `unloadAsset AssetId` escape hatch.
Release follows explicit ownership scopes or leases so one caller cannot
invalidate a shared canonical texture still used elsewhere. Shutdown remains a
complete final drain.

### D-8. The zoom-map scaling arc remains separate

This design observes zoom-atlas descriptor and memory use, but internal zoom
tiling/multi-resolution belongs to the chunk-residency design. The asset pool
provides safe registration and lifetime primitives without making the zoom map
depend on detailed chunk residency.

## Proposed architecture

```text
                  AssetKey
       path + sampling/upload interpretation
                         |
          +--------------+--------------+
          |                             |
   canonical resident map         in-flight load map
   AssetKey -> TextureResource     AssetKey -> pending subscribers
          |                             |
          +--------------+--------------+
                         |
               one canonical TextureHandle
                         |
                  handle -> slot
                         |
              bindless descriptor image

Owner scopes / leases reference AssetKey or canonical resource identity.
LoadRequestId/subscriber identity reports async completion but is not sampled
by shaders and therefore does not consume the stable handle namespace.
```

### Canonical resource record

The replacement for the misleading generic `TextureAtlas` role should make
the one-image case explicit. Exact names remain an implementation choice, but
the resource must own:

- canonical `AssetKey` and stable `TextureHandle`;
- image, view, memory, sampler-class, bindless slot, dimensions, and format;
- current state (`Loading`, `Ready`, or `Failed` with retry policy);
- owners/leases or an equivalent explicit lifetime count;
- cleanup action and diagnostic byte estimate; and
- optional subtexture metadata only for a resource that actually has it.

Renaming can be staged separately from behavioral work. No change should be
made merely to make the type vocabulary prettier while capacity defects remain.

### In-flight deduplication and request completion

The first request for an `AssetKey` creates one canonical handle and one upload.
Concurrent requests attach subscribers to that in-flight state instead of
allocating alias handles. A ready request reuses the same canonical handle. A
failure completes every subscriber and leaves an explicit retry policy rather
than a success-looking cache entry.

This requires separating the thing the shader samples (`TextureHandle`) from
the thing an asynchronous caller waits on (`LoadRequestId` or an internal
subscriber token). The public Lua shape can remain compatible during migration
by continuing to announce handle/path completion, but the pool cannot use a
fresh handle as its only request correlation mechanism forever.

### Lifetime classes

Every acquisition declares one of three broad lifetimes:

1. **Process/static:** boot catalogs and evergreen UI/world art. Kept resident
   deliberately and drained at orderly shutdown.
2. **Session/owner scoped:** preview pages, optional screens, world/session
   content, or future packs. Released when their named owner is torn down; a
   shared resource remains resident while any owner still holds it.
3. **Transient generation:** preview, zoom, decals, and similar resources whose
   producer already owns replacement ordering. These continue to use explicit
   generation-safe replacement and disposal rather than entering a blind LRU.

An owner can be a narrow opaque id or domain-scoped lease set. It is not an
`EngineEnv` field by default; the established capability procedure decides its
home only when a concrete owner needs cross-module access.

### Observability

A device-free snapshot plus a graphical runtime query/log should report:

- shader-declared descriptor capacity, accepted device capacity, and reserved
  slots;
- used, free, and high-water bindless slots, including pinned and transient
  counts where available;
- highest allocated stable handle, representable handles remaining, and
  allocation high-water mark;
- canonical resources, in-flight loads, failed loads, owners/leases, and alias
  count during migration;
- estimated resident image bytes by family/lifetime and largest images; and
- capacity failure counts and the paths/policies involved, rate-limited.

The snapshot must be queryable without scraping logs so headless tests can pin
counter transitions. A concise boot/shutdown summary is useful, but per-frame
logging is not.

### Capacity-aware authored inventory

The existing unit compiler remains its family's authority. The repaired direct
reference checker becomes a blocking base gate. A generated read-only catalog
then reports, by family, source images, generated artifacts, dimensions,
decoded RGBA byte estimates, sampler/lifetime declaration, and whether the
runtime loads the family eagerly, conditionally, or transiently.

Repository file counts must not be presented as live descriptor use. The
catalog supplies an expected static upper bound; runtime telemetry supplies
actual residency and churn. Both are needed before selecting another atlas
family.

### Choosing another atlas family

The first candidate is selected by measured simultaneous descriptor pressure,
not total files on disk. A suitable family has:

- many small images resident together;
- identical sampler, mip, color-space, and format requirements;
- a common lifetime and rebuild boundary;
- consumers that can cleanly accept UV rectangles and frame dimensions; and
- deterministic pack inputs with a strict freshness/inventory gate.

Building animation frames may resemble the proven unit-animation model, while
static UI/icons may offer a compact shared-lifetime sheet. Neither is selected
yet: the current building family is small, and UI/icon migration has broader
consumer and filtering consequences. Flora and vegetation should not be packed
merely because they contain many files; their world rendering, variant, and
lifetime behavior must first satisfy the same contract.

### Residency and eviction

A fully general texture streamer or LRU is not a prerequisite for the planned
asset expansion. It is added only if telemetry shows that process/static plus
owner-scoped release cannot preserve the agreed headroom. If needed, eviction
operates on canonical resource identity, never raw handles, and cannot recycle
a stable handle id while cached geometry may still contain it. Missing or
evicted handles continue to resolve through the undefined texture until a
deliberate reacquisition completes.

## Scope

### In scope

- Truthful descriptor-slot and handle-table exhaustion.
- Supported-device descriptor-layout correctness.
- Orderly shutdown release of ordinary textures.
- Policy-aware resource identity and in-flight deduplication.
- Separation of canonical texture handles from request completion identity.
- Explicit process, owner-scoped, and transient lifetimes.
- Capacity and residency observability.
- Direct-reference validation and a generated capacity catalog.
- One measurement-selected atlas pilot, if justified.
- A bounded residency/eviction follow-up only if justified by agreed thresholds.

### Out of scope

- Replacing bindless rendering or exposing raw descriptor slots to callers.
- One global mega-atlas for unrelated content.
- Dynamically repacking authored textures during ordinary game startup.
- Treating file checksums as semantic deduplication. Identical current pixels
  may intentionally represent different authored concepts and future variants.
- Texture compression/KTX2 unless separately designed; it addresses storage
  and memory more directly than descriptor count.
- Zoom-map internal tiling, owned by
  `docs/chunk_residency_streaming_design.md`.
- Repairing the visual-content findings ASSET-5 through ASSET-9; those remain
  separately processable report concerns.
- Increasing fixed limits as a substitute for truthful failure, lifetime, and
  measurement.

## Open questions

### Q-1. Which supported device defines the capacity target?

The code has a 16,384-entry shader array, but #1689 shows that accepted-device
handling does not currently express one valid rule. The design needs either a
named minimum supported GPU/capability or an explicit decision to require the
full fixed array on every accepted device. Runtime headroom should be measured
against that supported minimum, not only the development GPU.

### Q-2. What operating headroom should gate a large asset expansion?

The diagnostics can report exact counts immediately, but a warning/budget gate
needs a policy: for example, a maximum percentage of the supported device's
usable slots and handle table after a representative boot and long preview/
gameplay run. No percentage is selected yet because no representative runtime
high-water capture exists.

### Q-3. May `engine.loadTexture` converge on one canonical handle per `AssetKey`?

This is the cleanest way to stop aliases spending the finite stable-handle
namespace. It may require an internal `LoadRequestId` or subscriber registry so
multiple asynchronous callers can receive completion without receiving unique
shader handles. The alternative is to retain alias handles and add recycling,
which is harder because cached vertices can outlive an apparent release. The
preferred proposal is canonical handles plus separate request identity, subject
to a compatibility audit of Lua callbacks and texture-name registration.

### Q-4. Which first owner scopes are worth supporting?

The minimum useful set appears to be process/static, preview-session, and
world/session ownership. UI-screen-level ownership may be useful later but can
create churn and complexity without saving meaningful residency. The first
implementation should name only lifecycle boundaries with real teardown events.

## Delivery plan

### APS-1. Make bindless slot-registration failure a truthful terminal outcome

- Delivered by closed #1690.
- Gate every ready/cache/size/callback publication on successful registration.
- Publish the chosen terminal failure to all waiters and release prepared GPU
  objects; preserve the previous transient generation.
- Add the issue's device-free `bindless registration failure` coverage.
- **Depends on:** nothing.

### APS-2. Refuse stable texture handles the shader cannot represent

- Delivered by closed #1699.
- Refuse the first id outside `handleSlotTableSize` at one allocation or
  registration boundary and reuse APS-1's failure representation.
- Preserve the last representable id and all below-cap behavior.
- **Depends on:** APS-1.

### APS-3. Make accepted-device descriptor capacity match the shader contract

- Delivered by closed #1689.
- Choose and implement one valid fixed-array or runtime-array contract without
  `VARIABLE_DESCRIPTOR_COUNT`, preserving the MoltenVK constraint.
- Make physical-device selection and texture-system construction agree.
- **Depends on:** nothing; may proceed alongside APS-1/APS-2, but must be done
  before any capacity target is declared trustworthy.

### APS-4. Drain ordinary texture resources during orderly shutdown

- Delivered by closed #1691.
- Invoke the existing alias-safe drain while the device, descriptor set, and
  queues are alive; keep no-device boot modes unchanged.
- Do not add a public mid-session unload verb in this slice.
- **Depends on:** nothing; may proceed alongside APS-1 through APS-3.

### APS-5. Add descriptor, handle, residency, alias, and memory observability

> **Disposition:** No separate issue — `docs/asset_telemetry_design.md` owns
> the typed snapshot, Lua query, bounded detail, real-GPU probe, representative
> baselines, and pressure warnings. APS-6, APS-11, APS-12, and APS-13 consume
> that evidence without duplicating the telemetry arc.

### APS-6. Replace path-only texture identity with a policy-aware asset key

- Introduce the normalized path plus explicit sampler/upload-policy key.
- Migrate the resource and in-flight maps without changing rendering output.
- Keep generated gameplay unit atlases on the global scene sampler and UI-only
  textures pinned nearest.
- Add cross-policy same-path coverage so neither request inherits the wrong
  sampler.
- **Depends on:** ATEL-5's measured baseline so the migration's resource/alias
  effect is measurable.

### APS-7. Separate canonical texture identity from asynchronous load requests

- Resolve Q-3 with a callback/name-registry compatibility inventory.
- Make one upload and one canonical stable handle per in-flight or ready
  `AssetKey`; fan terminal completion out to subscribers.
- Remove fresh cached aliases from the normal acquisition path.
- Keep existing shader-facing handles stable and preserve successful behavior.
- **Depends on:** APS-6.

### APS-8. Add explicit lifetime classes and owner-scoped release

- Resolve Q-4 and introduce only owners tied to real lifecycle boundaries.
- Preserve process/static assets through shutdown, release preview/world owners
  at their teardown, and keep generation-owned transients explicit.
- Release a canonical resource only after its final owner is gone, through the
  existing handle invalidation before image destruction.
- **Depends on:** APS-4 and APS-7.

### APS-9. Remove preview's abandoned-request handle growth

> **Disposition:** No separate issue — ASSET-3 already established that the
> developer-only preview race is too small for a tactical tracker item. Fold
> its rapid A -> B -> A acceptance case into APS-7/APS-8: every successful
> completion becomes reusable by `AssetKey`, only the current selection changes
> the sprite, and the settled sequence does not grow handles, slots, or owners.

### APS-10. Make all direct texture references a blocking validated inventory

- Delivered by closed #1705.
- Make the checker comment/string aware, self-tested, fail-loud, green on the
  repository, and present in both CI and `make ci` with parity intact.
- Do not turn it into an orphan-asset or atlas compiler in this slice.
- **Depends on:** nothing and may proceed at any time.

### APS-11. Generate a capacity-aware asset catalog and budget report

- Inventory authored sources separately from generated atlas artifacts.
- Record image dimensions, decoded byte estimates, intended sampler/lifetime,
  and expected eager/conditional/transient residency by family.
- Compare the static estimate with ATEL-5 runtime high-water captures.
- Fail on missing/stale ownership metadata, not on intentionally identical
  pixel content.
- **Depends on:** ATEL-5 and APS-10.

### APS-12. Pilot another atlas family only if measurements justify it

- Select the largest safe simultaneously resident candidate from APS-11, not
  the family with the largest repository file count.
- Reuse the unit pipeline's source/derived separation, deterministic build,
  digest, inventory, UV, gutter/filter, and freshness contracts.
- Keep the slice to one family and prove descriptor reduction, memory delta,
  rendering parity, and rebuild locality.
- Mark `[no-issue]` if the measured headroom makes this unnecessary.
- **Depends on:** APS-11.

### APS-13. Add bounded lazy residency or eviction only if measured pressure remains

- First use owner-scoped release and selective atlasing to establish the new
  plateau.
- If the agreed Q-2 headroom is still breached, design a bounded policy over
  canonical resource identity with visible high-water/failure behavior.
- Never recycle stable handle ids while cached render data may contain them.
- Mark `[no-issue]` if measured capacity remains healthy.
- **Depends on:** APS-8, APS-11, and APS-12's measured result.

## Evidence map

- Asset record and path-only cache: `src/Engine/Asset/Types.hs`.
- Monotonic handle allocation and release paths:
  `src/Engine/Asset/Manager.hs`.
- Handle and descriptor bounds:
  `src/Engine/Graphics/Vulkan/Texture/Limits.hs`.
- Upload, cache aliasing, sampler policy, and publication:
  `src/Engine/Scripting/Lua/Message/Texture.hs`.
- Unit atlas registration:
  `src/Engine/Scripting/Lua/API/YamlTextures.hs` and
  `src/Engine/Scripting/Lua/API/Units/Yaml.hs`.
- Unit atlas source/index/runtime contract: `tools/pack_atlas.py` and
  `src/Unit/Atlas/`.
- Font atlas construction and dedicated upload descriptors:
  `src/Engine/Graphics/Font/Atlas.hs`, `SDF.hs`, and `Upload.hs`.
- Zoom atlas construction: `src/World/ZoomMap/ChunkTexture.hs`.
- Verified audit findings: `docs/asset_system_findings.md`.
- Related zoom scaling design:
  `docs/chunk_residency_streaming_design.md`.
