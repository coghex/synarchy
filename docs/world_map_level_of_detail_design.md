# World-map level-of-detail design

Synarchy's complete finite-world map must remain useful as worlds grow without
allocating one texture whose dimensions and decoded RGBA8 footprint scale at a
fixed 32 pixels per physical chunk. This design chooses the map representation,
supported world-size ceiling, and persistence boundary before the deferred
chunk-residency arc freezes a generated-world bundle around the current atlas.

Design state: `ready for issue processing`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [x] EPIC. Make the complete world map scale safely to the supported world size — [#2017]
- [x] WML-1. Carry signed 32-bit cylinder coordinates in world vertices — [#2019]
- [x] WML-2. Reject unsafe map image plans before allocation or upload — [#2020]
- [x] WML-3. Persist an opaque generated-world identity with save migration — [#2021]
- [x] WML-4. Establish the shared generated-world library lifecycle — [#2024]
- [x] WML-5. Generate deterministic spatial pyramid pages — [#2298]
- [x] WML-6. Measure map-page codecs and disk-cache budgets — [#2303]
- [ ] WML-7. Define the versioned map-artifact format — [deferred]: #2303 must report and the owner must select Q-17's codec, disk quota, and multiworld cache accounting
- [ ] WML-8. Publish mandatory map artifacts during world generation
- [ ] WML-9. Load and recover map artifacts transactionally
- [ ] WML-10. Serve lazy fine pages through bounded caches
- [ ] WML-11. Reuse a fixed global GPU page pool safely
- [ ] WML-12. Render paged LOD terrain with fallback and cross-fades
- [ ] WML-13. Preserve and smooth analytical climate map modes
- [ ] WML-14. Measure runtime residency and transition tuning
- [ ] WML-15. Cut over to the paged map and retire the legacy atlas
- [ ] WML-16. Gate supported world sizes and reconcile deferred streaming

## Epic contract

- **Goal:** Every supported finite world has a complete, usable map whose CPU
  and GPU representation stays within explicit bounds and never depends on
  detailed gameplay-chunk residency.
- **Done when:** The selected representation covers the owner-approved maximum
  world size without an over-limit image or unbounded contiguous allocation;
  existing successful world sizes retain their terrain pixels, map modes,
  interaction coordinates, live discovery/location overlays, and documented
  icon appearance; opening the map requests no detailed chunks; fresh-world and
  load paths have explicit memory, latency, device-limit, and failure behavior;
  and the deferred streaming design has been reconciled with the chosen map
  artifact contract.
- **Users and operators:** Players creating, loading, and navigating finite
  worlds; maintainers changing world generation, save/load, map rendering, and
  Vulkan texture upload.
- **Tracker relationship:** This specialized map epic extracts and supersedes
  #1997's zoom-map representation/persistence work and establishes the shared
  library foundation that #1997 later extends with base chunks. #1997 remains
  the detailed-residency/streaming epic; #2001 remains its independent
  canonical-chunk-identity prerequisite. The new epic must state this
  relationship when processed so the overlap is explicit rather than a silent
  duplicate.
- **Arc label:** None proposed.

## Current state and evidence

Evidence below was checked against `master` at
`b1020f303f7fc32652362e44d284909fe20429b9` on 2026-08-31.

### Generation, storage, and upload

- `World.ZoomMap.Types.zoomTileSize` is 32. Each physical chunk produces a
  32x32 RGBA8 pixel block from the same 16x16 tile-level terrain, fluid,
  vegetation, and ice inputs used to keep the detailed and zoom views aligned.
- `World.ZoomMap.Cache.BuildPixels` derives the map directly from
  `WorldGenParams`. It does not read `WorldTileData` or `LoadedChunk`, and the
  `World.ZoomMap` tree does not import `World.Render`. That direction is already
  correct and is preserved by D-1.
- For an even `worldSize = N`, lines 203-214 enumerate `N` values of `u` and
  `N` values of `v`, retain only even `u + v`, and deduplicate the transformed
  coordinates. The live result is exactly `N^2 / 2` physical chunks, not
  `N^2`. This corrects the initial sizing premise while leaving the large-world
  failure intact.
- `World.ZoomMap.ChunkTexture.buildZoomAtlas` packs all chunk blocks into one
  nearly square image. It chooses `ceil(sqrt(chunkCount))` chunk cells per row,
  allocates `width * height * 4` bytes contiguously, and copies every block into
  that buffer.
- Fresh world creation forces the cache, all chunk pixels, and the complete
  atlas before handing the contiguous bytes to the render thread
  (`World.Thread.Command.Init`). Save loading calls
  `buildZoomCacheWithPixels` without the fresh-generation bordered-terrain
  cache for every non-arena page; for the active page it forces all pixels,
  rebuilds the atlas and preview, and stages the bytes before transactional
  publication (`World.Load.Stage`). The map is therefore regenerated during
  an ordinary load rather than restored as an artifact.
- `handleZoomAtlasUpload` converts the staged `Int` dimensions to `Word32` and
  calls `createVulkanImage'`. It does not query or compare the physical
  device's `maxImageDimension2D`. There is also no construction-time dimension
  or checked-multiplication guard before the CPU allocation. By contrast,
  `Engine.Graphics.Font.Load.queryMaxImageDimension2D` already demonstrates the
  repository's Vulkan query for the font atlas.
- The create-world UI currently offers 32, 64, 128, 256, 512, and 1024 chunks,
  calling the last two "Huge" and "Massive"
  (`scripts/create_world/settings_tab.lua`). Engine normalization enforces a
  minimum/multiple but no maximum (`World.Generate.Config.Normalize`), so 1024
  is exposed even though the current zoom texture cannot fit a 16384-pixel
  device limit.
- The current vertex format creates a separate non-map constraint near the next
  doubling. `Engine.Graphics.Vulkan.Types.Vertex.packUV` packs signed cylinder
  coordinates `(u, v)` into two `Word16` halves and explicitly wraps beyond
  `|u|` or `|v| = 32767` tiles, documented as `worldSize` approximately 2048.
  Supporting worlds materially beyond 1024 therefore needs a renderer-wide
  coordinate-packing prerequisite or companion arc; changing only the map
  texture representation would not make those worlds correct.
- The repository has no generated-world artifact key or world-library runtime
  yet. `World.Page.Types.WorldIdentity` is optional player-facing display text,
  gloss, language provenance, and etymology; its own documentation distinguishes
  it from remappable `WorldPageId` and from storage naming. Neither type can
  safely key shared map artifacts. The deferred chunk-residency document
  proposes a world-library bundle, but its CRS-9 through CRS-11 slices are
  unimplemented. The existing `cryptohash-sha256` dependency is available if a
  canonical content digest is selected, but no canonical map-source digest
  stream exists today.

### Rendering and interaction

- `World.Render.Zoom.Bake` creates one `BakedZoomEntry` per physical chunk. With
  the atlas present, each entry selects its 32x32 cell by vector index; without
  it, rendering falls back to a per-material texture. `World.Render.Zoom.Quads`
  culls the baked quads to the camera view, but the CPU cache and whole atlas
  remain complete and resident.
- Climate map modes, longitude lighting, cylindrical wrap choice, cursor
  selection, and location icons are live render concerns layered over the
  generated pixels. `pixelToChunkOrigin` derives selection from camera/world
  geometry rather than atlas texel coordinates, which is the seam that lets a
  representation change preserve interaction coordinates.
- `ZoomMapMode` has one default and seven analytical climate modes. The seven
  climate paths in `World.Render.Zoom.Quads.makeMapQuads` currently choose an
  RGB value per physical chunk and put it in the quad's vertex colour; the
  bindless fragment shader multiplies that value by the sampled terrain texel.
  This is existing runtime tinting and produces the intended heat-map views.
  The initial brief's unqualified no-tinting statement and
  `World.Render.Zoom.Icons`'s claim that nothing else on the zoom map is tinted
  are therefore stale: the owner explicitly confirmed on 2026-08-31 that the
  analytical zoom map is an enumerated tinting exception, alongside existing
  underwater treatment. The new representation preserves this exception.
  Dynamic solar/seasonal shading remains a separate live lighting calculation.
- Location icons are rebuilt from current lifecycle state each frame. Unknown
  and hinted locations share the unknown marker; discovered and active
  locations use the definition icon; cleared and depleted locations use the
  same icon darkened. The darkening is the explicit icon-only exception to the
  no-tinting rule. A new map representation must keep these overlays live rather
  than baking discovery state into an immutable base image.
- `wmVisible` can contain up to `Engine.Graphics.Solar.maxSolarPages = 16`
  world pages, and the zoom renderer traverses all of them. At D-7's 2048
  RGBA8 root cap, retaining one full root for every visible page has a bounded
  worst-case decoded cost of 256 MiB. By contrast, duplicating Q-8's approximate
  141 MiB two-LOD fine working set per visible world would approach 2.2 GiB.
  A fine-page residency design therefore needs an explicit shared multiworld
  policy rather than interpreting "screen-bounded" as one independent pool per
  page.

### Corrected sizing measurements

These are exact source-backed calculations of the current enumeration and
packing formulas, not runtime allocation samples. Padding in the nearly square
atlas accounts for the small difference from exactly
`chunkCount * 32 * 32 * 4`.

| `worldSize` | physical entries | current atlas | contiguous RGBA8 |
|---:|---:|---:|---:|
| 256 | 32,768 | 5824x5792 | 128.68 MiB |
| 512 | 131,072 | 11616x11584 | 513.30 MiB |
| 1024 | 524,288 | 23200x23168 | 2050.39 MiB |
| 2048 | 2,097,152 | 46368x46336 | 8195.91 MiB |

Consequences:

- A 512 world is below a 16384-pixel per-image limit in the live layout, though
  its roughly 513 MiB contiguous CPU payload and matching decoded GPU image are
  still large.
- A 1024 world, already offered in the UI, exceeds that limit and requires
  about 2.0 GiB for the atlas alone. The driver-error failure remains real, but
  the initial 1024 MiB/4096 MiB estimates were high by a factor of two because
  they counted both parity classes as physical chunks.
- A limit check only in the upload handler would replace a driver error with a
  controlled refusal after the expensive CPU allocation. Every candidate needs
  a checked pre-allocation plan as well as device-aware upload validation.

### Relationship to chunk residency and the tracker

- `docs/chunk_residency_streaming_design.md` is explicitly deferred on this
  design. Its measured 184.3 MiB process peak did not move when a worldSize-64
  dump increased from 9-plus-camera chunks to 289 resident chunks; that is an
  upper-bound observation, not a per-chunk cost measurement. At present the map
  representation, not the measured detailed-cache high-water, is the known
  world-size constraint.
- That document's Arc B currently says the generated-world bundle stores a
  reusable zoom artifact and includes CRS-14 for large-world map coverage. Those
  are prior proposals, not constraints on Q-1 or Q-3. After this design lands,
  the deferred document must be reconciled rather than treated as an authority
  that pre-decided persistence.
- Open epic #1997 is the deferred chunk-residency/streaming umbrella and still
  describes reusable zoom output in its background, CRS-8, CRS-12, and done
  condition. Open #2001, canonical chunk identity, is independent of the map
  representation and remains valid. A 2026-08-31 readiness scan of all open
  Synarchy issue titles and bodies found no separate world-map LOD, generated-
  world-library, or 32-bit world-coordinate issue. This arc is therefore not a
  duplicate epic, but D-18's explicit extraction relationship must accompany
  its tracker creation and WML-16 must remove the stale zoom ownership from the
  deferred design.

## Desired experience

### Creating a world

The world-size control offers only sizes the game intentionally supports. A
chosen size has a map representation known to fit the design's CPU/GPU budgets
before expensive pixel assembly begins. Hardware capability cannot turn a
successfully generated world into a late Vulkan image-creation error.

### Opening and navigating the map

The player can always see complete global coverage. Moving or zooming may
replace coarse terrain with finer terrain through D-3's bounded pyramid, but
there are no black cache holes and no request, reservation, generation, or wait
for detailed gameplay chunks. Cursor-to-chunk results, seams, facings, climate
map modes, lighting, and live icons behave as they do at successful existing
sizes.

### Loading a save

The map's load cost and availability follow D-5's durable-coarse/cached-fine
contract. It is not an accidental full regeneration hidden inside transactional staging. Missing,
stale, corrupt, incompatible, and over-device-limit map data each have a
defined outcome that preserves the old live session when transactional load
cannot stage a valid replacement.

## Scope

### In scope

- The supported finite-world maximum and how the create-world UI and engine
  enforce it.
- Generated map levels/pages, their CPU representation, their GPU images, and
  bounded residency or selection policy.
- Fresh generation, save/load staging, optional on-disk artifact ownership,
  compatibility, and failure handling for map data.
- Device-limit discovery and overflow-safe validation before CPU allocation and
  Vulkan upload.
- Complete global coverage, visual compatibility at existing successful sizes,
  climate modes, map interaction, cylindrical seams/facings, and live location
  overlays.
- Reconciliation of the deferred chunk-residency design's zoom assumptions
  after this design is ready.

### Out of scope

- Detailed gameplay-chunk residency, reservation, eviction, or streaming.
- An infinite world or generation beyond the declared finite boundary.
- Changing terrain, hydrology, flora, location placement, or discovery rules.
- Changing the generated climate-region values, runtime climate simulation, or
  making interpolated map values authoritative outside the player-facing
  analytical map. Reusing this interpolation model elsewhere is a separate
  future design.
- Designing or performance-gating multiple simultaneous full-size worlds.
  Artifact/page keys remain world-qualified and the existing small auxiliary
  world capability must not be broken, but concurrent large-world fairness and
  budgets belong to future multiworld support.
- Baking live discovery/icon state into generated terrain imagery.
- Runtime tinting outside the owner's enumerated exceptions. Base terrain
  colour remains baked; the existing analytical zoom-map heat maps and
  cleared/depleted icon darkening remain live exceptions. Underwater and other
  existing renderer exceptions are unaffected by this arc.
- Drafting or creating tracker issues, changing labels, implementation, or
  publishing this local document.

## Design

### Settled ownership boundaries

`World.ZoomMap` remains the producer of representation-neutral map artifacts
from `WorldGenParams`; it never imports `World.Render` and never consults
resident `LoadedChunk`s. `World.Render.Zoom` consumes those artifacts, applies
view selection and live overlays, and owns interaction/render state. Any
artifact storage layer stores generated map data, not renderer structs, texture
handles, discovery state, or resident detailed chunks.

### Constraints common to every representation

Before allocating pixel storage, a pure plan must derive all levels/pages,
dimensions, decoded bytes, and checked arithmetic from the normalized world
size and the selected representation. Before each Vulkan image allocation, the
render path must compare both dimensions with the actual device's
`maxImageDimension2D`. The chosen failure/degradation contract cannot depend on
waiting for a driver error.

The format keeps a complete global fallback independent of fine-detail
availability. Live icons, cursor overlays, analytical map-mode tinting, and
longitude/seasonal lighting stay separate from immutable base-terrain pixels.
A level or page transition must not change the chunk coordinate under the
pointer.

### Proposed pyramid mechanics

The finest level keeps one 32x32 pixel cell per physical chunk, preserving the
current generated chunk image exactly. Each coarser cell covers a power-of-two
larger chunk footprint. Pages pack a spatially adjacent fixed-size block of
cells; page keys are `(world identity, level, page-u, page-v)`, not indices into
one complete per-chunk vector. The parity-valid cylindrical `(u,v)` lattice
needs a canonical compressed page address so the representation does not spend
half its fine cells on impossible parity coordinates.

Coarse pixels are deterministic baked output reduced from finer generated
pixels. Fresh world generation can stream fine pages through the reduction
tree, atomically publish the mandatory coarse pages, and discard non-prewarmed
fine pages instead of ever assembling the all-world finest level. On-demand
fine generation uses the same pure cell generator over a page footprint plus
the one-chunk neighbor halo required by the current cross-boundary ocean
extension. It never goes through the gameplay residency manager.

The renderer chooses the coarsest level whose cells do not undersample the
current logical framebuffer, requests only intersecting pages plus a bounded
prefetch ring, and falls back independently for each absent page to its nearest
available ancestor. Selection and icon/cursor geometry remain in world/chunk
coordinates, so neither page boundaries nor fallback levels change interaction
results. D-15 makes a ready child page cross-fade independently over that
ancestor as the camera settles.

Pages are spatial rasters in canonical cylindrical `(u,v)` map space, not
mini-atlases that still require one draw quad per physical chunk. At the finest
level, generation composes the existing 32x32 chunk diamonds into their exact
spatial positions within the page payload. A page is then drawn with O(1)
geometry under each camera facing; facing changes rotate/swap the page mapping
rather than rebuilding all-world chunk geometry. This is what removes both the
all-world `BakedZoomEntry` vector and a hidden 256-quads-per-finest-page
replacement for it.

### Maps-app page refinement

The nearest available ancestor draws immediately. Current-view fine requests
carry a view epoch and are prioritized as the camera settles; obsolete requests
may be cancelled or deprioritized, but an already valid completed page remains
ordinary identity-keyed cache data. A stale result never publishes into the
current view mapping.

When a requested child becomes GPU-ready for the current view, it cross-fades
independently over its ancestor. Both slots remain pinned until the fade and
normal frames-in-flight retirement complete. Moving away can abandon a
transition without losing fallback coverage. Cursor and icon overlays remain
single-source world-coordinate geometry throughout and never double, fade, or
shift with terrain refinement. The exact fade duration is selected by visual
measurement rather than frozen here without a probe.

### Fixed GPU page pool

Fine page identity cannot be represented by permanent texture handles.
`maxBindlessTextures` is 16,384 and the stable handle-to-slot table has 65,536
monotonic, never-recycled IDs; an 8192 world can contain more fine pages than
that even before reloads and cache churn. Allocating one handle per world page
would therefore exchange the atlas-dimension failure for deterministic handle
exhaustion.

The map renderer instead owns a fixed pool of GPU page slots sized from the
maximum simultaneous visible pages, prefetch ring, fallback overlap, and
frames-in-flight safety margin. Stable handles belong to pool slots, not map
page keys. Reassignment uploads a page into a reusable slot, publishes the new
page-key-to-slot mapping only when ready, and retires the old image after every
in-flight frame that could sample it. CPU and disk caches remain independently
keyed by page identity.

Per D-19, that fine pool is process-global rather than duplicated per visible
world. Its keys remain `GeneratedWorldId`-qualified and every miss retains the
normal complete ancestor fallback. The slot count is a measured fixed budget,
not `visibleWorlds * oneViewportPages`; complete roots remain separately
resident under D-7. This arc gates one full world plus small auxiliary worlds,
not simultaneous large-world fairness.

### Retained analytical heat maps and lazy tint data

The seven analytical `ZoomMapMode`s remain an explicit runtime-tint exception.
Base-terrain page identity does not gain a mode component, and the generated
world library does not store eight colour variants. The paged renderer applies
the existing mode palettes to immutable terrain texels while icons remain a
separate overlay and solar/seasonal shading remains a separate lighting input.

The current per-physical-chunk vertex colour cannot become an all-world
`BakedZoomEntry` vector in the scalable representation. A compatible candidate
keeps the existing shader multiplication but supplies its tint coefficient from
one dense scalar climate array for the selected mode, materialized lazily from
`WorldGenParams` and indexed by interpolated world `(u,v)`. That array may be an
SSBO or sampled image; it is numerical climate input, not a second coloured map
texture, and this design does not require a separate Vulkan shader pipeline.

Climate regions are 4x4 chunks, so a worldSize-8192 field is 2048x2048 samples:
16 MiB as one 32-bit float per region. Sea temperature additionally needs an
ocean/land signal; a second float would bound the pair at 32 MiB, while a packed
mask would be smaller. At the initial guaranteed worldSize 1024 the one- and
two-float figures are 0.25 MiB and 0.5 MiB. These are exact decoded-size
formulas, not upload-time measurements or a frozen GPU carrier.

Only the selected field is required on the GPU. On a mode change, the renderer
builds and uploads the requested field in the background, keeps the previous
complete view until publication, then switches atomically. Retaining more than
one prepared field is a later measured optimization, not the initial policy.
This applies the owner's general lazy-first rule without adding mode-specific
map artifacts or repeating base-terrain generation.

### Proposed physical page geometry

D-11 selects a 512x512-texel payload plus a duplicated one-texel outer
gutter wherever linear sampling can cross a page boundary. At the finest level
the payload contains 16x16 current 32-pixel physical-chunk cells. A decoded
514x514 RGBA8 upload is 1.008 MiB. At one screen pixel per payload texel, a
3840x2160 viewport intersects about 8x5 payload pages; a one-page prefetch ring
raises that to 10x7, and retaining two adjacent LODs during replacement is
about 140 pages or 141 MiB. The latter count is a planning assumption, not a
measured pool bound: camera rotation, cylindrical duplication, upload staging,
and frames in flight must be measured before the slot count is frozen under
D-20.

The neighboring candidates are 256-pixel payloads, which quarter decoded bytes
per page but increase requests, draws, and page-table churn, and 1024-pixel
payloads, which quarter the page count but make each decoded page approximately
4 MiB and coarsen eviction/prefetch granularity. All choices need seam gutters,
checked dimensions, and the same fixed-slot retirement protocol.

### Encoding and cache-budget measurement gate

Decoded RGBA8 sizes above are known, but encoded page ratios and random-access
decode costs are not. Before the artifact schema freezes a codec or disk-cache
budget, a delivery slice must capture representative default and analytical
pages spanning ocean, varied land, ice, and other high-contrast boundaries;
compare raw checksummed pages with PNG through the repository's existing
JuicyPixels dependency and any additional codec only after justifying its new
dependency; and record encoded bytes, encode/decode wall time, peak decoded
memory, deterministic-byte behavior, and corruption localization. D-12 makes
this an owner gate rather than inventing an unmeasured codec or cache quota in
the design.

### Representation candidates and measured prices

#### Proposal A: resolution pyramid with screen-bounded fine pages

Keep a coarse complete global level resident and select finer generated pages
only where the viewport can resolve them. At worldSize 1024, a 2-pixel-per-
chunk global RGBA8 level would be about 8.0 MiB and a 4-pixel level about
32.0 MiB under the live `N^2 / 2` physical-chunk geometry. Those are formula
measurements; the visually sufficient coarse level and maximum simultaneous
fine-page count have not been measured.

For a genuinely large target, the root level must be bounded by a chosen
screen/device resolution rather than retain any fixed pixels-per-chunk ratio.
Otherwise even a nominally coarse 2-pixel level reaches about 512 MiB at
worldSize 8192 and exceeds a 16384-pixel image at worldSize 16384. The intended
map-application shape is therefore a fixed-size complete root (or a fixed small
root page set) followed by progressively finer addressable pages.

The current 32-pixel detail can remain available without making its whole
roughly 2.0 GiB worldSize-1024 corpus resident. This best separates complete
coverage from view-bounded detail, but adds level selection, page addressing,
seam-safe borders, transition/fallback behavior, and a source for fine pages
(disk or regeneration). A full pyramid that kept every finest texel resident
would not solve the problem; screen-bounded fine residency is load-bearing.

#### Proposal B: tiled pages at the current 32-pixel resolution

Split the existing pixels into dimension-safe images without adding resolution
levels. For example, a 4096x4096 RGBA8 page covers 128x128 chunk cells and is
64 MiB. A worldSize-1024 corpus contains about 32 such pages and still totals
roughly 2.0 GiB decoded. Tiling alone fixes the single-image dimension limit; it
fixes memory only if page residency is bounded, and it offers no cheaper global
fallback unless every page is present.

This preserves current texels most directly and is simpler than a pyramid, but
complete always-resident coverage retains the current area scaling. Bounded
pages make global zoom-outs either page-heavy or dependent on an additional
coarse level, at which point this converges on Proposal A.

#### Proposal C: reduce the fixed pixels per chunk

Changing 32 to 8 cuts decoded area and bytes by 16. Under the live geometry:

| `worldSize` | approximate 8-pixel atlas | contiguous RGBA8 |
|---:|---:|---:|
| 512 | 2904x2896 | 32.08 MiB |
| 1024 | 5800x5792 | 128.15 MiB |
| 2048 | 11592x11584 | 512.24 MiB |

This buys two doublings of world side before reaching approximately the same
dimension/area class. It is operationally simpler than paging, but a universal
change would alter current terrain pixels and therefore conflicts with D-2.
A size-dependent representation could retain 32-pixel output for existing
successful sizes and use 8 pixels only above a threshold, but then cross-size
fidelity and the threshold become additional product decisions.

#### Proposal D: cap supported worlds at 512

Keep the current atlas representation, remove/refuse the exposed 1024 option,
and enforce 512 consistently in UI, Lua lifecycle, CLI/tooling, and load
validation. This avoids a new LOD representation, but accepts an approximately
513 MiB map texture and the same load-time regeneration cost at the maximum.
Device queries and checked pre-allocation validation remain necessary because
the design cannot assume every supported device accepts an 11616-pixel image
or that an incoming save is honest.

### Persistence candidates

Q-3 is independent enough to compare but not to decide before Q-1:

- **Persist all generated levels/pages:** world creation pays once and loads
  validate/read the artifact. This supports bounded fine-page loading but adds
  artifact schema/versioning, checksums, compatibility identity, atomic
  publication, potentially large disk use, sharing/cleanup, and migration or
  regeneration behavior. At worldSize 1024, storing every uncompressed
  32-pixel fine texel is still roughly 2.0 GiB before compression.
- **Persist only the complete coarse level:** loads can always open a small
  global map while fine pages are regenerated or obtained separately. This
  limits mandatory disk cost and aligns with a pyramid, but needs an explicit
  fine-detail latency/fallback contract.
- **Persist no map artifact:** retain regeneration from `WorldGenParams` on
  every load. This keeps save storage simpler but preserves quadratic
  generation work and makes bounded/page-on-demand representations responsible
  for their own regeneration scheduling. No representative worldSize-512 or
  1024 load time has been measured, so its latency is an assumption rather than
  a priced number.

For scale, storing uncompressed levels at 2, 4, 8, 16, and 32 pixels per chunk
would cost approximately:

| `worldSize` | complete 2-pixel level | full 2-to-32 pyramid |
|---:|---:|---:|
| 1024 | 8.01 MiB | 2.67 GiB |
| 2048 | 32.02 MiB | 10.66 GiB |
| 4096 | 128.02 MiB | 42.63 GiB |
| 8192 | 512.07 MiB | 170.52 GiB |

These are decoded RGBA8 formula measurements, not compressed-file
measurements. Compression ratio is deliberately unclaimed until representative
map pages are measured.

#### Selected persistence split

World generation should atomically publish a versioned map manifest and the
bounded complete root/coarse levels into the shared generated-world library.
That mandatory artifact makes every load start with global coverage and avoids
repeating all-world map generation. Descendant saves reference the generated
world identity rather than embedding or duplicating the artifact.

Fine pages should be reproducible cache data, generated from `WorldGenParams`
through the map pipeline without touching resident gameplay chunks. A viewed
page may be retained under a bounded per-world disk cache and safely deleted or
invalidated by generator/content identity. Missing fine pages fall back to the
nearest complete coarser level while background map work produces them; they do
not block opening the map or loading gameplay. This keeps mandatory disk usage
bounded while preserving the exact 32-pixel terrain when fine pages are ready.

Whether world creation prewarms some or all fine pages for smaller worlds is a
later measured policy, not part of the persistence contract: prewarming cannot
turn the fine cache back into an unbounded mandatory artifact at large sizes.

### Artifact identity and world-library boundary

D-5 requires descendant saves to share one immutable generated map artifact,
but the live identity types do not supply that key. D-17 adds a new opaque
`GeneratedWorldId`, assigned once and stored with separate generator/content
compatibility metadata. Display
`WorldIdentity`, remappable `WorldPageId`, save-slot names, and filesystem paths
are excluded from artifact identity.

D-18 makes this map arc establish the minimal library now—registry, atomic
artifact directory, reference from current saves, and reference-aware cleanup—
with only manifest/root/coarse map payloads. The deferred chunk-residency arc
then extends that same foundation with base-chunk records instead of first
inventing the library around a zoom representation it no longer owns. A
temporary map-only sidecar would shorten this arc but require a second storage
migration and duplicate identity/cleanup rules later.

## Decisions

### D-1. The world map is independent of detailed gameplay-chunk residency

The map is derived from `WorldGenParams`, not resident `LoadedChunk`s. Opening
or navigating it never requests, reserves, generates, or waits for detailed
gameplay chunks. `World.ZoomMap` remains independent of `World.Render`, with
the dependency running from render consumers to generated map data.

This was explicitly settled by the owner in the 2026-08-31 design brief.

### D-2. Existing successful map output and interaction remain compatible

At world sizes the current map can successfully render, terrain pixels and map
modes do not regress; cursor/chunk coordinates and seam/facing behavior stay
stable; live discovery and location icons remain overlays; and the existing
cleared/depleted darkened-icon exception is preserved. Base-terrain colour
belonging to generated levels/pages remains baked. The seven existing
analytical heat-map modes remain an explicit runtime-tint exception rather than
being multiplied into eight generated page pyramids.

The output/interaction requirement was explicitly settled in the owner's
2026-08-31 design brief. The owner clarified later that day that the brief's
unqualified no-tinting sentence did not revoke the established analytical
zoom-map exception.

### D-3. Use a bounded resolution pyramid

The map uses bounded complete root/coarse levels plus progressively finer
addressable pages. Root/coarse decoded dimensions are bounded by the chosen
screen/device envelope rather than any fixed pixels-per-chunk ratio. Fine
pages retain the current 32-pixel-per-chunk output where the viewport can
resolve it, but only a screen-bounded working set is resident.

This is not a full mip pyramid resident for the whole world. Missing fine data
falls back to the nearest complete coarser level, so global coverage never has
holes and fine-page availability never creates detailed gameplay-chunk demand.

The owner explicitly approved this choice on 2026-08-31, resolving Q-1.

### D-4. Guarantee 1024 initially and design map addressing through 8192

WorldSize 1024 is the minimum guaranteed product ceiling for this arc. The map
format, page keys, checked arithmetic, manifests, and synthetic boundary tests
are designed through worldSize 8192. Sizes above 1024 are exposed to players
only after broader world-generation and renderer gates prove them; map-format
addressability alone is not a claim that the rest of the engine can generate or
render them correctly.

This split keeps the map from imposing the next ceiling while refusing to
advertise unmeasured whole-engine support. The owner explicitly approved it on
2026-08-31, resolving Q-2.

### D-5. Persist coarse truth and cache fine detail

World generation atomically publishes a versioned map manifest and bounded
complete root/coarse levels in the shared generated-world library. Descendant
saves reference that generated-world identity; map artifacts are not embedded
or duplicated in saves.

Exact fine pages are reproducible, generator/content-identity-keyed cache data
derived from `WorldGenParams`, never resident gameplay chunks. They may be
retained under a bounded per-world disk cache and safely deleted or invalidated.
Missing fine pages fall back to complete coarse coverage while background map
work regenerates them. A mandatory all-world fine pyramid and all-world map
regeneration on every load are both rejected.

The owner explicitly approved this choice on 2026-08-31, resolving Q-3.

### D-6. Widen the world-coordinate render carrier in a prerequisite slice

The 16-bit packed world-coordinate carrier must be replaced before the map arc
claims addressability beyond its current approximately-worldSize-2048 wrap
boundary. This is a separate, first implementation slice on the critical path,
not an incidental edit hidden inside paged map rendering.

The wire shape is fixed by D-9: every world vertex carries signed 32-bit `u`
and `v`, even though the current shader consumes only longitude `u`. The owner
explicitly requested the prerequisite on 2026-08-31 and then made `v`'s intended
future seasonal/directional shading use explicit; the prerequisite must not
optimize that coordinate away merely because the shading work has not landed.

### D-7. Keep a 2048 root resident and persist complete coarse data through 4K

Each visible world keeps a complete root capped at 2048 pixels on its longest
axis resident. The mandatory generated artifact also carries complete coarse
pages through an approximately 4096-pixel-equivalent level; those sharper pages
use the fixed screen-bounded GPU pool rather than becoming an unconditional
second full resident image.

This keeps the unconditional decoded GPU floor near one 2048 RGBA8 image
(16 MiB before exploiting the physical half-lattice), while a 4K global view
can reach native coarse coverage without generating fine pages. The owner
explicitly approved this choice on 2026-08-31, resolving Q-5.

### D-8. Recover compatible coarse artifacts and refuse incompatible ones

A missing or corrupt fine page is discarded and regenerated as a cache miss. A
missing or corrupt mandatory manifest/root is regenerated and atomically
republished before staged-session publication only when the persisted
generator/content identity exactly matches an available generator. If identity
is incompatible or unavailable, transactional load fails with an actionable
diagnostic and leaves the old session intact. Blank coverage and a late driver
error are not recovery modes.

The owner explicitly approved this choice on 2026-08-31, resolving Q-6.

### D-9. Carry signed 32-bit `u` and `v` in every world vertex

Attribute location 6 becomes `FORMAT_R32G32_SINT` and carries two signed
32-bit cylinder coordinates. `solarPage` moves from byte offset 48 to 52 and
the world `Vertex` stride grows from 52 to 56 bytes. The shader may use only
`u` today, but both components are part of the forward render contract.

`v` is intended for future seasonal and directional shading. When the camera
faces ±90 degrees, the projected shadow response in summer and winter must be
able to depend on both world axes rather than reconstructing or inventing a
coordinate the vertex format discarded. Paying the 7.7 percent vertex-stride
increase now avoids another whole-renderer vertex-format migration when that
shading work begins.

The owner explicitly chose both 32-bit components on 2026-08-31, resolving
Q-4.

### D-10. Preserve analytical heat maps as a zoom-map tint exception

The seven non-default `ZoomMapMode`s keep their existing visual result as an
explicit exception to the general baked-colour rule. Base terrain remains
baked; analytical palette multiplication remains live and does not create eight
mode-specific map pyramids. The scalable renderer must preserve the current
climate-coordinate ownership and mode formulae without retaining one CPU quad
record per physical chunk; D-14 deliberately smooths the scalar lookup rather
than retaining nearest-region blocks.

Mode-specific render data follows a lazy-first policy: prepare only the selected
mode's tint coefficients, publish them atomically when complete, and add
retained/prewarmed fields only if measurements demonstrate a problem. Base map
pages remain unchanged and are still tinted by the shader. The exact scalable
carrier is settled by D-13. The owner clarified the exception and selected
lazy generation on 2026-08-31, resolving Q-7.

### D-11. Use 512-pixel page payloads

Each map page has a 512x512 texel payload plus the seam-safe outer gutters its
sampling path requires. At the finest level that payload contains 16x16 current
32-pixel physical-chunk cells. A one-pixel gutter on every side yields a
514x514 RGBA8 upload of 1.008 MiB. The fixed pool's slot count is measured from
facings, cylindrical duplication, prefetch, LOD overlap, staging, and
frames-in-flight behavior rather than inferred from payload size alone.

The owner approved this target on 2026-08-31, resolving Q-8.

### D-12. Make codec and cache-budget measurement an explicit tracker gate

The artifact codec and bounded fine-page disk-cache quota are not guessed in
this design. A dedicated tracked measurement slice precedes the manifest/schema
slice, records its representative corpus and results durably, and stops for an
owner decision. Codec- or quota-dependent delivery slices remain explicitly
blocked on that decision; neither value may be selected silently during
implementation.

The owner explicitly required this gate to appear in the tracker decomposition
on 2026-08-31, resolving Q-9.

### D-13. Feed shader tinting from one lazy scalar climate field

The scalable map keeps tinting the existing base terrain pages in the shader.
When a non-default analytical mode is selected, the renderer lazily linearizes
only that mode's climate scalar from `WorldGenParams` into a dense GPU-readable
field. Interpolated signed 32-bit `(u,v)` identifies the climate region for each
fragment. No mode-coloured map texture is generated, mode does not enter base
page identity, and the spatial page raster is drawn with O(1) geometry rather
than retaining the all-world `BakedZoomEntry` vector or rebuilding it inside
each visible page merely to transport per-chunk vertex RGB.

Whether the field is exposed as an SSBO or sampled scalar image, and whether
the generic bindless pipeline gains a guarded zoom path or a zoom-specific
variant owns it, remain measured implementation choices. The behavior contract
is one selected field, atomic publication, and lazy-first retention. The owner
approved this carrier on 2026-08-31, resolving Q-10.

### D-14. Smooth analytical heat maps in scalar climate space

Analytical map modes bilinearly interpolate their scalar climate samples before
applying the existing palette. Longitude interpolation wraps across the world
cylinder and bounded latitude interpolation clamps. Sea-temperature land/ocean
ownership remains discrete: land keeps its current grey result, while ocean
fragments interpolate only contributing ocean samples and renormalize those
weights so colours do not bleed across coasts.

The climate-region samples remain the generated source values, but their block
boundaries were an implementation limitation rather than the intended
player-facing presentation. This deliberate visual improvement supersedes
D-2's otherwise exact preservation of current heat-map output. Changing
climate truth or applying the interpolation outside the analytical map remains
out of scope. The owner approved smoothing on 2026-08-31, resolving Q-11.

### D-15. Refine coarse coverage with independent maps-app cross-fades

The nearest available coarse ancestor appears immediately. As the camera rests
on a view, finer pages load lazily; each current page cross-fades from its
ancestor when ready rather than waiting for the entire viewport or popping in
at a hard boundary. Obsolete view requests are cancelled or deprioritized, and
no stale result can replace the current mapping. Parent and child residency is
pinned only for the transition and frames-in-flight safety window.

This deliberately permits neighboring terrain pages to occupy different LODs
briefly, as ordinary map applications do. Complete fallback coverage, cursor
selection, icons, and interaction coordinates never wait or fade. The owner
approved this behavior on 2026-08-31, resolving Q-12.

### D-16. Reduce coarse levels with deterministic premultiplied box filtering

Every coarser terrain level is a repeated deterministic 2x2 box reduction of
the next finer spatial raster in the current UNORM numerical colour space. RGB
is accumulated premultiplied by alpha, alpha is averaged, and non-zero output
is converted back to straight RGBA8 using one specified integer-rounding rule.
Opaque terrain is the ordinary four-pixel mean; transparent world/page edges
retain nearby colour without a black fringe.

Source neighborhoods wrap across cylindrical longitude, remain transparent
outside bounded latitude, and fill page gutters from the same neighbor samples
so independent page builds agree at seams. The owner approved this reduction
on 2026-08-31, resolving Q-13.

### D-17. Give each generated foundation an opaque storage identity

A new opaque `GeneratedWorldId` is assigned once at world creation and
persisted in the generated artifact manifest and every descendant save. It is
distinct from optional player-facing `WorldIdentity`, remappable
`WorldPageId`, save-slot names, and paths. Generator, content, palette, and map
schema compatibility are recorded separately; page/artifact checksums establish
byte integrity separately.

Compatible regeneration republishes under the saved ID. Independently creating
the same seed does not deduplicate, which avoids freezing a canonical digest of
the large `WorldGenParams` surface merely for speculative cross-creation
sharing. The owner approved this identity on 2026-08-31, resolving Q-14.

### D-18. Establish the minimal shared generated-world library in this arc

This arc creates the shared library foundation needed by D-5: registry, atomic
artifact directory publication, current-save reference, and reference-aware
cleanup, initially containing only the map manifest and mandatory root/coarse
payloads. It does not create base-chunk records or detailed-chunk streaming.

After this design lands, the deferred chunk-residency document is reconciled so
its future bundle slices extend this library with base chunks and no longer own
zoom representation, persistence, or a second identity/cleanup migration. The
owner approved this ownership boundary on 2026-08-31, resolving Q-15.

### D-19. Share one fine-page pool and defer full multiworld scaling

The renderer owns one process-global fixed fine-page GPU pool. Page keys remain
qualified by `GeneratedWorldId`, so small auxiliary/teleport worlds can retain
the existing multiworld capability without colliding with the main world. The
pool is not multiplied by `maxSolarPages` and this arc does not attempt to make
16 simultaneous full-size worlds equally detailed.

The product currently uses one full world; auxiliary worlds are unused and are
intended to remain small enough that their root/coarse memory is not material.
When full multiworld support is designed, it may add measured fairness and
priority policy without changing map identity or artifact formats. The owner
approved the global pool and this deferral on 2026-08-31, resolving Q-16.

### D-20. Measure runtime tuning in a tracked prerequisite

After the pure page planner exists and before runtime constants freeze, one
tracked slice measures the global GPU slot budget, prefetch depth, LOD
hysteresis, and cross-fade duration across the supported viewport envelope,
facings, seam cases, camera movement, LOD overlap, transition pinning, staging,
and frames in flight. It records results and returns to the owner for numerical
approval; it does not change gameplay behavior or silently select constants.

The extra issue is useful because the current 4K count is explicitly only a
planning assumption, and harmless because it is an evidence gate rather than a
runtime mutation. The owner approved this procedure on 2026-08-31, resolving
Q-18.

### D-21. Give legacy saves a fresh generated-world ID during staging

Each compatible pre-`GeneratedWorldId` page receives a fresh opaque ID during
transactional load staging. Its mandatory map artifact is compatibility-built
and atomically published under that ID before session publication. The live
session carries the ID and the next ordinary save persists it; loading never
rewrites the source save automatically. Failed staging leaves neither a new
session nor a published artifact.

Loading the unchanged legacy save again before resaving may allocate another
ID, with the prior unreferenced artifact removed by library cleanup. The owner
approved this simple migration and reaffirmed the repository practice of
providing a migration for every save-format change even when no personal saves
need preservation, resolving Q-19.

## Open questions

### Q-1. Which representation should replace or bound the single atlas?

**Resolved by D-3.**

Choose among Proposal A (coarse global pyramid plus screen-bounded fine pages),
Proposal B (current-resolution pages), Proposal C (fewer fixed pixels per
chunk, potentially size-dependent), Proposal D (cap at 512), or an explicitly
defined combination. This choice determines generation output types, render
selection, fallback behavior, artifact shape, and most child-slice boundaries.

**Resolution history:** the owner first preferred the resolution pyramid, then
explicitly approved the load-bearing bounded-root/screen-bounded-fine form
recorded as D-3.

### Q-2. What maximum world size is a supported product target?

**Resolved by D-4.**

The current UI advertises 1024, but that size cannot upload on a 16384-pixel
limit today. A 512 maximum permits Proposal D; 1024 requires a new
representation; 2048 or larger makes the representation's asymptotic bounds
and likely world-generation cost more important. Generation time and non-map
peak memory at 512/1024/2048 have not been measured here and must not be
invented.

**Owner direction, not yet an exact maximum (2026-08-31):** 1024 is the bare
minimum acceptable ceiling and the desired target is much larger. The next
candidate targets are not equivalent: 2048 is four times 1024's world area and
already reaches the current 16-bit packed-world-coordinate boundary; 4096 is
sixteen times the area; 8192 is sixty-four times the area. Selecting either of
the latter makes the renderer coordinate repair and broader world-generation
scaling explicit prerequisites rather than work this map arc can silently
assume.

The owner subsequently approved the 1024 guarantee plus 8192 map-addressability
target recorded as D-4.

### Q-3. Does world generation write a reusable map artifact to disk?

**Resolved by D-5.**

The choices are all generated levels/pages, only the complete coarse level, or
none. This decides whether loads read/validate generated output or regenerate
it, how a pyramid obtains fine pages, whether descendant saves share data, and
which assumptions must be removed from the deferred chunk-residency arc. It
also decides whether missing/corrupt data is a load failure, a coarse-only
degradation, or a compatibility-checked regeneration.

The recommended durable-coarse/cached-fine split was explicitly approved and
recorded as D-5.

### Q-4. Does the vertex carry one signed 32-bit longitude or two signed
32-bit cylinder coordinates?

**Resolved by D-9.**

The current 52-byte `Vertex` stores two signed 16-bit `(u, v)` values in one
`Word32` at attribute location 6, but the bindless world vertex shader decodes
and uses only low-half longitude `u` for local solar phase. No live shader reads
`v`.

The rejected minimal alternative changed location 6 to `FORMAT_R32_SINT`,
carried one `Int32 worldU`, and kept the stride at 52 bytes. That priced only
the live shader, not the intended renderer: `v` is reserved for future
seasonal/directional shading, including the different projected shadow response
under ±90-degree camera facings. D-9 therefore chooses
`FORMAT_R32G32_SINT`, moves `solarPage`, and accepts the 56-byte stride.

Today `packWorldUV gx gy` computes `u = gx - gy` and `v = gx + gy`, packs both
into one `Word32`, and the bindless vertex shader masks only the low 16 bits
into `rawU`. D-9 keeps both values, sign-extends each to 32 bits, and removes
the pack/decode truncation. Existing longitude lighting remains equivalent
within the old range; future shading receives the preserved `v` directly.

### Q-5. How much complete coarse imagery is mandatory?

**Resolved by D-7.**

The complete fallback can be a smaller always-resident root, with sharper
coarse pages loaded through the same bounded pool, or it can keep complete
levels through approximately 4K global resolution. The former minimizes
per-visible-world memory but may briefly upscale on a 4K display; the latter
costs roughly four times as much per doubled side and gives crisp global
coverage before any page request resolves.

The proposal is an always-resident root capped at 2048 pixels on its longest
axis, plus complete persisted coarse pages through a 4096-pixel-equivalent
level loaded screen-bounded. This keeps the unconditional GPU floor near one
2048 RGBA8 image (16 MiB before exploiting the physical half-lattice) while a
4K view can reach native coarse coverage from mandatory disk data.

The owner approved this proposal on 2026-08-31.

### Q-6. What happens when mandatory coarse artifacts are missing or corrupt?

**Resolved by D-8.**

Fine cache damage is always a cache miss: discard and regenerate. The mandatory
manifest/root is different because D-3 requires complete coverage. The
proposal is compatibility-checked recovery: when the save's persisted
generator/content identity exactly matches an available generator, rebuild and
atomically republish the coarse artifact before publishing the staged session;
when identity is incompatible or unavailable, fail the transactional load with
an actionable diagnostic and leave the old session intact. A blank map or a
driver error is never a recovery mode.

The owner approved this proposal on 2026-08-31.

### Q-7. How do the seven analytical map modes fit the tinting contract?

**Resolved by D-10.**

The premise was incorrect. The current modes multiply base terrain texels by
per-quad vertex RGB, and the owner confirmed that the analytical zoom map is an
intentional tinting exception because the heat maps look right. The initial
brief and a live icon comment stated the no-tinting rule too broadly. D-10
preserves the exception, rejects eight baked mode pyramids, and applies the
owner's lazy-first policy to whichever scalable mode-data carrier Q-10 selects.

### Q-8. What is the decoded page payload size?

**Resolved by D-11.**

The proposed balance is a 512x512 payload with seam-safe outer gutters: about
1.008 MiB per decoded RGBA8 upload and 16x16 current physical-chunk cells at
the finest level. A 256 payload reduces eviction granularity but increases
request/draw/page-table pressure; a 1024 payload reduces page count but makes
each decoded upload about 4 MiB. The exact slot count remains measurement-led
because facings, wrap duplication, LOD overlap, staging, and frames in flight
change the viewport-only estimate.

The owner approved the 512-pixel payload target on 2026-08-31.

### Q-9. May encoding and the disk-cache quota remain a measured owner gate?

**Resolved by D-12.**

No representative page compression ratios or random-access decode timings
have been measured. The proposed delivery order therefore puts a format probe
before the manifest/schema slice, records the corpus and results durably, and
then stops for the owner to select the codec and bounded cache quota. No child
issue may silently infer either value. Approving this question approves that
decision procedure, not PNG, another codec, or a numerical quota.

The owner approved the explicit post-measurement decision and required it to be
represented in the tracker on 2026-08-31.

### Q-10. What carries analytical climate values into the paged renderer?

**Resolved by D-13.**

The current per-chunk vertex colours depend on an all-world
`BakedZoomEntry` vector, which must disappear for large worlds. Today Haskell
computes each chunk's climate RGB, stores it in that quad's vertices, and the
generic fragment shader performs `texColor * fragColor`. A 512-pixel page drawn
as one quad has only four vertex colours and cannot carry the different climate
regions inside its 16x16-chunk payload.

The proposed replacement keeps the shader-side tint operation and the existing
base terrain pages. It lazily linearizes only the selected scalar climate field
from the already-resident `WorldGenParams` into a dense GPU-readable array; the
shader uses interpolated signed 32-bit `(u,v)` from D-9 to index the correct
region and apply the existing palette formula. The carrier can be an SSBO or a
sampled scalar image. It is not a prepared coloured map texture, does not add
mode to page identity, and does not by itself require a dedicated shader
pipeline.

The alternative keeps per-chunk vertices inside every visible page. It more
directly reuses `makeMapQuads`, but at the finest level a 512-pixel payload has
only 16x16 physical chunks, so it still creates 256 quads per page and couples
page residency to a second geometry cache. It also makes coarse pages choose
between many invisible chunk subdivisions or a second tint representation.

**Owner clarification (2026-08-31):** keep tinting the existing terrain texture
in the shader; do not prepare mode-coloured map textures merely to replace the
working heat-map mechanism.

The owner approved one lazy dense scalar climate field as the shader's
replacement for per-chunk vertex RGB, leaving SSBO-versus-image and
shared-versus-zoom-specific pipeline as measured implementation choices.

### Q-11. Should analytical climate fields become smoothly interpolated?

**Resolved by D-14.**

Yes, the D-13 carrier makes this possible without generating coloured map
pages. The renderer can interpolate the underlying scalar climate values in
cylindrical `(u,v)` space and apply the existing palette after interpolation.
Longitude sampling must wrap; bounded latitude sampling must clamp. Applying
the palette after scalar interpolation avoids blending arbitrary display RGB
and preserves the meaning of continuous fields such as temperature, pressure,
humidity, precipitation, precipitation type fraction, and evaporation.

Sea temperature needs a separate ownership rule at coasts: land keeps the
current grey result, while an ocean fragment interpolates only contributing
ocean samples and renormalizes their weights. Otherwise ordinary bilinear
sampling would bleed ocean heat colours onto land or pull the land sentinel
into coastal water.

Nearest sampling preserves today's climate-region blocks exactly. Bilinear
sampling is an intentional visual change to heat-map output and therefore needs
owner approval despite being enabled naturally by D-13.

The owner approved seam-aware bilinear scalar interpolation in this arc on
2026-08-31. The existing blocks are accurate source-region samples but were a
visual limitation, not the desired player-facing map.

### Q-12. How does finer imagery replace an ancestor fallback?

**Resolved by D-15.**

Fine pages are lazy and camera motion can invalidate a requested viewport
before its full target set arrives. Waiting for every page in one viewport to
be ready gives a coherent single-LOD switch, but panning can repeatedly defer
that switch and hide already-useful detail. Immediate independent replacement
has the lowest residency cost but makes page boundaries visibly pop.

The proposed balance is an independent short cross-fade per page from its
nearest available ancestor. It never waits for an entire moving viewport,
never shows a hole, and confines dual residency to pages actively transitioning
plus the normal in-flight retirement margin. Terrain may temporarily contain
neighboring LODs, but the blend hides the hard tile boundary; cursor and icon
geometry remain single-source overlays and do not fade or move with terrain.
The exact duration is a visual measurement, not an unmeasured constant frozen
here.

The owner approved independent maps-app-style page cross-fades on 2026-08-31:
coarse coverage is immediate, and detail fades in page by page as the camera
sits on the view.

### Q-13. How are coarser terrain pixels reduced from exact fine output?

**Resolved by D-16.**

The finest spatial pages preserve the current 32x32 chunk pixels exactly, but
the pyramid needs one deterministic reduction rule. The current map images are
`R8G8B8A8_UNORM`, their diamond corners contain transparent black, and the zoom
sampler is linear. Straight-channel averaging at transparent boundaries can
darken edge RGB; nearest-neighbor reduction preserves hard pixel-art blocks but
aliases badly in the global map; higher-order filters add ringing, larger
halos, and more implementation/verification cost.

The proposed rule is a deterministic repeated 2x2 box reduction in the current
UNORM numerical colour space, accumulating RGB premultiplied by alpha, averaging
alpha, and converting the non-zero result back to straight RGBA8 with specified
integer rounding. Opaque terrain becomes the ordinary four-pixel mean;
transparent world/page edges retain nearby colour without a black fringe.
Longitude source neighborhoods wrap cylindrically, bounded latitude remains
outside-world transparent, and page gutters are filled from the same neighbor
samples so independently generated pages reduce identically at seams.

The owner approved premultiplied-alpha 2x2 box reduction on 2026-08-31.

### Q-14. What durable identity keys a generated-world map artifact?

**Resolved by D-17.**

Neither current identity is suitable: `WorldIdentity` is optional display
metadata and `WorldPageId` is remapped during load. Two valid designs remain:

- Assign one opaque `GeneratedWorldId` when a world is first created, persist it
  in every descendant save and the artifact manifest, and record generator,
  content, palette, and map-schema compatibility fields separately. Rebuilding
  a missing compatible artifact republishes under the saved ID. Independently
  recreating the same seed does not deduplicate, but identity is simple and
  stable across format evolution.
- Make the ID a SHA-256 over a canonical, version-tagged digest stream of every
  map-generating input and relevant content identity. This deduplicates
  independent equivalent creations and detects source drift intrinsically, but
  adds a new frozen canonicalization contract over the large
  `WorldGenParams`/palette source surface.

The proposal is the opaque ID plus explicit compatibility metadata. D-5 needs
descendant sharing, not speculative cross-creation deduplication, and artifact
checksums already establish byte integrity independently.

The owner approved the opaque generated-world ID on 2026-08-31.

### Q-15. Does this arc establish the shared generated-world library?

**Resolved by D-18.**

The deferred chunk-residency arc proposed the library but has not implemented
it, and it is blocked on this map design. D-5 cannot persist one shared artifact
per generated world without some owner for the registry, atomic publication,
save reference, and reference-aware cleanup.

The proposal is for this arc to create that minimal shared foundation with map
manifest/root/coarse payloads. After this design lands, the deferred document
is reconciled so its future bundle slices extend the existing library with base
chunks and no longer own zoom representation or zoom persistence. The
alternative is a temporary standalone map sidecar/cache, followed by a second
identity, save, cleanup, and migration pass when the deferred arc resumes.

The owner approved the minimal shared library in this arc on 2026-08-31.

### Q-16. Is the fine-page GPU pool shared across visible worlds?

**Resolved by D-19.**

The engine allows up to 16 visible world pages. A separate pool sized to the
one-view planning estimate for each page could consume about 2.2 GiB decoded,
before roots, climate fields, staging, or other game textures. Capping the zoom
map to one visible world would contradict the established multiworld renderer.

The proposed policy is one process-global fixed fine-page pool keyed by
`GeneratedWorldId` and page identity. Every visible world retains complete
root/coarse fallback; active-world and settled-camera demand receives highest
priority, while other visible worlds refine opportunistically within the same
budget. Pool pressure may make a secondary world temporarily coarser, but never
blank or incorrectly mapped. Exact slots and priority aging are measured in the
residency slice rather than guessed here.

The alternative is one fixed pool per visible world, preserving equal fine
detail at the cost of multiplying the GPU bound by up to 16.

The owner approved the shared global pool on 2026-08-31 and clarified that this
arc supports one full world while merely preserving the currently unused
small-auxiliary-world capability. Full multiworld scaling is deferred.

### Q-17. Which page codec and disk-cache quota should ship?

**Status: deliberately open behind D-12's tracked measurement gate.**

The measurement slice captures representative default terrain pages spanning
ocean, varied land, ice, lava, transparency, and seams; compares raw
checksummed pages with PNG and any justified additional codec; and records
encoded size, deterministic bytes, random-access latency, peak decoded memory,
and corruption localization. It also measures realistic revisit behavior to
propose a global fine-page disk quota and any small auxiliary-world share.

The artifact-schema and cache-policy slices stop and return to the owner if the
measurement has not selected a clearly dominant candidate. No issue may infer
a codec or numerical quota merely because it is next in the delivery order.

### Q-18. May runtime residency and transition constants use the same measured
owner gate?

**Resolved by D-20.**

The page payload, root/coarse bounds, and global-pool ownership are settled,
but no live page planner exists from which to measure the final slot count,
prefetch depth, LOD hysteresis, or cross-fade duration. Freezing those numbers
now would turn the approximate 4K page-count calculation into a false
measurement.

The proposal is a dedicated tracked tuning slice after the pure page planner
exists and before the GPU pool/renderer contract freezes. It exercises the
supported 800x600-through-4K envelope, four facings, cylinder seams, camera
motion/settling, LOD overlap, cross-fade pinning, and frames in flight; records
peak decoded/staging bytes and visible transition behavior; then stops for the
owner to approve the slot budget and user-visible thresholds.

The owner approved the explicit measured owner gate on 2026-08-31 after
confirming that it changes no gameplay behavior and exists to prevent guessed
constants from freezing.

### Q-19. How does a pre-`GeneratedWorldId` save enter the library?

**Resolved by D-21.**

Existing compatible saves contain `WorldGenParams` but no D-17 identity. The
proposed migration assigns each legacy generated page a fresh opaque ID during
transactional staging, builds and atomically publishes its mandatory compatible
map artifact, and carries the ID in the live session. The next ordinary save
persists it; loading never rewrites the source save behind the player's back.
If staging fails, no live session or published artifact survives. Loading the
same unchanged legacy save again before resaving can create another ID, but
reference-aware cleanup removes the unreferenced artifact; no display name,
path, or unstable page ID is promoted into storage identity.

The alternative adds a permanent deterministic legacy-only fingerprint or a
path registry solely to reuse an artifact before the player next saves, at the
cost of a second identity rule and rename/copy semantics.

The owner approved fresh-ID-on-successful-legacy-load migration on 2026-08-31
and explicitly retained the practice of migrating every save-format change.

### Q-20. Which runtime residency and transition constants should ship?

**Status: deliberately open behind D-20's tracked measurement gate.**

WML-11 and WML-12 make the page pool and renderer parameterized so the real
path can be measured without presenting guessed values as a product contract.
WML-14 records the evidence for the process-global GPU slot count, prefetch
depth and prioritization, LOD hysteresis, and page cross-fade duration across
the supported viewport/facing/motion envelope. It then stops and returns those
numbers and visible tradeoffs to the owner.

WML-15 may not freeze defaults or cut over from the legacy atlas until the
owner explicitly selects the shipping constants. An implementation may expose
temporary probe-only overrides before then, but an observed smooth run or a
dominant benchmark result is not implicit approval.

## Verification strategy

Every approved representation needs these signals; issue processing will add
the exact commands appropriate to each one-PR slice:

- Pure, allocation-free plan tests cover the full supported world-size range,
  checked arithmetic, page/level inventory, exact dimensions and bytes, and
  injected device limits just below/at/above every boundary.
- The coordinate prerequisite updates the independent
  `Graphics.VertexLayout` literals to a 56-byte stride, location 6
  `FORMAT_R32G32_SINT`, and `solarPage` at byte 52; round-trips positive and
  negative 32-bit `(u,v)` values; proves all four zoom-map facings preserve
  both components beyond the old 16-bit boundary through worldSize 8192; and
  leaves current longitude lighting unchanged. Implementing the future
  seasonal shadow model itself remains outside this arc.
- Existing zoom-terrain/detail parity and worldgen baselines remain green at
  existing successful sizes. Any genuinely changed generated output follows
  the repository's worldgen full-tier and rebaseline contract rather than
  silently updating expectations.
- Renderer tests cover level/page UVs, borders, cylindrical seams, all four
  facings, view culling, fallback selection, and stable
  `pixelToChunkOrigin` results across representation transitions.
- Analytical-mode tests cover all seven existing palette formulae, lazy field
  publication, exact region indexing through signed `(u,v)`, longitude wrap,
  latitude clamp, scalar-before-palette bilinear interpolation, coast-aware sea
  temperature weighting, and identical tint across terrain LOD boundaries.
- Location-map-icon tests continue to prove every lifecycle appearance, live
  state changes, map-mode independence, constant screen size, seam wrapping,
  multi-page ownership, deterministic paint order, and the deliberate
  darkening exception.
- A headless ownership test proves map operations enqueue no detailed chunk
  demand. A source audit or dependency test preserves the one-way
  `World.ZoomMap` to `World.Render.Zoom` boundary.
- A graphical/offscreen probe exercises real Vulkan publication at the
  supported maximum or a practical synthetic equivalent, including clean
  pre-allocation/device-limit refusal rather than a driver error.
- Load verification records map staging wall time, peak decoded CPU bytes, GPU
  decoded bytes, and time to complete global coverage. Under D-5 it also covers
  version/identity mismatch, truncation/checksum failure, atomic publication,
  missing artifact, regeneration policy, and transactional old-session
  preservation.
- The create-world UI, Lua lifecycle, CLI/tool paths, and incoming saves agree
  on the supported maximum; no path can normalize or deserialize a size that
  the map cannot represent.

## Delivery plan

### WML-1. Carry signed 32-bit cylinder coordinates in world vertices

- **Outcome:** Every world vertex transports exact signed 32-bit `(u,v)`
  cylinder coordinates without the current 16-bit wrap boundary.
- **Scope:** Change attribute location 6 to `FORMAT_R32G32_SINT`, move
  `solarPage` to byte offset 52, grow the stride to 56 bytes, update every
  producer/consumer and independent layout assertion, and preserve the current
  longitude-lighting result while removing the shader's packed-half decode.
- **Phase:** 1 — renderer foundation
- **Depends on:** `none`
- **Ordering:** `critical path`
- **Relevant decisions:** D-6, D-9
- **Acceptance signals:** Layout tests agree on format, offset, and stride;
  signed positive and negative coordinates round-trip beyond the old boundary;
  all four map facings remain correct through synthetic worldSize-8192
  coordinates; and current-range longitude lighting is unchanged.
- **Out of scope:** Implementing the future seasonal/directional shadow model,
  changing world generation, or enabling a larger product maximum.
- **Open questions:** `None`

### WML-2. Reject unsafe map image plans before allocation or upload

- **Outcome:** An oversized or arithmetically invalid map image becomes an
  actionable controlled refusal before a large CPU allocation or Vulkan image
  creation, never a driver error.
- **Scope:** Add checked map-image dimension/byte planning, query the actual
  device `maxImageDimension2D`, validate current atlas construction and every
  map upload boundary, and expose reusable validation for later root/page
  images. Keep diagnostics tied to the requested world size, planned image,
  decoded bytes, and device limit.
- **Phase:** 1 — safety foundation
- **Depends on:** `none`
- **Ordering:** `can land first`
- **Relevant decisions:** D-4, D-7, D-11
- **Acceptance signals:** Pure tests inject limits immediately below, at, and
  above planned dimensions; multiplication overflow and invalid conversions
  fail before allocation; the current 1024 atlas path refuses cleanly on a
  16384 limit; and upload performs a second device-aware validation.
- **Out of scope:** Making 1024 render successfully, choosing LOD pages, or
  changing the advertised maximum.
- **Open questions:** `None`

### WML-3. Persist an opaque generated-world identity with save migration

- **Outcome:** Fresh and loaded generated worlds carry a stable opaque
  `GeneratedWorldId`, and the save format preserves it without conflating it
  with display identity, page identity, slot names, or filesystem paths.
- **Scope:** Introduce the opaque type and generation rule, add it to the
  current save model, bump the save format, assign fresh IDs to compatible
  legacy generated pages during transactional staging, retain the old source
  save unchanged, and update the tracked compatibility fixtures and migration
  inventory. The later artifact-aware load slice completes publication under
  that staged ID once the library and map format exist.
- **Phase:** 1 — identity and compatibility foundation
- **Depends on:** `none`
- **Ordering:** `critical path`
- **Relevant decisions:** D-17, D-21
- **Acceptance signals:** New-save round trips preserve the opaque ID; legacy
  fixtures stage a fresh non-display-derived ID and persist it only on the next
  ordinary save; failed loads do not rewrite source saves; and the repository's
  normal save-version/compatibility gates cover the change.
- **Out of scope:** Artifact directories, map payload schemas, cross-creation
  deduplication, or deterministic legacy fingerprints.
- **Open questions:** `None`

### WML-4. Establish the shared generated-world library lifecycle

- **Outcome:** One generated foundation has one referenceable, atomically
  published library directory that current saves can share and unreferenced
  data can be cleaned safely.
- **Scope:** Add the minimal registry and directory lifecycle keyed by
  `GeneratedWorldId`, atomic temporary-to-final publication, current-save
  references, reference-aware cleanup, and crash/orphan recovery. Define a
  payload-neutral boundary that the map format can fill without coupling the
  library to renderer structs or detailed chunks.
- **Phase:** 2 — persistent library foundation
- **Depends on:** WML-3
- **Ordering:** `critical path`
- **Relevant decisions:** D-5, D-8, D-17, D-18, D-21
- **Acceptance signals:** Multiple descendant saves resolve one library entry;
  interrupted publication never exposes a partial final entry; cleanup retains
  referenced entries and removes proven-unreferenced/abandoned temporary data;
  and failures leave the previous live session and registry valid.
- **Out of scope:** Map manifests or pixels, fine-page cache policy, base-chunk
  records, detailed-chunk streaming, and a second map-only sidecar.
- **Open questions:** `None`

### WML-5. Generate deterministic spatial pyramid pages

- **Outcome:** A pure, bounded planner and generator can reproduce exact
  finest-detail map pixels and deterministic root/coarse pages without building
  a whole-world finest atlas or per-chunk render vector.
- **Scope:** Define canonical parity-compressed cylindrical level/page
  addressing through worldSize 8192; the 512x512 payload and seam gutters;
  spatial composition of existing 32x32 chunk diamonds; the one-chunk source
  halo; repeated premultiplied-alpha 2x2 reduction with a specified integer
  rounding rule; wrap/clamp behavior; and a streamable root/coarse inventory.
  Keep the implementation under `World.ZoomMap` and sourced only from
  `WorldGenParams`.
- **Phase:** 2 — pure map representation
- **Depends on:** `none`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2, D-3, D-4, D-7, D-11, D-16
- **Acceptance signals:** Pure tests cover every level/page boundary and checked
  byte plan through 8192; finest-page goldens match current chunk pixels at
  successful sizes; independently generated neighboring pages agree across
  gutters, parity seams, longitude wrap, and bounded latitude; and generation
  neither imports `World.Render` nor consults `LoadedChunk`.
- **Out of scope:** Filesystem encoding, Vulkan resources, runtime requests,
  climate tinting, or activation in fresh/load paths.
- **Open questions:** `None`

### WML-6. Measure map-page codecs and disk-cache budgets

- **Outcome:** Codec and fine-cache decisions are backed by a reproducible,
  tracked corpus and measurements instead of assumptions.
- **Scope:** Build a representative page corpus from declared seeds,
  coordinates, levels, and terrain categories; compare raw checksummed bytes,
  PNG via the existing dependency, and only justified additional candidates;
  record encoded size, deterministic-byte behavior, encode/decode latency,
  peak decoded memory, corruption localization, and platform/date context; and
  model realistic revisit/eviction traces for a bounded disk quota and
  auxiliary-world accounting. Present the results and stop for owner choice.
- **Phase:** 3 — format evidence gate
- **Depends on:** WML-5
- **Ordering:** `critical path`
- **Relevant decisions:** D-12
- **Acceptance signals:** The corpus definition and measurement procedure are
  reproducible from tracked inputs; results distinguish measured values from
  assumptions; every candidate is evaluated on the same pages and checks; and
  no runtime codec, quota, or eviction default changes in this slice.
- **Out of scope:** Selecting a shipping codec or quota without owner approval,
  defining the artifact schema, or implementing a production cache.
- **Open questions:** Q-17; this slice supplies the evidence and then stops.

### WML-7. Define the versioned map-artifact format

> **Deferred (2026-09-02).** Q-17 is unresolved. #2303 (WML-6) is filed but has
> not run, so no page codec, fine-page disk quota, or multiworld cache
> accounting has been measured or selected. This slice's own scope begins
> "After Q-17 is explicitly resolved", and Q-17 forbids any issue inferring a
> codec or quota from delivery order. Processable once #2303 has reported its
> measurements and the owner has explicitly selected all three values.

- **Outcome:** Mandatory root/coarse pages and reproducible fine-cache pages
  have one versioned, integrity-checked, world-qualified storage contract.
- **Scope:** After Q-17 is explicitly resolved, define manifest compatibility
  fields, generator/content/palette/map-schema versions, level/page inventory,
  dimensions, chosen encoding, per-payload checksums, required-versus-cache
  classification, cylindrical addressing, and deterministic serialization.
  Integrate payload read/write primitives with the atomic library boundary.
- **Phase:** 3 — persistent map format
- **Depends on:** WML-4, WML-5, WML-6
- **Ordering:** `critical path`
- **Relevant decisions:** D-5, D-7, D-8, D-12, D-16, D-17, D-18
- **Acceptance signals:** Golden manifests and pages round-trip byte-for-byte;
  truncation, checksum damage, unknown versions, incompatible identities, and
  missing required entries are distinguished; fine entries remain safely
  discardable; and format dimensions pass the checked planner.
- **Out of scope:** Generating artifacts during worldgen, load recovery,
  serving pages, or embedding live icons/render state.
- **Open questions:** Q-17; implementation must stop and ask if the owner has
  not selected the codec, quota, and multiworld cache accounting.

### WML-8. Publish mandatory map artifacts during world generation

- **Outcome:** Fresh world generation atomically publishes a complete bounded
  root/coarse map artifact under its `GeneratedWorldId` without constructing a
  mandatory all-world finest pyramid.
- **Scope:** Stream pure fine output through the reduction tree, publish the
  manifest and required root/coarse pages through the shared library, derive
  the preview from the new complete data, and record generation time and peak
  incremental memory. During the transition, keep the separately produced
  legacy atlas available to the still-active renderer until WML-15 removes it;
  this slice therefore establishes the new artifact but does not yet deliver
  the final peak-memory reduction.
- **Phase:** 4 — generation integration
- **Depends on:** WML-7
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-3, D-5, D-7, D-16, D-18
- **Acceptance signals:** Fresh generations publish a valid required artifact
  atomically; failure leaves no visible partial entry; required output is
  deterministic and globally complete; the new producer never reads gameplay
  chunks; and no all-world fine artifact is required or prewarmed by default.
- **Out of scope:** Removing the legacy atlas, loading/recovering artifacts,
  lazy fine-page service, or choosing a speculative fine prewarm policy.
- **Open questions:** `None`

### WML-9. Load and recover map artifacts transactionally

- **Outcome:** Save loading stages valid complete map coverage from the shared
  library, repairs compatible missing/corrupt mandatory data, and preserves the
  old live session on every unrecoverable failure.
- **Scope:** Resolve the saved `GeneratedWorldId`, validate the manifest and
  mandatory pages, treat fine corruption as a cache miss, compatibility-build
  and atomically republish missing/corrupt required data, and reject unavailable
  or incompatible generators with an actionable diagnostic. Complete D-21 by
  publishing the mandatory artifact under a legacy save's staged fresh ID
  before session publication. Keep the legacy render staging in parallel until
  WML-15 switches consumers.
- **Phase:** 4 — load integration
- **Depends on:** WML-3, WML-4, WML-7, WML-8
- **Ordering:** `critical path`
- **Relevant decisions:** D-5, D-8, D-17, D-18, D-21
- **Acceptance signals:** Warm staging reads required artifacts without
  regenerating all fine pages; missing/corrupt fine entries do not block;
  compatible mandatory damage recovers atomically; incompatible identity or
  version fails before session publication; failed legacy migration leaves no
  final artifact; and the source legacy save remains untouched.
- **Out of scope:** Removing the old atlas load path, fine-page request
  scheduling, or degrading to blank/incomplete global coverage.
- **Open questions:** `None`

### WML-10. Serve lazy fine pages through bounded caches

- **Outcome:** View-driven map work can obtain exact fine pages without any
  detailed gameplay-chunk request, while CPU work and disk use stay bounded
  and stale views cannot publish incorrect mappings.
- **Scope:** After Q-17 is resolved, implement world-qualified page requests,
  pure `WorldGenParams` generation with the required halo, selected encoding,
  checksum validation, the approved bounded disk-cache accounting/eviction
  policy, bounded decode/generation work, view epochs, prioritization, and
  cancellation or deprioritization of obsolete work. Expose completed immutable
  payloads to the renderer without allocating permanent texture handles.
- **Phase:** 5 — fine-page service
- **Depends on:** WML-4, WML-5, WML-7, WML-9
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-3, D-5, D-11, D-12, D-15, D-17, D-19
- **Acceptance signals:** Cache hit, miss, corruption, eviction, cancellation,
  and repeated-request tests are deterministic; page keys cannot collide
  across generated worlds; all work stays within configured CPU/disk bounds;
  and a headless ownership test observes no detailed chunk demand.
- **Out of scope:** GPU allocation, terrain draw selection, full multiworld
  fairness, permanent fine artifacts, or changing generated terrain truth.
- **Open questions:** Q-17; implementation must stop and ask if codec, quota,
  and cross-world cache accounting are not owner-approved.

### WML-11. Reuse a fixed global GPU page pool safely

- **Outcome:** Addressable coarse/fine map pages reuse a fixed process-global
  set of texture handles and GPU slots without monotonic handle exhaustion or
  in-flight image reuse.
- **Scope:** Implement parameterized slot ownership, page-key-to-slot mapping,
  asynchronous upload publication, transition pinning, eviction, and
  frames-in-flight retirement. Qualify mappings by `GeneratedWorldId`, retain
  the complete root separately, apply WML-2's device checks to every page, and
  make slot/prefetch-related values probe-configurable for WML-14 rather than
  freezing shipping constants here.
- **Phase:** 5 — GPU residency foundation
- **Depends on:** WML-2, WML-5
- **Ordering:** `critical path`
- **Relevant decisions:** D-3, D-7, D-11, D-15, D-19, D-20
- **Acceptance signals:** Synthetic churn reuses a fixed handle set; reassigned
  slots become visible only after upload completion; old images survive all
  sampling frames and active fades; pressure evicts pages rather than roots;
  and multiple world-qualified keys never alias one live mapping.
- **Out of scope:** Selecting the production slot count, page-request policy,
  rendering terrain, or guaranteeing equal detail among multiple large worlds.
- **Open questions:** Q-20; this parameterized foundation may land before the
  final numbers are chosen.

### WML-12. Render paged LOD terrain with fallback and cross-fades

- **Outcome:** A selectable paged render path shows immediate complete coarse
  coverage, refines current pages independently, and preserves established map
  geometry and overlays while the legacy renderer remains available for parity
  comparison.
- **Scope:** Draw canonical spatial page rasters with O(1) geometry per page
  under all facings; select LOD from logical framebuffer coverage; request the
  viewport plus parameterized prefetch; fall back per page to the nearest ready
  ancestor; perform D-15 cross-fades with correct pinning; and preserve
  `pixelToChunkOrigin`, cylinder seams, culling, live discovery/location icons,
  lighting inputs, and constant overlay behavior. Keep the new path behind a
  development/probe selector until WML-15.
- **Phase:** 6 — parallel renderer
- **Depends on:** WML-1, WML-2, WML-8, WML-9, WML-10, WML-11
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2, D-3, D-7, D-11, D-15, D-19, D-20
- **Acceptance signals:** Root-only, partial-child, completed-child, eviction,
  seam, facing, motion, and abandoned-transition cases never show a hole or
  stale page; cursor-to-chunk results match the legacy path; exact finest pages
  match current terrain pixels; and icons remain single, live, unfaded overlays.
- **Out of scope:** Analytical climate-mode migration, production cutover,
  legacy-atlas deletion, or freezing Q-20 constants.
- **Open questions:** Q-20; probe overrides remain temporary until the owner
  selects shipping values after WML-14.

### WML-13. Preserve and smooth analytical climate map modes

- **Outcome:** All seven analytical map modes tint paged base terrain lazily in
  the shader and present smooth scalar climate fields without mode-specific
  terrain pyramids or an all-world per-chunk geometry cache.
- **Scope:** Materialize only the selected dense scalar field from
  `WorldGenParams`, publish it atomically to a GPU-readable scalar carrier,
  index it from signed `(u,v)`, interpolate scalar-before-palette with longitude
  wrap and latitude clamp, apply coast-masked/renormalized sea temperature,
  and preserve the existing palettes and default-mode base terrain. Keep live
  icons and solar/seasonal inputs separate.
- **Phase:** 6 — analytical map compatibility
- **Depends on:** WML-1, WML-12
- **Ordering:** `critical path`
- **Relevant decisions:** D-2, D-9, D-10, D-13, D-14
- **Acceptance signals:** Every analytical formula and lazy mode switch is
  covered; no coloured mode page is generated or cached; bilinear values agree
  across page LODs and the cylinder seam; land and sea temperature do not bleed
  across coasts; and field publication cannot expose partial data.
- **Out of scope:** Changing generated climate samples, making interpolation
  authoritative outside the player map, retaining/prewarming multiple fields,
  or implementing future seasonal shadows.
- **Open questions:** `None`

### WML-14. Measure runtime residency and transition tuning

- **Outcome:** Shipping residency and transition constants can be chosen from
  tracked measurements of the actual paged renderer rather than the current
  4K planning estimate.
- **Scope:** Add a reproducible probe and evidence report covering the supported
  800x600-through-4K envelope, all facings, cylinder seams, continuous motion,
  camera settling, LOD boundaries, independent cross-fades, transition and
  frames-in-flight pinning, upload staging, decoded CPU bytes, and decoded GPU
  residency. Exercise candidate slot counts, prefetch/prioritization,
  hysteresis, and fade durations through probe-only overrides, present the
  tradeoffs, and stop for owner choice.
- **Phase:** 7 — runtime evidence gate
- **Depends on:** WML-12, WML-13
- **Ordering:** `critical path`
- **Relevant decisions:** D-15, D-19, D-20
- **Acceptance signals:** The procedure and inputs are tracked and repeatable;
  results label platform, viewport, facing, motion, and measured versus assumed
  values; peaks include transition/staging/in-flight overlap; visible artifacts
  are documented; and the slice changes no production defaults.
- **Out of scope:** Selecting constants without owner approval, production
  cutover, broad GPU performance work, or full multiworld fairness.
- **Open questions:** Q-20; this slice supplies the evidence and then stops.

### WML-15. Cut over to the paged map and retire the legacy atlas

- **Outcome:** Fresh generation, save loading, and rendering use the bounded
  paged representation exclusively, with owner-approved runtime constants and
  no all-world RGBA8 atlas or all-world finest render vector.
- **Scope:** After Q-20 is explicitly resolved, freeze the approved global pool
  size, prefetch/prioritization, LOD hysteresis, and fade duration; make the
  paged path the production path; remove `buildZoomAtlas`, atlas upload/staging,
  and the all-world `BakedZoomEntry`/finest-pixel handoff; stop rebuilding the
  legacy buffer during warm loads; retain complete fallback, exact finest
  terrain, live overlays, analytical modes, and device validation.
- **Phase:** 8 — production cutover
- **Depends on:** WML-14
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-2, D-3, D-5, D-7, D-11, D-15, D-19, D-20
- **Acceptance signals:** No fresh or load path allocates/uploads the legacy
  atlas; CPU/GPU fine residency stays within the approved fixed budgets while
  panning and fading; warm loads obtain immediate mandatory coverage without
  all-world fine regeneration; existing-size visual/interaction parity and all
  failure/recovery cases remain green.
- **Out of scope:** Enabling sizes above the product maximum, changing the map
  artifact format, base-chunk streaming, or multiworld scaling.
- **Open questions:** Q-20; this slice must stop and ask unless the owner has
  approved the shipping constants recorded by WML-14.

### WML-16. Gate supported world sizes and reconcile deferred streaming

- **Outcome:** Every creation/load/tool entry point consistently guarantees
  worldSize 1024, rejects unsupported larger worlds before expensive work, and
  the deferred chunk-residency design extends rather than contradicts the new
  map/library ownership.
- **Scope:** Enforce the initial product maximum in normalization, create-world
  UI, Lua lifecycle, CLI/tooling, and incoming-save validation; exercise a real
  1024 fresh/load/map flow on declared reference hardware; keep pure map
  address/format/overflow tests through 8192; document broader enablement gates;
  and revise `docs/chunk_residency_streaming_design.md` so its measurements,
  deferral, Arc B contract, and slices treat the shared library as established,
  leave zoom persistence/rendering to this arc, and retain #2001's independent
  canonical-chunk-identity role.
- **Phase:** 9 — support envelope and handoff
- **Depends on:** WML-15
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-3, D-4, D-6, D-8, D-18, D-19
- **Acceptance signals:** A real 1024 world creates, saves, loads, opens, pans,
  refines, and reports bounded map memory without driver failure; every public
  entry path agrees on the maximum; larger inputs fail before worldgen/map
  allocation with an actionable message; synthetic 8192 plans remain valid;
  and the deferred design no longer proposes a second zoom artifact, identity,
  or library lifecycle.
- **Out of scope:** Product support above 1024, proving full worldgen scaling to
  8192, detailed/base-chunk streaming, implementing #1997, changing #2001, or
  designing multiple simultaneous full-size worlds.
- **Open questions:** `None`
