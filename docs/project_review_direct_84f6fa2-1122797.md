# Project Review Findings: direct commits `84f6fa2`–`1122797`

These entries record focused evidence from a broad current-survivor inventory
of the repository's terminal pre-PR history. There are no merged pull requests
before PR #14, whose first commit has `84f6fa27` as its parent, so the
inventory is named by exact commit range rather than inventing a PR range. It
spans 191 first-parent commits, newest-first from `84f6fa27` (2026-02-02)
through the initial commit `1122797e` (2024-12-22), checked against
`master@4c2a26d2e707`. This inventory is triage evidence, not a granular
commit-by-commit cursor claim; findings move into bounded range reports as
those batches are reviewed.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an
issue · `[deferred]` blocked on a concrete precondition

A current-survivor scan found 12,534 live lines attributed to 132 of the 191
commits across 1,380 behavior-bearing tracked files; 5,079 of those lines are
the vendored `cbits/stb_truetype.h`. The higher-risk surviving clusters were
then traced through their current descendants, tests, tracker history, and
pending findings reports. Existing owners were not duplicated: shell/Lua
findings live in `docs/lua_script_findings.md`; texture slot-zero and stable
handle-table exhaustion are PRR-1/PRR-2 in
`docs/project_review_292-281.md`; worker lifecycle is issue #1147; atlas-alias
invalidation is issue #1281; and issue #1007 deliberately retained but did not
wire the asset teardown surface. The focused logger specs passed all 10
examples. No lower-capacity Vulkan device, forced descriptor-slot exhaustion,
graphical/offscreen session, full headless suite, full probe sweep, or
`make ci` was run. The two logger concerns from the first granular batch moved
to `docs/project_review_direct_84f6fa2-a3f4481.md`. Three candidate concerns
from older direct history remain here until their owning batches are reviewed.

## Status

- [x] PRR-1. Devices below 16,384 descriptors pass a gate their fixed-size shaders cannot satisfy — [#1689]
- [x] PRR-2. Descriptor-slot exhaustion is published as successful texture loading — [#1690]
- [x] PRR-3. Loaded disk-texture cleanup closures are unreachable from runtime and shutdown — [#1691]

## 1. Bindless shader/layout capacity

### [#1689] PRR-1. Devices below 16,384 descriptors pass a gate their fixed-size shaders cannot satisfy

> **Captured note:** The capability gate accepts a device with a post-reservation
> descriptor count as low as 256 and sizes the descriptor-set
> binding to that device-specific count. Both fragment shaders nevertheless
> declare a fixed `textures[16384]` array. Vulkan requires a statically sized
> decorated descriptor array to be no larger than its matching binding, so the
> advertised lower-capacity path cannot construct a valid shader interface.

**Verification:** Verified as a static Vulkan-contract violation. The current
Vulkan specification's descriptor-set interface rule states that a decorated
array's size must be no larger than the number of descriptors in the binding.
The repository can produce binding counts from 256 through 16,383 while both
statically used arrays remain 16,384. A qualifying lower-capacity physical
device was not available, so the resulting validation message or driver
failure was not reproduced. The invalid interface is not driver-dependent.

**Evidence:**

- `src/Engine/Graphics/Vulkan/ShaderCode.hs:143-166,311-338` compiles both the
  world and UI fragment shaders with
  `uniform sampler2D textures[${maxBindlessTextures}]` and performs
  non-uniform runtime indexing into those arrays.
- `src/Engine/Graphics/Vulkan/Texture/Limits.hs:13-30` defines
  `maxBindlessTextures = 16384`, calls it a fixed upper bound, and explicitly
  says the actually allocated slot count may be lower.
- `src/Engine/Graphics/Vulkan/Capability.hs:131-144` returns
  `BindlessTextures cappedSlots` for any otherwise capable device with at least
  256 post-reservation slots.
- `src/Engine/Graphics/Vulkan/Texture/System.hs:31-45` takes the minimum of
  16,384, that reported capacity, and the requested capacity. It passes the
  possibly smaller result as `bcMaxTextures`.
- `src/Engine/Graphics/Vulkan/Texture/Bindless.hs:190-219` creates the combined
  image-sampler binding with `descriptorCount = bcMaxTextures config`; it does
  not use a variable descriptor count.
- The Vulkan specification's [descriptor-set interface
  rule](https://docs.vulkan.org/spec/latest/chapters/interfaces.html#interfaces-resources-descset)
  requires the decorated array size to fit the binding. The Vulkan Guide's
  [descriptor-array example](https://docs.vulkan.org/guide/latest/descriptor_arrays.html)
  likewise pairs a fixed four-element shader array with `descriptorCount = 4`
  and distinguishes it from a runtime descriptor array.
- Direct commit `a7abb731` introduced both `textures[16384]` and the
  device-derived `actualMax` clamp. Issue #975 / PR #1061 later made 16,384 a
  shared definition but expressly preserved the lower clamp; issue #1282
  covers feature-query correctness, not this count mismatch. Searches of all
  issues and pending findings reports found no current owner.

**Handoff context:**

- **Current behavior:** A Vulkan 1.2 device with all three required indexing
  features and 256–16,383 usable update-after-bind sampled-image descriptors is
  classified as supported. Its descriptor layout is smaller than the fixed
  array used by each fragment shader, so graphical pipeline creation is outside
  Vulkan's validity contract rather than providing the advertised bounded
  capacity.
- **Expected behavior:** Every accepted device gets matching shader and layout
  descriptor counts. Plausible designs are to require the full fixed count, to
  make the shader array genuinely device-sized, or to adopt a supported runtime
  array/variable-count design; the processor should choose after checking the
  MoltenVK constraint rather than assuming the last option is portable.
- **Scope and constraints:** Cover both world and UI fragment shaders, the
  physical-device selection gate, the post-reservation count, and pipeline
  construction. Preserve the single-definition protection from #975 and the
  exact feature contract from #1282. `VARIABLE_DESCRIPTOR_COUNT` is currently
  avoided specifically for MoltenVK, so enabling it is not a mechanical fix.
- **Remaining uncertainty:** No supported GPU reporting fewer than 16,384
  usable slots was exercised, so the exact user-visible failure point is not
  measured. The shader/layout mismatch itself is fully specified and verified.

## 2. Slot-exhaustion outcome

### [#1690] PRR-2. Descriptor-slot exhaustion is published as successful texture loading

> **Captured note:** The bindless allocator reports capacity exhaustion as
> `Nothing`, but three live upload paths continue as if registration succeeded.
> Disk textures become `AssetReady` and emit `LuaAssetLoaded`; preview and zoom
> textures replace the previous generation and publish their new handles. None
> of those handles has a descriptor slot.

**Verification:** The failure path is statically verified end to end.
`allocateSlot` returns `Nothing` at capacity, `registerTexture` propagates it
without changing the bindless system, and the three callers described below do
not gate success publication on the result. The repository's blood-texture
uploader demonstrates the opposite, safe behavior by immediately releasing the
new GPU objects and declining to publish on `Nothing`. Actual exhaustion was
not forced on a GPU, so this is marked by an unmeasured trigger rather than an
uncertain control-flow conclusion.

**Evidence:**

- `src/Engine/Graphics/Vulkan/Texture/Slot.hs:49-76` returns `Nothing` when no
  free or fresh slot remains. `Texture/Bindless.hs:330-365` propagates
  `(Nothing, system)` without a descriptor write or handle-map insertion.
- `src/Engine/Scripting/Lua/Message/Texture.hs:228-281` logs a warning when
  registration returns `Nothing`, but still constructs and stores the atlas,
  writes `AssetReady`, records its dimensions, queues `LuaAssetLoaded`, and
  includes it in the returned loaded set.
- `src/Engine/Scripting/Lua/Message/WorldTexture.hs:123-176,245-297` discards
  the `Maybe` returned by both `registerPinnedTexture` calls. Each branch then
  disposes the previous generation, stores the unregistered replacement, and
  publishes a preview-ready message or zoom-atlas state.
- `src/World/Render/BloodQuads.hs:181-204` checks the same `Maybe`; on failure it
  runs the image/view cleanup and does not publish a blood texture. This is a
  live precedent for the expected ownership behavior.
- `src/Engine/Graphics/Vulkan/Capability.hs:131-144` permits a production
  descriptor-array capacity as low as 256; the allocator separately keeps slot
  zero reserved, making exhaustion much closer than the nominal 16,384 ceiling
  on a conforming lower-capacity device.
- Direct commit `a7abb731` established the contract and already continued from
  failed bindless registration to an `AssetLoaded` atlas and successful asset
  id. Later loader reorganizations preserved that outcome, and the preview/zoom
  callers repeated it. No test mentions slot exhaustion, `allocateSlot`, or
  registration failure. Tracker and report searches found no owner; the
  65,536-entry stable handle-table exhaustion in
  `docs/project_review_292-281.md` is a distinct resource limit.

**Handoff context:**

- **Current behavior:** An exhausted disk load produces a warning followed by
  the normal ready callback for a handle that resolves to no live slot. Preview
  and zoom uploads can additionally destroy a still-renderable old generation
  before publishing an unusable replacement. The newly created GPU objects
  remain owned by cleanup records despite never becoming shader-addressable.
- **Expected behavior:** A `Nothing` registration result never crosses a
  success boundary. The new view/image/sampler reference is released, disk
  assets report a defined failure or retryable state, and transient uploads
  retain the old generation unless a replacement registered successfully.
- **Scope and constraints:** Preserve render-thread Vulkan ownership, stable
  handles, slot-zero fallback semantics, pinned-sampler behavior, and the blood
  path's existing success case. A pure small-capacity allocator seam should
  cover exhaustion without requiring thousands of real images or a GPU test.
- **Remaining uncertainty:** The repository does not specify whether ordinary
  asset loads should fail, retry after an unload, or degrade to the undefined
  texture when capacity is exhausted. It does establish that a success callback
  and a ready handle currently promise rendering that did not become possible.

## 3. Disk-texture lifetime

### [#1691] PRR-3. Loaded disk-texture cleanup closures are unreachable from runtime and shutdown

> **Captured note:** Every disk atlas owns an explicit closure that destroys
> its image view, image, and device memory. The only functions that invoke
> those closures have no callers, and the main shutdown sequence tears down
> other Vulkan resources without invoking either function.

**Verification:** Verified by repository-wide call-site and ownership tracing.
`allocResource'IO` only returns a manual action; it does not register that
action in `vulkanCleanup`. The live loader stores the action in
`TextureAtlas.taCleanup`. `unloadAsset` and `cleanupAssetManager` are the only
consumers, both are definitions/exports only, and `shutdownEngine` does not call
them. This proves the explicit release path is unreachable. No long-running
GPU-memory measurement or validation-layer shutdown capture was performed.

**Evidence:**

- `src/Engine/Scripting/Lua/Message/Texture.hs:128-160,228-255` creates each
  image with a returned manual cleanup, creates a view with another cleanup,
  and stores `cleanView >> tupCleanImage prep` in `taCleanup`.
- `src/Engine/Core/Resource.hs:43-53` shows that `allocResource'IO` returns a
  plain `IO ()`; unlike the engine's registered cleanup stack, nothing runs it
  unless its caller retains and invokes it.
- `src/Engine/Asset/Manager.hs:1-23,81-178` documents the retained teardown as
  the only consumer, invokes `taCleanup` from final unload and bulk cleanup,
  and states that nothing currently calls either entry point. Repository-wide
  production/test search confirms only the two definitions and export names.
- `src/Engine/Loop/Shutdown.hs:68-108` waits for the device, destroys the last
  transient preview/zoom generations, runs `vulkanCleanup`, clears the sampler
  cache, and destroys cached buffers. It never traverses
  `apTextureAtlases` or invokes the asset manager.
- `docs/engineenv_capability_inventory.md`'s `assetPoolRef` row records the same
  live ownership gap. Issue #1007 intentionally retained this teardown and
  declared wiring it out of scope; open issue #1281 makes runtime `unloadAsset`
  unsafe until alias invalidation is fixed. Neither issue owns shutdown wiring.
- The load and cleanup model predates bindless textures (`80d99cf2`,
  `7e955c1f`) and survived through direct commit `a7abb731`; at the end of this
  review range, as now, the teardown functions had no external caller.

**Handoff context:**

- **Current behavior:** Disk-loaded atlases remain allocated for the lifetime
  of the Vulkan device even when no longer needed, and their explicit child
  destruction is skipped at shutdown. Process/device destruction ultimately
  reclaims driver memory, but repeated distinct loads cannot release it during
  the session and validation cannot observe orderly child teardown.
- **Expected behavior:** Every atlas cleanup has one reachable owner and runs
  exactly once before the Vulkan device is destroyed. Runtime unload, if
  supported, also unregisters every shader-visible handle before destroying
  its image.
- **Scope and constraints:** Shutdown wiring can use the existing single
  device-idle barrier and must occur before device teardown. Runtime unload is
  dependency-gated by #1281's alias-safe release contract; do not expose the
  present `unloadAsset` merely to make this finding look wired. Avoid double
  destruction with `vulkanCleanup` and preserve view-before-image ordering.
- **Remaining uncertainty:** The practical memory growth depends on how many
  distinct disk textures a session or mod loads, and no validation warning was
  captured. The missing call path and ownership gap are fully verified.
