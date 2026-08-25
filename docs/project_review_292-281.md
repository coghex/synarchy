# Project Review Findings: PRs #292–#281

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #292, #291, #290, #288, #287, #289, #284, #283, #285, #282, #280, and #281 — for later one-at-a-time disposition. The first-parent window contains exactly those twelve PR merges and no direct commits.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

PR #291's location definitions have since grown validated spatial, icon, content, and naming contracts; #290/#283/#280's keybinding model still merges old configuration over new defaults and routes camera actions through exact physical-key matching; #288's river extension now applies to every player-facing world size with the later #811 caldera guard; #287's per-world save schema was superseded by the transactional multi-world component codec; #289's generic HUD panel now has owner-keyed suppression and restoration; #284's hard-rock path retains seam-aware wet-neighbour filtering; #285/#282's invalidation workaround was deliberately superseded by #292's draw-time indirection; and #281's obsolete Lua structure harness remains absent while real structure/building probes cover the live APIs. Focused checks passed for input bindings (21/21), generic-info-panel suppression (1/1), and the headless texture-fallback value contract (1/1). No full headless suite, graphical session, worldgen tier, world check, baseline capture, behavior probe, or `make ci` was run. Two non-duplicate concerns remain, both in PR #292's stable-handle namespace; the capacity finding is deliberately retained despite its long-session reachability.

## Status

- [x] PRR-1. Texture handle zero is both the undefined sentinel and the first allocated asset — [#1696]
- [x] PRR-2. The fixed handle table silently expires after 65,536 allocations — [#1699]

## 1. Texture-handle zero ownership

### [#1696] PRR-1. Texture handle zero is both the undefined sentinel and the first allocated asset

> **Captured note:** Reserve texture-handle id 0 exclusively for the undefined/no-texture sentinel, or replace it with another representation that cannot be allocated to a real asset. Shader-side indirection must preserve the existing `TextureHandle 0` contracts instead of allowing the first uploaded texture to redefine what every zero handle means.

**Verification:** Verified structurally; the exact visible tint was not measured in a GPU session. The asset allocator's first result and the renderer's missing-texture sentinel are the same value. Registering that first real texture writes its nonzero bindless slot into table entry 0, after which the fragment shaders resolve every sentinel handle 0 to the real texture. A current world producer passes zero specifically to request the default face map, so this is not only a dormant type-level collision.

**Evidence:**

- Issue #286 / PR #292 changed every relevant vertex field from a bindless slot to a stable `TextureHandle` id and moved the handle→slot lookup into both fragment shaders. The PR states that sprites without a directional face map use an unregistered id so the shader falls back to the default face map.
- `src/Engine/Asset/Types.hs:35-52` initializes `apNextTextureHandle` to 0. `src/Engine/Asset/Manager.hs:54-57` returns the current counter before incrementing it, so the first real texture request receives `TextureHandle 0`.
- `src/Engine/Graphics/Vulkan/Texture/Slot.hs:27-46` separately reserves **bindless slot** 0 for the undefined checkerboard and starts real slot allocation at 1. Handle ids and slot ids are distinct namespaces; giving a real texture handle 0 therefore does not place it in undefined slot 0.
- `src/Engine/Graphics/Vulkan/Texture/Bindless.hs:335-363` allocates a real slot, writes `handleToSlot[hid]`, and inserts the handle into `btsHandleMap`. For the first asset this writes table entry 0 to slot 1 or later.
- `src/Engine/Graphics/Vulkan/Texture/Handle.hs:21-30` nevertheless defines the undefined bindless handle with `bthHandle = TextureHandle 0`. Other live types keep the same convention: `src/World/Flora/Types.hs:150-153` says harvested handle 0 means no depleted art, and `src/UI/Types.hs:336-342` uses 0 as an unset separator texture.
- `src/World/Render/FloraQuads.hs:88-89` passes `lookupFmSlot (TextureHandle 0)` for every flora quad's absent directional face map. `src/World/Render/Quads.hs:68-73` now sends that raw handle id to the shader.
- `src/Engine/Graphics/Vulkan/ShaderCode.hs:194-203,224-228` resolves the base and face-map ids through `handleToSlot`. The default-face-map fallback fires only when the **resolved slot** is 0. Once table entry 0 names the first real texture's slot, flora samples that arbitrary texture as its face map instead of `fragDefaultFaceMapSlot`.
- `src/Engine/Scripting/Lua/API/YamlTextures.hs:178-189` uses the same allocator for content textures, while `:323-343` returns literal `TextureHandle 0` for an omitted harvested texture. The two values can collide inside one freshly initialized asset pool.
- `test-headless/Test/Headless/Asset/TextureFallback.hs:103-157` explicitly claims that `TextureHandle 0` reaches the undefined/magenta bindless slot. The focused example passed, but it stops after asserting the Haskell handle value and performs no GPU registration or shader lookup; it therefore pins the intended contract while missing the indirection-table contradiction.
- Startup ordering does not make a particular texture a safe substitute. `scripts/startup_loader.lua:141-175` queues several content registries before structural face maps, and `src/Engine/Scripting/Lua/API/Core.hs:448-466` documents its `listDirectory` order as OS-dependent. Fonts and alternate boot profiles also draw from the same counter. The identity mapped into entry 0 is not an undefined-texture invariant.
- Tracker and pending-report searches for zero texture handles, undefined-handle sentinels, and handle 0 found no owner. Closed #983 documents slot 0 as the undefined-texture failure value but does not address stable handle id 0; open #1281 concerns alias invalidation during teardown and is independent.

**Handoff context:**

- **Current behavior:** The first successfully registered texture changes GPU meaning for every `TextureHandle 0` from undefined/default to that texture's live slot. CPU paths that explicitly filter zero remain safe, but shader-side users do not: flora's no-face-map marker bypasses the default-face-map fallback and can derive lighting weights from arbitrary art. A raw UI sprite or future producer that lets an unset zero handle reach the shader similarly samples the first asset rather than the checkerboard.
- **Expected behavior:** No allocatable texture handle aliases the missing/undefined sentinel. Handle 0 either always resolves to slot 0 regardless of table contents, or allocation starts outside the reserved sentinel range and registration rejects attempts to overwrite it. Producers use one documented no-face-map representation consistently, with a regression that crosses allocation, table resolution, and the world shader contract.
- **Scope and constraints:** Surfaced from PR #292 / issue #286. Preserve slot 0 as the undefined descriptor, stable draw-time resolution, alias sharing, default-face-map fallback, existing Lua integer handles, and no cached-slot dependency. Audit literal zero and negative texture-handle conventions before changing the allocator; some CPU paths deliberately use zero as an absence test and should retain that behavior.
- **Remaining uncertainty:** No screenshot or pixel readback was taken, so the exact visual severity on current shipped flora is inferred from the shader and startup data rather than measured. The namespace collision and the flora face-map path are direct; a small offscreen render using a deliberately colored first texture would settle the symptom and guard the repair.

## 2. Stable-handle table capacity

### [#1699] PRR-2. The fixed handle table silently expires after 65,536 allocations

> **Captured note:** Give stable texture handles an enforced lifetime/capacity contract. The engine must not accept a handle that the GPU table cannot represent and then announce it as loaded; grow or recycle the stable-id namespace safely, or reject exhaustion before allocating/uploading resources with an observable failure.

**Verification:** Verified structurally, with player reachability bounded to a sufficiently long or leaky process. Handles are dense and monotonically process-wide, the table covers only ids 0 through 65,535, and out-of-range writes are silent. The 65,537th generated handle is therefore stored as an ordinary ready asset and assigned a real bindless slot while both shaders resolve it to undefined slot 0.

**Evidence:**

- PR #292 chose a fixed 65,536-entry SSBO and explicitly described ids beyond it as clamping to slot 0. Neither issue #286 nor the PR established an allocation budget, rejection path, reset point, or proof that a process cannot reach the ceiling.
- `src/Engine/Graphics/Vulkan/Texture/Limits.hs:32-44` fixes `handleSlotTableSize = 65536`, says stable ids are dense and monotonic, and treats overflow as a graceful degradation confined to an extremely long session.
- `src/Engine/Asset/Manager.hs:54-57` monotonically increments `apNextTextureHandle` for every request without checking `handleSlotTableSize`. The counter starts at zero, so id 65,536 is produced on allocation 65,537.
- `src/Engine/Graphics/Vulkan/Texture/Bindless.hs:293-303` silently returns without writing when `hid >= handleSlotTableSize`; it emits no error or warning and gives the caller no failure result.
- `src/Engine/Graphics/Vulkan/Texture/Bindless.hs:335-363` still allocates and writes a descriptor slot, inserts the out-of-range handle into `btsHandleMap`/`btsImageViews`, and returns `Just bindlessHandle`. The caller therefore cannot distinguish table exhaustion from a fully usable registration.
- `src/Engine/Scripting/Lua/Message/Texture.hs:228-281` treats that `Just` as success, records the atlas and real slot, marks the handle `AssetReady`, stores its dimensions, and queues `LuaAssetLoaded`. Cached aliases follow the same false-success shape at `:86-126`: an out-of-range table write is dropped, but the alias becomes ready and receives the loaded callback.
- `src/Engine/Graphics/Vulkan/ShaderCode.hs:184-203,323-338` bounds-checks both world and UI handle ids and resolves any out-of-range id to slot 0. Thus the uploaded image and descriptor slot remain live while all geometry carrying the advertised handle displays the undefined texture.
- `src/Engine/Asset/Manager.hs:119-177` contains the only whole-pool cleanup path, which the module header says has no caller. Even if wired, it resets `apNextAssetId` but not `apNextTextureHandle`; ordinary texture unload also frees a descriptor slot without recycling the stable handle id. The namespace is process-lifetime monotonic by construction.
- Allocation is not limited to a fixed boot manifest: Lua `engine.loadTexture`, YAML content loaders, font atlases, preview/zoom textures, and procedural blood textures all call `generateTextureHandle`. Cached path reuse also consumes a fresh alias id rather than returning the canonical handle.
- Pending report `project_review_909-874.md` PRR-3 identifies one stale-preview race that can grow aliases and accelerate the counter, but repairing that leak does not make the finite process-wide namespace safe. Open #1281 owns final-release invalidation for aliases; closed #975 only deduplicated the table-size constant. Tracker and report searches found no issue that owns admission control or stable-id exhaustion itself.

**Handoff context:**

- **Current behavior:** After 65,536 texture-handle allocations, every later fresh texture or cached alias can report successful completion and occupy engine/GPU bookkeeping while rendering as slot 0. Unloading textures recovers descriptor slots but never restores handle-table capacity, so the process cannot recover short of restart.
- **Expected behavior:** Every successfully announced texture handle is representable by the shader lookup for its entire advertised lifetime. If capacity is finite, allocation fails before upload/state publication with a logged and script-visible result; otherwise the table grows or ids recycle only after all cached vertices, aliases, callbacks, and live references can no longer name the old generation.
- **Scope and constraints:** Surfaced from PR #292 / issue #286. Preserve stable cached geometry across descriptor-slot recycling, the 16,384 descriptor-array ceiling, alias-to-shared-slot behavior, render-thread Vulkan ownership, and slot-0 fallback for genuinely missing handles. Coordinate with PRR-1's reserved-zero decision because it changes the usable id range by one, but do not make fixing known alias leaks the only capacity guarantee.
- **Remaining uncertainty:** Current shipped startup content is far below the ceiling, and this review did not run a 65,537-request GPU reproduction. The failure transition is deterministic from the counter, range guard, registration result, and shader bounds check; practical priority depends on expected process lifetime and the rate of preview/runtime texture churn.
